# =========================================================================
# earnings_quality_module.R — Automated risk matrix + Quality of Earnings
#
# Builds on existing statement pulls (same row selectors as shenanigans /
# F-Score). Red-flag only: scores are veto / quality screens, never buy cues.
#
#   Beneish M-Score (8-var, 1999): M > −1.78 → manipulation-risk alert
#   Production–Valuation Divergence: AR YoY vs Revenue YoY (pp gap)
#   Accruals / cash conversion → 0–100 Quality of Earnings (QoE) score
#   Risk matrix: F-Score × M-Score × AR–Rev × Accruals → composite flag
# =========================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.EQ_THRESHOLDS <- list(
  mscore_alert = -1.78,   # Beneish classic cutoff (higher M = worse)
  mscore_watch = -2.22,   # milder watch band
  ar_rev_alert_pp = 15,   # AR growth − rev growth (percentage points)
  ar_rev_watch_pp = 8,
  accruals_alert = 0.10,  # (NI − OCF) / avg assets
  accruals_watch = 0.05,
  cash_conv_alert = 0.50, # OCF / NI when NI > 0
  cash_conv_watch = 0.80
)

.eq_finite <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  length(x) == 1L && is.finite(x)
}

.eq_pick <- function(df, patterns, include_ttm = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(numeric(0))
  v <- tryCatch(
    select_clean_metric_row_any(df, patterns, include_ttm = include_ttm),
    error = function(e) NA
  )
  if (is.null(v) || (length(v) == 1L && is.na(v[1]))) return(numeric(0))
  suppressWarnings(as.numeric(v))
}

.eq_ratio <- function(a, b) {
  if (!.eq_finite(a) || !.eq_finite(b) || b == 0) return(NA_real_)
  a / b
}

.eq_yoy <- function(v) {
  if (length(v) < 2L || !.eq_finite(v[1]) || !.eq_finite(v[2]) || v[2] == 0) {
    return(NA_real_)
  }
  (v[1] - v[2]) / abs(v[2])
}

.eq_safe_div <- function(num, den, default = 1) {
  if (!.eq_finite(num) || !.eq_finite(den) || den == 0) return(default)
  num / den
}

.eq_empty <- function(ok = FALSE, message = "") {
  list(
    ok = isTRUE(ok),
    message = as.character(message %||% ""),
    m_score = NA_real_,
    m_components = list(),
    m_flag = "資料不足",
    ar_rev_pp = NA_real_,
    ar_yoy = NA_real_,
    rev_yoy = NA_real_,
    ar_rev_flag = "資料不足",
    accruals_ratio = NA_real_,
    cash_conversion = NA_real_,
    f_score = NA_real_,
    f_quality = NA_real_,
    qoe_score = NA_real_,
    qoe_grade = "—",
    qoe_flag = "資料不足",
    qoe_parts = list(),
    risk_level = "資料不足",
    risk_rows = data.frame(
      metric = character(0),
      value = character(0),
      flag = character(0),
      note = character(0),
      stringsAsFactors = FALSE
    ),
    red_flags = character(0)
  )
}

.eq_ctx <- function(d_is, d_bs, d_cf) {
  list(
    rev = .eq_pick(d_is, c("^Total Revenue$")),
    gp = .eq_pick(d_is, c("^Gross Profit$")),
    sga = .eq_pick(d_is, c(
      "^Selling General And Administration$",
      "^Selling General Administrative$",
      "SG&A",
      "Selling And Marketing Expense"
    )),
    ni = .eq_pick(d_is, if (exists("NET_INCOME_PATTERNS")) {
      NET_INCOME_PATTERNS
    } else {
      c("Net Income Common Stockholders", "^Net Income$")
    }),
    ocf = .eq_pick(d_cf, c("^Operating Cash Flow$")),
    dep = .eq_pick(d_cf, c("^Depreciation And Amortization$", "^Depreciation$")),
    ar = .eq_pick(d_bs, c(
      "^Accounts Receivable$",
      "^Gross Accounts Receivable$",
      "Net Accounts Receivable"
    )),
    ca = .eq_pick(d_bs, c("^Current Assets$", "^Total Current Assets$")),
    ppe = .eq_pick(d_bs, c("^Net PPE$", "Net Property Plant And Equipment")),
    assets = .eq_pick(d_bs, c("^Total Assets$")),
    debt = .eq_pick(d_bs, c(
      "^Total Debt$",
      "^Long Term Debt And Capital Lease Obligation$",
      "^Long Term Debt$"
    )),
    cl = .eq_pick(d_bs, c("^Current Liabilities$", "^Total Current Liabilities$"))
  )
}

#' Beneish M-Score (8-variable model). Higher M → higher manipulation risk.
.eq_beneish_components <- function(ctx) {
  # Indices: [1]=latest year, [2]=prior year
  rev0 <- ctx$rev[1]; rev1 <- ctx$rev[2]
  ar0 <- ctx$ar[1]; ar1 <- ctx$ar[2]
  gp0 <- ctx$gp[1]; gp1 <- ctx$gp[2]
  ca0 <- ctx$ca[1]; ca1 <- ctx$ca[2]
  ppe0 <- ctx$ppe[1]; ppe1 <- ctx$ppe[2]
  ta0 <- ctx$assets[1]; ta1 <- ctx$assets[2]
  dep0 <- ctx$dep[1]; dep1 <- ctx$dep[2]
  sga0 <- ctx$sga[1]; sga1 <- ctx$sga[2]
  ni0 <- ctx$ni[1]
  ocf0 <- ctx$ocf[1]
  debt0 <- ctx$debt[1]; debt1 <- ctx$debt[2]

  dsri <- .eq_safe_div(
    .eq_ratio(ar0, rev0),
    .eq_ratio(ar1, rev1),
    default = NA_real_
  )
  gm0 <- .eq_ratio(gp0, rev0)
  gm1 <- .eq_ratio(gp1, rev1)
  gmi <- .eq_safe_div(gm1, gm0, default = NA_real_)

  soft0 <- if (.eq_finite(ta0)) {
    1 - ((if (.eq_finite(ca0)) ca0 else 0) + (if (.eq_finite(ppe0)) ppe0 else 0)) / ta0
  } else NA_real_
  soft1 <- if (.eq_finite(ta1)) {
    1 - ((if (.eq_finite(ca1)) ca1 else 0) + (if (.eq_finite(ppe1)) ppe1 else 0)) / ta1
  } else NA_real_
  aqi <- .eq_safe_div(soft0, soft1, default = NA_real_)

  sgi <- .eq_safe_div(rev0, rev1, default = NA_real_)

  depr_rate0 <- .eq_ratio(dep0, (if (.eq_finite(dep0)) dep0 else 0) + (if (.eq_finite(ppe0)) ppe0 else 0))
  depr_rate1 <- .eq_ratio(dep1, (if (.eq_finite(dep1)) dep1 else 0) + (if (.eq_finite(ppe1)) ppe1 else 0))
  depi <- .eq_safe_div(depr_rate1, depr_rate0, default = NA_real_)

  sgai <- .eq_safe_div(
    .eq_ratio(sga0, rev0),
    .eq_ratio(sga1, rev1),
    default = NA_real_
  )

  tata <- .eq_ratio(
    (if (.eq_finite(ni0)) ni0 else NA_real_) - (if (.eq_finite(ocf0)) ocf0 else NA_real_),
    ta0
  )

  lev0 <- .eq_ratio(debt0, ta0)
  lev1 <- .eq_ratio(debt1, ta1)
  lvgi <- .eq_safe_div(lev0, lev1, default = NA_real_)

  list(
    DSRI = dsri, GMI = gmi, AQI = aqi, SGI = sgi,
    DEPI = depi, SGAI = sgai, TATA = tata, LVGI = lvgi
  )
}

.eq_beneish_m <- function(comp) {
  need <- c("DSRI", "GMI", "AQI", "SGI", "DEPI", "SGAI", "TATA", "LVGI")
  vals <- vapply(need, function(k) {
    x <- suppressWarnings(as.numeric(comp[[k]])[1])
    if (!is.finite(x)) NA_real_ else x
  }, numeric(1))
  if (any(!is.finite(vals))) return(NA_real_)
  # Beneish (1999) unweighted 8-variable model
  -4.84 +
    0.920 * vals[["DSRI"]] +
    0.528 * vals[["GMI"]] +
    0.404 * vals[["AQI"]] +
    0.892 * vals[["SGI"]] +
    0.115 * vals[["DEPI"]] -
    0.172 * vals[["SGAI"]] +
    4.679 * vals[["TATA"]] -
    0.327 * vals[["LVGI"]]
}

.eq_flag_m <- function(m, th = .EQ_THRESHOLDS) {
  if (!.eq_finite(m)) return("資料不足")
  if (m > th$mscore_alert) return("警示")
  if (m > th$mscore_watch) return("觀察")
  "通過"
}

.eq_flag_ar_rev <- function(pp, th = .EQ_THRESHOLDS) {
  if (!.eq_finite(pp)) return("資料不足")
  if (pp >= th$ar_rev_alert_pp) return("警示")
  if (pp >= th$ar_rev_watch_pp) return("觀察")
  "通過"
}

.eq_qoe_grade <- function(score) {
  if (!.eq_finite(score)) return("—")
  if (score >= 80) return("A")
  if (score >= 65) return("B")
  if (score >= 50) return("C")
  if (score >= 35) return("D")
  "F"
}

.eq_flag_qoe <- function(score) {
  if (!.eq_finite(score)) return("資料不足")
  if (score < 35) return("警示")
  if (score < 50) return("觀察")
  "通過"
}

.eq_flag_fscore <- function(total) {
  if (!.eq_finite(total)) return("資料不足")
  if (total < 4) return("警示")
  if (total < 7) return("觀察")
  "通過"
}

.eq_composite_risk <- function(flags) {
  flags <- as.character(flags)
  flags <- flags[nzchar(flags) & flags != "資料不足"]
  if (!length(flags)) return("資料不足")
  if (any(flags == "警示")) return("高")
  if (any(flags == "觀察")) return("中")
  "低"
}

#' Compute Quality of Earnings pack + automated risk matrix.
#' @param f_score_pack optional list from compute_report_f_score (avoids recompute)
evaluate_earnings_quality <- function(d_is, d_bs, d_cf, f_score_pack = NULL) {
  if (is.null(d_is) || is.null(d_bs) || is.null(d_cf) ||
      !is.data.frame(d_is) || !is.data.frame(d_bs) || !is.data.frame(d_cf) ||
      nrow(d_is) < 1L || nrow(d_bs) < 1L || nrow(d_cf) < 1L) {
    return(.eq_empty(
      ok = FALSE,
      message = "載入損益、資產負債與現金流後，將顯示盈餘品質與風險矩陣。"
    ))
  }
  th <- .EQ_THRESHOLDS
  ctx <- tryCatch(.eq_ctx(d_is, d_bs, d_cf), error = function(e) NULL)
  if (is.null(ctx)) {
    return(.eq_empty(ok = FALSE, message = "財報讀取失敗，略過盈餘品質評分。"))
  }

  comp <- .eq_beneish_components(ctx)
  m_score <- .eq_beneish_m(comp)
  m_flag <- .eq_flag_m(m_score, th)

  ar_yoy <- .eq_yoy(ctx$ar)
  rev_yoy <- .eq_yoy(ctx$rev)
  ar_rev_pp <- if (.eq_finite(ar_yoy) && .eq_finite(rev_yoy)) {
    (ar_yoy - rev_yoy) * 100
  } else {
    NA_real_
  }
  ar_rev_flag <- .eq_flag_ar_rev(ar_rev_pp, th)

  ta0 <- ctx$assets[1]
  ta1 <- ctx$assets[2]
  avg_ta <- if (.eq_finite(ta0) && .eq_finite(ta1)) {
    (ta0 + ta1) / 2
  } else if (.eq_finite(ta0)) {
    ta0
  } else {
    NA_real_
  }
  ni0 <- ctx$ni[1]
  ocf0 <- ctx$ocf[1]
  accruals <- .eq_ratio(
    (if (.eq_finite(ni0)) ni0 else NA_real_) - (if (.eq_finite(ocf0)) ocf0 else NA_real_),
    avg_ta
  )
  cash_conv <- if (.eq_finite(ni0) && ni0 > 0 && .eq_finite(ocf0)) {
    ocf0 / ni0
  } else if (.eq_finite(ni0) && ni0 <= 0 && .eq_finite(ocf0) && ocf0 > 0) {
    # Loss but positive OCF → treat as strong cash quality for scoring
    1.2
  } else {
    NA_real_
  }

  if (is.null(f_score_pack) && exists("compute_report_f_score", mode = "function")) {
    f_score_pack <- tryCatch(
      compute_report_f_score(d_is, d_bs, d_cf),
      error = function(e) NULL
    )
  }
  f_total <- if (is.list(f_score_pack)) {
    suppressWarnings(as.numeric(f_score_pack$total)[1])
  } else {
    NA_real_
  }
  f_qual <- if (is.list(f_score_pack)) {
    suppressWarnings(as.numeric(f_score_pack$quality_flag)[1])
  } else {
    NA_real_
  }
  f_flag <- .eq_flag_fscore(f_total)

  # --- QoE score (0–100): higher = stronger cash earnings quality ---
  score <- 50
  parts <- list()

  # Accruals contribution
  acc_pts <- if (!.eq_finite(accruals)) {
    0
  } else if (accruals <= 0) {
    20
  } else if (accruals <= th$accruals_watch) {
    10
  } else if (accruals <= th$accruals_alert) {
    0
  } else if (accruals <= 0.15) {
    -15
  } else {
    -25
  }
  score <- score + acc_pts
  parts$accruals <- acc_pts

  # Cash conversion
  cc_pts <- if (!.eq_finite(cash_conv)) {
    0
  } else if (cash_conv >= 1) {
    15
  } else if (cash_conv >= th$cash_conv_watch) {
    8
  } else if (cash_conv >= th$cash_conv_alert) {
    0
  } else {
    -15
  }
  score <- score + cc_pts
  parts$cash_conversion <- cc_pts

  # F-Score earnings-quality gate (OCF > operating earnings)
  fq_pts <- if (!.eq_finite(f_qual)) {
    0
  } else if (identical(as.numeric(f_qual), 1)) {
    10
  } else {
    -10
  }
  score <- score + fq_pts
  parts$f_quality <- fq_pts

  # Beneish
  m_pts <- if (!.eq_finite(m_score)) {
    0
  } else if (m_score <= th$mscore_watch) {
    10
  } else if (m_score <= th$mscore_alert) {
    0
  } else {
    -20
  }
  score <- score + m_pts
  parts$m_score <- m_pts

  # AR–Revenue divergence (production–valuation)
  ar_pts <- if (!.eq_finite(ar_rev_pp)) {
    0
  } else if (ar_rev_pp <= th$ar_rev_watch_pp) {
    5
  } else if (ar_rev_pp <= th$ar_rev_alert_pp) {
    0
  } else {
    -15
  }
  score <- score + ar_pts
  parts$ar_rev <- ar_pts

  qoe <- max(0, min(100, round(score)))
  qoe_grade <- .eq_qoe_grade(qoe)
  qoe_flag <- .eq_flag_qoe(qoe)
  accruals_flag <- if (!.eq_finite(accruals)) {
    "資料不足"
  } else if (accruals > th$accruals_alert) {
    "警示"
  } else if (accruals > th$accruals_watch) {
    "觀察"
  } else {
    "通過"
  }

  fmt_num <- function(x, dig = 2) {
    if (!.eq_finite(x)) return("—")
    sprintf(paste0("%.", dig, "f"), x)
  }
  fmt_pct <- function(x, dig = 1) {
    if (!.eq_finite(x)) return("—")
    sprintf(paste0("%+.", dig, "f%%"), x * 100)
  }
  fmt_pp <- function(x, dig = 1) {
    if (!.eq_finite(x)) return("—")
    sprintf(paste0("%+.", dig, "f pp"), x)
  }

  risk_rows <- data.frame(
    metric = c(
      "Piotroski F-Score",
      "Beneish M-Score",
      "Production–Valuation Divergence",
      "Accruals ratio",
      "Quality of Earnings"
    ),
    value = c(
      if (.eq_finite(f_total)) paste0(as.integer(f_total), " / 9") else "—",
      fmt_num(m_score, 2),
      fmt_pp(ar_rev_pp),
      fmt_pct(accruals, 1),
      if (.eq_finite(qoe)) paste0(qoe, " (", qoe_grade, ")") else "—"
    ),
    flag = c(f_flag, m_flag, ar_rev_flag, accruals_flag, qoe_flag),
    note = c(
      "品質檢核閘門（≥7 偏強／＜4 偏弱）；非買進訊號",
      "M > −1.78 視為盈餘操縱風險偏高（Beneish 經典門檻）",
      "應收帳款 YoY − 營收 YoY；脫鉤過大為紅旗",
      "(NI − OCF)／平均總資產；應計偏高削弱獲利含金量",
      "綜合應計、現金轉換、F-Score 品質項、M-Score、應收脫鉤"
    ),
    stringsAsFactors = FALSE
  )

  risk_level <- .eq_composite_risk(risk_rows$flag)

  red_flags <- character(0)
  if (identical(m_flag, "警示")) {
    red_flags <- c(red_flags, sprintf(
      "Beneish M-Score＝%s（＞%.2f）— 盈餘操縱風險偏高",
      fmt_num(m_score, 2), th$mscore_alert
    ))
  }
  if (identical(ar_rev_flag, "警示")) {
    red_flags <- c(red_flags, sprintf(
      "應收帳款與營收成長脫鉤 %+0.1f pp（生產／評價分歧）",
      ar_rev_pp
    ))
  }
  if (identical(accruals_flag, "警示")) {
    red_flags <- c(red_flags, sprintf(
      "應計項目偏高：Accruals＝%s",
      fmt_pct(accruals, 1)
    ))
  }
  if (identical(f_flag, "警示")) {
    red_flags <- c(red_flags, sprintf(
      "Piotroski F-Score＝%s／9（品質檢核偏弱）",
      if (.eq_finite(f_total)) as.character(as.integer(f_total)) else "—"
    ))
  }
  if (identical(qoe_flag, "警示")) {
    red_flags <- c(red_flags, sprintf(
      "盈餘品質 QoE＝%s（%s）— 獲利含金量偏弱",
      if (.eq_finite(qoe)) as.character(qoe) else "—",
      qoe_grade
    ))
  }

  list(
    ok = TRUE,
    message = "",
    m_score = m_score,
    m_components = comp,
    m_flag = m_flag,
    ar_rev_pp = ar_rev_pp,
    ar_yoy = ar_yoy,
    rev_yoy = rev_yoy,
    ar_rev_flag = ar_rev_flag,
    accruals_ratio = accruals,
    cash_conversion = cash_conv,
    f_score = f_total,
    f_quality = f_qual,
    qoe_score = qoe,
    qoe_grade = qoe_grade,
    qoe_flag = qoe_flag,
    qoe_parts = parts,
    risk_level = risk_level,
    risk_rows = risk_rows,
    red_flags = red_flags
  )
}

.eq_status_class <- function(flag) {
  switch(as.character(flag)[1],
         "警示" = "ynow-eq-alert",
         "觀察" = "ynow-eq-watch",
         "通過" = "ynow-eq-pass",
         "高" = "ynow-eq-alert",
         "中" = "ynow-eq-watch",
         "低" = "ynow-eq-pass",
         "ynow-eq-na")
}

.eq_localize_flag <- function(flag, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  switch(as.character(flag)[1],
         "警示" = if (en) "Alert" else "警示",
         "觀察" = if (en) "Watch" else "觀察",
         "通過" = if (en) "Pass" else "通過",
         "資料不足" = if (en) "Insufficient data" else "資料不足",
         "高" = if (en) "High" else "高",
         "中" = if (en) "Medium" else "中",
         "低" = if (en) "Low" else "低",
         as.character(flag)[1])
}

.eq_localize_metric <- function(metric, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  if (!en) {
    return(switch(as.character(metric)[1],
      "Piotroski F-Score" = "Piotroski F-Score",
      "Beneish M-Score" = "Beneish M-Score",
      "Production–Valuation Divergence" = "應收／營收成長脫鉤",
      "Accruals ratio" = "應計比率",
      "Quality of Earnings" = "盈餘品質 (QoE)",
      as.character(metric)[1]
    ))
  }
  as.character(metric)[1]
}

.eq_localize_note <- function(metric, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  if (en) {
    return(switch(as.character(metric)[1],
      "Piotroski F-Score" = "Quality screen (≥7 stronger / <4 weaker); never a buy signal",
      "Beneish M-Score" = "M > −1.78 flags elevated earnings-manipulation risk (Beneish cutoff)",
      "Production–Valuation Divergence" = "AR YoY − Revenue YoY; large decoupling is a red flag",
      "Accruals ratio" = "(NI − OCF) / average total assets; high accruals weaken cash earnings",
      "Quality of Earnings" = "Combines accruals, cash conversion, F-Score quality gate, M-Score, AR decoupling",
      ""
    ))
  }
  switch(as.character(metric)[1],
    "Piotroski F-Score" = "品質檢核閘門（≥7 偏強／＜4 偏弱）；非買進訊號",
    "Beneish M-Score" = "M > −1.78 視為盈餘操縱風險偏高（Beneish 經典門檻）",
    "Production–Valuation Divergence" = "應收帳款 YoY − 營收 YoY；脫鉤過大為紅旗",
    "Accruals ratio" = "(NI − OCF)／平均總資產；應計偏高削弱獲利含金量",
    "Quality of Earnings" = "綜合應計、現金轉換、F-Score 品質項、M-Score、應收脫鉤",
    ""
  )
}

#' Quality of Earnings dashboard UI (shown before absolute statement focus).
earnings_quality_dashboard_ui <- function(pack, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  if (is.null(pack) || !isTRUE(pack$ok)) {
    return(tags$div(
      class = "ynow-eq-wrap",
      tags$p(
        class = "ynow-eq-waiting",
        pack$message %||% if (en) {
          "Load income, balance sheet, and cash flow to score earnings quality."
        } else {
          "載入損益、資產負債與現金流後，將顯示盈餘品質評分。"
        }
      )
    ))
  }

  score <- pack$qoe_score
  grade <- pack$qoe_grade
  flag <- .eq_localize_flag(pack$qoe_flag, locale)
  flag_cls <- .eq_status_class(pack$qoe_flag)

  fmt_pct <- function(x) {
    if (!.eq_finite(x)) return("—")
    sprintf("%+.1f%%", x * 100)
  }
  fmt_x <- function(x) {
    if (!.eq_finite(x)) return("—")
    sprintf("%.2f×", x)
  }
  fmt_m <- function(x) {
    if (!.eq_finite(x)) return("—")
    sprintf("%.2f", x)
  }
  fmt_pp <- function(x) {
    if (!.eq_finite(x)) return("—")
    sprintf("%+.1f pp", x)
  }

  title <- if (en) "Quality of Earnings" else "盈餘品質 (Quality of Earnings)"
  sub <- if (en) {
    "Cash earnings score before you lean on absolute P&L figures. Research screen only—not a buy signal."
  } else {
    "在解讀財報絕對數字前，先看獲利含金量評分。僅供研究／品質檢核，非買進訊號。"
  }

  bar_row <- function(lab, pts, max_abs = 25) {
    p <- suppressWarnings(as.numeric(pts)[1])
    if (!is.finite(p)) p <- 0
    pct <- max(0, min(100, abs(p) / max_abs * 100))
    cls <- if (p >= 0) "ynow-eq-bar-pos" else "ynow-eq-bar-neg"
    tags$div(
      class = "ynow-eq-bar-row",
      tags$span(class = "ynow-eq-bar-lab", lab),
      tags$div(
        class = "ynow-eq-bar-track",
        tags$div(class = paste("ynow-eq-bar-fill", cls), style = sprintf("width:%.0f%%;", pct))
      ),
      tags$span(class = "ynow-eq-bar-pts", sprintf("%+d", as.integer(round(p))))
    )
  }

  part_labs <- if (en) {
    list(
      accruals = "Accruals",
      cash_conversion = "Cash conversion",
      f_quality = "F-Score quality gate",
      m_score = "Beneish M-Score",
      ar_rev = "AR–Revenue decoupling"
    )
  } else {
    list(
      accruals = "應計項目",
      cash_conversion = "現金轉換",
      f_quality = "F-Score 品質閘門",
      m_score = "Beneish M-Score",
      ar_rev = "應收／營收脫鉤"
    )
  }

  tags$div(
    class = "ynow-eq-wrap",
    id = "ynow_eq_dashboard",
    tags$div(
      class = "ynow-eq-head",
      tags$h4(class = "ynow-eq-title", title),
      tags$p(class = "ynow-eq-sub", sub)
    ),
    tags$div(
      class = "ynow-eq-scorecard",
      tags$div(
        class = paste("ynow-eq-score-pill", flag_cls),
        tags$div(class = "ynow-eq-score-num", if (.eq_finite(score)) as.character(score) else "—"),
        tags$div(
          class = "ynow-eq-score-meta",
          tags$span(class = "ynow-eq-grade", paste0("Grade ", grade)),
          tags$span(class = paste("ynow-eq-flag", flag_cls), flag)
        )
      ),
      tags$div(
        class = "ynow-eq-kpis",
        tags$div(
          class = "ynow-eq-kpi",
          tags$span(class = "ynow-eq-kpi-k", if (en) "Accruals" else "應計比率"),
          tags$span(class = "ynow-eq-kpi-v", fmt_pct(pack$accruals_ratio))
        ),
        tags$div(
          class = "ynow-eq-kpi",
          tags$span(class = "ynow-eq-kpi-k", if (en) "Cash conversion" else "現金轉換"),
          tags$span(class = "ynow-eq-kpi-v", fmt_x(pack$cash_conversion))
        ),
        tags$div(
          class = "ynow-eq-kpi",
          tags$span(class = "ynow-eq-kpi-k", "M-Score"),
          tags$span(class = "ynow-eq-kpi-v", fmt_m(pack$m_score))
        ),
        tags$div(
          class = "ynow-eq-kpi",
          tags$span(class = "ynow-eq-kpi-k", if (en) "AR–Rev gap" else "應收－營收差"),
          tags$span(class = "ynow-eq-kpi-v", fmt_pp(pack$ar_rev_pp))
        )
      )
    ),
    tags$div(
      class = "ynow-eq-bars",
      bar_row(part_labs$accruals, pack$qoe_parts$accruals),
      bar_row(part_labs$cash_conversion, pack$qoe_parts$cash_conversion),
      bar_row(part_labs$f_quality, pack$qoe_parts$f_quality, max_abs = 10),
      bar_row(part_labs$m_score, pack$qoe_parts$m_score),
      bar_row(part_labs$ar_rev, pack$qoe_parts$ar_rev, max_abs = 15)
    )
  )
}

#' Automated risk matrix UI (F-Score / M-Score / AR–Rev / Accruals / QoE).
earnings_risk_matrix_ui <- function(pack, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  if (is.null(pack) || !isTRUE(pack$ok)) {
    return(tags$div(
      class = "ynow-eq-matrix-wrap",
      tags$p(
        class = "ynow-eq-waiting",
        pack$message %||% if (en) {
          "Risk matrix appears after statements load."
        } else {
          "載入財報後顯示風險矩陣。"
        }
      )
    ))
  }

  level <- .eq_localize_flag(pack$risk_level, locale)
  level_cls <- .eq_status_class(pack$risk_level)
  title <- if (en) "Automated risk matrix" else "自動化風險矩陣"
  sub <- if (en) {
    "Beneish M-Score, Piotroski F-Score, AR–Revenue decoupling, and accruals — red-flag scan only."
  } else {
    "Beneish M-Score、Piotroski F-Score、應收／營收脫鉤與應計項目——僅作紅旗掃描，非買進訊號。"
  }
  level_lab <- if (en) "Composite risk" else "綜合風險"

  rows <- pack$risk_rows
  cards <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    fl <- .eq_localize_flag(r$flag, locale)
    tags$div(
      class = paste("ynow-eq-matrix-card", .eq_status_class(r$flag)),
      tags$div(
        class = "ynow-eq-matrix-h",
        tags$span(class = "ynow-eq-matrix-metric", .eq_localize_metric(r$metric, locale)),
        tags$span(class = paste("ynow-eq-flag", .eq_status_class(r$flag)), fl)
      ),
      tags$div(class = "ynow-eq-matrix-val", r$value),
      tags$p(class = "ynow-eq-matrix-note", .eq_localize_note(r$metric, locale))
    )
  })

  flag_block <- NULL
  if (length(pack$red_flags) > 0L) {
    flag_block <- tags$div(
      class = "ynow-eq-redflags",
      tags$h5(tags$b(if (en) "Red flags" else "紅旗警示")),
      tags$ul(
        lapply(pack$red_flags, function(x) tags$li(x))
      )
    )
  }

  tags$div(
    class = "ynow-eq-matrix-wrap",
    id = "ynow_eq_risk_matrix",
    tags$div(
      class = "ynow-eq-head",
      tags$h4(class = "ynow-eq-title", title),
      tags$p(class = "ynow-eq-sub", sub),
      tags$div(
        class = paste("ynow-eq-level", level_cls),
        tags$span(class = "ynow-eq-level-k", level_lab),
        tags$span(class = "ynow-eq-level-v", level)
      )
    ),
    tags$div(class = "ynow-eq-matrix-grid", cards),
    flag_block
  )
}

#' Red-flag strings for PDF / fraud warning collectors.
collect_earnings_quality_warnings <- function(d_is, d_bs, d_cf, f_score_pack = NULL) {
  pack <- tryCatch(
    evaluate_earnings_quality(d_is, d_bs, d_cf, f_score_pack = f_score_pack),
    error = function(e) NULL
  )
  if (is.null(pack) || !isTRUE(pack$ok)) return(character(0))
  as.character(pack$red_flags %||% character(0))
}
