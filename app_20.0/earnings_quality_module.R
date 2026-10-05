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
    m_contributions = list(),
    m_flag = "資料不足",
    m_sgi_nuance = list(active = FALSE, message_zh = "", message_en = ""),
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
    cogs = .eq_pick(d_is, c("^Cost Of Revenue$", "^Cost Of Goods Sold$", "^Cost of Goods Sold$")),
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
    # Beneish LVGI uses Current Liabilities + Long-Term Debt (not total debt alone)
    cl = .eq_pick(d_bs, c("^Current Liabilities$", "^Total Current Liabilities$")),
    ltd = .eq_pick(d_bs, c(
      "^Long Term Debt$",
      "^Long Term Debt And Capital Lease Obligation$",
      "^Long-Term Debt$"
    )),
    debt = .eq_pick(d_bs, c(
      "^Total Debt$",
      "^Long Term Debt And Capital Lease Obligation$",
      "^Long Term Debt$"
    ))
  )
}

.eq_at <- function(v, i) {
  if (is.null(v) || length(v) < i) return(NA_real_)
  suppressWarnings(as.numeric(v[i])[1])
}

.eq_gm_margin <- function(sales, gp, cogs) {
  if (.eq_finite(gp) && .eq_finite(sales) && sales != 0) return(gp / sales)
  if (.eq_finite(sales) && .eq_finite(cogs) && sales != 0) return((sales - cogs) / sales)
  NA_real_
}

.eq_lvg_ratio <- function(cl, ltd, debt, ta) {
  if (!.eq_finite(ta) || ta == 0) return(NA_real_)
  # Prefer CL + LTD (Beneish); fall back to Total Debt if CL/LTD incomplete
  if (.eq_finite(cl) || .eq_finite(ltd)) {
    num <- (if (.eq_finite(cl)) cl else 0) + (if (.eq_finite(ltd)) ltd else 0)
    return(num / ta)
  }
  if (.eq_finite(debt)) return(debt / ta)
  NA_real_
}

#' Beneish M-Score (8-variable model). Higher M → higher manipulation risk.
.eq_beneish_components <- function(ctx) {
  # Indices: [1]=latest year t, [2]=prior year t-1
  rev0 <- .eq_at(ctx$rev, 1); rev1 <- .eq_at(ctx$rev, 2)
  ar0 <- .eq_at(ctx$ar, 1); ar1 <- .eq_at(ctx$ar, 2)
  gp0 <- .eq_at(ctx$gp, 1); gp1 <- .eq_at(ctx$gp, 2)
  cogs0 <- .eq_at(ctx$cogs, 1); cogs1 <- .eq_at(ctx$cogs, 2)
  ca0 <- .eq_at(ctx$ca, 1); ca1 <- .eq_at(ctx$ca, 2)
  ppe0 <- .eq_at(ctx$ppe, 1); ppe1 <- .eq_at(ctx$ppe, 2)
  ta0 <- .eq_at(ctx$assets, 1); ta1 <- .eq_at(ctx$assets, 2)
  dep0 <- .eq_at(ctx$dep, 1); dep1 <- .eq_at(ctx$dep, 2)
  sga0 <- .eq_at(ctx$sga, 1); sga1 <- .eq_at(ctx$sga, 2)
  ni0 <- .eq_at(ctx$ni, 1)
  ocf0 <- .eq_at(ctx$ocf, 1)
  cl0 <- .eq_at(ctx$cl, 1); cl1 <- .eq_at(ctx$cl, 2)
  ltd0 <- .eq_at(ctx$ltd, 1); ltd1 <- .eq_at(ctx$ltd, 2)
  debt0 <- .eq_at(ctx$debt, 1); debt1 <- .eq_at(ctx$debt, 2)

  # 1 DSRI
  dsri <- .eq_safe_div(
    .eq_ratio(ar0, rev0),
    .eq_ratio(ar1, rev1),
    default = NA_real_
  )
  # 2 GMI — Gross Profit preferred; COGS fallback
  gm0 <- .eq_gm_margin(rev0, gp0, cogs0)
  gm1 <- .eq_gm_margin(rev1, gp1, cogs1)
  gmi <- .eq_safe_div(gm1, gm0, default = NA_real_)

  # 3 AQI
  soft0 <- if (.eq_finite(ta0)) {
    1 - ((if (.eq_finite(ca0)) ca0 else 0) + (if (.eq_finite(ppe0)) ppe0 else 0)) / ta0
  } else NA_real_
  soft1 <- if (.eq_finite(ta1)) {
    1 - ((if (.eq_finite(ca1)) ca1 else 0) + (if (.eq_finite(ppe1)) ppe1 else 0)) / ta1
  } else NA_real_
  aqi <- if (.eq_finite(soft0) && .eq_finite(soft1)) {
    soft0 / max(soft1, 1e-4)
  } else {
    NA_real_
  }

  # 4 SGI
  sgi <- if (.eq_finite(rev0) && .eq_finite(rev1)) {
    rev0 / max(rev1, 1e-4)
  } else {
    NA_real_
  }

  # 5 DEPI
  depr_rate0 <- .eq_ratio(dep0, (if (.eq_finite(dep0)) dep0 else 0) + (if (.eq_finite(ppe0)) ppe0 else 0))
  depr_rate1 <- .eq_ratio(dep1, (if (.eq_finite(dep1)) dep1 else 0) + (if (.eq_finite(ppe1)) ppe1 else 0))
  depi <- if (.eq_finite(depr_rate0) && .eq_finite(depr_rate1)) {
    depr_rate1 / max(depr_rate0, 1e-4)
  } else {
    NA_real_
  }

  # 6 SGAI
  sgai <- .eq_safe_div(
    .eq_ratio(sga0, rev0),
    .eq_ratio(sga1, rev1),
    default = NA_real_
  )

  # 7 TATA — cash-flow accruals: (NI − OCF) / Total Assets
  tata <- .eq_ratio(
    (if (.eq_finite(ni0)) ni0 else NA_real_) - (if (.eq_finite(ocf0)) ocf0 else NA_real_),
    ta0
  )

  # 8 LVGI — (Current Liabilities + Long-Term Debt) / Total Assets
  lev0 <- .eq_lvg_ratio(cl0, ltd0, debt0, ta0)
  lev1 <- .eq_lvg_ratio(cl1, ltd1, debt1, ta1)
  lvgi <- if (.eq_finite(lev0) && .eq_finite(lev1)) {
    lev0 / max(lev1, 1e-4)
  } else {
    NA_real_
  }

  list(
    DSRI = dsri, GMI = gmi, AQI = aqi, SGI = sgi,
    DEPI = depi, SGAI = sgai, TATA = tata, LVGI = lvgi
  )
}

.EQ_BENEISH_COEF <- c(
  DSRI = 0.920, GMI = 0.528, AQI = 0.404, SGI = 0.892,
  DEPI = 0.115, SGAI = -0.172, TATA = 4.679, LVGI = -0.327
)

.eq_fill_beneish_comp <- function(comp, fill_neutral = TRUE) {
  need <- names(.EQ_BENEISH_COEF)
  out <- lapply(need, function(k) {
    x <- suppressWarnings(as.numeric(comp[[k]])[1])
    if (is.finite(x) && !is.infinite(x)) return(x)
    if (!isTRUE(fill_neutral)) return(NA_real_)
    if (identical(k, "TATA")) 0 else 1
  })
  names(out) <- need
  out
}

.eq_beneish_m <- function(comp, fill_neutral = FALSE) {
  vals <- .eq_fill_beneish_comp(comp, fill_neutral = fill_neutral)
  vals <- unlist(vals, use.names = TRUE)
  if (any(!is.finite(vals))) return(NA_real_)
  # Beneish (1999) unweighted 8-variable model
  -4.84 + sum(.EQ_BENEISH_COEF[names(vals)] * vals)
}

#' Linear contribution of each index vs neutral (indices=1, TATA=0).
.eq_beneish_contributions <- function(comp, fill_neutral = TRUE) {
  vals <- unlist(.eq_fill_beneish_comp(comp, fill_neutral = fill_neutral), use.names = TRUE)
  if (any(!is.finite(vals))) return(list())
  neutral <- vals
  neutral[names(neutral) != "TATA"] <- 1
  neutral["TATA"] <- 0
  contrib <- .EQ_BENEISH_COEF[names(vals)] * (vals - neutral)
  as.list(contrib)
}

#' Growth-driven M-Score nuance: high SGI alone can trip the −1.78 cutoff.
.eq_beneish_sgi_nuance <- function(m_score, comp, th = .EQ_THRESHOLDS) {
  if (!.eq_finite(m_score) || m_score <= th$mscore_alert) {
    return(list(active = FALSE, message_zh = "", message_en = ""))
  }
  contrib <- .eq_beneish_contributions(comp, fill_neutral = TRUE)
  if (!length(contrib)) {
    return(list(active = FALSE, message_zh = "", message_en = ""))
  }
  sgi_c <- suppressWarnings(as.numeric(contrib$SGI)[1])
  dsri <- suppressWarnings(as.numeric(comp$DSRI)[1])
  pos <- contrib[vapply(contrib, function(x) is.finite(x) && x > 0, logical(1))]
  if (!length(pos) || !is.finite(sgi_c) || sgi_c <= 0) {
    return(list(active = FALSE, message_zh = "", message_en = ""))
  }
  top <- names(pos)[which.max(unlist(pos))]
  dsri_ok <- !is.finite(dsri) || dsri < 1.2
  if (!identical(top, "SGI") || !isTRUE(dsri_ok)) {
    return(list(active = FALSE, message_zh = "", message_en = ""))
  }
  list(
    active = TRUE,
    message_zh = paste0(
      "此紅旗主要由營收成長指數 (SGI) 推高；若應收帳款指數 (DSRI) 未同步異常，",
      "可能是高成長科技股常見誤判，請再核對應收帳款與收入品質。"
    ),
    message_en = paste0(
      "This alert is driven mainly by the Sales Growth Index (SGI). ",
      "If Days Sales in Receivables (DSRI) is not also elevated, ",
      "it may be a high-growth false positive — re-check receivables vs revenue quality."
    )
  )
}

#' Public Beneish calculator (tibble / data.frame with *_t / *_t1 columns).
#'
#' Expected columns (any subset; missing indices fill to 1, TATA to 0 when
#' `fill_neutral = TRUE`): Receivables, Sales, COGS, Current_Assets, PPE,
#' Total_Assets, Depreciation, SGA, Current_Liabilities, Long_Term_Debt,
#' Net_Income, OCF — each with `_t` and `_t1` suffixes.
calculate_m_score <- function(fin_data, fill_neutral = TRUE) {
  if (is.null(fin_data) || !is.data.frame(fin_data) || nrow(fin_data) < 1L) {
    return(data.frame(
      M_Score = NA_real_, Risk_Flag = FALSE,
      DSRI = NA_real_, GMI = NA_real_, AQI = NA_real_, SGI = NA_real_,
      DEPI = NA_real_, SGAI = NA_real_, LVGI = NA_real_, TATA = NA_real_,
      SGI_Nuance = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  pick <- function(nm) {
    if (!nm %in% names(fin_data)) return(NA_real_)
    suppressWarnings(as.numeric(fin_data[[nm]][1]))
  }
  # Build a one-row ctx-like list from wide columns
  ctx <- list(
    rev = c(pick("Sales_t"), pick("Sales_t1")),
    gp = c(pick("Gross_Profit_t"), pick("Gross_Profit_t1")),
    cogs = c(pick("COGS_t"), pick("COGS_t1")),
    sga = c(pick("SGA_t"), pick("SGA_t1")),
    ni = c(pick("Net_Income_t"), NA_real_),
    ocf = c(pick("OCF_t"), NA_real_),
    dep = c(pick("Depreciation_t"), pick("Depreciation_t1")),
    ar = c(pick("Receivables_t"), pick("Receivables_t1")),
    ca = c(pick("Current_Assets_t"), pick("Current_Assets_t1")),
    ppe = c(pick("PPE_t"), pick("PPE_t1")),
    assets = c(pick("Total_Assets_t"), pick("Total_Assets_t1")),
    cl = c(pick("Current_Liabilities_t"), pick("Current_Liabilities_t1")),
    ltd = c(pick("Long_Term_Debt_t"), pick("Long_Term_Debt_t1")),
    debt = c(pick("Total_Debt_t"), pick("Total_Debt_t1"))
  )
  # Allow Gross Profit derived from Sales − COGS inside .eq_gm_margin
  if (!.eq_finite(ctx$gp[1]) && .eq_finite(ctx$rev[1]) && .eq_finite(ctx$cogs[1])) {
    ctx$gp[1] <- ctx$rev[1] - ctx$cogs[1]
  }
  if (!.eq_finite(ctx$gp[2]) && .eq_finite(ctx$rev[2]) && .eq_finite(ctx$cogs[2])) {
    ctx$gp[2] <- ctx$rev[2] - ctx$cogs[2]
  }
  comp <- .eq_beneish_components(ctx)
  m <- .eq_beneish_m(comp, fill_neutral = fill_neutral)
  filled <- .eq_fill_beneish_comp(comp, fill_neutral = fill_neutral)
  nuance <- .eq_beneish_sgi_nuance(m, filled, .EQ_THRESHOLDS)
  data.frame(
    M_Score = if (.eq_finite(m)) as.numeric(m) else NA_real_,
    Risk_Flag = isTRUE(.eq_finite(m) && m > .EQ_THRESHOLDS$mscore_alert),
    DSRI = filled$DSRI, GMI = filled$GMI, AQI = filled$AQI, SGI = filled$SGI,
    DEPI = filled$DEPI, SGAI = filled$SGAI, LVGI = filled$LVGI, TATA = filled$TATA,
    SGI_Nuance = isTRUE(nuance$active),
    stringsAsFactors = FALSE
  )
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
  m_score <- .eq_beneish_m(comp, fill_neutral = FALSE)
  # If a few optional ratios are missing, still estimate with neutral fills so the
  # control point lights up; mark incomplete components in the nuance path.
  if (!.eq_finite(m_score)) {
    m_score <- .eq_beneish_m(comp, fill_neutral = TRUE)
  }
  m_flag <- .eq_flag_m(m_score, th)
  m_contrib <- .eq_beneish_contributions(comp, fill_neutral = TRUE)
  m_sgi_nuance <- .eq_beneish_sgi_nuance(m_score, comp, th)

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
    if (isTRUE(m_sgi_nuance$active) && nzchar(m_sgi_nuance$message_zh)) {
      red_flags <- c(red_flags, m_sgi_nuance$message_zh)
    }
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
    m_contributions = m_contrib,
    m_flag = m_flag,
    m_sgi_nuance = m_sgi_nuance,
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

#' Prominent Beneish M-Score control-point widget (alert before DCF reliance).
#' Uses shinydashboard-friendly markup (no bslib dependency).
beneish_m_score_widget_ui <- function(pack, locale = "zh-TW") {
  en <- identical(as.character(locale)[1], "en")
  if (is.null(pack) || !isTRUE(pack$ok) || !.eq_finite(pack$m_score)) {
    return(tags$div(
      class = "ynow-mscore-widget ynow-mscore-widget--na",
      tags$div(
        class = "ynow-mscore-widget__head",
        tags$span(class = "ynow-mscore-widget__title", if (en) {
          "Beneish M-Score (earnings-manipulation screen)"
        } else {
          "Beneish M-Score（盈餘操縱風險）"
        }),
        tags$span(class = "ynow-mscore-widget__badge", if (en) "Insufficient data" else "資料不足")
      ),
      tags$p(
        class = "ynow-mscore-widget__msg",
        if (en) {
          "Load income, balance sheet, and cash flow (two years) to compute the 8-variable M-Score."
        } else {
          "載入至少兩年損益、資產負債與現金流後，將計算 8 變數 M-Score。"
        }
      )
    ))
  }

  m <- as.numeric(pack$m_score)[1]
  flag <- as.character(pack$m_flag %||% "")[1]
  risk <- identical(flag, "警示") || (is.finite(m) && m > .EQ_THRESHOLDS$mscore_alert)
  watch <- !risk && (identical(flag, "觀察") || (is.finite(m) && m > .EQ_THRESHOLDS$mscore_watch))
  tone <- if (risk) "danger" else if (watch) "watch" else "pass"
  icon_name <- if (risk) "exclamation-triangle" else if (watch) "eye" else "check-circle"
  status_txt <- if (en) {
    if (risk) {
      sprintf("Alert: elevated manipulation risk (M > %.2f)", .EQ_THRESHOLDS$mscore_alert)
    } else if (watch) {
      "Watch band — review accruals and receivables before relying on DCF."
    } else {
      "Within safer band — still a research screen, not a buy signal."
    }
  } else {
    if (risk) {
      sprintf("警告：盈餘操縱風險偏高（M > %.2f）", .EQ_THRESHOLDS$mscore_alert)
    } else if (watch) {
      "觀察帶 — 解讀 DCF 前請再核對應計與應收。"
    } else {
      "落於相對安全區間 — 僅供研究／品質檢核，非買進訊號。"
    }
  }

  comp <- pack$m_components %||% list()
  fmt2 <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) return("—")
    sprintf("%.2f", x)
  }
  detail_items <- list(
    list("DSRI", if (en) "Days Sales in Receivables" else "應收帳款指數", comp$DSRI),
    list("GMI", if (en) "Gross Margin Index" else "毛利率指數", comp$GMI),
    list("AQI", if (en) "Asset Quality Index" else "資產品質指數", comp$AQI),
    list("SGI", if (en) "Sales Growth Index" else "營收成長指數", comp$SGI),
    list("DEPI", if (en) "Depreciation Index" else "折舊率指數", comp$DEPI),
    list("SGAI", if (en) "SG&A Index" else "銷管費用指數", comp$SGAI),
    list("TATA", if (en) "Total Accruals / Assets" else "總應計／總資產", comp$TATA),
    list("LVGI", if (en) "Leverage Index" else "槓桿指數", comp$LVGI)
  )

  nuance <- pack$m_sgi_nuance
  nuance_block <- NULL
  if (is.list(nuance) && isTRUE(nuance$active)) {
    nuance_block <- tags$div(
      class = "ynow-mscore-widget__nuance",
      tags$b(if (en) "Growth nuance: " else "高成長誤判提示："),
      if (en) nuance$message_en else nuance$message_zh
    )
  }

  tags$div(
    class = paste0("ynow-mscore-widget ynow-mscore-widget--", tone),
    id = "ynow_mscore_widget",
    tags$div(
      class = "ynow-mscore-widget__head",
      tags$span(
        class = "ynow-mscore-widget__title",
        icon(icon_name),
        " ",
        if (en) {
          "Beneish M-Score (earnings-manipulation screen)"
        } else {
          "Beneish M-Score（盈餘操縱風險）"
        }
      ),
      tags$span(
        class = paste("ynow-mscore-widget__badge", paste0("ynow-mscore-widget__badge--", tone)),
        .eq_localize_flag(if (risk) "警示" else if (watch) "觀察" else "通過", locale)
      )
    ),
    tags$div(
      class = "ynow-mscore-widget__body",
      tags$div(
        class = "ynow-mscore-widget__value",
        sprintf("%.2f", m)
      ),
      tags$div(
        class = "ynow-mscore-widget__copy",
        tags$p(class = "ynow-mscore-widget__msg", status_txt),
        tags$p(
          class = "ynow-mscore-widget__hint",
          if (en) {
            "Internal control point: confirm statement quality before relying on DCF / MOS."
          } else {
            "內控防雷控制點：解讀 DCF／MOS 前先確認財報基礎是否可靠。"
          }
        )
      )
    ),
    nuance_block,
    tags$details(
      class = "ynow-mscore-widget__details",
      tags$summary(if (en) "8-variable breakdown" else "8 項指標細項"),
      tags$ul(
        class = "ynow-mscore-widget__list",
        lapply(detail_items, function(it) {
          tags$li(sprintf("%s (%s): %s", it[[1]], it[[2]], fmt2(it[[3]])))
        })
      )
    )
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
