# ==========================================
# setup.R - 財報數據處理與輔助函數模組
# ==========================================
# C-STRUCT-04: non-formula domains live in sibling files sourced below
# (setup_fx / setup_fs / setup_shares / setup_report / setup_plotly / setup_feedback).
# DCF / WACC / MOS / TV / SGR formula bodies stay here. Lifecycle stays in
# lifecycle_classification.R. valuation_guard.R is sourced at the end.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

.ynow_setup_dir <- (function() {
  frames <- sys.nframe()
  if (frames >= 1L) {
    for (i in rev(seq_len(frames))) {
      o <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE), error = function(e) NULL)
      if (is.character(o) && length(o) == 1L && grepl("setup\\.R$", o)) {
        return(dirname(normalizePath(o, winslash = "/", mustWork = FALSE)))
      }
    }
  }
  getwd()
})()

source(file.path(.ynow_setup_dir, "setup_fx.R"), local = TRUE, encoding = "UTF-8")
source(file.path(.ynow_setup_dir, "setup_fs.R"), local = TRUE, encoding = "UTF-8")
source(file.path(.ynow_setup_dir, "setup_shares.R"), local = TRUE, encoding = "UTF-8")


# 取得當期單一數值：流量科目優先 TTM，存量科目用最新財年
select_current_metric <- function(df, metric_name, type = c("flow", "stock")) {
  type <- match.arg(type)
  include_ttm <- identical(type, "flow")
  vals <- select_clean_metric_row(df, metric_name, include_ttm = include_ttm)
  if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
  vals[1]
}

select_current_metric_any <- function(df, metric_names, type = c("flow", "stock")) {
  type <- match.arg(type)
  include_ttm <- identical(type, "flow")
  vals <- select_clean_metric_row_any(df, metric_names, include_ttm = include_ttm)
  if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
  vals[1]
}

#' Mean of (num/den) over the first `n` paired observations (newest-first series).
#' Used for DCF projection margins when CapEx/Rev spikes vs recent history.
#' Default `n = 3` is a common smoothing window (not a regulatory standard);
#' override via FCF tab inputs `capex_spike_avg_years` / APP_DEFAULTS.
avg_ratio_newest <- function(num, den, n = 3L) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  m <- min(length(num), length(den), as.integer(n))
  if (m < 1L) return(NA_real_)
  ratios <- num[seq_len(m)] / den[seq_len(m)]
  ratios <- ratios[is.finite(ratios) & is.finite(den[seq_len(m)]) & den[seq_len(m)] != 0]
  if (length(ratios) < 1L) return(NA_real_)
  mean(ratios)
}

#' True when newest ratio is an outlier vs the prior window (excludes newest from baseline).
#' Default `mult = 1.35` is a heuristic (~35% above recent norm); not econometrically calibrated.
#' User may change via FCF tab `capex_spike_mult` / APP_DEFAULTS.
ratio_spike_vs_prior <- function(num, den, prior_n = 2L, mult = 1.35) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  if (length(num) < 2L || length(den) < 2L) return(FALSE)
  latest <- num[1] / den[1]
  if (!is.finite(latest) || !is.finite(den[1]) || den[1] == 0) return(FALSE)
  prior <- avg_ratio_newest(num[-1], den[-1], n = prior_n)
  if (!is.finite(prior) || prior <= 0) return(FALSE)
  isTRUE(latest > mult * prior)
}

#' CapEx / Revenue margin to seed FCF projection after statement sync.
#'
#' Always derived from the newly loaded CapEx and Revenue series (newest-first).
#' Spike smoothing (engineering heuristic): when the newest ratio exceeds
#' `mult` × the prior window, use the `avg_n`-year mean; otherwise use the
#' latest year. Callers must overwrite any prior `proj_capex_rate` so a
#' foundry-level ratio (e.g. TSM ~37%) cannot stick onto a fabless name.
#'
#' @param cap_hist CapEx series (newest first; signs ignored via abs)
#' @param rev_hist Revenue series (newest first)
#' @param settings list with enabled, mult, avg_n, prior_n (see FCF CapEx spike UI)
#' @return list(margin, pct, spike, latest_pct, avg_pct, ok)
fcf_sync_capex_revenue_margin <- function(cap_hist, rev_hist, settings = NULL) {
  empty <- list(
    margin = NA_real_, pct = NA_real_, spike = FALSE,
    latest_pct = NA_real_, avg_pct = NA_real_, ok = FALSE
  )
  if (is.null(settings) || !is.list(settings)) {
    settings <- list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
  }
  cap_hist <- abs(suppressWarnings(as.numeric(cap_hist)))
  rev_hist <- suppressWarnings(as.numeric(rev_hist))
  if (length(cap_hist) < 1L || length(rev_hist) < 1L) return(empty)
  latest_m <- cap_hist[1] / rev_hist[1]
  if (!is.finite(latest_m) || !is.finite(rev_hist[1]) || rev_hist[1] == 0) {
    return(empty)
  }
  avg_n <- suppressWarnings(as.integer(settings$avg_n)[1])
  if (!is.finite(avg_n) || avg_n < 1L) avg_n <- 3L
  prior_n <- suppressWarnings(as.integer(settings$prior_n)[1])
  if (!is.finite(prior_n) || prior_n < 1L) prior_n <- 2L
  mult <- suppressWarnings(as.numeric(settings$mult)[1])
  if (!is.finite(mult) || mult <= 1) mult <- 1.35
  avg_m <- avg_ratio_newest(cap_hist, rev_hist, n = avg_n)
  spike <- isTRUE(settings$enabled) && ratio_spike_vs_prior(
    cap_hist, rev_hist, prior_n = prior_n, mult = mult
  )
  margin <- if (isTRUE(spike) && is.finite(avg_m)) avg_m else latest_m
  list(
    margin = margin,
    pct = if (is.finite(margin)) margin * 100 else NA_real_,
    spike = spike,
    latest_pct = latest_m * 100,
    avg_pct = if (is.finite(avg_m)) avg_m * 100 else NA_real_,
    ok = is.finite(margin) && margin > 0
  )
}

# 裁切財務表格至指定科目（含該列）
# Yahoo 網頁列序：營收在上、end_metric 在下 → 保留 1:idx
# 若 end_metric 落在第 1 列（常見於未反轉的 yfinance），改取最後一個命中，避免只剩一列
trim_financial_table <- function(df, end_metric) {
  if (is.null(df) || nrow(df) == 0) return(df)
  idx <- grep(end_metric, df[[1]], ignore.case = TRUE)
  if (length(idx) == 0) return(df)
  end_i <- idx[1]
  if (end_i == 1L && length(idx) == 1L && nrow(df) > 1) {
    # 裁切錨點在頂端 → 視為列序相反，不裁切（保留全表）
    return(df)
  }
  if (end_i == 1L && length(idx) > 1L) {
    end_i <- idx[length(idx)]
  }
  df[seq_len(end_i), , drop = FALSE]
}

# 取得最新一期期末現金餘額
get_latest_cash_position <- function(df_cf) {
  if (is.null(df_cf) || nrow(df_cf) == 0) return(NA)
  cash_kws <- c("End Cash Position", "Ending Cash Position", "Cash at End of Period")
  for (kw in cash_kws) {
    val <- select_clean_metric_row(df_cf, kw)
    if (length(val) > 0 && !all(is.na(val))) return(select_current_metric(df_cf, kw, "flow"))
  }
  return(NA) 
}

# 計算陣列的平均值
get_avg <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) == 0) return(NA)
  return(mean(x, na.rm = TRUE))
}

# 計算陣列的平均成長率（輸入須為最新財年 → 最舊財年）
get_avg_growth <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(NA)
  
  # YoY = (較新 - 較舊) / |較舊|；勿用 diff()/head()（會把成長算成負值）
  rates <- (head(x, -1) - tail(x, -1)) / abs(tail(x, -1))
  rates <- rates[is.finite(rates)]
  
  if (length(rates) == 0) return(NA)
  mean(rates, na.rm = TRUE) * 100
}

#' TRUE when the income statement has a usable Gross Profit series.
stmt_has_gross_profit <- function(d_is) {
  vals <- tryCatch(
    select_clean_metric_row(d_is, "Gross Profit", include_ttm = FALSE),
    error = function(e) NA
  )
  length(vals) > 0L && any(is.finite(suppressWarnings(as.numeric(vals))))
}

#' Bank / financial IS shape: no Gross Profit, but Net Interest Income (or Interest Income).
#' Does NOT invent COGS / Gross Profit.
stmt_is_bank_interest_structure <- function(d_is) {
  if (isTRUE(stmt_has_gross_profit(d_is))) return(FALSE)
  nii <- tryCatch(
    select_clean_metric_row(d_is, "Net Interest Income", include_ttm = FALSE),
    error = function(e) NA
  )
  if (length(nii) > 0L && any(is.finite(suppressWarnings(as.numeric(nii))))) return(TRUE)
  ii <- tryCatch(
    select_clean_metric_row(d_is, "^Interest Income$", include_ttm = FALSE),
    error = function(e) NA
  )
  length(ii) > 0L && any(is.finite(suppressWarnings(as.numeric(ii))))
}

#' KPI filler for the Gross Margin / Gross Profit Growth boxes.
#'
#' Modes:
#' - `gross`: standard Gross Profit / Revenue (+ GP growth)
#' - `bank_nii`: engineering heuristic Net Interest Income / Revenue (+ NII growth),
#'   only when Gross Profit is absent and bank interest rows exist — never labeled as GM
#' - `unavailable`: no GP and no bank heuristic → N/A with explicit reason
kpi_gross_margin_box_metrics <- function(d_is) {
  empty <- function(mode, reason) {
    list(
      mode = mode,
      reason = reason,
      margin_pct = NA_real_,
      growth_pct = NA_real_,
      numer_avg = NA_real_,
      rev_avg = NA_real_
    )
  }
  if (!is.data.frame(d_is) || nrow(d_is) < 1L) {
    return(empty("unavailable", "empty_income_statement"))
  }
  rev_row <- select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)
  rev <- get_avg(rev_row)

  if (isTRUE(stmt_has_gross_profit(d_is))) {
    gp_row <- select_clean_metric_row(d_is, "Gross Profit", include_ttm = FALSE)
    gp <- get_avg(gp_row)
    margin <- if (!is.na(gp) && !is.na(rev) && rev != 0) gp / rev * 100 else NA_real_
    growth <- get_avg_growth(gp_row)
    return(list(
      mode = "gross",
      reason = "ok",
      margin_pct = if (is.finite(margin)) margin else NA_real_,
      growth_pct = if (is.finite(growth)) growth else NA_real_,
      numer_avg = if (is.finite(gp)) gp else NA_real_,
      rev_avg = if (is.finite(rev)) rev else NA_real_
    ))
  }

  if (isTRUE(stmt_is_bank_interest_structure(d_is))) {
    nii_row <- select_clean_metric_row(d_is, "Net Interest Income", include_ttm = FALSE)
    if (length(nii_row) == 0L || all(is.na(nii_row))) {
      nii_row <- select_clean_metric_row(d_is, "^Interest Income$", include_ttm = FALSE)
    }
    nii <- get_avg(nii_row)
    margin <- if (!is.na(nii) && !is.na(rev) && rev != 0) nii / rev * 100 else NA_real_
    growth <- get_avg_growth(nii_row)
    return(list(
      mode = "bank_nii",
      reason = "bank_nii_heuristic",
      margin_pct = if (is.finite(margin)) margin else NA_real_,
      growth_pct = if (is.finite(growth)) growth else NA_real_,
      numer_avg = if (is.finite(nii)) nii else NA_real_,
      rev_avg = if (is.finite(rev)) rev else NA_real_
    ))
  }

  empty("unavailable", "missing_gross_profit_row")
}

# ==========================================
# 🚨 數據缺失警示 UI 共用函數
# ==========================================
ui_missing_data_alert <- function(check_list, fallback_msg = "系統已自動將上述項目視為 0 代入計算。請確認是否需要手動補齊數值以確保預測精確度。") {
  
  # 找出 NA, NULL 或空字串的項目
  missing_items <- names(check_list)[sapply(check_list, function(x) {
    is.null(x) || is.na(x) || (is.character(x) && trimws(x) == "")
  })]
  
  if (length(missing_items) > 0) {
    shiny::div(
      style = "color: #a94442; background-color: #f2dede; border: 1px solid #ebccd1; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      shiny::icon("exclamation-triangle"), 
      shiny::tags$b(" 偵測到資料缺漏："), 
      shiny::tags$span(style = "color: #c7254e; font-weight: bold;", paste(missing_items, collapse = "、 ")),
      shiny::br(),
      shiny::span(style = "font-size: 13px;", fallback_msg)
    )
  } else {
    NULL # 無缺值則不回傳 UI
  }
}

# 確保數值安全，若無值則回傳 0
safe_num <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) return(0)
  return(x)
}

# Relative ±1% shock for formula-param elasticity tables (DCF / DDM / RI / P/B).
# |ε| is d ln V / d ln x at the current MODEL PARAMETERS, not ticker price/shares.
PARAM_SENSITIVITY_SHOCK <- 0.01

.param_rel_shock <- function(x, sign = -1, shock = PARAM_SENSITIVITY_SHOCK) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x) || abs(x) < 1e-12) return(NA_real_)
  x * (1 + sign * shock)
}

#' Build one row for relative ±1% elasticity tables (DCF / DDM / RI / P/B).
#' Shock is multiplicative on the parameter; Δ估值% ≈ elasticity ε when shock = 1%.
.param_sensitivity_infl_row <- function(param, base_val, unit, p0, p_down, p_up, note = "") {
  d_dn <- if (is.finite(p_down) && is.finite(p0) && abs(p0) > 1e-9) 100 * (p_down - p0) / abs(p0) else NA_real_
  d_up <- if (is.finite(p_up) && is.finite(p0) && abs(p0) > 1e-9) 100 * (p_up - p0) / abs(p0) else NA_real_
  infl <- mean(c(abs(d_dn), abs(d_up)), na.rm = TRUE)
  if (!is.finite(infl)) infl <- NA_real_
  data.frame(
    參數 = param,
    基準值 = if (identical(unit, "%")) sprintf("%.2f%%", base_val) else
      if (identical(unit, "$")) {
        if (exists("format_dollar_abbr", mode = "function")) format_dollar_abbr(base_val) else sprintf("%.2f", base_val)
      } else if (identical(unit, "x")) sprintf("%.2f", base_val) else as.character(base_val),
    `估值Δ% (−1%)` = if (is.finite(d_dn)) sprintf("%+.2f%%", d_dn) else "N/A",
    `估值Δ% (+1%)` = if (is.finite(d_up)) sprintf("%+.2f%%", d_up) else "N/A",
    `｜ε｜` = if (is.finite(infl)) sprintf("%.2f", infl) else "N/A",
    說明 = note,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Elasticity row when the feasible shock on x is not exactly ±1% (e.g. integer n).
#' Converts (ΔV/V)/(Δx/x) into the same ±1% display units as `.param_sensitivity_infl_row`.
.param_sensitivity_infl_row_xy <- function(param, base_val, unit, v0, v_dn, v_up,
                                           x0, x_dn, x_up, note = "",
                                           shock = PARAM_SENSITIVITY_SHOCK) {
  .el <- function(v1, x1) {
    if (!all(is.finite(c(v0, v1, x0, x1))) || abs(v0) <= 1e-9) return(NA_real_)
    if (!is.finite(x0) || abs(x0) < 1e-12 || abs(x1 - x0) < 1e-12) return(NA_real_)
    ((v1 - v0) / abs(v0)) / ((x1 - x0) / x0)
  }
  e_dn <- .el(v_dn, x_dn)
  e_up <- .el(v_up, x_up)
  d_dn <- if (is.finite(e_dn)) 100 * e_dn * (-shock) else NA_real_
  d_up <- if (is.finite(e_up)) 100 * e_up * shock else NA_real_
  p_dn <- if (is.finite(d_dn) && is.finite(v0)) v0 * (1 + d_dn / 100) else NA_real_
  p_up <- if (is.finite(d_up) && is.finite(v0)) v0 * (1 + d_up / 100) else NA_real_
  .param_sensitivity_infl_row(param, base_val, unit, v0, p_dn, p_up, note)
}

#' True when a parameter can take a relative ±1% shock (skip ~0 like DCF g).
.param_sensitivity_rel_ok <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  is.finite(x) && abs(x) > 1e-8
}

#' Canonical DCF enterprise value from formula parameters only (unit starting FCFF).
#' F_t = F0 × product of explicit-period growth; TV = F_n(1+g)/(r2−g).
#' Independent of ticker cash, debt, shares, market price, and FCFF dollar level.
#' Rates are decimals (e.g. 0.08 not 8).
.dcf_formula_ev <- function(n, r1, g_term, g_near = 0, r2 = NULL,
                            yr_stage1 = NULL, g_stage2 = NULL, f0 = 1,
                            yr_stage2 = NULL) {
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(n) || n < 1L) return(NA_real_)
  r1 <- suppressWarnings(as.numeric(r1)[1])
  r2 <- if (is.null(r2)) r1 else suppressWarnings(as.numeric(r2)[1])
  g_term <- suppressWarnings(as.numeric(g_term)[1])
  g_near <- suppressWarnings(as.numeric(g_near)[1])
  f0 <- suppressWarnings(as.numeric(f0)[1])
  if (!is.finite(g_near)) g_near <- 0
  if (!is.finite(r1) || !is.finite(r2) || !is.finite(g_term) || !is.finite(f0)) return(NA_real_)
  if (r1 <= -0.999 || r2 <= -0.999 || g_term >= r2) return(NA_real_)

  two_stage <- !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L
  if (two_stage) {
    if (!exists("validate_stage_years", mode = "function")) return(NA_real_)
    st <- validate_stage_years(n, yr_stage1, yr_stage2, g2 = g_stage2, g_term = g_term)
    if (!isTRUE(st$ok)) return(NA_real_)
    if (is.null(g_stage2) || !is.finite(as.numeric(g_stage2)[1])) return(NA_real_)
    y1 <- st$yr_stage1
    rs <- c(rep(r1, min(y1, n)), rep(r2, max(0L, n - y1)))
    g2 <- as.numeric(g_stage2)[1]
    gs <- c(rep(g_near, max(0L, y1 - 1L)), rep(g2, max(0L, n - y1)))
    if (length(gs) < n - 1L) {
      pad <- if (length(gs) == 0L) g_near else gs[length(gs)]
      gs <- c(gs, rep(pad, n - 1L - length(gs)))
    }
    gs <- gs[seq_len(max(0L, n - 1L))]
  } else {
    rs <- rep(r1, n)
    gs <- rep(g_near, max(0L, n - 1L))
  }

  f <- numeric(n)
  f[1] <- f0
  if (n >= 2L && length(gs) >= 1L) {
    for (t in seq_len(n - 1L)) {
      f[t + 1L] <- f[t] * (1 + gs[t])
    }
  }
  disc <- cumprod(1 + rs)
  if (any(!is.finite(disc)) || any(abs(disc) < 1e-15)) return(NA_real_)
  if (!is.finite(f[n]) || f[n] < 0) return(NA_real_)
  pv <- sum(f / disc)
  tv <- f[n] * (1 + g_term) / (r2 - g_term)
  pv + tv / disc[n]
}

#' Unit-revenue FCFF path matching `fcf_projection_module` (base Rev = 1).
#' Year-1 revenue grows from 1; FCFF_t = Rev_t × (NOPAT+D&A−CapEx margins) − ΔRev_t × NWC margin.
#' Independent of dollar scale. Growth rates and margins are decimals.
.dcf_unit_fcff_path <- function(n, g_near = 0, g_stage2 = NULL, yr_stage1 = NULL,
                                nopat_m = 0, depre_m = 0, capex_m = 0, nwc_m = 0,
                                two_stage = FALSE, yr_stage2 = NULL) {
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(n) || n < 1L) return(numeric(0))
  g_near <- suppressWarnings(as.numeric(g_near)[1])
  if (!is.finite(g_near)) g_near <- 0
  g2 <- if (!is.null(g_stage2) && is.finite(as.numeric(g_stage2)[1])) {
    as.numeric(g_stage2)[1]
  } else if (!isTRUE(two_stage)) {
    g_near
  } else {
    NA_real_
  }
  nopat_m <- suppressWarnings(as.numeric(nopat_m)[1]); if (!is.finite(nopat_m)) nopat_m <- 0
  depre_m <- suppressWarnings(as.numeric(depre_m)[1]); if (!is.finite(depre_m)) depre_m <- 0
  capex_m <- suppressWarnings(as.numeric(capex_m)[1]); if (!is.finite(capex_m)) capex_m <- 0
  nwc_m <- suppressWarnings(as.numeric(nwc_m)[1]); if (!is.finite(nwc_m)) nwc_m <- 0
  if (isTRUE(two_stage) && !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L) {
    if (!exists("validate_stage_years", mode = "function")) return(numeric(0))
    st <- validate_stage_years(n, yr_stage1, yr_stage2, g2 = g2, g_term = NA)
    if (!isTRUE(st$ok) || !is.finite(g2)) return(numeric(0))
    y1 <- st$yr_stage1
    g_path <- ifelse(seq_len(n) <= y1, g_near, g2)
  } else {
    g_path <- rep(g_near, n)
  }
  rev <- numeric(n)
  fcff <- numeric(n)
  base_rev <- 1
  for (i in seq_len(n)) {
    prev <- if (i == 1L) base_rev else rev[i - 1L]
    rev[i] <- prev * (1 + g_path[i])
    fcff[i] <- rev[i] * (nopat_m + depre_m - capex_m) - (rev[i] - prev) * nwc_m
  }
  fcff
}

#' Discount an explicit FCFF path with the same WACC / Gordon TV as `.dcf_formula_ev`.
.dcf_formula_ev_from_fcff <- function(fcff, r1, g_term, r2 = NULL, yr_stage1 = NULL,
                                    yr_stage2 = NULL) {
  fcff <- suppressWarnings(as.numeric(fcff))
  n <- length(fcff)
  if (n < 1L || any(!is.finite(fcff))) return(NA_real_)
  r1 <- suppressWarnings(as.numeric(r1)[1])
  r2 <- if (is.null(r2)) r1 else suppressWarnings(as.numeric(r2)[1])
  g_term <- suppressWarnings(as.numeric(g_term)[1])
  if (!is.finite(r1) || !is.finite(r2) || !is.finite(g_term)) return(NA_real_)
  if (r1 <= -0.999 || r2 <= -0.999 || g_term >= r2) return(NA_real_)
  if (!is.finite(fcff[n]) || fcff[n] < 0) return(NA_real_)
  two_stage <- !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L
  if (two_stage) {
    if (!exists("validate_stage_years", mode = "function")) return(NA_real_)
    st <- validate_stage_years(n, yr_stage1, yr_stage2)
    if (!isTRUE(st$ok)) return(NA_real_)
    y1 <- st$yr_stage1
    rs <- c(rep(r1, min(y1, n)), rep(r2, max(0L, n - y1)))
  } else {
    rs <- rep(r1, n)
  }
  disc <- cumprod(1 + rs)
  if (any(!is.finite(disc)) || any(abs(disc) < 1e-15)) return(NA_real_)
  pv <- sum(fcff / disc)
  tv <- fcff[n] * (1 + g_term) / (r2 - g_term)
  pv + tv / disc[n]
}

#' Present value of each explicit-period cash flow (no terminal value mixed in).
#' `rates` are decimals (WACC or Ke); length 1 is recycled.
#' Invalid rates stay NA. They are not replaced with 10%.
dcf_yearly_cf_pv <- function(cf, rates) {
  cf <- suppressWarnings(as.numeric(cf))
  n <- length(cf)
  if (n < 1L) return(numeric(0))
  rates <- suppressWarnings(as.numeric(rates))
  if (length(rates) < 1L || all(!is.finite(rates))) {
    out <- rep(NA_real_, n)
    attr(out, "invalid_rate") <- TRUE
    return(out)
  }
  if (length(rates) == 1L) rates <- rep(rates, n)
  if (length(rates) < n) rates <- c(rates, rep(tail(rates, 1), n - length(rates)))
  rates <- rates[seq_len(n)]
  bad <- !is.finite(rates) | rates <= -0.999
  rates[bad] <- NA_real_
  out <- cf / cumprod(1 + rates)
  attr(out, "invalid_rate") <- bad
  out
}

#' Gordon terminal value at year n and its t=0 present value (kept off the yearly PV series).
dcf_gordon_tv_pv <- function(last_cf, g, r, discount_factor_n) {
  last_cf <- suppressWarnings(as.numeric(last_cf)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  r <- suppressWarnings(as.numeric(r)[1])
  df_n <- suppressWarnings(as.numeric(discount_factor_n)[1])
  empty <- list(tv = NA_real_, pv_tv = NA_real_)
  if (!is.finite(last_cf) || !is.finite(g) || !is.finite(r) || !is.finite(df_n)) return(empty)
  if (r <= g || abs(df_n) < 1e-15) return(empty)
  tv <- last_cf * (1 + g) / (r - g)
  list(tv = tv, pv_tv = tv / df_n)
}

#' CAPM cost of equity in percent: Ke = Rf + β(Rm − Rf).
.capm_ke_pct <- function(rf_pct, beta, rm_pct) {
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  beta <- suppressWarnings(as.numeric(beta)[1])
  rm_pct <- suppressWarnings(as.numeric(rm_pct)[1])
  if (!is.finite(rf_pct) || !is.finite(beta) || !is.finite(rm_pct)) return(NA_real_)
  rf_pct + beta * (rm_pct - rf_pct)
}

#' Elasticity rows for CAPM inputs that feed Ke (DDM / RI). `p_at_ke_pct(ke_pct)` returns value.
.param_sensitivity_capm_ke_rows <- function(v0, p_at_ke_pct, rf0, beta0, rm0,
                                            shock = PARAM_SENSITIVITY_SHOCK) {
  rows <- list()
  .rel <- function(x, sign = -1) .param_rel_shock(x, sign = sign, shock = shock)
  .ke <- function(rf = rf0, beta = beta0, rm = rm0) .capm_ke_pct(rf, beta, rm)
  if (!is.finite(.ke()) || !is.finite(v0)) return(rows)
  if (.param_sensitivity_rel_ok(rf0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "無風險利率 Rf", rf0, "%", v0,
      p_at_ke_pct(.ke(rf = .rel(rf0, -1))),
      p_at_ke_pct(.ke(rf = .rel(rf0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  if (.param_sensitivity_rel_ok(beta0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "Beta (β)", beta0, "x", v0,
      p_at_ke_pct(.ke(beta = .rel(beta0, -1))),
      p_at_ke_pct(.ke(beta = .rel(beta0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  if (.param_sensitivity_rel_ok(rm0)) {
    rows[[length(rows) + 1]] <- .param_sensitivity_infl_row(
      "市場報酬率 Rm", rm0, "%", v0,
      p_at_ke_pct(.ke(rm = .rel(rm0, -1))),
      p_at_ke_pct(.ke(rm = .rel(rm0, +1))),
      "CAPM：Ke = Rf + β(Rm−Rf)"
    )
  }
  rows
}

#' Canonical Gordon DDM (StockFeel / CFA): V0 = D1 / (r − g), D1 = D0 × (1+g).
#' `r` is the equity required return (CAPM Ke). `ke` is accepted as an alias for `r`.
#' Independent of ticker price, shares, and dividend dollars. Rates are decimals.
.ddm_formula_p0 <- function(d0 = 1, g, r = NULL, ke = NULL) {
  d0 <- suppressWarnings(as.numeric(d0)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  r <- suppressWarnings(as.numeric(r %||% ke)[1])
  if (!is.finite(d0) || !is.finite(g) || !is.finite(r) || r <= g) return(NA_real_)
  d0 * (1 + g) / (r - g)
}

#' Sum of Perpetuities Method (SPM; Brown & Abraham 2012).
#' V0 = (E × g) / r² + D / r, with r = Ke (CAPM).
#' Separates capitalized sticky dividend from PV of earnings growth via retention.
#' Does not require r > g (unlike Gordon); only r > 0. Rates are decimals.
#' When ROE = r and g = ROE × retention, SPM equals forward-D Gordon V0 = D/(r−g).
.ddm_formula_spm <- function(eps, d, g, r = NULL, ke = NULL) {
  eps <- suppressWarnings(as.numeric(eps)[1])
  d <- suppressWarnings(as.numeric(d)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  r <- suppressWarnings(as.numeric(r %||% ke)[1])
  if (!is.finite(eps) || !is.finite(d) || !is.finite(g) || !is.finite(r)) return(NA_real_)
  if (r <= 0) return(NA_real_)
  if (d < 0) d <- 0
  (eps * g) / (r * r) + d / r
}

#' Two-stage DDM: high-growth g1 for n years, then Gordon at g2.
#' D_t = D0(1+g1)^t for t=1..n; Pn = D_n(1+g2)/(r−g2); V0 = Σ Dt/(1+r)^t + Pn/(1+r)^n.
#' `r` is the equity required return (CAPM Ke); `ke` is an alias.
.ddm_formula_two_stage <- function(d0 = 1, g1, n, g2, r = NULL, ke = NULL) {
  d0 <- suppressWarnings(as.numeric(d0)[1])
  g1 <- suppressWarnings(as.numeric(g1)[1])
  g2 <- suppressWarnings(as.numeric(g2)[1])
  r <- suppressWarnings(as.numeric(r %||% ke)[1])
  n <- suppressWarnings(as.integer(round(as.numeric(n)[1])))
  if (!is.finite(d0) || !is.finite(g1) || !is.finite(g2) || !is.finite(r)) return(NA_real_)
  if (!is.finite(n) || n < 1L) return(.ddm_formula_p0(d0 = d0, g = g2, r = r))
  if (r <= g2) return(NA_real_)
  dts <- d0 * (1 + g1)^seq_len(n)
  dfs <- (1 + r)^seq_len(n)
  pv_div <- sum(dts / dfs)
  pn <- dts[n] * (1 + g2) / (r - g2)
  pv_div + pn / dfs[n]
}

#' True when DCF cash-flow claim is FCFE (Ke, equity) rather than FCFF (WACC, EV).
dcf_claim_is_fcfe <- function(claim) {
  identical(as.character(claim %||% "fcff")[1], "fcfe")
}

#' Short cash-flow tag for DCF page labels: "FCFF" or "FCFE".
dcf_cf_tag <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "FCFE" else "FCFF"
}

#' Discount-rate tag for DCF page labels: "WACC" or "Ke".
dcf_disc_tag <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "Ke" else "WACC"
}

#' Full Chinese cash-flow label on the DCF page.
dcf_cf_full_zh <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "股權自由現金流 (FCFE)" else "企業自由現金流 (FCFF)"
}

#' Projection-tab formula banner (FCFF identity, or FCFE conversion + FCFF identity).
dcf_formula_banner_txt <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) {
    "FCFE = FCFF − Interest×(1−T) + Net Borrowing　｜　FCFF = CFO + Interest×(1−T) − CapEx ＝ NOPAT + D&A − ΔNWC − CapEx"
  } else {
    "FCFF = CFO + Interest×(1−T) − CapEx  ＝  NOPAT + D&A − ΔNWC − CapEx"
  }
}

dcf_hist_cf_label <- function(claim) {
  if (dcf_claim_is_fcfe(claim)) "歷史現金流 (FCF)" else "歷史現金流 (FCFF)"
}

dcf_fcst_cf_label <- function(claim) {
  sprintf("預測現金流 (%s)", dcf_cf_tag(claim))
}

dcf_yearly_pv_label <- function(claim) {
  sprintf("各年折現現金流 (PV／%s)", dcf_disc_tag(claim))
}

#' Reconstruct unlevered FCFF from statements (CFA / Damodaran CFO identity).
#' Yahoo "Free Cash Flow" is typically CFO − |CapEx| (after interest) ≈ levered FCF.
#' FCFF_t = CFO_t + Interest_t × (1−T) − |CapEx_t|.
#' Fallback: Yahoo FCF + after-tax interest when CFO or CapEx is missing.
reconstruct_hist_fcff <- function(d_cf, d_is = NULL, tax = NULL) {
  empty <- list(
    fcff = numeric(0), source = "none",
    cfo = numeric(0), capex = numeric(0), interest = numeric(0),
    iat = numeric(0), yahoo_fcf = numeric(0), tax = NA_real_
  )
  if (is.null(d_cf) || !is.data.frame(d_cf) || nrow(d_cf) < 1L) return(empty)
  t <- if (!is.null(tax) && is.finite(suppressWarnings(as.numeric(tax)[1]))) {
    .norm_tax_ratio(tax)
  } else {
    # Prefer IS effective tax; else market statutory (TW 20% / US 21%), not a hard-coded US T.
    effective_tax_ratio_from_is(d_is, fallback = statutory_tax_ratio())
  }
  cfo <- tryCatch(
    cashflow_total_row(d_cf, "ocf", include_ttm = FALSE),
    error = function(e) NA_real_
  )
  cap <- tryCatch(
    select_clean_metric_row_any(d_cf, CAPEX_PATTERNS, include_ttm = FALSE),
    error = function(e) NA_real_
  )
  yfcf <- tryCatch(
    select_clean_metric_row(d_cf, "^Free Cash Flow$", include_ttm = FALSE),
    error = function(e) NA_real_
  )
  if (length(yfcf) < 1L || all(is.na(yfcf))) {
    yfcf <- tryCatch(
      select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE),
      error = function(e) NA_real_
    )
  }
  inte <- tryCatch(
    select_clean_metric_row_any(d_is, INTEREST_EXPENSE_PATTERNS, include_ttm = FALSE),
    error = function(e) NA_real_
  )
  if (length(inte) < 1L || all(is.na(inte))) {
    inte <- tryCatch(
      select_clean_metric_row_any(d_cf, INTEREST_PAID_PATTERNS, include_ttm = FALSE),
      error = function(e) NA_real_
    )
  }
  n <- max(length(cfo), length(cap), length(yfcf), length(inte), 0L)
  if (n < 1L) return(empty)
  pad <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!length(x)) x <- NA_real_
    if (length(x) < n) x <- c(x, rep(NA_real_, n - length(x)))
    x[seq_len(n)]
  }
  cfo <- pad(cfo)
  cap <- pad(cap)
  yfcf <- pad(yfcf)
  inte <- pad(inte)
  cap_out <- ifelse(is.finite(cap), abs(cap), NA_real_)
  int_abs <- ifelse(is.finite(inte), abs(inte), 0)
  iat <- int_abs * (1 - t)
  fcff <- cfo + iat - cap_out
  src <- "cfo_identity"
  miss <- !is.finite(fcff)
  fb <- yfcf + iat
  use_fb <- miss & is.finite(fb)
  fcff[use_fb] <- fb[use_fb]
  cfo_ok <- any(is.finite(cfo) & is.finite(cap_out))
  if (!isTRUE(cfo_ok) && any(is.finite(fb))) {
    src <- if (any(is.finite(iat) & iat > 0)) "yahoo_fcf_plus_iat" else "yahoo_fcf"
  } else if (any(use_fb)) {
    src <- "cfo_identity_with_yahoo_fallback"
  }
  list(
    fcff = fcff, source = src, cfo = cfo, capex = cap_out,
    interest = int_abs, iat = iat, yahoo_fcf = yfcf, tax = t
  )
}

latest_hist_fcff <- function(d_cf, d_is = NULL, tax = NULL) {
  rec <- reconstruct_hist_fcff(d_cf, d_is = d_is, tax = tax)
  hit <- which(is.finite(rec$fcff))
  if (!length(hit)) return(NA_real_)
  rec$fcff[hit[1]]
}

hist_fcff_period_df <- function(d_cf, d_is = NULL, tax = NULL) {
  rec <- reconstruct_hist_fcff(d_cf, d_is = d_is, tax = tax)
  if (is.null(d_cf) || !is.data.frame(d_cf) || ncol(d_cf) < 2L) return(NULL)
  period_cols <- colnames(d_cf)[-1]
  period_cols <- period_cols[!grepl("^ttm$", period_cols, ignore.case = TRUE)]
  n <- min(length(rec$fcff), length(period_cols))
  if (n < 1L) return(NULL)
  data.frame(
    Period = as.character(period_cols[seq_len(n)]),
    Value = as.numeric(rec$fcff[seq_len(n)]),
    stringsAsFactors = FALSE
  )
}

#' Gordon TV; NA when r ≤ g or last CF is negative (do not perpetuate a cash drain).
dcf_gordon_tv <- function(last_fcf, g, r) {
  last_fcf <- suppressWarnings(as.numeric(last_fcf)[1])
  g <- suppressWarnings(as.numeric(g)[1])
  r <- suppressWarnings(as.numeric(r)[1])
  if (!is.finite(last_fcf) || !is.finite(g) || !is.finite(r) || r <= g) return(NA_real_)
  if (last_fcf < 0) return(NA_real_)
  last_fcf * (1 + g) / (r - g)
}

#' EV → equity bridge: Equity = EV + Cash − Debt.
dcf_ev_to_equity <- function(ev, cash = 0, debt = 0) {
  ev <- suppressWarnings(as.numeric(ev)[1])
  cash <- suppressWarnings(as.numeric(cash)[1])
  debt <- suppressWarnings(as.numeric(debt)[1])
  if (!is.finite(ev)) return(NA_real_)
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  ev + cash - debt
}

#' Convert FCFF path to FCFE with conversion components.
#' FCFE_t = FCFF_t − Interest(1−T) + Net borrowing_t.
#' Net borrowing grows starting debt with g_path (constant-leverage approximation).
fcff_to_fcfe_components <- function(fcff, interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff <- suppressWarnings(as.numeric(fcff))
  n <- length(fcff)
  empty <- data.frame(
    FCFF = numeric(0), InterestAfterTax = numeric(0),
    NetBorrowing = numeric(0), BeginningDebt = numeric(0), FCFE = numeric(0),
    stringsAsFactors = FALSE
  )
  if (n < 1L) return(empty)
  iat <- suppressWarnings(as.numeric(interest_after_tax)[1])
  if (!is.finite(iat)) iat <- 0
  debt <- suppressWarnings(as.numeric(debt0)[1])
  if (!is.finite(debt) || debt < 0) debt <- 0
  gp <- suppressWarnings(as.numeric(g_path))
  if (length(gp) == 1L) gp <- rep(gp, n)
  if (length(gp) < n) gp <- c(gp, rep(tail(gp, 1), n - length(gp)))
  gp[!is.finite(gp)] <- 0
  nb <- numeric(n)
  beg <- numeric(n)
  out <- numeric(n)
  for (i in seq_len(n)) {
    beg[i] <- debt
    nb[i] <- gp[i] * debt
    cf <- fcff[i]
    out[i] <- if (is.finite(cf)) cf - iat + nb[i] else NA_real_
    debt <- debt + nb[i]
  }
  data.frame(
    FCFF = fcff,
    InterestAfterTax = rep(iat, n),
    NetBorrowing = nb,
    BeginningDebt = beg,
    FCFE = out,
    stringsAsFactors = FALSE
  )
}

#' Convert FCFF path to FCFE: FCFE_t = FCFF_t − Interest(1−T) + Net borrowing_t.
fcff_to_fcfe <- function(fcff, interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff_to_fcfe_components(
    fcff, interest_after_tax = interest_after_tax, debt0 = debt0, g_path = g_path
  )$FCFE
}

#' Historical D/(D+E) from a balance sheet (TTM 欄略過).
.dcf_claim_debt_ratios <- function(d_bs) {
  if (is.null(d_bs) || !is.data.frame(d_bs) || nrow(d_bs) < 1L) {
    return(numeric(0))
  }
  debt <- tryCatch(
    select_clean_metric_row(d_bs, "^Total Debt$", include_ttm = FALSE),
    error = function(e) NULL
  )
  eq <- tryCatch(
    select_clean_metric_row_any(d_bs, EQUITY_PATTERNS, include_ttm = FALSE),
    error = function(e) NULL
  )
  debt <- suppressWarnings(as.numeric(debt))
  eq <- suppressWarnings(as.numeric(eq))
  n <- min(length(debt), length(eq))
  if (n < 1L) return(numeric(0))
  debt <- debt[seq_len(n)]
  eq <- eq[seq_len(n)]
  den <- debt + eq
  out <- debt / den
  out[!(is.finite(debt) & is.finite(eq) & is.finite(den) & den > 0)] <- NA_real_
  out[is.finite(out)]
}

#' Damodaran／CFA 對齊：建議 FCFF 或 FCFE（不改預設；僅提示）.
#' 偏好 FCFF：槓桿高或變動大、FCFE&lt;0 且 FCFF&gt;0。
#' 負債比穩定且 FCFE&gt;0：FCFE 適用（仍不自動切換）.
recommend_dcf_claim <- function(d_bs = NULL,
                                fcff = NA_real_,
                                fcfe = NA_real_,
                                high_lev = 0.50,
                                lev_range_unstable = 0.15,
                                lev_range_stable = 0.10) {
  fcff <- suppressWarnings(as.numeric(fcff)[1])
  fcfe <- suppressWarnings(as.numeric(fcfe)[1])
  dr <- .dcf_claim_debt_ratios(d_bs)
  lev_now <- if (length(dr)) dr[1] else NA_real_
  lev_span <- if (length(dr) >= 2L) diff(range(dr)) else NA_real_
  reasons <- character(0)
  prefer <- "fcff"
  fcfe_ok <- FALSE

  if (is.finite(fcff) && fcff > 0 && is.finite(fcfe) && fcfe < 0) {
    prefer <- "fcff"
    reasons <- c(reasons, "FCFE 為負而 FCFF 為正（高槓桿／償債後現金不足時不宜硬用 FCFE）")
  }
  if (is.finite(lev_now) && lev_now >= high_lev) {
    prefer <- "fcff"
    reasons <- c(reasons, sprintf("目前負債比偏高（D/(D+E)≈%.0f%%）", 100 * lev_now))
  }
  if (is.finite(lev_span) && lev_span >= lev_range_unstable) {
    prefer <- "fcff"
    reasons <- c(reasons, sprintf("負債比變動大（全距約 %.0f 個百分點），不必逐年預測淨舉債", 100 * lev_span))
  }
  if (identical(prefer, "fcff") && !length(reasons)) {
    reasons <- c("實務與 CFA 調查以 FCFF／WACC 為預設；與折扣率配對即可")
  }
  if (is.finite(lev_span) && lev_span < lev_range_stable && length(dr) >= 2L &&
      is.finite(fcfe) && fcfe > 0 && !(is.finite(lev_now) && lev_now >= high_lev)) {
    fcfe_ok <- TRUE
    reasons <- c(reasons, "負債比相對穩定且 FCFE 為正，固定槓桿近似下可用 FCFE／Ke")
  }
  list(
    prefer = prefer,
    fcfe_ok = isTRUE(fcfe_ok),
    lev_now = lev_now,
    lev_span = lev_span,
    fcff = fcff,
    fcfe = fcfe,
    reasons = unique(reasons)
  )
}

#' Holding / conglomerate NAV from a balance sheet (book SOTP).
#' NAV = Equity − holdco_discount × identified investments.
#' If no investment lines, NAV = common equity (same as book).
extract_nav_components <- function(df_bs, holdco_discount = 0) {
  empty <- list(
    cash = NA_real_, investments = NA_real_, equity = NA_real_,
    nav = NA_real_, discount = 0, note = "無資產負債表"
  )
  if (is.null(df_bs) || !is.data.frame(df_bs) || nrow(df_bs) == 0) return(empty)
  disc <- suppressWarnings(as.numeric(holdco_discount)[1])
  if (!is.finite(disc)) disc <- 0
  disc <- max(0, min(0.5, disc))
  cash <- tryCatch(
    select_current_metric_any(
      df_bs,
      c(
        "Cash Cash Equivalents And Short Term Investments",
        "Cash And Cash Equivalents",
        "^Total Cash$"
      ),
      "stock"
    ),
    error = function(e) NA_real_
  )
  inv <- tryCatch(
    select_current_metric_any(
      df_bs,
      c(
        "Investmentin Financial Assets",
        "Investments And Advances",
        "Long Term Investments",
        "Available For Sale Securities",
        "Equity Method Investments"
      ),
      "stock"
    ),
    error = function(e) NA_real_
  )
  equity <- tryCatch(
    select_current_metric_any(df_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  if (!is.finite(inv) || inv < 0) inv <- 0
  if (!is.finite(cash)) cash <- NA_real_
  if (!is.finite(equity) || equity <= 0) {
    return(list(
      cash = cash, investments = inv, equity = equity,
      nav = NA_real_, discount = disc,
      note = "無法取得股東權益，NAV 未計算"
    ))
  }
  nav <- equity - disc * inv
  note <- if (inv > 0 && disc > 0) {
    sprintf("NAV＝權益 − %.0f%%×投資科目（控股折價）", disc * 100)
  } else if (inv > 0) {
    "已辨識投資科目；折價=0 時 NAV＝帳面權益"
  } else {
    "無獨立投資科目，NAV＝帳面淨值"
  }
  list(cash = cash, investments = inv, equity = equity, nav = nav, discount = disc, note = note)
}

#' Canonical P/B fair price from formula parameters only (unit book).
#' P = basis × target P/B. Independent of ticker market price and share count.
.pb_formula_p <- function(basis = 1, pb) {
  basis <- suppressWarnings(as.numeric(basis)[1])
  pb <- suppressWarnings(as.numeric(pb)[1])
  if (!is.finite(basis) || basis <= 0 || !is.finite(pb) || pb <= 0) return(NA_real_)
  basis * pb
}

#' Sort elasticity table rows by |ε| descending.
.param_sensitivity_sort_by_abs_eps <- function(out) {
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(out)
  if (!("｜ε｜" %in% names(out))) return(out)
  infl_num <- suppressWarnings(as.numeric(out$`｜ε｜`))
  out <- out[order(-infl_num, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# 從預測表統一取出 FCFF 序列（相容舊欄位名 FCF）
extract_fcff_series <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(numeric(0))
  if ("FCFF" %in% colnames(df)) return(as.numeric(df$FCFF))
  if ("FCF" %in% colnames(df)) return(as.numeric(df$FCF))
  rep(NA_real_, nrow(df))
}

# 依現金流定義取出展示／折現序列：FCFF 原樣；FCFE 走既有 fcff_to_fcfe
extract_dcf_claim_series <- function(df, claim = "fcff",
                                     interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff <- extract_fcff_series(df)
  if (!dcf_claim_is_fcfe(claim)) return(fcff)
  fcff_to_fcfe(
    fcff, interest_after_tax = interest_after_tax, debt0 = debt0, g_path = g_path
  )
}

# 依 DCF 模式決定各預測年的營收成長率 (%)
# 0% is a real input. NA is not replaced with another stage's growth.
revenue_growth_pct_for_year <- function(year_idx, mode, g_est, g_stage1, g_stage2, yr_stage1) {
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) NA_real_ else x
  }
  g_est <- num(g_est)
  g_stage1 <- num(g_stage1)
  g_stage2 <- num(g_stage2)
  yr_stage1 <- suppressWarnings(as.integer(round(as.numeric(yr_stage1)[1])))
  if (identical(mode, "two_stage")) {
    if (!is.finite(yr_stage1) || yr_stage1 < 1L) return(NA_real_)
    if (year_idx <= yr_stage1) return(g_stage1)
    return(g_stage2)
  }
  if (is.finite(g_est)) return(g_est)
  NA_real_
}

# Stage 1 years must already satisfy 0 < yr_stage1 < n.
# Out-of-range input is NA. default_yr is retained for call compatibility and is not applied.
clamp_yr_stage1 <- function(n_years, yr_stage1, default_yr = 3L) {
  n_years <- suppressWarnings(as.integer(round(as.numeric(n_years)[1])))
  yr <- suppressWarnings(as.integer(round(as.numeric(yr_stage1)[1])))
  valid <- is.finite(n_years) && n_years > 1L && is.finite(yr) && yr > 0L && yr < n_years
  if (!valid) {
    out <- NA_integer_
    attr(out, "ynow_adjusted") <- FALSE
    attr(out, "ignored_default") <- default_yr
    return(out)
  }
  yr
}


# ==========================================
# 永續成長率 g 估計方法（DDM / DCF / RI 共用）
# ==========================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

# Lifecycle Classification lives in lifecycle_classification.R (single source).
# Old mix-in tiers (mature_tech / mature_sunset / mature_general) and
# stage-fixed terminal g tables are migration-only; they are not scoring keys.
.ynow_lifecycle_src <- local({
  frames <- sys.nframe()
  if (frames >= 1L) {
    for (i in rev(seq_len(frames))) {
      o <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE), error = function(e) NULL)
      if (is.character(o) && length(o) == 1L && grepl("setup\\.R$", o)) {
        return(file.path(dirname(normalizePath(o, winslash = "/", mustWork = FALSE)),
                         "lifecycle_classification.R"))
      }
    }
  }
  file.path(getwd(), "lifecycle_classification.R")
})
if (!file.exists(.ynow_lifecycle_src)) {
  stop("lifecycle_classification.R not found next to setup.R: ", .ynow_lifecycle_src)
}
source(.ynow_lifecycle_src, local = TRUE, encoding = "UTF-8")

#' 基本面永續 g（%）= Retention Ratio × ROE
calc_fundamental_sgr_pct <- function(d_is, d_bs, d_cf) {
  if (is.null(d_is) || is.null(d_bs) || !is.data.frame(d_is) || !is.data.frame(d_bs)) {
    return(NA_real_)
  }
  ni <- tryCatch(
    select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"),
    error = function(e) NA_real_
  )
  equity <- tryCatch(
    select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  if (is.na(ni) || is.na(equity) || equity <= 0) return(NA_real_)

  roe <- ni / equity
  div_paid <- tryCatch({
    if (is.null(d_cf) || !is.data.frame(d_cf)) NA_real_ else {
      abs(select_current_metric(d_cf, "Cash Dividends Paid", "flow"))
    }
  }, error = function(e) NA_real_)

  payout_ratio <- 0
  if (!is.na(ni) && ni > 0 && !is.na(div_paid)) {
    payout_ratio <- min(max(div_paid / ni, 0), 1)
  }
  retention <- 1 - payout_ratio
  round(roe * retention * 100, 2)
}

#' Verifiable Fundamental／SGR identity: g = Retention × ROE.
#' `matches_formula` is TRUE when the displayed components rebuild the engine g.
fundamental_sgr_identity <- function(ni, equity, dividends) {
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) NA_real_ else x
  }
  ni <- num(ni)
  equity <- num(equity)
  div_raw <- num(dividends)
  div_abs <- if (is.finite(div_raw)) abs(div_raw) else NA_real_
  roe <- if (is.finite(ni) && is.finite(equity) && equity > 0) ni / equity else NA_real_
  payout <- 0
  if (is.finite(ni) && ni > 0 && is.finite(div_abs)) {
    payout <- min(max(div_abs / ni, 0), 1)
  } else if (!is.finite(ni) || !is.finite(equity) || equity <= 0) {
    payout <- NA_real_
  }
  retention <- if (is.finite(payout)) 1 - payout else NA_real_
  g_hand <- if (is.finite(roe) && is.finite(retention)) round(roe * retention * 100, 2) else NA_real_
  d_is <- data.frame(Breakdown = "Net Income", Y2025 = ni, stringsAsFactors = FALSE)
  d_bs <- data.frame(Breakdown = "Stockholders Equity", Y2025 = equity, stringsAsFactors = FALSE)
  d_cf <- data.frame(Breakdown = "Cash Dividends Paid", Y2025 = div_raw, stringsAsFactors = FALSE)
  g_engine <- tryCatch(
    calc_fundamental_sgr_pct(d_is, d_bs, d_cf),
    error = function(e) NA_real_
  )
  list(
    ni = ni,
    equity = equity,
    dividends = div_abs,
    roe_pct = if (is.finite(roe)) round(roe * 100, 2) else NA_real_,
    payout = payout,
    retention = retention,
    g_pct = g_engine,
    g_from_components = g_hand,
    matches_formula = is.finite(g_engine) && is.finite(g_hand) && abs(g_engine - g_hand) < 0.02,
    ok = is.finite(g_engine)
  )
}

#' Gordon terminal gate: discount rate must be strictly above terminal g.
terminal_gordon_gate <- function(g_pct, discount_pct) {
  g <- suppressWarnings(as.numeric(g_pct)[1])
  r <- suppressWarnings(as.numeric(discount_pct)[1])
  spread <- if (is.finite(g) && is.finite(r)) r - g else NA_real_
  list(
    g_pct = g,
    discount_pct = r,
    spread_pp = spread,
    computable = is.finite(spread) && spread > 0,
    rule = "discount_pct > terminal_g_pct"
  )
}

#' Documented Bear / Base / Bull shocks for every valuation engine.
#' DCF / DDM / RI shocks are the same numbers `scenario_stress_factors` reprices with.
valuation_scenario_audit <- function(locale = "en") {
  en <- identical(tolower(as.character(locale %||% "")[1]), "en") ||
    identical(as.character(locale %||% "")[1], "en-US")
  rows <- list()
  add <- function(model, case, parameter, shock, rule) {
    rows[[length(rows) + 1L]] <<- data.frame(
      model = model, case = case, parameter = parameter,
      shock = shock, rule = rule, stringsAsFactors = FALSE
    )
  }
  rule_reprice <- if (en) {
    "Reprice with this shock. Bear fair value must not exceed Base."
  } else {
    "用此衝擊重算。Bear 合理價不得高於 Base。"
  }
  for (m in c("dcf", "ddm", "ri")) {
    sf <- scenario_stress_factors(m)
    for (case in c("bear", "base", "bull")) {
      sh <- sf[[case]]
      for (nm in names(sh)) {
        add(m, case, nm, as.numeric(sh[[nm]])[1], rule_reprice)
      }
    }
  }
  band_rule <- if (en) {
    "Bear / Base / Bull are the low / mid / high of that model's own band, not a discount-rate shock."
  } else {
    "Bear／Base／Bull 是該模型自己的低／中／高區間，不是折現率衝擊。"
  }
  add("pb", "bear", "band_low", NA_real_, band_rule)
  add("pb", "base", "band_mid", NA_real_, band_rule)
  add("pb", "bull", "band_high", NA_real_, band_rule)
  add("nav", "bear", "band_low", NA_real_, band_rule)
  add("nav", "base", "band_mid", NA_real_, band_rule)
  add("nav", "bull", "band_high", NA_real_, band_rule)
  ref_rule <- if (en) {
    "Not a Fair Value Bear/Base/Bull band. Verify Implied Price from the printed multiple, metric, and share count."
  } else {
    "不是 Fair Value 的 Bear／Base／Bull。用畫面上的倍數、指標與股數複核 Implied Price。"
  }
  add("multiples", "reference", "active_multiple", NA_real_, ref_rule)
  sotp_rule <- if (en) {
    "Meta-Framework. Verify Implied Price as Σ(segment revenue × EV/Sales) then the equity bridge. Conglomerate discount is not auto-applied."
  } else {
    "綜合型估值框架。用各事業營收 × EV/Sales 加總，再經權益橋接，複核 Implied Price。控股折價不自動套用。"
  }
  add("sotp", "reference", "segment_ev_sales", NA_real_, sotp_rule)
  do.call(rbind, rows)
}

#' Implied Price = multiple × metric / shares. Linear, so a 1% multiple move is a 1% price move.
multiple_implied_price <- function(multiple, metric, shares) {
  m <- suppressWarnings(as.numeric(multiple)[1])
  x <- suppressWarnings(as.numeric(metric)[1])
  s <- suppressWarnings(as.numeric(shares)[1])
  px <- if (is.finite(m) && is.finite(x) && is.finite(s) && s > 0) m * x / s else NA_real_
  list(
    price = px,
    identity = "multiple * metric / shares",
    one_pct_multiple_is_one_pct_price = is.finite(px),
    ok = is.finite(px)
  )
}

#' Enterprise multiple: (metric × multiple + cash − debt) / shares.
enterprise_implied_price <- function(metric, multiple, cash, debt, shares) {
  m <- suppressWarnings(as.numeric(metric)[1])
  mult <- suppressWarnings(as.numeric(multiple)[1])
  s <- suppressWarnings(as.numeric(shares)[1])
  ev <- if (is.finite(m) && is.finite(mult)) m * mult else NA_real_
  eq <- dcf_ev_to_equity(ev, cash = cash, debt = debt)
  px <- if (is.finite(eq) && is.finite(s) && s > 0) eq / s else NA_real_
  list(
    implied_ev = ev,
    equity = eq,
    price = px,
    identity = "(metric * multiple + cash - debt) / shares",
    ok = is.finite(px)
  )
}

#' SOTP equity bridge: (sum of segment values + non-operating + cash − debt) / shares.
sotp_implied_price <- function(segment_value, cash, debt, shares, non_operating = 0) {
  parts <- sum(suppressWarnings(as.numeric(segment_value)), na.rm = TRUE)
  if (!is.finite(parts)) parts <- NA_real_
  non_op <- suppressWarnings(as.numeric(non_operating)[1])
  if (!is.finite(non_op)) non_op <- 0
  ev <- if (is.finite(parts)) parts + non_op else NA_real_
  eq <- dcf_ev_to_equity(ev, cash = cash, debt = debt)
  s <- suppressWarnings(as.numeric(shares)[1])
  px <- if (is.finite(eq) && is.finite(s) && s > 0) eq / s else NA_real_
  list(
    enterprise_value = ev,
    equity = eq,
    price = px,
    identity = "(sum(segment value) + non_operating + cash - debt) / shares",
    ok = is.finite(px)
  )
}

#' Present value of terminal value / (explicit PV + terminal PV).
#' `heavy` uses the same 80% soft warning as valuation_guard.
tv_ev_share <- function(explicit_pv, pv_tv, warn = 0.80) {
  ex <- suppressWarnings(as.numeric(explicit_pv)[1])
  tv <- suppressWarnings(as.numeric(pv_tv)[1])
  ev <- if (is.finite(ex) && is.finite(tv)) ex + tv else NA_real_
  share <- if (is.finite(ev) && abs(ev) > 1e-8) tv / ev else NA_real_
  list(
    enterprise_value = ev,
    tv_share = share,
    explicit_share = if (is.finite(share)) 1 - share else NA_real_,
    heavy = is.finite(share) && share > warn
  )
}

#' Linear NOPAT margin from the first forecast year to the last.
#' A missing terminal margin keeps the starting margin flat.
nopat_margin_path <- function(start_margin, terminal_margin, n) {
  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n < 1L) return(numeric(0))
  start <- suppressWarnings(as.numeric(start_margin)[1])
  term <- suppressWarnings(as.numeric(terminal_margin)[1])
  if (!is.finite(start)) return(rep(NA_real_, n))
  if (!is.finite(term) || n == 1L) return(rep(start, n))
  seq(start, term, length.out = n)
}

#' Reverse DCF: terminal g implied by the market price, holding explicit-period
#' cash flows fixed. `equity_claim=TRUE` means the DCF value is already equity
#' (FCFE); otherwise price × shares = EV + cash − debt.
dcf_implied_terminal_g <- function(pv_explicit, last_fcf, discount, df_terminal,
                                   price, shares, cash = 0, debt = 0,
                                   equity_claim = FALSE) {
  pv <- suppressWarnings(as.numeric(pv_explicit)[1])
  last <- suppressWarnings(as.numeric(last_fcf)[1])
  r <- suppressWarnings(as.numeric(discount)[1])
  df <- suppressWarnings(as.numeric(df_terminal)[1])
  px <- suppressWarnings(as.numeric(price)[1])
  sh <- suppressWarnings(as.numeric(shares)[1])
  cash <- suppressWarnings(as.numeric(cash)[1])
  debt <- suppressWarnings(as.numeric(debt)[1])
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  na <- list(g = NA_real_, pv_tv = NA_real_, ok = FALSE, reason = "missing")
  if (!is.finite(pv) || !is.finite(last) || last <= 0 || !is.finite(r) || r <= 0 ||
      !is.finite(df) || df <= 0 || !is.finite(px) || !is.finite(sh) || sh <= 0) {
    return(na)
  }
  equity <- px * sh
  ev <- if (isTRUE(equity_claim)) equity else equity - cash + debt
  pv_tv <- ev - pv
  if (!is.finite(pv_tv) || pv_tv <= 0) {
    return(list(g = NA_real_, pv_tv = pv_tv, ok = FALSE, reason = "price_below_explicit"))
  }
  k <- pv_tv * df / last
  if (!is.finite(k) || k <= -1) {
    return(list(g = NA_real_, pv_tv = pv_tv, ok = FALSE, reason = "no_gordon_root"))
  }
  g <- (k * r - 1) / (1 + k)
  if (!is.finite(g) || g >= r) {
    return(list(g = NA_real_, pv_tv = pv_tv, ok = FALSE, reason = "g_not_below_discount"))
  }
  list(g = g, pv_tv = pv_tv, ok = TRUE, reason = "terminal_g")
}

#' One-stage Gordon implied g: V = FCF1 / (r − g). Not the app's multi-year DCF.
single_stage_implied_g <- function(fcf1, value, discount) {
  f <- suppressWarnings(as.numeric(fcf1)[1])
  v <- suppressWarnings(as.numeric(value)[1])
  r <- suppressWarnings(as.numeric(discount)[1])
  if (!is.finite(f) || f <= 0 || !is.finite(v) || v <= 0 || !is.finite(r)) {
    return(list(g = NA_real_, ok = FALSE))
  }
  g <- r - f / v
  list(g = g, ok = is.finite(g) && g < r)
}

#' Gap between a DCF per-share value and a DDM per-share value.
dcf_ddm_crosscheck <- function(dcf_px, ddm_px) {
  a <- suppressWarnings(as.numeric(dcf_px)[1])
  b <- suppressWarnings(as.numeric(ddm_px)[1])
  gap <- if (is.finite(a) && is.finite(b) && abs(b) > 1e-8) (a - b) / b else NA_real_
  list(
    dcf = a, ddm = b, gap = gap,
    wide = is.finite(gap) && abs(gap) > 0.25,
    ok = is.finite(a) && is.finite(b)
  )
}

#' 建議永續成長率估計法（不改寫演算法數值；僅推薦 macro／fundamental／lifecycle）
#' Lifecycle stage is one input. It never writes a fixed terminal g.
#' @return list(method, label, reason, fund_sgr_pct, auto_lifecycle, lifecycle_evidence, lifecycle_rule, lifecycle_result)
recommend_perpetual_g_method <- function(rf_pct = NA_real_,
                                         d_is = NULL,
                                         d_bs = NULL,
                                         d_cf = NULL,
                                         industry_text = "",
                                         ticker = "",
                                         wacc_pct = NA_real_,
                                         rev_cagr = NA_real_,
                                         locale = "zh-TW",
                                         industry_key = "",
                                         lifecycle_result = NULL,
                                         selected_stage = "auto") {
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  wacc_pct <- suppressWarnings(as.numeric(wacc_pct)[1])
  en <- identical(tolower(as.character(locale %||% "")[1]), "en") ||
    identical(as.character(locale %||% "")[1], "en-US")
  if (is.na(rev_cagr) || !is.finite(rev_cagr)) {
    rev_cagr <- tryCatch({
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    }, error = function(e) NA_real_)
  }
  if (is.null(lifecycle_result)) {
    lifecycle_result <- classify_lifecycle_result(
      industry_text = industry_text,
      industry_key = industry_key,
      ticker = ticker,
      d_is = d_is,
      d_bs = d_bs,
      d_cf = d_cf,
      wacc_pct = wacc_pct,
      expected_rev_cagr_pct = rev_cagr,
      selected_stage = selected_stage,
      locale = locale
    )
  }
  auto_stage <- as.character(lifecycle_result$autoDetectedStage %||% "")[1]
  evidence <- paste(lifecycle_result$classificationReasons %||% character(0), collapse = " ")
  fund_sgr <- calc_fundamental_sgr_pct(d_is, d_bs, d_cf)
  special <- as.character(lifecycle_result$specialBusinessType %||% "")[1]

  label_of <- function(m) {
    if (en) {
      switch(
        as.character(m)[1],
        "macro" = "Macroeconomic Anchoring (Macro)",
        "fundamental" = "Fundamental formula (SGR)",
        "lifecycle" = "Industry Lifecycle",
        as.character(m)[1]
      )
    } else {
      switch(
        as.character(m)[1],
        "macro" = "總體經濟錨定（Macro）",
        "fundamental" = "基本面公式（Fundamental／SGR）",
        "lifecycle" = "產業生命週期（Lifecycle）",
        as.character(m)[1]
      )
    }
  }

  pack <- function(method, reason) {
    list(
      method = method,
      label = label_of(method),
      reason = reason,
      fund_sgr_pct = if (is.finite(fund_sgr)) fund_sgr else NA_real_,
      auto_lifecycle = auto_stage,
      lifecycle_evidence = evidence,
      lifecycle_rule = lifecycle_result$fallbackSource %||% NA_character_,
      lifecycle_result = lifecycle_result
    )
  }

  if (!is.finite(fund_sgr)) {
    return(pack(
      "macro",
      if (en) {
        "Statements insufficient for Retention×ROE; suggest Macro (anchor to Rf)."
      } else {
        "財報不足以計算 Retention×ROE，建議改用 Macro（錨定 Rf）。"
      }
    ))
  }

  if (identical(special, "FINANCIAL_INSTITUTION") ||
      identical(auto_stage, "FINANCIAL_INSTITUTION")) {
    return(pack(
      "lifecycle",
      if (en) {
        paste0(
          "Financial institution: Retention×ROE is a weak terminal-g proxy because capital is an operating input. ",
          "Suggest Lifecycle as an economic-anchor method (not a fixed g). ",
          "Objective tier: ", evidence
        )
      } else {
        paste0(
          "金融機構：資本結構為營運要素，Retention×ROE 不適合作終值 g。",
          "建議以 Lifecycle 作經濟錨定（非固定 g）。客觀依據：", evidence
        )
      }
    ))
  }
  if (identical(special, "REGULATED_UTILITY") ||
      identical(auto_stage, "REGULATED_UTILITY")) {
    return(pack(
      "lifecycle",
      if (en) {
        paste0(
          "Regulated utility: allowed return and rate base dominate long-run growth. ",
          "Suggest Lifecycle as an economic-anchor method (not a fixed g). ",
          "Objective tier: ", evidence
        )
      } else {
        paste0(
          "受監管公用事業：長期成長受費率基礎與允許報酬約束。",
          "建議以 Lifecycle 作經濟錨定（非固定 g）。客觀依據：", evidence
        )
      }
    ))
  }
  if (identical(special, "DECLINING_OR_FINITE_LIFE") ||
      identical(auto_stage, "DECLINING_OR_FINITE_LIFE")) {
    return(pack(
      "lifecycle",
      if (en) {
        paste0(
          "Declining / finite-life profile: terminal g may be zero or negative. ",
          "Suggest Lifecycle (economic / fade path), not Retention×ROE. ",
          "Objective tier: ", evidence
        )
      } else {
        paste0(
          "衰退／有限年期：終值 g 可為零或負。建議 Lifecycle（經濟／消退路徑），",
          "勿用 Retention×ROE。客觀依據：", evidence
        )
      }
    ))
  }
  if (auto_stage %in% c("HIGH_GROWTH", "GROWTH_TO_MATURE")) {
    return(pack(
      "lifecycle",
      if (en) {
        paste0(
          "Auto-class ", auto_stage,
          if (is.finite(rev_cagr)) paste0(" (revenue CAGR≈", round(rev_cagr, 1), "%)") else "",
          "; near-term growth should fade toward an economic terminal g. ",
          "Suggest Lifecycle as the method (high growth does not raise terminal g). ",
          "Objective tier: ", evidence
        )
      } else {
        paste0(
          "自動分類 ", auto_stage,
          if (is.finite(rev_cagr)) paste0("（營收 CAGR≈", round(rev_cagr, 1), "%）") else "",
          "；近成長應向經濟終值收斂。建議 Lifecycle 作為方法",
          "（高成長不提高終值 g）。客觀依據：", evidence
        )
      }
    ))
  }

  if (is.finite(wacc_pct) && is.finite(fund_sgr) && fund_sgr >= (wacc_pct - 1)) {
    return(pack(
      "lifecycle",
      if (en) {
        paste0(
          "Fundamental／SGR≈", round(fund_sgr, 2), "% is near/above WACC (",
          round(wacc_pct, 2), "%); suggest Lifecycle or Macro — using Fundamental SGR as terminal g ",
          "can blow up TV. Auto tier=", auto_stage, ". Objective tier: ", evidence
        )
      } else {
        paste0(
          "Fundamental／SGR≈", round(fund_sgr, 2), "% 接近或高於 WACC（",
          round(wacc_pct, 2), "%）；建議改用 Lifecycle 或 Macro，Fundamental SGR 作為終值 g 易使 TV 失控。",
          "自動檔位=", auto_stage, "。客觀依據：", evidence
        )
      }
    ))
  }

  pack(
    "fundamental",
    if (en) {
      paste0(
        "Retention×ROE≈", round(fund_sgr, 2),
        "% is usable for a mature profile; keep Fundamental／SGR. ",
        "Objective tier: ", evidence
      )
    } else {
      paste0(
        "財報可算 Retention×ROE≈", round(fund_sgr, 2),
        "%，生命週期屬成熟檔；建議維持 Fundamental／SGR。",
        "客觀依據：", evidence
      )
    }
  )
}

#' Independent economic-anchor terminal g for the Lifecycle *method*.
#' Stage IDs never write a fixed percentage. High growth never raises g.
estimate_lifecycle_terminal_g <- function(rf_pct = NA_real_,
                                          wacc_pct = NA_real_,
                                          lifecycle_result = NULL,
                                          rf_source = NULL,
                                          locale = "zh-TW") {
  cfg <- if (exists("lifecycle_config", mode = "function")) lifecycle_config() else LIFECYCLE_CONFIG
  cap <- suppressWarnings(as.numeric(cfg$terminal$long_run_nominal_g_cap_pct)[1])
  if (!is.finite(cap)) cap <- 4.5
  decline_g <- suppressWarnings(as.numeric(cfg$terminal$declining_g_pct)[1])
  if (!is.finite(decline_g)) decline_g <- 0
  en <- identical(tolower(as.character(locale %||% "")[1]), "en") ||
    identical(as.character(locale %||% "")[1], "en-US")
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  wacc_pct <- suppressWarnings(as.numeric(wacc_pct)[1])
  src <- as.character(rf_source %||% "")[1]
  stage <- as.character(lifecycle_result$selectedStage %||% lifecycle_result$autoDetectedStage %||% "")[1]
  special <- as.character(lifecycle_result$specialBusinessType %||% "")[1]
  fallback <- "rf_economic_anchor"
  if (!is.finite(rf_pct)) {
    rf_pct <- 5
    if (!nzchar(src)) src <- "fallback"
    fallback <- "engineering_rf_fallback"
  }
  g_pct <- min(rf_pct, cap)
  if (identical(special, "DECLINING_OR_FINITE_LIFE") ||
      identical(stage, "DECLINING_OR_FINITE_LIFE")) {
    g_pct <- decline_g
    fallback <- "declining_or_finite_life_config"
  }
  # High growth / tech never lift terminal g above the economic cap.
  if (is.finite(wacc_pct) && is.finite(g_pct) && g_pct >= wacc_pct) {
    # Do not silently clamp here; validation_guard blocks Gordon when g >= WACC.
  }
  reason <- if (en) {
    paste0(
      "Lifecycle terminal g is an economic-anchor estimate, not a stage-fixed percentage. ",
      "Applied g=", sprintf("%.2f", g_pct), "% (cap ", sprintf("%.2f", cap),
      "% from LIFECYCLE_CONFIG; Rf=", sprintf("%.2f", rf_pct),
      "%; source=", if (nzchar(src)) src else "session",
      "; fallback=", fallback, "). ",
      "Stage=", stage, " does not write g. High growth does not raise terminal g."
    )
  } else {
    paste0(
      "Lifecycle 終值 g 為經濟錨定，而非生命週期檔位的固定百分比。",
      "採用 g=", sprintf("%.2f", g_pct), "%（上限 ", sprintf("%.2f", cap),
      "% 來自 LIFECYCLE_CONFIG；Rf=", sprintf("%.2f", rf_pct),
      "%；來源=", if (nzchar(src)) src else "session",
      "；fallback=", fallback, "）。",
      "檔位=", stage, " 不寫入 g。高成長不提高終值 g。"
    )
  }
  list(
    g_pct = round(g_pct, 2),
    reason = reason,
    fallback_source = fallback,
    suggest_two_stage = stage %in% c("HIGH_GROWTH", "GROWTH_TO_MATURE")
  )
}

#' 估計永續成長率（%）並附說明；必要時建議 two-stage
#' @param rf_pct CAPM／Macro 用 Rf（%，與 App 其餘處一致）
#' @param rf_source "live" | "last_known" | "fallback" | "session"；影響 Macro 文案
#' @param rf_label 公債來源標籤（如 Yahoo ^TNX）
#' @param locale "en" / "zh-TW"（Macro／Lifecycle 說明雙語）
#' @return list(g_pct, reason, lifecycle_stage, suggest_two_stage, g_stage1_pct, auto_lifecycle,
#'   recommended_method, recommend_label, recommend_reason, lifecycle_evidence, lifecycle_rule,
#'   rf_pct, rf_source, lifecycle_result)
estimate_perpetual_g <- function(method = "macro",
                                 rf_pct = NA_real_,
                                 d_is = NULL,
                                 d_bs = NULL,
                                 d_cf = NULL,
                                 industry_text = "",
                                 ticker = "",
                                 lifecycle_stage = "auto",
                                 wacc_pct = NA_real_,
                                 rev_cagr = NA_real_,
                                 rf_source = NULL,
                                 rf_label = NULL,
                                 locale = "zh-TW",
                                 industry_key = "",
                                 lifecycle_result = NULL) {
  method <- as.character(method %||% "macro")[1]
  rf_pct <- suppressWarnings(as.numeric(rf_pct)[1])
  wacc_pct <- suppressWarnings(as.numeric(wacc_pct)[1])
  rf_source <- as.character(rf_source %||% "")[1]
  rf_label <- as.character(rf_label %||% "")[1]
  en <- identical(tolower(as.character(locale %||% "")[1]), "en") ||
    identical(as.character(locale %||% "")[1], "en-US")
  if (is.na(rev_cagr) || !is.finite(rev_cagr)) {
    rev_cagr <- tryCatch({
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    }, error = function(e) NA_real_)
  }

  if (is.null(lifecycle_result)) {
    lifecycle_result <- classify_lifecycle_result(
      industry_text = industry_text,
      industry_key = industry_key,
      ticker = ticker,
      d_is = d_is,
      d_bs = d_bs,
      d_cf = d_cf,
      wacc_pct = wacc_pct,
      expected_rev_cagr_pct = rev_cagr,
      selected_stage = lifecycle_stage,
      locale = locale
    )
  }
  auto_stage <- as.character(lifecycle_result$autoDetectedStage %||% "")[1]
  evidence <- paste(lifecycle_result$classificationReasons %||% character(0), collapse = " ")
  stage <- as.character(lifecycle_result$selectedStage %||% lifecycle_stage %||% "auto")[1]
  if (!nzchar(stage) || identical(stage, "auto")) stage <- auto_stage

  g_pct <- NA_real_
  reason <- ""
  suggest_two_stage <- FALSE
  g_stage1_pct <- if (is.finite(rev_cagr)) {
    round(max(2, min(rev_cagr, 15)), 2)
  } else {
    NA_real_
  }

  if (identical(method, "fundamental")) {
    g_pct <- calc_fundamental_sgr_pct(d_is, d_bs, d_cf)
    if (is.na(g_pct) || !is.finite(g_pct)) {
      g_pct <- if (is.finite(rf_pct)) round(rf_pct, 2) else 3
      reason <- paste0(
        "Fundamental／SGR：財報不足以計算 Retention×ROE，已回退 Macro（Rf=",
        g_pct, "%）。僅適合成熟、財務結構穩定企業。"
      )
      if (en) {
        reason <- paste0(
          "Fundamental／SGR: statements insufficient for Retention×ROE; fell back to Macro (Rf=",
          g_pct, "%). Suitable only for mature, financially stable firms."
        )
      }
    } else {
      reason <- paste0(
        "Fundamental／SGR：g = Retention Ratio × ROE = ", g_pct,
        "%。應用限制：僅適合成熟、財務結構穩定企業。"
      )
      if (en) {
        reason <- paste0(
          "Fundamental／SGR: g = Retention Ratio × ROE = ", g_pct,
          "%. Limit: mature, financially stable firms only."
        )
      }
    }
  } else if (identical(method, "lifecycle")) {
    life_g <- estimate_lifecycle_terminal_g(
      rf_pct = rf_pct,
      wacc_pct = wacc_pct,
      lifecycle_result = lifecycle_result,
      rf_source = rf_source,
      locale = locale
    )
    g_pct <- life_g$g_pct
    reason <- life_g$reason
    suggest_two_stage <- isTRUE(life_g$suggest_two_stage)
    if (en) {
      reason <- paste0(
        reason, " Auto-class=", auto_stage, "; applied=", stage, ". ",
        "Objective evidence: ", evidence
      )
    } else {
      reason <- paste0(
        reason, " 自動分類=", auto_stage, "；目前採用=", stage, "。",
        "客觀依據：", evidence
      )
    }
  } else {
    # macro：錨定即時（或 last-known）10Y／市場 Rf；固定數僅為最後工程 fallback
    if (!is.finite(rf_pct)) {
      rf_pct <- 5
      if (!nzchar(rf_source)) rf_source <- "fallback"
    }
    g_pct <- round(rf_pct, 2)
    # Always show two decimals so live ~4.998% is not mistaken for integer fallback "5%"
    g_txt <- sprintf("%.2f", g_pct)
    lab <- if (nzchar(rf_label)) rf_label else "10Y Treasury / 10 年期公債"
    if (identical(rf_source, "fallback")) {
      reason <- if (en) {
        paste0(
          "Macroeconomic Anchoring: live Treasury fetch failed; engineering fallback Rf=",
          g_txt, "% (not a live yield; prefer Yahoo-scraped ", lab, ")."
        )
      } else {
        paste0(
          "Macroeconomic Anchoring：即時公債擷取失敗，工程 fallback Rf=", g_txt,
          "%（非即時殖利率；優先應使用 Yahoo 擷取的 ", lab, "）。"
        )
      }
    } else if (identical(rf_source, "last_known")) {
      reason <- if (en) {
        paste0(
          "Macroeconomic Anchoring: live fetch failed; using last successful ",
          lab, " Rf=", g_txt, "% (not a fixed default)."
        )
      } else {
        paste0(
          "Macroeconomic Anchoring：本次即時擷取失敗，改用最近一次成功的 ",
          lab, " Rf=", g_txt, "%（非固定預設）。"
        )
      }
    } else {
      reason <- if (en) {
        paste0(
          "Macroeconomic Anchoring: using ", lab, " Rf=", g_txt, "%",
          if (identical(rf_source, "session")) {
            " (synced with CAPM Rf; live scrape or user override)."
          } else {
            " (live scrape)."
          }
        )
      } else {
        paste0(
          "Macroeconomic Anchoring：採用", lab, " Rf=", g_txt, "%",
          if (identical(rf_source, "session")) {
            "（與 CAPM Rf 同步；來源為即時擷取或使用者覆寫）。"
          } else {
            "（即時擷取）。"
          }
        )
      }
    }
  }

  if (is.finite(wacc_pct) && is.finite(g_pct) && g_pct >= wacc_pct) {
    reason <- paste0(
      reason,
      if (en) {
        paste0(
          " ⚠ g≥WACC (", round(g_pct, 2), "% ≥ ", round(wacc_pct, 2),
          "%); Gordon terminal denominator ≤0 — DCF/RI cannot compute."
        )
      } else {
        paste0(
          " ⚠ g≥WACC（", round(g_pct, 2), "% ≥ ", round(wacc_pct, 2),
          "%），Gordon 終值分母≤0，DCF/RI 將無法計算。"
        )
      }
    )
  }

  rec <- recommend_perpetual_g_method(
    rf_pct = rf_pct,
    d_is = d_is,
    d_bs = d_bs,
    d_cf = d_cf,
    industry_text = industry_text,
    ticker = ticker,
    wacc_pct = wacc_pct,
    rev_cagr = rev_cagr,
    locale = locale,
    industry_key = industry_key,
    lifecycle_result = lifecycle_result,
    selected_stage = lifecycle_stage
  )

  list(
    g_pct = g_pct,
    reason = reason,
    lifecycle_stage = stage,
    auto_lifecycle = auto_stage,
    lifecycle_evidence = evidence,
    lifecycle_rule = lifecycle_result$fallbackSource %||% NA_character_,
    suggest_two_stage = isTRUE(suggest_two_stage),
    g_stage1_pct = g_stage1_pct,
    recommended_method = rec$method,
    recommend_label = rec$label,
    recommend_reason = rec$reason,
    rf_pct = if (is.finite(rf_pct)) round(rf_pct, 2) else NA_real_,
    rf_source = if (nzchar(rf_source)) rf_source else NA_character_,
    lifecycle_result = lifecycle_result
  )
}


# `%||%` 若環境尚無
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}


source(file.path(.ynow_setup_dir, "setup_report.R"), local = TRUE, encoding = "UTF-8")
source(file.path(.ynow_setup_dir, "setup_plotly.R"), local = TRUE, encoding = "UTF-8")
source(file.path(.ynow_setup_dir, "setup_feedback.R"), local = TRUE, encoding = "UTF-8")

# ==========================================
# Shared gray「回復預設」button (all valuation models)
# ==========================================
ynow_reset_defaults_btn <- function(input_id, label = "回復預設", block = TRUE) {
  actionButton(
    input_id,
    label,
    icon = icon("refresh"),
    class = if (isTRUE(block)) "btn-default btn-block ynow-btn-reset" else "btn-default ynow-btn-reset",
    style = paste(
      "padding: 12px; font-weight: bold; font-size: 16px;",
      "background-color: #7f8c8d; color: #ffffff; border-color: #6c757d;"
    )
  )
}

#' Companion green「試算」block button (pair with ynow_reset_defaults_btn).
#' Color: logo green (--ynow-logo-green / #249C60) via .ynow-btn-calc in ynow_ui.R.
ynow_calc_btn <- function(input_id, label, block = TRUE) {
  actionButton(
    input_id,
    label,
    icon = icon("calculator"),
    class = if (isTRUE(block)) {
      "btn-success btn-block ynow-btn-calc"
    } else {
      "btn-success ynow-btn-calc"
    },
    style = "padding: 12px; font-weight: bold; font-size: 16px;"
  )
}

source(file.path(.ynow_setup_dir, "valuation_guard.R"), local = TRUE, encoding = "UTF-8")
