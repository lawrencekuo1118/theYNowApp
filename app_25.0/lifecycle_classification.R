# ==========================================
# Lifecycle Classification — single source of truth
#
# This module classifies a firm's lifecycle stage. It does NOT:
#   - pick a valuation model (that stays in recommend_valuation_models)
#   - set a fixed Terminal Growth Rate (that stays in estimate_perpetual_g)
#   - emit warnings (that stays in valuation_diagnostic / valuation_guard)
#
# Calculation keys are stable IDs. UI labels live in ui_locale.R.
# Thresholds and weights live in LIFECYCLE_CONFIG (not in UI or callers).
# Missing metrics are excluded — never treated as zero, never auto-mature.
# ==========================================

LIFECYCLE_STAGE_IDS <- c(
  "HIGH_GROWTH",
  "GROWTH_TO_MATURE",
  "MATURE_GROWTH",
  "MATURE_STABLE",
  "DECLINING_OR_FINITE_LIFE",
  "REGULATED_UTILITY",
  "FINANCIAL_INSTITUTION"
)

LIFECYCLE_GENERAL_STAGE_IDS <- c(
  "HIGH_GROWTH",
  "GROWTH_TO_MATURE",
  "MATURE_GROWTH",
  "MATURE_STABLE"
)

LIFECYCLE_SPECIAL_STAGE_IDS <- c(
  "FINANCIAL_INSTITUTION",
  "REGULATED_UTILITY",
  "DECLINING_OR_FINITE_LIFE"
)

# Scoring / threshold configuration. Callers may pass an override list
# that is merged on top of these defaults. Do not hard-code the same
# numbers inside the scoring function.
LIFECYCLE_CONFIG <- list(
  cagr_high_pct = 20,
  cagr_g2m_lo_pct = 10,
  cagr_g2m_hi_pct = 20,
  cagr_mg_lo_pct = 5,
  cagr_mg_hi_pct = 10,
  cagr_ms_lo_pct = 0,
  cagr_ms_hi_pct = 5,
  cagr_decline_pct = -2,
  hist_cagr_years_min = 3L,
  hist_cagr_years_max = 5L,
  margin_level_high = 0.20,
  margin_level_mid = 0.08,
  margin_stable_cv = 0.25,
  margin_expand_pp = 0.015,
  fcf_positive_share = 0.50,
  fcf_vol_cv = 0.75,
  fcf_vol_stable_cv = 0.40,
  roic_wacc_spread_pp = 2,
  reinvestment_high = 0.55,
  reinvestment_low = 0.25,
  capex_to_da_high = 1.35,
  capex_to_da_low = 1.05,
  financial_interest_to_rev = 0.25,
  financial_fin_assets_to_assets = 0.45,
  utility_capex_to_da = 1.20,
  close_score_gap = 0.08,
  low_confidence = 0.45,
  low_completeness = 0.50,
  contradiction_penalty = 0.12,
  single_metric_penalty = 0.10,
  hist_fwd_gap_penalty = 0.08,
  missing_penalty = 0.07,
  special_confidence_floor = 0.55,
  forecast_years = c(
    HIGH_GROWTH = 10,
    GROWTH_TO_MATURE = 8,
    MATURE_GROWTH = 6,
    MATURE_STABLE = 5,
    DECLINING_OR_FINITE_LIFE = 5,
    REGULATED_UTILITY = 5,
    FINANCIAL_INSTITUTION = 5
  ),
  weights = c(
    fwd_rev_cagr = 0.18,
    hist_rev_cagr = 0.12,
    rev_trend = 0.08,
    op_margin_level = 0.07,
    op_margin_stability = 0.06,
    op_margin_expansion = 0.06,
    fcf_positive = 0.08,
    fcf_volatility = 0.06,
    roic_wacc_spread = 0.10,
    reinvestment = 0.07,
    capex_vs_da = 0.05,
    market_share_change = 0.03,
    sam_share = 0.02,
    fade_years = 0.02
  ),
  terminal = list(
    tv_ev_warn = 0.80,
    g_wacc_proximity = 0.01,
    long_run_nominal_g_cap_pct = 4.5,
    declining_g_pct = 0,
    jump_margin_pp = 0.04,
    jump_roic_pp = 0.03
  ),
  fi_industry_keys = c(
    "fn.Banking", "fn.Investment_Banking", "fn.Insurance", "fn.Asset_Management"
  ),
  utility_industry_keys = c("en.Utilities"),
  fi_industry_re = paste0(
    "Banks?[[:space:]-]|Life Insurance|Property[[:space:]&].*Casualty|",
    "Asset Management|Capital Markets|Diversified Financials|",
    "[[:space:]]Financials[[:space:]]|^Financials$|Banks -"
  ),
  utility_industry_re = paste0(
    "Electric Utilities|Gas Utilities|Regulated Utility|",
    "Multi-Utilities|Independent Power Producers|Water Utilities"
  )
)

# Legacy UI / snapshot values. Mapping is migration-only; old IDs are
# never used as the new scoring core. Ambiguous rows fall back to auto.
.LIFECYCLE_LEGACY_MAP <- c(
  auto = "auto",
  HIGH_GROWTH = "HIGH_GROWTH",
  GROWTH_TO_MATURE = "GROWTH_TO_MATURE",
  MATURE_GROWTH = "MATURE_GROWTH",
  MATURE_STABLE = "MATURE_STABLE",
  DECLINING_OR_FINITE_LIFE = "DECLINING_OR_FINITE_LIFE",
  REGULATED_UTILITY = "REGULATED_UTILITY",
  FINANCIAL_INSTITUTION = "FINANCIAL_INSTITUTION",
  growth_to_mature = "GROWTH_TO_MATURE",
  mature_sunset = NA_character_,
  mature_tech = NA_character_,
  mature_general = NA_character_
)

lifecycle_config <- function(override = NULL) {
  cfg <- LIFECYCLE_CONFIG
  if (is.null(override) || !length(override)) return(cfg)
  if (!is.list(override)) stop("lifecycle_config override must be a list")
  modifyList(cfg, override)
}

lifecycle_stage_ids <- function(include_auto = TRUE) {
  if (isTRUE(include_auto)) c("auto", LIFECYCLE_STAGE_IDS) else LIFECYCLE_STAGE_IDS
}

#' Display label for a stage ID. Calculation must never branch on this text.
lifecycle_stage_label <- function(stage, locale = "zh-TW") {
  id <- as.character(stage %||% "")[1]
  loc <- tryCatch({
    if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(locale) else locale
  }, error = function(e) locale)
  key <- switch(
    id,
    "auto" = "lifecycle_opt_auto",
    "HIGH_GROWTH" = "lifecycle_opt_high_growth",
    "GROWTH_TO_MATURE" = "lifecycle_opt_growth_to_mature",
    "MATURE_GROWTH" = "lifecycle_opt_mature_growth",
    "MATURE_STABLE" = "lifecycle_opt_mature_stable",
    "DECLINING_OR_FINITE_LIFE" = "lifecycle_opt_declining",
    "REGULATED_UTILITY" = "lifecycle_opt_utility",
    "FINANCIAL_INSTITUTION" = "lifecycle_opt_financial",
    "mature_sunset" = "lifecycle_opt_sunset",
    "mature_tech" = "lifecycle_opt_tech",
    "growth_to_mature" = "lifecycle_opt_growth_to_mature",
    "mature_general" = "lifecycle_opt_general",
    NA_character_
  )
  if (nzchar(key %||% "") && exists("ui_str", mode = "function")) {
    lab <- tryCatch(ui_str(key, loc), error = function(e) NULL)
    if (!is.null(lab) && !identical(lab, key)) return(lab)
  }
  fallback <- list(
    auto = if (identical(loc, "en")) "Auto-detect" else "自動偵測",
    HIGH_GROWTH = "High Growth",
    GROWTH_TO_MATURE = "Growth-to-Mature",
    MATURE_GROWTH = "Mature Growth",
    MATURE_STABLE = "Mature Stable",
    DECLINING_OR_FINITE_LIFE = "Declining / Finite Life",
    REGULATED_UTILITY = "Regulated Utility",
    FINANCIAL_INSTITUTION = "Financial Institution",
    mature_sunset = if (identical(loc, "en")) {
      "Legacy: highly mature / financial-utility"
    } else {
      "舊版：高度成熟／金融公用"
    },
    mature_tech = if (identical(loc, "en")) "Legacy: mature tech" else "舊版：成熟科技",
    growth_to_mature = "Growth-to-Mature",
    mature_general = if (identical(loc, "en")) "Legacy: general mature" else "舊版：一般成熟"
  )
  if (id %in% names(fallback)) fallback[[id]] else id
}

lifecycle_stage_choices <- function(locale = "zh-TW", include_auto = TRUE) {
  ids <- lifecycle_stage_ids(include_auto = include_auto)
  stats::setNames(ids, vapply(ids, lifecycle_stage_label, character(1), locale = locale))
}

#' Map a stored / UI value to a current stage ID.
#' Ambiguous legacy rows become auto and keep the original value.
map_legacy_lifecycle_stage <- function(value) {
  raw <- as.character(value %||% "")[1]
  if (!nzchar(raw) || identical(raw, "NA")) {
    return(list(
      selected_stage = "auto",
      selection_mode = "auto",
      legacy_value = raw,
      mapped = FALSE,
      reliable = TRUE,
      fallback_reason = "empty"
    ))
  }
  if (raw %in% LIFECYCLE_STAGE_IDS || identical(raw, "auto")) {
    return(list(
      selected_stage = raw,
      selection_mode = if (identical(raw, "auto")) "auto" else "manual",
      legacy_value = raw,
      mapped = FALSE,
      reliable = TRUE,
      fallback_reason = NA_character_
    ))
  }
  hit <- unname(.LIFECYCLE_LEGACY_MAP[raw])
  if (!length(hit) || is.na(hit) || !nzchar(hit)) {
    return(list(
      selected_stage = "auto",
      selection_mode = "auto",
      legacy_value = raw,
      mapped = TRUE,
      reliable = FALSE,
      fallback_reason = "unreliable_legacy"
    ))
  }
  list(
    selected_stage = hit,
    selection_mode = if (identical(hit, "auto")) "auto" else "manual",
    legacy_value = raw,
    mapped = TRUE,
    reliable = TRUE,
    fallback_reason = "legacy_direct"
  )
}

is_legacy_lifecycle_stage <- function(value) {
  raw <- as.character(value %||% "")[1]
  raw %in% c("mature_sunset", "mature_tech", "mature_general", "growth_to_mature")
}

.lc_num <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) NA_real_ else x
}

.lc_finite <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

.lc_series_cagr_pct <- function(vals, n_min = 2L, n_max = 5L) {
  v <- .lc_finite(vals)
  if (length(v) < n_min) return(NA_real_)
  use_n <- min(length(v), as.integer(n_max))
  v <- v[seq_len(use_n)]
  newest <- v[1]
  oldest <- v[length(v)]
  years <- length(v) - 1L
  if (!is.finite(newest) || !is.finite(oldest) || oldest <= 0 || years < 1L) {
    return(NA_real_)
  }
  if (newest <= 0) return(NA_real_)
  ((newest / oldest)^(1 / years) - 1) * 100
}

.lc_yoy_pct <- function(vals) {
  v <- .lc_finite(vals)
  if (length(v) < 2L) return(numeric(0))
  older <- v[-1]
  newer <- v[-length(v)]
  out <- (newer - older) / abs(older)
  out[is.finite(out) & older != 0] * 100
}

.lc_cv <- function(vals) {
  v <- .lc_finite(vals)
  if (length(v) < 2L) return(NA_real_)
  m <- mean(v)
  if (!is.finite(m) || abs(m) < 1e-12) return(NA_real_)
  stats::sd(v) / abs(m)
}

.lc_metric_row <- function(df, names) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(numeric(0))
  if (exists("select_clean_metric_row_any", mode = "function")) {
    vals <- tryCatch(
      select_clean_metric_row_any(df, names, include_ttm = FALSE),
      error = function(e) NULL
    )
    if (!is.null(vals) && length(vals) && !all(is.na(vals))) {
      return(suppressWarnings(as.numeric(vals)))
    }
  }
  if (exists("select_clean_metric_row", mode = "function")) {
    for (nm in names) {
      vals <- tryCatch(
        select_clean_metric_row(df, nm, include_ttm = FALSE),
        error = function(e) NULL
      )
      if (!is.null(vals) && length(vals) && !all(is.na(vals))) {
        return(suppressWarnings(as.numeric(vals)))
      }
    }
  }
  numeric(0)
}

.lc_current <- function(vals) {
  v <- .lc_finite(vals)
  if (!length(v)) return(NA_real_)
  v[1]
}

# Trapezoid membership in [0, 1]. NA input → NA (caller must drop it).
.lc_trap <- function(x, lo, peak_lo, peak_hi, hi) {
  x <- .lc_num(x)
  if (!is.finite(x) || !is.finite(lo) || !is.finite(hi)) return(NA_real_)
  if (x <= lo || x >= hi) return(0)
  if (x >= peak_lo && x <= peak_hi) return(1)
  if (x > lo && x < peak_lo) return((x - lo) / max(peak_lo - lo, 1e-9))
  (hi - x) / max(hi - peak_hi, 1e-9)
}

.lc_high_side <- function(x, start, full) {
  x <- .lc_num(x)
  if (!is.finite(x) || !is.finite(start) || !is.finite(full)) return(NA_real_)
  if (x <= start) return(0)
  if (x >= full) return(1)
  (x - start) / max(full - start, 1e-9)
}

.lc_low_side <- function(x, full, end) {
  x <- .lc_num(x)
  if (!is.finite(x) || !is.finite(full) || !is.finite(end)) return(NA_real_)
  if (x >= end) return(0)
  if (x <= full) return(1)
  (end - x) / max(end - full, 1e-9)
}

empty_lifecycle_result <- function(selected_stage = "auto",
                                   selection_mode = "auto",
                                   legacy_value = NA_character_,
                                   missing_inputs = character(0),
                                   reason = "No usable classification inputs") {
  list(
    autoDetectedStage = NA_character_,
    selectedStage = selected_stage,
    selectionMode = selection_mode,
    secondaryCandidate = NA_character_,
    confidenceScore = NA_real_,
    dataCompletenessScore = 0,
    classificationReasons = as.character(reason),
    missingInputs = unique(as.character(missing_inputs)),
    metricScores = list(),
    classScores = stats::setNames(rep(NA_real_, length(LIFECYCLE_STAGE_IDS)), LIFECYCLE_STAGE_IDS),
    specialBusinessType = NA_character_,
    recommendedForecastYears = NA_integer_,
    legacyValue = legacy_value,
    fallbackSource = "insufficient_inputs"
  )
}

#' Extract classification metrics. Missing values stay NA — never coerced to 0.
extract_lifecycle_metrics <- function(d_is = NULL,
                                      d_bs = NULL,
                                      d_cf = NULL,
                                      wacc_pct = NA_real_,
                                      expected_rev_cagr_pct = NA_real_,
                                      market_share_change = NA_real_,
                                      sam_share = NA_real_,
                                      fade_years = NA_real_,
                                      finite_life = FALSE,
                                      tax_ratio = NA_real_,
                                      config = NULL) {
  cfg <- lifecycle_config(config)
  rev <- .lc_metric_row(d_is, c("^Total Revenue$", "Total Revenue", "Operating Revenue"))
  ebit <- .lc_metric_row(d_is, c("^Operating Income$", "Operating Income", "^EBIT$", "EBIT"))
  interest <- .lc_metric_row(d_is, c("Interest Expense", "Interest Income", "Net Interest Income"))
  interest_inc <- .lc_metric_row(d_is, c("^Interest Income$", "Net Interest Income"))
  ni <- .lc_metric_row(d_is, if (exists("NET_INCOME_PATTERNS")) NET_INCOME_PATTERNS else c("Net Income"))
  fcf <- .lc_metric_row(d_cf, c("^Free Cash Flow$", "Free Cash Flow"))
  if (!length(.lc_finite(fcf)) && exists("reconstruct_hist_fcff", mode = "function")) {
    rec <- tryCatch(reconstruct_hist_fcff(d_cf, d_is = d_is), error = function(e) NULL)
    if (!is.null(rec) && length(rec$fcff)) fcf <- suppressWarnings(as.numeric(rec$fcff))
  }
  capex <- abs(.lc_metric_row(d_cf, if (exists("CAPEX_PATTERNS")) CAPEX_PATTERNS else c("Capital Expenditure")))
  da <- .lc_metric_row(d_cf, if (exists("DA_PATTERNS")) DA_PATTERNS else c("Depreciation And Amortization", "Depreciation"))
  assets <- .lc_metric_row(d_bs, c("^Total Assets$", "Total Assets"))
  equity <- .lc_metric_row(d_bs, if (exists("EQUITY_PATTERNS")) EQUITY_PATTERNS else c("Stockholders Equity", "Common Stock Equity"))
  cash <- .lc_metric_row(d_bs, c("Cash And Cash Equivalents", "Cash & Cash Equivalents", "^Cash$"))
  curr_liab <- .lc_metric_row(d_bs, c("Total Current Liabilities", "Current Liabilities"))
  st_debt <- .lc_metric_row(d_bs, c("Current Debt", "Short Term Debt"))
  fin_assets <- .lc_metric_row(d_bs, c(
    "Net Loan", "Gross Loan", "Investment Securities",
    "Trading Securities", "Total Investments"
  ))
  ppe <- .lc_metric_row(d_bs, c("Net PPE", "Property Plant And Equipment", "Gross PPE"))

  hist_cagr <- .lc_series_cagr_pct(
    rev,
    n_min = cfg$hist_cagr_years_min,
    n_max = cfg$hist_cagr_years_max
  )
  yoy <- .lc_yoy_pct(rev)
  rev_trend <- if (length(yoy) >= 2L) {
    # newest YoY minus older mean: negative = deceleration
    yoy[1] - mean(yoy[-1])
  } else {
    NA_real_
  }
  fwd_cagr <- .lc_num(expected_rev_cagr_pct)
  op_m <- if (length(.lc_finite(rev)) && length(.lc_finite(ebit))) {
    r <- suppressWarnings(as.numeric(rev))
    e <- suppressWarnings(as.numeric(ebit))
    n <- min(length(r), length(e))
    out <- e[seq_len(n)] / r[seq_len(n)]
    out[!(is.finite(r[seq_len(n)]) & r[seq_len(n)] > 0 & is.finite(e[seq_len(n)]))] <- NA_real_
    out
  } else {
    numeric(0)
  }
  op_level <- .lc_current(op_m)
  op_cv <- .lc_cv(op_m)
  op_expand <- if (length(.lc_finite(op_m)) >= 2L) {
    v <- .lc_finite(op_m)
    v[1] - v[length(v)]
  } else {
    NA_real_
  }
  fcf_pos_share <- if (length(.lc_finite(fcf))) {
    mean(.lc_finite(fcf) > 0)
  } else {
    NA_real_
  }
  fcf_cv <- .lc_cv(fcf)
  fcf_latest <- .lc_current(fcf)
  fcf_turned_pos <- is.finite(fcf_latest) && fcf_latest > 0 &&
    length(.lc_finite(fcf)) >= 2L && tail(.lc_finite(fcf), 1) <= 0

  tax <- .lc_num(tax_ratio)
  if (!is.finite(tax) || tax < 0 || tax > 1) tax <- NA_real_
  nopat <- {
    e <- .lc_current(ebit)
    if (!is.finite(e) || !is.finite(tax)) NA_real_ else e * (1 - tax)
  }
  ic <- {
    a <- .lc_current(assets)
    csh <- .lc_current(cash)
    cl <- .lc_current(curr_liab)
    sd <- .lc_current(st_debt)
    if (!is.finite(a)) {
      NA_real_
    } else {
      csh_use <- if (is.finite(csh)) csh else NA_real_
      cl_use <- if (is.finite(cl)) cl else NA_real_
      sd_use <- if (is.finite(sd)) sd else 0
      if (!is.finite(csh_use) || !is.finite(cl_use)) {
        NA_real_
      } else {
        (a - csh_use) - (cl_use - sd_use)
      }
    }
  }
  roic_pct <- if (is.finite(nopat) && is.finite(ic) && ic > 0) nopat / ic * 100 else NA_real_
  wacc <- .lc_num(wacc_pct)
  spread_pp <- if (is.finite(roic_pct) && is.finite(wacc)) roic_pct - wacc else NA_real_
  reinvest <- if (is.finite(nopat) && nopat > 0) {
    cx <- .lc_current(capex)
    d <- .lc_current(da)
    if (!is.finite(cx) || !is.finite(d)) NA_real_ else (cx - d) / nopat
  } else {
    NA_real_
  }
  capex_da <- {
    cx <- .lc_current(capex)
    d <- .lc_current(da)
    if (!is.finite(cx) || !is.finite(d) || d <= 0) NA_real_ else cx / d
  }
  int_to_rev <- {
    inc <- .lc_current(interest_inc)
    r <- .lc_current(rev)
    if (is.finite(inc) && is.finite(r) && r > 0) inc / r else NA_real_
  }
  fin_asset_share <- {
    fa <- .lc_current(fin_assets)
    a <- .lc_current(assets)
    if (is.finite(fa) && is.finite(a) && a > 0) fa / a else NA_real_
  }
  ppe_share <- {
    p <- .lc_current(ppe)
    a <- .lc_current(assets)
    if (is.finite(p) && is.finite(a) && a > 0) p / a else NA_real_
  }

  list(
    fwd_rev_cagr = fwd_cagr,
    hist_rev_cagr = hist_cagr,
    rev_trend = rev_trend,
    op_margin_level = op_level,
    op_margin_stability = if (is.finite(op_cv)) 1 / (1 + op_cv) else NA_real_,
    op_margin_cv = op_cv,
    op_margin_expansion = op_expand,
    fcf_positive = fcf_pos_share,
    fcf_latest = fcf_latest,
    fcf_turned_positive = isTRUE(fcf_turned_pos),
    fcf_volatility = fcf_cv,
    roic_pct = roic_pct,
    wacc_pct = wacc,
    roic_wacc_spread = spread_pp,
    reinvestment = reinvest,
    capex_vs_da = capex_da,
    market_share_change = .lc_num(market_share_change),
    sam_share = .lc_num(sam_share),
    fade_years = .lc_num(fade_years),
    finite_life = isTRUE(finite_life),
    interest_to_rev = int_to_rev,
    fin_assets_to_assets = fin_asset_share,
    ppe_to_assets = ppe_share,
    revenue_latest = .lc_current(rev),
    nopat = nopat,
    invested_capital = ic
  )
}

.lc_missing_from_metrics <- function(m) {
  needed <- c(
    fwd_rev_cagr = "expected_rev_cagr_3y",
    hist_rev_cagr = "hist_rev_cagr_3_5y",
    rev_trend = "rev_growth_trend",
    op_margin_level = "operating_margin",
    op_margin_stability = "operating_margin_stability",
    op_margin_expansion = "operating_margin_expansion",
    fcf_positive = "fcf_positive_share",
    fcf_volatility = "fcf_volatility",
    roic_pct = "roic",
    wacc_pct = "wacc",
    roic_wacc_spread = "roic_wacc_spread",
    reinvestment = "reinvestment_rate",
    capex_vs_da = "capex_to_da",
    market_share_change = "market_share_change",
    sam_share = "sam_share",
    fade_years = "fade_years"
  )
  miss <- character(0)
  for (nm in names(needed)) {
    val <- m[[nm]]
    if (is.null(val) || !is.finite(.lc_num(val))) miss <- c(miss, needed[[nm]])
  }
  unique(miss)
}

.lc_detect_financial_institution <- function(industry_text, industry_key, metrics, cfg) {
  key <- trimws(as.character(industry_key %||% "")[1])
  txt <- paste(industry_text %||% "", collapse = " ")
  key_hit <- nzchar(key) && key %in% cfg$fi_industry_keys
  # Industry text is auxiliary evidence only; never company/ticker name.
  txt_hit <- nzchar(txt) && grepl(cfg$fi_industry_re, txt, ignore.case = TRUE, perl = FALSE)
  stmt_int <- is.finite(metrics$interest_to_rev) &&
    metrics$interest_to_rev >= cfg$financial_interest_to_rev
  stmt_fa <- is.finite(metrics$fin_assets_to_assets) &&
    metrics$fin_assets_to_assets >= cfg$financial_fin_assets_to_assets
  reasons <- character(0)
  if (key_hit) {
    reasons <- c(reasons, paste0("Industry taxonomy key ", key, " is a financial-institution class."))
  }
  if (txt_hit) {
    reasons <- c(reasons, "Industry description matches a financial-institution taxonomy (not the company name).")
  }
  if (stmt_int) {
    reasons <- c(reasons, sprintf(
      "Interest income / revenue = %.1f%%, consistent with a spread business.",
      100 * metrics$interest_to_rev
    ))
  }
  if (stmt_fa) {
    reasons <- c(reasons, sprintf(
      "Financial assets / total assets = %.1f%%, so capital structure is an operating input.",
      100 * metrics$fin_assets_to_assets
    ))
  }
  # Require taxonomy or (industry text AND statement evidence). Name tokens alone never qualify.
  ok <- isTRUE(key_hit) || (isTRUE(txt_hit) && (isTRUE(stmt_int) || isTRUE(stmt_fa))) ||
    (isTRUE(stmt_int) && isTRUE(stmt_fa))
  list(hit = ok, reasons = reasons, evidence = list(
    industry_key = key_hit, industry_text = txt_hit,
    interest_to_rev = stmt_int, fin_assets = stmt_fa
  ))
}

.lc_detect_regulated_utility <- function(industry_text, industry_key, metrics, cfg) {
  key <- trimws(as.character(industry_key %||% "")[1])
  txt <- paste(industry_text %||% "", collapse = " ")
  key_hit <- nzchar(key) && key %in% cfg$utility_industry_keys
  txt_hit <- nzchar(txt) && grepl(cfg$utility_industry_re, txt, ignore.case = TRUE)
  capex_hit <- is.finite(metrics$capex_vs_da) && metrics$capex_vs_da >= cfg$utility_capex_to_da
  ppe_hit <- is.finite(metrics$ppe_to_assets) && metrics$ppe_to_assets >= 0.35
  reasons <- character(0)
  if (key_hit) reasons <- c(reasons, paste0("Industry taxonomy key ", key, " is a regulated-utility class."))
  if (txt_hit) reasons <- c(reasons, "Industry description matches a regulated-utility taxonomy.")
  if (capex_hit) {
    reasons <- c(reasons, sprintf(
      "CapEx / D&A = %.2f, consistent with a rate-base capital cycle.",
      metrics$capex_vs_da
    ))
  }
  if (ppe_hit) {
    reasons <- c(reasons, sprintf("Net PPE / assets = %.1f%%.", 100 * metrics$ppe_to_assets))
  }
  ok <- isTRUE(key_hit) || (isTRUE(txt_hit) && (isTRUE(capex_hit) || isTRUE(ppe_hit)))
  list(hit = ok, reasons = reasons, evidence = list(
    industry_key = key_hit, industry_text = txt_hit,
    capex_da = capex_hit, ppe = ppe_hit
  ))
}

.lc_detect_declining <- function(metrics, cfg) {
  hist_neg <- is.finite(metrics$hist_rev_cagr) && metrics$hist_rev_cagr <= cfg$cagr_decline_pct
  fwd_neg <- is.finite(metrics$fwd_rev_cagr) && metrics$fwd_rev_cagr <= cfg$cagr_decline_pct
  spread_neg <- is.finite(metrics$roic_wacc_spread) && metrics$roic_wacc_spread < 0
  low_rr <- is.finite(metrics$reinvestment) && metrics$reinvestment <= cfg$reinvestment_low
  finite <- isTRUE(metrics$finite_life)
  reasons <- character(0)
  if (hist_neg) {
    reasons <- c(reasons, sprintf(
      "Historical 3–5y revenue CAGR is %.1f%% (at or below the structural-decline threshold of %.1f%%).",
      metrics$hist_rev_cagr, cfg$cagr_decline_pct
    ))
  }
  if (fwd_neg) {
    reasons <- c(reasons, sprintf(
      "Expected 3y revenue CAGR is %.1f%%.",
      metrics$fwd_rev_cagr
    ))
  }
  if (spread_neg) {
    reasons <- c(reasons, sprintf(
      "ROIC (%.1f%%) is below WACC (%.1f%%).",
      metrics$roic_pct, metrics$wacc_pct
    ))
  }
  if (low_rr) {
    reasons <- c(reasons, sprintf(
      "Reinvestment rate is %.1f%%, so incremental capital is limited.",
      100 * metrics$reinvestment
    ))
  }
  if (finite) reasons <- c(reasons, "A finite-life flag was supplied (asset, contract, or concession).")
  # Need structural growth evidence plus at least one economic or finite-life signal.
  growth_hit <- isTRUE(hist_neg) || (isTRUE(fwd_neg) && isTRUE(hist_neg))
  econ_hit <- isTRUE(spread_neg) || isTRUE(low_rr) || isTRUE(finite)
  ok <- isTRUE(growth_hit) && isTRUE(econ_hit)
  list(hit = ok, reasons = reasons)
}

.lc_class_metric_scores <- function(metrics, cfg) {
  fwd <- metrics$fwd_rev_cagr
  hist <- metrics$hist_rev_cagr
  growth_anchor <- if (is.finite(fwd)) fwd else hist
  trend <- metrics$rev_trend
  op_lv <- metrics$op_margin_level
  op_st <- metrics$op_margin_stability
  op_ex <- metrics$op_margin_expansion
  fcf_p <- metrics$fcf_positive
  fcf_v <- metrics$fcf_volatility
  spread <- metrics$roic_wacc_spread
  rr <- metrics$reinvestment
  cda <- metrics$capex_vs_da
  mkt <- metrics$market_share_change
  sam <- metrics$sam_share
  fade <- metrics$fade_years

  score_one <- function(stage) {
    parts <- list()
    parts$fwd_rev_cagr <- switch(
      stage,
      HIGH_GROWTH = .lc_high_side(growth_anchor, cfg$cagr_g2m_hi_pct - 2, cfg$cagr_high_pct),
      GROWTH_TO_MATURE = .lc_trap(
        growth_anchor, cfg$cagr_mg_lo_pct, cfg$cagr_g2m_lo_pct,
        cfg$cagr_g2m_hi_pct, cfg$cagr_high_pct + 5
      ),
      MATURE_GROWTH = .lc_trap(
        growth_anchor, cfg$cagr_ms_lo_pct, cfg$cagr_mg_lo_pct,
        cfg$cagr_mg_hi_pct, cfg$cagr_g2m_hi_pct
      ),
      MATURE_STABLE = .lc_trap(
        growth_anchor, cfg$cagr_decline_pct, cfg$cagr_ms_lo_pct,
        cfg$cagr_ms_hi_pct, cfg$cagr_mg_hi_pct
      ),
      NA_real_
    )
    parts$hist_rev_cagr <- switch(
      stage,
      HIGH_GROWTH = .lc_high_side(hist, cfg$cagr_g2m_lo_pct, cfg$cagr_high_pct),
      GROWTH_TO_MATURE = .lc_trap(hist, 4, 8, 18, 28),
      MATURE_GROWTH = .lc_trap(hist, 1, 4, 10, 16),
      MATURE_STABLE = .lc_trap(hist, -3, 0, 5, 10),
      NA_real_
    )
    parts$rev_trend <- if (!is.finite(trend)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_high_side(trend, -4, 2)
    } else if (identical(stage, "GROWTH_TO_MATURE")) {
      .lc_low_side(trend, -8, 2)
    } else if (identical(stage, "MATURE_GROWTH")) {
      .lc_trap(trend, -6, -2, 2, 6)
    } else {
      .lc_trap(trend, -4, -1, 1, 4)
    }
    parts$op_margin_level <- if (!is.finite(op_lv)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_low_side(op_lv, 0, cfg$margin_level_mid)
    } else if (identical(stage, "GROWTH_TO_MATURE")) {
      .lc_trap(op_lv, 0, cfg$margin_level_mid * 0.5, cfg$margin_level_high, 0.45)
    } else {
      .lc_high_side(op_lv, 0, cfg$margin_level_mid)
    }
    parts$op_margin_stability <- if (!is.finite(op_st)) {
      NA_real_
    } else if (stage %in% c("MATURE_GROWTH", "MATURE_STABLE")) {
      .lc_high_side(op_st, 0.5, 0.85)
    } else {
      1 - .lc_high_side(op_st, 0.5, 0.85)
    }
    parts$op_margin_expansion <- if (!is.finite(op_ex)) {
      NA_real_
    } else if (stage %in% c("HIGH_GROWTH", "GROWTH_TO_MATURE")) {
      .lc_high_side(op_ex, 0, cfg$margin_expand_pp)
    } else {
      .lc_low_side(op_ex, -cfg$margin_expand_pp, cfg$margin_expand_pp)
    }
    parts$fcf_positive <- if (!is.finite(fcf_p)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      1 - fcf_p
    } else {
      fcf_p
    }
    parts$fcf_volatility <- if (!is.finite(fcf_v)) {
      NA_real_
    } else if (stage %in% c("MATURE_GROWTH", "MATURE_STABLE")) {
      .lc_low_side(fcf_v, cfg$fcf_vol_stable_cv, cfg$fcf_vol_cv + 0.4)
    } else {
      .lc_high_side(fcf_v, cfg$fcf_vol_stable_cv, cfg$fcf_vol_cv)
    }
    parts$roic_wacc_spread <- if (!is.finite(spread)) {
      NA_real_
    } else if (identical(stage, "MATURE_STABLE")) {
      .lc_trap(spread, -2, 0, 6, 20)
    } else {
      .lc_high_side(spread, 0, cfg$roic_wacc_spread_pp)
    }
    parts$reinvestment <- if (!is.finite(rr)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_high_side(rr, cfg$reinvestment_low, cfg$reinvestment_high)
    } else if (identical(stage, "MATURE_STABLE")) {
      .lc_low_side(rr, 0, cfg$reinvestment_high)
    } else {
      .lc_trap(rr, 0, cfg$reinvestment_low, cfg$reinvestment_high, 1.2)
    }
    parts$capex_vs_da <- if (!is.finite(cda)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_high_side(cda, 1, cfg$capex_to_da_high)
    } else if (identical(stage, "MATURE_STABLE")) {
      .lc_low_side(cda, 0.7, cfg$capex_to_da_high)
    } else {
      .lc_trap(cda, 0.6, 0.9, 1.6, 2.5)
    }
    parts$market_share_change <- if (!is.finite(mkt)) {
      NA_real_
    } else if (stage %in% c("HIGH_GROWTH", "GROWTH_TO_MATURE")) {
      .lc_high_side(mkt, 0, 0.02)
    } else {
      .lc_low_side(mkt, -0.01, 0.02)
    }
    parts$sam_share <- if (!is.finite(sam)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_low_side(sam, 0, 0.15)
    } else {
      .lc_high_side(sam, 0.05, 0.35)
    }
    parts$fade_years <- if (!is.finite(fade)) {
      NA_real_
    } else if (identical(stage, "HIGH_GROWTH")) {
      .lc_high_side(fade, 5, 12)
    } else if (identical(stage, "MATURE_STABLE")) {
      .lc_low_side(fade, 2, 8)
    } else {
      .lc_trap(fade, 2, 5, 10, 15)
    }
    parts
  }

  out <- lapply(stats::setNames(LIFECYCLE_GENERAL_STAGE_IDS, LIFECYCLE_GENERAL_STAGE_IDS), score_one)
  out
}

.lc_weighted_class_scores <- function(metric_parts, cfg) {
  w <- cfg$weights
  scores <- stats::setNames(rep(NA_real_, length(LIFECYCLE_GENERAL_STAGE_IDS)), LIFECYCLE_GENERAL_STAGE_IDS)
  used <- list()
  for (st in LIFECYCLE_GENERAL_STAGE_IDS) {
    parts <- metric_parts[[st]]
    num <- 0
    den <- 0
    keep <- list()
    for (nm in names(w)) {
      val <- parts[[nm]]
      wt <- .lc_num(w[[nm]])
      if (!is.finite(val) || !is.finite(wt) || wt <= 0) next
      num <- num + wt * val
      den <- den + wt
      keep[[nm]] <- val
    }
    scores[[st]] <- if (den > 0) num / den else NA_real_
    used[[st]] <- keep
  }
  list(scores = scores, metric_scores = used)
}

.lc_completeness <- function(missing, cfg) {
  tracked <- c(
    "expected_rev_cagr_3y", "hist_rev_cagr_3_5y", "rev_growth_trend",
    "operating_margin", "operating_margin_stability", "operating_margin_expansion",
    "fcf_positive_share", "fcf_volatility", "roic", "wacc",
    "roic_wacc_spread", "reinvestment_rate", "capex_to_da",
    "market_share_change", "sam_share", "fade_years"
  )
  present <- setdiff(tracked, missing)
  length(present) / length(tracked)
}

.lc_confidence <- function(top, second, completeness, contradictions, single_metric,
                           hist_fwd_conflict, cfg, special = FALSE) {
  gap <- if (is.finite(top) && is.finite(second)) top - second else if (is.finite(top)) top else 0
  gap_term <- min(1, max(0, gap / max(cfg$close_score_gap * 2.5, 1e-6)))
  conf <- 0.35 * completeness + 0.35 * gap_term + 0.15 * top
  if (isTRUE(contradictions)) conf <- conf - cfg$contradiction_penalty
  if (isTRUE(single_metric)) conf <- conf - cfg$single_metric_penalty
  if (isTRUE(hist_fwd_conflict)) conf <- conf - cfg$hist_fwd_gap_penalty
  if (isTRUE(special)) conf <- max(conf, cfg$special_confidence_floor * completeness)
  max(0, min(1, conf))
}

.lc_reasons_from_metrics <- function(stage, metrics, locale = "zh-TW") {
  en <- tryCatch({
    loc <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(locale) else locale
    identical(loc, "en")
  }, error = function(e) FALSE)
  bits <- character(0)
  add <- function(zh, en_txt) bits <<- c(bits, if (en) en_txt else zh)
  if (is.finite(metrics$fwd_rev_cagr)) {
    add(
      sprintf("預期三年營收 CAGR 為 %.1f%%。", metrics$fwd_rev_cagr),
      sprintf("Expected 3-year revenue CAGR is %.1f%%.", metrics$fwd_rev_cagr)
    )
  } else if (is.finite(metrics$hist_rev_cagr)) {
    add(
      sprintf("歷史三至五年營收 CAGR 為 %.1f%%。", metrics$hist_rev_cagr),
      sprintf("Historical 3–5 year revenue CAGR is %.1f%%.", metrics$hist_rev_cagr)
    )
  }
  if (is.finite(metrics$rev_trend) && metrics$rev_trend < -1) {
    add(
      sprintf("營收成長率呈下降（近期 YoY 較前期均值低 %.1f 個百分點）。", abs(metrics$rev_trend)),
      sprintf("Revenue growth is decelerating (recent YoY is %.1f pp below the earlier mean).", abs(metrics$rev_trend))
    )
  }
  if (isTRUE(metrics$fcf_turned_positive)) {
    add("自由現金流已由負轉正。", "Free cash flow has turned from negative to positive.")
  } else if (is.finite(metrics$fcf_positive) && metrics$fcf_positive >= 0.67) {
    add("自由現金流多數年度為正。", "Free cash flow is positive in most observed years.")
  } else if (is.finite(metrics$fcf_latest) && metrics$fcf_latest < 0) {
    add(
      sprintf("最近一期自由現金流為負（%.2f）。", metrics$fcf_latest),
      sprintf("Latest free cash flow is negative (%.2f).", metrics$fcf_latest)
    )
  }
  if (is.finite(metrics$op_margin_expansion) && metrics$op_margin_expansion > 0.005) {
    add(
      sprintf("營業利益率仍在擴張（%.1f 個百分點）。", 100 * metrics$op_margin_expansion),
      sprintf("Operating margin is still expanding (%.1f percentage points).", 100 * metrics$op_margin_expansion)
    )
  } else if (is.finite(metrics$op_margin_cv) && metrics$op_margin_cv <= 0.25) {
    add("營業利益率相對穩定。", "Operating margin is relatively stable.")
  }
  if (is.finite(metrics$roic_wacc_spread) && metrics$roic_wacc_spread > 0) {
    add(
      sprintf("ROIC（%.1f%%）高於 WACC（%.1f%%）。", metrics$roic_pct, metrics$wacc_pct),
      sprintf("ROIC (%.1f%%) is above WACC (%.1f%%).", metrics$roic_pct, metrics$wacc_pct)
    )
  } else if (is.finite(metrics$roic_wacc_spread) && metrics$roic_wacc_spread < 0) {
    add(
      sprintf("ROIC（%.1f%%）低於 WACC（%.1f%%）。", metrics$roic_pct, metrics$wacc_pct),
      sprintf("ROIC (%.1f%%) is below WACC (%.1f%%).", metrics$roic_pct, metrics$wacc_pct)
    )
  }
  if (is.finite(metrics$reinvestment)) {
    add(
      sprintf("再投資率約 %.1f%%。", 100 * metrics$reinvestment),
      sprintf("Reinvestment rate is about %.1f%%.", 100 * metrics$reinvestment)
    )
  }
  unique(bits)
}

#' Multi-factor lifecycle classification. Pure given metrics + industry evidence.
#'
#' Special-type order: Financial Institution → Regulated Utility →
#' Declining / Finite Life → general multi-factor scores.
classify_lifecycle_result <- function(industry_text = "",
                                      industry_key = "",
                                      ticker = "",
                                      d_is = NULL,
                                      d_bs = NULL,
                                      d_cf = NULL,
                                      wacc_pct = NA_real_,
                                      expected_rev_cagr_pct = NA_real_,
                                      market_share_change = NA_real_,
                                      sam_share = NA_real_,
                                      fade_years = NA_real_,
                                      finite_life = FALSE,
                                      tax_ratio = NA_real_,
                                      selected_stage = "auto",
                                      selection_mode = NULL,
                                      legacy_value = NA_character_,
                                      locale = "zh-TW",
                                      config = NULL,
                                      metrics = NULL) {
  cfg <- lifecycle_config(config)
  # ticker is accepted for call-site compatibility only and is never a feature.
  invisible(ticker)
  mapped <- map_legacy_lifecycle_stage(selected_stage)
  user_stage <- mapped$selected_stage
  mode <- as.character(selection_mode %||% "")[1]
  if (!nzchar(mode) || !mode %in% c("auto", "manual")) {
    mode <- if (identical(user_stage, "auto")) "auto" else "manual"
  }
  if (is.na(legacy_value) || !nzchar(as.character(legacy_value)[1])) {
    legacy_value <- mapped$legacy_value
  }

  if (is.null(metrics)) {
    metrics <- extract_lifecycle_metrics(
      d_is = d_is, d_bs = d_bs, d_cf = d_cf,
      wacc_pct = wacc_pct,
      expected_rev_cagr_pct = expected_rev_cagr_pct,
      market_share_change = market_share_change,
      sam_share = sam_share,
      fade_years = fade_years,
      finite_life = finite_life,
      tax_ratio = tax_ratio,
      config = cfg
    )
  }
  missing <- .lc_missing_from_metrics(metrics)
  completeness <- .lc_completeness(missing, cfg)
  class_scores <- stats::setNames(rep(NA_real_, length(LIFECYCLE_STAGE_IDS)), LIFECYCLE_STAGE_IDS)
  metric_scores <- list()
  special <- NA_character_
  reasons <- character(0)
  fallback <- NA_character_

  fi <- .lc_detect_financial_institution(industry_text, industry_key, metrics, cfg)
  ut <- .lc_detect_regulated_utility(industry_text, industry_key, metrics, cfg)
  dec <- .lc_detect_declining(metrics, cfg)

  if (isTRUE(fi$hit)) {
    special <- "FINANCIAL_INSTITUTION"
    class_scores[["FINANCIAL_INSTITUTION"]] <- 1
    reasons <- fi$reasons
    fallback <- "special_financial_institution"
  } else if (isTRUE(ut$hit)) {
    special <- "REGULATED_UTILITY"
    class_scores[["REGULATED_UTILITY"]] <- 1
    reasons <- ut$reasons
    fallback <- "special_regulated_utility"
  } else if (isTRUE(dec$hit)) {
    special <- "DECLINING_OR_FINITE_LIFE"
    class_scores[["DECLINING_OR_FINITE_LIFE"]] <- 1
    reasons <- dec$reasons
    fallback <- "special_declining_or_finite_life"
  } else {
    parts <- .lc_class_metric_scores(metrics, cfg)
    packed <- .lc_weighted_class_scores(parts, cfg)
    for (nm in names(packed$scores)) class_scores[[nm]] <- packed$scores[[nm]]
    metric_scores <- packed$metric_scores
    fallback <- "multi_factor_scores"
    # Do not treat missing growth as mature: if no growth anchor exists,
    # leave general scores NA rather than defaulting to MATURE_STABLE.
    if (!is.finite(metrics$fwd_rev_cagr) && !is.finite(metrics$hist_rev_cagr)) {
      fallback <- "missing_growth_anchor"
    }
  }

  ranked <- sort(class_scores[is.finite(class_scores)], decreasing = TRUE)
  auto_stage <- if (length(ranked)) names(ranked)[1] else NA_character_
  secondary <- if (length(ranked) >= 2L) names(ranked)[2] else NA_character_
  top <- if (length(ranked)) unname(ranked[1]) else NA_real_
  sec <- if (length(ranked) >= 2L) unname(ranked[2]) else NA_real_

  contradictions <- FALSE
  if (is.finite(metrics$fwd_rev_cagr) && is.finite(metrics$roic_wacc_spread)) {
    if (metrics$fwd_rev_cagr >= cfg$cagr_high_pct && metrics$roic_wacc_spread < 0) {
      contradictions <- TRUE
    }
    if (metrics$fwd_rev_cagr <= cfg$cagr_ms_hi_pct && metrics$roic_wacc_spread >= 8) {
      contradictions <- TRUE
    }
  }
  hist_fwd_conflict <- is.finite(metrics$fwd_rev_cagr) && is.finite(metrics$hist_rev_cagr) &&
    abs(metrics$fwd_rev_cagr - metrics$hist_rev_cagr) >= 10
  used_metrics <- if (length(metric_scores) && !is.na(auto_stage) && auto_stage %in% names(metric_scores)) {
    metric_scores[[auto_stage]]
  } else {
    list()
  }
  single_metric <- length(used_metrics) <= 1L && !isTRUE(!is.na(special))

  conf <- .lc_confidence(
    if (is.finite(top)) top else 0,
    if (is.finite(sec)) sec else 0,
    completeness, contradictions, single_metric, hist_fwd_conflict, cfg,
    special = !is.na(special)
  )
  if (is.finite(top) && is.finite(sec) && (top - sec) < cfg$close_score_gap) {
    conf <- min(conf, 0.55)
  }

  if (!length(reasons) && !is.na(auto_stage)) {
    reasons <- .lc_reasons_from_metrics(auto_stage, metrics, locale = locale)
  } else if (length(reasons) && !is.na(auto_stage) && is.na(special)) {
    reasons <- unique(c(reasons, .lc_reasons_from_metrics(auto_stage, metrics, locale = locale)))
  }
  if (!length(reasons)) {
    reasons <- if (identical(fallback, "missing_growth_anchor")) {
      "Expected and historical revenue CAGRs are both missing, so no general stage is forced."
    } else {
      "Classification inputs are incomplete; scores use only available metrics."
    }
  }

  selected <- if (identical(mode, "manual") && user_stage %in% LIFECYCLE_STAGE_IDS) {
    user_stage
  } else if (!is.na(auto_stage)) {
    auto_stage
  } else {
    "auto"
  }
  if (identical(mode, "auto") && !is.na(auto_stage)) selected <- auto_stage

  rec_years <- if (!is.na(auto_stage) && auto_stage %in% names(cfg$forecast_years)) {
    as.integer(unname(cfg$forecast_years[[auto_stage]]))
  } else {
    NA_integer_
  }

  list(
    autoDetectedStage = auto_stage,
    selectedStage = selected,
    selectionMode = mode,
    secondaryCandidate = secondary,
    confidenceScore = conf,
    dataCompletenessScore = completeness,
    classificationReasons = unname(as.character(reasons)),
    missingInputs = missing,
    metricScores = metric_scores,
    classScores = class_scores,
    specialBusinessType = special,
    recommendedForecastYears = rec_years,
    legacyValue = as.character(legacy_value)[1],
    fallbackSource = fallback,
    metrics = metrics,
    closeRace = is.finite(top) && is.finite(sec) && (top - sec) < cfg$close_score_gap
  )
}

#' Backward-compatible wrapper. Returns a current stage ID (never a legacy mix-in).
classify_lifecycle_stage <- function(industry_text = "",
                                     ticker = "",
                                     rev_cagr = NA_real_,
                                     detail = FALSE,
                                     industry_key = "",
                                     d_is = NULL,
                                     d_bs = NULL,
                                     d_cf = NULL,
                                     wacc_pct = NA_real_,
                                     expected_rev_cagr_pct = NA_real_,
                                     selected_stage = "auto",
                                     locale = "zh-TW",
                                     config = NULL) {
  fwd <- .lc_num(expected_rev_cagr_pct)
  if (!is.finite(fwd)) fwd <- .lc_num(rev_cagr)
  res <- classify_lifecycle_result(
    industry_text = industry_text,
    industry_key = industry_key,
    ticker = ticker,
    d_is = d_is,
    d_bs = d_bs,
    d_cf = d_cf,
    wacc_pct = wacc_pct,
    expected_rev_cagr_pct = fwd,
    selected_stage = selected_stage,
    locale = locale,
    config = config
  )
  stage <- res$autoDetectedStage
  if (is.na(stage) || !nzchar(stage)) stage <- NA_character_
  if (!isTRUE(detail)) return(stage)
  list(
    stage = stage,
    rule = res$fallbackSource,
    rev_cagr = fwd,
    industry_hit = res$specialBusinessType,
    evidence_zh = paste(res$classificationReasons, collapse = " "),
    evidence_en = paste(res$classificationReasons, collapse = " "),
    result = res
  )
}

recommended_forecast_years_for_stage <- function(stage, config = NULL) {
  cfg <- lifecycle_config(config)
  st <- as.character(stage %||% "")[1]
  if (!nzchar(st) || !st %in% names(cfg$forecast_years)) return(NA_integer_)
  as.integer(unname(cfg$forecast_years[[st]]))
}
