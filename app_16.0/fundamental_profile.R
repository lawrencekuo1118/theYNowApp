# ==========================================
# fundamental_profile.R — 財報屬性分群 → 重視指標
#
# 規則式啟發式（工程門檻，非學術標準／非 ticker 特例）。
# 與 industry_choice 正交：產業負責同業色碼；本檔負責「該重視什麼」。
# ==========================================

#' 工程啟發式門檻（可調；非學術標準）
.FUNDAMENTAL_PROFILE_THRESH <- list(
  rev_growth_high = 12,       # % CAGR → growth
  rev_growth_soft = 8,        # % with unstable FCF → growth
  fcf_cv_stable = 0.75,
  fcf_cv_volatile = 1.0,
  div_cv_stable = 0.50,
  capex_rev_high = 0.12,      # CapEx / Revenue ratio
  capex_rev_low = 0.04,
  gpm_high = 40,              # % gross margin for asset_light
  asset_turnover_low = 0.55
)

#' 屬性 → 有序重視指標（KPI／FS metric id）
PROFILE_FOCUS_METRICS <- list(
  holding_asset = c(
    "eqt_multiplier", "roa", "roe", "net_profit_margin"
  ),
  financial_book = c(
    "roe", "roa", "eqt_multiplier", "net_profit_margin",
    "pe_ratio", "beta"
  ),
  growth = c(
    "rev_growth", "gross_profit_growth", "gross_profit_margin",
    "op_cash_flow_growth", "roe"
  ),
  capital_intensive = c(
    "roa", "asset_turnover", "ocf_net_income",
    "gross_profit_margin", "rev_growth"
  ),
  mature_dividend = c(
    "net_profit_margin", "roe", "ocf_net_income",
    "dividend_yield", "pe_ratio"
  ),
  cyclical_volatile = c(
    "eqt_multiplier", "ocf_net_income", "gross_profit_margin", "rev_growth"
  ),
  asset_light = c(
    "gross_profit_margin", "opex_ratio", "rev_growth", "roe"
  ),
  fallback = character(0)
)

#' 屬性顯示名稱 key → ui_locale（fallback 字串）
.fundamental_profile_label_fallback <- function(profile_id, locale = "zh-TW") {
  id <- as.character(profile_id %||% "fallback")[1]
  zh <- c(
    holding_asset = "控股／資產導向",
    financial_book = "金融／帳面驅動",
    growth = "高成長",
    capital_intensive = "資本密集",
    mature_dividend = "成熟配息",
    cyclical_volatile = "景氣循環／高波動",
    asset_light = "輕資產高毛利",
    fallback = "資料受限"
  )
  en <- c(
    holding_asset = "Holding / Asset-led",
    financial_book = "Financial / Book-driven",
    growth = "High growth",
    capital_intensive = "Capital-intensive",
    mature_dividend = "Mature dividend",
    cyclical_volatile = "Cyclical / Volatile",
    asset_light = "Asset-light / High margin",
    fallback = "Data-limited"
  )
  if (identical(as.character(locale)[1], "en") || identical(as.character(locale)[1], "en-US")) {
    return(unname(en[[id]] %||% id))
  }
  unname(zh[[id]] %||% id)
}

#' 屬性 why 短句 fallback（locale 優先於 ui_str）
.fundamental_profile_why_fallback <- function(profile_id, locale = "zh-TW") {
  id <- as.character(profile_id %||% "fallback")[1]
  zh <- c(
    holding_asset = "以帳面淨資產與槓桿為錨；交叉閱讀 ROA／ROE。",
    financial_book = "資本與淨值為尺；優先 ROE／ROA／槓桿，並對照 PE／Beta。",
    growth = "成長動能優先：營收／毛利成長、毛利率與營業現金成長。",
    capital_intensive = "重資產：看 ROA、資產週轉、現金品質與毛利率。",
    mature_dividend = "配息穩定：淨利率、ROE、OCF／淨利與 Yield／PE。",
    cyclical_volatile = "波動高：先看槓桿與現金品質，再讀毛利率與成長。",
    asset_light = "輕資產：毛利率、費用比、成長與 ROE。",
    fallback = "資料不足；請先讀完整 Annotation 表，勿僅依單一數字。"
  )
  en <- c(
    holding_asset = "Anchor on book NAV and leverage; cross-read ROA/ROE.",
    financial_book = "Capital and book value matter; prioritize ROE/ROA/leverage plus PE/Beta.",
    growth = "Prioritize growth: revenue/GP growth, GPM, and operating CF growth.",
    capital_intensive = "Heavy assets: ROA, asset turnover, cash quality, and GPM.",
    mature_dividend = "Stable payout: NPM, ROE, OCF/NI, plus Yield/PE.",
    cyclical_volatile = "High volatility: leverage and cash quality first, then margins/growth.",
    asset_light = "Light assets: GPM, opex ratio, growth, and ROE.",
    fallback = "Limited data; use the full Annotation guide, not a single number."
  )
  if (identical(as.character(locale)[1], "en") || identical(as.character(locale)[1], "en-US")) {
    return(unname(en[[id]] %||% ""))
  }
  unname(zh[[id]] %||% "")
}

#' @return character vector of focus metric ids (may be empty)
profile_focus_metrics <- function(profile_id) {
  id <- as.character(profile_id %||% "fallback")[1]
  mets <- PROFILE_FOCUS_METRICS[[id]]
  if (is.null(mets)) return(character(0))
  as.character(mets)
}

#' @return logical
is_profile_focus_metric <- function(profile_id, metric_id) {
  mid <- as.character(metric_id %||% "")[1]
  if (!nzchar(mid)) return(FALSE)
  mid %in% profile_focus_metrics(profile_id)
}

#' FS Yahoo item name → focus metric id（無對應則 NA）
fs_item_to_focus_metric <- function(item) {
  it <- as.character(item %||% "")[1]
  if (!nzchar(it)) return(NA_character_)
  if (grepl("^PE Ratio", it, ignore.case = TRUE)) return("pe_ratio")
  if (grepl("^Yield$", it, ignore.case = TRUE) || grepl("Dividend Yield", it, ignore.case = TRUE)) {
    return("dividend_yield")
  }
  if (grepl("^Beta", it, ignore.case = TRUE)) return("beta")
  if (grepl("^Dividend$", it, ignore.case = TRUE)) return("dividend_yield")
  NA_character_
}

#' Annotation 列 metric → focus id（毛利成長等用專用 id）
.annotation_row_focus_id <- function(band_metric, label_zh) {
  lab <- as.character(label_zh %||% "")[1]
  if (identical(lab, "毛利成長率")) return("gross_profit_growth")
  if (identical(lab, "營業現金成長")) return("op_cash_flow_growth")
  if (identical(lab, "投資現金成長")) return("inv_cash_flow_growth")
  if (identical(lab, "籌資現金成長")) return("fin_cash_flow_growth")
  if (identical(lab, "資產週轉率")) return("asset_turnover")
  if (identical(lab, "OCF／淨利") || identical(lab, "OCF/淨利")) return("ocf_net_income")
  bm <- as.character(band_metric %||% "")[1]
  if (nzchar(bm) && !is.na(bm)) return(bm)
  NA_character_
}

#' 琥珀色「屬性重視」標記 HTML（數字旁）
.ynow_focus_metric_mark <- function(title = NULL) {
  tip <- as.character(title %||% "")[1]
  if (!nzchar(tip)) tip <- "本財報屬性關鍵指標"
  tags$span(
    class = "ynow-focus-metric-dot",
    title = tip,
    `aria-label` = tip
  )
}

#' 依三大報表規則分群財報屬性（優先序：先命中先歸類）
#'
#' @return list(profile, signals, reason, focus_metrics)
classify_fundamental_profile <- function(d_cf = NULL, d_is = NULL, d_bs = NULL,
                                         industry_text = "", industry_choice = NULL) {
  thr <- .FUNDAMENTAL_PROFILE_THRESH
  empty <- list(
    profile = "fallback",
    signals = list(),
    reason = "資料不足，暫不標示屬性重視指標。",
    focus_metrics = character(0)
  )

  has_cf <- !is.null(d_cf) && is.data.frame(d_cf) && nrow(d_cf) > 0
  has_is <- !is.null(d_is) && is.data.frame(d_is) && nrow(d_is) > 0
  has_bs <- !is.null(d_bs) && is.data.frame(d_bs) && nrow(d_bs) > 0
  if (!isTRUE(has_is) && !isTRUE(has_cf)) return(empty)

  fcf_seq <- if (isTRUE(has_cf)) {
    tryCatch(select_clean_metric_row(d_cf, "Free Cash Flow", include_ttm = FALSE),
             error = function(e) NULL)
  } else {
    NULL
  }
  div_seq <- if (isTRUE(has_cf)) {
    tryCatch(select_clean_metric_row(d_cf, "Cash Dividends Paid", include_ttm = FALSE),
             error = function(e) NULL)
  } else {
    NULL
  }
  fcf_vals <- suppressWarnings(as.numeric(na.omit(fcf_seq)))
  fcf_cv <- if (length(fcf_vals) >= 2) {
    stats::sd(fcf_vals) / max(abs(mean(fcf_vals)), 1e-9)
  } else {
    NA_real_
  }
  is_fcf_pos <- length(fcf_vals) > 0 && isTRUE(mean(fcf_vals, na.rm = TRUE) > 0)
  is_fcf_stable <- isTRUE(is_fcf_pos) && (is.na(fcf_cv) || fcf_cv <= thr$fcf_cv_stable)

  div_vals <- abs(suppressWarnings(as.numeric(na.omit(div_seq))))
  div_cv <- if (length(div_vals) >= 2 && mean(div_vals) > 0) {
    stats::sd(div_vals) / max(abs(mean(div_vals)), 1e-9)
  } else {
    NA_real_
  }
  is_div <- length(div_vals) > 0 && isTRUE(mean(div_vals, na.rm = TRUE) > 0)
  is_div_stable <- isTRUE(is_div) && (is.na(div_cv) || div_cv <= thr$div_cv_stable)

  ind_txt <- as.character(industry_text %||% "")[1]
  ind_key <- as.character(industry_choice %||% "")[1]
  is_financial <- grepl(
    "Bank|Insurance|Financial|Conglomerate|fn\\.|Insurance Brokers",
    ind_txt, ignore.case = TRUE
  ) || grepl("^fn\\.", ind_key)
  is_holding <- grepl(
    "Conglomerate|Holding|Berkshire|fn\\.Conglomerate",
    paste(ind_txt, ind_key), ignore.case = TRUE
  ) || grepl("^fn\\.Conglomerate", ind_key)
  asset_or_book_driven <- isTRUE(is_financial) || grepl(
    "REIT|Real Estate|Asset|Bank|Insurance|Utility|Utilities",
    ind_txt, ignore.case = TRUE
  )

  rev_g <- if (isTRUE(has_is)) {
    tryCatch(
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)),
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  gp <- if (isTRUE(has_is)) {
    tryCatch(get_avg(select_clean_metric_row(d_is, "Gross Profit", include_ttm = FALSE)),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }
  rev <- if (isTRUE(has_is)) {
    tryCatch(get_avg(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE)),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }
  gpm <- if (is.finite(gp) && is.finite(rev) && rev != 0) gp / rev * 100 else NA_real_

  capex <- if (isTRUE(has_cf)) {
    tryCatch(
      abs(get_avg(select_clean_metric_row(d_cf, "Capital Expenditure", include_ttm = FALSE))),
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  capex_rev <- if (is.finite(capex) && is.finite(rev) && rev > 0) capex / rev else NA_real_

  assets <- if (isTRUE(has_bs)) {
    tryCatch(get_avg(select_clean_metric_row(d_bs, "Total Assets", include_ttm = FALSE)),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }
  asset_turnover <- if (is.finite(rev) && is.finite(assets) && assets != 0) rev / assets else NA_real_

  signals <- list(
    rev_g = rev_g, fcf_cv = fcf_cv, div_cv = div_cv,
    is_fcf_pos = is_fcf_pos, is_fcf_stable = is_fcf_stable,
    is_div_stable = is_div_stable, gpm = gpm, capex_rev = capex_rev,
    asset_turnover = asset_turnover,
    is_holding = is_holding, is_financial = is_financial,
    asset_or_book_driven = asset_or_book_driven
  )

  .out <- function(profile, reason) {
    list(
      profile = profile,
      signals = signals,
      reason = reason,
      focus_metrics = profile_focus_metrics(profile)
    )
  }

  # 1) Holding
  if (isTRUE(is_holding)) {
    return(.out("holding_asset", "控股／綜合：以帳面淨資產與槓桿為屬性錨。"))
  }

  # 2) Financial / book-driven
  if (isTRUE(asset_or_book_driven) || isTRUE(is_financial)) {
    return(.out("financial_book", "金融／帳面驅動：資本與淨值相關指標優先。"))
  }

  # 3) Growth
  is_growth <- (is.finite(rev_g) && rev_g > thr$rev_growth_high && isTRUE(is_fcf_pos)) ||
    (is.finite(rev_g) && rev_g > 15) ||
    (isTRUE(is_fcf_pos) && !isTRUE(is_fcf_stable) && is.finite(rev_g) && rev_g > thr$rev_growth_soft)
  if (isTRUE(is_growth)) {
    return(.out("growth", "高營收成長或成長期 FCF 仍波動：成長動能指標優先。"))
  }

  # 4) Capital-intensive
  is_capex_heavy <- (is.finite(capex_rev) && capex_rev >= thr$capex_rev_high) ||
    (is.finite(capex_rev) && capex_rev >= 0.08 &&
       is.finite(asset_turnover) && asset_turnover < thr$asset_turnover_low)
  if (isTRUE(is_capex_heavy)) {
    return(.out("capital_intensive", "CapEx／營收偏高或資產週轉偏低：資本密集指標優先。"))
  }

  # 5) Mature dividend
  if (isTRUE(is_div_stable) && !(is.finite(rev_g) && rev_g > thr$rev_growth_high)) {
    return(.out("mature_dividend", "配息穩定且非高成長：配息與獲利品質指標優先。"))
  }

  # 6) Cyclical / volatile FCF
  if (is.finite(fcf_cv) && fcf_cv >= thr$fcf_cv_volatile) {
    return(.out("cyclical_volatile", "FCF 波動偏高：槓桿與現金品質優先。"))
  }

  # 7) Asset-light high margin
  if (is.finite(gpm) && gpm >= thr$gpm_high &&
      (is.na(capex_rev) || capex_rev <= thr$capex_rev_low)) {
    return(.out("asset_light", "高毛利且 CapEx／營收偏低：輕資產效率指標優先。"))
  }

  # 8) Stable FCF mature (map to mature_dividend focus if div exists, else cyclical soft → asset_light-ish)
  if (isTRUE(is_fcf_stable)) {
    if (isTRUE(is_div)) {
      return(.out("mature_dividend", "FCF 穩定且有配息：成熟配息／獲利品質優先。"))
    }
    return(.out("asset_light", "FCF 穩定：以獲利結構與效率指標綜合判斷。"))
  }

  .out("fallback", "配息與 FCF 訊號不足：暫不強制標示屬性重視點。")
}
