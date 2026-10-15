# ==========================================
# lab_industry_method.R — 實驗區：產業 × 評價方法 × 績優候選
#
# 宇宙依市場模式：美股＝Nasdaq／NYSE 主要上市（lab_us_universe.R；S&P GICS 疊加）；台股＝上市／上櫃／興櫃（lab_tw_universe.R；績優候選不含興櫃）。
# 績優原則：在 Piotroski 高門檻（F-Score≥7；不含盈餘品質）後，選「模型合理價相對現價」、
# 並依各標的財報推算之預測年數 n（生命週期建議；非一體適用 APP_DEFAULTS$years）換算年化漲幅最大者。
# 「宇宙檔數（N）」（lab_im_max_n；預設 25）＝分析後明細／排行最終顯示上限（非 Yahoo 撈取檔數）。
# 撈取／評估檔數＝lab_resolve_im_eval_n(N)（通常大於 N）；明細＝合格池取前 N，不足不湊滿。
# 盈餘品質／Piotroski 高門檻勾選套用於排行榜與明細顯示池（同一合格定義）。
# 流程：宇宙池先依「候選截斷邏輯」全市排序／篩選（市值／概念股／近一年漲幅／隨機），
# 再取評估檔數 eval_n 的前段 → Yahoo 評估 → 合格者最多顯示 N 檔。
# 市值模式：對整池排序後再取 eval_n（已有市值時不做 S&P 預篩，避免打亂排序）。
# 排行榜＝同一批合格者的 Top 10（預設 F-Score≥7；gate_only=FALSE 時不設 F 門檻；不足 10 不湊滿）。
# 產業建議方法對齊 recommend_valuation_models 的產業層規則（簡化估值）。
# ==========================================

# Canonical sidebar valuation order (top → bottom in ynow_ui.R):
# NAV → Justified P/B → DCF → DDM → RI → Enterprise → Earnings → Revenue → Book multiples → SOTP
# Target P/B source decides taxonomy: Justified → Absolute Fair Value (`pb`);
# industry / history trading multiples → Relative Implied Price (`book`).
LAB_SIDEBAR_METHOD_ORDER <- c(
  "nav", "pb", "dcf", "ddm", "ri", "enterprise", "earnings", "revenue", "book", "sotp"
)

# Blue Chip Models filter UI groups (絕對 / Absolute vs 相對 / Relative).
# Absolute = Fair Value / intrinsic (+ Justified P/B). SOTP is Meta-Framework —
# kept in sidebar valuation order but not offered in this Absolute multi-select.
# Relative = Multiples families (peer / history Book multiples stay here).
LAB_METHOD_ABS_KEYS <- c("nav", "pb", "dcf", "ddm", "ri")
LAB_METHOD_REL_KEYS <- c("enterprise", "earnings", "revenue", "book")

LAB_METHOD_LABELS <- c(
  nav = "NAV（帳面控股淨資產）",
  dcf = "DCF（現金流折現）",
  ddm = "DDM（股利折現）",
  ri  = "RI（剩餘收益）",
  pb  = "Justified P/B（帳面／資產型 Fair Value）",
  book = "淨值相對倍數（產業／歷史 P/B）",
  enterprise = "企業價值乘數（EV/EBITDA · EV/EBIT · EV/FCF）",
  earnings = "盈餘乘數（P/E · Forward P/E · PEG）",
  revenue = "營收乘數（P/S · EV/Sales · EV/ARR）",
  multiples = "Multiples（市場倍數 Implied Price）",
  sotp = "SOTP（分部加總）"
)

LAB_METHOD_LABELS_EN <- c(
  nav = "NAV (book HoldCo net assets)",
  dcf = "DCF (discounted cash flow)",
  ddm = "DDM (dividend discount)",
  ri  = "RI (residual income)",
  pb  = "Justified P/B (Book / Asset Fair Value)",
  book = "Book multiples (industry / history P/B)",
  enterprise = "Enterprise value (EV/EBITDA · EV/EBIT · EV/FCF)",
  earnings = "Earnings (P/E · Forward P/E · PEG)",
  revenue = "Revenue (P/S · EV/Sales · EV/ARR)",
  multiples = "Multiples (market Implied Price)",
  sotp = "SOTP (sum of the parts)"
)

#' Industry-level Multiples reference family (no per-name statements).
#' Book businesses stay on P/B. Cyclical / capital-intensive names use enterprise
#' value. Pre-profit growth uses revenue. Everything else uses earnings.
lab_default_rel_family <- function(industry_key, primary = NA_character_) {
  k <- as.character(industry_key %||% "")[1]
  p <- as.character(primary %||% "")[1]
  if (identical(p, "pb") || grepl("^fn\\.", k) || k %in% c("re.REIT", "en.Utilities")) {
    return("book")
  }
  if (k %in% c("hc.Biotech", "auto.EV_Startups", "auto.Automotive_EV")) return("revenue")
  if (exists("is_cyclical_industry", mode = "function") &&
      isTRUE(is_cyclical_industry(k, ""))) {
    return("enterprise")
  }
  if (k %in% c("sc.Foundry", "sc.Equipment", "ind.Construction", "tel.Telecom")) {
    return("enterprise")
  }
  "earnings"
}

#' Localized primary-method display labels (sidebar order keys).
lab_method_labels <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (identical(loc, "en")) LAB_METHOD_LABELS_EN else LAB_METHOD_LABELS
}

#' DataTables column render: header clicks sort percents, market-cap suffixes,
#' and plain numbers numerically; em dashes sort as missing.
ynow_dt_sort_column_def <- function() {
  list(
    targets = "_all",
    render = DT::JS(
      "function(data, type) {",
      "  if (type !== 'sort' && type !== 'type' && type !== 'filter') return data;",
      "  var s = data == null ? '' : String(data);",
      "  s = s.replace(/<[^>]*>/g, '').replace(/&nbsp;/gi, ' ').replace(/\\s+/g, ' ').trim();",
      "  if (type === 'filter') return s;",
      "  if (!s || s === '\\u2014' || s === '\\u2013' || s === '-') return null;",
      "  var pct = s.match(/^([+-]?\\d+(?:\\.\\d+)?)%$/);",
      "  if (pct) return parseFloat(pct[1]);",
      "  var mag = s.match(/^([+-]?\\d+(?:\\.\\d+)?)([TBMK])$/i);",
      "  if (mag) {",
      "    var mul = {K:1e3, M:1e6, B:1e9, T:1e12}[mag[2].toUpperCase()];",
      "    return parseFloat(mag[1]) * mul;",
      "  }",
      "  var sci = s.replace(/,/g, '');",
      "  if (/^[+-]?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?$/.test(sci)) return parseFloat(sci);",
      "  return s.toLowerCase();",
      "}"
    )
  )
}

#' Blue Chip table with clickable headers. Initial row order is unchanged
#' until the user sorts a column.
ynow_sortable_datatable <- function(data, escape = TRUE, page_length = NULL,
                                    filter = "none", extra_options = list()) {
  if (is.null(data) || !is.data.frame(data)) {
    data <- data.frame(Message = character(0), stringsAsFactors = FALSE)
  }
  n <- nrow(data)
  plen <- if (is.null(page_length)) max(as.integer(n), 1L) else as.integer(page_length)[1]
  if (!is.finite(plen) || plen < 1L) plen <- 1L
  base <- list(
    paging = isTRUE(n > plen),
    pageLength = plen,
    scrollX = TRUE,
    ordering = TRUE,
    order = list(),
    orderClasses = TRUE,
    deferRender = TRUE,
    dom = "t"
  )
  opts <- if (length(extra_options)) utils::modifyList(base, extra_options) else base
  opts$ordering <- TRUE
  prev_defs <- opts$columnDefs
  opts$columnDefs <- c(list(ynow_dt_sort_column_def()), prev_defs)
  DT::datatable(
    data,
    rownames = FALSE,
    escape = escape,
    filter = filter,
    class = "stripe hover cell-border ynow-sortable-dt",
    options = opts
  )
}

#' Rename Blue Chip table column headers for the active UI locale.
lab_localize_im_colnames <- function(df, locale = "zh-TW") {
  if (is.null(df) || !is.data.frame(df)) return(df)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (!identical(loc, "en")) return(df)
  map <- c(
    "訊息" = "Message",
    "建議評價方法" = "Suggested valuation method",
    "產業數" = "Industries",
    "候選檔數" = "Candidates",
    "排名" = "Rank",
    "產業內排名" = "Industry rank",
    "產業" = "Industry",
    "產業鏈" = "Industry chain",
    "市值加權年化估值漲幅" = "Mcap-weighted annualized upside",
    "總市值" = "Total market cap",
    "檔數" = "Names",
    "代號" = "Ticker",
    "公司名稱" = "Company",
    "年化估值漲幅" = "Annualized upside",
    "總潛在漲幅" = "Total upside",
    "實際估值方法" = "Model used",
    "估值方法" = "Model",
    "現價" = "Price",
    "合理價" = "Fair value"
  )
  nm <- names(df)
  hit <- nm %in% names(map)
  if (any(hit)) nm[hit] <- unname(map[nm[hit]])
  names(df) <- nm
  df
}

#' Sort method keys to sidebar top→bottom order; unknown keys last (stable by name).
lab_order_methods_like_sidebar <- function(methods) {
  methods <- unique(as.character(methods %||% character(0)))
  methods <- methods[nzchar(methods) & !is.na(methods)]
  if (!length(methods)) return(character(0))
  ord <- match(methods, LAB_SIDEBAR_METHOD_ORDER)
  unknown <- is.na(ord)
  if (any(unknown)) {
    ord[unknown] <- length(LAB_SIDEBAR_METHOD_ORDER) +
      rank(methods[unknown], ties.method = "first")
  }
  methods[order(ord)]
}

# 目錄代碼 → 公司全稱（評估前即可顯示；Yahoo 名稱可覆寫）
LAB_TICKER_NAMES <- c(
  AVGO = "Broadcom Inc.", AMD = "Advanced Micro Devices, Inc.",
  QCOM = "QUALCOMM Incorporated", NVDA = "NVIDIA Corporation",
  TSM = "Taiwan Semiconductor Manufacturing Company Limited",
  AMKR = "Amkor Technology, Inc.", MU = "Micron Technology, Inc.",
  AMAT = "Applied Materials, Inc.", LRCX = "Lam Research Corporation",
  KLAC = "KLA Corporation", ASML = "ASML Holding N.V.",
  MSFT = "Microsoft Corporation", ORCL = "Oracle Corporation",
  ADBE = "Adobe Inc.", CRM = "Salesforce, Inc.",
  NOW = "ServiceNow, Inc.", SNOW = "Snowflake Inc.",
  GOOGL = "Alphabet Inc.", META = "Meta Platforms, Inc.",
  AMZN = "Amazon.com, Inc.", AAPL = "Apple Inc.",
  TXN = "Texas Instruments Incorporated", ADI = "Analog Devices, Inc.",
  JPM = "JPMorgan Chase & Co.", BAC = "Bank of America Corporation",
  WFC = "Wells Fargo & Company", GS = "The Goldman Sachs Group, Inc.",
  MS = "Morgan Stanley", PGR = "The Progressive Corporation",
  CB = "Chubb Limited", AIG = "American International Group, Inc.",
  BLK = "BlackRock, Inc.", BX = "Blackstone Inc.",
  V = "Visa Inc.", MA = "Mastercard Incorporated",
  SQ = "Block, Inc.", `BRK-B` = "Berkshire Hathaway Inc.",
  SHOP = "Shopify Inc.", COST = "Costco Wholesale Corporation",
  WMT = "Walmart Inc.", HD = "The Home Depot, Inc.",
  PEP = "PepsiCo, Inc.", KO = "The Coca-Cola Company",
  PG = "The Procter & Gamble Company", CL = "Colgate-Palmolive Company",
  EL = "The Estée Lauder Companies Inc.", LULU = "lululemon athletica inc.",
  TPR = "Tapestry, Inc.", NKE = "NIKE, Inc.",
  SBUX = "Starbucks Corporation", GM = "General Motors Company",
  F = "Ford Motor Company", TSLA = "Tesla, Inc.",
  APTV = "Aptiv PLC", BWA = "BorgWarner Inc.",
  RIVN = "Rivian Automotive, Inc.", LCID = "Lucid Group, Inc.",
  UNH = "UnitedHealth Group Incorporated", CI = "The Cigna Group",
  LLY = "Eli Lilly and Company", JNJ = "Johnson & Johnson",
  MRK = "Merck & Co., Inc.", PFE = "Pfizer Inc.",
  ABT = "Abbott Laboratories", MDT = "Medtronic plc",
  ISRG = "Intuitive Surgical, Inc.", VRTX = "Vertex Pharmaceuticals Incorporated",
  REGN = "Regeneron Pharmaceuticals, Inc.", AMGN = "Amgen Inc.",
  CAT = "Caterpillar Inc.", DE = "Deere & Company",
  RTX = "RTX Corporation", LMT = "Lockheed Martin Corporation",
  BA = "The Boeing Company", VMC = "Vulcan Materials Company",
  MLM = "Martin Marietta Materials, Inc.", LIN = "Linde plc",
  APD = "Air Products and Chemicals, Inc.", FCX = "Freeport-McMoRan Inc.",
  NEM = "Newmont Corporation", XOM = "Exxon Mobil Corporation",
  CVX = "Chevron Corporation", NEE = "NextEra Energy, Inc.",
  DUK = "Duke Energy Corporation", ENPH = "Enphase Energy, Inc.",
  FSLR = "First Solar, Inc.", VZ = "Verizon Communications Inc.",
  T = "AT&T Inc.", UNP = "Union Pacific Corporation",
  UPS = "United Parcel Service, Inc.", FDX = "FedEx Corporation",
  DAL = "Delta Air Lines, Inc.", UAL = "United Airlines Holdings, Inc.",
  PLD = "Prologis, Inc.", AMT = "American Tower Corporation",
  O = "Realty Income Corporation", DIS = "The Walt Disney Company",
  NFLX = "Netflix, Inc.", RBLX = "Roblox Corporation",
  EA = "Electronic Arts Inc.", MAR = "Marriott International, Inc.",
  BKNG = "Booking Holdings Inc.", ABNB = "Airbnb, Inc."
)

#' 公司全稱：Yahoo 名稱優先，否則 S&P 宇宙／目錄對照
lab_company_display_name <- function(ticker, yahoo_name = NULL) {
  raw_tk <- trimws(as.character(ticker %||% "")[1])
  tk <- toupper(gsub("\\.", "-", raw_tk))
  # 台股：優先宇宙中文法定全稱（OpenAPI／MOPS CSV），再退 Yahoo 簡稱／英文
  look_syms <- unique(c(
    raw_tk,
    gsub("-", ".", tk, fixed = TRUE),
    if (grepl("-(TW|TWO)$", tk)) gsub("-", ".", tk, fixed = TRUE) else character(0)
  ))
  if (exists("lookup_tw_universe_company_name", mode = "function") &&
      (grepl("\\.(TW|TWO)$", raw_tk, ignore.case = TRUE) ||
       grepl("-(TW|TWO)$", tk) ||
       grepl("^[0-9]{4,6}[A-Z]?$", toupper(raw_tk)))) {
    for (sym in look_syms) {
      uni <- tryCatch(
        lookup_tw_universe_company_name(sym),
        error = function(e) ""
      )
      uni <- trimws(as.character(uni %||% "")[1])
      if (nzchar(uni) && !identical(uni, "NA") &&
          !identical(toupper(uni), tk) &&
          !identical(toupper(uni), toupper(raw_tk))) {
        return(uni)
      }
    }
  }
  yn <- trimws(as.character(yahoo_name %||% "")[1])
  if (is.na(yn)) yn <- ""
  if (nzchar(yn) && !identical(toupper(yn), tk)) return(yn)
  sp <- tryCatch(lab_sp500_company_name(tk), error = function(e) NA_character_)
  if (is.character(sp) && length(sp) && nzchar(sp[1]) && !is.na(sp[1])) return(sp[1])
  us <- tryCatch(lab_us_company_name(tk), error = function(e) NA_character_)
  if (is.character(us) && length(us) && nzchar(us[1]) && !is.na(us[1])) return(us[1])
  nm <- unname(LAB_TICKER_NAMES[tk])
  if (!is.na(nm) && nzchar(nm)) return(as.character(nm)[1])
  if (nzchar(yn)) return(yn)
  "—"
}

#' App 預設預測年數（僅作缺財報／缺生命週期時的後備）
lab_model_horizon_years <- function() {
  n <- tryCatch(
    suppressWarnings(as.integer(APP_DEFAULTS$years %||% 5L)[1]),
    error = function(e) 5L
  )
  if (!is.finite(n) || n < 1L) 5L else as.integer(n)
}

#' 總潛在漲幅 % → 依 n 年換算年化 %
lab_annualized_upside_pct <- function(fv, price, n_years = NULL) {
  fv <- suppressWarnings(as.numeric(fv)[1])
  px <- suppressWarnings(as.numeric(price)[1])
  n <- suppressWarnings(as.numeric(n_years %||% lab_model_horizon_years())[1])
  if (!is.finite(fv) || !is.finite(px) || px <= 0 || fv <= 0) return(NA_real_)
  if (!is.finite(n) || n < 1) n <- 5
  ((fv / px)^(1 / n) - 1) * 100
}

#' 自 Yahoo Summary 取市值、現價、β 與公司全稱
lab_fetch_summary_metrics <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(
    market_cap = NA_real_, price = NA_real_, beta = NA_real_,
    company_name = NA_character_
  )
  if (!nzchar(tk)) return(out)
  # Prefer fast-tier memoised summary (15m); fall back to uncached only if missing.
  sum_df <- tryCatch({
    if (exists("cached_get_summary_data", mode = "function")) {
      cached_get_summary_data(tk)
    } else {
      get_summary_data(tk)
    }
  }, error = function(e) NULL)
  if (is.null(sum_df) || !is.data.frame(sum_df) || nrow(sum_df) == 0) {
    return(out)
  }
  idx_m <- grep("^Market Cap \\(intraday\\)$|^Market Cap$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_m) > 0) {
    out$market_cap <- parse_financial_number(sum_df$Value[idx_m[1]])[1]
  }
  idx_p <- grep("^Previous Close$|^Market Price$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_p) > 0) {
    out$price <- parse_financial_number(sum_df$Value[idx_p[1]])[1]
  }
  idx_b <- grep("^Beta \\(5Y Monthly\\)$|^Beta$", sum_df$Item, ignore.case = TRUE)
  if (length(idx_b) > 0) {
    out$beta <- parse_financial_number(sum_df$Value[idx_b[1]])[1]
  }
  cname <- tryCatch(attr(sum_df, "company_name"), error = function(e) NULL)
  cname <- trimws(as.character(cname %||% "")[1])
  if (nzchar(cname)) out$company_name <- cname
  out
}

#' 依個股最新財報／β／生命週期推算實驗區 n、Ke、終值 g、近端 g1（勿一體適用 App 預設）
#' @return list(n_years, ke_pct, g_pct, g1_pct, beta, beta_source, rf_pct, rm_pct,
#'   lifecycle_stage, fund_sgr_pct, rev_cagr_pct)
lab_resolve_ticker_params <- function(d_is, d_bs, d_cf,
                                      industry_key = "",
                                      beta = NA_real_,
                                      rf_pct = NA_real_,
                                      rm_pct = NA_real_,
                                      ticker = "") {
  key <- as.character(industry_key %||% "")[1]
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  fallback_n <- lab_model_horizon_years()
  fallback_ke <- tryCatch(lab_industry_ke_pct(key), error = function(e) 9)
  if (!is.finite(fallback_ke) || fallback_ke <= 0) fallback_ke <- 9
  fallback_g <- suppressWarnings(as.numeric(APP_DEFAULTS$sgr %||% 4)[1])
  if (!is.finite(fallback_g)) fallback_g <- 4
  fallback_g1 <- suppressWarnings(as.numeric(APP_DEFAULTS$custom_g %||% fallback_g)[1])
  if (!is.finite(fallback_g1)) fallback_g1 <- fallback_g

  rf <- suppressWarnings(as.numeric(rf_pct)[1])
  if (!is.finite(rf)) {
    rf <- tryCatch({
      if (exists("cached_get_risk_free_rate", mode = "function")) {
        suppressWarnings(as.numeric(cached_get_risk_free_rate())[1])
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }
  if (!is.finite(rf)) {
    rf <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rf %||% 4)[1])
  }
  if (!is.finite(rf)) rf <- 4

  b <- suppressWarnings(as.numeric(beta)[1])
  beta_src <- "ticker"
  if (!is.finite(b) || b <= 0) {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    b <- if (!is.null(ind) && !is.null(ind$beta_avg)) {
      suppressWarnings(as.numeric(ind$beta_avg)[1])
    } else {
      NA_real_
    }
    beta_src <- "industry"
  }
  if (!is.finite(b) || b <= 0) {
    b <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_beta %||% 1)[1])
    beta_src <- "default"
  }
  if (!is.finite(b) || b <= 0) {
    b <- 1
    beta_src <- "default"
  }

  rm <- suppressWarnings(as.numeric(rm_pct)[1])
  if (!is.finite(rm)) {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    rm <- if (!is.null(ind) && !is.null(ind$rm_avg)) {
      suppressWarnings(as.numeric(ind$rm_avg)[1])
    } else {
      NA_real_
    }
  }
  if (!is.finite(rm) || rm < rf + 2) {
    rm <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rm %||% (rf + 5))[1])
  }
  if (!is.finite(rm) || rm < rf + 2) rm <- rf + 5

  ke_pct <- rf + b * (rm - rf)
  if (!is.finite(ke_pct) || ke_pct <= 0) ke_pct <- fallback_ke

  life <- tryCatch({
    if (exists("classify_lifecycle_result", mode = "function")) {
      classify_lifecycle_result(
        industry_key = key,
        ticker = tk,
        d_is = d_is,
        d_bs = d_bs,
        d_cf = d_cf,
        wacc_pct = ke_pct,
        selected_stage = "auto",
        locale = "zh-TW"
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)
  stage <- if (!is.null(life)) {
    as.character(life$autoDetectedStage %||% NA_character_)[1]
  } else {
    NA_character_
  }
  n <- if (!is.null(life) && is.finite(suppressWarnings(as.numeric(life$recommendedForecastYears)[1]))) {
    as.integer(life$recommendedForecastYears)[1]
  } else if (exists("recommended_forecast_years_for_stage", mode = "function")) {
    suppressWarnings(as.integer(recommended_forecast_years_for_stage(stage))[1])
  } else {
    NA_integer_
  }
  if (!is.finite(n) || n < 1L) n <- fallback_n
  n <- as.integer(max(3L, min(15L, n)))

  fund_sgr <- tryCatch({
    if (exists("calc_fundamental_sgr_pct", mode = "function")) {
      calc_fundamental_sgr_pct(d_is, d_bs, d_cf)
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  long_cap <- tryCatch({
    if (exists("LIFECYCLE_CONFIG", inherits = TRUE)) {
      suppressWarnings(as.numeric(LIFECYCLE_CONFIG$terminal$long_run_nominal_g_cap_pct %||% 4.5)[1])
    } else {
      4.5
    }
  }, error = function(e) 4.5)
  if (!is.finite(long_cap) || long_cap <= 0) long_cap <- 4.5
  g_pct <- if (is.finite(fund_sgr)) fund_sgr else fallback_g
  g_ceil <- min(long_cap, ke_pct - 1.5)
  if (!is.finite(g_ceil) || g_ceil < 0.5) g_ceil <- 0.5
  g_pct <- max(0.5, min(g_pct, g_ceil))
  if (!is.finite(g_pct)) g_pct <- min(fallback_g, g_ceil)

  rev_cagr <- tryCatch({
    if (exists("get_avg_growth", mode = "function") &&
        exists("select_clean_metric_row", mode = "function") &&
        !is.null(d_is) && is.data.frame(d_is)) {
      get_avg_growth(select_clean_metric_row(d_is, "Total Revenue", include_ttm = FALSE))
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  g1_pct <- if (is.finite(rev_cagr)) rev_cagr else fallback_g1
  g1_pct <- max(1, min(g1_pct, 15))
  if (!is.finite(g1_pct)) g1_pct <- fallback_g1

  list(
    n_years = as.integer(n),
    ke_pct = round(as.numeric(ke_pct)[1], 2),
    g_pct = round(as.numeric(g_pct)[1], 2),
    g1_pct = round(as.numeric(g1_pct)[1], 2),
    beta = round(as.numeric(b)[1], 3),
    beta_source = as.character(beta_src)[1],
    rf_pct = round(as.numeric(rf)[1], 2),
    rm_pct = round(as.numeric(rm)[1], 2),
    lifecycle_stage = if (is.na(stage) || !nzchar(stage)) NA_character_ else stage,
    fund_sgr_pct = if (is.finite(fund_sgr)) round(fund_sgr, 2) else NA_real_,
    rev_cagr_pct = if (is.finite(rev_cagr)) round(rev_cagr, 2) else NA_real_
  )
}

#' 宇宙檔數 N："all"／"全部" → 不設上限；空／自訂未填 → 預設 25
#' 已解析的數值（含 Inf）原樣保留，避免二次 clamp 把 Inf 誤判成預設 25。
lab_parse_im_max_n <- function(x, default_n = 25L) {
  default_n <- max(1L, as.integer(default_n)[1])
  if (is.null(x) || !length(x) || (length(x) == 1L && is.na(x))) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  # Already-resolved numeric from a prior lab_resolve / lab_clamp call
  if (is.numeric(x) && !is.factor(x)) {
    n0 <- suppressWarnings(as.numeric(x)[1])
    if (length(n0) == 1L && !is.na(n0) && !is.finite(n0)) {
      return(list(n = Inf, label = "全部", unlimited = TRUE))
    }
    if (is.finite(n0) && n0 >= 1) {
      n_i <- as.integer(round(n0))
      return(list(n = n_i, label = as.character(n_i), unlimited = FALSE))
    }
  }
  s <- trimws(as.character(x)[1])
  if (!nzchar(s) || identical(s, "custom") || identical(s, "自訂")) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  if (identical(s, "all") || identical(s, "全部") ||
      identical(s, "Inf") || identical(tolower(s), "inf")) {
    return(list(n = Inf, label = "全部", unlimited = TRUE))
  }
  n <- suppressWarnings(as.integer(s)[1])
  if (!is.finite(n) || n < 1L) {
    return(list(n = default_n, label = as.character(default_n), unlimited = FALSE))
  }
  list(n = as.integer(n), label = as.character(n), unlimited = FALSE)
}

lab_im_max_n_label <- function(x) {
  lab_parse_im_max_n(x)$label
}

#' 相容舊呼叫：回傳整數上限，或 Inf 表示全部
lab_clamp_im_max_n <- function(x, default = 25L, lo = 1L, hi = NULL) {
  def_n <- if (is.finite(default)) as.integer(default)[1] else 25L
  parsed <- lab_parse_im_max_n(x, default_n = def_n)
  n <- parsed$n
  if (!is.finite(n)) return(Inf)
  n <- max(as.integer(lo)[1], n)
  if (!is.null(hi) && is.finite(hi)) n <- min(as.integer(hi)[1], n)
  as.integer(n)
}

#' Shared Evaluation count / Universe size (N) choices for Detail + Clustering
lab_im_max_n_select_choices <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (identical(loc, "en")) {
    c(
      "25 names" = "25",
      "50 names" = "50",
      "100 names" = "100",
      "200 names" = "200",
      "500 names" = "500",
      "All" = "all",
      "Custom…" = "custom"
    )
  } else {
    c(
      "25 檔" = "25",
      "50 檔" = "50",
      "100 檔" = "100",
      "200 檔" = "200",
      "500 檔" = "500",
      "全部" = "all",
      "自訂…" = "custom"
    )
  }
}

#' Resolve select + optional custom numeric into clampable max_n
lab_resolve_im_max_n <- function(sel, custom = NULL, lo = 1L, hi = 500L) {
  s <- if (is.null(sel) || !length(sel)) "" else trimws(as.character(sel)[1])
  if (identical(s, "custom") || identical(s, "自訂")) {
    return(lab_clamp_im_max_n(custom, default = 25L, lo = lo, hi = hi))
  }
  lab_clamp_im_max_n(sel, default = 25L, lo = lo, hi = hi)
}

#' Coerce display/eval N for lab_im_run_eval_job — preserve Inf ("全部").
#'
#' `as.integer(Inf)` is NA in R; that used to collapse universe N=全部 to the
#' default 25 via lab_clamp_im_max_n(NA). Keep Inf, coerce finite values to int.
lab_coerce_im_job_n <- function(x) {
  if (is.null(x) || !length(x)) return(NA_real_)
  if (is.numeric(x) && !is.factor(x)) {
    n0 <- suppressWarnings(as.numeric(x)[1])
    if (length(n0) == 1L && !is.na(n0) && !is.finite(n0)) return(Inf)
    if (is.finite(n0) && n0 >= 1) return(as.integer(round(n0)))
  }
  lab_clamp_im_max_n(x)
}

#' Filter a ticker data.frame to selected concept-group members (display path).
#'
#' Ranking / detail views call this when pool_rank == concept so the concept
#' picker stays linked even if scores were produced under a different truncate rule.
lab_filter_df_by_concept_keys <- function(df, concept_keys, market_mode = "US") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!"ticker" %in% names(df)) return(df)
  ckeys <- if (exists("lab_normalize_multi_filter", mode = "function")) {
    lab_normalize_multi_filter(concept_keys)
  } else {
    unique(as.character(unlist(concept_keys, use.names = FALSE)))
  }
  ckeys <- ckeys[!is.na(ckeys) & nzchar(ckeys)]
  if (!length(ckeys)) return(df)
  if (!exists("lab_concept_tickers", mode = "function")) return(df)
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")
  ctks <- lab_concept_tickers(ckeys, market = mm)
  if (!length(ctks)) return(df[FALSE, , drop = FALSE])
  tk <- toupper(trimws(as.character(df$ticker)))
  ctks_u <- unique(toupper(trimws(ctks)))
  bare <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
  keep <- tk %in% ctks_u |
    bare(tk) %in% bare(ctks_u) |
    gsub("\\.", "-", tk) %in% gsub("\\.", "-", ctks_u)
  df[keep, , drop = FALSE]
}

lab_resolve_im_max_n_label <- function(sel, custom = NULL) {
  s <- if (is.null(sel) || !length(sel)) "" else trimws(as.character(sel)[1])
  if (identical(s, "custom") || identical(s, "自訂")) {
    n <- lab_resolve_im_max_n(sel, custom)
    return(if (is.finite(n)) as.character(n) else "全部")
  }
  lab_im_max_n_label(sel)
}

#' Resolve Yahoo evaluation pool size from display N
#'
#' Universe size (N) is the **post-analysis display cap** (qualified rows), not
#' the fetch/eval count. Evaluation uses a larger ordered pool so enough names
#' may pass quality gates; detail/leaderboard then show at most N without padding.
#'
#' @param display_n finite integer or Inf ("全部")
#' @return Inf when display_n is unlimited; otherwise max(display_n, min(hi, max(display_n * 4, 100)))
lab_resolve_im_eval_n <- function(display_n, lo = 1L, hi = 500L) {
  if (is.null(display_n) || length(display_n) < 1L) return(100L)
  d <- suppressWarnings(as.numeric(display_n)[1])
  if (!is.finite(d)) return(Inf)
  d <- as.integer(max(as.integer(lo)[1], d))
  hi_i <- as.integer(hi)[1]
  if (!is.finite(hi_i) || hi_i < 1L) hi_i <- 500L
  eval_n <- max(d * 4L, 100L)
  eval_n <- max(d, min(hi_i, eval_n))
  as.integer(eval_n)
}

#' Cap detail rows from the qualified pool — never pad to fill display_n
#'
#' Qualified = lab_leaderboard_pool (finite annualized upside; optional F≥7 / EQ / no FS alerts).
#' Returns at most display_n rows; if fewer qualify, returns that shorter set.
lab_cap_detail_display <- function(merged_df, display_n,
                                   eq_only = FALSE, gate_only = TRUE,
                                   no_alert = FALSE) {
  pool <- lab_leaderboard_pool(
    merged_df,
    eq_only = eq_only,
    gate_only = gate_only,
    no_alert = no_alert
  )
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) {
    return(pool)
  }
  d <- suppressWarnings(as.numeric(display_n)[1])
  if (!is.finite(d) || d < 1) return(pool)
  utils::head(pool, as.integer(d))
}

lab_dedupe_eval_pool <- function(pool) {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0L) {
    return(pool)
  }
  keep <- !is.na(pool$ticker) & nzchar(as.character(pool$ticker))
  pool <- pool[keep, , drop = FALSE]
  if (nrow(pool) == 0L) return(pool)
  pool$ticker <- toupper(trimws(as.character(pool$ticker)))
  pool[!duplicated(pool$ticker), , drop = FALSE]
}

.lab_mcap_cache <- new.env(parent = emptyenv())
.lab_ret1y_cache <- new.env(parent = emptyenv())
.lab_universe_metrics_cache <- new.env(parent = emptyenv())

#' Path to bundled offline universe metrics (market_cap / ret_1y / paid_in_capital)
lab_universe_metrics_snapshot_path <- function() {
  opt <- getOption("ynow.universe_metrics_snapshot", default = NULL)
  if (!is.null(opt) && length(opt) >= 1L) {
    p <- as.character(opt)[1]
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  candidates <- c(
    file.path("data", "universe_metrics_snapshot.csv"),
    file.path("app_18.0", "data", "universe_metrics_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

#' Load offline universe metrics snapshot once per session
lab_load_universe_metrics_snapshot <- function(force = FALSE) {
  if (!isTRUE(force) &&
      exists("df", envir = .lab_universe_metrics_cache, inherits = FALSE)) {
    return(get("df", envir = .lab_universe_metrics_cache, inherits = FALSE))
  }
  path <- lab_universe_metrics_snapshot_path()
  empty <- data.frame(
    ticker = character(0),
    market_cap = numeric(0),
    ret_1y = numeric(0),
    paid_in_capital = numeric(0),
    stringsAsFactors = FALSE
  )
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    assign("df", empty, envir = .lab_universe_metrics_cache)
    attr(empty, "snapshot_source") <- "missing"
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L || !"ticker" %in% names(raw)) {
    assign("df", empty, envir = .lab_universe_metrics_cache)
    attr(empty, "snapshot_source") <- "empty"
    return(empty)
  }
  df <- data.frame(
    ticker = toupper(trimws(as.character(raw$ticker))),
    market_cap = if ("market_cap" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$market_cap))
    } else {
      NA_real_
    },
    ret_1y = if ("ret_1y" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$ret_1y))
    } else {
      NA_real_
    },
    paid_in_capital = if ("paid_in_capital" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$paid_in_capital))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  df <- df[nzchar(df$ticker) & !is.na(df$ticker), , drop = FALSE]
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "snapshot_source") <- path
  if ("snapshot_at" %in% names(raw)) {
    attr(df, "snapshot_at") <- as.character(raw$snapshot_at[[1]])
  }
  assign("df", df, envir = .lab_universe_metrics_cache)
  df
}

.lab_lookup_metric_vec <- function(tickers, col) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)
  snap <- tryCatch(lab_load_universe_metrics_snapshot(), error = function(e) NULL)
  if (is.null(snap) || !is.data.frame(snap) || nrow(snap) == 0L || !col %in% names(snap)) {
    return(out)
  }
  idx <- match(tks, snap$ticker)
  hit <- which(is.finite(idx))
  if (!length(hit)) return(out)
  vals <- suppressWarnings(as.numeric(snap[[col]][idx[hit]]))
  out[tks[hit]] <- vals
  # BRK.B / BRK-B style alts
  miss <- tks[is.na(out) | !is.finite(out)]
  if (length(miss)) {
    alts <- gsub("\\.", "-", miss)
    idx2 <- match(alts, gsub("\\.", "-", snap$ticker))
    hit2 <- which(is.finite(idx2))
    if (length(hit2)) {
      out[miss[hit2]] <- suppressWarnings(as.numeric(snap[[col]][idx2[hit2]]))
    }
  }
  out
}

#' 批次市值：離線快照優先，缺口再打 Yahoo；仍缺則 NA（呼叫端維持原宇宙順序）
lab_fetch_market_caps_usd <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)

  # 1) session cache
  need <- character(0)
  for (tk in tks) {
    if (exists(tk, envir = .lab_mcap_cache, inherits = FALSE)) {
      out[[tk]] <- suppressWarnings(as.numeric(get(tk, envir = .lab_mcap_cache, inherits = FALSE))[1])
    } else {
      need <- c(need, tk)
    }
  }

  # 2) bundled offline snapshot (full US/TW universe when shipped)
  if (length(need)) {
    snap_caps <- .lab_lookup_metric_vec(need, "market_cap")
    for (tk in need) {
      v <- suppressWarnings(as.numeric(snap_caps[[tk]])[1])
      if (is.finite(v) && v > 0) {
        out[[tk]] <- v
        assign(tk, v, envir = .lab_mcap_cache)
      }
    }
    need <- need[!(is.finite(out[need]) & out[need] > 0)]
  }

  if (!length(need)) return(out)

  # 3) live Yahoo batch for remaining gaps
  fetched <- NULL
  if (exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper()) &&
      exists("get_market_caps_batch", mode = "function")) {
    fetched <- tryCatch(get_market_caps_batch(need), error = function(e) NULL)
  }
  if (!is.null(fetched) && (is.list(fetched) || is.numeric(fetched))) {
    nms <- toupper(trimws(as.character(names(fetched))))
    vals <- suppressWarnings(as.numeric(unlist(fetched, use.names = FALSE)))
    if (length(nms) == length(vals) && length(nms) > 0) {
      for (i in seq_along(nms)) {
        key <- gsub("/", "-", nms[[i]])
        v <- vals[[i]]
        if (!nzchar(key)) next
        if (is.finite(v) && v > 0) {
          out[[key]] <- v
          assign(key, v, envir = .lab_mcap_cache)
          alt <- gsub("\\.", "-", key)
          if (!identical(alt, key) && alt %in% names(out) && !is.finite(out[[alt]])) {
            out[[alt]] <- v
            assign(alt, v, envir = .lab_mcap_cache)
          }
        }
      }
    }
  }
  out
}

#' 掛上市值，供候選 > N 時依市值降序截斷評估池（非規模篩選）
lab_attach_market_caps <- function(pool) {
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) return(pool)
  caps <- lab_fetch_market_caps_usd(pool$ticker)
  pool$market_cap <- unname(caps[pool$ticker])
  # TW 實收資本額（資本額）；不作為市值排序代理
  paid <- .lab_lookup_metric_vec(pool$ticker, "paid_in_capital")
  pool$paid_in_capital <- unname(paid[pool$ticker])
  pool
}

#' Pool truncate / rank modes when candidates > N
lab_im_pool_rank_choices <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  if (is_zh) {
    c(
      "依市值（由大到小）" = "mcap",
      "所選概念股全部" = "concept",
      "依近一年股價漲幅" = "ret_1y",
      "系統隨機" = "random"
    )
  } else {
    c(
      "By market cap (largest first)" = "mcap",
      "All selected concept groups" = "concept",
      "By 1Y price return" = "ret_1y",
      "System random" = "random"
    )
  }
}

lab_normalize_pool_rank_mode <- function(x, default = "mcap") {
  s <- tolower(trimws(as.character(x %||% default)[1]))
  if (!nzchar(s)) s <- default
  if (s %in% c("mcap", "market_cap", "cap", "市值")) return("mcap")
  if (s %in% c("concept", "concepts", "theme", "概念", "概念股")) return("concept")
  if (s %in% c("ret_1y", "return_1y", "1y", "漲幅", "近一年")) return("ret_1y")
  if (s %in% c("random", "rand", "隨機")) return("random")
  default
}

#' Batch 1Y total return (fraction): offline snapshot first, then Yahoo
lab_fetch_returns_1y <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers))))
  tks <- tks[nzchar(tks) & !is.na(tks)]
  out <- stats::setNames(rep(NA_real_, length(tks)), tks)
  if (!length(tks)) return(out)

  need <- character(0)
  for (tk in tks) {
    if (exists(tk, envir = .lab_ret1y_cache, inherits = FALSE)) {
      out[[tk]] <- suppressWarnings(as.numeric(get(tk, envir = .lab_ret1y_cache, inherits = FALSE))[1])
    } else {
      need <- c(need, tk)
    }
  }

  if (length(need)) {
    snap_rets <- .lab_lookup_metric_vec(need, "ret_1y")
    for (tk in need) {
      v <- suppressWarnings(as.numeric(snap_rets[[tk]])[1])
      if (is.finite(v)) {
        out[[tk]] <- v
        assign(tk, v, envir = .lab_ret1y_cache)
      }
    }
    need <- need[!(is.finite(out[need]))]
  }

  if (!length(need)) return(out)

  fetched <- NULL
  if (exists(".ensure_python_scraper", mode = "function") &&
      isTRUE(.ensure_python_scraper()) &&
      exists("get_returns_1y_batch", mode = "function")) {
    fetched <- tryCatch(get_returns_1y_batch(need), error = function(e) NULL)
  }
  if (!is.null(fetched) && (is.list(fetched) || is.numeric(fetched))) {
    nms <- toupper(trimws(as.character(names(fetched))))
    vals <- suppressWarnings(as.numeric(unlist(fetched, use.names = FALSE)))
    if (length(nms) == length(vals) && length(nms) > 0) {
      for (i in seq_along(nms)) {
        key <- gsub("/", "-", nms[[i]])
        v <- vals[[i]]
        if (!nzchar(key) || !is.finite(v)) next
        out[[key]] <- v
        assign(key, v, envir = .lab_ret1y_cache)
        alt <- gsub("\\.", "-", key)
        if (!identical(alt, key) && alt %in% names(out) && !is.finite(out[[alt]])) {
          out[[alt]] <- v
          assign(alt, v, envir = .lab_ret1y_cache)
        }
      }
    }
  }
  out
}

lab_attach_returns_1y <- function(pool) {
  pool <- lab_dedupe_eval_pool(pool)
  if (is.null(pool) || nrow(pool) == 0L) return(pool)
  rets <- lab_fetch_returns_1y(pool$ticker)
  pool$ret_1y <- unname(rets[pool$ticker])
  pool
}

#' 依市值降序取最多 max_n 檔（無市值置後，再依代碼）
#' 顯示檔數 N：截斷評估池用 eval_n；明細／排行自合格池取至多 N，不足不湊滿。
#' 不做市值分級／規模篩選。
lab_rank_and_cap_eval_pool <- function(pool, max_n = 25L) {
  lab_select_eval_pool(pool, max_n = max_n, mode = "mcap")
}

#' Select / truncate evaluation pool by user mode
#'
#' Semantic order (always):
#' 1. Apply Candidate truncate rule on the full filtered universe pool
#'    (sort by market cap / filter to selected concept groups / sort by 1Y return / random).
#' 2. Then take the first evaluation-count (eval_n) rows — typically larger than
#'    display N so enough names may pass quality gates after Yahoo scoring.
#'
#' Modes:
#' - mcap: sort entire pool by market cap (largest first), then take eval_n
#' - concept: keep selected concept-group tickers first; if still > eval_n, mcap within then take eval_n
#' - ret_1y: sort by 1Y price return (highest first), then take eval_n
#' - random: uniform random sample of eval_n
lab_select_eval_pool <- function(pool, max_n = 25L, mode = "mcap",
                                 concept_keys = NULL,
                                 market_mode = "US",
                                 seed = NULL) {
  pool <- lab_dedupe_eval_pool(pool)
  mode <- lab_normalize_pool_rank_mode(mode)
  if (is.null(pool) || !is.data.frame(pool)) {
    empty <- data.frame(ticker = character(0), stringsAsFactors = FALSE)
    attr(empty, "n_filtered") <- 0L
    attr(empty, "max_n") <- lab_clamp_im_max_n(max_n)
    attr(empty, "used_market_cap") <- FALSE
    attr(empty, "pool_rank_mode") <- mode
    attr(empty, "pool_rank_note") <- "empty"
    return(empty)
  }
  max_n <- lab_clamp_im_max_n(max_n)
  n_filtered <- nrow(pool)
  note <- mode
  used_mcap <- FALSE
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")

  .pool_has_usable_mcap <- function(df) {
    if (is.null(df) || !is.data.frame(df) || !"market_cap" %in% names(df)) return(FALSE)
    caps <- suppressWarnings(as.numeric(df$market_cap))
    isTRUE(sum(is.finite(caps) & caps > 0, na.rm = TRUE) > 0L)
  }

  # --- Step 1a: concept filter on the full pool (before any N cut) ---
  if (identical(mode, "concept")) {
    if (!exists("lab_concept_tickers", mode = "function")) {
      mode <- "mcap"
      note <- "concept_unavailable_fallback_mcap"
    } else {
      ckeys <- lab_normalize_multi_filter(concept_keys)
      ctks <- lab_concept_tickers(ckeys, market = mm)
      if (!length(ckeys) || !length(ctks)) {
        mode <- "mcap"
        note <- "concept_none_selected_fallback_mcap"
      } else {
        pool$ticker <- toupper(trimws(as.character(pool$ticker)))
        ctks_u <- unique(toupper(trimws(ctks)))
        bare <- function(x) sub("\\.(TW|TWO)$", "", x, ignore.case = TRUE)
        keep <- pool$ticker %in% ctks_u |
          bare(pool$ticker) %in% bare(ctks_u) |
          gsub("\\.", "-", pool$ticker) %in% gsub("\\.", "-", ctks_u)
        pool_c <- pool[keep, , drop = FALSE]
        if (nrow(pool_c) == 0L) {
          mode <- "mcap"
          note <- "concept_no_overlap_fallback_mcap"
        } else {
          pool <- pool_c
          note <- sprintf("concept_n=%d", nrow(pool))
        }
      }
    }
  }

  # Soft Yahoo-cost guard for 1Y returns only. Never pre-cut a pool that already
  # has market_cap (would break "sort all by mcap, then take N").
  if (identical(mode, "ret_1y") &&
      exists("lab_us_prescreen_eval_pool", mode = "function")) {
    pre_n <- if (exists("LAB_US_EVAL_PRESCREEN", inherits = TRUE)) {
      as.integer(LAB_US_EVAL_PRESCREEN)[1]
    } else {
      800L
    }
    if (is.finite(pre_n) && nrow(pool) > pre_n) {
      pool <- lab_us_prescreen_eval_pool(pool, max_n = pre_n)
      note <- paste0(as.character(note)[1], ";prescreen=", nrow(pool))
    }
  }

  if (identical(mode, "random")) {
    if (is.finite(max_n) && nrow(pool) > max_n) {
      if (!is.null(seed) && is.finite(as.numeric(seed)[1])) {
        set.seed(as.integer(seed)[1])
      }
      pool <- pool[sample.int(nrow(pool), size = as.integer(max_n)), , drop = FALSE]
      pool <- pool[order(pool$ticker), , drop = FALSE]
    }
    attr(pool, "n_filtered") <- as.integer(n_filtered)
    attr(pool, "max_n") <- max_n
    attr(pool, "used_market_cap") <- FALSE
    attr(pool, "pool_rank_mode") <- "random"
    attr(pool, "pool_rank_note") <- note
    return(pool)
  }

  if (identical(mode, "ret_1y")) {
    # Step 1b: rank entire (post-filter) pool by 1Y return
    pool <- lab_attach_returns_1y(pool)
    ret <- suppressWarnings(as.numeric(pool$ret_1y))
    missing <- is.na(ret) | !is.finite(ret)
    o <- order(missing, -ifelse(missing, 0, ret), pool$ticker, na.last = TRUE)
    pool <- pool[o, , drop = FALSE]
    # Step 2: take first N
    if (is.finite(max_n) && nrow(pool) > max_n) pool <- utils::head(pool, max_n)
    attr(pool, "n_filtered") <- as.integer(n_filtered)
    attr(pool, "max_n") <- max_n
    attr(pool, "used_market_cap") <- FALSE
    attr(pool, "pool_rank_mode") <- "ret_1y"
    attr(pool, "pool_rank_note") <- note
    return(pool)
  }

  # Default / concept-fallback / mcap-within-concept: market cap
  # Step 1b: attach caps to the FULL remaining pool, then sort all (no S&P pre-cut)
  if (!.pool_has_usable_mcap(pool)) {
    if (is.finite(max_n) && nrow(pool) > max_n) {
      pool <- lab_attach_market_caps(pool)
    } else if (!is.finite(max_n) || nrow(pool) <= max_n) {
      # Still attach when useful for display / stable ordering
      if (nrow(pool) > 0L && nrow(pool) <= 800L) {
        pool <- lab_attach_market_caps(pool)
      }
    }
  }
  if (!"market_cap" %in% names(pool)) pool$market_cap <- NA_real_
  mcap <- suppressWarnings(as.numeric(pool$market_cap))
  missing <- is.na(mcap) | !is.finite(mcap) | mcap <= 0
  used_mcap <- isTRUE(sum(!missing) > 0L)
  o <- order(missing, -ifelse(missing, 0, mcap), pool$ticker, na.last = TRUE)
  pool <- pool[o, , drop = FALSE]
  # Step 2: take first N from the sorted pool
  if (is.finite(max_n) && nrow(pool) > max_n) pool <- utils::head(pool, max_n)
  attr(pool, "n_filtered") <- as.integer(n_filtered)
  attr(pool, "max_n") <- max_n
  attr(pool, "used_market_cap") <- used_mcap
  attr(pool, "pool_rank_mode") <- "mcap"
  attr(pool, "pool_rank_note") <- note
  pool
}

#' 正規化複選篩選：NULL／空／含 "all" → character(0) 表示不過濾
lab_normalize_multi_filter <- function(x) {
  v <- unique(as.character(unlist(x, use.names = FALSE)))
  v <- v[nzchar(v) & !is.na(v) & v != "all"]
  v
}

# S&P 500（GICS）＋美股全市場主要上市宇宙；須在候選目錄／名稱查詢之前載入
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_adr.R", local = TRUE, encoding = "UTF-8")
source("lab_us_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_concept_groups.R", local = TRUE, encoding = "UTF-8")

#' 產業 CAPM Ke（%）
lab_industry_ke_pct <- function(industry_key) {
  ind <- tryCatch(industry_standards[[as.character(industry_key)[1]]], error = function(e) NULL)
  rf <- suppressWarnings(as.numeric(APP_DEFAULTS$capm_rf %||% 4)[1])
  if (!is.finite(rf)) rf <- 4
  beta <- if (!is.null(ind$beta_avg)) suppressWarnings(as.numeric(ind$beta_avg)[1]) else
    suppressWarnings(as.numeric(APP_DEFAULTS$capm_beta %||% 1)[1])
  if (!is.finite(beta)) beta <- 1
  rm <- if (!is.null(ind$rm_avg)) suppressWarnings(as.numeric(ind$rm_avg)[1]) else
    suppressWarnings(as.numeric(APP_DEFAULTS$capm_rm %||% (rf + 5))[1])
  if (!is.finite(rm) || rm < rf + 2) rm <- rf + 5
  rf + beta * (rm - rf)
}

#' 實驗區簡化合理價／股（對齊產業主方法；非完整 UI 估值引擎）
#' n／Ke／g／g1 預設由個股財報＋β＋生命週期推算（lab_resolve_ticker_params）；
#' 僅在缺財報時退回 App／產業預設。可傳 n_years／ke_pct／g_pct／g1_pct／beta 覆寫。
lab_estimate_fv_per_share <- function(method, industry_key, d_is, d_bs, d_cf,
                                      price = NA_real_, market_cap = NA_real_,
                                      beta = NA_real_, ticker = "",
                                      n_years = NULL, ke_pct = NULL,
                                      g_pct = NULL, g1_pct = NULL) {
  method <- as.character(method %||% "pb")[1]
  key <- as.character(industry_key %||% "")[1]
  params <- tryCatch(
    lab_resolve_ticker_params(
      d_is, d_bs, d_cf,
      industry_key = key,
      beta = beta,
      ticker = ticker
    ),
    error = function(e) NULL
  )
  n <- suppressWarnings(as.integer(n_years %||% params$n_years %||% lab_model_horizon_years())[1])
  if (!is.finite(n) || n < 1L) n <- lab_model_horizon_years()
  ke_pct <- suppressWarnings(as.numeric(ke_pct %||% params$ke_pct %||% lab_industry_ke_pct(key))[1])
  if (!is.finite(ke_pct) || ke_pct <= 0) ke_pct <- lab_industry_ke_pct(key)
  ke <- ke_pct / 100
  g_pct <- suppressWarnings(as.numeric(g_pct %||% params$g_pct %||% APP_DEFAULTS$sgr %||% 4)[1])
  if (!is.finite(g_pct)) g_pct <- 4
  g <- max(0.005, min(g_pct / 100, ke - 0.015))
  g1_pct <- suppressWarnings(as.numeric(g1_pct %||% params$g1_pct %||% APP_DEFAULTS$custom_g %||% g_pct)[1])
  if (!is.finite(g1_pct)) g1_pct <- g_pct
  g1 <- max(0.01, min(g1_pct / 100, 0.15))

  shares <- tryCatch(
    select_current_metric(d_bs, "Ordinary Shares Number|Share Issued|Total Shares Outstanding", "stock"),
    error = function(e) NA_real_
  )
  shares <- suppressWarnings(as.numeric(shares)[1])
  sh_res <- tryCatch(
    resolve_shares_for_price(
      shares,
      price = price,
      market_cap = market_cap,
      ticker = ""
    ),
    error = function(e) NULL
  )
  if (!is.null(sh_res) && is.finite(sh_res$shares) && sh_res$shares > 0) {
    shares <- sh_res$shares
  }
  if (!is.finite(shares) || shares <= 0) {
    px <- suppressWarnings(as.numeric(price)[1])
    mc <- suppressWarnings(as.numeric(market_cap)[1])
    if (is.finite(px) && px > 0 && is.finite(mc) && mc > 0) shares <- mc / px
  }
  if (!is.finite(shares) || shares <= 0) {
    return(list(fv = NA_real_, method = method, n_years = n, note = "無股數"))
  }

  equity <- tryCatch(
    select_current_metric_any(d_bs, EQUITY_PATTERNS, "stock"),
    error = function(e) NA_real_
  )
  equity <- suppressWarnings(as.numeric(equity)[1])
  bvps <- if (is.finite(equity) && equity > 0) equity / shares else NA_real_

  ni <- tryCatch(
    select_current_metric_any(d_is, NET_INCOME_PATTERNS, "flow"),
    error = function(e) NA_real_
  )
  ni <- suppressWarnings(as.numeric(ni)[1])
  roe <- if (is.finite(ni) && is.finite(equity) && equity > 0) ni / equity else NA_real_

  .pb_mid <- function() {
    ind <- tryCatch(industry_standards[[key]], error = function(e) NULL)
    band <- ind$pb_band
    if (!is.null(band) && length(band) >= 2) {
      if (length(band) >= 3) return(suppressWarnings(as.numeric(band[3])[1]))
      return(mean(suppressWarnings(as.numeric(band[1:2]))))
    }
    1.4
  }

  fv <- NA_real_
  note <- method

  if (identical(method, "pb")) {
    pb_m <- .pb_mid()
    # 可用 Justified 時與產業 mid 平均
    just <- NA_real_
    if (is.finite(roe) && is.finite(ke) && ke > g) {
      just <- max(0.3, min(8, (roe - g) / (ke - g)))
    }
    mult <- if (is.finite(just)) (0.55 * just + 0.45 * pb_m) else pb_m
    fv <- if (is.finite(bvps)) bvps * mult else NA_real_
    note <- "P/B×BVPS"
  } else if (identical(method, "ddm")) {
    div_tot <- tryCatch(
      abs(select_current_metric(d_cf, "Cash Dividends Paid", "flow")),
      error = function(e) NA_real_
    )
    div_tot <- suppressWarnings(as.numeric(div_tot)[1])
    dps <- if (is.finite(div_tot) && div_tot > 0) div_tot / shares else NA_real_
    if (is.finite(dps) && dps > 0 && ke > g) {
      fv <- dps * (1 + g) / (ke - g)
      note <- "Gordon DDM"
    } else {
      # 無股利 → 退回 P/B
      fv <- if (is.finite(bvps)) bvps * .pb_mid() else NA_real_
      note <- "DDM缺股利→P/B"
    }
  } else if (identical(method, "ri")) {
    if (is.finite(bvps) && is.finite(roe) && ke > g) {
      # 簡化：B0 + PV of perpetual residual income
      fv <- bvps + (roe - ke) * bvps / (ke - g)
      if (fv < 0) fv <- bvps
      note <- "簡化 RI"
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "RI缺ROE→P/B"
    }
  } else if (identical(method, "nav")) {
    disc <- suppressWarnings(as.numeric(APP_DEFAULTS$nav_holdco_discount %||% 0)[1])
    if (!is.finite(disc)) disc <- 0
    disc <- max(0, min(0.5, disc))
    navc <- tryCatch(
      extract_nav_components(d_bs, holdco_discount = disc),
      error = function(e) NULL
    )
    nav_tot <- if (!is.null(navc)) suppressWarnings(as.numeric(navc$nav)[1]) else NA_real_
    navps <- if (is.finite(nav_tot) && nav_tot > 0) nav_tot / shares else NA_real_
    if (!is.finite(navps) && is.finite(bvps) && bvps > 0) navps <- bvps
    mult <- suppressWarnings(as.numeric(APP_DEFAULTS$nav_mid %||% 1)[1])
    if (!is.finite(mult) || mult <= 0) mult <- 1
    if (is.finite(navps) && navps > 0) {
      fv <- navps * mult
      note <- if (!is.null(navc) && nzchar(as.character(navc$note %||% "")[1])) {
        paste0("簡化NAV×", sprintf("%.2f", mult), "（", as.character(navc$note)[1], "）")
      } else {
        paste0("簡化NAV×", sprintf("%.2f", mult))
      }
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "NAV缺權益→P/B"
    }
  } else if (identical(method, "multiples")) {
    # Simplified trading multiple: Trailing EPS × selected/default P/E (Implied Price)
    eps <- tryCatch(
      select_current_metric(d_is, "Diluted EPS|Basic EPS|^EPS$", "flow"),
      error = function(e) NA_real_
    )
    eps <- suppressWarnings(as.numeric(eps)[1])
    if (!is.finite(eps) && is.finite(ni) && is.finite(shares) && shares > 0) eps <- ni / shares
    pe_m <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_pe_multiple %||% 18)[1])
    if (!is.finite(pe_m) || pe_m <= 0) pe_m <- 18
    if (is.finite(eps) && eps > 0) {
      fv <- eps * pe_m
      note <- paste0("Implied P/E×", sprintf("%.1f", pe_m))
    } else {
      rev <- tryCatch(
        select_current_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue", "flow"),
        error = function(e) NA_real_
      )
      rev <- suppressWarnings(as.numeric(rev)[1])
      ps_m <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_ps_multiple %||% 3)[1])
      if (!is.finite(ps_m) || ps_m <= 0) ps_m <- 3
      if (is.finite(rev) && rev > 0 && is.finite(shares) && shares > 0) {
        fv <- (rev * ps_m) / shares
        note <- paste0("Implied P/S×", sprintf("%.1f", ps_m))
      } else if (is.finite(bvps)) {
        fv <- bvps * .pb_mid()
        note <- "Multiples缺EPS→P/B"
      }
    }
  } else if (identical(method, "sotp")) {
    # Lab simplified SOTP proxy: total revenue × EV/Sales − net debt (not true multi-segment)
    rev <- tryCatch(
      select_current_metric(d_is, "Total Revenue|^Revenue$|Operating Revenue", "flow"),
      error = function(e) NA_real_
    )
    rev <- suppressWarnings(as.numeric(rev)[1])
    evs <- suppressWarnings(as.numeric(APP_DEFAULTS$rel_ev_sales_multiple %||% 3)[1])
    if (!is.finite(evs) || evs <= 0) evs <- 3
    debt <- tryCatch(
      select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
      error = function(e) NA_real_
    )
    cash <- tryCatch(
      select_current_metric(d_bs, "Cash And Cash Equivalents|Cash Cash Equivalents And Short Term Investments", "stock"),
      error = function(e) NA_real_
    )
    debt <- suppressWarnings(as.numeric(debt)[1])
    cash <- suppressWarnings(as.numeric(cash)[1])
    if (is.finite(rev) && rev > 0 && is.finite(shares) && shares > 0) {
      ev <- rev * evs
      eq <- ev + (if (is.finite(cash)) cash else 0) - (if (is.finite(debt)) debt else 0)
      fv <- eq / shares
      note <- paste0("SOTP proxy Rev×EV/Sales ", sprintf("%.1f", evs), "×")
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "SOTP缺營收→P/B"
    }
  } else {
    # DCF：n 年顯式 FCF 成長＋終值（用 Ke 近似折現；實驗區簡化）
    fcf <- tryCatch(
      latest_hist_fcff(d_cf, d_is = d_is),
      error = function(e) NA_real_
    )
    fcf <- suppressWarnings(as.numeric(fcf)[1])
    r <- max(ke, g + 0.02)
    if (is.finite(fcf) && fcf > 0) {
      fcfs <- fcf * (1 + g1)^seq_len(n)
      dfs <- cumprod(rep(1 + r, n))
      pv_fcf <- sum(fcfs / dfs)
      tv <- fcfs[n] * (1 + g) / (r - g)
      enterprise <- pv_fcf + tv / dfs[n]
      debt <- tryCatch(
        select_current_metric(d_bs, "^Total Debt$|Long Term Debt", "stock"),
        error = function(e) NA_real_
      )
      cash <- tryCatch(
        select_current_metric(d_bs, "Cash And Cash Equivalents|Cash Cash Equivalents And Short Term Investments", "stock"),
        error = function(e) NA_real_
      )
      debt <- suppressWarnings(as.numeric(debt)[1])
      cash <- suppressWarnings(as.numeric(cash)[1])
      net_debt <- if (is.finite(debt)) debt - (if (is.finite(cash)) cash else 0) else 0
      equity_val <- enterprise - net_debt
      fv <- equity_val / shares
      note <- paste0("簡化DCF(", n, "年)")
    } else if (is.finite(bvps)) {
      fv <- bvps * .pb_mid()
      note <- "DCF缺FCF→P/B"
    }
  }

  list(
    fv = suppressWarnings(as.numeric(fv)[1]),
    method = method,
    n_years = as.integer(n),
    note = note,
    ke_pct = ke_pct,
    g_pct = g_pct,
    g1_pct = g1_pct,
    beta = if (!is.null(params)) params$beta else NA_real_,
    beta_source = if (!is.null(params)) params$beta_source else NA_character_,
    lifecycle_stage = if (!is.null(params)) params$lifecycle_stage else NA_character_,
    fund_sgr_pct = if (!is.null(params)) params$fund_sgr_pct else NA_real_,
    rev_cagr_pct = if (!is.null(params)) params$rev_cagr_pct else NA_real_
  )
}

#' 產業預設主／副評價方法（無個股財報時的規則映射）
#' @return data.frame: industry_key, primary, secondary, suggest_two_stage, rationale
lab_industry_method_defaults <- function() {
  rows <- list(
    list("tech.Optoelectronics", "dcf", "pb", FALSE, "光電／顯示循環 → DCF"),
    list("tech.Electronics_Distribution", "dcf", "pb", FALSE, "電子通路低毛利周轉 → DCF"),
    list("tech.IT_Services", "dcf", "pb", FALSE, "資訊服務 → DCF"),
    list("cons.Restaurants", "dcf", "ddm", FALSE, "餐飲現金流／成熟配息 → DCF"),
    list("cons.Home_Living", "dcf", "pb", FALSE, "居家生活消費 → DCF"),
    list("cons.Sports_Leisure", "dcf", "pb", FALSE, "運動休閒 → DCF"),
    list("ind.Conglomerate", "nav", "sotp", FALSE, "綜合／其他 → 純 NAV；SOTP 結構交叉"),
    list("mat.Textiles", "dcf", "pb", FALSE, "紡織纖維循環 → DCF"),
    list("mat.Paper_Packaging", "dcf", "pb", FALSE, "造紙包裝 → DCF"),
    list("mat.Glass_Ceramics", "dcf", "pb", FALSE, "玻璃陶瓷 → DCF"),
    list("en.Environmental", "dcf", "pb", TRUE, "綠能環保服務成長 → DCF"),
    list("ag.Agriculture", "dcf", "pb", FALSE, "農業科技 → DCF"),
    list("media.Advertising", "dcf", "pb", FALSE, "廣告行銷 → DCF"),
    list("bus.Professional_Services", "dcf", "pb", FALSE, "專業服務 → DCF"),

    # 金融／控股／REIT／公用 → P/B（+ RI）；控股主模型純 NAV
    list("fn.Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B；ROE 可時交叉 RI"),
    list("fn.Investment_Banking", "pb", "ri", FALSE, "金融簿價驅動 → P/B"),
    list("fn.Insurance", "pb", "ri", FALSE, "保險／帳面導向 → P/B"),
    list("fn.Asset_Management", "pb", "nav", FALSE, "資產管理偏帳面 → P/B；副選純 NAV"),
    list("fn.Fintech", "dcf", "pb", TRUE, "成長型金融科技 → 兩階段 DCF"),
    list("fn.Conglomerate_Holding", "nav", "sotp", FALSE, "控股／綜合企業 → 純 NAV；SOTP 結構交叉"),
    list("re.REIT", "pb", "nav", FALSE, "REIT 簿價／殖利率 → P/B；副選純 NAV"),
    list("en.Utilities", "pb", "ddm", FALSE, "公用事業簿價／管制資產 → P/B；高配息輔 DDM"),

    # 高成長科技／生技／EV → DCF two-stage
    # Multiples is a universal Reference (not a secondary candidate) — pair FV secondaries instead.
    list("saas.SaaS_Cloud", "dcf", "ri", TRUE, "SaaS／雲端成長 → 兩階段 DCF；副選 RI；Multiples 為參照"),
    list("tech.Internet_Platform", "dcf", "ri", TRUE, "網路平台成長 → 兩階段 DCF；副選 RI；Multiples 為參照"),
    list("tech.Software", "dcf", "ri", TRUE, "軟體成長／穩健 FCF → DCF；副選 RI；Multiples 為參照"),
    list("hc.Biotech", "dcf", "pb", TRUE, "生技早期成長 → 兩階段 DCF（風險高）"),
    list("auto.Automotive_EV", "dcf", "pb", TRUE, "電動車成長 → 兩階段 DCF"),
    list("auto.EV_Startups", "dcf", "pb", TRUE, "新創 EV → 兩階段 DCF"),
    list("en.Renewables", "dcf", "pb", TRUE, "再生能源成長 → 兩階段 DCF"),

    # 半導體／硬體
    list("sc.IC_Design", "dcf", "pb", TRUE, "IC 設計常具成長與 FCF → DCF"),
    list("sc.Foundry", "dcf", "pb", FALSE, "晶圓代工資本密集／景氣循環 → DCF；P/B 為副模型"),
    list("sc.Packaging", "dcf", "pb", FALSE, "封測循環／FCF → DCF"),
    list("sc.Memory", "dcf", "pb", FALSE, "記憶體循環股 → DCF（波動大）"),
    list("sc.Equipment", "dcf", "pb", TRUE, "設備材料成長循環 → DCF"),
    list("tech.Hardware", "dcf", "ddm", FALSE, "硬體成熟 FCF／配息 → DCF（穩健者可輔 DDM）"),
    list("ec.Hardware", "dcf", "pb", FALSE, "電子零組件 → DCF"),

    # 消費成熟配息傾向
    list("fmcg.Food_Beverages", "ddm", "dcf", FALSE, "食品飲料穩定配息 → DDM；FCF 穩則輔 DCF"),
    list("fmcg.Household_Personal", "ddm", "dcf", FALSE, "家用品／個護防禦配息 → DDM"),
    list("fmcg.Health_Beauty", "dcf", "ddm", FALSE, "健康美容穩健 FCF → DCF"),
    list("lxg.Luxury_Fashion", "dcf", "pb", FALSE, "精品現金流品質 → DCF"),
    list("ecr.Ecommerce_Retail", "dcf", "pb", TRUE, "電商成長 → DCF"),
    list("retail.Brick_Mortar", "dcf", "ddm", FALSE, "實體零售 → DCF／成熟配息者 DDM"),
    list("cons.Discretionary", "dcf", "pb", FALSE, "非必需消費 → DCF"),

    # 汽車／醫療成熟
    list("auto.Vehicle_Manufacturing", "dcf", "pb", FALSE, "整車製造循環 → DCF"),
    list("auto.Parts_Suppliers", "dcf", "pb", FALSE, "汽車零組件 → DCF"),
    list("hc.Healthcare_Services", "dcf", "pb", FALSE, "醫療服務穩定現金流 → DCF"),
    list("hc.Pharma", "dcf", "ddm", FALSE, "製藥成熟 FCF／配息 → DCF（可輔 DDM）"),
    list("hc.Medtech", "dcf", "pb", FALSE, "醫材 → DCF"),

    # 工業／原物料／能源／通訊／運輸／媒體
    list("ind.Machinery", "dcf", "pb", FALSE, "機械設備 → DCF"),
    list("ind.Aerospace_Defense", "dcf", "pb", FALSE, "航太國防 → DCF"),
    list("ind.Construction", "dcf", "pb", FALSE, "營建工程 → DCF"),
    list("mat.Chemicals", "dcf", "pb", FALSE, "化學循環 → DCF"),
    list("mat.Metals_Mining", "dcf", "pb", FALSE, "金屬礦業循環 → DCF"),
    list("en.Energy_OilGas", "dcf", "pb", FALSE, "油氣循環／FCF → DCF；P/B 為副模型"),
    list("tel.Telecom", "ddm", "dcf", FALSE, "電信高配息 → DDM；穩健 FCF 輔 DCF"),
    list("tr.Logistics_Shipping", "dcf", "pb", FALSE, "物流運輸 → DCF"),
    list("tr.Airlines", "dcf", "pb", FALSE, "航空循環／資本密集 → DCF"),
    list("media.Entertainment", "dcf", "pb", TRUE, "娛樂內容成長／轉型 → DCF"),
    list("media.Gaming", "dcf", "pb", TRUE, "遊戲成長 → DCF"),
    list("hosp.Hotels_Travel", "dcf", "pb", FALSE, "旅宿循環 → DCF")
  )

  df <- do.call(rbind, lapply(rows, function(r) {
    data.frame(
      industry_key = r[[1]],
      primary = r[[2]],
      secondary = r[[3]],
      suggest_two_stage = isTRUE(r[[4]]),
      rationale = r[[5]],
      stringsAsFactors = FALSE
    )
  }))

  # 覆蓋產業標籤；缺漏的產業鍵用 DCF 後備
  all_keys <- names(industry_labels)
  miss <- setdiff(all_keys, df$industry_key)
  if (length(miss) > 0) {
    df <- rbind(
      df,
      data.frame(
        industry_key = miss,
        primary = "dcf",
        secondary = "pb",
        suggest_two_stage = FALSE,
        rationale = "未特化產業規則 → 預設 DCF",
        stringsAsFactors = FALSE
      )
    )
  }
  if (exists("LAB_UNMAPPED_KEY", inherits = TRUE) &&
      !LAB_UNMAPPED_KEY %in% df$industry_key) {
    df <- rbind(
      df,
      data.frame(
        industry_key = LAB_UNMAPPED_KEY,
        primary = "dcf",
        secondary = "pb",
        suggest_two_stage = FALSE,
        rationale = "GICS 未對應 App 產業 → 預設 DCF",
        stringsAsFactors = FALSE
      )
    )
  }
  # Industry-catalog prior only (no statements yet). Per-stock recommend_valuation_models
  # ranks from financial-statement attributes; a cyclical label does not force P/B.
  if (exists("is_cyclical_industry", mode = "function") &&
      exists("cyclical_pb_slot", mode = "function")) {
    for (i in seq_len(nrow(df))) {
      if (!isTRUE(is_cyclical_industry(df$industry_key[[i]], ""))) next
      slot <- cyclical_pb_slot(df$primary[[i]], df$secondary[[i]], pb_ok = TRUE)
      if (isTRUE(slot$changed)) {
        df$secondary[[i]] <- slot$secondary
        df$rationale[[i]] <- paste0(df$rationale[[i]], "；景氣循環 → P/B 為主或副模型")
      }
    }
  }
  df$industry_label <- ifelse(
    exists("LAB_UNMAPPED_KEY", inherits = TRUE) & df$industry_key == LAB_UNMAPPED_KEY,
    LAB_UNMAPPED_LABEL,
    as.character(industry_labels[df$industry_key])
  )
  df$rel_family <- vapply(seq_len(nrow(df)), function(i) {
    lab_default_rel_family(df$industry_key[[i]], df$primary[[i]])
  }, character(1))
  df$primary_label <- as.character(LAB_METHOD_LABELS[df$primary])
  df$secondary_label <- ifelse(
    is.na(df$secondary) | !nzchar(df$secondary),
    "",
    as.character(LAB_METHOD_LABELS[df$secondary])
  )
  df[order(df$primary, df$industry_label), , drop = FALSE]
}

#' 各產業美股績優候選（US primary listings 宇宙；一檔對一產業，不重抓財報）
#' @return named list: industry_key → character vector of US tickers
lab_us_quality_candidates <- function() {
  u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) {
    u <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) NULL)
  }
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) return(list())
  tks <- as.character(u$ticker)
  keys <- as.character(u$industry_key)
  keep <- nzchar(tks) & !is.na(tks) & nzchar(keys) & !is.na(keys)
  tks <- tks[keep]
  keys <- keys[keep]
  drop <- grepl("\\.TW$|\\.TWO$|-TW$|-TWO$", tks, ignore.case = TRUE) |
    tks %in% c("SPY", "QQQ", "DIA", "IWM", "VOO", "IVV", "VTI", "QQQM")
  tks <- tks[!drop]
  keys <- keys[!drop]
  if (!length(tks)) return(list())
  split(tks, keys)
}

#' 依市場模式回傳績優候選（美股主要上市／台股依 boards）
#' @param boards TW only：TWSE／TPEX／ESB（預設上市＋上櫃）
lab_quality_candidates_for_market <- function(mode = get_market_mode(),
                                              boards = c("TWSE", "TPEX")) {
  mode <- if (exists("normalize_market_mode", mode = "function")) {
    normalize_market_mode(mode)
  } else {
    m <- toupper(trimws(as.character(mode)[1]))
    if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
  }
  if (identical(mode, "TW") && exists("lab_tw_quality_candidates", mode = "function")) {
    return(lab_tw_quality_candidates(boards = boards))
  }
  lab_us_quality_candidates()
}

#' Attach TW exchange／board columns onto a ticker frame (no-op for US)
lab_attach_tw_exchange <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !"ticker" %in% names(df)) {
    return(df)
  }
  if (!exists("lookup_tw_universe_exchange", mode = "function")) return(df)
  if (!"exchange" %in% names(df)) df$exchange <- NA_character_
  miss <- is.na(df$exchange) | !nzchar(trimws(as.character(df$exchange)))
  if (any(miss)) {
    df$exchange[miss] <- vapply(
      as.character(df$ticker[miss]),
      function(tk) {
        ex <- tryCatch(lookup_tw_universe_exchange(tk), error = function(e) "")
        if (!nzchar(ex)) "" else ex
      },
      character(1)
    )
  }
  if (exists("lab_tw_exchange_to_board", mode = "function")) {
    df$board <- vapply(
      as.character(df$exchange),
      function(ex) lab_tw_exchange_to_board(ex),
      character(1)
    )
  }
  df
}

#' Filter a ticker frame by TW boards (TWSE／TPEX／ESB). Empty boards → empty frame.
lab_filter_df_by_tw_boards <- function(df, boards = c("TWSE", "TPEX")) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
  if (!exists("lab_normalize_tw_boards", mode = "function")) return(df)
  boards <- lab_normalize_tw_boards(boards)
  if (!length(boards)) return(df[0, , drop = FALSE])
  df <- lab_attach_tw_exchange(df)
  if (!"exchange" %in% names(df) && !"board" %in% names(df)) return(df)
  keep <- if ("board" %in% names(df)) {
    as.character(df$board) %in% boards
  } else {
    vapply(
      as.character(df$exchange),
      function(ex) isTRUE(lab_tw_exchange_in_boards(ex, boards)),
      logical(1)
    )
  }
  df[keep, , drop = FALSE]
}

#' 展開為一列一檔的產業×方法×候選表
#' @param market_mode "US" | "TW"（預設讀取 get_market_mode）
#' @param boards TW only：TWSE／TPEX／ESB（預設上市＋上櫃）
lab_build_industry_method_catalog <- function(market_mode = NULL,
                                             boards = c("TWSE", "TPEX")) {
  mode <- if (!is.null(market_mode)) {
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market_mode)
    } else {
      m <- toupper(trimws(as.character(market_mode)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  defaults <- lab_industry_method_defaults()
  cands <- lab_quality_candidates_for_market(mode, boards = boards)
  rows <- list()
  for (i in seq_len(nrow(defaults))) {
    key <- defaults$industry_key[[i]]
    tickers <- unique(as.character(cands[[key]] %||% character(0)))
    tickers <- tickers[nzchar(tickers)]
    if (identical(mode, "TW")) {
      tickers <- tickers[grepl("\\.(TW|TWO)$", tickers, ignore.case = TRUE)]
      tickers <- setdiff(tickers, c("0050.TW", "0056.TW", "^TWII"))
    } else {
      # 排除台股與常見指數 ETF
      tickers <- tickers[!grepl("\\.TW$|\\.TWO$", tickers, ignore.case = TRUE)]
      tickers <- setdiff(tickers, c("SPY", "QQQ", "DIA", "IWM"))
    }
    if (length(tickers) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        industry_key = key,
        industry_label = defaults$industry_label[[i]],
        primary = defaults$primary[[i]],
        primary_label = defaults$primary_label[[i]],
        secondary = defaults$secondary[[i]],
        secondary_label = defaults$secondary_label[[i]],
        suggest_two_stage = defaults$suggest_two_stage[[i]],
        rationale = defaults$rationale[[i]],
        rel_family = defaults$rel_family[[i]],
        ticker = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      for (tk in tickers) {
        rows[[length(rows) + 1L]] <- data.frame(
          industry_key = key,
          industry_label = defaults$industry_label[[i]],
          primary = defaults$primary[[i]],
          primary_label = defaults$primary_label[[i]],
          secondary = defaults$secondary[[i]],
          secondary_label = defaults$secondary_label[[i]],
          suggest_two_stage = defaults$suggest_two_stage[[i]],
          rationale = defaults$rationale[[i]],
          rel_family = defaults$rel_family[[i]],
          ticker = tk,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  # Attach ADR flag from US universe (TW rows are never US ADR)
  if (identical(mode, "US") && exists("lab_is_us_adr", mode = "function") &&
      is.data.frame(out) && nrow(out) > 0L && "ticker" %in% names(out)) {
    u <- tryCatch(lab_get_us_universe(FALSE), error = function(e) NULL)
    if (!is.null(u) && is.data.frame(u) && "is_adr" %in% names(u)) {
      idx <- match(
        toupper(trimws(as.character(out$ticker))),
        toupper(trimws(as.character(u$ticker)))
      )
      out$is_adr <- FALSE
      hit <- which(is.finite(idx))
      if (length(hit)) {
        out$is_adr[hit] <- vapply(
          idx[hit],
          function(j) isTRUE(as.logical(u$is_adr[[j]])),
          logical(1)
        )
      }
    } else {
      out$is_adr <- vapply(
        as.character(out$ticker),
        function(tk) isTRUE(lab_is_us_adr(tk, NULL)),
        logical(1)
      )
    }
  } else if (is.data.frame(out) && nrow(out) > 0L) {
    out$is_adr <- FALSE
  }
  # TW: attach exchange／board for 上市／上櫃／興櫃 filters
  if (identical(mode, "TW") && is.data.frame(out) && nrow(out) > 0L) {
    out <- lab_attach_tw_exchange(out)
  }
  out
}

#' 是否通過舊版綜合品質門檻（F-Score≥7 且盈餘品質通過；保留供相容）
lab_is_quality_stock <- function(f_score, quality_flag, min_score = 7L) {
  fs <- suppressWarnings(as.numeric(f_score)[1])
  qf <- suppressWarnings(as.numeric(quality_flag)[1])
  isTRUE(is.finite(fs) && fs >= as.numeric(min_score) && isTRUE(qf == 1))
}

#' 評估單一標的：F-Score 門檻＋產業主方法簡化合理價＋個股 n 年年化漲幅
lab_evaluate_ticker_fscore <- function(ticker, industry_key = NULL, method = NULL) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  n_yrs <- lab_model_horizon_years()
  out <- list(
    ticker = tk,
    ok = FALSE,
    f_score = NA_real_,
    quality_flag = NA_real_,
    n_fs_alerts = NA_integer_,
    is_quality = FALSE,
    market_cap = NA_real_,
    price = NA_real_,
    beta = NA_real_,
    fv = NA_real_,
    upside_total_pct = NA_real_,
    upside_cagr_pct = NA_real_,
    n_years = n_yrs,
    ke_pct = NA_real_,
    g_pct = NA_real_,
    g1_pct = NA_real_,
    lifecycle_stage = NA_character_,
    fv_note = NA_character_,
    company_type = NA_character_,
    company_name = NA_character_,
    primary_live = NA_character_,
    method_used = as.character(method %||% NA_character_)[1],
    is_quality_upside = FALSE,
    error = NULL
  )
  if (!nzchar(tk)) {
    out$error <- "空代號"
    return(out)
  }

  sm <- tryCatch(lab_fetch_summary_metrics(tk), error = function(e) {
    list(market_cap = NA_real_, price = NA_real_, beta = NA_real_, company_name = NA_character_)
  })
  out$market_cap <- suppressWarnings(as.numeric(sm$market_cap)[1])
  out$price <- suppressWarnings(as.numeric(sm$price)[1])
  out$beta <- suppressWarnings(as.numeric(sm$beta)[1])
  out$company_name <- lab_company_display_name(tk, sm$company_name)

  res <- tryCatch(cached_scrape_financials(tk), error = function(e) e)
  if (inherits(res, "error")) {
    out$error <- conditionMessage(res)
    return(out)
  }
  if (is.null(res) || !is.list(res)) {
    out$error <- "無法取得財報"
    return(out)
  }
  d_is <- tryCatch(res[["Income Statement"]]$expanded, error = function(e) NULL)
  d_bs <- tryCatch(res[["Balance Sheet"]]$expanded, error = function(e) NULL)
  d_cf <- tryCatch(res[["Cash Flow"]]$expanded, error = function(e) NULL)
  fs <- tryCatch(
    compute_report_f_score(d_is, d_bs, d_cf),
    error = function(e) list(total = NA_real_, quality_flag = NA_real_)
  )
  out$f_score <- suppressWarnings(as.numeric(fs$total)[1])
  out$quality_flag <- suppressWarnings(as.numeric(fs$quality_flag)[1])
  out$is_quality <- lab_is_quality_stock(out$f_score, out$quality_flag)

  ind_key <- as.character(industry_key %||% "")[1]
  # 財報警訊：與 YNOW 漏斗相同定義（Schilit 警示 + 盈餘品質紅旗）
  if (exists("collect_fraud_warnings", mode = "function")) {
    warns <- tryCatch(
      collect_fraud_warnings(d_cf, d_is, d_bs, industry_key = ind_key),
      error = function(e) character(0)
    )
    out$n_fs_alerts <- as.integer(length(warns))
  } else {
    out$n_fs_alerts <- NA_integer_
  }
  meth <- as.character(method %||% "")[1]
  if (!nzchar(meth)) meth <- "dcf"

  rec <- tryCatch(
    recommend_valuation_models(
      d_cf, industry_text = "", d_is = d_is, d_bs = d_bs, industry_choice = ind_key
    ),
    error = function(e) NULL
  )
  if (!is.null(rec)) {
    out$company_type <- as.character(rec$company_type %||% NA_character_)
    out$primary_live <- as.character(rec$primary %||% NA_character_)
  }

  # 估值用實際建議主方法（有財報時），否則用產業預設
  meth_fv <- if (!is.null(rec) && nzchar(as.character(rec$primary %||% ""))) {
    as.character(rec$primary)[1]
  } else {
    meth
  }

  fv_res <- tryCatch(
    lab_estimate_fv_per_share(
      meth_fv, ind_key, d_is, d_bs, d_cf,
      price = out$price, market_cap = out$market_cap,
      beta = out$beta, ticker = tk
    ),
    error = function(e) list(fv = NA_real_, n_years = n_yrs, note = e$message)
  )
  out$fv <- suppressWarnings(as.numeric(fv_res$fv)[1])
  out$n_years <- suppressWarnings(as.integer(fv_res$n_years %||% n_yrs)[1])
  out$ke_pct <- suppressWarnings(as.numeric(fv_res$ke_pct)[1])
  out$g_pct <- suppressWarnings(as.numeric(fv_res$g_pct)[1])
  out$g1_pct <- suppressWarnings(as.numeric(fv_res$g1_pct)[1])
  out$lifecycle_stage <- as.character(fv_res$lifecycle_stage %||% NA_character_)[1]
  out$fv_note <- as.character(fv_res$note %||% "")[1]
  out$method_used <- meth_fv
  if (is.finite(out$fv) && is.finite(out$price) && out$price > 0) {
    out$upside_total_pct <- (out$fv / out$price - 1) * 100
    out$upside_cagr_pct <- lab_annualized_upside_pct(out$fv, out$price, out$n_years)
  }

  # 績優候選：通過門檻且能算出年化漲幅
  out$is_quality_upside <- isTRUE(out$is_quality) && is.finite(out$upside_cagr_pct)

  out$ok <- isTRUE(is.finite(out$f_score))
  if (!out$ok && is.null(out$error)) out$error <- "F-Score 無法計算"
  out
}

#' 批次評估；tickers_df 可含 ticker / industry_key / primary 欄
#' 呼叫端應已用 lab_rank_and_cap_eval_pool 取好評估池；max_n=Inf 時不再截斷。
lab_screen_tickers_fscore <- function(tickers, progress_cb = NULL, max_n = Inf,
                                      industry_keys = NULL, methods = NULL) {
  # 接受字元向量或 data.frame
  if (is.data.frame(tickers)) {
    tks <- as.character(tickers$ticker)
    ind_keys <- as.character(tickers$industry_key %||% rep(NA_character_, nrow(tickers)))
    meths <- as.character(tickers$primary %||% rep(NA_character_, nrow(tickers)))
  } else {
    tks <- as.character(tickers)
    ind_keys <- if (is.null(industry_keys)) rep(NA_character_, length(tks)) else as.character(industry_keys)
    meths <- if (is.null(methods)) rep(NA_character_, length(tks)) else as.character(methods)
  }
  keep <- nzchar(toupper(trimws(tks))) & !is.na(tks)
  tks <- toupper(trimws(tks[keep]))
  ind_keys <- ind_keys[keep]
  meths <- meths[keep]
  mode <- if (exists("get_market_mode", mode = "function")) get_market_mode() else "US"
  if (identical(mode, "TW")) {
    keep_mkt <- grepl("\\.(TW|TWO)$", tks) | grepl("^\\^", tks)
    tks <- tks[keep_mkt]; ind_keys <- ind_keys[keep_mkt]; meths <- meths[keep_mkt]
  } else {
    drop_tw <- grepl("\\.TW$|\\.TWO$", tks)
    tks <- tks[!drop_tw]; ind_keys <- ind_keys[!drop_tw]; meths <- meths[!drop_tw]
  }

  # 同一代碼只評估一次（取第一筆產業／方法）
  ord <- !duplicated(tks)
  tks <- tks[ord]; ind_keys <- ind_keys[ord]; meths <- meths[ord]

  if (is.finite(max_n) && length(tks) > as.integer(max_n)) {
    tks <- head(tks, as.integer(max_n))
    ind_keys <- head(ind_keys, as.integer(max_n))
    meths <- head(meths, as.integer(max_n))
  }
  n <- length(tks)
  rows <- vector("list", n)
  for (i in seq_along(tks)) {
    if (is.function(progress_cb)) {
      tryCatch(progress_cb(i, n, tks[[i]]), error = function(e) NULL)
    }
    ev <- lab_evaluate_ticker_fscore(tks[[i]], industry_key = ind_keys[[i]], method = meths[[i]])
    rows[[i]] <- data.frame(
      ticker = ev$ticker,
      ok = isTRUE(ev$ok),
      f_score = ev$f_score,
      quality_flag = ev$quality_flag,
      n_fs_alerts = as.integer(ev$n_fs_alerts %||% NA_integer_),
      is_quality = isTRUE(ev$is_quality),
      is_quality_upside = isTRUE(ev$is_quality_upside),
      market_cap = ev$market_cap,
      price = ev$price,
      fv = ev$fv,
      upside_total_pct = ev$upside_total_pct,
      upside_cagr_pct = ev$upside_cagr_pct,
      n_years = ev$n_years,
      fv_note = ev$fv_note %||% NA_character_,
      method_used = ev$method_used %||% NA_character_,
      primary_live = ev$primary_live %||% NA_character_,
      company_type = ev$company_type %||% NA_character_,
      company_name = ev$company_name %||% NA_character_,
      error = ev$error %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
  empty <- data.frame(
    ticker = character(0), ok = logical(0), f_score = numeric(0),
    quality_flag = numeric(0), n_fs_alerts = integer(0),
    is_quality = logical(0),
    is_quality_upside = logical(0),
    market_cap = numeric(0),
    price = numeric(0), fv = numeric(0),
    upside_total_pct = numeric(0), upside_cagr_pct = numeric(0),
    n_years = integer(0), fv_note = character(0), method_used = character(0),
    primary_live = character(0), company_type = character(0),
    company_name = character(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
  if (n == 0L) return(empty)
  do.call(rbind, rows)
}

#' Pure Blue Chip evaluation job for ExtendedTask / future workers.
#' All inputs are plain R objects (no reactives / session). Returns a list with
#' `scores`, optional `pool_note` (character), and meta fields for UI copy.
lab_im_run_eval_job <- function(catlg,
                                method_filter = NULL,
                                industry_filter = NULL,
                                include_adr = TRUE,
                                display_n = 25L,
                                eval_n = 25L,
                                rank_mode = "mcap",
                                concept_keys = NULL,
                                market_mode = "US",
                                locale = "zh-TW") {
  mm <- tryCatch(normalize_market_mode(market_mode), error = function(e) "US")
  tryCatch(set_market_mode(mm), error = function(e) NULL)
  loc <- tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
    return(list(ok = FALSE, reason = "empty_catalog", scores = NULL))
  }
  pool <- lab_merge_catalog_scores(
    catlg,
    scores = NULL,
    method_filter = method_filter,
    industry_filter = industry_filter,
    eq_only = FALSE,
    gate_only = FALSE
  )
  pool <- lab_dedupe_eval_pool(pool)
  if (!identical(mm, "TW") && exists("lab_filter_pool_adr", mode = "function")) {
    pool <- lab_filter_pool_adr(pool, include_adr = isTRUE(include_adr))
  }
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L) {
    return(list(ok = FALSE, reason = "no_candidates", scores = NULL))
  }
  display_n <- lab_coerce_im_job_n(display_n)
  eval_n <- lab_coerce_im_job_n(eval_n)
  if (!is.finite(eval_n) || (is.finite(eval_n) && eval_n < 1L)) eval_n <- display_n
  rank_mode <- lab_normalize_pool_rank_mode(rank_mode %||% "mcap")
  n_raw <- nrow(pool)
  if (is.finite(eval_n) && n_raw > eval_n) {
    if (identical(rank_mode, "mcap") || identical(rank_mode, "concept")) {
      pool <- lab_attach_market_caps(pool)
    }
  }
  pool <- lab_select_eval_pool(
    pool,
    max_n = eval_n,
    mode = rank_mode,
    concept_keys = concept_keys,
    market_mode = mm,
    seed = as.integer(Sys.time())
  )
  n_filtered <- as.integer(attr(pool, "n_filtered") %||% nrow(pool))
  used_mcap <- isTRUE(attr(pool, "used_market_cap"))
  mode_used <- as.character(attr(pool, "pool_rank_mode") %||% rank_mode)[1]
  note <- as.character(attr(pool, "pool_rank_note") %||% "")[1]
  if (nrow(pool) < 1L) {
    return(list(ok = FALSE, reason = "no_candidates_ind_model", scores = NULL))
  }
  pool_note <- NULL
  if (is.finite(eval_n) && n_filtered > nrow(pool)) {
    how <- switch(
      mode_used,
      ret_1y = tryCatch(ui_str("lab_im_rank_how_ret_1y", loc), error = function(e) "by 1Y return"),
      random = tryCatch(ui_str("lab_im_rank_how_random", loc), error = function(e) "random"),
      concept = tryCatch(ui_str("lab_im_rank_how_concept", loc), error = function(e) "by concept"),
      if (used_mcap) {
        tryCatch(ui_str("lab_im_rank_how_mcap", loc), error = function(e) "by market cap")
      } else {
        tryCatch(ui_str("lab_im_rank_how_mcap_fallback", loc), error = function(e) "by market cap")
      }
    )
    if (grepl("fallback_mcap", note, fixed = TRUE)) {
      how <- paste0(
        how,
        tryCatch(ui_str("lab_im_rank_how_concept_fallback", loc), error = function(e) "")
      )
    }
    adr_note <- if (!identical(mm, "TW") && !isTRUE(include_adr)) {
      tryCatch(ui_str("lab_im_rank_adr_excluded", loc), error = function(e) "")
    } else {
      ""
    }
    disp_note <- if (is.finite(display_n) && is.finite(eval_n) && eval_n > display_n) {
      tryCatch(
        .ynow_fmt_str(ui_str("lab_im_rank_disp_note", loc), n = display_n),
        error = function(e) ""
      )
    } else {
      ""
    }
    pool_note <- tryCatch(
      .ynow_fmt_str(
        ui_str("lab_im_rank_pool_note", loc),
        n_filtered = n_filtered, how = how, n_eval = nrow(pool),
        adr = adr_note, disp = disp_note
      ),
      error = function(e) NULL
    )
  }
  cols <- intersect(c("ticker", "industry_key", "primary"), names(pool))
  scores <- lab_screen_tickers_fscore(
    pool[, cols, drop = FALSE],
    max_n = eval_n,
    progress_cb = NULL
  )
  list(
    ok = is.data.frame(scores) && nrow(scores) > 0L,
    reason = if (is.data.frame(scores) && nrow(scores) > 0L) "ok" else "empty_scores",
    scores = scores,
    pool_note = pool_note,
    display_n = display_n,
    market_mode = mm,
    locale = loc
  )
}

#' Query-filter keys → row hit.
#' Absolute `pb` = Justified P/B Fair Value (primary/secondary).
#' Relative `book` = Multiples reference family book (industry / history).
#' Enterprise / earnings / revenue match the industry Multiples reference family.
#' Legacy `multiples` still matches any relative family.
lab_method_filter_hits <- function(df, method_filter) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  if (n < 1L) return(logical(0))
  mf <- lab_normalize_multi_filter(method_filter)
  if (!length(mf)) return(rep(TRUE, n))
  legacy_mult <- "multiples" %in% mf
  mf <- setdiff(mf, "multiples")
  fv <- intersect(mf, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
  fam <- intersect(mf, c("enterprise", "earnings", "revenue", "book"))
  hit <- rep(FALSE, n)
  if (length(fv)) {
    hit <- hit | as.character(df$primary) %in% fv | as.character(df$secondary) %in% fv
  }
  if ("rel_family" %in% names(df)) {
    rel <- as.character(df$rel_family)
    if (length(fam)) hit <- hit | rel %in% fam
    if (legacy_mult) {
      hit <- hit | rel %in% c("enterprise", "earnings", "revenue", "book")
    }
  }
  hit
}

#' 合併目錄與評估結果，複選篩選後依年化估值漲幅排序
#' @param evaluated_only TRUE：只保留已評估代碼（明細／排行榜與評估檔數 N 對齊）
lab_merge_catalog_scores <- function(catalog, scores = NULL,
                                     method_filter = character(0),
                                     industry_filter = character(0),
                                     eq_only = FALSE,
                                     gate_only = FALSE,
                                     no_alert = FALSE,
                                     quality_only = FALSE,
                                     evaluated_only = FALSE) {
  # quality_only：舊「只看通過」別名 → Piotroski 高門檻（F-Score≥7；不含盈餘品質）
  if (isTRUE(quality_only)) gate_only <- TRUE
  df <- catalog
  score_cols_na <- function(d) {
    d$ok <- NA
    d$f_score <- NA_real_
    d$quality_flag <- NA_real_
    d$n_fs_alerts <- NA_integer_
    d$is_quality <- NA
    d$is_quality_upside <- NA
    d$market_cap <- NA_real_
    d$price <- NA_real_
    d$fv <- NA_real_
    d$upside_total_pct <- NA_real_
    d$upside_cagr_pct <- NA_real_
    d$n_years <- NA_integer_
    d$fv_note <- NA_character_
    d$method_used <- NA_character_
    d$primary_live <- NA_character_
    d$company_type <- NA_character_
    d$company_name <- NA_character_
    d$error <- NA_character_
    d
  }
  has_scores <- !is.null(scores) && is.data.frame(scores) && nrow(scores) > 0
  .coalesce_merge_mcap <- function(d) {
    if ("market_cap" %in% names(d)) return(d)
    if ("market_cap.y" %in% names(d) || "market_cap.x" %in% names(d)) {
      my <- if ("market_cap.y" %in% names(d)) {
        suppressWarnings(as.numeric(d$market_cap.y))
      } else {
        rep(NA_real_, nrow(d))
      }
      mx <- if ("market_cap.x" %in% names(d)) {
        suppressWarnings(as.numeric(d$market_cap.x))
      } else {
        rep(NA_real_, nrow(d))
      }
      d$market_cap <- ifelse(is.finite(my) & my > 0, my, mx)
      drop <- intersect(c("market_cap.x", "market_cap.y"), names(d))
      if (length(drop)) d[drop] <- NULL
    }
    d
  }
  if (isTRUE(evaluated_only)) {
    if (!has_scores) {
      df <- score_cols_na(df)
      return(df[0, , drop = FALSE])
    }
    df <- merge(df, scores, by = "ticker", all.x = FALSE, all.y = FALSE, sort = FALSE)
    df <- df[!is.na(df$ticker) & nzchar(as.character(df$ticker)), , drop = FALSE]
    df <- df[!duplicated(df$ticker), , drop = FALSE]
    df <- .coalesce_merge_mcap(df)
  } else if (has_scores) {
    df <- merge(df, scores, by = "ticker", all.x = TRUE, sort = FALSE)
    df <- .coalesce_merge_mcap(df)
  } else {
    df <- score_cols_na(df)
  }

  mf <- lab_normalize_multi_filter(method_filter)
  if (length(mf) > 0) {
    df <- df[lab_method_filter_hits(df, mf), , drop = FALSE]
  }
  indf <- lab_normalize_multi_filter(industry_filter)
  if (length(indf) > 0) {
    df <- df[df$industry_key %in% indf, , drop = FALSE]
  }
  if (isTRUE(eq_only)) {
    df <- df[!is.na(df$quality_flag) & df$quality_flag %in% 1, , drop = FALSE]
  }
  if (isTRUE(gate_only)) {
    # Piotroski 高門檻：僅 F-Score≥7（與盈餘品質無關）
    fs <- suppressWarnings(as.numeric(df$f_score))
    df <- df[is.finite(fs) & fs >= 7, , drop = FALSE]
  }
  if (isTRUE(no_alert) && "n_fs_alerts" %in% names(df)) {
    # 無財報警訊：Schilit／盈餘品質紅旗合計為 0（與 YNOW 財報警訊一致）
    na_n <- suppressWarnings(as.numeric(df$n_fs_alerts))
    df <- df[is.finite(na_n) & na_n <= 0, , drop = FALSE]
  }
  if (nrow(df) == 0L) return(df)
  # 年化估值漲幅由大到小（NA 置後）— 即「未來期間漲幅最大者」優先
  ind_lab <- if ("industry_label" %in% names(df)) df$industry_label else rep("", nrow(df))
  o <- order(is.na(df$upside_cagr_pct), -df$upside_cagr_pct, ind_lab, df$ticker,
             na.last = TRUE)
  df[o, , drop = FALSE]
}

#' Tickers in the Blue Chip query-filter pool (pre-truncate), one row per ticker.
#' Industry × model filters, then optional US ADR exclude. Board filter is
#' assumed already applied when building `catalog`.
lab_structural_filter_pool <- function(catalog,
                                       method_filter = character(0),
                                       industry_filter = character(0),
                                       include_adr = TRUE,
                                       market_mode = "US") {
  empty <- data.frame(
    ticker = character(0),
    industry_key = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(catalog) || !is.data.frame(catalog) || nrow(catalog) < 1L) {
    return(empty)
  }
  pool <- lab_merge_catalog_scores(
    catalog,
    scores = NULL,
    method_filter = method_filter,
    industry_filter = industry_filter,
    eq_only = FALSE,
    gate_only = FALSE
  )
  mm <- tryCatch(
    normalize_market_mode(market_mode),
    error = function(e) as.character(market_mode %||% "US")[1]
  )
  if (!identical(mm, "TW") &&
      exists("lab_filter_pool_adr", mode = "function") &&
      !isTRUE(include_adr)) {
    pool <- lab_filter_pool_adr(pool, include_adr = FALSE)
  }
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L ||
      !"ticker" %in% names(pool)) {
    return(empty)
  }
  pool$ticker <- as.character(pool$ticker)
  pool <- pool[!is.na(pool$ticker) & nzchar(pool$ticker), , drop = FALSE]
  if (!nrow(pool)) return(empty)
  if (!"industry_key" %in% names(pool)) pool$industry_key <- NA_character_
  pool <- pool[!duplicated(pool$ticker), c("ticker", "industry_key"), drop = FALSE]
  rownames(pool) <- NULL
  pool
}

#' Count unique tickers in the Blue Chip query-filter pool (pre-truncate).
#' @return integer count (≥ 0)
lab_count_filter_pool_tickers <- function(catalog,
                                          method_filter = character(0),
                                          industry_filter = character(0),
                                          include_adr = TRUE,
                                          market_mode = "US") {
  as.integer(nrow(lab_structural_filter_pool(
    catalog,
    method_filter = method_filter,
    industry_filter = industry_filter,
    include_adr = include_adr,
    market_mode = market_mode
  )))
}

#' Slow-cache lifetime for earnings-quality / F-Score / FS-alert snapshots.
LAB_QUALITY_SNAPSHOT_TTL_SEC <- 24 * 60 * 60

lab_quality_snapshot_empty <- function() {
  data.frame(
    ticker = character(0),
    market = character(0),
    updated_at = as.POSIXct(character(0)),
    f_score = numeric(0),
    quality_flag = numeric(0),
    n_fs_alerts = integer(0),
    ok = logical(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
}

lab_quality_snapshot_dir <- function() {
  override <- Sys.getenv("YNOW_SLOW_CACHE_DIR", unset = "")
  d <- if (nzchar(override)) {
    override
  } else if (exists(".ynow_slow_cache_dir", mode = "function")) {
    .ynow_slow_cache_dir()
  } else {
    file.path(tempdir(), "ynow_cache", "slow")
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

lab_quality_snapshot_path <- function(market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  file.path(lab_quality_snapshot_dir(), sprintf("lab_quality_snapshot_%s.rds", mm))
}

lab_load_quality_snapshot <- function(market = "US") {
  path <- lab_quality_snapshot_path(market)
  if (!file.exists(path)) return(lab_quality_snapshot_empty())
  snap <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(snap) || !nrow(snap) || !"ticker" %in% names(snap)) {
    return(lab_quality_snapshot_empty())
  }
  base <- lab_quality_snapshot_empty()
  for (nm in names(base)) {
    if (!nm %in% names(snap)) snap[[nm]] <- base[[nm]][0]
  }
  snap <- snap[, names(base), drop = FALSE]
  snap$ticker <- toupper(trimws(as.character(snap$ticker)))
  snap <- snap[!is.na(snap$ticker) & nzchar(snap$ticker), , drop = FALSE]
  snap <- snap[!duplicated(snap$ticker), , drop = FALSE]
  snap$updated_at <- as.POSIXct(snap$updated_at, origin = "1970-01-01", tz = "UTC")
  snap$f_score <- suppressWarnings(as.numeric(snap$f_score))
  snap$quality_flag <- suppressWarnings(as.numeric(snap$quality_flag))
  snap$n_fs_alerts <- suppressWarnings(as.integer(snap$n_fs_alerts))
  snap$ok <- as.logical(snap$ok)
  snap$ok[is.na(snap$ok)] <- FALSE
  rownames(snap) <- NULL
  snap
}

lab_save_quality_snapshot <- function(snapshot, market = "US") {
  path <- lab_quality_snapshot_path(market)
  snap <- if (is.data.frame(snapshot)) snapshot else lab_quality_snapshot_empty()
  if (!nrow(snap)) snap <- lab_quality_snapshot_empty()
  tmp <- paste0(path, ".tmp")
  saveRDS(snap, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

#' Statement-only quality fields (no price / fair value).
lab_compute_quality_metrics <- function(ticker, industry_key = NULL) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  out <- list(
    ticker = tk,
    f_score = NA_real_,
    quality_flag = NA_real_,
    n_fs_alerts = NA_integer_,
    ok = FALSE,
    error = NA_character_
  )
  if (!nzchar(tk)) {
    out$error <- "empty ticker"
    return(out)
  }
  if (!exists("cached_scrape_financials", mode = "function")) {
    out$error <- "financials unavailable"
    return(out)
  }
  res <- tryCatch(cached_scrape_financials(tk), error = function(e) e)
  if (inherits(res, "error") || is.null(res) || !is.list(res)) {
    out$error <- if (inherits(res, "error")) conditionMessage(res) else "no statements"
    return(out)
  }
  d_is <- tryCatch(res[["Income Statement"]]$expanded, error = function(e) NULL)
  d_bs <- tryCatch(res[["Balance Sheet"]]$expanded, error = function(e) NULL)
  d_cf <- tryCatch(res[["Cash Flow"]]$expanded, error = function(e) NULL)
  fs <- if (exists("compute_report_f_score", mode = "function")) {
    tryCatch(
      compute_report_f_score(d_is, d_bs, d_cf),
      error = function(e) list(total = NA_real_, quality_flag = NA_real_)
    )
  } else {
    list(total = NA_real_, quality_flag = NA_real_)
  }
  out$f_score <- suppressWarnings(as.numeric(fs$total)[1])
  out$quality_flag <- suppressWarnings(as.numeric(fs$quality_flag)[1])
  ind_key <- as.character(industry_key %||% "")[1]
  if (exists("collect_fraud_warnings", mode = "function")) {
    warns <- tryCatch(
      collect_fraud_warnings(d_cf, d_is, d_bs, industry_key = ind_key),
      error = function(e) character(0)
    )
    out$n_fs_alerts <- as.integer(length(warns))
  }
  out$ok <- is.finite(out$f_score) || is.finite(out$quality_flag) ||
    is.finite(out$n_fs_alerts)
  if (!isTRUE(out$ok) && is.na(out$error)) out$error <- "quality metrics unavailable"
  out
}

lab_quality_row_is_fresh <- function(updated_at, ok, now = Sys.time(),
                                     ttl_sec = LAB_QUALITY_SNAPSHOT_TTL_SEC) {
  isTRUE(ok) && !is.na(updated_at) &&
    is.finite(as.numeric(difftime(now, updated_at, units = "secs"))) &&
    as.numeric(difftime(now, updated_at, units = "secs")) <= ttl_sec
}

#' Upsert quality rows. `rows` needs ticker plus f_score / quality_flag / n_fs_alerts.
lab_upsert_quality_rows <- function(rows, market = "US", now = Sys.time()) {
  mm <- tryCatch(normalize_market_mode(market), error = function(e) "US")
  snap <- lab_load_quality_snapshot(mm)
  if (!is.data.frame(rows) || !nrow(rows) || !"ticker" %in% names(rows)) {
    return(snap)
  }
  add <- lab_quality_snapshot_empty()
  add <- add[seq_len(nrow(rows)), , drop = FALSE]
  add$ticker <- toupper(trimws(as.character(rows$ticker)))
  add$market <- mm
  add$updated_at <- as.POSIXct(now, tz = "UTC")
  add$f_score <- suppressWarnings(as.numeric(rows$f_score))
  add$quality_flag <- suppressWarnings(as.numeric(rows$quality_flag))
  add$n_fs_alerts <- suppressWarnings(as.integer(rows$n_fs_alerts))
  add$ok <- is.finite(add$f_score) | is.finite(add$quality_flag) | is.finite(add$n_fs_alerts)
  add$error <- NA_character_
  add <- add[!is.na(add$ticker) & nzchar(add$ticker), , drop = FALSE]
  add <- add[!duplicated(add$ticker), , drop = FALSE]
  if (!nrow(add)) return(snap)
  if (nrow(snap)) snap <- snap[!snap$ticker %in% add$ticker, , drop = FALSE]
  snap <- rbind(snap, add)
  lab_save_quality_snapshot(snap, mm)
  snap
}

#' Refresh quality snapshot for the structural pool. Fresh rows (≤ 24h) are kept.
lab_refresh_quality_snapshot <- function(pool, market = "US", force = FALSE,
                                         progress_cb = NULL, compute_fn = NULL,
                                         now = Sys.time()) {
  mm <- tryCatch(normalize_market_mode(market), error = function(e) "US")
  if (is.character(pool)) {
    pool <- data.frame(
      ticker = pool, industry_key = NA_character_, stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(pool) || !nrow(pool) || !"ticker" %in% names(pool)) {
    return(list(
      snapshot = lab_load_quality_snapshot(mm),
      n_run = 0L, n_skip = 0L, n_pool = 0L
    ))
  }
  pool$ticker <- toupper(trimws(as.character(pool$ticker)))
  if (!"industry_key" %in% names(pool)) pool$industry_key <- NA_character_
  pool <- pool[!is.na(pool$ticker) & nzchar(pool$ticker), , drop = FALSE]
  pool <- pool[!duplicated(pool$ticker), , drop = FALSE]
  snap <- lab_load_quality_snapshot(mm)
  n <- nrow(pool)
  n_run <- 0L
  n_skip <- 0L
  fn <- if (is.function(compute_fn)) compute_fn else lab_compute_quality_metrics
  for (i in seq_len(n)) {
    tk <- pool$ticker[[i]]
    if (is.function(progress_cb)) {
      tryCatch(progress_cb(i, n, tk), error = function(e) NULL)
    }
    hit <- if (nrow(snap)) match(tk, snap$ticker) else NA_integer_
    if (!isTRUE(force) && !is.na(hit) &&
        lab_quality_row_is_fresh(snap$updated_at[[hit]], snap$ok[[hit]], now = now)) {
      n_skip <- n_skip + 1L
      next
    }
    ev <- tryCatch(
      fn(tk, pool$industry_key[[i]]),
      error = function(e) list(
        ticker = tk, f_score = NA_real_, quality_flag = NA_real_,
        n_fs_alerts = NA_integer_, ok = FALSE, error = conditionMessage(e)
      )
    )
    row <- lab_quality_snapshot_empty()
    row <- row[1, , drop = FALSE]
    row$ticker <- tk
    row$market <- mm
    row$updated_at <- as.POSIXct(now, tz = "UTC")
    row$f_score <- suppressWarnings(as.numeric(ev$f_score)[1])
    row$quality_flag <- suppressWarnings(as.numeric(ev$quality_flag)[1])
    row$n_fs_alerts <- suppressWarnings(as.integer(ev$n_fs_alerts)[1])
    row$ok <- isTRUE(ev$ok) || is.finite(row$f_score) || is.finite(row$quality_flag) ||
      is.finite(row$n_fs_alerts)
    row$error <- as.character(ev$error %||% NA_character_)[1]
    if (!is.na(hit)) {
      snap[hit, ] <- row
    } else {
      snap <- rbind(snap, row)
    }
    n_run <- n_run + 1L
  }
  lab_save_quality_snapshot(snap, mm)
  list(snapshot = snap, n_run = n_run, n_skip = n_skip, n_pool = n)
}

#' Pool size after quality gates. Missing snapshot rows are excluded only while
#' a gate is checked. With every gate off, the structural pool size is returned.
#' If a gate is checked but the snapshot covers none of the structural pool
#' (common on TW before Search / refresh), keep the structural count — do not
#' report 0 as if every name failed the gate.
lab_apply_quality_snapshot <- function(tickers, snapshot,
                                       eq_only = FALSE, gate_only = FALSE,
                                       no_alert = FALSE, now = Sys.time()) {
  tickers <- unique(as.character(tickers))
  tickers <- tickers[!is.na(tickers) & nzchar(tickers)]
  n_pool <- length(tickers)
  snap <- if (is.data.frame(snapshot)) snapshot else lab_quality_snapshot_empty()
  idx <- if (n_pool && nrow(snap)) match(tickers, as.character(snap$ticker)) else rep(NA_integer_, n_pool)
  covered <- rep(FALSE, n_pool)
  if (n_pool && any(!is.na(idx))) {
    hit <- which(!is.na(idx))
    covered[hit] <- isTRUE(nrow(snap) > 0) & as.logical(snap$ok[idx[hit]])
    covered[is.na(covered)] <- FALSE
  }
  n_covered <- sum(covered)
  updated_at <- as.POSIXct(NA)
  if (n_covered > 0L) {
    updated_at <- max(snap$updated_at[idx[covered]], na.rm = TRUE)
    if (!is.finite(as.numeric(updated_at))) updated_at <- as.POSIXct(NA)
  }
  gates_on <- isTRUE(eq_only) || isTRUE(gate_only) || isTRUE(no_alert)
  n_show <- n_pool
  gates_applied <- FALSE
  if (gates_on && n_covered < 1L) {
    # No usable quality rows for this pool — defer gates; keep structural N.
    n_show <- n_pool
    gates_applied <- FALSE
  } else if (gates_on) {
    pass <- rep(FALSE, n_pool)
    sub_i <- which(covered)
    rows <- snap[idx[sub_i], , drop = FALSE]
    p <- rep(TRUE, length(sub_i))
    if (isTRUE(eq_only)) {
      qf <- suppressWarnings(as.numeric(rows$quality_flag))
      p <- p & !is.na(qf) & qf %in% 1
    }
    if (isTRUE(gate_only)) {
      fs <- suppressWarnings(as.numeric(rows$f_score))
      p <- p & is.finite(fs) & fs >= 7
    }
    if (isTRUE(no_alert)) {
      na_n <- suppressWarnings(as.numeric(rows$n_fs_alerts))
      p <- p & is.finite(na_n) & na_n <= 0
    }
    pass[sub_i] <- p
    n_show <- sum(pass)
    gates_applied <- TRUE
  }
  age_sec <- if (!is.na(updated_at)) {
    as.numeric(difftime(now, updated_at, units = "secs"))
  } else {
    NA_real_
  }
  list(
    n_show = as.integer(n_show),
    n_pool = as.integer(n_pool),
    n_covered = as.integer(n_covered),
    n_missing = as.integer(n_pool - n_covered),
    updated_at = updated_at,
    stale = is.finite(age_sec) && age_sec > LAB_QUALITY_SNAPSHOT_TTL_SEC,
    gates_on = gates_on,
    gates_applied = gates_applied
  )
}

#' 排行榜候選池：有年化漲幅；可選 F-Score≥7／無財報警訊；同一代碼只留最高 CAGR 一列
#' @param gate_only 若 TRUE（預設），只保留 F-Score≥7；FALSE＝不設 F 門檻（仍須有年化漲幅）
#' @param no_alert 若 TRUE，只保留財報警訊數＝0（Schilit＋盈餘品質紅旗）
lab_leaderboard_pool <- function(merged_df, eq_only = FALSE, gate_only = TRUE,
                                 no_alert = FALSE) {
  empty <- merged_df[0, , drop = FALSE]
  if (is.null(merged_df) || !is.data.frame(merged_df) || nrow(merged_df) == 0) {
    return(empty)
  }
  df <- merged_df
  if (isTRUE(eq_only) && "quality_flag" %in% names(df)) {
    df <- df[!is.na(df$quality_flag) & df$quality_flag %in% 1, , drop = FALSE]
  }
  fs <- suppressWarnings(as.numeric(df$f_score))
  keep <- is.finite(df$upside_cagr_pct)
  if (isTRUE(gate_only)) {
    keep <- keep & is.finite(fs) & fs >= 7
  }
  if (isTRUE(no_alert) && "n_fs_alerts" %in% names(df)) {
    na_n <- suppressWarnings(as.numeric(df$n_fs_alerts))
    keep <- keep & is.finite(na_n) & na_n <= 0
  }
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) return(df)
  df <- df[order(-df$upside_cagr_pct, df$ticker), , drop = FALSE]
  df[!duplicated(df$ticker), , drop = FALSE]
}

#' 排行產業下拉：全部 + 目前排行候選池內的產業標籤
#' @return named character vector（顯示名 = 值；`__all__` 為全部）
lab_leaderboard_industry_choices <- function(merged_df, eq_only = FALSE,
                                            gate_only = TRUE,
                                            no_alert = FALSE,
                                            all_label = "全部產業") {
  pool <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  labs <- if (nrow(pool) > 0 && "industry_label" %in% names(pool)) {
    unique(as.character(pool$industry_label))
  } else {
    character(0)
  }
  labs <- labs[nzchar(labs) & !is.na(labs)]
  labs <- sort(unique(labs))
  out <- c(stats::setNames("__all__", all_label))
  if (length(labs)) {
    out <- c(out, stats::setNames(labs, labs))
  }
  out
}

#' 績優排行榜：同一評估／明細集合中，依年化漲幅降序取 Top-K（預設再套 F-Score≥7）
#' （只截斷顯示，不另抽樣；輸入應已是本次評估的 N 檔。）
#' @param eq_only 若 TRUE，再只保留盈餘品質通過者（與 Piotroski 高門檻獨立）
#' @param gate_only 若 TRUE，只列 F-Score≥7；FALSE＝不設 F 門檻
#' @param no_alert 若 TRUE，只列財報警訊數＝0者
#' @param scope `"overall"`＝整體前十（跨產業、含產業欄；依年化估值漲幅）；
#'   `"by_industry"`＝選定產業內前十（單一 Top-K；產業內排名自該產業第 1 名起算）；
#'   `"industry_avg"`＝產業市值加權年化估值漲幅排名（每元市值加權平均）；
#'   `"undervalued"`＝價值低估前十（跨產業；依推薦主模型總潛在漲幅＝FV vs 市價差距，非年化）
#' @param industry_filter 選定產業：`industry_key` 或 `industry_label` 字串向量；
#'   `NULL`／空／`"__all__"`＝不另限產業（由呼叫端決定是否已篩過）
lab_quality_leaderboard <- function(merged_df, top_n = 10L, eq_only = FALSE,
                                    gate_only = TRUE, no_alert = FALSE,
                                    scope = c("overall", "by_industry", "industry_avg",
                                              "undervalued"),
                                    industry_filter = NULL) {
  scope <- match.arg(scope)
  if (identical(scope, "industry_avg")) {
    return(lab_industry_mcap_upside_leaderboard(
      merged_df, top_n = top_n, eq_only = eq_only, gate_only = gate_only,
      no_alert = no_alert, industry_filter = industry_filter
    ))
  }
  # Overall / undervalued: Rank, Ticker, Company, Industry, Model, upsides, F-Score.
  # Selected-industry Top 10 also inserts Industry, then Industry rank, after Rank.
  lb_cols_cross <- c(
    "排名", "代號", "公司名稱", "產業", "估值方法",
    "年化估值漲幅", "總潛在漲幅", "F-Score"
  )
  lb_cols_by_ind <- c(
    "排名", "產業", "產業內排名", "代號", "公司名稱", "估值方法",
    "年化估值漲幅", "總潛在漲幅", "F-Score"
  )
  empty <- data.frame(
    排名 = integer(0), 代號 = character(0), 公司名稱 = character(0), 產業 = character(0),
    估值方法 = character(0),
    年化估值漲幅 = character(0),
    總潛在漲幅 = character(0),
    `F-Score` = numeric(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (identical(scope, "by_industry")) {
    empty$產業內排名 <- integer(0)
    empty <- empty[, lb_cols_by_ind, drop = FALSE]
  } else {
    empty <- empty[, lb_cols_cross, drop = FALSE]
  }
  df <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  if (nrow(df) == 0) return(empty)

  indf <- as.character(industry_filter %||% character(0))
  indf <- indf[!is.na(indf) & nzchar(indf)]
  indf <- indf[!indf %in% c("__all__", "NULL", "null")]
  if (length(indf) > 0L) {
    ind_lab <- if ("industry_label" %in% names(df)) as.character(df$industry_label) else rep("", nrow(df))
    ind_key <- if ("industry_key" %in% names(df)) as.character(df$industry_key) else rep("", nrow(df))
    keep <- (ind_key %in% indf) | (ind_lab %in% indf)
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  top_n <- max(1L, as.integer(top_n)[1])
  .fmt_lb_rows <- function(sub, ind_rank_values = NULL) {
    if (nrow(sub) == 0) return(NULL)
    meth <- as.character(sub$method_used)
    prim <- as.character(sub$primary)
    miss <- is.na(meth) | !nzchar(meth)
    meth[miss] <- prim[miss]
    meth <- toupper(meth)
    meth[is.na(meth) | !nzchar(meth)] <- "—"
    yahoo_nm <- if ("company_name" %in% names(sub)) sub$company_name else NA_character_
    names_out <- vapply(seq_len(nrow(sub)), function(i) {
      nm <- lab_company_display_name(sub$ticker[[i]], yahoo_nm[[i]])
      if (!nzchar(nm) || identical(nm, "—")) as.character(sub$ticker[[i]]) else nm
    }, character(1))
    ind_lab <- if ("industry_label" %in% names(sub)) {
      as.character(sub$industry_label)
    } else {
      rep("—", nrow(sub))
    }
    ind_lab[is.na(ind_lab) | !nzchar(ind_lab)] <- "—"
    out <- data.frame(
      排名 = seq_len(nrow(sub)),
      代號 = if (exists("display_tickers_for_market", mode = "function")) {
        display_tickers_for_market(sub$ticker, tryCatch(get_market_mode(), error = function(e) "US"))
      } else {
        sub$ticker
      },
      公司名稱 = names_out,
      產業 = ind_lab,
      估值方法 = meth,
      年化估值漲幅 = sprintf("%+.1f%%", sub$upside_cagr_pct),
      總潛在漲幅 = ifelse(
        is.na(sub$upside_total_pct), "—", sprintf("%+.1f%%", sub$upside_total_pct)
      ),
      `F-Score` = sub$f_score,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cols <- lb_cols_cross
    if (!is.null(ind_rank_values) && length(ind_rank_values) == nrow(sub)) {
      out$產業內排名 <- as.integer(ind_rank_values)
      cols <- lb_cols_by_ind
    }
    out[, cols, drop = FALSE]
  }

  # Within-industry rank: sort by industry then CAGR so each industry starts at 1
  ind_key_vec <- if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else if ("industry_label" %in% names(df)) {
    as.character(df$industry_label)
  } else {
    rep("", nrow(df))
  }
  ind_key_vec[is.na(ind_key_vec) | !nzchar(ind_key_vec)] <- "—"
  o_ind <- order(
    is.na(df$upside_cagr_pct), ind_key_vec, -df$upside_cagr_pct, df$ticker,
    na.last = TRUE
  )
  df_ind <- df[o_ind, , drop = FALSE]
  ind_key_sorted <- ind_key_vec[o_ind]
  df_ind$.ind_rank <- ave(seq_len(nrow(df_ind)), ind_key_sorted, FUN = seq_along)

  if (identical(scope, "undervalued")) {
    # 價值低估前十：推薦主模型總潛在漲幅（FV vs 市價，非年化）由大到小；只列仍被低估者
    tot <- suppressWarnings(as.numeric(df_ind$upside_total_pct))
    keep_uv <- is.finite(tot) & tot > 0
    df_uv <- df_ind[keep_uv, , drop = FALSE]
    tot <- tot[keep_uv]
    if (nrow(df_uv) == 0L) return(empty)
    o_uv <- order(-tot, df_uv$ticker, na.last = TRUE)
    df_uv <- df_uv[o_uv, , drop = FALSE]
    df_uv <- head(df_uv, top_n)
    out <- .fmt_lb_rows(df_uv)
    if (is.null(out) || nrow(out) == 0) return(empty)
    return(out)
  }

  # Cross-industry Top-K still by global CAGR (single list)
  o_glob <- order(
    is.na(df_ind$upside_cagr_pct), -df_ind$upside_cagr_pct, df_ind$ticker,
    na.last = TRUE
  )
  df_glob <- df_ind[o_glob, , drop = FALSE]
  df_glob <- head(df_glob, top_n)

  if (identical(scope, "by_industry")) {
    # 產業內排名 = that name's rank within its industry (1 = best in industry),
    # not its place in the cross-industry Top 10.
    out <- .fmt_lb_rows(df_glob, ind_rank_values = df_glob$.ind_rank)
    if (is.null(out) || nrow(out) == 0) return(empty)
    return(out)
  }

  out <- .fmt_lb_rows(df_glob)
  if (is.null(out) || nrow(out) == 0) return(empty)
  out
}

#' Industry ranking by market-cap-weighted average annualized valuation upside
#' (每元市值加權：Σ(mcap × upside) / Σ(mcap)；缺市值列不計入權重)
lab_industry_mcap_upside_leaderboard <- function(merged_df, top_n = 10L,
                                                eq_only = FALSE, gate_only = TRUE,
                                                no_alert = FALSE,
                                                industry_filter = NULL,
                                                locale = "zh-TW") {
  empty <- data.frame(
    排名 = integer(0),
    產業 = character(0),
    市值加權年化估值漲幅 = character(0),
    總市值 = character(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  df <- lab_leaderboard_pool(
    merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
  )
  if (nrow(df) == 0) return(empty)

  indf <- as.character(industry_filter %||% character(0))
  indf <- indf[!is.na(indf) & nzchar(indf)]
  indf <- indf[!indf %in% c("__all__", "NULL", "null")]
  if (length(indf) > 0L) {
    ind_lab <- if ("industry_label" %in% names(df)) as.character(df$industry_label) else rep("", nrow(df))
    ind_key <- if ("industry_key" %in% names(df)) as.character(df$industry_key) else rep("", nrow(df))
    keep <- (ind_key %in% indf) | (ind_lab %in% indf)
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  # Ensure market_cap when missing (best-effort; offline snapshot / Yahoo)
  if (!"market_cap" %in% names(df) ||
      !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
             suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
    if (exists("lab_attach_market_caps", mode = "function")) {
      df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
    }
  }
  mcap <- suppressWarnings(as.numeric(df$market_cap))
  ups <- suppressWarnings(as.numeric(df$upside_cagr_pct))
  ok <- is.finite(mcap) & mcap > 0 & is.finite(ups)
  df <- df[ok, , drop = FALSE]
  mcap <- mcap[ok]
  ups <- ups[ok]
  if (nrow(df) == 0) return(empty)

  ind_lab <- if ("industry_label" %in% names(df)) {
    as.character(df$industry_label)
  } else if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else {
    rep("—", nrow(df))
  }
  ind_lab[is.na(ind_lab) | !nzchar(ind_lab)] <- "—"
  ind_key <- if ("industry_key" %in% names(df)) {
    as.character(df$industry_key)
  } else {
    ind_lab
  }
  ind_key[is.na(ind_key) | !nzchar(ind_key)] <- ind_lab[is.na(ind_key) | !nzchar(ind_key)]

  keys <- unique(ind_key)
  rows <- lapply(keys, function(k) {
    hit <- which(ind_key == k)
    w <- mcap[hit]
    u <- ups[hit]
    wavg <- sum(w * u) / sum(w)
    lab <- ind_lab[hit][[1]]
    data.frame(
      industry_key = k,
      industry_label = lab,
      wavg_cagr = wavg,
      total_mcap = sum(w),
      stringsAsFactors = FALSE
    )
  })
  agg <- do.call(rbind, rows)
  agg <- agg[order(-agg$wavg_cagr, agg$industry_label), , drop = FALSE]
  top_n <- max(1L, as.integer(top_n)[1])
  agg <- head(agg, top_n)
  if (nrow(agg) == 0) return(empty)

  .fmt_mcap <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x) || x <= 0) return("—")
    if (x >= 1e12) sprintf("%.2fT", x / 1e12)
    else if (x >= 1e9) sprintf("%.2fB", x / 1e9)
    else if (x >= 1e6) sprintf("%.2fM", x / 1e6)
    else sprintf("%.0f", x)
  }
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  ind_disp <- vapply(seq_len(nrow(agg)), function(i) {
    k <- as.character(agg$industry_key[[i]] %||% "")[1]
    if (nzchar(k) && exists("industry_label", mode = "function")) {
      lab <- tryCatch(industry_label(k, locale = loc), error = function(e) NA_character_)
      if (is.character(lab) && length(lab) && nzchar(lab[[1]]) && !is.na(lab[[1]])) {
        return(as.character(lab[[1]]))
      }
    }
    as.character(agg$industry_label[[i]] %||% k)[1]
  }, character(1))
  lab_localize_im_colnames(
    data.frame(
      排名 = seq_len(nrow(agg)),
      產業 = ind_disp,
      市值加權年化估值漲幅 = sprintf("%+.1f%%", agg$wavg_cagr),
      總市值 = vapply(agg$total_mcap, .fmt_mcap, character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    loc
  )
}

# ---- Industry-chain mcap-weighted upside ranking (snapshot + all chains) ----

.lab_ind_mcap_chain_cache <- new.env(parent = emptyenv())

#' Bundled / override path for industry-chain mcap-weighted rank snapshot
lab_ind_mcap_chain_snapshot_path <- function(market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  opt <- getOption("ynow.ind_mcap_chain_snapshot", default = NULL)
  if (!is.null(opt) && length(opt) >= 1L) {
    p <- as.character(opt)[1]
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  candidates <- c(
    file.path("data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm)),
    file.path("data", "ind_mcap_chain_rank_snapshot.csv"),
    file.path("app_22.0", "data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm)),
    file.path("app_22.0", "data", "ind_mcap_chain_rank_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  # Prefer writable cache for saves when bundled file is missing
  cache_dir <- tryCatch({
    if (exists("lab_quality_snapshot_dir", mode = "function")) {
      lab_quality_snapshot_dir()
    } else {
      file.path(tempdir(), "ynow_cache", "slow")
    }
  }, error = function(e) file.path(tempdir(), "ynow_cache", "slow"))
  file.path(cache_dir, sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm))
}

lab_ind_mcap_chain_snapshot_empty <- function() {
  data.frame(
    chain_id = character(0),
    wavg_cagr = numeric(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    snapshot_at = character(0),
    market = character(0),
    stringsAsFactors = FALSE
  )
}

#' Load industry-chain rank snapshot (bundled default or prior 更新排名 result)
lab_load_ind_mcap_chain_snapshot <- function(market = "US", force = FALSE) {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  cache_key <- paste0("df_", mm)
  if (!isTRUE(force) &&
      exists(cache_key, envir = .lab_ind_mcap_chain_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .lab_ind_mcap_chain_cache, inherits = FALSE))
  }
  path <- lab_ind_mcap_chain_snapshot_path(mm)
  empty <- lab_ind_mcap_chain_snapshot_empty()
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    assign(cache_key, empty, envir = .lab_ind_mcap_chain_cache)
    attr(empty, "snapshot_source") <- "missing"
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L || !"chain_id" %in% names(raw)) {
    assign(cache_key, empty, envir = .lab_ind_mcap_chain_cache)
    attr(empty, "snapshot_source") <- "empty"
    return(empty)
  }
  df <- data.frame(
    chain_id = as.character(raw$chain_id),
    wavg_cagr = if ("wavg_cagr" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$wavg_cagr))
    } else {
      NA_real_
    },
    total_mcap = if ("total_mcap" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$total_mcap))
    } else {
      NA_real_
    },
    n_names = if ("n_names" %in% names(raw)) {
      as.integer(suppressWarnings(as.numeric(raw$n_names)))
    } else {
      NA_integer_
    },
    snapshot_at = if ("snapshot_at" %in% names(raw)) {
      as.character(raw$snapshot_at)
    } else {
      rep(NA_character_, nrow(raw))
    },
    market = if ("market" %in% names(raw)) {
      as.character(raw$market)
    } else {
      rep(mm, nrow(raw))
    },
    stringsAsFactors = FALSE
  )
  df <- df[nzchar(df$chain_id) & !is.na(df$chain_id), , drop = FALSE]
  df <- lab_ind_mcap_drop_unmapped_chains(df)
  rownames(df) <- NULL
  attr(df, "snapshot_source") <- path
  if (nrow(df) && any(nzchar(df$snapshot_at), na.rm = TRUE)) {
    attr(df, "snapshot_at") <- as.character(df$snapshot_at[nzchar(df$snapshot_at)][[1]])
  }
  assign(cache_key, df, envir = .lab_ind_mcap_chain_cache)
  df
}

#' Persist industry-chain rank snapshot (bundled data/ when writable; else slow cache)
lab_save_ind_mcap_chain_snapshot <- function(snapshot, market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  snap <- if (is.data.frame(snapshot)) snapshot else lab_ind_mcap_chain_snapshot_empty()
  snap <- lab_ind_mcap_drop_unmapped_chains(snap)
  if (!is.data.frame(snap) || !nrow(snap)) snap <- lab_ind_mcap_chain_snapshot_empty()
  if (!"market" %in% names(snap)) snap$market <- mm
  snap$market[!nzchar(as.character(snap$market))] <- mm
  # Option path wins when set (tests); else bundled data/ when writable
  opt <- getOption("ynow.ind_mcap_chain_snapshot", default = NULL)
  bundled <- file.path("data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm))
  path <- if (!is.null(opt) && length(opt) >= 1L && nzchar(as.character(opt)[1])) {
    as.character(opt)[1]
  } else if (dir.exists("data") && file.access("data", 2) == 0L) {
    bundled
  } else {
    lab_ind_mcap_chain_snapshot_path(mm)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(snap, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  file.rename(tmp, path)
  cache_key <- paste0("df_", mm)
  out_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  attr(snap, "snapshot_source") <- out_path
  if (nrow(snap) && any(nzchar(as.character(snap$snapshot_at)), na.rm = TRUE)) {
    attr(snap, "snapshot_at") <- as.character(
      snap$snapshot_at[nzchar(as.character(snap$snapshot_at))][[1]]
    )
  }
  assign(cache_key, snap, envir = .lab_ind_mcap_chain_cache)
  out_path
}

#' Chain ids excluded from Industry cap-weighted upside ranking (Unmapped bucket).
lab_ind_mcap_unmapped_chain_ids <- function() {
  ids <- c("未對應", "Unmapped", "unmapped")
  if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) {
    ids <- c(ids, as.character(LAB_UNMAPPED_KEY)[1])
  }
  unique(ids[nzchar(ids) & !is.na(ids)])
}

#' TRUE when `chain_id` is the Unmapped industry-chain bucket.
lab_ind_mcap_is_unmapped_chain <- function(chain_id) {
  cid <- trimws(as.character(chain_id %||% "")[1])
  if (!nzchar(cid) || is.na(cid)) return(TRUE)
  drop <- lab_ind_mcap_unmapped_chain_ids()
  if (cid %in% drop) return(TRUE)
  identical(tolower(cid), "unmapped") ||
    identical(tolower(cid), "unmapped industry")
}

#' Drop Unmapped rows from a chain aggregate / snapshot frame.
lab_ind_mcap_drop_unmapped_chains <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  if (!"chain_id" %in% names(df)) return(df)
  keep <- !vapply(df$chain_id, lab_ind_mcap_is_unmapped_chain, logical(1))
  out <- df[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Per-chain universe market-cap totals (ALL names in catalog, not eval sample).
#' Unmapped／未對應 chains are dropped (same rule as the upside board).
lab_chain_mcap_universe_totals <- function(universe_df) {
  empty <- data.frame(
    chain_id = character(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(universe_df) || !is.data.frame(universe_df) || nrow(universe_df) < 1L) {
    return(empty)
  }
  df <- universe_df
  if (!"market_cap" %in% names(df) ||
      !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
             suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
    if (exists("lab_attach_market_caps", mode = "function")) {
      df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
    }
  }
  mcap <- suppressWarnings(as.numeric(df$market_cap))
  ok <- is.finite(mcap) & mcap > 0
  df <- df[ok, , drop = FALSE]
  mcap <- mcap[ok]
  if (!nrow(df)) return(empty)
  chain_ids <- if ("industry_key" %in% names(df) &&
                   exists("industry_chain_id", mode = "function")) {
    vapply(as.character(df$industry_key), function(k) {
      cid <- tryCatch(industry_chain_id(k), error = function(e) "")
      as.character(cid %||% "")[1]
    }, character(1))
  } else if ("chain_id" %in% names(df)) {
    as.character(df$chain_id)
  } else {
    rep("", nrow(df))
  }
  miss <- is.na(chain_ids) | !nzchar(chain_ids)
  if (any(miss) && "industry_label" %in% names(df)) {
    chain_ids[miss] <- as.character(df$industry_label[miss])
  }
  keep <- !vapply(chain_ids, lab_ind_mcap_is_unmapped_chain, logical(1))
  df <- df[keep, , drop = FALSE]
  mcap <- mcap[keep]
  chain_ids <- chain_ids[keep]
  if (!nrow(df)) return(empty)
  keys <- unique(chain_ids)
  rows <- lapply(keys, function(k) {
    hit <- which(chain_ids == k)
    data.frame(
      chain_id = k,
      total_mcap = sum(mcap[hit]),
      n_names = length(hit),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  lab_ind_mcap_drop_unmapped_chains(out)
}

#' Format raw chain snapshot / aggregate rows into the DT display table.
lab_format_ind_mcap_chain_table <- function(agg_df, locale = "zh-TW") {
  empty <- data.frame(
    排名 = integer(0),
    產業鏈 = character(0),
    市值加權年化估值漲幅 = character(0),
    總市值 = character(0),
    檔數 = integer(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  agg_df <- lab_ind_mcap_drop_unmapped_chains(agg_df)
  if (is.null(agg_df) || !is.data.frame(agg_df) || nrow(agg_df) == 0L) {
    return(lab_localize_im_colnames(empty, locale))
  }
  .fmt_mcap <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x) || x <= 0) return("—")
    if (x >= 1e12) sprintf("%.2fT", x / 1e12)
    else if (x >= 1e9) sprintf("%.2fB", x / 1e9)
    else if (x >= 1e6) sprintf("%.2fM", x / 1e6)
    else sprintf("%.0f", x)
  }
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  n <- nrow(agg_df)
  chain_ids <- as.character(agg_df$chain_id %||% rep("", n))
  labs <- vapply(seq_len(n), function(i) {
    cid <- chain_ids[[i]]
    if (nzchar(cid) && exists("industry_chain_label", mode = "function")) {
      lab <- tryCatch(industry_chain_label(cid, locale = loc), error = function(e) NA_character_)
      if (is.character(lab) && length(lab) && nzchar(lab[[1]]) && !is.na(lab[[1]])) {
        return(as.character(lab[[1]]))
      }
    }
    if ("industry_label" %in% names(agg_df)) {
      alt <- as.character(agg_df$industry_label[[i]] %||% "")[1]
      if (nzchar(alt)) return(alt)
    }
    if (nzchar(cid)) cid else "—"
  }, character(1))
  wavg <- suppressWarnings(as.numeric(agg_df$wavg_cagr))
  wavg_txt <- ifelse(
    is.finite(wavg),
    sprintf("%+.1f%%", wavg),
    "—"
  )
  mcap_txt <- vapply(agg_df$total_mcap, .fmt_mcap, character(1))
  n_names <- if ("n_names" %in% names(agg_df)) {
    as.integer(suppressWarnings(as.numeric(agg_df$n_names)))
  } else {
    rep(NA_integer_, n)
  }
  lab_localize_im_colnames(
    data.frame(
      排名 = seq_len(n),
      產業鏈 = labs,
      市值加權年化估值漲幅 = wavg_txt,
      總市值 = mcap_txt,
      檔數 = n_names,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    loc
  )
}

#' All industry chains ranked by mcap-weighted annualized valuation upside.
#' Includes every mapped chain from the picker (missing data → — / NA), sorted by
#' wavg desc. Unmapped（未對應）is excluded from ranking and display.
#' @param universe_df Full catalog/universe for 總市值／檔數 (all names, not eval sample).
lab_industry_chain_mcap_upside_leaderboard <- function(merged_df,
                                                      eq_only = FALSE,
                                                      gate_only = FALSE,
                                                      no_alert = FALSE,
                                                      locale = "zh-TW",
                                                      include_all_chains = TRUE,
                                                      universe_df = NULL) {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  all_ids <- character(0)
  if (isTRUE(include_all_chains) &&
      exists("lab_industry_chain_picker_choices", mode = "function")) {
    all_ids <- unname(lab_industry_chain_picker_choices(loc))
  } else if (isTRUE(include_all_chains) &&
             exists("industry_chain_picker_choices", mode = "function")) {
    all_ids <- unname(industry_chain_picker_choices(loc))
  }
  all_ids <- unique(as.character(all_ids %||% character(0)))
  all_ids <- all_ids[nzchar(all_ids) & !is.na(all_ids)]
  # Never pad / rank the Unmapped bucket on this board
  all_ids <- all_ids[!vapply(all_ids, lab_ind_mcap_is_unmapped_chain, logical(1))]

  empty_agg <- data.frame(
    chain_id = character(0),
    wavg_cagr = numeric(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    stringsAsFactors = FALSE
  )

  df <- tryCatch(
    lab_leaderboard_pool(
      merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
    ),
    error = function(e) NULL
  )
  agg <- empty_agg
  if (is.data.frame(df) && nrow(df) > 0L) {
    if (!"market_cap" %in% names(df) ||
        !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
               suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
      if (exists("lab_attach_market_caps", mode = "function")) {
        df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
      }
    }
    mcap <- suppressWarnings(as.numeric(df$market_cap))
    ups <- suppressWarnings(as.numeric(df$upside_cagr_pct))
    ok <- is.finite(mcap) & mcap > 0 & is.finite(ups)
    df <- df[ok, , drop = FALSE]
    mcap <- mcap[ok]
    ups <- ups[ok]
    if (nrow(df) > 0L) {
      chain_ids <- if ("industry_key" %in% names(df) &&
                       exists("industry_chain_id", mode = "function")) {
        vapply(as.character(df$industry_key), function(k) {
          cid <- tryCatch(industry_chain_id(k), error = function(e) "")
          if (!nzchar(cid) && "industry_label" %in% names(df)) {
            # fall through
          }
          as.character(cid %||% "")[1]
        }, character(1))
      } else {
        rep("", nrow(df))
      }
      miss <- !nzchar(chain_ids) | is.na(chain_ids)
      if (any(miss) && "industry_label" %in% names(df)) {
        chain_ids[miss] <- as.character(df$industry_label[miss])
      }
      # Drop unmapped / blank chains — do not create an Unmapped rank row
      keep_ch <- !vapply(chain_ids, lab_ind_mcap_is_unmapped_chain, logical(1))
      df <- df[keep_ch, , drop = FALSE]
      mcap <- mcap[keep_ch]
      ups <- ups[keep_ch]
      chain_ids <- chain_ids[keep_ch]
      if (nrow(df) > 0L) {
        keys <- unique(chain_ids)
        rows <- lapply(keys, function(k) {
          hit <- which(chain_ids == k)
          w <- mcap[hit]
          u <- ups[hit]
          data.frame(
            chain_id = k,
            wavg_cagr = sum(w * u) / sum(w),
            # Placeholder — overwritten by full-universe totals below
            total_mcap = sum(w),
            n_names = length(hit),
            stringsAsFactors = FALSE
          )
        })
        agg <- do.call(rbind, rows)
      }
    }
  }

  # 總市值／檔數 = ALL names in the chain universe (not eval / Top-N sample)
  uni_src <- if (is.data.frame(universe_df) && nrow(universe_df) > 0L) {
    universe_df
  } else {
    NULL
  }
  totals <- tryCatch(
    lab_chain_mcap_universe_totals(uni_src),
    error = function(e) NULL
  )
  if (is.data.frame(totals) && nrow(totals) > 0L) {
    if (!nrow(agg)) {
      agg <- data.frame(
        chain_id = totals$chain_id,
        wavg_cagr = NA_real_,
        total_mcap = totals$total_mcap,
        n_names = as.integer(totals$n_names),
        stringsAsFactors = FALSE
      )
    } else {
      idx <- match(agg$chain_id, totals$chain_id)
      hit <- which(is.finite(idx))
      if (length(hit)) {
        agg$total_mcap[hit] <- totals$total_mcap[idx[hit]]
        agg$n_names[hit] <- as.integer(totals$n_names[idx[hit]])
      }
      extra <- setdiff(totals$chain_id, agg$chain_id)
      if (length(extra)) {
        j <- match(extra, totals$chain_id)
        agg <- rbind(
          agg,
          data.frame(
            chain_id = totals$chain_id[j],
            wavg_cagr = NA_real_,
            total_mcap = totals$total_mcap[j],
            n_names = as.integer(totals$n_names[j]),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  agg <- lab_ind_mcap_drop_unmapped_chains(agg)
  if (is.null(agg) || !is.data.frame(agg)) agg <- empty_agg

  if (length(all_ids)) {
    missing <- setdiff(all_ids, agg$chain_id)
    if (length(missing)) {
      agg <- rbind(
        agg,
        data.frame(
          chain_id = missing,
          wavg_cagr = NA_real_,
          total_mcap = NA_real_,
          n_names = 0L,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (!nrow(agg)) {
    return(list(
      aggregate = empty_agg,
      table = lab_format_ind_mcap_chain_table(empty_agg, loc)
    ))
  }
  ord <- order(
    !is.finite(agg$wavg_cagr),
    -ifelse(is.finite(agg$wavg_cagr), agg$wavg_cagr, -Inf),
    agg$chain_id,
    method = "radix"
  )
  agg <- agg[ord, , drop = FALSE]
  rownames(agg) <- NULL
  list(
    aggregate = agg,
    table = lab_format_ind_mcap_chain_table(agg, loc)
  )
}

#' Build / refresh chain-rank snapshot from a merged evaluated frame.
#' @param universe_df Full catalog for 總市值 (all chain names).
lab_build_ind_mcap_chain_snapshot <- function(merged_df,
                                             market = "US",
                                             eq_only = FALSE,
                                             gate_only = FALSE,
                                             no_alert = FALSE,
                                             locale = "zh-TW",
                                             snapshot_at = NULL,
                                             universe_df = NULL) {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  # #region agent log
  tryCatch({
    payload <- list(
      hypothesisId = "F",
      location = "lab_industry_method.R:lab_build_ind_mcap_chain_snapshot",
      message = "build snapshot (include_all_chains=TRUE fixed)",
      data = list(
        market = mm,
        n_merged = if (is.data.frame(merged_df)) as.integer(nrow(merged_df)) else 0L,
        n_universe = if (is.data.frame(universe_df)) as.integer(nrow(universe_df)) else 0L,
        eq_only = isTRUE(eq_only),
        gate_only = isTRUE(gate_only),
        no_alert = isTRUE(no_alert),
        include_all_chains = TRUE
      ),
      timestamp = as.numeric(Sys.time()) * 1000,
      runId = "ind-mcap-auto"
    )
    cat(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
        "\n", file = "/opt/cursor/logs/debug.log", append = TRUE)
  }, error = function(e) NULL)
  # #endregion
  built <- lab_industry_chain_mcap_upside_leaderboard(
    merged_df,
    eq_only = eq_only,
    gate_only = gate_only,
    no_alert = no_alert,
    locale = locale,
    include_all_chains = TRUE,
    universe_df = universe_df
  )
  agg <- built$aggregate
  if (!is.data.frame(agg) || nrow(agg) == 0L) {
    agg <- lab_ind_mcap_chain_snapshot_empty()
  }
  ts <- as.character(snapshot_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]
  snap <- data.frame(
    chain_id = as.character(agg$chain_id),
    wavg_cagr = suppressWarnings(as.numeric(agg$wavg_cagr)),
    total_mcap = suppressWarnings(as.numeric(agg$total_mcap)),
    n_names = as.integer(suppressWarnings(as.numeric(agg$n_names))),
    snapshot_at = rep(ts, nrow(agg)),
    market = rep(mm, nrow(agg)),
    stringsAsFactors = FALSE
  )
  path <- tryCatch(lab_save_ind_mcap_chain_snapshot(snap, mm), error = function(e) NA_character_)
  list(snapshot = snap, table = built$table, path = path)
}

#' Independent industry-chain mcap-weighted upside job (all chains).
#'
#' Decoupled from Blue Chip page filters (truncate / concepts / N / boards /
#' methods / quality gates). Samples top `per_chain` mcap names per chain,
#' evaluates FV upside, builds + saves the default snapshot.
#'
#' Pure inputs only (safe for ExtendedTask / future workers).
lab_im_run_ind_mcap_job <- function(catlg,
                                    market_mode = "US",
                                    locale = "zh-TW",
                                    per_chain = 2L) {
  mm <- tryCatch(normalize_market_mode(market_mode), error = function(e) "US")
  tryCatch(set_market_mode(mm), error = function(e) NULL)
  loc <- tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  pc <- suppressWarnings(as.integer(per_chain)[1])
  if (!is.finite(pc) || pc < 1L) pc <- 2L

  empty_out <- function(reason) {
    list(
      ok = FALSE,
      reason = as.character(reason)[1],
      snapshot = lab_ind_mcap_chain_snapshot_empty(),
      table = NULL,
      n_eval = 0L,
      n_chains = 0L,
      path = NA_character_,
      market_mode = mm,
      locale = loc
    )
  }

  if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
    return(empty_out("empty_catalog"))
  }
  if (!exists("industry_chain_id", mode = "function") ||
      !exists("lab_industry_chain_picker_choices", mode = "function") ||
      !exists("lab_screen_tickers_fscore", mode = "function")) {
    return(empty_out("missing_helpers"))
  }

  catlg <- catlg
  if (!"chain_id" %in% names(catlg) ||
      !any(nzchar(as.character(catlg$chain_id)), na.rm = TRUE)) {
    catlg$chain_id <- vapply(
      as.character(catlg$industry_key %||% rep(NA_character_, nrow(catlg))),
      function(k) tryCatch(industry_chain_id(k), error = function(e) ""),
      character(1)
    )
  }
  catlg <- tryCatch(lab_attach_market_caps(catlg), error = function(e) catlg)
  mcap <- suppressWarnings(as.numeric(catlg$market_cap))
  if (!any(is.finite(mcap) & mcap > 0, na.rm = TRUE)) {
    return(empty_out("no_market_cap"))
  }

  chains <- unname(lab_industry_chain_picker_choices(loc))
  chains <- chains[nzchar(as.character(chains)) & !is.na(chains)]
  chains <- chains[!vapply(chains, lab_ind_mcap_is_unmapped_chain, logical(1))]
  if (!length(chains)) return(empty_out("no_chains"))

  pick <- lapply(chains, function(cid) {
    sub <- catlg[
      !is.na(catlg$chain_id) & as.character(catlg$chain_id) == as.character(cid) &
        is.finite(suppressWarnings(as.numeric(catlg$market_cap))) &
        suppressWarnings(as.numeric(catlg$market_cap)) > 0,
      ,
      drop = FALSE
    ]
    if (!nrow(sub)) return(NULL)
    sub <- sub[order(-suppressWarnings(as.numeric(sub$market_cap))), , drop = FALSE]
    utils::head(sub, pc)
  })
  pool <- do.call(rbind, Filter(Negate(is.null), pick))
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L) {
    return(empty_out("empty_pool"))
  }
  pool <- pool[!duplicated(toupper(as.character(pool$ticker))), , drop = FALSE]

  cols <- intersect(c("ticker", "industry_key", "primary"), names(pool))
  scores <- tryCatch(
    lab_screen_tickers_fscore(
      pool[, cols, drop = FALSE],
      max_n = nrow(pool),
      progress_cb = NULL
    ),
    error = function(e) NULL
  )
  if (!is.data.frame(scores) || nrow(scores) < 1L) {
    return(empty_out("empty_scores"))
  }

  merged <- lab_merge_catalog_scores(
    catlg,
    scores = scores,
    method_filter = character(0),
    industry_filter = character(0),
    eq_only = FALSE,
    gate_only = FALSE,
    evaluated_only = TRUE
  )
  if (!is.data.frame(merged) || nrow(merged) < 1L) {
    return(empty_out("empty_merged"))
  }
  if (!"market_cap" %in% names(merged) ||
      !any(is.finite(suppressWarnings(as.numeric(merged$market_cap))), na.rm = TRUE)) {
    merged <- tryCatch(lab_attach_market_caps(merged), error = function(e) merged)
  }
  if ("market_cap" %in% names(scores)) {
    idx <- match(toupper(merged$ticker), toupper(scores$ticker))
    hit <- which(is.finite(idx))
    sc_m <- suppressWarnings(as.numeric(scores$market_cap[idx[hit]]))
    use <- is.finite(sc_m) & sc_m > 0
    if (any(use)) merged$market_cap[hit[use]] <- sc_m[use]
  }

  built <- tryCatch(
    lab_build_ind_mcap_chain_snapshot(
      merged,
      market = mm,
      eq_only = FALSE,
      gate_only = FALSE,
      no_alert = FALSE,
      locale = loc,
      universe_df = catlg
    ),
    error = function(e) NULL
  )
  if (is.null(built) || !is.data.frame(built$snapshot) || nrow(built$snapshot) < 1L) {
    return(empty_out("build_failed"))
  }

  list(
    ok = TRUE,
    reason = "ok",
    snapshot = built$snapshot,
    table = built$table,
    n_eval = as.integer(nrow(scores)),
    n_chains = as.integer(nrow(built$snapshot)),
    path = as.character(built$path %||% NA_character_)[1],
    market_mode = mm,
    locale = loc
  )
}

#' Models the query filter can unlock (sidebar order).
#' Absolute Justified P/B (`pb`) unlocks from Fair Value primary/secondary.
#' Relative families (including Book multiples) unlock from Multiples reference.
lab_im_available_primary_methods <- function(catalog) {
  if (is.null(catalog) || !is.data.frame(catalog) || !nrow(catalog)) {
    return(character(0))
  }
  keys <- character(0)
  if ("primary" %in% names(catalog)) {
    p <- unique(as.character(catalog$primary))
    p <- p[nzchar(p) & !is.na(p)]
    p <- intersect(p, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
    keys <- c(keys, p)
  }
  if ("secondary" %in% names(catalog)) {
    s <- unique(as.character(catalog$secondary))
    s <- s[nzchar(s) & !is.na(s)]
    s <- intersect(s, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
    keys <- c(keys, s)
  }
  if ("rel_family" %in% names(catalog)) {
    f <- unique(as.character(catalog$rel_family))
    f <- f[nzchar(f) & !is.na(f)]
    f <- intersect(f, c("enterprise", "earnings", "revenue", "book"))
    keys <- c(keys, f)
  }
  lab_order_methods_like_sidebar(unique(keys))
}

#' 顯示用摘要：依主方法分組的產業數／候選檔數
#' Row order follows sidebar model order
#' (NAV → Justified P/B → DCF → DDM → RI → Enterprise → Earnings → Revenue → Book multiples → SOTP).
#' @param include_all TRUE：側欄模型列都顯示（無主模型者產業數／候選檔數為 0）
#' @param locale UI locale (`en` / `zh-TW`); display labels and column headers follow locale.
lab_method_group_summary <- function(catalog, include_all = FALSE, locale = "zh-TW") {
  labels <- lab_method_labels(locale)
  empty_row <- function(m) {
    data.frame(
      建議評價方法 = as.character(labels[[m]] %||% m),
      產業數 = 0L,
      候選檔數 = 0L,
      method_key = m,
      has_primary = FALSE,
      stringsAsFactors = FALSE
    )
  }
  out <- if (is.null(catalog) || !nrow(catalog)) {
    if (isTRUE(include_all)) {
      do.call(rbind, lapply(LAB_SIDEBAR_METHOD_ORDER, empty_row))
    } else {
      data.frame(
        建議評價方法 = character(0),
        產業數 = integer(0),
        候選檔數 = integer(0),
        method_key = character(0),
        has_primary = logical(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    methods <- if (isTRUE(include_all)) {
      as.character(LAB_SIDEBAR_METHOD_ORDER)
    } else {
      lab_order_methods_like_sidebar(catalog$primary)
    }
    do.call(rbind, lapply(methods, function(m) {
      sub <- if (m %in% c("enterprise", "earnings", "revenue", "book")) {
        if (!"rel_family" %in% names(catalog)) {
          catalog[0, , drop = FALSE]
        } else {
          catalog[as.character(catalog$rel_family) == m, , drop = FALSE]
        }
      } else {
        catalog[as.character(catalog$primary) == m, , drop = FALSE]
      }
      n_tk <- length(unique(stats::na.omit(sub$ticker)))
      data.frame(
        建議評價方法 = as.character(labels[[m]] %||% m),
        產業數 = if (n_tk > 0L) length(unique(sub$industry_key)) else 0L,
        候選檔數 = as.integer(n_tk),
        method_key = m,
        has_primary = n_tk > 0L,
        stringsAsFactors = FALSE
      )
    }))
  }
  lab_localize_im_colnames(out, locale)
}

#' 明細表：F-Score 著色膠囊（盈餘品質／Piotroski 高門檻改由工具列勾選過濾）
lab_html_fscore_pill <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(v),
    '<span class="ynow-lab-pill ynow-lab-pill-muted">—</span>',
    sprintf('<span class="ynow-lab-pill ynow-lab-pill-fs">%s</span>', as.integer(round(v)))
  )
}

lab_df_to_csv_text <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return("(無資料)\n")
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  utils::write.csv(df, tf, row.names = FALSE, fileEncoding = "UTF-8")
  paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

lab_lab_report_use_zip <- function() {
  nzchar(Sys.which("zip"))
}

#' 本頁報告：markdown 表頭 + 摘要／排行／明細 CSV（zip；無 zip 則單一 .md）
lab_write_lab_page_report <- function(dest, header_lines,
                                      summary_df = NULL,
                                      leaderboard_df = NULL,
                                      detail_df = NULL,
                                      as_zip = NULL) {
  if (is.null(as_zip)) as_zip <- lab_lab_report_use_zip()
  hdr <- as.character(header_lines %||% character(0))
  if (isTRUE(as_zip)) {
    td <- tempfile("ynow_lab_report_")
    dir.create(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    writeLines(hdr, file.path(td, "00_header.md"), useBytes = TRUE)
    utils::write.csv(
      if (is.null(summary_df)) data.frame(訊息 = "無摘要") else summary_df,
      file.path(td, "01_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    utils::write.csv(
      if (is.null(leaderboard_df)) data.frame(訊息 = "尚未評估") else leaderboard_df,
      file.path(td, "02_leaderboard.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    utils::write.csv(
      if (is.null(detail_df)) data.frame(訊息 = "無明細") else detail_df,
      file.path(td, "03_detail.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    files <- list.files(td)
    if (file.exists(dest)) unlink(dest)
    owd <- getwd()
    setwd(td)
    on.exit(setwd(owd), add = TRUE)
    zip_ok <- tryCatch({
      utils::zip(dest, files = files, flags = "-r9Xq")
      file.exists(dest) && isTRUE(file.info(dest)$size > 20)
    }, error = function(e) FALSE)
    if (isTRUE(zip_ok)) return(invisible(dest))
  }
  body <- c(
    hdr,
    "",
    "## 摘要（候選檔數）",
    "```csv",
    lab_df_to_csv_text(summary_df),
    "```",
    "",
    "## 績優排行榜",
    "```csv",
    lab_df_to_csv_text(leaderboard_df),
    "```",
    "",
    "## 明細",
    "```csv",
    lab_df_to_csv_text(detail_df),
    "```",
    ""
  )
  writeLines(body, dest, useBytes = TRUE)
  invisible(dest)
}
