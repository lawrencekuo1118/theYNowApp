# ==========================================
# lab_im_sidebar.R — extracted from lab_industry_method.R (C-STRUCT-03)
# Behavior-preserving structural split; sourced by lab_industry_method.R.
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
