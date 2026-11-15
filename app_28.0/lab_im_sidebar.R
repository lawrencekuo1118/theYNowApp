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

# Column titles (zh-TW + en-US) that must sort as market cap / percent even when
# renderDT(..., server = TRUE) sorts in R — so the data frame column must be numeric.
YNOW_DT_MCAP_COL_NAMES <- c("總市值", "Total market cap")
YNOW_DT_PCT_COL_NAMES <- c(
  "市值加權年化估值漲幅", "Mcap-weighted annualized upside",
  "年化估值漲幅", "Annualized upside",
  "總潛在漲幅", "Total upside"
)

#' Parse display market-cap text (`1.20T`, `$800.00B`, em dash) to numeric USD.
ynow_coerce_mcap_sort_value <- function(x) {
  if (is.numeric(x)) {
    v <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(v) || v <= 0) return(NA_real_)
    return(v)
  }
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s) || s %in% c("\u2014", "\u2013", "-", "NA", "NaN")) return(NA_real_)
  s <- gsub(",", "", s, fixed = TRUE)
  s <- sub("^\\$", "", s)
  m <- regexec("^([+-]?[0-9]+(?:\\.[0-9]+)?)([TBMK])$", s, ignore.case = TRUE, perl = TRUE)
  g <- regmatches(s, m)[[1]]
  if (length(g) >= 3L) {
    mul <- c(K = 1e3, M = 1e6, B = 1e9, T = 1e12)[[toupper(g[[3]])]]
    v <- suppressWarnings(as.numeric(g[[2]])) * mul
    if (is.finite(v) && v > 0) return(v)
    return(NA_real_)
  }
  v <- suppressWarnings(as.numeric(s))
  if (is.finite(v) && v > 0) v else NA_real_
}

#' Parse display percent text (`+12.5%`, em dash) to numeric percentage points.
ynow_coerce_pct_sort_value <- function(x) {
  if (is.numeric(x)) {
    v <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(v)) return(NA_real_)
    return(v)
  }
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s) || s %in% c("\u2014", "\u2013", "-", "NA", "NaN")) return(NA_real_)
  s <- gsub(",", "", s, fixed = TRUE)
  s <- sub("%$", "", s)
  v <- suppressWarnings(as.numeric(s))
  if (is.finite(v)) v else NA_real_
}

#' Coerce known mcap / percent display columns to numeric for server-side DT sort.
ynow_dt_numericize_sort_columns <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !ncol(data)) {
    return(list(data = data, mcap_idx = integer(0), pct_idx = integer(0)))
  }
  nm <- names(data)
  mcap_i <- which(nm %in% YNOW_DT_MCAP_COL_NAMES)
  pct_i <- which(nm %in% YNOW_DT_PCT_COL_NAMES)
  for (i in mcap_i) {
    data[[i]] <- vapply(data[[i]], ynow_coerce_mcap_sort_value, numeric(1))
  }
  for (i in pct_i) {
    data[[i]] <- vapply(data[[i]], ynow_coerce_pct_sort_value, numeric(1))
  }
  list(
    data = data,
    mcap_idx = as.integer(mcap_i - 1L),
    pct_idx = as.integer(pct_i - 1L)
  )
}

#' DataTables column render: display formats mcap/percent; sort uses numeric
#' (R numeric columns under server=TRUE, or parsed strings under client-side).
ynow_dt_sort_column_def <- function(mcap_idx = integer(0), pct_idx = integer(0)) {
  mcap_js <- paste0("[", paste(as.integer(mcap_idx), collapse = ","), "]")
  pct_js <- paste0("[", paste(as.integer(pct_idx), collapse = ","), "]")
  list(
    targets = "_all",
    render = DT::JS(
      paste0("function(data, type, row, meta) {"),
      paste0("  var mcapCols = ", mcap_js, ";"),
      paste0("  var pctCols = ", pct_js, ";"),
      "  var col = meta && meta.col != null ? meta.col : -1;",
      "  var isMcap = mcapCols.indexOf(col) >= 0;",
      "  var isPct = pctCols.indexOf(col) >= 0;",
      "  var formatMcap = function(v) {",
      "    if (v == null || v === '') return '\\u2014';",
      "    var x = typeof v === 'number' ? v : parseFloat(v);",
      "    if (!isFinite(x) || x <= 0) return '\\u2014';",
      "    if (x >= 1e12) return (x / 1e12).toFixed(2) + 'T';",
      "    if (x >= 1e9) return (x / 1e9).toFixed(2) + 'B';",
      "    if (x >= 1e6) return (x / 1e6).toFixed(2) + 'M';",
      "    return String(Math.round(x));",
      "  };",
      "  var formatPct = function(v) {",
      "    if (v == null || v === '') return '\\u2014';",
      "    var x = typeof v === 'number' ? v : parseFloat(v);",
      "    if (!isFinite(x)) return '\\u2014';",
      "    return (x > 0 ? '+' : '') + x.toFixed(1) + '%';",
      "  };",
      "  if (type === 'display') {",
      "    if (isMcap) return formatMcap(data);",
      "    if (isPct) return formatPct(data);",
      "    return data;",
      "  }",
      "  if (typeof data === 'number' && isFinite(data)) {",
      "    if (type === 'filter') {",
      "      if (isMcap) return formatMcap(data);",
      "      if (isPct) return formatPct(data);",
      "    }",
      "    return data;",
      "  }",
      "  var s = data == null ? '' : String(data);",
      "  s = s.replace(/<[^>]*>/g, '').replace(/&nbsp;/gi, ' ').replace(/\\s+/g, ' ').trim();",
      "  if (type === 'filter') {",
      "    if (isMcap) return formatMcap(s);",
      "    if (isPct) return formatPct(s);",
      "    return s;",
      "  }",
      "  if (!s || s === '\\u2014' || s === '\\u2013' || s === '-') return null;",
      "  s = s.replace(/,/g, '').replace(/^\\$/, '');",
      "  var pct = s.match(/^([+-]?\\d+(?:\\.\\d+)?)%$/);",
      "  if (pct) return parseFloat(pct[1]);",
      "  var mag = s.match(/^([+-]?\\d+(?:\\.\\d+)?)([TBMK])$/i);",
      "  if (mag) {",
      "    var mul = {K:1e3, M:1e6, B:1e9, T:1e12}[mag[2].toUpperCase()];",
      "    return parseFloat(mag[1]) * mul;",
      "  }",
      "  if (/^[+-]?\\d+(?:\\.\\d+)?(?:[eE][+-]?\\d+)?$/.test(s)) return parseFloat(s);",
      "  return s.toLowerCase();",
      "}"
    )
  )
}

# zh-TW header → stable English Ajax / filter key (DT server=TRUE uses names(data)).
# Locale must NOT rename these keys in the data frame or in-flight Ajax can
# stop() inside DT::dataTablesFilter ("column name not found") → tn/7.
LAB_IM_COL_ZH_TO_EN <- c(
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
  "合理價" = "Fair value",
  "資料覆蓋" = "Coverage"
)

#' Canonical English keys for Blue Chip / Lab DT columns (Ajax-stable).
lab_canonicalize_im_colnames <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  nm <- names(df)
  hit <- nm %in% names(LAB_IM_COL_ZH_TO_EN)
  if (any(hit)) nm[hit] <- unname(LAB_IM_COL_ZH_TO_EN[nm[hit]])
  names(df) <- nm
  df
}

#' Display labels for canonical Lab DT column keys under the active locale.
lab_im_col_display_names <- function(keys, locale = "zh-TW") {
  keys <- as.character(keys %||% character(0))
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (identical(loc, "en") || !length(keys)) return(keys)
  en_to_zh <- setNames(names(LAB_IM_COL_ZH_TO_EN), unname(LAB_IM_COL_ZH_TO_EN))
  hit <- keys %in% names(en_to_zh)
  if (any(hit)) keys[hit] <- unname(en_to_zh[keys[hit]])
  keys
}

# FlatList / RecyclerView equivalent for DT: server-side paging + deferRender
# + capped pageLength so only the visible page stays in the DOM (mobile).
YNOW_DT_VIRTUAL_PAGE_DEFAULT <- 25L

#' Resolve DT pageLength for viewport virtualization.
#'
#' - Small tables (n ≤ virtual_page): show all rows (no paging chrome).
#' - Large tables / "show all" requests: cap to `virtual_page` so 500+ row
#'   Stock Prospecting Detail / Custom Screen results do not mount every `<tr>`.
ynow_dt_resolve_page_length <- function(n, page_length = NULL,
                                        virtual_page = YNOW_DT_VIRTUAL_PAGE_DEFAULT) {
  n_i <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n_i) || n_i < 0L) n_i <- 0L
  vp <- suppressWarnings(as.integer(virtual_page)[1])
  if (!is.finite(vp) || vp < 1L) vp <- YNOW_DT_VIRTUAL_PAGE_DEFAULT
  if (is.null(page_length)) {
    return(if (n_i <= vp) max(n_i, 1L) else vp)
  }
  plen <- suppressWarnings(as.integer(page_length)[1])
  if (!is.finite(plen) || plen < 1L) plen <- vp
  # Explicit "pageLength = nrow" / huge page on a large table → still virtualize.
  if (n_i > vp && plen >= n_i) return(vp)
  plen
}

#' DT language pack (en-US / zh-TW) for pagination chrome on virtualized tables.
ynow_dt_language <- function(locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  s <- function(key, fallback) {
    if (exists("ui_str", mode = "function")) {
      tryCatch({
        out <- ui_str(key, loc)
        if (is.null(out) || !nzchar(as.character(out)[1])) fallback else as.character(out)[1]
      }, error = function(e) fallback)
    } else {
      fallback
    }
  }
  if (identical(loc, "en")) {
    list(
      paginate = list(
        previous = s("dt_paginate_prev", "Prev"),
        `next` = s("dt_paginate_next", "Next")
      ),
      info = s("dt_info", "Showing _START_–_END_ of _TOTAL_"),
      infoEmpty = s("dt_info_empty", "Showing 0–0 of 0"),
      infoFiltered = s("dt_info_filtered", "(filtered from _MAX_ total)"),
      emptyTable = s("dt_empty", "No data"),
      zeroRecords = s("dt_zero", "No matching rows"),
      processing = s("dt_processing", "…")
    )
  } else {
    list(
      paginate = list(
        previous = s("dt_paginate_prev", "上一頁"),
        `next` = s("dt_paginate_next", "下一頁")
      ),
      info = s("dt_info", "顯示第 _START_–_END_ 筆，共 _TOTAL_ 筆"),
      infoEmpty = s("dt_info_empty", "顯示第 0–0 筆，共 0 筆"),
      infoFiltered = s("dt_info_filtered", "（自 _MAX_ 筆中篩選）"),
      emptyTable = s("dt_empty", "尚無資料"),
      zeroRecords = s("dt_zero", "沒有符合的列"),
      processing = s("dt_processing", "…")
    )
  }
}

#' Blue Chip table with clickable headers. Initial row order is unchanged
#' until the user sorts a column.
#'
#' Market-cap and percent columns are coerced to numeric so
#' `DT::renderDT(..., server = TRUE)` sorts by true magnitude (not `"5.10T"` text).
#' Display still shows human-formatted suffixes via the column render.
#'
#' Internal `names(data)` stay on stable English keys so server-side Ajax
#' filter/sort requests survive locale flips (zh-TW ↔ en). Visible headers use
#' `colnames=` only.
#'
#' Large lists (N ≥ ~25, especially 500 / All) use viewport virtualization:
#' capped `pageLength`, `deferRender`, and pagination chrome (`dom = "tip"`)
#' so off-screen rows are not kept in the mobile DOM.
ynow_sortable_datatable <- function(data, escape = TRUE, page_length = NULL,
                                    filter = "none", extra_options = list(),
                                    locale = NULL,
                                    virtual_page = YNOW_DT_VIRTUAL_PAGE_DEFAULT) {
  if (is.null(data) || !is.data.frame(data)) {
    data <- data.frame(Message = character(0), stringsAsFactors = FALSE)
  }
  data <- lab_canonicalize_im_colnames(data)
  prep <- ynow_dt_numericize_sort_columns(data)
  data <- prep$data
  display_names <- if (!is.null(locale)) {
    lab_im_col_display_names(names(data), locale)
  } else {
    names(data)
  }
  n <- nrow(data)
  plen <- ynow_dt_resolve_page_length(n, page_length = page_length, virtual_page = virtual_page)
  if (!is.finite(plen) || plen < 1L) plen <- 1L
  paging <- isTRUE(n > plen)
  # "t" alone hides Prev/Next — fine for Top-10 / empty messages; large lists need "tip".
  base_dom <- if (isTRUE(paging)) "tip" else "t"
  base <- list(
    paging = paging,
    pageLength = plen,
    scrollX = TRUE,
    ordering = TRUE,
    order = list(),
    orderClasses = TRUE,
    deferRender = TRUE,
    processing = isTRUE(paging),
    dom = base_dom,
    language = ynow_dt_language(locale %||% "zh-TW")
  )
  opts <- if (length(extra_options)) utils::modifyList(base, extra_options) else base
  opts$ordering <- TRUE
  # Re-assert paging contract if extra_options tried to remount the whole table.
  if (isTRUE(paging)) {
    opts$paging <- TRUE
    if (is.null(opts$pageLength) || !is.finite(opts$pageLength) ||
        as.integer(opts$pageLength)[1] >= n) {
      opts$pageLength <- plen
    }
    if (identical(as.character(opts$dom %||% "")[1], "t")) opts$dom <- "tip"
  }
  prev_defs <- opts$columnDefs
  opts$columnDefs <- c(
    list(ynow_dt_sort_column_def(prep$mcap_idx, prep$pct_idx)),
    prev_defs
  )
  DT::datatable(
    data,
    rownames = FALSE,
    colnames = display_names,
    escape = escape,
    filter = filter,
    class = "stripe hover cell-border ynow-sortable-dt",
    options = opts
  )
}

#' Rename Blue Chip table column headers for the active UI locale.
#'
#' Prefer `ynow_sortable_datatable(..., locale=)` for DT outputs (Ajax-stable
#' English keys + visible `colnames=`). This helper still renames the data.frame
#' itself for non-DT callers / exports: zh-TW leaves zh headers; en maps to English.
lab_localize_im_colnames <- function(df, locale = "zh-TW") {
  if (is.null(df) || !is.data.frame(df)) return(df)
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  if (!identical(loc, "en")) return(df)
  lab_canonicalize_im_colnames(df)
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
