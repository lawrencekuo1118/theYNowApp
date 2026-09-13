# ==========================================
# lab_tw_universe.R — Lab 宇宙：台股上市／上櫃／興櫃
# 對齊 lab_sp500_universe.R：快取 CSV、過期可背景更新。
# 資料來源（優先 OpenAPI；失敗則 TWSE ISIN 公開一覽）：
#   上市  OpenAPI t187ap03_L ／ ISIN strMode=2
#   上櫃  OpenAPI mopsfin_t187ap03_O ／ ISIN strMode=4
#   興櫃  OpenAPI mopsfin_t187ap03_R ／ ISIN strMode=5
# Yahoo 代號：####.TW（上市）／####.TWO（上櫃與興櫃；Yahoo exchangeName=TWO）
# 搜尋／解析：三板皆可；Blue Chip 績優篩選僅上市＋上櫃（排除興櫃流動性／資料品質風險）
# ==========================================

LAB_TW_STALE_DAYS <- 7
LAB_TW_CACHE_REL <- file.path("data", "tw_universe.csv")
LAB_TWSE_COMPANY_URL <- "https://openapi.twse.com.tw/v1/opendata/t187ap03_L"
LAB_TPEX_COMPANY_URL <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap03_O"
LAB_TPEX_ESB_COMPANY_URL <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap03_R"
# TWSE ISIN 公開一覽：2=上市、4=上櫃、5=興櫃（官方 HTML；OpenAPI 傳輸不穩時備援）
LAB_TW_ISIN_LISTED_URL <- "https://isin.twse.com.tw/isin/C_public.jsp?strMode=2"
LAB_TW_ISIN_OTC_URL <- "https://isin.twse.com.tw/isin/C_public.jsp?strMode=4"
LAB_TW_ISIN_ESB_URL <- "https://isin.twse.com.tw/isin/C_public.jsp?strMode=5"
LAB_TW_INDEX_ETFS <- c("0050.TW", "0056.TW", "006208.TW", "00878.TW", "^TWII")

.lab_tw_env <- new.env(parent = emptyenv())

lab_empty_tw <- function() {
  data.frame(
    ticker = character(0),
    name = character(0),
    exchange = character(0),
    industry_raw = character(0),
    industry_key = character(0),
    fetched_at = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
}

#' 粗略對應證交所產業別 → App industry_standards 鍵
lab_map_tw_industry_to_key <- function(industry_raw) {
  s <- as.character(industry_raw %||% "")[1]
  if (!nzchar(s) || identical(s, "NA")) {
    return(if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped")
  }
  rules <- list(
    "sc.Foundry" = c("半導體", "電子工業"),
    "ec.Hardware" = c("電腦及週邊", "光電", "電子零組件", "其他電子", "通信網路", "資訊服務"),
    "auto.Parts_Suppliers" = c("汽車工業"),
    "fn.Banking" = c("金融保險", "銀行"),
    "fn.Asset_Management" = c("證券"),
    "en.Utilities" = c("油電燃氣", "電器電纜"),
    "mat.Chemicals" = c("化學工業", "塑膠工業", "橡膠工業"),
    "mat.Metals_Mining" = c("鋼鐵工業", "油電"),
    "ind.Machinery" = c("電機機械", "建材營造"),
    "ind.Construction" = c("建材營造", "水泥工業"),
    "fmcg.Food_Beverages" = c("食品工業"),
    "hc.Medtech" = c("生技醫療", "醫療"),
    "hc.Biotech" = c("生技"),
    "tel.Telecom" = c("通信網路", "電信"),
    "tr.Logistics_Shipping" = c("航運業", "貿易百貨"),
    "retail.Brick_Mortar" = c("貿易百貨", "觀光事業", "觀光餐旅"),
    "media.Entertainment" = c("文化創意"),
    "re.REIT" = c("觀光")
  )
  for (key in names(rules)) {
    for (pat in rules[[key]]) {
      if (grepl(pat, s, fixed = TRUE)) return(key)
    }
  }
  if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"
}

lab_is_excluded_tw_ticker <- function(code, yahoo_sym, name = "") {
  c0 <- toupper(trimws(as.character(code %||% "")[1]))
  y <- toupper(trimws(as.character(yahoo_sym %||% "")[1]))
  nm <- as.character(name %||% "")[1]
  if (!nzchar(c0) && !nzchar(y)) return(TRUE)
  # ETF / 受益憑證 / 特別股／權證粗濾
  if (grepl("ETF|指數|基金|受益|權證|牛證|熊證|特別股", nm)) return(TRUE)
  if (grepl("^[0-9]{4}[A-Z]$", c0) && grepl("特別|甲|乙", nm)) return(TRUE)
  # 常見 ETF 代號（含首位 0）
  if (grepl("^00[0-9]{2}", c0) || grepl("^00[0-9]{2}\\.TW", y)) return(TRUE)
  y %in% LAB_TW_INDEX_ETFS
}

.lab_tw_pick_field <- function(row, patterns) {
  nms <- names(row)
  if (is.null(nms) || !length(nms)) return(NA_character_)
  for (pat in patterns) {
    hit <- grepl(pat, nms, ignore.case = TRUE, perl = TRUE)
    if (any(hit)) {
      v <- row[[which(hit)[1]]]
      return(trimws(as.character(v %||% "")[1]))
    }
  }
  NA_character_
}

#' Yahoo 後綴：上市 .TW；上櫃／興櫃皆 .TWO（Yahoo exchangeName=TWO）
lab_tw_yahoo_suffix <- function(exchange) {
  ex <- toupper(trimws(as.character(exchange %||% "")[1]))
  if (ex %in% c("TWSE", "TSE", "LISTED")) return(".TW")
  ".TWO"
}

#' 板別優先序（同代號去重）：上市 > 上櫃 > 興櫃
lab_tw_exchange_rank <- function(exchange) {
  ex <- toupper(trimws(as.character(exchange %||% "")[1]))
  if (ex %in% c("TWSE", "TSE", "LISTED")) return(0L)
  if (ex %in% c("TPEX", "TWO", "OTC", "ROTC")) return(1L)
  if (ex %in% c("ESB", "EMERGING", "TPEX_ESB")) return(2L)
  9L
}

lab_tw_row_to_record <- function(row, exchange, source, fetched_at) {
  code <- .lab_tw_pick_field(
    row,
    c(
      "公司代號", "SecuritiesCompanyCode", "Code", "code",
      "證券代號", "^股票代號$"
    )
  )
  name <- .lab_tw_pick_field(
    row,
    c(
      "公司名稱", "CompanyName", "Name", "name",
      "證券名稱", "公司簡稱", "CompanyAbbreviation"
    )
  )
  # Prefer short abbreviation as secondary fill if full name missing
  if (!nzchar(name) || identical(name, "NA")) {
    name <- .lab_tw_pick_field(row, c("CompanyAbbreviation", "公司簡稱"))
  }
  ind <- .lab_tw_pick_field(
    row,
    c("產業別", "SecuritiesIndustryCode", "Industry", "industry", "產業類別")
  )
  if (!nzchar(code) || identical(code, "NA")) return(NULL)
  code <- gsub("[^0-9A-Za-z]", "", code)
  if (!nzchar(code)) return(NULL)
  y <- paste0(toupper(code), lab_tw_yahoo_suffix(exchange))
  if (lab_is_excluded_tw_ticker(code, y, name)) return(NULL)
  key <- lab_map_tw_industry_to_key(ind)
  data.frame(
    ticker = y,
    name = if (nzchar(name) && !identical(name, "NA")) name else y,
    exchange = as.character(exchange)[1],
    industry_raw = if (nzchar(ind) && !identical(ind, "NA")) ind else "",
    industry_key = key,
    fetched_at = as.character(fetched_at)[1],
    source = as.character(source)[1],
    stringsAsFactors = FALSE
  )
}

lab_fetch_tw_json <- function(url, timeout_sec = 20) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  ok <- FALSE
  if (exists(".lab_http_download", mode = "function")) {
    ok <- isTRUE(.lab_http_download(url, tmp, timeout_sec))
  } else {
    ok <- tryCatch({
      utils::download.file(url, destfile = tmp, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE)
  }
  if (!isTRUE(ok) || !file.exists(tmp) || isTRUE(file.info(tmp)$size < 20)) {
    stop("TW universe download failed: ", url)
  }
  raw <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!nzchar(raw)) stop("empty TW universe body")
  # Reject truncated JSON (TPEx OpenAPI occasionally resets mid-transfer)
  if (!grepl("\\]\\s*$", raw) && !grepl("\\}\\s*$", raw)) {
    stop("TW universe JSON looks truncated: ", url)
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyDataFrame = TRUE),
      error = function(e) stop("TW universe JSON parse failed: ", conditionMessage(e))
    )
    return(parsed)
  }
  stop("jsonlite required for TW universe")
}

lab_finalize_tw_list <- function(lst, exchange, source, fetched_at) {
  empty <- lab_empty_tw()
  if (is.null(lst)) return(empty)
  if (is.data.frame(lst)) {
    rows <- lapply(seq_len(nrow(lst)), function(i) {
      lab_tw_row_to_record(lst[i, , drop = FALSE], exchange, source, fetched_at)
    })
  } else if (is.list(lst)) {
    rows <- lapply(lst, function(row) {
      if (is.data.frame(row)) row <- as.list(row[1, , drop = TRUE])
      lab_tw_row_to_record(row, exchange, source, fetched_at)
    })
  } else {
    return(empty)
  }
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  df <- do.call(rbind, rows)
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  df
}

#' TWSE ISIN 公開一覽 → 宇宙列
#' @param url ISIN C_public.jsp URL（strMode=2/4/5）
#' @param exchange TWSE／TPEX／ESB
#' @param market_pat 市場別關鍵字（上市／上櫃／興櫃）
lab_fetch_tw_isin_board <- function(url, exchange, market_pat, timeout_sec = 30,
                                    source_tag = "twse_isin") {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  ok <- FALSE
  if (exists(".lab_http_download", mode = "function")) {
    ok <- isTRUE(.lab_http_download(url, tmp, timeout_sec))
  } else {
    ok <- tryCatch({
      utils::download.file(url, destfile = tmp, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE)
  }
  if (!isTRUE(ok) || !file.exists(tmp) || isTRUE(file.info(tmp)$size < 200)) {
    stop("TW ISIN download failed: ", url)
  }
  html <- NULL
  for (enc in c("CP950", "BIG5", "UTF-8")) {
    html <- tryCatch(
      paste(readLines(tmp, warn = FALSE, encoding = enc), collapse = "\n"),
      error = function(e) NULL
    )
    if (!is.null(html) && !identical(Encoding(html), "UTF-8")) {
      html <- tryCatch(
        iconv(html, from = enc, to = "UTF-8", sub = "byte"),
        error = function(e) html
      )
    }
    if (!is.null(html) && grepl(market_pat, html, fixed = TRUE)) break
  }
  if (is.null(html) || !nzchar(html)) stop("TW ISIN decode failed: ", url)

  trs <- regmatches(html, gregexpr("(?is)<tr[^>]*>.*?</tr>", html, perl = TRUE))[[1]]
  if (!length(trs)) stop("TW ISIN: no table rows")

  strip_tags <- function(s) {
    s <- gsub("(?is)<[^>]+>", "", s, perl = TRUE)
    s <- gsub("&nbsp;", " ", s, fixed = TRUE)
    s <- gsub("\u00a0", " ", s, fixed = TRUE)
    trimws(s)
  }

  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rows <- list()
  for (tr in trs) {
    tds <- regmatches(tr, gregexpr("(?is)<td[^>]*>.*?</td>", tr, perl = TRUE))[[1]]
    if (length(tds) < 5L) next
    cell <- strip_tags(tds[[1]])
    m <- regexec("^([0-9]{4,6}[A-Za-z]?)[\\s\u3000]+(.+)$", cell, perl = TRUE)
    mm <- regmatches(cell, m)[[1]]
    if (length(mm) < 3L) next
    code <- mm[[2]]
    name <- mm[[3]]
    market <- strip_tags(tds[[4]])
    industry <- strip_tags(tds[[5]])
    cfi <- if (length(tds) >= 6L) strip_tags(tds[[6]]) else ""
    if (nzchar(market) && !grepl(market_pat, market, fixed = TRUE)) next
    # 普通股 CFI 多以 ES 開頭；過濾權證／ETF 等
    if (nzchar(cfi) && !grepl("^ES", cfi, ignore.case = TRUE)) next
    rec <- lab_tw_row_to_record(
      data.frame(
        公司代號 = code,
        公司名稱 = name,
        產業別 = industry,
        stringsAsFactors = FALSE
      ),
      exchange = exchange,
      source = source_tag,
      fetched_at = ts
    )
    if (!is.null(rec)) rows[[length(rows) + 1L]] <- rec
  }
  if (!length(rows)) stop("TW ISIN: no usable equities for ", market_pat)
  df <- do.call(rbind, rows)
  df <- df[!duplicated(df$ticker), , drop = FALSE]
  df
}

lab_fetch_tw_isin_otc <- function(timeout_sec = 30) {
  lab_fetch_tw_isin_board(
    LAB_TW_ISIN_OTC_URL, "TPEX", "上櫃", timeout_sec,
    source_tag = "twse_isin_strMode4"
  )
}

lab_fetch_tw_isin_esb <- function(timeout_sec = 30) {
  lab_fetch_tw_isin_board(
    LAB_TW_ISIN_ESB_URL, "ESB", "興櫃", timeout_sec,
    source_tag = "twse_isin_strMode5"
  )
}

lab_fetch_tw_otc_live <- function(timeout_sec = 25) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  openapi <- tryCatch(
    lab_finalize_tw_list(
      lab_fetch_tw_json(LAB_TPEX_COMPANY_URL, timeout_sec),
      exchange = "TPEX",
      source = "tpex_openapi",
      fetched_at = ts
    ),
    error = function(e) lab_empty_tw()
  )
  if (nrow(openapi) >= 100L) return(openapi)
  lab_fetch_tw_isin_otc(max(timeout_sec, 30))
}

lab_fetch_tw_esb_live <- function(timeout_sec = 25) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  openapi <- tryCatch(
    lab_finalize_tw_list(
      lab_fetch_tw_json(LAB_TPEX_ESB_COMPANY_URL, timeout_sec),
      exchange = "ESB",
      source = "tpex_openapi_esb",
      fetched_at = ts
    ),
    error = function(e) lab_empty_tw()
  )
  if (nrow(openapi) >= 50L) return(openapi)
  lab_fetch_tw_isin_esb(max(timeout_sec, 30))
}

#' 若某板直播抓取失敗／過少，回填舊快取該板列（避免刷新把上櫃整板洗掉）
lab_tw_fill_board_from_cache <- function(live_df, exchange_codes, min_n = 1L) {
  live_n <- if (is.null(live_df) || !is.data.frame(live_df)) 0L else nrow(live_df)
  if (live_n >= as.integer(min_n)) return(live_df)
  cached <- tryCatch(lab_read_tw_cache(), error = function(e) lab_empty_tw())
  if (!nrow(cached) || !"exchange" %in% names(cached)) return(live_df)
  ex <- toupper(as.character(cached$exchange))
  codes <- toupper(as.character(exchange_codes))
  keep <- cached[ex %in% codes, , drop = FALSE]
  if (!nrow(keep)) return(live_df)
  keep
}

lab_fetch_tw_universe_live <- function(timeout_sec = 20) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  twse <- tryCatch(
    lab_finalize_tw_list(
      lab_fetch_tw_json(LAB_TWSE_COMPANY_URL, timeout_sec),
      exchange = "TWSE",
      source = "twse_openapi",
      fetched_at = ts
    ),
    error = function(e) lab_empty_tw()
  )
  if (nrow(twse) < 100L) {
    twse <- tryCatch(
      lab_fetch_tw_isin_board(
        LAB_TW_ISIN_LISTED_URL, "TWSE", "上市", max(timeout_sec, 40),
        source_tag = "twse_isin_strMode2"
      ),
      error = function(e) lab_tw_fill_board_from_cache(twse, c("TWSE", "TSE", "LISTED"), 100L)
    )
  }
  tpex <- tryCatch(
    lab_fetch_tw_otc_live(timeout_sec),
    error = function(e) lab_empty_tw()
  )
  tpex <- lab_tw_fill_board_from_cache(tpex, c("TPEX", "TWO", "OTC", "ROTC"), 100L)
  esb <- tryCatch(
    lab_fetch_tw_esb_live(timeout_sec),
    error = function(e) lab_empty_tw()
  )
  esb <- lab_tw_fill_board_from_cache(esb, c("ESB", "EMERGING", "TPEX_ESB"), 50L)

  df <- rbind(twse, tpex, esb)
  if (nrow(df) == 0L) stop("TWSE/TPEx/ISIN returned no usable equities")
  # Prefer TWSE > TPEX > ESB if same numeric appears twice
  df$.rank <- vapply(df$exchange, lab_tw_exchange_rank, integer(1))
  df <- df[order(df$.rank, df$ticker), , drop = FALSE]
  df <- df[!duplicated(sub("\\.(TW|TWO)$", "", df$ticker, ignore.case = TRUE)), , drop = FALSE]
  df$.rank <- NULL
  src_bits <- c()
  if (nrow(twse)) src_bits <- c(src_bits, paste0("twse:", unique(as.character(twse$source))[1]))
  if (nrow(tpex)) src_bits <- c(src_bits, paste0("tpex:", unique(as.character(tpex$source))[1]))
  if (nrow(esb)) src_bits <- c(src_bits, paste0("esb:", unique(as.character(esb$source))[1]))
  df$source <- paste(c("tw_boards", src_bits), collapse = "|")
  df$fetched_at <- ts
  df
}

lab_tw_cache_candidates <- function() {
  unique(c(
    file.path(getwd(), LAB_TW_CACHE_REL),
    LAB_TW_CACHE_REL,
    # tests/ 目錄執行時可回退到 app 根目錄的完整宇宙
    file.path(dirname(getwd()), LAB_TW_CACHE_REL),
    file.path("app_15.0", LAB_TW_CACHE_REL)
  ))
}

#' 讀取用快取路徑：優先選既有且檔案較大者（完整宇宙優於 tests fixture）
lab_tw_cache_path <- function() {
  hits <- lab_tw_cache_candidates()
  hits <- hits[file.exists(hits)]
  if (length(hits)) {
    sizes <- suppressWarnings(as.numeric(file.info(hits)$size))
    sizes[is.na(sizes)] <- 0
    return(normalizePath(hits[[which.max(sizes)]], mustWork = FALSE))
  }
  file.path(getwd(), LAB_TW_CACHE_REL)
}

#' 寫入用路徑：固定寫到目前工作目錄下（app 啟動會 setwd 到 app 根）
lab_tw_cache_write_path <- function() {
  wd <- getwd()
  if (identical(basename(wd), "tests") &&
      dir.exists(file.path(dirname(wd), "data"))) {
    return(file.path(dirname(wd), LAB_TW_CACHE_REL))
  }
  file.path(wd, LAB_TW_CACHE_REL)
}

lab_write_tw_cache <- function(df) {
  path <- lab_tw_cache_write_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

lab_read_tw_cache <- function() {
  path <- lab_tw_cache_path()
  if (!file.exists(path)) return(lab_empty_tw())
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) lab_empty_tw()
  )
  need <- c("ticker", "name", "exchange", "industry_raw", "industry_key", "fetched_at", "source")
  for (nm in need) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  df[, need, drop = FALSE]
}

lab_tw_is_stale <- function(max_days = LAB_TW_STALE_DAYS) {
  meta <- lab_tw_universe_meta()
  fa <- as.character(meta$fetched_at %||% "")[1]
  if (!nzchar(fa) || identical(fa, "NA")) return(TRUE)
  t0 <- tryCatch(as.POSIXct(fa, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), error = function(e) NA)
  if (is.na(t0)) return(TRUE)
  as.numeric(difftime(Sys.time(), t0, units = "days")) > as.numeric(max_days)
}

lab_refresh_tw_universe <- function() {
  df <- lab_fetch_tw_universe_live()
  lab_write_tw_cache(df)
  .lab_tw_env$universe <- df
  df
}

lab_get_tw_universe <- function(force_refresh = FALSE) {
  if (isTRUE(force_refresh)) {
    return(tryCatch(lab_refresh_tw_universe(), error = function(e) {
      cached <- lab_read_tw_cache()
      if (nrow(cached) > 0) return(cached)
      stop(e)
    }))
  }
  if (!is.null(.lab_tw_env$universe) && is.data.frame(.lab_tw_env$universe) &&
      nrow(.lab_tw_env$universe) > 0) {
    return(.lab_tw_env$universe)
  }
  cached <- lab_read_tw_cache()
  if (nrow(cached) > 0) {
    .lab_tw_env$universe <- cached
    return(cached)
  }
  tryCatch({
    df <- lab_refresh_tw_universe()
    df
  }, error = function(e) lab_empty_tw())
}

lab_tw_universe_meta <- function() {
  u <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) lab_empty_tw())
  ex <- toupper(as.character(u$exchange %||% character(0)))
  list(
    n = nrow(u),
    n_twse = sum(ex %in% c("TWSE", "TSE", "LISTED"), na.rm = TRUE),
    n_tpex = sum(ex %in% c("TPEX", "TWO", "OTC", "ROTC"), na.rm = TRUE),
    n_esb = sum(ex %in% c("ESB", "EMERGING", "TPEX_ESB"), na.rm = TRUE),
    n_unmapped = sum(
      u$industry_key == (if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"),
      na.rm = TRUE
    ),
    fetched_at = if (nrow(u)) as.character(u$fetched_at[[1]]) else "",
    source = if (nrow(u)) as.character(u$source[[1]]) else ""
  )
}

#' 自上市／上櫃／興櫃宇宙查公司中文全稱（依 Yahoo 代號，如 2330.TW／6488.TWO）
lookup_tw_universe_company_name <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk)) return("")
  u <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) < 1L ||
      !all(c("ticker", "name") %in% names(u))) {
    return("")
  }
  tks <- toupper(as.character(u$ticker))
  hit <- which(tks == tk)
  if (!length(hit)) {
    bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
    bases <- sub("\\.(TW|TWO)$", "", tks, ignore.case = TRUE)
    hit <- which(bases == bare)
    # 優先上市 .TW；同 .TWO 時優先上櫃（非興櫃）
    if (length(hit) > 1L) {
      tw <- hit[grepl("\\.TW$", tks[hit])]
      if (length(tw)) {
        hit <- tw
      } else if ("exchange" %in% names(u)) {
        ranks <- vapply(as.character(u$exchange[hit]), lab_tw_exchange_rank, integer(1))
        hit <- hit[order(ranks)]
      }
    }
  }
  if (!length(hit)) return("")
  nm <- trimws(as.character(u$name[[hit[[1]]]]))
  if (!nzchar(nm) || identical(nm, "NA") || identical(toupper(nm), tk)) return("")
  nm
}

#' 台股績優候選：industry_key → tickers（僅上市＋上櫃；不含興櫃）
lab_tw_quality_candidates <- function() {
  u <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) return(list())
  if ("exchange" %in% names(u)) {
    ex <- toupper(as.character(u$exchange))
    keep_board <- ex %in% c("TWSE", "TSE", "LISTED", "TPEX", "TWO", "OTC", "ROTC")
    u <- u[keep_board, , drop = FALSE]
  }
  if (nrow(u) == 0L) return(list())
  tks <- as.character(u$ticker)
  keys <- as.character(u$industry_key)
  keep <- nzchar(tks) & !is.na(tks) & nzchar(keys) & !is.na(keys)
  tks <- tks[keep]
  keys <- keys[keep]
  if (!length(tks)) return(list())
  split(tks, keys)
}

tryCatch(lab_get_tw_universe(force_refresh = FALSE), error = function(e) NULL)
