# ==========================================
# lab_tw_universe.R — Lab 宇宙：台股上市／上櫃（TWSE／TPEx OpenAPI）
# 對齊 lab_sp500_universe.R：快取 CSV、過期可背景更新。
# Yahoo 代號：####.TW（上市）／####.TWO（上櫃）。
# ==========================================

LAB_TW_STALE_DAYS <- 7
LAB_TW_CACHE_REL <- file.path("data", "tw_universe.csv")
LAB_TWSE_COMPANY_URL <- "https://openapi.twse.com.tw/v1/opendata/t187ap03_L"
LAB_TPEX_COMPANY_URL <- "https://www.tpex.org.tw/openapi/v1/mopsfin_t187ap03_O"
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
    "retail.Brick_Mortar" = c("貿易百貨", "觀光事業"),
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

lab_tw_row_to_record <- function(row, exchange, source, fetched_at) {
  code <- .lab_tw_pick_field(row, c("公司代號", "Code", "code", "證券代號", "^股票代號$"))
  name <- .lab_tw_pick_field(row, c("公司名稱", "Name", "name", "證券名稱", "公司簡稱"))
  ind <- .lab_tw_pick_field(row, c("產業別", "Industry", "industry", "產業類別"))
  if (!nzchar(code) || identical(code, "NA")) return(NULL)
  code <- gsub("[^0-9A-Za-z]", "", code)
  if (!nzchar(code)) return(NULL)
  suffix <- if (identical(toupper(exchange), "TPEX") || identical(toupper(exchange), "TWO")) {
    ".TWO"
  } else {
    ".TW"
  }
  y <- paste0(toupper(code), suffix)
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
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::fromJSON(raw, simplifyDataFrame = TRUE))
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
  tpex <- tryCatch(
    lab_finalize_tw_list(
      lab_fetch_tw_json(LAB_TPEX_COMPANY_URL, timeout_sec),
      exchange = "TPEX",
      source = "tpex_openapi",
      fetched_at = ts
    ),
    error = function(e) lab_empty_tw()
  )
  df <- rbind(twse, tpex)
  if (nrow(df) == 0L) stop("TWSE/TPEx OpenAPI returned no usable equities")
  # Prefer TWSE over TPEx if same numeric appears twice
  df <- df[order(df$exchange != "TWSE", df$ticker), , drop = FALSE]
  df <- df[!duplicated(sub("\\.(TW|TWO)$", "", df$ticker, ignore.case = TRUE)), , drop = FALSE]
  df$source <- "twse_tpex_openapi"
  df$fetched_at <- ts
  df
}

lab_tw_cache_path <- function() {
  file.path(getwd(), LAB_TW_CACHE_REL)
}

lab_write_tw_cache <- function(df) {
  path <- lab_tw_cache_path()
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
  list(
    n = nrow(u),
    n_unmapped = sum(u$industry_key == (if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"), na.rm = TRUE),
    fetched_at = if (nrow(u)) as.character(u$fetched_at[[1]]) else "",
    source = if (nrow(u)) as.character(u$source[[1]]) else ""
  )
}

#' 自上市櫃宇宙查公司中文全稱（依 Yahoo 代號，如 2330.TW／6488.TWO）
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
    # 優先上市 .TW
    if (length(hit) > 1L) {
      tw <- hit[grepl("\\.TW$", tks[hit])]
      if (length(tw)) hit <- tw
    }
  }
  if (!length(hit)) return("")
  nm <- trimws(as.character(u$name[[hit[[1]]]]))
  if (!nzchar(nm) || identical(nm, "NA") || identical(toupper(nm), tk)) return("")
  nm
}

#' 台股績優候選：industry_key → tickers
lab_tw_quality_candidates <- function() {
  u <- tryCatch(lab_get_tw_universe(FALSE), error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) == 0L) return(list())
  tks <- as.character(u$ticker)
  keys <- as.character(u$industry_key)
  keep <- nzchar(tks) & !is.na(tks) & nzchar(keys) & !is.na(keys)
  tks <- tks[keep]
  keys <- keys[keep]
  if (!length(tks)) return(list())
  split(tks, keys)
}

tryCatch(lab_get_tw_universe(force_refresh = FALSE), error = function(e) NULL)
