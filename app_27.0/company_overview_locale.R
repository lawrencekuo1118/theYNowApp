# company_overview_locale.R — Company Overview business summary → zh-TW
#
# Yahoo yfinance longBusinessSummary is English-only. Under UI locale zh-TW we:
#   1) Prefer Yahoo TW「主要經營業務」for TW tickers (native Traditional Chinese).
#   2) Else machine-translate the English summary to zh-TW (MyMemory), then apply
#      a Taiwan investment-desk terminology filter (reject Simplified / 陸系 traps).
#   3) On any failure, keep the English source (honest fallback).
#
# Sector / Industry chip labels stay formal English by design (ui_locale.R).

`%||%` <- function(a, b) if (is.null(a)) b else a

.ynow_co_cjk_count <- function(text) {
  s <- as.character(text %||% "")[1]
  if (!nzchar(s)) return(0L)
  m <- gregexpr("[\u4e00-\u9fff]", s, perl = TRUE)[[1]]
  if (length(m) == 1L && identical(m[[1]], -1L)) 0L else as.integer(length(m))
}

.ynow_co_cjk_ratio <- function(text) {
  s <- as.character(text %||% "")[1]
  n <- nchar(s, type = "chars")
  if (!is.finite(n) || n <= 0) return(0)
  .ynow_co_cjk_count(s) / n
}

#' TRUE when text is effectively Latin/English and needs a zh-TW body.
.ynow_co_needs_zh_tw_body <- function(text) {
  s <- trimws(as.character(text %||% "")[1])
  if (!nzchar(s) || identical(s, "None")) return(FALSE)
  .ynow_co_cjk_ratio(s) < 0.08
}

#' Lightweight Mainland / Simplified → Taiwan Traditional desk wording.
.ynow_co_tw_desk_filter <- function(text) {
  s <- as.character(text %||% "")[1]
  if (!nzchar(s)) return(s)
  reps <- c(
    # Simplified orthography
    "\u8f6f\u4ef6" = "\u8edf\u9ad4",       # 软件 → 軟體
    "\u4fe1\u606f" = "\u8cc7\u8a0a",       # 信息 → 資訊
    "\u6570\u636e" = "\u8cc7\u6599",       # 数据 → 資料
    "\u9ed8\u8ba4" = "\u9810\u8a2d",       # 默认 → 預設
    "\u7f51\u7edc" = "\u7db2\u8def",       # 网络 → 網路
    "\u53c2\u6570" = "\u53c3\u6578",       # 参数 → 參數
    "\u7528\u6237" = "\u4f7f\u7528\u8005", # 用户 → 使用者
    "\u8d26\u6237" = "\u5e33\u6236",       # 账户 → 帳戶
    "\u8d28\u91cf" = "\u54c1\u8cea",       # 质量 → 品質
    "\u62a5\u8868" = "\u5831\u8868",       # 报表 → 報表
    # Traditional glyphs but Mainland finance / product wording
    "\u8edf\u4ef6" = "\u8edf\u9ad4",       # 軟件 → 軟體
    "\u7db2\u7d61" = "\u7db2\u8def",       # 網絡 → 網路
    "\u6295\u8cc7\u8005" = "\u6295\u8cc7\u4eba", # 投資者 → 投資人
    "\u98a8\u96aa\u63d0\u793a" = "\u98a8\u96aa\u8b66\u8a9e", # 風險提示 → 風險警語
    "\u5de5\u85dd" = "\u88fd\u7a0b",       # 工藝 → 製程 (MT slip)
    "\u5305\u88dd\u548c\u6e2c\u8a66" = "\u5c01\u88dd\u8207\u6e2c\u8a66", # 包裝和測試 → 封裝與測試
    "\u5305\u88dd\u53ca\u6e2c\u8a66" = "\u5c01\u88dd\u53ca\u6e2c\u8a66",
    "\u82af\u7247" = "\u6676\u7247",       # 芯片 → 晶片
    "\u96c6\u56e2" = "\u96c6\u5718"        # 集团 → 集團
  )
  for (nm in names(reps)) {
    s <- gsub(nm, unname(reps[[nm]]), s, fixed = TRUE)
  }
  s
}

.ynow_co_normalize_tw_symbol <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk)) return("")
  tk <- sub("\\.TW$", "", tk, ignore.case = TRUE)
  tk <- sub("\\.TWO$", "", tk, ignore.case = TRUE)
  tk <- sub("^TW:", "", tk, ignore.case = TRUE)
  # Pure 4-digit TW codes
  if (grepl("^[0-9]{4}$", tk)) return(paste0(tk, ".TW"))
  # Already Yahoo-style
  if (grepl("\\.(TW|TWO)$", tk)) return(tk)
  ""
}

#' Fetch Yahoo TW「主要經營業務」HTML field (Traditional Chinese).
.fetch_yahoo_tw_business_scope <- function(ticker, timeout_sec = 12) {
  sym <- .ynow_co_normalize_tw_symbol(ticker)
  if (!nzchar(sym)) return("")
  url <- paste0("https://tw.stock.yahoo.com/quote/", utils::URLencode(sym, reserved = TRUE), "/profile")
  html <- tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(
        url,
        httr::add_headers(
          `User-Agent` = "Mozilla/5.0 TheYNowApp",
          `Accept-Language` = "zh-TW,zh;q=0.9"
        ),
        httr::timeout(timeout_sec)
      )
      if (httr::http_error(resp)) return("")
      httr::content(resp, as = "text", encoding = "UTF-8")
    } else {
      con <- url(url, open = "rb")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      rawToChar(readBin(con, what = "raw", n = 8e5))
    }
  }, error = function(e) "")
  if (!nzchar(html)) return("")
  # Label then value div (Yahoo TW company profile)
  m <- regexec(
    "\u4e3b\u8981\u7d93\u71df\u696d\u52d9</span></span><div[^>]*>([^<]{8,4000})</div>",
    html,
    perl = TRUE
  )
  hit <- regmatches(html, m)[[1]]
  if (length(hit) < 2L) {
    m2 <- regexec(
      "\u4e3b\u8981\u7d93\u71df\u696d\u52d9</span></span><div class=\"[^\"]+\">([^<]{8,4000})</div>",
      html,
      perl = TRUE
    )
    hit <- regmatches(html, m2)[[1]]
  }
  if (length(hit) < 2L) return("")
  txt <- trimws(gsub("[\r\n\t]+", " ", hit[[2]], perl = TRUE))
  txt <- gsub("\\s{2,}", " ", txt)
  if (.ynow_co_cjk_count(txt) < 8L) return("")
  .ynow_co_tw_desk_filter(txt)
}

.cached_fetch_yahoo_tw_business_scope <- local({
  memo <- NULL
  function(ticker, timeout_sec = 12) {
    if (is.null(memo) && exists(".ynow_slow_cache", inherits = TRUE) &&
        requireNamespace("memoise", quietly = TRUE)) {
      memo <<- tryCatch(
        memoise::memoise(.fetch_yahoo_tw_business_scope, cache = .ynow_slow_cache),
        error = function(e) NULL
      )
    }
    if (!is.null(memo)) {
      return(tryCatch(memo(ticker, timeout_sec = timeout_sec), error = function(e) ""))
    }
    .fetch_yahoo_tw_business_scope(ticker, timeout_sec = timeout_sec)
  }
})

#' MyMemory en→zh-TW (chunked). Returns "" on failure.
.translate_en_to_zh_tw_mymemory <- function(text, timeout_sec = 12) {
  src <- trimws(as.character(text %||% "")[1])
  if (!nzchar(src)) return("")
  # MyMemory free q length is small; chunk by characters.
  chunk_n <- 420L
  chars <- strsplit(src, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- character(0)
  i <- 1L
  while (i <= n) {
    j <- min(i + chunk_n - 1L, n)
    # Prefer break on sentence end within the chunk
    if (j < n) {
      window <- paste(chars[i:j], collapse = "")
      br <- max(
        gregexpr("[.!?]\\s", window, perl = TRUE)[[1]],
        0L
      )
      if (is.finite(br) && br > 80L) j <- i + as.integer(br)
    }
    piece <- paste(chars[i:j], collapse = "")
    i <- j + 1L
    q <- utils::URLencode(piece, reserved = TRUE)
    api <- paste0(
      "https://api.mymemory.translated.net/get?q=", q,
      "&langpair=en%7Czh-TW"
    )
    translated <- tryCatch({
      if (requireNamespace("httr", quietly = TRUE)) {
        resp <- httr::GET(
          api,
          httr::add_headers(`User-Agent` = "Mozilla/5.0 TheYNowApp"),
          httr::timeout(timeout_sec)
        )
        if (httr::http_error(resp)) return("")
        raw <- httr::content(resp, as = "parsed", type = "application/json",
                             encoding = "UTF-8")
      } else {
        raw <- jsonlite::fromJSON(api)
      }
      as.character(raw$responseData$translatedText %||% "")[1]
    }, error = function(e) "")
    if (!nzchar(translated)) return("")
    # Reject MyMemory "QUERY LENGTH LIMIT EXCEEDED" / same-as-source garbage
    if (grepl("QUERY LENGTH|MYMEMORY WARNING", translated, ignore.case = TRUE)) {
      return("")
    }
    out <- c(out, translated)
  }
  paste(out, collapse = "")
}

.cached_translate_en_to_zh_tw <- local({
  memo <- NULL
  function(text) {
    key_src <- trimws(as.character(text %||% "")[1])
    if (!nzchar(key_src)) return("")
    # Hash-keyed wrapper so memoise keys on content, not giant strings alone
    inner <- function(digest_key, payload) {
      invisible(digest_key)
      .translate_en_to_zh_tw_mymemory(payload)
    }
    dig <- tryCatch(
      digest::digest(key_src, algo = "md5"),
      error = function(e) substr(key_src, 1, 64)
    )
    if (is.null(memo) && exists(".ynow_slow_cache", inherits = TRUE) &&
        requireNamespace("memoise", quietly = TRUE)) {
      memo <<- tryCatch(
        memoise::memoise(inner, cache = .ynow_slow_cache),
        error = function(e) NULL
      )
    }
    if (!is.null(memo)) {
      return(tryCatch(memo(dig, key_src), error = function(e) ""))
    }
    inner(dig, key_src)
  }
})

#' Localize Company Overview business-summary body for the active UI locale.
#'
#' @param summary English (or mixed) Yahoo longBusinessSummary
#' @param ticker Search ticker (e.g. 2330 / 2330.TW / AAPL)
#' @param locale UI locale (`en` / `zh-TW`)
#' @param market_mode `US` / `TW` (helps TW Yahoo path)
#' @return Display string (zh-TW when requested and available; else original)
localize_company_business_summary <- function(summary,
                                              ticker = "",
                                              locale = "en",
                                              market_mode = "US") {
  src <- trimws(as.character(summary %||% "")[1])
  if (identical(src, "None")) src <- ""
  loc <- tryCatch(
    normalize_ui_locale(locale),
    error = function(e) {
      loc0 <- tolower(as.character(locale %||% "en")[1])
      if (startsWith(loc0, "zh")) "zh-TW" else "en"
    }
  )
  if (!identical(loc, "zh-TW")) return(src)
  # Already Traditional / Chinese-heavy — still run desk filter
  if (nzchar(src) && !.ynow_co_needs_zh_tw_body(src)) {
    return(.ynow_co_tw_desk_filter(src))
  }

  mode <- tryCatch(
    normalize_market_mode(market_mode),
    error = function(e) {
      m0 <- toupper(as.character(market_mode %||% "US")[1])
      if (identical(m0, "TW")) "TW" else "US"
    }
  )
  tk <- as.character(ticker %||% "")[1]
  tw_sym <- .ynow_co_normalize_tw_symbol(tk)
  if (!nzchar(tw_sym) && identical(mode, "TW") && grepl("^[0-9]{4}", tk)) {
    tw_sym <- .ynow_co_normalize_tw_symbol(substr(tk, 1, 4))
  }

  # Prefer native Yahoo TW business scope for TW names
  if (nzchar(tw_sym) || identical(mode, "TW")) {
    tw_body <- tryCatch(
      .cached_fetch_yahoo_tw_business_scope(if (nzchar(tw_sym)) tw_sym else tk),
      error = function(e) ""
    )
    if (nzchar(tw_body) && .ynow_co_cjk_count(tw_body) >= 8L) {
      return(tw_body)
    }
  }

  if (!nzchar(src)) return("")

  zh <- tryCatch(.cached_translate_en_to_zh_tw(src), error = function(e) "")
  zh <- .ynow_co_tw_desk_filter(zh)
  if (nzchar(zh) && .ynow_co_cjk_count(zh) >= 8L) return(zh)
  src
}
