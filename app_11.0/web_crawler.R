# ==========================================
# web_crawler.R - 資料抓取（app_11.0 雲端版：純 yfinance，不用 Chromote）
# ==========================================

library(rvest)
library(magrittr)
library(purrr)
library(reticulate)
library(memoise)
library(cachem)

# ==========================================
# 🚀 1. 記憶體快取與 Python 爬蟲初始化
# ==========================================
my_cache <- cachem::cache_mem(max_size = 50 * 1024^2, max_age = 3600)

if (!py_available(initialize = TRUE)) {
  reticulate::py_config()
}

tryCatch({
  reticulate::source_python("deep_scraper.py")
  message("✅ Python 深度爬蟲腳本載入成功！")
}, error = function(e) {
  message("⚠️ Python 腳本載入失敗: ", e$message)
})

.empty_summary <- function(stock_code, company_name = NULL) {
  df <- data.frame(
    Item = character(0),
    Value = character(0),
    stringsAsFactors = FALSE
  )
  attr(df, "company_name") <- if (is.null(company_name) || !nzchar(company_name)) {
    stock_code
  } else {
    as.character(company_name)
  }
  df
}

cached_scrape_financials <- memoise::memoise(
  function(stock_code) {
    message(paste("🚀 正在啟動 Python 財報抓取:", stock_code))
    if (!exists("scrape_all_financials", mode = "function")) {
      stop("scrape_all_financials 未載入（Python / reticulate 失敗）")
    }
    normalize_all_financials(scrape_all_financials(stock_code))
  },
  cache = my_cache
)

tryCatch(memoise::forget(cached_scrape_financials), error = function(e) NULL)

# ==========================================
# 🏭 2. 公司／產業資訊
# ==========================================
get_yahoo_industry <- function(stock_code) {
  message(paste("🔍 正在透過 yfinance 抓取公司與產業資訊:", stock_code))

  result <- tryCatch({
    info <- fast_get_company_info(stock_code)
    sector <- info$sector
    industry <- info$industry
    company_name <- info$company_name
    display_text <- paste0("Sector: ", sector, " | Industry: ", industry)
    list(
      company_name = company_name,
      sector = sector,
      industry = industry,
      display_text = display_text
    )
  }, error = function(e) {
    message("⚠️ 產業資訊抓取失敗: ", e$message)
    list(
      company_name = stock_code,
      sector = "N/A",
      industry = "N/A",
      display_text = "Sector: N/A | Industry: N/A"
    )
  })

  result
}

# ==========================================
# 🌐 3. Summary（僅 yfinance，shinyapps 無 Chrome）
# ==========================================
get_summary_data <- function(stock_code) {
  message(paste("🌐 正在讀取 Summary (yfinance):", stock_code))

  tryCatch({
    if (!exists("get_summary_quote", mode = "function")) {
      stop("get_summary_quote 未載入（請確認 deep_scraper.py / requirements.txt）")
    }
    res <- get_summary_quote(stock_code)

    # 新格式：Item / Value 純 list（reticulate 穩定）
    items <- res$Item
    values <- res$Value
    if (is.null(items) && !is.null(res$table)) {
      # 舊格式相容
      tbl <- res$table
      if (!is.data.frame(tbl)) tbl <- tryCatch(reticulate::py_to_r(tbl), error = function(e) NULL)
      if (is.data.frame(tbl) && nrow(tbl) > 0) {
        items <- tbl$Item
        values <- tbl$Value
      }
    }

    items <- as.character(unlist(items, use.names = FALSE))
    values <- as.character(unlist(values, use.names = FALSE))
    if (length(items) == 0) stop("yfinance 回傳空的 summary 表")

    tbl <- data.frame(Item = items, Value = values, stringsAsFactors = FALSE)
    cname <- res$company_name
    if (is.null(cname) || length(cname) < 1 || is.na(cname) || !nzchar(as.character(cname))) {
      cname <- stock_code
    }
    attr(tbl, "company_name") <- as.character(cname)[1]
    message("✅ Summary OK rows=", nrow(tbl), " name=", attr(tbl, "company_name"))
    tbl
  }, error = function(e) {
    message("⚠️ Summary yfinance 失敗: ", e$message)
    .empty_summary(stock_code)
  })
}

# ==========================================
# 🇺🇸 4. 無風險利率 Rf（僅 yfinance）
# ==========================================
get_risk_free_rate <- function() {
  message("🔍 正在抓取美國 10 年期公債殖利率 (^TNX) via yfinance...")

  tryCatch({
    if (!exists("get_risk_free_rate_yf", mode = "function")) {
      stop("get_risk_free_rate_yf 未載入")
    }
    rf <- as.numeric(get_risk_free_rate_yf())
    if (is.na(rf) || rf <= 0) stop("invalid rf")
    message(paste("✅ yfinance Rf:", rf, "%"))
    rf
  }, error = function(e) {
    message("⚠️ Rf 抓取失敗，套用預設值 4.0%。原因: ", e$message)
    4.0
  })
}

cached_get_risk_free_rate <- memoise::memoise(get_risk_free_rate, cache = my_cache)

# ==========================================
# 🔎 5. Ticker 下拉預選／即時搜尋
# ==========================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

TICKER_PRESETS <- c(
  "AMZN — Amazon.com" = "AMZN",
  "AAPL — Apple" = "AAPL",
  "MSFT — Microsoft" = "MSFT",
  "GOOGL — Alphabet" = "GOOGL",
  "META — Meta Platforms" = "META",
  "NVDA — NVIDIA" = "NVDA",
  "TSLA — Tesla" = "TSLA",
  "BRK-B — Berkshire Hathaway" = "BRK-B",
  "JPM — JPMorgan Chase" = "JPM",
  "V — Visa" = "V",
  "JNJ — Johnson & Johnson" = "JNJ",
  "XOM — Exxon Mobil" = "XOM",
  "UNH — UnitedHealth" = "UNH",
  "LLY — Eli Lilly" = "LLY",
  "AVGO — Broadcom" = "AVGO",
  "TSM — Taiwan Semiconductor (ADR)" = "TSM",
  "2330.TW — 台積電" = "2330.TW",
  "2317.TW — 鴻海" = "2317.TW",
  "2454.TW — 聯發科" = "2454.TW",
  "2308.TW — 台達電" = "2308.TW",
  "2382.TW — 廣達" = "2382.TW",
  "2881.TW — 富邦金" = "2881.TW",
  "2882.TW — 國泰金" = "2882.TW",
  "SPY — S&P 500 ETF" = "SPY",
  "QQQ — Nasdaq 100 ETF" = "QQQ"
)

#' Yahoo／yfinance typeahead → named character vector (label = symbol)
search_ticker_choices <- function(query, max_results = 12L) {
  query <- trimws(as.character(query %||% ""))
  if (!nzchar(query)) return(TICKER_PRESETS)

  py_hits <- tryCatch({
    if (!exists("search_tickers", mode = "function")) {
      return(NULL)
    }
    search_tickers(query, as.integer(max_results))
  }, error = function(e) {
    message("⚠️ search_tickers: ", e$message)
    NULL
  })

  out <- character(0)
  if (!is.null(py_hits) && length(py_hits) > 0) {
    # reticulate may return list of named lists or data.frame-like
    if (is.data.frame(py_hits)) {
      syms <- as.character(py_hits$symbol)
      labs <- as.character(py_hits$label)
    } else {
      syms <- vapply(py_hits, function(x) {
        as.character(x[["symbol"]] %||% NA_character_)
      }, character(1))
      labs <- vapply(py_hits, function(x) {
        as.character(x[["label"]] %||% x[["symbol"]] %||% NA_character_)
      }, character(1))
    }
    ok <- !is.na(syms) & nzchar(syms)
    syms <- syms[ok]
    labs <- labs[ok]
    if (length(syms) > 0) {
      labs[!nzchar(labs)] <- syms[!nzchar(labs)]
      # named vector: names shown in dropdown, values are symbols
      out <- stats::setNames(syms, labs)
      out <- out[!duplicated(out)]
    }
  }

  # Local preset filter (ticker or company label) as fallback / supplement
  q_up <- toupper(query)
  preset_keep <- grepl(q_up, toupper(TICKER_PRESETS), fixed = TRUE) |
    grepl(q_up, toupper(names(TICKER_PRESETS)), fixed = TRUE)
  local_hits <- TICKER_PRESETS[preset_keep]

  if (length(out) == 0) {
    if (length(local_hits) > 0) return(local_hits)
    # Still allow exact typed symbol as a choice
    return(stats::setNames(toupper(query), toupper(query)))
  }

  # Merge: Yahoo first, then presets not already present
  extra <- local_hits[!(unname(local_hits) %in% unname(out))]
  c(out, extra)
}
