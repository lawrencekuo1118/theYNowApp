# ==========================================
# web_crawler.R - 資料抓取（app_11.0 雲端版：純 yfinance，不用 Chromote）
# ==========================================

library(rvest)
library(magrittr)
library(purrr)
library(reticulate)
library(memoise)
library(cachem)

if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <- function(...) invisible(NULL)
}

# ==========================================
# 🚀 1. 記憶體快取與 Python 爬蟲初始化
# ==========================================
my_cache <- cachem::cache_mem(max_size = 50 * 1024^2, max_age = 3600)

# 勿在 source 時呼叫 py_available(initialize=TRUE)：可能直接 abort worker → shinyapps 500。
.py_scraper_ready <- FALSE
.ensure_python_scraper <- function() {
  if (isTRUE(.py_scraper_ready) && exists("scrape_all_financials", mode = "function")) {
    return(TRUE)
  }
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) return(FALSE)
  ok <- FALSE
  tryCatch({
    ok <- isTRUE(reticulate::py_available(initialize = FALSE))
    if (!ok) {
      suppressMessages(reticulate::py_config())
      ok <- isTRUE(reticulate::py_available(initialize = FALSE))
    }
    if (ok) {
      reticulate::source_python("deep_scraper.py")
      .py_scraper_ready <<- TRUE
    }
  }, error = function(e) {
    .ynow_log("⚠️ Python 爬蟲延遲載入失敗: ", e$message)
    ok <<- FALSE
  })
  isTRUE(ok) && exists("scrape_all_financials", mode = "function")
}

tryCatch({
  if (!identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1") &&
      isTRUE(reticulate::py_available(initialize = FALSE))) {
    reticulate::source_python("deep_scraper.py")
    .py_scraper_ready <- TRUE
    .ynow_log("✅ Python 深度爬蟲腳本載入成功！")
  } else {
    .ynow_log("ℹ️ Python 爬蟲改為延遲載入（避免啟動期 initialize 導致 500）")
  }
}, error = function(e) {
  .ynow_log("⚠️ Python 腳本載入失敗: ", e$message)
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
  q_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else "USD"
  # Unknown reporting currency → NA (do not copy quote; ADR must not assume USD=USD).
  f_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else NA_character_
  attr(df, "currency") <- q_ccy
  attr(df, "financialCurrency") <- f_ccy
  df
}

cached_scrape_financials <- memoise::memoise(
  function(stock_code) {
    .ynow_log(paste("🚀 正在啟動 Python 財報抓取:", stock_code))
    if (!isTRUE(.ensure_python_scraper())) {
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
  .ynow_log(paste("🔍 正在透過 yfinance 抓取公司與產業資訊:", stock_code))

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
    .ynow_log("⚠️ 產業資訊抓取失敗: ", e$message)
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
  .ynow_log(paste("🌐 正在讀取 Summary (yfinance):", stock_code))

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
    q_ccy <- tryCatch(as.character(res$currency %||% "")[1], error = function(e) "")
    f_ccy <- tryCatch(as.character(res$financialCurrency %||% "")[1], error = function(e) "")
    if (!nzchar(q_ccy) || identical(q_ccy, "NA")) {
      q_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else "USD"
    }
    # Missing financialCurrency: keep NA for non-.TW (refuse silent quote copy).
    if (!nzchar(f_ccy) || identical(f_ccy, "NA")) {
      f_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else NA_character_
    }
    attr(tbl, "currency") <- toupper(q_ccy)
    attr(tbl, "financialCurrency") <- if (is.na(f_ccy) || !nzchar(f_ccy)) {
      NA_character_
    } else {
      toupper(f_ccy)
    }
    .ynow_log(
      "✅ Summary OK rows=", nrow(tbl),
      " name=", attr(tbl, "company_name"),
      " ccy=", attr(tbl, "currency"), "/", attr(tbl, "financialCurrency")
    )
    tbl
  }, error = function(e) {
    .ynow_log("⚠️ Summary yfinance 失敗: ", e$message)
    .empty_summary(stock_code)
  })
}

# ==========================================
# 💱 USD/TWD 即期匯率
# ==========================================
get_usd_twd_fx <- function() {
  tryCatch({
    if (!.ensure_python_scraper() || !exists("get_usd_twd_rate", mode = "function")) {
      return(NA_real_)
    }
    px <- suppressWarnings(as.numeric(get_usd_twd_rate())[1])
    if (is.finite(px) && px > 0) return(px)
    NA_real_
  }, error = function(e) {
    .ynow_log("⚠️ get_usd_twd_fx: ", e$message)
    NA_real_
  })
}

cached_get_usd_twd_fx <- memoise::memoise(get_usd_twd_fx, cache = my_cache)

# ==========================================
# 🇺🇸／🇹🇼 4. 無風險利率 Rf（僅 yfinance；依市場）
# ==========================================
get_risk_free_rate <- function(market = NULL) {
  mode <- if (!is.null(market)) {
    if (exists("normalize_market_mode", mode = "function")) normalize_market_mode(market) else "US"
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  prof <- if (exists("market_profile", mode = "function")) market_profile(mode) else list(rf_fallback = 4.0, rf_label_zh = "Rf")
  .ynow_log(paste0("🔍 正在抓取 Rf（", prof$rf_label_zh %||% mode, "）via yfinance..."))

  tryCatch({
    if (!exists("get_risk_free_rate_yf", mode = "function")) {
      stop("get_risk_free_rate_yf 未載入")
    }
    rf <- as.numeric(get_risk_free_rate_yf(mode))
    if (is.na(rf) || rf <= 0) stop("invalid rf")
    .ynow_log(paste("✅ yfinance Rf:", rf, "%"))
    rf
  }, error = function(e) {
    fb <- suppressWarnings(as.numeric(prof$rf_fallback %||% 4.0)[1])
    if (!is.finite(fb) || fb <= 0) fb <- if (identical(mode, "TW")) 1.8 else 4.0
    .ynow_log("⚠️ Rf 抓取失敗，套用預設值 ", fb, "%。原因: ", e$message)
    fb
  })
}

# memoise by market：以 wrapper 包一層避免跨市場互相污染
.cached_get_risk_free_rate_us <- memoise::memoise(function() get_risk_free_rate("US"), cache = my_cache)
.cached_get_risk_free_rate_tw <- memoise::memoise(function() get_risk_free_rate("TW"), cache = my_cache)

cached_get_risk_free_rate <- function(market = NULL) {
  mode <- if (!is.null(market)) {
    if (exists("normalize_market_mode", mode = "function")) normalize_market_mode(market) else "US"
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  if (identical(mode, "TW")) .cached_get_risk_free_rate_tw() else .cached_get_risk_free_rate_us()
}

# ==========================================
# 🔎 5. Ticker 下拉預選／即時搜尋
# ==========================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

TICKER_PRESETS_US <- c(
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
  "SPY — S&P 500 ETF" = "SPY",
  "QQQ — Nasdaq 100 ETF" = "QQQ"
)

TICKER_PRESETS_TW <- c(
  "2330.TW — 台積電" = "2330.TW",
  "2317.TW — 鴻海" = "2317.TW",
  "2454.TW — 聯發科" = "2454.TW",
  "2308.TW — 台達電" = "2308.TW",
  "2382.TW — 廣達" = "2382.TW",
  "2303.TW — 聯電" = "2303.TW",
  "2881.TW — 富邦金" = "2881.TW",
  "2882.TW — 國泰金" = "2882.TW",
  "2891.TW — 中信金" = "2891.TW",
  "2412.TW — 中華電" = "2412.TW",
  "1301.TW — 台塑" = "1301.TW",
  "2002.TW — 中鋼" = "2002.TW",
  "0050.TW — 元大台灣50" = "0050.TW"
)

# 向後相容：合併預設（搜尋仍可用）
TICKER_PRESETS <- c(TICKER_PRESETS_US, TICKER_PRESETS_TW)

ticker_presets_for_market <- function(mode = NULL) {
  mode <- if (!is.null(mode)) {
    if (exists("normalize_market_mode", mode = "function")) normalize_market_mode(mode) else "US"
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  if (identical(mode, "TW")) TICKER_PRESETS_TW else TICKER_PRESETS_US
}

#' Yahoo／yfinance typeahead → named character vector (label = symbol)
search_ticker_choices <- function(query, max_results = 12L, market = NULL) {
  mode <- if (!is.null(market)) {
    if (exists("normalize_market_mode", mode = "function")) normalize_market_mode(market) else "US"
  } else if (exists("get_market_mode", mode = "function")) {
    get_market_mode()
  } else {
    "US"
  }
  presets <- ticker_presets_for_market(mode)
  query <- trimws(as.character(query %||% ""))
  if (!nzchar(query)) return(presets)

  py_hits <- tryCatch({
    if (!exists("search_tickers", mode = "function")) {
      return(NULL)
    }
    search_tickers(query, as.integer(max_results))
  }, error = function(e) {
    .ynow_log("⚠️ search_tickers: ", e$message)
    NULL
  })

  out <- character(0)
  if (!is.null(py_hits) && length(py_hits) > 0) {
    # reticulate may return list of named lists or data.frame-like
    if (is.data.frame(py_hits)) {
      syms <- as.character(py_hits$symbol)
      labs <- as.character(py_hits$label)
      exch <- if ("exchDisp" %in% names(py_hits)) as.character(py_hits$exchDisp) else
        if ("exchange" %in% names(py_hits)) as.character(py_hits$exchange) else
          rep("", length(syms))
    } else {
      syms <- vapply(py_hits, function(x) {
        as.character(x[["symbol"]] %||% NA_character_)
      }, character(1))
      labs <- vapply(py_hits, function(x) {
        as.character(x[["label"]] %||% x[["symbol"]] %||% NA_character_)
      }, character(1))
      exch <- vapply(py_hits, function(x) {
        as.character(x[["exchDisp"]] %||% x[["exchange"]] %||% "")
      }, character(1))
    }
    ok <- !is.na(syms) & nzchar(syms)
    syms <- syms[ok]; labs <- labs[ok]; exch <- exch[ok]
    if (identical(mode, "TW")) {
      keep <- grepl("\\.(TW|TWO)$", syms, ignore.case = TRUE) |
        grepl("TAI|TWO|Taiwan|TWSE|TPEx|OTC", exch, ignore.case = TRUE)
      # bare 4-digit codes from search often need .TW
      bare <- grepl("^[0-9]{4}$", syms) & !keep
      if (any(bare)) {
        syms[bare] <- paste0(syms[bare], ".TW")
        keep[bare] <- TRUE
      }
      syms <- syms[keep]; labs <- labs[keep]
    } else {
      drop <- grepl("\\.(TW|TWO)$", syms, ignore.case = TRUE)
      syms <- syms[!drop]; labs <- labs[!drop]
    }
    if (length(syms)) {
      labs[!nzchar(labs)] <- syms[!nzchar(labs)]
      out <- stats::setNames(syms, labs)
      out <- out[!duplicated(out)]
    }
  }

  # Local preset filter (ticker or company label) as fallback / supplement
  q_up <- toupper(query)
  preset_keep <- grepl(q_up, toupper(presets), fixed = TRUE) |
    grepl(q_up, toupper(names(presets)), fixed = TRUE)
  local_hits <- presets[preset_keep]

  if (length(out) == 0) {
    if (length(local_hits) > 0) return(local_hits)
    # Still allow exact typed symbol as a choice（依市場正規化）
    q_norm <- if (exists("normalize_ticker_for_market", mode = "function")) {
      normalize_ticker_for_market(query, mode)
    } else {
      toupper(query)
    }
    return(stats::setNames(q_norm, q_norm))
  }

  # Merge: Yahoo first, then presets not already present
  extra <- local_hits[!(unname(local_hits) %in% unname(out))]
  c(out, extra)
}

#' Yahoo β / D/E inputs for unlevered & bottom-up industry beta
fetch_beta_unlever_inputs <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk)) return(NULL)
  if (!exists("get_beta_unlever_inputs", mode = "function")) {
    .ynow_log("⚠️ get_beta_unlever_inputs 未載入")
    return(NULL)
  }
  tryCatch(get_beta_unlever_inputs(tk), error = function(e) {
    .ynow_log("⚠️ fetch_beta_unlever_inputs(", tk, "): ", e$message)
    NULL
  })
}

#' Batch peer fetch (list of dicts from Python)
fetch_beta_unlever_inputs_batch <- function(tickers) {
  tks <- unique(toupper(trimws(as.character(tickers %||% character(0)))))
  tks <- tks[nzchar(tks)]
  if (length(tks) == 0) return(list())
  if (!exists("get_beta_unlever_inputs_batch", mode = "function")) {
    return(lapply(tks, fetch_beta_unlever_inputs))
  }
  tryCatch(get_beta_unlever_inputs_batch(tks), error = function(e) {
    .ynow_log("⚠️ fetch_beta_unlever_inputs_batch: ", e$message)
    lapply(tks, fetch_beta_unlever_inputs)
  })
}

# ==========================================
# 財報附註 (SEC EDGAR)：年報／季報附註與重大訊息（僅美股）
# ==========================================
#' 抓取美股最新年報（10-K／20-F／40-F）、季報（10-Q）或重大訊息（8-K／6-K）。
#' 年報請求會自動 fallback 到外國發行人的 20-F／40-F；
#' 重大訊息請求（form=8-K）會自動 fallback 到 6-K。
#' 回傳 list：ok/error/company/form/filing_date/report_date/accession/
#' primary_doc_url 與平行向量 short_names/urls/important/char_counts/
#' excerpts/full_texts。
fetch_sec_report_notes <- function(ticker, form = "10-K", max_chars = 1500L) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  form <- toupper(trimws(as.character(form %||% "10-K")[1]))
  if (!form %in% c("10-K", "10-Q", "20-F", "40-F", "8-K", "6-K")) form <- "10-K"
  empty <- list(
    ok = FALSE, error = "", company = tk, form = form,
    filing_date = "", report_date = "", accession = "", primary_doc_url = "",
    short_names = character(0), urls = character(0), important = logical(0),
    char_counts = integer(0), excerpts = character(0), full_texts = character(0),
    summaries = list()
  )
  if (!nzchar(tk)) {
    empty$error <- "請輸入美股代號"
    return(empty)
  }
  if (!isTRUE(.ensure_python_scraper()) || !exists("sec_report_notes", mode = "function")) {
    empty$error <- "sec_report_notes 未載入（Python / reticulate 失敗）"
    return(empty)
  }
  tryCatch({
    res <- sec_report_notes(tk, form, as.integer(max_chars))
    if (is.null(res)) return(empty)
    res
  }, error = function(e) {
    .ynow_log("⚠️ fetch_sec_report_notes(", tk, " ", form, "): ", e$message)
    empty$error <- e$message
    empty
  })
}

cached_fetch_sec_report_notes <- memoise::memoise(
  fetch_sec_report_notes, cache = my_cache
)
