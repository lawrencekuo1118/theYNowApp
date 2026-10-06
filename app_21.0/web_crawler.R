# ==========================================
# web_crawler.R - 資料抓取（純 yfinance；套件由 global.R 載入）
# ==========================================
#
# Cache architecture (strict fast vs slow — do not mix TTLs):
#
#   SLOW (fundamentals): income / balance / cash flow, SEC notes, industry labels.
#     Change cadence = quarter / year. Use cache_disk (fallback: cache_mem) with
#     ≥24h TTL so statement pulls are stable and do not thrash Yahoo.
#
#   FAST (market / macro): live quotes, Rf (10Y), USD/TWD FX.
#     High-frequency; drives CAPM Ke, MOS vs market, and sensitivity baselines.
#     Use cache_mem with ≤15m TTL (or call uncached) so DCF is not priced off stale Rf.
#
# ==========================================

if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <- function(...) invisible(NULL)
}

# ==========================================
# 🚀 1. Fast vs slow caches + Python scraper init
# ==========================================
.YNOW_CACHE_FAST_TTL_SEC <- 15L * 60L      # 15 minutes — prices / Rf / FX
.YNOW_CACHE_SLOW_TTL_SEC <- 24L * 3600L    # 24 hours — statements / filings

.ynow_slow_cache_dir <- function() {
  # Optional sticky dir (YNOW_SLOW_CACHE_DIR) survives short worker restarts better than
  # a pure in-memory miss storm; default stays under tempdir for shinyapps safety.
  override <- Sys.getenv("YNOW_SLOW_CACHE_DIR", unset = "")
  d <- if (nzchar(override)) {
    override
  } else {
    file.path(tempdir(), "ynow_cache_slow")
  }
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.ynow_fast_cache <- cachem::cache_mem(
  max_size = 48 * 1024^2,
  max_age = .YNOW_CACHE_FAST_TTL_SEC
)

.ynow_slow_cache <- tryCatch({
  cachem::cache_disk(
    dir = .ynow_slow_cache_dir(),
    max_size = 512 * 1024^2,
    max_age = .YNOW_CACHE_SLOW_TTL_SEC,
    destroy_on_finalize = FALSE
  )
}, error = function(e) {
  .ynow_log("⚠️ slow cache_disk unavailable; falling back to cache_mem @ 24h: ", e$message)
  cachem::cache_mem(
    max_size = 128 * 1024^2,
    max_age = .YNOW_CACHE_SLOW_TTL_SEC
  )
})

# Historical name `my_cache` = FAST tier only (Rf / FX / live quotes).
# Fundamentals must use `.ynow_slow_cache` — never park statements here.
my_cache <- .ynow_fast_cache

#' Inspect cache tier TTLs (for tests / diagnostics).
ynow_cache_tier_info <- function() {
  list(
    fast_ttl_sec = as.integer(.YNOW_CACHE_FAST_TTL_SEC),
    slow_ttl_sec = as.integer(.YNOW_CACHE_SLOW_TTL_SEC),
    fast_class = paste(class(.ynow_fast_cache), collapse = "/"),
    slow_class = paste(class(.ynow_slow_cache), collapse = "/"),
    slow_dir = tryCatch(.ynow_slow_cache_dir(), error = function(e) NA_character_)
  )
}

# 勿在 source 時呼叫 py_available(initialize=TRUE)：可能直接 abort worker → shinyapps 500。
# Persist reticulate exports here: source_python() defaults to envir=parent.frame();
# delayed load from inside .ensure_* must bind into this module env, not a temp frame.
.py_scraper_env <- environment()
.py_scraper_ready <- FALSE
.py_scraper_fn_ready <- function() {
  exists(
    "scrape_all_financials",
    envir = .py_scraper_env,
    inherits = FALSE,
    mode = "function"
  )
}
.ensure_python_scraper <- function() {
  if (isTRUE(.py_scraper_ready) && isTRUE(.py_scraper_fn_ready())) {
    return(TRUE)
  }
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) return(FALSE)
  ok <- FALSE
  tryCatch({
    if (exists(".ynow_ensure_python", mode = "function")) {
      .ynow_ensure_python()
    }
    ok <- isTRUE(reticulate::py_available(initialize = FALSE))
    if (!ok) {
      suppressMessages(reticulate::py_config())
      ok <- isTRUE(reticulate::py_available(initialize = FALSE))
    }
    if (ok) {
      reticulate::source_python("deep_scraper.py", envir = .py_scraper_env)
      .py_scraper_ready <<- TRUE
    }
  }, error = function(e) {
    .ynow_log("⚠️ Python 爬蟲延遲載入失敗: ", e$message)
    ok <<- FALSE
  })
  isTRUE(ok) && isTRUE(.py_scraper_fn_ready())
}

# Source-time reticulate probes removed: shinyapps must Listen before Python.

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
  # Unknown reporting currency → NA (do not copy quote — ADR must not assume USD=USD).
  f_ccy <- if (grepl("\\.(TW|TWO)$", stock_code, ignore.case = TRUE)) "TWD" else NA_character_
  attr(df, "currency") <- q_ccy
  attr(df, "financialCurrency") <- f_ccy
  df
}

# SLOW: quarterly/annual statements (24h disk).
# Important: never leave EMPTY frames in the slow cache — Yahoo rate-limits and
# transient misses would otherwise poison Multiples / DCF for 24h.
.scrape_financials_uncached <- function(stock_code) {
  .ynow_log(paste("🚀 正在啟動 Python 財報抓取:", stock_code))
  if (!isTRUE(.ensure_python_scraper())) {
    stop("scrape_all_financials 未載入（Python / reticulate 失敗）")
  }
  normalize_all_financials(scrape_all_financials(stock_code))
}

.cached_scrape_financials_memo <- memoise::memoise(
  .scrape_financials_uncached,
  cache = .ynow_slow_cache
)

tryCatch(memoise::forget(.cached_scrape_financials_memo), error = function(e) NULL)

#' Config-driven local ordinary share ticker for ADRs (HTCDI issuers).
#' Not a valuation formula branch — only used when Yahoo returns empty statements
#' for the quoted ticker and a local ordinary listing is known.
lookup_statement_fallback_ticker <- function(stock_code) {
  tk <- toupper(trimws(as.character(stock_code %||% "")[1]))
  if (!nzchar(tk)) return("")
  if (!exists("htcdi_find_issuer", mode = "function")) return("")
  iss <- tryCatch(htcdi_find_issuer(tk), error = function(e) NULL)
  if (is.null(iss)) return("")
  itype <- toupper(as.character(iss$instrument_type %||% "")[1])
  if (!identical(itype, "ADR")) return("")
  loc <- as.character(iss$local_ordinary_ticker %||% "")[1]
  loc <- toupper(trimws(loc))
  if (!nzchar(loc) || identical(loc, tk)) return("")
  loc
}

.drop_financials_cache_key <- function(stock_code) {
  tryCatch(
    memoise::drop_cache(.cached_scrape_financials_memo)(stock_code),
    error = function(e) NULL
  )
  invisible(NULL)
}

.stamp_financials_fallback <- function(res, source_ticker, reason = "local_ordinary") {
  if (is.null(res)) return(res)
  attr(res, "statement_source_ticker") <- as.character(source_ticker)[1]
  attr(res, "statement_fallback") <- as.character(reason)[1]
  res
}

#' Public financials fetch: memoised when non-empty; empty results are not kept.
#' When the quoted ticker yields empty IS/BS/CF, retry via ADR local ordinary
#' ticker from HTCDI config (e.g. TSM → 2330.TW) — generic, not ticker-hardcoded.
cached_scrape_financials <- function(stock_code) {
  stock_code <- as.character(stock_code %||% "")[1]
  if (!nzchar(stock_code)) stop("stock_code is empty")

  res <- tryCatch(
    .cached_scrape_financials_memo(stock_code),
    error = function(e) e
  )
  if (inherits(res, "error")) stop(res)

  empty_fn <- if (exists("financials_is_bs_cf_all_empty", mode = "function")) {
    financials_is_bs_cf_all_empty
  } else {
    function(x) TRUE
  }

  if (!isTRUE(empty_fn(res))) {
    return(res)
  }

  # Drop poisoned empty entry (rate-limit / transient miss).
  .drop_financials_cache_key(stock_code)

  alt <- lookup_statement_fallback_ticker(stock_code)
  if (nzchar(alt)) {
    .ynow_log(paste("↩️  quoted ticker statements empty; trying local ordinary:", alt))
    res_alt <- tryCatch(
      .cached_scrape_financials_memo(alt),
      error = function(e) NULL
    )
    if (!is.null(res_alt) && !isTRUE(empty_fn(res_alt))) {
      return(.stamp_financials_fallback(res_alt, alt, "local_ordinary"))
    }
    if (!is.null(res_alt) && isTRUE(empty_fn(res_alt))) {
      .drop_financials_cache_key(alt)
    }
    # One uncached retry on ordinary (avoids serving a just-dropped empty memo).
    res_alt2 <- tryCatch(.scrape_financials_uncached(alt), error = function(e) NULL)
    if (!is.null(res_alt2) && !isTRUE(empty_fn(res_alt2))) {
      return(.stamp_financials_fallback(res_alt2, alt, "local_ordinary_uncached"))
    }
  }

  # Uncached retry on the quoted ticker itself (after dropping empty memo).
  res2 <- tryCatch(.scrape_financials_uncached(stock_code), error = function(e) res)
  if (!isTRUE(empty_fn(res2))) {
    return(res2)
  }
  res2
}

# ==========================================
# 🏭 2. 公司／產業資訊
# ==========================================
get_yahoo_industry <- function(stock_code) {
  .ynow_log(paste("🔍 正在透過 yfinance 抓取公司與產業資訊:", stock_code))

  result <- tryCatch({
    # 啟動期可能延遲載入 deep_scraper；與財報路徑一樣先 ensure，否則 Sector/Industry 會一直 N/A
    if (!isTRUE(.ensure_python_scraper()) ||
        !exists("fast_get_company_info", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
      stop("fast_get_company_info 未載入（Python / reticulate 失敗）")
    }
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

# SLOW: sector / industry labels (rarely change within a day)
cached_get_yahoo_industry <- memoise::memoise(get_yahoo_industry, cache = .ynow_slow_cache)
tryCatch(memoise::forget(cached_get_yahoo_industry), error = function(e) NULL)

# ==========================================
# 🌐 3. Summary（僅 yfinance，shinyapps 無 Chrome）
# ==========================================
get_summary_data <- function(stock_code) {
  .ynow_log(paste("🌐 正在讀取 Summary (yfinance):", stock_code))

  tryCatch({
    if (!isTRUE(.ensure_python_scraper()) ||
        !exists("get_summary_quote", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
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

# FAST: live quote / market cap / last price (≤15m) — never park on slow statement cache
cached_get_summary_data <- memoise::memoise(get_summary_data, cache = .ynow_fast_cache)
tryCatch(memoise::forget(cached_get_summary_data), error = function(e) NULL)

# ==========================================
# 💱 USD/TWD 即期匯率
# ==========================================
get_usd_twd_fx <- function() {
  tryCatch({
    if (!isTRUE(.ensure_python_scraper()) ||
        !exists("get_usd_twd_rate", envir = .py_scraper_env, inherits = FALSE, mode = "function")) {
      return(NA_real_)
    }
    px <- suppressWarnings(as.numeric(.py_scraper_env$get_usd_twd_rate())[1])
    if (is.finite(px) && px > 0) return(px)
    NA_real_
  }, error = function(e) {
    .ynow_log("⚠️ get_usd_twd_fx: ", e$message)
    NA_real_
  })
}

# FAST: FX
cached_get_usd_twd_fx <- memoise::memoise(get_usd_twd_fx, cache = .ynow_fast_cache)
tryCatch(memoise::forget(cached_get_usd_twd_fx), error = function(e) NULL)

# ==========================================
# 🇺🇸／🇹🇼 4. 無風險利率 Rf（US: Yahoo ^TNX；TW: TPEx Curve 10Y）
# ==========================================
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
# Last successful *live* Rf by market (not the documented fixed fallback).
.rf_live_last <- new.env(parent = emptyenv())

.rf_market_mode <- function(market = NULL) {
  if (!is.null(market)) {
    if (exists("normalize_market_mode", mode = "function")) {
      return(normalize_market_mode(market))
    }
    return("US")
  }
  if (exists("get_market_mode", mode = "function")) get_market_mode() else "US"
}

.rf_profile_bits <- function(mode) {
  if (exists("market_profile", mode = "function")) {
    market_profile(mode)
  } else {
    list(
      rf_fallback = if (identical(mode, "TW")) 1.8 else 5.0,
      rf_label_zh = if (identical(mode, "TW")) "台灣 10 年期公債（TPEx）" else "美國 10 年期公債（^TNX）",
      rf_symbol = if (identical(mode, "TW")) "TPEX_CURVE_10Y" else "^TNX"
    )
  }
}

.rf_documented_fallback <- function(mode, prof = NULL) {
  prof <- prof %||% .rf_profile_bits(mode)
  fb <- suppressWarnings(as.numeric(prof$rf_fallback %||% NA_real_)[1])
  if (!is.finite(fb) || fb <= 0) fb <- if (identical(mode, "TW")) 1.8 else 5.0
  fb
}

#' Live Rf（失敗則 throw；不回傳固定 fallback）。
#' US: Yahoo ^TNX；TW: 櫃買 TPEx 公債殖利率曲線 10 年期。
.fetch_risk_free_rate_live <- function(mode = "US") {
  mode <- .rf_market_mode(mode)
  if (!isTRUE(.ensure_python_scraper())) {
    stop("Python scraper unavailable for Rf")
  }
  if (!exists("get_risk_free_rate_yf", envir = .py_scraper_env, inherits = FALSE, mode = "function") &&
      !exists("get_risk_free_rate_yf", mode = "function")) {
    stop("get_risk_free_rate_yf 未載入")
  }
  rf <- as.numeric(get_risk_free_rate_yf(mode))
  if (is.na(rf) || !is.finite(rf) || rf <= 0) stop("invalid rf")
  rf
}

# FAST: live Rf — short TTL so CAPM Ke / terminal g track market (not stale hour-old yields)
.cached_fetch_rf_live_us <- memoise::memoise(
  function() .fetch_risk_free_rate_live("US"),
  cache = .ynow_fast_cache
)
.cached_fetch_rf_live_tw <- memoise::memoise(
  function() .fetch_risk_free_rate_live("TW"),
  cache = .ynow_fast_cache
)
tryCatch(memoise::forget(.cached_fetch_rf_live_us), error = function(e) NULL)
tryCatch(memoise::forget(.cached_fetch_rf_live_tw), error = function(e) NULL)

#' Resolve Rf with explicit source for Macro／CAPM UI.
#' @return list(rf_pct, source, label, symbol, is_fallback)
#'   source: "live" | "last_known" | "fallback"
get_risk_free_rate_detail <- function(market = NULL) {
  mode <- .rf_market_mode(market)
  prof <- .rf_profile_bits(mode)
  label <- as.character(prof$rf_label_zh %||% "Rf")[1]
  symbol <- as.character(prof$rf_symbol %||% "")[1]
  fb <- .rf_documented_fallback(mode, prof)

  .ynow_log(paste0("🔍 正在抓取 Rf（", label, "）via ", symbol, "..."))

  live <- tryCatch({
    r <- if (identical(mode, "TW")) {
      .cached_fetch_rf_live_tw()
    } else {
      .cached_fetch_rf_live_us()
    }
    suppressWarnings(as.numeric(r)[1])
  }, error = function(e) {
    .ynow_log("⚠️ Rf 即時抓取失敗: ", e$message)
    NA_real_
  })

  if (is.finite(live) && live > 0) {
    .rf_live_last[[mode]] <- list(rf = live, at = Sys.time())
    rf_rounded <- round(live, 2)
    .ynow_log(paste("✅ Rf (live):", live, "% →", sprintf("%.2f", rf_rounded), "%"))
    return(list(
      rf_pct = rf_rounded,
      source = "live",
      label = label,
      symbol = symbol,
      is_fallback = FALSE
    ))
  }

  prev <- .rf_live_last[[mode]]
  if (is.list(prev)) {
    prev_rf <- suppressWarnings(as.numeric(prev$rf)[1])
    if (is.finite(prev_rf) && prev_rf > 0) {
      .ynow_log(paste("ℹ️ Rf 使用最近一次成功抓取值:", prev_rf, "%"))
      return(list(
        rf_pct = round(prev_rf, 2),
        source = "last_known",
        label = label,
        symbol = symbol,
        is_fallback = FALSE
      ))
    }
  }

  .ynow_log("⚠️ Rf 無即時／快取，套用工程 fallback ", fb, "%（非即時公債殖利率）")
  list(
    rf_pct = round(fb, 2),
    source = "fallback",
    label = label,
    symbol = symbol,
    is_fallback = TRUE
  )
}

get_risk_free_rate <- function(market = NULL) {
  get_risk_free_rate_detail(market)$rf_pct
}

cached_get_risk_free_rate <- function(market = NULL) {
  get_risk_free_rate_detail(market)$rf_pct
}

cached_get_risk_free_rate_detail <- function(market = NULL) {
  get_risk_free_rate_detail(market)
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
  "KO — Coca-Cola" = "KO",
  "AVGO — Broadcom" = "AVGO",
  "TSM — Taiwan Semiconductor (ADR)" = "TSM",
  "SPY — S&P 500 ETF" = "SPY",
  "QQQ — Nasdaq 100 ETF" = "QQQ"
)

TICKER_PRESETS_TW <- c(
  "2330 — 台積電" = "2330.TW",
  "2317 — 鴻海" = "2317.TW",
  "2454 — 聯發科" = "2454.TW",
  "2308 — 台達電" = "2308.TW",
  "2382 — 廣達" = "2382.TW",
  "2303 — 聯電" = "2303.TW",
  "2881 — 富邦金" = "2881.TW",
  "2882 — 國泰金" = "2882.TW",
  "2891 — 中信金" = "2891.TW",
  "2412 — 中華電" = "2412.TW",
  "1301 — 台塑" = "1301.TW",
  "2002 — 中鋼" = "2002.TW",
  "0050 — 元大台灣50" = "0050.TW"
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

#' 建議列標籤：TW 顯示乾淨代號；US 維持原 label
.format_suggest_label <- function(sym, lab, mode) {
  sym <- as.character(sym %||% "")[1]
  lab <- trimws(as.character(lab %||% "")[1])
  if (identical(mode, "TW") && exists("display_ticker_for_market", mode = "function")) {
    disp <- display_ticker_for_market(sym, "TW")
    # 剝除 label 開頭的 Yahoo 後綴代號
    extra <- lab
    esc_sym <- gsub("([.\\^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", sym, perl = TRUE)
    esc_disp <- gsub("([.\\^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", disp, perl = TRUE)
    extra <- sub(paste0("^", esc_sym, "(\\s|[—\\-–])+"), "", extra, perl = TRUE)
    extra <- sub(paste0("^", esc_disp, "(\\s|[—\\-–])+"), "", extra, perl = TRUE)
    extra <- sub("\\.(TW|TWO)\\s*[—\\-–]\\s*", "", extra, ignore.case = TRUE, perl = TRUE)
    extra <- trimws(extra)
    if (nzchar(extra) && !identical(toupper(extra), toupper(sym)) &&
        !identical(toupper(extra), toupper(disp))) {
      return(paste0(disp, " — ", extra))
    }
    return(disp)
  }
  if (nzchar(lab)) lab else sym
}

#' Yahoo／yfinance typeahead → named character vector (label = display, value = fetch symbol)
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

  max_results <- max(1L, as.integer(max_results)[1])
  py_hits <- tryCatch({
    if (!exists("search_tickers", mode = "function")) {
      NULL
    } else {
      search_tickers(query, as.integer(max_results))
    }
  }, error = function(e) {
    .ynow_log("⚠️ search_tickers: ", e$message)
    NULL
  })

  out <- character(0)
  if (!is.null(py_hits) && length(py_hits) > 0) {
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
      bare <- grepl("^[0-9]{4,6}[A-Z]?$", syms) & !keep
      if (any(bare)) {
        syms[bare] <- vapply(syms[bare], function(s) {
          if (exists("normalize_ticker_for_market", mode = "function")) {
            normalize_ticker_for_market(s, "TW")
          } else {
            paste0(s, ".TW")
          }
        }, character(1))
        keep[bare] <- TRUE
      }
      if (exists("normalize_ticker_for_market", mode = "function")) {
        tw_idx <- grepl("\\.(TW|TWO)$", syms, ignore.case = TRUE)
        if (any(tw_idx)) {
          syms[tw_idx] <- vapply(syms[tw_idx], function(s) {
            normalize_ticker_for_market(s, "TW")
          }, character(1))
        }
      }
      syms <- syms[keep]; labs <- labs[keep]
    } else {
      # 美股：納入 Nasdaq + NYSE 主要上市；排除台股後綴、海外交易所後綴、期貨／外匯
      drop <- grepl("\\.(TW|TWO)$", syms, ignore.case = TRUE) |
        grepl(
          "\\.(TO|V|L|DE|PA|HK|T|AX|SS|SZ|KS|KQ|BO|NS|SA|MX|NE|F|SW|MI|AS|BR|HE|ST|CO|OL|IC|LS|AT)$",
          syms,
          ignore.case = TRUE
        ) |
        grepl("=F$|=X$", syms) |
        grepl("^\\^", syms)
      keep <- !drop
      if (length(exch) == length(syms) && exists("is_us_primary_listing_exchange", mode = "function")) {
        has_ex <- nzchar(trimws(as.character(exch)))
        us_ex <- vapply(
          exch,
          function(e) isTRUE(is_us_primary_listing_exchange(e)),
          logical(1)
        )
        # 有交易所碼時必須是 Nasdaq／NYSE；無碼時僅靠符號過濾（保留 AAPL、BRK-B）
        keep <- keep & (!has_ex | us_ex)
      }
      syms <- syms[keep]
      labs <- labs[keep]
      if (length(exch) == length(keep)) exch <- exch[keep]
    }
    if (length(syms)) {
      labs <- vapply(seq_along(syms), function(i) {
        .format_suggest_label(syms[[i]], labs[[i]], mode)
      }, character(1))
      out <- stats::setNames(syms, labs)
      out <- out[!duplicated(out)]
    }
  }

  # Local preset filter (ticker or company label)
  # 注意：不可用 && 接 vector grepl（R ≥ 4.3 會直接錯誤，建議列整段失敗）
  q_up <- toupper(query)
  preset_keep <- grepl(q_up, toupper(presets), fixed = TRUE) |
    grepl(q_up, toupper(names(presets)), fixed = TRUE)
  if (identical(mode, "TW")) {
    preset_keep <- preset_keep | grepl(query, names(presets), fixed = TRUE)
  }
  local_hits <- presets[preset_keep]

  # TW：CJK／公司名／純數字 → 上市／上櫃／興櫃宇宙（Yahoo typeahead 常對中文弱）
  # US：公司名／代號 → 美股主要上市宇宙（離線命中 Nasdaq／NYSE）
  cjk_hits <- character(0)
  if (identical(mode, "TW") && exists("search_tw_universe_by_name", mode = "function")) {
    cjk_hits <- tryCatch(
      search_tw_universe_by_name(query, max_results = max_results),
      error = function(e) character(0)
    )
  }
  us_uni_hits <- character(0)
  if (identical(mode, "US") && exists("search_us_universe_by_name", mode = "function")) {
    us_uni_hits <- tryCatch(
      search_us_universe_by_name(query, max_results = max_results),
      error = function(e) character(0)
    )
  }

  merge_named <- function(...) {
    parts <- list(...)
    acc <- character(0)
    for (p in parts) {
      if (is.null(p) || !length(p)) next
      extra <- p[!(unname(p) %in% unname(acc))]
      acc <- c(acc, extra)
    }
    if (length(acc) > max_results) acc <- acc[seq_len(max_results)]
    acc
  }

  # Prefer: 宇宙（中文或純數字代號）→ Yahoo → presets；其餘 Yahoo 優先
  tw_prefer_universe <- identical(mode, "TW") && (
    (exists("query_has_cjk", mode = "function") && isTRUE(query_has_cjk(query))) ||
      grepl("^[0-9]{4,6}[A-Za-z]?$", gsub("\\s+", "", query))
  )
  if (isTRUE(tw_prefer_universe)) {
    merged <- merge_named(cjk_hits, out, local_hits)
  } else if (identical(mode, "US")) {
    merged <- merge_named(out, us_uni_hits, local_hits)
  } else {
    merged <- merge_named(out, cjk_hits, local_hits)
  }

  # Exact ticker match first (AAPL before AAPU/AAPD leveraged ETFs; 2330 before peers).
  if (length(merged)) {
    q_sym <- if (exists("normalize_ticker_for_market", mode = "function")) {
      tryCatch(normalize_ticker_for_market(query, mode), error = function(e) toupper(query))
    } else {
      toupper(gsub("\\s+", "", query))
    }
    q_sym <- toupper(trimws(as.character(q_sym %||% "")[1]))
    q_bare <- sub("\\.(TW|TWO)$", "", q_sym, ignore.case = TRUE)
    vals <- toupper(unname(merged))
    bare_vals <- sub("\\.(TW|TWO)$", "", vals, ignore.case = TRUE)
    exact_idx <- which(vals == q_sym | bare_vals == q_bare)
    if (length(exact_idx)) {
      merged <- c(merged[exact_idx], merged[-exact_idx])
      merged <- merged[!duplicated(unname(merged))]
    }
  }

  if (!length(merged)) {
    q_norm <- if (exists("normalize_ticker_for_market", mode = "function")) {
      normalize_ticker_for_market(query, mode)
    } else {
      toupper(query)
    }
    if (is.na(q_norm) || !nzchar(q_norm)) return(character(0))
    lab <- if (exists("display_ticker_for_market", mode = "function")) {
      display_ticker_for_market(q_norm, mode)
    } else {
      q_norm
    }
    return(stats::setNames(q_norm, lab))
  }

  # 再整理 TW 標籤為乾淨代號
  if (identical(mode, "TW")) {
    labs2 <- vapply(seq_along(merged), function(i) {
      .format_suggest_label(unname(merged)[[i]], names(merged)[[i]], mode)
    }, character(1))
    merged <- stats::setNames(unname(merged), labs2)
  }
  merged
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
    summaries = list(), segment_tables_json = "[]"
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
  fetch_sec_report_notes, cache = .ynow_slow_cache
)

# Lab Search: segment / revenue-disaggregation notes only, wall-clock budget.
# Full 10-K/20-F note download blocks the Shiny loop and forces iOS "Reload".
fetch_sec_segment_notes <- function(ticker, form = "10-K", max_chars = 1500L,
                                    max_seconds = 18) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  form <- toupper(trimws(as.character(form %||% "10-K")[1]))
  if (!form %in% c("10-K", "10-Q", "20-F", "40-F", "8-K", "6-K")) form <- "10-K"
  empty <- list(
    ok = FALSE, error = "", company = tk, form = form,
    filing_date = "", report_date = "", accession = "", primary_doc_url = "",
    short_names = character(0), urls = character(0), important = logical(0),
    char_counts = integer(0), excerpts = character(0), full_texts = character(0),
    summaries = list(), segment_tables_json = "[]"
  )
  if (!nzchar(tk)) {
    empty$error <- "請輸入美股代號"
    return(empty)
  }
  if (!isTRUE(.ensure_python_scraper()) || !exists("sec_report_notes", mode = "function")) {
    empty$error <- "sec_report_notes 未載入（Python / reticulate 失敗）"
    return(empty)
  }
  budget <- suppressWarnings(as.numeric(max_seconds)[1])
  if (!is.finite(budget) || budget <= 0) budget <- 18
  tryCatch({
    res <- sec_report_notes(
      tk, form, as.integer(max_chars),
      note_filter = "segment",
      max_seconds = budget
    )
    if (is.null(res)) return(empty)
    res
  }, error = function(e) {
    .ynow_log("⚠️ fetch_sec_segment_notes(", tk, " ", form, "): ", e$message)
    empty$error <- e$message
    empty
  })
}

cached_fetch_sec_segment_notes <- memoise::memoise(
  fetch_sec_segment_notes, cache = .ynow_slow_cache
)
