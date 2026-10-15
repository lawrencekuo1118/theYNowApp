#!/usr/bin/env Rscript
# Ticker typeahead: exact equity before leveraged single-stock ETFs (offline).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

source("market_profile.R", local = FALSE, encoding = "UTF-8")
# Minimal stubs used by search_ticker_choices when PY is skipped
if (!exists("search_tickers", mode = "function")) {
  search_tickers <<- function(query, max_results = 12L) {
    # Deliberately ETF-first order (Yahoo sometimes surfaces leveraged products early).
    list(
      list(
        symbol = "AAPU", name = "Direxion Daily AAPL Bull 2X Shares",
        type = "ETF", exchange = "NASDAQ",
        label = "AAPU — Direxion Daily AAPL Bull 2X Shares (ETF · NASDAQ)"
      ),
      list(
        symbol = "AAPD", name = "Direxion Daily AAPL Bear 1X Shares",
        type = "ETF", exchange = "NASDAQ",
        label = "AAPD — Direxion Daily AAPL Bear 1X Shares (ETF · NASDAQ)"
      ),
      list(
        symbol = "AAPL", name = "Apple Inc.",
        type = "EQUITY", exchange = "NASDAQ",
        label = "AAPL — Apple Inc. (EQUITY · NASDAQ)"
      ),
      list(
        symbol = "AAPL.TO", name = "APPLE CDR",
        type = "EQUITY", exchange = "Toronto",
        label = "AAPL.TO — APPLE CDR (EQUITY · Toronto)"
      )
    )
  }
}
if (!exists("is_us_primary_listing_exchange", mode = "function")) {
  is_us_primary_listing_exchange <<- function(ex) {
    grepl("NASDAQ|NYSE|BATS|NYSEArca|NMS|NGM|NCM", as.character(ex %||% ""), ignore.case = TRUE)
  }
}
if (!exists("ticker_presets_for_market", mode = "function")) {
  ticker_presets_for_market <<- function(mode) {
    if (identical(normalize_market_mode(mode), "TW")) {
      stats::setNames("2330.TW", "2330 — 台積電")
    } else {
      stats::setNames(c("AAPL", "MSFT", "JPM", "KO"), c("AAPL", "MSFT", "JPM", "KO"))
    }
  }
}
if (!exists("search_us_universe_by_name", mode = "function")) {
  search_us_universe_by_name <<- function(query, max_results = 12L) character(0)
}
if (!exists("search_tw_universe_by_name", mode = "function")) {
  search_tw_universe_by_name <<- function(query, max_results = 12L) {
    if (grepl("2330", query)) stats::setNames("2330.TW", "2330 — 台積電") else character(0)
  }
}
if (!exists(".format_suggest_label", mode = "function")) {
  .format_suggest_label <<- function(sym, lab, mode) {
    if (nzchar(as.character(lab %||% "")[1])) as.character(lab)[1] else as.character(sym)[1]
  }
}
if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <<- function(...) invisible(NULL)
}

source("web_crawler.R", local = FALSE, encoding = "UTF-8")

hits <- search_ticker_choices("AAPL", market = "US")
check("AAPL query returns hits", length(hits) >= 1L)
check("AAPL exact equity is first suggestion", identical(unname(hits)[[1]], "AAPL"))
check("AAPL appears before AAPU", {
  vals <- unname(hits)
  i_eq <- match("AAPL", vals)
  i_etf <- match("AAPU", vals)
  is.finite(i_eq) && (is.na(i_etf) || i_eq < i_etf)
})

# TW bare code → universe / normalize preference
tw_hits <- search_ticker_choices("2330", market = "TW")
check("TW 2330 returns hits", length(tw_hits) >= 1L)
check("TW 2330 first is 2330.TW", identical(toupper(unname(tw_hits)[[1]]), "2330.TW"))

# Python ranking helper (offline mock of Yahoo quote order)
py <- file.path(app_dir, ".ynow_venv", "bin", "python")
if (!file.exists(py)) py <- Sys.which("python3")
py_script <- file.path(test_dir, "test_ticker_search_rank.py")
py_rank <- system2(py, args = py_script, stdout = TRUE, stderr = TRUE)
check(
  "Python ranking puts AAPL before leveraged ETFs",
  length(py_rank) >= 1L && grepl("^OK AAPL,", py_rank[[length(py_rank)]])
)

cat("FAILS ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
