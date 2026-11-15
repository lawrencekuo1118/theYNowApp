#!/usr/bin/env Rscript
# Phase F: Ranking summary batch + ranking-screen empty-retry gate.
# No live Yahoo. Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_lab_ranking_summary_batch.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

.lab_ranking_summary_clear()
options(ynow.ranking_screen = TRUE)

# --- Batch prefetch feeds lab_fetch_summary_metrics without Summary quote ---
batch_n <- new.env(parent = emptyenv())
batch_n$calls <- 0L
assign(".ensure_python_scraper", function() TRUE, envir = .GlobalEnv)
assign("get_ranking_summary_batch", function(tickers) {
  batch_n$calls <- batch_n$calls + 1L
  tks <- toupper(trimws(as.character(unlist(tickers, use.names = FALSE))))
  out <- stats::setNames(vector("list", length(tks)), tks)
  for (tk in tks) {
    if (identical(tk, "AAA")) {
      out[[tk]] <- list(
        market_cap = 1e9, price = 10, beta = 1.1,
        company_name = "Alpha Co", currency = "USD", financial_currency = "USD"
      )
    } else if (identical(tk, "BBB")) {
      out[[tk]] <- list(
        market_cap = 2e9, price = 20, beta = 0.9,
        company_name = "Beta Co", currency = "USD", financial_currency = "USD"
      )
    } else if (identical(tk, "NAMEONLY")) {
      # Yahoo often returns a name with no price/mcap — must not block Summary.
      out[[tk]] <- list(
        market_cap = NULL, price = NULL, beta = NULL,
        company_name = "Name Only Inc", currency = "USD", financial_currency = NULL
      )
    } else {
      out[[tk]] <- NULL
    }
  }
  out
}, envir = .GlobalEnv)

# Guard: live summary must not run when complete batch hit exists.
sum_calls <- new.env(parent = emptyenv())
sum_calls$n <- 0L
assign("cached_get_summary_data", function(stock_code) {
  sum_calls$n <- sum_calls$n + 1L
  stop("should not call cached_get_summary_data for complete batch hit")
}, envir = .GlobalEnv)

n_hit <- lab_prefetch_ranking_summaries(c("AAA", "BBB", "CCC", "NAMEONLY"))
check("prefetch reports two complete hits", identical(as.integer(n_hit), 2L))
check("prefetch called batch once", identical(batch_n$calls, 1L))

sum_calls$n <- 0L
sm_a <- lab_fetch_summary_metrics("AAA")
sm_b <- lab_fetch_summary_metrics("BBB")
check("AAA price from batch", isTRUE(abs(sm_a$price - 10) < 1e-9))
check("AAA mcap from batch", isTRUE(abs(sm_a$market_cap - 1e9) < 1e-6))
check("AAA beta from batch", isTRUE(abs(sm_a$beta - 1.1) < 1e-9))
check("AAA name from batch", identical(sm_a$company_name, "Alpha Co"))
check("BBB price from batch", isTRUE(abs(sm_b$price - 20) < 1e-9))
check("complete batch hits skip Summary quote", identical(sum_calls$n, 0L))

# Partial name-only must fall through to Summary (price required).
assign("cached_get_summary_data", function(stock_code) {
  sum_calls$n <- sum_calls$n + 1L
  df <- data.frame(
    Item = c("Market Cap", "Previous Close", "Beta (5Y Monthly)"),
    Value = c("5B", "55", "0.8"),
    stringsAsFactors = FALSE
  )
  attr(df, "company_name") <- "From Summary"
  df
}, envir = .GlobalEnv)
sum_calls$n <- 0L
sm_name <- lab_fetch_summary_metrics("NAMEONLY")
check("name-only batch falls through to Summary", sum_calls$n >= 1L)
check("name-only gets Summary price", isTRUE(abs(sm_name$price - 55) < 1e-9))
check("name-only gets Summary mcap", isTRUE(abs(sm_name$market_cap - 5e9) < 1e-3))

# CCC is a batch miss: Summary fallback is allowed (and expected).
assign("cached_get_summary_data", function(stock_code) {
  sum_calls$n <- sum_calls$n + 1L
  data.frame(Item = character(0), Value = character(0), stringsAsFactors = FALSE)
}, envir = .GlobalEnv)
sum_calls$n <- 0L
sm_c <- lab_fetch_summary_metrics("CCC")
check("CCC miss stays NA (no false fill)", !is.finite(sm_c$price))
check("CCC miss may call Summary fallback", sum_calls$n >= 1L)

# Equivalence: same batch inputs → same metrics on second prefetch
.lab_ranking_summary_clear()
lab_prefetch_ranking_summaries(c("AAA", "BBB"))
sm_a2 <- lab_fetch_summary_metrics("AAA")
check(
  "identical inputs → identical batch metrics",
  isTRUE(abs(sm_a2$price - sm_a$price) < 1e-12) &&
    isTRUE(abs(sm_a2$market_cap - sm_a$market_cap) < 1e-6) &&
    isTRUE(abs(sm_a2$beta - sm_a$beta) < 1e-12)
)

# Outside Ranking screen the batch cache must not shadow Summary.
sum_calls$n <- 0L
assign("cached_get_summary_data", function(stock_code) {
  sum_calls$n <- sum_calls$n + 1L
  df <- data.frame(
    Item = c("Market Cap", "Previous Close", "Beta (5Y Monthly)"),
    Value = c("9B", "99", "1.2"),
    stringsAsFactors = FALSE
  )
  attr(df, "company_name") <- "Post Screen"
  df
}, envir = .GlobalEnv)
options(ynow.ranking_screen = FALSE)
sm_out <- lab_fetch_summary_metrics("AAA")
check("outside ranking_screen uses Summary", sum_calls$n >= 1L)
check("outside ranking_screen ignores batch price", isTRUE(abs(sm_out$price - 99) < 1e-9))
options(ynow.ranking_screen = TRUE)

# Screen wires prefetch + ranking_screen option + clears cache on exit
screen_src <- paste(
  readLines(file.path(app_dir, "lab_im_bluechip_eval.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "screen prefetches ranking summaries",
  grepl("lab_prefetch_ranking_summaries", screen_src, fixed = TRUE)
)
check(
  "screen sets ynow.ranking_screen",
  grepl('options(ynow.ranking_screen = TRUE)', screen_src, fixed = TRUE)
)
check(
  "screen clears batch cache on exit",
  grepl(".lab_ranking_summary_clear", screen_src, fixed = TRUE) &&
    grepl("on.exit", screen_src, fixed = TRUE)
)

# --- Ranking screen: empty statements do not immediately re-hit Yahoo ---
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <<- function(...) invisible(NULL)
}
suppressPackageStartupMessages({
  library(cachem)
  library(memoise)
})
source(file.path(app_dir, "htcdi_config.R"), local = FALSE)
source(file.path(app_dir, "web_crawler.R"), local = FALSE)
# Rebuild memo with stubs after sourcing web_crawler.
empty_payload <- list(
  "Income Statement" = list(
    collapsed = data.frame(Breakdown = character(0), stringsAsFactors = FALSE),
    expanded = data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
  ),
  "Balance Sheet" = list(
    collapsed = data.frame(Breakdown = character(0), stringsAsFactors = FALSE),
    expanded = data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
  ),
  "Cash Flow" = list(
    collapsed = data.frame(Breakdown = character(0), stringsAsFactors = FALSE),
    expanded = data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
  )
)
uncached_n <- new.env(parent = emptyenv())
uncached_n$n <- 0L
assign(".scrape_financials_uncached", function(stock_code) {
  uncached_n$n <- uncached_n$n + 1L
  empty_payload
}, envir = .GlobalEnv)
assign("lookup_statement_fallback_ticker", function(stock_code) "", envir = .GlobalEnv)
assign("financials_is_bs_cf_all_empty", function(res) TRUE, envir = .GlobalEnv)
.cached_scrape_financials_memo <<- memoise::memoise(
  function(stock_code) {
    uncached_n$n <- uncached_n$n + 1L
    empty_payload
  },
  cache = cachem::cache_mem(max_age = 3600)
)

options(ynow.ranking_screen = TRUE)
uncached_n$n <- 0L
res_rank <- cached_scrape_financials("EMPTY1")
calls_rank <- uncached_n$n
check("ranking empty returns empty payload", isTRUE(financials_is_bs_cf_all_empty(res_rank)))
check(
  "ranking empty does not double-fetch",
  identical(as.integer(calls_rank), 1L)
)

options(ynow.ranking_screen = FALSE)
uncached_n$n <- 0L
.cached_scrape_financials_memo <<- memoise::memoise(
  function(stock_code) {
    uncached_n$n <- uncached_n$n + 1L
    empty_payload
  },
  cache = cachem::cache_mem(max_age = 3600)
)
res_search <- cached_scrape_financials("EMPTY2")
calls_search <- uncached_n$n
check("search empty still retries once", identical(as.integer(calls_search), 2L))
check("search empty still empty after retry", isTRUE(financials_is_bs_cf_all_empty(res_search)))

# Python API presence (source text — no network)
py <- paste(readLines(file.path(app_dir, "deep_scraper.py"), warn = FALSE), collapse = "\n")
check("get_ranking_summary_batch defined", grepl("def get_ranking_summary_batch\\(", py))
check("yf http session is reused", grepl("_YF_HTTP_SESSION", py, fixed = TRUE))
check("statement path can skip stock.info", grepl("_RANKING_CCY_HINTS", py, fixed = TRUE))
check(
  "need_info requires financial_currency for non-TW",
  grepl("not quote_ccy or not fin_ccy", py, fixed = TRUE)
)
pool_src <- paste(readLines(file.path(app_dir, "lab_im_pool.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "batch complete requires price",
  grepl(".lab_ranking_summary_complete", pool_src, fixed = TRUE) &&
    grepl("ynow.ranking_screen", pool_src, fixed = TRUE)
)
wc <- paste(readLines(file.path(app_dir, "web_crawler.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "ranking defers dividend event scrape",
  grepl("dividends_deferred", wc, fixed = TRUE) &&
    grepl("ynow.ranking_screen", wc, fixed = TRUE)
)
check(
  "search lazily attaches deferred dividends",
  grepl(".ynow_ensure_statement_dividends", wc, fixed = TRUE)
)

options(ynow.ranking_screen = NULL)
.lab_ranking_summary_clear()

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
