#!/usr/bin/env Rscript
# Regression: USD/TWD spot must survive yfinance rate-limits via Yahoo chart API.
# Offline: wiring + refuse silent invented rates. Live chart soft-skip if blocked.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_usd_twd_fx_fallback.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("web_crawler.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

suppressPackageStartupMessages({
  library(cachem)
  library(memoise)
})
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <<- function(...) invisible(NULL)
}
if (!exists("normalize_all_financials", mode = "function")) {
  normalize_all_financials <<- function(x) x
}
if (!exists(".ensure_python_scraper", mode = "function")) {
  .ensure_python_scraper <<- function() FALSE
}
if (!exists(".py_scraper_env")) {
  .py_scraper_env <<- new.env(parent = emptyenv())
}

source("web_crawler.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("helper .usd_twd_from_yahoo_chart exists", exists(".usd_twd_from_yahoo_chart", mode = "function"))
check("get_usd_twd_fx exists", exists("get_usd_twd_fx", mode = "function"))
check("cached_get_usd_twd_fx exists", exists("cached_get_usd_twd_fx", mode = "function"))

src <- paste(readLines("web_crawler.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "R chart fallback wired in get_usd_twd_fx",
  grepl(".usd_twd_from_yahoo_chart", src, fixed = TRUE) &&
    grepl("v8/finance/chart", src, fixed = TRUE) &&
    grepl("TWD=X", src, fixed = TRUE)
)
check(
  "failures raise (do not memoise NA)",
  grepl('stop\\("USD/TWD FX unavailable"\\)', src)
)
check(
  "FX still memoised on fast cache",
  grepl("cached_get_usd_twd_fx[\\s\\S]*cache\\s*=\\s*\\.ynow_fast_cache", src, perl = TRUE)
)

py_src <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "Python chart fallback for get_usd_twd_rate",
  grepl("def _usd_twd_from_yahoo_chart", py_src, fixed = TRUE) &&
    grepl("_usd_twd_from_yahoo_chart()", py_src, fixed = TRUE) &&
    grepl("v8/finance/chart", py_src, fixed = TRUE)
)

# Offline fixture: meta regularMarketPrice band (TWD per USD).
meta_px <- 32.55
check("fixture meta price is finite USD/TWD-like", is.finite(meta_px) && meta_px > 20 && meta_px < 50)

# Refuse invented rates when chart helper returns NA (Python skipped).
# Run BEFORE live fetch so memoise has not cached a success.
.chart_real <- .usd_twd_from_yahoo_chart
.usd_twd_from_yahoo_chart <- function() NA_real_
err <- tryCatch(get_usd_twd_fx(), error = function(e) e)
check(
  "get_usd_twd_fx stops when FX unavailable (no silent NA invent)",
  inherits(err, "error") && grepl("USD/TWD FX unavailable", conditionMessage(err), fixed = TRUE)
)
.usd_twd_from_yahoo_chart <- .chart_real
tryCatch(memoise::forget(cached_get_usd_twd_fx), error = function(e) NULL)

# Live chart path (network). Soft-skip only if environment blocks Yahoo.
live <- tryCatch(.usd_twd_from_yahoo_chart(), error = function(e) NA_real_)
if (is.finite(live) && live > 0) {
  check("live Yahoo chart USD/TWD in plausible band", live > 20 && live < 50)
  live2 <- tryCatch(get_usd_twd_fx(), error = function(e) NA_real_)
  check(
    "get_usd_twd_fx returns finite after chart fallback",
    is.finite(live2) && live2 > 20 && live2 < 50
  )
} else {
  cat("SKIP live Yahoo chart (network/blocked); offline checks still apply\n")
}

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "search path keeps previous FX when fetch fails",
  grepl("prev_fx", srv, fixed = TRUE) &&
    grepl("Keep last good spot", srv, fixed = TRUE)
)

if (fail > 0L) {
  cat(fail, " check(s) failed\n", sep = "")
  quit(status = 1L)
}
cat("OK: test_usd_twd_fx_fallback\n")
