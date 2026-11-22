#!/usr/bin/env Rscript
# Offline: TW OTC／興櫃 Company KPI blank — wrong .TW board suffix + .TWO fallback.
# Examples: 6488.TWO（環球晶／上櫃）, 1269.TWO（乾杯／興櫃）.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_tw_otc_kpi_summary.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
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

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source("setup.R", local = TRUE, encoding = "UTF-8")
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8")

# --- summary_quote_usable: all-N/A table must not count as success ---
na_sum <- data.frame(
  Item = c(
    "Market Price", "Previous Close", "Market Cap (intraday)",
    "EPS (TTM)", "PE Ratio (TTM)"
  ),
  Value = rep("N/A", 5L),
  stringsAsFactors = FALSE
)
ok_sum <- data.frame(
  Item = c("Market Price", "Previous Close", "Market Cap (intraday)", "EPS (TTM)"),
  Value = c("441.50", "490.00", "233160000000", "19.58"),
  stringsAsFactors = FALSE
)
check("all-N/A summary not usable", isFALSE(summary_quote_usable(na_sum)))
check("priced summary usable", isTRUE(summary_quote_usable(ok_sum)))
check("empty summary not usable", isFALSE(summary_quote_usable(data.frame(
  Item = character(0), Value = character(0), stringsAsFactors = FALSE
))))
check("NULL summary not usable", isFALSE(summary_quote_usable(NULL)))

# --- Universe: bare vs exact exchange (the old fallback pitfall) ---
uni_path <- file.path(app_dir, "data", "tw_universe.csv")
check("tw_universe.csv present", file.exists(uni_path))
if (file.exists(uni_path)) {
  u <- utils::read.csv(uni_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  lab_get_tw_universe <- function(...) u

  check(
    "OTC bare 6488 → 6488.TWO",
    identical(normalize_ticker_for_market("6488", "TW"), "6488.TWO")
  )
  check(
    "wrong suffix 6488.TW re-resolves → 6488.TWO",
    identical(normalize_ticker_for_market("6488.TW", "TW"), "6488.TWO")
  )
  check(
    "ESB bare 1269 → 1269.TWO",
    identical(normalize_ticker_for_market("1269", "TW"), "1269.TWO")
  )
  check(
    "listed 2330 stays .TW",
    identical(normalize_ticker_for_market("2330", "TW"), "2330.TW")
  )

  check(
    "bare lookup 6488.TW sees TPEX (pitfall)",
    identical(lookup_tw_universe_exchange("6488.TW"), "TPEX")
  )
  check(
    "exact lookup 6488.TW is empty",
    identical(lookup_tw_universe_exchange_exact("6488.TW"), "")
  )
  check(
    "exact lookup 6488.TWO is TPEX",
    identical(lookup_tw_universe_exchange_exact("6488.TWO"), "TPEX")
  )
  check(
    "exact lookup 2330.TW is TWSE",
    identical(lookup_tw_universe_exchange_exact("2330.TW"), "TWSE")
  )
  check(
    "exact ESB 1269.TWO",
    identical(lookup_tw_universe_exchange_exact("1269.TWO"), "ESB")
  )

  # Old gate: !nzchar(bare_exchange) blocked OTC retry. New helper must allow it.
  check(
    "TWO fallback for wrong OTC .TW + all-N/A",
    isTRUE(tw_should_try_two_fallback("6488.TW", na_sum))
  )
  check(
    "TWO fallback for wrong ESB .TW + all-N/A",
    isTRUE(tw_should_try_two_fallback("1269.TW", na_sum))
  )
  check(
    "no TWO fallback when quote usable on .TW",
    isFALSE(tw_should_try_two_fallback("6488.TW", ok_sum))
  )
  check(
    "no TWO fallback for exact TWSE even if quote empty",
    isFALSE(tw_should_try_two_fallback("2330.TW", na_sum))
  )
  check(
    "no TWO fallback for .TWO ticker",
    isFALSE(tw_should_try_two_fallback("6488.TWO", na_sum))
  )
}

if (fail > 0L) {
  cat("FAILED:", fail, "checks\n")
  quit(status = 1L)
}
cat("PASS: TW OTC／興櫃 KPI summary usability + .TWO fallback gates\n")
