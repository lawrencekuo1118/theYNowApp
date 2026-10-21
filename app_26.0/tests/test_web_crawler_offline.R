#!/usr/bin/env Rscript
# Offline smoke: web_crawler helpers beyond cache tiers / yahoo gate (no network).
# Asserts current pure helpers + presets — does not hit live Yahoo/Python.
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_web_crawler_offline.R

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
    file.path(wd, "app_26.0", "tests", "ynow_test_app_dir.R")
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

# market_profile supplies normalize_market_mode so TW/US presets diverge.
if (file.exists("market_profile.R")) {
  source("market_profile.R", local = FALSE, encoding = "UTF-8")
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

# --- Empty summary currency attrs (quote vs reporting) ---
s_us <- .empty_summary("AAPL", "Apple")
check("empty summary US company_name", identical(attr(s_us, "company_name"), "Apple"))
check("empty summary US quote USD", identical(attr(s_us, "currency"), "USD"))
check("empty summary US financialCurrency NA (ADR-safe)", identical(attr(s_us, "financialCurrency"), NA_character_))

s_tw <- .empty_summary("2330.TW")
check("empty summary TW company defaults to ticker", identical(attr(s_tw, "company_name"), "2330.TW"))
check("empty summary TW quote TWD", identical(attr(s_tw, "currency"), "TWD"))
check("empty summary TW financialCurrency TWD", identical(attr(s_tw, "financialCurrency"), "TWD"))

s_two <- .empty_summary("5483.TWO", "中美晶")
check("empty summary TWO TWD", identical(attr(s_two, "currency"), "TWD") &&
        identical(attr(s_two, "financialCurrency"), "TWD"))

# --- Rf helpers (documented fallbacks; no live fetch) ---
check("rf mode default US without market helpers", identical(.rf_market_mode(NULL), "US") ||
        identical(.rf_market_mode("US"), "US"))
bits_us <- .rf_profile_bits("US")
bits_tw <- .rf_profile_bits("TW")
fb_us <- .rf_documented_fallback("US", list(rf_fallback = 5.0))
fb_tw <- .rf_documented_fallback("TW", list(rf_fallback = 1.8))
check("rf documented US 5.0", isTRUE(abs(fb_us - 5.0) < 1e-9))
check("rf documented TW 1.8", isTRUE(abs(fb_tw - 1.8) < 1e-9))
check(
  "rf profile bits feed documented fallback",
  is.finite(.rf_documented_fallback("US", bits_us)) &&
    is.finite(.rf_documented_fallback("TW", bits_tw))
)
check(
  "rf invalid fallback → US default",
  isTRUE(abs(.rf_documented_fallback("US", list(rf_fallback = -1)) - 5.0) < 1e-9)
)
check(
  "rf invalid fallback → TW default",
  isTRUE(abs(.rf_documented_fallback("TW", list(rf_fallback = NA_real_)) - 1.8) < 1e-9)
)
check("rf profile US has symbol", nzchar(as.character(bits_us$rf_symbol %||% "")[1]))
check("rf profile TW has label", nzchar(as.character(bits_tw$rf_label_zh %||% "")[1]))

# --- Statement fallback stamp (attrs only; no HTCDI network) ---
toy <- list(income = data.frame(a = 1), balance = data.frame(b = 2), cash = data.frame(c = 3))
stamped <- .stamp_financials_fallback(toy, "2330.TW", reason = "local_ordinary")
check(
  "stamp sets source ticker",
  identical(attr(stamped, "statement_source_ticker"), "2330.TW")
)
check(
  "stamp sets fallback reason",
  identical(attr(stamped, "statement_fallback"), "local_ordinary")
)
check("stamp NULL stays NULL", is.null(.stamp_financials_fallback(NULL, "X")))

# --- Market ticker presets / suggest labels / empty search ---
us_p <- ticker_presets_for_market("US")
tw_p <- ticker_presets_for_market("TW")
check("US presets named character", is.character(us_p) && length(us_p) >= 5L && !is.null(names(us_p)))
check("TW presets named character", is.character(tw_p) && length(tw_p) >= 5L && !is.null(names(tw_p)))
check("US presets include AAPL", "AAPL" %in% unname(us_p))
check("TW presets include 2330.TW", "2330.TW" %in% unname(tw_p))
check("US/TW presets differ", !identical(us_p, tw_p))

lab_us <- .format_suggest_label("MSFT", "MSFT — Microsoft", "US")
check("US suggest keeps label", identical(lab_us, "MSFT — Microsoft") || grepl("Microsoft", lab_us, fixed = TRUE))
lab_blank <- .format_suggest_label("ZZZ", "", "US")
check("US suggest blank label → symbol", identical(lab_blank, "ZZZ"))

empty_q <- search_ticker_choices("", max_results = 5L, market = "US")
check("empty query returns US presets", identical(unname(empty_q), unname(us_p)) ||
        identical(empty_q, us_p) || setequal(unname(empty_q), unname(us_p)))

# Guard: live fetch paths stay behind YNOW_DEBUG_SKIP_PY / scraper; we only exercise pure helpers.
src <- paste(readLines("web_crawler.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "crawler still separates fast vs slow TTLs",
  grepl("YNOW_CACHE_FAST_TTL_SEC", src, fixed = TRUE) &&
    grepl("YNOW_CACHE_SLOW_TTL_SEC", src, fixed = TRUE)
)
check(
  "empty summary helper present for ADR currency safety",
  grepl("\\.empty_summary\\s*<-\\s*function", src)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All web_crawler offline checks passed.\n")
