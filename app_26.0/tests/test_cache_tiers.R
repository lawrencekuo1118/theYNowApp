#!/usr/bin/env Rscript
# Fast vs slow cache tiers (no network).
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_cache_tiers.R

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

# Minimal deps for sourcing crawler without full global.R
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

info <- ynow_cache_tier_info()
check("fast TTL is 15 minutes", identical(as.integer(info$fast_ttl_sec), 900L))
check("slow TTL is 24 hours", identical(as.integer(info$slow_ttl_sec), 86400L))
# Local/dev default: in-memory fast tier. Shared disk when YNOW_CACHE_ROOT /
# YNOW_FAST_CACHE_DIR / hosted — covered by test_yahoo_rate_gate.R.
check(
  "fast cache is mem (local) or disk (shared)",
  grepl("cache_mem", info$fast_class, fixed = TRUE) ||
    grepl("cache_disk", info$fast_class, fixed = TRUE)
)
check(
  "slow cache is disk or mem fallback",
  grepl("cache_disk", info$slow_class, fixed = TRUE) ||
    grepl("cache_mem", info$slow_class, fixed = TRUE)
)
check("my_cache aliases fast tier", identical(my_cache, .ynow_fast_cache))
check("financials != fast cache object", !identical(.ynow_slow_cache, .ynow_fast_cache))
check("cache info exposes yahoo gate fields", is.logical(info$yahoo_gate_enabled) &&
  is.finite(as.numeric(info$yahoo_min_interval_ms)))
check("cache info exposes cache_root", is.character(info$cache_root) && nzchar(info$cache_root))

src <- paste(readLines("web_crawler.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "financials memoised on slow cache",
  grepl("\\.cached_scrape_financials_memo[\\s\\S]*cache\\s*=\\s*\\.ynow_slow_cache", src, perl = TRUE)
)
check(
  "Rf memoised on fast cache",
  grepl("\\.cached_fetch_rf_live_us[\\s\\S]*cache\\s*=\\s*\\.ynow_fast_cache", src, perl = TRUE) &&
    grepl("\\.cached_fetch_rf_live_tw[\\s\\S]*cache\\s*=\\s*\\.ynow_fast_cache", src, perl = TRUE)
)
check(
  "FX memoised on fast cache",
  grepl("cached_get_usd_twd_fx[\\s\\S]*cache\\s*=\\s*\\.ynow_fast_cache", src, perl = TRUE)
)
check(
  "summary quote memoised on fast cache",
  grepl("cached_get_summary_data[\\s\\S]*cache\\s*=\\s*\\.ynow_fast_cache", src, perl = TRUE)
)
check(
  "SEC notes on slow cache",
  grepl("cached_fetch_sec_report_notes[\\s\\S]*cache\\s*=\\s*\\.ynow_slow_cache", src, perl = TRUE) &&
    grepl("cached_fetch_sec_segment_notes[\\s\\S]*cache\\s*=\\s*\\.ynow_slow_cache", src, perl = TRUE)
)
check(
  "architecture comment documents tiers",
  grepl("strict fast vs slow", src, fixed = TRUE) &&
    grepl("15m", src, fixed = TRUE) &&
    grepl("24h", src, fixed = TRUE)
)

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server uses cached summary", grepl("cached_get_summary_data(", srv, fixed = TRUE))
check("server uses cached industry", grepl("cached_get_yahoo_industry(", srv, fixed = TRUE))
check(
  "FX refresh default uses fast TTL",
  grepl("\\.refresh_fx_if_stale\\s*<-\\s*function\\(max_age_sec\\s*=\\s*\\.YNOW_CACHE_FAST_TTL_SEC\\)", srv)
)

# Guard: financials must not share the fast memoise cache reference in source
check(
  "financials memo uses slow cache",
  grepl("\\.cached_scrape_financials_memo\\s*<-\\s*memoise::memoise", src) &&
    grepl("\\.cached_scrape_financials_memo[\\s\\S]{0,200}?cache\\s*=\\s*\\.ynow_slow_cache", src, perl = TRUE)
)
check(
  "financials memo does not use .ynow_fast_cache",
  !grepl("\\.cached_scrape_financials_memo[\\s\\S]{0,200}?\\.ynow_fast_cache", src, perl = TRUE)
)
check(
  "empty financials are not left in memo (drop + wrapper)",
  grepl("drop_financials_cache_key", src, fixed = TRUE) &&
    grepl("lookup_statement_fallback_ticker", src, fixed = TRUE) &&
    grepl("cached_scrape_financials\\s*<-\\s*function", src)
)
# Source-time memoise::forget() calls cache$reset() on the shared tier and
# wipes sticky hosted hits for every key. Tests may still forget intentionally.
check(
  "no source-time memoise::forget on shared tiers",
  !grepl("tryCatch\\(memoise::forget\\(", src)
)

# Offline put/get on the fast tier object (no network; asserts cachem API)
check(
  "fast cache set/get roundtrip",
  {
    key <- paste0("ynow_test_cache_tiers_", as.integer(Sys.time()))
    .ynow_fast_cache$set(key, list(ok = TRUE, n = 42L))
    got <- .ynow_fast_cache$get(key)
    .ynow_fast_cache$remove(key)
    is.list(got) && isTRUE(got$ok) && identical(got$n, 42L)
  }
)
check(
  "slow cache set/get roundtrip",
  {
    key <- paste0("ynow_test_cache_tiers_slow_", as.integer(Sys.time()))
    .ynow_slow_cache$set(key, "slow-ok")
    got <- .ynow_slow_cache$get(key)
    .ynow_slow_cache$remove(key)
    identical(got, "slow-ok")
  }
)
check(
  "fast and slow caches are distinct objects",
  !identical(.ynow_fast_cache, .ynow_slow_cache)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
