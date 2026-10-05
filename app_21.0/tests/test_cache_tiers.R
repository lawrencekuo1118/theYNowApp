#!/usr/bin/env Rscript
# Fast vs slow cache tiers (no network).
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_cache_tiers.R

root <- if (file.exists("web_crawler.R")) {
  getwd()
} else if (file.exists("../web_crawler.R")) {
  normalizePath("..")
} else if (dir.exists("app_21.0") && file.exists("app_21.0/web_crawler.R")) {
  normalizePath("app_21.0")
} else {
  stop("Cannot locate app_21.0 / web_crawler.R")
}
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
check("fast cache is cache_mem", grepl("cache_mem", info$fast_class, fixed = TRUE))
check(
  "slow cache is disk or mem fallback",
  grepl("cache_disk", info$slow_class, fixed = TRUE) ||
    grepl("cache_mem", info$slow_class, fixed = TRUE)
)
check("my_cache aliases fast tier", identical(my_cache, .ynow_fast_cache))
check("financials != fast cache object", !identical(.ynow_slow_cache, .ynow_fast_cache))

src <- paste(readLines("web_crawler.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "financials memoised on slow cache",
  grepl("cached_scrape_financials[\\s\\S]*cache\\s*=\\s*\\.ynow_slow_cache", src, perl = TRUE)
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
fin_block <- regmatches(
  src,
  regexpr(
    "cached_scrape_financials\\s*<-\\s*memoise::memoise\\([\\s\\S]*?\\n\\)",
    src,
    perl = TRUE
  )
)
check("financials block found", length(fin_block) == 1L && nzchar(fin_block))
check(
  "financials block does not use .ynow_fast_cache",
  !grepl("\\.ynow_fast_cache", fin_block, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
