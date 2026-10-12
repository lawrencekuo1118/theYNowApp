#!/usr/bin/env Rscript
# Statement fetch resilience: empty-cache drop + ADR ordinary fallback (no network).
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_financials_empty_fallback.R

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
if (!exists("financials_is_bs_cf_all_empty", mode = "function")) {
  source("setup.R", local = TRUE, encoding = "UTF-8")
}

source("htcdi_config.R", local = TRUE, encoding = "UTF-8")
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

check(
  "TSM maps to 2330.TW via HTCDI ordinary fallback",
  identical(lookup_statement_fallback_ticker("TSM"), "2330.TW")
)
check(
  "AAPL has no ordinary fallback",
  identical(lookup_statement_fallback_ticker("AAPL"), "")
)
check(
  "ASMIY ADR maps when local_ordinary set (or empty if unset)",
  {
    loc <- lookup_statement_fallback_ticker("ASMIY")
    is.character(loc) && length(loc) == 1L
  }
)
check(
  "2330.TW itself has no fallback (already ordinary)",
  identical(lookup_statement_fallback_ticker("2330.TW"), "")
)

# Empty-frame helpers
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
filled <- empty_payload
filled[["Income Statement"]]$expanded <- data.frame(
  Breakdown = "Total Revenue", `12/31/2024` = 100, check.names = FALSE,
  stringsAsFactors = FALSE
)
filled[["Balance Sheet"]]$expanded <- data.frame(
  Breakdown = "Total Debt", `12/31/2024` = 10, check.names = FALSE,
  stringsAsFactors = FALSE
)
filled[["Cash Flow"]]$expanded <- data.frame(
  Breakdown = "Free Cash Flow", `12/31/2024` = 40, check.names = FALSE,
  stringsAsFactors = FALSE
)

check("empty helper true on empty_payload", isTRUE(financials_is_bs_cf_all_empty(empty_payload)))
check("empty helper false on filled", isFALSE(financials_is_bs_cf_all_empty(filled)))

# Simulate memo poison: empty then drop + ordinary returns filled
calls <- character(0)
scrape_all_financials <<- function(stock_code) {
  calls <<- c(calls, as.character(stock_code)[1])
  if (identical(toupper(stock_code), "TSM")) return(empty_payload)
  if (identical(toupper(stock_code), "2330.TW")) return(filled)
  empty_payload
}
.ensure_python_scraper <<- function() TRUE
normalize_all_financials <<- function(x) x

# Rebuild memo against the stub (forget old)
tryCatch(memoise::forget(.cached_scrape_financials_memo), error = function(e) NULL)
.cached_scrape_financials_memo <<- memoise::memoise(
  .scrape_financials_uncached,
  cache = cachem::cache_mem(max_age = 3600)
)

res <- cached_scrape_financials("TSM")
check("fallback used local ordinary source attr", identical(attr(res, "statement_source_ticker"), "2330.TW"))
check("fallback reason stamped", grepl("local_ordinary", attr(res, "statement_fallback") %||% "", fixed = TRUE))
check("fallback statements not empty", isFALSE(financials_is_bs_cf_all_empty(res)))
check("TSM empty then 2330.TW attempted", any(toupper(calls) == "TSM") && any(toupper(calls) == "2330.TW"))

# Second call should not re-poison: if memo for TSM was empty it must have been dropped
calls <- character(0)
res2 <- cached_scrape_financials("TSM")
check("second TSM call still non-empty", isFALSE(financials_is_bs_cf_all_empty(res2)))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All financials empty-fallback checks passed.\n")
