#!/usr/bin/env Rscript
# Regression: Yahoo statement scrape must prefer curl_cffi session so TW/US
# Lite Smart Analysis can pick a Fair Value primary under shared-host rate limits.
# Offline wiring checks always; live 2330.TW soft-skip if network blocked.
# Run: cd app_27.0/tests && Rscript test_yf_session_financials.R

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
root <- ynow_test_app_dir("deep_scraper.py")
setwd(root)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

py <- paste(readLines("deep_scraper.py", warn = FALSE), collapse = "\n")

check(
  "deep_scraper defines _yf_http_session",
  grepl("def _yf_http_session\\(", py)
)
check(
  "deep_scraper defines _yf_ticker",
  grepl("def _yf_ticker\\(", py)
)
check(
  "_yf_http_session uses curl_cffi chrome impersonation",
  grepl("curl_cffi", py, fixed = TRUE) &&
    grepl("impersonate\\s*=\\s*[\"']chrome[\"']", py)
)
check(
  "scrape_all_financials_yf uses _yf_ticker",
  {
    i <- regexpr("def scrape_all_financials_yf", py, fixed = TRUE)[1]
    i > 0 && grepl("stock = _yf_ticker(", substr(py, i, i + 500L), fixed = TRUE)
  }
)
check(
  "summary quote uses _yf_ticker",
  {
    i <- regexpr("def _get_summary_quote_impl", py, fixed = TRUE)[1]
    i > 0 && grepl("stock = _yf_ticker(", substr(py, i, i + 400L), fixed = TRUE)
  }
)
check(
  "summary quote has chart last-price fallback",
  grepl("def _chart_last_price(", py, fixed = TRUE) &&
    grepl("_chart_last_price()", py, fixed = TRUE)
)
check(
  "fast_get_company_info uses _yf_ticker",
  {
    i <- regexpr("def fast_get_company_info", py, fixed = TRUE)[1]
    i > 0 && grepl("stock = _yf_ticker(", substr(py, i, i + 400L), fixed = TRUE)
  }
)

# Live soft-check: TW 2330 statements must not be empty when Yahoo is reachable.
live <- tolower(Sys.getenv("YNOW_SKIP_LIVE_YAHOO", "")) %in% c("1", "true", "yes")
if (isTRUE(live)) {
  cat("SKIP live 2330.TW scrape (YNOW_SKIP_LIVE_YAHOO)\n")
} else {
  Sys.unsetenv("YNOW_DEBUG_SKIP_PY")
  ok_py <- FALSE
  tryCatch({
    suppressPackageStartupMessages(source("global.R", local = TRUE))
    ok_py <- isTRUE(.ensure_python_scraper())
  }, error = function(e) {
    cat("SKIP live scrape (bootstrap): ", conditionMessage(e), "\n", sep = "")
  })
  if (isTRUE(ok_py) && exists("scrape_all_financials", mode = "function") &&
      exists("normalize_all_financials", mode = "function") &&
      exists("financials_is_bs_cf_all_empty", mode = "function") &&
      exists("recommend_valuation_models", mode = "function")) {
    raw <- tryCatch(scrape_all_financials("2330.TW"), error = function(e) e)
    if (inherits(raw, "error")) {
      cat("SKIP live 2330.TW scrape: ", conditionMessage(raw), "\n", sep = "")
    } else {
      norm <- normalize_all_financials(raw)
      empty <- isTRUE(financials_is_bs_cf_all_empty(norm))
      check("live 2330.TW statements not all-empty", !empty)
      if (!empty) {
        cf <- norm[["Cash Flow"]]$expanded
        is <- norm[["Income Statement"]]$expanded
        bs <- norm[["Balance Sheet"]]$expanded
        rec <- tryCatch(
          recommend_valuation_models(
            cf,
            industry_text = "Semiconductors",
            d_is = is,
            d_bs = bs,
            locale = "en"
          ),
          error = function(e) NULL
        )
        prim <- as.character(rec$primary %||% "")[1]
        check(
          "live 2330.TW recommends Fair Value primary",
          nzchar(prim) && prim %in% c("dcf", "ddm", "pb", "ri", "nav")
        )
      }
    }
  } else if (!isTRUE(ok_py)) {
    cat("SKIP live scrape (Python scraper unavailable)\n")
  }
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All yf-session financials checks passed.\n")
