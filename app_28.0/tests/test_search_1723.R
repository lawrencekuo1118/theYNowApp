#!/usr/bin/env Rscript
# Regression: bare 1723 (中鋼碳素) must resolve in typeahead + Search wiring.
# Offline: universe / normalize / US→TW Search goto Company / deferred name paint.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_search_1723.R
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

check("1723 looks like TW code", isTRUE(looks_like_tw_stock_code("1723")))
check(
  "normalize 1723 → 1723.TW",
  identical(normalize_ticker_for_market("1723", "TW"), "1723.TW")
)

uni_path <- file.path(app_dir, "data", "tw_universe.csv")
check("tw_universe.csv present", file.exists(uni_path))
if (file.exists(uni_path)) {
  # Prefer committed fixture over network.
  u <- utils::read.csv(uni_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  lab_get_tw_universe <- function(...) u
  check("universe contains 1723.TW", any(toupper(u$ticker) == "1723.TW"))
  nm <- lookup_tw_universe_company_name("1723.TW")
  check(
    "official name is 中鋼碳素…",
    nzchar(nm) && grepl("中鋼碳素", nm, fixed = TRUE)
  )
  hits <- search_tw_universe_by_name("1723", max_results = 5L)
  check("universe search hits 1723.TW", "1723.TW" %in% unname(hits))
}

search_tickers <- function(...) {
  list(
    list(
      symbol = "1723.T", label = "1723.T — Tokyo",
      exchDisp = "Tokyo Stock Exchange"
    ),
    list(
      symbol = "TIGACX", label = "TIGACX — mutual fund",
      exchDisp = "NASDAQ"
    ),
    list(
      symbol = "1723.TW", label = "1723.TW — China Steel Chemical",
      exchDisp = "Taiwan"
    )
  )
}
source("web_crawler.R", local = TRUE, encoding = "UTF-8")
ch_tw <- search_ticker_choices("1723", market = "TW")
ch_us <- search_ticker_choices("1723", market = "US")
check("TW typeahead includes 1723.TW", "1723.TW" %in% unname(ch_tw))
check("US typeahead includes 1723.TW", "1723.TW" %in% unname(ch_us))
check(
  "US typeahead ranks 1723.TW first",
  length(ch_us) >= 1L && identical(toupper(unname(ch_us)[[1]]), "1723.TW")
)

srv <- paste(
  readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "Search US→TW pending goto dashboard",
  grepl(
    "pending_recent_fetch\\(list\\([\\s\\S]{0,200}?goto_tab\\s*=\\s*[\"']dashboard[\"']",
    srv,
    perl = TRUE
  )
)
check(
  "Search uses crawl-wanted gate for goto",
  grepl("search_goto", srv, fixed = TRUE) &&
    grepl("ynow_statement_crawl_wanted\\(tab_now", srv, perl = TRUE)
)
# Deferred crawl still paints TW official name (before return).
obs_pos <- regexpr(
  "observeEvent\\(list\\(current_ticker\\(\\), ticker_search_gen\\(\\)\\)",
  srv,
  perl = TRUE
)[1]
obs_chunk <- if (obs_pos > 0L) {
  substr(srv, obs_pos, min(nchar(srv), obs_pos + 4500L))
} else {
  ""
}
name_pos <- regexpr("lookup_tw_universe_company_name", obs_chunk, fixed = TRUE)[1]
defer_pos <- regexpr("statement_crawl_deferred\\(TRUE\\)", obs_chunk, perl = TRUE)[1]
check(
  "TW official name painted before deferred crawl return",
  name_pos > 0L && defer_pos > 0L && name_pos < defer_pos
)
check(
  "requirements pin yfinance>=0.2.66",
  {
    req <- paste(readLines("requirements.txt", warn = FALSE), collapse = "\n")
    grepl("yfinance==0\\.2\\.6[6-9]|yfinance==0\\.2\\.[7-9]|yfinance==1\\.", req)
  }
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED: search 1723 regression\n")
