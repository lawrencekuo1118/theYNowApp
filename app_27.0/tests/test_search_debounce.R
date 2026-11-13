#!/usr/bin/env Rscript
# Search-box debounce + typeahead error handling (offline).
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_search_debounce.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

root <- if (file.exists("ynow_ui.R")) {
  normalizePath(".")
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else {
  stop("Cannot locate app_27.0")
}
setwd(root)

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
css <- paste(readLines("www/ynow_widgets.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
py <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("web_crawler.R", local = TRUE, encoding = "UTF-8")

check(
  "YNOW_SEARCH_DEBOUNCE_MS is 300",
  exists("YNOW_SEARCH_DEBOUNCE_MS") && identical(as.integer(YNOW_SEARCH_DEBOUNCE_MS), 300L)
)
check(
  "YNOW_SEARCH_DEBOUNCE_COALESCE_MS is 50",
  exists("YNOW_SEARCH_DEBOUNCE_COALESCE_MS") &&
    identical(as.integer(YNOW_SEARCH_DEBOUNCE_COALESCE_MS), 50L)
)

check(
  "client typeahead debounce 300ms",
  grepl("YNOW_SEARCH_DEBOUNCE_MS = 300", ui, fixed = TRUE) &&
    grepl("scheduleTypeahead", ui, fixed = TRUE) &&
    grepl("setTimeout(function()", ui, fixed = TRUE)
)
check(
  "client pushTypeahead uses try/catch",
  grepl("function pushTypeahead", ui, fixed = TRUE) &&
    grepl("catch (eSet)", ui, fixed = TRUE)
)
check(
  "client flush on Enter/blur",
  grepl("function flushTypeahead", ui, fixed = TRUE) &&
    grepl("flushTypeahead(inputId)", ui, fixed = TRUE)
)

check(
  "server typeahead uses shiny::debounce coalesce",
  grepl("ticker_typeahead_q <- shiny::debounce", srv, fixed = TRUE) &&
    grepl("YNOW_SEARCH_DEBOUNCE_COALESCE_MS", srv, fixed = TRUE) &&
    !grepl("millis = 280", srv, fixed = TRUE)
)
check(
  "server typeahead observeEvent wraps tryCatch",
  grepl("observeEvent\\(ticker_typeahead_q\\(\\)", srv, perl = TRUE) &&
    grepl("ticker_typeahead_err", srv, fixed = TRUE) &&
    grepl("typeahead_network_error", srv, fixed = TRUE) &&
    grepl("ticker_typeahead_gen", srv, fixed = TRUE)
)
check(
  "Radar focus suggest filters on debounced query",
  grepl("ticker_typeahead_q\\(\\)", srv, perl = TRUE) &&
    grepl("Debounced query only", srv, fixed = TRUE) &&
    !grepl(
      "q <- trimws\\(as\\.character\\(input\\$ticker_typeahead",
      srv,
      perl = TRUE
    )
)

check(
  "SEC keyword filter is debounced ~300ms",
  grepl("lab_sec_keyword_q <- shiny::debounce", srv, fixed = TRUE) &&
    grepl("YNOW_SEARCH_DEBOUNCE_MS", srv, fixed = TRUE) &&
    grepl("lab_sec_keyword_q\\(\\)", srv, perl = TRUE)
)

check(
  "i18n typeahead_network_error en + zh-TW",
  grepl("typeahead_network_error = paste0(", loc, fixed = TRUE) &&
    grepl(
      "Ticker search is temporarily unavailable",
      loc,
      fixed = TRUE
    ) &&
    grepl("代號搜尋暫時無法連線", loc, fixed = TRUE)
)
check(
  "suggest error CSS present",
  grepl("ynow-suggest-error", css, fixed = TRUE)
)

check(
  "Python search_tickers has try/except",
  grepl("def search_tickers", py, fixed = TRUE) &&
    grepl("except Exception as e:", py, fixed = TRUE)
)

# Offline search_ticker_choices must not throw; network-error attr only on remote fail.
search_tickers <- function(query, max_results = 12L) {
  stop("simulated network failure")
}
hits <- tryCatch(
  search_ticker_choices("AAPL", market = "US"),
  error = function(e) structure(character(0), boom = conditionMessage(e))
)
check("search_ticker_choices survives remote throw", !isTRUE(!is.null(attr(hits, "boom"))))
check(
  "search_ticker_choices marks typeahead_network_error",
  isTRUE(attr(hits, "typeahead_network_error"))
)
check(
  "search_ticker_choices still returns local/fallback hits",
  is.character(hits) && length(hits) >= 1L
)

# Screener / Ranking stay button-driven (no keystroke API).
scr <- paste(readLines("stock_screener.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "Custom Screen runs on scr_run button not keystroke",
  grepl("observeEvent\\(input\\$scr_run", scr, perl = TRUE) &&
    !grepl("shiny::debounce\\(\\s*reactive\\(\\{?\\s*input\\$scr_", scr, perl = TRUE)
)
check(
  "Search button (not typing) fires ticker crawl",
  grepl("observeEvent\\(input\\$search,", srv, perl = TRUE) &&
    grepl("Search button / submit only", srv, fixed = TRUE)
)

# Lite-free: typeahead + Radar focus remain in shipped UI (no full-only gate).
check(
  "Lite keeps sc + lab_cluster_focus typeahead",
  grepl("bindTypeahead('sc')", ui, fixed = TRUE) &&
    grepl("bindTypeahead('lab_cluster_focus')", ui, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("All search debounce checks passed.\n")
