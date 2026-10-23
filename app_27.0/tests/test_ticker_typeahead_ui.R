#!/usr/bin/env Rscript
# Ticker / Stock Code typeahead UI contracts (no network).
# Run: cd app_27.0 && Rscript tests/test_ticker_typeahead_ui.R
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

check("bindTypeahead for sc", grepl("bindTypeahead('sc')", ui, fixed = TRUE))
check("sc_ticker_suggest_ui host", grepl('uiOutput("sc_ticker_suggest_ui")', ui, fixed = TRUE))
check("shiny:value re-shows sc suggest", grepl("sc_ticker_suggest_ui", ui, fixed = TRUE) &&
        grepl("showSuggest('sc')", ui, fixed = TRUE))
check(
  "suggest stays warm when chrome hidden",
  grepl('outputOptions(output, "sc_ticker_suggest_ui", suspendWhenHidden = FALSE)', srv, fixed = TRUE)
)
check(
  "tab change retries showSuggest",
  grepl("showSuggestWhenReady", ui, fixed = TRUE) &&
    grepl("shiny:inputchanged", ui, fixed = TRUE) &&
    grepl("sidebar_tabs", ui, fixed = TRUE)
)
check(
  "stock chrome hidden on Macro/Home/Blue Chip",
  grepl("macro_market", ui, fixed = TRUE) &&
    grepl("ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("sidebar_tabs != 'home'", ui, fixed = TRUE) &&
    grepl("sidebar_tabs != 'macro_market'", ui, fixed = TRUE) &&
    grepl("sidebar_tabs != 'bluechip'", ui, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("All ticker typeahead UI checks passed.\n")
