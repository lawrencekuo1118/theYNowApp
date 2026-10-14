# Recent Search chips: click restores ticker + jumps to Company / 個股
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "ynow_server.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("uiOutput recentsearch", grepl('uiOutput\\(\"recentsearch\"\\)', ui, perl = TRUE))
check("textOutput recentsearch removed", !grepl('textOutput\\(\"recentsearch\"\\)', ui, perl = TRUE))
check("chip CSS class", grepl("ynow-recent-chip", ui, fixed = TRUE))
check("JS sets recent_search_pick", grepl("recent_search_pick", ui, fixed = TRUE))
check("server observes recent_search_pick", grepl("observeEvent\\(input\\$recent_search_pick", srv, perl = TRUE))
check("shared fire helper", grepl("\\.ynow_fire_ticker_search", srv, perl = TRUE))
check("goto dashboard on recent", grepl('goto_tab = \"dashboard\"', srv, fixed = TRUE))
check("pending cross-market fetch", grepl("pending_recent_fetch", srv, fixed = TRUE))
check("en recent_search_aria", grepl("recent_search_aria", loc, fixed = TRUE))
check("zh recent_search_empty", grepl("尚無最近搜尋代號", loc, fixed = TRUE))
check(
  "reorder recent on search",
  grepl("setdiff\\(values\\$recentsearch, stock_code\\)", srv, perl = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
