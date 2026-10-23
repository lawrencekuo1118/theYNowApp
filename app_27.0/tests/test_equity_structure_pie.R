# Equity structure pie slices (share nature). No network.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_equity_structure_pie.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
setwd(app_dir)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("setup.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

# Minimal BS helper (Metric + one stock column)
.bs <- function(...) {
  rows <- list(...)
  data.frame(
    Breakdown = names(rows),
    `2024-12-31` = unlist(rows, use.names = FALSE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
.sum <- function(...) {
  rows <- list(...)
  data.frame(
    Item = names(rows),
    Value = as.character(unlist(rows, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}

# Plain common-only → no pie
plain <- equity_structure_slices(
  .bs(
    "Ordinary Shares Number" = 1e9,
    "Common Stock Equity" = 5e10,
    "Total Assets" = 1e11
  ),
  .sum("Shares Outstanding" = 1e9, "Implied Shares Outstanding" = 1e9),
  ticker = "AAPL"
)
check("plain single-class hidden", is.null(plain))

# Preferred vs Ordinary share counts
pref_n <- equity_structure_slices(
  .bs(
    "Ordinary Shares Number" = 3e9,
    "Preferred Shares Number" = 2.5e8,
    "Common Stock Equity" = 1e11,
    "Preferred Stock Equity" = 1e10
  ),
  ticker = "JPM",
  locale = "en"
)
check("preferred share-count path", is.data.frame(pref_n) && nrow(pref_n) == 2L)
check("preferred path unit shares", identical(pref_n$unit[[1]], "shares"))
check("preferred path labels en", any(grepl("Ordinary", pref_n$label)) && any(grepl("Preferred", pref_n$label)))

pref_zh <- equity_structure_slices(
  .bs(
    "Ordinary Shares Number" = 3e9,
    "Preferred Shares Number" = 2.5e8
  ),
  ticker = "JPM",
  locale = "zh-TW"
)
check("preferred labels zh-TW", any(grepl("普通股", pref_zh$label)) && any(grepl("特別股", pref_zh$label)))

# Dual-class via listed vs implied
dual <- equity_structure_slices(
  .bs("Ordinary Shares Number" = 12e9, "Common Stock Equity" = 4e11),
  .sum("Shares Outstanding" = 5.8e9, "Implied Shares Outstanding" = 12.2e9),
  ticker = "GOOGL",
  locale = "en"
)
check("dual-class GOOGL shown", is.data.frame(dual) && nrow(dual) == 2L)
check("dual-class GOOGL voting label", any(grepl("Class A \\(voting\\)", dual$label)))
check("dual-class values sum to implied", abs(sum(dual$value) - 12.2e9) < 1)

# Book preferred path when share counts missing
book <- equity_structure_slices(
  .bs(
    "Common Stock Equity" = 37e9,
    "Preferred Stock Equity" = 745e6,
    "Minority Interest" = 0
  ),
  ticker = "V",
  locale = "en"
)
check("book preferred path", is.data.frame(book) && nrow(book) == 2L)
check("book path unit currency", identical(book$unit[[1]], "currency"))

# UI wiring
ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI uses bs_equity_charts_ui", grepl("bs_equity_charts_ui", ui_txt, fixed = TRUE))
check("server has eq_structure_plot", grepl("eq_structure_plot", srv_txt, fixed = TRUE))
check("scraper exposes share counts", {
  py <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("Implied Shares Outstanding", py, fixed = TRUE) &&
    grepl("Shares Outstanding", py, fixed = TRUE)
})

en_keys <- names(.UI_STRINGS$en)
zh_keys <- names(.UI_STRINGS$`zh-TW`)
need <- c("eq_pie_title", "eq_pie_ordinary_voting", "eq_pie_preferred_limited", "bs_pie_title")
check("i18n keys en+zh", all(need %in% en_keys) && all(need %in% zh_keys))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
