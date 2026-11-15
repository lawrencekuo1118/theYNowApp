#!/usr/bin/env Rscript
# Piotroski liquidity point: current ratio up, Yahoo HTML and yfinance row names.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

stmt <- function(rows) {
  df <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  df
}

is_df <- stmt(list(
  Breakdown = "Total Revenue",
  `12/31/2024` = "100",
  `12/31/2023` = "90"
))
cf_df <- stmt(list(
  Breakdown = "Operating Cash Flow",
  `12/31/2024` = "10",
  `12/31/2023` = "8"
))

liq_pass <- function(fs) {
  identical(as.character(fs$checklist[6, 2]), "通過")
}

# yfinance names. Other Current Assets is listed first and falls, so an
# unanchored grep("Current Assets") would score the component instead.
bs_yf <- data.frame(
  Breakdown = c("Other Current Assets", "Current Assets", "Current Liabilities", "Total Assets"),
  `12/31/2024` = c("5", "80", "40", "200"),
  `12/31/2023` = c("20", "40", "40", "180"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fs_yf <- compute_report_f_score(is_df, bs_yf, cf_df)
check("yfinance Current Assets ratio up passes", liq_pass(fs_yf))

bs_html <- data.frame(
  Breakdown = c("Total Current Assets", "Total Current Liabilities", "Total Assets"),
  `12/31/2024` = c("80", "40", "200"),
  `12/31/2023` = c("40", "40", "180"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fs_html <- compute_report_f_score(is_df, bs_html, cf_df)
check("Total Current * ratio up passes", liq_pass(fs_html))

bs_down <- bs_yf
bs_down$`12/31/2024`[bs_down$Breakdown == "Current Assets"] <- "20"
fs_down <- compute_report_f_score(is_df, bs_down, cf_df)
check("current ratio down fails", identical(as.character(fs_down$checklist[6, 2]), "未達標"))

bs_other <- data.frame(
  Breakdown = c("Other Current Assets", "Other Current Liabilities", "Total Assets"),
  `12/31/2024` = c("80", "20", "200"),
  `12/31/2023` = c("40", "40", "180"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fs_other <- compute_report_f_score(is_df, bs_other, cf_df)
check("Other Current * is not the current ratio", identical(as.character(fs_other$checklist[6, 2]), "未達標"))

# Both names present: Total* is the aggregate used for the ratio.
bs_both <- data.frame(
  Breakdown = c("Current Assets", "Total Current Assets", "Current Liabilities", "Total Current Liabilities", "Total Assets"),
  `12/31/2024` = c("10", "80", "40", "40", "200"),
  `12/31/2023` = c("40", "40", "40", "40", "180"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
fs_both <- compute_report_f_score(is_df, bs_both, cf_df)
check("Total Current * wins when both labels exist", liq_pass(fs_both))

if (fail > 0L) {
  cat("\n", fail, " failed\n", sep = "")
  quit(status = 1)
}
cat("\nAll liquidity F-Score checks passed.\n")
