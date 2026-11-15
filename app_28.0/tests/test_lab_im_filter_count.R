#!/usr/bin/env Rscript
# Live Blue Chip filter-pool count (industry × model × ADR)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

catlg <- data.frame(
  ticker = c("AAPL", "TSM", "MSFT", "JPM", "SKHY"),
  industry_key = c("tech.Hardware", "sc.Foundry", "tech.Software", "fin.Banks", "sc.Memory"),
  primary = c("dcf", "dcf", "dcf", "ri", "dcf"),
  secondary = c("multiples", "multiples", "multiples", "pb", "multiples"),
  is_adr = c(FALSE, TRUE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

n_all <- lab_count_filter_pool_tickers(catlg, market_mode = "US", include_adr = TRUE)
check("all filters empty → 5", identical(n_all, 5L))

n_no_adr <- lab_count_filter_pool_tickers(
  catlg, market_mode = "US", include_adr = FALSE
)
check("exclude ADR → 3", identical(n_no_adr, 3L))

n_dcf <- lab_count_filter_pool_tickers(
  catlg, method_filter = "dcf", market_mode = "US", include_adr = TRUE
)
check("method dcf → 4", identical(n_dcf, 4L))

n_sc <- lab_count_filter_pool_tickers(
  catlg,
  industry_filter = c("sc.Foundry", "sc.Memory"),
  market_mode = "US",
  include_adr = TRUE
)
check("industry semiconductor → 2", identical(n_sc, 2L))

n_combo <- lab_count_filter_pool_tickers(
  catlg,
  method_filter = "dcf",
  industry_filter = c("sc.Foundry", "sc.Memory"),
  include_adr = FALSE,
  market_mode = "US"
)
check("dcf + sc − ADR → 0", identical(n_combo, 0L))

n_tw <- lab_count_filter_pool_tickers(
  catlg, market_mode = "TW", include_adr = FALSE
)
check("TW ignores ADR flag → 5", identical(n_tw, 5L))

check("empty catalog → 0", identical(lab_count_filter_pool_tickers(NULL), 0L))
check(
  "empty df → 0",
  identical(lab_count_filter_pool_tickers(catlg[0, , drop = FALSE]), 0L)
)

# Locale + UI wiring
for (loc in c("en", "zh-TW")) {
  check(
    paste0("locale has filter_count keys (", loc, ")"),
    nzchar(ui_str("lab_im_filter_count_label", loc)) &&
      nzchar(ui_str("lab_im_filter_count_unit", loc)) &&
      nzchar(ui_str("lab_im_filter_count_hint", loc)) &&
      grepl("{n}", ui_str("lab_im_filter_count_qualified", loc), fixed = TRUE)
  )
}
check(
  "zh-TW label uses 檔 wording",
  identical(ui_str("lab_im_filter_count_unit", "zh-TW"), "檔")
)
check(
  "en unit is names",
  identical(ui_str("lab_im_filter_count_unit", "en"), "names")
)

ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "UI mounts lab_im_filter_count",
  grepl('uiOutput("lab_im_filter_count")', ui_txt, fixed = TRUE) &&
    grepl("ynow-lab-im-filter-count", ui_txt, fixed = TRUE)
)
check(
  "server renders live count",
  grepl("output\\$lab_im_filter_count", srv_txt) &&
    grepl("lab_count_filter_pool_tickers", srv_txt, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("All filter-count checks passed.\n")
