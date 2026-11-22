#!/usr/bin/env Rscript
# Company overview: locale keys (en-US + zh-TW). Offline; no source greps.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    message("PASS: ", label)
  } else {
    fail <<- fail + 1L
    message("FAIL: ", label)
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_company_overview.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(root, "ui_locale.R"), local = TRUE)

keys <- c(
  "company_overview_title",
  "company_overview_waiting",
  "company_overview_source",
  "company_overview_sector",
  "company_overview_industry",
  "company_overview_hq",
  "company_overview_exchange",
  "company_overview_employees",
  "company_overview_website",
  "company_overview_summary_empty",
  "company_overview_show_more",
  "company_overview_show_less"
)

has_cjk <- function(s) grepl("[\u4e00-\u9fff]", s, perl = TRUE)
# Sector / Industry stay formal English in both locales (GICS labels).
eng_finance_keys <- c("company_overview_sector", "company_overview_industry")

for (k in keys) {
  en <- ui_str(k, "en")
  zh <- ui_str(k, "zh-TW")
  check(paste("en", k), nzchar(en) && !has_cjk(en))
  if (k %in% eng_finance_keys) {
    check(paste("zh", k, "eng-term"), nzchar(zh) && !has_cjk(zh))
  } else {
    check(paste("zh", k), nzchar(zh) && has_cjk(zh))
  }
}

check(
  "title en Company overview",
  identical(ui_str("company_overview_title", "en"), "Company overview")
)
check(
  "title zh 公司概況",
  identical(ui_str("company_overview_title", "zh-TW"), "公司概況")
)

if (fail > 0L) {
  quit(status = 1L)
}
message("ALL PASS: test_company_overview.R")
