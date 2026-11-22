#!/usr/bin/env Rscript
# g ≥ WACC gate: locale copy for discount-rate guide toasts (offline).
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
  normalizePath("test_g_wacc_guide.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(root, "ui_locale.R"), local = TRUE)

has_cjk <- function(s) grepl("[\u4e00-\u9fff]", s, perl = TRUE)

for (k in c(
  "notif_g_ge_wacc", "notif_g2_ge_wacc2", "notif_fcfe_g_ge_ke",
  "notif_g_discount_guide_menu"
)) {
  en <- ui_str(k, "en")
  zh <- ui_str(k, "zh-TW")
  check(paste("en", k), nzchar(en) && !has_cjk(en))
  check(paste("zh", k), nzchar(zh) && has_cjk(zh))
}

check(
  "en toast mentions Model Dashboard",
  grepl("Model Dashboard", ui_str("notif_g_ge_wacc", "en"), fixed = TRUE)
)
check(
  "zh toast mentions Model Dashboard",
  grepl("Model Dashboard", ui_str("notif_g_ge_wacc", "zh-TW"), fixed = TRUE)
)
check(
  "en toast mentions sidebar",
  grepl("sidebar", ui_str("notif_g_ge_wacc", "en"), ignore.case = TRUE)
)
check(
  "zh toast mentions 側邊欄",
  grepl("側邊欄", ui_str("notif_g_ge_wacc", "zh-TW"), fixed = TRUE)
)

if (fail > 0L) quit(status = 1L)
message("ALL PASS: test_g_wacc_guide.R")
