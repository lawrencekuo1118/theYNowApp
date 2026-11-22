#!/usr/bin/env Rscript
# Sys. Admin / 系統管理員 locale strings (en-US + zh-TW).
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_sysadmin_locale.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

source(file.path(root, "ui_locale.R"), local = FALSE, encoding = "UTF-8")

en_link <- ui_str("test_link", "en")
zh_link <- ui_str("test_link", "zh-TW")
check("en test_link Sys. Admin", grepl("Sys. Admin", en_link, fixed = TRUE))
check("zh test_link 系統管理員", grepl("系統管理員", zh_link, fixed = TRUE))
check("zh test_link not English", !grepl("Sys. Admin", zh_link, fixed = TRUE))
check("zh testing_page_title", identical(ui_str("testing_page_title", "zh-TW"), "系統管理員"))
check("zh pts_sysadmin_title", identical(ui_str("pts_sysadmin_title", "zh-TW"), "系統管理員"))
check("no Mainland 一鍵 in zh test strings", !grepl("一鍵", zh_link, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
