# Sys. Admin / 系統管理員 locale wiring
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "ui_locale.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

source(file.path(root, "ui_locale.R"), local = FALSE, encoding = "UTF-8")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

en_link <- ui_str("test_link", "en")
zh_link <- ui_str("test_link", "zh-TW")
check("en test_link Sys. Admin", grepl("Sys. Admin", en_link, fixed = TRUE))
check("zh test_link 系統管理員", grepl("系統管理員", zh_link, fixed = TRUE))
check("zh test_link not English", !grepl("Sys. Admin", zh_link, fixed = TRUE))
check("zh testing_page_title", identical(ui_str("testing_page_title", "zh-TW"), "系統管理員"))
check("zh pts_sysadmin_title", identical(ui_str("pts_sysadmin_title", "zh-TW"), "系統管理員"))
check("applyUiLocale wires test_link", grepl("ynow_test_link_label", ui, fixed = TRUE) &&
  grepl("s.test_link", ui, fixed = TRUE))
check("no Mainland 一鍵 in zh test strings", !grepl("一鍵", zh_link, fixed = TRUE))
# Foot label first-paint defaults are English; locale push switches.
check("foot feedback default English first paint", grepl(
  'ynow_feedback_link_label", " Feedback"', ui, fixed = TRUE
) || grepl("ynow_feedback_link_label\", \" Feedback\"", ui, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
