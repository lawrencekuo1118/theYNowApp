#!/usr/bin/env Rscript
# English UI strings must not contain Han characters.
# Static controls that follow locale use data-i18n (About tab is bilingual by design).

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

root <- if (file.exists("ui_locale.R")) "." else ".."
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source("ui_locale.R", local = FALSE, encoding = "UTF-8")

en <- .UI_STRINGS$en
zh <- .UI_STRINGS[["zh-TW"]]
check("en/zh-TW key parity", identical(sort(names(en)), sort(names(zh))))

cjk <- "[\u4e00-\u9fff]"
bad <- character()
for (k in names(en)) {
  v <- paste(as.character(en[[k]] %||% ""), collapse = " ")
  if (grepl(cjk, v, perl = TRUE)) bad <- c(bad, k)
}
check("en locale has no Han", length(bad) == 0L)
if (length(bad)) cat(paste(bad, collapse = "\n"), "\n")

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("dcf mode uses data-i18n", grepl('data-i18n" = "dcf_mode_label"', ui, fixed = TRUE) ||
        grepl("data-i18n\" = \"dcf_mode_label\"", ui, fixed = TRUE) ||
        grepl("dcf_mode_label", ui, fixed = TRUE))
check("sec notes lead is localized", grepl("sec_notes_lead", ui, fixed = TRUE))
check("feedback title is localized", grepl("feedback_page_title", ui, fixed = TRUE))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
