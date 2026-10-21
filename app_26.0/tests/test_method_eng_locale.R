#!/usr/bin/env Rscript
# Valuation Methodology ENG: American English tables/paragraphs (no CJK when lang=en).
# Run: cd app_26.0/tests && Rscript test_method_eng_locale.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

has_cjk <- function(x) {
  grepl("[\u4e00-\u9fff]", paste(as.character(x), collapse = " "), perl = TRUE)
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)

for (k in c("method_section_title", "method_matrix_fv_note", "method_matrix_rel_note")) {
  check(paste("en", k), nzchar(ui_str(k, "en")) && !has_cjk(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")) && has_cjk(ui_str(k, "zh-TW")))
}
check("en title has no pipe Chinese", !grepl("評價", ui_str("method_section_title", "en"), fixed = TRUE))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("CSS hides zh when not zh*", grepl('html:not\\(\\[lang\\^="zh"\\]\\) \\.ynow-loc-zh', ui))
check("CSS hides en on zh*", grepl('html\\[lang\\^="zh"\\] \\.ynow-loc-en', ui))
check("EN FV table Comparison header", grepl("<th>Comparison</th>", ui, fixed = TRUE))
check("ZH FV table 對照項目", grepl("<th>對照項目</th>", ui, fixed = TRUE))
check("EN Multiples header", grepl("Multiples (market multiples)", ui, fixed = TRUE))
check("lead zh has ynow-loc-zh", grepl('id = "ynow_method_lead_zh"[\\s\\S]*ynow-loc-zh|ynow-loc-zh[\\s\\S]*id = "ynow_method_lead_zh"', ui, perl = TRUE))
check("lead en has ynow-loc-en", grepl("ynow_method_lead_en", ui, fixed = TRUE) && grepl("ynow-loc-en", ui, fixed = TRUE))
check("applyUiLocale wires title", grepl("method_section_title", ui, fixed = TRUE) &&
        grepl("ynow_method_section_title", ui, fixed = TRUE))
check("default title string is EN-only", grepl(
  'id = "ynow_method_section_title", "Valuation Methodology"',
  ui,
  fixed = TRUE
))
check(
  "section title default has no Chinese",
  !grepl(
    'id = "ynow_method_section_title"[^\n]*[\u4e00-\u9fff]',
    ui,
    perl = TRUE
  )
)

if (fail > 0L) {
  cat("FAILED ", fail, " / ", pass + fail, "\n", sep = "")
  quit(status = 1)
}
cat("All method_eng_locale checks passed (", pass, ").\n", sep = "")
