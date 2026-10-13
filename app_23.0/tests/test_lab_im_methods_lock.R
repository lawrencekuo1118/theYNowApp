#!/usr/bin/env Rscript
# Models filter locks methods with no primary; summary table lists all seven.
# Run: cd app_21.0/tests && Rscript test_lab_im_methods_lock.R

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
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("JS lock handler", grepl("ynowLabImMethodsLock", ui, fixed = TRUE))
check("locked CSS class", grepl("ynow-lab-im-method--locked", ui, fixed = TRUE))
check("server sends lock message", grepl('ynowLabImMethodsLock', srv, fixed = TRUE))
check("server uses include_all summary", grepl("include_all = TRUE", srv, fixed = TRUE))
check("server scope catalog ignores method filter",
      grepl("\\.lab_im_method_scope_catalog", srv) &&
        grepl("method_filter = character\\(0\\)", srv))
check("methods hint locale en",
      nzchar(ui_str("lab_im_methods_locked_hint", "en")) &&
        grepl("Locked|locked|Listed", ui_str("lab_im_methods_locked_hint", "en")))
check("methods hint locale zh-TW",
      grepl("主模型", ui_str("lab_im_methods_locked_hint", "zh-TW"), fixed = TRUE) &&
        !grepl("默认|参数|数据", ui_str("lab_im_methods_locked_hint", "zh-TW")))
check("hint wired in UI", grepl("ynow_lab_im_methods_locked_hint", ui, fixed = TRUE))
check("applyUiLocale wires hint",
      grepl("ynow_lab_im_methods_locked_hint", ui, fixed = TRUE) &&
        grepl("lab_im_methods_locked_hint", ui, fixed = TRUE))
check("abs/rel group labels in UI",
      grepl("ynow_lab_im_methods_abs_label", ui, fixed = TRUE) &&
        grepl("ynow_lab_im_methods_rel_label", ui, fixed = TRUE) &&
        grepl("ynow-lab-im-methods-group--abs", ui, fixed = TRUE) &&
        grepl("ynow-lab-im-methods-group--rel", ui, fixed = TRUE))
check("abs/rel locale keys",
      identical(ui_str("lab_im_methods_abs_label", "en"), "Absolute") &&
        identical(ui_str("lab_im_methods_rel_label", "en"), "Relative") &&
        identical(ui_str("lab_im_methods_abs_label", "zh-TW"), "絕對") &&
        identical(ui_str("lab_im_methods_rel_label", "zh-TW"), "相對"))
check("applyUiLocale wires abs/rel labels",
      grepl("lab_im_methods_abs_label", ui, fixed = TRUE) &&
        grepl("lab_im_methods_rel_label", ui, fixed = TRUE))
check("abs/rel taxonomy keys",
      identical(LAB_METHOD_ABS_KEYS, c("nav", "pb", "dcf", "ddm", "ri", "sotp")) &&
        identical(LAB_METHOD_REL_KEYS, c("enterprise", "earnings", "revenue", "book")) &&
        identical(
          sort(c(LAB_METHOD_ABS_KEYS, LAB_METHOD_REL_KEYS)),
          sort(as.character(LAB_SIDEBAR_METHOD_ORDER))
        ))

# Runtime: US catalog → RI/SOTP not primary → locked; Justified P/B absolute + Book multiples relative.
catlg <- lab_build_industry_method_catalog(market_mode = "US", boards = c("TWSE", "TPEX"))
avail <- lab_im_available_primary_methods(catlg)
sm <- lab_method_group_summary(catlg, include_all = TRUE)
check("US summary matches sidebar order", nrow(sm) == length(LAB_SIDEBAR_METHOD_ORDER) &&
        identical(sm$method_key, LAB_SIDEBAR_METHOD_ORDER))
check("US available keeps Justified P/B absolute and Book multiples relative",
      all(c("dcf", "pb", "book") %in% avail) &&
        all(c("enterprise", "earnings", "revenue") %in% avail) &&
        # RI / SOTP unlock via secondary Fair Value slots (not primary counts)
        all(c("ri", "sotp") %in% avail))
check("US table zeros for locked primary methods",
      all(sm$候選檔數[sm$method_key %in% c("ri", "sotp")] == 0L) &&
        all(!sm$has_primary[sm$method_key %in% c("ri", "sotp")]))
check("US table positive for DCF and Justified P/B",
      isTRUE(sm$候選檔數[sm$method_key == "dcf"][1] > 0L) &&
        isTRUE(sm$候選檔數[sm$method_key == "pb"][1] > 0L) &&
        isTRUE(sm$候選檔數[sm$method_key == "book"][1] > 0L))

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_im_methods_lock\n")
