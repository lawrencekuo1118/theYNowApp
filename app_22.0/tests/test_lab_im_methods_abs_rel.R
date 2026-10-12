#!/usr/bin/env Rscript
# Models filter Absolute / Relative groups (Blue Chip 查詢條件).
# Run: cd app_22.0/tests && Rscript test_lab_im_methods_abs_rel.R

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

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

# Source only the helper region of ynow_ui would pull shiny UI; parse helpers via eval of defs.
# Instead: read UI source and assert structure + evaluate helpers with shiny tags stubbed if needed.
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("helper .lab_im_methods_filter_ui defined",
      grepl("\\.lab_im_methods_filter_ui\\s*<-\\s*function", ui))
check("UI uses helper not flat checkboxGroupInput for methods",
      grepl("\\.lab_im_methods_filter_ui\\(APP_DEFAULTS\\$lab_im_methods\\)", ui, fixed = FALSE) &&
        !grepl(
          "checkboxGroupInput\\(\\s*[\"']lab_im_methods[\"']",
          ui
        ))
check("group CSS classes present",
      grepl("ynow-lab-im-methods-group--abs", ui, fixed = TRUE) &&
        grepl("ynow-lab-im-methods-group--rel", ui, fixed = TRUE) &&
        grepl("ynow-lab-im-methods-groups", ui, fixed = TRUE))
check("locale applyUiLocale abs/rel",
      grepl("ynow_lab_im_methods_abs_label", ui, fixed = TRUE) &&
        grepl("lab_im_methods_abs_label", ui, fixed = TRUE) &&
        grepl("ynow_lab_im_methods_rel_label", ui, fixed = TRUE) &&
        grepl("lab_im_methods_rel_label", ui, fixed = TRUE))

check("en Absolute / Relative",
      identical(ui_str("lab_im_methods_abs_label", "en"), "Absolute") &&
        identical(ui_str("lab_im_methods_rel_label", "en"), "Relative") &&
        !grepl("[\u4e00-\u9fff]", ui_str("lab_im_methods_abs_label", "en")))
check("zh-TW 絕對 / 相對",
      identical(ui_str("lab_im_methods_abs_label", "zh-TW"), "絕對") &&
        identical(ui_str("lab_im_methods_rel_label", "zh-TW"), "相對") &&
        !grepl("默认|参数|数据", ui_str("lab_im_methods_abs_label", "zh-TW")))

check("taxonomy Absolute = NAV/DCF/DDM/RI/SOTP",
      identical(LAB_METHOD_ABS_KEYS, c("nav", "dcf", "ddm", "ri", "sotp")))
check("taxonomy Relative = Enterprise/Earnings/Revenue/Book",
      identical(LAB_METHOD_REL_KEYS, c("enterprise", "earnings", "revenue", "book")))
check("no overlap abs vs rel",
      length(intersect(LAB_METHOD_ABS_KEYS, LAB_METHOD_REL_KEYS)) == 0L)
check("partition covers sidebar order",
      identical(
        sort(c(LAB_METHOD_ABS_KEYS, LAB_METHOD_REL_KEYS)),
        sort(as.character(LAB_SIDEBAR_METHOD_ORDER))
      ))

# Filter hit logic still treats book as P/B primary and enterprise as rel family.
df <- data.frame(
  primary = c("dcf", "pb", "nav", "multiples"),
  secondary = c("ri", NA, "sotp", NA),
  rel_family = c("earnings", "book", NA, "enterprise"),
  stringsAsFactors = FALSE
)
check(
  "filter abs DCF still hits",
  identical(lab_method_filter_hits(df, "dcf"), c(TRUE, FALSE, FALSE, FALSE))
)
check(
  "filter rel enterprise still hits",
  identical(lab_method_filter_hits(df, "enterprise"), c(FALSE, FALSE, FALSE, TRUE))
)
check(
  "filter book still hits P/B primary",
  isTRUE(lab_method_filter_hits(df, "book")[2])
)

if (fail > 0L) {
  cat("FAILED ", fail, " check(s)\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_im_methods_abs_rel\n")
