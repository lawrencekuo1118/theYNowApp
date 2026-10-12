# Contract: box numerals H/V-centered + fluid clamp (到值框格置中／縮放)
# Run: cd app_23.0 && Rscript tests/test_box_value_center_css.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) cat("OK ", label, "\n", sep = "") else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}
check("global small-box inner flex center", grepl("Global valueBox: numeral", ui, fixed = TRUE) &&
        grepl(".content-wrapper .small-box .inner", ui, fixed = TRUE) &&
        grepl("justify-content: center !important", ui, fixed = TRUE) &&
        grepl("align-items: center !important", ui, fixed = TRUE))
check("info-box content centered", grepl(".content-wrapper .info-box-content", ui, fixed = TRUE) &&
        grepl("text-align: center !important", ui, fixed = TRUE))
check("smart-card value clamp", grepl("ynow-smart-card-value", ui, fixed = TRUE) &&
        grepl("clamp(16px, 3.2vw, 26px)", ui, fixed = TRUE))
check("fs-card center", grepl(".ynow-fs-card", ui, fixed = TRUE) &&
        grepl("align-items: center", ui, fixed = TRUE))
check("macro kpi center", grepl(".ynow-macro-kpi", ui, fixed = TRUE) &&
        grepl("justify-content: center", ui, fixed = TRUE))
check("mscore value clamp", grepl("clamp(22px, 5vw, 34px)", ui, fixed = TRUE))
check("eq score num clamp", grepl("clamp(24px, 5.5vw, 36px)", ui, fixed = TRUE))
check("metric-card body center", grepl(".ynow-metric-card__body", ui, fixed = TRUE) &&
        grepl("justify-content: center", ui, fixed = TRUE))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
