# Contract: box numerals text-centered + fluid clamp (文字／數值縮放；不改框格尺寸)
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
# Text/value scaling only — must not reintroduce box-dimension overrides
check("no global small-box flex resize", !grepl("Global valueBox: numeral", ui, fixed = TRUE))
check("no info-box min-height 90", !grepl("min-height: 90px", ui, fixed = TRUE))
check("eq-pill has no added min-height", {
  m <- regexpr("\\.ynow-eq-score-pill \\{[^}]+\\}", ui, perl = TRUE)
  block <- if (m[1] > 0) regmatches(ui, m) else ""
  !grepl("min-height", block, fixed = TRUE)
})
check("small-box keeps padding-right 44", grepl("padding-right: 44px", ui, fixed = TRUE))
check("small-box h3 clamp + center", grepl("clamp(14px, 2.2vw + 0.25rem, 32px)", ui, fixed = TRUE) &&
        grepl(".content-wrapper .small-box .inner h3", ui, fixed = TRUE) &&
        grepl("text-align: center !important", ui, fixed = TRUE))
check("info-box number clamp + center", grepl("clamp(14px, 2vw + 0.25rem, 28px)", ui, fixed = TRUE) &&
        grepl(".content-wrapper .info-box .info-box-number", ui, fixed = TRUE))
check("smart-card value clamp", grepl("ynow-smart-card-value", ui, fixed = TRUE) &&
        grepl("clamp(16px, 3.2vw, 26px)", ui, fixed = TRUE))
check("fs-value clamp", grepl("clamp(13px, 2.6vw, 17px)", ui, fixed = TRUE))
check("macro kpi value clamp", grepl("clamp(18px, 4.2vw, 28px)", ui, fixed = TRUE))
check("mscore value clamp", grepl("clamp(22px, 5vw, 34px)", ui, fixed = TRUE))
check("eq score num clamp", grepl("clamp(24px, 5.5vw, 36px)", ui, fixed = TRUE))
check("metric-card value center", grepl(".ynow-metric-card__value", ui, fixed = TRUE) &&
        grepl("text-align: center", ui, fixed = TRUE))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
