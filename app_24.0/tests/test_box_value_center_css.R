# Contract: KPI by Sheet / Crossover KPIs centered; values >> labels; mobile header 1:1:1
# Run: cd app_24.0 && Rscript tests/test_box_value_center_css.R
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
# Only KPI grids are H/V-centered
check("kpi-grid center comment", grepl("KPI by Sheet / Crossover KPIs only", ui, fixed = TRUE))
check("kpi-grid inner flex center", grepl(".ynow-kpi-grid .small-box .inner", ui, fixed = TRUE) &&
        grepl("justify-content: center !important", ui, fixed = TRUE) &&
        grepl("align-items: center !important", ui, fixed = TRUE))
check("kpi-grid value >> label", grepl("clamp(18px, 3.4vw, 28px)", ui, fixed = TRUE) &&
        grepl("clamp(9px, 1.2vw, 11px)", ui, fixed = TRUE))
# Global boxes: no forced center on .inner
check("global small-box inner not text-align center", {
  m <- regexpr("Global boxes:[^\n]*\n\\s*\\.content-wrapper \\.small-box \\.inner \\{[^}]+\\}", ui, perl = TRUE)
  block <- if (m[1] > 0) regmatches(ui, m) else ""
  grepl("padding-right: 44px", block, fixed = TRUE) && !grepl("text-align: center", block, fixed = TRUE)
})
check("global value >> label clamps", grepl("clamp(18px, 2.6vw + 0.45rem, 36px)", ui, fixed = TRUE) &&
        grepl("clamp(10px, 1.1vw, 12px)", ui, fixed = TRUE))
# Mobile header KPI 1:1:1
check("header kpi cols col-xs-4", grepl('class = "ynow-header-kpi-row"', ui, fixed = TRUE) &&
        length(gregexpr('class = "col-xs-4"', ui, fixed = TRUE)[[1]]) >= 3L)
check("header kpi mobile 1:1:1", grepl("LAST PRICE / MARKET CAP / EPS (TTM) 同列 1:1:1", ui, fixed = TRUE) &&
        grepl('ynow-header-kpi-row > [class*="col-"]', ui, fixed = TRUE) &&
        grepl("33.333%", ui, fixed = TRUE))
check("mobile full-width excludes header kpi row", grepl("row:not(.ynow-header-kpi-row)", ui, fixed = TRUE))
# Focus KPI alert bell: inner top-right of gold frame + thicker pulsing border
check("kpi-grid alertable bell inset", grepl(".ynow-kpi-grid .ynow-alertable .ynow-alert-bell", ui, fixed = TRUE) &&
        grepl("z-index: 5", ui, fixed = TRUE))
check("kpi-grid alertable col safety", grepl(".ynow-kpi-grid .ynow-alertable > [class*='col-']", ui, fixed = TRUE))
check("focus gold pulse animation", grepl("@keyframes ynow-focus-gold-pulse", ui, fixed = TRUE) &&
        grepl("border: 3px solid var(--ynow-gold", ui, fixed = TRUE))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
