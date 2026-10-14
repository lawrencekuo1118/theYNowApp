# Contract: Value Alert (到值通知) wiring for required KPI surfaces
# Run: cd app_23.0 && Rscript tests/test_alertable_coverage.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) cat("OK ", label, "\n", sep = "") else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}
readf <- function(rel) {
  paste(readLines(file.path(app_dir, rel), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
srv <- readf("ynow_server.R")
kpi <- readf("kpi_module.R")
dec <- readf("investment_decision_module.R")
eq <- readf("earnings_quality_module.R")
macro <- readf("macro_market_module.R")
htcdi <- readf("htcdi_module.R")
ui <- readf("ynow_ui.R")

check("EPS (TTM)", grepl('metric = "eps_ttm"', srv, fixed = TRUE))
check("Summary beta", grepl('metric = "summary_beta"', srv, fixed = TRUE))
check("Lite primary FV", grepl('metric = "fair_value"', srv, fixed = TRUE))
check("Lite secondary FV", grepl('metric = "fair_value_secondary"', srv, fixed = TRUE))
check("Gold focus KPIs (module)", grepl("fund_focus_", kpi, fixed = TRUE) &&
        grepl("ynow_alertable", kpi, fixed = TRUE) &&
        grepl("mark_focus = focus", kpi, fixed = TRUE))
check("Gold focus FS cards", grepl("fund_focus_", srv, fixed = TRUE) &&
        grepl("ynow-focus-metric-box", srv, fixed = TRUE))
check("YNOW F-Score", grepl('metric = "f_score"', dec, fixed = TRUE))
check("YNOW MOS", grepl('metric = "ynow_mos_pct"', dec, fixed = TRUE))
check("YNOW statement alerts", grepl('metric = "statement_alert_count"', dec, fixed = TRUE))
check("QoE Grade", grepl('metric = "qoe_grade"', eq, fixed = TRUE) &&
        grepl("grade_lte", eq, fixed = TRUE))
check("Beneish M-Score", grepl('metric = "beneish_m_score"', dec, fixed = TRUE))
check("Full composite Base/Secondary alertable", grepl("ynow-composite-band-card", dec, fixed = TRUE) &&
        grepl('metric = "fair_value"', dec, fixed = TRUE) &&
        grepl('metric = "fair_value_secondary"', dec, fixed = TRUE) &&
        grepl("below_fair", dec, fixed = TRUE))
check("Hamada unlevered beta_u", grepl('metric = "hamada_beta_u"', srv, fixed = TRUE) &&
        grepl("vbx_beta_unlever_firm", srv, fixed = TRUE) &&
        grepl('uiOutput("vbx_beta_unlever_firm"', ui, fixed = TRUE))
check("Macro index KPI alertable", grepl("\\.macro_wrap_alertable", macro) &&
        grepl('metric = "last_price"', macro, fixed = TRUE) &&
        grepl("index_kpi_row", macro, fixed = TRUE))
check("Macro Rf alertable", grepl('metric = "macro_rf_pct"', macro, fixed = TRUE) &&
        grepl("RF_US", macro, fixed = TRUE) &&
        grepl("RF_TW", macro, fixed = TRUE))
check("Macro YNOW/TYNOW alertable", grepl('metric = "macro_ynow_level"', macro, fixed = TRUE))
check("Macro HTCDI alertable", grepl('metric = "macro_htcdi"', htcdi, fixed = TRUE) &&
        grepl('ticker = "HTCDI"', htcdi, fixed = TRUE))

if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
