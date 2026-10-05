#!/usr/bin/env Rscript
# Earnings quality + Beneish M-Score + risk matrix (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "earnings_quality_module.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

mk_stmt <- function(...) {
  rows <- list(...)
  nms <- names(rows)
  n <- max(vapply(rows, length, integer(1)))
  df <- data.frame(Metric = nms, check.names = FALSE, stringsAsFactors = FALSE)
  yrs <- paste0("Y", seq_len(n))
  for (j in seq_len(n)) {
    df[[yrs[j]]] <- vapply(rows, function(v) {
      if (length(v) >= j) as.numeric(v[j]) else NA_real_
    }, numeric(1))
  }
  df
}

empty <- evaluate_earnings_quality(NULL, NULL, NULL)
check("empty not ok", identical(empty$ok, FALSE))
check("empty qoe NA", !is.finite(empty$qoe_score))

# Healthy: OCF > NI, AR tracks revenue, moderate growth
is_ok <- mk_stmt(
  `Total Revenue` = c(100, 95, 90),
  `Gross Profit` = c(40, 38, 36),
  `Selling General And Administration` = c(12, 11.5, 11),
  `Net Income` = c(12, 11, 10),
  `Operating Income` = c(18, 17, 16)
)
bs_ok <- mk_stmt(
  `Accounts Receivable` = c(12, 11.5, 11),
  `Current Assets` = c(30, 29, 28),
  `Net PPE` = c(20, 19.5, 19),
  `Total Assets` = c(80, 78, 76),
  `Total Debt` = c(10, 10, 10),
  `Current Liabilities` = c(15, 14.5, 14),
  `Long Term Debt` = c(8, 8, 8),
  `Ordinary Shares Number` = c(10, 10, 10)
)
cf_ok <- mk_stmt(
  `Operating Cash Flow` = c(16, 15, 14),
  `Depreciation And Amortization` = c(4, 3.9, 3.8)
)

fs_ok <- compute_report_f_score(is_ok, bs_ok, cf_ok)
pack_ok <- evaluate_earnings_quality(is_ok, bs_ok, cf_ok, f_score_pack = fs_ok)
check("healthy ok", isTRUE(pack_ok$ok))
check("healthy m_score finite", is.finite(pack_ok$m_score))
check("healthy m not alert", !identical(pack_ok$m_flag, "警示"))
check("healthy qoe finite", is.finite(pack_ok$qoe_score))
check("healthy qoe grade letter", grepl("^[A-F]$", pack_ok$qoe_grade))
check("healthy risk rows 5", nrow(pack_ok$risk_rows) == 5L)
check("healthy ar-rev not alert", !identical(pack_ok$ar_rev_flag, "警示"))
check("healthy composite not 高", !identical(pack_ok$risk_level, "高"))

# Manipulative-ish: AR surges vs revenue, NI >> OCF, high accruals
is_bad <- mk_stmt(
  `Total Revenue` = c(110, 100, 95),
  `Gross Profit` = c(55, 40, 38),
  `Selling General And Administration` = c(8, 12, 11),
  `Net Income` = c(30, 12, 11),
  `Operating Income` = c(28, 14, 13)
)
bs_bad <- mk_stmt(
  `Accounts Receivable` = c(40, 12, 11),
  `Current Assets` = c(70, 30, 28),
  `Net PPE` = c(15, 20, 19),
  `Total Assets` = c(120, 80, 76),
  `Total Debt` = c(50, 10, 10),
  `Current Liabilities` = c(25, 15, 14),
  `Long Term Debt` = c(40, 8, 8),
  `Ordinary Shares Number` = c(12, 10, 10)
)
cf_bad <- mk_stmt(
  `Operating Cash Flow` = c(2, 14, 13),
  `Depreciation And Amortization` = c(2, 4, 3.8)
)
fs_bad <- compute_report_f_score(is_bad, bs_bad, cf_bad)
pack_bad <- evaluate_earnings_quality(is_bad, bs_bad, cf_bad, f_score_pack = fs_bad)
check("bad ok", isTRUE(pack_bad$ok))
check("bad ar-rev alert", identical(pack_bad$ar_rev_flag, "警示"))
check("bad ar_rev_pp large", is.finite(pack_bad$ar_rev_pp) && pack_bad$ar_rev_pp >= 15)
check("bad accruals positive", is.finite(pack_bad$accruals_ratio) && pack_bad$accruals_ratio > 0)
check("bad has red flags", length(pack_bad$red_flags) >= 1L)
check("bad risk high or medium", pack_bad$risk_level %in% c("高", "中"))
check("bad qoe below healthy", is.finite(pack_bad$qoe_score) && pack_bad$qoe_score < pack_ok$qoe_score)

# Beneish components present
check("DSRI in components", !is.null(pack_ok$m_components$DSRI))
check("TATA in components", !is.null(pack_ok$m_components$TATA))

# Warnings collector
warns <- collect_earnings_quality_warnings(is_bad, bs_bad, cf_bad, f_score_pack = fs_bad)
check("collector returns flags", length(warns) >= 1L)

# UI smoke
suppressPackageStartupMessages(library(shiny))
ui_ok <- as.character(earnings_quality_dashboard_ui(pack_ok, locale = "zh-TW"))
check("dashboard has QoE title", grepl("盈餘品質", ui_ok, fixed = TRUE))
check("dashboard has score", grepl(as.character(pack_ok$qoe_score), ui_ok, fixed = TRUE))
ui_en <- as.character(earnings_quality_dashboard_ui(pack_ok, locale = "en"))
check("dashboard en title", grepl("Quality of Earnings", ui_en, fixed = TRUE))
mx <- as.character(earnings_risk_matrix_ui(pack_bad, locale = "zh-TW"))
check("matrix title", grepl("自動化風險矩陣", mx, fixed = TRUE))
check("matrix has Beneish", grepl("Beneish", mx, fixed = TRUE))
check("matrix red flags block", grepl("紅旗警示", mx, fixed = TRUE))

# Locale helpers
check("flag localize en", identical(.eq_localize_flag("警示", "en"), "Alert"))
check("metric localize zh AR", grepl("應收", .eq_localize_metric("Production–Valuation Divergence", "zh-TW")))

# Wired into decision module + global
dec <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE), collapse = "\n")
check("decision has qoe_dashboard", grepl("qoe_dashboard", dec, fixed = TRUE))
check("decision has risk_matrix_panel", grepl("risk_matrix_panel", dec, fixed = TRUE))
glob <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE), collapse = "\n")
check("global sources earnings_quality_module", grepl("earnings_quality_module.R", glob, fixed = TRUE))

# collect_fraud_warnings merges EQ flags
msgs <- collect_fraud_warnings(cf_bad, is_bad, bs_bad)
check("fraud warnings include EQ or shen", length(msgs) >= 1L)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
