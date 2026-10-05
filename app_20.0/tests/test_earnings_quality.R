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
check("LVGI in components", !is.null(pack_ok$m_components$LVGI))
check("contributions present", length(pack_ok$m_contributions) >= 1L)

# Public calculate_m_score API (wide t / t1 columns)
fin_wide <- data.frame(
  Sales_t = 100, Sales_t1 = 95,
  Receivables_t = 12, Receivables_t1 = 11.5,
  COGS_t = 60, COGS_t1 = 57,
  Current_Assets_t = 30, Current_Assets_t1 = 29,
  PPE_t = 20, PPE_t1 = 19.5,
  Total_Assets_t = 80, Total_Assets_t1 = 78,
  Depreciation_t = 4, Depreciation_t1 = 3.9,
  SGA_t = 12, SGA_t1 = 11.5,
  Current_Liabilities_t = 15, Current_Liabilities_t1 = 14.5,
  Long_Term_Debt_t = 8, Long_Term_Debt_t1 = 8,
  Net_Income_t = 12, OCF_t = 16,
  stringsAsFactors = FALSE
)
ms <- calculate_m_score(fin_wide)
check("calculate_m_score finite", is.finite(ms$M_Score[1]))
check("calculate_m_score not risk by default healthy", isFALSE(ms$Risk_Flag[1]))
check("calculate_m_score returns 8 comps", all(c("DSRI", "GMI", "AQI", "SGI", "DEPI", "SGAI", "LVGI", "TATA") %in% names(ms)))

# Growth-only SGI spike: high SGI, calm DSRI → nuance when M > −1.78
sgi_spike <- fin_wide
sgi_spike$Sales_t <- 220
sgi_spike$Receivables_t <- 24  # AR tracks sales → DSRI ~1
ms_sgi <- calculate_m_score(sgi_spike)
check("sgi spike can raise M", is.finite(ms_sgi$M_Score[1]) && ms_sgi$M_Score[1] > ms$M_Score[1])
if (isTRUE(ms_sgi$Risk_Flag[1])) {
  check("sgi nuance when alert + calm DSRI", isTRUE(ms_sgi$SGI_Nuance[1]))
} else {
  check("sgi nuance skipped when not alert", TRUE)
}

# LVGI uses CL + LTD
ctx_lv <- list(
  rev = c(100, 95), gp = c(40, 38), cogs = c(60, 57),
  sga = c(12, 11.5), ni = c(12, 11), ocf = c(16, 15),
  dep = c(4, 3.9), ar = c(12, 11.5), ca = c(30, 29),
  ppe = c(20, 19.5), assets = c(80, 78),
  cl = c(15, 14.5), ltd = c(8, 8), debt = c(10, 10)
)
comp_lv <- .eq_beneish_components(ctx_lv)
lev_t <- (15 + 8) / 80
lev_t1 <- (14.5 + 8) / 78
expect_lvgi <- lev_t / lev_t1
check("LVGI uses CL+LTD", is.finite(comp_lv$LVGI) && abs(comp_lv$LVGI - expect_lvgi) < 1e-6)

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

mw <- as.character(beneish_m_score_widget_ui(pack_ok, locale = "zh-TW"))
check("mscore widget title", grepl("Beneish M-Score", mw, fixed = TRUE))
check("mscore widget value", grepl(sprintf("%.2f", pack_ok$m_score), mw, fixed = TRUE))
check("mscore widget details", grepl("8 項指標細項", mw, fixed = TRUE))
mw_en <- as.character(beneish_m_score_widget_ui(pack_ok, locale = "en"))
check("mscore widget en", grepl("earnings-manipulation", mw_en, fixed = TRUE))

# Locale helpers
check("flag localize en", identical(.eq_localize_flag("警示", "en"), "Alert"))
check("metric localize zh AR", grepl("應收", .eq_localize_metric("Production–Valuation Divergence", "zh-TW")))

# Wired into decision module + global
dec <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE), collapse = "\n")
check("decision has qoe_dashboard", grepl("qoe_dashboard", dec, fixed = TRUE))
check("decision has risk_matrix_panel", grepl("risk_matrix_panel", dec, fixed = TRUE))
check("decision has mscore_widget", grepl("mscore_widget", dec, fixed = TRUE))
check("decision mounts beneish widget", grepl("beneish_m_score_widget_ui", dec, fixed = TRUE))
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
