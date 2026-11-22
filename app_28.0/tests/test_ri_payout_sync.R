#!/usr/bin/env Rscript
# RI payout sync must stay runnable on Yahoo-style CF (Cash Dividends Paid, no Preferred).
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ri_payout_sync.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

mk <- function(names, vals) {
  df <- data.frame(Breakdown = names, check.names = FALSE, stringsAsFactors = FALSE)
  df[["2024"]] <- vals
  df
}

# Integrity: common claim still NA without Preferred (unchanged for DDM).
yahoo_only <- mk("Cash Dividends Paid", -30)
check("common_cash_dividends Yahoo-only still NA", identical(common_cash_dividends(yahoo_only), NA_real_))

# RI payout helper: use total cash dividends.
check(
  "ri_cash_dividends_for_payout Yahoo-only uses total",
  isTRUE(abs(ri_cash_dividends_for_payout(yahoo_only) - 30) < 1e-9)
)

yahoo_zero <- mk("Cash Dividends Paid", 0)
check(
  "ri_cash_dividends_for_payout Cash=0 → 0",
  isTRUE(abs(ri_cash_dividends_for_payout(yahoo_zero) - 0) < 1e-9)
)

no_div_row <- mk(c("Operating Cash Flow", "Capital Expenditure"), c(10, -5))
check(
  "ri_cash_dividends_for_payout no dividend row → 0",
  isTRUE(abs(ri_cash_dividends_for_payout(no_div_row) - 0) < 1e-9)
)

check(
  "ri_cash_dividends_for_payout empty CF → NA",
  identical(ri_cash_dividends_for_payout(data.frame()), NA_real_)
)

with_pref <- mk(c("Cash Dividends Paid", "Preferred Stock Dividend Paid"), c(-30, -10))
check(
  "ri_cash_dividends_for_payout prefers common claim",
  isTRUE(abs(ri_cash_dividends_for_payout(with_pref) - 20) < 1e-9)
)

# Selection rule still promotes RI when Gordon-blocked + profitable (keep).
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
stmt <- function(rows, start_year = 2024L) {
  n <- max(lengths(rows))
  years <- as.character(start_year - seq_len(n) + 1L)
  df <- data.frame(Metric = names(rows), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(n)) {
    df[[years[i]]] <- vapply(rows, function(v) {
      if (length(v) >= i && is.finite(v[i])) v[i] else NA_real_
    }, numeric(1))
  }
  df
}
is <- stmt(list(
  "Net Income Common Stockholders" = c(80, 70),
  "Total Revenue" = c(400, 350),
  "Interest Expense" = c(5, 5),
  "Pretax Income" = c(100, 90),
  "Income Tax Expense" = c(20, 18)
))
bs <- stmt(list(
  "Common Stock Equity" = c(500, 480),
  "Total Debt" = c(80, 80),
  "Total Assets" = c(900, 860),
  "Goodwill" = c(10, 10),
  "Ordinary Shares Number" = c(100, 100)
))
cf <- stmt(list(
  "Operating Cash Flow" = c(20, 30),
  "Capital Expenditure" = c(-80, -70),
  "Free Cash Flow" = c(-60, -40),
  "Cash Dividends Paid" = c(0, 0)
))
rec <- recommend_valuation_models(cf, "Electronic Components", is, bs, industry_choice = "")
check("neg FCFF + NI>0 still RI primary (keep rule)", identical(rec$primary, "ri"))
check("company_type ni_pos_fcff_neg", identical(rec$company_type, "ni_pos_fcff_neg"))
ni <- common_net_income(is)
clip <- function(x, lo, hi) max(lo, min(hi, x))
payout <- clip((ri_cash_dividends_for_payout(cf) / ni) * 100, 0, 100)
check("7772-like CF yields finite payout for RI UI", is.finite(payout) && payout == 0)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("PASS: test_ri_payout_sync\n")
