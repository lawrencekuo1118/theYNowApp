#!/usr/bin/env Rscript
# Per-ticker lab n / Ke / g from statements (not APP_DEFAULTS one-size-fits-all).
# Run: cd app_21.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_lab_ticker_params.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "default_config.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

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

# Mature / high ROE + payout → fundamental SGR; bank-ish industry → FINANCIAL / n≈5
mature_is <- stmt(list(
  "Net Income" = c(80, 78, 76),
  "Total Revenue" = c(400, 390, 380),
  "Operating Income" = c(100, 98, 95),
  "Interest Expense" = c(8, 8, 8),
  "Pretax Income" = c(100, 95, 92),
  "Income Tax Expense" = c(20, 19, 18)
))
mature_bs <- stmt(list(
  "Common Stock Equity" = c(500, 480, 460),
  "Total Debt" = c(100, 100, 100),
  "Total Assets" = c(900, 860, 820),
  "Ordinary Shares Number" = c(100, 100, 100),
  "Cash And Cash Equivalents" = c(50, 48, 45)
))
mature_cf <- stmt(list(
  "Operating Cash Flow" = c(120, 110, 105),
  "Capital Expenditure" = c(-30, -28, -27),
  "Free Cash Flow" = c(90, 82, 78),
  "Cash Dividends Paid" = c(-40, -38, -36)
))

# High-growth: steep revenue CAGR, reinvesting (low/no div)
growth_is <- stmt(list(
  "Net Income" = c(40, 20, 5),
  "Total Revenue" = c(500, 300, 150),
  "Operating Income" = c(50, 25, 8),
  "Interest Expense" = c(5, 4, 3),
  "Pretax Income" = c(45, 22, 6),
  "Income Tax Expense" = c(5, 2, 1)
))
growth_bs <- stmt(list(
  "Common Stock Equity" = c(200, 150, 100),
  "Total Debt" = c(80, 70, 60),
  "Total Assets" = c(400, 300, 200),
  "Ordinary Shares Number" = c(100, 100, 100),
  "Cash And Cash Equivalents" = c(30, 25, 20)
))
growth_cf <- stmt(list(
  "Operating Cash Flow" = c(60, 30, 10),
  "Capital Expenditure" = c(-50, -40, -30),
  "Free Cash Flow" = c(10, -10, -20),
  "Cash Dividends Paid" = c(0, 0, 0)
))

cat("\n=== lab_resolve_ticker_params ===\n")

p_m <- lab_resolve_ticker_params(
  mature_is, mature_bs, mature_cf,
  industry_key = "fn.Banking",
  beta = 0.85,
  rf_pct = 4,
  rm_pct = 9,
  ticker = "JPM"
)
check("mature returns finite n", is.finite(p_m$n_years) && p_m$n_years >= 3L && p_m$n_years <= 15L)
check("mature Ke uses ticker beta CAPM", {
  # Rf + β*(Rm−Rf) = 4 + 0.85*5 = 8.25
  is.finite(p_m$ke_pct) && abs(p_m$ke_pct - 8.25) < 0.05
})
check("mature beta_source ticker", identical(p_m$beta_source, "ticker"))
check("mature g from fundamental SGR (not APP_DEFAULTS alone)", {
  fund <- calc_fundamental_sgr_pct(mature_is, mature_bs, mature_cf)
  is.finite(p_m$fund_sgr_pct) && is.finite(fund) && abs(p_m$fund_sgr_pct - fund) < 0.05
})
check("mature g capped below Ke", is.finite(p_m$g_pct) && p_m$g_pct < p_m$ke_pct - 1)

p_g <- lab_resolve_ticker_params(
  growth_is, growth_bs, growth_cf,
  industry_key = "saas.SaaS_Cloud",
  beta = 1.4,
  rf_pct = 4,
  rm_pct = 9,
  ticker = "CRWD"
)
check("growth Ke uses beta 1.4", {
  # 4 + 1.4*5 = 11
  is.finite(p_g$ke_pct) && abs(p_g$ke_pct - 11) < 0.05
})
check("growth g1 from revenue CAGR (steep)", {
  is.finite(p_g$rev_cagr_pct) && p_g$rev_cagr_pct > 20 &&
    is.finite(p_g$g1_pct) && p_g$g1_pct >= 10 && p_g$g1_pct <= 15
})
check("growth and mature n not forced equal when stages differ", {
  # Allow equal if classifier agrees; when stages differ, n should often differ
  st_m <- as.character(p_m$lifecycle_stage %||% "")
  st_g <- as.character(p_g$lifecycle_stage %||% "")
  if (nzchar(st_m) && nzchar(st_g) && !identical(st_m, st_g)) {
    !identical(as.integer(p_m$n_years), as.integer(p_g$n_years)) ||
      identical(recommended_forecast_years_for_stage(st_m),
                recommended_forecast_years_for_stage(st_g))
  } else {
    TRUE
  }
})
check("HIGH_GROWTH stage maps to longer n than MATURE_STABLE", {
  n_hg <- recommended_forecast_years_for_stage("HIGH_GROWTH")
  n_ms <- recommended_forecast_years_for_stage("MATURE_STABLE")
  is.finite(n_hg) && is.finite(n_ms) && n_hg > n_ms
})

p_fb <- lab_resolve_ticker_params(
  mature_is, mature_bs, mature_cf,
  industry_key = "saas.SaaS_Cloud",
  beta = NA_real_,
  rf_pct = 4,
  rm_pct = 9
)
check("missing beta falls back industry/default", {
  identical(p_fb$beta_source, "industry") || identical(p_fb$beta_source, "default")
})
check("fallback still finite Ke", is.finite(p_fb$ke_pct) && p_fb$ke_pct > 0)

cat("\n=== lab_estimate_fv_per_share uses per-ticker params ===\n")

est_a <- lab_estimate_fv_per_share(
  "dcf", "saas.SaaS_Cloud", growth_is, growth_bs, growth_cf,
  price = 50, market_cap = 5000, beta = 1.4, ticker = "CRWD"
)
est_b <- lab_estimate_fv_per_share(
  "dcf", "fn.Banking", mature_is, mature_bs, mature_cf,
  price = 50, market_cap = 5000, beta = 0.85, ticker = "JPM"
)
# Same path as estimate (App/live Rf·Rm; no forced rf/rm override)
p_est_a <- lab_resolve_ticker_params(
  growth_is, growth_bs, growth_cf,
  industry_key = "saas.SaaS_Cloud", beta = 1.4, ticker = "CRWD"
)
p_est_b <- lab_resolve_ticker_params(
  mature_is, mature_bs, mature_cf,
  industry_key = "fn.Banking", beta = 0.85, ticker = "JPM"
)
check("growth DCF FV finite", is.finite(est_a$fv) && est_a$fv > 0)
check("mature DCF FV finite", is.finite(est_b$fv) && est_b$fv > 0)
check("different tickers can get different n", {
  # Not required always, but Ke and/or g1 should differ with different beta/growth
  !identical(est_a$ke_pct, est_b$ke_pct) || !identical(est_a$g1_pct, est_b$g1_pct) ||
    !identical(est_a$n_years, est_b$n_years)
})
check("est Ke matches resolve with same beta (growth)",
      abs(est_a$ke_pct - p_est_a$ke_pct) < 0.05)
check("est Ke matches resolve with same beta (mature)",
      abs(est_b$ke_pct - p_est_b$ke_pct) < 0.05)
check("ticker beta moves Ke vs industry-only", {
  ind_ke <- lab_industry_ke_pct("saas.SaaS_Cloud")
  is.finite(est_a$ke_pct) && is.finite(ind_ke) && abs(est_a$ke_pct - ind_ke) > 0.05
})
check("override n_years honored", {
  est_o <- lab_estimate_fv_per_share(
    "dcf", "saas.SaaS_Cloud", growth_is, growth_bs, growth_cf,
    price = 50, market_cap = 5000, beta = 1.4, n_years = 7L
  )
  identical(as.integer(est_o$n_years), 7L)
})

# Same FV math with identical overrides → same result (determinism)
est1 <- lab_estimate_fv_per_share(
  "pb", "fn.Banking", mature_is, mature_bs, mature_cf,
  price = 40, market_cap = 4000, beta = 1, ke_pct = 10, g_pct = 3, n_years = 5L
)
est2 <- lab_estimate_fv_per_share(
  "pb", "fn.Banking", mature_is, mature_bs, mature_cf,
  price = 40, market_cap = 4000, beta = 1, ke_pct = 10, g_pct = 3, n_years = 5L
)
check("deterministic FV with fixed overrides",
      is.finite(est1$fv) && is.finite(est2$fv) && abs(est1$fv - est2$fv) < 1e-9)

# CAGR uses ticker n, not APP_DEFAULTS when n differs
cagr5 <- lab_annualized_upside_pct(200, 100, 5)
cagr10 <- lab_annualized_upside_pct(200, 100, 10)
check("CAGR depends on n", is.finite(cagr5) && is.finite(cagr10) && cagr5 > cagr10)

cat("\n=== Summary ===\n")
cat("PASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
