# Unit tests: MOS next-period stats + hist PIT helpers
app_dir <- if (file.exists("backtest_validation.R")) {
  getwd()
} else if (file.exists("app_15.0/backtest_validation.R")) {
  file.path(getwd(), "app_15.0")
} else {
  stop("Cannot locate app_15.0")
}

source(file.path(app_dir, "backtest_module.R"), local = FALSE)
source(file.path(app_dir, "backtest_validation.R"), local = FALSE)

approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}
check <- function(label, cond) {
  if (!isTRUE(cond)) stop("FAIL: ", label)
  message("OK: ", label)
}

# --- inventory table ---
inv <- pit_param_inventory_table()
check("inventory has rows", is.data.frame(inv) && nrow(inv) >= 10)
check("inventory has 狀態 col", "狀態" %in% names(inv))

# --- hist helpers ---
fund <- list(
  rev_growth = 12, eps_growth = 8, fcf_growth = 10,
  interest_expense = 50, debt = 1000,
  tax_expense = 21, pretax_income = 100,
  g_pit = NA_real_
)
g <- .hist_pit_growth_decimal(fund, fallback = 0.025)
check("growth from YoY fields clamped", approx_eq(g, 0.08, 1e-6))
check("clamp below r", approx_eq(.hist_clamp_g_below_r(0.12, 0.08), 0.075, 1e-6))
check("pit rd", approx_eq(.hist_pit_rd(fund), 0.05, 1e-8))
check("pit tax", approx_eq(.hist_pit_tax(fund), 0.21, 1e-8))
pb <- .hist_justified_pb(roe = 0.15, ke = 0.10, g = 0.03, fallback = 1.5)
check("justified pb", approx_eq(pb, (0.15 - 0.03) / (0.10 - 0.03), 1e-8))

# --- reconstruct hist uses g/rd/tax/pb diagnostics ---
fund_row <- list(
  shares = 100, fcf = 200, cash = 50, debt = 1000,
  ni = 150, equity_book = 1000, dividends_paid = 40,
  rev_growth = 10, eps_growth = 10, fcf_growth = 10,
  g_pit = 0.05,
  interest_expense = 40, tax_expense = 20, pretax_income = 100
)
mp <- list(wacc = 0.09, ke = 0.10, rd = 0.99, tax = 0.99, fv_models = c("dcf", "ri", "pb"))
pit <- reconstruct_fair_value_pit(fund_row, price = 8, mp, use_session_assumptions = FALSE)
check("hist g_used finite", is.finite(pit$g_used))
check("hist rd from interest", approx_eq(pit$rd_used, 0.04, 1e-8))
check("hist tax from pretax", approx_eq(pit$tax_used, 0.20, 1e-8))
check("hist pb justified", is.finite(pit$pb_mid_used) && pit$pb_mid_used > 0)
check("hist fv finite", is.finite(pit$fair_value) && pit$fair_value > 0)

# --- MOS next-period stats (legacy helper still available) ---
vd <- data.frame(
  Date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
                   "2021-03-31", "2021-06-30")),
  hist_price = c(100, 110, 105, 120, 115, 130),
  mos = c(0.40, 0.35, -0.05, 0.05, 0.55, 0.12),
  fair_value = c(140, 150, 100, 126, 180, 130),
  stringsAsFactors = FALSE
)
stats <- summarize_mos_next_period_stats(vd)
check("stats rows", nrow(stats) == length(.MOS_BUCKET_LEVELS))
b30 <- stats[stats$bucket == "便宜 MOS[30%,50%)", , drop = FALSE]
check("bucket 30-50 has n>=1", b30$n[1] >= 1L)
check("p_up in [0,1] or NA", is.na(b30$p_up[1]) || (b30$p_up[1] >= 0 && b30$p_up[1] <= 1))

out <- lookup_mos_bucket_outlook(0.42, stats)
check("outlook bucket", identical(out$bucket, "便宜 MOS[30%,50%)"))
check("outlook has note", is.character(out$note) && nzchar(out$note))

out2 <- lookup_mos_bucket_outlook(-0.15, stats)
check("outlook expensive bucket", identical(out2$bucket, "偏貴 MOS<-10%"))

# --- FV market validation: P_{t+1} vs FV_t + magnitude + OOS ---
pairs <- build_fv_convergence_pairs(vd)
check("pairs n=5", nrow(pairs) == 5L)
check("first toward", identical(pairs$outcome[1], "趨近"))
check("second away", identical(pairs$outcome[2], "遠離"))
check("first below FV", identical(pairs$vs_fv[1], "之下"))
check("third above FV", identical(pairs$vs_fv[3], "之上"))
check("gap_next present", "gap_next" %in% names(pairs) && is.finite(pairs$gap_next[1]))

sum_all <- summarize_fv_market_validation(vd, oos_mode = "insample")
check("sum n=5", sum_all$n == 5L)
check("above+below+flat = n",
      sum_all$n_above + sum_all$n_below + sum_all$n_flat_vs == sum_all$n)
check("median_gap finite", is.finite(sum_all$median_gap))
check("frame note", grepl("非策略回測", sum_all$frame))

sum_win <- summarize_fv_market_validation(
  vd, from = as.Date("2020-06-01"), to = as.Date("2020-12-31"), oos_mode = "insample"
)
check("window filters", sum_win$n >= 1L && sum_win$n < sum_all$n)

sum_real <- summarize_fv_market_validation(
  vd, as_of = as.Date("2020-09-01"), oos_mode = "realized"
)
check("realized as_of filters", sum_real$n < sum_all$n)

sum_exp <- summarize_fv_market_validation(vd, oos_mode = "expanding")
check("expanding runs", is.character(sum_exp$oos_mode))

message("ALL PASS")
