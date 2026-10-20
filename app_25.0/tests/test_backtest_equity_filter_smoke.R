#!/usr/bin/env Rscript
# Offline smoke: backtest equity-curve / filter / exposure integration (no network).
# Asserts current helpers + .run_backtest_core fixture path — no formula changes.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_backtest_equity_filter_smoke.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_25.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("setup.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("setup.R", local = FALSE)
source("market_profile.R", local = FALSE)
source("backtest_module.R", local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# --- Exposure / sentiment helpers ---
check("mos deep cheap → max_exp", approx_eq(mos_hysteresis_target(0.35, 1, 0.90), 0.90))
check("mos mild cheap mid band", approx_eq(mos_hysteresis_target(0.15, 1, 0.90), 0.90 * (0.65 / 0.90)))
check("mos fair band", approx_eq(mos_hysteresis_target(0.05, 1, 0.90), 0.90 * (0.40 / 0.90)))
check("mos expensive → 0", approx_eq(mos_hysteresis_target(-0.20, 1, 0.90), 0))
check("mos NA → flat min(0.40,max_exp)", approx_eq(mos_hysteresis_target(NA_real_, 1, 0.90), 0.40))

check("sentiment equal weights", approx_eq(sentiment_score(0.8, 0.2, 0.5, 0.5), 0.5))
check("sentiment mom-heavy", approx_eq(sentiment_score(1, 0, 1, 0), 1))

mb0 <- mode_b_exposure(0, 0.9, 0.9)
check("mode_b zero Exp_A → pos_b 0", identical(mb0$pos_b, 0) || approx_eq(mb0$pos_b, 0))
mb <- mode_b_exposure(0.6, 0.9, 0.9, 0.5, 0.5, 0.90)
check("mode_b diverges from Exp_A", is.finite(mb$pos_b) && mb$pos_b > 0 && mb$pos_b <= 0.90)
check("mode_b sent in [0,1]", isTRUE(mb$sent >= 0 && mb$sent <= 1))

# --- Great filter / holding filter ---
pass_row <- list(net_margin = 12, rev_growth = 15, eps_growth = 12)
gf_ok <- .great_filter_pass(pass_row, 5, 10, 10, 25, 10)
check("great filter pass healthy", isTRUE(gf_ok$pass))

fail_npm <- .great_filter_pass(
  list(net_margin = 1, rev_growth = 15, eps_growth = 12), 5, 10, 10, 25, 10
)
check("great filter fail low npm", !isTRUE(fail_npm$pass))

loss <- .great_filter_pass(
  list(net_margin = -5, rev_growth = 20, eps_growth = -50), 5, 10, 10, 25, 10
)
check("great filter loss-loose path", isTRUE(loss$pass) && grepl("虧損", loss$path, fixed = TRUE))

hold <- evaluate_holding_filter(
  list(net_margin = 12, rev_growth = 15, eps_growth = 12, fcf_cv = 10),
  list(bt_net_margin = 5, bt_rev_growth = 10, bt_eps_growth = 10, bt_fcf_cv = 25)
)
check("holding filter overall pass", isTRUE(hold$overall))
check("holding filter has 4 rows", length(hold$rows) == 4L)

hold_fail <- evaluate_holding_filter(
  list(net_margin = 1, rev_growth = 2, eps_growth = 2, fcf_cv = 80),
  list(bt_net_margin = 5, bt_rev_growth = 10, bt_eps_growth = 10, bt_fcf_cv = 25)
)
check("holding filter overall fail", !isTRUE(hold_fail$overall))

# --- Equity curve helpers ---
n_eq <- 60L
dates_eq <- as.Date("2023-01-03") + seq_len(n_eq) - 1L
eq <- data.frame(
  Date = dates_eq,
  Trade_A = 1 + seq_len(n_eq) * 0.002,
  Trade_B = 1 + seq_len(n_eq) * 0.0015,
  Model_B = 1 + seq_len(n_eq) * 0.0015,
  BuyHold = 1 + seq_len(n_eq) * 0.0025,
  Benchmark = 1 + seq_len(n_eq) * 0.001,
  stringsAsFactors = FALSE
)
reb <- slice_rebase_nav(eq, from = dates_eq[11], to = dates_eq[50])
check("slice_rebase keeps rows", is.data.frame(reb) && nrow(reb) == 40L)
check("slice_rebase Trade_A starts ~1", approx_eq(reb$Trade_A[1], 1, 1e-9))
check("slice_rebase Model_B mirrors Trade_B", approx_eq(reb$Model_B[1], reb$Trade_B[1], 1e-12))

perf <- nav_perf_metrics(eq)
check("nav_perf returns list", is.list(perf) && "best" %in% names(perf))
check("nav_perf sharpe_a finite on long series", is.finite(perf$sharpe_a))
check("nav_perf empty on short", {
  empty <- nav_perf_metrics(eq[1:2, , drop = FALSE])
  identical(empty$best, "A") && !is.finite(empty$sharpe_a)
})

# --- prepare_daily_df + .run_backtest_core fixture integration ---
n <- 520L
dates <- seq.Date(as.Date("2022-01-03"), by = "day", length.out = n)
# Skip weekends-ish by keeping every day; RSI/ret20 still fine on dense calendar.
px <- data.frame(
  Date = dates,
  Close = 100 * (1.0008 ^ seq_len(n)),
  stringsAsFactors = FALSE
)
bench <- data.frame(
  Date = dates,
  Close = 100 * (1.0005 ^ seq_len(n)),
  stringsAsFactors = FALSE
)
df <- .prepare_daily_df(px, bench)
check("prepare_daily_df rows", is.data.frame(df) && nrow(df) >= 80L)
check("prepare_daily_df has RSI/ret20", all(c("RSI", "ret20", "ret5") %in% names(df)))
check("RSI finite after warm-up", sum(is.finite(df$RSI), na.rm = TRUE) > 50L)

fund <- data.frame(
  year = c(2020L, 2021L, 2022L, 2023L),
  period_end = as.Date(c("2020-12-31", "2021-12-31", "2022-12-31", "2023-12-31")),
  net_margin = c(12, 13, 14, 15),
  rev_growth = c(10, 12, 11, 13),
  eps_growth = c(9, 11, 10, 12),
  fcf_growth = c(8, 10, 9, 11),
  revenue = c(1000, 1100, 1220, 1380),
  fcf = c(200, 220, 240, 260),
  cash = c(50, 60, 70, 80),
  debt = c(100, 90, 80, 70),
  shares = c(100, 100, 100, 100),
  dividends_paid = c(40, 42, 44, 46),
  equity_book = c(800, 860, 930, 1000),
  ni = c(120, 140, 160, 180),
  interest_expense = c(8, 7, 6, 5),
  tax_expense = c(24, 28, 32, 36),
  pretax_income = c(144, 168, 192, 216),
  nopat_m = c(0.12, 0.12, 0.13, 0.13),
  depre_m = c(0.03, 0.03, 0.03, 0.03),
  capex_m = c(0.04, 0.04, 0.04, 0.04),
  nwc_m = c(0.01, 0.01, 0.01, 0.01),
  fund_source = rep("fixture", 4L),
  stringsAsFactors = FALSE
)

params <- list(
  bt_net_margin = 5, bt_rev_growth = 5, bt_eps_growth = 5, bt_fcf_cv = 80,
  bt_w_mom = 0.5, bt_w_rsi = 0.5, bt_w_vg = 0.7,
  bt_max_exp = 0.90, bt_min_exp_pass = 0.05,
  bt_rebal_freq = "quarterly"
)
mp <- list(
  rf = 0.04, rm = 0.08, ke = 0.09, wacc = 0.08, tax = 0.21,
  beta_fallback = 1.0, beta_lookback_months = 12, beta_min_months = 6,
  fv_models = c("dcf", "ri")
)

res <- tryCatch(
  .run_backtest_core(df, fund, params, mp, mos_fallback = 0, tnx_df = NULL),
  error = function(e) e
)
check("run_backtest_core returns list", is.list(res) && !inherits(res, "error"))
if (is.list(res) && !inherits(res, "error")) {
  check("equity_df has Trade_A/B + BH", {
    eqd <- res$equity_df
    is.data.frame(eqd) && nrow(eqd) > 50L &&
      all(c("Trade_A", "Trade_B", "BuyHold", "Benchmark", "Exp_A", "Exp_B") %in% names(eqd))
  })
  check("valuation_df has rebalance rows", {
    vd <- res$valuation_df
    is.data.frame(vd) && nrow(vd) >= 4L &&
      all(c("mos", "filter_pass", "fair_value", "exp_a", "exp_b") %in% names(vd))
  })
  check("some filter_pass TRUE on healthy fundamentals", {
    vd <- res$valuation_df
    isTRUE(any(vd$filter_pass %in% TRUE))
  })
  check("Trade_A starts near 1 after align", {
    ta <- suppressWarnings(as.numeric(res$equity_df$Trade_A))
    i0 <- which(is.finite(ta) & ta > 0)[1]
    is.finite(i0) && abs(ta[i0] - 1) < 1e-6
  })
  check("metrics list present", is.list(res$metrics) && "best" %in% names(res$metrics))
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All backtest equity/filter smoke checks passed.\n")
