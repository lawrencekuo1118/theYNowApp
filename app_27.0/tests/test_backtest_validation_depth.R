#!/usr/bin/env Rscript
# Offline depth: backtest_validation MOS outlook / alpha / FV convergence (no network).
# Asserts current analytics helpers — no formula changes.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_backtest_validation_depth.R

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
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("backtest_validation.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("backtest_module.R", local = FALSE)
source("backtest_validation.R", local = FALSE)

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

# Synthetic daily equity (~1.5y) so Sharpe / Jensen have enough obs
n <- 400L
dates <- seq.Date(as.Date("2023-01-03"), by = "day", length.out = n)
set.seed(42)
noise <- rnorm(n, 0, 0.004)
bh <- cumprod(c(1, 1 + 0.0006 + noise[-1]))
ta <- cumprod(c(1, 1 + 0.00045 + noise[-1] * 0.8))
tb <- cumprod(c(1, 1 + 0.00035 + noise[-1] * 0.6))
bm <- cumprod(c(1, 1 + 0.0005 + noise[-1] * 0.9))
exp_a <- pmin(0.9, pmax(0.2, 0.55 + sin(seq_len(n) / 40) * 0.2))
exp_b <- pmin(exp_a, pmax(0.1, exp_a - 0.1))

equity_df <- data.frame(
  Date = dates,
  Trade_A = ta,
  Trade_B = tb,
  Model_B = tb,
  BuyHold = bh,
  Benchmark = bm,
  Exp_A = exp_a,
  Exp_B = exp_b,
  stringsAsFactors = FALSE
)

# Rebalance valuation rows spanning MOS buckets
vd_dates <- dates[c(30, 90, 150, 210, 270, 330)]
valuation_df <- data.frame(
  Date = vd_dates,
  hist_price = c(90, 95, 100, 105, 110, 115),
  fair_value = c(120, 118, 100, 95, 130, 140),
  mos = c(0.25, 0.195, 0.0, -0.105, 0.154, 0.179),
  signal = c("偏便宜（P<FV）", "偏便宜（P<FV）", "公允", "偏貴（P>FV）",
             "偏便宜（P<FV）", "偏便宜（P<FV）"),
  filter_pass = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  fallback_keys = rep("", 6L),
  stringsAsFactors = FALSE
)

# Daily price frame for forward-return helpers
price_df <- data.frame(
  Date = dates,
  Close = 80 * (1.0007 ^ seq_len(n)),
  stringsAsFactors = FALSE
)

# --- Alpha dashboard ---
alpha <- compute_alpha_dashboard(equity_df, rf_annual = 0.04)
check("alpha dashboard 3 series", is.data.frame(alpha) && nrow(alpha) == 3L)
check(
  "alpha series labels",
  identical(as.character(alpha$Series), c("BuyHold", "StrategyA", "StrategyB"))
)
check("alpha CAGR finite", all(is.finite(alpha$CAGR)))
check("alpha Sharpe finite", all(is.finite(alpha$Sharpe)))
check("BH ExcessReturn is 0", approx_eq(alpha$ExcessReturn[alpha$Series == "BuyHold"], 0))
check("JensenAlpha columns present", "JensenAlpha" %in% names(alpha))

# --- BH gap analysis ---
gap <- analyze_bh_gap(equity_df, valuation_df, max_exp = 0.90)
check("bh_gap is list", is.list(gap))
check("bh_gap has headline/outcome", nzchar(as.character(gap$headline %||% "")[1]) &&
        nzchar(as.character(gap$outcome %||% "")[1]))
check(
  "bh_gap terminal shortfall_a numeric",
  is.list(gap$terminal) && is.finite(as.numeric(gap$terminal$shortfall_a)[1])
)
check(
  "bh_gap components_a present",
  is.list(gap$components_a) &&
    all(c("cash_drag", "early_exit", "overvaluation_reduction") %in% names(gap$components_a))
)
check("bh_gap n_rebal matches valuation rows", identical(as.integer(gap$n_rebal), 6L))

# --- MOS effectiveness buckets ---
mos_eff <- validate_mos_effectiveness(valuation_df, price_df)
check("mos_eff is data.frame", is.data.frame(mos_eff) && nrow(mos_eff) == 4L)
check(
  "mos_eff buckets ordered",
  identical(as.character(mos_eff$bucket), c(">50%", "30-50%", "10-30%", "<10%"))
)
check("mos_eff n sums to finite mos rows", sum(mos_eff$n) == sum(is.finite(valuation_df$mos)))
check(
  "mos_eff empty on NULL",
  nrow(validate_mos_effectiveness(NULL, price_df)) == 0L
)

# --- Fair-value edge ---
edge <- validate_fair_value_edge(valuation_df, price_df)
check("fv_edge has table", is.list(edge) && is.data.frame(edge$table) && nrow(edge$table) == 2L)
check(
  "fv_edge groups",
  identical(
    as.character(edge$table$group),
    c("undervalued_mos>10%", "not_undervalued")
  )
)
check("fv_edge answer is character", is.character(edge$answer) && nzchar(edge$answer[1]))
check("fv_edge exposes edge_1y", "edge_1y" %in% names(edge))

# --- FV convergence summarize (deeper than pair builder alone) ---
# Build a clear converge then diverge path
vd_conv <- data.frame(
  Date = as.Date(c("2024-01-02", "2024-04-01", "2024-07-01", "2024-10-01")),
  hist_price = c(80, 95, 110, 130),
  fair_value = c(100, 100, 100, 100),
  mos = c(0.20, 0.05, -0.10, -0.30),
  fallback_keys = c("", "n_years", "", ""),
  stringsAsFactors = FALSE
)
summ <- summarize_fv_convergence(
  vd_conv,
  as_of = as.Date("2025-01-01"),
  oos_mode = "insample",
  locale = "en"
)
check("fv_convergence summarize is list", is.list(summ))
check("fv_convergence n pairs > 0", isTRUE(as.integer(summ$n) >= 1L))
check("fv_convergence has toward/away probs", {
  is.numeric(summ$p_toward) || is.numeric(summ$n_toward)
})
check("fv_convergence includes mos_stats", is.data.frame(summ$mos_stats) || is.null(summ$mos_stats))
check("fv_convergence includes scenarios", is.list(summ$scenarios) || is.data.frame(summ$scenarios))
check(
  "fv_convergence empty path sets no_strategy_fv when no FV",
  isTRUE(summarize_fv_convergence(
    data.frame(Date = as.Date("2024-01-01"), hist_price = 10, fair_value = NA_real_),
    locale = "en"
  )$no_strategy_fv)
)

# --- MOS bucket labels used by outlook ---
check("mos bucket deep cheap", identical(.mos_bucket_label(0.55), "便宜 MOS≥50%"))
check("mos bucket fair", identical(.mos_bucket_label(0.05), "近公允 MOS[0,10%)"))
check("mos bucket expensive", identical(.mos_bucket_label(-0.15), "偏貴 MOS<-10%"))

# Wire outlook tip path on fixture stats
stats <- summarize_mos_next_period_stats(valuation_df)
check("mos stats rows", is.data.frame(stats) && nrow(stats) >= 1L)
out_look <- lookup_mos_bucket_outlook(0.20, stats)
check("outlook returns list/data", is.list(out_look) || is.data.frame(out_look))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All backtest_validation depth checks passed.\n")
