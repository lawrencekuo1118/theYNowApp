#!/usr/bin/env Rscript
# Verify 策略淨值（累積財富，起始＝1） compounding + rebase match documented semantics.
# No network. Plan: display logic is reasonable; cash drag explains lag vs Buy&Hold.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

# Minimal deps for helpers used by slice_rebase_nav / mode_b_exposure
suppressPackageStartupMessages({
  if (!requireNamespace("zoo", quietly = TRUE)) {
    # backtest_module may pull zoo via other paths; keep test self-contained
  }
})
source(file.path(app_dir, "backtest_module.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# --- Daily wealth update (same recurrence as .run_backtest_core) ---
sim_nav <- function(rets, exp) {
  n <- length(rets)
  eq <- numeric(n)
  eq[1] <- 1
  for (i in 2:n) {
    eq[i] <- eq[i - 1] * (1 + exp[i] * rets[i])
  }
  eq
}

rets <- c(0, 0.01, -0.02, 0.015, 0.00, 0.03)
exp_full <- rep(1, length(rets))
exp_90 <- rep(0.90, length(rets))
exp_cash <- c(1, 1, 0, 0, 0, 0)  # filter fail after day 2

bh <- sim_nav(rets, exp_full)
a90 <- sim_nav(rets, exp_90)
a0 <- sim_nav(rets, exp_cash)

check("BuyHold starts at 1", approx_eq(bh[1], 1))
check("max_exp 0.90 ends below full BuyHold", a90[length(a90)] < bh[length(bh)])
check("Exp=0 days leave NAV flat", approx_eq(a0[3], a0[2]) && approx_eq(a0[6], a0[3]))
check(
  "full Exp matches BuyHold path",
  all(mapply(approx_eq, bh, sim_nav(rets, exp_full)))
)

# Explicit hand compound: day2 Exp=0.9, r=1% → 1 * 1.009
check(
  "hand compound Exp*r",
  approx_eq(sim_nav(c(0, 0.01), c(1, 0.9))[2], 1.009)
)

# --- slice_rebase_nav: window start = 1 for each series ---
eq_df <- data.frame(
  Date = as.Date("2020-01-01") + 0:5,
  Trade_A = a90,
  Trade_B = a90 * 1.01,
  BuyHold = bh,
  Benchmark = bh * 0.95,
  Exp_A = exp_90,
  Close = 100 * cumprod(1 + rets),
  FairValue = rep(110, 6),
  stringsAsFactors = FALSE
)
# Mid-window slice
win <- slice_rebase_nav(eq_df, from = as.Date("2020-01-03"), to = as.Date("2020-01-06"))
check("rebase Trade_A starts at 1", approx_eq(win$Trade_A[1], 1))
check("rebase BuyHold starts at 1", approx_eq(win$BuyHold[1], 1))
check("rebase leaves Exp_A unchanged scale", approx_eq(win$Exp_A[1], 0.90))
check("rebase leaves Close unscaled", approx_eq(win$Close[1], eq_df$Close[3]))

# --- mode_b: Exp_A=0 ⇒ Exp_B=0 ---
mb0 <- mode_b_exposure(0, mom_score = 1, rsi_score = 1, max_exp = 0.9)
check("Exp_A=0 forces Exp_B=0", approx_eq(mb0$pos_b, 0))

mb1 <- mode_b_exposure(0.8, mom_score = 0.2, rsi_score = 0.2, max_exp = 0.9)
check("Exp_B stays within max_exp", mb1$pos_b <= 0.9 + 1e-12)

# --- UI copy still documents wealth index (not price) ---
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("UI title 策略淨值（累積財富，起始＝1）", grepl("策略淨值（累積財富，起始＝1）", ui, fixed = TRUE))
check("UI says 財富指數，不是每股價格", grepl("這是財富指數，不是每股價格", ui, fixed = TRUE))
check("UI documents Exp_A×日報酬", grepl("Exp_A×日報酬", ui, fixed = TRUE))

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check(
  "exposure stats mentions cash drag vs B&H",
  grepl("平均持股偏低時，終值常低於滿持股 B&H", srv, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All NAV reasonableness checks passed.\n")
