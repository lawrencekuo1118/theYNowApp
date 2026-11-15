#!/usr/bin/env Rscript
# CapEx / Revenue sync after statement load (no network).
# Guards against sticky proj_capex_rate from a prior ticker (e.g. TSM 37.07%
# left on a fabless name whose own CapEx/Rev is ~2–3%).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
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
approx_eq <- function(a, b, tol = 1e-6) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

spike_cfg <- list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)

# NVDA-like annuals (USD): CapEx / Revenue ~2–3%, far below foundry levels
nvda_cap <- c(6.042e9, 3.236e9, 1.069e9, 1.833e9)
nvda_rev <- c(2.15938e11, 1.30497e11, 6.0922e10, 2.6974e10)
nvda <- fcf_sync_capex_revenue_margin(nvda_cap, nvda_rev, spike_cfg)
check("NVDA-like sync ok", isTRUE(nvda$ok))
check("NVDA-like latest ~2.80%", approx_eq(nvda$latest_pct, 2.798025, tol = 0.02))
check("NVDA-like 3y avg ~2.34%", approx_eq(nvda$avg_pct, 2.34416, tol = 0.05))
check("NVDA-like not a CapEx spike", isFALSE(nvda$spike))
check("NVDA-like seeds latest (~2.8%), not foundry ~37%", approx_eq(nvda$pct, nvda$latest_pct, tol = 1e-9))
check("NVDA-like margin << 10%", is.finite(nvda$pct) && nvda$pct < 10)

# TSM-like annuals: 3y CapEx/Rev mean is the reported 37.07% sticky value
tsm_cap <- c(1282597200000, 964981600000, 955398400000, 1089626400000)
tsm_rev <- c(3809054300000, 2894307700000, 2161735800000, 2263891300000)
tsm <- fcf_sync_capex_revenue_margin(tsm_cap, tsm_rev, spike_cfg)
check("TSM-like sync ok", isTRUE(tsm$ok))
check("TSM-like 3y avg ~37.07%", approx_eq(tsm$avg_pct, 37.07, tol = 0.02))
check("TSM-like latest ~33.67%", approx_eq(tsm$latest_pct, 33.67, tol = 0.02))

# Simulate ticker switch: prior seeded rate must be replaced by the new series
prior_sticky_pct <- round(tsm$avg_pct, 2)  # 37.07 — what do_sync used to leave in place
check("sticky prior is foundry-level", approx_eq(prior_sticky_pct, 37.07, tol = 0.02))
after_switch <- fcf_sync_capex_revenue_margin(nvda_cap, nvda_rev, spike_cfg)
# New sync always returns the new ticker's margin (caller overwrites the input)
check(
  "after TSM→NVDA sync, rate is NVDA (~2.8%), not sticky 37.07",
  isTRUE(after_switch$ok) &&
    approx_eq(after_switch$pct, after_switch$latest_pct, tol = 1e-9) &&
    abs(after_switch$pct - prior_sticky_pct) > 20
)

# Spike path: newest CapEx/Rev >> prior → seed N-year average
spike_cap <- c(100, 10, 10, 10)
spike_rev <- c(100, 100, 100, 100)
sp <- fcf_sync_capex_revenue_margin(spike_cap, spike_rev, spike_cfg)
check("spike detected", isTRUE(sp$spike))
check("spike seeds 3y avg (40%)", approx_eq(sp$pct, 40, tol = 0.01))

# Spike smoothing off → always latest
sp_off <- fcf_sync_capex_revenue_margin(
  spike_cap, spike_rev,
  list(enabled = FALSE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("spike off uses latest 100%", approx_eq(sp_off$pct, 100, tol = 0.01))
check("spike off not flagged", isFALSE(sp_off$spike))

# Quarterly TTM-like NVDA (~2.43%): four quarters CapEx/Rev
q_cap <- c(2.677e9, 1.757e9, 1.284e9, 1.636e9)
q_rev <- c(96.221e9, 81.615e9, 68.127e9, 57.006e9)
q_ttm_pct <- sum(q_cap) / sum(q_rev) * 100
check("user TTM CapEx/Rev ~2.43%", approx_eq(q_ttm_pct, 2.43, tol = 0.02))
q_sync <- fcf_sync_capex_revenue_margin(q_cap, q_rev, spike_cfg)
check("quarterly series sync stays ~2–3%", isTRUE(q_sync$ok) && q_sync$pct < 5)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("All CapEx sync checks passed.\n")
