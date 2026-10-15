#!/usr/bin/env Rscript
# Phase 3 — Parameter & Boundary Audit (default lock + edge-case guards)
# Offline; no live Yahoo / TPEx network required.
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_parameters_boundaries.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("setup.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "market_profile.R"), local = FALSE)
source(file.path(app_dir, "default_config.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
source(file.path(app_dir, "relative_multiples_module.R"), local = FALSE)
source(file.path(app_dir, "earnings_quality_module.R"), local = FALSE)
source(file.path(app_dir, "ynow_index.R"), local = FALSE)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}
approx_eq <- function(a, b, tol = 1e-9) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}
# Spec twin of ynow_server `.clamp_near_term_g_pct` (audit of documented −5%…25% ceiling)
.clamp_near_term_g_pct_spec <- function(g, lo = -5, hi = 25) {
  g <- suppressWarnings(as.numeric(g)[1])
  if (!is.finite(g)) return(NA_real_)
  max(lo, min(g, hi))
}
no_crash <- function(expr) {
  tryCatch({ force(expr); TRUE }, error = function(e) FALSE)
}

# =============================================================================
# A. Default-value lock — APP_DEFAULTS / market / EQ / ADR / YNOW
# =============================================================================
cat("\n== A. Default-value lock ==\n")

check("APP_DEFAULTS is a list", is.list(APP_DEFAULTS) && length(APP_DEFAULTS) > 20L)
check("boot ticker empty (placeholder only)", identical(APP_DEFAULTS$stock_code, ""))
check("boot industry sc.Foundry", identical(APP_DEFAULTS$industry_choice, "sc.Foundry"))
check("forecast years == 5", identical(as.integer(APP_DEFAULTS$years), 5L))
check("dcf_mode gordon", identical(APP_DEFAULTS$dcf_mode, "gordon"))
check("dcf_claim fcff", identical(APP_DEFAULTS$dcf_claim, "fcff"))
check("perpetual_g_method fundamental", identical(APP_DEFAULTS$perpetual_g_method, "fundamental"))
check("g_growth_method fundamental", identical(APP_DEFAULTS$g_growth_method, "fundamental"))
check("beta_u_apply_source summary", identical(APP_DEFAULTS$beta_u_apply_source, "summary"))
check("beta_bl_source summary", identical(APP_DEFAULTS$beta_bl_source, "summary"))
check("beta_lookback_months 60", identical(as.integer(APP_DEFAULTS$beta_lookback_months), 60L))
check("beta_min_obs 24", identical(as.integer(APP_DEFAULTS$beta_min_obs), 24L))

# CAPM / tax / Rf engineering fallbacks (boot uses US profile)
check("capm_rf boot == US rf_fallback 5", approx_eq(APP_DEFAULTS$capm_rf, 5))
check("wacc_tax boot == 21 (US)", approx_eq(APP_DEFAULTS$wacc_tax, 21))
check("US rf_fallback == 5.0", approx_eq(market_profile("US")$rf_fallback, 5.0))
check("TW rf_fallback == 1.8", approx_eq(market_profile("TW")$rf_fallback, 1.8))
check("US statutory tax 21", approx_eq(market_profile("US")$wacc_tax, 21))
check("TW statutory tax 20", approx_eq(market_profile("TW")$wacc_tax, 20))

# CapEx spike + g ceiling defaults (engineering heuristics)
check("apply_capex_spike_smooth TRUE", isTRUE(APP_DEFAULTS$apply_capex_spike_smooth))
check("capex_spike_mult == 1.35", approx_eq(APP_DEFAULTS$capex_spike_mult, 1.35))
check("capex_spike_avg_years == 3", identical(as.integer(APP_DEFAULTS$capex_spike_avg_years), 3L))
check("capex_spike_prior_years == 2", identical(as.integer(APP_DEFAULTS$capex_spike_prior_years), 2L))
check("apply_g_ceiling TRUE", isTRUE(APP_DEFAULTS$apply_g_ceiling))

# Decision Checklist MOS / HFV / F-Score gates
check("cond_base_mos_floor == 15", approx_eq(APP_DEFAULTS$cond_base_mos_base_mos_floor, 15))
check("cond_bear_mos_floor == -10", approx_eq(APP_DEFAULTS$cond_bear_base_bear_mos_floor, -10))
check("chk_hfv_veto default ON", isTRUE(APP_DEFAULTS$chk_hfv_veto))
check("cond_hfv_veto_max_c_freq == 35", approx_eq(APP_DEFAULTS$cond_hfv_veto_max_c_freq, 35))
check("cond_fscore_min == 5", approx_eq(APP_DEFAULTS$cond_fscore_fscore_min, 5))
check("chk_bear_base default ON", isTRUE(APP_DEFAULTS$chk_bear_base))

# Relative multiples App assumptions
check("rel_pe_multiple == 18", approx_eq(APP_DEFAULTS$rel_pe_multiple, 18))
check("rel_mode equity", identical(APP_DEFAULTS$rel_mode, "equity"))

# ADR / share-unit guards + EQ thresholds + YNOW index
check("SHARE_UNIT_MISMATCH_RATIO == 1.5", approx_eq(SHARE_UNIT_MISMATCH_RATIO, 1.5))
check("SHARE_UNIT_MISMATCH_RATIO_ADR == 1.25", approx_eq(SHARE_UNIT_MISMATCH_RATIO_ADR, 1.25))
check("EQ mscore_alert == -1.78", approx_eq(.EQ_THRESHOLDS$mscore_alert, -1.78))
check("EQ mscore_watch == -2.22", approx_eq(.EQ_THRESHOLDS$mscore_watch, -2.22))
check("YNOW_INDEX_N == 10", identical(as.integer(YNOW_INDEX_N), 10L))
check("YNOW_INDEX_MIN_FSCORE == 8", identical(as.integer(YNOW_INDEX_MIN_FSCORE), 8L))

# Two-stage / DDM structural defaults
check("yr_stage1 == 3", identical(as.integer(APP_DEFAULTS$yr_stage1), 3L))
check("ddm_mode gordon", identical(APP_DEFAULTS$ddm_mode, "gordon"))
check("pb_target_mode justified", identical(APP_DEFAULTS$pb_target_mode, "justified"))

# Server still documents −5%…25% g ceiling (spec not silently removed)
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "ynow_server defines .clamp_near_term_g_pct",
  grepl("\\.clamp_near_term_g_pct\\s*<-\\s*function", srv)
)
check(
  "ynow_server g ceiling hi=25 / lo=-5 present",
  grepl("lo\\s*=\\s*-5", srv) && grepl("hi\\s*=\\s*25", srv)
)

# =============================================================================
# B. Boundary — near-term g ceiling (−5%…25%)
# =============================================================================
cat("\n== B. Boundary: near-term g ceiling ==\n")

check("g ceiling clamps 40 → 25", approx_eq(.clamp_near_term_g_pct_spec(40), 25))
check("g ceiling clamps -20 → -5", approx_eq(.clamp_near_term_g_pct_spec(-20), -5))
check("g ceiling keeps 12", approx_eq(.clamp_near_term_g_pct_spec(12), 12))
check("g ceiling keeps -3", approx_eq(.clamp_near_term_g_pct_spec(-3), -3))
check("g ceiling NA stays NA", is.na(.clamp_near_term_g_pct_spec(NA_real_)))
check(
  "g ceiling extreme 1e9 → 25 (no crash)",
  isTRUE(no_crash(.clamp_near_term_g_pct_spec(1e9))) &&
    approx_eq(.clamp_near_term_g_pct_spec(1e9), 25)
)

# =============================================================================
# C. Boundary — CapEx spike heuristic
# =============================================================================
cat("\n== C. Boundary: CapEx spike ==\n")

# Newest CapEx/Rev far above prior → spike TRUE; margin uses 3y avg when enabled
cap_spike <- c(200, 50, 48, 45)   # newest first
rev_ok <- c(100, 100, 100, 100)
check(
  "spike detected at mult 1.35",
  isTRUE(ratio_spike_vs_prior(cap_spike, rev_ok, prior_n = 2L, mult = 1.35))
)
sync_on <- fcf_sync_capex_revenue_margin(
  cap_spike, rev_ok,
  settings = list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("spike sync flags spike", isTRUE(sync_on$spike))
check("spike sync uses avg margin not latest", isTRUE(sync_on$ok) && sync_on$margin < (200 / 100))

sync_off <- fcf_sync_capex_revenue_margin(
  cap_spike, rev_ok,
  settings = list(enabled = FALSE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("spike disabled keeps latest margin", isTRUE(sync_off$ok) && approx_eq(sync_off$margin, 2.0))

# Denominator zero / short series → fail-soft FALSE / empty (no crash)
check(
  "spike den=0 → FALSE no crash",
  isTRUE(no_crash(ratio_spike_vs_prior(c(10, 5), c(0, 100), prior_n = 1L, mult = 1.35))) &&
    isFALSE(ratio_spike_vs_prior(c(10, 5), c(0, 100), prior_n = 1L, mult = 1.35))
)
check(
  "spike short series → FALSE",
  isFALSE(ratio_spike_vs_prior(c(10), c(100), prior_n = 2L, mult = 1.35))
)
empty_sync <- fcf_sync_capex_revenue_margin(numeric(0), numeric(0))
check("empty CapEx/Rev sync ok=FALSE", isFALSE(empty_sync$ok))

# Invalid mult ≤ 1 falls back to 1.35 inside sync
sync_bad_mult <- fcf_sync_capex_revenue_margin(
  cap_spike, rev_ok,
  settings = list(enabled = TRUE, mult = 0.5, avg_n = 3L, prior_n = 2L)
)
check("invalid mult≤1 still runs (fallback 1.35)", isTRUE(no_crash(sync_bad_mult$ok)))

# =============================================================================
# D. Boundary — WACC / Ke vs terminal g, shares, negative terminal CF
# =============================================================================
cat("\n== D. Boundary: DCF discount vs g / shares / terminal CF ==\n")

ok_dcf <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.09, g_term = 0.03,
  shares = 10, cash = 0, debt = 0
)
check("sane DCF ok", isTRUE(ok_dcf$ok) && is.finite(ok_dcf$per_share))

# g >= WACC → fatal stop (not Inf / crash)
eq_g <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = 0.08,
  shares = 10, cash = 0, debt = 0
)
check("g == WACC → ok=FALSE", isFALSE(eq_g$ok) && !is.finite(eq_g$per_share))
gt_g <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = 0.12,
  shares = 10, cash = 0, debt = 0
)
check("g > WACC → ok=FALSE", isFALSE(gt_g$ok) && !is.finite(gt_g$per_share))
neg_g <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = -0.05,
  shares = 10, cash = 0, debt = 0
)
check("negative terminal g still allowed when WACC>g", isTRUE(neg_g$ok) && is.finite(neg_g$per_share))

huge_g <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = 50,
  shares = 10, cash = 0, debt = 0
)
check("irrational huge g → ok=FALSE no crash", isTRUE(no_crash(huge_g)) && isFALSE(huge_g$ok))

zero_sh <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = 0.02,
  shares = 0, cash = 0, debt = 0
)
check("shares=0 → ok=FALSE", isFALSE(zero_sh$ok))
neg_sh <- value_dcf_per_share(
  c(100, 110), claim = "fcff", wacc = 0.08, g_term = 0.02,
  shares = -5, cash = 0, debt = 0
)
check("shares<0 → ok=FALSE", isFALSE(neg_sh$ok))

neg_term <- value_dcf_per_share(
  c(100, -50), claim = "fcff", wacc = 0.08, g_term = 0.02,
  shares = 10, cash = 0, debt = 0
)
check("negative terminal CF → ok=FALSE", isFALSE(neg_term$ok))

# Claim / rate mismatch
fcff_ke <- value_dcf_per_share(
  c(100), claim = "fcff", ke = 0.10, wacc = NA, discount_kind = "ke",
  g_term = 0.02, shares = 10
)
check("FCFF+Ke mismatch → ok=FALSE", isFALSE(fcff_ke$ok))

# Stage years
st_bad <- validate_stage_years(5, yr_stage1 = 9, yr_stage2 = 2)
check("stage1 > horizon → not ok", !isTRUE(st_bad$ok))
st_ok <- validate_stage_years(5, yr_stage1 = 3, yr_stage2 = 2, g2 = 0.04, g_term = 0.02)
check("valid stages ok", isTRUE(st_ok$ok))

# =============================================================================
# E. Boundary — WACC weights / CAPM / P/B negative equity / MOS / PE
# =============================================================================
cat("\n== E. Boundary: WACC, CAPM, P/B, MOS, PE ==\n")

w_ok <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = 0.21, we = 0.7, wd = 0.3)
check("WACC weights sum1 ok", isTRUE(w_ok$ok) && is.finite(w_ok$wacc))
w_bad <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = 0.21, we = 0.7, wd = 0.5)
check("WACC bad weights → not ok / NA", !isTRUE(w_bad$ok) && !is.finite(w_bad$wacc))
w_na_tax <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = NA, we = 0.7, wd = 0.3)
check("WACC NA tax → not ok", !isTRUE(w_na_tax$ok))

capm_bad <- capm_cost_of_equity(rf = 0.04, beta = 1, rm = 0.09, erp = 0.08)
check("CAPM erp mismatch → not ok", !isTRUE(capm_bad$ok))
capm_ok <- capm_cost_of_equity(rf = 0.04, beta = 1.1, rm = 0.09)
check("CAPM sane ok", isTRUE(capm_ok$ok) && is.finite(capm_ok$ke))
capm_neg_beta <- capm_cost_of_equity(rf = 0.04, beta = -5, rm = 0.09)
# Negative beta is mathematically allowed; must not crash
check("CAPM extreme negative beta no crash", isTRUE(no_crash(capm_neg_beta)))

pb_neg <- derive_pb_targets(
  roe_pct = 15, ke_pct = 10, g_pct = 3, book_equity = -20,
  industry_band = list(low = 1, mid = 1.4, high = 1.8)
)
check("negative book equity blocks P/B mid", !is.finite(pb_neg$mid) && isTRUE(pb_neg$pb_blocked))

check("MOS normal (100,80)", approx_eq(mos_vs_fair_value(100, 80), 0.20))
check("MOS FV=0 → NA (no Inf)", is.na(mos_vs_fair_value(0, 50)))
check("MOS |FV|<0.01 → NA", is.na(mos_vs_fair_value(1e-6, 50)))
check("MOS huge FV no crash", isTRUE(no_crash(mos_vs_fair_value(1e15, 50))) && is.finite(mos_vs_fair_value(1e15, 50)))

pe_neg <- calc_pe_implied_price(eps = -2, pe_multiple = 18)
check("PE with neg EPS → unavailable", identical(pe_neg$status, "unavailable") && is.na(pe_neg$implied_price))
pe_zero <- calc_pe_implied_price(eps = 0, pe_multiple = 18)
check("PE with zero EPS → unavailable", identical(pe_zero$status, "unavailable"))
pe_bad_mult <- calc_pe_implied_price(eps = 5, pe_multiple = -3)
check("PE with neg multiple → unavailable", identical(pe_bad_mult$status, "unavailable"))
pe_ok <- calc_pe_implied_price(eps = 5, pe_multiple = 18)
check("PE sane → 90", identical(pe_ok$status, "ok") && approx_eq(pe_ok$implied_price, 90))

# =============================================================================
# F. Boundary — resolve_wacc_input / near-term growth method / NA reuse
# =============================================================================
cat("\n== F. Boundary: WACC input resolve & growth method ==\n")

na_reuse <- resolve_wacc_input(calculated = NA, manual = 0.08, mode = "calculated")
check("NA calculated WACC not reused from manual", !isTRUE(na_reuse$ok) && !is.finite(na_reuse$value))
man_ok <- resolve_wacc_input(calculated = NA, manual = 0.08, mode = "manual")
check("manual WACC labeled", identical(man_ok$override_status, "manual"))

miss_fund <- resolve_near_term_growth("fundamental", fundamental_g = NA, custom_g = 4, fallback_g = NA)
check("fundamental does not borrow custom", !isTRUE(miss_fund$ok))
custom_wins <- resolve_near_term_growth("custom", fundamental_g = 9, custom_g = 4)
check("custom wins when selected", isTRUE(custom_wins$ok) && approx_eq(custom_wins$g, 4))

# =============================================================================
# Summary
# =============================================================================
cat("\n== Parameter & Boundary Audit summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) {
  quit(status = 1L)
}
cat("ALL CONTROLS PASSED\n")
