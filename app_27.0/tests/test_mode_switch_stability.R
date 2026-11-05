#!/usr/bin/env Rscript
# Offline mode-switch stability matrix for valuation FV identities.
# Does NOT alter formulas — asserts toggles rebind inputs correctly and
# Bear ≤ Base ≤ Bull ordering / FCFF≠FCFE / locale shocks invariant.
#
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_mode_switch_stability.R

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
source(file.path(app_dir, "setup_report.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
source(file.path(app_dir, "backtest_module.R"), local = FALSE)

fail <- 0L
rows <- list()
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
record <- function(...) {
  rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Fixture ticker path (offline synthetic "AAPL-like" unit FCFF / FCFE inputs)
# ---------------------------------------------------------------------------
base_cf <- c(120, 126, 132)          # explicit-period FCFF
base_wacc <- 0.09
base_ke <- 0.11
base_g <- 0.03
shares <- 10
cash <- 80
debt <- 250
iat <- 12                            # interest after tax for FCFE bridge

# CapEx / Rev history for spike toggle (spike when enabled; margins stay Gordon-safe)
cap_hist <- c(20, 8, 7)              # newest first → latest 20%, prior avg ~7.5%
rev_hist <- c(100, 100, 100)

# ---------------------------------------------------------------------------
# 1) Bear / Base / Bull via scenario_stress_factors (same shocks as live UI)
# ---------------------------------------------------------------------------
sf <- scenario_stress_factors("dcf")
check("stress factors expose bear/base/bull", {
  all(c("bear", "base", "bull") %in% names(sf))
})

reprice_dcf <- function(wacc_pp, g_pp, near_g_mult, claim = "fcff") {
  wacc <- base_wacc + wacc_pp / 100
  g <- base_g + g_pp / 100
  cf <- base_cf * near_g_mult
  if (identical(claim, "fcfe")) {
    cf <- fcff_to_fcfe(cf, interest_after_tax = iat, debt0 = debt, g_path = g)
    value_dcf_per_share(
      cf, claim = "fcfe", ke = base_ke + wacc_pp / 100, g_term = g,
      shares = shares, cash = cash, debt = debt
    )
  } else {
    value_dcf_per_share(
      cf, claim = "fcff", wacc = wacc, g_term = g,
      shares = shares, cash = cash, debt = debt
    )
  }
}

fv_base <- reprice_dcf(sf$base$wacc_pp, sf$base$g_pp, sf$base$near_g_mult)
fv_bear <- reprice_dcf(sf$bear$wacc_pp, sf$bear$g_pp, sf$bear$near_g_mult)
fv_bull <- reprice_dcf(sf$bull$wacc_pp, sf$bull$g_pp, sf$bull$near_g_mult)
check("base DCF FV finite", isTRUE(fv_base$ok) && is.finite(fv_base$per_share))
check("bear DCF FV finite", isTRUE(fv_bear$ok) && is.finite(fv_bear$per_share))
check("bull DCF FV finite", isTRUE(fv_bull$ok) && is.finite(fv_bull$per_share))
check(
  "Bear < Base < Bull DCF FV order",
  fv_bear$per_share < fv_base$per_share && fv_base$per_share < fv_bull$per_share
)
record(
  toggle = "bear_base_bull", claim = "fcff",
  bear = fv_bear$per_share, base = fv_base$per_share, bull = fv_bull$per_share,
  note = "WACC±1pp, g∓0.5pp, near_g×0.85/1.10"
)

# ---------------------------------------------------------------------------
# 2) FCFF vs FCFE — different discount + no cash−debt bridge on FCFE
# ---------------------------------------------------------------------------
fv_fcff <- reprice_dcf(0, 0, 1, "fcff")
fv_fcfe <- reprice_dcf(0, 0, 1, "fcfe")
check("FCFF FV finite", isTRUE(fv_fcff$ok) && is.finite(fv_fcff$per_share))
check("FCFE FV finite", isTRUE(fv_fcfe$ok) && is.finite(fv_fcfe$per_share))
check("FCFF != FCFE when leverage/interest present", {
  !approx_eq(fv_fcff$per_share, fv_fcfe$per_share, 1e-4)
})
check("dcf tags FCFF->WACC / FCFE->Ke", {
  identical(dcf_disc_tag("fcff"), "WACC") && identical(dcf_disc_tag("fcfe"), "Ke")
})
# FCFE must reject WACC and FCFF must reject Ke (claim/rate integrity)
bad_fcfe_wacc <- value_dcf_per_share(
  base_cf, claim = "fcfe", wacc = base_wacc, g_term = base_g,
  shares = shares, cash = cash, debt = debt
)
bad_fcff_ke <- value_dcf_per_share(
  base_cf, claim = "fcff", ke = base_ke, g_term = base_g,
  shares = shares, cash = cash, debt = debt
)
check("FCFE rejects WACC-only pricing", !isTRUE(bad_fcfe_wacc$ok))
check("FCFF rejects Ke-only pricing", !isTRUE(bad_fcff_ke$ok))
record(
  toggle = "fcff_vs_fcfe", claim = "both",
  bear = NA_real_, base = fv_fcff$per_share, bull = fv_fcfe$per_share,
  note = sprintf("FCFF=%.6f FCFE=%.6f", fv_fcff$per_share, fv_fcfe$per_share)
)

# ---------------------------------------------------------------------------
# 3) CapEx spike smooth on/off — margin rebinds, formula identity unchanged
# ---------------------------------------------------------------------------
on <- fcf_sync_capex_revenue_margin(
  cap_hist, rev_hist, list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
off <- fcf_sync_capex_revenue_margin(
  cap_hist, rev_hist, list(enabled = FALSE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("CapEx spike detected when enabled", isTRUE(on$ok) && isTRUE(on$spike))
check("CapEx spike off uses latest", isTRUE(off$ok) && !isTRUE(off$spike))
check("spike on seeds avg ≠ latest", {
  is.finite(on$pct) && is.finite(off$pct) && !approx_eq(on$pct, off$pct, 1e-6)
})
# Unit FCFF with different CapEx margins must move FV (input rebind, not silent clamp)
fcff_spike_on <- .dcf_unit_fcff_path(
  3L, g_near = 0.05, nopat_m = 0.20, depre_m = 0.05,
  capex_m = on$margin, nwc_m = 0.02
)
fcff_spike_off <- .dcf_unit_fcff_path(
  3L, g_near = 0.05, nopat_m = 0.20, depre_m = 0.05,
  capex_m = off$margin, nwc_m = 0.02
)
ev_on <- .dcf_formula_ev_from_fcff(fcff_spike_on, r1 = base_wacc, g_term = base_g)
ev_off <- .dcf_formula_ev_from_fcff(fcff_spike_off, r1 = base_wacc, g_term = base_g)
check("CapEx spike on/off changes EV", is.finite(ev_on) && is.finite(ev_off) && !approx_eq(ev_on, ev_off, 1e-6))
check("smoothed CapEx (lower) → higher EV", ev_on > ev_off)
record(
  toggle = "capex_spike", claim = "fcff_unit",
  bear = ev_off, base = ev_on, bull = NA_real_,
  note = sprintf("off_pct=%.2f on_pct=%.2f", off$pct, on$pct)
)

# ---------------------------------------------------------------------------
# 4) Terminal g / WACC rebind — gate blocks g≥WACC; open path finite
# ---------------------------------------------------------------------------
gate_block <- terminal_gordon_gate(base_wacc * 100, base_wacc * 100)
gate_open <- terminal_gordon_gate(base_g * 100, base_wacc * 100)
check("g==WACC blocked", isTRUE(!gate_block$computable))
check("g<WACC open", isTRUE(gate_open$computable))
fv_g_hi <- value_dcf_per_share(
  base_cf, claim = "fcff", wacc = base_wacc, g_term = base_wacc,
  shares = shares, cash = cash, debt = debt
)
fv_g_ok <- value_dcf_per_share(
  base_cf, claim = "fcff", wacc = base_wacc, g_term = base_g,
  shares = shares, cash = cash, debt = debt
)
check("g>=WACC refuses FV", !isTRUE(fv_g_hi$ok))
check("g<WACC yields FV", isTRUE(fv_g_ok$ok))
record(
  toggle = "terminal_g_gate", claim = "fcff",
  bear = NA_real_, base = fv_g_ok$per_share, bull = NA_real_,
  note = "g>=WACC refused; g<WACC finite"
)

# ---------------------------------------------------------------------------
# 5) Two-stage vs Gordon identity when stage rates equal
# ---------------------------------------------------------------------------
ev_gordon <- .dcf_formula_ev_from_fcff(base_cf, r1 = base_wacc, g_term = base_g)
ev_two <- .dcf_formula_ev_from_fcff(
  base_cf, r1 = base_wacc, g_term = base_g, r2 = base_wacc,
  yr_stage1 = 2L, yr_stage2 = 1L
)
check("two-stage r1=r2 equals Gordon", approx_eq(ev_gordon, ev_two, 1e-8))
ev_two_hi <- .dcf_formula_ev_from_fcff(
  base_cf, r1 = base_wacc + 0.01, g_term = base_g, r2 = base_wacc,
  yr_stage1 = 2L, yr_stage2 = 1L
)
check("higher stage-1 WACC lowers EV", is.finite(ev_two_hi) && ev_two_hi < ev_gordon)
record(
  toggle = "gordon_vs_two_stage", claim = "fcff",
  bear = ev_two_hi, base = ev_gordon, bull = ev_two,
  note = "r1=r2 identity; r1+1pp lower"
)

# ---------------------------------------------------------------------------
# 6) Locale en-US vs zh-TW — scenario shocks identical (labels differ)
# ---------------------------------------------------------------------------
audit_en <- valuation_scenario_audit("en")
audit_zh <- valuation_scenario_audit("zh-TW")
check("locale shocks identical", identical(audit_en$shock, audit_zh$shock))
check("locale rules differ (copy only)", !identical(audit_en$rule[[1]], audit_zh$rule[[1]]))
dcf_bear_shock <- audit_en$shock[
  audit_en$model == "dcf" & audit_en$case == "bear" & audit_en$parameter == "wacc_pp"
][1]
check("DCF bear WACC shock +1pp", approx_eq(dcf_bear_shock, 1, 1e-9))
record(
  toggle = "locale_en_zh", claim = "audit",
  bear = dcf_bear_shock, base = 0, bull = NA_real_,
  note = "shocks identical across locales"
)

# ---------------------------------------------------------------------------
# 7) PIT hist DCF: FCFF bridge vs FCFE no bridge (mode claim switch)
# ---------------------------------------------------------------------------
pit_fcff <- estimate_hist_dcf(
  fcf0 = 100, cash = 999, debt = 50, shares = 10,
  sgr = 0.03, wacc = 0.10, n_years = 1L, claim = "fcff", ke = 0.10, rd = 0.05, tax = 0.21
)
pit_fcfe <- estimate_hist_dcf(
  fcf0 = 100, cash = 999, debt = 50, shares = 10,
  sgr = 0.03, wacc = 0.10, n_years = 1L, claim = "fcfe", ke = 0.10, rd = 0.05, tax = 0.21
)
check("PIT FCFF finite", is.finite(pit_fcff))
check("PIT FCFE finite", is.finite(pit_fcfe))
check("PIT FCFF uses cash-debt bridge (cash=999 moves FV)", {
  pit_fcff_no_cash <- estimate_hist_dcf(
    fcf0 = 100, cash = 0, debt = 50, shares = 10,
    sgr = 0.03, wacc = 0.10, n_years = 1L, claim = "fcff", ke = 0.10, rd = 0.05, tax = 0.21
  )
  !approx_eq(pit_fcff, pit_fcff_no_cash, 1e-4)
})
check("PIT FCFE ignores cash add-back", {
  pit_fcfe_no_cash <- estimate_hist_dcf(
    fcf0 = 100, cash = 0, debt = 50, shares = 10,
    sgr = 0.03, wacc = 0.10, n_years = 1L, claim = "fcfe", ke = 0.10, rd = 0.05, tax = 0.21
  )
  approx_eq(pit_fcfe, pit_fcfe_no_cash, 1e-6)
})
record(
  toggle = "pit_claim_bridge", claim = "hist",
  bear = pit_fcfe, base = pit_fcff, bull = NA_real_,
  note = "FCFF bridges cash; FCFE does not"
)

# ---------------------------------------------------------------------------
# Matrix printout (ship evidence)
# ---------------------------------------------------------------------------
mat <- do.call(rbind, rows)
cat("\n== MODE-SWITCH FV MATRIX ==\n")
print(mat, row.names = FALSE)
out_path <- Sys.getenv("YNOW_STABILITY_OUT", unset = "")
if (!nzchar(out_path)) {
  out_path <- file.path(tempdir(), "mode_switch_stability_matrix.csv")
}
tryCatch({
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(mat, out_path, row.names = FALSE)
  cat("Wrote ", out_path, "\n", sep = "")
}, error = function(e) cat("Matrix write skipped: ", conditionMessage(e), "\n", sep = ""))

if (fail > 0L) {
  cat("FAILED ", fail, " mode-switch stability checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL MODE-SWITCH STABILITY CHECKS PASSED\n")
