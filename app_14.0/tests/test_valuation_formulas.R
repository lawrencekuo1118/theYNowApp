#!/usr/bin/env Rscript
# Canonical valuation-formula checks (no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "ri_module.R"), local = FALSE)
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
approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# Gordon DDM: P0 = D0(1+g)/(Ke−g)
p_ddm <- .ddm_formula_p0(d0 = 1, g = 0.05, ke = 0.10)
check("DDM Gordon", approx_eq(p_ddm, 1.05 / 0.05))

# P/B: P = BVPS × target
check("P/B linear", approx_eq(.pb_formula_p(basis = 12, pb = 1.5), 18))

# Live DCF Gordon n=1: EV = F1/(1+r) + F1(1+g)/((r−g)(1+r))
r <- 0.10; g <- 0.03; f0 <- 1
ev <- .dcf_formula_ev(n = 1, r1 = r, g_term = g, g_near = 0, f0 = f0)
ev_closed <- f0 / (1 + r) * (1 + (1 + g) / (r - g))
check("DCF Gordon n=1", approx_eq(ev, ev_closed, 1e-10))

# Unit FCFF path uses NOPAT+D&A−CapEx − ΔRev×NWC (not NI)
fcff1 <- .dcf_unit_fcff_path(
  n = 1, g_near = 0.10, nopat_m = 0.20, depre_m = 0.05, capex_m = 0.08, nwc_m = 0.10
)
rev1 <- 1.10
fcff1_closed <- rev1 * (0.20 + 0.05 - 0.08) - (rev1 - 1) * 0.10
check("FCFF NOPAT path y1", approx_eq(fcff1[1], fcff1_closed))

# Discounting the NOPAT path matches .dcf_formula_ev_from_fcff
ev_path <- .dcf_formula_ev_from_fcff(fcff1, r1 = 0.10, g_term = 0.03)
tv <- fcff1[1] * 1.03 / (0.10 - 0.03)
ev_path_closed <- fcff1[1] / 1.10 + tv / 1.10
check("DCF from NOPAT FCFF", approx_eq(ev_path, ev_path_closed, 1e-10))

# RI: V0 = B0 + PV(RI) + PV(TV)
cell <- compute_ri_valuation(
  b0 = 10, ke = 0.10, g = 0.03, n = 1, payout = 0.40, roe_path = 0.15
)
ri1 <- (0.15 - 0.10) * 10
pv_ri <- ri1 / 1.10
tv_ri <- ri1 * 1.03 / (0.10 - 0.03)
v_closed <- 10 + pv_ri + tv_ri / 1.10
check("RI intrinsic", identical(cell$status, "success") && approx_eq(cell$intrinsic, v_closed, 1e-10))

# PIT hist DCF is geometric FCF0×(1+g)^t, not revenue table
fv <- estimate_hist_dcf(
  fcf0 = 100, cash = 0, debt = 0, shares = 10,
  wacc = 0.10, sgr = 0.03, n_years = 1, g_explicit = 0.05
)
fcf1 <- 100 * 1.05
ev_hist <- fcf1 / 1.10 + (fcf1 * 1.03 / (0.10 - 0.03)) / 1.10
check("PIT DCF geometric", approx_eq(fv, ev_hist / 10, 1e-8))

if (fail > 0L) {
  cat(fail, " formula check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("All valuation formula checks passed.\n")
