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

# Two-stage DDM with g1 = g2 equals Gordon for any n
p_ts_eq <- .ddm_formula_two_stage(d0 = 1, g1 = 0.05, n = 5, g2 = 0.05, ke = 0.10)
check("DDM two-stage g1=g2 equals Gordon", approx_eq(p_ts_eq, p_ddm, 1e-10))

# Two-stage closed form: n=2, g1=8%, g2=4%, Ke=10%
d0 <- 1; g1 <- 0.08; n1 <- 2L; g2 <- 0.04; ke <- 0.10
d1 <- d0 * (1 + g1)
d2 <- d0 * (1 + g1)^2
tv <- d2 * (1 + g2) / (ke - g2)
p_ts_closed <- d1 / (1 + ke) + (d2 + tv) / (1 + ke)^2
p_ts <- .ddm_formula_two_stage(d0 = d0, g1 = g1, n = n1, g2 = g2, ke = ke)
check("DDM two-stage n=2 closed form", approx_eq(p_ts, p_ts_closed, 1e-10))

# P/B: P = BVPS × target
check("P/B linear", approx_eq(.pb_formula_p(basis = 12, pb = 1.5), 18))

# FCFE conversion: FCFE = FCFF − Int(1−T) + g×debt (debt then compounds)
fcfe_path <- fcff_to_fcfe(c(100, 110), interest_after_tax = 10, debt0 = 200, g_path = 0.05)
check("FCFE y1", approx_eq(fcfe_path[1], 100 - 10 + 0.05 * 200))
check("FCFE y2", approx_eq(fcfe_path[2], 110 - 10 + 0.05 * 210))
comp <- fcff_to_fcfe_components(c(100, 110), interest_after_tax = 10, debt0 = 200, g_path = 0.05)
check("FCFE components match path", approx_eq(comp$FCFE[1], fcfe_path[1]) && approx_eq(comp$FCFE[2], fcfe_path[2]))
check("dcf_cf_tag fcff", identical(dcf_cf_tag("fcff"), "FCFF") && identical(dcf_disc_tag("fcff"), "WACC"))
check("dcf_cf_tag fcfe", identical(dcf_cf_tag("fcfe"), "FCFE") && identical(dcf_disc_tag("fcfe"), "Ke"))
check("dcf_yearly_pv_label", {
  grepl("WACC", dcf_yearly_pv_label("fcff"), fixed = TRUE) &&
    grepl("Ke", dcf_yearly_pv_label("fcfe"), fixed = TRUE)
})
check("extract_dcf_claim_series fcff", {
  df <- data.frame(FCFF = c(100, 110))
  all(extract_dcf_claim_series(df, "fcff") == c(100, 110))
})
check("extract_dcf_claim_series fcfe", {
  df <- data.frame(FCFF = c(100, 110))
  approx_eq(extract_dcf_claim_series(df, "fcfe", 10, 200, 0.05)[1], fcfe_path[1])
})

# Holding NAV = Equity − discount × investments
df_nav <- data.frame(
  Metric = c("Common Stock Equity", "Long Term Investments", "Cash And Cash Equivalents"),
  `12/31/2024` = c(1000, 400, 50),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
nav_disc <- extract_nav_components(df_nav, holdco_discount = 0.20)
check("NAV discounted", approx_eq(nav_disc$nav, 1000 - 0.20 * 400))
nav_zero <- extract_nav_components(df_nav, holdco_discount = 0)
check("NAV discount=0 equals equity", approx_eq(nav_zero$nav, 1000))
df_book <- data.frame(
  Metric = "Common Stock Equity",
  `12/31/2024` = 800,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
nav_book <- extract_nav_components(df_book, holdco_discount = 0.20)
check("NAV no investments equals equity", approx_eq(nav_book$nav, 800))
nav_clamp <- extract_nav_components(df_nav, holdco_discount = 0.90)
check("NAV discount clamped to 50%", approx_eq(nav_clamp$discount, 0.5) && approx_eq(nav_clamp$nav, 1000 - 0.5 * 400))

# Signal labels: P vs FV, no inverted jargon
check("signal cheap P<FV", identical(valuation_signal_label(10, 8), VALUATION_SIGNAL_CHEAP))
check("signal expensive P>FV", identical(valuation_signal_label(8, 10), VALUATION_SIGNAL_EXPENSIVE))
check("signal fair", identical(valuation_signal_label(10, 10), VALUATION_SIGNAL_FAIR))

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

# Overview chart: yearly PV must stay below undiscounted CF and must not include TV
cf_chart <- c(100, 110, 121, 133.1, 146.41)
r_chart <- 0.10
pv_chart <- dcf_yearly_cf_pv(cf_chart, r_chart)
check("yearly PV y1 = CF/(1+r)", approx_eq(pv_chart[1], cf_chart[1] / 1.10))
check("yearly PV y5 = CF/(1+r)^5", approx_eq(pv_chart[5], cf_chart[5] / 1.10^5))
check("yearly PV sits below forecast", all(pv_chart < cf_chart))
tv_chart <- dcf_gordon_tv_pv(cf_chart[5], 0.03, r_chart, 1.10^5)
tv_closed <- cf_chart[5] * 1.03 / (0.10 - 0.03)
check("PV(TV) separate from yearly", approx_eq(tv_chart$tv, tv_closed) &&
        approx_eq(tv_chart$pv_tv, tv_closed / 1.10^5))
check("year-5 PV is not CF5+PV(TV)", abs(pv_chart[5] - (pv_chart[5] + tv_chart$pv_tv)) > 1)
pv_ts <- dcf_yearly_cf_pv(c(100, 110), c(0.12, 0.08))
check("two-stage yearly PV", approx_eq(pv_ts[1], 100 / 1.12) &&
        approx_eq(pv_ts[2], 110 / (1.12 * 1.08)))
check("TV blocked when r<=g", !is.finite(dcf_gordon_tv_pv(100, 0.10, 0.10, 1.1)$pv_tv))

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

# PIT FCFE: discount at Ke, no EV→equity cash−debt bridge
fv_fcfe <- estimate_hist_dcf(
  fcf0 = 100, cash = 999, debt = 50, shares = 10,
  wacc = 0.08, sgr = 0.03, n_years = 1, g_explicit = 0.05,
  claim = "fcfe", ke = 0.10, rd = 0.05, tax = 0.21
)
fcf1 <- 100 * 1.05
iat <- 50 * 0.05 * (1 - 0.21)
nb <- 0.05 * 50
fcfe1 <- fcf1 - iat + nb
ev_fcfe <- fcfe1 / 1.10 + (fcfe1 * 1.03 / (0.10 - 0.03)) / 1.10
check("PIT FCFE no cash-debt bridge", approx_eq(fv_fcfe, ev_fcfe / 10, 1e-8))
check("PIT FCFE ignores cash add-back", !approx_eq(fv_fcfe, (ev_fcfe + 999 - 50) / 10, 1e-6))

if (fail > 0L) {
  cat(fail, " formula check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("All valuation formula checks passed.\n")
