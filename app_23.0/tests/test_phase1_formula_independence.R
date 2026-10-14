#!/usr/bin/env Rscript
# Phase 1 — Independent formula verification (P1-A DCF/RI/DDM + P1-B MULT).
# Oracles live in audit_independent_oracles.R (no engine calls).
# Engine under test: app_23.0 production helpers only.
#
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_phase1_formula_independence.R
# Optional: PHASE1_FINDINGS_JSON=/path/findings.json

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

# --- Load oracles FIRST (must stay independent of engines) ---
sys.source(file.path(test_dir, "audit_independent_oracles.R"), envir = environment())

# --- Load engines under test ---
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
source(file.path(app_dir, "ri_module.R"), local = FALSE)
source(file.path(app_dir, "relative_multiples_module.R"), local = FALSE)

fail <- 0L
pass <- 0L
findings <- list()

check <- function(label, cond, finding = NULL) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
  if (!is.null(finding)) {
    finding$test_label <- label
    finding$result <- if (isTRUE(cond)) "PASS" else "FAIL"
    findings[[length(findings) + 1L]] <<- finding
  }
}

approx_eq <- function(a, b, tol = 1e-8) .oracle_approx_eq(a, b, tol)

mk_finding <- function(id, model, risk, formula_spec, independent_basis,
                       engine_value, oracle_value, delta = NA_real_,
                       error_cases = character(0), remediation = "none",
                       deploy_impact = "none_proven", notes = "") {
  list(
    id = id,
    model = model,
    risk_level = risk,
    formula_spec = formula_spec,
    independent_basis = independent_basis,
    engine_value = engine_value,
    oracle_value = oracle_value,
    delta = if (is.finite(delta)) delta else {
      a <- suppressWarnings(as.numeric(engine_value)[1])
      b <- suppressWarnings(as.numeric(oracle_value)[1])
      if (is.finite(a) && is.finite(b)) a - b else NA_real_
    },
    error_cases = paste(error_cases, collapse = "; "),
    remediation = remediation,
    deploy_impact = deploy_impact,
    notes = notes
  )
}

# =============================================================================
# P1-A — DCF
# =============================================================================
cat("\n== P1-A DCF: path PV / TV / equity bridge / FCFE / two-rate ==\n")

# Base FCFF path with intermediate steps
cf <- c(100, 105, 110)
wacc <- 0.09; g <- 0.03; shares <- 10; cash <- 50; debt <- 200
h <- oracle_dcf_path(cf, wacc, g, cash, debt, shares, bridge = "ev")
eng <- value_dcf_per_share(
  cf, claim = "fcff", wacc = wacc, g_term = g,
  shares = shares, cash = cash, debt = debt
)
# Reconstruct engine intermediates via same algebra on engine EV
eng_ev <- eng$enterprise
check(
  "DCF FCFF per-share vs independent oracle",
  isTRUE(eng$ok) && approx_eq(eng$per_share, h$per_share, 1e-6),
  mk_finding(
    "DCF-FCFF-PS", "DCF", "P0",
    "EV=Σ CF_t/(1+WACC)^t + [CF_n(1+g)/(WACC−g)]/(1+WACC)^n; Equity=EV+cash−debt; PS=Equity/shares. Requires WACC>g, CF_n>0, shares>0.",
    "oracle_dcf_path (audit_independent_oracles.R) — no engine calls",
    eng$per_share, h$per_share,
    error_cases = c("WACC<=g → ok=FALSE", "shares<=0 → ok=FALSE", "CF_n<0 → ok=FALSE"),
    remediation = if (isTRUE(eng$ok) && approx_eq(eng$per_share, h$per_share, 1e-6)) "none" else "investigate DCF engine",
    deploy_impact = "would_distort_live_FV_if_fail",
    notes = sprintf("oracle pv_cf=%.6f tv=%.6f pv_tv=%.6f EV=%.6f Eq=%.6f",
                    h$pv_cf, h$tv, h$pv_tv, h$enterprise, h$equity)
  )
)
check(
  "DCF FCFF enterprise vs oracle EV",
  isTRUE(eng$ok) && approx_eq(eng_ev, h$enterprise, 1e-6)
)
check(
  "DCF FCFF equity vs oracle equity",
  isTRUE(eng$ok) && approx_eq(eng$equity, h$equity, 1e-6)
)

# FCFE: no cash−debt bridge; discount at Ke
cf_e <- c(80, 90, 100)
ke <- 0.11; g_e <- 0.02
h_e <- oracle_dcf_path(cf_e, ke, g_e, cash = 999, debt = 50, shares = 5, bridge = "none")
eng_e <- value_dcf_per_share(
  cf_e, claim = "fcfe", ke = ke, wacc = 0.08, g_term = g_e,
  shares = 5, cash = 999, debt = 50
)
check(
  "DCF FCFE ignores cash−debt bridge",
  isTRUE(eng_e$ok) && approx_eq(eng_e$per_share, h_e$per_share, 1e-6) &&
    !approx_eq(eng_e$per_share, (h_e$enterprise + 999 - 50) / 5, 1e-3),
  mk_finding(
    "DCF-FCFE-BRIDGE", "DCF", "P0",
    "FCFE discounted at Ke; Equity = PV(FCFE)+PV(TV) with no +cash−debt.",
    "oracle_dcf_path(bridge='none') vs value_dcf_per_share(claim='fcfe')",
    eng_e$per_share, h_e$per_share,
    error_cases = "If FCFF bridge were wrongly applied, PS would include +999−50",
    remediation = "none",
    deploy_impact = "would_distort_FCFE_mode_if_fail"
  )
)

# FCFF→FCFE conversion identity
fcff <- c(100, 110); iat <- 10; debt0 <- 200; gp <- 0.05
h_conv <- oracle_fcff_to_fcfe(fcff, iat, debt0, gp)
e_conv <- fcff_to_fcfe(fcff, interest_after_tax = iat, debt0 = debt0, g_path = gp)
check(
  "FCFF→FCFE y1 vs oracle",
  approx_eq(e_conv[1], h_conv$FCFE[1], 1e-10)
)
check(
  "FCFF→FCFE y2 vs oracle (debt roll)",
  approx_eq(e_conv[2], h_conv$FCFE[2], 1e-10),
  mk_finding(
    "DCF-FCFF-FCFE-CONV", "DCF", "P1",
    "FCFE_t = FCFF_t − IAT + g_t×BegDebt; BegDebt_{t+1}=BegDebt_t+NetBorrowing_t.",
    "oracle_fcff_to_fcfe",
    e_conv[2], h_conv$FCFE[2],
    error_cases = "Wrong debt roll → year-2 FCFE drift",
    remediation = "none",
    deploy_impact = "affects FCFE claim path on live v23.28+"
  )
)

# Two-rate explicit path (engine requires yr_stage1 + yr_stage2; remainder not inferred)
cf2 <- c(50, 55, 60, 66)
h2 <- oracle_dcf_two_rate(cf2, r1 = 0.10, r2 = 0.08, g = 0.03, yr_stage1 = 2L,
                          cash = 10, debt = 40, shares = 8)
e2_missing_y2 <- .dcf_formula_ev_from_fcff(
  cf2, r1 = 0.10, g_term = 0.03, r2 = 0.08, yr_stage1 = 2L
)
check(
  "DCF two-rate without yr_stage2 → NA (strict gate)",
  !is.finite(e2_missing_y2),
  mk_finding(
    "DCF-TWO-RATE-GATE", "DCF", "P1",
    "Two-stage path requires yr_stage1 + yr_stage2 with y1+y2 = n; remainder is not inferred.",
    "Literal expectation NA when yr_stage2 omitted",
    as.character(e2_missing_y2), "NA",
    error_cases = "Missing stage-2 years stops two-stage EV (fail-soft)",
    remediation = "none — guard is intentional",
    deploy_impact = "prevents silent remainder inference on live two-stage DCF",
    notes = "Not a formula error; documented control from validate_stage_years"
  )
)
e2 <- .dcf_formula_ev_from_fcff(
  cf2, r1 = 0.10, g_term = 0.03, r2 = 0.08, yr_stage1 = 2L, yr_stage2 = 2L
)
check(
  "DCF two-rate EV vs oracle (y1=2,y2=2)",
  approx_eq(e2, h2$enterprise, 1e-6),
  mk_finding(
    "DCF-TWO-RATE", "DCF", "P1",
    "Years 1..y1 discount at r1; remaining at r2; Gordon TV uses r2−g.",
    "oracle_dcf_two_rate vs .dcf_formula_ev_from_fcff(yr_stage1, yr_stage2)",
    e2, h2$enterprise,
    error_cases = "Mis-assigned stage rates → TV/PV distortion",
    remediation = "none",
    deploy_impact = "two-stage WACC mode on live app"
  )
)

# Unit Gordon n=1 closed form
hu <- oracle_dcf_unit_gordon(n = 1, r1 = 0.10, g_term = 0.03, g_near = 0, f0 = 1)
eu <- .dcf_formula_ev(n = 1, r1 = 0.10, g_term = 0.03, g_near = 0, f0 = 1)
check("DCF unit Gordon n=1 vs oracle", approx_eq(eu, hu$enterprise, 1e-10))

# Gate: WACC <= g must fail
bad <- value_dcf_per_share(c(100), wacc = 0.05, g_term = 0.05, shares = 10)
check(
  "DCF gate WACC==g unavailable",
  isFALSE(bad$ok),
  mk_finding(
    "DCF-GATE-WACC-G", "DCF", "P0",
    "Gordon requires discount rate > terminal g.",
    "Literal gate expectation (ok=FALSE)",
    as.character(bad$ok), "FALSE",
    error_cases = "WACC<=g would yield Inf/negative TV if ungated",
    remediation = "none",
    deploy_impact = "guard prevents live Inf FV"
  )
)

# =============================================================================
# P1-A — DDM
# =============================================================================
cat("\n== P1-A DDM: Gordon / SPM / two-stage (oracle vs helpers + UI algebra) ==\n")

hg <- oracle_ddm_gordon(d0 = 2, g = 0.04, ke = 0.09)
eg <- .ddm_formula_p0(d0 = 2, g = 0.04, r = 0.09)
# Live UI Gordon inlines d1/(ke−g) then rounds — compare unrounded algebra
ui_gordon_raw <- hg$d1 / (0.09 - 0.04)
check(
  "DDM Gordon helper vs oracle",
  approx_eq(eg, hg$p0, 1e-12) && approx_eq(ui_gordon_raw, hg$p0, 1e-12),
  mk_finding(
    "DDM-GORDON", "DDM", "P0",
    "V0 = D1/(Ke−g), D1=D0(1+g). Premise: Ke>g, D0>0.",
    "oracle_ddm_gordon; UI path uses same algebra then round(p0,2)",
    eg, hg$p0,
    error_cases = c("Ke<=g → NA/error", "D0<=0 → UI error"),
    remediation = "none",
    deploy_impact = "display rounds to 2dp (not a formula bug)",
    notes = "UI round(p0,2) is presentation-only; helper returns full precision"
  )
)

hs <- oracle_ddm_spm(eps = 3, d = 1.2, g = 0.05, ke = 0.10)
es <- .ddm_formula_spm(eps = 3, d = 1.2, g = 0.05, r = 0.10)
check(
  "DDM SPM vs oracle",
  approx_eq(es, hs, 1e-12),
  mk_finding(
    "DDM-SPM", "DDM", "P1",
    "V0 = E·g/Ke² + D/Ke (Brown & Abraham). Requires Ke>0; does not require Ke>g.",
    "oracle_ddm_spm",
    es, hs,
    error_cases = "Ke<=0 → NA",
    remediation = "none",
    deploy_impact = "SPM mode on live DDM tab"
  )
)

# SPM when g > Ke still defined (unlike Gordon)
hs2 <- oracle_ddm_spm(eps = 2, d = 1, g = 0.12, ke = 0.10)
es2 <- .ddm_formula_spm(eps = 2, d = 1, g = 0.12, r = 0.10)
check("DDM SPM allows g>Ke", is.finite(es2) && approx_eq(es2, hs2, 1e-12))

ht <- oracle_ddm_two_stage(d0 = 1, g1 = 0.12, n = 3L, g2 = 0.04, ke = 0.10)
et <- .ddm_formula_two_stage(d0 = 1, g1 = 0.12, n = 3L, g2 = 0.04, r = 0.10)
check(
  "DDM two-stage vs oracle (n=3 intermediates)",
  approx_eq(et, ht$p0, 1e-10),
  mk_finding(
    "DDM-TWO-STAGE", "DDM", "P0",
    "D_t=D0(1+g1)^t; Pn=D_n(1+g2)/(Ke−g2); V0=Σ D_t/(1+Ke)^t + Pn/(1+Ke)^n.",
    "oracle_ddm_two_stage with explicit dts/pn",
    et, ht$p0,
    error_cases = "Ke<=g2 → NA",
    remediation = "none",
    deploy_impact = "two_stage DDM mode",
    notes = sprintf("oracle pn=%.6f pv_div=%.6f", ht$pn, ht$pv_div)
  )
)

# =============================================================================
# P1-A — Residual Income
# =============================================================================
cat("\n== P1-A RI: BV roll / RI_t / TV / linear ROE fade ==\n")

roe_c <- c(0.18, 0.16, 0.14)
hr <- oracle_ri(b0 = 50, ke = 0.10, g = 0.03, n = 3L, payout = 0.4, roe_path = roe_c)
er <- compute_ri_valuation(
  b0 = 50, ke = 0.10, g = 0.03, n = 3L, payout = 0.4, roe_path = roe_c
)
check(
  "RI intrinsic vs oracle",
  identical(er$status, "success") && approx_eq(er$intrinsic, hr$intrinsic, 1e-8),
  mk_finding(
    "RI-INTRINSIC", "RI", "P0",
    "RI_t=(ROE_t−Ke)·BV_{t−1}; BV_t=BV_{t−1}+NI_t(1−payout); V0=B0+PV(RI)+PV[RI_n(1+g)/(Ke−g)].",
    "oracle_ri year-by-year BV/RI/TV",
    er$intrinsic, hr$intrinsic,
    error_cases = c("g>=Ke → error", "invalid ROE path → error"),
    remediation = "none",
    deploy_impact = "RI tab fair value"
  )
)
check("RI year-1 Beg_BV", approx_eq(er$df$Beg_BV[1], hr$beg_bv[1], 1e-10))
check("RI year-2 Beg_BV (roll)", approx_eq(er$df$Beg_BV[2], hr$beg_bv[2], 1e-10))
check("RI year-3 RI", approx_eq(er$df$RI[3], hr$ri[3], 1e-10))
check("RI PV_RI sum", approx_eq(er$pv_ri, hr$pv_ri, 1e-8))
check("RI PV_terminal", approx_eq(er$pv_terminal, hr$pv_terminal, 1e-8))

# Linear ROE fade path
path_lin <- oracle_roe_linear(0.20, 0.12, 4L)
eng_path <- build_roe_path(method = "linear", n = 4, roe_start = 0.20, roe_terminal = 0.12)
check(
  "RI linear ROE path vs oracle",
  length(eng_path) == 4L && all(mapply(approx_eq, eng_path, path_lin, MoreArgs = list(tol = 1e-12))),
  mk_finding(
    "RI-ROE-LINEAR", "RI", "P1",
    "ROE path = seq(roe_start, roe_terminal, length.out=n) for linear fade.",
    "oracle_roe_linear vs build_roe_path(method='linear')",
    eng_path[4], path_lin[4],
    error_cases = "Wrong fade → RI_t and TV scale errors",
    remediation = "none",
    deploy_impact = "linear / industry ROE convergence UI"
  )
)

hl <- oracle_ri(b0 = 100, ke = 0.09, g = 0.02, n = 4L, payout = 0.35, roe_path = path_lin)
el <- compute_ri_valuation(
  b0 = 100, ke = 0.09, g = 0.02, n = 4L, payout = 0.35, roe_path = eng_path
)
check("RI with linear fade intrinsic", approx_eq(el$intrinsic, hl$intrinsic, 1e-8))

# g >= Ke gate
ri_bad <- compute_ri_valuation(b0 = 10, ke = 0.08, g = 0.08, n = 2L, payout = 0.5, roe_path = c(0.1, 0.1))
check("RI gate g==Ke error", identical(ri_bad$status, "error"))

# =============================================================================
# P1-B — MULT coverage
# =============================================================================
cat("\n== P1-B MULT: each method oracle + boundaries + P/S vs EV/Sales ==\n")

# P/E
h_pe <- oracle_pe_price(2.5, 16)
e_pe <- calc_pe_implied_price(2.5, 16)
check(
  "MULT P/E price",
  identical(e_pe$status, "ok") && approx_eq(e_pe$implied_price, h_pe, 1e-12),
  mk_finding(
    "MULT-PE", "MULT", "P0",
    "Implied Price = EPS × P/E. Unavailable if EPS≤0 or multiple≤0.",
    "oracle_pe_price",
    e_pe$implied_price, h_pe,
    error_cases = c("EPS<=0", "PE<=0", "EPS missing"),
    remediation = "none",
    deploy_impact = "Multiples equity family"
  )
)
check("MULT P/E neg EPS unavailable", identical(calc_pe_implied_price(-1, 16)$status, "unavailable"))

# Forward P/E end-to-end
h_fpe <- oracle_forward_pe_price(2.0, 10, 18)
feps <- .rel_forward_eps_from_trailing(2.0, 10)
e_fpe <- calc_pe_implied_price(feps, 18)
check(
  "MULT Forward P/E end-to-end",
  approx_eq(feps, oracle_forward_eps(2.0, 10), 1e-12) &&
    identical(e_fpe$status, "ok") && approx_eq(e_fpe$implied_price, h_fpe, 1e-12),
  mk_finding(
    "MULT-FWD-PE", "MULT", "P0",
    "Fwd EPS = Trailing×(1+g/100); Implied = Fwd EPS × Fwd P/E. No price÷P/E back-solve.",
    "oracle_forward_pe_price",
    e_fpe$implied_price, h_fpe,
    error_cases = c("missing growth → NA fwd EPS", "nonpositive trailing → NA"),
    remediation = "none",
    deploy_impact = "Forward P/E in Multiples sync"
  )
)
check(
  "MULT Forward EPS no price back-solve",
  !is.finite(.rel_forward_eps_from_trailing(2.0, NA_real_))
)

# PEG (indicator only)
h_peg <- oracle_peg(30, 15)
e_peg <- calc_peg(30, 15)
check(
  "MULT PEG ratio",
  identical(e_peg$status, "ok") && approx_eq(e_peg$peg, h_peg, 1e-12),
  mk_finding(
    "MULT-PEG", "MULT", "P2",
    "PEG = P/E ÷ growth%. Indicator only — no Implied Price.",
    "oracle_peg",
    e_peg$peg, h_peg,
    error_cases = c("growth<=0", "PE<=0"),
    remediation = "none",
    deploy_impact = "indicator; does not set FV",
    notes = "Out of price-oracle scope by design"
  )
)

# EV multiples
for (row in list(
  list(id = "MULT-EV-FCF", name = "EV/FCF", metric = 100, m = 10, fn = function() {
    calc_ev_fcf_implied_price(100, 10, cash = 20, debt = 50, shares = 10)
  }),
  list(id = "MULT-EV-EBIT", name = "EV/EBIT", metric = 80, m = 12, fn = function() {
    calc_ev_metric_implied_price(80, 12, 20, 50, 10, "EBIT")
  }),
  list(id = "MULT-EV-EBITDA", name = "EV/EBITDA", metric = 120, m = 10, fn = function() {
    calc_ev_metric_implied_price(120, 10, 20, 50, 10, "EBITDA")
  }),
  list(id = "MULT-EV-SALES", name = "EV/Sales", metric = 500, m = 3, fn = function() {
    calc_ev_metric_implied_price(500, 3, 20, 50, 10, "Revenue")
  }),
  list(id = "MULT-EV-ARR", name = "EV/ARR", metric = 200, m = 10, fn = function() {
    calc_ev_metric_implied_price(200, 10, 20, 50, 10, "ARR")
  })
)) {
  h_ev <- oracle_ev_multiple_price(row$metric, row$m, 20, 50, 10)
  e_ev <- row$fn()
  check(
    paste0("MULT ", row$name, " price"),
    identical(e_ev$status, "ok") && approx_eq(e_ev$implied_price, h_ev$price, 1e-9),
    mk_finding(
      row$id, "MULT", "P0",
      sprintf("%s: EV=metric×multiple; Equity=EV+cash−debt; Price=Equity/shares. Metric≤0 → unavailable.", row$name),
      "oracle_ev_multiple_price",
      e_ev$implied_price, h_ev$price,
      error_cases = c("metric<=0", "multiple<=0", "shares missing"),
      remediation = "none",
      deploy_impact = "Multiples enterprise family"
    )
  )
  check(paste0("MULT ", row$name, " EV intermediate"), approx_eq(e_ev$implied_ev, h_ev$implied_ev, 1e-9))
}

# P/S — must NOT apply cash−debt (contrast EV/Sales)
h_ps <- oracle_ps_price(500, 3, 10)
e_ps <- calc_ps_implied_price(500, 3, 10)
h_evs_same <- oracle_ev_multiple_price(500, 3, cash = 20, debt = 50, shares = 10)
check(
  "MULT P/S price (no net-debt bridge)",
  identical(e_ps$status, "ok") && approx_eq(e_ps$implied_price, h_ps$price, 1e-9) &&
    !approx_eq(e_ps$implied_price, h_evs_same$price, 1e-3),
  mk_finding(
    "MULT-PS", "MULT", "P0",
    "P/S: Equity=Revenue×P/S; Price=Equity/shares. Equity-side — no +cash−debt (unlike EV/Sales).",
    "oracle_ps_price contrasted with oracle_ev_multiple_price same inputs",
    e_ps$implied_price, h_ps$price,
    error_cases = "If cash−debt wrongly applied, P/S would equal EV/Sales path",
    remediation = "none",
    deploy_impact = "equity vs enterprise family distinction",
    notes = sprintf("P/S=%.2f vs EV/Sales=%.2f with cash=20 debt=50", h_ps$price, h_evs_same$price)
  )
)

# SOTP
h_sotp <- oracle_sotp_revenue(c(100, 200), c(2, 4), cash = 20, debt = 50, shares = 10, non_op = 0)
segs <- data.frame(name = c("A", "B"), revenue = c(100, 200), multiple = c(2, 4),
                   stringsAsFactors = FALSE)
e_sotp <- calc_sotp_revenue_implied(segs, 3, cash = 20, debt = 50, shares = 10)
check(
  "MULT SOTP revenue per-segment",
  identical(e_sotp$status, "ok") && approx_eq(e_sotp$implied_price, h_sotp$price, 1e-9),
  mk_finding(
    "MULT-SOTP", "MULT", "P1",
    "EV=Σ(seg_rev×EV/Sales)+non_op; Equity=EV+cash−debt. Requires ≥2 positive segments.",
    "oracle_sotp_revenue",
    e_sotp$implied_price, h_sotp$price,
    error_cases = c("<2 segments", "invalid multiple", "shares missing"),
    remediation = "none",
    deploy_impact = "SOTP revenue engine"
  )
)
check(
  "MULT SOTP single segment unavailable",
  identical(
    calc_sotp_revenue_implied(
      data.frame(name = "Only", revenue = 100, stringsAsFactors = FALSE),
      3, 20, 50, 10
    )$status,
    "unavailable"
  )
)

# Cross-check: module vs thin setup helpers (enterprise / multiple identities)
# multiple_implied_price(multiple, metric, shares) → list$price
if (exists("enterprise_implied_price", mode = "function") &&
    exists("multiple_implied_price", mode = "function")) {
  thin_pe <- multiple_implied_price(16, 2.5, shares = 1)
  check(
    "MULT thin multiple_implied_price == oracle P/E",
    isTRUE(thin_pe$ok) && approx_eq(thin_pe$price, h_pe, 1e-12)
  )
  thin_ev <- enterprise_implied_price(100, 10, cash = 20, debt = 50, shares = 10)
  h_thin <- oracle_ev_multiple_price(100, 10, 20, 50, 10)
  check(
    "MULT thin enterprise_implied_price == oracle EV/FCF",
    isTRUE(thin_ev$ok) && approx_eq(thin_ev$price, h_thin$price, 1e-9)
  )
}

# =============================================================================
# Summary + findings export
# =============================================================================
cat("\n== Phase 1 Formula Independence summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")

findings_path <- Sys.getenv(
  "PHASE1_FINDINGS_JSON",
  unset = file.path(app_dir, "..", "docs", "phase1_findings.json")
)
findings_path <- normalizePath(findings_path, mustWork = FALSE)
dir.create(dirname(findings_path), recursive = TRUE, showWarnings = FALSE)

# Write JSON without requiring jsonlite
esc <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", as.character(x %||% ""), fixed = FALSE)
  x <- gsub("\"", "\\\\\"", x, fixed = FALSE)
  x <- gsub("\n", "\\\\n", x, fixed = FALSE)
  x
}
con <- file(findings_path, open = "w", encoding = "UTF-8")
writeLines("[", con)
for (i in seq_along(findings)) {
  f <- findings[[i]]
  line <- sprintf(
    paste0(
      "{\"id\":\"%s\",\"model\":\"%s\",\"risk_level\":\"%s\",",
      "\"test_label\":\"%s\",\"result\":\"%s\",",
      "\"formula_spec\":\"%s\",\"independent_basis\":\"%s\",",
      "\"engine_value\":\"%s\",\"oracle_value\":\"%s\",\"delta\":%s,",
      "\"error_cases\":\"%s\",\"remediation\":\"%s\",",
      "\"deploy_impact\":\"%s\",\"notes\":\"%s\"}%s"
    ),
    esc(f$id), esc(f$model), esc(f$risk_level),
    esc(f$test_label), esc(f$result),
    esc(f$formula_spec), esc(f$independent_basis),
    esc(f$engine_value), esc(f$oracle_value),
    if (is.finite(f$delta)) sprintf("%.10g", f$delta) else "null",
    esc(f$error_cases), esc(f$remediation),
    esc(f$deploy_impact), esc(f$notes),
    if (i < length(findings)) "," else ""
  )
  writeLines(line, con)
}
writeLines("]", con)
close(con)
cat("Wrote findings JSON → ", findings_path, "\n", sep = "")

if (fail > 0L) {
  cat("SOME CONTROLS FAILED\n")
  quit(status = 1)
}
cat("ALL CONTROLS PASSED\n")
quit(status = 0)
