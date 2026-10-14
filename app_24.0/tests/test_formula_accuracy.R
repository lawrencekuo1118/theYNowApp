#!/usr/bin/env Rscript
# Phase 4 — Formula Accuracy Audit (deterministic closed-form + 3 scenarios)
# Offline; no live Yahoo / TPEx network required.
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_formula_accuracy.R

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

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "valuation_guard.R"), local = FALSE)
source(file.path(app_dir, "ri_module.R"), local = FALSE)
source(file.path(app_dir, "earnings_quality_module.R"), local = FALSE)

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
# assert_almost_equal analogue (float tolerance)
approx_eq <- function(a, b, tol = 1e-8) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}
mk_stmt <- function(...) {
  rows <- list(...)
  nms <- names(rows)
  n <- max(vapply(rows, length, integer(1)))
  df <- data.frame(Breakdown = nms, check.names = FALSE, stringsAsFactors = FALSE)
  yrs <- paste0("Y", seq_len(n))
  for (j in seq_len(n)) {
    df[[yrs[j]]] <- vapply(rows, function(v) {
      if (length(v) >= j) as.numeric(v[j]) else NA_real_
    }, numeric(1))
  }
  df
}
passed <- function(fs, i) identical(as.character(fs$checklist[i, 2]), "通過")

# =============================================================================
# A. Closed-form identities (hand-computed Expected Output)
# =============================================================================
cat("\n== A. Closed-form: WACC / CAPM / DDM / DCF / MOS / SGR ==\n")

# WACC = We·Re + Wd·Rd·(1−T)
# Re=10%, Rd=5%, T=21%, We=0.7, Wd=0.3 → 0.7*0.10 + 0.3*0.05*0.79 = 0.07 + 0.01185 = 0.08185
w_exp <- 0.7 * 0.10 + 0.3 * 0.05 * (1 - 0.21)
w <- compute_wacc(re = 0.10, rd = 0.05, tax_ratio = 0.21, we = 0.7, wd = 0.3)
check("WACC closed form 0.08185", isTRUE(w$ok) && approx_eq(w$wacc, w_exp, 1e-10))

# CAPM Ke% = Rf + β(Rm − Rf) = 4 + 1.2*(9−4) = 10
ke_exp <- 4 + 1.2 * (9 - 4)
check("CAPM Ke% closed form 10", approx_eq(.capm_ke_pct(4, 1.2, 9), ke_exp, 1e-10))
capm <- capm_cost_of_equity(rf = 0.04, beta = 1.2, rm = 0.09)
check("capm_cost_of_equity Ke=0.10", isTRUE(capm$ok) && approx_eq(capm$ke, 0.10, 1e-10))

# DDM Gordon: V0 = D0(1+g)/(r−g) ; D0=1, g=5%, r=10% → 21
ddm_exp <- 1.05 / 0.05
check("DDM Gordon V0=21", approx_eq(.ddm_formula_p0(d0 = 1, g = 0.05, r = 0.10), ddm_exp, 1e-10))

# DDM SPM: V0 = E·g/r² + D/r
spm_exp <- 2 * 0.05 / 0.01 + 1 / 0.10
check("DDM SPM closed form", approx_eq(.ddm_formula_spm(eps = 2, d = 1, g = 0.05, r = 0.10), spm_exp, 1e-10))

# DDM two-stage n=2 hand path
d0 <- 1; g1 <- 0.08; n1 <- 2L; g2 <- 0.04; r <- 0.10
d1 <- d0 * (1 + g1); d2 <- d0 * (1 + g1)^2
pn <- d2 * (1 + g2) / (r - g2)
ts_exp <- d1 / (1 + r) + (d2 + pn) / (1 + r)^2
check(
  "DDM two-stage n=2 closed form",
  approx_eq(.ddm_formula_two_stage(d0 = d0, g1 = g1, n = n1, g2 = g2, r = r), ts_exp, 1e-10)
)

# Gordon TV = F·(1+g)/(r−g)
tv_exp <- 100 * 1.03 / 0.07
check("Gordon TV closed form", approx_eq(dcf_gordon_tv(100, 0.03, 0.10), tv_exp, 1e-10))
tv_pv <- dcf_gordon_tv_pv(100, 0.03, 0.10, 1.10^5)
check("Gordon TV PV = TV/(1+r)^n", approx_eq(tv_pv$pv_tv, tv_exp / (1.10^5), 1e-10))

# DCF Gordon n=1 unit path: EV = F0/(1+r) * (1 + (1+g)/(r−g))
ev1 <- .dcf_formula_ev(n = 1, r1 = 0.10, g_term = 0.03, g_near = 0, f0 = 1)
ev1_exp <- 1 / 1.10 * (1 + 1.03 / 0.07)
check("DCF Gordon n=1 EV", approx_eq(ev1, ev1_exp, 1e-10))

# EV → equity bridge
check("EV bridge +cash −debt", approx_eq(dcf_ev_to_equity(1000, cash = 400, debt = 50), 1350, 1e-10))

# MOS = (FV − P) / FV
check("MOS (100,80)=0.20", approx_eq(mos_vs_fair_value(100, 80), 0.20, 1e-10))
check("MOS (50,80)=-0.60", approx_eq(mos_vs_fair_value(50, 80), -0.60, 1e-10))

# SGR identity: g% = Retention × ROE × 100
# NI=20, Equity=100 → ROE=0.2; Div=4 → payout=0.2; retention=0.8 → g=16%
sgr_id <- fundamental_sgr_identity(ni = 20, equity = 100, dividends = 4)
check("SGR identity g%=16", approx_eq(sgr_id$g_pct, 16, 1e-8))
check("SGR identity matches_formula", isTRUE(sgr_id$matches_formula))

# =============================================================================
# B. Three scenarios — DCF per-share / RI / MOS (base / bull / bear)
# =============================================================================
cat("\n== B. Three scenarios: DCF per-share, RI, MOS ==\n")

scenarios <- list(
  base = list(
    label = "base",
    cf = c(100, 105, 110),
    wacc = 0.09, g = 0.03, shares = 10, cash = 50, debt = 200,
    # Hand: path PV + Gordon TV on last CF
    # PV_cf = 100/1.09 + 105/1.09^2 + 110/1.09^3
    # TV = 110*1.03/(0.09-0.03); PV_TV = TV/1.09^3
    # EV = sum; Equity = EV+50-200; PS = Equity/10
    ri = list(b0 = 100, ke = 0.10, g = 0.03, n = 2L, payout = 0.4, roe = c(0.15, 0.15)),
    price = 80
  ),
  bull = list(
    label = "bull",
    cf = c(150, 165, 180),
    wacc = 0.08, g = 0.04, shares = 10, cash = 300, debt = 50,
    ri = list(b0 = 120, ke = 0.09, g = 0.03, n = 2L, payout = 0.3, roe = c(0.20, 0.18)),
    price = 70
  ),
  bear = list(
    label = "bear",
    cf = c(40, 38, 36),
    wacc = 0.12, g = 0.01, shares = 20, cash = 10, debt = 400,
    ri = list(b0 = 80, ke = 0.12, g = 0.02, n = 2L, payout = 0.6, roe = c(0.08, 0.07)),
    price = 60
  )
)

hand_dcf_ps <- function(cf, wacc, g, shares, cash, debt) {
  n <- length(cf)
  pv <- sum(cf / (1 + wacc)^seq_len(n))
  tv <- cf[n] * (1 + g) / (wacc - g)
  ev <- pv + tv / (1 + wacc)^n
  eq <- ev + cash - debt
  eq / shares
}

hand_ri <- function(b0, ke, g, n, payout, roe_path) {
  curr <- b0
  pv_ri <- 0
  ri_n <- NA_real_
  for (i in seq_len(n)) {
    ri <- (roe_path[i] - ke) * curr
    pv_ri <- pv_ri + ri / ((1 + ke)^i)
    ni <- curr * roe_path[i]
    curr <- curr + ni - ni * payout
    ri_n <- ri
  }
  tv <- ri_n * (1 + g) / (ke - g)
  pv_tv <- tv / ((1 + ke)^n)
  list(intrinsic = b0 + pv_ri + pv_tv, pv_ri = pv_ri, pv_terminal = pv_tv)
}

for (sc in scenarios) {
  lab <- sc$label
  # Engine via value_dcf_per_share (same Gordon path as closed form)
  eng <- value_dcf_per_share(
    sc$cf, claim = "fcff", wacc = sc$wacc, g_term = sc$g,
    shares = sc$shares, cash = sc$cash, debt = sc$debt
  )
  exp_ps <- hand_dcf_ps(sc$cf, sc$wacc, sc$g, sc$shares, sc$cash, sc$debt)
  check(
    paste0(lab, ": DCF per-share matches hand calc"),
    isTRUE(eng$ok) && approx_eq(eng$per_share, exp_ps, 1e-6)
  )

  ri_h <- hand_ri(sc$ri$b0, sc$ri$ke, sc$ri$g, sc$ri$n, sc$ri$payout, sc$ri$roe)
  ri_e <- compute_ri_valuation(
    b0 = sc$ri$b0, ke = sc$ri$ke, g = sc$ri$g, n = sc$ri$n,
    payout = sc$ri$payout, roe_path = sc$ri$roe
  )
  check(
    paste0(lab, ": RI intrinsic matches hand calc"),
    identical(ri_e$status, "success") && approx_eq(ri_e$intrinsic, ri_h$intrinsic, 1e-6)
  )
  check(
    paste0(lab, ": RI PV_RI sub-step matches"),
    approx_eq(ri_e$pv_ri, ri_h$pv_ri, 1e-6)
  )
  check(
    paste0(lab, ": RI PV_terminal sub-step matches"),
    approx_eq(ri_e$pv_terminal, ri_h$pv_terminal, 1e-6)
  )

  mos_e <- mos_vs_fair_value(eng$per_share, sc$price)
  mos_h <- (exp_ps - sc$price) / exp_ps
  check(
    paste0(lab, ": MOS vs DCF FV matches hand"),
    approx_eq(mos_e, mos_h, 1e-6)
  )
}

# Ordering sanity across scenarios
ps_base <- value_dcf_per_share(
  scenarios$base$cf, wacc = scenarios$base$wacc, g_term = scenarios$base$g,
  shares = scenarios$base$shares, cash = scenarios$base$cash, debt = scenarios$base$debt
)$per_share
ps_bull <- value_dcf_per_share(
  scenarios$bull$cf, wacc = scenarios$bull$wacc, g_term = scenarios$bull$g,
  shares = scenarios$bull$shares, cash = scenarios$bull$cash, debt = scenarios$bull$debt
)$per_share
ps_bear <- value_dcf_per_share(
  scenarios$bear$cf, wacc = scenarios$bear$wacc, g_term = scenarios$bear$g,
  shares = scenarios$bear$shares, cash = scenarios$bear$cash, debt = scenarios$bear$debt
)$per_share
check("scenario order bull > base > bear (DCF PS)", ps_bull > ps_base && ps_base > ps_bear)

# =============================================================================
# C. F-Score — 9 signals decomposed (excellent / mixed / poor)
# =============================================================================
cat("\n== C. F-Score 9-signal decomposition (3 scenarios) ==\n")

# Excellent: all 9 should pass
is_ex <- mk_stmt(
  `Total Revenue` = c(100, 90),
  `Gross Profit` = c(40, 33),
  `Net Income Common Stockholders` = c(50, 40),
  `Operating Income` = c(45, 38)
)
bs_ex <- mk_stmt(
  `Total Assets` = c(500, 480),
  `Long Term Debt` = c(80, 100),
  `Current Assets` = c(200, 150),
  `Current Liabilities` = c(100, 100),
  `Ordinary Shares Number` = c(100, 100)
)
cf_ex <- mk_stmt(
  `Operating Cash Flow` = c(60, 50),
  `Free Cash Flow` = c(40, 30)
)
fs_ex <- compute_report_f_score(is_ex, bs_ex, cf_ex)
check("excellent F-Score total == 9", identical(as.integer(fs_ex$total), 9L))
for (i in 1:9) {
  check(paste0("excellent signal ", i, " pass"), passed(fs_ex, i))
}

# Mixed / normal: ROA+, OCF+, but leverage up + dilution + GM down → partial
is_mx <- mk_stmt(
  `Total Revenue` = c(100, 95),
  `Gross Profit` = c(35, 38),          # GM down → p8 fail
  `Net Income Common Stockholders` = c(20, 18),
  `Operating Income` = c(22, 20)
)
bs_mx <- mk_stmt(
  `Total Assets` = c(400, 390),
  `Long Term Debt` = c(120, 100),      # leverage up → p5 fail
  `Current Assets` = c(180, 160),
  `Current Liabilities` = c(90, 90),   # CR up → p6 pass
  `Ordinary Shares Number` = c(120, 100) # dilution >2% → p7 fail
)
cf_mx <- mk_stmt(
  `Operating Cash Flow` = c(30, 25),   # OCF > OpInc → p4 pass
  `Free Cash Flow` = c(15, 12)
)
fs_mx <- compute_report_f_score(is_mx, bs_mx, cf_mx)
# Hand expectations:
# p1 ROA>0: 20/400>0 pass
# p2 OCF>0 pass
# p3 ROA up: 0.05 vs 18/390≈0.046 pass
# p4 OCF>OpInc: 30>22 pass
# p5 lev down: 120/400=0.3 vs 100/390≈0.256 fail
# p6 CR up: 2 vs 160/90≈1.78 pass
# p7 shares: 120 > 100*1.02 fail
# p8 GM up: 0.35 vs 38/95≈0.40 fail
# p9 AT up: 100/400=0.25 vs 95/390≈0.244 pass
# total = 1+1+1+1+0+1+0+0+1 = 6
check("mixed F-Score total == 6", identical(as.integer(fs_mx$total), 6L))
check("mixed p5 leverage fail", !passed(fs_mx, 5))
check("mixed p7 dilution fail", !passed(fs_mx, 7))
check("mixed p8 GM fail", !passed(fs_mx, 8))
check("mixed p1/p2/p4 pass", passed(fs_mx, 1) && passed(fs_mx, 2) && passed(fs_mx, 4))

# Poor: negative NI, neg OCF, falling metrics
is_poor <- mk_stmt(
  `Total Revenue` = c(80, 100),
  `Gross Profit` = c(20, 40),
  `Net Income Common Stockholders` = c(-10, 5),
  `Operating Income` = c(-8, 6)
)
bs_poor <- mk_stmt(
  `Total Assets` = c(500, 450),
  `Long Term Debt` = c(200, 100),
  `Current Assets` = c(80, 120),
  `Current Liabilities` = c(100, 80),
  `Ordinary Shares Number` = c(150, 100)
)
cf_poor <- mk_stmt(
  `Operating Cash Flow` = c(-20, 10),
  `Free Cash Flow` = c(-40, 0)
)
fs_poor <- compute_report_f_score(is_poor, bs_poor, cf_poor)
check("poor F-Score total < mixed", is.finite(fs_poor$total) && fs_poor$total < fs_mx$total)
check("poor p1 ROA fail (neg NI)", !passed(fs_poor, 1))
check("poor p2 OCF fail", !passed(fs_poor, 2))
check("excellent > mixed > poor totals", fs_ex$total > fs_mx$total && fs_mx$total > fs_poor$total)

# =============================================================================
# D. Beneish M-Score — 8 components + total (hand calc)
# =============================================================================
cat("\n== D. Beneish M-Score: 8 components + M (3 scenarios) ==\n")

# Base healthy wide row (same numbers as test_earnings_quality fin_wide)
fin_base <- data.frame(
  Sales_t = 100, Sales_t1 = 95,
  Receivables_t = 12, Receivables_t1 = 11.5,
  COGS_t = 60, COGS_t1 = 57,
  Gross_Profit_t = 40, Gross_Profit_t1 = 38,
  Current_Assets_t = 30, Current_Assets_t1 = 29,
  PPE_t = 20, PPE_t1 = 19.5,
  Total_Assets_t = 80, Total_Assets_t1 = 78,
  Depreciation_t = 4, Depreciation_t1 = 3.9,
  SGA_t = 12, SGA_t1 = 11.5,
  Current_Liabilities_t = 15, Current_Liabilities_t1 = 14.5,
  Long_Term_Debt_t = 8, Long_Term_Debt_t1 = 8,
  Net_Income_t = 12, OCF_t = 16,
  stringsAsFactors = FALSE
)

hand_beneish <- function(f) {
  dsri <- (f$Receivables_t / f$Sales_t) / (f$Receivables_t1 / f$Sales_t1)
  gm0 <- f$Gross_Profit_t / f$Sales_t
  gm1 <- f$Gross_Profit_t1 / f$Sales_t1
  gmi <- gm1 / gm0
  soft0 <- 1 - (f$Current_Assets_t + f$PPE_t) / f$Total_Assets_t
  soft1 <- 1 - (f$Current_Assets_t1 + f$PPE_t1) / f$Total_Assets_t1
  aqi <- soft0 / max(soft1, 1e-4)
  sgi <- f$Sales_t / max(f$Sales_t1, 1e-4)
  dr0 <- f$Depreciation_t / (f$Depreciation_t + f$PPE_t)
  dr1 <- f$Depreciation_t1 / (f$Depreciation_t1 + f$PPE_t1)
  depi <- dr1 / max(dr0, 1e-4)
  sgai <- (f$SGA_t / f$Sales_t) / (f$SGA_t1 / f$Sales_t1)
  tata <- (f$Net_Income_t - f$OCF_t) / f$Total_Assets_t
  lev0 <- (f$Current_Liabilities_t + f$Long_Term_Debt_t) / f$Total_Assets_t
  lev1 <- (f$Current_Liabilities_t1 + f$Long_Term_Debt_t1) / f$Total_Assets_t1
  lvgi <- lev0 / max(lev1, 1e-4)
  coef <- c(DSRI = 0.920, GMI = 0.528, AQI = 0.404, SGI = 0.892,
            DEPI = 0.115, SGAI = -0.172, TATA = 4.679, LVGI = -0.327)
  vals <- c(DSRI = dsri, GMI = gmi, AQI = aqi, SGI = sgi,
            DEPI = depi, SGAI = sgai, TATA = tata, LVGI = lvgi)
  m <- -4.84 + sum(coef * vals)
  list(comp = as.list(vals), m = m)
}

hb <- hand_beneish(fin_base)
ms_base <- calculate_m_score(fin_base, fill_neutral = TRUE)
for (nm in names(hb$comp)) {
  check(
    paste0("base Beneish ", nm, " matches hand"),
    approx_eq(ms_base[[nm]][1], hb$comp[[nm]], 1e-6)
  )
}
check("base Beneish M matches hand", approx_eq(ms_base$M_Score[1], hb$m, 1e-6))
check("base M below alert −1.78", isTRUE(ms_base$M_Score[1] < -1.78))

# Bull / excellent quality: even cleaner accruals (higher OCF), modest growth
fin_bull <- fin_base
fin_bull$OCF_t <- 20
fin_bull$Net_Income_t <- 12
fin_bull$Receivables_t <- 11
hb_bull <- hand_beneish(fin_bull)
ms_bull <- calculate_m_score(fin_bull)
check("bull Beneish M matches hand", approx_eq(ms_bull$M_Score[1], hb_bull$m, 1e-6))
check("bull M lower (better) than base", ms_bull$M_Score[1] < ms_base$M_Score[1])

# Bear / manipulative: AR surge, NI >> OCF, leverage up
fin_bear <- fin_base
fin_bear$Sales_t <- 110
fin_bear$Receivables_t <- 40
fin_bear$Net_Income_t <- 30
fin_bear$OCF_t <- 2
fin_bear$Current_Liabilities_t <- 25
fin_bear$Long_Term_Debt_t <- 40
fin_bear$Total_Assets_t <- 120
fin_bear$Current_Assets_t <- 70
fin_bear$PPE_t <- 15
hb_bear <- hand_beneish(fin_bear)
ms_bear <- calculate_m_score(fin_bear)
check("bear Beneish M matches hand", approx_eq(ms_bear$M_Score[1], hb_bear$m, 1e-6))
check("bear DSRI >> 1", isTRUE(ms_bear$DSRI[1] > 2))
check("bear TATA positive (NI>OCF)", isTRUE(ms_bear$TATA[1] > 0))
check("bear M worse than base", ms_bear$M_Score[1] > ms_base$M_Score[1])
check(
  "scenario M order bull < base < bear",
  ms_bull$M_Score[1] < ms_base$M_Score[1] && ms_base$M_Score[1] < ms_bear$M_Score[1]
)

# =============================================================================
# E. FCFE conversion + value_dcf_per_share FCFF identity
# =============================================================================
cat("\n== E. FCFE conversion identity ==\n")

fcfe_h1 <- 100 - 10 + 0.05 * 200
fcfe_h2 <- 110 - 10 + 0.05 * 210
fcfe_path <- fcff_to_fcfe(c(100, 110), interest_after_tax = 10, debt0 = 200, g_path = 0.05)
check("FCFE y1 hand", approx_eq(fcfe_path[1], fcfe_h1, 1e-10))
check("FCFE y2 hand", approx_eq(fcfe_path[2], fcfe_h2, 1e-10))

# =============================================================================
# Summary
# =============================================================================
cat("\n== Formula Accuracy Audit summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) {
  quit(status = 1L)
}
cat("ALL CONTROLS PASSED\n")
