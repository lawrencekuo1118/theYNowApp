#!/usr/bin/env Rscript
# Acceptance — CAPM Rf fallback vs alternate Rf: Ke / WACC / DCF FV impact.
# Offline closed-form; does NOT claim live Treasury success.
#
# Important CAPM identity (Rm fixed):
#   Ke = Rf + β(Rm − Rf) = β·Rm + Rf·(1 − β)
# When β > 1, a higher Rf *lowers* Ke (ERP shrinks faster than Rf rises).
# When β < 1, a higher Rf raises Ke. Tests cover both.
#
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_audit_rf_impact.R

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

source("setup.R", local = FALSE)
source("valuation_guard.R", local = FALSE)

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
approx_eq <- function(a, b, tol = 1e-8) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

rf_fallback_pct <- 5.0
rf_live_like_pct <- 4.25
rm_pct <- 9.0
rd <- 0.05
tax <- 0.21
we <- 0.70
wd <- 0.30
fcf <- c(100, 105, 110)
g_term <- 0.03
shares <- 10

cat("== Rf impact fixture ==\n")
cat("rf_fallback_pct=", rf_fallback_pct, " rf_live_like_pct=", rf_live_like_pct, "\n", sep = "")

path_for_beta <- function(beta) {
  ke_fb <- capm_cost_of_equity(rf = rf_fallback_pct / 100, beta = beta, rm = rm_pct / 100)
  ke_lv <- capm_cost_of_equity(rf = rf_live_like_pct / 100, beta = beta, rm = rm_pct / 100)
  list(
    ke_fb = ke_fb, ke_lv = ke_lv,
    ke_fb_exp = (rf_fallback_pct + beta * (rm_pct - rf_fallback_pct)) / 100,
    ke_lv_exp = (rf_live_like_pct + beta * (rm_pct - rf_live_like_pct)) / 100,
    d_ke_identity = (1 - beta) * (rf_fallback_pct - rf_live_like_pct) / 100
  )
}

# --- β > 1: higher Rf lowers Ke ---
beta_hi <- 1.20
p_hi <- path_for_beta(beta_hi)
check("β>1 fallback CAPM Ke ok", isTRUE(p_hi$ke_fb$ok) && is.finite(p_hi$ke_fb$ke))
check("β>1 live-like CAPM Ke ok", isTRUE(p_hi$ke_lv$ok) && is.finite(p_hi$ke_lv$ke))
check("β>1 fallback Ke matches CAPM identity", approx_eq(p_hi$ke_fb$ke, p_hi$ke_fb_exp, 1e-10))
check("β>1 live-like Ke matches CAPM identity", approx_eq(p_hi$ke_lv$ke, p_hi$ke_lv_exp, 1e-10))
check(
  "β>1: higher Rf (fallback) lowers Ke when Rm fixed",
  p_hi$ke_fb$ke < p_hi$ke_lv$ke &&
    approx_eq(p_hi$ke_fb$ke - p_hi$ke_lv$ke, p_hi$d_ke_identity, 1e-10)
)

w_fb_hi <- compute_wacc(re = p_hi$ke_fb$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
w_lv_hi <- compute_wacc(re = p_hi$ke_lv$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
check("β>1 fallback WACC ok", isTRUE(w_fb_hi$ok) && is.finite(w_fb_hi$wacc))
check("β>1: lower Ke lowers WACC (We>0)", w_fb_hi$wacc < w_lv_hi$wacc)

dcf_fb_hi <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_fb_hi$wacc, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
dcf_lv_hi <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_lv_hi$wacc, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
check("β>1 fallback DCF FV ok", isTRUE(dcf_fb_hi$ok) && is.finite(dcf_fb_hi$per_share))
check(
  "β>1: lower WACC (from higher fallback Rf) raises DCF per-share FV",
  isTRUE(dcf_lv_hi$ok) && dcf_fb_hi$per_share > dcf_lv_hi$per_share
)

dcf_fcfe_fb <- value_dcf_per_share(
  fcf, claim = "fcfe", ke = p_hi$ke_fb$ke, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
dcf_fcfe_lv <- value_dcf_per_share(
  fcf, claim = "fcfe", ke = p_hi$ke_lv$ke, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
check("β>1 fallback FCFE FV ok", isTRUE(dcf_fcfe_fb$ok) && is.finite(dcf_fcfe_fb$per_share))
check(
  "β>1: lower Ke raises FCFE per-share FV",
  isTRUE(dcf_fcfe_lv$ok) && dcf_fcfe_fb$per_share > dcf_fcfe_lv$per_share
)

# --- β < 1: higher Rf raises Ke ---
beta_lo <- 0.80
p_lo <- path_for_beta(beta_lo)
check("β<1 fallback Ke matches identity", approx_eq(p_lo$ke_fb$ke, p_lo$ke_fb_exp, 1e-10))
check(
  "β<1: higher Rf (fallback) raises Ke when Rm fixed",
  p_lo$ke_fb$ke > p_lo$ke_lv$ke &&
    approx_eq(p_lo$ke_fb$ke - p_lo$ke_lv$ke, p_lo$d_ke_identity, 1e-10)
)
w_fb_lo <- compute_wacc(re = p_lo$ke_fb$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
w_lv_lo <- compute_wacc(re = p_lo$ke_lv$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
dcf_fb_lo <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_fb_lo$wacc, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
dcf_lv_lo <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_lv_lo$wacc, g_term = g_term,
  shares = shares, cash = 0, debt = 0
)
check(
  "β<1: higher Ke raises WACC and lowers DCF FV",
  isTRUE(w_fb_lo$ok) && isTRUE(w_lv_lo$ok) &&
    w_fb_lo$wacc > w_lv_lo$wacc &&
    isTRUE(dcf_fb_lo$ok) && isTRUE(dcf_lv_lo$ok) &&
    dcf_fb_lo$per_share < dcf_lv_lo$per_share
)

delta_ke_bps <- (p_hi$ke_fb$ke - p_hi$ke_lv$ke) * 10000
delta_wacc_bps <- (w_fb_hi$wacc - w_lv_hi$wacc) * 10000
delta_ps_pct <- (dcf_fb_hi$per_share / dcf_lv_hi$per_share - 1) * 100
cat("\n== Numeric deltas β=1.2 (fallback vs live-like) ==\n")
cat(sprintf("ΔKe = %+.2f bps (sign follows (1−β)·ΔRf)\n", delta_ke_bps))
cat(sprintf("ΔWACC = %+.2f bps\n", delta_wacc_bps))
cat(sprintf("ΔDCF per-share = %+.2f%%\n", delta_ps_pct))
cat(sprintf(
  "FV fallback=%.4f live-like=%.4f\n",
  dcf_fb_hi$per_share, dcf_lv_hi$per_share
))

check("ΔKe is material (|Δ| > 1 bp)", abs(delta_ke_bps) > 1)
check("ΔFV% is material (not identical)", abs(delta_ps_pct) > 0.05)
check(
  "documented engineering fallback Rf seed is finite positive",
  is.finite(rf_fallback_pct) && rf_fallback_pct > 0
)
cfg_txt <- paste(readLines("default_config.R", warn = FALSE), collapse = "\n")
check(
  "default_config documents default_rf <- 5.0 fallback",
  grepl("default_rf\\s*<-\\s*5\\.0", cfg_txt)
)

cat("\n== Rf impact summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
cat("NOTE: live Treasury scrape success is NOT claimed by this suite.\n")
cat("NOTE: fallback Rf impact on Ke depends on β vs 1 when Rm is held fixed.\n")
if (fail > 0L) quit(status = 1)
cat("ALL RF IMPACT CHECKS PASSED\n")
quit(status = 0)
