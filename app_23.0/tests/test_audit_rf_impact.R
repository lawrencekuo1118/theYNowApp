#!/usr/bin/env Rscript
# Acceptance — CAPM Rf fallback vs alternate Rf: Ke / WACC / DCF FV impact.
# Offline closed-form; does NOT claim live Treasury success.
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

# Documented engineering fallback used by .current_rf_detail / default_config
# when live Rf is unavailable (default_rf <- 5.0). Avoid sourcing full
# default_config.R here (needs industry_standards).
rf_fallback_pct <- 5.0
# Alternate "live-like" Treasury level for sensitivity (not a live scrape claim)
rf_live_like_pct <- 4.25
beta <- 1.20
rm_pct <- 9.0
rd <- 0.05
tax <- 0.21
we <- 0.70
wd <- 0.30
fcf <- c(100, 105, 110)
g_term <- 0.03
shares <- 10
cash <- 0
debt <- 0

cat("== Rf impact fixture ==\n")
cat("rf_fallback_pct=", rf_fallback_pct, " rf_live_like_pct=", rf_live_like_pct, "\n", sep = "")

ke_fb <- capm_cost_of_equity(rf = rf_fallback_pct / 100, beta = beta, rm = rm_pct / 100)
ke_lv <- capm_cost_of_equity(rf = rf_live_like_pct / 100, beta = beta, rm = rm_pct / 100)
check("fallback CAPM Ke ok", isTRUE(ke_fb$ok) && is.finite(ke_fb$ke))
check("live-like CAPM Ke ok", isTRUE(ke_lv$ok) && is.finite(ke_lv$ke))

ke_fb_exp <- (rf_fallback_pct + beta * (rm_pct - rf_fallback_pct)) / 100
ke_lv_exp <- (rf_live_like_pct + beta * (rm_pct - rf_live_like_pct)) / 100
check("fallback Ke matches CAPM identity", approx_eq(ke_fb$ke, ke_fb_exp, 1e-10))
check("live-like Ke matches CAPM identity", approx_eq(ke_lv$ke, ke_lv_exp, 1e-10))
check(
  "higher Rf (fallback 5 vs 4.25) raises Ke when Rm fixed",
  ke_fb$ke > ke_lv$ke
)

w_fb <- compute_wacc(re = ke_fb$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
w_lv <- compute_wacc(re = ke_lv$ke, rd = rd, tax_ratio = tax, we = we, wd = wd)
check("fallback WACC ok", isTRUE(w_fb$ok) && is.finite(w_fb$wacc))
check("live-like WACC ok", isTRUE(w_lv$ok) && is.finite(w_lv$wacc))
check("higher Ke raises WACC (We>0)", w_fb$wacc > w_lv$wacc)

dcf_fb <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_fb$wacc, g_term = g_term,
  shares = shares, cash = cash, debt = debt
)
dcf_lv <- value_dcf_per_share(
  fcf, claim = "fcff", wacc = w_lv$wacc, g_term = g_term,
  shares = shares, cash = cash, debt = debt
)
check("fallback DCF FV ok", isTRUE(dcf_fb$ok) && is.finite(dcf_fb$per_share))
check("live-like DCF FV ok", isTRUE(dcf_lv$ok) && is.finite(dcf_lv$per_share))
check(
  "higher WACC (from fallback Rf) lowers DCF per-share FV",
  dcf_fb$per_share < dcf_lv$per_share
)

# FCFE path: discount at Ke directly
dcf_fcfe_fb <- value_dcf_per_share(
  fcf, claim = "fcfe", ke = ke_fb$ke, g_term = g_term,
  shares = shares, cash = cash, debt = debt
)
dcf_fcfe_lv <- value_dcf_per_share(
  fcf, claim = "fcfe", ke = ke_lv$ke, g_term = g_term,
  shares = shares, cash = cash, debt = debt
)
check("fallback FCFE FV ok", isTRUE(dcf_fcfe_fb$ok) && is.finite(dcf_fcfe_fb$per_share))
check(
  "higher Ke lowers FCFE per-share FV",
  isTRUE(dcf_fcfe_lv$ok) && dcf_fcfe_fb$per_share < dcf_fcfe_lv$per_share
)

delta_ke_bps <- (ke_fb$ke - ke_lv$ke) * 10000
delta_wacc_bps <- (w_fb$wacc - w_lv$wacc) * 10000
delta_ps_pct <- (dcf_fb$per_share / dcf_lv$per_share - 1) * 100
cat("\n== Numeric deltas (fallback vs live-like) ==\n")
cat(sprintf("ΔKe = %+.2f bps\n", delta_ke_bps))
cat(sprintf("ΔWACC = %+.2f bps\n", delta_wacc_bps))
cat(sprintf("ΔDCF per-share = %+.2f%%\n", delta_ps_pct))
cat(sprintf(
  "FV fallback=%.4f live-like=%.4f\n",
  dcf_fb$per_share, dcf_lv$per_share
))

check("ΔKe is material (>1 bp for 75 bp Rf gap × β)", abs(delta_ke_bps) > 1)
check("ΔFV% is material (not identical)", abs(delta_ps_pct) > 0.05)

# Fallback flag contract: documented numeric rate used when live fails
check(
  "documented engineering fallback Rf seed is finite positive",
  is.finite(rf_fallback_pct) && rf_fallback_pct > 0
)
# Confirm server / default_config still document the same 5% family
cfg_txt <- paste(readLines("default_config.R", warn = FALSE), collapse = "\n")
check(
  "default_config documents default_rf <- 5.0 fallback",
  grepl("default_rf\\s*<-\\s*5\\.0", cfg_txt)
)

cat("\n== Rf impact summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
cat("NOTE: live Treasury scrape success is NOT claimed by this suite.\n")
if (fail > 0L) quit(status = 1)
cat("ALL RF IMPACT CHECKS PASSED\n")
quit(status = 0)
