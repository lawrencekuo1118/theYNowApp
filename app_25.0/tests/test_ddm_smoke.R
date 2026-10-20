#!/usr/bin/env Rscript
# Offline smoke: DDM hot path (formulas + module wiring). No network.
# Asserts current outputs — does not change valuation formulas.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ddm_smoke.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_25.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("setup.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("setup.R", local = FALSE)
source("ddm_module.R", local = FALSE)

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

check("ddm_module_server exported", exists("ddm_module_server", mode = "function"))
check(".ddm_formula_p0 exported", exists(".ddm_formula_p0", mode = "function"))
check(".ddm_formula_spm exported", exists(".ddm_formula_spm", mode = "function"))
check(".ddm_formula_two_stage exported", exists(".ddm_formula_two_stage", mode = "function"))

# Gordon path used by ddm_calc (inline) matches helper
d0 <- 2; g <- 0.04; ke <- 0.09
d1 <- d0 * (1 + g)
p_inline <- d1 / (ke - g)
p_helper <- .ddm_formula_p0(d0 = d0, g = g, r = ke)
check("Gordon helper matches module inline", approx_eq(p_helper, p_inline))
check("Gordon StockFeel D0=1 g=5% r=10% → 21", approx_eq(.ddm_formula_p0(1, 0.05, 0.10), 21))
check("Gordon NA when r<=g", !is.finite(.ddm_formula_p0(1, 0.10, 0.10)))
check("Gordon ke alias", approx_eq(.ddm_formula_p0(1, 0.05, ke = 0.10), 21))

# SPM path (module rounds to 2 dp on success)
p_spm <- .ddm_formula_spm(eps = 2, d = 1, g = 0.05, r = 0.10)
check("SPM closed form", approx_eq(p_spm, 2 * 0.05 / 0.01 + 1 / 0.10))
check("SPM module round(p0, 2) stable", approx_eq(round(p_spm, 2), 20))
check("SPM NA when r<=0", !is.finite(.ddm_formula_spm(2, 1, 0.05, 0)))
check("SPM finite when r<g", is.finite(.ddm_formula_spm(2, 1, 0.12, 0.10)))

# Two-stage path
p_ts <- .ddm_formula_two_stage(d0 = 1, g1 = 0.08, n = 2L, g2 = 0.04, r = 0.10)
d1ts <- 1 * 1.08; d2ts <- 1 * 1.08^2
pn <- d2ts * 1.04 / (0.10 - 0.04)
p_ts_closed <- d1ts / 1.10 + (d2ts + pn) / 1.10^2
check("two-stage n=2 closed form", approx_eq(p_ts, p_ts_closed, 1e-10))
check("two-stage g1=g2 equals Gordon", approx_eq(
  .ddm_formula_two_stage(1, 0.05, 5L, 0.05, 0.10),
  .ddm_formula_p0(1, 0.05, 0.10),
  1e-10
))
check("two-stage NA when r<=g2", !is.finite(.ddm_formula_two_stage(1, 0.08, 3L, 0.10, 0.10)))

src <- paste(readLines("ddm_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module wires two_stage helper", grepl(".ddm_formula_two_stage(", src, fixed = TRUE))
check("module wires spm helper", grepl(".ddm_formula_spm(", src, fixed = TRUE))
check("module has gordon/spm/two_stage modes", grepl('"gordon"', src, fixed = TRUE) &&
  grepl('"spm"', src, fixed = TRUE) && grepl('"two_stage"', src, fixed = TRUE))
check("module rejects d0<=0 for Gordon", grepl("ddm_err_d0", src, fixed = TRUE))
check("module rejects ke<=g for Gordon", grepl("ddm_err_r_le_g", src, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All DDM smoke checks passed.\n")
