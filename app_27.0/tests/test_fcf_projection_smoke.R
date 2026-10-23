#!/usr/bin/env Rscript
# Offline smoke: deepen FCF projection / CapEx sync beyond test_fcf_capex_sync.R.
# Asserts current outputs — does not change valuation formulas.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_fcf_projection_smoke.R

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
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("setup.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("setup.R", local = FALSE)
source("fcf_projection_module.R", local = FALSE)

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

check("fcf_projection_module_server exported", exists("fcf_projection_module_server", mode = "function"))
check("fcf_projection_module_ui exported", exists("fcf_projection_module_ui", mode = "function"))
check("fcf_sync_capex_revenue_margin exported", exists("fcf_sync_capex_revenue_margin", mode = "function"))
check(".dcf_unit_fcff_path exported", exists(".dcf_unit_fcff_path", mode = "function"))

# CapEx sync edge fixtures (complement test_fcf_capex_sync.R)
empty <- fcf_sync_capex_revenue_margin(numeric(0), numeric(0))
check("empty hist → not ok", isFALSE(empty$ok) && !isTRUE(empty$spike))

bad_rev <- fcf_sync_capex_revenue_margin(c(10, 10), c(0, 100))
check("zero latest rev → not ok", isFALSE(bad_rev$ok))

# Borderline spike: latest = exactly mult × prior mean → NOT spike (strict >)
# prior years 10/100=0.10, 10/100=0.10 → prior mean 0.10; latest 13.5/100=0.135 = 1.35×
border <- fcf_sync_capex_revenue_margin(
  c(13.5, 10, 10), c(100, 100, 100),
  list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("exact mult borderline not spike", isFALSE(border$spike) && approx_eq(border$pct, 13.5, 0.01))

# Just over threshold → spike → seeds 3y avg
over <- fcf_sync_capex_revenue_margin(
  c(14, 10, 10), c(100, 100, 100),
  list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("over mult is spike", isTRUE(over$spike))
check("spike seeds 3y avg ~11.333%", approx_eq(over$pct, (14 + 10 + 10) / 3, 0.05))

# Unit FCFF path (projection engine under FCF tab)
fcff1 <- .dcf_unit_fcff_path(
  n = 1, g_near = 0.10, nopat_m = 0.20, depre_m = 0.05, capex_m = 0.08, nwc_m = 0.10
)
rev1 <- 1.10
fcff1_closed <- rev1 * (0.20 + 0.05 - 0.08) - (rev1 - 1) * 0.10
check("unit FCFF y1 NOPAT path", approx_eq(fcff1[1], fcff1_closed))

fcff3 <- .dcf_unit_fcff_path(
  n = 3, g_near = 0.05, nopat_m = 0.15, depre_m = 0.04, capex_m = 0.06, nwc_m = 0.02
)
check("unit FCFF length 3", length(fcff3) == 3L && all(is.finite(fcff3)))
# y1: rev=1.05, FCFF = 1.05*(0.15+0.04-0.06) - 0.05*0.02
check("unit FCFF y1 closed", approx_eq(fcff3[1], 1.05 * 0.13 - 0.05 * 0.02))

# EV from projected FCFF + Gordon TV (same engine as DCF overview)
ev <- .dcf_formula_ev_from_fcff(fcff3, r1 = 0.10, g_term = 0.03)
tv <- fcff3[3] * 1.03 / (0.10 - 0.03)
dfs <- 1.10^seq_len(3)
ev_closed <- sum(fcff3 / dfs) + tv / dfs[3]
check("EV from FCFF path + TV", approx_eq(ev, ev_closed, 1e-8))
check("EV blocked when g>=r", !is.finite(.dcf_formula_ev_from_fcff(fcff3, 0.10, 0.10)))

src <- paste(readLines("fcf_projection_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module exposes CapEx spike controls", grepl("apply_capex_spike_smooth", src, fixed = TRUE) &&
  grepl("capex_spike_mult", src, fixed = TRUE))
check("module calls fcf_sync_capex_revenue_margin", grepl("fcf_sync_capex_revenue_margin(", src, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All FCF projection smoke checks passed.\n")
