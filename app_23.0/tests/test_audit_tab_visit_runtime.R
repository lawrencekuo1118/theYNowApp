#!/usr/bin/env Rscript
# Acceptance — tab-visit auto-calc gate runtime (not grepl-only).
# Exercises pure gate helpers + fire callback; proves wait / skip / fire paths.
# Does NOT boot full Shiny; browser E2E remains a separate evidence layer.
#
# Run: cd app_23.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_audit_tab_visit_runtime.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("ynow_server.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

source("tab_visit_calc_gates.R", local = TRUE, encoding = "UTF-8")

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

# --- DCF gates ---
check(
  "DCF fires when searched + ready + FV missing",
  isTRUE(tab_visit_dcf_should_fire("dcf_calculator", TRUE, FALSE, TRUE))
)
check(
  "DCF waits when inputs not ready",
  !isTRUE(tab_visit_dcf_should_fire("dcf_calculator", TRUE, FALSE, FALSE))
)
check(
  "DCF skips when FV already present",
  !isTRUE(tab_visit_dcf_should_fire("dcf_calculator", TRUE, TRUE, TRUE))
)
check(
  "DCF ignores wrong tab",
  !isTRUE(tab_visit_dcf_should_fire("smart_analysis", TRUE, FALSE, TRUE))
)
check(
  "DCF waits before search",
  !isTRUE(tab_visit_dcf_should_fire("dcf_calculator", FALSE, FALSE, TRUE))
)

# --- Companion gates ---
check(
  "DDM companion key",
  identical(tab_visit_companion_key("ddm_calculator"), "ddm")
)
check(
  "RI companion key",
  identical(tab_visit_companion_key("ri_calculator"), "ri")
)
check(
  "MULT companion keys",
  identical(tab_visit_companion_key("rel_multiples_calculator"), "multiples") &&
    identical(tab_visit_companion_key("rel_mult_ev"), "multiples") &&
    identical(tab_visit_companion_key("rel_mult_rev"), "multiples")
)
check(
  "Companion fires when ready",
  isTRUE(tab_visit_companion_should_fire("ddm_calculator", TRUE, TRUE))
)
check(
  "Companion waits when not ready",
  !isTRUE(tab_visit_companion_should_fire("ri_calculator", TRUE, FALSE))
)

# --- Observable fire callback (runtime of decide_and_fire) ---
fired <- character(0)
fire_fn <- function(k) fired <<- c(fired, k)

d1 <- tab_visit_decide_and_fire("dcf_calculator", TRUE, FALSE, TRUE, fire_fn)
check("decide DCF fires once", isTRUE(d1$fired) && identical(d1$key, "dcf") &&
        identical(fired, "dcf"))

fired <- character(0)
d2 <- tab_visit_decide_and_fire("dcf_calculator", TRUE, TRUE, TRUE, fire_fn)
check("decide DCF skip preserves no fire", !isTRUE(d2$fired) &&
        identical(d2$reason, "fv_present") && length(fired) == 0L)

fired <- character(0)
d3 <- tab_visit_decide_and_fire("dcf_calculator", TRUE, FALSE, FALSE, fire_fn)
check("decide DCF not_ready no fire", !isTRUE(d3$fired) &&
        identical(d3$reason, "not_ready") && length(fired) == 0L)

fired <- character(0)
d4 <- tab_visit_decide_and_fire("ddm_calculator", TRUE, FALSE, TRUE, fire_fn)
d5 <- tab_visit_decide_and_fire("ri_calculator", TRUE, FALSE, TRUE, fire_fn)
d6 <- tab_visit_decide_and_fire("rel_multiples_calculator", TRUE, FALSE, TRUE, fire_fn)
check(
  "decide companion DDM/RI/MULT each fire",
  isTRUE(d4$fired) && isTRUE(d5$fired) && isTRUE(d6$fired) &&
    identical(fired, c("ddm", "ri", "multiples"))
)

# --- Server still wires helpers (structural bridge to production observers) ---
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "ynow_server uses tab_visit_dcf_should_fire",
  grepl("tab_visit_dcf_should_fire\\(", srv)
)
check(
  "ynow_server uses tab_visit_companion_key",
  grepl("tab_visit_companion_key\\(", srv)
)

cat("\n== Tab-visit runtime gate summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
cat("ALL TAB-VISIT GATE CHECKS PASSED\n")
quit(status = 0)
