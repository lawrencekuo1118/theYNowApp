#!/usr/bin/env Rscript
# HFV Chart overlay / Replay model pickers follow sidebar Fair Value order.
# Run: cd app_28.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_hfv_fv_engine_order.R

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
source(file.path(app_dir, "market_profile.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

want <- c("nav", "pb", "dcf", "ddm", "ri")
check("YNOW_FV_ENGINE_ORDER sidebar-aligned", identical(YNOW_FV_ENGINE_ORDER, want))
ch <- ynow_fv_engine_choices()
check("choices values order", identical(unname(ch), want))
check("choices labels", identical(names(ch), c("NAV", "Justified P/B", "DCF", "DDM", "RI")))
check(
  "order helper shuffles",
  identical(ynow_order_fv_engines(c("pb", "nav", "ri", "dcf")), c("nav", "pb", "dcf", "ri"))
)
check("order helper drops multiples/sotp", identical(ynow_order_fv_engines(c("multiples", "sotp", "dcf")), "dcf"))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASS\n")
