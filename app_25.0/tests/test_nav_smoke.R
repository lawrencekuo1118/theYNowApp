#!/usr/bin/env Rscript
# Offline smoke: pure NAV hot path (formula + holdco extract + module wiring).
# Asserts current outputs — does not change valuation formulas.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_nav_smoke.R

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
source("nav_module.R", local = FALSE)

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

check("nav_module_server exported", exists("nav_module_server", mode = "function"))
check("nav_module_ui exported", exists("nav_module_ui", mode = "function"))
check(".nav_formula_p exported", exists(".nav_formula_p", mode = "function"))
check("extract_nav_components exported", exists("extract_nav_components", mode = "function"))

# Fair price = NAVPS × multiple (module band uses this for low/mid/high)
check("NAV mid 1.0×", approx_eq(.nav_formula_p(20, 1.0), 20))
check("NAV low 0.90×", approx_eq(.nav_formula_p(20, 0.90), 18))
check("NAV high 1.05×", approx_eq(.nav_formula_p(20, 1.05), 21))
check("NAV NA when navps<=0", !is.finite(.nav_formula_p(0, 1)) && !is.finite(.nav_formula_p(-1, 1)))
check("NAV NA when multiple<=0", !is.finite(.nav_formula_p(20, 0)) && !is.finite(.nav_formula_p(20, -0.5)))
check("NAV NA inputs", !is.finite(.nav_formula_p(NA_real_, 1)) && !is.finite(.nav_formula_p(20, NA_real_)))

df_nav <- data.frame(
  Metric = c("Common Stock Equity", "Long Term Investments", "Cash And Cash Equivalents"),
  `12/31/2024` = c(1000, 400, 50),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
nav_disc <- extract_nav_components(df_nav, holdco_discount = 0.20)
check("holdco NAV = equity − 20%×inv", approx_eq(nav_disc$nav, 1000 - 0.20 * 400))
check("holdco investments identified", approx_eq(nav_disc$investments, 400))
check("holdco equity", approx_eq(nav_disc$equity, 1000))

nav_zero <- extract_nav_components(df_nav, holdco_discount = 0)
check("discount 0 → NAV = equity", approx_eq(nav_zero$nav, 1000))

nav_clamp <- extract_nav_components(df_nav, holdco_discount = 0.90)
check("discount clamped to 50%", approx_eq(nav_clamp$discount, 0.5) &&
  approx_eq(nav_clamp$nav, 1000 - 0.5 * 400))

empty <- extract_nav_components(NULL, 0.2)
check("NULL BS → empty nav", !is.finite(empty$nav) && identical(empty$discount, 0))

df_no_eq <- data.frame(
  Metric = "Long Term Investments",
  `12/31/2024` = 400,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
no_eq <- extract_nav_components(df_no_eq, 0.2)
check("missing equity → nav NA", !is.finite(no_eq$nav))

# Per-share path used by module: NAV / shares → .nav_formula_p
navps <- nav_disc$nav / 50
check("NAVPS×mid matches fair", approx_eq(.nav_formula_p(navps, 1.0), navps))

src <- paste(readLines("nav_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module uses .nav_formula_p for band", grepl(".nav_formula_p(", src, fixed = TRUE))
check("module syncs via extract_nav_components", grepl("extract_nav_components(", src, fixed = TRUE))
check("module tab is nav_calculator", grepl('"nav_calculator"', src, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All NAV smoke checks passed.\n")
