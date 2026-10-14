#!/usr/bin/env Rscript
# Acceptance — Shiny reactive runtime for tab-visit auto-calc gates.
# Proves observeEvent wiring fires when FV is empty and skips when FV exists,
# using the same helpers as ynow_server.R (not grepl-only, not browser-only).
#
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_audit_tab_visit_shiny_runtime.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("tab_visit_calc_gates.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

suppressPackageStartupMessages(library(shiny))
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

# Minimal server mirroring production tab-visit observers (gates + fire only).
server <- function(input, output, session) {
  user_has_searched <- reactiveVal(FALSE)
  stock_price_estimate_val <- reactiveVal(NULL)
  ready_map <- reactiveVal(list(dcf = FALSE, ddm = FALSE, ri = FALSE, multiples = FALSE))
  fire_log <- reactiveVal(character(0))

  .ready <- function(key) {
    m <- isolate(ready_map())
    isTRUE(m[[as.character(key)[1]]])
  }
  .fire <- function(key) {
    fire_log(c(isolate(fire_log()), as.character(key)[1]))
  }

  observeEvent(input$sidebar_tabs, {
    tab <- as.character(input$sidebar_tabs %||% "")[1]
    has_ps <- is.finite(suppressWarnings(as.numeric(isolate(stock_price_estimate_val()))[1]))
    if (!isTRUE(tab_visit_dcf_should_fire(
      tab,
      isolate(user_has_searched()),
      has_ps,
      .ready("dcf")
    ))) {
      return()
    }
    .fire("dcf")
  }, ignoreInit = TRUE)

  observeEvent(input$sidebar_tabs, {
    tab <- as.character(input$sidebar_tabs %||% "")[1]
    key <- tab_visit_companion_key(tab)
    if (!nzchar(key)) return()
    if (!isTRUE(tab_visit_companion_should_fire(
      tab,
      isolate(user_has_searched()),
      .ready(key)
    ))) {
      return()
    }
    .fire(key)
  }, ignoreInit = TRUE)

  list(
    user_has_searched = user_has_searched,
    stock_price_estimate_val = stock_price_estimate_val,
    ready_map = ready_map,
    fire_log = fire_log
  )
}

shiny::testServer(server, {
  # Initial: not searched → no fire
  session$setInputs(sidebar_tabs = "dcf_calculator")
  check("Shiny: no fire before search", identical(fire_log(), character(0)))

  user_has_searched(TRUE)
  session$setInputs(sidebar_tabs = "smart_analysis")
  session$setInputs(sidebar_tabs = "dcf_calculator")
  check("Shiny: no fire when DCF not ready", identical(fire_log(), character(0)))

  # Ready + FV empty → fire on DCF visit
  ready_map(list(dcf = TRUE, ddm = TRUE, ri = TRUE, multiples = TRUE))
  stock_price_estimate_val(NULL)
  session$setInputs(sidebar_tabs = "get_started")
  session$setInputs(sidebar_tabs = "dcf_calculator")
  check(
    "Shiny: DCF fires when FV empty and ready",
    identical(fire_log(), "dcf")
  )

  # FV present → skip unnecessary DCF recalc
  stock_price_estimate_val(201.79)
  session$setInputs(sidebar_tabs = "smart_analysis")
  session$setInputs(sidebar_tabs = "dcf_calculator")
  check(
    "Shiny: DCF skips when FV already present",
    identical(fire_log(), "dcf")
  )

  # Companion tabs fire when ready (even if FV present — matches server design)
  n0 <- length(fire_log())
  session$setInputs(sidebar_tabs = "ddm_calculator")
  session$setInputs(sidebar_tabs = "ri_calculator")
  session$setInputs(sidebar_tabs = "rel_multiples_calculator")
  check(
    "Shiny: companion DDM/RI/MULT each fire once",
    identical(fire_log(), c("dcf", "ddm", "ri", "multiples")) &&
      length(fire_log()) == n0 + 3L
  )

  # Clear FV again and re-visit DCF → second DCF fire (heal path)
  stock_price_estimate_val(NULL)
  session$setInputs(sidebar_tabs = "dashboard")
  session$setInputs(sidebar_tabs = "dcf_calculator")
  check(
    "Shiny: DCF re-fires after FV cleared (heal path)",
    identical(fire_log(), c("dcf", "ddm", "ri", "multiples", "dcf"))
  )

  # Not-ready companion waits
  ready_map(list(dcf = TRUE, ddm = FALSE, ri = FALSE, multiples = FALSE))
  n1 <- length(fire_log())
  session$setInputs(sidebar_tabs = "ddm_calculator")
  check(
    "Shiny: companion waits when not ready",
    length(fire_log()) == n1
  )
})

# Production server still uses the same helpers (bridge check)
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "production ynow_server still calls tab_visit_dcf_should_fire",
  grepl("tab_visit_dcf_should_fire\\(", srv)
)

cat("\n== Tab-visit Shiny runtime summary ==\n")
cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
cat("ALL TAB-VISIT SHINY RUNTIME CHECKS PASSED\n")
quit(status = 0)
