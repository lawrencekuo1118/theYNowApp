# Lazy tab hosts: once-per-session mount, no remount on revisit (no network).

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat required")
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
})

testthat::local_edition(3)

root <- if (file.exists("lazy_tabs.R")) {
  "."
} else if (file.exists("../lazy_tabs.R")) {
  ".."
} else {
  stop("Cannot locate lazy_tabs.R")
}
setwd(root)

source("lazy_tabs.R", local = TRUE, encoding = "UTF-8")

testthat::test_that("host id is stable and filesystem-safe", {
  testthat::expect_identical(.ynow_lazy_host_id("bluechip"), "ynow_lazy_host_bluechip")
  testthat::expect_identical(.ynow_lazy_host_id("lab_notes"), "ynow_lazy_host_lab_notes")
})

testthat::test_that("UI shells use lazy hosts for heavy tabs", {
  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (host in c(
    "ynow_lazy_host_macro_market",
    "ynow_lazy_host_bluechip",
    "ynow_lazy_host_hfv",
    "ynow_lazy_host_lab_notes",
    "ynow_lazy_host_testing"
  )) {
    testthat::expect_true(grepl(sprintf('uiOutput("%s")', host), ui_txt, fixed = TRUE), info = host)
  }
  testthat::expect_true(grepl("\\.ynow_page_ui_bluechip\\s*<-\\s*function", ui_txt, perl = TRUE))
  testthat::expect_true(grepl("\\.ynow_page_ui_lab_notes\\s*<-\\s*function", ui_txt, perl = TRUE))
  # Body MathJax deferred off dashboardBody first paint
  testthat::expect_false(grepl(
    "dashboardBody\\([\\s\\S]{0,200}?withMathJax\\(\\)",
    ui_txt,
    perl = TRUE
  ))
})

testthat::test_that("decision checklist body is lazy-hosted", {
  dc_txt <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("decision_checklist_tab_body_ui", dc_txt, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_decision_checklist")', dc_txt, fixed = TRUE))
})

testthat::test_that("register mounts once and caches builder result", {
  builds <- 0L
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    input <- session$input
    output <- session$output
    ctrl <- .ynow_register_lazy_tabs(
      input, output, session,
      builders = list(
        bluechip = function() {
          builds <<- builds + 1L
          htmltools::tags$div(id = "bc", "Blue Chip")
        }
      )
    )
    session$setInputs(sidebar_tabs = "home")
    session$flushReact()
    testthat::expect_identical(builds, 0L)
    testthat::expect_false(ctrl$mounted("bluechip"))

    session$setInputs(sidebar_tabs = "bluechip")
    session$flushReact()
    testthat::expect_identical(builds, 1L)
    testthat::expect_true(ctrl$mounted("bluechip"))

    session$setInputs(sidebar_tabs = "home")
    session$flushReact()
    session$setInputs(sidebar_tabs = "bluechip")
    session$flushReact()
    testthat::expect_identical(builds, 1L)
  })
})

cat("PASS test_lazy_tabs\n")
