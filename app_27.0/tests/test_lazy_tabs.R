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
    "ynow_lazy_host_testing",
    "ynow_lazy_host_about",
    "ynow_lazy_host_feedback",
    "ynow_lazy_host_sensitivity",
    "ynow_lazy_host_smart_analysis",
    "ynow_lazy_host_company_advance"
  )) {
    testthat::expect_true(grepl(sprintf('uiOutput("%s")', host), ui_txt, fixed = TRUE), info = host)
  }
  testthat::expect_true(grepl("\\.ynow_page_ui_bluechip\\s*<-\\s*function", ui_txt, perl = TRUE))
  testthat::expect_true(grepl("\\.ynow_page_ui_lab_notes\\s*<-\\s*function", ui_txt, perl = TRUE))
  testthat::expect_true(grepl("\\.ynow_page_ui_about\\s*<-\\s*function", ui_txt, perl = TRUE))
  testthat::expect_true(grepl("\\.ynow_page_ui_feedback\\s*<-\\s*function", ui_txt, perl = TRUE))
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

testthat::test_that("price alerts gate stays eager; body is lazy-hosted", {
  pa_txt <- paste(readLines("price_alert.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("price_alert_tab_body_ui", pa_txt, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_price_alerts")', pa_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_pa_guest_gate", pa_txt, fixed = TRUE))
})

testthat::test_that("shared-input panes stay eager in UI source", {
  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # get_started / dcf keep real markup (not only lazy hosts)
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_get_started")', ui_txt, fixed = TRUE))
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_dcf_calculator")', ui_txt, fixed = TRUE))
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_ddm_calculator")', ui_txt, fixed = TRUE))
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_dashboard")', ui_txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "get_started"', ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("wacc_gordon", ui_txt, fixed = TRUE))
  # Wave-1 builders registered on the server
  for (nm in c(
    "about = function",
    "feedback = function",
    "sensitivity = function",
    "smart_analysis = function",
    "company_advance = function",
    "price_alerts = function"
  )) {
    testthat::expect_true(grepl(nm, srv_txt, fixed = TRUE), info = nm)
  }
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

    # Soft refresh rebuilds UI tags without a second module mount hook.
    testthat::expect_true(isTRUE(ctrl$refresh("bluechip")))
    session$flushReact()
    testthat::expect_identical(builds, 2L)
    testthat::expect_true(ctrl$mounted("bluechip"))
    testthat::expect_false(isTRUE(ctrl$refresh("home")))
  })
})

testthat::test_that("deferred on_first_mount restores reactive domain for moduleServer", {
  testthat::skip_if_not_installed("later")
  domain_ok <- FALSE
  module_err <- NULL
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    input <- session$input
    output <- session$output
    ctrl <- .ynow_register_lazy_tabs(
      input, output, session,
      builders = list(
        macro_market = function() {
          htmltools::tags$div(
            class = "ynow-macro-report",
            shiny::uiOutput("macro-index_kpi_row")
          )
        }
      ),
      on_first_mount = list(
        macro_market = function() {
          domain_ok <<- inherits(
            shiny::getDefaultReactiveDomain(),
            c("ShinySession", "session_proxy", "MockShinySession")
          )
          tryCatch(
            shiny::moduleServer("macro", function(input, output, session) {
              output$index_kpi_row <- shiny::renderUI({
                htmltools::tags$div(class = "ynow-macro-kpi-row", "ok")
              })
            }),
            error = function(e) {
              module_err <<- conditionMessage(e)
              NULL
            }
          )
        }
      )
    )
    session$setInputs(sidebar_tabs = "macro_market")
    session$flushReact()
    testthat::expect_true(ctrl$mounted("macro_market"))
    deadline <- Sys.time() + 2
    while ((!isTRUE(domain_ok) || !is.null(module_err)) && Sys.time() < deadline) {
      later::run_now(timeoutSecs = 0.05)
      if (isTRUE(domain_ok) && is.null(module_err)) break
    }
  })
  testthat::expect_true(isTRUE(domain_ok))
  testthat::expect_null(module_err)
})

testthat::test_that("lazy_tabs wraps deferred hooks in withReactiveDomain", {
  lazy_txt <- paste(readLines("lazy_tabs.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl(
    "withReactiveDomain\\(sess,\\s*\\{\\s*hook_fn\\(\\)",
    lazy_txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl("later::later drops the Shiny reactive domain", lazy_txt, fixed = TRUE))
})

cat("PASS test_lazy_tabs\n")
