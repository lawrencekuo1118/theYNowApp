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
    "ynow_lazy_host_company_advance",
    "ynow_lazy_host_snapshot",
    "ynow_lazy_host_dashboard",
    "ynow_lazy_host_ddm_calculator",
    "ynow_lazy_host_ri_calculator",
    "ynow_lazy_host_pb_calculator",
    "ynow_lazy_host_nav_calculator",
    "ynow_lazy_host_sotp_calculator",
    "ynow_lazy_host_rel_mult_ev",
    "ynow_lazy_host_rel_multiples_calculator",
    "ynow_lazy_host_rel_mult_rev"
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

.lazy_tabitem_src <- function(ui_lines, tab) {
  pd <- getParseData(parse(text = paste(ui_lines, collapse = "\n"), keep.source = TRUE))
  desc <- function(id) {
    kids <- pd$id[pd$parent == id]
    if (!length(kids)) return(integer())
    c(kids, unlist(lapply(kids, desc), use.names = FALSE))
  }
  src <- paste(ui_lines, collapse = "\n")
  off <- function(line, col) {
    if (line <= 1L) return(col)
    sum(nchar(ui_lines[seq_len(line - 1L)])) + (line - 1L) + col
  }
  syms <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "tabItem")
  for (i in syms) {
    cid <- pd$parent[pd$id == pd$parent[i]]
    call <- pd[pd$id == cid, ]
    drows <- pd[pd$id %in% c(cid, desc(cid)), ]
    strs <- drows$text[drows$token == "STR_CONST"]
    if (!length(strs)) next
    name <- gsub('^"|"$', "", strs[1])
    if (!identical(name, tab)) next
    return(substr(src, off(call$line1, call$col1), off(call$line2, call$col2)))
  }
  ""
}

testthat::test_that("get_started and DCF stay eager; wave-1 shells are hosts", {
  ui_lines <- readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8")
  ui_txt <- paste(ui_lines, collapse = "\n")
  srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  gs <- .lazy_tabitem_src(ui_lines, "get_started")
  dcf <- .lazy_tabitem_src(ui_lines, "dcf_calculator")
  home <- .lazy_tabitem_src(ui_lines, "home")
  testthat::expect_false(grepl("ynow_lazy_host_get_started", gs, fixed = TRUE))
  testthat::expect_false(grepl("ynow_lazy_host_dcf", dcf, fixed = TRUE))
  testthat::expect_false(grepl("ynow_lazy_host_home", home, fixed = TRUE))
  testthat::expect_true(grepl(".dcf_core_params_box()", gs, fixed = TRUE))
  testthat::expect_true(grepl(".shared_bridge_box()", gs, fixed = TRUE))
  testthat::expect_true(grepl("beta_overview_section_ui()", gs, fixed = TRUE))
  testthat::expect_true(grepl('"wacc_gordon"', dcf, fixed = TRUE))
  testthat::expect_true(grepl(".home_brand_ui()", home, fixed = TRUE))
  testthat::expect_gt(nchar(gs), 500L)
  testthat::expect_gt(nchar(dcf), 500L)
  dash <- .lazy_tabitem_src(ui_lines, "dashboard")
  ddm <- .lazy_tabitem_src(ui_lines, "ddm_calculator")
  snap <- .lazy_tabitem_src(ui_lines, "snapshot")
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_dashboard")', dash, fixed = TRUE))
  testthat::expect_false(grepl("company_overview_ui", dash, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_ddm_calculator")', ddm, fixed = TRUE))
  testthat::expect_false(grepl("mod_ddm-d0", ddm, fixed = TRUE))
  testthat::expect_true(grepl("mod_ddm-d0", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_snapshot")', snap, fixed = TRUE))
  testthat::expect_false(grepl("ynow_snapshot_page_title", snap, fixed = TRUE))
  for (nm in c(
    "about = function",
    "feedback = function",
    "sensitivity = function",
    "smart_analysis = function",
    "company_advance = function",
    "price_alerts = function",
    "snapshot = function",
    "dashboard = function",
    "ddm_calculator = function",
    "ri_calculator = function",
    "pb_calculator = function",
    "nav_calculator = function",
    "sotp_calculator = function",
    "rel_mult_ev = function",
    "rel_multiples_calculator = function",
    "rel_mult_rev = function"
  )) {
    testthat::expect_true(grepl(nm, srv_txt, fixed = TRUE), info = nm)
  }
  testthat::expect_true(grepl("ri_module_server(", srv_txt, fixed = TRUE))
  testthat::expect_false(grepl("on_first_mount[\\s\\S]{0,500}ri_module_server\\(", srv_txt, perl = TRUE))
  testthat::expect_true(grepl(".ynow_page_ui_dashboard <- function", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl(".ynow_page_ui_ddm_calculator <- function", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl(".ynow_page_ui_snapshot <- function", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl(".ynow_page_ui_price_alerts <- function", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("company_overview_ui", ui_txt, fixed = TRUE))
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

cat("PASS test_lazy_tabs\n")
