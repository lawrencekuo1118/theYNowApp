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
  ui_txt <- paste(c(
    readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"),
    readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8")
  ), collapse = "\n")
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
    "ynow_lazy_host_rel_mult_ev",
    "ynow_lazy_host_rel_multiples_calculator",
    "ynow_lazy_host_rel_mult_rev"
  )) {
    testthat::expect_true(grepl(sprintf('uiOutput("%s")', host), ui_txt, fixed = TRUE), info = host)
  }
  testthat::expect_true(grepl("\\.ynow_page_ui_snapshot\\s*<-\\s*function", ui_txt, perl = TRUE))
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

testthat::test_that("snapshot body is lazy; lock promo templates stay in eager chrome", {
  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fn <- regexpr("\\.ynow_page_ui_snapshot\\s*<-\\s*function", ui_txt, perl = TRUE)[1]
  nxt <- regexpr(
    "\\.ynow_page_ui_testing\\s*<-\\s*function",
    substr(ui_txt, fn, nchar(ui_txt)),
    perl = TRUE
  )[1]
  testthat::expect_true(fn > 0 && nxt > 1)
  body <- substr(ui_txt, fn, fn + nxt - 2L)
  testthat::expect_false(grepl("ynow_inner_tab_lock_promo_tpl", body, fixed = TRUE))
  testthat::expect_false(grepl("ynow_atx_lock_promo_tpl", body, fixed = TRUE))
  testthat::expect_true(grepl('id = "snapshot_report"', body, fixed = TRUE))
  chrome <- regexpr("dashboardBody(", ui_txt, fixed = TRUE)[1]
  tabs <- regexpr(".ynow_tab_items(", ui_txt, fixed = TRUE)[1]
  inner <- regexpr("ynow_inner_tab_lock_promo_tpl", ui_txt, fixed = TRUE)[1]
  atx <- regexpr("ynow_atx_lock_promo_tpl", ui_txt, fixed = TRUE)[1]
  testthat::expect_true(chrome > 0 && tabs > chrome)
  testthat::expect_true(inner > chrome && inner < tabs)
  testthat::expect_true(atx > inner && atx < tabs)
})

testthat::test_that("price alerts gate stays eager; body is lazy-hosted", {
  pa_txt <- paste(readLines("price_alert.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("price_alert_tab_body_ui", pa_txt, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_price_alerts")', pa_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_pa_guest_gate", pa_txt, fixed = TRUE))
})

.slice_between <- function(txt, start_pat, end_pat) {
  fn <- regexpr(start_pat, txt, perl = TRUE)[1]
  rest <- substr(txt, fn + 1L, nchar(txt))
  nxt <- regexpr(end_pat, rest, perl = TRUE)[1]
  testthat::expect_true(fn > 0 && nxt > 1)
  substr(txt, fn, fn + nxt - 1L)
}

testthat::test_that("shared-input panes stay eager in UI source", {
  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rel_txt <- paste(readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  wave_txt <- paste(readLines("wave2_readiness_shells.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # get_started and MOS stay eager. DCF, DDM, RI, P/B, NAV, and SOTP keep
  # readiness inputs in an eager shell and lazy-mount the heavy body.
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_get_started")', ui_txt, fixed = TRUE))
  testthat::expect_false(grepl('uiOutput("ynow_lazy_host_mos_assumptions")', ui_txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "get_started"', ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("wacc_gordon", ui_txt, fixed = TRUE))
  for (tab in c(
    "dcf_calculator", "ddm_calculator", "ri_calculator", "pb_calculator",
    "nav_calculator", "sotp_calculator"
  )) {
    testthat::expect_true(
      grepl(sprintf('.ynow_wave2_tab_item("%s")', tab), ui_txt, fixed = TRUE),
      info = tab
    )
    testthat::expect_identical(.ynow_lazy_host_id(tab), paste0("ynow_lazy_host_", tab))
  }
  testthat::expect_true(grepl("uiOutput(.ynow_lazy_host_id(tab))", wave_txt, fixed = TRUE))
  # Company: industry_choice eager shell; body lazy
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_dashboard")', ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("\\.ynow_page_ui_dashboard\\s*<-\\s*function", ui_txt, perl = TRUE))
  fn <- regexpr("\\.ynow_page_ui_dashboard\\s*<-\\s*function", ui_txt, perl = TRUE)[1]
  nxt <- regexpr(
    "\\.ynow_page_ui_snapshot\\s*<-\\s*function",
    substr(ui_txt, fn, nchar(ui_txt)),
    perl = TRUE
  )[1]
  testthat::expect_true(fn > 0 && nxt > 1)
  dash_body <- substr(ui_txt, fn, fn + nxt - 2L)
  testthat::expect_false(grepl("industry_choice", dash_body, fixed = TRUE))
  testthat::expect_true(grepl('inputId = "industry_choice"', ui_txt, fixed = TRUE))
  # Relative multiples: host shells + body builders
  testthat::expect_true(grepl("relative_multiples_bodies_ui", rel_txt, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput("ynow_lazy_host_rel_mult_ev")', rel_txt, fixed = TRUE))
  for (nm in c(
    "about = function",
    "feedback = function",
    "sensitivity = function",
    "smart_analysis = function",
    "company_advance = function",
    "snapshot = function",
    "price_alerts = function",
    "dashboard = function",
    "rel_mult_ev = function",
    "rel_multiples_calculator = function",
    "rel_mult_rev = function"
  )) {
    testthat::expect_true(grepl(nm, srv_txt, fixed = TRUE), info = nm)
  }
  testthat::expect_true(grepl(".ynow_wave2_builders()", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("may_mount = function(tab)", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("gate_deps = function()", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_session_may_mount_lazy_tab", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_session_treat_as_guest", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("sotp_calculator = function", srv_txt, fixed = TRUE))
})

testthat::test_that("wave2 shell ids stay out of lazy bodies", {
  source("wave2_readiness_shells.R", local = TRUE)
  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ddm_shell <- .slice_between(
    ui_txt,
    "\\.ynow_ddm_shell_ui\\s*<-\\s*function",
    "\\.ynow_page_ui_ddm\\s*<-\\s*function"
  )
  ddm_body <- .slice_between(
    ui_txt,
    "\\.ynow_page_ui_ddm\\s*<-\\s*function",
    "\\.ynow_dcf_shell_ui\\s*<-\\s*function"
  )
  testthat::expect_true(grepl('id_prefix = "ddm_"', ddm_shell, fixed = TRUE))
  testthat::expect_false(grepl('id_prefix = "ddm_"', ddm_body, fixed = TRUE))
  testthat::expect_false(grepl("capm_beta_settings_ui", ddm_body, fixed = TRUE))
  testthat::expect_true(grepl(".ddm_two_stage_params_box()", ddm_shell, fixed = TRUE))
  testthat::expect_false(grepl(".ddm_two_stage_params_box()", ddm_body, fixed = TRUE))
  dcf_shell <- .slice_between(
    ui_txt,
    "\\.ynow_dcf_shell_ui\\s*<-\\s*function",
    "\\.ynow_dcf_body_ui\\s*<-\\s*function"
  )
  dcf_body <- .slice_between(
    ui_txt,
    "\\.ynow_dcf_body_ui\\s*<-\\s*function",
    "\\nui <- dashboardPage"
  )
  testthat::expect_true(grepl("wacc_gordon", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("wacc_gordon", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl("fcf_projection_module_ui", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("fcf_projection_module_ui", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl(".dcf_two_stage_params_box()", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("plotlyOutput", dcf_shell, fixed = TRUE))
  # DCF Overview (trajectory chart) is the leftmost shell sub-tab.
  # Other charts stay in the lazy body.
  testthat::expect_true(grepl("plt_dcf_trajectory", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("plt_fcf_trend", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("mod_fcf-fcf_plot", dcf_shell, fixed = TRUE))
  # Growth KPIs live on FCF Projection (shell → module UI); WACC KPIs on WACC tab.
  testthat::expect_false(grepl("ibx_estimated_g", dcf_body, fixed = TRUE))
  testthat::expect_false(grepl("ibx_sgr", dcf_body, fixed = TRUE))
  testthat::expect_false(grepl("vbx_equity_val", dcf_body, fixed = TRUE))
  testthat::expect_false(grepl("vbx_debt_val", dcf_body, fixed = TRUE))
  testthat::expect_false(grepl("ibx_wacc", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl("vbx_equity_val", dcf_shell, fixed = TRUE))
  testthat::expect_true(grepl("ibx_wacc", dcf_shell, fixed = TRUE))
  testthat::expect_true(grepl("ibx_rd", dcf_shell, fixed = TRUE))
  testthat::expect_true(grepl("ibx_re", dcf_shell, fixed = TRUE))
  testthat::expect_false(grepl("plt_dcf_trajectory", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl("plt_fcf_trend", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl("mod_fcf-fcf_plot", dcf_body, fixed = TRUE))
  testthat::expect_true(grepl('plotlyOutput("dcf_sensitivity_heatmap"', dcf_body, fixed = TRUE))
  fcf_txt <- paste(readLines("fcf_projection_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fcf_ui <- .slice_between(
    fcf_txt,
    "fcf_projection_module_ui\\s*<-\\s*function",
    "fcf_projection_module_server\\s*<-\\s*function"
  )
  testthat::expect_true(grepl('infoBoxOutput("ibx_estimated_g"', fcf_ui, fixed = TRUE))
  testthat::expect_true(grepl('infoBoxOutput("ibx_sgr"', fcf_ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow-model-kpi-tone-2", fcf_ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow-model-kpi-tone-3", fcf_ui, fixed = TRUE))
  testthat::expect_true(grepl('ns("fcf_revenue")', fcf_ui, fixed = TRUE))
  testthat::expect_true(grepl('uiOutput(ns("fcf_table_slot"))', fcf_ui, fixed = TRUE))
  testthat::expect_false(grepl("dataTableOutput", fcf_ui, fixed = TRUE))
  for (id in ynow_wave2_shell_ids("dcf_calculator")) {
    home <- if (id %in% c("yr_stage1", "g_stage1", "wacc_stage1", "wacc_stage2")) {
      .slice_between(
        ui_txt,
        "\\.dcf_two_stage_params_box\\s*<-\\s*function",
        "\\.ddm_two_stage_params_box\\s*<-\\s*function"
      )
    } else if (id %in% c("g_growth_method", "custom_g")) {
      fcf_ui
    } else if (id %in% c("capm_rf", "capm_rm", "capm_beta", "sync_gs_beta")) {
      .slice_between(
        ui_txt,
        "capm_beta_settings_ui\\s*<-\\s*function",
        "ddm_ke_bridge_settings_ui\\s*<-\\s*function"
      )
    } else if (id %in% c("rd_interest_expense", "rd_interest_bearing_debt")) {
      .slice_between(
        ui_txt,
        "rd_estimate_settings_ui\\s*<-\\s*function",
        "\\.BETA_U_APPLY_SOURCE_IDS"
      )
    } else {
      dcf_shell
    }
    testthat::expect_true(grepl(id, home, fixed = TRUE), info = id)
    testthat::expect_false(grepl(id, dcf_body, fixed = TRUE), info = id)
  }
  stage_box <- .slice_between(
    ui_txt,
    "\\.ddm_two_stage_params_box\\s*<-\\s*function",
    "\\.ddm_formula_banner\\s*<-\\s*function"
  )
  for (id in ynow_wave2_shell_ids("ddm_calculator")) {
    if (grepl("^ddm_capm_|^ddm_sync_gs_beta$", id)) next
    home <- if (id %in% c("mod_ddm-g_stage1", "mod_ddm-yr_stage1")) stage_box else ddm_shell
    testthat::expect_true(grepl(id, home, fixed = TRUE), info = id)
    testthat::expect_false(grepl(id, ddm_body, fixed = TRUE), info = id)
  }

  pairs <- list(
    ri = list(
      file = "ri_module.R", prefix = "mod_ri-",
      shell = "ri_module_shell_ui\\s*<-\\s*function",
      body = "ri_module_body_ui\\s*<-\\s*function",
      stop = "ri_module_ui\\s*<-\\s*function"
    ),
    pb = list(
      file = "pb_asset_module.R", prefix = "mod_pb-",
      shell = "pb_asset_module_shell_ui\\s*<-\\s*function",
      body = "pb_asset_module_body_ui\\s*<-\\s*function",
      stop = "pb_asset_module_ui\\s*<-\\s*function"
    ),
    nav = list(
      file = "nav_module.R", prefix = "mod_nav-",
      shell = "nav_module_shell_ui\\s*<-\\s*function",
      body = "nav_module_body_ui\\s*<-\\s*function",
      stop = "nav_module_ui\\s*<-\\s*function"
    ),
    sotp = list(
      file = "sotp_module.R", prefix = "mod_sotp-",
      shell = "sotp_module_shell_ui\\s*<-\\s*function",
      body = "sotp_module_body_ui\\s*<-\\s*function",
      stop = "sotp_module_ui\\s*<-\\s*function"
    )
  )
  tabs <- c(ri = "ri_calculator", pb = "pb_calculator", nav = "nav_calculator", sotp = "sotp_calculator")
  for (key in names(pairs)) {
    spec <- pairs[[key]]
    src <- paste(readLines(spec$file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    shell <- .slice_between(src, spec$shell, spec$body)
    body <- .slice_between(src, spec$body, spec$stop)
    for (id in ynow_wave2_shell_ids(tabs[[key]])) {
      token <- if (startsWith(id, spec$prefix)) {
        sprintf('ns("%s")', substring(id, nchar(spec$prefix) + 1L))
      } else {
        id
      }
      testthat::expect_true(grepl(token, shell, fixed = TRUE), info = paste(key, token))
      testthat::expect_false(grepl(token, body, fixed = TRUE), info = paste(key, token))
    }
  }
  sotp_body <- .slice_between(
    paste(readLines("sotp_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    "sotp_module_body_ui\\s*<-\\s*function",
    "sotp_module_ui\\s*<-\\s*function"
  )
  for (id in c("default_ev_sales", "nonop", "manual_segments")) {
    testthat::expect_true(grepl(sprintf('ns("%s")', id), sotp_body, fixed = TRUE), info = id)
  }
})

testthat::test_that("may_mount FALSE leaves the tab unmounted until the gate opens", {
  builds <- 0L
  hooks <- 0L
  state <- new.env(parent = emptyenv())
  state$allow <- FALSE
  gate <- shiny::reactiveVal(0L)
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    input <- session$input
    output <- session$output
    ctrl <- .ynow_register_lazy_tabs(
      input, output, session,
      builders = list(
        lab_notes = function() {
          builds <<- builds + 1L
          htmltools::tags$div(id = "lab", "Lab")
        },
        about = function() htmltools::tags$div(id = "about", "About")
      ),
      on_first_mount = list(
        lab_notes = function() {
          hooks <<- hooks + 1L
        }
      ),
      may_mount = function(tab) {
        if (identical(tab, "lab_notes")) return(isTRUE(state$allow))
        TRUE
      },
      gate_deps = function() gate()
    )
    session$setInputs(sidebar_tabs = "lab_notes")
    session$flushReact()
    testthat::expect_false(ctrl$mounted("lab_notes"))
    testthat::expect_identical(builds, 0L)
    if (requireNamespace("later", quietly = TRUE)) later::run_now(timeoutSecs = 0.1)
    testthat::expect_identical(hooks, 0L)

    session$setInputs(sidebar_tabs = "about")
    session$flushReact()
    testthat::expect_true(ctrl$mounted("about"))
    testthat::expect_false(ctrl$mounted("lab_notes"))

    session$setInputs(sidebar_tabs = "lab_notes")
    session$flushReact()
    testthat::expect_false(ctrl$mounted("lab_notes"))
    testthat::expect_identical(builds, 0L)

    state$allow <- TRUE
    gate(1L)
    session$flushReact()
    testthat::expect_true(ctrl$mounted("lab_notes"))
    testthat::expect_identical(builds, 1L)
    if (requireNamespace("later", quietly = TRUE)) {
      deadline <- Sys.time() + 2
      while (hooks < 1L && Sys.time() < deadline) later::run_now(timeoutSecs = 0.05)
    }
    testthat::expect_identical(hooks, 1L)

    gate(2L)
    session$flushReact()
    if (requireNamespace("later", quietly = TRUE)) later::run_now(timeoutSecs = 0.2)
    testthat::expect_identical(builds, 1L)
    testthat::expect_identical(hooks, 1L)
  })
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

.ynow_slice_lines <- function(lines, line1, col1, line2, col2) {
  if (line1 == line2) {
    return(substr(lines[[line1]], col1, col2))
  }
  mid <- if (line2 > line1 + 1L) lines[(line1 + 1L):(line2 - 1L)] else character()
  paste(c(
    substr(lines[[line1]], col1, nchar(lines[[line1]])),
    mid,
    substr(lines[[line2]], 1L, col2)
  ), collapse = "\n")
}

.ynow_lazy_builders_src <- function(path = "ynow_server.R") {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pd <- getParseData(parse(text = paste(lines, collapse = "\n"), keep.source = TRUE))
  symbol_row <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == ".ynow_register_lazy_tabs")
  if (length(symbol_row) != 1L) {
    stop(sprintf(
      "expected one .ynow_register_lazy_tabs call in %s, found %d",
      path, length(symbol_row)
    ))
  }
  name_expr_id <- pd$parent[[symbol_row]]
  call_id <- pd$parent[pd$id == name_expr_id]
  if (length(call_id) != 1L) stop("could not locate the .ynow_register_lazy_tabs call expression")
  kids <- pd[pd$parent == call_id, , drop = FALSE]
  named <- kids[kids$token == "SYMBOL_SUB" & kids$text == "builders", , drop = FALSE]
  if (nrow(named) != 1L) stop("builders argument not found on .ynow_register_lazy_tabs")
  exprs <- kids[kids$token == "expr", , drop = FALSE]
  after <- exprs$line1 > named$line1 | (exprs$line1 == named$line1 & exprs$col1 > named$col2)
  exprs <- exprs[after, , drop = FALSE]
  exprs <- exprs[order(exprs$line1, exprs$col1), , drop = FALSE]
  if (nrow(exprs) < 1L) stop("builders value expression not found")
  val <- exprs[1L, ]
  .ynow_slice_lines(lines, val$line1, val$col1, val$line2, val$col2)
}

testthat::test_that("lazy builders list evaluates and MOS Assumptions stays eager", {
  src <- .ynow_lazy_builders_src("ynow_server.R")
  err <- NULL
  eval_env <- new.env(parent = baseenv())
  sys.source("lazy_tabs.R", eval_env)
  sys.source("wave2_readiness_shells.R", eval_env)
  built <- tryCatch(
    eval(parse(text = src), envir = eval_env),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  testthat::expect_identical(if (is.null(err)) "ok" else err, "ok")
  if (is.null(err)) {
    testthat::expect_type(built, "list")
    testthat::expect_gt(length(built), 0L)
    testthat::expect_false(is.null(names(built)))
    testthat::expect_true(all(nzchar(names(built))))
    testthat::expect_true(all(vapply(built, is.function, logical(1L))))
    testthat::expect_true("rel_mult_rev" %in% names(built))
    testthat::expect_false("mos_assumptions" %in% names(built))
    testthat::expect_false("get_started" %in% names(built))
    for (tab in c(
      "dcf_calculator", "ddm_calculator", "ri_calculator", "pb_calculator",
      "nav_calculator", "sotp_calculator"
    )) {
      testthat::expect_true(tab %in% names(built), info = tab)
    }

    broken <- sub("\\)\\s*$", ",)", src)
    broken_err <- tryCatch(
      eval(parse(text = broken), envir = eval_env),
      error = function(e) conditionMessage(e)
    )
    testthat::expect_match(broken_err, "^argument [0-9]+ is empty$")
  }

  ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(
    grepl("mos_assumptions_tab_ui()", ui_txt, fixed = TRUE),
    info = "ynow_ui.R must call mos_assumptions_tab_ui()"
  )
  testthat::expect_false(
    grepl("ynow_lazy_host_mos_assumptions", ui_txt, fixed = TRUE),
    info = "MOS Assumptions must not use ynow_lazy_host_mos_assumptions"
  )
})

cat("PASS test_lazy_tabs\n")
