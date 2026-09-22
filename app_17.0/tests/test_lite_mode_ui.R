# Lite mode: UI chrome keys, locale strings, and Smart Analysis wiring
# (no new scrapers — reuses recommend_valuation_models / auto_calc)

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("testthat required")
  }
})

testthat::local_edition(3)

testthat::test_that("Lite locale keys exist in en and zh-TW", {
  source(file.path("..", "ui_locale.R"), local = TRUE)
  keys <- c(
    "menu_smart_analysis", "smart_page_title", "smart_page_sub",
    "smart_chart_title", "smart_primary_kicker", "smart_secondary_kicker",
    "smart_price_kicker", "smart_mos_kicker", "smart_waiting",
    "smart_calc_pending", "smart_reason_title",
    "smart_scenario_title", "smart_scenario_two_stage", "smart_scenario_gordon",
    "smart_scenario_sgr", "smart_scenario_claim",
    "lab_im_eq_label", "lab_im_eq_hint",
    "lab_im_eq_explain_title", "lab_im_eq_explain_body",
    "lab_im_include_adr_label", "lab_im_include_adr_hint",
    "lite_toggle_title", "lite_toggle_aria",
    "snapshot_page_help_lite", "snapshot_defaults_help_lite"
  )
  for (k in keys) {
    en <- .UI_STRINGS$en[[k]]
    zh <- .UI_STRINGS$`zh-TW`[[k]]
    testthat::expect_true(is.character(en) && nzchar(en[1]), info = paste("en", k))
    testthat::expect_true(is.character(zh) && nzchar(zh[1]), info = paste("zh-TW", k))
  }
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$menu_smart_analysis, "智慧分析")
  testthat::expect_identical(.UI_STRINGS$en$menu_smart_analysis, "Smart Analysis")
  testthat::expect_true(grepl("參數情境", .UI_STRINGS$`zh-TW`$smart_page_sub, fixed = TRUE))
  testthat::expect_true(grepl("parameter scenario", .UI_STRINGS$en$smart_page_sub, fixed = TRUE))
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$smart_scenario_title, "已套用參數情境：")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$lab_im_eq_label, "盈餘品質")
  testthat::expect_identical(.UI_STRINGS$en$lab_im_eq_label, "Earnings quality")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$lab_im_eq_explain_title, "盈餘品質：")
  testthat::expect_true(grepl("OCF", .UI_STRINGS$`zh-TW`$lab_im_eq_explain_body, fixed = TRUE))
})

testthat::test_that("ynow_ui wires Lite toggle, Smart Analysis tab, and CSS hooks", {
  ui_path <- file.path("..", "ynow_ui.R")
  txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("ynow_lite_toggle", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lite-badge", txt, fixed = TRUE))
  testthat::expect_true(grepl("body.ynow-lite", txt, fixed = TRUE))
  testthat::expect_true(grepl('tabName = "smart_analysis"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-full-only", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lab_im_detail_tab", txt, fixed = TRUE))
  testthat::expect_true(grepl("The YNow App v17.89", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-report", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-toolbar", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-hfv-chapter", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_hfv_ch1_title", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-report", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-toolbar", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-backtest-chapter", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_bt_ch1_title", txt, fixed = TRUE))
  testthat::expect_true(grepl("about_lite_intro_ui", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lite-only", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-sidebar-test-link", txt, fixed = TRUE))
  testthat::expect_true(grepl("ensureLiteSnapshotDefaultsTab", txt, fixed = TRUE))
  testthat::expect_true(grepl('value = "snap_defaults"', txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_snapshot_page_help_lite", txt, fixed = TRUE))
  testthat::expect_true(grepl(
    "body\\.ynow-lite[\\s\\S]*ynow-sidebar-test-link",
    txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl(
    "body\\.ynow-lite[\\s\\S]*snap_audit",
    txt,
    perl = TRUE
  ))
  # Header quote KPIs are Dashboard-only (not Smart Analysis)
  testthat::expect_true(grepl(
    "Header KPIs: Dashboard only",
    txt,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    'condition = "input.sidebar_tabs == \'dashboard\'"',
    txt,
    fixed = TRUE
  ))
  testthat::expect_false(grepl(
    "input.sidebar_tabs == 'dashboard' || input.sidebar_tabs == 'smart_analysis'",
    txt,
    fixed = TRUE
  ))
  # Lite About must not mount methodology outside ynow-full-only
  testthat::expect_true(grepl(
    "ynow-full-only[\\s\\S]*valuation_methodology_section_ui",
    txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl("ynow-lab-im-eq-adr-row", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lab_im_eq_explain", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-lab-im-eq-explain ynow-lite-only", txt, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite scenario apply before auto-calc", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl(".apply_lite_recommended_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".lite_desired_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".clamp_g_below_rate", txt, fixed = TRUE))
  testthat::expect_true(grepl(".lite_resync_discount_consistency", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_dcf_silent", txt, fixed = TRUE))
  testthat::expect_true(grepl(".heal_g_vs_rate", txt, fixed = TRUE))
  testthat::expect_true(grepl(".auto_calc_shares_ready", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_scenario_applied_sig", txt, fixed = TRUE))
  testthat::expect_true(grepl("calculated_wacc()", txt, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite auto-calc and Smart Analysis outputs", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("lite_mode <- reactive", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_default_keys", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lite_mode", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_summary", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_chart", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_reason", txt, fixed = TRUE))
})

testthat::test_that("clamp_g_below_rate keeps g strictly below discount", {
  # Mirror of server helper (pure math) for regression without Shiny session
  clamp_g_below_rate <- function(g_pct, rate_pct, margin = 0.5) {
    g_pct <- suppressWarnings(as.numeric(g_pct)[1])
    rate_pct <- suppressWarnings(as.numeric(rate_pct)[1])
    if (!is.finite(g_pct)) return(NA_real_)
    if (!is.finite(rate_pct) || rate_pct <= 0) return(g_pct)
    if (g_pct < rate_pct - 1e-6) return(g_pct)
    max(rate_pct - margin, 0)
  }
  testthat::expect_equal(clamp_g_below_rate(3, 8), 3)
  testthat::expect_equal(clamp_g_below_rate(8, 8), 7.5)
  testthat::expect_equal(clamp_g_below_rate(12, 9), 8.5)
  testthat::expect_true(clamp_g_below_rate(10, 8) < 8)
})
