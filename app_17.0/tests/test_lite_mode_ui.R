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
    "lite_toggle_title", "lite_toggle_aria"
  )
  for (k in keys) {
    en <- .UI_STRINGS$en[[k]]
    zh <- .UI_STRINGS$`zh-TW`[[k]]
    testthat::expect_true(is.character(en) && nzchar(en[1]), info = paste("en", k))
    testthat::expect_true(is.character(zh) && nzchar(zh[1]), info = paste("zh-TW", k))
  }
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$menu_smart_analysis, "智慧分析")
  testthat::expect_identical(.UI_STRINGS$en$menu_smart_analysis, "Smart Analysis")
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
  testthat::expect_true(grepl("The YNow App v17.80", txt, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite scenario apply before auto-calc", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl(".apply_lite_recommended_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".lite_desired_scenario", txt, fixed = TRUE))
  testthat::expect_true(grepl(".clamp_g_below_rate", txt, fixed = TRUE))
  testthat::expect_true(grepl(".auto_calc_shares_ready", txt, fixed = TRUE))
  testthat::expect_true(grepl("lite_scenario_applied_sig", txt, fixed = TRUE))
  testthat::expect_true(grepl("calculated_wacc()", txt, fixed = TRUE))
})

testthat::test_that("ynow_server wires Lite auto-calc and Smart Analysis outputs", {
  srv_path <- file.path("..", "ynow_server.R")
  txt <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("lite_mode <- reactive", txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_lite_mode", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_summary", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_chart", txt, fixed = TRUE))
  testthat::expect_true(grepl("output$smart_analysis_reason", txt, fixed = TRUE))
})
