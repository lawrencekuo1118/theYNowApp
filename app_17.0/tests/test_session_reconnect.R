# Smoke: deploy-resilience reconnect + optional Refresh-on-new-version.

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("testthat required")
  }
})

testthat::local_edition(3)

testthat::test_that("server enables force reconnect for shinyapps-style hosts", {
  srv <- paste(readLines(file.path("..", "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl('allowReconnect\\(\"force\"\\)', srv))
  testthat::expect_true(grepl("ynow_client_reconnected", srv))
  testthat::expect_true(grepl("ynowBuildInfo", srv, fixed = TRUE))
  testthat::expect_true(grepl("ynow_soft_reload_done", srv, fixed = TRUE))
})

testthat::test_that("UI softens platform Reload dialog and localizes reconnect copy", {
  ui <- paste(readLines(file.path("..", "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("ss-connect-dialog", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow-reconnect-banner", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynowInstallReconnectBackoff", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow_client_reconnected", ui, fixed = TRUE))
})

testthat::test_that("optional Refresh sits beside Feedback and restores workspace", {
  ui <- paste(readLines(file.path("..", "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  testthat::expect_true(grepl("ynow_sidebar_refresh_btn", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow-sidebar-refresh-link", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynowApplyAppUpdate", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow_soft_reload_state", ui, fixed = TRUE))
  testthat::expect_true(grepl("__YNOW_PAGE_BUILD", ui, fixed = TRUE))
  testthat::expect_true(grepl("ynow_build.json", ui, fixed = TRUE))
  # Refresh markup appears after Feedback control in source order
  fb <- regexpr("ynow_sidebar_feedback_btn", ui, fixed = TRUE)[1]
  rf <- regexpr("ynow_sidebar_refresh_btn", ui, fixed = TRUE)[1]
  testthat::expect_true(fb > 0 && rf > fb)
})

testthat::test_that("build manifest and display version stay aligned", {
  cfg <- paste(readLines(file.path("..", "default_config.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- regmatches(cfg, regexpr('YNOW_DISPLAY_VERSION\\s*<-\\s*\"[^\"]+\"', cfg))
  testthat::expect_true(length(m) == 1L)
  ver <- sub('.*\"([^\"]+)\".*', "\\1", m)
  build <- jsonlite::fromJSON(file.path("..", "www", "ynow_build.json"))
  testthat::expect_identical(as.character(build$version), as.character(ver))
  testthat::expect_identical(ver, "v17.91")
})

testthat::test_that("reconnect/refresh i18n keys exist in en and zh-TW", {
  source(file.path("..", "ui_locale.R"), local = TRUE, encoding = "UTF-8")
  keys <- c(
    "reconnect_attempting",
    "reconnect_try_now",
    "reconnect_banner",
    "reconnect_restored",
    "refresh_link",
    "refresh_title",
    "refresh_applied"
  )
  for (k in keys) {
    en <- ui_str(k, "en")
    zh <- ui_str(k, "zh-TW")
    testthat::expect_true(nzchar(en), info = k)
    testthat::expect_true(nzchar(zh), info = k)
    testthat::expect_false(identical(en, k), info = k)
    testthat::expect_false(identical(zh, k), info = k)
  }
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$refresh_link, " 重新整理")
  testthat::expect_identical(.UI_STRINGS$en$refresh_link, " Refresh")
})
