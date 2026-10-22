# Banner right logo: soft-refresh active tab + clockwise spin while busy.

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat required")
})

testthat::local_edition(3)

root <- if (file.exists("ynow_ui.R")) {
  "."
} else if (file.exists("../ynow_ui.R")) {
  ".."
} else {
  stop("Cannot locate app root")
}
setwd(root)

ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc_txt <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
lazy_txt <- paste(readLines("lazy_tabs.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

testthat::test_that("header logo wires tab-scoped refresh (not home jump / not session reload)", {
  testthat::expect_true(grepl("ynow_header_logo_home", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("bindHeaderLogoRefresh", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("refreshCurrentSidebarTab", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow_tab_refresh", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynowRefreshCurrentSidebarTab", ui_txt, fixed = TRUE))
  # Must not navigate home on logo click anymore.
  testthat::expect_false(grepl(
    "bindHeaderLogoRefresh[\\s\\S]{0,800}?gotoTab\\('home'\\)",
    ui_txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl("observeEvent(input$ynow_tab_refresh", srv_txt, fixed = TRUE))
  testthat::expect_false(grepl(
    "observeEvent\\(input\\$ynow_tab_refresh[\\s\\S]{0,1200}?session\\$reload",
    srv_txt,
    perl = TRUE
  ))
  testthat::expect_true(grepl(".ynow_lazy_tab_ctrl$refresh", srv_txt, fixed = TRUE))
})

testthat::test_that("logo spin CSS + busy sync exist", {
  testthat::expect_true(grepl("ynow-logo-spinning", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynow-logo-spin-cw", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("syncHeaderLogoSpin", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynowHeaderLogoSpin", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("ynowHeaderLogoSpin", srv_txt, fixed = TRUE))
  testthat::expect_true(grepl("animation: ynow-logo-spin-cw", ui_txt, fixed = TRUE))
})

testthat::test_that("locale titles are refresh copy (en-US + zh-TW)", {
  source("ui_locale.R", local = TRUE, encoding = "UTF-8")
  testthat::expect_identical(.UI_STRINGS$en$header_logo_refresh_title, "Refresh current page")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$header_logo_refresh_title, "重新整理目前分頁")
  testthat::expect_identical(.UI_STRINGS$en$header_logo_home_title, "Refresh current page")
  testthat::expect_identical(.UI_STRINGS$`zh-TW`$header_logo_home_title, "重新整理目前分頁")
  testthat::expect_true(grepl("header_logo_refresh_title", loc_txt, fixed = TRUE))
})

testthat::test_that("lazy_tabs exposes soft refresh without remount hook", {
  testthat::expect_true(grepl("refresh = function(tab)", lazy_txt, fixed = TRUE))
  testthat::expect_true(grepl("refresh_tokens", lazy_txt, fixed = TRUE))
})

cat("PASS test_banner_logo_tab_refresh\n")
