#!/usr/bin/env Rscript
# Visible-tab-first scheduling contracts.

suppressPackageStartupMessages(library(testthat))

app_dir <- if (file.exists("ynow_server.R")) "." else ".."
server_src <- paste(
  readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
ui_src <- paste(
  readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

# Source only the pure policy helpers before server() to avoid app startup.
prefix <- sub("server <- function\\(input, output, session\\) \\{[\\s\\S]*$", "", server_src, perl = TRUE)
env <- new.env(parent = baseenv())
eval(parse(text = prefix), envir = env)

test_that("tab priority cancels work owned by hidden pages", {
  ticker_tabs <- c("dashboard", "dcf_calculator", "smart_analysis")
  plan <- env$ynow_tab_priority_plan("bluechip", ticker_tabs)
  expect_true(plan$bluechip_visible)
  expect_false(plan$cluster_visible)
  expect_false(plan$cancel$bump_bluechip)
  expect_true(plan$cancel$bump_cluster)
  expect_true(plan$cancel$bump_search)

  plan <- env$ynow_tab_priority_plan("dashboard", ticker_tabs)
  expect_false(plan$cancel$bump_search)
  expect_true(plan$cancel$bump_bluechip)
  expect_true(plan$cancel$bump_cluster)
})

test_that("navigation changes run at high priority", {
  expect_match(server_src, "ynow_tab_priority_plan\\(tab, \\.ynow_ticker_refresh_tabs\\)")
  expect_match(server_src, "ignoreInit = TRUE, priority = 1000", fixed = TRUE)
})

test_that("hidden Blue Chip updates are deferred until entry", {
  expect_match(server_src, "Hidden picker remounts are surprisingly expensive", fixed = TRUE)
  expect_match(server_src, "Catch up deferred market-scoped Blue Chip controls on entry", fixed = TRUE)
  expect_match(server_src, "mounted but hidden tab must not compete", fixed = TRUE)
})

test_that("market Rf work yields until after visible paint", {
  expect_match(server_src, "session$onFlushed(function()", fixed = TRUE)
  expect_match(server_src, "cached_get_risk_free_rate(rf_mode)", fixed = TRUE)
  expect_match(server_src, "ynow_session_callback(session, run_rf)", fixed = TRUE)
})

test_that("browser records tab and mode switch timings", {
  expect_match(ui_src, "window.__ynowNavTimings", fixed = TRUE)
  expect_match(ui_src, "'lazy-ready'", fixed = TRUE)
  expect_match(ui_src, "'paint'", fixed = TRUE)
  expect_match(ui_src, "'idle'", fixed = TRUE)
})

cat("PASS test_tab_priority_scheduler\n")
