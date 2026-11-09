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

test_that("statement crawl runs only on company and valuation tabs", {
  ticker_tabs <- c(
    "dashboard", "smart_analysis", "company_advance",
    "dcf_calculator", "get_started"
  )
  expect_false(env$ynow_statement_crawl_wanted("global_prediction", ticker_tabs))
  expect_false(env$ynow_statement_crawl_wanted("macro_market", ticker_tabs))
  expect_false(env$ynow_statement_crawl_wanted("bluechip", ticker_tabs))
  expect_false(env$ynow_statement_crawl_wanted("home", ticker_tabs))
  expect_false(env$ynow_statement_crawl_wanted("", ticker_tabs))
  expect_true(env$ynow_statement_crawl_wanted("dashboard", ticker_tabs))
  expect_true(env$ynow_statement_crawl_wanted("dcf_calculator", ticker_tabs))
  expect_true(env$ynow_statement_crawl_wanted("business_breakdown_lab", ticker_tabs))
})

test_that("a non-company tab never opens the statement progress toast", {
  obs <- regexpr(
    "observeEvent(list(current_ticker(), ticker_search_gen())",
    server_src, fixed = TRUE
  )
  prog <- regexpr("shiny::Progress$new", server_src, fixed = TRUE)
  gate <- regexpr("statement_crawl_deferred(TRUE)", server_src, fixed = TRUE)
  expect_true(obs[[1]] > 0)
  expect_true(gate[[1]] > obs[[1]] && gate[[1]] < prog[[1]])
  expect_match(server_src, "ynow_statement_crawl_wanted", fixed = TRUE)
  expect_match(
    server_src,
    "later::later[\\s\\S]{0,500}phase2_after_yahoo",
    perl = TRUE
  )
})

test_that("leaving a company tab hides the statement toast immediately", {
  expect_match(ui_src, "ynowDropCompanyFetchChrome", fixed = TRUE)
  expect_match(ui_src, "YNOW_STATEMENT_TABS", fixed = TRUE)
  expect_match(ui_src, "global_prediction", fixed = TRUE)
})

test_that("navigation changes run at high priority", {
  expect_match(server_src, "ynow_tab_priority_plan\\(tab, \\.ynow_ticker_refresh_tabs\\)")
  expect_match(server_src, "ignoreInit = TRUE, priority = 1000", fixed = TRUE)
})

test_that("hidden Blue Chip updates are deferred until entry", {
  expect_match(server_src, "Hidden picker remounts are surprisingly expensive", fixed = TRUE)
  expect_match(server_src, "Catch up deferred market-scoped Blue Chip controls on entry", fixed = TRUE)
  expect_match(server_src, 'if (!identical(as.character(isolate(input$sidebar_tabs) %||% "")[1], "bluechip")) return()', fixed = TRUE)
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
