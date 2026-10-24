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

testthat::test_that("focus click accelerates spin and cancels other tabs", {
  testthat::expect_true(grepl("ynow-logo-spin-fast", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("animation-duration: 0.7s", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl(
    "ynow-header-logo-mark.ynow-logo-spinning.ynow-logo-spin-fast",
    ui_txt,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    paste0(
      "@media (prefers-reduced-motion: reduce) {\n",
      "          .ynow-header-logo-mark.ynow-logo-spinning,\n",
      "          .ynow-header-logo-mark.ynow-logo-spinning.ynow-logo-spin-fast {\n",
      "            animation-duration: 6s;"
    ),
    ui_txt,
    fixed = TRUE
  ))
  testthat::expect_true(grepl("window.ynowClearOtherTabBusy", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("window.ynowFocusCurrentTabBusy", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("classList.remove('ynow-logo-spin-fast')", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("n < focusN", ui_txt, fixed = TRUE))
  testthat::expect_true(grepl("tabHasOtherBusy", ui_txt, fixed = TRUE))

  refresh <- regmatches(ui_txt, regexpr(
    "function refreshCurrentSidebarTab\\(\\)[\\s\\S]*?window\\.ynowRefreshCurrentSidebarTab",
    ui_txt,
    perl = TRUE
  ))
  testthat::expect_match(refresh, "contains\\('ynow-logo-spinning'\\)", perl = TRUE)
  testthat::expect_match(refresh, "focus: focus", fixed = TRUE)
  testthat::expect_match(refresh, "n: n", fixed = TRUE)
  testthat::expect_match(refresh, "__ynowLogoFocusN", fixed = TRUE)
  testthat::expect_match(refresh, "ynow-logo-spin-fast", fixed = TRUE)
  testthat::expect_match(refresh, "ynowFocusCurrentTabBusy", fixed = TRUE)
  spin_at <- regexpr("contains('ynow-logo-spinning')", refresh, fixed = TRUE)
  send_at <- regexpr("setInputValue", refresh, fixed = TRUE)
  testthat::expect_gt(spin_at[[1]], 0)
  testthat::expect_gt(send_at[[1]], spin_at[[1]])

  obs <- regmatches(srv_txt, regexpr(
    "observeEvent\\(input\\$ynow_tab_refresh[\\s\\S]*?ignoreNULL = TRUE\\)",
    srv_txt,
    perl = TRUE
  ))
  testthat::expect_match(obs, ".ynow_cancel_other_tab_work", fixed = TRUE)
  testthat::expect_match(obs, ".ynow_fire_ticker_search", fixed = TRUE)
  cancel_at <- regexpr(".ynow_cancel_other_tab_work", obs, fixed = TRUE)
  fire_at <- regexpr(".ynow_fire_ticker_search", obs, fixed = TRUE)
  flush_at <- regexpr("session$onFlushed", obs, fixed = TRUE)
  testthat::expect_gt(cancel_at[[1]], 0)
  testthat::expect_gt(fire_at[[1]], cancel_at[[1]])
  testthat::expect_gt(flush_at[[1]], fire_at[[1]])
  testthat::expect_false(grepl("session$reload", obs, fixed = TRUE))
  testthat::expect_false(grepl("ynow_heavy_job_release", obs, fixed = TRUE))
  testthat::expect_match(obs, 'rate = if (isTRUE(focus)) "fast" else "slow"', fixed = TRUE)
  testthat::expect_match(obs, "if (!isTRUE(focus_click))", fixed = TRUE)
  testthat::expect_false(grepl("if (isTRUE(focus)) return()", obs, fixed = TRUE))

  cancel <- regmatches(srv_txt, regexpr(
    "\\.ynow_cancel_other_tab_work <- function\\(keep\\)[\\s\\S]*?\\n  \\}\n  observeEvent\\(input\\$ynow_tab_refresh",
    srv_txt,
    perl = TRUE
  ))
  testthat::expect_match(cancel, "ynow_logo_cancel_plan", fixed = TRUE)
  testthat::expect_match(cancel, "ticker_search_discard_gen", fixed = TRUE)
  testthat::expect_match(cancel, "lab_im_eval_pending(NULL)", fixed = TRUE)
  testthat::expect_match(cancel, "lab_im_ind_mcap_pending(NULL)", fixed = TRUE)
  testthat::expect_match(cancel, "lab_cluster_pending(NULL)", fixed = TRUE)
  testthat::expect_false(grepl("ynow_heavy_job_release", cancel, fixed = TRUE))
  testthat::expect_false(grepl(".ynow_fire_ticker_search", cancel, fixed = TRUE))
  testthat::expect_match(
    srv_txt,
    "ticker_search_discard_gen(NA_integer_)",
    fixed = TRUE
  )
})

testthat::test_that("logo focus plan cancels the other work only", {
  logo_env <- new.env(parent = baseenv())
  sys.source("ynow_server.R", envir = logo_env, keep.source = FALSE)
  plan <- get("ynow_logo_cancel_plan", envir = logo_env)
  focus_flag <- get("ynow_logo_payload_focus", envir = logo_env)
  stale <- get("ynow_logo_inflight_stale", envir = logo_env)

  raw <- regmatches(srv_txt, regexpr(
    "\\.ynow_ticker_refresh_tabs <- c\\([\\s\\S]*?\\)",
    srv_txt,
    perl = TRUE
  ))
  ticker_tabs <- eval(parse(text = sub(
    "^\\.ynow_ticker_refresh_tabs <-\\s*",
    "",
    raw
  )))

  dash <- plan("dashboard", ticker_tabs)
  testthat::expect_false(dash$bump_search)
  testthat::expect_true(dash$bump_bluechip)
  testthat::expect_true(dash$bump_cluster)

  notes <- plan("lab_notes", ticker_tabs)
  testthat::expect_false(notes$bump_search)
  testthat::expect_true(notes$bump_bluechip)
  testthat::expect_false(notes$bump_cluster)

  chip <- plan("bluechip", ticker_tabs)
  testthat::expect_true(chip$bump_search)
  testthat::expect_false(chip$bump_bluechip)
  testthat::expect_true(chip$bump_cluster)

  macro <- plan("macro_market", ticker_tabs)
  testthat::expect_true(macro$bump_search)
  testthat::expect_true(macro$bump_bluechip)
  testthat::expect_true(macro$bump_cluster)

  home <- plan("home", ticker_tabs)
  testthat::expect_true(all(unlist(home)))

  testthat::expect_true(focus_flag(TRUE))
  testthat::expect_true(focus_flag("true"))
  testthat::expect_true(focus_flag(1L))
  testthat::expect_false(focus_flag(FALSE))
  testthat::expect_false(focus_flag("false"))
  testthat::expect_false(focus_flag(NULL))
  testthat::expect_false(focus_flag(0))

  testthat::expect_false(stale(4L, 4L))
  testthat::expect_true(stale(4L, 5L))
  testthat::expect_false(stale(NULL, 5L))
  testthat::expect_false(stale(NA_integer_, 5L))
})

cat("PASS test_banner_logo_tab_refresh\n")
