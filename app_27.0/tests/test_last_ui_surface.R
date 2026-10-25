#!/usr/bin/env Rscript
# Last authenticated UI surface (tab + ticker) — prefs + auth/UI contracts.
# Run: cd app_27.0 && Rscript tests/test_last_ui_surface.R

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "user_prefs.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

Sys.setenv(YNOW_USER_PREFS_FORCE_FILE = "1")
tmpdir <- tempfile("ynow_prefs_last_ui_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
old_wd <- getwd()
on.exit({
  setwd(old_wd)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  Sys.unsetenv("YNOW_USER_PREFS_FORCE_FILE")
}, add = TRUE)
setwd(tmpdir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0L)
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")
ynow_prefs_reset_memory()

check("constant defined", identical(YNOW_PREF_LAST_UI_SURFACE, "last_ui_surface"))

norm_empty <- ynow_prefs_normalize_last_ui_surface(NULL)
check("normalize null empty tab", identical(norm_empty$tab, ""))
check("normalize null empty ticker", identical(norm_empty$ticker, ""))

norm_test <- ynow_prefs_normalize_last_ui_surface(list(tab = "testing", ticker = "AAPL"))
check("normalize drops testing tab", identical(norm_test$tab, ""))
check("normalize keeps ticker when tab dropped", identical(norm_test$ticker, "AAPL"))

norm_bad <- ynow_prefs_normalize_last_ui_surface(list(tab = "not_a_real_tab", ticker = "msft"))
check("normalize drops unknown tab", identical(norm_bad$tab, ""))
check("normalize uppercases ticker", identical(norm_bad$ticker, "MSFT"))

norm_ok <- ynow_prefs_normalize_last_ui_surface(
  list(tab = "dcf_calculator", ticker = "aapl")
)
check("normalize keeps dcf tab", identical(norm_ok$tab, "dcf_calculator"))
check("normalize ticker AAPL", identical(norm_ok$ticker, "AAPL"))

norm_legacy <- ynow_prefs_normalize_last_ui_surface(
  list(tab = "business_breakdown_lab", ticker = "")
)
check("normalize remaps BB lab tab", identical(norm_legacy$tab, "company_advance"))

norm_json <- ynow_prefs_normalize_last_ui_surface(
  '{"tab":"hfv","ticker":"brk.b"}'
)
check(
  "normalize json string from prefs store",
  identical(norm_json$tab, "hfv") && identical(norm_json$ticker, "BRK.B")
)

uid <- "test-user-last-ui"
check(
  "empty get",
  identical(ynow_prefs_get_last_ui_surface(uid), ynow_prefs_empty_last_ui_surface())
)

saved <- ynow_prefs_set_last_ui_surface(
  uid,
  list(tab = "dcf_calculator", ticker = "TSLA")
)
check("set returns tab", identical(saved$tab, "dcf_calculator"))
check("set returns ticker", identical(saved$ticker, "TSLA"))
check(
  "get after set",
  identical(ynow_prefs_get_last_ui_surface(uid)$tab, "dcf_calculator")
)

ynow_prefs_reset_memory()
reloaded <- ynow_prefs_get_last_ui_surface(uid)
check("file reload tab", identical(reloaded$tab, "dcf_calculator"))
check("file reload ticker", identical(reloaded$ticker, "TSLA"))

upd <- ynow_prefs_update_last_ui_surface(uid, tab = "macro_market")
check("update tab keeps ticker", identical(upd$tab, "macro_market") &&
        identical(upd$ticker, "TSLA"))
upd2 <- ynow_prefs_update_last_ui_surface(uid, ticker = "NVDA")
check("update ticker keeps tab", identical(upd2$tab, "macro_market") &&
        identical(upd2$ticker, "NVDA"))

# Contracts: resume restores durable surface; in-session finish keeps chrome tab
# (see test_login_redirect_current_tab.R). Client localStorage key.
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
            collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")

check(
  "sign_out persists last UI",
  grepl(".persist_last_ui_surface", auth, fixed = TRUE) &&
    grepl(".sign_out <- function", auth, fixed = TRUE)
)
check(
  "finish_auth loads last UI surface",
  grepl(".load_last_ui_surface", auth, fixed = TRUE) &&
    grepl("restore_ticker", auth, fixed = TRUE)
)
check(
  "resume path restores last UI",
  grepl("ynow_auth_resume_token", auth, fixed = TRUE) &&
    grepl("ynowAuthPostLogin", auth, fixed = TRUE) &&
    grepl("restore_ticker", auth, fixed = TRUE)
)
check(
  "client localStorage last UI key",
  grepl("ynow_last_ui_v1", ui, fixed = TRUE)
)
check(
  "client early restore on resume offer",
  grepl("ynowAuthResumeOffer", ui, fixed = TRUE) &&
    grepl("ynowLastUiRead", ui, fixed = TRUE) &&
    grepl("ynowApplyRestoredTab", ui, fixed = TRUE)
)
check(
  "server persists tab while signed in",
  grepl("ynow_prefs_update_last_ui_surface", srv, fixed = TRUE)
)
check(
  "server restores ticker via input",
  grepl("ynow_auth_restore_ticker", srv, fixed = TRUE)
)

between <- function(src, a, b) {
  ia <- regexpr(a, src, fixed = TRUE)
  ib <- regexpr(b, src, fixed = TRUE)
  if (ia < 1L || ib < 1L || ib <= ia) return("")
  substr(src, as.integer(ia), as.integer(ib) - 1L)
}
finish_body <- between(
  auth,
  ".finish_auth_success <- function",
  ".finish_auth_oauth_failure <- function"
)
resume_body <- between(
  auth,
  "observeEvent(input$ynow_auth_resume_token",
  "observeEvent(input$ynow_auth_submit"
)
sign_body <- between(auth, ".sign_out <- function", ".open_auth_modal <- function")
load_body <- between(auth, ".load_last_ui_surface <- function", ".sign_out <- function")
check(
  "finish chrome PostLogin before durable last-ui load",
  {
    post_at <- regexpr("ynowAuthPostLogin", finish_body, fixed = TRUE)[1]
    load_at <- regexpr(".load_last_ui_surface", finish_body, fixed = TRUE)[1]
    post_at > 0L && load_at > post_at &&
      grepl("after_chrome", finish_body, fixed = TRUE) &&
      grepl("ynowAuthPostLogin", finish_body, fixed = TRUE)
  }
)
check(
  "resume chrome PostLogin before durable last-ui load",
  {
    post_at <- regexpr("ynowAuthPostLogin", resume_body, fixed = TRUE)[1]
    load_at <- regexpr(".load_last_ui_surface", resume_body, fixed = TRUE)[1]
    post_at > 0L && load_at > post_at &&
      grepl("after_chrome", resume_body, fixed = TRUE) &&
      grepl("ynowAuthPostLogin", resume_body, fixed = TRUE)
  }
)
check(
  "load reads get_last_ui_surface",
  grepl("ynow_prefs_get_last_ui_surface", load_body, fixed = TRUE)
)
save_at <- regexpr(".persist_last_ui_surface", sign_body, fixed = TRUE)
clear_at <- regexpr("account_user(NULL)", sign_body, fixed = TRUE)
check(
  "sign-out saves surface before clearing auth",
  save_at > 0L && clear_at > save_at
)

ticker_fn <- between(ui, "var ynowApplyRestoredTicker", "var ynowAuthResumeOffer")
check(
  "client sets sc on restore",
  grepl("setInputValue('sc'", ticker_fn, fixed = TRUE)
)
check(
  "client sets ticker_typeahead on restore",
  grepl("ticker_typeahead", ticker_fn, fixed = TRUE)
)
sidebar_mirror <- between(ui, "e.name !== 'sidebar_tabs'", "eLastUi")
check(
  "authed sidebar updates last UI mirror",
  grepl("ynow-authed", sidebar_mirror, fixed = TRUE) &&
    grepl("ynowLastUiWrite", sidebar_mirror, fixed = TRUE)
)

uid_helper <- between(
  srv,
  ".ynow_signed_in_uid <- function",
  "Persist last member screen"
)
check(
  "signed-in uid is not the view-as subject",
  nzchar(uid_helper) && !grepl("ynow_pts_view_as", uid_helper, fixed = TRUE)
)
upd_hits <- gregexpr("ynow_prefs_update_last_ui_surface(", srv, fixed = TRUE)[[1]]
check(
  "surface writes use signed-in uid",
  length(upd_hits) >= 1L && upd_hits[[1]] > 0L && all(vapply(upd_hits, function(pos) {
    chunk <- substr(srv, max(1L, pos - 500L), pos)
    grepl(".ynow_signed_in_uid", chunk, fixed = TRUE)
  }, logical(1)))
)

if (fail > 0L) {
  cat("FAIL count=", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("ALL PASS\n")
