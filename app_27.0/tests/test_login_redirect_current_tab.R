#!/usr/bin/env Rscript
# In-session login must stay on the current tab (not Home / get_started).
# Durable last-tab prefs apply on browser resume only.
# Run: cd app_27.0 && Rscript tests/test_login_redirect_current_tab.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
root <- if (file.exists(file.path(test_dir, "..", "auth_account.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("auth_account.R")) {
  normalizePath(".")
} else {
  stop("Cannot locate auth_account.R")
}
setwd(root)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

between <- function(src, a, b) {
  ia <- regexpr(a, src, fixed = TRUE)
  ib <- regexpr(b, src, fixed = TRUE)
  if (ia < 1L || ib < 1L || ib <= ia) return("")
  substr(src, as.integer(ia), as.integer(ib) - 1L)
}

auth <- paste(readLines("auth_account.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

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
persist_fn <- between(
  ui,
  "window.ynowPersistPreAuthTab = function",
  "function ynowParseQuery"
)
post_login <- between(
  ui,
  "addHandler('ynowAuthPostLogin'",
  "addHandler('ynowAuthOpenModal'"
)
oauth_start <- between(
  ui,
  "window.ynowStartOAuthRedirect = function",
  "addHandler('ynowAuthOAuthRedirect'"
)
request_modal <- between(
  ui,
  "function ynowRequestAuthModal()",
  "window.ynowRequestAuthModal = ynowRequestAuthModal"
)
gate_capture <- between(
  ui,
  "window.__ynowAuthOpenCapture = true",
  "function ynowClearSidebarGuide"
)

check("finish_auth body present", nchar(finish_body) > 200L)
check(
  "finish chrome uses restore_tab / sidebar_tabs before durable load",
  {
    post_at <- regexpr("ynowAuthPostLogin", finish_body, fixed = TRUE)[1]
    load_at <- regexpr(".load_last_ui_surface", finish_body, fixed = TRUE)[1]
    post_at > 0L && load_at > post_at
  }
)
check(
  "in-session login prefers chrome_tab over durable stab",
  grepl("chrome_tab", finish_body, fixed = TRUE) &&
    grepl("stay_tab", finish_body, fixed = TRUE) &&
    grepl("tab = stay_tab", finish_body, fixed = TRUE) &&
    !grepl("if (nzchar(stab)) stab else chrome_tab", finish_body, fixed = TRUE) &&
    !grepl("Durable prefs win when present", finish_body, fixed = TRUE)
)
check(
  "in-session after_chrome does not apply saved$tab as navigation target",
  {
    apply_chunk <- between(
      finish_body,
      ".ynow_auth_apply_surface(",
      ".persist_last_ui_surface("
    )
    grepl("tab = stay_tab", apply_chunk, fixed = TRUE) &&
      !grepl("saved$tab", apply_chunk, fixed = TRUE) &&
      !grepl("\\bstab\\b", apply_chunk)
  }
)
check(
  "resume path still restores durable last-tab",
  grepl(".load_last_ui_surface", resume_body, fixed = TRUE) &&
    grepl("tab = stab", resume_body, fixed = TRUE) &&
    grepl("ynowAuthPostLogin", resume_body, fixed = TRUE)
)
check(
  "password login passes ynow_auth_restore_tab",
  grepl(
    "restore_tab = as.character(input$ynow_auth_restore_tab",
    auth,
    fixed = TRUE
  )
)
check(
  "OAuth callback restore_tab wired",
  grepl("restore_tab = restore_tab", auth, fixed = TRUE) ||
    grepl("restore_tab = as.character(payload$restore_tab", auth, fixed = TRUE) ||
    grepl("restore_tab <- as.character(payload$restore_tab", auth, fixed = TRUE)
)
check(
  "PersistPreAuthTab captures sidebar + foot + hash",
  grepl("ynow_auth_restore_tab", persist_fn, fixed = TRUE) &&
    grepl("ynow-sidebar-snapshot-foot", persist_fn, fixed = TRUE) &&
    grepl("location.hash", persist_fn, fixed = TRUE) &&
    grepl("ynow_pre_oauth_tab", persist_fn, fixed = TRUE)
)
check(
  "RequestAuthModal persists pre-auth tab (gates + header)",
  grepl("ynowPersistPreAuthTab", request_modal, fixed = TRUE)
)
check(
  "gate Sign-in CTAs open auth modal via capture",
  grepl("ynow_pa_gate_signin", gate_capture, fixed = TRUE) &&
    grepl("ynow_lab_gate_signin", gate_capture, fixed = TRUE) &&
    grepl("ynow_chk_gate_signin", gate_capture, fixed = TRUE) &&
    grepl("ynowRequestAuthModal", gate_capture, fixed = TRUE)
)
check(
  "PostLogin falls back to active tab when restore empty (non-quiet)",
  grepl("!payload.quiet", post_login, fixed = TRUE) &&
    grepl("ynowApplyRestoredTab", post_login, fixed = TRUE) &&
    grepl("sidebar-menu li.active", post_login, fixed = TRUE)
)
check(
  "OAuth redirect persists current tab before leave",
  grepl("ynow_pre_oauth_tab", oauth_start, fixed = TRUE) &&
    (grepl("ynowPersistPreAuthTab", oauth_start, fixed = TRUE) ||
       grepl("sidebar-menu li.active", oauth_start, fixed = TRUE))
)
check(
  "never force get_started on PostLogin success path",
  !grepl("get_started", post_login, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAIL count=", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("ALL PASS\n")
