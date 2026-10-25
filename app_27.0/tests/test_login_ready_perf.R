#!/usr/bin/env Rscript
# Login→ready perf contracts: schema cache + deferred heavy path wiring.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_login_ready_perf.R
options(warn = 1)
app_dir <- if (file.exists("points_wallet.R")) {
  normalizePath(".")
} else if (file.exists("app_27.0/points_wallet.R")) {
  normalizePath("app_27.0")
} else {
  stop("run from repo root or app_27.0/")
}
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 ||
        (is.atomic(a) && !nzchar(as.character(a)[1]))) b else a
}

n_fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "PASS " else "FAIL ", label, "\n", sep = "")
  if (!ok) n_fail <<- n_fail + 1L
}

source("auth_db.R", local = FALSE)
source("points_wallet.R", local = FALSE)
source("user_prefs.R", local = FALSE)
source("access_locks.R", local = FALSE)
source("maintenance_mode.R", local = FALSE)
source("admin_ops_audit.R", local = FALSE)

# --- Schema cache: second ensure is a no-op (no DB required) ---
ynow_points_schema_cache_reset()
ynow_prefs_schema_cache_reset()
ynow_auth_sessions_schema_cache_reset()
ynow_access_schema_cache_reset()
ynow_maintenance_schema_cache_reset()

check("points cache starts cold", !isTRUE(.ynow_points_schema_cache$ready))
.ynow_points_schema_cache$ready <- TRUE
n_calls <- 0L
# Force path with ready=TRUE must skip run body — mark ready and call ensure.
check(
  "points ensure short-circuits when ready",
  isTRUE(ynow_points_ensure_schema())
)
check("points still ready", isTRUE(.ynow_points_schema_cache$ready))

.ynow_prefs_schema_cache$ready <- TRUE
check("prefs ensure short-circuits", isTRUE(ynow_prefs_ensure_schema()))
.ynow_auth_sessions_schema_cache$ready <- TRUE
check("sessions ensure short-circuits", isTRUE(ynow_auth_sessions_ensure_schema()))
.ynow_access_schema_cache$ready <- TRUE
check("access ensure short-circuits", isTRUE(ynow_access_ensure_schema()))
.ynow_maint_schema_cache$ready <- TRUE
check("maintenance ensure short-circuits", isTRUE(ynow_maintenance_ensure_schema()))

ynow_points_schema_cache_reset()
check("points reset clears ready", !isTRUE(.ynow_points_schema_cache$ready))

# --- ACL effective uses entry helper (no per-row entries() re-fetch in source) ---
al <- paste(readLines("access_locks.R", warn = FALSE), collapse = "\n")
check(
  "effective_entries uses unlock_satisfied_entry",
  grepl("ynow_access_unlock_satisfied_entry", al, fixed = TRUE) &&
    grepl("ynow_access_get_unlock_states_for_user", al, fixed = TRUE)
)
check(
  "quota badges batch unlock states",
  grepl("ynow_access_get_unlock_states_for_user", al, fixed = TRUE)
)

# --- Auth chrome before points; deferred schedule ---
auth <- paste(readLines("auth_account.R", warn = FALSE), collapse = "\n")
check(
  "finish schedules heavy after chrome",
  grepl("ynow_schedule_auth_heavy", auth, fixed = TRUE) &&
    grepl("\\.push_auth_state\\(TRUE", auth)
)
# points_on_login should not appear before PostLogin in finish body.
finish_start <- regexpr("\\.finish_auth_success <- function", auth)
finish_end <- regexpr("\\.finish_auth_oauth_failure <- function", auth)
finish_body <- substr(auth, finish_start, finish_end)
post_pos <- regexpr("ynowAuthPostLogin", finish_body, fixed = TRUE)[1]
pts_pos <- regexpr("ynow_points_on_login", finish_body, fixed = TRUE)[1]
check(
  "finish PostLogin before deferred points helper",
  post_pos > 0 && (pts_pos < 0 || pts_pos > post_pos)
)
check(
  "resume schedules heavy / no sync points before PostLogin",
  grepl("ynow_schedule_auth_heavy", auth, fixed = TRUE)
)
# Mint / durable last-ui must not run before first PostLogin in finish/resume.
mint_pos <- regexpr(".persist_auth_browser", finish_body, fixed = TRUE)[1]
load_pos <- regexpr(".load_last_ui_surface", finish_body, fixed = TRUE)[1]
check(
  "finish defers mint and last-ui after PostLogin",
  post_pos > 0 && mint_pos > post_pos && load_pos > post_pos &&
    grepl("after_chrome", finish_body, fixed = TRUE)
)
resume_start <- regexpr("observeEvent(input$ynow_auth_resume_token", auth, fixed = TRUE)
resume_end <- regexpr("observeEvent(input$ynow_auth_submit", auth, fixed = TRUE)
resume_body <- substr(auth, resume_start, resume_end)
r_post <- regexpr("ynowAuthPostLogin", resume_body, fixed = TRUE)[1]
r_load <- regexpr(".load_last_ui_surface", resume_body, fixed = TRUE)[1]
check(
  "resume defers last-ui after chrome PostLogin",
  r_post > 0 && r_load > r_post && grepl("after_chrome", resume_body, fixed = TRUE)
)

srv <- paste(readLines("ynow_server.R", warn = FALSE), collapse = "\n")
check(
  "server defines schedule + heavy tick",
  grepl("ynow_auth_heavy_tick", srv, fixed = TRUE) &&
    grepl("ynow_schedule_auth_heavy", srv, fixed = TRUE) &&
    grepl("after_chrome", srv, fixed = TRUE)
)
# Login-class ACL (e.g. tab:asset_transmission) must clear on the first
# post-chrome flush, before points_on_login, so ATX unlocks like Checklist.
sched_start <- regexpr("ynow_schedule_auth_heavy <- function", srv, fixed = TRUE)
sched_end <- regexpr("ynow_pts_view_as <- reactiveVal", srv, fixed = TRUE)
sched_body <- substr(srv, sched_start, sched_end)
acl_pos <- regexpr(".ynow_push_access_locks()", sched_body, fixed = TRUE)[1]
pts_sched_pos <- regexpr("ynow_points_on_login", sched_body, fixed = TRUE)[1]
check(
  "post-chrome flush pushes ACL before points",
  acl_pos > 0 && pts_sched_pos > 0 && acl_pos < pts_sched_pos
)
check(
  "banner bundle defaults without ledger",
  grepl("include_ledger = FALSE", srv, fixed = TRUE) ||
    grepl("include_ledger = FALSE", srv, fixed = TRUE)
)
check(
  "points panel still requests ledger",
  grepl("include_ledger = TRUE", srv, fixed = TRUE)
)

# Self-view apply (guest / signed-in / omniscient): chrome before ACL / audit DB.
apply_start <- regexpr(".ynow_self_view_apply <- function", srv, fixed = TRUE)
apply_end <- regexpr(".ynow_self_view_clear <- function", srv, fixed = TRUE)
apply_body <- substr(srv, apply_start, apply_end)
chrome_pos <- regexpr(".ynow_self_view_push_chrome(mode)", apply_body, fixed = TRUE)[1]
flush_pos <- regexpr("session$onFlushed", apply_body, fixed = TRUE)[1]
acl_apply_pos <- regexpr(".ynow_push_access_locks()", apply_body, fixed = TRUE)[1]
check(
  "self-view chrome before deferred ACL flush",
  chrome_pos > 0 && flush_pos > chrome_pos && acl_apply_pos > flush_pos
)
enter_start <- regexpr("observeEvent(input$ynow_pts_sysadmin_enter", srv, fixed = TRUE)
enter_end <- regexpr("observeEvent(input$ynow_pts_sysadmin_exit", srv, fixed = TRUE)
enter_body <- substr(srv, enter_start, enter_end)
check(
  "sysadmin enter clears self-view; defers enter log + ACL",
  grepl("self_view=none", enter_body, fixed = TRUE) &&
    grepl("ynow_pts_self_view_mode(NULL)", enter_body, fixed = TRUE) &&
    grepl(".ynow_self_view_push_chrome(NULL)", enter_body, fixed = TRUE) &&
    grepl("sysadmin.enter", enter_body, fixed = TRUE) &&
    grepl("session$onFlushed", enter_body, fixed = TRUE) &&
    !grepl(".ynow_self_view_apply(", enter_body, fixed = TRUE)
)
check(
  "sysadmin enter pins testing tab after unlock",
  grepl('tab = "testing"', enter_body, fixed = TRUE) ||
    grepl("\"testing\"", enter_body, fixed = TRUE)
)
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
check(
  "sysadmin foot hide exempts unlocked portal",
  grepl("body.ynow-authed:not(.ynow-sysadmin-on)", ui_src, fixed = TRUE) &&
    grepl("ynowSyncSysAdminFootVisibility", ui_src, fixed = TRUE)
)
check(
  "gotoTab allows testing while sysadmin portal unlocked",
  grepl("!document.body.classList.contains('ynow-sysadmin-on')", ui_src, fixed = TRUE) &&
    grepl("function gotoTab(tab)", ui_src, fixed = TRUE) &&
    grepl("function showSidebarFootTab(tab)", ui_src, fixed = TRUE)
)
check(
  "self-view does not hide sysadmin box (control panel nested inside)",
  !grepl("body.ynow-self-view .ynow-pts-sysadmin-box", ui_src, fixed = TRUE) &&
    grepl("body.ynow-self-view #ynow_pts_sysadmin_panel", ui_src, fixed = TRUE) &&
    grepl("body.ynow-self-view .ynow-sysadmin-login-chrome", ui_src, fixed = TRUE)
)
if (exists("ynow_admin_ops_schema_cache_reset", mode = "function")) {
  ynow_admin_ops_schema_cache_reset()
  .ynow_admin_ops_schema_cache$ready <- TRUE
  check(
    "admin_ops ensure short-circuits when ready",
    isTRUE(ynow_admin_ops_ensure_schema())
  )
} else {
  check("admin_ops schema cache helper present", FALSE)
}

# Memory ACL unlock eval still works without DB
uid <- "user-perf-test"
if (exists("ynow_access_set", mode = "function")) {
  # Soft check: satisfied_entry with NULL entry → TRUE
  check(
    "unlock_satisfied_entry NULL is free",
    isTRUE(ynow_access_unlock_satisfied_entry(uid, "x", NULL, authed = TRUE))
  )
}

# --- Resume slim: second on_login same day skips credit apply ---
Sys.setenv(YNOW_POINTS_BACKEND = "memory", YNOW_POINTS_ENABLED = "1")
mem_uid <- "user-perf-login-slim"
ynow_points_mem_register_profile(mem_uid, role = "user")
day1 <- as.POSIXct("2026-10-24 10:00:00", tz = "Asia/Taipei")
first <- ynow_points_on_login(mem_uid, when = day1)
check("first login grants something or ok", isTRUE(first$ok))
second <- ynow_points_on_login(mem_uid, when = day1)
check("resume slim flag set", isTRUE(second$resume_slim))
check("resume slim no new grants", !isTRUE(second$gift_granted) && !isTRUE(second$checkin_granted))
check(
  "grants_done helper sees both",
  {
    d <- .ynow_points_login_grants_done(mem_uid, when = day1)
    isTRUE(d$signup) && isTRUE(d$checkin)
  }
)

if (n_fail > 0L) {
  cat("FAILED ", n_fail, "\n", sep = "")
  quit(status = 1)
}
cat("ALL PASS\n")
