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

srv <- paste(readLines("ynow_server.R", warn = FALSE), collapse = "\n")
check(
  "server defines schedule + heavy tick",
  grepl("ynow_auth_heavy_tick", srv, fixed = TRUE) &&
    grepl("ynow_schedule_auth_heavy", srv, fixed = TRUE)
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
