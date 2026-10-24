#!/usr/bin/env Rscript
# Asset Transmission vs Checklist: freemium A + login-class ACL unlock timing.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_atx_login_acl_unlock.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- if (file.exists("membership_locks.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../membership_locks.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate membership_locks.R")
}
setwd(app_dir)

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

source("lock_catalog_shared.R", local = TRUE)
source("membership_locks.R", local = TRUE)
source("auth_account.R", local = TRUE)
source("access_locks.R", local = TRUE)

check("ATX sidebar is freemium A", identical(ynow_membership_lock_tier("asset_transmission"), "A"))
check("Checklist sidebar is freemium A", identical(ynow_membership_lock_tier("decision_checklist"), "A"))
check(
  "ATX in guest lock set like Checklist",
  "asset_transmission" %in% ynow_sidebar_guest_lock_tabs() &&
    "decision_checklist" %in% ynow_sidebar_guest_lock_tabs()
)
check(
  "persist B map excludes ATX",
  !"asset_transmission" %in% names(ynow_sidebar_persist_lock_js_map())
)

.ynow_atx_acl_clean <- function() {
  ynow_access_reset_memory()
  # access_set also writes data/access_locks.json — drop it so cases stay isolated.
  path <- tryCatch(.ynow_access_file(), error = function(e) "data/access_locks.json")
  if (is.character(path) && nzchar(path) && file.exists(path)) {
    unlink(path)
  }
  invisible(TRUE)
}

.ynow_atx_acl_clean()
ynow_access_set(
  "__all__", "tab:asset_transmission", TRUE,
  effect = "lock", unlock_type = "login"
)
guest_ent <- ynow_access_effective_entries("", authed = FALSE)
member_ent <- ynow_access_effective_entries("u-atx-login", authed = TRUE)
check(
  "login ACL locks ATX for guest",
  is.data.frame(guest_ent) &&
    "tab:asset_transmission" %in% as.character(guest_ent$id)
)
check(
  "login ACL clears for signed-in member (Checklist-parity unlock)",
  is.data.frame(member_ent) &&
    !"tab:asset_transmission" %in% as.character(member_ent$id)
)

.ynow_atx_acl_clean()
ynow_access_set(
  "__all__", "tab:asset_transmission", TRUE,
  effect = "lock", unlock_type = "points", points_cost = 5L
)
points_ent <- ynow_access_effective_entries("u-atx-pts", authed = TRUE)
check(
  "points ACL stays locked after login until FYN spend",
  is.data.frame(points_ent) &&
    "tab:asset_transmission" %in% as.character(points_ent$id)
)
.ynow_atx_acl_clean()

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sched_start <- regexpr("ynow_schedule_auth_heavy <- function", srv, fixed = TRUE)
sched_end <- regexpr("ynow_pts_view_as <- reactiveVal", srv, fixed = TRUE)
sched_body <- substr(srv, sched_start, sched_end)
acl_pos <- regexpr(".ynow_push_access_locks()", sched_body, fixed = TRUE)[1]
pts_pos <- regexpr("ynow_points_on_login", sched_body, fixed = TRUE)[1]
check(
  "post-chrome schedule pushes ACL before points waterfall",
  acl_pos > 0L && pts_pos > 0L && acl_pos < pts_pos
)

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
