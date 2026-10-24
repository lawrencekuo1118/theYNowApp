#!/usr/bin/env Rscript
# Post-login lock gate mode matrix (ACL overlay Sign-in vs spend vs top-up).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

ok <- 0L
bad <- 0L
check <- function(name, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK ", name)
  } else {
    bad <<- bad + 1L
    message("FAIL ", name)
  }
}

app_dir <- normalizePath("..", mustWork = TRUE)
if (!grepl("app_27.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}
source(file.path(app_dir, "access_locks.R"), local = FALSE)

check("guest + points cost -> signin", {
  identical(ynow_lock_gate_mode(FALSE, "points", 25L), "signin")
})
check("guest + sticky -> signin", {
  identical(ynow_lock_gate_mode(FALSE, NA_character_, NA_integer_), "signin")
})
check("authed + points 25 -> spend", {
  identical(ynow_lock_gate_mode(TRUE, "points", 25L), "spend")
})
check("authed + points 1 -> spend", {
  identical(ynow_lock_gate_mode(TRUE, "points", 1L), "spend")
})
check("authed + points NA cost -> topup", {
  identical(ynow_lock_gate_mode(TRUE, "points", NA_integer_), "topup")
})
check("authed + points 0 -> topup", {
  identical(ynow_lock_gate_mode(TRUE, "points", 0L), "topup")
})
check("authed + conditional -> topup", {
  identical(ynow_lock_gate_mode(TRUE, "conditional", NA_integer_), "topup")
})
check("authed + sticky NA -> topup", {
  identical(ynow_lock_gate_mode(TRUE, NA_character_, NA_integer_), "topup")
})
check("authed + empty unlock -> topup", {
  identical(ynow_lock_gate_mode(TRUE, "", NA_integer_), "topup")
})
check("authed + login stale -> topup", {
  identical(ynow_lock_gate_mode(TRUE, "login", NA_integer_), "topup")
})
check("authed never signin", {
  modes <- c(
    ynow_lock_gate_mode(TRUE, "points", 5L),
    ynow_lock_gate_mode(TRUE, NA_character_, NA_integer_),
    ynow_lock_gate_mode(TRUE, "conditional", 3L),
    ynow_lock_gate_mode(TRUE, "login", 1L)
  )
  !any(modes == "signin")
})

row_pts <- ynow_access_lock_client_row(
  "tab:lab_notes", "lock", unlock_type = "points", points_cost = 7L
)
check("client row points includes type+cost", {
  identical(row_pts$id, "tab:lab_notes") &&
    identical(row_pts$effect, "lock") &&
    identical(row_pts$unlock_type, "points") &&
    identical(as.integer(row_pts$points_cost), 7L)
})
row_sticky <- ynow_access_lock_client_row(
  "tab:asset_transmission", "lock",
  unlock_type = NA_character_, points_cost = NA_integer_
)
check("client row sticky omits type+cost", {
  is.null(row_sticky$unlock_type) && is.null(row_sticky$points_cost)
})
row_hide <- ynow_access_lock_client_row(
  "tab:lab_notes", "hide", unlock_type = "points", points_cost = 3L
)
check("client row hide drops unlock fields", {
  identical(row_hide$effect, "hide") &&
    is.null(row_hide$unlock_type) && is.null(row_hide$points_cost)
})

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("UI defines ynowResolveLockGateMode", {
  grepl("ynowResolveLockGateMode", ui, fixed = TRUE)
})
check("UI defines ynowAdaptPostLoginLockGate", {
  grepl("ynowAdaptPostLoginLockGate", ui, fixed = TRUE)
})
check("UI spend class is not auth-open", {
  grepl("ynow-lock-spend", ui, fixed = TRUE) &&
    grepl("ynow-lock-topup", ui, fixed = TRUE) &&
    grepl("ynow-lock-gate-actions", ui, fixed = TRUE)
})
check("UI has no Mainland 一鍵 in lock gate path", {
  !grepl("一鍵解鎖", ui, fixed = TRUE)
})

auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE), collapse = "\n")
check("authed CSS shows lock-gate-actions", {
  grepl("ynow-lock-gate-actions", auth, fixed = TRUE) &&
    grepl("body.ynow-authed .ynow-lock-gate-actions", auth, fixed = TRUE)
})

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("server observes ynow_lock_spend", {
  grepl("ynow_lock_spend", srv, fixed = TRUE) &&
    grepl("acl-unlock:", srv, fixed = TRUE)
})
check("push uses ynow_access_lock_client_row", {
  grepl("ynow_access_lock_client_row", srv, fixed = TRUE)
})

loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE), collapse = "\n")
check("locale has lock_gate spend/topup en+zh", {
  grepl("lock_gate_spend_title", loc, fixed = TRUE) &&
    grepl("lock_gate_topup_cta", loc, fixed = TRUE) &&
    grepl("以 FYN 解鎖", loc, fixed = TRUE) &&
    grepl("前往點數儲值", loc, fixed = TRUE) &&
    !grepl("一鍵解鎖", loc, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
quit(status = if (bad > 0L) 1L else 0L)
