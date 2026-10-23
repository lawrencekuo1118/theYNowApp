# System-admin ops audit log: append, 24h expiry, no delete UI.
# Offline — force file/memory store; no network / shinyapps.

options(warn = 1)
app_dir <- if (dir.exists("app_27.0")) {
  normalizePath("app_27.0")
} else if (basename(getwd()) == "tests" && dir.exists("..")) {
  normalizePath("..")
} else {
  normalizePath(getwd())
}

tmpdir <- tempfile("ynow_admin_ops_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
audit_path <- file.path(tmpdir, "admin_ops_audit.json")
Sys.setenv(
  YNOW_ADMIN_OPS_AUDIT_FORCE_FILE = "1",
  YNOW_ADMIN_OPS_AUDIT_PATH = audit_path,
  YNOW_DEBUG_SKIP_PY = "1"
)

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "admin_ops_audit.R"), local = FALSE)

.pass <- 0L
.fail <- 0L
check <- function(label, expr) {
  ok <- isTRUE(tryCatch(expr, error = function(e) {
    message("ERROR [", label, "]: ", conditionMessage(e))
    FALSE
  }))
  if (ok) {
    .pass <<- .pass + 1L
    cat("PASS:", label, "\n")
  } else {
    .fail <<- .fail + 1L
    cat("FAIL:", label, "\n")
  }
  invisible(ok)
}

ynow_admin_ops_reset_memory()
unlink(audit_path, force = TRUE)

check("append writes one row with required fields", {
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-22T12:00:00Z", tz = "UTC"))
  res <- ynow_admin_ops_append(
    action = "acl.apply",
    detail = "item=tab:lab_notes; scope=all",
    actor = "ynow"
  )
  isTRUE(res$ok) && nzchar(res$id) && {
    df <- ynow_admin_ops_list()
    nrow(df) == 1L &&
      identical(as.character(df$action[[1]]), "acl.apply") &&
      identical(as.character(df$actor[[1]]), "ynow") &&
      grepl("tab:lab_notes", as.character(df$detail[[1]]), fixed = TRUE) &&
      !is.na(df$created_at[[1]])
  }
})

check("list is newest-first after second append", {
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-22T12:05:00Z", tz = "UTC"))
  ynow_admin_ops_append(
    action = "maintenance.set",
    detail = "item=tab:home",
    actor = "ynow"
  )
  df <- ynow_admin_ops_list()
  nrow(df) >= 2L &&
    identical(as.character(df$action[[1]]), "maintenance.set") &&
    identical(as.character(df$action[[2]]), "acl.apply")
})

check("taxonomy covers required admin mutations", {
  need <- c(
    "sysadmin.enter", "sysadmin.exit",
    "acl.apply", "acl.remove",
    "maintenance.set", "maintenance.clear",
    "view_as.enter", "view_as.exit",
    "self_view.apply", "self_view.exit",
    "points.adjust"
  )
  all(need %in% YNOW_ADMIN_OPS_ACTIONS)
})

check("rows older than 24h are purged on list", {
  ynow_admin_ops_reset_memory()
  unlink(audit_path, force = TRUE)
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-21T10:00:00Z", tz = "UTC"))
  ynow_admin_ops_append(
    action = "points.adjust",
    detail = "old",
    actor = "ynow",
    created_at = as.POSIXct("2026-10-21T10:00:00Z", tz = "UTC")
  )
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-22T11:00:01Z", tz = "UTC"))
  ynow_admin_ops_append(
    action = "sysadmin.enter",
    detail = "fresh",
    actor = "ynow",
    created_at = as.POSIXct("2026-10-22T11:00:01Z", tz = "UTC")
  )
  df <- ynow_admin_ops_list(now = as.POSIXct("2026-10-22T11:00:01Z", tz = "UTC"))
  nrow(df) == 1L &&
    identical(as.character(df$action[[1]]), "sysadmin.enter") &&
    identical(as.character(df$detail[[1]]), "fresh")
})

check("purge drops exclusively expired store (TTL = 24h)", {
  ynow_admin_ops_reset_memory()
  unlink(audit_path, force = TRUE)
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-20T00:00:00Z", tz = "UTC"))
  ynow_admin_ops_append(
    action = "view_as.enter",
    detail = "stale",
    actor = "ynow"
  )
  ynow_admin_ops_set_now_override(as.POSIXct("2026-10-22T00:00:01Z", tz = "UTC"))
  df <- ynow_admin_ops_purge_expired()
  nrow(df) == 0L
})

check("action labels exist for en and zh-TW", {
  en <- ynow_admin_ops_action_label("acl.apply", "en")
  zh <- ynow_admin_ops_action_label("acl.apply", "zh-TW")
  nzchar(en) && nzchar(zh) && !identical(en, zh)
})

# --- UI / i18n static checks ---
ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glb <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global sources admin_ops_audit.R + ensure schema", {
  grepl('source\\("admin_ops_audit\\.R"', glb) &&
    grepl("ynow_admin_ops_ensure_schema", glb, fixed = TRUE)
})

check("i18n keys for audit tab (en + zh-TW)", {
  grepl("admin_ops_tab\\s*=\\s*\"Audit log\"", loc) &&
    grepl("admin_ops_tab\\s*=\\s*\"操作紀錄\"", loc) &&
    grepl("admin_ops_empty\\s*=", loc) &&
    grepl("admin_ops_help\\s*=", loc) &&
    grepl("admin_ops_controls_tab\\s*=", loc)
})

check("server has audit tab + list UI, no clear/delete controls", {
  grepl("ynow_admin_ops_audit_list", srv, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_tabs", srv, fixed = TRUE) &&
    grepl("admin_ops_tab", srv, fixed = TRUE) &&
    grepl("\\.ynow_admin_ops_log", srv) &&
    !grepl("ynow_admin_ops_clear", srv, fixed = TRUE) &&
    !grepl("ynow_admin_ops_delete", srv, fixed = TRUE) &&
    !grepl("admin_ops_clear_btn", srv, fixed = TRUE) &&
    !grepl("admin_ops_delete_btn", srv, fixed = TRUE)
})

check("server logs required mutation paths", {
  grepl('"sysadmin.enter"', srv, fixed = TRUE) &&
    grepl('"sysadmin.exit"', srv, fixed = TRUE) &&
    grepl('"acl.apply"', srv, fixed = TRUE) &&
    grepl('"acl.remove"', srv, fixed = TRUE) &&
    grepl('"maintenance.set"', srv, fixed = TRUE) &&
    grepl('"maintenance.clear"', srv, fixed = TRUE) &&
    grepl('"view_as.enter"', srv, fixed = TRUE) &&
    grepl('"view_as.exit"', srv, fixed = TRUE) &&
    grepl('"self_view.apply"', srv, fixed = TRUE) &&
    grepl('"self_view.exit"', srv, fixed = TRUE) &&
    grepl('"points.adjust"', srv, fixed = TRUE)
})

check("locale applyUiLocale wires audit tab labels", {
  grepl("ynow_admin_ops_tab_label", ui, fixed = TRUE) &&
    grepl("admin_ops_tab", ui, fixed = TRUE) &&
    grepl("ynow_admin_ops_controls_tab_label", ui, fixed = TRUE)
})

check("no delete/clear buttons in audit pane markup ids", {
  !grepl("ynow_admin_ops_clear", ui, fixed = TRUE) &&
    !grepl("ynow_admin_ops_delete", ui, fixed = TRUE) &&
    !grepl("admin_ops_clear_btn", loc, fixed = TRUE) &&
    !grepl("admin_ops_delete_btn", loc, fixed = TRUE)
})

ynow_admin_ops_reset_memory()
unlink(tmpdir, recursive = TRUE, force = TRUE)

cat("\nadmin_ops_audit tests: ", .pass, " passed, ", .fail, " failed\n", sep = "")
if (.fail > 0L) quit(status = 1L)
invisible(TRUE)
