#!/usr/bin/env Rscript
# System admin maintenance windows: active/inactive, auto-lift, non-clickable chrome, i18n.
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
Sys.setenv(YNOW_MAINTENANCE_FORCE_FILE = "1")
Sys.setenv(YNOW_ACCESS_LOCKS_FORCE_FILE = "1")
tmp <- tempfile(fileext = ".json")
tmp_acl <- tempfile(fileext = ".json")
Sys.setenv(YNOW_MAINTENANCE_PATH = tmp)
Sys.setenv(YNOW_ACCESS_LOCKS_PATH = tmp_acl)
on.exit({
  unlink(tmp)
  unlink(tmp_acl)
}, add = TRUE)

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

# Resolve app_26.0/ whether invoked from repo root or from tests/ cwd.
.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.script_path <- if (length(.file_arg)) {
  normalizePath(sub("^--file=", "", .file_arg[[1]]), mustWork = FALSE)
} else {
  NA_character_
}
.app_dir_cands <- unique(c(
  if (!is.na(.script_path) && nzchar(.script_path)) {
    normalizePath(file.path(dirname(.script_path), ".."), mustWork = FALSE)
  } else {
    character()
  },
  if (dir.exists("app_26.0")) normalizePath("app_26.0", mustWork = FALSE) else character(),
  if (dir.exists(file.path("..", "app_26.0"))) {
    normalizePath(file.path("..", "app_26.0"), mustWork = FALSE)
  } else {
    character()
  },
  normalizePath("..", mustWork = FALSE),
  normalizePath(".", mustWork = FALSE)
))
app_dir <- ""
for (.cand in .app_dir_cands) {
  if (is.character(.cand) && nzchar(.cand) &&
      file.exists(file.path(.cand, "maintenance_mode.R")) &&
      file.exists(file.path(.cand, "access_locks.R")) &&
      file.exists(file.path(.cand, "lock_catalog_shared.R"))) {
    app_dir <- .cand
    break
  }
}
rm(list = intersect(
  c(".args", ".file_arg", ".script_path", ".app_dir_cands", ".cand"),
  ls(all.names = TRUE)
))
if (!nzchar(app_dir)) {
  stop("Cannot locate app_26.0/ (maintenance_mode.R / access_locks.R / lock_catalog_shared.R)")
}
source(file.path(app_dir, "lock_catalog_shared.R"), local = FALSE)
source(file.path(app_dir, "access_locks.R"), local = FALSE)
source(file.path(app_dir, "maintenance_mode.R"), local = FALSE)
ynow_access_reset_memory()
ynow_maintenance_reset_memory()

t0 <- as.POSIXct("2026-10-22T12:00:00Z", tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
t_start <- as.POSIXct("2026-10-22T12:00:00Z", tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
t_end <- as.POSIXct("2026-10-22T14:00:00Z", tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")

check("catalog shared with ACL", {
  "tab:lab_notes" %in% ynow_maintenance_catalog()$id &&
    identical(
      ynow_maintenance_label("tab:decision_checklist", "zh-TW"),
      "決策檢核"
    )
})

check("reject bad item", {
  res <- ynow_maintenance_set("tab:not_real", TRUE, t_start, t_end)
  isTRUE(!res$ok) && identical(res$message_key, "maint_bad_item")
})

check("reject missing window", {
  res <- ynow_maintenance_set("tab:lab_notes", TRUE, NULL, NULL)
  isTRUE(!res$ok) && identical(res$message_key, "maint_bad_window")
})

check("reject end before start", {
  res <- ynow_maintenance_set("tab:lab_notes", TRUE, t_end, t_start)
  isTRUE(!res$ok) && identical(res$message_key, "maint_bad_window_order")
})

check("set window ok", {
  ynow_maintenance_reset_memory()
  unlink(tmp)
  Sys.setenv(YNOW_MAINTENANCE_PATH = tmp)
  res <- ynow_maintenance_set("tab:lab_notes", TRUE, t_start, t_end)
  isTRUE(res$ok) && identical(res$message_key, "maint_ok_set")
})

check("active inside window", {
  ynow_maintenance_set_now_override("2026-10-22T13:00:00Z")
  isTRUE(ynow_maintenance_is_active("tab:lab_notes")) &&
    "tab:lab_notes" %in% ynow_maintenance_active_ids()
})

check("inactive before window", {
  ynow_maintenance_set_now_override("2026-10-22T11:00:00Z")
  !isTRUE(ynow_maintenance_is_active("tab:lab_notes"))
})

check("auto-lift at end (no admin click)", {
  ynow_maintenance_set_now_override("2026-10-22T14:00:00Z")
  !isTRUE(ynow_maintenance_is_active("tab:lab_notes")) &&
    !"tab:lab_notes" %in% ynow_maintenance_active_ids()
})

check("expire marks inactive in store", {
  ynow_maintenance_set_now_override("2026-10-22T15:00:00Z")
  df <- ynow_maintenance_expire_past()
  hit <- df[df$id == "tab:lab_notes", , drop = FALSE]
  nrow(hit) == 1L && isTRUE(!isTRUE(hit$active[[1]]))
})

check("clear removes row", {
  ynow_maintenance_set_now_override(t0)
  ynow_maintenance_set("tab:macro_market", TRUE, t_start, t_end)
  res <- ynow_maintenance_set("tab:macro_market", FALSE)
  isTRUE(res$ok) && !("tab:macro_market" %in% ynow_maintenance_store_entries()$id)
})

check("note zh-TW", {
  note <- ynow_maintenance_note(t_start, t_end, "zh-TW")
  grepl("維護中", note, fixed = TRUE) &&
    grepl("預計時間:", note, fixed = TRUE) &&
    grepl("\n", note, fixed = TRUE) &&
    grepl("2026-10-22 12:00-2026-10-22 14:00", note, fixed = TRUE)
})

check("note en-US", {
  note <- ynow_maintenance_note(t_start, t_end, "en")
  grepl("Under maintenance", note, fixed = TRUE) &&
    grepl("Expected:", note, fixed = TRUE) &&
    grepl("\n", note, fixed = TRUE)
})

check("client payload only active", {
  ynow_maintenance_reset_memory()
  unlink(tmp)
  Sys.setenv(YNOW_MAINTENANCE_PATH = tmp)
  ynow_maintenance_set("tab:lab_notes", TRUE, t_start, t_end)
  ynow_maintenance_set_now_override("2026-10-22T13:00:00Z")
  p <- ynow_maintenance_client_payload(locale = "zh-TW")
  length(p$entries) == 1L &&
    identical(p$entries[[1]]$id, "tab:lab_notes") &&
    grepl("維護中", p$entries[[1]]$note, fixed = TRUE) &&
    identical(p$next_end, "2026-10-22T14:00:00Z")
})

check("client payload empty after end", {
  ynow_maintenance_set_now_override("2026-10-22T14:30:00Z")
  p <- ynow_maintenance_client_payload(locale = "en")
  length(p$entries) == 0L
})

check("file round-trip", {
  ynow_maintenance_reset_memory()
  ynow_maintenance_set_now_override("2026-10-22T13:00:00Z")
  ynow_maintenance_set("input:sgr", TRUE, t_start, t_end)
  .ynow_maint_mem$by_item <- list()
  isTRUE(ynow_maintenance_is_active("input:sgr"))
})

# UI / client enforcement (non-clickable chrome distinct from locks)
ui_path <- file.path(app_dir, "ynow_ui.R")
ui <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("CSS pointer-events none on maintenance nav", {
  grepl("ynow-maintenance-nav", ui, fixed = TRUE) &&
    grepl("pointer-events: none !important", ui, fixed = TRUE)
})
check("JS apply + click guard", {
  grepl("ynowApplyMaintenance", ui, fixed = TRUE) &&
    grepl("ynowMaintenance", ui, fixed = TRUE) &&
    grepl("data-ynow-maint-click-guard", ui, fixed = TRUE) &&
    grepl("ynowScheduleMaintRefresh", ui, fixed = TRUE)
})
check("maintenance distinct from access lock chrome", {
  grepl("ynow-access-locked-nav", ui, fixed = TRUE) &&
    grepl("ynow-maintenance-nav", ui, fixed = TRUE) &&
    (grepl("unlike ACL", ui, fixed = TRUE) ||
       grepl("NON-clickable", ui, fixed = TRUE))
})

srv_path <- file.path(app_dir, "ynow_server.R")
srv <- paste(readLines(srv_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("admin UI near item locks", {
  grepl('maint_title', srv, fixed = TRUE) &&
    grepl("ynow_maint_apply", srv, fixed = TRUE) &&
    grepl("ynow_maint_clear", srv, fixed = TRUE) &&
    grepl("ynow_maint_status_now", srv, fixed = TRUE) &&
    grepl("\\.ynow_push_maintenance", srv)
})
check("server auto-lift invalidateLater", {
  grepl("ynow_maintenance_next_end", srv, fixed = TRUE) &&
    grepl("invalidateLater", srv, fixed = TRUE) &&
    grepl("ynow_maint_refresh", srv, fixed = TRUE)
})
check("download deny during maintenance", {
  grepl("ynow_maintenance_is_active", srv, fixed = TRUE) &&
    grepl("maint_denied_report", srv, fixed = TRUE)
})

loc_path <- file.path(app_dir, "ui_locale.R")
loc <- paste(readLines(loc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale en maint keys", {
  grepl('maint_title = "Maintenance"', loc, fixed = TRUE) &&
    grepl('maint_title_short = "Under maintenance"', loc, fixed = TRUE) &&
    grepl('maint_expected_prefix = "Expected:"', loc, fixed = TRUE)
})
check("locale zh-TW maint keys", {
  grepl('maint_title = "維護"', loc, fixed = TRUE) &&
    grepl('maint_title_short = "維護中"', loc, fixed = TRUE) &&
    grepl('maint_expected_prefix = "預計時間:"', loc, fixed = TRUE)
})

glb <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources maintenance_mode.R", {
  grepl('source("maintenance_mode.R"', glb, fixed = TRUE) &&
    grepl("ynow_maintenance_ensure_schema", glb, fixed = TRUE)
})

# Domains stay separate from guest_menu_lock / checklist gate
check("module header separates domains", {
  src <- paste(readLines(file.path(app_dir, "maintenance_mode.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("guest_menu_lock", src, fixed = TRUE) &&
    grepl("Checklist gate", src, fixed = TRUE) &&
    grepl("access_locks", src, fixed = TRUE)
})

message("")
message(sprintf("Passed: %d  Failed: %d", ok, bad))
if (bad > 0L) quit(status = 1L)
invisible(NULL)
