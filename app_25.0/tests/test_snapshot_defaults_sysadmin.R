#!/usr/bin/env Rscript
# Snapshot APP_DEFAULTS tab gated by System admin session unlock.
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
if (!grepl("app_25.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

source(file.path(app_dir, "ui_locale.R"), local = FALSE)

check("locale snapshot_defaults_locked en", {
  en <- ui_str("snapshot_defaults_locked", "en")
  grepl("APP_DEFAULTS", en, fixed = TRUE) &&
    grepl("System admin", en, fixed = TRUE) &&
    !grepl("默认", en, fixed = TRUE)
})
check("locale snapshot_defaults_locked zh-TW", {
  zh <- ui_str("snapshot_defaults_locked", "zh-TW")
  grepl("APP_DEFAULTS", zh, fixed = TRUE) &&
    grepl("系統管理", zh, fixed = TRUE) &&
    !grepl("默认|参数|数据", zh) &&
    !grepl("離開系統管理", zh) == FALSE
})
check("locale page help mentions unlock en", {
  grepl("System admin unlock", ui_str("snapshot_page_help", "en"), fixed = TRUE)
})
check("locale page help mentions unlock zh-TW", {
  grepl("系統管理解鎖", ui_str("snapshot_page_help", "zh-TW"), fixed = TRUE)
})

check("ui hides snap_defaults unless sysadmin-on", {
  grepl(
    'body:not(.ynow-sysadmin-on) #snapshot_report > .nav-tabs > li:has(> a[data-value="snap_defaults"])',
    ui,
    fixed = TRUE
  )
})
check("ui has locked placeholder", {
  grepl("ynow-snap-defaults-locked", ui, fixed = TRUE) &&
    grepl("ynow_snapshot_defaults_locked_msg", ui, fixed = TRUE) &&
    grepl("ynow-snap-defaults-body", ui, fixed = TRUE)
})
check("ui applyUiLocale wired for locked msg", {
  grepl("ynow_snapshot_defaults_locked_msg", ui, fixed = TRUE) &&
    grepl("snapshot_defaults_locked", ui, fixed = TRUE)
})
check("js ensureLite requires sysadmin-on", {
  grepl("ensureLiteSnapshotDefaultsTab", ui, fixed = TRUE) &&
    grepl(
      "if (!document.body.classList.contains('ynow-sysadmin-on')) return;",
      ui,
      fixed = TRUE
    )
})
check("js exit sysadmin leaves snap_defaults", {
  grepl('data-value") === \'snap_defaults\'', ui, fixed = TRUE) ||
    grepl('data-value") === "snap_defaults"', ui, fixed = TRUE) ||
    grepl("snap_defaults", ui, fixed = TRUE) &&
      grepl("ynowSysadminMode", ui, fixed = TRUE) &&
      grepl('a[data-value="snap_current"]', ui, fixed = TRUE)
})
check("server gates defaults_table on sysadmin", {
  grepl("output$defaults_table", srv, fixed = TRUE) &&
    grepl("req(isTRUE(ynow_pts_sysadmin_on()))", srv, fixed = TRUE)
})
check("server gates download_defaults on sysadmin", {
  grepl("output$download_defaults", srv, fixed = TRUE) &&
    grepl("isolate(ynow_pts_sysadmin_on())", srv, fixed = TRUE) &&
    grepl("require System admin unlock", srv, fixed = TRUE)
})
check("ui parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_ui.R")), error = function(e) e), "expression"))
check("server parses", inherits(tryCatch(parse(file.path(app_dir, "ynow_server.R")), error = function(e) e), "expression"))
check("locale parses", inherits(tryCatch(parse(file.path(app_dir, "ui_locale.R")), error = function(e) e), "expression"))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
