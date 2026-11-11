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
if (!grepl("app_27.0$", app_dir)) {
  app_dir <- normalizePath(".", mustWork = TRUE)
}

source(file.path(app_dir, "tests", "ynow_test_sources.R"))
src <- ynow_test_ui_sources(app_dir)
ui <- src$ui
shipped <- src$shipped
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

check("Lite omits snap_defaults; Full guest lock chrome", {
  # Prefs: Lite hides snap_defaults entirely; Full non-sysadmin keeps tab + lock glyph.
  # Selectors are unescaped in ynow_chrome.css.
  grepl(
    'body.ynow-lite #snapshot_report > li:has(> a[data-value="snap_defaults"])',
    shipped,
    fixed = TRUE
  ) &&
    grepl("ynow-snap-tab-lock", shipped, fixed = TRUE) &&
    grepl(
      'body:not(.ynow-lite):not(.ynow-sysadmin-on) #snapshot_report a[data-value="snap_defaults"] > i:not(.ynow-snap-tab-lock)',
      shipped,
      fixed = TRUE
    ) &&
    grepl(
      'body:not(.ynow-sysadmin-on) #shiny-tab-snap_defaults .ynow-snap-defaults-locked',
      shipped,
      fixed = TRUE
    )
})
check("locked promo sits inside the snap_defaults pane", {
  def <- regexpr('value = "snap_defaults"', ui, fixed = TRUE)[1]
  locked <- regexpr('id = "ynow_snapshot_defaults_locked"', ui, fixed = TRUE)[1]
  body <- regexpr('class = "ynow-snap-defaults-body"', ui, fixed = TRUE)[1]
  def > 0 && locked > def && body > locked
})
check("ui has locked placeholder", {
  grepl("ynow-snap-defaults-locked", ui, fixed = TRUE) &&
    grepl("ynow_snapshot_defaults_locked_msg", ui, fixed = TRUE) &&
    grepl("ynow-snap-defaults-body", ui, fixed = TRUE)
})
check("locked placeholder uses mono gate palette", {
  grepl("--ynow-gate-accent", shipped, fixed = TRUE) &&
    grepl("ynow-gate-callout", shipped, fixed = TRUE) &&
    grepl("ynow-snap-defaults-locked", shipped, fixed = TRUE) &&
    !grepl("\\.ynow-snap-defaults-locked[^}]*#fff8e6", shipped) &&
    !grepl("\\.ynow-snap-defaults-locked[^}]*#e0a800", shipped) &&
    !grepl("\\.ynow-snap-defaults-locked[^}]*#f0d78c", shipped)
})
check("access-locked pane callout is mono", {
  grepl("ynow-access-locked-pane::before", shipped, fixed = TRUE) &&
    grepl("--ynow-gate-bg", shipped, fixed = TRUE) &&
    !grepl("ynow-access-locked-pane::before[^}]*#fff8e6", shipped) &&
    !grepl("ynow-access-locked-pane::before[^}]*#e0c36a", shipped)
})
check("ui applyUiLocale wired for locked msg", {
  grepl("ynow_snapshot_defaults_locked_msg", ui, fixed = TRUE) &&
    grepl("snapshot_defaults_locked", ui, fixed = TRUE)
})
check("js ensureLite prefers free Snapshot tabs", {
  # Lite omits snap_defaults; helper aliases keep old name for callers.
  grepl("ensureLiteSnapshotFreeTab", shipped, fixed = TRUE) &&
    grepl("ensureLiteSnapshotDefaultsTab = ensureLiteSnapshotFreeTab", shipped, fixed = TRUE) &&
    grepl("Lite omits snap_defaults entirely", shipped, fixed = TRUE) &&
    !grepl(
      "if (!document.body.classList.contains('ynow-sysadmin-on')) return;",
      shipped,
      fixed = TRUE
    )
})
check("js exit sysadmin leaves snap_defaults", {
  leave <- regexpr("Leaving sysadmin: leave APP_DEFAULTS", shipped, fixed = TRUE)[1]
  start <- if (leave > 0) max(1L, leave - 240L) else 0L
  chunk <- if (leave > 0) substr(shipped, start, min(nchar(shipped), leave + 700L)) else ""
  grepl("ynowSysadminMode", shipped, fixed = TRUE) &&
    grepl("snap_defaults", chunk, fixed = TRUE) &&
    grepl('data-value="snap_current"', chunk, fixed = TRUE) &&
    grepl('data-value="snap_audit"', chunk, fixed = TRUE)
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
