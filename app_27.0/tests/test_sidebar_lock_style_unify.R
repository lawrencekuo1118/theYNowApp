# Unified locked sidebar chrome: ACL (ATX) + Full guest locks share one treatment.
# Run: cd app_27.0 && Rscript tests/test_sidebar_lock_style_unify.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- if (file.exists("ynow_ui.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate ynow_ui.R")
}

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

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

source(file.path(app_dir, "auth_account.R"), local = TRUE)

guest_tabs <- ynow_sidebar_guest_lock_tabs()
check("guest lock set is Lab / Checklist / Alerts / ATX", {
  identical(
    sort(guest_tabs),
    sort(c("lab_notes", "decision_checklist", "price_alerts", "asset_transmission"))
  )
})

check("shared class token ynow-sidebar-locked exists", {
  grepl("ynow-sidebar-locked", ui, fixed = TRUE) &&
    grepl("Unified locked sidebar", ui, fixed = TRUE)
})

check("ACL + guest share opacity 0.55 rule", {
  grepl("li.ynow-sidebar-locked > a", ui, fixed = TRUE) &&
    grepl("li.ynow-access-locked-nav > a", ui, fixed = TRUE) &&
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a:has(> .ynow-menu-lock)",
      ui,
      fixed = TRUE
    ) &&
    grepl("opacity: 0.55", ui, fixed = TRUE)
})

check("unified lock glyph visibility for ACL + guest", {
  grepl("li.ynow-sidebar-locked > a > .ynow-menu-lock", ui, fixed = TRUE) &&
    grepl("li.ynow-access-locked-nav > a > .ynow-menu-lock", ui, fixed = TRUE) &&
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .sidebar-menu a > .ynow-menu-lock",
      ui,
      fixed = TRUE
    ) &&
    grepl("display: none !important", ui, fixed = TRUE) &&
    grepl("display: inline-block !important", ui, fixed = TRUE)
})

check("sidebar icon column excludes ynow-menu-lock (FA display override)", {
  grepl(":not(.pull-right):not(.ynow-menu-lock)", ui, fixed = TRUE)
})

check("ynowSyncGuestSidebarLocked wires guest LIs", {
  grepl("ynowSyncGuestSidebarLocked", ui, fixed = TRUE) &&
    grepl("__YNOW_SIDEBAR_GUEST_LOCK_TABS", ui, fixed = TRUE) &&
    grepl("__YNOW_SIDEBAR_PERSIST_LOCK_TABS", ui, fixed = TRUE) &&
    grepl("classList.add('ynow-sidebar-locked')", ui, fixed = TRUE) &&
    grepl("classList.remove('ynow-sidebar-locked')", ui, fixed = TRUE)
})

check("SyncGuest keeps ACL/persist chrome; A tabs unlock after auth", {
  grepl("ynow-access-locked-nav", ui, fixed = TRUE) &&
    grepl("persist[tab]", ui, fixed = TRUE) &&
    grepl("else if (tabs[tab])", ui, fixed = TRUE) &&
    grepl("A unlock-after-login: drop locked chrome only", ui, fixed = TRUE) &&
    grepl("ynow-view-as", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarLockGlyph", ui, fixed = TRUE)
})

check("ACL apply also stamps ynow-sidebar-locked", {
  grepl("li.classList.add('ynow-access-locked-nav')", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-locked", ui, fixed = TRUE) &&
    grepl("ynowApplyAccessLocks", ui, fixed = TRUE)
})

check("old divergent 'keep same color / no dim' comment removed", {
  !grepl("keep the same sidebar\n           title color as Asset Transmission Root", ui, fixed = TRUE) &&
    !grepl("it does not dim the label", ui, fixed = TRUE)
})

check("Download Report keeps dedicated muted button styling", {
  grepl("ynow-sidebar-dl-lock", ui, fixed = TRUE) &&
    grepl("color: #a3a3a3 !important", ui, fixed = TRUE) &&
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-wrap #download_report",
      ui,
      fixed = TRUE
    )
})

check("Lite still omits guest-locked tabs (no grayed locks)", {
  css <- ynow_sidebar_lite_omit_css()
  all(vapply(guest_tabs, function(t) {
    grepl(sprintf('data-value="%s"', t), css, fixed = TRUE)
  }, logical(1))) &&
    grepl("body.ynow-lite .ynow-menu-lock", ui, fixed = TRUE)
})

check("ATX remains ACL-lockable with promo pane", {
  grepl("tab === 'asset_transmission'", ui, fixed = TRUE) &&
    grepl("ynow-atx-locked-pane", ui, fixed = TRUE) &&
    grepl("ynowEnsureAtxLockPromo", ui, fixed = TRUE)
})

fixture <- file.path(app_dir, "tests", "_fixture_sidebar_lock_style_unify.R")
check("unify fixture script exists", file.exists(fixture))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
