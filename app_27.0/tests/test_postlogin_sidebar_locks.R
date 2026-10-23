# Post-login still-locked sidebar chrome (ACL / persist B) + A unlock cleanup.
# Run: cd app_27.0 && Rscript tests/test_postlogin_sidebar_locks.R
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
source(file.path(app_dir, "lock_catalog_shared.R"), local = TRUE)
source(file.path(app_dir, "membership_locks.R"), local = TRUE)
source(file.path(app_dir, "auth_account.R"), local = TRUE)

guest <- ynow_sidebar_guest_lock_tabs()
check("guest lock tabs are A (unlock-after-login)", {
  identical(
    sort(guest),
    sort(c("lab_notes", "decision_checklist", "price_alerts", "asset_transmission"))
  ) &&
    all(vapply(guest, function(t) identical(ynow_membership_lock_tier(t), "A"), logical(1)))
})

persist <- ynow_sidebar_persist_lock_js_map()
check("persist map is list (B sidebar tabs; may be empty)", is.list(persist))
check("persist map excludes A guest_menu_lock tabs", {
  !any(guest %in% names(persist))
})
check("no B sidebar_tab in catalog today (or persist matches)", {
  b_tabs <- ynow_membership_lock_ids(tier = "B", kind = "sidebar_tab")
  identical(sort(names(persist)), sort(b_tabs))
})

check("UI injects persist lock map", {
  grepl("__YNOW_SIDEBAR_PERSIST_LOCK_TABS", ui, fixed = TRUE) &&
    grepl("ynow_sidebar_persist_lock_js_map", ui, fixed = TRUE)
})

check("FA icon column excludes lock glyph", {
  grepl(":not(.pull-right):not(.ynow-menu-lock)", ui, fixed = TRUE)
})

check("menu-lock hide uses !important (beats FA .fas display)", {
  grepl(".sidebar-menu .ynow-menu-lock,", ui, fixed = TRUE) &&
    grepl("display: none !important", ui, fixed = TRUE)
})

check("locked show uses !important + ynow-sidebar-locked", {
  grepl("li.ynow-sidebar-locked > a > .ynow-menu-lock", ui, fixed = TRUE) &&
    grepl("display: inline-block !important", ui, fixed = TRUE)
})

check("SyncGuest keeps still-locked chrome; never hides via access-hidden", {
  grepl("Still-locked after auth: ACL + persist B", ui, fixed = TRUE) &&
    grepl("ynow-access-hidden", ui, fixed = TRUE) &&
    grepl("li.classList.remove('ynow-access-hidden')", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarLockGlyph", ui, fixed = TRUE)
})

check("A unlock path drops chrome only (items stay in sidebar)", {
  grepl("A unlock-after-login: drop locked chrome only", ui, fixed = TRUE) &&
    grepl("classList.remove('ynow-sidebar-locked')", ui, fixed = TRUE)
})

check("Lite omit set unchanged (guest-locked still omitted)", {
  omit <- ynow_sidebar_lite_omit_tabs()
  all(guest %in% omit) &&
    "asset_transmission" %in% omit &&
    all(c("account_admin", "account_points", "settings") %in% omit)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
