# Sidebar tab reorder: prefs key, normalize, signed-in wiring, page-pin disabled
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "user_prefs.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

Sys.setenv(YNOW_USER_PREFS_FORCE_FILE = "1")
tmpdir <- tempfile("ynow_sidebar_reorder_")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
old_wd <- getwd()
on.exit({
  setwd(old_wd)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  Sys.unsetenv("YNOW_USER_PREFS_FORCE_FILE")
}, add = TRUE)
setwd(tmpdir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0L)
}

source(file.path(root, "user_prefs.R"), local = FALSE, encoding = "UTF-8")
ynow_prefs_reset_memory()

check(
  "pref key is sidebar_tab_order",
  identical(YNOW_PREF_SIDEBAR_TAB_ORDER, "sidebar_tab_order")
)
check(
  "default order includes home + categories",
  {
    d <- ynow_sidebar_default_order()
    all(c("home", "cat_asset", "lab_notes", "about") %in% d) &&
      identical(d[[1]], "home")
  }
)
check(
  "normalize drops junk + fills defaults",
  {
    got <- ynow_prefs_normalize_sidebar_order(c("about", "nope", "home", "home"))
    identical(got[1:2], c("about", "home")) &&
      all(ynow_sidebar_default_order() %in% got) &&
      !("nope" %in% got)
  }
)

uid <- "test-user-sidebar-reorder-1"
check(
  "empty get returns defaults",
  identical(ynow_prefs_get_sidebar_tab_order(uid), ynow_sidebar_default_order())
)

custom <- c("about", "home", "dashboard", "macro_market")
saved <- ynow_prefs_set_sidebar_tab_order(uid, custom)
check("set returns normalized with defaults filled", {
  identical(saved[1:4], c("about", "home", "dashboard", "macro_market")) &&
    length(saved) == length(ynow_sidebar_default_order())
})
check(
  "get after set",
  identical(ynow_prefs_get_sidebar_tab_order(uid), saved)
)

# Survive memory clear → file reload.
ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_sidebar_tab_order(uid), saved)
)

# Coexist with other prefs keys under same user.
invisible(ynow_prefs_set_pinned_recent(uid, c("AAPL")))
invisible(ynow_prefs_set_pinned_sidebar(uid, c("home")))
ynow_prefs_reset_memory()
check(
  "order survives sibling prefs write + reload",
  identical(ynow_prefs_get_sidebar_tab_order(uid), saved)
)
check(
  "recent pins still present",
  identical(ynow_prefs_get_pinned_recent(uid), "AAPL")
)
check(
  "legacy sidebar pins API still present",
  identical(ynow_prefs_get_pinned_sidebar(uid), "home")
)

# Wiring smoke (source text).
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("UI has customize button on date row", {
  grepl("ynow_sidebar_reorder_btn", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-date-row", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-reorder-btn", ui, fixed = TRUE)
})
check("CSS shows btn only when authed", {
  grepl("body.ynow-authed .ynow-sidebar-reorder-btn", auth, fixed = TRUE)
})
check("CSS edit-mode grab cursor", {
  grepl("ynow-sidebar-reorder-edit", auth, fixed = TRUE)
})
check("JS long-press drag reorder", {
  grepl("ynow-sidebar-reorder-edit", ui, fixed = TRUE) &&
    grepl("sidebar_tab_order_save", ui, fixed = TRUE) &&
    grepl("ynowApplySidebarTabOrder", ui, fixed = TRUE) &&
    grepl("LONG_MS = 420", ui, fixed = TRUE)
})
check("page-pin long-press on menu disabled", {
  !grepl(
    "pointerdown', '.sidebar-menu a\\[data-value\\], .ynow-sidebar-pin-tile'",
    ui,
    perl = TRUE
  ) &&
    grepl("page-pin UX disabled", ui, ignore.case = TRUE)
})
check("server saves order for signed-in", {
  grepl("observeEvent\\(input\\$sidebar_tab_order_save", srv, perl = TRUE) &&
    grepl("ynow_prefs_set_sidebar_tab_order", srv, fixed = TRUE) &&
    grepl("ynow_prefs_get_sidebar_tab_order", srv, fixed = TRUE)
})
check("server pin tray render is NULL", {
  grepl("output\\$pinned_sidebar_tabs <- renderUI", srv, perl = TRUE) &&
    grepl("Page-pin tray disabled", srv, fixed = TRUE)
})
check("en + zh-TW i18n keys", {
  grepl("sidebar_reorder_btn = \"Customize\"", loc, fixed = TRUE) &&
    grepl("sidebar_reorder_btn = \"自訂設定\"", loc, fixed = TRUE) &&
    grepl("sidebar_reorder_btn_done = \"Done\"", loc, fixed = TRUE) &&
    grepl("sidebar_reorder_btn_done = \"完成\"", loc, fixed = TRUE) &&
    grepl("sidebar_reorder_saved_toast", loc, fixed = TRUE)
})
check("legacy page-pin tray CSS hidden", {
  grepl(".ynow-sidebar-pins { display:none !important; }", auth, fixed = TRUE) ||
    grepl(".ynow-sidebar-pins { display:none !important; }", auth, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
