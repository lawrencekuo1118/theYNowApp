# Signed-in Settings prefs: hide locked sidebar, locale/market, compact.
# Run: cd app_26.0 && Rscript tests/test_user_settings.R
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
tmpdir <- tempfile("ynow_user_settings_")
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
  "pref key is user_settings",
  identical(YNOW_PREF_USER_SETTINGS, "user_settings")
)
check(
  "defaults hide_locked OFF",
  {
    d <- ynow_prefs_default_user_settings()
    identical(d$hide_locked_sidebar, FALSE) &&
      identical(d$preferred_ui_locale, "auto") &&
      identical(d$preferred_market, "auto") &&
      identical(d$sidebar_compact, FALSE)
  }
)
check(
  "normalize coerces hide + locale aliases",
  {
    n <- ynow_prefs_normalize_user_settings(list(
      hide_locked_sidebar = "1",
      preferred_ui_locale = "zh_tw",
      preferred_market = "tw",
      sidebar_compact = TRUE,
      junk = 1
    ))
    isTRUE(n$hide_locked_sidebar) &&
      identical(n$preferred_ui_locale, "zh-TW") &&
      identical(n$preferred_market, "TW") &&
      isTRUE(n$sidebar_compact) &&
      is.null(n$junk)
  }
)

uid <- "test-user-settings-1"
check(
  "empty get returns defaults",
  identical(ynow_prefs_get_user_settings(uid), ynow_prefs_default_user_settings())
)

saved <- ynow_prefs_set_user_settings(uid, list(
  hide_locked_sidebar = TRUE,
  preferred_ui_locale = "en",
  preferred_market = "US",
  sidebar_compact = TRUE
))
check("set returns normalized", {
  isTRUE(saved$hide_locked_sidebar) &&
    identical(saved$preferred_ui_locale, "en") &&
    identical(saved$preferred_market, "US") &&
    isTRUE(saved$sidebar_compact)
})
check("get after set", identical(ynow_prefs_get_user_settings(uid), saved))

ynow_prefs_reset_memory()
check(
  "file reload after memory clear",
  identical(ynow_prefs_get_user_settings(uid), saved)
)

# Coexist with sibling prefs.
invisible(ynow_prefs_set_pinned_recent(uid, c("MSFT")))
invisible(ynow_prefs_set_sidebar_tab_order(uid, c("settings", "about")))
ynow_prefs_reset_memory()
check(
  "settings survive sibling prefs write + reload",
  identical(ynow_prefs_get_user_settings(uid), saved)
)
check(
  "default order includes settings before account",
  {
    d <- ynow_sidebar_default_order()
    "settings" %in% d &&
      which(d == "settings") < which(d == "account_admin")
  }
)

# Wiring smoke (source text).
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(root, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "settings menu + tab wired",
  grepl("ynow_settings_menu_item", ui, fixed = TRUE) &&
    grepl("tabName = \"settings\"", auth, fixed = TRUE) &&
    grepl("ynow_settings_panel", auth, fixed = TRUE) &&
    grepl("output\\$ynow_settings_panel", srv, perl = TRUE)
)
check(
  "white separator under About before Settings (authed)",
  grepl("li:has(> a[data-value=\\\"settings\\\"])::before", auth, fixed = TRUE) &&
    grepl("border-top:1px solid #ffffff", auth, fixed = TRUE)
)
check(
  "hide-locked CSS opt-in class",
  grepl("ynow-hide-locked-nav", auth, fixed = TRUE) &&
    grepl("ynow-hide-locked-nav", ui, fixed = TRUE) &&
    grepl("ynowUserSettings", ui, fixed = TRUE) &&
    grepl("ynowUserSettings", srv, fixed = TRUE)
)
check(
  "i18n en + zh-TW keys",
  grepl("settings_hide_locked = \"Quick-hide locked items\"", loc, fixed = TRUE) &&
    grepl("settings_hide_locked = \"快速隱藏鎖住項目\"", loc, fixed = TRUE) &&
    grepl("menu_settings = \"Settings\"", loc, fixed = TRUE) &&
    grepl("menu_settings = \"設定\"", loc, fixed = TRUE) &&
    !grepl("一鍵隱藏", loc, fixed = TRUE)
)
check(
  "Lite omit includes settings",
  {
    Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
    source(file.path(root, "lock_catalog_shared.R"), local = TRUE)
    source(file.path(root, "membership_locks.R"), local = TRUE)
    source(file.path(root, "auth_account.R"), local = TRUE)
    "settings" %in% ynow_sidebar_lite_omit_tabs()
  }
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
