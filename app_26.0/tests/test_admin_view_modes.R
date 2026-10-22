# Self view modes (guest / signed-in / omniscient) + omniscient warning-red title.
# Offline static checks — no network / shinyapps.

options(warn = 1)
app_dir <- if (dir.exists("app_26.0")) {
  normalizePath("app_26.0")
} else if (basename(getwd()) == "tests" && dir.exists("..")) {
  normalizePath("..")
} else {
  normalizePath(getwd())
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)

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

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("UI has self-view mode buttons next to Switch view", {
  grepl("ynow_pts_sysadmin_self_guest", ui, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_self_signed_in", ui, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_self_omniscient", ui, fixed = TRUE) &&
    grepl("ynow_pts_sysadmin_viewas", ui, fixed = TRUE) &&
    grepl("ynow-sysadmin-self-view-group", ui, fixed = TRUE)
})

check("UI has self-view chrome + exit", {
  grepl("ynow_selfview_chrome", ui, fixed = TRUE) &&
    grepl("ynow-selfview-chrome", ui, fixed = TRUE) &&
    grepl("ynow_selfview_chrome_exit", ui, fixed = TRUE) &&
    grepl("ynowSelfViewMode", ui, fixed = TRUE)
})

check("Omniscient title uses warning red #DC3545", {
  grepl("body.ynow-omniscient .main-header .logo", ui, fixed = TRUE) &&
    grepl("body.ynow-omniscient .main-header .logo .ynow-app-title-fill-inner", ui, fixed = TRUE) &&
    grepl("#DC3545", ui, fixed = TRUE) &&
    grepl("ynow-omniscient", ui, fixed = TRUE)
})

check("Self-view CSS hides portal actions like view-as", {
  grepl("body.ynow-self-view .ynow-sysadmin-unlocked-actions", ui, fixed = TRUE) &&
    grepl(":not(.ynow-view-as):not(.ynow-self-view)", ui, fixed = TRUE)
})

check("Client lifts ACL + guest/persist locks in omniscient", {
  grepl("var omniscient = !!(document.body && document.body.classList.contains('ynow-omniscient'))", ui, fixed = TRUE) &&
    grepl("if (omniscient) entries = []", ui, fixed = TRUE) &&
    grepl("if (omniscient) {", ui, fixed = TRUE) &&
    grepl("ynow-self-view-guest", ui, fixed = TRUE) &&
    grepl("ynow-self-view-signed-in", ui, fixed = TRUE)
})

check("Server reactive + apply/clear self view modes", {
  grepl("ynow_pts_self_view_mode", srv, fixed = TRUE) &&
    grepl(".ynow_self_view_apply", srv, fixed = TRUE) &&
    grepl(".ynow_self_view_clear", srv, fixed = TRUE) &&
    grepl(".ynow_self_view_push_chrome", srv, fixed = TRUE) &&
    grepl('ynow_pts_sysadmin_self_guest', srv, fixed = TRUE) &&
    grepl('ynow_pts_sysadmin_self_signed_in', srv, fixed = TRUE) &&
    grepl('ynow_pts_sysadmin_self_omniscient', srv, fixed = TRUE) &&
    grepl('ynow_pts_sysadmin_self_exit', srv, fixed = TRUE) &&
    grepl('"guest"', srv, fixed = TRUE) &&
    grepl('"signed_in"', srv, fixed = TRUE) &&
    grepl('"omniscient"', srv, fixed = TRUE)
})

check("Self-view mutually exclusive with selected-user view-as", {
  grepl("Mutually exclusive with selected-user view-as", srv, fixed = TRUE) &&
    grepl("Mutually exclusive with self view modes", srv, fixed = TRUE)
})

check("Access push: omniscient empty; guest/signed_in use __all__ only", {
  grepl('identical(mode, "omniscient")', srv, fixed = TRUE) &&
    grepl('identical(mode, "guest") || identical(mode, "signed_in")', srv, fixed = TRUE) &&
    grepl("authed <- identical(mode, \"signed_in\")", srv, fixed = TRUE) &&
    grepl("__self_member__", srv, fixed = TRUE) &&
    grepl("__omniscient__", srv, fixed = TRUE)
})

check("Locale en-US + zh-TW self-view keys (no 一鍵)", {
  keys <- c(
    "pts_sysadmin_self_view_label",
    "pts_sysadmin_self_guest",
    "pts_sysadmin_self_signed_in",
    "pts_sysadmin_self_omniscient",
    "pts_sysadmin_self_apply",
    "pts_sysadmin_self_exit",
    "pts_sysadmin_self_active",
    "pts_sysadmin_self_help",
    "pts_sysadmin_self_chrome_guest",
    "pts_sysadmin_self_chrome_signed_in",
    "pts_sysadmin_self_chrome_omniscient",
    "pts_sysadmin_self_guest_ok",
    "pts_sysadmin_self_signed_in_ok",
    "pts_sysadmin_self_omniscient_ok",
    "pts_sysadmin_self_exit_ok"
  )
  all(vapply(keys, function(k) {
    en <- ui_str(k, "en")
    zh <- ui_str(k, "zh-TW")
    nzchar(en) && nzchar(zh) && !grepl("一鍵|默认|数据|参数", zh)
  }, logical(1))) &&
    identical(ui_str("pts_sysadmin_self_guest", "en"), "Guest mode") &&
    identical(ui_str("pts_sysadmin_self_guest", "zh-TW"), "訪客模式") &&
    identical(ui_str("pts_sysadmin_self_omniscient", "en"), "Omniscient mode") &&
    identical(ui_str("pts_sysadmin_self_omniscient", "zh-TW"), "全知模式") &&
    identical(ui_str("pts_sysadmin_self_apply", "en"), "Quick apply") &&
    identical(ui_str("pts_sysadmin_self_apply", "zh-TW"), "快速套用") &&
    grepl("warning red", ui_str("pts_sysadmin_self_chrome_omniscient", "en"), fixed = TRUE) &&
    grepl("警示紅", ui_str("pts_sysadmin_self_chrome_omniscient", "zh-TW"), fixed = TRUE)
})

check("applyUiLocale wires self-view labels", {
  grepl("ynow_pts_sysadmin_self_guest_label", ui, fixed = TRUE) &&
    grepl("pts_sysadmin_self_omniscient", ui, fixed = TRUE) &&
    grepl("ynowRefreshSelfViewChromeLocale", ui, fixed = TRUE)
})

check("Exit omniscient restores via clearing ynow-omniscient class", {
  grepl("document.body.classList.remove('ynow-self-view', 'ynow-omniscient'", ui, fixed = TRUE) ||
    grepl('document.body.classList.remove(\n                \'ynow-self-view\', \'ynow-omniscient\'', ui) ||
    grepl("classList.remove(\\s*'ynow-self-view',\\s*'ynow-omniscient'", ui, perl = TRUE)
})

cat("\nSummary: PASS=", .pass, " FAIL=", .fail, "\n", sep = "")
if (.fail > 0L) quit(status = 1L)
invisible(NULL)
