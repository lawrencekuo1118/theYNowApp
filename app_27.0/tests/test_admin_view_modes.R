# Self view modes (guest / signed-in / omniscient) + omniscient warning-red title.
# Offline static checks — no network / shinyapps.

options(warn = 1)
app_dir <- if (dir.exists("app_27.0")) {
  normalizePath("app_27.0")
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

check("Omniscient button shares Guest btn-default + red text class", {
  grepl("ynow-sysadmin-btn-omniscient", ui, fixed = TRUE) &&
    grepl(
      'class = "btn-default btn-sm ynow-sysadmin-btn-omniscient"',
      ui,
      fixed = TRUE
    ) &&
    grepl(
      ".ynow-sysadmin-self-view-group .btn.ynow-sysadmin-btn-omniscient",
      ui,
      fixed = TRUE
    ) &&
    grepl("color: #DC3545 !important;", ui, fixed = TRUE) &&
    # Guest stays plain btn-default (no omniscient class).
    grepl(
      'id = "ynow_pts_sysadmin_self_guest_label"',
      ui,
      fixed = TRUE
    ) &&
    grepl('class = "btn-default btn-sm"', ui, fixed = TRUE) &&
    !grepl(
      'ynow_pts_sysadmin_self_omniscient"[\\s\\S]{0,400}btn-warning',
      ui,
      perl = TRUE
    )
})

check("Admin Apply changes uses shared red-bg white-text class", {
  grepl("ynow-sysadmin-btn-apply", ui, fixed = TRUE) &&
    grepl(
      ".ynow-pts-sysadmin-box .btn.ynow-sysadmin-btn-apply",
      ui,
      fixed = TRUE
    ) &&
    grepl("background: #c0392b !important;", ui, fixed = TRUE) &&
    grepl("ynow-sysadmin-btn-apply", srv, fixed = TRUE) &&
    grepl(
      'class = "btn-danger ynow-sysadmin-btn-apply"',
      srv,
      fixed = TRUE
    ) &&
    grepl("ynow_pts_sysadmin_apply", srv, fixed = TRUE) &&
    grepl("ynow_access_apply_changes", srv, fixed = TRUE)
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

check("Sysadmin enter defaults to Signed-in user mode (not guest/omniscient)", {
  # Unlock path must apply signed_in immediately after portal unlock.
  grepl("ynow_pts_sysadmin_enter", srv, fixed = TRUE) &&
    grepl(
      '.ynow_self_view_apply("signed_in", notify = FALSE, goto_home = FALSE)',
      srv,
      fixed = TRUE
    ) &&
    grepl("default_self_view=signed_in", srv, fixed = TRUE) &&
    grepl(
      "Default self-view after unlock: Signed-in user mode",
      srv,
      fixed = TRUE
    ) &&
    # Must not default-apply guest or omniscient on enter.
    !grepl(
      'ynow_pts_sysadmin_on\\(TRUE\\)[\\s\\S]{0,400}\\.ynow_self_view_apply\\("guest"',
      srv,
      perl = TRUE
    ) &&
    !grepl(
      'ynow_pts_sysadmin_on\\(TRUE\\)[\\s\\S]{0,400}\\.ynow_self_view_apply\\("omniscient"',
      srv,
      perl = TRUE
    )
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

check("Signed-in / omniscient self-view opens Account+Settings (not guest-gated)", {
  grepl(".ynow_self_view_is_member_like", srv, fixed = TRUE) &&
    grepl(".ynow_self_view_preview_user", srv, fixed = TRUE) &&
    grepl('identical(mode, "signed_in") || identical(mode, "omniscient")', srv, fixed = TRUE) &&
    grepl("settings_self_view_toast", srv, fixed = TRUE) &&
    grepl("Settings applied for this self view", srv, fixed = TRUE) &&
    # Account / Settings / Points panels re-render on self-view apply.
    grepl("ynow_pts_panel_tick(isolate(ynow_pts_panel_tick()) + 1L)", srv, fixed = TRUE) &&
    grepl("tryCatch(ynow_pts_self_view_mode(), error = function(e) NULL)", srv, fixed = TRUE) &&
    # Gate returns NULL for member-like self-view before real auth$user check.
    grepl(
      "Self-view: signed-in / omniscient open Account+Settings like a real member",
      srv,
      fixed = TRUE
    ) &&
    # Client: signed_in / omniscient add ynow-authed (CSS unhides Settings/Account).
    grepl("ynow-self-view-signed-in", ui, fixed = TRUE) &&
    grepl("document.body.classList.add('ynow-authed')", ui, fixed = TRUE)
})

check("Locale Settings self-view toast + signed-in chrome mentions Account", {
  en_toast <- ui_str("settings_self_view_toast", "en")
  zh_toast <- ui_str("settings_self_view_toast", "zh-TW")
  en_chrome <- ui_str("pts_sysadmin_self_chrome_signed_in", "en")
  zh_chrome <- ui_str("pts_sysadmin_self_chrome_signed_in", "zh-TW")
  nzchar(en_toast) && nzchar(zh_toast) &&
    grepl("self view", en_toast, ignore.case = TRUE) &&
    grepl("自身視角", zh_toast, fixed = TRUE) &&
    !grepl("一鍵|默认|数据|参数", zh_toast) &&
    grepl("Settings and Account", en_chrome, fixed = TRUE) &&
    grepl("設定與帳號", zh_chrome, fixed = TRUE)
})

# Permission matrix (static): CSS hide vs lock vs lift.
auth <- paste(
  readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check("Matrix: guest CSS-hides Settings+Account until ynow-authed", {
  grepl(
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value=\\\"settings\\\"]",
    auth,
    fixed = TRUE
  ) || grepl(
    'body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value="settings"]',
    auth,
    fixed = TRUE
  )
})

check("Matrix: signed-in self-view forces member chrome (ynow-authed)", {
  grepl("mode === 'signed_in'", ui, fixed = TRUE) &&
    grepl("ynow-self-view-signed-in", ui, fixed = TRUE) &&
    grepl("selfSigned = (selfMode === 'signed_in' || selfMode === 'omniscient')", ui, fixed = TRUE)
})

check("Matrix: omniscient clears ACL entries client-side", {
  grepl("if (omniscient) entries = []", ui, fixed = TRUE) &&
    grepl("body.ynow-omniscient .main-header .logo", ui, fixed = TRUE) &&
    grepl("#DC3545", ui, fixed = TRUE)
})

cat("\nSummary: PASS=", .pass, " FAIL=", .fail, "\n", sep = "")
if (.fail > 0L) quit(status = 1L)
invisible(NULL)
