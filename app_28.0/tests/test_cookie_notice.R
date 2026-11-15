#!/usr/bin/env Rscript
# Cookie notice: bilingual locale keys and site-wide banner markup.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "ui_locale.R"), local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

en <- .UI_STRINGS$en
zh <- .UI_STRINGS[["zh-TW"]]
keys <- c(
  "cookie_banner_body", "cookie_settings_btn", "cookie_accept_all",
  "cookie_reject_optional", "cookie_save", "cookie_panel_title",
  "cookie_necessary_label", "cookie_necessary_hint",
  "cookie_analytics_label", "cookie_analytics_hint",
  "cookie_preferences_label", "cookie_preferences_hint",
  "cookie_always_on", "cookie_reopen", "sidebar_copyright"
)
for (k in keys) {
  check(paste("en has", k), !is.null(en[[k]]) && nzchar(as.character(en[[k]])[1]))
  check(paste("zh-TW has", k), !is.null(zh[[k]]) && nzchar(as.character(zh[[k]])[1]))
}

check(
  "en banner",
  identical(
    as.character(en$cookie_banner_body)[1],
    paste0(
      "To improve your experience, please allow this site to use cookies. ",
      "Select Cookie settings to learn more and manage cookies on this site."
    )
  )
)
check(
  "zh-TW banner",
  identical(
    as.character(zh$cookie_banner_body)[1],
    paste0(
      "為增進您的使用體驗，請允許本網站使用您的 Cookie。",
      "點選「Cookie 設定」了解更多資訊並管理本網站上的 Cookie。"
    )
  )
)
check("en Cookie settings", identical(as.character(en$cookie_settings_btn)[1], "Cookie settings"))
check("zh-TW Cookie 設定", identical(as.character(zh$cookie_settings_btn)[1], "Cookie 設定"))
check("zh-TW banner uses 點選", grepl("點選", zh$cookie_banner_body, fixed = TRUE))
check("zh-TW banner has no 點擊", !grepl("點擊", zh$cookie_banner_body, fixed = TRUE))
check(
  "boot bundle keeps cookie_",
  grepl("cookie_", paste(deparse(ui_locale_boot_bundle), collapse = "\n"), fixed = TRUE)
)

# C-STRUCT-02 seam 2: markup/JS in ynow_cookie_notice_ui.R; CSS + applyUiLocale on facade.
ui_facade <- paste(
  readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
ui_cookie <- paste(
  readLines(file.path(app_dir, "ynow_cookie_notice_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
ui <- paste(ui_facade, ui_cookie, sep = "\n")
check(
  "facade sources cookie sibling",
  grepl(
    'source("ynow_cookie_notice_ui.R", local = TRUE, encoding = "UTF-8")',
    ui_facade,
    fixed = TRUE
  )
)
check("banner markup", grepl('id = "ynow_cookie_banner"', ui_cookie, fixed = TRUE))
check("settings control", grepl('id = "ynow_cookie_settings_btn"', ui_cookie, fixed = TRUE))
check("necessary locked", grepl('id = "ynow_cookie_necessary"', ui_cookie, fixed = TRUE) &&
  grepl("disabled = \"disabled\"", ui_cookie, fixed = TRUE))
check("analytics toggle", grepl('id = "ynow_cookie_analytics"', ui_cookie, fixed = TRUE))
check("preferences toggle", grepl('id = "ynow_cookie_preferences"', ui_cookie, fixed = TRUE))
check("persist localStorage", grepl("localStorage.setItem(KEY, raw)", ui_cookie, fixed = TRUE))
check("persist cookie", grepl("document.cookie = KEY", ui_cookie, fixed = TRUE))
check(
  "applyUiLocale body",
  grepl("setBtText('ynow_cookie_banner_body', 'cookie_banner_body')", ui_facade, fixed = TRUE)
)
check("reopen pinned bottom-right", grepl(".ynow-cookie-reopen", ui_facade, fixed = TRUE) &&
  grepl("right: 16px;", ui_facade, fixed = TRUE) &&
  !grepl("left: 12px;\n          bottom: 12px;", ui_facade, fixed = TRUE))
check(
  "sidebar copyright copy",
  identical(as.character(en$sidebar_copyright)[1], "2026 © YNow Team. All rights reserved.") &&
    identical(as.character(zh$sidebar_copyright)[1], "2026 © YNow Team. 版權所有。")
)
check("sidebar copyright markup", grepl('id = "ynow_sidebar_copyright"', ui_facade, fixed = TRUE))
check("reduced motion", grepl("prefers-reduced-motion: reduce", ui_facade, fixed = TRUE) &&
  grepl(".ynow-cookie-banner", ui_facade, fixed = TRUE))
check(
  "no login gate on banner",
  !grepl("ynow_cookie_banner[\\s\\S]{0,400}ynow-menu-lock", ui, perl = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, "\n", sep = "")
  quit(status = 1)
}
cat("ALL PASS\n")
