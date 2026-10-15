# Account login: designed, disabled by default
# Run: Rscript app_25.0/tests/test_auth_account.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_auth_account.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  if (ok) {
    pass <<- pass + 1L
    cat("OK", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
  invisible(ok)
}

src_auth <- file.path(app_dir, "auth_account.R")
src_locale <- file.path(app_dir, "ui_locale.R")
src_global <- file.path(app_dir, "global.R")
src_ui <- file.path(app_dir, "ynow_ui.R")
src_srv <- file.path(app_dir, "ynow_server.R")

check("auth_account.R exists", file.exists(src_auth))
check("global sources auth_account.R", {
  g <- paste(readLines(src_global, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"auth_account\\.R\"', g)
})

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists("ui_str", mode = "function")) {
  source(src_locale, local = FALSE, encoding = "UTF-8")
}
source(src_auth, local = FALSE, encoding = "UTF-8")

# Default: disabled (unset / 0)
old_en <- Sys.getenv("YNOW_ENABLE_ACCOUNT_LOGIN", unset = NA_character_)
old_em <- Sys.getenv("YNOW_AUTH_DEMO_EMAIL", unset = NA_character_)
old_pw <- Sys.getenv("YNOW_AUTH_DEMO_PASSWORD", unset = NA_character_)
on.exit({
  if (is.na(old_en)) Sys.unsetenv("YNOW_ENABLE_ACCOUNT_LOGIN") else Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = old_en)
  if (is.na(old_em)) Sys.unsetenv("YNOW_AUTH_DEMO_EMAIL") else Sys.setenv(YNOW_AUTH_DEMO_EMAIL = old_em)
  if (is.na(old_pw)) Sys.unsetenv("YNOW_AUTH_DEMO_PASSWORD") else Sys.setenv(YNOW_AUTH_DEMO_PASSWORD = old_pw)
}, add = TRUE)

Sys.unsetenv("YNOW_ENABLE_ACCOUNT_LOGIN")
check("default auth disabled", identical(ynow_auth_enabled(), FALSE))
check("header UI NULL when disabled", is.null(ynow_auth_header_ui()))
check("modal UI NULL when disabled", is.null(ynow_auth_modal_ui()))
check("css NULL when disabled", is.null(ynow_auth_css()))

res_off <- ynow_auth_try_login("a@b.com", "x")
check("login rejected when disabled", isFALSE(res_off$ok) && identical(res_off$message_key, "auth_msg_disabled"))

Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
check("env 1 enables auth", isTRUE(ynow_auth_enabled()))
suppressPackageStartupMessages({
  library(htmltools)
  library(shiny)
})
# shinyBS optional for modal; skip modal deep check if missing
check("header UI present when enabled", {
  h <- tryCatch(ynow_auth_header_ui(), error = function(e) e)
  !inherits(h, "error") && !is.null(h)
})
check("modal UI present when enabled", {
  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    TRUE  # skip when shinyBS unavailable in bare test env
  } else {
    m <- tryCatch(ynow_auth_modal_ui(), error = function(e) e)
    !inherits(m, "error") && !is.null(m)
  }
})

Sys.unsetenv("YNOW_AUTH_DEMO_EMAIL")
Sys.unsetenv("YNOW_AUTH_DEMO_PASSWORD")
res_nb <- ynow_auth_try_login("a@b.com", "secret")
check("no backend message", isFALSE(res_nb$ok) && identical(res_nb$message_key, "auth_msg_no_backend"))

Sys.setenv(YNOW_AUTH_DEMO_EMAIL = "demo@ynow.app", YNOW_AUTH_DEMO_PASSWORD = "demo-pass")
res_bad <- ynow_auth_try_login("demo@ynow.app", "wrong")
check("invalid password", isFALSE(res_bad$ok) && identical(res_bad$message_key, "auth_msg_invalid"))
res_ok <- ynow_auth_try_login("Demo@Ynow.app", "demo-pass")
check("demo login ok", isTRUE(res_ok$ok) && identical(res_ok$user$email, "demo@ynow.app"))
check("missing fields", {
  r <- ynow_auth_try_login("", "")
  isFALSE(r$ok) && identical(r$message_key, "auth_msg_missing")
})

# Disable again — chrome must disappear
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
check("0 disables auth", identical(ynow_auth_enabled(), FALSE))
check("header gone when re-disabled", is.null(ynow_auth_header_ui()))

keys <- c(
  "auth_sign_in", "auth_sign_out", "auth_modal_title", "auth_modal_help",
  "auth_email_label", "auth_password_label", "auth_submit", "auth_cancel",
  "auth_msg_ok", "auth_msg_disabled", "auth_msg_missing", "auth_msg_invalid",
  "auth_msg_no_backend", "auth_about_note"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check("zh uses 登入", grepl("登入", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh no simplified 登录", !grepl("登录", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh uses 帳號", grepl("帳號", ui_str("auth_modal_title", "zh-TW"), fixed = TRUE))
check("zh no 账户", !grepl("账户", ui_str("auth_modal_title", "zh-TW"), fixed = TRUE))

ui <- paste(readLines(src_ui, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(src_srv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI mounts auth header helper", grepl("ynow_auth_header_ui", ui, fixed = TRUE))
check("UI mounts auth modal helper", grepl("ynow_auth_modal_ui", ui, fixed = TRUE))
check("UI About mentions account sign-in", grepl("Account sign-in", ui, fixed = TRUE) &&
        grepl("帳號登入", ui, fixed = TRUE))
check("UI About says disabled by default", grepl("disabled by default", ui, fixed = TRUE) &&
        grepl("預設關閉", ui, fixed = TRUE))
check("UI wires ynowAuthState handler", grepl("ynowAuthState", ui, fixed = TRUE))
check("server wires ynow_auth_server", grepl("ynow_auth_server", srv, fixed = TRUE))
check("display version v25", grepl("The YNow App v25", ui, fixed = TRUE))

# Live line folder name
check("live folder is app_25.0", basename(app_dir) == "app_25.0")

cat(sprintf("PASS %d  FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
