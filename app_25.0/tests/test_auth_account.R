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
src_oauth <- file.path(app_dir, "auth_oauth.R")
src_db <- file.path(app_dir, "auth_db.R")
src_locale <- file.path(app_dir, "ui_locale.R")
src_global <- file.path(app_dir, "global.R")
src_ui <- file.path(app_dir, "ynow_ui.R")
src_srv <- file.path(app_dir, "ynow_server.R")

check("auth_account.R exists", file.exists(src_auth))
check("auth_oauth.R exists", file.exists(src_oauth))
check("auth_db.R exists", file.exists(src_db))
check("global sources auth_account.R", {
  g <- paste(readLines(src_global, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"auth_account\\.R\"', g)
})
check("global sources auth_oauth.R", {
  g <- paste(readLines(src_global, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"auth_oauth\\.R\"', g)
})
check("global sources auth_db.R", {
  g <- paste(readLines(src_global, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"auth_db\\.R\"', g)
})

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists("ui_str", mode = "function")) {
  source(src_locale, local = FALSE, encoding = "UTF-8")
}
suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  if (!requireNamespace("digest", quietly = TRUE)) stop("digest required")
  if (!requireNamespace("openssl", quietly = TRUE)) stop("openssl required")
})
source(src_auth, local = FALSE, encoding = "UTF-8")
source(src_db, local = FALSE, encoding = "UTF-8")
source(src_oauth, local = FALSE, encoding = "UTF-8")

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
check("default auth enabled", isTRUE(ynow_auth_enabled()))

Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
check("0 disables auth", identical(ynow_auth_enabled(), FALSE))
check("header UI NULL when disabled", is.null(ynow_auth_header_ui()))
check("modal UI NULL when disabled", is.null(ynow_auth_modal_ui()))
check("css NULL when disabled", is.null(ynow_auth_css()))
check("compact drops NULL", {
  identical(ynow_compact_ui(NULL, NULL), list()) &&
    length(ynow_compact_ui(NULL, list(a = 1), NULL)) == 1L
})

# Regression: shinydashboard header must not receive NULL auth children when disabled
check("dashboardHeader tolerates disabled auth via .list", {
  suppressPackageStartupMessages({
    library(htmltools)
    library(shiny)
    library(shinydashboard)
  })
  err <- tryCatch({
    dashboardHeader(
      title = "t",
      titleWidth = 250,
      tags$li(class = "dropdown", tags$span("mkt")),
      tags$li(class = "dropdown", tags$span("lang")),
      tags$li(class = "dropdown", tags$span("logo")),
      .list = ynow_compact_ui(ynow_auth_header_ui())
    )
    NULL
  }, error = function(e) conditionMessage(e))
  is.null(err)
})
check("raw NULL child still errors (documents root cause)", {
  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
  })
  err <- tryCatch({
    dashboardHeader(
      title = "t",
      tags$li(class = "dropdown", "a"),
      NULL,
      tags$li(class = "dropdown", "b")
    )
    ""
  }, error = function(e) conditionMessage(e))
  grepl("shiny.tag", err, fixed = TRUE)
})

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
check("header gone when re-disabled", is.null(ynow_auth_header_ui()))

# Lite never requires login; Full does when auth feature is on
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
check("Lite never requires login", identical(ynow_auth_requires_login(TRUE), FALSE))
check("Full requires login when auth on", identical(ynow_auth_requires_login(FALSE), TRUE))
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
check("Full guest OK when auth off", identical(ynow_auth_requires_login(FALSE), FALSE))

Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
suppressPackageStartupMessages(library(shinydashboard))
check("admin menu when enabled", !is.null(ynow_admin_menu_item()))
check("admin tab when enabled", !is.null(ynow_admin_tab_ui()))
check("admin panel builds", {
  p <- ynow_admin_panel_ui(list(email = "demo@ynow.app", logged_in_at = "t"), "en", "v25.01")
  inherits(p, "shiny.tag") || inherits(p, "shiny.tag.list")
})
check("pa guest gate builds", inherits(ynow_pa_guest_gate_ui(), "shiny.tag"))
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
check("admin menu NULL when disabled", is.null(ynow_admin_menu_item()))

keys <- c(
  "auth_sign_in", "auth_sign_out", "auth_modal_title", "auth_modal_help",
  "auth_email_label", "auth_password_label", "auth_submit", "auth_cancel",
  "auth_or_email", "auth_google", "auth_facebook", "auth_apple", "auth_forgot",
  "auth_register", "auth_resend",
  "auth_msg_ok", "auth_msg_disabled", "auth_msg_missing", "auth_msg_invalid",
  "auth_msg_no_backend", "auth_msg_oauth_soon", "auth_msg_oauth_need_google",
  "auth_msg_oauth_need_facebook", "auth_msg_oauth_need_apple",
  "auth_msg_oauth_failed", "auth_msg_oauth_state",
  "auth_msg_oauth_no_email", "auth_msg_register_soon", "auth_msg_register_ok",
  "auth_msg_register_failed", "auth_msg_email_taken", "auth_msg_password_short",
  "auth_msg_disabled_account", "auth_about_neon",
  "auth_msg_forgot_soon", "auth_msg_resend_soon", "auth_about_note",
  "menu_admin", "auth_admin_title", "auth_admin_intro", "auth_pa_gate_title",
  "auth_pa_gate_body", "auth_admin_tool_lite", "pa_email_account_help"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check("zh uses 登入", grepl("登入", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh no simplified 登录", !grepl("登录", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh uses 帳號", grepl("帳號", ui_str("auth_register", "zh-TW"), fixed = TRUE))
check("zh no 账户", !grepl("账户", ui_str("auth_register", "zh-TW"), fixed = TRUE))
check("zh modal title YNow", grepl("The YNow App", ui_str("auth_modal_title", "zh-TW"), fixed = TRUE))
check("zh Email 登入 submit", grepl("Email 登入", ui_str("auth_submit", "zh-TW"), fixed = TRUE))

ui <- paste(readLines(src_ui, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(src_srv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI mounts auth sidebar helper", grepl("ynow_auth_links_ui", ui, fixed = TRUE) ||
        grepl("ynow_auth_header_ui", ui, fixed = TRUE))
check("UI mounts auth modal helper", grepl("ynow_auth_modal_ui", ui, fixed = TRUE))
check("UI mounts auth on sidebar date + home legal", {
  grepl("ynow-sidebar-date-row", ui, fixed = TRUE) &&
    grepl('ynow_auth_links_ui("sidebar")', ui, fixed = TRUE) &&
    grepl("ynow_auth_home_ui", ui, fixed = TRUE) &&
    grepl("ynow-home-legal-links", ui, fixed = TRUE) &&
    !grepl("ynow-lang-col", ui, fixed = TRUE)
})
check("UI About mentions account sign-in", grepl("Account sign-in", ui, fixed = TRUE) &&
        grepl("帳號登入", ui, fixed = TRUE))
check("UI wires ynowAuthState handler", grepl("ynowAuthState", ui, fixed = TRUE))
check("UI wires ynowAuthFeature handler", grepl("ynowAuthFeature", ui, fixed = TRUE))
check("UI wires OAuth redirect + callback", grepl("ynowAuthOAuthRedirect", ui, fixed = TRUE) &&
        grepl("ynow_auth_oauth_callback", ui, fixed = TRUE))
check("UI collapses sidebar before auth modal", {
  auth_open <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynowAuthOpenModal", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarCollapsed", ui, fixed = TRUE) &&
    grepl("ynowAuthOpenModal", auth_open, fixed = TRUE)
})
check("Neon not ready without env", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DB_USER")
  Sys.unsetenv("YNOW_DB_PASSWORD")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.unsetenv("DATABASE_URL")
  identical(ynow_db_ready(), FALSE)
})
check("Neon URL parse", {
  p <- ynow_db_parse_url(
    "postgresql://neondb_owner:s3cret@ep-cool-pooler.us-east-2.aws.neon.tech/neondb?sslmode=require"
  )
  !is.null(p) && identical(p$user, "neondb_owner") && identical(p$password, "s3cret") &&
    identical(p$host, "ep-cool-pooler.us-east-2.aws.neon.tech") &&
    identical(p$dbname, "neondb") && identical(p$sslmode, "require")
})
check("Neon password hash verify", {
  h <- ynow_db_hash_password("correct-horse-battery")
  isTRUE(ynow_db_verify_password("correct-horse-battery", h)) &&
    !isTRUE(ynow_db_verify_password("wrong", h))
})
check("register without Neon says soon", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_auth_try_register("a@b.com", "password123")
  isFALSE(r$ok) && identical(r$message_key, "auth_msg_register_soon")
})
check("UI About mentions Neon", grepl("Neon Free", ui, fixed = TRUE) &&
        grepl("Neon Free", ui, fixed = TRUE))
check("migrate script exists", file.exists(file.path(dirname(app_dir), "scripts", "neon_migrate.R")) ||
        file.exists(file.path(app_dir, "..", "scripts", "neon_migrate.R")))
check("OAuth google not ready without env", {
  Sys.unsetenv("YNOW_SUPABASE_URL")
  Sys.unsetenv("YNOW_SUPABASE_ANON_KEY")
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_ID")
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_SECRET")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_oauth_authorize_url("google")
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_need_google")
})
check("OAuth facebook not ready without env", {
  Sys.unsetenv("YNOW_FACEBOOK_APP_ID")
  Sys.unsetenv("YNOW_FACEBOOK_APP_SECRET")
  r <- ynow_oauth_authorize_url("facebook")
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_need_facebook")
})
check("OAuth apple not ready without Supabase", {
  Sys.unsetenv("YNOW_SUPABASE_URL")
  Sys.unsetenv("YNOW_SUPABASE_ANON_KEY")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_oauth_authorize_url("apple")
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_need_apple")
})
check("OAuth google URL when client set", {
  Sys.setenv(
    YNOW_GOOGLE_CLIENT_ID = "test-client.apps.googleusercontent.com",
    YNOW_GOOGLE_CLIENT_SECRET = "test-secret",
    YNOW_OAUTH_STATE_SECRET = "state-secret-for-tests",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  r <- ynow_oauth_authorize_url("google")
  isTRUE(r$ok) && grepl("accounts.google.com", r$url, fixed = TRUE) &&
    grepl("client_id=test-client", r$url, fixed = TRUE)
})
check("OAuth facebook URL when app set", {
  Sys.setenv(
    YNOW_FACEBOOK_APP_ID = "1234567890",
    YNOW_FACEBOOK_APP_SECRET = "fb-secret",
    YNOW_OAUTH_STATE_SECRET = "state-secret-for-tests",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  r <- ynow_oauth_authorize_url("facebook")
  isTRUE(r$ok) && grepl("facebook.com", r$url, fixed = TRUE) &&
    grepl("client_id=1234567890", r$url, fixed = TRUE)
})
check("OAuth supabase authorize URL", {
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_ID")
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_SECRET")
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  r <- ynow_oauth_authorize_url("google")
  isTRUE(r$ok) && grepl("abc.supabase.co/auth/v1/authorize", r$url, fixed = TRUE) &&
    grepl("provider=google", r$url, fixed = TRUE)
})
check("OAuth supabase apple URL", {
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  r <- ynow_oauth_authorize_url("apple")
  isTRUE(r$ok) && grepl("provider=apple", r$url, fixed = TRUE)
})
check("OAuth state round-trip", {
  Sys.setenv(YNOW_OAUTH_STATE_SECRET = "state-secret-for-tests")
  st <- ynow_oauth_make_state("google")
  p <- ynow_oauth_parse_state(st)
  isTRUE(p$ok) && identical(p$provider, "google")
})
check("OAuth state rejects tamper", {
  Sys.setenv(YNOW_OAUTH_STATE_SECRET = "state-secret-for-tests")
  st <- ynow_oauth_make_state("google")
  bad <- paste0(st, "x")
  p <- ynow_oauth_parse_state(bad)
  identical(p$ok, FALSE)
})
check("oauth sentinel never verifies as password", {
  h <- ynow_db_oauth_password_sentinel("google", "uid-1")
  startsWith(h, "oauth$") && !isTRUE(ynow_db_verify_password("anything", h))
})
check("social accept without Neon is self-serve user", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_auth_accept_social_user(list(
    email = "New.User@Gmail.com",
    display_name = "New User",
    provider = "google",
    provider_uid = "sub-123"
  ))
  isTRUE(r$ok) && identical(r$user$email, "new.user@gmail.com") &&
    identical(r$user$role, "user") && identical(r$user$provider, "google")
})
check("social accept rejects empty email", {
  r <- ynow_auth_accept_social_user(list(email = "", provider = "google"))
  isFALSE(r$ok) && identical(r$message_key, "auth_msg_oauth_no_email")
})
check("locale says self-serve no admin", {
  grepl("no admin permission", ui_str("auth_modal_help", "en"), fixed = TRUE) &&
    grepl("無需後台開權限", ui_str("auth_modal_help", "zh-TW"), fixed = TRUE)
})
check("UI About Lite no account", grepl("Lite never requires an account", ui, fixed = TRUE) &&
        grepl("Lite 版無需帳號即可完整使用", ui, fixed = TRUE))
auth_src <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("auth link classes for sidebar + home", grepl("ynow-auth-links--sidebar", auth_src, fixed = TRUE) &&
        grepl("ynow-auth-links--home", auth_src, fixed = TRUE) &&
        grepl("ynow-auth-link", auth_src, fixed = TRUE))
check("UI CSS hides auth in Lite", grepl("body.ynow-lite .ynow-auth-links", auth_src, fixed = TRUE))
check("login card uses app palette tokens", {
  grepl("--ynow-ink", auth_src, fixed = TRUE) &&
    grepl("--ynow-logo-blue", auth_src, fixed = TRUE) &&
    grepl("--ynow-wash", auth_src, fixed = TRUE) &&
    grepl("ynow-auth-social--google", auth_src, fixed = TRUE) &&
    grepl("ynow-auth-social--apple", auth_src, fixed = TRUE) &&
    grepl("ynow-auth-email-submit", auth_src, fixed = TRUE)
})
check("sidebar auth UI is div not navbar li", {
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  h <- ynow_auth_links_ui("sidebar")
  home <- ynow_auth_home_ui()
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
  inherits(h, "shiny.tag") && identical(h$name, "div") &&
    grepl("ynow-auth-links--sidebar", paste(as.character(h), collapse = ""), fixed = TRUE) &&
    grepl("ynow-auth-links--home", paste(as.character(home), collapse = ""), fixed = TRUE) &&
    grepl("ynow_auth_open_home", paste(as.character(home), collapse = ""), fixed = TRUE)
})
check("modal card layout present when enabled", {
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
    TRUE
  } else {
    m <- ynow_auth_modal_ui()
    html <- paste(as.character(m), collapse = "")
    Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
    grepl("ynow-auth-card", html, fixed = TRUE) &&
      grepl("ynow_auth_google", html, fixed = TRUE) &&
      grepl("ynow_auth_facebook", html, fixed = TRUE) &&
      grepl("ynow_auth_apple", html, fixed = TRUE) &&
      grepl("ynow-auth-email-submit", html, fixed = TRUE)
  }
})
check("server wires ynow_auth_server", grepl("ynow_auth_server", srv, fixed = TRUE))
check("server renders admin panel", grepl("output\\$ynow_admin_panel", srv))
check("server prefills alert email from account", grepl("ynow_auth_session_email", srv, fixed = TRUE) &&
        grepl("pa_email", srv, fixed = TRUE))
check("pa save gates login + resolve email", {
  pa <- paste(readLines(file.path(app_dir, "price_alert.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("\\.pa_login_ok", pa) && grepl("\\.pa_resolve_email", pa) &&
    grepl("ynow_kpi_alert_open", pa, fixed = TRUE) && grepl("pa_quick_save", pa, fixed = TRUE)
})
check("screener add gates login", {
  scr <- paste(readLines(file.path(app_dir, "stock_screener.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("scr_add", scr, fixed = TRUE) && grepl("auth_pa_gate_title", scr, fixed = TRUE) &&
    grepl("ynow_auth_session_email", scr, fixed = TRUE)
})
check("auth default on (unset)", {
  a <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('unset = "1"', a, fixed = TRUE)
})
check("locale about auth default on", grepl("on by default", ui_str("auth_about_note", "en"), fixed = TRUE) &&
        grepl("預設啟用", ui_str("auth_about_note", "zh-TW"), fixed = TRUE))
check("display version v25", grepl("The YNow App v25", ui, fixed = TRUE))

# Live line folder name
check("live folder is app_25.0", basename(app_dir) == "app_25.0")

cat(sprintf("PASS %d  FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
