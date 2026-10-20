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
# Isolate from app_25.0/.Renviron (R auto-loads it when cwd is the app dir).
# Live account store is Supabase Postgres via YNOW_DB_* — not needed for unit paths.
.db_env_keys <- c(
  "YNOW_DB_HOST", "YNOW_DB_USER", "YNOW_DB_PASSWORD", "YNOW_DB_NAME",
  "YNOW_DB_PORT", "YNOW_DB_SSLMODE", "YNOW_DATABASE_URL", "DATABASE_URL",
  "YNOW_POINTS_BACKEND"
)
.old_db <- setNames(lapply(.db_env_keys, function(k) Sys.getenv(k, unset = NA_character_)), .db_env_keys)
on.exit({
  if (is.na(old_en)) Sys.unsetenv("YNOW_ENABLE_ACCOUNT_LOGIN") else Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = old_en)
  if (is.na(old_em)) Sys.unsetenv("YNOW_AUTH_DEMO_EMAIL") else Sys.setenv(YNOW_AUTH_DEMO_EMAIL = old_em)
  if (is.na(old_pw)) Sys.unsetenv("YNOW_AUTH_DEMO_PASSWORD") else Sys.setenv(YNOW_AUTH_DEMO_PASSWORD = old_pw)
  for (k in names(.old_db)) {
    v <- .old_db[[k]]
    if (is.na(v)) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(v), k))
  }
}, add = TRUE)
for (k in .db_env_keys) Sys.unsetenv(k)

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
# Demo-path checks require no Postgres bind (Cloud agents often export YNOW_DB_*).
Sys.unsetenv("YNOW_DB_HOST")
Sys.unsetenv("YNOW_DB_USER")
Sys.unsetenv("YNOW_DB_PASSWORD")
Sys.unsetenv("YNOW_DB_NAME")
Sys.unsetenv("YNOW_DATABASE_URL")
Sys.unsetenv("DATABASE_URL")
res_nb <- ynow_auth_try_login("a@b.com", "secret")
check("no backend message", isFALSE(res_nb$ok) && identical(res_nb$message_key, "auth_msg_no_backend"))

Sys.setenv(YNOW_AUTH_DEMO_EMAIL = "demo@ynow.app", YNOW_AUTH_DEMO_PASSWORD = "demo-pass")
res_bad <- ynow_auth_try_login("demo@ynow.app", "wrong")
check("invalid password", isFALSE(res_bad$ok) && identical(res_bad$message_key, "auth_msg_invalid"))
res_ok <- ynow_auth_try_login("Demo@Ynow.app", "demo-pass")
check("demo login ok", isTRUE(res_ok$ok) && identical(res_ok$user$email, "demo@ynow.app") &&
        grepl("^mem:", as.character(res_ok$user$id %||% "")[1]))
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
check("admin panel flat block (no AdminLTE box)", {
  p <- ynow_admin_panel_ui(list(email = "demo@ynow.app", logged_in_at = "t"), "en", "v25.01")
  html <- as.character(p)
  grepl("ynow-maint-block", html, fixed = TRUE) &&
    grepl("ynow-maint-shell", html, fixed = TRUE) &&
    grepl("ynow-maint-theme-select", html, fixed = TRUE) &&
    grepl('data-ynow-maint-theme="logo"', html, fixed = TRUE) &&
    !grepl("box-solid", html, fixed = TRUE) &&
    !grepl("box-primary", html, fixed = TRUE)
})
check("points panel flat block + theme", {
  p <- ynow_points_panel_ui(list(email = "demo@ynow.app"), "zh-TW", fyn_balance = 10L, pyn_balance = 0L)
  html <- as.character(p)
  grepl("ynow-maint-block", html, fixed = TRUE) &&
    grepl("ynow-maint-theme-select", html, fixed = TRUE) &&
    grepl("黑白色系", html, fixed = TRUE) &&
    grepl("藍海色系", html, fixed = TRUE) &&
    grepl("森林色系", html, fixed = TRUE) &&
    !grepl("box-solid", html, fixed = TRUE)
})
check("auth css includes maint theme + JS", {
  css <- ynow_auth_css()
  html <- paste(as.character(css), collapse = "\n")
  grepl("ynow-maint-shell", html, fixed = TRUE) &&
    grepl("ynow_maint_theme", html, fixed = TRUE) &&
    grepl("data-ynow-maint-theme", html, fixed = TRUE)
})
check("pa guest gate builds", inherits(ynow_pa_guest_gate_ui(), "shiny.tag"))
check("member gate includes promo and FAQ", {
  html <- paste(as.character(ynow_pa_guest_gate_ui()), collapse = "\n")
  grepl("ynow-gate-promo", html, fixed = TRUE) &&
    grepl("ynow-gate-faq", html, fixed = TRUE) &&
    grepl("ynow-gate-promo-kicker", html, fixed = TRUE) &&
    grepl("ynow-gate-faq-q1", html, fixed = TRUE) &&
    grepl("ynow-auth-open", html, fixed = TRUE) &&
    grepl('data-ynow-gate-feature="alerts"', html, fixed = TRUE) &&
    grepl("ynow-gate-demo-video", html, fixed = TRUE) &&
    grepl("demos/alerts.mp4", html, fixed = TRUE)
})
check("lab and checklist gates are feature-specific", {
  lab <- paste(as.character(ynow_member_gate_ui(
    "g", "t", "b", "btn", feature = "lab"
  )), collapse = "\n")
  chk <- paste(as.character(ynow_member_gate_ui(
    "g2", "t2", "b2", "btn2", feature = "checklist"
  )), collapse = "\n")
  grepl("demos/lab.mp4", lab, fixed = TRUE) &&
    grepl("Unlock Quantitative Backtest Lab", lab, fixed = TRUE) &&
    grepl("demos/checklist.mp4", chk, fixed = TRUE) &&
    grepl("Unlock Decision Checklist", chk, fixed = TRUE) &&
    !grepl("Unlock Value Alerts", lab, fixed = TRUE)
})
check("backtest and checklist require the member gate", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  dc <- paste(readLines(file.path(app_dir, "decision_checklist_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  auth <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_lab_body", ui, fixed = TRUE) &&
    grepl("ynow-member-body", ui, fixed = TRUE) &&
    grepl("ynow_chk_body", dc, fixed = TRUE) &&
    grepl("ynow-member-gate", auth, fixed = TRUE) &&
    grepl("not(.ynow-lite):not(.ynow-authed) .ynow-member-body", auth, fixed = TRUE)
})
check("login tabs show a lock before sign-in", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  auth <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow-menu-lock", ui, fixed = TRUE) &&
    grepl("ynow_sidebar_guest_lock_tabs", auth, fixed = TRUE) &&
    grepl("__YNOW_SIDEBAR_GUEST_LOCK_TABS", ui, fixed = TRUE) &&
    grepl("lab_notes: true", ui, fixed = TRUE) &&
    grepl("decision_checklist: true", ui, fixed = TRUE) &&
    grepl("price_alerts: true", ui, fixed = TRUE) &&
    !grepl("account_admin: true", ui, fixed = TRUE) &&
    !grepl("account_points: true", ui, fixed = TRUE) &&
    !grepl("body.skin-black.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .main-sidebar .sidebar-menu a:has(.ynow-menu-lock)", ui, fixed = TRUE) &&
    grepl("not(.ynow-lite):not(.ynow-authed) .sidebar-menu .ynow-menu-lock", ui, fixed = TRUE) &&
    grepl("a:has(.ynow-menu-lock) > i:not(.ynow-menu-lock):not(.pull-right)", ui, fixed = TRUE) &&
    grepl("ynow-auth-header", ui, fixed = TRUE) &&
    grepl("display:none !important;", auth, fixed = TRUE) &&
    grepl("treeview:has(a[data-value=\\\"account_admin\\\"])", auth, fixed = TRUE)
})
check("Lite CSS omits every Full guest-locked sidebar tab", {
  auth <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tabs <- tryCatch(ynow_sidebar_guest_lock_tabs(), error = function(e) character(0))
  css <- tryCatch(ynow_sidebar_lite_omit_css(), error = function(e) {
    tryCatch(ynow_sidebar_guest_lock_lite_css(), error = function(e2) "")
  })
  length(tabs) >= 3L &&
    all(c("lab_notes", "decision_checklist", "price_alerts") %in% tabs) &&
    (grepl("ynow_sidebar_lite_omit_css", ui, fixed = TRUE) ||
       grepl("ynow_sidebar_guest_lock_lite_css", ui, fixed = TRUE)) &&
    grepl("ynow_sidebar_lite_omit", auth, fixed = TRUE) &&
    all(vapply(tabs, function(t) grepl(sprintf('data-value="%s"', t), css, fixed = TRUE), logical(1)))
})
check("guest PDF download control uses muted locked styling", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynowSyncSidebarDownloadIcon", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-dl-lock", ui, fixed = TRUE) &&
    grepl(
      "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-sidebar-download-wrap #download_report",
      ui,
      fixed = TRUE
    ) &&
    grepl("color: #a3a3a3 !important", ui, fixed = TRUE)
})
check("stock chrome hidden on guest gate tabs and account panels", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("input.sidebar_tabs != 'account_admin'", ui, fixed = TRUE) &&
    grepl("input.sidebar_tabs != 'account_points'", ui, fixed = TRUE) &&
    grepl("shiny-tab-lab_notes.active) .ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("shiny-tab-decision_checklist.active) .ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("shiny-tab-price_alerts.active) .ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-snapshot-foot-links", ui, fixed = TRUE)
})
check("member gate uses mono gate box not warning amber", {
  auth <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow-gate-box", auth, fixed = TRUE) &&
    !grepl('class = "box box-warning ynow-member-gate"', auth, fixed = TRUE) &&
    grepl("\\.box\\.ynow-gate-box", ui) &&
    grepl("--ynow-gate-accent", ui, fixed = TRUE) &&
    grepl("ynow-gate-promo", ui, fixed = TRUE) &&
    grepl("ynow-gate-faq", ui, fixed = TRUE)
})
check("account gate uses shared promo FAQ helper", {
  srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynow_gate_promo_faq_ui", srv, fixed = TRUE) &&
    grepl('feature = "account"', srv, fixed = TRUE) &&
    grepl("ynow-auth-gate", srv, fixed = TRUE) &&
    grepl("ynow-auth-open", srv, fixed = TRUE)
})
check("snap_defaults gate keeps promo; sidebar has no PDF promo chrome", {
  ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  auth <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  !grepl("ynow_pdf_guest_gate", ui, fixed = TRUE) &&
    !grepl("ynow_pdf_gate_signin", ui, fixed = TRUE) &&
    grepl('feature = "snap_defaults"', ui, fixed = TRUE) &&
    grepl("ynowSyncSidebarDownloadIcon", ui, fixed = TRUE) &&
    grepl("ynow-sidebar-dl-lock", ui, fixed = TRUE) &&
    grepl("ynow-gate-demo-video", auth, fixed = TRUE) &&
    grepl("demos/", auth, fixed = TRUE)
})
check("demo clips exist under www/demos", {
  demos <- file.path(app_dir, "www", "demos")
  files <- c(
    "lab.mp4", "lab.webm", "checklist.mp4", "alerts.mp4",
    "account.mp4", "pdf.mp4", "snap_defaults.mp4", "inner_lock.mp4",
    "atx.mp4", "atx.webm"
  )
  all(file.exists(file.path(demos, files)))
})
Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
check("admin menu NULL when disabled", is.null(ynow_admin_menu_item()))

keys <- c(
  "auth_sign_in", "auth_sign_out", "auth_modal_title", "auth_modal_help",
  "auth_email_label", "auth_password_label", "auth_submit", "auth_cancel",
  "auth_or_email", "auth_google", "auth_facebook", "auth_linkedin", "auth_forgot",
  "auth_register", "auth_register_btn", "auth_modal_title_register", "auth_resend",
  "auth_msg_ok", "auth_msg_disabled", "auth_msg_missing", "auth_msg_invalid",
  "auth_msg_no_backend", "auth_msg_oauth_soon", "auth_msg_oauth_need_google",
  "auth_msg_oauth_need_facebook", "auth_msg_oauth_need_linkedin",
  "auth_msg_oauth_failed", "auth_msg_oauth_state",
  "auth_msg_oauth_no_email", "auth_msg_register_soon", "auth_msg_register_ok",
  "auth_msg_register_failed", "auth_msg_email_taken", "auth_msg_password_short",
  "auth_msg_disabled_account", "auth_about_neon",
  "auth_msg_forgot_soon", "auth_msg_resend_soon", "auth_about_note",
  "menu_admin", "auth_admin_title", "auth_admin_intro", "auth_pa_gate_title",
  "auth_pa_gate_body", "auth_admin_tool_lite", "pa_email_account_help",
  "auth_gate_lab_body", "auth_gate_lab_kicker", "auth_gate_lab_lead",
  "auth_gate_lab_b1", "auth_gate_lab_b2", "auth_gate_lab_b3", "auth_gate_lab_demo_caption",
  "auth_gate_checklist_body", "auth_gate_checklist_kicker", "auth_gate_checklist_lead",
  "auth_gate_checklist_b1", "auth_gate_checklist_b2", "auth_gate_checklist_b3",
  "auth_gate_checklist_demo_caption",
  "auth_gate_alerts_body", "auth_gate_alerts_kicker", "auth_gate_alerts_lead",
  "auth_gate_alerts_b1", "auth_gate_alerts_b2", "auth_gate_alerts_b3",
  "auth_gate_alerts_demo_caption",
  "auth_gate_account_body", "auth_gate_account_kicker", "auth_gate_account_lead",
  "auth_gate_account_b1", "auth_gate_account_b2", "auth_gate_account_b3",
  "auth_gate_account_demo_caption",
  "auth_gate_pdf_body", "auth_gate_pdf_kicker", "auth_gate_pdf_lead",
  "auth_gate_pdf_b1", "auth_gate_pdf_b2", "auth_gate_pdf_b3", "auth_gate_pdf_demo_caption",
  "auth_gate_snap_defaults_body", "auth_gate_snap_defaults_kicker",
  "auth_gate_snap_defaults_lead", "auth_gate_snap_defaults_b1",
  "auth_gate_snap_defaults_b2", "auth_gate_snap_defaults_b3",
  "auth_gate_snap_defaults_demo_caption",
  "auth_gate_inner_lock_body", "auth_gate_inner_lock_kicker",
  "auth_gate_inner_lock_lead", "auth_gate_inner_lock_b1",
  "auth_gate_inner_lock_b2", "auth_gate_inner_lock_b3",
  "auth_gate_inner_lock_demo_caption",
  "auth_gate_atx_body", "auth_gate_atx_kicker", "auth_gate_atx_lead",
  "auth_gate_atx_b1", "auth_gate_atx_b2", "auth_gate_atx_b3",
  "auth_gate_atx_demo_caption",
  "auth_gate_faq_title",
  "auth_gate_faq_q1", "auth_gate_faq_a1", "auth_gate_faq_q2", "auth_gate_faq_a2",
  "auth_gate_faq_q3", "auth_gate_faq_a3", "auth_gate_faq_q4", "auth_gate_faq_a4",
  "auth_admin_theme", "auth_admin_theme_scope", "auth_admin_theme_logo",
  "auth_admin_theme_mono", "auth_admin_theme_ocean", "auth_admin_theme_forest"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check("zh uses 登入", grepl("登入", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh no simplified 登录", !grepl("登录", ui_str("auth_sign_in", "zh-TW"), fixed = TRUE))
check("zh uses 帳號", grepl("帳號", ui_str("auth_register", "zh-TW"), fixed = TRUE))
check("zh no 账户", !grepl("账户", ui_str("auth_register", "zh-TW"), fixed = TRUE))
check("register button is 註冊 / Register", {
  identical(ui_str("auth_register_btn", "en"), "Register") &&
    identical(ui_str("auth_register_btn", "zh-TW"), "註冊") &&
    !grepl("注册", ui_str("auth_register_btn", "zh-TW"), fixed = TRUE) &&
    grepl("Register for The YNow App", ui_str("auth_modal_title_register", "en"), fixed = TRUE) &&
    grepl("註冊 The YNow App", ui_str("auth_modal_title_register", "zh-TW"), fixed = TRUE)
})
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
check("Sys. Admin foot link hidden when signed in", {
  grepl("body.ynow-authed .ynow-sidebar-snapshot-foot #ynow_sidebar_test_btn", ui, fixed = TRUE) &&
    grepl("a#ynow_sidebar_test_btn.ynow-sidebar-test-link", ui, fixed = TRUE) &&
    grepl("display: none !important", ui, fixed = TRUE) &&
    grepl("ynowSyncSysAdminFootVisibility", ui, fixed = TRUE) &&
    grepl("tab === 'testing') tab = 'home'", ui, fixed = TRUE)
})
check("post-login server remaps testing tab to home", {
  auth_txt <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('identical(tab, "testing")) tab <- "home"', auth_txt, fixed = TRUE)
})
check("server blocks Sys. Admin click when signed in", {
  grepl("sidebar_test_click", srv, fixed = TRUE) &&
    grepl("ynow_auth$is_logged_in", srv, fixed = TRUE) &&
    grepl('updateTabItems(session, "sidebar_tabs", "home")', srv, fixed = TRUE)
})
check("client remaps testing→home when authed (goto + foot)", {
  grepl("tab === 'testing' && document.body &&", ui, fixed = TRUE) &&
    grepl("document.body.classList.contains('ynow-authed')", ui, fixed = TRUE) &&
    grepl("classList.contains('ynow-authed')) { return false; }", ui, fixed = TRUE) &&
    grepl("pointer-events", ui, fixed = TRUE)
})
check("server blocks testing tab + lazy mount when signed in", {
  grepl('identical(tab, "testing")', srv, fixed = TRUE) &&
    grepl("ynow-sysadmin-blocked", srv, fixed = TRUE)
})
check("UI wires ynowAuthFeature handler", grepl("ynowAuthFeature", ui, fixed = TRUE))
check("UI wires OAuth redirect + callback", grepl("ynowAuthOAuthRedirect", ui, fixed = TRUE) &&
        grepl("ynow_auth_oauth_callback", ui, fixed = TRUE))
check("UI collapses sidebar before auth modal", {
  auth_open <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("ynowAuthOpenModal", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarCollapsed", ui, fixed = TRUE) &&
    grepl("ynowAuthOpenModal", auth_open, fixed = TRUE)
})
check("ynowAuthOpenModal handler is unary (Shiny 1.14)", {
  # Zero-arg handlers throw in Shiny 1.14+ and never register.
  grepl("addHandler('ynowAuthOpenModal', function (_msg)", ui, fixed = TRUE)
})
check("sidebar collapse helper handles sidebar-open", {
  grepl("sidebar-open", ui, fixed = TRUE) &&
    grepl("ynowEnsureSidebarCollapsed", ui, fixed = TRUE) &&
    grepl("max-width: 767px", ui, fixed = TRUE)
})
check("client capture opens auth modal without server round-trip", {
  grepl("ynowRequestAuthModal", ui, fixed = TRUE) &&
    grepl("__ynowAuthOpenCapture", ui, fixed = TRUE) &&
    grepl("a.ynow-auth-open", ui, fixed = TRUE)
})
check("authed guards skip auth modal reopen (client)", {
  # Post-login: no-op show/request when body.ynow-authed;
  # capture skips authed or invisible; Bootstrap show.bs.modal veto;
  # ynowAuthPostLogin sets ynow-authed immediately on success.
  grepl("function ynowShowAuthModal()", ui, fixed = TRUE) &&
    grepl("function ynowRequestAuthModal()", ui, fixed = TRUE) &&
    grepl("function ynowIsAuthed()", ui, fixed = TRUE) &&
    grepl("classList.contains('ynow-authed')", ui, fixed = TRUE) &&
    grepl("hit.offsetParent === null", ui, fixed = TRUE) &&
    grepl("disp === 'none'", ui, fixed = TRUE) &&
    grepl("var invisible = authed || disp === 'none' || hit.offsetParent === null", ui, fixed = TRUE) &&
    grepl("__ynowAuthModalShowVeto", ui, fixed = TRUE) &&
    grepl("show.bs.modal", ui, fixed = TRUE) &&
    grepl("document.body.classList.add('ynow-authed')", ui, fixed = TRUE) &&
    !grepl("ynowAuthSidebarDbg", ui, fixed = TRUE)
})
check("ynow_ui.R parses as R (shinyapps source gate)", {
  # Bundle 12699126 died here: JS comment with bare "open" inside R "..." string.
  inherits(tryCatch(parse(src_ui, encoding = "UTF-8"), error = function(e) e), "expression")
})
check("auth modal veto JS comments have no bare double-quotes", {
  # Bare " in /* */ or // comments inside the R-embedded script string terminates
  # the R string early (see shinyapps exit status 1 / unexpected symbol).
  lines <- readLines(src_ui, warn = FALSE, encoding = "UTF-8")
  idx <- which(grepl("__ynowAuthModalShowVeto", lines, fixed = TRUE))
  if (!length(idx)) return(FALSE)
  # Window around veto + ynowRequestAuthModal assignment.
  lo <- max(1L, idx[[1]] - 15L)
  hi <- min(length(lines), idx[[1]] + 40L)
  chunk <- lines[lo:hi]
  cmt <- chunk[grepl("^\\s*(//|/\\*|\\*)", chunk)]
  !any(grepl('"', cmt, fixed = TRUE)) &&
    !grepl('toggleModal(..., "open")', paste(chunk, collapse = "\n"), fixed = TRUE)
})
check("authed guards skip auth modal reopen (server + CSS)", {
  auth_txt <- paste(readLines(src_auth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("body.ynow-authed .ynow-auth-open", auth_txt, fixed = TRUE) &&
    grepl("body.ynow-authed #ynow_auth_modal", auth_txt, fixed = TRUE) &&
    grepl("pointer-events:none !important", auth_txt, fixed = TRUE) &&
    grepl("if (!is.null(account_user())) return()", auth_txt, fixed = TRUE) &&
    grepl(".open_auth_modal <- function()", auth_txt, fixed = TRUE) &&
    !grepl("ynow_auth_sidebar_dbg", auth_txt, fixed = TRUE) &&
    !grepl("ynow_auth_sidebar_dbg_event", auth_txt, fixed = TRUE)
})
check("DB not ready without env", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DB_USER")
  Sys.unsetenv("YNOW_DB_PASSWORD")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.unsetenv("DATABASE_URL")
  identical(ynow_db_ready(), FALSE)
})
check("host_kind classifies neon/supabase/postgres/none", {
  identical(ynow_db_host_kind(NULL), "none") &&
    identical(ynow_db_host_kind(list(host = "")), "none") &&
    identical(
      ynow_db_host_kind(list(host = "ep-cool-pooler.us-east-2.aws.neon.tech")),
      "neon"
    ) &&
    identical(
      ynow_db_host_kind(list(host = "aws-0-ap-southeast-1.pooler.supabase.com")),
      "supabase"
    ) &&
    identical(ynow_db_host_kind(list(host = "db.internal.example")), "postgres")
})
check("DB URL parse", {
  p <- ynow_db_parse_url(
    "postgresql://neondb_owner:s3cret@ep-cool-pooler.us-east-2.aws.neon.tech/neondb?sslmode=require"
  )
  !is.null(p) && identical(p$user, "neondb_owner") && identical(p$password, "s3cret") &&
    identical(p$host, "ep-cool-pooler.us-east-2.aws.neon.tech") &&
    identical(p$dbname, "neondb") && identical(p$sslmode, "require")
})
check("DB password hash verify", {
  h <- ynow_db_hash_password("correct-horse-battery")
  isTRUE(ynow_db_verify_password("correct-horse-battery", h)) &&
    !isTRUE(ynow_db_verify_password("wrong", h))
})
check("register without DB says soon", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_auth_try_register("a@b.com", "password123")
  isFALSE(r$ok) && identical(r$message_key, "auth_msg_register_soon")
})
check("UI About mentions Neon Free as example provider", {
  grepl("Neon Free", ui, fixed = TRUE) &&
    grepl("YNOW_DB_", ui, fixed = TRUE) &&
    grepl("Supabase", ui, fixed = TRUE)
})
check("no debug probe instrumentation in auth_db", {
  db_txt <- paste(readLines(src_db, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  !grepl("ynow_db_debug_log_probe", db_txt, fixed = TRUE) &&
    !grepl("/opt/cursor/logs/debug.log", db_txt, fixed = TRUE) &&
    !grepl("ynow_db_runtime_probe", db_txt, fixed = TRUE)
})
check("migrate script exists", file.exists(file.path(dirname(app_dir), "scripts", "neon_migrate.R")) ||
        file.exists(file.path(app_dir, "..", "scripts", "neon_migrate.R")))
check("ynow_users schema enables fail-closed RLS", {
  auth_db_txt <- paste(readLines(file.path(app_dir, "auth_db.R"), warn = FALSE), collapse = "\n")
  grepl("ENABLE ROW LEVEL SECURITY", auth_db_txt, fixed = TRUE) &&
    grepl(".ynow_db_rls_fail_closed", auth_db_txt, fixed = TRUE) &&
    grepl("REVOKE ALL ON TABLE", auth_db_txt, fixed = TRUE) &&
    grepl("anon", auth_db_txt, fixed = TRUE) &&
    grepl("authenticated", auth_db_txt, fixed = TRUE) &&
    !grepl("FORCE ROW LEVEL SECURITY", auth_db_txt, fixed = TRUE)
})
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
check("OAuth linkedin not ready without Supabase", {
  Sys.unsetenv("YNOW_SUPABASE_URL")
  Sys.unsetenv("YNOW_SUPABASE_ANON_KEY")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  r <- ynow_oauth_authorize_url("linkedin")
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_need_linkedin")
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
check("locale has auth_msg_oauth_provider_disabled en+zh-TW", {
  en <- ui_str("auth_msg_oauth_provider_disabled", "en")
  zh <- ui_str("auth_msg_oauth_provider_disabled", "zh-TW")
  grepl("Supabase", en, fixed = TRUE) && grepl("Providers", en, fixed = TRUE) &&
    grepl("Supabase", zh, fixed = TRUE) && grepl("Providers", zh, fixed = TRUE) &&
    grepl("啟用", zh, fixed = TRUE)
})
check("OAuth supabase authorize URL", {
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_ID")
  Sys.unsetenv("YNOW_GOOGLE_CLIENT_SECRET")
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  # Mock settings: providers enabled (avoid live /auth/v1/settings probe).
  .ynow_oauth_cache$settings <- list(
    external = list(google = TRUE, facebook = TRUE, linkedin_oidc = TRUE)
  )
  r <- ynow_oauth_authorize_url("google")
  isTRUE(r$ok) && grepl("abc.supabase.co/auth/v1/authorize", r$url, fixed = TRUE) &&
    grepl("provider=google", r$url, fixed = TRUE)
})
check("OAuth supabase linkedin URL uses linkedin_oidc", {
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app"
  )
  .ynow_oauth_cache$settings <- list(
    external = list(google = TRUE, facebook = TRUE, linkedin_oidc = TRUE)
  )
  r <- ynow_oauth_authorize_url("linkedin")
  isTRUE(r$ok) && grepl("provider=linkedin_oidc", r$url, fixed = TRUE)
})
check("OAuth supabase provider disabled returns clear key", {
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_OAUTH_REDIRECT_URI = "https://example.com/app",
    YNOW_ENABLE_ACCOUNT_LOGIN = "1"
  )
  .ynow_oauth_cache$settings <- list(
    external = list(google = FALSE, facebook = FALSE, linkedin_oidc = FALSE, linkedin = FALSE)
  )
  rg <- ynow_oauth_authorize_url("google")
  rf <- ynow_oauth_authorize_url("facebook")
  rl <- ynow_oauth_authorize_url("linkedin")
  identical(rg$ok, FALSE) && identical(rg$message_key, "auth_msg_oauth_provider_disabled") &&
    is.null(rg$url) &&
    identical(rf$message_key, "auth_msg_oauth_provider_disabled") &&
    identical(rl$message_key, "auth_msg_oauth_provider_disabled")
})
check("OAuth provider_enabled reads external flags", {
  .ynow_oauth_cache$settings <- list(
    external = list(
      google = TRUE,
      facebook = FALSE,
      linkedin_oidc = list(enabled = TRUE),
      linkedin = FALSE
    )
  )
  identical(ynow_oauth_supabase_provider_enabled("google"), TRUE) &&
    identical(ynow_oauth_supabase_provider_enabled("facebook"), FALSE) &&
    identical(ynow_oauth_supabase_provider_enabled("linkedin"), TRUE)
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
check("OAuth supabase PKCE uses grant_type=pkce", {
  oauth_src <- paste(readLines(src_oauth, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Direct Google still uses grant_type=authorization_code; Supabase must use pkce.
  grepl("token?grant_type=pkce", oauth_src, fixed = TRUE) &&
    grepl("auth_code", oauth_src, fixed = TRUE) &&
    grepl("ynow_oauth_supabase_exchange_code", oauth_src, fixed = TRUE)
})
check("OAuth supabase code without verifier fails clearly", {
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_ENABLE_ACCOUNT_LOGIN = "1"
  )
  r <- ynow_oauth_complete(list(code = "testcode123", state = "badstate"))
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_state")
})
check("OAuth complete prefers supabase PKCE when verifier present", {
  # Without network, exchange fails with auth_msg_oauth_failed (not HMAC state).
  Sys.setenv(
    YNOW_SUPABASE_URL = "https://abc.supabase.co",
    YNOW_SUPABASE_ANON_KEY = "anon-key",
    YNOW_ENABLE_ACCOUNT_LOGIN = "1"
  )
  r <- ynow_oauth_complete(list(
    code = "testcode123",
    state = "not-hmac",
    code_verifier = "pkce-verifier-for-tests"
  ))
  identical(r$ok, FALSE) && identical(r$message_key, "auth_msg_oauth_failed")
})
check("UI wires PKCE + post-login restore", {
  grepl("ynowPkceVerifier", ui, fixed = TRUE) &&
    grepl("ynowAuthPostLogin", ui, fixed = TRUE) &&
    grepl("ynow_pre_oauth_tab", ui, fixed = TRUE) &&
    grepl("code_challenge_method=S256", ui, fixed = TRUE)
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
    identical(r$user$role, "user") && identical(r$user$provider, "google") &&
    grepl("^mem:", as.character(r$user$id %||% "")[1]) &&
    identical(
      as.character(r$user$id)[1],
      ynow_auth_memory_user_id("new.user@gmail.com")
    )
})
check("no-Neon social id enables points_on_login gift", {
  Sys.unsetenv("YNOW_DB_HOST")
  Sys.unsetenv("YNOW_DATABASE_URL")
  Sys.setenv(YNOW_POINTS_BACKEND = "memory", YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  pts_src <- file.path(app_dir, "points_wallet.R")
  if (file.exists(pts_src) && !exists("ynow_points_on_login", mode = "function")) {
    sys.source(pts_src, envir = environment())
  }
  if (exists("ynow_points_reset_memory", mode = "function")) {
    tryCatch(ynow_points_reset_memory(), error = function(e) NULL)
  }
  r <- ynow_auth_accept_social_user(list(
    email = "pts.gift@example.com",
    provider = "google",
    provider_uid = "sub-gift"
  ))
  uid <- as.character(r$user$id %||% "")[1]
  pts <- if (nzchar(uid) && exists("ynow_points_on_login", mode = "function")) {
    ynow_points_on_login(uid)
  } else {
    NULL
  }
  bals <- if (nzchar(uid) && exists("ynow_points_balances", mode = "function")) {
    ynow_points_balances(uid)
  } else {
    list(fyn = NA_integer_)
  }
  isTRUE(r$ok) && nzchar(uid) && isTRUE(pts$gift_granted) &&
    isTRUE(as.integer(bals$fyn)[1] >= 100L)
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
    grepl("ynow-auth-social--linkedin", auth_src, fixed = TRUE) &&
    grepl("flex-direction:row", auth_src, fixed = TRUE) &&
    grepl("ynow-auth-email-submit", auth_src, fixed = TRUE) &&
    !grepl("ynow-auth-social--apple", auth_src, fixed = TRUE)
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
check("sign-in and register share one row", {
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  h <- paste(as.character(ynow_auth_links_ui("sidebar")), collapse = "")
  home <- paste(as.character(ynow_auth_home_ui()), collapse = "")
  gate <- paste(as.character(ynow_member_gate_ui(
    "g", "t", "b", "btn_pair", feature = "lab"
  )), collapse = "")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
  grepl("ynow-auth-pair", h, fixed = TRUE) &&
    grepl("ynow_auth_register_open", h, fixed = TRUE) &&
    grepl("ynow-auth-register-open", home, fixed = TRUE) &&
    grepl("ynow_auth_register_open_home", home, fixed = TRUE) &&
    grepl("ynow-auth-pair--gate", gate, fixed = TRUE) &&
    grepl("btn_pair_register", gate, fixed = TRUE) &&
    grepl("ynow_auth_try_register", auth_src, fixed = TRUE) &&
    grepl("ynow-auth-pair--modal", auth_src, fixed = TRUE) &&
    grepl("height:28px", auth_src, fixed = TRUE) &&
    grepl("height:40px", auth_src, fixed = TRUE) &&
    grepl("height:46px", auth_src, fixed = TRUE)
})
check("home Sign in · Register are plain text links not buttons", {
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "1")
  home <- paste(as.character(ynow_auth_home_ui()), collapse = "")
  Sys.setenv(YNOW_ENABLE_ACCOUNT_LOGIN = "0")
  grepl("ynow-auth-pair--home-text", home, fixed = TRUE) &&
    grepl("ynow-auth-pair-sep", home, fixed = TRUE) &&
    !grepl("ynow-auth-btn", home, fixed = TRUE) &&
    grepl("background:transparent !important; color:#0b57d0", auth_src, fixed = TRUE) &&
    grepl("text-decoration:underline !important; text-underline-offset:2px", auth_src, fixed = TRUE) &&
    !grepl(
      "ynow-auth-links--home .ynow-auth-pair a.ynow-auth-register-open \\{[\\s\\S]*?background:#0b57d0",
      auth_src,
      perl = TRUE
    )
})
check("home legal stack order: auth → disclaimer → About", {
  legal <- regmatches(
    ui,
    regexpr('class = "ynow-home-legal"[\\s\\S]*?tags\\$script', ui, perl = TRUE)
  )
  if (!length(legal) || !nzchar(legal[[1]])) return(FALSE)
  block <- legal[[1]]
  i_auth <- regexpr("ynow_auth_home_ui", block, fixed = TRUE)[1]
  i_disc <- regexpr('id = "ynow_home_legal"', block, fixed = TRUE)[1]
  i_about <- regexpr('id = "ynow_home_legal_about"', block, fixed = TRUE)[1]
  i_auth > 0 && i_disc > 0 && i_about > 0 &&
    i_auth < i_disc && i_disc < i_about
})
check("home card titles are centered", {
  k <- regexpr(".ynow-home-card-k \\{", ui)
  ok <- k[1] > 0
  if (!ok) return(FALSE)
  tail <- substr(ui, k[1], k[1] + 220)
  grepl("text-align: center;", tail, fixed = TRUE)
})
check("home card descriptions are centered", {
  d <- regexpr(".ynow-home-card-d \\{", ui)
  ok <- d[1] > 0
  if (!ok) return(FALSE)
  tail <- substr(ui, d[1], d[1] + 220)
  grepl("text-align: center;", tail, fixed = TRUE)
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
      grepl("ynow_auth_linkedin", html, fixed = TRUE) &&
      !grepl("ynow_auth_apple", html, fixed = TRUE) &&
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
