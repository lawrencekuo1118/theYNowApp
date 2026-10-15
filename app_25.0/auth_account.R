# ==========================================
# Account login (designed; off by default)
# ==========================================
# Enable only when YNOW_ENABLE_ACCOUNT_LOGIN is 1/true/yes/on.
# Until then the app stays fully guest-accessible and no login chrome is shown.
#
# Optional demo credentials (used only when enabled):
#   YNOW_AUTH_DEMO_EMAIL / YNOW_AUTH_DEMO_PASSWORD
# Never ship real secrets in the repo; leave demo env unset on production
# until a real identity backend is wired.

#' TRUE when account login is enabled (default ON).
#' Set YNOW_ENABLE_ACCOUNT_LOGIN=0/false/off to disable chrome + gates.
ynow_auth_enabled <- function() {
  v <- tolower(trimws(Sys.getenv("YNOW_ENABLE_ACCOUNT_LOGIN", unset = "1")))
  if (!nzchar(v)) return(TRUE)
  !(v %in% c("0", "false", "no", "off"))
}

#' Drop NULL UI children (shinydashboard::dashboardHeader tagAssert rejects NULL).
#' Prefer omitting disabled auth chrome over placeholder tags.
ynow_compact_ui <- function(...) {
  Filter(Negate(is.null), list(...))
}

#' Normalize email for compare (trim + lower).
ynow_auth_norm_email <- function(email) {
  tolower(trimws(as.character(email %||% "")[1]))
}

#' Demo credential lookup — env-only stub; returns NULL when unset / incomplete.
ynow_auth_demo_credentials <- function() {
  email <- ynow_auth_norm_email(Sys.getenv("YNOW_AUTH_DEMO_EMAIL", unset = ""))
  pass <- as.character(Sys.getenv("YNOW_AUTH_DEMO_PASSWORD", unset = ""))[1]
  if (!nzchar(email) || !nzchar(pass)) return(NULL)
  list(email = email, password = pass)
}

#' Constant-time-ish password compare for the demo stub (not a crypto library).
ynow_auth_password_ok <- function(provided, expected) {
  a <- as.character(provided %||% "")[1]
  b <- as.character(expected %||% "")[1]
  if (!nzchar(b)) return(FALSE)
  identical(a, b)
}

#' Validate credentials against the demo store (when enabled).
#' @return list(ok=, user=, message_key=) — user is NULL on failure.
ynow_auth_try_login <- function(email, password) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled"))
  }
  em <- ynow_auth_norm_email(email)
  pw <- as.character(password %||% "")[1]
  if (!nzchar(em) || !nzchar(pw)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_missing"))
  }
  demo <- ynow_auth_demo_credentials()
  if (is.null(demo)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_no_backend"))
  }
  if (!identical(em, demo$email) || !ynow_auth_password_ok(pw, demo$password)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_invalid"))
  }
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = em,
      provider = "demo_env",
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_ok"
  )
}

#' Sign-in / Sign-out text links under the ZH/EN stack (right-aligned).
#' Returns a div for nesting inside the lang header column — not a navbar <li>.
ynow_auth_header_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tags$div(
    id = "ynow-auth-header",
    class = "ynow-auth-under-lang",
    role = "group",
    `aria-label` = "Account",
    actionLink(
      inputId = "ynow_auth_open",
      label = tags$span(id = "ynow_auth_header_label", "Sign in"),
      class = "ynow-auth-link"
    ),
    tags$span(
      id = "ynow_auth_user_chip",
      class = "ynow-auth-user-chip",
      style = "display:none;",
      title = "",
      ""
    ),
    actionLink(
      inputId = "ynow_auth_sign_out",
      label = tags$span(id = "ynow_auth_sign_out_label", "Sign out"),
      class = "ynow-auth-link ynow-auth-signout",
      style = "display:none;"
    )
  )
}

#' Login modal markup — only when auth is enabled (otherwise NULL).
ynow_auth_modal_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  shinyBS::bsModal(
    id = "ynow_auth_modal",
    title = tags$span(id = "ynow_auth_modal_title", "Account sign-in"),
    trigger = NULL,
    size = "small",
    tags$p(
      id = "ynow_auth_modal_help",
      class = "help-block",
      "Sign in with your account email and password."
    ),
    textInput(
      "ynow_auth_email",
      label = tags$span(id = "ynow_auth_email_label", "Email"),
      value = "",
      placeholder = "you@example.com"
    ),
    passwordInput(
      "ynow_auth_password",
      label = tags$span(id = "ynow_auth_password_label", "Password"),
      value = ""
    ),
    uiOutput("ynow_auth_status"),
    footer = tagList(
      actionButton(
        "ynow_auth_submit",
        label = tags$span(id = "ynow_auth_submit_label", "Sign in"),
        class = "btn-primary"
      ),
      tags$button(
        type = "button",
        class = "btn btn-default",
        `data-dismiss` = "modal",
        tags$span(id = "ynow_auth_cancel_label", "Cancel")
      )
    )
  )
}

#' TRUE when a Full-mode session may require sign-in for gated surfaces.
#' Lite never requires an account — always returns FALSE for Lite callers.
ynow_auth_requires_login <- function(lite = FALSE) {
  if (isTRUE(lite)) return(FALSE)
  isTRUE(ynow_auth_enabled())
}

#' Account email for the current session user (or "").
ynow_auth_session_email <- function(auth) {
  if (is.null(auth) || !is.list(auth) || is.null(auth$user)) return("")
  u <- tryCatch(auth$user(), error = function(e) NULL)
  if (is.null(u)) return("")
  ynow_auth_norm_email(u$email %||% "")
}

#' Minimal CSS for auth chrome + maintenance nav + Lite exemptions.
ynow_auth_css <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  # Lang column: ZH/EN on top; Sign in / Sign out text links below, right-aligned.
  # Email chip stays hidden in header (account email lives in Maintenance).
  tags$style(HTML(paste0(
    ".ynow-lang-col {",
    "  display:flex; flex-direction:column; align-items:flex-end; justify-content:center;",
    "  gap:2px; line-height:1.15;",
    "}",
    ".ynow-auth-under-lang {",
    "  display:flex; flex-direction:column; align-items:flex-end; justify-content:center;",
    "  margin:0; padding:0; max-width:100%;",
    "}",
    ".ynow-auth-under-lang .ynow-auth-link,",
    ".ynow-auth-under-lang a.ynow-auth-link {",
    "  color:rgba(255,255,255,0.88) !important;",
    "  font-size:11px; font-weight:500; line-height:1.2;",
    "  padding:0 !important; margin:0 !important;",
    "  text-decoration:none !important; background:transparent !important;",
    "  border:0 !important; box-shadow:none !important;",
    "  white-space:nowrap; text-align:right;",
    "}",
    ".ynow-auth-under-lang .ynow-auth-link:hover,",
    ".ynow-auth-under-lang a.ynow-auth-link:hover {",
    "  color:#fff !important; text-decoration:underline !important;",
    "}",
    ".ynow-auth-under-lang .ynow-auth-user-chip { display:none !important; }",
    "body.ynow-lite .ynow-auth-under-lang { display:none !important; }",
    "body.ynow-lite .sidebar-menu a[data-value=\"account_admin\"],",
    "body.ynow-lite .sidebar-menu li:has(> a[data-value=\"account_admin\"]) {",
    "  display:none !important;",
    "}",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value=\"account_admin\"],",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li:has(> a[data-value=\"account_admin\"]) {",
    "  display:none !important;",
    "}",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) #ynow_pa_body { display:none !important; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) #ynow_pa_guest_gate { display:block !important; }",
    "#ynow_pa_guest_gate { display:none; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-alert-bell {",
    "  opacity:0.35; pointer-events:none; cursor:not-allowed;",
    "}",
    ".ynow-admin-panel { max-width:960px; margin:12px auto; }",
    ".ynow-admin-panel .ynow-admin-meta { color:#555; font-size:13px; line-height:1.5; }",
    "body.ynow-authed #pa_email, body.ynow-authed #pa_quick_email {",
    "  background:#f5f5f5;",
    "}"
  )))
}

#' Sidebar menu item for Maintenance (Full + signed-in; CSS hides otherwise).
ynow_admin_menu_item <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  menuItem(
    text = tags$span(id = "ynow_menu_admin", "Maintenance"),
    tabName = "account_admin",
    icon = icon("toolbox")
  )
}

#' Tab shell for maintenance backend (body filled by server).
ynow_admin_tab_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tabItem(
    tabName = "account_admin",
    uiOutput("ynow_admin_panel")
  )
}

#' Build maintenance panel UI for a signed-in user.
ynow_admin_panel_ui <- function(user, locale = "zh-TW", build_version = "") {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  email <- ynow_auth_norm_email(user$email %||% "")
  at <- as.character(user$logged_in_at %||% "")[1]
  tags$div(
    class = "ynow-admin-panel",
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = tags$span(id = "ynow_admin_box_title", .lab("auth_admin_title", "Maintenance")),
        tags$p(
          class = "ynow-admin-meta",
          tags$span(id = "ynow_admin_intro", .lab(
            "auth_admin_intro",
            "Signed-in maintenance workspace. Lite mode never requires an account."
          ))
        ),
        tags$hr(),
        tags$p(
          tags$b(id = "ynow_admin_account_label", .lab("auth_admin_account", "Account")),
          ": ",
          tags$code(if (nzchar(email)) email else "—")
        ),
        tags$p(
          tags$b(id = "ynow_admin_since_label", .lab("auth_admin_since", "Signed in at")),
          ": ",
          if (nzchar(at)) at else "—"
        ),
        tags$p(
          tags$b(id = "ynow_admin_build_label", .lab("auth_admin_build", "Display version")),
          ": ",
          tags$code(if (nzchar(build_version)) build_version else "—")
        ),
        tags$hr(),
        tags$h4(id = "ynow_admin_tools_h", .lab("auth_admin_tools", "Tools")),
        tags$ul(
          tags$li(id = "ynow_admin_tool_alerts", .lab(
            "auth_admin_tool_alerts",
            "Value Alerts: email notifications default to this account mailbox."
          )),
          tags$li(id = "ynow_admin_tool_lite", .lab(
            "auth_admin_tool_lite",
            "Lite mode stays fully usable without sign-in (no maintenance nav)."
          ))
        ),
        actionButton(
          "ynow_admin_goto_alerts",
          label = tags$span(id = "ynow_admin_goto_alerts_label", .lab("auth_admin_goto_alerts", "Open Value Alerts")),
          class = "btn-default"
        ),
        actionButton(
          "ynow_auth_sign_out_admin",
          label = tags$span(id = "ynow_admin_sign_out_label", .lab("auth_sign_out", "Sign out")),
          class = "btn-warning"
        )
      )
    )
  )
}

#' Guest gate copy for Full-mode Value Alerts when auth is on (CSS-hidden otherwise).
ynow_pa_guest_gate_ui <- function() {
  tags$div(
    id = "ynow_pa_guest_gate",
    class = "box box-warning",
    style = "display:none;",
    tags$div(
      class = "box-header with-border",
      tags$h3(
        class = "box-title",
        tags$span(id = "ynow_pa_gate_title", "Sign in required")
      )
    ),
    tags$div(
      class = "box-body",
      tags$p(id = "ynow_pa_gate_body", paste0(
        "Value Alerts in Full mode require an account. ",
        "Lite mode does not require sign-in and remains fully usable."
      )),
      actionButton(
        "ynow_pa_gate_signin",
        label = tags$span(id = "ynow_pa_gate_signin_label", "Sign in"),
        class = "btn-primary"
      )
    )
  )
}

#' Server: session account state. Always registers; UI/actions no-op when disabled.
#' @return list(enabled, user, is_logged_in, is_guest)
ynow_auth_server <- function(input, output, session, ui_locale = NULL) {
  account_user <- reactiveVal(NULL)

  .loc <- function() {
    if (is.function(ui_locale)) {
      tryCatch(normalize_ui_locale(ui_locale()), error = function(e) "zh-TW")
    } else {
      "zh-TW"
    }
  }
  .str <- function(key, fallback = key) {
    tryCatch(ui_str(key, .loc()), error = function(e) fallback)
  }

  output$ynow_auth_status <- renderUI({
    if (!isTRUE(ynow_auth_enabled())) return(NULL)
    NULL
  })

  .push_auth_state <- function(logged_in = FALSE, email = "") {
    session$sendCustomMessage(
      "ynowAuthState",
      list(
        logged_in = isTRUE(logged_in),
        email = as.character(email %||% "")[1],
        label_sign_in = .str("auth_sign_in", "Sign in"),
        label_sign_out = .str("auth_sign_out", "Sign out")
      )
    )
  }

  .sign_out <- function() {
    account_user(NULL)
    .push_auth_state(FALSE, "")
  }

  observeEvent(input$ynow_auth_open, {
    if (!isTRUE(ynow_auth_enabled())) return()
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_submit, {
    if (!isTRUE(ynow_auth_enabled())) return()
    res <- ynow_auth_try_login(input$ynow_auth_email, input$ynow_auth_password)
    if (isTRUE(res$ok)) {
      account_user(res$user)
      output$ynow_auth_status <- renderUI({
        tags$p(class = "text-success", .str("auth_msg_ok", "Signed in."))
      })
      .push_auth_state(TRUE, res$user$email %||% "")
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_invalid")[1]
      output$ynow_auth_status <- renderUI({
        tags$p(class = "text-danger", .str(mk, "Sign-in failed."))
      })
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_sign_out, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_sign_out_admin, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_pa_gate_signin, {
    if (!isTRUE(ynow_auth_enabled())) return()
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_admin_goto_alerts, {
    tryCatch(
      updateTabItems(session, "sidebar_tabs", "price_alerts"),
      error = function(e) {
        tryCatch(
          session$sendInputMessage("sidebar_tabs", list(value = "price_alerts")),
          error = function(e2) NULL
        )
      }
    )
  }, ignoreInit = TRUE)

  # Mark body as auth-feature so CSS gates Full-only surfaces; Lite stays exempt.
  if (isTRUE(ynow_auth_enabled())) {
    session$onFlushed(function() {
      session$sendCustomMessage("ynowAuthFeature", list(on = TRUE))
    }, once = TRUE)
  }

  # Push locale labels when language changes (only matters when chrome exists).
  observe({
    if (!isTRUE(ynow_auth_enabled())) return()
    loc <- .loc()
    session$sendCustomMessage(
      "ynowAuthLocale",
      list(
        sign_in = tryCatch(ui_str("auth_sign_in", loc), error = function(e) "Sign in"),
        sign_out = tryCatch(ui_str("auth_sign_out", loc), error = function(e) "Sign out"),
        modal_title = tryCatch(ui_str("auth_modal_title", loc), error = function(e) "Account sign-in"),
        modal_help = tryCatch(ui_str("auth_modal_help", loc), error = function(e) ""),
        email_label = tryCatch(ui_str("auth_email_label", loc), error = function(e) "Email"),
        password_label = tryCatch(ui_str("auth_password_label", loc), error = function(e) "Password"),
        submit = tryCatch(ui_str("auth_submit", loc), error = function(e) "Sign in"),
        cancel = tryCatch(ui_str("auth_cancel", loc), error = function(e) "Cancel"),
        admin_title = tryCatch(ui_str("auth_admin_title", loc), error = function(e) "Maintenance"),
        admin_intro = tryCatch(ui_str("auth_admin_intro", loc), error = function(e) ""),
        pa_gate_title = tryCatch(ui_str("auth_pa_gate_title", loc), error = function(e) "Sign in required"),
        pa_gate_body = tryCatch(ui_str("auth_pa_gate_body", loc), error = function(e) ""),
        menu_admin = tryCatch(ui_str("menu_admin", loc), error = function(e) "Maintenance")
      )
    )
  })

  list(
    enabled = ynow_auth_enabled(),
    user = account_user,
    is_logged_in = reactive({ !is.null(account_user()) }),
    is_guest = reactive({ is.null(account_user()) }),
    sign_out = .sign_out,
    push_auth_state = .push_auth_state
  )
}
