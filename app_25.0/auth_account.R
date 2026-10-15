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

#' TRUE only when account login is explicitly enabled.
ynow_auth_enabled <- function() {
  v <- tolower(trimws(Sys.getenv("YNOW_ENABLE_ACCOUNT_LOGIN", unset = "0")))
  v %in% c("1", "true", "yes", "on")
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

#' Header Sign-in / account chip — rendered only when auth is enabled.
ynow_auth_header_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tags$li(
    id = "ynow-auth-header",
    class = "dropdown ynow-auth-header",
    tags$div(
      class = "ynow-auth-stack",
      role = "group",
      `aria-label` = "Account",
      actionLink(
        inputId = "ynow_auth_open",
        label = tags$span(id = "ynow_auth_header_label", "Sign in"),
        class = "ynow-auth-btn"
      ),
      tags$span(
        id = "ynow_auth_user_chip",
        class = "ynow-auth-user-chip",
        style = "display:none;",
        ""
      ),
      actionLink(
        inputId = "ynow_auth_sign_out",
        label = tags$span(id = "ynow_auth_sign_out_label", "Sign out"),
        class = "ynow-auth-btn ynow-auth-signout",
        style = "display:none;"
      )
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

#' Minimal CSS for the header auth chip (harmless when auth is off / unused).
ynow_auth_css <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tags$style(HTML(paste0(
    ".main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header {",
    "  display:flex; align-items:center; height:100%; list-style:none; margin:0; padding:0 8px;",
    "}",
    ".ynow-auth-stack { display:flex; align-items:center; gap:8px; }",
    ".ynow-auth-btn { color:#f5f5f5 !important; font-size:12px; font-weight:600; }",
    ".ynow-auth-btn:hover { color:#fff !important; text-decoration:underline; }",
    ".ynow-auth-user-chip { color:#ddd; font-size:11px; max-width:140px; overflow:hidden;",
    "  text-overflow:ellipsis; white-space:nowrap; }"
  )))
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
      session$sendCustomMessage(
        "ynowAuthState",
        list(
          logged_in = TRUE,
          email = res$user$email %||% "",
          label_sign_in = .str("auth_sign_in", "Sign in"),
          label_sign_out = .str("auth_sign_out", "Sign out")
        )
      )
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
    account_user(NULL)
    session$sendCustomMessage(
      "ynowAuthState",
      list(
        logged_in = FALSE,
        email = "",
        label_sign_in = .str("auth_sign_in", "Sign in"),
        label_sign_out = .str("auth_sign_out", "Sign out")
      )
    )
  }, ignoreInit = TRUE)

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
        cancel = tryCatch(ui_str("auth_cancel", loc), error = function(e) "Cancel")
      )
    )
  })

  list(
    enabled = ynow_auth_enabled(),
    user = account_user,
    is_logged_in = reactive({ !is.null(account_user()) }),
    is_guest = reactive({ is.null(account_user()) })
  )
}
