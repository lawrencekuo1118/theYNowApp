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

#' Stable in-process user id when Neon is not configured.
#' Memory points wallet keys on this id so first-gift / daily check-in still run.
ynow_auth_memory_user_id <- function(email) {
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em)) return("")
  hx <- tryCatch(
    digest::digest(paste0("ynow-mem:", em), algo = "sha256", serialize = FALSE),
    error = function(e) {
      tryCatch(
        paste(as.character(openssl::sha256(charToRaw(paste0("ynow-mem:", em)))), collapse = ""),
        error = function(e2) gsub("[^a-z0-9]", "", em)
      )
    }
  )
  paste0("mem:", substr(as.character(hx)[1], 1L, 32L))
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

#' Validate credentials: Neon users table first, then demo env fallback.
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
  # Preferred: account Postgres table via YNOW_DB_* (own users, same Email modal).
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_try_login", mode = "function")) {
    return(ynow_db_try_login(em, pw))
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
      id = ynow_auth_memory_user_id(em),
      email = em,
      display_name = em,
      provider = "demo_env",
      role = "admin",
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_ok"
  )
}

#' Register via Neon when configured; otherwise not open.
#' New Email accounts are role = "user" immediately — no admin approval.
ynow_auth_try_register <- function(email, password, display_name = NULL) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled"))
  }
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_create_user", mode = "function")) {
    return(ynow_db_create_user(email, password, display_name = display_name, role = "user"))
  }
  list(ok = FALSE, user = NULL, message_key = "auth_msg_register_soon")
}

#' Accept a Google / Facebook (or Supabase) profile as a self-serve account.
#' Persists to Neon when configured; otherwise still signs the session in as
#' role = "user" so end users are never blocked waiting for admin provisioning.
#' @param oauth_user list from ynow_oauth_complete()$user
#' @return list(ok=, user=, message_key=)
ynow_auth_accept_social_user <- function(oauth_user) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled"))
  }
  if (!is.list(oauth_user)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  em <- ynow_auth_norm_email(oauth_user$email %||% "")
  if (!nzchar(em)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_no_email"))
  }
  dn <- as.character(oauth_user$display_name %||% em)[1]
  prov <- as.character(oauth_user$provider %||% "oauth")[1]
  puid <- as.character(oauth_user$provider_uid %||% "")[1]

  db_ready <- exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready())

  if (db_ready && exists("ynow_db_upsert_oauth_user", mode = "function")) {
    return(ynow_db_upsert_oauth_user(
      email = em,
      display_name = dn,
      provider = prov,
      provider_uid = puid
    ))
  }

  # No Neon yet: still allow self-serve session (not an admin-gated whitelist).
  # Assign a stable mem: id so FYN/PYN on_login (gift + daily check-in) can run
  # against the in-process memory wallet until YNOW_DB_* is configured.
  list(
    ok = TRUE,
    user = list(
      id = ynow_auth_memory_user_id(em),
      email = em,
      display_name = dn,
      provider = prov,
      provider_uid = puid,
      role = "user",
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_ok"
  )
}

#' Sign-in / Sign-out text links for a placement (sidebar date row or Home legal).
#' @param placement "sidebar" | "home"
#' @return shiny tag or NULL when auth disabled
ynow_auth_links_ui <- function(placement = c("sidebar", "home")) {
  placement <- match.arg(placement)
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  # Unique input ids per placement (Shiny forbids duplicate ids).
  open_id <- if (identical(placement, "sidebar")) "ynow_auth_open" else "ynow_auth_open_home"
  out_id <- if (identical(placement, "sidebar")) "ynow_auth_sign_out" else "ynow_auth_sign_out_home"
  open_lbl <- if (identical(placement, "sidebar")) "ynow_auth_header_label" else "ynow_auth_home_label"
  out_lbl <- if (identical(placement, "sidebar")) {
    "ynow_auth_sign_out_label"
  } else {
    "ynow_auth_sign_out_home_label"
  }
  wrap_id <- if (identical(placement, "sidebar")) "ynow-auth-sidebar" else "ynow-auth-home"
  wrap_class <- if (identical(placement, "sidebar")) {
    "ynow-auth-links ynow-auth-links--sidebar"
  } else {
    "ynow-auth-links ynow-auth-links--home"
  }
  tags$div(
    id = wrap_id,
    class = wrap_class,
    role = "group",
    `aria-label` = "Account",
    actionLink(
      inputId = open_id,
      label = tags$span(id = open_lbl, class = "ynow-auth-open-label", "Sign in"),
      class = "ynow-auth-link ynow-auth-open"
    ),
    tags$span(
      class = "ynow-auth-user-chip",
      style = "display:none;",
      title = "",
      ""
    ),
    actionLink(
      inputId = out_id,
      label = tags$span(id = out_lbl, class = "ynow-auth-signout-label", "Sign out"),
      class = "ynow-auth-link ynow-auth-signout",
      style = "display:none;"
    )
  )
}

#' @deprecated Prefer ynow_auth_links_ui("sidebar"); kept for tests / callers.
ynow_auth_header_ui <- function() {
  ynow_auth_links_ui("sidebar")
}

#' Home legal-row companion (left of About · Privacy).
ynow_auth_home_ui <- function() {
  ynow_auth_links_ui("home")
}

#' Inline Google "G" mark for social button (decorative).
.ynow_auth_google_icon <- function() {
  HTML(paste0(
    '<svg class="ynow-auth-social-icon" viewBox="0 0 48 48" width="18" height="18" aria-hidden="true" focusable="false">',
    '<path fill="#EA4335" d="M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z"/>',
    '<path fill="#4285F4" d="M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z"/>',
    '<path fill="#FBBC05" d="M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z"/>',
    '<path fill="#34A853" d="M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z"/>',
    '</svg>'
  ))
}

#' Inline "f" mark for Facebook-style social button (decorative).
.ynow_auth_facebook_icon <- function() {
  HTML(paste0(
    '<svg class="ynow-auth-social-icon ynow-auth-social-icon--fb" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">',
    '<path fill="currentColor" d="M22 12.07C22 6.48 17.52 2 11.93 2S1.86 6.48 1.86 12.07c0 5.02 3.66 9.18 8.44 9.93v-7.02H7.9v-2.91h2.4V9.84c0-2.37 1.4-3.68 3.56-3.68 1.03 0 2.11.18 2.11.18v2.33h-1.19c-1.17 0-1.54.73-1.54 1.48v1.78h2.62l-.42 2.91h-2.2V22c4.78-.75 8.44-4.91 8.44-9.93z"/>',
    '</svg>'
  ))
}

#' LinkedIn "in" mark for social button (decorative; white on LinkedIn blue).
.ynow_auth_linkedin_icon <- function() {
  HTML(paste0(
    '<svg class="ynow-auth-social-icon ynow-auth-social-icon--linkedin" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">',
    '<path fill="currentColor" d="M20.45 20.45h-3.56v-5.57c0-1.33-.02-3.04-1.85-3.04-1.85 0-2.14 1.45-2.14 2.94v5.67H9.35V9h3.41v1.56h.05c.48-.9 1.64-1.85 3.37-1.85 3.6 0 4.27 2.37 4.27 5.46v6.28zM5.34 7.43a2.06 2.06 0 1 1 0-4.12 2.06 2.06 0 0 1 0 4.12zM7.12 20.45H3.56V9h3.56v11.45zM22.23 0H1.77C.79 0 0 .77 0 1.73v20.54C0 23.23.79 24 1.77 24h20.46c.98 0 1.77-.77 1.77-1.73V1.73C24 .77 23.21 0 22.23 0z"/>',
    '</svg>'
  ))
}

#' Login modal — StatementDog-style card; YNow ink / logo-blue / wash palette.
#' Social / register / forgot are UI stubs until OAuth + free DB are wired.
ynow_auth_modal_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  shinyBS::bsModal(
    id = "ynow_auth_modal",
    title = tags$span(id = "ynow_auth_modal_title", "Sign in to The YNow App"),
    trigger = NULL,
    size = "small",
    tags$div(
      class = "ynow-auth-card",
      # Hidden help kept for locale push / a11y (visually collapsed).
      tags$p(
        id = "ynow_auth_modal_help",
        class = "ynow-auth-help-sr",
        "Sign in with your account email and password."
      ),
      actionButton(
        inputId = "ynow_auth_google",
        label = tagList(
          .ynow_auth_google_icon(),
          tags$span(id = "ynow_auth_google_label", "Continue with Google")
        ),
        class = "ynow-auth-social ynow-auth-social--google"
      ),
      actionButton(
        inputId = "ynow_auth_facebook",
        label = tagList(
          .ynow_auth_facebook_icon(),
          tags$span(id = "ynow_auth_facebook_label", "Continue with Facebook")
        ),
        class = "ynow-auth-social ynow-auth-social--facebook"
      ),
      actionButton(
        inputId = "ynow_auth_linkedin",
        label = tagList(
          .ynow_auth_linkedin_icon(),
          tags$span(id = "ynow_auth_linkedin_label", "Continue with LinkedIn")
        ),
        class = "ynow-auth-social ynow-auth-social--linkedin"
      ),
      tags$div(
        class = "ynow-auth-divider",
        tags$span(id = "ynow_auth_or_email", "Or sign in with Email")
      ),
      tags$div(
        class = "ynow-auth-field",
        tags$label(
          `for` = "ynow_auth_email",
          tags$span(id = "ynow_auth_email_label", "Enter your Email account:")
        ),
        tags$input(
          id = "ynow_auth_email",
          type = "email",
          class = "form-control ynow-auth-input",
          value = "",
          autocomplete = "username",
          placeholder = "you@example.com"
        )
      ),
      tags$div(
        class = "ynow-auth-field",
        tags$label(
          `for` = "ynow_auth_password",
          tags$span(id = "ynow_auth_password_label", "Enter password:")
        ),
        tags$input(
          id = "ynow_auth_password",
          type = "password",
          class = "form-control ynow-auth-input",
          value = "",
          autocomplete = "current-password"
        ),
        tags$div(
          class = "ynow-auth-forgot-row",
          actionLink(
            "ynow_auth_forgot",
            label = tags$span(id = "ynow_auth_forgot_label", "Forgot password?"),
            class = "ynow-auth-text-link"
          )
        )
      ),
      uiOutput("ynow_auth_status"),
      actionButton(
        "ynow_auth_submit",
        label = tags$span(id = "ynow_auth_submit_label", "Email sign-in"),
        class = "ynow-auth-email-submit"
      ),
      tags$div(
        class = "ynow-auth-card-foot",
        actionLink(
          "ynow_auth_register",
          label = tags$span(id = "ynow_auth_register_label", "Register a new account"),
          class = "ynow-auth-text-link"
        ),
        tags$span(class = "ynow-auth-foot-dot", "·"),
        actionLink(
          "ynow_auth_resend",
          label = tags$span(
            id = "ynow_auth_resend_label",
            "Didn't receive the verification email"
          ),
          class = "ynow-auth-text-link"
        )
      )
    ),
    footer = tags$button(
      type = "button",
      class = "btn btn-default ynow-auth-cancel-btn",
      `data-dismiss` = "modal",
      tags$span(id = "ynow_auth_cancel_label", "Cancel")
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

#' Minimal CSS for auth chrome + StatementDog-style login card + Lite exemptions.
#' Palette: --ynow-ink / --ynow-logo-blue / --ynow-wash / --ynow-line (app tokens).
ynow_auth_css <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  # Sign-in lives in sidebar date row + Home legal row (not under ZH/EN).
  # Email chip stays hidden (account email lives in Maintenance).
  # Returns style + maintenance theme JS (page-scoped color picker).
  tagList(
  tags$style(HTML(paste0(
    ".ynow-sidebar-date-row {",
    "  display:flex; flex-direction:row; align-items:center; justify-content:flex-start;",
    "  gap:10px; flex-wrap:nowrap; width:100%; min-height:22px;",
    "  margin:0; padding:6px 4px 2px;",
    "}",
    ".ynow-sidebar-date-row .ynow-sidebar-date,",
    ".ynow-sidebar-date-row #today {",
    "  flex:0 0 auto; margin:0; padding:0; font-size:12px; line-height:1.2;",
    "  white-space:nowrap; color:inherit;",
    "}",
    # Mobile: FYN/PYN sit right of the login date in the sidebar (not top header).
    "#ynow-pts-sidebar.ynow-pts-sidebar {",
    "  display:none; flex:0 0 auto; margin:0; padding:0; min-width:0;",
    "}",
    "#ynow-pts-sidebar .ynow-pts-sidebar-banner {",
    "  display:flex; flex-direction:column; align-items:flex-start; justify-content:center;",
    "  gap:0; line-height:1.15; font-size:10px; font-weight:600;",
    "  color:rgba(255,255,255,0.92); white-space:nowrap;",
    "}",
    "#ynow-pts-sidebar .ynow-pts-sidebar-line {",
    "  display:flex; flex-direction:row; align-items:baseline; gap:3px;",
    "}",
    "#ynow-pts-sidebar .ynow-pts-sidebar-val {",
    "  font-variant-numeric:tabular-nums; font-weight:700;",
    "}",
    "@media (max-width:767px) {",
    "  #ynow-pts-sidebar.ynow-pts-sidebar:not(.ynow-pts-hidden) {",
    "    display:flex !important; align-items:center;",
    "  }",
    "  #ynow-pts-sidebar.ynow-pts-sidebar.ynow-pts-hidden {",
    "    display:none !important;",
    "  }",
    "}",
    ".ynow-auth-links {",
    "  display:inline-flex; flex-direction:row; align-items:center; gap:8px;",
    "  margin:0; padding:0; flex:0 0 auto;",
    "}",
    ".ynow-auth-links .ynow-auth-link,",
    ".ynow-auth-links a.ynow-auth-link {",
    "  padding:0 !important; margin:0 !important;",
    "  text-decoration:none !important; background:transparent !important;",
    "  border:0 !important; box-shadow:none !important;",
    "  white-space:nowrap; font-weight:500; line-height:1.2;",
    "}",
    ".ynow-auth-links--sidebar .ynow-auth-link,",
    ".ynow-auth-links--sidebar a.ynow-auth-link {",
    "  color:rgba(255,255,255,0.88) !important; font-size:12px;",
    "}",
    ".ynow-auth-links--sidebar .ynow-auth-link:hover,",
    ".ynow-auth-links--sidebar a.ynow-auth-link:hover {",
    "  color:#fff !important; text-decoration:underline !important;",
    "}",
    ".ynow-auth-links--home .ynow-auth-link,",
    ".ynow-auth-links--home a.ynow-auth-link {",
    "  color:#0b57d0 !important; font-size:12px;",
    "  text-decoration:underline !important; text-underline-offset:2px;",
    "}",
    ".ynow-auth-links--home .ynow-auth-link:hover,",
    ".ynow-auth-links--home a.ynow-auth-link:hover {",
    "  color:#0842a0 !important;",
    "}",
    ".ynow-auth-links .ynow-auth-user-chip { display:none !important; }",
    ".ynow-home-legal-links {",
    "  display:inline-flex; flex-direction:row; align-items:center;",
    "  justify-content:center; gap:14px; flex-wrap:wrap;",
    "}",
    "body.ynow-lite .ynow-auth-links { display:none !important; }",
    # Logged-in: never show Sign-in modal or hit Sign-in links (post-login race).
    "body.ynow-authed #ynow_auth_modal,",
    "body.ynow-authed #ynow_auth_modal.in,",
    "body.ynow-authed #ynow_auth_modal.show {",
    "  display:none !important; visibility:hidden !important;",
    "}",
    "body.ynow-authed .ynow-auth-open,",
    "body.ynow-authed a.ynow-auth-open {",
    "  display:none !important; pointer-events:none !important;",
    "}",
    "#ynow_auth_modal .modal-dialog { width:92%; max-width:420px; margin:40px auto; }",
    "#ynow_auth_modal .modal-content {",
    "  border:1px solid var(--ynow-line, #d0d0d0); border-radius:2px;",
    "  box-shadow:0 8px 28px rgba(26,26,26,0.12); background:#fff;",
    "}",
    "#ynow_auth_modal .modal-header {",
    "  border-bottom:0; padding:28px 28px 8px; background:#fff;",
    "}",
    "#ynow_auth_modal .modal-header .close {",
    "  margin-top:-8px; font-weight:300; color:var(--ynow-ink-soft, #333); opacity:0.55;",
    "}",
    "#ynow_auth_modal .modal-title,",
    "#ynow_auth_modal #ynow_auth_modal_title {",
    "  display:block; font-size:22px; font-weight:700; line-height:1.3;",
    "  color:var(--ynow-ink, #1a1a1a); letter-spacing:-0.01em;",
    "}",
    "#ynow_auth_modal .modal-body { padding:8px 28px 8px; background:#fff; }",
    "#ynow_auth_modal .modal-footer {",
    "  border-top:0; padding:0 28px 20px; text-align:center; background:#fff;",
    "}",
    "#ynow_auth_modal .ynow-auth-cancel-btn { display:none !important; }",
    ".ynow-auth-help-sr {",
    "  position:absolute; width:1px; height:1px; padding:0; margin:-1px;",
    "  overflow:hidden; clip:rect(0,0,0,0); border:0;",
    "}",
    ".ynow-auth-card { display:flex; flex-direction:column; gap:12px; }",
    # Logo + continue label on one row (override Bootstrap .btn).
    "#ynow_auth_modal button.ynow-auth-social.btn,",
    "#ynow_auth_modal .ynow-auth-social.btn,",
    ".ynow-auth-social.btn,",
    ".ynow-auth-social {",
    "  display:inline-flex !important; flex-direction:row !important;",
    "  flex-wrap:nowrap !important; align-items:center !important;",
    "  justify-content:center !important; gap:10px !important;",
    "  width:100%; height:44px; margin:0; padding:0 14px;",
    "  border-radius:2px; font-size:14px; font-weight:600; line-height:1;",
    "  white-space:nowrap !important; vertical-align:middle;",
    "  cursor:pointer; box-shadow:none !important; text-shadow:none;",
    "}",
    "#ynow_auth_modal .ynow-auth-social > .ynow-auth-social-icon,",
    "#ynow_auth_modal .ynow-auth-social > span,",
    ".ynow-auth-social > .ynow-auth-social-icon,",
    ".ynow-auth-social > span {",
    "  display:inline-flex !important; align-items:center !important;",
    "  flex:0 0 auto !important; margin:0 !important; float:none !important;",
    "  vertical-align:middle;",
    "}",
    ".ynow-auth-social--google {",
    "  background:#fff !important; color:var(--ynow-ink, #1a1a1a) !important;",
    "  border:1px solid var(--ynow-line, #d0d0d0) !important;",
    "}",
    ".ynow-auth-social--google:hover {",
    "  background:var(--ynow-wash, #f5f5f5) !important;",
    "  border-color:#b8b8b8 !important; color:var(--ynow-ink, #1a1a1a) !important;",
    "}",
    ".ynow-auth-social--facebook {",
    "  background:var(--ynow-logo-blue, #0C5484) !important;",
    "  border:1px solid var(--ynow-logo-blue, #0C5484) !important;",
    "  color:#fff !important;",
    "}",
    ".ynow-auth-social--facebook:hover {",
    "  background:#0a456c !important; border-color:#0a456c !important; color:#fff !important;",
    "}",
    ".ynow-auth-social--linkedin {",
    "  background:#0A66C2 !important; border:1px solid #0A66C2 !important; color:#fff !important;",
    "}",
    ".ynow-auth-social--linkedin:hover {",
    "  background:#004182 !important; border-color:#004182 !important; color:#fff !important;",
    "}",
    ".ynow-auth-social-icon { width:18px; height:18px; }",
    ".ynow-auth-social-icon--fb { color:#fff; }",
    ".ynow-auth-social-icon--linkedin { color:#fff; }",
    ".ynow-auth-divider {",
    "  display:flex; align-items:center; gap:12px; margin:4px 0 2px;",
    "  color:#888; font-size:12px;",
    "}",
    ".ynow-auth-divider::before, .ynow-auth-divider::after {",
    "  content:''; flex:1 1 auto; height:1px; background:var(--ynow-line, #d0d0d0);",
    "}",
    ".ynow-auth-divider span { white-space:nowrap; flex:0 0 auto; }",
    ".ynow-auth-field { margin:0; }",
    ".ynow-auth-field > label {",
    "  display:block; margin:0 0 6px; font-size:13px; font-weight:600;",
    "  color:var(--ynow-ink-soft, #333);",
    "}",
    ".ynow-auth-input.form-control,",
    "#ynow_auth_modal .ynow-auth-input {",
    "  height:42px; border-radius:2px; border:1px solid transparent;",
    "  background:var(--ynow-wash, #f5f5f5) !important;",
    "  box-shadow:none !important; color:var(--ynow-ink, #1a1a1a);",
    "}",
    "#ynow_auth_modal .ynow-auth-input:focus {",
    "  border-color:var(--ynow-logo-blue, #0C5484) !important;",
    "  background:#fff !important;",
    "}",
    ".ynow-auth-forgot-row { margin:6px 0 0; }",
    ".ynow-auth-text-link,",
    "#ynow_auth_modal a.ynow-auth-text-link {",
    "  color:var(--ynow-logo-blue, #0C5484) !important;",
    "  font-size:13px; font-weight:500; text-decoration:none !important;",
    "  padding:0 !important; background:transparent !important; border:0 !important;",
    "}",
    "#ynow_auth_modal a.ynow-auth-text-link:hover {",
    "  color:#0a456c !important; text-decoration:underline !important;",
    "}",
    ".ynow-auth-email-submit,",
    "#ynow_auth_modal .ynow-auth-email-submit.btn {",
    "  display:block; width:100%; height:46px; margin:4px 0 0; padding:0;",
    "  border-radius:2px; border:1px solid var(--ynow-ink, #1a1a1a) !important;",
    "  background:var(--ynow-ink, #1a1a1a) !important; color:#fff !important;",
    "  font-size:15px; font-weight:700; line-height:46px;",
    "  box-shadow:none !important; text-shadow:none;",
    "}",
    "#ynow_auth_modal .ynow-auth-email-submit.btn:hover,",
    "#ynow_auth_modal .ynow-auth-email-submit.btn:focus {",
    "  background:#000 !important; border-color:#000 !important; color:#fff !important;",
    "}",
    ".ynow-auth-card-foot {",
    "  display:flex; flex-wrap:wrap; align-items:center; justify-content:center;",
    "  gap:8px; margin:8px 0 4px; text-align:center;",
    "}",
    ".ynow-auth-foot-dot { color:#bbb; font-size:13px; }",
    "#ynow_auth_modal #ynow_auth_status .text-danger { color:#c0392b; font-size:13px; }",
    "#ynow_auth_modal #ynow_auth_status .text-success { color:var(--ynow-logo-green, #249C60); font-size:13px; }",
    "#ynow_auth_modal #ynow_auth_status .text-muted { color:#777; font-size:13px; }",
    "body.ynow-lite .sidebar-menu a[data-value=\"account_admin\"],",
    "body.ynow-lite .sidebar-menu li:has(> a[data-value=\"account_admin\"]),",
    "body.ynow-lite .sidebar-menu a[data-value=\"account_points\"],",
    "body.ynow-lite .sidebar-menu li:has(> a[data-value=\"account_points\"]),",
    "body.ynow-lite .sidebar-menu li.treeview:has(a[data-value=\"account_admin\"]),",
    "body.ynow-lite .sidebar-menu li.treeview:has(a[data-value=\"account_points\"]) {",
    "  display:none !important;",
    "}",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value=\"account_admin\"],",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li:has(> a[data-value=\"account_admin\"]),",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value=\"account_points\"],",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li:has(> a[data-value=\"account_points\"]),",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li.treeview:has(a[data-value=\"account_admin\"]),",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li.treeview:has(a[data-value=\"account_points\"]) {",
    "  display:none !important;",
    "}",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) #ynow_pa_body { display:none !important; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) #ynow_pa_guest_gate { display:block !important; }",
    "#ynow_pa_guest_gate { display:none; }",
    ".ynow-member-gate { display:none; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-member-gate { display:block !important; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-member-body { display:none !important; }",
    "body.ynow-auth-feature:not(.ynow-lite):not(.ynow-authed) .ynow-alert-bell {",
    "  opacity:0.35; pointer-events:none; cursor:not-allowed;",
    "}",
    # Account maintenance: flat block (Blue Chip ind-mcap style) + page-scoped themes
    ".ynow-admin-panel,",
    ".ynow-pts-ledger-panel { max-width:1100px; margin:12px auto; }",
    ".ynow-maint-shell {",
    "  --ynow-maint-accent: var(--ynow-logo-blue, #0C5484);",
    "  --ynow-maint-accent-2: var(--ynow-logo-green, #249C60);",
    "  --ynow-maint-accent-3: var(--ynow-logo-cyan, #1AA8B8);",
    "  --ynow-maint-ink: #1a1a1a;",
    "  --ynow-maint-muted: #666;",
    "  --ynow-maint-line: #e5e5e5;",
    "  --ynow-maint-wash: #f7faf8;",
    "  --ynow-maint-card-bg: #fafafa;",
    "  --ynow-maint-btn-fg: #fff;",
    "}",
    ".ynow-maint-shell[data-ynow-maint-theme=\"logo\"] {",
    "  --ynow-maint-accent: var(--ynow-logo-blue, #0C5484);",
    "  --ynow-maint-accent-2: var(--ynow-logo-green, #249C60);",
    "  --ynow-maint-accent-3: var(--ynow-logo-cyan, #1AA8B8);",
    "  --ynow-maint-wash: #f3f8f6;",
    "  --ynow-maint-card-bg: #f7fbf9;",
    "  --ynow-maint-line: #d5e5dc;",
    "}",
    ".ynow-maint-shell[data-ynow-maint-theme=\"mono\"] {",
    "  --ynow-maint-accent: #1a1a1a;",
    "  --ynow-maint-accent-2: #444;",
    "  --ynow-maint-accent-3: #666;",
    "  --ynow-maint-ink: #111;",
    "  --ynow-maint-muted: #666;",
    "  --ynow-maint-line: #ddd;",
    "  --ynow-maint-wash: #f5f5f5;",
    "  --ynow-maint-card-bg: #fafafa;",
    "}",
    ".ynow-maint-shell[data-ynow-maint-theme=\"ocean\"] {",
    "  --ynow-maint-accent: #0A4D8C;",
    "  --ynow-maint-accent-2: #1B7BB5;",
    "  --ynow-maint-accent-3: #3AA0C8;",
    "  --ynow-maint-wash: #f2f7fb;",
    "  --ynow-maint-card-bg: #f6fafc;",
    "  --ynow-maint-line: #d0e0ec;",
    "}",
    ".ynow-maint-shell[data-ynow-maint-theme=\"forest\"] {",
    "  --ynow-maint-accent: #1B5E3B;",
    "  --ynow-maint-accent-2: #2E8B57;",
    "  --ynow-maint-accent-3: #3FA06A;",
    "  --ynow-maint-wash: #f3f8f4;",
    "  --ynow-maint-card-bg: #f6fbf7;",
    "  --ynow-maint-line: #d2e5d8;",
    "}",
    ".ynow-maint-block {",
    "  margin: 0 0 16px 0;",
    "  padding: 12px 14px;",
    "  border: 1px solid var(--ynow-maint-line, #e5e5e5);",
    "  border-radius: 4px;",
    "  background: var(--ynow-maint-card-bg, #fafafa);",
    "  color: var(--ynow-maint-ink, #1a1a1a);",
    "}",
    ".ynow-maint-title-row {",
    "  display: flex;",
    "  align-items: center;",
    "  justify-content: space-between;",
    "  gap: 10px;",
    "  width: 100%;",
    "  margin: 0 0 6px 0;",
    "  flex-wrap: wrap;",
    "}",
    ".ynow-maint-title-left {",
    "  display: flex;",
    "  flex-wrap: wrap;",
    "  align-items: baseline;",
    "  gap: 8px;",
    "  flex: 1 1 auto;",
    "  min-width: 0;",
    "}",
    ".ynow-maint-title-left h4 {",
    "  margin: 0;",
    "  font-size: 15px;",
    "  font-weight: 600;",
    "  color: var(--ynow-maint-accent, #0C5484);",
    "}",
    ".ynow-maint-badge {",
    "  font-size: 11px;",
    "  color: var(--ynow-maint-muted, #888);",
    "}",
    ".ynow-maint-help {",
    "  color: var(--ynow-maint-muted, #888);",
    "  font-size: 12px;",
    "  line-height: 1.45;",
    "  margin: 0 0 10px 0;",
    "}",
    ".ynow-maint-theme-wrap {",
    "  display: flex;",
    "  align-items: center;",
    "  gap: 6px;",
    "  flex: 0 0 auto;",
    "}",
    ".ynow-maint-theme-label {",
    "  margin: 0;",
    "  font-size: 11.5px;",
    "  font-weight: 600;",
    "  color: var(--ynow-maint-muted, #666);",
    "  white-space: nowrap;",
    "}",
    ".ynow-maint-theme-select.form-control {",
    "  width: auto;",
    "  min-width: 132px;",
    "  height: 28px;",
    "  padding: 2px 8px;",
    "  font-size: 12px;",
    "  line-height: 1.35;",
    "  border-radius: 3px;",
    "  border: 1px solid var(--ynow-maint-line, #ccc);",
    "  background: #fff;",
    "  color: var(--ynow-maint-ink, #1a1a1a);",
    "  box-shadow: none;",
    "}",
    ".ynow-maint-meta-grid {",
    "  display: grid;",
    "  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));",
    "  gap: 8px 14px;",
    "  margin: 0 0 12px 0;",
    "  font-size: 13px;",
    "  line-height: 1.45;",
    "}",
    ".ynow-maint-meta-grid b { color: var(--ynow-maint-ink, #1a1a1a); }",
    ".ynow-maint-meta-grid code {",
    "  color: var(--ynow-maint-accent, #0C5484);",
    "  background: var(--ynow-maint-wash, #f3f8f6);",
    "  padding: 1px 5px;",
    "  border-radius: 2px;",
    "}",
    ".ynow-admin-panel .ynow-admin-meta,",
    ".ynow-pts-ledger-panel .ynow-admin-meta {",
    "  color: var(--ynow-maint-muted, #555);",
    "  font-size: 13px;",
    "  line-height: 1.5;",
    "}",
    ".ynow-maint-note {",
    "  color: var(--ynow-maint-muted, #666);",
    "  font-size: 12.5px;",
    "  line-height: 1.45;",
    "  margin: 0 0 8px 0;",
    "}",
    ".ynow-maint-section-h {",
    "  margin: 12px 0 6px;",
    "  font-size: 13.5px;",
    "  font-weight: 600;",
    "  color: var(--ynow-maint-accent-2, #249C60);",
    "}",
    ".ynow-maint-tools {",
    "  margin: 0 0 12px 1.1em;",
    "  padding: 0;",
    "  font-size: 13px;",
    "  color: var(--ynow-maint-ink, #1a1a1a);",
    "}",
    ".ynow-maint-tools li { margin: 0 0 4px; }",
    ".ynow-maint-actions {",
    "  display: flex;",
    "  flex-wrap: wrap;",
    "  gap: 8px;",
    "  margin: 10px 0 0;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-primary,",
    ".ynow-maint-shell .btn.btn-primary.ynow-maint-btn-primary {",
    "  background: var(--ynow-maint-accent, #0C5484) !important;",
    "  border: 1px solid var(--ynow-maint-accent, #0C5484) !important;",
    "  color: var(--ynow-maint-btn-fg, #fff) !important;",
    "  box-shadow: none !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-primary:hover {",
    "  filter: brightness(0.92);",
    "  color: #fff !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-default,",
    ".ynow-maint-shell .btn.btn-default.ynow-maint-btn-default {",
    "  background: #fff !important;",
    "  border: 1px solid var(--ynow-maint-accent-2, #249C60) !important;",
    "  color: var(--ynow-maint-accent-2, #249C60) !important;",
    "  box-shadow: none !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-default:hover {",
    "  background: var(--ynow-maint-wash, #f3f8f6) !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-warn,",
    ".ynow-maint-shell .btn.btn-warning.ynow-maint-btn-warn {",
    "  background: transparent !important;",
    "  border: 1px solid var(--ynow-maint-accent-3, #1AA8B8) !important;",
    "  color: var(--ynow-maint-accent-3, #1AA8B8) !important;",
    "  box-shadow: none !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-warn:hover {",
    "  background: var(--ynow-maint-wash, #f3f8f6) !important;",
    "}",
    ".ynow-maint-shell[data-ynow-maint-theme=\"mono\"] .btn.ynow-maint-btn-warn,",
    ".ynow-maint-shell[data-ynow-maint-theme=\"mono\"] .btn.btn-warning.ynow-maint-btn-warn {",
    "  border-color: #888 !important;",
    "  color: #444 !important;",
    "}",
    "#ynow_pts_ledger_wrap {",
    "  margin: 4px 0 8px;",
    "  border: 1px solid var(--ynow-maint-line, #e5e5e5);",
    "  border-radius: 3px;",
    "  background: #fff;",
    "  padding: 6px;",
    "  overflow-x: auto;",
    "}",
    "body.ynow-authed #pa_email, body.ynow-authed #pa_quick_email {",
    "  background:#f5f5f5;",
    "}"
  ))),
  tags$script(HTML(paste0(
    "(function(){",
    "  var KEY='ynow_maint_theme';",
    "  var ALLOWED={logo:1,mono:1,ocean:1,forest:1};",
    "  function norm(v){",
    "    v=String(v||'logo');",
    "    return ALLOWED[v]?v:'logo';",
    "  }",
    "  function read(){",
    "    try{return norm(localStorage.getItem(KEY));}catch(e){return 'logo';}",
    "  }",
    "  function write(v){",
    "    try{localStorage.setItem(KEY,norm(v));}catch(e){}",
    "  }",
    "  function apply(theme){",
    "    theme=norm(theme);",
    "    var shells=document.querySelectorAll('.ynow-maint-shell');",
    "    for(var i=0;i<shells.length;i++){",
    "      shells[i].setAttribute('data-ynow-maint-theme',theme);",
    "    }",
    "    var sels=document.querySelectorAll('select.ynow-maint-theme-select');",
    "    for(var j=0;j<sels.length;j++){",
    "      if(sels[j].value!==theme) sels[j].value=theme;",
    "    }",
    "  }",
    "  function onChange(ev){",
    "    var t=norm(ev&&ev.target&&ev.target.value);",
    "    write(t);",
    "    apply(t);",
    "  }",
    "  function bind(){",
    "    var sels=document.querySelectorAll('select.ynow-maint-theme-select');",
    "    for(var i=0;i<sels.length;i++){",
    "      if(sels[i].dataset.ynowMaintBound==='1') continue;",
    "      sels[i].dataset.ynowMaintBound='1';",
    "      sels[i].addEventListener('change',onChange);",
    "    }",
    "    apply(read());",
    "  }",
    "  if(document.readyState==='loading'){",
    "    document.addEventListener('DOMContentLoaded',bind);",
    "  } else { bind(); }",
    "  if(window.jQuery){",
    "    jQuery(document).on('shiny:value shiny:idle', function(){ setTimeout(bind,0); });",
    "  }",
    "  window.ynowMaintThemeApply=apply;",
    "})();"
  )))
  )
}

#' Sidebar: Account tree with Overview + Points ledger (Full + signed-in).
ynow_admin_menu_item <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  menuItem(
    text = tags$span(id = "ynow_menu_admin", "Account"),
    icon = icon("toolbox"),
    startExpanded = FALSE,
    menuSubItem(
      text = tags$span(id = "ynow_menu_admin_overview", "Overview"),
      tabName = "account_admin",
      icon = icon("user-cog")
    ),
    menuSubItem(
      text = tags$span(id = "ynow_menu_pts_ledger", "Points ledger"),
      tabName = "account_points",
      icon = icon("list-alt")
    )
  )
}

#' Tab shells for Account overview + Points ledger (bodies filled by server).
ynow_admin_tab_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tagList(
    tabItem(
      tabName = "account_admin",
      uiOutput("ynow_admin_panel")
    ),
    tabItem(
      tabName = "account_points",
      uiOutput("ynow_points_panel")
    )
  )
}

#' Theme picker for Account maintenance pages (client-side; localStorage).
#' Default = logo blue-green; alternates = mono / ocean / forest.
ynow_maint_theme_select_ui <- function(locale = "zh-TW") {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  tags$div(
    class = "ynow-maint-theme-wrap",
    tags$span(
      class = "ynow-maint-theme-label",
      .lab("auth_admin_theme", "Color theme")
    ),
    tags$select(
      class = "ynow-maint-theme-select form-control input-sm",
      `aria-label` = .lab("auth_admin_theme", "Color theme"),
      tags$option(
        value = "logo",
        selected = "selected",
        .lab("auth_admin_theme_logo", "Logo blue-green")
      ),
      tags$option(value = "mono", .lab("auth_admin_theme_mono", "Monochrome")),
      tags$option(value = "ocean", .lab("auth_admin_theme_ocean", "Ocean blue")),
      tags$option(value = "forest", .lab("auth_admin_theme_forest", "Forest green"))
    )
  )
}

#' Build Account overview panel (signed-in). Ledger lives on account_points tab.
#' Flat block layout (Blue Chip ind-mcap style) — no AdminLTE box.
ynow_admin_panel_ui <- function(user, locale = "zh-TW", build_version = "",
                                points_balance = NA_integer_,
                                points_ledger = NULL,
                                fyn_balance = NA_integer_,
                                pyn_balance = NA_integer_) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  email <- ynow_auth_norm_email(user$email %||% "")
  at <- as.character(user$logged_in_at %||% "")[1]
  fyn <- suppressWarnings(as.integer(fyn_balance %||% points_balance)[1])
  pyn <- suppressWarnings(as.integer(pyn_balance)[1])
  fyn_txt <- if (is.finite(fyn)) as.character(fyn) else "—"
  pyn_txt <- if (is.finite(pyn)) as.character(pyn) else "—"
  pts_on <- exists("ynow_points_enabled", mode = "function") && isTRUE(ynow_points_enabled())
  tags$div(
    class = "ynow-admin-panel ynow-maint-shell",
    `data-ynow-maint-theme` = "logo",
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "ynow-maint-block",
          tags$div(
            class = "ynow-maint-title-row",
            tags$div(
              class = "ynow-maint-title-left",
              tags$h4(
                id = "ynow_admin_box_title",
                .lab("auth_admin_title", "Account overview")
              ),
              tags$span(
                class = "ynow-maint-badge",
                id = "ynow_admin_theme_badge",
                .lab("auth_admin_theme_scope", "This page only")
              )
            ),
            ynow_maint_theme_select_ui(loc)
          ),
          tags$div(
            class = "ynow-maint-help ynow-admin-meta",
            tags$span(id = "ynow_admin_intro", .lab(
              "auth_admin_intro",
              "Signed-in maintenance workspace. Lite mode never requires an account."
            ))
          ),
          tags$div(
            class = "ynow-maint-meta-grid",
            tags$div(
              tags$b(id = "ynow_admin_account_label", .lab("auth_admin_account", "Account")),
              ": ",
              tags$code(if (nzchar(email)) email else "—")
            ),
            tags$div(
              tags$b(id = "ynow_admin_since_label", .lab("auth_admin_since", "Signed in at")),
              ": ",
              if (nzchar(at)) at else "—"
            ),
            tags$div(
              tags$b(id = "ynow_admin_build_label", .lab("auth_admin_build", "Display version")),
              ": ",
              tags$code(if (nzchar(build_version)) build_version else "—")
            ),
            tags$div(
              tags$b(id = "ynow_pts_fyn_label", .lab("pts_fyn_label", "FYN points")),
              ": ",
              tags$code(id = "ynow_pts_fyn_value", fyn_txt),
              " · ",
              tags$b(id = "ynow_pts_pyn_label", .lab("pts_pyn_label", "PYN points")),
              ": ",
              tags$code(id = "ynow_pts_pyn_value", pyn_txt)
            )
          ),
          tags$p(
            class = "ynow-maint-note",
            if (isTRUE(pts_on)) {
              .lab("pts_enabled_note", "Points enforcement is on.")
            } else {
              .lab("pts_disabled_note", "Points enforcement is off.")
            }
          ),
          tags$p(
            class = "ynow-maint-note",
            id = "ynow_pts_rules_note",
            .lab(
              "pts_rules_note",
              "New accounts receive 100 FYN once. Daily check-in adds +30 FYN once per Asia/Taipei day. Balances are never reset on login."
            )
          ),
          tags$h4(
            class = "ynow-maint-section-h",
            id = "ynow_admin_tools_h",
            .lab("auth_admin_tools", "Tools")
          ),
          tags$ul(
            class = "ynow-maint-tools",
            tags$li(id = "ynow_admin_tool_alerts", .lab(
              "auth_admin_tool_alerts",
              "Value Alerts: email notifications default to this account mailbox."
            )),
            tags$li(id = "ynow_admin_tool_lite", .lab(
              "auth_admin_tool_lite",
              "Lite mode stays fully usable without sign-in (no maintenance nav)."
            )),
            tags$li(id = "ynow_admin_tool_points", .lab(
              "auth_admin_tool_points",
              "Points: FYN (free) and PYN (paid) are stored separately; open Points ledger for history."
            ))
          ),
          tags$div(
            class = "ynow-maint-actions",
            actionButton(
              "ynow_admin_goto_points",
              label = tags$span(
                id = "ynow_admin_goto_points_label",
                .lab("pts_goto_ledger", "Open Points ledger")
              ),
              class = "btn-primary btn-sm ynow-maint-btn-primary"
            ),
            actionButton(
              "ynow_admin_goto_alerts",
              label = tags$span(
                id = "ynow_admin_goto_alerts_label",
                .lab("auth_admin_goto_alerts", "Open Value Alerts")
              ),
              class = "btn-default btn-sm ynow-maint-btn-default"
            ),
            actionButton(
              "ynow_auth_sign_out_admin",
              label = tags$span(
                id = "ynow_admin_sign_out_label",
                .lab("auth_sign_out", "Sign out")
              ),
              class = "btn-warning btn-sm ynow-maint-btn-warn"
            )
          )
        )
      )
    )
  )
}

#' Points ledger panel — full history for the signed-in user.
#' Flat block layout (same maintenance shell / theme as Account overview).
#' Table body is filled by server via DT::dataTableOutput("ynow_pts_ledger_dt").
#' System admin adjust lives on the System admin portal (de-identified user list).
ynow_points_panel_ui <- function(user, locale = "zh-TW",
                                 points_balance = NA_integer_,
                                 points_ledger = NULL,
                                 fyn_balance = NA_integer_,
                                 pyn_balance = NA_integer_) {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  email <- ynow_auth_norm_email(user$email %||% "")
  fyn <- suppressWarnings(as.integer(fyn_balance %||% points_balance)[1])
  pyn <- suppressWarnings(as.integer(pyn_balance)[1])
  fyn_txt <- if (is.finite(fyn)) as.character(fyn) else "—"
  pyn_txt <- if (is.finite(pyn)) as.character(pyn) else "—"
  n_rows <- if (is.data.frame(points_ledger)) nrow(points_ledger) else 0L
  ledger_ui <- tags$div(
    id = "ynow_pts_ledger_wrap",
    if (requireNamespace("DT", quietly = TRUE)) {
      DT::dataTableOutput("ynow_pts_ledger_dt")
    } else {
      uiOutput("ynow_pts_ledger_fallback")
    }
  )
  tags$div(
    class = "ynow-pts-ledger-panel ynow-maint-shell",
    `data-ynow-maint-theme` = "logo",
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "ynow-maint-block",
          tags$div(
            class = "ynow-maint-title-row",
            tags$div(
              class = "ynow-maint-title-left",
              tags$h4(
                id = "ynow_pts_box_title",
                .lab("pts_ledger_title", "Points ledger")
              ),
              tags$span(
                class = "ynow-maint-badge",
                .lab("auth_admin_theme_scope", "This page only")
              )
            ),
            ynow_maint_theme_select_ui(loc)
          ),
          tags$div(
            class = "ynow-maint-help ynow-admin-meta",
            tags$span(id = "ynow_pts_ledger_intro", .lab(
              "pts_ledger_intro",
              "Every points change for this account is stored in the database and listed here."
            ))
          ),
          tags$p(
            class = "ynow-maint-note",
            id = "ynow_pts_ledger_rules_note",
            .lab(
              "pts_rules_note",
              "New accounts receive 100 FYN once. Daily check-in adds +30 FYN once per Asia/Taipei day. Balances are never reset on login."
            )
          ),
          tags$div(
            class = "ynow-maint-meta-grid",
            tags$div(
              tags$b(id = "ynow_pts_ledger_account_label", .lab("auth_admin_account", "Account")),
              ": ",
              tags$code(if (nzchar(email)) email else "—")
            ),
            tags$div(
              tags$b(id = "ynow_pts_ledger_fyn_label", .lab("pts_fyn_label", "FYN points")),
              ": ",
              tags$code(id = "ynow_pts_ledger_fyn_value", fyn_txt),
              " · ",
              tags$b(id = "ynow_pts_ledger_pyn_label", .lab("pts_pyn_label", "PYN points")),
              ": ",
              tags$code(id = "ynow_pts_ledger_pyn_value", pyn_txt)
            ),
            tags$div(
              tags$span(
                id = "ynow_pts_ledger_count",
                sprintf(.lab("pts_ledger_count_fmt", "%s records"), as.character(n_rows))
              )
            )
          ),
          tags$h4(
            class = "ynow-maint-section-h",
            id = "ynow_pts_ledger_h",
            .lab("pts_ledger", "All points activity")
          ),
          ledger_ui,
          tags$div(
            class = "ynow-maint-actions",
            actionButton(
              "ynow_pts_goto_overview",
              label = tags$span(
                id = "ynow_pts_goto_overview_label",
                .lab("pts_goto_overview", "Back to Account overview")
              ),
              class = "btn-default btn-sm ynow-maint-btn-default"
            ),
            actionButton(
              "ynow_auth_sign_out_points",
              label = tags$span(
                id = "ynow_pts_sign_out_label",
                .lab("auth_sign_out", "Sign out")
              ),
              class = "btn-warning btn-sm ynow-maint-btn-warn"
            )
          )
        )
      )
    )
  )
}

#' Known gate feature ids (promo + demo are keyed per feature).
ynow_gate_features <- function() {
  c("lab", "checklist", "alerts", "account", "pdf", "snap_defaults")
}

#' Locale key helpers for a feature-specific gate promo.
ynow_gate_promo_keys <- function(feature) {
  feature <- as.character(feature %||% "lab")[1]
  if (!feature %in% ynow_gate_features()) feature <- "lab"
  list(
    feature = feature,
    body = paste0("auth_gate_", feature, "_body"),
    kicker = paste0("auth_gate_", feature, "_kicker"),
    lead = paste0("auth_gate_", feature, "_lead"),
    b1 = paste0("auth_gate_", feature, "_b1"),
    b2 = paste0("auth_gate_", feature, "_b2"),
    b3 = paste0("auth_gate_", feature, "_b3"),
    demo_caption = paste0("auth_gate_", feature, "_demo_caption")
  )
}

.ynow_gate_str <- function(key, fallback = "") {
  tryCatch(ui_str(key, "en"), error = function(e) fallback)
}

#' Shared promo + demo + member FAQ for Sign-in / unlock gate surfaces.
#' @param feature one of [ynow_gate_features()]
#' @param include_faq shared membership Q&A (accordion)
#' @param compact tighter layout (sidebar PDF promo)
ynow_gate_promo_faq_ui <- function(feature = "lab", include_faq = TRUE, compact = FALSE) {
  keys <- ynow_gate_promo_keys(feature)
  feature <- keys$feature
  demo_base <- paste0("demos/", feature)
  tags$div(
    class = paste(
      "ynow-gate-extras",
      if (isTRUE(compact)) "ynow-gate-extras--compact" else NULL
    ),
    `data-ynow-gate-feature` = feature,
    tags$div(
      class = "ynow-gate-promo",
      tags$div(
        class = "ynow-gate-promo__banner",
        tags$span(class = "ynow-gate-promo__badge", "APP"),
        tags$span(
          class = "ynow-gate-promo-kicker",
          .ynow_gate_str(keys$kicker, "APP member exclusive")
        )
      ),
      tags$p(
        class = "ynow-gate-promo-lead",
        .ynow_gate_str(keys$lead, "")
      ),
      tags$ul(
        class = "ynow-gate-promo__list",
        tags$li(tags$span(class = "ynow-gate-promo-b1", .ynow_gate_str(keys$b1, ""))),
        tags$li(tags$span(class = "ynow-gate-promo-b2", .ynow_gate_str(keys$b2, ""))),
        tags$li(tags$span(class = "ynow-gate-promo-b3", .ynow_gate_str(keys$b3, "")))
      )
    ),
    tags$div(
      class = "ynow-gate-demo",
      tags$p(
        class = "ynow-gate-demo-caption",
        .ynow_gate_str(keys$demo_caption, "Feature demo")
      ),
      tags$video(
        class = "ynow-gate-demo-video",
        controls = NA,
        muted = NA,
        playsinline = NA,
        preload = "metadata",
        autoplay = NA,
        loop = NA,
        tags$source(src = paste0(demo_base, ".webm"), type = "video/webm"),
        tags$source(src = paste0(demo_base, ".mp4"), type = "video/mp4"),
        "Demo video unavailable."
      )
    ),
    if (isTRUE(include_faq)) {
      tags$details(
        class = "ynow-gate-faq",
        tags$summary(
          class = "ynow-gate-faq__summary",
          tags$span(
            class = "ynow-gate-faq-title",
            .ynow_gate_str("auth_gate_faq_title", "Member benefits — FAQ")
          )
        ),
        tags$div(
          class = "ynow-gate-faq__body",
          tags$div(
            class = "ynow-gate-faq-item",
            tags$p(
              class = "ynow-gate-faq-q ynow-gate-faq-q1",
              .ynow_gate_str("auth_gate_faq_q1", "Why sign in?")
            ),
            tags$p(
              class = "ynow-gate-faq-a ynow-gate-faq-a1",
              .ynow_gate_str("auth_gate_faq_a1", "")
            )
          ),
          tags$div(
            class = "ynow-gate-faq-item",
            tags$p(
              class = "ynow-gate-faq-q ynow-gate-faq-q2",
              .ynow_gate_str("auth_gate_faq_q2", "What does membership unlock?")
            ),
            tags$p(
              class = "ynow-gate-faq-a ynow-gate-faq-a2",
              .ynow_gate_str("auth_gate_faq_a2", "")
            )
          ),
          tags$div(
            class = "ynow-gate-faq-item",
            tags$p(
              class = "ynow-gate-faq-q ynow-gate-faq-q3",
              .ynow_gate_str("auth_gate_faq_q3", "What are FYN and PYN?")
            ),
            tags$p(
              class = "ynow-gate-faq-a ynow-gate-faq-a3",
              .ynow_gate_str("auth_gate_faq_a3", "")
            )
          ),
          tags$div(
            class = "ynow-gate-faq-item",
            tags$p(
              class = "ynow-gate-faq-q ynow-gate-faq-q4",
              .ynow_gate_str("auth_gate_faq_q4", "Does Lite stay open?")
            ),
            tags$p(
              class = "ynow-gate-faq-a ynow-gate-faq-a4",
              .ynow_gate_str("auth_gate_faq_a4", "")
            )
          )
        )
      )
    } else {
      NULL
    }
  )
}

#' Sign-in panel for Full-mode pages that stay closed until the user is logged in.
#' Hidden until body has ynow-auth-feature and lacks ynow-lite / ynow-authed.
#' @param feature feature key for promo / demo (see [ynow_gate_features()])
ynow_member_gate_ui <- function(gate_id, title_id, body_id, btn_id, feature = "lab") {
  keys <- ynow_gate_promo_keys(feature)
  body_fb <- paste0(
    "This member area requires a signed-in account in Full mode. ",
    "Lite mode does not require sign-in and remains fully usable."
  )
  tags$div(
    id = gate_id,
    class = "box box-solid ynow-member-gate ynow-gate-box",
    `data-ynow-gate-feature` = keys$feature,
    style = "display:none; max-width:760px; margin:18px auto;",
    tags$div(
      class = "box-header with-border",
      tags$h3(
        class = "box-title",
        tags$span(id = title_id, "Sign in required")
      )
    ),
    tags$div(
      class = "box-body",
      tags$p(
        id = body_id,
        class = "ynow-gate-body",
        .ynow_gate_str(keys$body, body_fb)
      ),
      ynow_gate_promo_faq_ui(feature = keys$feature, include_faq = TRUE),
      actionButton(
        btn_id,
        label = tags$span(
          id = paste0(btn_id, "_label"),
          class = "ynow-member-gate-signin-label",
          "Sign in"
        ),
        class = "btn-primary ynow-auth-open"
      )
    )
  )
}

#' Guest gate for Full-mode Value Alerts (same member-gate rule as Backtest and Checklist).
ynow_pa_guest_gate_ui <- function() {
  ynow_member_gate_ui(
    "ynow_pa_guest_gate",
    "ynow_pa_gate_title",
    "ynow_pa_gate_body",
    "ynow_pa_gate_signin",
    feature = "alerts"
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

  #' Collapse sidebar first (client), then open the login modal.
  .open_auth_modal <- function() {
    if (!isTRUE(ynow_auth_enabled())) return()
    # Already signed in: ignore stale Sign-in actionLink clicks (display:none links still fire).
    if (!is.null(account_user())) return()
    # Non-empty payload: some Shiny/json paths drop empty list() customs.
    session$sendCustomMessage("ynowAuthOpenModal", list(ok = TRUE))
  }
  observeEvent(input$ynow_auth_open, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_open_home, {
    .open_auth_modal()
  }, ignoreInit = TRUE)

  .show_auth_note <- function(key, fallback, cls = "text-muted") {
    output$ynow_auth_status <- renderUI({
      tags$p(class = cls, .str(key, fallback))
    })
  }

  #' After successful login: toast, close modal, restore pre-login tab (client).
  .finish_auth_success <- function(user, message_key = "auth_msg_ok",
                                   fallback = "Signed in.", restore_tab = NULL) {
    account_user(user)
    # First gift (100 FYN once) + daily check-in (+30 FYN / Asia/Taipei day). Never reset.
    pts_login <- NULL
    if (exists("ynow_points_on_login", mode = "function")) {
      uid <- as.character(user$id %||% "")[1]
      if (nzchar(uid)) {
        pts_login <- tryCatch(
          ynow_points_on_login(uid),
          error = function(e) NULL
        )
      }
    }
    mk <- as.character(message_key %||% "auth_msg_ok")[1]
    msg <- .str(mk, fallback)
    if (isTRUE(pts_login$checkin_granted)) {
      amt <- tryCatch(ynow_points_daily_checkin_amount(), error = function(e) 30L)
      pts_note <- tryCatch(
        sprintf(.str("pts_daily_checkin_fmt", "Daily check-in: +%s FYN."), as.character(amt)),
        error = function(e) sprintf("Daily check-in: +%s FYN.", as.character(amt))
      )
      msg <- paste(msg, pts_note)
    } else if (isTRUE(pts_login$gift_granted)) {
      gift_n <- tryCatch(ynow_points_signup_bonus(), error = function(e) 100L)
      pts_note <- tryCatch(
        sprintf(.str("pts_first_gift_fmt", "Welcome gift: +%s FYN."), as.character(gift_n)),
        error = function(e) sprintf("Welcome gift: +%s FYN.", as.character(gift_n))
      )
      msg <- paste(msg, pts_note)
    }
    output$ynow_auth_status <- renderUI({
      tags$p(class = "text-success", msg)
    })
    .push_auth_state(TRUE, user$email %||% "")
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
    tryCatch(
      showNotification(msg, type = "message", duration = 5),
      error = function(e) NULL
    )
    tab <- as.character(restore_tab %||% "")[1]
    if (!nzchar(tab)) {
      tab <- as.character(isolate(input$sidebar_tabs) %||% "")[1]
    }
    # Sys. Admin (testing) is ops-only — never keep a signed-in user on that tab.
    if (identical(tab, "testing")) tab <- "home"
    session$sendCustomMessage(
      "ynowAuthPostLogin",
      list(ok = TRUE, restore_tab = tab, message = msg)
    )
    if (nzchar(tab) && !identical(tab, as.character(isolate(input$sidebar_tabs) %||% "")[1])) {
      tryCatch(
        updateTabItems(session, "sidebar_tabs", tab),
        error = function(e) {
          tryCatch(
            session$sendInputMessage("sidebar_tabs", list(value = tab)),
            error = function(e2) NULL
          )
        }
      )
    }
  }

  #' OAuth failure after IdP return: toast only — do not auto-open login modal.
  .finish_auth_oauth_failure <- function(message_key, fallback = "Social sign-in failed.") {
    mk <- as.character(message_key %||% "auth_msg_oauth_failed")[1]
    msg <- .str(mk, fallback)
    .show_auth_note(mk, fallback, cls = "text-danger")
    tryCatch(
      showNotification(msg, type = "error", duration = 6),
      error = function(e) NULL
    )
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
    session$sendCustomMessage(
      "ynowAuthPostLogin",
      list(ok = FALSE, restore_tab = "", message = msg)
    )
  }

  observeEvent(input$ynow_auth_submit, {
    if (!isTRUE(ynow_auth_enabled())) return()
    res <- ynow_auth_try_login(input$ynow_auth_email, input$ynow_auth_password)
    if (isTRUE(res$ok)) {
      .finish_auth_success(
        res$user,
        message_key = res$message_key %||% "auth_msg_ok",
        restore_tab = as.character(input$ynow_auth_restore_tab %||% "")[1]
      )
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_invalid")[1]
      output$ynow_auth_status <- renderUI({
        tags$p(class = "text-danger", .str(mk, "Sign-in failed."))
      })
    }
  }, ignoreInit = TRUE)

  .start_oauth <- function(provider) {
    if (!isTRUE(ynow_auth_enabled())) return()
    if (!exists("ynow_oauth_authorize_url", mode = "function")) {
      .show_auth_note("auth_msg_oauth_soon", "Social sign-in is not configured yet.")
      return()
    }
    res <- ynow_oauth_authorize_url(provider)
    if (!isTRUE(res$ok) || !nzchar(as.character(res$url %||% "")[1])) {
      mk <- as.character(res$message_key %||% "auth_msg_oauth_soon")[1]
      fallback <- if (identical(mk, "auth_msg_oauth_provider_disabled")) {
        "This social provider is not enabled in Supabase Auth yet."
      } else {
        "Social sign-in is not configured yet."
      }
      .show_auth_note(mk, fallback, cls = "text-danger")
      return()
    }
    tab_now <- as.character(isolate(input$sidebar_tabs) %||% "")[1]
    session$sendCustomMessage(
      "ynowAuthOAuthRedirect",
      list(url = as.character(res$url)[1], restore_tab = tab_now)
    )
  }

  observeEvent(input$ynow_auth_google, {
    .start_oauth("google")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_facebook, {
    .start_oauth("facebook")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_linkedin, {
    .start_oauth("linkedin")
  }, ignoreInit = TRUE)

  # Browser returned from Google / Facebook / LinkedIn / Supabase with code or access_token.
  # Self-serve: upsert into Neon (role=user) and sign in — no admin permission step.
  observeEvent(input$ynow_auth_oauth_callback, {
    if (!isTRUE(ynow_auth_enabled())) return()
    if (!exists("ynow_oauth_complete", mode = "function")) {
      .show_auth_note("auth_msg_oauth_failed", "Social sign-in failed.")
      return()
    }
    payload <- input$ynow_auth_oauth_callback
    restore_tab <- as.character(payload$restore_tab %||% "")[1]
    oauth_res <- ynow_oauth_complete(payload)
    if (!isTRUE(oauth_res$ok) || is.null(oauth_res$user)) {
      mk <- as.character(oauth_res$message_key %||% "auth_msg_oauth_failed")[1]
      .finish_auth_oauth_failure(mk, "Social sign-in failed.")
      return()
    }
    res <- ynow_auth_accept_social_user(oauth_res$user)
    if (isTRUE(res$ok) && !is.null(res$user)) {
      .finish_auth_success(
        res$user,
        message_key = res$message_key %||% "auth_msg_ok",
        restore_tab = restore_tab
      )
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_oauth_failed")[1]
      .finish_auth_oauth_failure(mk, "Social sign-in failed.")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_forgot, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .show_auth_note("auth_msg_forgot_soon", "Password reset is not available yet.")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_register, {
    if (!isTRUE(ynow_auth_enabled())) return()
    # Same Email/password fields as sign-in; Neon creates the account when configured.
    res <- ynow_auth_try_register(input$ynow_auth_email, input$ynow_auth_password)
    if (isTRUE(res$ok) && !is.null(res$user)) {
      .finish_auth_success(
        res$user,
        message_key = "auth_msg_register_ok",
        fallback = "Account created. Signed in.",
        restore_tab = as.character(input$ynow_auth_restore_tab %||% "")[1]
      )
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_register_soon")[1]
      cls <- if (identical(mk, "auth_msg_register_soon")) "text-muted" else "text-danger"
      .show_auth_note(mk, "Registration is not open yet.", cls = cls)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_resend, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .show_auth_note("auth_msg_resend_soon", "Verification email resend is not available yet.")
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_sign_out, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_sign_out_home, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_sign_out_admin, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_sign_out_points, {
    if (!isTRUE(ynow_auth_enabled())) return()
    .sign_out()
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_pa_gate_signin, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_lab_gate_signin, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_chk_gate_signin, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_pdf_gate_signin, {
    .open_auth_modal()
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
  # Also re-send from the locale observe below: onFlushed can race before the
  # client registers ynowAuthFeature (Shiny 1.14 custom handlers).
  if (isTRUE(ynow_auth_enabled())) {
    session$onFlushed(function() {
      session$sendCustomMessage("ynowAuthFeature", list(on = TRUE))
    }, once = TRUE)
  }

  # Push locale labels when language changes (only matters when chrome exists).
  observe({
    if (!isTRUE(ynow_auth_enabled())) return()
    loc <- .loc()
    session$sendCustomMessage("ynowAuthFeature", list(on = TRUE))
    session$sendCustomMessage(
      "ynowAuthLocale",
      list(
        sign_in = tryCatch(ui_str("auth_sign_in", loc), error = function(e) "Sign in"),
        sign_out = tryCatch(ui_str("auth_sign_out", loc), error = function(e) "Sign out"),
        modal_title = tryCatch(ui_str("auth_modal_title", loc), error = function(e) "Sign in to The YNow App"),
        modal_help = tryCatch(ui_str("auth_modal_help", loc), error = function(e) ""),
        email_label = tryCatch(ui_str("auth_email_label", loc), error = function(e) "Enter your Email account:"),
        password_label = tryCatch(ui_str("auth_password_label", loc), error = function(e) "Enter password:"),
        submit = tryCatch(ui_str("auth_submit", loc), error = function(e) "Email sign-in"),
        cancel = tryCatch(ui_str("auth_cancel", loc), error = function(e) "Cancel"),
        or_email = tryCatch(ui_str("auth_or_email", loc), error = function(e) "Or sign in with Email"),
        google = tryCatch(ui_str("auth_google", loc), error = function(e) "Continue with Google"),
        facebook = tryCatch(ui_str("auth_facebook", loc), error = function(e) "Continue with Facebook"),
        linkedin = tryCatch(ui_str("auth_linkedin", loc), error = function(e) "Continue with LinkedIn"),
        forgot = tryCatch(ui_str("auth_forgot", loc), error = function(e) "Forgot password?"),
        register = tryCatch(ui_str("auth_register", loc), error = function(e) "Register a new account"),
        resend = tryCatch(ui_str("auth_resend", loc), error = function(e) "Didn't receive the verification email"),
        admin_title = tryCatch(ui_str("auth_admin_title", loc), error = function(e) "Account overview"),
        admin_intro = tryCatch(ui_str("auth_admin_intro", loc), error = function(e) ""),
        pa_gate_title = tryCatch(ui_str("auth_pa_gate_title", loc), error = function(e) "Sign in required"),
        pa_gate_body = tryCatch(ui_str("auth_pa_gate_body", loc), error = function(e) ""),
        gate_lab_body = tryCatch(ui_str("auth_gate_lab_body", loc), error = function(e) ""),
        gate_lab_kicker = tryCatch(ui_str("auth_gate_lab_kicker", loc), error = function(e) ""),
        gate_lab_lead = tryCatch(ui_str("auth_gate_lab_lead", loc), error = function(e) ""),
        gate_lab_b1 = tryCatch(ui_str("auth_gate_lab_b1", loc), error = function(e) ""),
        gate_lab_b2 = tryCatch(ui_str("auth_gate_lab_b2", loc), error = function(e) ""),
        gate_lab_b3 = tryCatch(ui_str("auth_gate_lab_b3", loc), error = function(e) ""),
        gate_lab_demo_caption = tryCatch(ui_str("auth_gate_lab_demo_caption", loc), error = function(e) ""),
        gate_checklist_body = tryCatch(ui_str("auth_gate_checklist_body", loc), error = function(e) ""),
        gate_checklist_kicker = tryCatch(ui_str("auth_gate_checklist_kicker", loc), error = function(e) ""),
        gate_checklist_lead = tryCatch(ui_str("auth_gate_checklist_lead", loc), error = function(e) ""),
        gate_checklist_b1 = tryCatch(ui_str("auth_gate_checklist_b1", loc), error = function(e) ""),
        gate_checklist_b2 = tryCatch(ui_str("auth_gate_checklist_b2", loc), error = function(e) ""),
        gate_checklist_b3 = tryCatch(ui_str("auth_gate_checklist_b3", loc), error = function(e) ""),
        gate_checklist_demo_caption = tryCatch(ui_str("auth_gate_checklist_demo_caption", loc), error = function(e) ""),
        gate_alerts_body = tryCatch(ui_str("auth_gate_alerts_body", loc), error = function(e) ""),
        gate_alerts_kicker = tryCatch(ui_str("auth_gate_alerts_kicker", loc), error = function(e) ""),
        gate_alerts_lead = tryCatch(ui_str("auth_gate_alerts_lead", loc), error = function(e) ""),
        gate_alerts_b1 = tryCatch(ui_str("auth_gate_alerts_b1", loc), error = function(e) ""),
        gate_alerts_b2 = tryCatch(ui_str("auth_gate_alerts_b2", loc), error = function(e) ""),
        gate_alerts_b3 = tryCatch(ui_str("auth_gate_alerts_b3", loc), error = function(e) ""),
        gate_alerts_demo_caption = tryCatch(ui_str("auth_gate_alerts_demo_caption", loc), error = function(e) ""),
        gate_account_body = tryCatch(ui_str("auth_gate_account_body", loc), error = function(e) ""),
        gate_account_kicker = tryCatch(ui_str("auth_gate_account_kicker", loc), error = function(e) ""),
        gate_account_lead = tryCatch(ui_str("auth_gate_account_lead", loc), error = function(e) ""),
        gate_account_b1 = tryCatch(ui_str("auth_gate_account_b1", loc), error = function(e) ""),
        gate_account_b2 = tryCatch(ui_str("auth_gate_account_b2", loc), error = function(e) ""),
        gate_account_b3 = tryCatch(ui_str("auth_gate_account_b3", loc), error = function(e) ""),
        gate_account_demo_caption = tryCatch(ui_str("auth_gate_account_demo_caption", loc), error = function(e) ""),
        gate_pdf_body = tryCatch(ui_str("auth_gate_pdf_body", loc), error = function(e) ""),
        gate_pdf_kicker = tryCatch(ui_str("auth_gate_pdf_kicker", loc), error = function(e) ""),
        gate_pdf_lead = tryCatch(ui_str("auth_gate_pdf_lead", loc), error = function(e) ""),
        gate_pdf_b1 = tryCatch(ui_str("auth_gate_pdf_b1", loc), error = function(e) ""),
        gate_pdf_b2 = tryCatch(ui_str("auth_gate_pdf_b2", loc), error = function(e) ""),
        gate_pdf_b3 = tryCatch(ui_str("auth_gate_pdf_b3", loc), error = function(e) ""),
        gate_pdf_demo_caption = tryCatch(ui_str("auth_gate_pdf_demo_caption", loc), error = function(e) ""),
        gate_snap_defaults_body = tryCatch(ui_str("auth_gate_snap_defaults_body", loc), error = function(e) ""),
        gate_snap_defaults_kicker = tryCatch(ui_str("auth_gate_snap_defaults_kicker", loc), error = function(e) ""),
        gate_snap_defaults_lead = tryCatch(ui_str("auth_gate_snap_defaults_lead", loc), error = function(e) ""),
        gate_snap_defaults_b1 = tryCatch(ui_str("auth_gate_snap_defaults_b1", loc), error = function(e) ""),
        gate_snap_defaults_b2 = tryCatch(ui_str("auth_gate_snap_defaults_b2", loc), error = function(e) ""),
        gate_snap_defaults_b3 = tryCatch(ui_str("auth_gate_snap_defaults_b3", loc), error = function(e) ""),
        gate_snap_defaults_demo_caption = tryCatch(ui_str("auth_gate_snap_defaults_demo_caption", loc), error = function(e) ""),
        gate_faq_title = tryCatch(ui_str("auth_gate_faq_title", loc), error = function(e) "Member benefits — FAQ"),
        gate_faq_q1 = tryCatch(ui_str("auth_gate_faq_q1", loc), error = function(e) ""),
        gate_faq_a1 = tryCatch(ui_str("auth_gate_faq_a1", loc), error = function(e) ""),
        gate_faq_q2 = tryCatch(ui_str("auth_gate_faq_q2", loc), error = function(e) ""),
        gate_faq_a2 = tryCatch(ui_str("auth_gate_faq_a2", loc), error = function(e) ""),
        gate_faq_q3 = tryCatch(ui_str("auth_gate_faq_q3", loc), error = function(e) ""),
        gate_faq_a3 = tryCatch(ui_str("auth_gate_faq_a3", loc), error = function(e) ""),
        gate_faq_q4 = tryCatch(ui_str("auth_gate_faq_q4", loc), error = function(e) ""),
        gate_faq_a4 = tryCatch(ui_str("auth_gate_faq_a4", loc), error = function(e) ""),
        menu_admin = tryCatch(ui_str("menu_admin", loc), error = function(e) "Account"),
        menu_admin_overview = tryCatch(ui_str("menu_admin_overview", loc), error = function(e) "Overview"),
        menu_pts_ledger = tryCatch(ui_str("menu_pts_ledger", loc), error = function(e) "Points ledger"),
        pts_box_title = tryCatch(ui_str("pts_ledger_title", loc), error = function(e) "Points ledger"),
        pts_ledger_intro = tryCatch(ui_str("pts_ledger_intro", loc), error = function(e) "")
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
