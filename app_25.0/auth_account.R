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
  # Preferred: Neon Free account table (own users, same Email modal).
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

  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_upsert_oauth_user", mode = "function")) {
    return(ynow_db_upsert_oauth_user(
      email = em,
      display_name = dn,
      provider = prov,
      provider_uid = puid
    ))
  }

  # No Neon yet: still allow self-serve session (not an admin-gated whitelist).
  list(
    ok = TRUE,
    user = list(
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

#' Apple mark for Sign in with Apple (decorative; white on black button).
.ynow_auth_apple_icon <- function() {
  HTML(paste0(
    '<svg class="ynow-auth-social-icon ynow-auth-social-icon--apple" viewBox="0 0 24 24" width="18" height="18" aria-hidden="true" focusable="false">',
    '<path fill="currentColor" d="M16.365 1.43c0 1.14-.418 2.206-1.18 3.048-.81.89-2.15 1.576-3.3 1.48-.14-1.1.42-2.26 1.2-3.06.81-.84 2.2-1.46 3.28-1.47zM20.5 17.24c-.58 1.34-.86 1.93-1.61 3.11-1.05 1.63-2.53 3.66-4.37 3.68-1.63.02-2.05-1.06-4.27-1.05-2.22.01-2.68 1.07-4.31 1.05-1.84-.02-3.25-1.85-4.3-3.48C-.01 17.6-.9 12.6 1.3 9.2c1.1-1.72 2.85-2.8 4.5-2.8 1.68 0 2.74 1.1 4.13 1.1 1.35 0 2.17-1.11 4.12-1.11 1.47 0 3.03.8 4.13 2.18-3.63 1.99-3.04 7.18.32 8.67z"/>',
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
        inputId = "ynow_auth_apple",
        label = tagList(
          .ynow_auth_apple_icon(),
          tags$span(id = "ynow_auth_apple_label", "Continue with Apple")
        ),
        class = "ynow-auth-social ynow-auth-social--apple"
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
    ".ynow-auth-social {",
    "  display:flex; align-items:center; justify-content:center; gap:10px;",
    "  width:100%; height:44px; margin:0; padding:0 14px;",
    "  border-radius:2px; font-size:14px; font-weight:600; line-height:1;",
    "  cursor:pointer; box-shadow:none !important; text-shadow:none;",
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
    ".ynow-auth-social--apple {",
    "  background:#000 !important; border:1px solid #000 !important; color:#fff !important;",
    "}",
    ".ynow-auth-social--apple:hover {",
    "  background:#222 !important; border-color:#222 !important; color:#fff !important;",
    "}",
    ".ynow-auth-social-icon { flex:0 0 auto; display:block; }",
    ".ynow-auth-social-icon--fb { color:#fff; }",
    ".ynow-auth-social-icon--apple { color:#fff; }",
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

  .show_auth_note <- function(key, fallback, cls = "text-muted") {
    output$ynow_auth_status <- renderUI({
      tags$p(class = cls, .str(key, fallback))
    })
  }

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

  .start_oauth <- function(provider) {
    if (!isTRUE(ynow_auth_enabled())) return()
    if (!exists("ynow_oauth_authorize_url", mode = "function")) {
      .show_auth_note("auth_msg_oauth_soon", "Social sign-in is not configured yet.")
      return()
    }
    res <- ynow_oauth_authorize_url(provider)
    if (!isTRUE(res$ok) || !nzchar(as.character(res$url %||% "")[1])) {
      mk <- as.character(res$message_key %||% "auth_msg_oauth_soon")[1]
      .show_auth_note(mk, "Social sign-in is not configured yet.")
      return()
    }
    session$sendCustomMessage(
      "ynowAuthOAuthRedirect",
      list(url = as.character(res$url)[1])
    )
  }

  observeEvent(input$ynow_auth_google, {
    .start_oauth("google")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_facebook, {
    .start_oauth("facebook")
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_apple, {
    .start_oauth("apple")
  }, ignoreInit = TRUE)

  # Browser returned from Google / Facebook / Supabase with code or access_token.
  # Self-serve: upsert into Neon (role=user) and sign in — no admin permission step.
  observeEvent(input$ynow_auth_oauth_callback, {
    if (!isTRUE(ynow_auth_enabled())) return()
    if (!exists("ynow_oauth_complete", mode = "function")) {
      .show_auth_note("auth_msg_oauth_failed", "Social sign-in failed.")
      return()
    }
    payload <- input$ynow_auth_oauth_callback
    oauth_res <- ynow_oauth_complete(payload)
    if (!isTRUE(oauth_res$ok) || is.null(oauth_res$user)) {
      mk <- as.character(oauth_res$message_key %||% "auth_msg_oauth_failed")[1]
      .show_auth_note(mk, "Social sign-in failed.", cls = "text-danger")
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
      return()
    }
    res <- ynow_auth_accept_social_user(oauth_res$user)
    if (isTRUE(res$ok) && !is.null(res$user)) {
      account_user(res$user)
      mk_ok <- as.character(res$message_key %||% "auth_msg_ok")[1]
      output$ynow_auth_status <- renderUI({
        tags$p(class = "text-success", .str(mk_ok, "Signed in."))
      })
      .push_auth_state(TRUE, res$user$email %||% "")
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_oauth_failed")[1]
      .show_auth_note(mk, "Social sign-in failed.", cls = "text-danger")
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
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
      account_user(res$user)
      output$ynow_auth_status <- renderUI({
        tags$p(class = "text-success", .str("auth_msg_register_ok", "Account created. Signed in."))
      })
      .push_auth_state(TRUE, res$user$email %||% "")
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
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
        modal_title = tryCatch(ui_str("auth_modal_title", loc), error = function(e) "Sign in to The YNow App"),
        modal_help = tryCatch(ui_str("auth_modal_help", loc), error = function(e) ""),
        email_label = tryCatch(ui_str("auth_email_label", loc), error = function(e) "Enter your Email account:"),
        password_label = tryCatch(ui_str("auth_password_label", loc), error = function(e) "Enter password:"),
        submit = tryCatch(ui_str("auth_submit", loc), error = function(e) "Email sign-in"),
        cancel = tryCatch(ui_str("auth_cancel", loc), error = function(e) "Cancel"),
        or_email = tryCatch(ui_str("auth_or_email", loc), error = function(e) "Or sign in with Email"),
        google = tryCatch(ui_str("auth_google", loc), error = function(e) "Continue with Google"),
        facebook = tryCatch(ui_str("auth_facebook", loc), error = function(e) "Continue with Facebook"),
        apple = tryCatch(ui_str("auth_apple", loc), error = function(e) "Continue with Apple"),
        forgot = tryCatch(ui_str("auth_forgot", loc), error = function(e) "Forgot password?"),
        register = tryCatch(ui_str("auth_register", loc), error = function(e) "Register a new account"),
        resend = tryCatch(ui_str("auth_resend", loc), error = function(e) "Didn't receive the verification email"),
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
