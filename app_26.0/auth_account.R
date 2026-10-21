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
#
# Freemium A/B membership locks: see membership_locks.R (sourced into this env).
if (!exists("ynow_membership_lock_catalog", mode = "function")) {
  .ynow_ml_ofile <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) ""
  )
  .ynow_ml_cands <- unique(c(
    if (nzchar(.ynow_ml_ofile)) {
      file.path(dirname(.ynow_ml_ofile), "membership_locks.R")
    } else {
      character()
    },
    "membership_locks.R",
    file.path("app_26.0", "membership_locks.R"),
    file.path("..", "membership_locks.R")
  ))
  .ynow_ml_loaded <- FALSE
  for (.ynow_ml_p in .ynow_ml_cands) {
    if (is.character(.ynow_ml_p) && nzchar(.ynow_ml_p) && file.exists(.ynow_ml_p)) {
      # environment() at top-level of source() is the caller's target env (local=TRUE safe).
      sys.source(.ynow_ml_p, envir = environment(), keep.source = TRUE)
      .ynow_ml_loaded <- exists("ynow_membership_lock_catalog", mode = "function")
      if (isTRUE(.ynow_ml_loaded)) break
    }
  }
  if (!isTRUE(.ynow_ml_loaded)) {
    warning("membership_locks.R not found; membership A/B catalog unavailable", call. = FALSE)
  }
  rm(list = intersect(
    c(".ynow_ml_ofile", ".ynow_ml_cands", ".ynow_ml_p", ".ynow_ml_loaded"),
    ls(all.names = TRUE)
  ))
}

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

#' Sidebar tabNames that Full shows with a guest lock glyph (auth on + guest).
#' Lite must omit these entirely (no grayed locks).
#' Source of truth: membership_locks.R (tier A, mechanism guest_menu_lock).
#' @return character vector of shinydashboard tabName values
ynow_sidebar_guest_lock_tabs <- function() {
  if (exists("ynow_membership_lock_ids", mode = "function")) {
    tabs <- ynow_membership_lock_ids(
      tier = "A",
      kind = "sidebar_tab",
      mechanism = "guest_menu_lock"
    )
    if (length(tabs)) return(tabs)
  }
  c("lab_notes", "decision_checklist", "price_alerts")
}

#' Full-only model / advance sidebar tabs (unlocked on Full, omitted in Lite).
#' Not guest-locked; still must not appear in the Lite sidebar.
#' Includes Asset Transmission Root (`asset_transmission`) — Full only.
ynow_sidebar_full_only_tabs <- function() {
  c(
    "get_started",
    "nav_calculator", "pb_calculator",
    "dcf_calculator", "ddm_calculator", "ri_calculator",
    "rel_mult_ev", "rel_multiples_calculator", "rel_mult_rev",
    "sotp_calculator",
    "hfv",
    "company_advance",
    "asset_transmission"
  )
}

#' Every sidebar tab Lite must omit (guest-locked ∪ Full-only ∪ Account workspace).
#' Snapshot foot stays open in Lite (only inner snap_defaults is hidden elsewhere).
#' Sys. Admin foot uses `.ynow-full-only` / `.ynow-sidebar-test-link`, not this list.
ynow_sidebar_lite_omit_tabs <- function() {
  unique(c(
    ynow_sidebar_guest_lock_tabs(),
    ynow_sidebar_full_only_tabs(),
    "account_admin", "account_points", "settings"
  ))
}

#' TRUE when tabName is in the Full guest-lock set.
ynow_sidebar_tab_guest_locked <- function(tab) {
  as.character(tab %||% "")[1] %in% ynow_sidebar_guest_lock_tabs()
}

#' Menu label with optional guest lock glyph (Full shows lock; Lite CSS hides the tab).
ynow_sidebar_guest_lock_label <- function(tab, span_id, label) {
  lab <- tags$span(id = span_id, label)
  if (!isTRUE(ynow_sidebar_tab_guest_locked(tab))) return(lab)
  tagList(icon("lock", class = "ynow-menu-lock"), lab)
}

#' CSS selectors hiding one sidebar tabName in Lite (item + parent treeview when nested).
.ynow_sidebar_lite_omit_selectors <- function(tab) {
  t <- as.character(tab %||% "")[1]
  if (!nzchar(t)) return(character())
  c(
    sprintf('body.ynow-lite .sidebar-menu a[data-value="%s"]', t),
    sprintf('body.ynow-lite .sidebar-menu li:has(> a[data-value="%s"])', t),
    # Nested menuSubItem: also hide ancestor treeview that contains it
    sprintf('body.ynow-lite .sidebar-menu li:has(a[data-value="%s"])', t)
  )
}

#' CSS: hide every Lite-omitted sidebar tab (guest locks + Full-only + Account).
ynow_sidebar_lite_omit_css <- function() {
  tabs <- ynow_sidebar_lite_omit_tabs()
  if (!length(tabs)) return("/* no Lite-omitted sidebar tabs */\n")
  sels <- unique(unlist(lapply(tabs, .ynow_sidebar_lite_omit_selectors), use.names = FALSE))
  paste0(
    "/* Lite sidebar omit (ynow_sidebar_lite_omit_tabs): guest-locked + Full-only (incl. ATX) + Account. */\n",
    paste(sels, collapse = ",\n"),
    " {\n  display: none !important;\n}\n"
  )
}

#' @deprecated Use ynow_sidebar_lite_omit_css(); kept for older call sites / tests.
ynow_sidebar_guest_lock_lite_css <- function() {
  ynow_sidebar_lite_omit_css()
}

#' Named list / JSON-ready object for client loginTabs (glyph re-inject on locale apply).
ynow_sidebar_guest_lock_js_map <- function() {
  tabs <- ynow_sidebar_guest_lock_tabs()
  as.list(stats::setNames(rep(list(TRUE), length(tabs)), tabs))
}

#' Sidebar tabs that keep locked chrome after sign-in (tier B / points-gated).
#' Unlock-after-login (A) guest_menu_lock tabs are NOT listed — they unlock normally.
#' ACL still-locked tabs use `.ynow-access-locked-nav` separately (same chrome).
#' @return named list for `window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS`
ynow_sidebar_persist_lock_js_map <- function() {
  tabs <- character(0)
  if (exists("ynow_membership_lock_ids", mode = "function")) {
    tabs <- ynow_membership_lock_ids(
      tier = "B",
      kind = "sidebar_tab"
    )
  }
  if (!length(tabs)) return(setNames(list(), character(0)))
  as.list(stats::setNames(rep(list(TRUE), length(tabs)), tabs))
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

#' Sign-in / Sign-out controls for a placement (header bar, mobile date row, or Home legal).
#' @param placement "sidebar" (desktop header buttons) | "date_row" (mobile text links) | "home"
#' @return shiny tag or NULL when auth disabled
ynow_auth_links_ui <- function(placement = c("sidebar", "date_row", "home")) {
  placement <- match.arg(placement)
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  # Unique input ids per placement (Shiny forbids duplicate ids).
  open_id <- switch(
    placement,
    sidebar = "ynow_auth_open",
    date_row = "ynow_auth_open_date",
    "ynow_auth_open_home"
  )
  out_id <- switch(
    placement,
    sidebar = "ynow_auth_sign_out",
    date_row = "ynow_auth_sign_out_date",
    "ynow_auth_sign_out_home"
  )
  open_lbl <- switch(
    placement,
    sidebar = "ynow_auth_header_label",
    date_row = "ynow_auth_date_label",
    "ynow_auth_home_label"
  )
  out_lbl <- switch(
    placement,
    sidebar = "ynow_auth_sign_out_label",
    date_row = "ynow_auth_sign_out_date_label",
    "ynow_auth_sign_out_home_label"
  )
  wrap_id <- switch(
    placement,
    sidebar = "ynow-auth-sidebar",
    date_row = "ynow-auth-date-row",
    "ynow-auth-home"
  )
  wrap_class <- switch(
    placement,
    sidebar = "ynow-auth-links ynow-auth-links--sidebar",
    date_row = "ynow-auth-links ynow-auth-links--date-row",
    "ynow-auth-links ynow-auth-links--home"
  )
  # Home + mobile date row: plain text links (no button chrome).
  # Header (sidebar placement): Sign in only — compact black bar has no Register entry.
  # Register remains on Home, feature gates, and the auth modal.
  home_text <- identical(placement, "home")
  date_text <- identical(placement, "date_row")
  link_class <- if (home_text || date_text) {
    "ynow-auth-link ynow-auth-open"
  } else {
    "ynow-auth-link ynow-auth-btn ynow-auth-open"
  }
  sign_in_link <- actionLink(
    inputId = open_id,
    label = tags$span(id = open_lbl, class = "ynow-auth-open-label", "Sign in"),
    class = link_class
  )
  auth_pair <- if (home_text) {
    tags$div(
      class = "ynow-auth-pair ynow-auth-pair--home-text",
      sign_in_link,
      tags$span(class = "ynow-auth-pair-sep", "·", `aria-hidden` = "true"),
      actionLink(
        inputId = "ynow_auth_register_open_home",
        label = tags$span(
          id = "ynow_auth_register_home_label",
          class = "ynow-auth-register-label",
          "Register"
        ),
        class = "ynow-auth-link ynow-auth-register-open"
      )
    )
  } else if (date_text) {
    # Mobile date row: Sign in / Sign out as text links matching date font size.
    tags$div(class = "ynow-auth-pair ynow-auth-pair--date-text", sign_in_link)
  } else {
    # Single Sign-in control (no pair / Register) for the top header bar.
    tags$div(class = "ynow-auth-pair ynow-auth-pair--signin-only", sign_in_link)
  }
  tags$div(
    id = wrap_id,
    class = wrap_class,
    role = "group",
    `aria-label` = "Account",
    auth_pair,
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

#' Home legal stack companion (Sign in · Register above disclaimer / About).
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
#' Forgot password uses Supabase Auth /recover; recovery return shows set-password.
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
      tags$div(
        class = "ynow-auth-signin-block",
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
        tags$div(
          class = "ynow-auth-pair ynow-auth-pair--modal",
          actionButton(
            "ynow_auth_submit",
            label = tags$span(id = "ynow_auth_submit_label", "Email sign-in"),
            class = "ynow-auth-email-submit ynow-auth-btn"
          ),
          actionButton(
            "ynow_auth_register",
            label = tags$span(
              id = "ynow_auth_register_label",
              class = "ynow-auth-register-label",
              "Register"
            ),
            class = "ynow-auth-register-submit ynow-auth-btn"
          )
        ),
        tags$div(
          class = "ynow-auth-card-foot",
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
      # Shown after Supabase recovery redirect (type=recovery); hides sign-in block.
      tags$div(
        class = "ynow-auth-reset-block",
        style = "display:none;",
        tags$p(
          id = "ynow_auth_reset_help",
          class = "ynow-auth-reset-help",
          "Choose a new password for your account."
        ),
        tags$div(
          class = "ynow-auth-field",
          tags$label(
            `for` = "ynow_auth_reset_password",
            tags$span(id = "ynow_auth_reset_password_label", "New password:")
          ),
          tags$input(
            id = "ynow_auth_reset_password",
            type = "password",
            class = "form-control ynow-auth-input",
            value = "",
            autocomplete = "new-password"
          )
        ),
        tags$div(
          class = "ynow-auth-field",
          tags$label(
            `for` = "ynow_auth_reset_password2",
            tags$span(id = "ynow_auth_reset_password2_label", "Confirm new password:")
          ),
          tags$input(
            id = "ynow_auth_reset_password2",
            type = "password",
            class = "form-control ynow-auth-input",
            value = "",
            autocomplete = "new-password"
          )
        ),
        actionButton(
          "ynow_auth_reset_submit",
          label = tags$span(id = "ynow_auth_reset_submit_label", "Save new password"),
          class = "ynow-auth-email-submit ynow-auth-btn ynow-auth-reset-submit"
        ),
        tags$div(
          class = "ynow-auth-forgot-row",
          actionLink(
            "ynow_auth_reset_back",
            label = tags$span(id = "ynow_auth_reset_back_label", "Back to sign in"),
            class = "ynow-auth-text-link"
          )
        )
      ),
      uiOutput("ynow_auth_status")
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

#' App-generated / proprietary download output ids that require sign-in when auth is on.
#' Full guests are gated; Lite keeps Download Report (and other Lite-visible downloads) open.
#' Source of truth: membership_locks.R (kind = download; tier A or B).
ynow_gated_download_ids <- function() {
  if (exists("ynow_membership_lock_ids", mode = "function")) {
    ids <- ynow_membership_lock_ids(kind = "download")
    if (length(ids)) return(ids)
  }
  c(
    "download_report",
    "download_snapshot",
    "download_param_restore",
    "download_defaults",
    "lab_im_download_report",
    "param_audit_pdf_go",
    "bblab-export_data",
    "bblab-export_chart"
  )
}

#' Public-data passthrough downloads left open for guests (raw statements).
#' FS_download has a UI button but no server handler today; kept here as public intent.
ynow_public_download_ids <- function() {
  c("IS_download", "BS_download", "CF_download", "FS_download")
}

#' TRUE when auth is on and the subject has no signed-in user id (guest).
#' Lite never denies downloads — free surface stays open (hide Full-only locks elsewhere).
ynow_guest_download_denied <- function(user_id = "", auth_enabled = NULL, lite = FALSE) {
  if (isTRUE(lite)) return(FALSE)
  if (is.null(auth_enabled)) {
    auth_enabled <- exists("ynow_auth_enabled", mode = "function") &&
      isTRUE(ynow_auth_enabled())
  }
  isTRUE(auth_enabled) && !nzchar(as.character(user_id %||% "")[1])
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
    "  gap:8px; flex-wrap:nowrap; width:100%; min-height:22px;",
    "  margin:0; padding:6px 4px 2px;",
    "}",
    ".ynow-sidebar-date-row .ynow-sidebar-date,",
    ".ynow-sidebar-date-row #today {",
    "  flex:0 0 auto; margin:0; padding:0; font-size:12px; line-height:1.2;",
    "  white-space:nowrap; color:inherit;",
    "}",
    "/* Mobile date-row auth: text links beside date (hidden on desktop). */",
    ".ynow-auth-links--date-row {",
    "  display:none; flex:0 0 auto; margin:0; padding:0;",
    "  align-items:center; gap:6px;",
    "}",
    ".ynow-auth-links--date-row .ynow-auth-pair--date-text {",
    "  display:inline-flex; flex-direction:row; align-items:center;",
    "  gap:0; margin:0; padding:0;",
    "}",
    # Guest: style Sign in only. Never !important-display on Sign out (keeps inline none).
    ".ynow-auth-links--date-row a.ynow-auth-open {",
    "  display:inline !important; height:auto !important; min-height:0 !important;",
    "  padding:0 !important; margin:0 !important; border:0 !important;",
    "  border-radius:0 !important; box-shadow:none !important;",
    "  background:transparent !important; color:rgba(255,255,255,0.88) !important;",
    "  font-size:12px !important; font-weight:500 !important; line-height:1.2 !important;",
    "  text-decoration:none !important; white-space:nowrap !important;",
    "}",
    ".ynow-auth-links--date-row a.ynow-auth-signout {",
    "  height:auto !important; min-height:0 !important;",
    "  padding:0 !important; margin:0 !important; border:0 !important;",
    "  border-radius:0 !important; box-shadow:none !important;",
    "  background:transparent !important; color:rgba(255,255,255,0.88) !important;",
    "  font-size:12px !important; font-weight:500 !important; line-height:1.2 !important;",
    "  text-decoration:none !important; white-space:nowrap !important;",
    "}",
    ".ynow-auth-links--date-row a.ynow-auth-open:hover,",
    ".ynow-auth-links--date-row a.ynow-auth-signout:hover {",
    "  color:#fff !important; text-decoration:underline !important;",
    "  background:transparent !important;",
    "}",
    "body.ynow-authed .ynow-auth-links--date-row a.ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links--date-row a.ynow-auth-signout {",
    "  display:inline !important; color:rgba(255,255,255,0.88) !important;",
    "}",
    "@media (max-width:767px) {",
    "  .ynow-auth-links--date-row { display:inline-flex !important; }",
    "  .main-header .navbar-custom-menu .navbar-nav > li.ynow-auth-header {",
    "    display:none !important; width:0 !important; min-width:0 !important;",
    "    max-width:0 !important; padding:0 !important; margin:0 !important;",
    "    overflow:hidden !important; pointer-events:none !important;",
    "  }",
    "}",
    "/* Customize / Done — same row as date; signed-in only. */",
    ".ynow-sidebar-reorder-btn {",
    "  display:none !important; margin-left:auto; flex:0 0 auto;",
    "  align-items:center; justify-content:center; gap:4px;",
    "  height:22px; min-width:22px; padding:0 6px; border:1px solid rgba(255,255,255,0.28);",
    "  border-radius:3px; background:rgba(255,255,255,0.06); color:rgba(255,255,255,0.92);",
    "  font-size:11px; font-weight:600; line-height:1; cursor:pointer;",
    "  white-space:nowrap; -webkit-appearance:none; appearance:none;",
    "}",
    ".ynow-sidebar-reorder-btn .fa, .ynow-sidebar-reorder-btn .fas { font-size:11px; }",
    ".ynow-sidebar-reorder-btn-label { font-size:11px; }",
    "body.ynow-authed .ynow-sidebar-reorder-btn { display:inline-flex !important; }",
    "body.ynow-sidebar-reorder-edit .ynow-sidebar-reorder-btn {",
    "  border-color:rgba(255,214,102,0.75); background:rgba(255,214,102,0.18);",
    "  color:#ffe9a8;",
    "}",
    "body.ynow-sidebar-reorder-edit .sidebar-menu > li > a {",
    "  cursor: grab; touch-action: none; user-select: none; -webkit-user-select: none;",
    "}",
    "body.ynow-sidebar-reorder-edit .sidebar-menu > li.ynow-sidebar-reorder-dragging {",
    "  opacity: 0.55;",
    "}",
    "body.ynow-sidebar-reorder-edit .sidebar-menu > li.ynow-sidebar-reorder-dragging > a {",
    "  cursor: grabbing;",
    "}",
    "body.ynow-sidebar-reorder-edit .sidebar-menu > li.ynow-sidebar-reorder-drop-target > a {",
    "  box-shadow: inset 0 2px 0 0 rgba(255,214,102,0.85);",
    "}",
    "/* Legacy page-pin tray (v25.93 misread) — hidden; tab reorder owns long-press. */",
    ".ynow-sidebar-pins { display:none !important; }",
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
    ".ynow-auth-pair {",
    "  display:inline-flex; flex-direction:row; align-items:stretch;",
    "  justify-content:flex-start; gap:6px; flex-wrap:nowrap; margin:0; padding:0;",
    "}",
    ".ynow-auth-links .ynow-auth-pair a.ynow-auth-btn {",
    "  display:inline-flex !important; align-items:center !important; justify-content:center !important;",
    "  box-sizing:border-box !important; height:28px !important; min-height:28px !important;",
    "  padding:0 10px !important; margin:0 !important; border-radius:2px !important;",
    "  font-size:12px !important; font-weight:700 !important; line-height:1 !important;",
    "  text-decoration:none !important; white-space:nowrap !important;",
    "}",
    ".ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-open {",
    "  color:#fff !important; background:transparent !important;",
    "  border:1px solid rgba(255,255,255,0.55) !important;",
    "}",
    ".ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-open:hover {",
    "  color:#fff !important; background:rgba(255,255,255,0.12) !important;",
    "  text-decoration:none !important;",
    "}",
    ".ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-register-open {",
    "  color:#1a1a1a !important; background:#fff !important; border:1px solid #fff !important;",
    "}",
    # Home: plain blue underlined text links (match .ynow-home-legal-link), not buttons.
    ".ynow-auth-links--home .ynow-auth-pair--home-text {",
    "  display:inline-flex; flex-direction:row; align-items:center;",
    "  justify-content:center; gap:0; flex-wrap:wrap;",
    "}",
    ".ynow-auth-links--home .ynow-auth-pair-sep {",
    "  display:inline-block; margin:0 6px; color:#0b57d0; font-size:12px;",
    "  line-height:1.4; user-select:none;",
    "}",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-link,",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-open,",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-register-open {",
    "  display:inline !important; height:auto !important; min-height:0 !important;",
    "  padding:0 !important; margin:0 !important; border:0 !important;",
    "  border-radius:0 !important; box-shadow:none !important;",
    "  background:transparent !important; color:#0b57d0 !important;",
    "  font-size:12px !important; font-weight:500 !important; line-height:1.4 !important;",
    "  text-decoration:underline !important; text-underline-offset:2px !important;",
    "  white-space:nowrap !important;",
    "}",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-link:hover,",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-open:hover,",
    ".ynow-auth-links--home .ynow-auth-pair a.ynow-auth-register-open:hover {",
    "  color:#0842a0 !important; background:transparent !important;",
    "}",
    ".ynow-auth-pair--gate {",
    "  display:flex; flex-direction:row; align-items:stretch; gap:8px;",
    "  margin-top:12px; width:100%; max-width:420px;",
    "}",
    ".ynow-auth-pair--gate > .btn,",
    ".ynow-auth-pair--gate > .ynow-auth-btn {",
    "  flex:1 1 0; height:40px !important; min-height:40px !important;",
    "  margin:0 !important; padding:0 12px !important;",
    "  display:inline-flex !important; align-items:center !important; justify-content:center !important;",
    "  line-height:1.2 !important; white-space:nowrap;",
    "}",
    ".ynow-auth-pair--gate > .ynow-auth-register-open {",
    "  background:#fff !important; color:var(--ynow-ink, #1a1a1a) !important;",
    "  border:1px solid var(--ynow-ink, #1a1a1a) !important;",
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
    # Logged-in / view-as session preview: never show Sign-in modal or open/register
    # chrome. Specificity must beat `.ynow-auth-links .ynow-auth-pair a.ynow-auth-btn`
    # (display:inline-flex !important) — see post-login lock-chrome debug.
    "body.ynow-authed #ynow_auth_modal,",
    "body.ynow-authed #ynow_auth_modal.in,",
    "body.ynow-authed #ynow_auth_modal.show,",
    "body.ynow-view-as #ynow_auth_modal,",
    "body.ynow-view-as #ynow_auth_modal.in,",
    "body.ynow-view-as #ynow_auth_modal.show {",
    "  display:none !important; visibility:hidden !important;",
    "}",
    # Beat guest display:!important (0,3,1) on header btn + home text links.
    # Also cover body.ynow-view-as (sysadmin session preview).
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair-sep,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair a.ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair a.ynow-auth-btn,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-pair a.ynow-auth-btn.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links--sidebar .ynow-auth-pair a.ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-links--date-row .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links--date-row .ynow-auth-pair a.ynow-auth-link,",
    "body.ynow-authed .ynow-auth-links--home .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links--home .ynow-auth-pair a.ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-links--home .ynow-auth-pair a.ynow-auth-link,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-links a.ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-pair a.ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-pair .ynow-auth-pair-sep,",
    "body.ynow-authed .ynow-auth-pair--gate > .ynow-auth-open,",
    "body.ynow-authed .ynow-auth-pair--gate > .ynow-auth-register-open,",
    "body.ynow-authed .ynow-auth-pair--gate > .btn,",
    "body.ynow-authed .ynow-auth-pair--gate > .ynow-auth-btn,",
    "body.ynow-authed .ynow-auth-open,",
    "body.ynow-authed a.ynow-auth-open,",
    "body.ynow-authed .ynow-auth-register-open,",
    "body.ynow-authed a.ynow-auth-register-open,",
    "body.ynow-view-as .ynow-auth-links .ynow-auth-pair,",
    "body.ynow-view-as .ynow-auth-links .ynow-auth-pair-sep,",
    "body.ynow-view-as .ynow-auth-links .ynow-auth-open,",
    "body.ynow-view-as .ynow-auth-links a.ynow-auth-open,",
    "body.ynow-view-as .ynow-auth-links .ynow-auth-register-open,",
    "body.ynow-view-as .ynow-auth-links a.ynow-auth-register-open,",
    "body.ynow-view-as .ynow-auth-pair a.ynow-auth-open,",
    "body.ynow-view-as .ynow-auth-pair a.ynow-auth-register-open,",
    "body.ynow-view-as .ynow-auth-pair .ynow-auth-pair-sep {",
    "  display:none !important; pointer-events:none !important;",
    "}",
    # Force Sign out only (inline style=display:none loses to !important otherwise).
    "body.ynow-authed .ynow-auth-links a.ynow-auth-signout,",
    "body.ynow-authed .ynow-auth-links .ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links a.ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links .ynow-auth-signout {",
    "  display:inline !important; pointer-events:auto !important;",
    "}",
    "body.ynow-authed .ynow-auth-links--sidebar a.ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links--sidebar a.ynow-auth-signout,",
    "body.ynow-authed .ynow-auth-links--date-row a.ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links--date-row a.ynow-auth-signout {",
    "  color:rgba(255,255,255,0.88) !important;",
    "}",
    "body.ynow-authed .ynow-auth-links--home a.ynow-auth-signout,",
    "body.ynow-view-as .ynow-auth-links--home a.ynow-auth-signout {",
    "  color:#0b57d0 !important; text-decoration:underline !important;",
    "  text-underline-offset:2px !important;",
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
    "#ynow_auth_modal.ynow-auth-mode-reset .ynow-auth-signin-block { display:none !important; }",
    "#ynow_auth_modal.ynow-auth-mode-reset .ynow-auth-reset-block { display:block !important; }",
    "#ynow_auth_modal:not(.ynow-auth-mode-reset) .ynow-auth-reset-block { display:none !important; }",
    ".ynow-auth-reset-help { font-size:13px; color:#555; margin:0 0 12px; line-height:1.45; }",
    ".ynow-auth-reset-submit { width:100%; margin-top:8px; }",
    ".ynow-auth-change-pw {",
    "  margin: 16px 0 8px 0;",
    "  padding: 12px 14px;",
    "  border: 1px solid var(--ynow-maint-line, #d5e5dc);",
    "  border-radius: 4px;",
    "  background: #fff;",
    "}",
    ".ynow-auth-change-pw .ynow-auth-change-pw-title {",
    "  margin: 0 0 6px 0; font-size: 15px; font-weight: 600;",
    "}",
    ".ynow-auth-change-pw .ynow-auth-change-pw-help {",
    "  margin: 0 0 10px 0; font-size: 13px; color: #666; line-height: 1.45;",
    "}",
    ".ynow-auth-change-pw .ynow-auth-field { margin: 0 0 10px 0; }",
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
    "  display:inline-flex; align-items:center; justify-content:center;",
    "  width:100%; height:46px; margin:4px 0 0; padding:0;",
    "  border-radius:2px; border:1px solid var(--ynow-ink, #1a1a1a) !important;",
    "  background:var(--ynow-ink, #1a1a1a) !important; color:#fff !important;",
    "  font-size:15px; font-weight:700; line-height:1.2;",
    "  box-shadow:none !important; text-shadow:none;",
    "}",
    "#ynow_auth_modal .ynow-auth-pair--modal {",
    "  display:flex; flex-direction:row; align-items:stretch; gap:8px; width:100%;",
    "}",
    "#ynow_auth_modal .ynow-auth-pair--modal > .ynow-auth-email-submit.btn,",
    "#ynow_auth_modal .ynow-auth-pair--modal > .ynow-auth-register-submit.btn {",
    "  flex:1 1 0; width:auto !important; height:46px !important; min-height:46px !important;",
    "  margin:4px 0 0 !important; padding:0 8px !important;",
    "  display:inline-flex !important; align-items:center !important; justify-content:center !important;",
    "  line-height:1.2 !important; white-space:nowrap;",
    "}",
    "#ynow_auth_modal .ynow-auth-pair--modal > .ynow-auth-register-submit.btn {",
    "  background:#fff !important; color:var(--ynow-ink, #1a1a1a) !important;",
    "  border:1px solid var(--ynow-ink, #1a1a1a) !important;",
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
    "body.ynow-lite .sidebar-menu a[data-value=\"settings\"],",
    "body.ynow-lite .sidebar-menu li:has(> a[data-value=\"settings\"]),",
    "body.ynow-lite .sidebar-menu li.treeview:has(a[data-value=\"account_admin\"]),",
    "body.ynow-lite .sidebar-menu li.treeview:has(a[data-value=\"account_points\"]) {",
    "  display:none !important;",
    "}",
    # Account / Settings are signed-in workspace, not guest products. Hide until sign-in.
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li.treeview:has(a[data-value=\"account_admin\"]),",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li.treeview:has(a[data-value=\"account_points\"]),",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu a[data-value=\"settings\"],",
    "body.ynow-auth-feature:not(.ynow-authed) .sidebar-menu li:has(> a[data-value=\"settings\"]) {",
    "  display:none !important;",
    "}",
    # Signed-in: white separator under About before Settings (Account sits below).
    "body.ynow-authed:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value=\"settings\"]) {",
    "  position:relative;",
    "  margin-top:8px;",
    "  padding-top:8px;",
    "  border-top:none;",
    "}",
    "body.ynow-authed:not(.ynow-lite) .sidebar-menu > li:has(> a[data-value=\"settings\"])::before {",
    "  content:\"\";",
    "  position:absolute;",
    "  top:0;",
    "  left:12%;",
    "  right:12%;",
    "  border-top:1px solid #ffffff;",
    "  pointer-events:none;",
    "}",
    # Opt-in: hide still-locked sidebar items (Settings → 快速隱藏鎖住項目).
    "body.ynow-authed.ynow-hide-locked-nav:not(.ynow-lite) .sidebar-menu li.ynow-sidebar-locked {",
    "  display:none !important;",
    "}",
    # Opt-in: denser sidebar spacing.
    "body.ynow-authed.ynow-sidebar-compact:not(.ynow-lite) .sidebar-menu > li > a {",
    "  padding-top:8px !important;",
    "  padding-bottom:8px !important;",
    "}",
    ".ynow-settings-panel { max-width:720px; margin:12px auto; }",
    ".ynow-settings-panel .ynow-settings-row { margin:0 0 14px; }",
    ".ynow-settings-panel .ynow-settings-help { color:#666; font-size:13px; margin:4px 0 0; }",
    ".ynow-settings-panel .ynow-settings-actions { margin-top:18px; }",
    ".ynow-settings-gain-swatches { display:inline-flex; gap:6px; margin:6px 0 0; vertical-align:middle; }",
    ".ynow-settings-gain-swatches .ynow-gain-swatch {",
    "  display:inline-block; width:18px; height:18px; border-radius:3px;",
    "  border:1px solid rgba(0,0,0,0.18);",
    "}",
    ".ynow-settings-gain-swatches .ynow-gain-swatch-up { background: var(--ynow-chg-up, #1e7a46); }",
    ".ynow-settings-gain-swatches .ynow-gain-swatch-down { background: var(--ynow-chg-down, #c0392b); }",
    ".ynow-pts-topup {",
    "  margin: 16px 0 4px 0;",
    "  padding: 12px 14px;",
    "  border: 1px dashed var(--ynow-maint-line, #d5e5dc);",
    "  border-radius: 4px;",
    "  background: var(--ynow-maint-wash, #f3f8f6);",
    "}",
    ".ynow-pts-topup .ynow-pts-topup-title {",
    "  margin: 0 0 6px 0;",
    "  font-size: 15px;",
    "  font-weight: 600;",
    "  color: var(--ynow-maint-ink, #1a1a1a);",
    "}",
    ".ynow-pts-topup .ynow-pts-topup-copy {",
    "  margin: 0 0 10px 0;",
    "  font-size: 13px;",
    "  line-height: 1.5;",
    "  color: var(--ynow-maint-muted, #666);",
    "}",
    ".ynow-pts-topup .ynow-pts-topup-cta[disabled],",
    ".ynow-pts-topup .ynow-pts-topup-cta:disabled {",
    "  cursor: not-allowed;",
    "  opacity: 0.7;",
    "  pointer-events: none;",
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
    ".ynow-maint-actions-auth {",
    "  display: inline-flex;",
    "  flex-direction: row;",
    "  align-items: stretch;",
    "  flex-wrap: nowrap;",
    "  gap: 8px;",
    "}",
    ".ynow-maint-actions-auth > .btn {",
    "  margin: 0 !important;",
    "  min-height: 30px;",
    "  height: 30px;",
    "  line-height: 1.2;",
    "  box-sizing: border-box;",
    "  display: inline-flex;",
    "  align-items: center;",
    "  justify-content: center;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-danger,",
    ".ynow-maint-shell .btn.btn-danger.ynow-maint-btn-danger {",
    "  background: #c0392b !important;",
    "  border: 1px solid #a93226 !important;",
    "  color: #fff !important;",
    "  box-shadow: none !important;",
    "}",
    ".ynow-maint-shell .btn.ynow-maint-btn-danger:hover,",
    ".ynow-maint-shell .btn.ynow-maint-btn-danger:focus {",
    "  background: #a93226 !important;",
    "  border-color: #922b21 !important;",
    "  color: #fff !important;",
    "}",
    "#ynow_auth_delete_confirm[disabled],",
    "#ynow_auth_delete_confirm.disabled {",
    "  opacity: 0.45;",
    "  cursor: not-allowed;",
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

#' Sidebar: Settings (signed-in Full only; CSS hides for guests / Lite).
ynow_settings_menu_item <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  menuItem(
    text = tags$span(id = "ynow_menu_settings", "Settings"),
    tabName = "settings",
    icon = icon("cog")
  )
}

#' Sidebar: Account tree with Overview + Points ledger (Full; guests see locked/dimmed).
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

#' Tab shells for Settings + Account overview + Points ledger (bodies filled by server).
ynow_admin_tab_ui <- function() {
  if (!isTRUE(ynow_auth_enabled())) return(NULL)
  tagList(
    tabItem(
      tabName = "settings",
      uiOutput("ynow_settings_panel")
    ),
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

#' Settings panel (signed-in). Flat block — no AdminLTE box.
#' @param settings list from ynow_prefs_get_user_settings()
ynow_settings_panel_ui <- function(settings = NULL, locale = "zh-TW") {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  s <- if (exists("ynow_prefs_normalize_user_settings", mode = "function")) {
    ynow_prefs_normalize_user_settings(settings)
  } else {
    list(
      hide_locked_sidebar = FALSE,
      preferred_ui_locale = "auto",
      preferred_market = "auto",
      sidebar_compact = FALSE,
      gain_loss_scheme = "auto"
    )
  }
  hide_on <- isTRUE(s$hide_locked_sidebar)
  compact_on <- isTRUE(s$sidebar_compact)
  loc_sel <- as.character(s$preferred_ui_locale %||% "auto")[1]
  mkt_sel <- as.character(s$preferred_market %||% "auto")[1]
  gain_sel <- if (exists("ynow_prefs_normalize_gain_loss_scheme", mode = "function")) {
    ynow_prefs_normalize_gain_loss_scheme(s$gain_loss_scheme %||% "auto")
  } else {
    as.character(s$gain_loss_scheme %||% "auto")[1]
  }
  tags$div(
    class = "ynow-settings-panel ynow-maint-shell",
    `data-ynow-maint-theme` = "logo",
    tags$div(
      class = "ynow-maint-block",
      tags$div(
        class = "ynow-maint-title-row",
        tags$div(
          class = "ynow-maint-title-left",
          tags$h4(
            id = "ynow_settings_box_title",
            .lab("settings_title", "Settings")
          ),
          tags$p(
            id = "ynow_settings_intro",
            class = "ynow-settings-help",
            .lab(
              "settings_intro",
              "Preferences for this signed-in account. Saved settings reload on the next sign-in."
            )
          )
        )
      ),
      tags$div(
        class = "ynow-settings-row",
        checkboxInput(
          "ynow_settings_hide_locked",
          label = tags$span(
            id = "ynow_settings_hide_locked_label",
            .lab("settings_hide_locked", "Quick-hide locked items")
          ),
          value = hide_on
        ),
        tags$p(
          id = "ynow_settings_hide_locked_help",
          class = "ynow-settings-help",
          .lab(
            "settings_hide_locked_help",
            "When on, hide sidebar items that stay locked after sign-in (points-gated / ACL). When off, show the same lock icon and gray chrome as guests."
          )
        )
      ),
      tags$div(
        class = "ynow-settings-row",
        selectInput(
          "ynow_settings_ui_locale",
          label = tags$span(
            id = "ynow_settings_ui_locale_label",
            .lab("settings_ui_locale", "Preferred language")
          ),
          choices = stats::setNames(
            c("auto", "zh-TW", "en"),
            c(
              .lab("settings_opt_auto", "Follow device / header"),
              .lab("settings_opt_zh", "Traditional Chinese (Taiwan)"),
              .lab("settings_opt_en", "English (US)")
            )
          ),
          selected = loc_sel,
          width = "100%"
        )
      ),
      tags$div(
        class = "ynow-settings-row",
        selectInput(
          "ynow_settings_market",
          label = tags$span(
            id = "ynow_settings_market_label",
            .lab("settings_market", "Preferred market on sign-in")
          ),
          choices = stats::setNames(
            c("auto", "US", "TW"),
            c(
              .lab("settings_opt_auto", "Follow device / header"),
              .lab("settings_opt_us", "US equities"),
              .lab("settings_opt_tw", "TW equities")
            )
          ),
          selected = mkt_sel,
          width = "100%"
        )
      ),
      tags$div(
        class = "ynow-settings-row",
        checkboxInput(
          "ynow_settings_sidebar_compact",
          label = tags$span(
            id = "ynow_settings_sidebar_compact_label",
            .lab("settings_sidebar_compact", "Compact sidebar spacing")
          ),
          value = compact_on
        ),
        tags$p(
          id = "ynow_settings_sidebar_compact_help",
          class = "ynow-settings-help",
          .lab(
            "settings_sidebar_compact_help",
            "Slightly tighter vertical padding on top-level sidebar tabs."
          )
        )
      ),
      tags$div(
        class = "ynow-settings-row",
        selectInput(
          "ynow_settings_gain_loss",
          label = tags$span(
            id = "ynow_settings_gain_loss_label",
            .lab("settings_gain_loss", "Gain / loss colors")
          ),
          choices = stats::setNames(
            c("auto", "green_red", "red_green", "blue_red", "red_blue"),
            c(
              .lab("settings_gain_opt_auto", "Follow market (US green-up / TW red-up)"),
              .lab("settings_gain_opt_green_red", "Green up / red down"),
              .lab("settings_gain_opt_red_green", "Red up / green down"),
              .lab("settings_gain_opt_blue_red", "Blue up / red down"),
              .lab("settings_gain_opt_red_blue", "Red up / blue down")
            )
          ),
          selected = gain_sel,
          width = "100%"
        ),
        tags$p(
          id = "ynow_settings_gain_loss_help",
          class = "ynow-settings-help",
          .lab(
            "settings_gain_loss_help",
            "Applies to price-change chrome (tables, badges, and move-colored charts). Choose green/red, blue/red, or a swapped pair."
          )
        ),
        tags$div(
          class = "ynow-settings-gain-swatches",
          `aria-hidden` = "true",
          tags$span(class = "ynow-gain-swatch ynow-gain-swatch-up"),
          tags$span(class = "ynow-gain-swatch ynow-gain-swatch-down")
        )
      ),
      tags$div(
        class = "ynow-settings-actions",
        actionButton(
          "ynow_settings_save",
          label = tags$span(
            id = "ynow_settings_save_label",
            .lab("settings_save", "Save settings")
          ),
          class = "btn-primary"
        )
      )
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

#' Delete-account confirm dialog: type DELETE (all caps) before confirm enables.
ynow_delete_account_modal <- function(locale = "zh-TW") {
  loc <- as.character(locale %||% "zh-TW")[1]
  .lab <- function(key, fb) tryCatch(ui_str(key, loc), error = function(e) fb)
  token <- if (exists("ynow_auth_delete_confirm_token", mode = "function")) {
    ynow_auth_delete_confirm_token()
  } else {
    "DELETE"
  }
  modalDialog(
    title = tags$span(
      id = "ynow_auth_delete_modal_title",
      .lab("auth_delete_modal_title", "Delete account permanently?")
    ),
    tags$div(
      class = "ynow-auth-delete-modal",
      tags$p(
        id = "ynow_auth_delete_modal_warn",
        class = "text-danger",
        .lab(
          "auth_delete_modal_warn",
          paste0(
            "This permanently deletes your account and related data ",
            "(FYN/PYN points, preferences, access locks, Value Alerts). ",
            "This cannot be undone."
          )
        )
      ),
      tags$p(
        id = "ynow_auth_delete_modal_type",
        .lab(
          "auth_delete_modal_type",
          "Type DELETE (all caps) to confirm."
        )
      ),
      textInput(
        "ynow_auth_delete_confirm_text",
        label = NULL,
        value = "",
        placeholder = token
      )
    ),
    footer = tagList(
      modalButton(.lab("auth_delete_modal_cancel", "Cancel")),
      tags$button(
        id = "ynow_auth_delete_confirm",
        type = "button",
        class = "btn btn-danger action-button",
        disabled = "disabled",
        tags$span(
          id = "ynow_auth_delete_confirm_label",
          .lab("auth_delete_modal_confirm", "Permanently delete")
        )
      )
    ),
    easyClose = TRUE,
    fade = TRUE,
    tags$script(HTML(paste0(
      "(function(){",
      "var tok=", jsonlite::toJSON(token, auto_unbox = TRUE), ";",
      "function sync(){",
      "  var inp=document.getElementById('ynow_auth_delete_confirm_text');",
      "  var btn=document.getElementById('ynow_auth_delete_confirm');",
      "  if(!btn) return;",
      "  var ok=inp && String(inp.value||'').trim()===tok;",
      "  if(ok){btn.removeAttribute('disabled');btn.classList.remove('disabled');}",
      "  else{btn.setAttribute('disabled','disabled');btn.classList.add('disabled');}",
      "}",
      "var inp=document.getElementById('ynow_auth_delete_confirm_text');",
      "if(inp){",
      "  inp.addEventListener('input',sync);",
      "  inp.addEventListener('keyup',sync);",
      "  sync();",
      "  setTimeout(function(){try{inp.focus();}catch(e){}},80);",
      "}",
      "})();"
    )))
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
              tags$code(id = "ynow_pts_pyn_value", class = "ynow-pyn-num", pyn_txt)
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
          tags$div(
            class = "ynow-pts-topup",
            id = "ynow_pts_topup_slot",
            tags$h5(
              class = "ynow-pts-topup-title",
              id = "ynow_pts_topup_title",
              .lab("pts_topup_title", "Points top-up")
            ),
            tags$p(
              class = "ynow-pts-topup-copy",
              id = "ynow_pts_topup_copy",
              .lab(
                "pts_topup_copy",
                "A paid points top-up lane will land here. Checkout is not available yet — no charge will be taken from this page."
              )
            ),
            tags$button(
              id = "ynow_pts_topup_cta",
              type = "button",
              class = "btn btn-default btn-sm ynow-pts-topup-cta",
              disabled = "disabled",
              `aria-disabled` = "true",
              tags$span(
                id = "ynow_pts_topup_cta_label",
                .lab("pts_topup_cta", "Coming soon")
              )
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
            class = "ynow-auth-change-pw",
            id = "ynow_auth_change_pw_slot",
            tags$h5(
              class = "ynow-auth-change-pw-title",
              id = "ynow_auth_change_pw_title",
              .lab("auth_change_pw_title", "Change password")
            ),
            tags$p(
              class = "ynow-auth-change-pw-help",
              id = "ynow_auth_change_pw_help",
              .lab(
                "auth_change_pw_help",
                "Update the Email sign-in password for this account. Social-only accounts can set a first password here without a current password."
              )
            ),
            tags$div(
              class = "ynow-auth-field",
              tags$label(
                `for` = "ynow_auth_change_current",
                tags$span(id = "ynow_auth_change_current_label", .lab("auth_change_pw_current", "Current password"))
              ),
              tags$input(
                id = "ynow_auth_change_current",
                type = "password",
                class = "form-control ynow-auth-input",
                value = "",
                autocomplete = "current-password"
              )
            ),
            tags$div(
              class = "ynow-auth-field",
              tags$label(
                `for` = "ynow_auth_change_new",
                tags$span(id = "ynow_auth_change_new_label", .lab("auth_change_pw_new", "New password"))
              ),
              tags$input(
                id = "ynow_auth_change_new",
                type = "password",
                class = "form-control ynow-auth-input",
                value = "",
                autocomplete = "new-password"
              )
            ),
            tags$div(
              class = "ynow-auth-field",
              tags$label(
                `for` = "ynow_auth_change_new2",
                tags$span(id = "ynow_auth_change_new2_label", .lab("auth_change_pw_confirm", "Confirm new password"))
              ),
              tags$input(
                id = "ynow_auth_change_new2",
                type = "password",
                class = "form-control ynow-auth-input",
                value = "",
                autocomplete = "new-password"
              )
            ),
            actionButton(
              "ynow_auth_change_submit",
              label = tags$span(
                id = "ynow_auth_change_submit_label",
                .lab("auth_change_pw_submit", "Update password")
              ),
              class = "btn-primary btn-sm ynow-maint-btn-primary"
            ),
            uiOutput("ynow_auth_change_status")
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
            tags$div(
              class = "ynow-maint-actions-auth",
              actionButton(
                "ynow_auth_sign_out_admin",
                label = tags$span(
                  id = "ynow_admin_sign_out_label",
                  .lab("auth_sign_out", "Sign out")
                ),
                class = "btn-warning btn-sm ynow-maint-btn-warn"
              ),
              actionButton(
                "ynow_auth_delete_account",
                label = tags$span(
                  id = "ynow_admin_delete_account_label",
                  class = "ynow-auth-delete-account-label",
                  .lab("auth_delete_account", "Delete account")
                ),
                class = "btn-danger btn-sm ynow-maint-btn-danger"
              )
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
              tags$code(id = "ynow_pts_ledger_pyn_value", class = "ynow-pyn-num", pyn_txt)
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
#' Demo clips live in www/demos/<feature>.{mp4,webm} (~10s; regenerate with
#' scripts/generate_gate_demo_videos.py).
#' Membership gates (A/B) come from membership_locks.R; ops_acl / sysadmin gate
#' ids (snap_defaults, inner_lock, atx) are non-membership but still listed for demos.
ynow_gate_features <- function() {
  mem <- if (exists("ynow_membership_gate_features", mode = "function")) {
    ynow_membership_gate_features()
  } else {
    c("lab", "checklist", "alerts", "account", "pdf")
  }
  ops <- if (exists("ynow_ops_gate_features", mode = "function")) {
    ynow_ops_gate_features()
  } else {
    c("snap_defaults", "inner_lock", "atx")
  }
  unique(c(mem, ops))
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
#' @param compact tighter layout for constrained surfaces
ynow_gate_promo_faq_ui <- function(feature = "lab", include_faq = TRUE, compact = FALSE) {
  keys <- ynow_gate_promo_keys(feature)
  feature <- keys$feature
  demo_base <- paste0("demos/", feature)
  tier_attr <- if (exists("ynow_membership_lock_tier_attr", mode = "function")) {
    ynow_membership_lock_tier_attr(feature)
  } else {
    ""
  }
  scope_attr <- if (!nzchar(tier_attr) &&
      exists("ynow_non_membership_lock_catalog", mode = "function")) {
    nm <- ynow_non_membership_lock_catalog()
    hit <- nm[nm$id == feature, , drop = FALSE]
    if (nrow(hit)) as.character(hit$scope[[1]]) else "ops_acl"
  } else {
    NULL
  }
  tags$div(
    class = paste(
      "ynow-gate-extras",
      if (isTRUE(compact)) "ynow-gate-extras--compact" else NULL
    ),
    `data-ynow-gate-feature` = feature,
    `data-ynow-lock-tier` = if (nzchar(tier_attr)) tier_attr else NULL,
    `data-ynow-lock-scope` = scope_attr,
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
  tier_attr <- if (exists("ynow_membership_lock_tier_attr", mode = "function")) {
    ynow_membership_lock_tier_attr(keys$feature)
  } else {
    "A"
  }
  tags$div(
    id = gate_id,
    class = "box box-solid ynow-member-gate ynow-gate-box",
    `data-ynow-gate-feature` = keys$feature,
    `data-ynow-lock-tier` = if (nzchar(tier_attr)) tier_attr else NULL,
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
      tags$div(
        class = "ynow-auth-pair ynow-auth-pair--gate",
        actionButton(
          btn_id,
          label = tags$span(
            id = paste0(btn_id, "_label"),
            class = "ynow-member-gate-signin-label",
            "Sign in"
          ),
          class = "btn-primary ynow-auth-open ynow-auth-btn"
        ),
        actionButton(
          paste0(btn_id, "_register"),
          label = tags$span(
            id = paste0(btn_id, "_register_label"),
            class = "ynow-auth-register-label",
            "Register"
          ),
          class = "btn-default ynow-auth-register-open ynow-auth-btn"
        )
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
  # Session-only recovery access_token (never logged / never sent to client).
  recovery_access_token <- reactiveVal("")
  # Opaque browser-resume token for this Shiny session (revoked on Sign out).
  resume_token_held <- reactiveVal("")

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

  .set_auth_modal_reset_mode <- function(on = TRUE) {
    session$sendCustomMessage(
      "ynowAuthResetMode",
      list(on = isTRUE(on))
    )
  }

  output$ynow_auth_status <- renderUI({
    if (!isTRUE(ynow_auth_enabled())) return(NULL)
    NULL
  })
  output$ynow_auth_change_status <- renderUI({ NULL })

  .push_auth_state <- function(logged_in = FALSE, email = "") {
    session$sendCustomMessage(
      "ynowAuthState",
      list(
        logged_in = isTRUE(logged_in),
        email = as.character(email %||% "")[1],
        label_sign_in = .str("auth_sign_in", "Sign in"),
        label_sign_out = .str("auth_sign_out", "Sign out"),
        label_register = .str("auth_register_btn", "Register")
      )
    )
  }

  # Local-only: seed a signed-in session so gate-demo capture can scroll real tab UI.
  # Never set YNOW_DEMO_CAPTURE=1 on shinyapps.io / production.
  if (identical(Sys.getenv("YNOW_DEMO_CAPTURE"), "1")) {
    session$onFlushed(function() {
      tryCatch({
        # Valid UUID shape so points/prefs DB helpers do not error on type uuid.
        u <- list(
          id = "00000000-0000-4000-8000-0000000000d1",
          email = "demo.capture@local",
          display_name = "Demo Capture",
          role = "user",
          logged_in_at = as.character(Sys.time())
        )
        account_user(u)
        .push_auth_state(TRUE, u$email)
      }, error = function(e) NULL)
    }, once = TRUE)
  }

  .persist_auth_browser <- function(user) {
    uid <- as.character(user$id %||% "")[1]
    if (!nzchar(uid) || !exists("ynow_auth_session_mint", mode = "function")) return()
    minted <- tryCatch(ynow_auth_session_mint(uid), error = function(e) NULL)
    tok <- as.character(minted$token %||% "")[1]
    if (!isTRUE(minted$ok) || !nzchar(tok)) return()
    resume_token_held(tok)
    tryCatch(
      session$sendCustomMessage("ynowAuthPersist", list(token = tok, clear = FALSE)),
      error = function(e) NULL
    )
  }

  .clear_auth_browser <- function(revoke_held = TRUE) {
    tok <- as.character(isolate(resume_token_held()) %||% "")[1]
    if (isTRUE(revoke_held) && nzchar(tok) &&
          exists("ynow_auth_session_revoke", mode = "function")) {
      tryCatch(ynow_auth_session_revoke(tok), error = function(e) NULL)
    }
    resume_token_held("")
    tryCatch(
      session$sendCustomMessage("ynowAuthPersist", list(token = "", clear = TRUE)),
      error = function(e) NULL
    )
  }

  .persist_last_ui_surface <- function(user = NULL, tab = NULL, ticker = NULL) {
    if (!exists("ynow_prefs_set_last_ui_surface", mode = "function") &&
          !exists("ynow_prefs_update_last_ui_surface", mode = "function")) {
      return(invisible(NULL))
    }
    u <- user
    if (is.null(u)) u <- tryCatch(isolate(account_user()), error = function(e) NULL)
    uid <- as.character(u$id %||% "")[1]
    if (!nzchar(uid)) return(invisible(NULL))
    tab_now <- as.character(tab %||% isolate(input$sidebar_tabs) %||% "")[1]
    tk_now <- as.character(ticker %||% "")[1]
    if (!nzchar(tk_now) && exists("ynow_prefs_get_last_ui_surface", mode = "function")) {
      # Keep prior ticker when only tab is known at this call site.
      prev <- tryCatch(ynow_prefs_get_last_ui_surface(uid), error = function(e) NULL)
      if (is.list(prev)) tk_now <- as.character(prev$ticker %||% "")[1]
    }
    tryCatch({
      if (exists("ynow_prefs_update_last_ui_surface", mode = "function")) {
        ynow_prefs_update_last_ui_surface(uid, tab = tab_now, ticker = tk_now)
      } else {
        ynow_prefs_set_last_ui_surface(uid, list(tab = tab_now, ticker = tk_now))
      }
    }, error = function(e) NULL)
    invisible(NULL)
  }

  .load_last_ui_surface <- function(user = NULL) {
    empty <- list(tab = "", ticker = "")
    if (!exists("ynow_prefs_get_last_ui_surface", mode = "function")) return(empty)
    u <- user
    if (is.null(u)) u <- tryCatch(isolate(account_user()), error = function(e) NULL)
    uid <- as.character(u$id %||% "")[1]
    if (!nzchar(uid)) return(empty)
    tryCatch(
      ynow_prefs_get_last_ui_surface(uid),
      error = function(e) empty
    )
  }

  .sign_out <- function() {
    # Keep last member screen for the next sign-in / resume (prefs survive logout).
    tryCatch(.persist_last_ui_surface(), error = function(e) NULL)
    .clear_auth_browser(TRUE)
    account_user(NULL)
    .push_auth_state(FALSE, "")
  }

  #' Collapse sidebar first (client), then open the login modal.
  auth_open_mode <- "signin"
  .open_auth_modal <- function() {
    if (!isTRUE(ynow_auth_enabled())) return()
    # Already signed in: ignore stale Sign-in actionLink clicks (display:none links still fire).
    if (!is.null(account_user())) return()
    mode <- if (identical(auth_open_mode, "register")) "register" else "signin"
    auth_open_mode <<- "signin"
    # Non-empty payload: some Shiny/json paths drop empty list() customs.
    session$sendCustomMessage("ynowAuthOpenModal", list(ok = TRUE, mode = mode))
  }
  .open_auth_modal_register <- function() {
    auth_open_mode <<- "register"
    .open_auth_modal()
  }
  observeEvent(input$ynow_auth_open, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_open_date, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_open_home, {
    .open_auth_modal()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_register_open, {
    .open_auth_modal_register()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_register_open_home, {
    .open_auth_modal_register()
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
    .persist_auth_browser(user)
    tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "close"), error = function(e) NULL)
    tryCatch(
      showNotification(msg, type = "message", duration = 5),
      error = function(e) NULL
    )
    saved <- .load_last_ui_surface(user)
    tab <- as.character(saved$tab %||% "")[1]
    # Prefer durable last member screen over same-session guest pre-auth tab.
    if (!nzchar(tab)) {
      tab <- as.character(restore_tab %||% "")[1]
    }
    if (!nzchar(tab)) {
      tab <- as.character(isolate(input$sidebar_tabs) %||% "")[1]
    }
    # Sys. Admin (testing) is ops-only — never keep a signed-in user on that tab.
    if (identical(tab, "testing")) tab <- "home"
    ticker <- as.character(saved$ticker %||% "")[1]
    session$sendCustomMessage(
      "ynowAuthPostLogin",
      list(
        ok = TRUE,
        restore_tab = tab,
        restore_ticker = ticker,
        message = msg
      )
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
    # Remember the surface we just restored (or current) as the new baseline.
    tryCatch(
      .persist_last_ui_surface(user, tab = tab, ticker = ticker),
      error = function(e) NULL
    )
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

  # Browser localStorage resume after tab / browser close (quiet; no toast).
  observeEvent(input$ynow_auth_resume_token, {
    if (!isTRUE(ynow_auth_enabled())) return()
    if (!is.null(isolate(account_user()))) return()
    tok <- as.character(input$ynow_auth_resume_token %||% "")[1]
    if (!nzchar(tok) || !exists("ynow_auth_session_resume", mode = "function")) return()
    res <- tryCatch(ynow_auth_session_resume(tok), error = function(e) NULL)
    if (isTRUE(res$ok) && is.list(res$user)) {
      resume_token_held(tok)
      account_user(res$user)
      .push_auth_state(TRUE, res$user$email %||% "")
      # Daily check-in / first gift still run on resume when eligible.
      if (exists("ynow_points_on_login", mode = "function")) {
        uid <- as.character(res$user$id %||% "")[1]
        if (nzchar(uid)) {
          tryCatch(ynow_points_on_login(uid), error = function(e) NULL)
        }
      }
      # Restore last member screen with the authenticated shell (no toast).
      saved <- .load_last_ui_surface(res$user)
      tab <- as.character(saved$tab %||% "")[1]
      if (identical(tab, "testing")) tab <- "home"
      ticker <- as.character(saved$ticker %||% "")[1]
      if (nzchar(tab) || nzchar(ticker)) {
        session$sendCustomMessage(
          "ynowAuthPostLogin",
          list(
            ok = TRUE,
            restore_tab = tab,
            restore_ticker = ticker,
            message = "",
            quiet = TRUE
          )
        )
      }
      if (nzchar(tab) &&
            !identical(tab, as.character(isolate(input$sidebar_tabs) %||% "")[1])) {
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
    } else {
      .clear_auth_browser(FALSE)
    }
  }, ignoreInit = TRUE)

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
  # Recovery links (type=recovery) open set-password UI instead of auto sign-in.
  # Self-serve social: upsert into Neon (role=user) and sign in — no admin permission step.
  observeEvent(input$ynow_auth_oauth_callback, {
    if (!isTRUE(ynow_auth_enabled())) return()
    payload <- input$ynow_auth_oauth_callback
    restore_tab <- as.character(payload$restore_tab %||% "")[1]
    cb_type <- tolower(trimws(as.character(payload$type %||% "")[1]))
    access <- as.character(payload$access_token %||% "")[1]
    if (identical(cb_type, "recovery")) {
      # Implicit hash tokens, or PKCE ?code= → exchange for access_token.
      if (!nzchar(access) && exists("ynow_oauth_supabase_exchange_code", mode = "function")) {
        code <- as.character(payload$code %||% "")[1]
        ver <- as.character(payload$code_verifier %||% "")[1]
        if (nzchar(code)) {
          ex <- tryCatch(
            ynow_oauth_supabase_exchange_code(code, ver),
            error = function(e) list(ok = FALSE)
          )
          access <- as.character(ex$access_token %||% "")[1]
        }
      }
      if (!nzchar(access)) {
        .show_auth_note("auth_msg_reset_session", "Reset session expired. Request a new link.", cls = "text-danger")
        return()
      }
      recovery_access_token(access)
      .set_auth_modal_reset_mode(TRUE)
      tryCatch(shinyBS::toggleModal(session, "ynow_auth_modal", "open"), error = function(e) NULL)
      .show_auth_note(
        "auth_msg_reset_ready",
        "Enter and confirm your new password.",
        cls = "text-muted"
      )
      return()
    }
    if (!exists("ynow_oauth_complete", mode = "function")) {
      .show_auth_note("auth_msg_oauth_failed", "Social sign-in failed.")
      return()
    }
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
    if (!exists("ynow_auth_request_password_reset", mode = "function")) {
      .show_auth_note("auth_msg_forgot_soon", "Password reset is not available yet.")
      return()
    }
    res <- ynow_auth_request_password_reset(input$ynow_auth_email)
    mk <- as.character(res$message_key %||% "auth_msg_forgot_soon")[1]
    cls <- if (isTRUE(res$ok) || identical(mk, "auth_msg_forgot_sent")) {
      "text-success"
    } else if (identical(mk, "auth_msg_forgot_email")) {
      "text-danger"
    } else {
      "text-muted"
    }
    fallback <- if (identical(mk, "auth_msg_forgot_sent")) {
      "If an account exists for that email, a reset link has been sent."
    } else if (identical(mk, "auth_msg_forgot_email")) {
      "Enter a valid email address to reset your password."
    } else {
      "Password reset is not available yet."
    }
    .show_auth_note(mk, fallback, cls = cls)
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_reset_submit, {
    if (!isTRUE(ynow_auth_enabled())) return()
    tok <- as.character(recovery_access_token() %||% "")[1]
    if (!nzchar(tok)) {
      .show_auth_note("auth_msg_reset_session", "Reset session expired. Request a new link.", cls = "text-danger")
      .set_auth_modal_reset_mode(FALSE)
      return()
    }
    if (!exists("ynow_auth_complete_password_reset", mode = "function")) {
      .show_auth_note("auth_msg_reset_failed", "Could not update password.", cls = "text-danger")
      return()
    }
    res <- ynow_auth_complete_password_reset(
      tok,
      input$ynow_auth_reset_password,
      input$ynow_auth_reset_password2
    )
    if (isTRUE(res$ok) && !is.null(res$user)) {
      recovery_access_token("")
      .set_auth_modal_reset_mode(FALSE)
      .finish_auth_success(
        res$user,
        message_key = res$message_key %||% "auth_msg_reset_ok",
        fallback = "Password updated. Signed in.",
        restore_tab = as.character(input$ynow_auth_restore_tab %||% "")[1]
      )
    } else {
      mk <- as.character(res$message_key %||% "auth_msg_reset_failed")[1]
      .show_auth_note(mk, "Could not update password.", cls = "text-danger")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_reset_back, {
    if (!isTRUE(ynow_auth_enabled())) return()
    recovery_access_token("")
    .set_auth_modal_reset_mode(FALSE)
    output$ynow_auth_status <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_auth_change_submit, {
    if (!isTRUE(ynow_auth_enabled())) return()
    u <- account_user()
    if (!is.list(u) || !nzchar(as.character(u$id %||% "")[1])) {
      output$ynow_auth_change_status <- renderUI({
        tags$p(class = "text-danger", .str("auth_msg_change_need_login", "Sign in to change your password."))
      })
      return()
    }
    pw <- as.character(input$ynow_auth_change_new %||% "")[1]
    conf <- as.character(input$ynow_auth_change_new2 %||% "")[1]
    if (!identical(pw, conf)) {
      output$ynow_auth_change_status <- renderUI({
        tags$p(class = "text-danger", .str("auth_msg_password_mismatch", "New passwords do not match."))
      })
      return()
    }
    if (!exists("ynow_db_change_password", mode = "function")) {
      output$ynow_auth_change_status <- renderUI({
        tags$p(class = "text-muted", .str("auth_msg_no_backend", "Sign-in backend is not configured yet."))
      })
      return()
    }
    res <- ynow_db_change_password(
      u$id,
      input$ynow_auth_change_current,
      pw
    )
    mk <- as.character(res$message_key %||% "auth_msg_change_failed")[1]
    cls <- if (isTRUE(res$ok)) "text-success" else "text-danger"
    if (isTRUE(res$ok)) {
      # Password change invalidates every durable browser resume for this user.
      uid <- as.character(u$id %||% "")[1]
      if (nzchar(uid) && exists("ynow_auth_session_revoke_user", mode = "function")) {
        tryCatch(ynow_auth_session_revoke_user(uid), error = function(e) NULL)
      }
      .persist_auth_browser(u)
    }
    output$ynow_auth_change_status <- renderUI({
      tags$p(class = cls, .str(mk, if (isTRUE(res$ok)) "Password updated." else "Could not update password."))
    })
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
  observeEvent(input$ynow_auth_sign_out_date, {
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

  observeEvent(input$ynow_auth_delete_account, {
    if (!isTRUE(ynow_auth_enabled())) return()
    u <- account_user()
    if (is.null(u)) {
      tryCatch(
        showNotification(
          .str("auth_delete_msg_no_user", "No signed-in account to delete."),
          type = "warning",
          duration = 5
        ),
        error = function(e) NULL
      )
      return()
    }
    showModal(ynow_delete_account_modal(.loc()))
  }, ignoreInit = TRUE)

  observeEvent(input$ynow_auth_delete_confirm, {
    if (!isTRUE(ynow_auth_enabled())) return()
    u <- account_user()
    if (is.null(u)) {
      tryCatch(removeModal(), error = function(e) NULL)
      tryCatch(
        showNotification(
          .str("auth_delete_msg_no_user", "No signed-in account to delete."),
          type = "warning",
          duration = 5
        ),
        error = function(e) NULL
      )
      return()
    }
    confirm_txt <- as.character(input$ynow_auth_delete_confirm_text %||% "")[1]
    if (!exists("ynow_db_purge_user", mode = "function")) {
      tryCatch(
        showNotification(
          .str("auth_delete_msg_fail", "Could not delete the account. Try again later."),
          type = "error",
          duration = 6
        ),
        error = function(e) NULL
      )
      return()
    }
    res <- tryCatch(
      ynow_db_purge_user(
        user_id = u$id %||% "",
        email = u$email %||% "",
        confirm = confirm_txt,
        provider_uid = u$provider_uid %||% ""
      ),
      error = function(e) {
        list(ok = FALSE, message_key = "auth_delete_msg_fail")
      }
    )
    if (!isTRUE(res$ok)) {
      mk <- as.character(res$message_key %||% "auth_delete_msg_fail")[1]
      fb <- if (identical(mk, "auth_delete_msg_confirm")) {
        "Type DELETE exactly to confirm account deletion."
      } else if (identical(mk, "auth_delete_msg_no_user")) {
        "No signed-in account to delete."
      } else {
        "Could not delete the account. Try again later."
      }
      tryCatch(
        showNotification(.str(mk, fb), type = "error", duration = 6),
        error = function(e) NULL
      )
      return()
    }
    tryCatch(removeModal(), error = function(e) NULL)
    .sign_out()
    mk_ok <- as.character(res$message_key %||% "auth_delete_msg_ok")[1]
    fb_ok <- if (identical(mk_ok, "auth_delete_msg_ok_auth_skipped")) {
      paste0(
        "Account data deleted and you are signed out. ",
        "Login identity may still exist until the app owner configures ",
        "YNOW_SUPABASE_SERVICE_ROLE on the server."
      )
    } else if (identical(mk_ok, "auth_delete_msg_ok_auth_failed")) {
      paste0(
        "Account data deleted and you are signed out. ",
        "Removing the login identity failed — contact support if you can still sign in."
      )
    } else {
      "Account deleted. You have been signed out."
    }
    ntype <- if (identical(mk_ok, "auth_delete_msg_ok")) "message" else "warning"
    ndur <- if (identical(mk_ok, "auth_delete_msg_ok")) 6 else 12
    tryCatch(
      showNotification(.str(mk_ok, fb_ok), type = ntype, duration = ndur),
      error = function(e) NULL
    )
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
  observeEvent(input$ynow_pa_gate_signin_register, {
    .open_auth_modal_register()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_lab_gate_signin_register, {
    .open_auth_modal_register()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_chk_gate_signin_register, {
    .open_auth_modal_register()
  }, ignoreInit = TRUE)
  observeEvent(input$ynow_acct_gate_register, {
    .open_auth_modal_register()
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
        reset_help = tryCatch(
          ui_str("auth_reset_help", loc),
          error = function(e) "Choose a new password for your account (at least 8 characters)."
        ),
        reset_password_label = tryCatch(
          ui_str("auth_reset_password_label", loc),
          error = function(e) "New password:"
        ),
        reset_password2_label = tryCatch(
          ui_str("auth_reset_password2_label", loc),
          error = function(e) "Confirm new password:"
        ),
        reset_submit = tryCatch(ui_str("auth_reset_submit", loc), error = function(e) "Save new password"),
        reset_back = tryCatch(ui_str("auth_reset_back", loc), error = function(e) "Back to sign in"),
        modal_title_reset = tryCatch(
          ui_str("auth_modal_title_reset", loc),
          error = function(e) "Set a new password"
        ),
        change_pw_title = tryCatch(
          ui_str("auth_change_pw_title", loc),
          error = function(e) "Change password"
        ),
        change_pw_help = tryCatch(ui_str("auth_change_pw_help", loc), error = function(e) ""),
        change_pw_current = tryCatch(
          ui_str("auth_change_pw_current", loc),
          error = function(e) "Current password"
        ),
        change_pw_new = tryCatch(ui_str("auth_change_pw_new", loc), error = function(e) "New password"),
        change_pw_confirm = tryCatch(
          ui_str("auth_change_pw_confirm", loc),
          error = function(e) "Confirm new password"
        ),
        change_pw_submit = tryCatch(
          ui_str("auth_change_pw_submit", loc),
          error = function(e) "Update password"
        ),
        register = tryCatch(ui_str("auth_register", loc), error = function(e) "Register a new account"),
        register_btn = tryCatch(ui_str("auth_register_btn", loc), error = function(e) "Register"),
        modal_title_register = tryCatch(
          ui_str("auth_modal_title_register", loc),
          error = function(e) "Register for The YNow App"
        ),
        resend = tryCatch(ui_str("auth_resend", loc), error = function(e) "Didn't receive the verification email"),
        admin_title = tryCatch(ui_str("auth_admin_title", loc), error = function(e) "Account overview"),
        admin_intro = tryCatch(ui_str("auth_admin_intro", loc), error = function(e) ""),
        delete_account = tryCatch(
          ui_str("auth_delete_account", loc),
          error = function(e) "Delete account"
        ),
        delete_modal_title = tryCatch(
          ui_str("auth_delete_modal_title", loc),
          error = function(e) "Delete account permanently?"
        ),
        delete_modal_warn = tryCatch(
          ui_str("auth_delete_modal_warn", loc),
          error = function(e) ""
        ),
        delete_modal_type = tryCatch(
          ui_str("auth_delete_modal_type", loc),
          error = function(e) "Type DELETE (all caps) to confirm."
        ),
        delete_modal_confirm = tryCatch(
          ui_str("auth_delete_modal_confirm", loc),
          error = function(e) "Permanently delete"
        ),
        delete_modal_cancel = tryCatch(
          ui_str("auth_delete_modal_cancel", loc),
          error = function(e) "Cancel"
        ),
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
        gate_inner_lock_body = tryCatch(ui_str("auth_gate_inner_lock_body", loc), error = function(e) ""),
        gate_inner_lock_kicker = tryCatch(ui_str("auth_gate_inner_lock_kicker", loc), error = function(e) ""),
        gate_inner_lock_lead = tryCatch(ui_str("auth_gate_inner_lock_lead", loc), error = function(e) ""),
        gate_inner_lock_b1 = tryCatch(ui_str("auth_gate_inner_lock_b1", loc), error = function(e) ""),
        gate_inner_lock_b2 = tryCatch(ui_str("auth_gate_inner_lock_b2", loc), error = function(e) ""),
        gate_inner_lock_b3 = tryCatch(ui_str("auth_gate_inner_lock_b3", loc), error = function(e) ""),
        gate_inner_lock_demo_caption = tryCatch(ui_str("auth_gate_inner_lock_demo_caption", loc), error = function(e) ""),
        gate_atx_body = tryCatch(ui_str("auth_gate_atx_body", loc), error = function(e) ""),
        gate_atx_kicker = tryCatch(ui_str("auth_gate_atx_kicker", loc), error = function(e) ""),
        gate_atx_lead = tryCatch(ui_str("auth_gate_atx_lead", loc), error = function(e) ""),
        gate_atx_b1 = tryCatch(ui_str("auth_gate_atx_b1", loc), error = function(e) ""),
        gate_atx_b2 = tryCatch(ui_str("auth_gate_atx_b2", loc), error = function(e) ""),
        gate_atx_b3 = tryCatch(ui_str("auth_gate_atx_b3", loc), error = function(e) ""),
        gate_atx_demo_caption = tryCatch(ui_str("auth_gate_atx_demo_caption", loc), error = function(e) ""),
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
        menu_settings = tryCatch(ui_str("menu_settings", loc), error = function(e) "Settings"),
        settings_title = tryCatch(ui_str("settings_title", loc), error = function(e) "Settings"),
        settings_intro = tryCatch(ui_str("settings_intro", loc), error = function(e) ""),
        settings_hide_locked = tryCatch(
          ui_str("settings_hide_locked", loc),
          error = function(e) "Quick-hide locked items"
        ),
        settings_hide_locked_help = tryCatch(
          ui_str("settings_hide_locked_help", loc),
          error = function(e) ""
        ),
        settings_ui_locale = tryCatch(
          ui_str("settings_ui_locale", loc),
          error = function(e) "Preferred language"
        ),
        settings_market = tryCatch(
          ui_str("settings_market", loc),
          error = function(e) "Preferred market on sign-in"
        ),
        settings_sidebar_compact = tryCatch(
          ui_str("settings_sidebar_compact", loc),
          error = function(e) "Compact sidebar spacing"
        ),
        settings_sidebar_compact_help = tryCatch(
          ui_str("settings_sidebar_compact_help", loc),
          error = function(e) ""
        ),
        settings_gain_loss = tryCatch(
          ui_str("settings_gain_loss", loc),
          error = function(e) "Gain / loss colors"
        ),
        settings_gain_loss_help = tryCatch(
          ui_str("settings_gain_loss_help", loc),
          error = function(e) ""
        ),
        settings_save = tryCatch(ui_str("settings_save", loc), error = function(e) "Save settings"),
        pts_box_title = tryCatch(ui_str("pts_ledger_title", loc), error = function(e) "Points ledger"),
        pts_ledger_intro = tryCatch(ui_str("pts_ledger_intro", loc), error = function(e) ""),
        pts_topup_title = tryCatch(ui_str("pts_topup_title", loc), error = function(e) "Points top-up"),
        pts_topup_copy = tryCatch(ui_str("pts_topup_copy", loc), error = function(e) ""),
        pts_topup_cta = tryCatch(ui_str("pts_topup_cta", loc), error = function(e) "Coming soon")
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
