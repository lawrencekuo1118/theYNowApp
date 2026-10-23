# ==========================================
# Social OAuth (Google / Facebook / LinkedIn) — self-serve signup
# ==========================================
# Product model: any end user clicks Google / Facebook / LinkedIn → provider consent →
# app auto-creates a Neon `ynow_users` row (role=user) and signs them in.
# There is NO admin whitelist / permission step for ordinary users.
#
# One-time owner setup (credentials only — not per-user provisioning):
#
# Preferred path — Supabase Auth (one project covers Google + Facebook + LinkedIn):
#   YNOW_SUPABASE_URL=https://xxxx.supabase.co
#   YNOW_SUPABASE_ANON_KEY=eyJ...
#   YNOW_SUPABASE_SERVICE_ROLE=eyJ...   # server-only; Admin API (delete + password-reset bridge)
#   YNOW_OAUTH_REDIRECT_URI=https://hopesmasher1118.shinyapps.io/TheYNowApp/  # pragma: allowlist secret
#
# Service role is required to DELETE /auth/v1/admin/users/{id} when a member
# confirms account deletion, and to ensure Auth users exist for Email password
# recovery (/auth/v1/recover). Never put it in client JS or git; only .Renviron /
# shinyapps bundle / Cloud Agent secrets → scripts/inject_renviron_from_env.R.
#
# Forgot password: POST /auth/v1/recover → email link (type=recovery) → set new
# password via PUT /auth/v1/user, then sync password_hash in ynow_users.
#
# Owner must also enable each provider in the Supabase dashboard (env alone is not enough):
#   1. https://supabase.com/dashboard → project (ref in YNOW_SUPABASE_URL)
#   2. Authentication → Providers
#   3. Google / Facebook / LinkedIn (OIDC) → Enable, paste Client ID / Secret
#   4. Authentication → URL Configuration: Site URL + Redirect URLs include YNOW_OAUTH_REDIRECT_URI
#   5. Leave "Confirm email" / disable-sign-ups as needed; keep public signup allowed for self-serve
# If a provider stays OFF, /auth/v1/authorize returns:
#   {"error_code":"validation_failed","msg":"Unsupported provider: provider is not enabled"}
# The app probes /auth/v1/settings and surfaces auth_msg_oauth_provider_disabled instead of a raw 400.
# LinkedIn uses Supabase slug `linkedin_oidc` (UI / app provider id remains `linkedin`).
#
# Direct path (no Supabase) — Google / Facebook only; LinkedIn stays Supabase-only:
#   YNOW_GOOGLE_CLIENT_ID=...
#   YNOW_GOOGLE_CLIENT_SECRET=...
#   YNOW_FACEBOOK_APP_ID=...
#   YNOW_FACEBOOK_APP_SECRET=...
#   YNOW_OAUTH_REDIRECT_URI=https://hopesmasher1118.shinyapps.io/TheYNowApp/  # pragma: allowlist secret
#   YNOW_OAUTH_STATE_SECRET=<random long string>
#
# Never commit real secrets. Use app_27.0/.Renviron (gitignored) for shinyapps.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

ynow_oauth_redirect_uri <- function() {
  u <- trimws(Sys.getenv("YNOW_OAUTH_REDIRECT_URI", unset = ""))
  if (nzchar(u)) return(sub("/+$", "", u))
  "https://hopesmasher1118.shinyapps.io/TheYNowApp"  # pragma: allowlist secret
}

ynow_oauth_supabase_url <- function() {
  sub("/+$", "", trimws(Sys.getenv("YNOW_SUPABASE_URL", unset = "")))
}

ynow_oauth_supabase_anon <- function() {
  trimws(Sys.getenv("YNOW_SUPABASE_ANON_KEY", unset = ""))
}

ynow_oauth_supabase_ready <- function() {
  nzchar(ynow_oauth_supabase_url()) && nzchar(ynow_oauth_supabase_anon())
}

.ynow_oauth_cache <- new.env(parent = emptyenv())

#' Fetch Supabase Auth settings (which external providers are enabled).
#' Cached per process; pass force=TRUE after dashboard changes in the same session.
#' Keep timeout short: a cold probe on the click path used to stall social login.
ynow_oauth_supabase_settings <- function(force = FALSE) {
  if (!isTRUE(force) && is.list(.ynow_oauth_cache$settings)) {
    return(.ynow_oauth_cache$settings)
  }
  if (!isTRUE(ynow_oauth_supabase_ready())) return(NULL)
  base <- ynow_oauth_supabase_url()
  anon <- ynow_oauth_supabase_anon()
  settings <- tryCatch(
    {
      resp <- httr2::request(paste0(base, "/auth/v1/settings")) |>
        httr2::req_headers(apikey = anon, Authorization = paste("Bearer", anon)) |>
        httr2::req_timeout(2.5) |>
        httr2::req_perform()
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },
    error = function(e) NULL
  )
  if (is.list(settings)) .ynow_oauth_cache$settings <- settings
  settings
}

#' Warm Supabase Auth settings cache (session start / modal open). Non-fatal.
ynow_oauth_warm_settings <- function() {
  if (!isTRUE(ynow_oauth_supabase_ready())) return(invisible(FALSE))
  tryCatch(
    {
      ynow_oauth_supabase_settings()
      invisible(TRUE)
    },
    error = function(e) invisible(FALSE)
  )
}

#' Client bootstrap for immediate social redirect (no Shiny round-trip on click).
#' @return list(ready=, mode=, redirect_uri=, providers= named list of
#'   list(enabled=, url=, slug=) for google/facebook/linkedin)
ynow_oauth_client_bootstrap <- function() {
  out <- list(
    ready = FALSE,
    mode = "none",
    redirect_uri = ynow_oauth_redirect_uri(),
    providers = list()
  )
  if (!isTRUE(ynow_auth_enabled())) return(out)
  if (!isTRUE(ynow_oauth_supabase_ready())) return(out)
  # Prefer cached settings; warm when empty so enabled flags are accurate.
  if (!is.list(.ynow_oauth_cache$settings)) {
    ynow_oauth_warm_settings()
  }
  base <- ynow_oauth_supabase_url()
  redir <- ynow_oauth_redirect_uri()
  providers <- list()
  for (p in c("google", "facebook", "linkedin")) {
    en <- ynow_oauth_supabase_provider_enabled(p)
    slug <- ynow_oauth_supabase_slug(p)
    url <- ""
    if (!identical(en, FALSE)) {
      url <- sprintf(
        "%s/auth/v1/authorize?provider=%s&redirect_to=%s",
        base,
        utils::URLencode(slug, reserved = TRUE),
        utils::URLencode(redir, reserved = TRUE)
      )
    }
    providers[[p]] <- list(
      enabled = if (isTRUE(is.na(en))) NULL else isTRUE(en),
      slug = slug,
      url = url
    )
  }
  list(
    ready = TRUE,
    mode = "supabase",
    redirect_uri = redir,
    providers = providers
  )
}

#' Map app provider id → Supabase /auth/v1/authorize slug.
ynow_oauth_supabase_slug <- function(provider) {
  provider <- tolower(trimws(as.character(provider %||% "")[1]))
  if (identical(provider, "linkedin")) return("linkedin_oidc")
  provider
}

#' TRUE when Supabase reports the named external provider as enabled.
#' Unknown / unreachable settings → NA (caller may still attempt authorize).
#' LinkedIn checks `linkedin_oidc` first, then legacy `linkedin`.
ynow_oauth_supabase_provider_enabled <- function(provider) {
  provider <- tolower(trimws(as.character(provider %||% "")[1]))
  if (!nzchar(provider)) return(NA)
  settings <- ynow_oauth_supabase_settings()
  if (!is.list(settings)) return(NA)
  ext <- settings$external
  if (!is.list(ext)) return(FALSE)
  keys <- unique(c(ynow_oauth_supabase_slug(provider), provider))
  found <- FALSE
  for (k in keys) {
    if (!(k %in% names(ext))) next
    found <- TRUE
    val <- ext[[k]]
    en <- if (is.list(val) && !is.null(val$enabled)) isTRUE(val$enabled) else isTRUE(val)
    if (isTRUE(en)) return(TRUE)
  }
  if (!found) return(FALSE)
  FALSE
}

ynow_oauth_google_ready <- function() {
  if (isTRUE(ynow_oauth_supabase_ready())) return(TRUE)
  nzchar(trimws(Sys.getenv("YNOW_GOOGLE_CLIENT_ID", unset = ""))) &&
    nzchar(trimws(Sys.getenv("YNOW_GOOGLE_CLIENT_SECRET", unset = "")))
}

ynow_oauth_facebook_ready <- function() {
  if (isTRUE(ynow_oauth_supabase_ready())) return(TRUE)
  nzchar(trimws(Sys.getenv("YNOW_FACEBOOK_APP_ID", unset = ""))) &&
    nzchar(trimws(Sys.getenv("YNOW_FACEBOOK_APP_SECRET", unset = "")))
}

#' LinkedIn Sign In via Supabase Auth only (linkedin_oidc; no direct client path).
ynow_oauth_linkedin_ready <- function() {
  isTRUE(ynow_oauth_supabase_ready())
}

ynow_oauth_state_secret <- function() {
  s <- trimws(Sys.getenv("YNOW_OAUTH_STATE_SECRET", unset = ""))
  if (nzchar(s)) return(s)
  # Fallback: derive from available secrets (not ideal; set YNOW_OAUTH_STATE_SECRET).
  paste(
    Sys.getenv("YNOW_GOOGLE_CLIENT_SECRET", unset = ""),
    Sys.getenv("YNOW_FACEBOOK_APP_SECRET", unset = ""),
    Sys.getenv("YNOW_SUPABASE_ANON_KEY", unset = ""),
    "ynow-oauth-state",
    sep = "|"
  )
}

#' Signed opaque state so a new Shiny session can verify the OAuth return.
ynow_oauth_make_state <- function(provider) {
  payload <- list(
    p = as.character(provider)[1],
    n = paste(sample(c(letters, LETTERS, 0:9), 24L, replace = TRUE), collapse = ""),
    t = as.numeric(Sys.time())
  )
  body <- jsonlite::base64_enc(charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE)))
  body <- gsub("\n", "", body, fixed = TRUE)
  sig <- digest::hmac(ynow_oauth_state_secret(), body, algo = "sha256")
  paste0(body, ".", sig)
}

ynow_oauth_parse_state <- function(state, max_age_sec = 900) {
  state <- as.character(state %||% "")[1]
  if (!nzchar(state) || !grepl(".", state, fixed = TRUE)) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  parts <- strsplit(state, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  body <- parts[[1]]
  sig <- parts[[2]]
  expect <- digest::hmac(ynow_oauth_state_secret(), body, algo = "sha256")
  if (!identical(sig, expect)) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  raw <- tryCatch(jsonlite::base64_dec(body), error = function(e) NULL)
  if (is.null(raw)) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  payload <- tryCatch(
    jsonlite::fromJSON(rawToChar(raw)),
    error = function(e) NULL
  )
  if (is.null(payload) || is.null(payload$t)) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  age <- as.numeric(Sys.time()) - as.numeric(payload$t)
  if (!is.finite(age) || age < 0 || age > max_age_sec) {
    return(list(ok = FALSE, provider = NULL, message_key = "auth_msg_oauth_state"))
  }
  list(ok = TRUE, provider = as.character(payload$p %||% "")[1], message_key = NULL)
}

#' Build authorize URL for Google / Facebook / LinkedIn (Supabase or direct).
#' LinkedIn requires Supabase Auth (`linkedin_oidc`); no direct LinkedIn client path.
#' @return list(ok=, url=, message_key=)
ynow_oauth_authorize_url <- function(provider = c("google", "facebook", "linkedin")) {
  provider <- match.arg(provider)
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, url = NULL, message_key = "auth_msg_disabled"))
  }
  redir <- ynow_oauth_redirect_uri()
  if (isTRUE(ynow_oauth_supabase_ready())) {
    # Only gate on a *cached* disabled flag. Never block the click path on a
    # cold /auth/v1/settings probe (that was the social-login delay).
    if (is.list(.ynow_oauth_cache$settings)) {
      enabled <- ynow_oauth_supabase_provider_enabled(provider)
      if (identical(enabled, FALSE)) {
        return(list(
          ok = FALSE,
          url = NULL,
          message_key = "auth_msg_oauth_provider_disabled",
          provider = provider
        ))
      }
    }
    base <- ynow_oauth_supabase_url()
    slug <- ynow_oauth_supabase_slug(provider)
    url <- sprintf(
      "%s/auth/v1/authorize?provider=%s&redirect_to=%s",
      base,
      utils::URLencode(slug, reserved = TRUE),
      utils::URLencode(redir, reserved = TRUE)
    )
    return(list(ok = TRUE, url = url, message_key = NULL))
  }
  if (identical(provider, "linkedin")) {
    return(list(ok = FALSE, url = NULL, message_key = "auth_msg_oauth_need_linkedin"))
  }
  state <- ynow_oauth_make_state(provider)
  if (identical(provider, "google")) {
    if (!isTRUE(ynow_oauth_google_ready())) {
      return(list(ok = FALSE, url = NULL, message_key = "auth_msg_oauth_need_google"))
    }
    cid <- trimws(Sys.getenv("YNOW_GOOGLE_CLIENT_ID", unset = ""))
    url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", utils::URLencode(cid, reserved = TRUE),
      "&redirect_uri=", utils::URLencode(redir, reserved = TRUE),
      "&response_type=code",
      "&scope=", utils::URLencode("openid email profile", reserved = TRUE),
      "&access_type=online",
      "&include_granted_scopes=true",
      "&prompt=select_account",
      "&state=", utils::URLencode(state, reserved = TRUE)
    )
    return(list(ok = TRUE, url = url, message_key = NULL))
  }
  if (!isTRUE(ynow_oauth_facebook_ready())) {
    return(list(ok = FALSE, url = NULL, message_key = "auth_msg_oauth_need_facebook"))
  }
  aid <- trimws(Sys.getenv("YNOW_FACEBOOK_APP_ID", unset = ""))
  url <- paste0(
    "https://www.facebook.com/v21.0/dialog/oauth?",
    "client_id=", utils::URLencode(aid, reserved = TRUE),
    "&redirect_uri=", utils::URLencode(redir, reserved = TRUE),
    "&state=", utils::URLencode(state, reserved = TRUE),
    "&scope=", utils::URLencode("email,public_profile", reserved = TRUE),
    "&response_type=code"
  )
  list(ok = TRUE, url = url, message_key = NULL)
}

.ynow_oauth_http_json <- function(req) {
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },
    error = function(e) NULL
  )
}

#' Exchange authorization code (direct Google / Facebook).
ynow_oauth_exchange_code <- function(provider, code) {
  provider <- as.character(provider %||% "")[1]
  code <- as.character(code %||% "")[1]
  if (!nzchar(provider) || !nzchar(code)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  redir <- ynow_oauth_redirect_uri()
  if (identical(provider, "google")) {
    tok <- .ynow_oauth_http_json(
      httr2::request("https://oauth2.googleapis.com/token") |>
        httr2::req_method("POST") |>
        httr2::req_body_form(
          code = code,
          client_id = Sys.getenv("YNOW_GOOGLE_CLIENT_ID", unset = ""),
          client_secret = Sys.getenv("YNOW_GOOGLE_CLIENT_SECRET", unset = ""),
          redirect_uri = redir,
          grant_type = "authorization_code"
        )
    )
    access <- as.character(tok$access_token %||% "")[1]
    if (!nzchar(access)) {
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
    }
    info <- .ynow_oauth_http_json(
      httr2::request("https://openidconnect.googleapis.com/v1/userinfo") |>
        httr2::req_headers(Authorization = paste("Bearer", access))
    )
    email <- ynow_auth_norm_email(info$email %||% "")
    if (!nzchar(email)) {
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
    }
    return(list(
      ok = TRUE,
      user = list(
        email = email,
        display_name = as.character(info$name %||% email)[1],
        provider = "google",
        provider_uid = as.character(info$sub %||% "")[1],
        logged_in_at = as.character(Sys.time())
      ),
      message_key = "auth_msg_ok"
    ))
  }
  if (identical(provider, "facebook")) {
    tok <- .ynow_oauth_http_json(
      httr2::request("https://graph.facebook.com/v21.0/oauth/access_token") |>
        httr2::req_url_query(
          client_id = Sys.getenv("YNOW_FACEBOOK_APP_ID", unset = ""),
          client_secret = Sys.getenv("YNOW_FACEBOOK_APP_SECRET", unset = ""),
          redirect_uri = redir,
          code = code
        )
    )
    access <- as.character(tok$access_token %||% "")[1]
    if (!nzchar(access)) {
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
    }
    info <- .ynow_oauth_http_json(
      httr2::request("https://graph.facebook.com/me") |>
        httr2::req_url_query(
          fields = "id,name,email",
          access_token = access
        )
    )
    email <- ynow_auth_norm_email(info$email %||% "")
    if (!nzchar(email)) {
      # Facebook may omit email without permission / verified address.
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_no_email"))
    }
    return(list(
      ok = TRUE,
      user = list(
        email = email,
        display_name = as.character(info$name %||% email)[1],
        provider = "facebook",
        provider_uid = as.character(info$id %||% "")[1],
        logged_in_at = as.character(Sys.time())
      ),
      message_key = "auth_msg_ok"
    ))
  }
  list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed")
}

#' Exchange Supabase Auth authorization code (PKCE) for a session access_token.
#' @param code auth code from redirect `?code=`
#' @param code_verifier PKCE verifier stored in the browser before IdP leave
ynow_oauth_supabase_exchange_code <- function(code, code_verifier = "") {
  code <- as.character(code %||% "")[1]
  code_verifier <- as.character(code_verifier %||% "")[1]
  if (!nzchar(code) || !isTRUE(ynow_oauth_supabase_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  if (!nzchar(code_verifier)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_state"))
  }
  base <- ynow_oauth_supabase_url()
  anon <- ynow_oauth_supabase_anon()
  # GoTrue expects grant_type=pkce (not OAuth "authorization_code").
  tok <- .ynow_oauth_http_json(
    httr2::request(paste0(base, "/auth/v1/token?grant_type=pkce")) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        apikey = anon,
        Authorization = paste("Bearer", anon),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        auth_code = code,
        code_verifier = code_verifier
      )) |>
      httr2::req_timeout(12)
  )
  access <- as.character(tok$access_token %||% "")[1]
  if (!nzchar(access)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  out <- ynow_oauth_user_from_supabase(access)
  if (is.list(out)) out$access_token <- access
  out
}

#' Resolve Supabase access_token → user profile.
ynow_oauth_user_from_supabase <- function(access_token) {
  access_token <- as.character(access_token %||% "")[1]
  if (!nzchar(access_token) || !isTRUE(ynow_oauth_supabase_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  info <- .ynow_oauth_http_json(
    httr2::request(paste0(ynow_oauth_supabase_url(), "/auth/v1/user")) |>
      httr2::req_headers(
        Authorization = paste("Bearer", access_token),
        apikey = ynow_oauth_supabase_anon()
      )
  )
  email <- ynow_auth_norm_email(info$email %||% "")
  if (!nzchar(email)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_no_email"))
  }
  app_meta <- info$app_metadata
  provider <- "supabase"
  if (is.list(app_meta) && !is.null(app_meta$provider)) {
    provider <- as.character(app_meta$provider)[1]
  }
  um <- info$user_metadata
  display <- email
  if (is.list(um)) {
    display <- as.character(um$name %||% um$full_name %||% email)[1]
  }
  list(
    ok = TRUE,
    user = list(
      email = email,
      display_name = display,
      provider = provider,
      provider_uid = as.character(info$id %||% "")[1],
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_ok"
  )
}

#' Handle browser callback payload from JS (code or access_token).
#' Supabase returns `?code=` (PKCE). Direct Google/Facebook use HMAC `state` + code.
ynow_oauth_complete <- function(payload) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled"))
  }
  if (!is.list(payload)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  err <- as.character(payload$error %||% "")[1]
  if (nzchar(err)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  access <- as.character(payload$access_token %||% "")[1]
  if (nzchar(access) && isTRUE(ynow_oauth_supabase_ready())) {
    return(ynow_oauth_user_from_supabase(access))
  }
  code <- as.character(payload$code %||% "")[1]
  state <- as.character(payload$state %||% "")[1]
  code_verifier <- as.character(payload$code_verifier %||% "")[1]
  if (!nzchar(code)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  # Supabase Auth PKCE: code + code_verifier → session token (no HMAC state).
  if (isTRUE(ynow_oauth_supabase_ready()) && nzchar(code_verifier)) {
    return(ynow_oauth_supabase_exchange_code(code, code_verifier))
  }
  # Direct Google / Facebook: signed state selects provider for code exchange.
  st <- ynow_oauth_parse_state(state)
  if (isTRUE(st$ok)) {
    return(ynow_oauth_exchange_code(st$provider, code))
  }
  # Supabase code without verifier (stale tab / missing PKCE) — do not pretend HMAC failed.
  if (isTRUE(ynow_oauth_supabase_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_state"))
  }
  list(ok = FALSE, user = NULL, message_key = st$message_key %||% "auth_msg_oauth_state")
}

#' Optional service-role key for admin Auth API (server-only; never to client).
ynow_oauth_supabase_service_role <- function() {
  trimws(Sys.getenv("YNOW_SUPABASE_SERVICE_ROLE", unset = ""))
}

#' TRUE when `uid` looks like a Supabase Auth user UUID (not a raw Google sub).
ynow_oauth_is_supabase_auth_uid <- function(uid) {
  grepl(
    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
    as.character(uid %||% "")[1]
  )
}

#' Best-effort delete of a Supabase Auth user (OAuth IdP row).
#' Requires `YNOW_SUPABASE_SERVICE_ROLE` + `YNOW_SUPABASE_URL`. Skips cleanly
#' when unset or when `provider_uid` is not a Supabase Auth UUID.
#' HTTP 404 (already gone) counts as success. Never logs the service-role value.
ynow_oauth_delete_supabase_user <- function(provider_uid) {
  uid <- as.character(provider_uid %||% "")[1]
  if (!nzchar(uid)) {
    return(list(ok = FALSE, skipped = TRUE, reason = "no_uid", needed = FALSE))
  }
  if (!isTRUE(ynow_oauth_is_supabase_auth_uid(uid))) {
    message(
      "ynow_oauth_delete_supabase_user: skipped (not_supabase_uid) — ",
      "direct OAuth provider id is not a Supabase Auth user"
    )
    return(list(ok = FALSE, skipped = TRUE, reason = "not_supabase_uid", needed = FALSE))
  }
  key <- ynow_oauth_supabase_service_role()
  base <- ynow_oauth_supabase_url()
  if (!nzchar(key) || !nzchar(base)) {
    reason <- if (!nzchar(base)) "no_supabase_url" else "no_service_role"
    message(
      "ynow_oauth_delete_supabase_user: skipped (", reason, ") — ",
      "set YNOW_SUPABASE_SERVICE_ROLE + YNOW_SUPABASE_URL (server-only) to purge Auth IdP"
    )
    return(list(ok = FALSE, skipped = TRUE, reason = reason, needed = TRUE))
  }
  tryCatch(
    {
      resp <- httr2::request(paste0(base, "/auth/v1/admin/users/", utils::URLencode(uid, reserved = TRUE))) |>
        httr2::req_method("DELETE") |>
        httr2::req_headers(
          apikey = key,
          Authorization = paste("Bearer", key)
        ) |>
        httr2::req_timeout(12) |>
        httr2::req_perform()
      st <- as.integer(httr2::resp_status(resp))[1]
      # 2xx deleted; 404 already absent — both OK for account purge.
      ok <- isTRUE(st >= 200L && st < 300L) || identical(st, 404L)
      if (!isTRUE(ok)) {
        message(
          "ynow_oauth_delete_supabase_user: Auth Admin DELETE failed status=", st
        )
      } else if (identical(st, 404L)) {
        message("ynow_oauth_delete_supabase_user: Auth user already absent (404)")
      } else {
        message("ynow_oauth_delete_supabase_user: Auth user deleted status=", st)
      }
      list(ok = ok, skipped = FALSE, status = st, needed = TRUE, reason = if (ok) "deleted" else "http_error")
    },
    error = function(e) {
      message(
        "ynow_oauth_delete_supabase_user: error — ",
        conditionMessage(e)
      )
      list(
        ok = FALSE,
        skipped = FALSE,
        error = conditionMessage(e),
        needed = TRUE,
        reason = "error"
      )
    }
  )
}

# ------------------------------------------
# Password recovery (Supabase Auth /recover)
# ------------------------------------------
# Email/password accounts live in ynow_users; Supabase Auth sends the recovery
# email. When service_role is set, we ensure an Auth user exists for known
# app accounts so /recover actually delivers mail. Never reveal whether an
# email exists to the client (same generic success as GoTrue).

#' TRUE when password recovery can be attempted (Supabase Auth configured).
ynow_oauth_password_reset_ready <- function() {
  isTRUE(ynow_oauth_supabase_ready())
}

#' POST /auth/v1/recover — request Supabase Auth recovery email.
#' Always treats 2xx as success. Does not disclose whether the address exists.
#' @return list(ok=, status=, message_key=)
ynow_oauth_supabase_recover <- function(email, redirect_to = NULL) {
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em) || !grepl("@", em, fixed = TRUE)) {
    return(list(ok = FALSE, status = NA_integer_, message_key = "auth_msg_forgot_email"))
  }
  if (!isTRUE(ynow_oauth_supabase_ready())) {
    return(list(ok = FALSE, status = NA_integer_, message_key = "auth_msg_forgot_soon"))
  }
  base <- ynow_oauth_supabase_url()
  anon <- ynow_oauth_supabase_anon()
  redir <- as.character(redirect_to %||% ynow_oauth_redirect_uri())[1]
  body <- list(email = em)
  if (nzchar(redir)) body$redirect_to <- redir
  tryCatch(
    {
      resp <- httr2::request(paste0(base, "/auth/v1/recover")) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          apikey = anon,
          Authorization = paste("Bearer", anon),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(12) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform()
      st <- as.integer(httr2::resp_status(resp))[1]
      # GoTrue returns 200 even when the address is unknown (anti-enumeration).
      ok <- isTRUE(st >= 200L && st < 300L)
      if (!isTRUE(ok)) {
        message("ynow_oauth_supabase_recover: status=", st)
      }
      list(
        ok = ok,
        status = st,
        message_key = if (ok) "auth_msg_forgot_sent" else "auth_msg_forgot_failed"
      )
    },
    error = function(e) {
      message("ynow_oauth_supabase_recover: error — ", conditionMessage(e))
      list(ok = FALSE, status = NA_integer_, message_key = "auth_msg_forgot_failed")
    }
  )
}

#' Admin: create a confirmed Auth user for email when missing (service_role).
#' Used so app-store password users can receive recovery mail. Never logs secrets.
#' @return list(ok=, created=, skipped=, reason=)
ynow_oauth_supabase_admin_ensure_user <- function(email) {
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em)) {
    return(list(ok = FALSE, created = FALSE, skipped = TRUE, reason = "no_email"))
  }
  key <- ynow_oauth_supabase_service_role()
  base <- ynow_oauth_supabase_url()
  if (!nzchar(key) || !nzchar(base)) {
    reason <- if (!nzchar(base)) "no_supabase_url" else "no_service_role"
    return(list(ok = FALSE, created = FALSE, skipped = TRUE, reason = reason))
  }
  tryCatch(
    {
      resp <- httr2::request(paste0(base, "/auth/v1/admin/users")) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          apikey = key,
          Authorization = paste("Bearer", key),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(list(
          email = em,
          email_confirm = TRUE,
          app_metadata = list(ynow_password_bridge = TRUE)
        )) |>
        httr2::req_timeout(12) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform()
      st <- as.integer(httr2::resp_status(resp))[1]
      # 200/201 created; 422 already exists — both OK for bridge.
      if (isTRUE(st >= 200L && st < 300L)) {
        return(list(ok = TRUE, created = TRUE, skipped = FALSE, reason = "created", status = st))
      }
      if (identical(st, 422L)) {
        return(list(ok = TRUE, created = FALSE, skipped = FALSE, reason = "exists", status = st))
      }
      message("ynow_oauth_supabase_admin_ensure_user: status=", st)
      list(ok = FALSE, created = FALSE, skipped = FALSE, reason = "http_error", status = st)
    },
    error = function(e) {
      message("ynow_oauth_supabase_admin_ensure_user: error — ", conditionMessage(e))
      list(ok = FALSE, created = FALSE, skipped = FALSE, reason = "error")
    }
  )
}

#' PUT /auth/v1/user — set password for a recovery / signed-in Auth session.
#' @return list(ok=, message_key=, user=)
ynow_oauth_supabase_update_password <- function(access_token, password) {
  access_token <- as.character(access_token %||% "")[1]
  pw <- as.character(password %||% "")[1]
  if (!nzchar(access_token)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_reset_session"))
  }
  if (!nzchar(pw) || nchar(pw) < 8L) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_password_short"))
  }
  if (!isTRUE(ynow_oauth_supabase_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_forgot_soon"))
  }
  base <- ynow_oauth_supabase_url()
  anon <- ynow_oauth_supabase_anon()
  tryCatch(
    {
      resp <- httr2::request(paste0(base, "/auth/v1/user")) |>
        httr2::req_method("PUT") |>
        httr2::req_headers(
          apikey = anon,
          Authorization = paste("Bearer", access_token),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(list(password = pw)) |>
        httr2::req_timeout(12) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform()
      st <- as.integer(httr2::resp_status(resp))[1]
      if (!isTRUE(st >= 200L && st < 300L)) {
        message("ynow_oauth_supabase_update_password: status=", st)
        return(list(ok = FALSE, user = NULL, message_key = "auth_msg_reset_failed"))
      }
      info <- tryCatch(
        httr2::resp_body_json(resp, simplifyVector = TRUE),
        error = function(e) NULL
      )
      email <- ynow_auth_norm_email(info$email %||% "")
      list(
        ok = TRUE,
        user = list(
          email = email,
          display_name = as.character(info$user_metadata$full_name %||% email)[1],
          provider = "password",
          provider_uid = as.character(info$id %||% "")[1],
          id = as.character(info$id %||% "")[1]
        ),
        message_key = "auth_msg_reset_ok"
      )
    },
    error = function(e) {
      message("ynow_oauth_supabase_update_password: error — ", conditionMessage(e))
      list(ok = FALSE, user = NULL, message_key = "auth_msg_reset_failed")
    }
  )
}

#' Request password reset for an email (Supabase Auth recovery).
#' Anti-enumeration: on a valid email shape + configured Auth, always returns
#' the same success key whether or not the address exists. Fail closed when
#' Auth is not configured. Never logs the email or secrets.
#' @return list(ok=, message_key=)
ynow_auth_request_password_reset <- function(email, redirect_to = NULL) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, message_key = "auth_msg_disabled"))
  }
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em) || !grepl("@", em, fixed = TRUE)) {
    return(list(ok = FALSE, message_key = "auth_msg_forgot_email"))
  }
  if (!isTRUE(ynow_oauth_password_reset_ready())) {
    return(list(ok = FALSE, message_key = "auth_msg_forgot_soon"))
  }
  # Bridge known app accounts into Auth so recovery mail can deliver.
  # Only when the address already exists in ynow_users (no new Auth users for
  # random probes). Errors are swallowed — client still gets generic success.
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_get_user_by_email", mode = "function")) {
    u <- tryCatch(ynow_db_get_user_by_email(em), error = function(e) NULL)
    if (!is.null(u)) {
      tryCatch(ynow_oauth_supabase_admin_ensure_user(em), error = function(e) NULL)
    }
  }
  res <- ynow_oauth_supabase_recover(em, redirect_to = redirect_to)
  # Always generic success after a recover attempt when Auth is configured —
  # matches GoTrue anti-enumeration (do not leak existence via error text).
  # Network / Auth hard failure after config check: still generic to the client.
  invisible(res)
  list(ok = TRUE, message_key = "auth_msg_forgot_sent")
}

#' Apply a new password after recovery: Auth session + app users table.
#' @return list(ok=, user=, message_key=)
ynow_auth_complete_password_reset <- function(access_token, password, password_confirm = NULL) {
  if (!isTRUE(ynow_auth_enabled())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled"))
  }
  pw <- as.character(password %||% "")[1]
  conf <- as.character(password_confirm %||% pw)[1]
  if (!identical(pw, conf)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_password_mismatch"))
  }
  if (!nzchar(pw) || nchar(pw) < 8L) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_password_short"))
  }
  upd <- ynow_oauth_supabase_update_password(access_token, pw)
  if (!isTRUE(upd$ok)) {
    return(list(ok = FALSE, user = NULL, message_key = upd$message_key %||% "auth_msg_reset_failed"))
  }
  em <- ynow_auth_norm_email(upd$user$email %||% "")
  if (!nzchar(em)) {
    # Fallback: resolve email from /auth/v1/user GET
    info <- tryCatch(
      ynow_oauth_user_from_supabase(access_token),
      error = function(e) list(ok = FALSE)
    )
    if (isTRUE(info$ok)) em <- ynow_auth_norm_email(info$user$email %||% "")
  }
  if (!nzchar(em)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_reset_failed"))
  }
  # Sync app store so Email sign-in keeps working (ynow_users password_hash).
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_set_password_by_email", mode = "function")) {
    db_res <- tryCatch(
      ynow_db_set_password_by_email(em, pw),
      error = function(e) list(ok = FALSE, message_key = "auth_msg_reset_failed")
    )
    if (!isTRUE(db_res$ok)) {
      # Auth password already changed; still try to sign the session in via upsert.
      message("ynow_auth_complete_password_reset: app store sync failed")
    }
  }
  # Build session user from app DB when possible.
  sess_user <- NULL
  if (exists("ynow_db_ready", mode = "function") && isTRUE(ynow_db_ready()) &&
      exists("ynow_db_get_user_by_email", mode = "function")) {
    u <- tryCatch(ynow_db_get_user_by_email(em), error = function(e) NULL)
    if (!is.null(u)) {
      sess_user <- list(
        id = as.character(u$id %||% "")[1],
        email = em,
        display_name = as.character(u$display_name %||% em)[1],
        provider = "password",
        provider_uid = as.character(upd$user$provider_uid %||% u$provider_uid %||% "")[1],
        role = as.character(u$role %||% "user")[1],
        logged_in_at = as.character(Sys.time())
      )
    }
  }
  if (is.null(sess_user)) {
    sess_user <- list(
      id = ynow_auth_memory_user_id(em),
      email = em,
      display_name = em,
      provider = "password",
      provider_uid = as.character(upd$user$provider_uid %||% "")[1],
      role = "user",
      logged_in_at = as.character(Sys.time())
    )
  }
  list(ok = TRUE, user = sess_user, message_key = "auth_msg_reset_ok")
}
