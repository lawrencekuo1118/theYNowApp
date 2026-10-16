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
#   YNOW_OAUTH_REDIRECT_URI=https://hopesmasher1118.shinyapps.io/TheYNowApp/
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
#   YNOW_OAUTH_REDIRECT_URI=https://hopesmasher1118.shinyapps.io/TheYNowApp/
#   YNOW_OAUTH_STATE_SECRET=<random long string>
#
# Never commit real secrets. Use app_25.0/.Renviron (gitignored) for shinyapps.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

ynow_oauth_redirect_uri <- function() {
  u <- trimws(Sys.getenv("YNOW_OAUTH_REDIRECT_URI", unset = ""))
  if (nzchar(u)) return(sub("/+$", "", u))
  "https://hopesmasher1118.shinyapps.io/TheYNowApp"
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
        httr2::req_timeout(8) |>
        httr2::req_perform()
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },
    error = function(e) NULL
  )
  if (is.list(settings)) .ynow_oauth_cache$settings <- settings
  settings
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
    enabled <- ynow_oauth_supabase_provider_enabled(provider)
    if (identical(enabled, FALSE)) {
      return(list(
        ok = FALSE,
        url = NULL,
        message_key = "auth_msg_oauth_provider_disabled",
        provider = provider
      ))
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
  if (!nzchar(code)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
  }
  st <- ynow_oauth_parse_state(state)
  if (!isTRUE(st$ok)) {
    return(list(ok = FALSE, user = NULL, message_key = st$message_key %||% "auth_msg_oauth_state"))
  }
  ynow_oauth_exchange_code(st$provider, code)
}
