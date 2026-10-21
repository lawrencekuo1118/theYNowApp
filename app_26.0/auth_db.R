# ==========================================
# Postgres account store (Neon / Supabase) — self-serve
# ==========================================
# Preferred backend: own `ynow_users` table on managed Postgres
# (Neon Free or Supabase; same YNOW_DB_* / YNOW_DATABASE_URL shape).
# UI label uses ynow_db_host_kind() — supabase | neon | postgres | none.
# Anyone may self-register via Google / Facebook / Email — no admin approval.
# Optional YNOW_AUTH_SEED_* only creates YOUR maintenance admin row; it is not
# a whitelist for end users.
#
# Env (put in app_26.0/.Renviron — gitignored):
#   YNOW_DB_HOST=…-pooler…   # prefer IPv4 pooler when runtime lacks IPv6
#   YNOW_DB_NAME=neondb|postgres
#   YNOW_DB_USER=neondb_owner | postgres.<PROJECT_REF> (Supabase pooler)
#   YNOW_DB_PASSWORD=...
#   YNOW_DB_PORT=5432
#   YNOW_DB_SSLMODE=require
# Or a single URL:
#   YNOW_DATABASE_URL=postgresql://user:pass@…pooler…/dbname?sslmode=require
#
# One-time schema: Rscript app_26.0/../scripts/neon_migrate.R
# Optional seed admin: set YNOW_AUTH_SEED_EMAIL / YNOW_AUTH_SEED_PASSWORD then migrate.

.ynow_db_pool <- NULL

ynow_db_parse_url <- function(url) {
  url <- trimws(as.character(url %||% "")[1])
  if (!nzchar(url)) return(NULL)
  # postgresql://user:pass@host:port/db?sslmode=require
  m <- regexec(
    "^postgres(ql)?://([^:@/]+):([^@/]+)@([^:/?]+)(:([0-9]+))?/([^?]+)(\\?.*)?$",
    url,
    perl = TRUE
  )
  hit <- regmatches(url, m)[[1]]
  if (!length(hit)) return(NULL)
  list(
    user = utils::URLdecode(hit[[3]]),
    password = utils::URLdecode(hit[[4]]),
    host = hit[[5]],
    port = if (nzchar(hit[[7]] %||% "")) as.integer(hit[[7]]) else 5432L,
    dbname = hit[[8]],
    sslmode = {
      q <- hit[[9]] %||% ""
      mm <- regexec("sslmode=([^&]+)", q, perl = TRUE)
      qq <- regmatches(q, mm)[[1]]
      if (length(qq) >= 2L) qq[[2]] else "require"
    }
  )
}

ynow_db_config <- function() {
  url <- trimws(Sys.getenv("YNOW_DATABASE_URL", unset = ""))
  if (!nzchar(url)) url <- trimws(Sys.getenv("DATABASE_URL", unset = ""))
  parsed <- ynow_db_parse_url(url)
  if (!is.null(parsed)) return(parsed)
  host <- trimws(Sys.getenv("YNOW_DB_HOST", unset = ""))
  user <- trimws(Sys.getenv("YNOW_DB_USER", unset = ""))
  password <- as.character(Sys.getenv("YNOW_DB_PASSWORD", unset = ""))[1]
  dbname <- trimws(Sys.getenv("YNOW_DB_NAME", unset = "neondb"))
  if (!nzchar(host) || !nzchar(user) || !nzchar(password)) return(NULL)
  port <- suppressWarnings(as.integer(Sys.getenv("YNOW_DB_PORT", unset = "5432")))
  if (!is.finite(port) || port <= 0) port <- 5432L
  sslmode <- trimws(Sys.getenv("YNOW_DB_SSLMODE", unset = "require"))
  if (!nzchar(sslmode)) sslmode <- "require"
  list(
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = sslmode
  )
}

#' TRUE when account Postgres credentials are present (does not open a socket).
#' Host may be Neon Free, Supabase pooler, or any Postgres — see ynow_db_host_kind().
ynow_db_ready <- function() {
  !is.null(ynow_db_config()) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RPostgres", quietly = TRUE)
}

#' Classify account DB host vendor without exposing hostname/URL/password.
#' @return "neon" | "supabase" | "postgres" | "none"
ynow_db_host_kind <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- tryCatch(ynow_db_config(), error = function(e) NULL)
  if (is.null(cfg)) return("none")
  host <- tolower(as.character(cfg$host %||% "")[1])
  if (!nzchar(host)) return("none")
  if (grepl("neon\\.tech", host, perl = TRUE)) return("neon")
  if (grepl("supabase", host, fixed = TRUE)) return("supabase")
  "postgres"
}

ynow_db_driver_ok <- function() {
  requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RPostgres", quietly = TRUE)
}

#' Get (or create) a small connection pool. NULL when not configured / unavailable.
ynow_db_pool <- function(force_new = FALSE) {
  if (!isTRUE(ynow_db_ready())) return(NULL)
  if (isTRUE(force_new) && !is.null(.ynow_db_pool)) {
    tryCatch(pool::poolClose(.ynow_db_pool), error = function(e) NULL)
    .ynow_db_pool <<- NULL
  }
  if (!is.null(.ynow_db_pool)) return(.ynow_db_pool)
  cfg <- ynow_db_config()
  if (is.null(cfg)) return(NULL)
  if (!requireNamespace("pool", quietly = TRUE)) {
    # Fall back to a one-shot DBI connection wrapped as list with $conn semantics — use pool only.
    return(NULL)
  }
  # Hosted: many Shiny workers × large pools exhaust Neon Free connection
  # limits under concurrent login. Keep per-worker pool tiny; pooler host is
  # expected (ep-*-pooler). Override with YNOW_DB_POOL_MAX.
  hosted <- exists(".ynow_is_hosted_connect", mode = "function") &&
    isTRUE(.ynow_is_hosted_connect())
  pool_max <- suppressWarnings(as.integer(Sys.getenv("YNOW_DB_POOL_MAX", unset = "")))
  if (!is.finite(pool_max) || pool_max < 1L) {
    pool_max <- if (isTRUE(hosted)) 2L else 3L
  }
  pool_max <- max(1L, min(10L, pool_max))
  .ynow_db_pool <<- tryCatch(
    pool::dbPool(
      drv = RPostgres::Postgres(),
      host = cfg$host,
      port = cfg$port,
      dbname = cfg$dbname,
      user = cfg$user,
      password = cfg$password,
      sslmode = cfg$sslmode,
      minSize = 0,
      maxSize = pool_max,
      idleTimeout = 60
    ),
    error = function(e) {
      warning("ynow_db_pool: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  .ynow_db_pool
}

# Nest-safe checkout stack (R is single-threaded for sync Shiny handlers).
# Reuses the outer connection so nested ynow_db_with never exhausts a tiny pool
# and so DBI::dbWithTransaction works (Pool objects are not supported).
.ynow_db_with_stack <- new.env(parent = emptyenv())
.ynow_db_with_stack$cons <- list()

#' Run `fn(con)` with a real DBI connection from the pool.
#' Nested calls reuse the outer checkout (safe for apply → balances).
ynow_db_with <- function(fn) {
  if (!is.function(fn)) return(NULL)

  # Nested first — reuse outer connection even if pool is temporarily empty.
  n <- length(.ynow_db_with_stack$cons)
  if (n >= 1L) {
    return(tryCatch(
      fn(.ynow_db_with_stack$cons[[n]]),
      error = function(e) {
        warning("ynow_db_with: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    ))
  }

  p <- ynow_db_pool()
  if (is.null(p)) return(NULL)
  if (!requireNamespace("pool", quietly = TRUE)) {
    warning("ynow_db_with: pool package unavailable", call. = FALSE)
    return(NULL)
  }
  con <- tryCatch(
    pool::poolCheckout(p),
    error = function(e) {
      warning("ynow_db_with checkout: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(con)) return(NULL)
  .ynow_db_with_stack$cons[[length(.ynow_db_with_stack$cons) + 1L]] <- con
  on.exit({
    .ynow_db_with_stack$cons <- .ynow_db_with_stack$cons[-length(.ynow_db_with_stack$cons)]
    tryCatch(pool::poolReturn(con), error = function(e) NULL)
  }, add = FALSE)
  tryCatch(
    fn(con),
    error = function(e) {
      warning("ynow_db_with: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

#' Password hash using sodium (preferred) or openssl salted SHA-256 fallback.
ynow_db_hash_password <- function(password) {
  pw <- as.character(password %||% "")[1]
  if (!nzchar(pw)) stop("empty password")
  if (requireNamespace("sodium", quietly = TRUE)) {
    return(as.character(sodium::password_store(pw))[1])
  }
  salt <- openssl::base64_encode(openssl::rand_bytes(16))
  dig <- openssl::sha256(charToRaw(paste0(salt, ":", pw)))
  paste0("sha256$", salt, "$", openssl::base64_encode(dig))
}

ynow_db_verify_password <- function(password, hash) {
  pw <- as.character(password %||% "")[1]
  hash <- as.character(hash %||% "")[1]
  if (!nzchar(pw) || !nzchar(hash)) return(FALSE)
  # OAuth-only rows store an unusable sentinel — never accept password login.
  if (startsWith(hash, "oauth$")) return(FALSE)
  if (startsWith(hash, "$argon2") || startsWith(hash, "$7$") ||
      grepl("^\\$[a-z0-9]+\\$", hash)) {
    if (!requireNamespace("sodium", quietly = TRUE)) return(FALSE)
    return(isTRUE(tryCatch(sodium::password_verify(hash, pw), error = function(e) FALSE)))
  }
  if (startsWith(hash, "sha256$")) {
    parts <- strsplit(hash, "$", fixed = TRUE)[[1]]
    if (length(parts) < 3L) return(FALSE)
    salt <- parts[[2]]
    expect <- parts[[3]]
    dig <- openssl::base64_encode(
      openssl::sha256(charToRaw(paste0(salt, ":", pw)))
    )
    return(identical(dig, expect))
  }
  # Legacy demo plain compare only if hash looks like plaintext (should not happen in DB).
  identical(pw, hash)
}

#' Unusable password_hash for Google / Facebook self-registered rows.
ynow_db_oauth_password_sentinel <- function(provider, provider_uid = "") {
  nonce <- paste(
    as.character(provider %||% "oauth")[1],
    as.character(provider_uid %||% "")[1],
    as.character(Sys.time()),
    paste(sample(c(letters, LETTERS, 0:9), 32L, replace = TRUE), collapse = ""),
    sep = "|"
  )
  dig <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(nonce, algo = "sha256")
  } else {
    paste(sample(c(letters, 0:9), 40L, replace = TRUE), collapse = "")
  }
  paste0("oauth$", dig)
}

#' Fail-closed RLS for account tables (Supabase PostgREST / anon key safe).
#' Enables RLS without FORCE so the YNOW_DB_* owner/server role still works;
#' revokes anon/authenticated/PUBLIC table grants; adds no open policies.
.ynow_db_rls_fail_closed <- function(con, table = "ynow_users") {
  tbl <- as.character(table %||% "")[1]
  if (!nzchar(tbl) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", tbl)) {
    return(FALSE)
  }
  # ENABLE (not FORCE): app connects as table owner / postgres via YNOW_DB_*;
  # FORCE would block that role unless BYPASSRLS or dedicated policies.
  tryCatch(
    DBI::dbExecute(
      con,
      paste0("ALTER TABLE ", tbl, " ENABLE ROW LEVEL SECURITY")
    ),
    error = function(e) NULL
  )
  # No permissive policies for anon / authenticated — default deny under RLS.
  for (pol in c(
    paste0(tbl, "_anon_all"),
    paste0(tbl, "_authenticated_all"),
    paste0(tbl, "_public_all")
  )) {
    tryCatch(
      DBI::dbExecute(con, paste0("DROP POLICY IF EXISTS ", pol, " ON ", tbl)),
      error = function(e) NULL
    )
  }
  # Strip API/public grants even if RLS were mis-disabled later.
  for (role in c("PUBLIC", "anon", "authenticated")) {
    tryCatch(
      DBI::dbExecute(
        con,
        paste0("REVOKE ALL ON TABLE ", tbl, " FROM ", role)
      ),
      error = function(e) NULL
    )
  }
  TRUE
}

ynow_db_ensure_schema <- function(conn = NULL) {
  run <- function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_users (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        email TEXT NOT NULL UNIQUE,
        password_hash TEXT NOT NULL,
        display_name TEXT,
        role TEXT NOT NULL DEFAULT 'user',
        provider TEXT NOT NULL DEFAULT 'password',
        provider_uid TEXT,
        disabled BOOLEAN NOT NULL DEFAULT FALSE,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        last_login_at TIMESTAMPTZ
      )
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS ynow_users_email_lower_idx
        ON ynow_users (lower(email))
    ")
    # Fail-closed for Supabase anon / PostgREST (principle: supabase-rls-exposure).
    .ynow_db_rls_fail_closed(con, "ynow_users")
    # Points wallet columns / tables (no-op when points_wallet.R not sourced yet).
    if (exists("ynow_points_ensure_schema", mode = "function")) {
      tryCatch(ynow_points_ensure_schema(con), error = function(e) NULL)
    } else {
      # Minimal column so older deploys still migrate users table.
      tryCatch(
        DBI::dbExecute(
          con,
          "ALTER TABLE ynow_users
             ADD COLUMN IF NOT EXISTS points_balance INTEGER NOT NULL DEFAULT 0"
        ),
        error = function(e) NULL
      )
    }
    # Durable UI prefs (pinned recent search, 24h search history, …).
    if (exists("ynow_prefs_ensure_schema", mode = "function")) {
      tryCatch(ynow_prefs_ensure_schema(con), error = function(e) NULL)
    }
    TRUE
  }
  if (!is.null(conn)) return(run(conn))
  out <- ynow_db_with(run)
  isTRUE(out)
}

ynow_db_get_user_by_email <- function(email) {
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em) || !isTRUE(ynow_db_ready())) return(NULL)
  ynow_db_with(function(con) {
    df <- DBI::dbGetQuery(
      con,
      "SELECT id::text AS id, email, password_hash, display_name, role,
              provider, provider_uid, disabled, created_at, last_login_at
         FROM ynow_users
        WHERE lower(email) = $1
        LIMIT 1",
      params = list(em)
    )
    if (!is.data.frame(df) || nrow(df) < 1L) return(NULL)
    as.list(df[1, , drop = FALSE])
  })
}

ynow_db_touch_login <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid) || !isTRUE(ynow_db_ready())) return(invisible(FALSE))
  ynow_db_with(function(con) {
    DBI::dbExecute(
      con,
      "UPDATE ynow_users SET last_login_at = NOW() WHERE id = $1::uuid",
      params = list(uid)
    )
    TRUE
  })
  invisible(TRUE)
}

#' Create a password user. Returns list(ok=, user=, message_key=).
ynow_db_create_user <- function(email, password, display_name = NULL,
                                role = "user") {
  if (!isTRUE(ynow_db_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_no_backend"))
  }
  em <- ynow_auth_norm_email(email)
  pw <- as.character(password %||% "")[1]
  if (!nzchar(em) || !nzchar(pw)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_missing"))
  }
  if (nchar(pw) < 8L) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_password_short"))
  }
  if (!is.null(ynow_db_get_user_by_email(em))) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_email_taken"))
  }
  dn <- as.character(display_name %||% em)[1]
  if (!nzchar(dn)) dn <- em
  role <- as.character(role %||% "user")[1]
  if (!role %in% c("user", "admin")) role <- "user"
  ph <- tryCatch(ynow_db_hash_password(pw), error = function(e) NULL)
  if (is.null(ph)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_no_backend"))
  }
  ok <- ynow_db_with(function(con) {
    ynow_db_ensure_schema(con)
    DBI::dbExecute(
      con,
      "INSERT INTO ynow_users (email, password_hash, display_name, role, provider)
       VALUES ($1, $2, $3, $4, 'password')",
      params = list(em, ph, dn, role)
    )
    TRUE
  })
  if (!isTRUE(ok)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_register_failed"))
  }
  u <- ynow_db_get_user_by_email(em)
  uid <- as.character(u$id %||% "")[1]
  if (nzchar(uid) && exists("ynow_points_grant_signup_bonus", mode = "function")) {
    tryCatch(ynow_points_grant_signup_bonus(uid), error = function(e) NULL)
  }
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = as.character(u$display_name %||% em)[1],
      provider = "password",
      role = as.character(u$role %||% role)[1],
      id = uid,
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_register_ok"
  )
}

#' Login against Neon users table (password provider).
ynow_db_try_login <- function(email, password) {
  if (!isTRUE(ynow_db_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_no_backend"))
  }
  em <- ynow_auth_norm_email(email)
  pw <- as.character(password %||% "")[1]
  if (!nzchar(em) || !nzchar(pw)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_missing"))
  }
  u <- ynow_db_get_user_by_email(em)
  if (is.null(u)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_invalid"))
  }
  if (isTRUE(as.logical(u$disabled %||% FALSE))) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled_account"))
  }
  if (!isTRUE(ynow_db_verify_password(pw, u$password_hash %||% ""))) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_invalid"))
  }
  tryCatch(ynow_db_touch_login(u$id), error = function(e) NULL)
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = as.character(u$display_name %||% em)[1],
      provider = as.character(u$provider %||% "password")[1],
      role = as.character(u$role %||% "user")[1],
      id = as.character(u$id %||% "")[1],
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_ok"
  )
}

#' Upsert a self-serve Google / Facebook user. No admin approval.
#' Existing Email users with the same address are linked and signed in.
#' New rows always get role = "user" and disabled = FALSE.
#' @return list(ok=, user=, message_key=)
ynow_db_upsert_oauth_user <- function(email, display_name = NULL,
                                      provider = "google",
                                      provider_uid = NULL) {
  if (!isTRUE(ynow_db_ready())) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_no_backend"))
  }
  em <- ynow_auth_norm_email(email)
  if (!nzchar(em)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_no_email"))
  }
  prov <- tolower(as.character(provider %||% "oauth")[1])
  if (!nzchar(prov)) prov <- "oauth"
  if (!(prov %in% c("google", "facebook", "apple", "supabase", "oauth"))) {
    # Keep unknown IdP labels but store as oauth-family.
    if (!nzchar(prov)) prov <- "oauth"
  }
  puid <- as.character(provider_uid %||% "")[1]
  dn <- as.character(display_name %||% em)[1]
  if (!nzchar(dn)) dn <- em

  existing <- ynow_db_get_user_by_email(em)
  if (!is.null(existing)) {
    if (isTRUE(as.logical(existing$disabled %||% FALSE))) {
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_disabled_account"))
    }
    ok <- ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        "UPDATE ynow_users
            SET display_name = COALESCE(NULLIF($1, ''), display_name),
                provider = $2,
                provider_uid = COALESCE(NULLIF($3, ''), provider_uid),
                last_login_at = NOW()
          WHERE id = $4::uuid",
        params = list(dn, prov, puid, as.character(existing$id)[1])
      )
      TRUE
    })
    if (!isTRUE(ok)) {
      return(list(ok = FALSE, user = NULL, message_key = "auth_msg_oauth_failed"))
    }
    u <- ynow_db_get_user_by_email(em)
    return(list(
      ok = TRUE,
      user = list(
        email = em,
        display_name = as.character(u$display_name %||% dn)[1],
        provider = as.character(u$provider %||% prov)[1],
        provider_uid = as.character(u$provider_uid %||% puid)[1],
        role = as.character(u$role %||% "user")[1],
        id = as.character(u$id %||% "")[1],
        logged_in_at = as.character(Sys.time())
      ),
      message_key = "auth_msg_ok"
    ))
  }

  sentinel <- ynow_db_oauth_password_sentinel(prov, puid)
  ok <- ynow_db_with(function(con) {
    ynow_db_ensure_schema(con)
    DBI::dbExecute(
      con,
      "INSERT INTO ynow_users
         (email, password_hash, display_name, role, provider, provider_uid, disabled)
       VALUES ($1, $2, $3, 'user', $4, $5, FALSE)",
      params = list(em, sentinel, dn, prov, if (nzchar(puid)) puid else NA_character_)
    )
    TRUE
  })
  if (!isTRUE(ok)) {
    return(list(ok = FALSE, user = NULL, message_key = "auth_msg_register_failed"))
  }
  u <- ynow_db_get_user_by_email(em)
  uid <- as.character(u$id %||% "")[1]
  if (nzchar(uid) && exists("ynow_points_grant_signup_bonus", mode = "function")) {
    tryCatch(ynow_points_grant_signup_bonus(uid), error = function(e) NULL)
  }
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = as.character(u$display_name %||% dn)[1],
      provider = as.character(u$provider %||% prov)[1],
      provider_uid = as.character(u$provider_uid %||% puid)[1],
      role = "user",
      id = uid,
      logged_in_at = as.character(Sys.time())
    ),
    message_key = "auth_msg_register_ok"
  )
}

#' Seed optional maintenance admin from env (idempotent). Not a user whitelist.
ynow_db_seed_admin_from_env <- function() {
  if (!isTRUE(ynow_db_ready())) return(invisible(FALSE))
  em <- ynow_auth_norm_email(Sys.getenv("YNOW_AUTH_SEED_EMAIL", unset = ""))
  pw <- as.character(Sys.getenv("YNOW_AUTH_SEED_PASSWORD", unset = ""))[1]
  if (!nzchar(em) || !nzchar(pw)) return(invisible(FALSE))
  if (!is.null(ynow_db_get_user_by_email(em))) return(invisible(TRUE))
  res <- ynow_db_create_user(em, pw, display_name = em, role = "admin")
  isTRUE(res$ok)
}

#' Exact confirm token required before account hard-delete (UI + server gate).
ynow_auth_delete_confirm_token <- function() {
  "DELETE"
}

#' TRUE when `text` matches the delete-account confirm token exactly (after trim).
ynow_auth_delete_confirm_ok <- function(text) {
  identical(
    trimws(as.character(text %||% "")[1]),
    ynow_auth_delete_confirm_token()
  )
}

.ynow_db_is_uuid <- function(user_id) {
  grepl(
    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
    as.character(user_id %||% "")[1]
  )
}

#' Clear process-local / file stores for one member (always safe; no secrets).
.ynow_purge_user_local_stores <- function(user_id, email = "") {
  uid <- as.character(user_id %||% "")[1]
  em <- if (exists("ynow_auth_norm_email", mode = "function")) {
    ynow_auth_norm_email(email %||% "")
  } else {
    tolower(trimws(as.character(email %||% "")[1]))
  }
  out <- list()
  if (nzchar(uid) && exists("ynow_points_purge_user_local", mode = "function")) {
    out$points_local <- tryCatch(
      ynow_points_purge_user_local(uid),
      error = function(e) FALSE
    )
  }
  if (nzchar(uid) && exists("ynow_prefs_purge_user", mode = "function")) {
    out$prefs_local <- tryCatch(
      ynow_prefs_purge_user(uid),
      error = function(e) FALSE
    )
  }
  if (nzchar(uid) && exists("ynow_access_purge_user", mode = "function")) {
    out$access_local <- tryCatch(
      ynow_access_purge_user(uid),
      error = function(e) FALSE
    )
  }
  if (nzchar(em) && exists("price_alert_purge_email", mode = "function")) {
    out$alerts <- tryCatch(
      price_alert_purge_email(em),
      error = function(e) 0L
    )
  }
  out
}

#' Map Auth IdP delete result → locale message key after a successful DB purge.
#'
#' Policy: Postgres / local purge always proceeds; Auth Admin delete is best-effort.
#' Report Auth status so the member knows when the login identity may remain.
#'
#' @param auth_idp list from [ynow_oauth_delete_supabase_user()] (or skip stub)
#' @return character scalar locale key
ynow_auth_delete_message_key <- function(auth_idp) {
  idp <- if (is.list(auth_idp)) auth_idp else list()
  needed <- isTRUE(idp$needed)
  if (!isTRUE(needed)) {
    # Email-only / non-Supabase provider id / no Auth uid — DB purge is enough.
    return("auth_delete_msg_ok")
  }
  if (isTRUE(idp$ok)) return("auth_delete_msg_ok")
  if (isTRUE(idp$skipped)) return("auth_delete_msg_ok_auth_skipped")
  "auth_delete_msg_ok_auth_failed"
}

#' Hard-delete one member account and user-linked app rows (server-side only).
#'
#' Purge order (single transaction when Postgres is ready):
#' `ynow_points_ledger` → `ynow_pdf_quota` → `ynow_pdf_repeat` →
#' `ynow_user_prefs` → `ynow_access_locks` → `ynow_users`.
#' Also clears memory/file prefs, access locks, points/PDF mem, and price alerts
#' keyed by email. Always attempts Supabase Auth Admin delete when a Supabase
#' Auth UUID is known; requires `YNOW_SUPABASE_SERVICE_ROLE` (never sent to the
#' client). DB purge succeeds even if Auth delete is skipped or fails — see
#' [ynow_auth_delete_message_key()].
#'
#' @param user_id UUID or mem: id
#' @param email optional mailbox for alert purge / lookup
#' @param confirm must be exactly `DELETE` (see [ynow_auth_delete_confirm_ok()])
#' @param provider_uid optional Supabase Auth user id from the signed-in session
#'   (used when DB row is already gone or memory-only)
#' @return list(ok=, message_key=, purged=, auth_idp=, user_id=, email=)
ynow_db_purge_user <- function(user_id, email = NULL, confirm = NULL,
                               provider_uid = NULL) {
  if (!isTRUE(ynow_auth_delete_confirm_ok(confirm))) {
    return(list(
      ok = FALSE,
      message_key = "auth_delete_msg_confirm",
      purged = list(),
      auth_idp = list(ok = FALSE, skipped = TRUE, needed = FALSE)
    ))
  }
  uid <- as.character(user_id %||% "")[1]
  em <- if (exists("ynow_auth_norm_email", mode = "function")) {
    ynow_auth_norm_email(email %||% "")
  } else {
    tolower(trimws(as.character(email %||% "")[1]))
  }
  session_puid <- as.character(provider_uid %||% "")[1]
  if (!nzchar(uid) && nzchar(em) && isTRUE(ynow_db_ready())) {
    u <- tryCatch(ynow_db_get_user_by_email(em), error = function(e) NULL)
    if (!is.null(u)) uid <- as.character(u$id %||% "")[1]
  }
  if (!nzchar(uid)) {
    return(list(
      ok = FALSE,
      message_key = "auth_delete_msg_no_user",
      purged = list(),
      auth_idp = list(ok = FALSE, skipped = TRUE, needed = FALSE)
    ))
  }

  puid <- session_puid
  if (isTRUE(ynow_db_ready()) && isTRUE(.ynow_db_is_uuid(uid))) {
    urow <- tryCatch(
      ynow_db_with(function(con) {
        DBI::dbGetQuery(
          con,
          "SELECT email, provider, COALESCE(provider_uid, '') AS provider_uid
             FROM ynow_users WHERE id = $1::uuid LIMIT 1",
          params = list(uid)
        )
      }),
      error = function(e) NULL
    )
    if (is.data.frame(urow) && nrow(urow) >= 1L) {
      if (!nzchar(em)) {
        em <- if (exists("ynow_auth_norm_email", mode = "function")) {
          ynow_auth_norm_email(urow$email[[1]])
        } else {
          tolower(trimws(as.character(urow$email[[1]])))
        }
      }
      db_puid <- as.character(urow$provider_uid %||% "")[1]
      if (nzchar(db_puid)) puid <- db_puid
    }
  }

  purged <- list(
    points_ledger = 0L,
    pdf_quota = 0L,
    pdf_repeat = 0L,
    user_prefs = 0L,
    access_locks = 0L,
    users = 0L
  )
  db_ok <- TRUE
  if (isTRUE(ynow_db_ready()) && isTRUE(.ynow_db_is_uuid(uid))) {
    tx <- tryCatch(
      ynow_db_with(function(con) {
        .exec <- function(sql) {
          as.integer(tryCatch(
            DBI::dbExecute(con, sql, params = list(uid)),
            error = function(e) 0L
          ))
        }
        if (requireNamespace("DBI", quietly = TRUE) &&
              exists("dbWithTransaction", where = asNamespace("DBI"), mode = "function")) {
          DBI::dbWithTransaction(con, {
            list(
              points_ledger = .exec(
                "DELETE FROM ynow_points_ledger WHERE user_id = $1::uuid"
              ),
              pdf_quota = .exec("DELETE FROM ynow_pdf_quota WHERE user_id = $1"),
              pdf_repeat = .exec("DELETE FROM ynow_pdf_repeat WHERE user_id = $1"),
              user_prefs = .exec("DELETE FROM ynow_user_prefs WHERE user_id = $1"),
              access_locks = .exec(
                "DELETE FROM ynow_access_locks WHERE user_id = $1"
              ),
              users = .exec("DELETE FROM ynow_users WHERE id = $1::uuid")
            )
          })
        } else {
          list(
            points_ledger = .exec(
              "DELETE FROM ynow_points_ledger WHERE user_id = $1::uuid"
            ),
            pdf_quota = .exec("DELETE FROM ynow_pdf_quota WHERE user_id = $1"),
            pdf_repeat = .exec("DELETE FROM ynow_pdf_repeat WHERE user_id = $1"),
            user_prefs = .exec("DELETE FROM ynow_user_prefs WHERE user_id = $1"),
            access_locks = .exec(
              "DELETE FROM ynow_access_locks WHERE user_id = $1"
            ),
            users = .exec("DELETE FROM ynow_users WHERE id = $1::uuid")
          )
        }
      }),
      error = function(e) {
        warning("ynow_db_purge_user: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.list(tx)) {
      for (nm in names(purged)) {
        if (!is.null(tx[[nm]])) purged[[nm]] <- as.integer(tx[[nm]])[1]
      }
      # UUID member with a live DB must remove the users row.
      db_ok <- isTRUE(as.integer(purged$users %||% 0L)[1] >= 1L) ||
        # Already gone (retry) — treat as success if no user remains.
        isTRUE(is.null(tryCatch(
          ynow_db_with(function(con) {
            df <- DBI::dbGetQuery(
              con,
              "SELECT 1 AS ok FROM ynow_users WHERE id = $1::uuid LIMIT 1",
              params = list(uid)
            )
            if (is.data.frame(df) && nrow(df) >= 1L) TRUE else NULL
          }),
          error = function(e) NULL
        )))
    } else {
      db_ok <- FALSE
    }
  }

  local <- .ynow_purge_user_local_stores(uid, em)
  for (nm in names(local)) purged[[nm]] <- local[[nm]]

  auth_idp <- list(ok = FALSE, skipped = TRUE, needed = FALSE, reason = "no_uid")
  if (nzchar(puid) &&
        exists("ynow_oauth_delete_supabase_user", mode = "function")) {
    auth_idp <- tryCatch(
      ynow_oauth_delete_supabase_user(puid),
      error = function(e) {
        list(
          ok = FALSE,
          skipped = FALSE,
          needed = TRUE,
          error = conditionMessage(e),
          reason = "error"
        )
      }
    )
  }

  if (!isTRUE(db_ok) && isTRUE(ynow_db_ready()) && isTRUE(.ynow_db_is_uuid(uid))) {
    return(list(
      ok = FALSE,
      message_key = "auth_delete_msg_fail",
      purged = purged,
      auth_idp = auth_idp,
      user_id = uid,
      email = em
    ))
  }

  mk <- ynow_auth_delete_message_key(auth_idp)
  message(
    "ynow_db_purge_user: ok user=", uid,
    " message_key=", mk,
    " auth_idp.needed=", isTRUE(auth_idp$needed),
    " auth_idp.ok=", isTRUE(auth_idp$ok),
    " auth_idp.skipped=", isTRUE(auth_idp$skipped),
    " auth_idp.reason=", as.character(auth_idp$reason %||% "")[1]
  )
  list(
    ok = TRUE,
    message_key = mk,
    purged = purged,
    auth_idp = auth_idp,
    user_id = uid,
    email = em
  )
}
