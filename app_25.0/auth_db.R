# ==========================================
# Neon Free Postgres — account store (self-serve)
# ==========================================
# Preferred backend: own `ynow_users` table on Neon Free.
# Anyone may self-register via Google / Facebook / Email — no admin approval.
# Optional YNOW_AUTH_SEED_* only creates YOUR maintenance admin row; it is not
# a whitelist for end users.
#
# Env (put in app_25.0/.Renviron — gitignored):
#   YNOW_DB_HOST=ep-xxxx-pooler.region.aws.neon.tech
#   YNOW_DB_NAME=neondb
#   YNOW_DB_USER=neondb_owner
#   YNOW_DB_PASSWORD=...
#   YNOW_DB_PORT=5432
#   YNOW_DB_SSLMODE=require
# Or a single URL:
#   YNOW_DATABASE_URL=postgresql://user:pass@ep-xxxx-pooler.../neondb?sslmode=require
#
# One-time schema: Rscript app_25.0/../scripts/neon_migrate.R
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

#' TRUE when Neon/Postgres credentials are present (does not open a socket).
ynow_db_ready <- function() {
  !is.null(ynow_db_config()) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RPostgres", quietly = TRUE)
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
      maxSize = 3,
      idleTimeout = 60
    ),
    error = function(e) {
      warning("ynow_db_pool: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  .ynow_db_pool
}

ynow_db_with <- function(fn) {
  p <- ynow_db_pool()
  if (is.null(p)) return(NULL)
  tryCatch(fn(p), error = function(e) {
    warning("ynow_db_with: ", conditionMessage(e), call. = FALSE)
    NULL
  })
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
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = as.character(u$display_name %||% em)[1],
      provider = "password",
      role = as.character(u$role %||% role)[1],
      id = as.character(u$id %||% "")[1],
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
  if (!(prov %in% c("google", "facebook", "supabase", "oauth"))) {
    # Keep unknown IdP labels (e.g. apple) but store as oauth-family.
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
  list(
    ok = TRUE,
    user = list(
      email = em,
      display_name = as.character(u$display_name %||% dn)[1],
      provider = as.character(u$provider %||% prov)[1],
      provider_uid = as.character(u$provider_uid %||% puid)[1],
      role = "user",
      id = as.character(u$id %||% "")[1],
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
