# ==========================================
# Durable per-user preferences (Supabase Postgres)
# ==========================================
# Primary store: table `ynow_user_prefs` via YNOW_DB_* (same backend as ynow_users).
# Fallback when DB is off: in-process memory + local JSON file (same pattern as
# access_locks) so demo / memory-auth users still keep pins across logout/login
# within one deploy host. Guests never write here — client localStorage only.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

YNOW_PREF_PINNED_RECENT <- "pinned_recent_search"
.YNOW_PINNED_RECENT_MAX <- 8L

.ynow_prefs_mem <- new.env(parent = emptyenv())
.ynow_prefs_mem$by_user <- list()

.ynow_prefs_db_on <- function() {
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function") &&
    !identical(Sys.getenv("YNOW_USER_PREFS_FORCE_FILE", ""), "1")
}

.ynow_prefs_file <- function() {
  root <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    error = function(e) tempdir()
  )
  file.path(root, "ynow_data", "user_prefs.json")
}

.ynow_prefs_read_file <- function() {
  path <- .ynow_prefs_file()
  if (!file.exists(path)) return(list())
  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(raw)) return(list())
  raw
}

.ynow_prefs_write_file <- function(store) {
  path <- .ynow_prefs_file()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(store, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' Normalize a ticker list for pinned recent search.
ynow_prefs_normalize_tickers <- function(tickers, max_n = .YNOW_PINNED_RECENT_MAX) {
  raw <- unique(toupper(trimws(as.character(tickers %||% character(0)))))
  raw <- raw[nzchar(raw) & !is.na(raw)]
  # Reject pathological tokens (spaces, control chars).
  raw <- raw[grepl("^[A-Z0-9.\\-]{1,24}$", raw)]
  head(raw, as.integer(max_n)[1])
}

ynow_prefs_ensure_schema <- function(conn = NULL) {
  run <- function(con) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ynow_user_prefs (
        user_id TEXT NOT NULL,
        pref_key TEXT NOT NULL,
        pref_value JSONB NOT NULL DEFAULT '[]'::jsonb,
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        PRIMARY KEY (user_id, pref_key)
      )
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS ynow_user_prefs_user_idx
        ON ynow_user_prefs (user_id)
    ")
    if (exists(".ynow_db_rls_fail_closed", mode = "function")) {
      tryCatch(.ynow_db_rls_fail_closed(con, "ynow_user_prefs"), error = function(e) NULL)
    }
    TRUE
  }
  if (!is.null(conn)) return(run(conn))
  if (!.ynow_prefs_db_on()) return(invisible(FALSE))
  out <- ynow_db_with(run)
  invisible(isTRUE(out))
}

.ynow_prefs_parse_json_tickers <- function(raw) {
  if (is.null(raw)) return(character(0))
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(character(0))
    return(ynow_prefs_normalize_tickers(parsed))
  }
  if (is.list(raw) || is.character(raw)) {
    return(ynow_prefs_normalize_tickers(unlist(raw, use.names = FALSE)))
  }
  character(0)
}

#' Read pinned recent-search tickers for a signed-in user id.
ynow_prefs_get_pinned_recent <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    df <- ynow_db_with(function(con) {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT pref_value::text AS pref_value",
          "FROM ynow_user_prefs",
          "WHERE user_id = $1 AND pref_key = $2",
          "LIMIT 1"
        ),
        params = list(uid, YNOW_PREF_PINNED_RECENT)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) {
      return(.ynow_prefs_parse_json_tickers(df$pref_value[[1]]))
    }
    return(character(0))
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[YNOW_PREF_PINNED_RECENT]])) {
    return(ynow_prefs_normalize_tickers(mem[[YNOW_PREF_PINNED_RECENT]]))
  }
  if (is.character(mem)) return(ynow_prefs_normalize_tickers(mem))
  character(0)
}

#' Persist pinned recent-search tickers for a signed-in user id.
ynow_prefs_set_pinned_recent <- function(user_id, tickers) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  pins <- ynow_prefs_normalize_tickers(tickers)
  payload <- jsonlite::toJSON(as.list(pins), auto_unbox = TRUE)
  if (.ynow_prefs_db_on()) {
    ynow_prefs_ensure_schema()
    ynow_db_with(function(con) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO ynow_user_prefs (user_id, pref_key, pref_value, updated_at)",
          "VALUES ($1, $2, $3::jsonb, NOW())",
          "ON CONFLICT (user_id, pref_key) DO UPDATE",
          "SET pref_value = EXCLUDED.pref_value, updated_at = NOW()"
        ),
        params = list(uid, YNOW_PREF_PINNED_RECENT, as.character(payload)[1])
      )
    })
    return(pins)
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[YNOW_PREF_PINNED_RECENT]] <- as.list(pins)
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  pins
}

#' Toggle one ticker in the pinned list. Returns the new pinned vector.
ynow_prefs_toggle_pinned_recent <- function(user_id, ticker) {
  tk <- ynow_prefs_normalize_tickers(ticker, max_n = 1L)
  if (!length(tk)) {
    return(ynow_prefs_get_pinned_recent(user_id))
  }
  cur <- ynow_prefs_get_pinned_recent(user_id)
  if (tk[[1]] %in% cur) {
    nxt <- setdiff(cur, tk[[1]])
  } else {
    nxt <- head(c(tk[[1]], setdiff(cur, tk[[1]])), .YNOW_PINNED_RECENT_MAX)
  }
  ynow_prefs_set_pinned_recent(user_id, nxt)
}

#' Test helper: clear in-process memory prefs.
ynow_prefs_reset_memory <- function() {
  .ynow_prefs_mem$by_user <- list()
  invisible(TRUE)
}
