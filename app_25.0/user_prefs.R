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
# Signed-in rolling search history (ticker lookups). Separate key from pinned chips
# and from any sidebar-pin prefs — additive only on ynow_user_prefs.
YNOW_PREF_RECENT_SEARCH_24H <- "recent_search_history_24h"
.YNOW_PINNED_RECENT_MAX <- 8L
.YNOW_RECENT_SEARCH_24H_MAX <- 20L
.YNOW_RECENT_SEARCH_24H_SECS <- 24L * 3600L

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

# ---- Recent search history (signed-in, rolling 24h) ----

.ynow_prefs_now_posix <- function() {
  as.POSIXct(Sys.time(), tz = "UTC")
}

.ynow_prefs_parse_iso_time <- function(x) {
  raw <- as.character(x %||% "")[1]
  if (!nzchar(raw)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  # Accept ISO-8601 with trailing Z.
  raw <- sub("Z$", "", raw, ignore.case = TRUE)
  parsed <- tryCatch(
    as.POSIXct(raw, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  )
  if (is.na(parsed)) {
    parsed <- tryCatch(
      as.POSIXct(raw, tz = "UTC"),
      error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    )
  }
  parsed
}

.ynow_prefs_format_iso_time <- function(t) {
  format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Normalize timed recent-search entries; drop older than 24h; newest first; dedupe.
#'
#' @param entries list of lists with `ticker` / `at`, or a character ticker vector
#'   (treated as "now").
#' @param now POSIXct reference time (UTC).
#' @return list of list(ticker=, at=) within the rolling window.
ynow_prefs_normalize_recent_entries <- function(entries,
                                                now = .ynow_prefs_now_posix(),
                                                max_n = .YNOW_RECENT_SEARCH_24H_MAX,
                                                window_secs = .YNOW_RECENT_SEARCH_24H_SECS) {
  now <- as.POSIXct(now, tz = "UTC")
  cutoff <- now - as.numeric(window_secs)[1]
  out <- list()
  if (is.character(entries)) {
    tickers <- ynow_prefs_normalize_tickers(entries, max_n = max_n)
    if (!length(tickers)) return(list())
    at <- .ynow_prefs_format_iso_time(now)
    return(lapply(tickers, function(tk) list(ticker = tk, at = at)))
  }
  if (!is.list(entries) || !length(entries)) return(list())
  for (el in entries) {
    if (is.character(el) && length(el) >= 1L) {
      tk <- ynow_prefs_normalize_tickers(el[[1]], max_n = 1L)
      if (!length(tk)) next
      out[[length(out) + 1L]] <- list(
        ticker = tk[[1]],
        at = .ynow_prefs_format_iso_time(now)
      )
      next
    }
    if (!is.list(el)) next
    tk <- ynow_prefs_normalize_tickers(el$ticker %||% el[[1]], max_n = 1L)
    if (!length(tk)) next
    at_raw <- el$at %||% el$ts %||% el$time %||% NA
    at <- .ynow_prefs_parse_iso_time(at_raw)
    if (is.na(at)) at <- now
    if (at < cutoff) next
    out[[length(out) + 1L]] <- list(
      ticker = tk[[1]],
      at = .ynow_prefs_format_iso_time(at)
    )
  }
  if (!length(out)) return(list())
  # Newest first; keep first occurrence per ticker (most recent).
  ords <- order(
    vapply(out, function(x) as.numeric(.ynow_prefs_parse_iso_time(x$at)), numeric(1)),
    decreasing = TRUE
  )
  out <- out[ords]
  seen <- character(0)
  kept <- list()
  for (el in out) {
    tk <- el$ticker
    if (tk %in% seen) next
    seen <- c(seen, tk)
    kept[[length(kept) + 1L]] <- el
    if (length(kept) >= as.integer(max_n)[1]) break
  }
  kept
}

ynow_prefs_recent_entries_tickers <- function(entries) {
  ents <- ynow_prefs_normalize_recent_entries(entries)
  if (!length(ents)) return(character(0))
  vapply(ents, function(x) as.character(x$ticker)[1], character(1))
}

.ynow_prefs_parse_json_recent_entries <- function(raw) {
  if (is.null(raw)) return(list())
  if (is.character(raw) && length(raw) == 1L) {
    parsed <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) return(list())
    return(ynow_prefs_normalize_recent_entries(parsed))
  }
  if (is.list(raw)) return(ynow_prefs_normalize_recent_entries(raw))
  if (is.character(raw)) return(ynow_prefs_normalize_recent_entries(raw))
  list()
}

.ynow_prefs_read_pref_raw <- function(user_id, pref_key) {
  uid <- as.character(user_id %||% "")[1]
  key <- as.character(pref_key %||% "")[1]
  if (!nzchar(uid) || !nzchar(key)) return(NULL)
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
        params = list(uid, key)
      )
    })
    if (is.data.frame(df) && nrow(df) >= 1L) return(df$pref_value[[1]])
    return(NULL)
  }
  mem <- .ynow_prefs_mem$by_user[[uid]]
  if (is.null(mem)) {
    store <- .ynow_prefs_read_file()
    mem <- store[[uid]]
  }
  if (is.list(mem) && !is.null(mem[[key]])) return(mem[[key]])
  NULL
}

.ynow_prefs_write_pref_json <- function(user_id, pref_key, value_obj) {
  uid <- as.character(user_id %||% "")[1]
  key <- as.character(pref_key %||% "")[1]
  if (!nzchar(uid) || !nzchar(key)) return(invisible(FALSE))
  payload <- jsonlite::toJSON(value_obj, auto_unbox = TRUE)
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
        params = list(uid, key, as.character(payload)[1])
      )
    })
    return(invisible(TRUE))
  }
  store <- .ynow_prefs_read_file()
  cur <- .ynow_prefs_mem$by_user[[uid]]
  if (!is.list(cur)) cur <- store[[uid]]
  if (!is.list(cur)) cur <- list()
  cur[[key]] <- value_obj
  .ynow_prefs_mem$by_user[[uid]] <- cur
  store[[uid]] <- cur
  .ynow_prefs_write_file(store)
  invisible(TRUE)
}

#' Timed recent-search entries for a signed-in user (already pruned to 24h).
ynow_prefs_get_recent_search_entries <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(list())
  raw <- .ynow_prefs_read_pref_raw(uid, YNOW_PREF_RECENT_SEARCH_24H)
  .ynow_prefs_parse_json_recent_entries(raw)
}

#' Tickers only (newest first) within the rolling 24h window.
ynow_prefs_get_recent_search_24h <- function(user_id) {
  ynow_prefs_recent_entries_tickers(ynow_prefs_get_recent_search_entries(user_id))
}

#' Replace the signed-in 24h history with normalized entries / tickers.
ynow_prefs_set_recent_search_24h <- function(user_id, entries) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  ents <- ynow_prefs_normalize_recent_entries(entries)
  .ynow_prefs_write_pref_json(uid, YNOW_PREF_RECENT_SEARCH_24H, ents)
  ynow_prefs_recent_entries_tickers(ents)
}

#' Record one successful ticker lookup for a signed-in user (bumps to front / now).
ynow_prefs_record_recent_search <- function(user_id, ticker, at = .ynow_prefs_now_posix()) {
  uid <- as.character(user_id %||% "")[1]
  tk <- ynow_prefs_normalize_tickers(ticker, max_n = 1L)
  if (!nzchar(uid) || !length(tk)) {
    return(ynow_prefs_get_recent_search_24h(uid))
  }
  cur <- ynow_prefs_get_recent_search_entries(uid)
  # Drop prior rows for this ticker so the new timestamp wins.
  cur <- Filter(function(el) !identical(el$ticker, tk[[1]]), cur)
  cur <- c(
    list(list(ticker = tk[[1]], at = .ynow_prefs_format_iso_time(at))),
    cur
  )
  ynow_prefs_set_recent_search_24h(uid, cur)
}

#' Merge guest/session tickers into durable 24h history (login handoff).
#' Guest tickers are stamped "now"; server entries keep their timestamps.
ynow_prefs_merge_recent_search_24h <- function(user_id, guest_tickers) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(character(0))
  server <- ynow_prefs_get_recent_search_entries(uid)
  guest <- ynow_prefs_normalize_tickers(guest_tickers)
  if (!length(guest)) {
    return(ynow_prefs_recent_entries_tickers(server))
  }
  now_at <- .ynow_prefs_format_iso_time(.ynow_prefs_now_posix())
  guest_ents <- lapply(guest, function(tk) list(ticker = tk, at = now_at))
  # Guest session wins ordering for overlap (just searched this session).
  ynow_prefs_set_recent_search_24h(uid, c(guest_ents, server))
}

#' Test helper: clear in-process memory prefs.
ynow_prefs_reset_memory <- function() {
  .ynow_prefs_mem$by_user <- list()
  invisible(TRUE)
}

#' Remove one user's prefs from memory + file fallback (DB rows deleted in purge txn).
ynow_prefs_purge_user <- function(user_id) {
  uid <- as.character(user_id %||% "")[1]
  if (!nzchar(uid)) return(invisible(FALSE))
  if (!is.null(.ynow_prefs_mem$by_user[[uid]])) {
    .ynow_prefs_mem$by_user[[uid]] <- NULL
  }
  store <- tryCatch(.ynow_prefs_read_file(), error = function(e) list())
  if (is.list(store) && !is.null(store[[uid]])) {
    store[[uid]] <- NULL
    tryCatch(.ynow_prefs_write_file(store), error = function(e) NULL)
  }
  invisible(TRUE)
}
