# Shared market cache (Postgres when YNOW_DB_* is set; memory otherwise).
# Statements stay fresh for a quarter. Price history used for Rolling β refreshes daily.
# Live quotes (1d–3mo windows) stay on the fast 15-minute tier.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

.ynow_market_mem <- new.env(parent = emptyenv())
.ynow_market_mem$rows <- new.env(parent = emptyenv())

ynow_market_statement_ttl_sec <- function() {
  days <- suppressWarnings(as.integer(Sys.getenv("YNOW_CACHE_STATEMENT_TTL_DAYS", unset = "90"))[1])
  if (!is.finite(days) || days < 1L) days <- 90L
  as.integer(days) * 86400L
}

ynow_market_price_day_ttl_sec <- function() {
  sec <- suppressWarnings(as.integer(Sys.getenv("YNOW_CACHE_PRICE_DAY_TTL_SEC", unset = "86400"))[1])
  if (!is.finite(sec) || sec < 60L) 86400L else as.integer(sec)
}

ynow_market_cache_ttl <- function(kind) {
  k <- tolower(trimws(as.character(kind %||% "")[1]))
  if (identical(k, "statement")) return(ynow_market_statement_ttl_sec())
  if (k %in% c("price_day", "beta", "news")) return(ynow_market_price_day_ttl_sec())
  fast <- tryCatch(as.integer(.YNOW_CACHE_FAST_TTL_SEC)[1], error = function(e) NA_integer_)
  if (!is.finite(fast) || fast < 60L) 900L else fast
}

ynow_price_history_kind <- function(period) {
  p <- tolower(trimws(as.character(period %||% "5y")[1]))
  if (p %in% c("1d", "5d", "1wk", "1mo", "3mo")) "quote" else "price_day"
}

ynow_market_cache_reset <- function() {
  .ynow_market_mem$rows <- new.env(parent = emptyenv())
  invisible(TRUE)
}

.ynow_market_db_on <- function() {
  if (identical(Sys.getenv("YNOW_MARKET_CACHE_FORCE_MEM", ""), "1")) return(FALSE)
  exists("ynow_db_ready", mode = "function") &&
    isTRUE(ynow_db_ready()) &&
    exists("ynow_db_with", mode = "function")
}

ynow_market_cache_ensure_schema <- function() {
  if (!.ynow_market_db_on()) return(invisible(FALSE))
  ynow_db_with(function(con) {
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS ynow_market_cache (",
      "cache_key TEXT PRIMARY KEY,",
      "kind TEXT NOT NULL,",
      "ticker TEXT NOT NULL,",
      "payload BYTEA NOT NULL,",
      "fetched_at TIMESTAMPTZ NOT NULL DEFAULT NOW()",
      ")"
    ))
  })
  invisible(TRUE)
}

.ynow_market_age_sec <- function(fetched_at) {
  ts <- suppressWarnings(as.POSIXct(fetched_at, tz = "UTC"))
  if (length(ts) != 1L || is.na(ts)) return(NA_real_)
  as.numeric(difftime(Sys.time(), ts, units = "secs"))
}

.ynow_market_unpack <- function(payload) {
  raw <- payload
  if (is.list(raw) && !is.raw(raw)) raw <- raw[[1]]
  if (is.character(raw)) {
    raw <- tryCatch(jsonlite::base64_dec(raw[1]), error = function(e) NULL)
  }
  if (!is.raw(raw) || !length(raw)) return(NULL)
  tryCatch(unserialize(raw), error = function(e) NULL)
}

ynow_market_cache_get <- function(cache_key, max_age_sec) {
  key <- as.character(cache_key %||% "")[1]
  max_age <- suppressWarnings(as.numeric(max_age_sec)[1])
  if (!nzchar(key) || !is.finite(max_age) || max_age <= 0) return(NULL)
  if (.ynow_market_db_on()) {
    tryCatch(ynow_market_cache_ensure_schema(), error = function(e) NULL)
    row <- tryCatch(
      ynow_db_with(function(con) {
        DBI::dbGetQuery(
          con,
          "SELECT payload, fetched_at FROM ynow_market_cache WHERE cache_key = $1",
          params = list(key)
        )
      }),
      error = function(e) NULL
    )
    if (is.data.frame(row) && nrow(row) >= 1L) {
      age <- .ynow_market_age_sec(row$fetched_at[[1]])
      if (is.finite(age) && age <= max_age) {
        return(.ynow_market_unpack(row$payload[[1]]))
      }
    }
    return(NULL)
  }
  hit <- .ynow_market_mem$rows[[key]]
  if (is.null(hit)) return(NULL)
  age <- as.numeric(Sys.time()) - as.numeric(hit$fetched_at %||% 0)
  if (!is.finite(age) || age > max_age) return(NULL)
  hit$value
}

ynow_market_cache_put <- function(cache_key, kind, ticker, value, ttl_sec = NULL) {
  key <- as.character(cache_key %||% "")[1]
  if (!nzchar(key) || is.null(value)) return(invisible(FALSE))
  if (is.data.frame(value) && nrow(value) < 1L) return(invisible(FALSE))
  k <- as.character(kind %||% "statement")[1]
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (.ynow_market_db_on()) {
    blob <- serialize(value, NULL)
    tryCatch(ynow_market_cache_ensure_schema(), error = function(e) NULL)
    ok <- tryCatch(
      ynow_db_with(function(con) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO ynow_market_cache (cache_key, kind, ticker, payload, fetched_at)",
            "VALUES ($1, $2, $3, $4, NOW())",
            "ON CONFLICT (cache_key) DO UPDATE SET",
            "kind = EXCLUDED.kind, ticker = EXCLUDED.ticker,",
            "payload = EXCLUDED.payload, fetched_at = NOW()"
          ),
          params = list(key, k, tk, blob)
        )
      }),
      error = function(e) NULL
    )
    return(invisible(is.numeric(ok) || isTRUE(ok)))
  }
  .ynow_market_mem$rows[[key]] <- list(
    value = value,
    kind = k,
    ticker = tk,
    fetched_at = Sys.time()
  )
  invisible(TRUE)
}

#' Read a fresh price history row, otherwise call `fetch` once and store it.
ynow_price_history_through_cache <- function(ticker, period, fetch) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  per <- as.character(period %||% "5y")[1]
  if (!nzchar(tk) || !is.function(fetch)) return(fetch())
  kind <- ynow_price_history_kind(per)
  key <- paste0("px:", tk, ":", tolower(trimws(per)))
  hit <- ynow_market_cache_get(key, ynow_market_cache_ttl(kind))
  if (is.data.frame(hit) && nrow(hit) > 0L) return(hit)
  df <- fetch()
  if (is.data.frame(df) && nrow(df) > 0L) {
    ynow_market_cache_put(key, kind, tk, df)
  }
  df
}
