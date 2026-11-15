# Macro Market last-good snapshot (offline-first Home/Market paint).
# Durable JSON under the slow cache root + optional ynow_market_cache mirror.
# Browser localStorage mirrors the same payload (see www/ynow_app.js).

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

MACRO_LAST_GOOD_VERSION <- 1L
# Stale-but-useful window for weak-network reopen (not a live quote TTL).
MACRO_LAST_GOOD_MAX_AGE_SEC <- 7L * 86400L

macro_last_good_dir <- function() {
  root <- tryCatch({
    if (exists(".ynow_slow_cache_dir", mode = "function")) {
      .ynow_slow_cache_dir()
    } else if (exists(".ynow_cache_root", mode = "function")) {
      file.path(.ynow_cache_root(), "slow")
    } else {
      file.path(tempdir(), "ynow_cache", "slow")
    }
  }, error = function(e) file.path(tempdir(), "ynow_cache", "slow"))
  d <- file.path(root, "macro_last_good")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

macro_last_good_path <- function(mode = "US") {
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(mode)
    } else {
      toupper(as.character(mode %||% "US")[1])
    }
  }, error = function(e) "US")
  if (!mm %in% c("US", "TW")) mm <- "US"
  file.path(macro_last_good_dir(), paste0(mm, ".json"))
}

macro_last_good_market_key <- function(mode = "US") {
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(mode)
    } else {
      toupper(as.character(mode %||% "US")[1])
    }
  }, error = function(e) "US")
  if (!mm %in% c("US", "TW")) mm <- "US"
  paste0("macro_lg:", mm)
}

.macro_last_good_norm_mode <- function(mode) {
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(mode)
    } else {
      toupper(as.character(mode %||% "US")[1])
    }
  }, error = function(e) "US")
  if (!mm %in% c("US", "TW")) mm <- "US"
  mm
}

.macro_last_good_parse_iso <- function(x) {
  raw <- as.character(x %||% "")[1]
  if (!nzchar(raw)) return(as.POSIXct(NA, tz = "UTC"))
  ts <- tryCatch(
    suppressWarnings(as.POSIXct(raw, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")),
    error = function(e) as.POSIXct(NA, tz = "UTC")
  )
  if (length(ts) == 1L && !is.na(ts)) return(ts)
  ts2 <- tryCatch(
    suppressWarnings(as.POSIXct(raw, tz = "UTC")),
    error = function(e) as.POSIXct(NA, tz = "UTC")
  )
  if (length(ts2) == 1L && !is.na(ts2)) return(ts2)
  as.POSIXct(NA, tz = "UTC")
}

.macro_last_good_age_sec <- function(snap) {
  if (!is.list(snap)) return(Inf)
  ts <- .macro_last_good_parse_iso(snap$captured_at)
  if (is.na(ts)) return(Inf)
  as.numeric(difftime(Sys.time(), ts, units = "secs"))
}

macro_last_good_quotes_usable <- function(quotes) {
  if (!is.list(quotes) || !length(quotes)) return(FALSE)
  any(vapply(quotes, function(r) {
    if (!is.list(r)) return(FALSE)
    is.finite(suppressWarnings(as.numeric(r$last %||% NA_real_)[1]))
  }, logical(1)))
}

macro_last_good_rf_usable <- function(rf) {
  if (!is.list(rf)) return(FALSE)
  is.finite(suppressWarnings(as.numeric(rf$rf_pct %||% NA_real_)[1]))
}

macro_last_good_htcdi_usable <- function(htcdi) {
  if (!is.list(htcdi)) return(FALSE)
  if (identical(as.character(htcdi$availability %||% "")[1], "unavailable")) return(FALSE)
  is.finite(suppressWarnings(as.numeric(htcdi$composite %||% NA_real_)[1]))
}

.macro_last_good_sanitize_quotes <- function(quotes) {
  if (!is.list(quotes) || !length(quotes)) return(list())
  out <- list()
  for (r in quotes) {
    if (!is.list(r)) next
    sym <- as.character(r$symbol %||% "")[1]
    if (!nzchar(sym)) next
    out[[length(out) + 1L]] <- list(
      symbol = sym,
      label = as.character(r$label %||% sym)[1],
      last = suppressWarnings(as.numeric(r$last %||% NA_real_)[1]),
      chg_pct = suppressWarnings(as.numeric(r$chg_pct %||% NA_real_)[1])
    )
  }
  out
}

.macro_last_good_sanitize_rf <- function(rf) {
  if (!is.list(rf)) return(NULL)
  pct <- suppressWarnings(as.numeric(rf$rf_pct %||% NA_real_)[1])
  if (!is.finite(pct)) return(NULL)
  list(
    rf_pct = round(pct, 2),
    source = as.character(rf$source %||% "last_known")[1],
    label = as.character(rf$label %||% "")[1],
    symbol = as.character(rf$symbol %||% "")[1],
    is_fallback = isTRUE(rf$is_fallback)
  )
}

.macro_last_good_sanitize_htcdi <- function(htcdi) {
  if (!is.list(htcdi)) return(NULL)
  score <- suppressWarnings(as.numeric(htcdi$composite %||% NA_real_)[1])
  if (!is.finite(score)) return(NULL)
  list(
    composite = score,
    availability = "ready",
    from_cache = TRUE,
    alert = as.character(htcdi$alert %||% htcdi$band %||% "")[1]
  )
}

.macro_last_good_validate <- function(snap, max_age_sec = MACRO_LAST_GOOD_MAX_AGE_SEC) {
  if (!is.list(snap)) return(NULL)
  ver <- suppressWarnings(as.integer(snap$v %||% snap$version %||% NA_integer_)[1])
  if (!is.finite(ver) || ver < 1L) return(NULL)
  mode <- .macro_last_good_norm_mode(snap$mode %||% "US")
  age <- .macro_last_good_age_sec(snap)
  max_age <- suppressWarnings(as.numeric(max_age_sec)[1])
  if (!is.finite(max_age) || max_age <= 0) max_age <- MACRO_LAST_GOOD_MAX_AGE_SEC
  if (!is.finite(age) || age > max_age) return(NULL)
  quotes <- .macro_last_good_sanitize_quotes(snap$quotes)
  rf <- .macro_last_good_sanitize_rf(snap$rf)
  htcdi <- .macro_last_good_sanitize_htcdi(snap$htcdi)
  if (!macro_last_good_quotes_usable(quotes) &&
      !macro_last_good_rf_usable(rf) &&
      !macro_last_good_htcdi_usable(htcdi)) {
    return(NULL)
  }
  list(
    v = as.integer(MACRO_LAST_GOOD_VERSION),
    mode = mode,
    captured_at = as.character(snap$captured_at %||% "")[1],
    quotes = quotes,
    rf = rf,
    htcdi = htcdi
  )
}

macro_last_good_empty <- function(mode = "US") {
  list(
    v = as.integer(MACRO_LAST_GOOD_VERSION),
    mode = .macro_last_good_norm_mode(mode),
    captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    quotes = list(),
    rf = NULL,
    htcdi = NULL
  )
}

#' Read durable Macro last-good snapshot (disk JSON, then market_cache mirror).
macro_last_good_read <- function(mode = "US",
                                 max_age_sec = MACRO_LAST_GOOD_MAX_AGE_SEC) {
  mm <- .macro_last_good_norm_mode(mode)
  path <- macro_last_good_path(mm)
  snap <- NULL
  if (file.exists(path)) {
    snap <- tryCatch({
      raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      if (!nzchar(raw)) NULL else jsonlite::fromJSON(raw, simplifyVector = FALSE)
    }, error = function(e) NULL)
  }
  validated <- .macro_last_good_validate(snap, max_age_sec = max_age_sec)
  if (!is.null(validated)) return(validated)

  if (exists("ynow_market_cache_get", mode = "function")) {
    hit <- tryCatch(
      ynow_market_cache_get(macro_last_good_market_key(mm), max_age_sec),
      error = function(e) NULL
    )
    validated <- .macro_last_good_validate(hit, max_age_sec = max_age_sec)
    if (!is.null(validated)) return(validated)
  }
  NULL
}

#' Accept a browser localStorage payload as a seed when disk is empty/cold.
macro_last_good_from_client <- function(payload,
                                        max_age_sec = MACRO_LAST_GOOD_MAX_AGE_SEC) {
  snap <- payload
  if (is.character(payload) && length(payload) == 1L) {
    snap <- tryCatch(
      jsonlite::fromJSON(payload, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  .macro_last_good_validate(snap, max_age_sec = max_age_sec)
}

#' Write Macro last-good snapshot to disk (+ market_cache mirror when available).
macro_last_good_write <- function(payload) {
  snap <- .macro_last_good_validate(
    payload,
    max_age_sec = MACRO_LAST_GOOD_MAX_AGE_SEC * 2L
  )
  if (is.null(snap)) return(invisible(FALSE))
  if (!nzchar(as.character(snap$captured_at %||% "")[1])) {
    snap$captured_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  }
  path <- macro_last_good_path(snap$mode)
  ok <- tryCatch({
    txt <- jsonlite::toJSON(snap, auto_unbox = TRUE, null = "null", pretty = FALSE)
    writeLines(txt, path, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (exists("ynow_market_cache_put", mode = "function")) {
    tryCatch(
      ynow_market_cache_put(
        macro_last_good_market_key(snap$mode),
        "macro_lg",
        snap$mode,
        snap
      ),
      error = function(e) NULL
    )
  }
  invisible(isTRUE(ok))
}

#' Merge partial live success into last-good and persist.
#' @return updated snapshot (or NULL on failure)
macro_last_good_merge <- function(mode = "US",
                                  quotes = NULL,
                                  rf = NULL,
                                  htcdi = NULL,
                                  captured_at = NULL) {
  mm <- .macro_last_good_norm_mode(mode)
  cur <- macro_last_good_read(mm, max_age_sec = MACRO_LAST_GOOD_MAX_AGE_SEC * 2L)
  if (is.null(cur)) cur <- macro_last_good_empty(mm)

  q_new <- .macro_last_good_sanitize_quotes(quotes)
  if (length(q_new) && macro_last_good_quotes_usable(q_new)) {
    # Prefer live finite values; keep prior last for symbols that failed this pass.
    by_sym <- list()
    for (r in cur$quotes %||% list()) {
      if (is.list(r) && nzchar(as.character(r$symbol %||% "")[1])) {
        by_sym[[as.character(r$symbol)[1]]] <- r
      }
    }
    for (r in q_new) {
      sym <- as.character(r$symbol)[1]
      if (is.finite(r$last)) {
        by_sym[[sym]] <- r
      } else if (is.null(by_sym[[sym]])) {
        by_sym[[sym]] <- r
      }
    }
    cur$quotes <- unname(by_sym)
  }

  rf_new <- .macro_last_good_sanitize_rf(rf)
  if (!is.null(rf_new)) cur$rf <- rf_new

  ht_new <- .macro_last_good_sanitize_htcdi(htcdi)
  if (!is.null(ht_new)) {
    ht_new$from_cache <- FALSE
    cur$htcdi <- ht_new
  }

  if (nzchar(as.character(captured_at %||% "")[1])) {
    cur$captured_at <- as.character(captured_at)[1]
  } else {
    cur$captured_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  }
  cur$mode <- mm
  cur$v <- as.integer(MACRO_LAST_GOOD_VERSION)
  if (!isTRUE(macro_last_good_write(cur))) return(NULL)
  cur
}

#' True when a quote strip has no finite last prices (blank-on-offline risk).
macro_quotes_all_blank <- function(quotes) {
  !macro_last_good_quotes_usable(quotes)
}
