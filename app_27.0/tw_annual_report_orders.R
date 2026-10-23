# ==========================================
# tw_annual_report_orders.R — TW major customer / supplier from MOPS 年報
#
# Official doc.twse / MOPS shareholder annual report PDFs only.
# Slow-cache tier (filings). Wired into Business Breakdown orders packs.
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
.tw_ar_chr <- function(x, default = NA_character_) {
  if (exists(".bblab_chr", mode = "function")) return(.bblab_chr(x, default))
  s <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
  if (!length(s) || is.na(s) || !nzchar(s)) default else s
}

.tw_ar_orders_env <- environment()
.tw_ar_orders_py_ready <- FALSE

.tw_ar_orders_fn_ready <- function() {
  exists(
    "fetch_tw_annual_order_notes",
    envir = .tw_ar_orders_env,
    inherits = FALSE,
    mode = "function"
  ) || exists(
    "scrape_tw_annual_order_notes",
    envir = .tw_ar_orders_env,
    inherits = FALSE,
    mode = "function"
  )
}

.ensure_tw_annual_order_py <- function() {
  if (isTRUE(.tw_ar_orders_py_ready) && isTRUE(.tw_ar_orders_fn_ready())) {
    return(TRUE)
  }
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) return(FALSE)
  ok <- FALSE
  tryCatch({
    if (exists(".ynow_ensure_python", mode = "function")) {
      tryCatch(.ynow_ensure_python(), error = function(e) FALSE)
    }
    if (exists(".ensure_python_scraper", mode = "function")) {
      tryCatch(.ensure_python_scraper(), error = function(e) FALSE)
    }
    if (!isTRUE(reticulate::py_available(initialize = FALSE))) {
      suppressMessages(reticulate::py_config())
    }
    py_file <- "tw_annual_report_orders.py"
    if (!file.exists(py_file)) return(FALSE)
    reticulate::source_python(py_file, envir = .tw_ar_orders_env)
    .tw_ar_orders_py_ready <<- TRUE
    ok <- TRUE
  }, error = function(e) {
    if (exists(".ynow_log", mode = "function")) {
      .ynow_log("⚠️ TW annual-report orders module load failed: ", e$message)
    }
    ok <<- FALSE
  })
  isTRUE(ok) && isTRUE(.tw_ar_orders_fn_ready())
}

.tw_ar_orders_call_py <- function(ticker) {
  if (exists("fetch_tw_annual_order_notes", envir = .tw_ar_orders_env, inherits = FALSE)) {
    return(.tw_ar_orders_env$fetch_tw_annual_order_notes(ticker))
  }
  if (exists("scrape_tw_annual_order_notes", envir = .tw_ar_orders_env, inherits = FALSE)) {
    return(.tw_ar_orders_env$scrape_tw_annual_order_notes(ticker))
  }
  NULL
}

#' Normalize Python notes dict → R list usable as BBLab `notes`.
.normalize_tw_annual_order_notes <- function(raw, ticker = NULL) {
  empty <- list(
    ok = FALSE,
    ticker = .tw_ar_chr(ticker, NA_character_),
    source = "mops_annual_report",
    form = "TW-AR",
    excerpts = character(0),
    full_texts = character(0),
    short_names = character(0),
    segment_tables = list(),
    segment_tables_json = "[]",
    error = "empty",
    meta = list()
  )
  if (is.null(raw)) return(empty)
  if (is.list(raw) && !is.null(raw$error) && identical(raw$ok, FALSE) &&
      !length(raw$segment_tables %||% list())) {
    empty$error <- as.character(raw$error %||% "error")[1]
    empty$meta <- raw$meta %||% list()
    empty$ticker <- .tw_ar_chr(raw$ticker %||% ticker, NA_character_)
    return(empty)
  }
  tables <- raw$segment_tables %||% list()
  if (!is.null(tables) && !is.list(tables)) tables <- list()
  # reticulate may return a single dict as named list — wrap if it looks like one table.
  if (length(tables) && !is.null(tables$kind) && is.null(tables[[1]])) {
    tables <- list(tables)
  }
  excerpts <- as.character(raw$excerpts %||% character(0))
  full_texts <- as.character(raw$full_texts %||% character(0))
  short_names <- as.character(raw$short_names %||% character(0))
  json <- tryCatch({
    if (exists("jsonlite") || requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::toJSON(tables, auto_unbox = TRUE, null = "null")
    } else {
      "[]"
    }
  }, error = function(e) "[]")
  list(
    ok = isTRUE(raw$ok) || length(tables) > 0L || length(excerpts) > 0L,
    ticker = .tw_ar_chr(raw$ticker %||% ticker, NA_character_),
    source = .tw_ar_chr(raw$source, "mops_annual_report"),
    form = .tw_ar_chr(raw$form, "TW-AR"),
    excerpts = excerpts,
    full_texts = full_texts,
    short_names = short_names,
    segment_tables = tables,
    segment_tables_json = as.character(json)[1],
    error = if (isTRUE(raw$ok) || length(tables) || length(excerpts)) {
      NULL
    } else {
      as.character(raw$error %||% "concentration_not_found")[1]
    },
    meta = raw$meta %||% list()
  )
}

#' Fetch TW annual-report concentration notes (uncached).
fetch_tw_annual_order_notes <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  empty <- .normalize_tw_annual_order_notes(NULL, tk)
  if (!nzchar(tk) || !grepl("\\.(TW|TWO)$", tk)) {
    empty$error <- "not_tw_ticker"
    return(empty)
  }
  if (!isTRUE(.ensure_tw_annual_order_py())) {
    empty$error <- "tw_annual_report_orders_py_unavailable"
    return(empty)
  }
  raw <- tryCatch(
    .tw_ar_orders_call_py(tk),
    error = function(e) {
      if (exists(".ynow_log", mode = "function")) {
        .ynow_log("⚠️ fetch_tw_annual_order_notes(", tk, "): ", e$message)
      }
      NULL
    }
  )
  .normalize_tw_annual_order_notes(raw, tk)
}

#' Slow-cache wrapper (same tier as SEC segment notes).
cached_fetch_tw_annual_order_notes <- memoise::memoise(
  fetch_tw_annual_order_notes,
  cache = if (exists(".ynow_slow_cache")) .ynow_slow_cache else cachem::cache_mem(max_age = 24 * 3600)
)
