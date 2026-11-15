# US mode + 4-digit TW code → auto-switch to Taiwan market on Search only.
# Offline structural + helper checks (no network).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

root <- normalizePath("..", mustWork = TRUE)
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source(file.path(root, "market_profile.R"), local = TRUE, encoding = "UTF-8")

# --- helper: clear TW-style codes ---
check("2330 is TW code", isTRUE(looks_like_tw_stock_code("2330")))
check("0050 leading zeros", isTRUE(looks_like_tw_stock_code("0050")))
check("spaced bare", isTRUE(looks_like_tw_stock_code("  2330  ")))
check("2330.TW suffix", isTRUE(looks_like_tw_stock_code("2330.TW")))
check("3105.TWO suffix", isTRUE(looks_like_tw_stock_code("3105.TWO")))
check("label strip bare", isTRUE(looks_like_tw_stock_code("2330 — 台積電")))
check("label strip yahoo", isTRUE(looks_like_tw_stock_code("2330.TW — 台積電")))
check("lowercase suffix", isTRUE(looks_like_tw_stock_code("2330.tw")))

check("AAPL not TW", isFALSE(looks_like_tw_stock_code("AAPL")))
check("TSM not TW", isFALSE(looks_like_tw_stock_code("TSM")))
check("3-digit not TW", isFALSE(looks_like_tw_stock_code("233")))
check("5-digit not TW", isFALSE(looks_like_tw_stock_code("12345")))
check("letter suffix out of scope", isFALSE(looks_like_tw_stock_code("6488A")))
check("empty not TW", isFALSE(looks_like_tw_stock_code("")))
check("NA not TW", isFALSE(looks_like_tw_stock_code(NA_character_)))
check("US numeric-looking BRK not", isFALSE(looks_like_tw_stock_code("BRK-B")))

# --- server: Search action only (not typing / not shared fire helper) ---
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

search_obs_pos <- regexpr("observeEvent\\(input\\$search,", srv, perl = TRUE)[1]
check("has Search observeEvent", search_obs_pos > 0L)

# Slice from just before Search observer (include gate comment) until Recent Search.
recent_pos <- regexpr(
  "observeEvent\\(input\\$recent_search_pick,",
  srv,
  perl = TRUE
)[1]
search_start <- max(1L, search_obs_pos - 160L)
search_chunk <- if (search_obs_pos > 0L && recent_pos > search_obs_pos) {
  substr(srv, search_start, recent_pos - 1L)
} else if (search_obs_pos > 0L) {
  substr(srv, search_start, min(nchar(srv), search_obs_pos + 2500L))
} else {
  ""
}

check(
  "Search observer calls looks_like_tw_stock_code",
  grepl("looks_like_tw_stock_code\\(raw\\)", search_chunk, perl = TRUE)
)
check(
  "Search observer US mode gate",
  grepl("identical\\(mode, \"US\"\\)", search_chunk, perl = TRUE)
)
check(
  "Search observer sets market_mode_pick TW",
  grepl("market_mode_pick', 'TW'", search_chunk, fixed = TRUE)
)
check(
  "Search observer uses pending_recent_fetch list",
  grepl("pending_recent_fetch\\(list\\(", search_chunk, perl = TRUE)
)
check(
  "Search observer comment: not on typing",
  grepl("never switch market on ticker typing", search_chunk, fixed = TRUE) ||
    grepl("Search button / submit only", search_chunk, fixed = TRUE)
)

# Must NOT live in the shared fire helper (other entry points / refresh).
fire_pos <- regexpr("\\.ynow_fire_ticker_search <- function", srv, perl = TRUE)[1]
fire_end <- if (fire_pos > 0L) {
  # End at Search observeEvent which follows the helper.
  if (search_obs_pos > fire_pos) search_obs_pos - 1L else fire_pos + 3500L
} else {
  0L
}
fire_chunk <- if (fire_pos > 0L) substr(srv, fire_pos, fire_end) else ""
check(
  "fire helper does NOT auto-switch on 4-digit",
  !grepl("looks_like_tw_stock_code", fire_chunk, fixed = TRUE)
)

# No typing/input-change observer that switches market for sc.
check(
  "no observeEvent(input$sc) market switch",
  !grepl(
    "observeEvent\\(\\s*input\\$sc[\\s\\S]{0,800}?market_mode_pick",
    srv,
    perl = TRUE
  )
)
check(
  "no observeEvent(list(...sc...)) looks_like",
  !grepl(
    "observeEvent\\(\\s*list\\([^)]*input\\$sc[\\s\\S]{0,800}?looks_like_tw_stock_code",
    srv,
    perl = TRUE
  )
)

check(
  "market switch resolves list pending",
  grepl("is\\.list\\(pend\\)", srv, perl = TRUE) &&
    grepl("pend\\$symbol", srv, perl = TRUE) &&
    grepl("pend\\$goto_tab", srv, perl = TRUE)
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
