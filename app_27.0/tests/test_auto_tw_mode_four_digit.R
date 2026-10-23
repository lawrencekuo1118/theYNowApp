# US mode + 4-digit TW code → auto-switch to Taiwan market then search.
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

# --- server wires US→TW switch before normalize ---
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "fire helper calls looks_like_tw_stock_code",
  grepl("looks_like_tw_stock_code\\(raw\\)", srv, perl = TRUE)
)
check(
  "US mode gate before pending",
  grepl("identical\\(mode, \"US\"\\)", srv, perl = TRUE) &&
    grepl("looks_like_tw_stock_code", srv, fixed = TRUE)
)
check(
  "auto-switch sets market_mode_pick TW",
  grepl(
    "looks_like_tw_stock_code[\\s\\S]{0,600}?market_mode_pick', 'TW'",
    srv,
    perl = TRUE
  )
)
check(
  "auto-switch uses pending_recent_fetch list",
  grepl(
    "looks_like_tw_stock_code[\\s\\S]{0,400}?pending_recent_fetch\\(list\\(",
    srv,
    perl = TRUE
  )
)
check(
  "pending list preserves goto_tab",
  grepl("symbol = pend_sym", srv, fixed = TRUE) &&
    grepl("goto_tab = goto_tab", srv, fixed = TRUE)
)
check(
  "market switch resolves list pending",
  grepl("is\\.list\\(pend\\)", srv, perl = TRUE) &&
    grepl("pend\\$symbol", srv, perl = TRUE) &&
    grepl("pend\\$goto_tab", srv, perl = TRUE)
)
# Ensure auto-detect sits before US normalize (so 2330 is not treated as US).
fire_pos <- regexpr("\\.ynow_fire_ticker_search <- function", srv, perl = TRUE)[1]
fire_chunk <- if (fire_pos > 0L) {
  substr(srv, fire_pos, min(nchar(srv), fire_pos + 4500L))
} else {
  ""
}
like_rel <- regexpr("looks_like_tw_stock_code\\(raw\\)", fire_chunk, perl = TRUE)[1]
norm_rel <- regexpr(
  "normalize_ticker_for_market\\(raw, mode\\)",
  fire_chunk,
  perl = TRUE
)[1]
check(
  "TW detect before normalize in fire helper",
  fire_pos > 0L && like_rel > 0L && norm_rel > 0L && like_rel < norm_rel
)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
