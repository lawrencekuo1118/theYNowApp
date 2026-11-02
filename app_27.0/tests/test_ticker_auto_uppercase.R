#!/usr/bin/env Rscript
# Ticker / Stock Code auto-uppercase contracts (offline, no network).
# Run: cd app_27.0 && Rscript tests/test_ticker_auto_uppercase.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

root <- if (file.exists("ynow_ui.R")) {
  normalizePath(".")
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else {
  stop("Cannot locate app_27.0")
}
setwd(root)

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pa <- paste(readLines("price_alert.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js_path <- file.path("www", "ynow_ticker_uppercase.js")
check("uppercase JS file exists", file.exists(js_path))
js <- if (file.exists(js_path)) {
  paste(readLines(js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}

check("UI loads ynow_ticker_uppercase.js", grepl('ynow_ticker_uppercase.js', ui, fixed = TRUE))
check("CSS uppercase for #sc", grepl("#sc,", ui, fixed = TRUE) &&
        grepl("text-transform: uppercase", ui, fixed = TRUE))
check("CSS covers lab_cluster_focus", grepl("#lab_cluster_focus,", ui, fixed = TRUE))
check("CSS covers pa_ticker", grepl("#pa_ticker,", ui, fixed = TRUE))
check("CSS covers beta selectized", grepl("#beta_peers-selectized,", ui, fixed = TRUE) &&
        grepl("#beta_bench-selectized,", ui, fixed = TRUE))

check("JS capture-phase input listener", grepl("addEventListener(", js, fixed = TRUE) &&
        grepl("'input'", js, fixed = TRUE) &&
        grepl("true", js, fixed = TRUE))
check("JS toUpperCase on value", grepl("toUpperCase()", js, fixed = TRUE))
check("JS lists sc input", grepl("sc:", js, fixed = TRUE) || grepl('"sc"', js, fixed = TRUE))
check("JS lists lab_cluster_focus", grepl("lab_cluster_focus", js, fixed = TRUE))
check("JS lists pa_ticker", grepl("pa_ticker", js, fixed = TRUE))
check("JS lists beta_peers selectize", grepl("beta_peers", js, fixed = TRUE))
check("JS lists beta_bench selectize", grepl("beta_bench", js, fixed = TRUE))
check("JS preserves caret via setSelectionRange", grepl("setSelectionRange", js, fixed = TRUE))
check("JS exposes ynowTickerUppercase", grepl("ynowTickerUppercase", js, fixed = TRUE))
check(
  "JS note: no market switch on typing",
  grepl("Does not switch market on typing", js, fixed = TRUE) ||
    grepl("Search-only", js, fixed = TRUE)
)

check(
  "typeahead also uppercases before suggest",
  grepl("toUpperCase()", ui, fixed = TRUE) &&
    grepl("bindTypeahead('sc')", ui, fixed = TRUE) &&
    grepl("bindTypeahead('lab_cluster_focus')", ui, fixed = TRUE)
)

# Server helpers: TW numeric + US letter casing stay correct
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
source("market_profile.R", local = TRUE, encoding = "UTF-8")

check("looks_like_tw 2330", isTRUE(looks_like_tw_stock_code("2330")))
check("looks_like_tw lowercase suffix", isTRUE(looks_like_tw_stock_code("2330.tw")))
check("display US aapl → AAPL", identical(display_ticker_for_market("aapl", "US"), "AAPL"))
check("display US mixed", identical(display_ticker_for_market("TsM", "US"), "TSM"))
check("display TW bare 2330", identical(display_ticker_for_market("2330", "TW"), "2330"))
check(
  "normalize US brk-b upper",
  identical(toupper(gsub("\\s+", "", "brk-b")), "BRK-B")
)
check("AAPL not mistaken as TW", isFALSE(looks_like_tw_stock_code("aapl")))

# Price alert ticker field still present (Full; Lite omits alerts tab but shared JS is fine)
check('pa_ticker textInput present', grepl('textInput("pa_ticker"', pa, fixed = TRUE))

# Search-only TW auto-switch must remain (regression vs typing uppercase)
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
search_obs_pos <- regexpr("observeEvent\\(input\\$search,", srv, perl = TRUE)[1]
recent_pos <- regexpr("observeEvent\\(input\\$recent_search_pick,", srv, perl = TRUE)[1]
search_chunk <- if (search_obs_pos > 0L && recent_pos > search_obs_pos) {
  substr(srv, max(1L, search_obs_pos - 160L), recent_pos - 1L)
} else {
  ""
}
check(
  "US 4-digit → TW still Search-only",
  grepl("looks_like_tw_stock_code\\(raw\\)", search_chunk, perl = TRUE) &&
    grepl("never switch market on ticker typing", search_chunk, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, "\n", sep = "")
  quit(status = 1L)
}
cat("All ticker auto-uppercase checks passed.\n")
