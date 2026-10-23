#!/usr/bin/env Rscript
# Corp events: key-date chips, board election, CB dilution.
# Default: offline asserts (locale, dilution math, chips UI, wiring).
# Optional network: set YNOW_CORP_EVENTS_LIVE=1 for TWSE board/CB live smoke.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_corp_events.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    message("PASS: ", label)
  } else {
    fail <<- fail + 1L
    message("FAIL: ", label)
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_corp_events.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

suppressPackageStartupMessages({
  library(htmltools)
})

source(file.path(root, "ui_locale.R"), local = TRUE)

# Locale keys (en-US + zh-TW)
keys <- c(
  "key_date_fs_release", "key_date_earnings_call", "key_date_guidance",
  "key_date_guidance_soft", "tab_corp_events",
  "corp_board_title", "corp_cb_title", "corp_events_waiting",
  "corp_cb_dilution_help", "corp_board_us_note"
)
has_cjk <- function(s) grepl("[\u4e00-\u9fff]", s, perl = TRUE)
for (k in keys) {
  en <- ui_str(k, "en")
  zh <- ui_str(k, "zh-TW")
  check(paste("en", k), nzchar(en) && !has_cjk(en))
  check(paste("zh", k), nzchar(zh) && has_cjk(zh))
}
check("tab en Board", grepl("Board|CB", ui_str("tab_corp_events", "en")))
check("tab zh 董監", grepl("董監|可轉債", ui_str("tab_corp_events", "zh-TW")))

# Source corp_events helpers without full Shiny stack
source(file.path(root, "corp_events.R"), local = TRUE)

check("issuer 2330.TW", identical(.tw_issuer_code("2330.TW"), "2330"))
check("issuer 1101.TWO", identical(.tw_issuer_code("1101.TWO"), "1101"))
check("roc date 1150528", identical(.tw_packed_date_to_iso("1150528"), "2026-05-28"))
check("ymd 20250311", identical(.tw_packed_date_to_iso("20250311"), "2025-03-11"))

# Dilution math: 8e9 face / 36.5 price → ~219.178M shares; vs 1e9 shares → ~21.9%
cb <- data.frame(
  scope = "domestic",
  bond_code = "11011",
  short_name = "test",
  issue_amount = 8e9,
  outstanding = 8e9,
  conv_price = 36.5,
  currency = "1",
  period_start = "2025-03-11",
  period_end = "2029-12-10",
  maturity = "2029-12-10",
  coupon = 0,
  stringsAsFactors = FALSE
)
dil <- .compute_cb_dilution(cb, shares_out = 1e9, fx_usd_twd = 32)
check("potential shares finite", is.finite(dil$potential_shares) && dil$potential_shares > 2e8)
check("dilution pct ~21.9", is.finite(dil$dilution_pct) && dil$dilution_pct > 20 && dil$dilution_pct < 23)

# Overseas USD face
cb2 <- cb
cb2$currency <- "2"
cb2$outstanding <- 1e8
cb2$conv_price <- 40
dil2 <- .compute_cb_dilution(cb2, shares_out = 1e9, fx_usd_twd = 32)
# (1e8 * 32) / 40 = 80e6 → 8%
check("usd cb dilution ~8%", is.finite(dil2$dilution_pct) && abs(dil2$dilution_pct - 8) < 0.2)

# Date chips UI (no dates → NULL)
check("empty chips NULL", is.null(.ynow_key_date_chips_ui(NULL, "US", "en")))
chips <- .ynow_key_date_chips_ui(
  list(
    earnings_date = "2026-11-02",
    earnings_call = "2026-11-02",
    guidance_date = "2026-11-02",
    guidance_soft = TRUE,
    is_estimate = FALSE
  ),
  market_mode = "US",
  loc = "en"
)
check("chips html class", inherits(chips, "shiny.tag") || inherits(chips, "shiny.tag.list"))
chip_html <- as.character(chips)
check("chip class present", grepl("ynow-key-date-chip", chip_html, fixed = TRUE))
check("US guidance chip", grepl("Guidance", chip_html, fixed = TRUE))

# Wiring
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE), collapse = "\n")
gl <- paste(readLines(file.path(root, "global.R"), warn = FALSE), collapse = "\n")
py <- paste(readLines(file.path(root, "deep_scraper.py"), warn = FALSE), collapse = "\n")

check("global sources corp_events", grepl('source("corp_events.R"', gl, fixed = TRUE))
check("ui corp_events tab", grepl('value = "corp_events"', ui, fixed = TRUE))
check("ui key-date CSS", grepl("ynow-key-date-chip", ui, fixed = TRUE))
check("ui fund-profile-like gold", grepl("ynow-key-date-chip", ui, fixed = TRUE) &&
        grepl("--ynow-gold", ui, fixed = TRUE))
check("server company_key_dates", grepl("company_key_dates", srv, fixed = TRUE))
check("server corp_events_ui", grepl("output\\$corp_events_ui", srv))
check("server board election fetch", grepl("cached_fetch_tw_board_election", srv, fixed = TRUE))
check("server cb fetch", grepl("fetch_tw_convertible_bonds_live", srv, fixed = TRUE))
check("py get_company_key_dates", grepl("def get_company_key_dates", py, fixed = TRUE))
check("locale tab map corp_events", grepl('"corp_events" = "tab_corp_events"', 
  paste(readLines(file.path(root, "ui_locale.R"), warn = FALSE), collapse = "\n"), fixed = TRUE))

# Optional live smoke (network) — not part of default offline path
live <- identical(Sys.getenv("YNOW_CORP_EVENTS_LIVE"), "1")
if (isTRUE(live)) {
  board <- tryCatch(fetch_tw_board_election("2330.TW"), error = function(e) NULL)
  check("live board 2330 has_data", isTRUE(board$has_data))
  check("live board 2330 meeting date", nzchar(board$meeting_date %||% ""))
  cb_live <- tryCatch(
    fetch_tw_convertible_bonds("1101.TW", shares_out = 7.7e9, fx_usd_twd = 32),
    error = function(e) NULL
  )
  check("live cb 1101 has_data", isTRUE(cb_live$has_data))
  check("live cb 1101 dilution", is.finite(cb_live$dilution_pct))
} else {
  message("SKIP live corp_events smoke (set YNOW_CORP_EVENTS_LIVE=1 to enable)")
}

if (fail > 0L) {
  message("FAILED: ", fail)
  quit(status = 1L)
}
message("All corp_events offline checks passed.")
