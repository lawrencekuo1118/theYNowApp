# Price / condition alert rules. No SMTP and no live quotes.
# Run: cd app_22.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_price_alert.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
app_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath("..", mustWork = TRUE)
}
setwd(app_dir)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
suppressPackageStartupMessages({
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("htmltools required for alertable wrap checks")
  }
  library(htmltools)
})
source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("price_alert.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

check("gte hits at the target", isTRUE(price_alert_condition_met("price_gte", 800, 800)))
check("gte misses below", !isTRUE(price_alert_condition_met("price_gte", 800, 799.9)))
check("lte hits at the target", isTRUE(price_alert_condition_met("price_lte", 50, 50)))
check("lte misses above", !isTRUE(price_alert_condition_met("price_lte", 50, 50.01)))
check("day change +3 hits", isTRUE(price_alert_condition_met("day_chg_gte", 3, 103, 100)))
check("day change +2 misses +3", !isTRUE(price_alert_condition_met("day_chg_gte", 3, 102, 100)))
check("day change -4 hits lte -3", isTRUE(price_alert_condition_met("day_chg_lte", -3, 96, 100)))
check("fair MOS 20 hits at 80", isTRUE(price_alert_condition_met("below_fair", 100, 80, mos_pct = 20)))
check("fair MOS 20 misses at 81", !isTRUE(price_alert_condition_met("below_fair", 100, 81, mos_pct = 20)))
check("missing price is not a hit", !isTRUE(price_alert_condition_met("price_gte", 1, NA)))

Sys.setenv(YNOW_ALERT_TO = "owner@example.com", YNOW_ALERT_ALLOWLIST = "")
check("allowlist accepts owner", isTRUE(price_alert_email_allowed("Owner@Example.com")))
check("allowlist rejects stranger", !isTRUE(price_alert_email_allowed("other@example.com")))
check("bad ticker rejected", !isTRUE(price_alert_valid_ticker("AS ML")))
check("tw ticker ok", isTRUE(price_alert_valid_ticker("2330.TW")))

store <- tempfile(fileext = ".json")
hist <- tempfile(fileext = ".json")
Sys.setenv(YNOW_ALERT_STORE = store, YNOW_ALERT_HISTORY = hist)

added <- price_alert_add(
  price_alert_empty(), "owner@example.com", "ASML", "price_gte", 800, 0, "test",
  notify_app = FALSE, notify_email = TRUE
)
check("rule saved", isTRUE(added$ok) && nrow(added$df) == 1L)
blocked <- price_alert_add(added$df, "other@example.com", "ASML", "price_gte", 1,
                           notify_app = FALSE, notify_email = TRUE)
check("stranger cannot save", !isTRUE(blocked$ok) && identical(blocked$reason, "bad_email"))

app_only <- price_alert_add(
  price_alert_empty(), "", "ASML", "price_lte", 100, 0, "app",
  notify_app = TRUE, notify_email = FALSE, source = "kpi_quick", metric = "last_price"
)
check("app-only without email ok", isTRUE(app_only$ok) && isTRUE(app_only$df$notify_app[[1]]))
no_ch <- price_alert_add(price_alert_empty(), "", "ASML", "price_lte", 1,
                         notify_app = FALSE, notify_email = FALSE)
check("no channel rejected", !isTRUE(no_ch$ok) && identical(no_ch$reason, "no_channel"))

quotes <- list(ASML = list(ok = TRUE, price = 820, prev = 810))
sent_to <- character(0)
fake_send <- function(to, subject, body) {
  sent_to <<- c(sent_to, to)
  list(ok = TRUE, reason = "sent")
}
once <- price_alert_apply_checks(added$df, quotes, send_fn = fake_send, history_path = hist)
check("hit sends once", once$sent == 1L && identical(sent_to, "owner@example.com"))
check("sent rule is disarmed", isTRUE(!once$df$armed[[1]]) && identical(once$df$status[[1]], "sent"))
hist_df <- price_alert_history_read(hist)
check("history records email fire", nrow(hist_df) >= 1L && grepl("email", hist_df$channel[[1]]))
twice <- price_alert_apply_checks(once$df, quotes, send_fn = fake_send, history_path = hist)
check("disarmed rule does not send again", twice$sent == 0L && length(sent_to) == 1L)

no_smtp <- price_alert_apply_checks(
  added$df, quotes,
  send_fn = function(...) list(ok = FALSE, reason = "smtp_missing"),
  history_path = hist
)
check("smtp miss keeps the rule armed", isTRUE(no_smtp$df$armed[[1]]) && identical(no_smtp$df$status[[1]], "smtp_missing"))

app_hit <- price_alert_apply_checks(
  app_only$df, list(ASML = list(ok = TRUE, price = 90, prev = 95)),
  send_fn = function(...) list(ok = FALSE, reason = "should_not_email"),
  history_path = hist
)
check("app-only fires toast channel", length(app_hit$app_toasts) == 1L)
check("app-only disarms without email", isTRUE(!app_hit$df$armed[[1]]) && identical(app_hit$df$status[[1]], "notified_app"))

price_alert_write(added$df, store)
round <- price_alert_read(store)
check("json round trip", identical(round$ticker[[1]], "ASML") && isTRUE(round$armed[[1]]))

wrap <- ynow_alertable(tags$span("x"), metric = "last_price", ticker = "ASML", value = 10, ready = TRUE)
check("alertable ready attr", identical(wrap$attribs$`data-ynow-alert-ready`, "1"))
blocked_wrap <- ynow_alertable(tags$span("x"), metric = "last_price", ticker = "ASML", value = NA, ready = FALSE)
check("alertable blocked when not ready", identical(blocked_wrap$attribs$`data-ynow-alert-ready`, "0"))

en_keys <- names(.UI_STRINGS$en)
zh_keys <- names(.UI_STRINGS$`zh-TW`)
pa_keys <- en_keys[grepl("^pa_|^menu_price_alert$", en_keys)]
check("zh has price-alert keys", all(pa_keys %in% zh_keys))
need <- c(
  "pa_notify_app", "pa_notify_email", "pa_quick_not_ready", "pa_history_title",
  "pa_col_channel", "pa_no_channel"
)
check("new i18n keys present", all(need %in% en_keys) && all(need %in% zh_keys))

ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("gesture JS wired", grepl("ynow_kpi_alert_open", ui_txt, fixed = TRUE))
check("blocked input wired", grepl("ynow_kpi_alert_blocked", ui_txt, fixed = TRUE))
srv_txt <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("last price uses alertable", grepl("ynow_alertable", srv_txt, fixed = TRUE) &&
        grepl('metric = \"last_price\"', srv_txt, fixed = TRUE))

if (fail > 0L) quit(status = 1)
cat("ALL PASS\n")
