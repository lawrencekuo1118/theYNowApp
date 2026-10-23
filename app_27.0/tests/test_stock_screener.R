# Custom screen conditions and the value-alert cross-link.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_stock_screener.R

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
source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("price_alert.R", local = FALSE, encoding = "UTF-8")
source("stock_screener.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

universe <- data.frame(
  ticker = c("AAA", "BBB", "2330.TW", "1101.TW"),
  name = c("Alpha", "Beta", "TSMC", "TCC"),
  stringsAsFactors = FALSE
)
features <- data.frame(
  ticker = c("AAA", "BBB", "2330.TW", "1101.TW"),
  market_cap = c(2e11, 5e9, 1e12, NA),
  ROE = c(18, 9, 22, 15),
  Operating_Margin = c(25, 10, 40, 12),
  Rev_YoY = c(12, 4, 20, NA),
  OpInc_YoY = c(8, 1, 15, 3),
  Debt_Ratio = c(20, 70, 15, 40),
  PE_Ratio = c(14, 30, 18, NA),
  PB_Ratio = c(2.5, 1.2, 4, 1.1),
  stringsAsFactors = FALSE
)
metrics <- data.frame(
  ticker = c("AAA", "BBB", "2330.TW", "1101.TW"),
  market_cap = c(NA, NA, NA, 8e9),
  ret_1y = c(0.25, -0.10, 0.40, 0.05),
  stringsAsFactors = FALSE
)

us <- screener_assemble(universe, features, metrics, "US")
tw <- screener_assemble(universe, features, metrics, "TW")
check("US pool drops TW tickers", identical(sort(us$ticker), c("AAA", "BBB")))
check("TW pool keeps .TW", identical(sort(tw$ticker), c("1101.TW", "2330.TW")))
check("market cap in USD bn", isTRUE(abs(us$market_cap_bn[us$ticker == "AAA"] - 200) < 1e-6))
check("TW market cap falls back to metrics", isTRUE(abs(tw$market_cap_bn[tw$ticker == "1101.TW"] - 8) < 1e-6))
check("1-year return is percent", isTRUE(abs(us$ret_1y_pct[us$ticker == "AAA"] - 25) < 1e-6))

conds <- data.frame(
  use = c(TRUE, TRUE, FALSE),
  field = c("roe", "pe", "debt_ratio"),
  op = c("gte", "lte", "lte"),
  value = c(15, 20, 50),
  stringsAsFactors = FALSE
)
hit <- screener_apply(us, conds)
check("AND keeps AAA", isTRUE(hit$ok) && identical(hit$df$ticker, "AAA"))
check("low ROE dropped", !"BBB" %in% hit$df$ticker)
check("unchecked debt row ignored", identical(hit$conditions$field, c("roe", "pe")))

na_pe <- screener_apply(tw, data.frame(
  field = "pe", op = "lte", value = 100, stringsAsFactors = FALSE
))
check("missing P/E does not pass", isTRUE(na_pe$ok) && identical(na_pe$df$ticker, "2330.TW"))

edge <- screener_apply(us, data.frame(field = "roe", op = "gte", value = 18, stringsAsFactors = FALSE))
check("gte includes the boundary", identical(edge$df$ticker, "AAA"))

none <- screener_apply(us, data.frame(
  use = FALSE, field = "roe", op = "gte", value = 1, stringsAsFactors = FALSE
))
check("no active condition", identical(none$reason, "no_conditions") && !isTRUE(none$ok))

bad <- screener_apply(us, data.frame(
  field = "roe", op = "gte", value = NA_real_, stringsAsFactors = FALSE
))
check("non-finite level rejected", identical(bad$reason, "bad_value"))

label <- screener_condition_label(conds, "zh-TW")
check("zh label uses ROE and P/E", grepl("ROE", label, fixed = TRUE) && grepl("P/E", label, fixed = TRUE))
check("zh label has traditional growth wording available", identical(
  .UI_STRINGS$`zh-TW`$scr_field_rev_yoy, "營收年增率 (%)"
))

en_keys <- names(.UI_STRINGS$en)
zh_keys <- names(.UI_STRINGS$`zh-TW`)
scr_keys <- en_keys[grepl("^scr_|^pa_col_screen$", en_keys)]
check("zh has screener keys", length(scr_keys) > 10L && all(scr_keys %in% zh_keys))

ref <- "scr_test_0001|ROE (%) ≥ 15; P/E ≤ 20"
check("screen text strips id", identical(
  price_alert_screen_text(ref), "ROE (%) ≥ 15; P/E ≤ 20"
))

store <- tempfile(fileext = ".json")
Sys.setenv(YNOW_ALERT_STORE = store)
on.exit(unlink(store), add = TRUE)

alerts <- price_alert_empty()
added <- price_alert_add(
  alerts, "", "AAA", "price_lte", 100, 0, "",
  notify_app = TRUE, notify_email = FALSE,
  source = "screener", screen_ref = ref
)
check("screener alert saved", isTRUE(added$ok) && identical(added$df$source[[1]], "screener"))
check("screen_ref stored on the alert", identical(added$df$screen_ref[[1]], ref))
linked <- screener_alerts_for_ref(added$df, ref)
check("lookup by screen ref", identical(linked$ticker, "AAA"))

marked <- screener_mark_alerts(hit$df, added$df)
check("result highlights ticker that already has an alert", isTRUE(marked$has_alert[[1]]))
plain <- screener_mark_alerts(us, added$df)
check("other ticker stays unmarked", isTRUE(!plain$has_alert[plain$ticker == "BBB"]))

# Legacy JSON without screen_ref still loads.
legacy <- added$df
legacy$screen_ref <- NULL
jsonlite::write_json(legacy, store, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE, na = "null")
round <- price_alert_read(store)
check("legacy alert gains empty screen_ref", identical(round$screen_ref[[1]], ""))

price_alert_write(added$df, store)
round2 <- price_alert_read(store)
check("screen_ref round trip", identical(round2$screen_ref[[1]], ref))
check("alert list text is the condition", identical(price_alert_screen_text(round2$screen_ref), "ROE (%) ≥ 15; P/E ≤ 20"))

made <- screener_make_ref("ROE (%) ≥ 15")
check("generated ref keeps the label", identical(price_alert_screen_text(made), "ROE (%) ≥ 15"))

# Bundled snapshot: conditions only, no invented fields.
bundled <- screener_load_pool("US", app_dir)
bhit <- screener_apply(bundled, data.frame(
  use = c(TRUE, TRUE),
  field = c("roe", "pe"),
  op = c("gte", "lte"),
  value = c(15, 15),
  stringsAsFactors = FALSE
))
check("bundled US screen runs", isTRUE(bhit$ok))
if (nrow(bhit$df)) {
  check("bundled hits meet ROE and P/E", all(bhit$df$roe >= 15 & bhit$df$pe <= 15))
  check("bundled hits are US tickers", !any(grepl("\\.(TW|TWO)$", bhit$df$ticker)))
} else {
  check("bundled hits meet ROE and P/E", TRUE)
  check("bundled hits are US tickers", TRUE)
}

catalog <- screener_condition_catalog()
check("catalog has master packs", all(c("buffett", "graham") %in% catalog$pack))
check("catalog fields are known", all(catalog$field %in% screener_field_ids()))

# UI placement: Custom screen under Blue Chip after Clustering; Value Alerts = sibling tabs.
ui_txt <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pa_txt <- paste(readLines(file.path(app_dir, "price_alert.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
scr_src <- paste(readLines(file.path(app_dir, "stock_screener.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "bluechip hosts screener after cluster",
  grepl(
    'value = "im_cluster"[\\s\\S]*value = "im_screener"[\\s\\S]*screener_panel_ui\\(embed = "bluechip"\\)',
    ui_txt,
    perl = TRUE
  )
)
check(
  "value alerts use sibling tabs for rules + history",
  grepl('value = "pa_alerts"', pa_txt, fixed = TRUE) &&
    grepl('value = "pa_history"', pa_txt, fixed = TRUE) &&
    !grepl("screener_panel_ui", pa_txt, fixed = TRUE)
)
check(
  "screener panel supports bluechip embed",
  grepl('embed = c\\("standalone", "bluechip"\\)', scr_src, perl = TRUE)
)
check(
  "screen rows are added from Value Alerts settings",
  grepl('actionButton\\("scr_add"', pa_txt) &&
    !grepl("scr_alert_kind", scr_src, fixed = TRUE) &&
    grepl("input\\$pa_kind", scr_src) &&
    grepl("input\\$pa_level", scr_src)
)
check(
  "locale has tab_im_screener both sides",
  identical(.UI_STRINGS$en$tab_im_screener, "Custom Screen") &&
    identical(.UI_STRINGS$`zh-TW`$tab_im_screener, "自訂選股")
)

payload_json <- jsonlite::toJSON(
  list(
    list(id = "m_buffett_roe", field = "roe", op = "gte", value = 15),
    list(id = "m_buffett_debt", field = "debt_ratio", op = "lte", value = 30)
  ),
  auto_unbox = TRUE
)
from_dnd <- screener_payload_to_conditions(as.character(payload_json))
check("dnd payload becomes conditions", nrow(from_dnd) == 2L && all(from_dnd$use))
dnd_hit <- screener_apply(us, from_dnd)
check("dnd Buffett-like filter keeps AAA", isTRUE(dnd_hit$ok) && "AAA" %in% dnd_hit$df$ticker)
check("empty dnd payload is no conditions", identical(
  screener_payload_to_conditions("[]")$field, character(0)
))
card_keys <- paste0("scr_card_", catalog$id)
check("en card labels present", all(card_keys %in% names(.UI_STRINGS$en)))
check("zh card labels present", all(card_keys %in% names(.UI_STRINGS$`zh-TW`)))
check("dnd locale keys paired", all(c(
  "scr_save", "scr_clear", "scr_drop_hint", "scr_cat_masters", "scr_pack_buffett"
) %in% names(.UI_STRINGS$en) & c(
  "scr_save", "scr_clear", "scr_drop_hint", "scr_cat_masters", "scr_pack_buffett"
) %in% names(.UI_STRINGS$`zh-TW`)))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("PASS stock_screener\n")
