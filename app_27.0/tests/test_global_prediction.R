#!/usr/bin/env Rscript
# Global Prediction (ATX child): registry, parsers, crowd agreement, brand colors, UI wiring.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_global_prediction.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
fx_dir <- file.path(test_dir, "fixtures", "global_prediction")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

sys.source(
  file.path(app_dir, "global_prediction_module.R"),
  envir = environment(),
  keep.source = FALSE
)

reg <- gp_theme_registry("US")
check("US registry has 5 themes", nrow(reg) == 5L)
check(
  "US registry order macro→index",
  identical(
    as.character(reg$theme_id),
    c("fed_rates", "inflation_cpi", "recession", "treasury_yields", "equities_index")
  )
)
check("US breadth tiers nondecreasing", !is.unsorted(as.integer(reg$breadth_tier)))
reg_tw <- gp_theme_registry("TW")
check("TW registry has 5 themes", nrow(reg_tw) == 5L)
check(
  "TW registry order macro→index→sector",
  identical(
    as.character(reg_tw$theme_id),
    c("fed_rates", "inflation_cpi", "tw_macro", "tw_equity", "tw_housing")
  )
)
check("TW breadth tiers nondecreasing", !is.unsorted(as.integer(reg_tw$breadth_tier)))
check("TW and US both keep Fed overlap", "fed_rates" %in% reg_tw$theme_id && "fed_rates" %in% reg$theme_id)
check("no single-stock tier ahead of macro", {
  max_macro <- max(reg$breadth_tier[reg$theme_id %in% c("fed_rates", "inflation_cpi", "recession")])
  min_eq <- min(reg$breadth_tier[reg$theme_id == "equities_index"])
  max_macro < min_eq || max_macro == min_eq
})

cols <- gp_brand_colors()
check("Kalshi brand hex", identical(cols$kalshi, "#00D66C"))
check("Polymarket brand hex", identical(cols$polymarket, "#2E5CFF"))
check("FEEX brand hex", identical(cols$feex, "#C17A2E"))

feex <- gp_fetch_feex_theme("tw_equity", use_cache = FALSE)
check("FEEX honest empty ok=FALSE", isFALSE(feex$ok))
check("FEEX never invents quotes", is.data.frame(feex$quotes) && nrow(feex$quotes) == 0L)
check("FEEX status defunct", identical(feex$status, "defunct") || identical(feex$error, "feex_no_public_feed"))

kalshi_raw <- jsonlite::fromJSON(
  file.path(fx_dir, "kalshi_markets.json"),
  simplifyVector = FALSE
)
kq <- gp_parse_kalshi_markets(kalshi_raw, theme_id = "fed_rates", limit = 3L)
check("kalshi parse rows", is.data.frame(kq) && nrow(kq) >= 1L)
check("kalshi venue tag", all(kq$venue == "kalshi"))
check("kalshi yes_prob in [0,1]", all(is.finite(kq$yes_prob) & kq$yes_prob >= 0 & kq$yes_prob <= 1))
check("kalshi no invented on empty", nrow(gp_parse_kalshi_markets(list(), "fed_rates")) == 0L)
check(
  "kalshi bad json path empty",
  nrow(gp_parse_kalshi_markets(list(markets = list(list(ticker = "X"))), "fed_rates")) == 0L
)

poly_raw <- jsonlite::fromJSON(
  file.path(fx_dir, "polymarket_events.json"),
  simplifyVector = FALSE
)
pq <- gp_parse_polymarket_events(poly_raw, theme_id = "fed_rates", limit = 3L)
check("polymarket parse rows", is.data.frame(pq) && nrow(pq) >= 1L)
check("polymarket venue tag", all(pq$venue == "polymarket"))
check("polymarket yes_prob in [0,1]", all(is.finite(pq$yes_prob) & pq$yes_prob >= 0 & pq$yes_prob <= 1))
check("polymarket empty fail-closed", nrow(gp_parse_polymarket_events(list(), "fed_rates")) == 0L)
check("bad json string is empty", nrow(gp_parse_kalshi_markets("{", "fed_rates")) == 0L)
check("null json is empty", nrow(gp_parse_kalshi_markets("null", "fed_rates")) == 0L)

cents_path <- file.path(fx_dir, "kalshi_cents.json")
if (file.exists(cents_path)) {
  cents <- gp_parse_kalshi_markets(
    jsonlite::fromJSON(cents_path, simplifyVector = FALSE),
    theme_id = "fed_rates",
    limit = 10L
  )
  legacy <- cents$yes_prob[cents$market_id == "KXRATECUT-LEGACY"]
  check("legacy cents are a probability", length(legacy) == 1L && isTRUE(all.equal(legacy, 0.45)))
  check(
    "out-of-range dollar text is not rescaled",
    !any(cents$market_id %in% c("KXRATECUT-BADTEXT", "KXRATECUT-OOB"))
  )
}

search_path <- file.path(fx_dir, "polymarket_search.json")
if (file.exists(search_path)) {
  swapped <- gp_parse_polymarket_events(
    jsonlite::fromJSON(search_path, simplifyVector = FALSE),
    theme_id = "fed_rates",
    limit = 10L,
    keywords = c("fed", "fomc", "rate cut")
  )
  swap_p <- swapped$yes_prob[swapped$market_id == "swap-yes"]
  named_p <- swapped$yes_prob[swapped$market_id == "616902"]
  check("Yes price follows the Yes label", length(named_p) == 1L && isTRUE(all.equal(named_p, 0.965)))
  check("Yes is not position zero", length(swap_p) == 1L && isTRUE(all.equal(swap_p, 0.80)))
  check(
    "unlabeled outcome is dropped",
    !any(swapped$market_id %in% c("no-yes-leg", "closed-fed", "election-1"))
  )
}

ag <- gp_crowd_agreement(c(0.40, 0.42), c(0.41, 0.43))
check("crowd agree status", identical(ag$status, "agree"))
check("crowd agree has simple_mean", is.finite(ag$simple_mean))

dg <- gp_crowd_agreement(0.20, 0.80)
check("crowd diverge status", identical(dg$status, "diverge"))

mx <- gp_crowd_agreement(0.40, 0.55)
check("crowd mixed status", identical(mx$status, "mixed"))

ins <- gp_crowd_agreement(numeric(0), 0.5)
check("crowd insufficient one-sided", identical(ins$status, "insufficient"))
check("crowd insufficient no fake mean", !is.finite(ins$simple_mean))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glob <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global.R sources module", grepl("global_prediction_module.R", glob, fixed = TRUE))
check("ATX page has Global Prediction tab", grepl("ynow_atx_tab_global_prediction", ui, fixed = TRUE))
check("ATX page mounts global_prediction_ui", grepl("global_prediction_ui(", ui, fixed = TRUE))
check("server wires global_prediction_server", grepl("global_prediction_server(", srv, fixed = TRUE))
check("server passes market_mode_rv", grepl("market_mode_rv = market_mode", srv, fixed = TRUE))
check("locale en tab title", grepl('atx_tab_global_prediction = "Global Prediction"', loc, fixed = TRUE))
check("locale zh tab title", grepl("atx_tab_global_prediction = \"全球預測\"", loc, fixed = TRUE))
check("locale en wisdom framing", grepl("Wisdom of crowds", loc, fixed = TRUE))
check("locale zh wisdom framing", grepl("群眾智慧", loc, fixed = TRUE))
check("locale en not buy signal", grepl("not a buy signal", loc, fixed = TRUE))
check("locale zh not buy signal", grepl("不是買進訊號", loc, fixed = TRUE))
check("locale en desk vs crowd", grepl("crowd-priced", loc, fixed = TRUE))
check("locale zh desk vs crowd", grepl("群眾定價", loc, fixed = TRUE))
check("locale en participation bias", grepl("self-selected", loc, fixed = TRUE))
check("locale zh participation bias", grepl("自我選擇", loc, fixed = TRUE))
check("locale en risk appetite", grepl("risk appetite", loc, fixed = TRUE))
check("locale zh risk appetite", grepl("風險偏好", loc, fixed = TRUE))
check("locale en Galton mean label", grepl("Galton-style aggregate", loc, fixed = TRUE))
check("locale zh Galton mean label", grepl("Galton 式加總", loc, fixed = TRUE))
check("locale en herding caution", grepl("Herding", loc, fixed = TRUE))
check("locale zh herding caution", grepl("羊群", loc, fixed = TRUE))
check("locale feex venue name", grepl("gp_venue_feex = \"未來事件交易所\"", loc, fixed = TRUE))
check("locale feex unavailable en", grepl("no live public market feed", loc, fixed = TRUE))
check("locale feex unavailable zh", grepl("沒有可用的公開即時行情", loc, fixed = TRUE))
check("locale mode us", grepl("gp_mode_us", loc, fixed = TRUE))
check("locale mode tw", grepl("gp_mode_tw", loc, fixed = TRUE))
check("applyUiLocale gp lead", grepl("ynow_gp_lead", ui, fixed = TRUE))
check("applyUiLocale gp participation", grepl("ynow_gp_participation", ui, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All global prediction checks passed.\n")
