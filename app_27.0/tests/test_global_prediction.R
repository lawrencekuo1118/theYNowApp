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

# Fast-tier stand-in so TTL / invalidate tests do not need web_crawler.R.
if (requireNamespace("cachem", quietly = TRUE)) {
  .ynow_fast_cache <- cachem::cache_mem(max_size = 8 * 1024^2, max_age = 3600)
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

# ---- Freshness / crawl policy ----------------------------------------------
pol <- gp_crawl_policy()
check("policy quote TTL minutes-scale", is.finite(pol$quote_ttl_sec) && pol$quote_ttl_sec <= 15L * 60L)
check("policy discovery TTL > quote TTL", pol$discovery_ttl_sec > pol$quote_ttl_sec)
check("policy hard triggers listed", all(c("tab_open", "tab_focus", "mode_switch", "manual_refresh") %in% pol$hard_triggers))
check("policy fail_closed", isTRUE(pol$fail_closed))
check("policy never invent probs", isFALSE(pol$invent_probs))
check("policy fast cache tier", identical(pol$cache_tier, "fast"))
check("hard trigger helper", isTRUE(gp_is_hard_trigger("mode_switch")) && isFALSE(gp_is_hard_trigger("periodic_visible")))

live <- .gp_quote_row(
  venue = "kalshi", theme_id = "fed_rates", market_id = "LIVE-1",
  title = "Will the Fed cut rates?", yes_prob = 0.4,
  expires_at = "2099-01-01T00:00:00Z", raw_status = "active", breadth_tier = 1L
)
dead_status <- .gp_quote_row(
  venue = "kalshi", theme_id = "fed_rates", market_id = "DEAD-S",
  title = "Settled Fed cut", yes_prob = 0.9,
  expires_at = "2099-01-01T00:00:00Z", raw_status = "settled", breadth_tier = 1L
)
dead_time <- .gp_quote_row(
  venue = "polymarket", theme_id = "fed_rates", market_id = "DEAD-T",
  title = "Past FOMC window", yes_prob = 0.5,
  expires_at = "2020-01-01T00:00:00Z", raw_status = "open", breadth_tier = 1L
)
eq_live <- .gp_quote_row(
  venue = "polymarket", theme_id = "fed_rates", market_id = "EQ-1",
  title = "Will the Nasdaq-100 hit a new high?", yes_prob = 0.55,
  expires_at = "2099-06-01T00:00:00Z", raw_status = "open", breadth_tier = 1L
)
mixed <- rbind(live, dead_status, dead_time, eq_live)
filtered <- gp_filter_live_quotes(mixed)
check("filter drops settled status", !any(filtered$market_id == "DEAD-S"))
check("filter drops past expires_at", !any(filtered$market_id == "DEAD-T"))
check("filter keeps live", any(filtered$market_id == "LIVE-1"))

reclass <- gp_reclassify_quotes(mixed, market_mode = "US")
check("reclassify drops expired", !any(reclass$market_id %in% c("DEAD-S", "DEAD-T")))
check(
  "reclassify promotes equity title to equities_index",
  {
    row <- reclass[reclass$market_id == "EQ-1", , drop = FALSE]
    nrow(row) == 1L && identical(as.character(row$theme_id[[1]]), "equities_index") &&
      identical(as.integer(row$breadth_tier[[1]]), 2L)
  }
)
check(
  "reclassify keeps Fed title on fed_rates",
  {
    row <- reclass[reclass$market_id == "LIVE-1", , drop = FALSE]
    nrow(row) == 1L && identical(as.character(row$theme_id[[1]]), "fed_rates")
  }
)

# Stamped TTL cache: fresh hit, then expire, then invalidate.
if (exists(".ynow_fast_cache", inherits = FALSE) || exists(".ynow_fast_cache", inherits = TRUE)) {
  k <- .gp_cache_key("test", "ttl", "quote")
  payload <- list(ok = TRUE, quotes = live, error = NULL)
  check("cache key is cachem-safe", grepl("^[a-z0-9]+$", k))
  .gp_cache_set_fresh(k, payload, layer = "quote")
  hit <- .gp_cache_get_fresh(k, ttl_sec = 600L)
  check("TTL fresh hit", is.list(hit) && isTRUE(hit$ok))
  # Force-stale by rewriting cached_at into the past.
  .gp_cache_set(k, list(cached_at = Sys.time() - 9999, layer = "quote", value = payload))
  miss <- .gp_cache_get_fresh(k, ttl_sec = 60L)
  check("TTL expired miss", is.null(miss))
  .gp_cache_set_fresh(k, payload, layer = "quote")
  n_rm <- gp_invalidate_crawl_cache()
  check("invalidate removes gp keys", is.numeric(n_rm) && n_rm >= 1L)
  check("invalidate leaves key missing", is.null(.gp_cache_get_fresh(k, 600L)))
  # Legacy unstamped payloads must not stick forever.
  .gp_cache_set(k, payload)
  check("legacy unstamped treated as miss", is.null(.gp_cache_get_fresh(k, 600L)))
} else {
  check("TTL cache skipped (no cachem)", TRUE)
}

mod_src <- paste(readLines(file.path(app_dir, "global_prediction_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server wires page_focus", grepl("page_focus", mod_src, fixed = TRUE))
check("server wires periodic invalidateLater", grepl("invalidateLater", mod_src, fixed = TRUE))
check("server hard trigger tab_open", grepl("tab_open", mod_src, fixed = TRUE))
check("server hard trigger mode_switch", grepl("mode_switch", mod_src, fixed = TRUE))
check("gp_fetch_snapshot accepts trigger", grepl("trigger = \"periodic_visible\"", mod_src, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All global prediction checks passed.\n")
