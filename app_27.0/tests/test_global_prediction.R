#!/usr/bin/env Rscript
# Global Prediction (standalone): boards, parsers, crowd agreement, brand colors, UI wiring.
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

check(
  "board ids Economic/Finance/Company",
  identical(gp_board_ids(), c("economic", "finance", "company"))
)
check("norm board aliases", identical(gp_norm_board("econ"), "economic") &&
        identical(gp_norm_board("Financials"), "finance") &&
        identical(gp_norm_board("stocks"), "company"))

eco <- gp_board_sources("economic")
fin <- gp_board_sources("finance")
co <- gp_board_sources("company")
check("economic monthly+ freq", all(c("monthly", "annual") %in% eco$kalshi_freq))
check("finance monthly+ freq", all(c("monthly", "annual") %in% fin$kalshi_freq))
check("economic kalshi Economics", "Economics" %in% eco$kalshi_categories)
check("economic poly economy+commodities", all(c("economy", "commodities") %in% eco$poly_tags))
check("finance kalshi Financials", "Financials" %in% fin$kalshi_categories)
check(
  "finance poly monthly+indices",
  any(c("monthly", "indices", "indicies") %in% fin$poly_tags)
)
check("company filter on", isTRUE(co$company_filter))
check("company poly tech/stocks/earnings", all(c("tech", "stocks", "earnings") %in% co$poly_tags))
check(
  "economic themes cover Fed→trade",
  all(c(
    "fed_rates", "inflation_cpi", "growth_jobs", "recession_labor",
    "housing", "energy_commod", "trade_policy", "econ_other"
  ) %in% vapply(eco$theme_buckets, function(b) b$id, character(1)))
)
check(
  "finance themes cover rates→ipo",
  all(c(
    "rates_credit", "equity_index", "vol_credit", "fx_commod_fin",
    "ipo_ma", "fin_other"
  ) %in% vapply(fin$theme_buckets, function(b) b$id, character(1)))
)
check(
  "company themes cover mega/semis/TW/earnings",
  all(c(
    "us_mega", "semis", "tw_listed", "earnings", "co_other"
  ) %in% vapply(co$theme_buckets, function(b) b$id, character(1)))
)

reg <- gp_theme_registry("US")
check("legacy registry maps Economic board", nrow(reg) >= 6L)
check("legacy registry has fed_rates", "fed_rates" %in% reg$theme_id)
check("breadth tiers nondecreasing", !is.unsorted(as.integer(reg$breadth_tier)))

check("company TW/US filter TSMC", isTRUE(gp_is_tw_us_listed_title("Will TSMC (2330) beat EPS?")))
check("company TW/US filter Apple", isTRUE(gp_is_tw_us_listed_title("Apple (AAPL) market cap")))
check("company filter rejects politics", isFALSE(gp_is_tw_us_listed_title("Who wins the election?")))

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
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("global.R sources module", grepl("global_prediction_module.R", glob, fixed = TRUE))
check("standalone sidebar tab", grepl('tabName = "global_prediction"', ui, fixed = TRUE))
check(
  "sidebar markup: ATX then GP then Blue Chip (above dashed separator)",
  {
    m_atx <- regexpr('tabName = "asset_transmission"', ui, fixed = TRUE)
    m_gp <- regexpr('tabName = "global_prediction"', ui, fixed = TRUE)
    m_bc <- regexpr('tabName = "bluechip"', ui, fixed = TRUE)
    m_atx[1] > 0 && m_gp[1] > 0 && m_bc[1] > 0 &&
      m_atx[1] < m_gp[1] && m_gp[1] < m_bc[1]
  }
)
check("lazy host for GP page", grepl('ynow_lazy_host_global_prediction', ui, fixed = TRUE))
check("page UI mounts global_prediction_ui", grepl("global_prediction_ui(", ui, fixed = TRUE))
check("ATX no longer hosts GP subtab", !grepl("ynow_atx_tab_global_prediction", ui, fixed = TRUE))
check("server wires global_prediction_server", grepl("global_prediction_server(", srv, fixed = TRUE))
check("server passes market_mode_rv", grepl("market_mode_rv = market_mode", srv, fixed = TRUE))
check("lazy page key global_prediction", grepl("global_prediction = function()", srv, fixed = TRUE))
check("locale en menu", grepl('menu_global_prediction = "Global Prediction"', loc, fixed = TRUE))
check("locale zh menu", grepl('menu_global_prediction = "全球預測"', loc, fixed = TRUE))
check("locale en boards", grepl('gp_board_economic = "Economic"', loc, fixed = TRUE) &&
        grepl('gp_board_finance = "Finance"', loc, fixed = TRUE) &&
        grepl('gp_board_company = "Company"', loc, fixed = TRUE))
check("locale zh boards", grepl("gp_board_economic", loc, fixed = TRUE) &&
        grepl("gp_board_finance", loc, fixed = TRUE) &&
        grepl("gp_board_company", loc, fixed = TRUE))
check("locale auth gate en", grepl("auth_gate_gp_kicker", loc, fixed = TRUE))
check("locale auth gate zh", grepl("解鎖全球預測", loc, fixed = TRUE))
check("locale en wisdom framing", grepl("Wisdom of crowds", loc, fixed = TRUE))
check("locale zh wisdom framing", grepl("群眾智慧", loc, fixed = TRUE))
check("locale en not buy signal", grepl("not a buy signal", loc, fixed = TRUE))
check("locale zh not buy signal", grepl("不是買進訊號", loc, fixed = TRUE))
check("locale en participation bias", grepl("self-selected", loc, fixed = TRUE))
check("locale zh participation bias", grepl("自我選擇", loc, fixed = TRUE))
check("locale en Galton mean label", grepl("Galton-style aggregate", loc, fixed = TRUE))
check("locale zh Galton mean label", grepl("Galton 式加總", loc, fixed = TRUE))
check("locale en herding caution", grepl("Herding", loc, fixed = TRUE))
check("locale zh herding caution", grepl("羊群", loc, fixed = TRUE))
check("locale feex venue name", grepl("gp_venue_feex = \"未來事件交易所\"", loc, fixed = TRUE))
check("gate layout includes gp", grepl(
  'c("lab", "checklist", "alerts", "atx", "gp")',
  auth,
  fixed = TRUE
))
check(
  "promo features include gp",
  grepl('ynow_gate_demo_features(), "gp"', auth, fixed = TRUE) ||
    grepl('unique(c(ynow_gate_demo_features(), "gp"))', auth, fixed = TRUE)
)
check("page gate uses feature gp", grepl('feature = "gp"', ui, fixed = TRUE))
check("applyUiLocale gp lead", grepl("ynow_gp_lead", ui, fixed = TRUE))
check("applyUiLocale gp page title", grepl("ynow_gp_page_title", ui, fixed = TRUE))
check("applyUiLocale gp participation", grepl("ynow_gp_participation", ui, fixed = TRUE))
check("guest lock map includes GP", grepl("global_prediction:true", ui, fixed = TRUE) ||
        grepl("global_prediction: true", ui, fixed = TRUE))
check(
  "GP page hides global Ticker / Yahoo industry chrome",
  grepl("input.sidebar_tabs != 'global_prediction'", ui, fixed = TRUE) &&
    grepl("ynow-stock-chrome", ui, fixed = TRUE) &&
    grepl("ynow_industry_info_yahoo", ui, fixed = TRUE)
)

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
cpi_live <- .gp_quote_row(
  venue = "polymarket", theme_id = "fed_rates", market_id = "CPI-1",
  title = "Will US CPI print above 3%?", yes_prob = 0.55,
  expires_at = "2099-06-01T00:00:00Z", raw_status = "open", breadth_tier = 1L
)
mixed <- rbind(live, dead_status, dead_time, cpi_live)
filtered <- gp_filter_live_quotes(mixed)
check("filter drops settled status", !any(filtered$market_id == "DEAD-S"))
check("filter drops past expires_at", !any(filtered$market_id == "DEAD-T"))
check("filter keeps live", any(filtered$market_id == "LIVE-1"))

reclass <- gp_reclassify_quotes(mixed, market_mode = "US")
check("reclassify drops expired", !any(reclass$market_id %in% c("DEAD-S", "DEAD-T")))
check(
  "reclassify promotes CPI title to inflation_cpi",
  {
    row <- reclass[reclass$market_id == "CPI-1", , drop = FALSE]
    nrow(row) == 1L && identical(as.character(row$theme_id[[1]]), "inflation_cpi")
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
check("module board buttons", grepl("board_economic", mod_src, fixed = TRUE) &&
        grepl("board_finance", mod_src, fixed = TRUE) &&
        grepl("board_company", mod_src, fixed = TRUE))
check("gp_fetch_snapshot accepts board", grepl("board = \"economic\"", mod_src, fixed = TRUE))
check(
  "GP CSS escapes percent for sprintf",
  grepl("width: 100%%;", mod_src, fixed = TRUE) &&
    grepl("flex-basis: 100%%;", mod_src, fixed = TRUE) &&
    grepl("height: 100%%;", mod_src, fixed = TRUE)
)

ui_mount_ok <- FALSE
ui_mount_err <- NULL
ui_html <- ""
tryCatch({
  ui_tag <- global_prediction_ui("global_prediction", locale = "en")
  ui_html <- as.character(ui_tag)
  ui_mount_ok <- isTRUE(nzchar(ui_html)) &&
    grepl("ynow-gp-dot-kalshi", ui_html, fixed = TRUE) &&
    grepl(cols$kalshi, ui_html, fixed = TRUE) &&
    grepl("ynow-gp-board-btn", ui_html, fixed = TRUE) &&
    grepl("width: 100%;", ui_html, fixed = TRUE)
}, error = function(e) {
  ui_mount_err <<- conditionMessage(e)
})
check(
  "global_prediction_ui mounts without sprintf format error",
  isTRUE(ui_mount_ok) && is.null(ui_mount_err)
)
if (!is.null(ui_mount_err)) {
  cat("  mount error: ", ui_mount_err, "\n", sep = "")
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All global prediction checks passed.\n")
