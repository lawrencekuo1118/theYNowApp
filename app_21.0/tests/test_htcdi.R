#!/usr/bin/env Rscript
# HTCDI engine + Macro placement (no network)
# Run: cd app_21.0 && Rscript tests/test_htcdi.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "htcdi_engine.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("htcdi_engine.R")) {
  normalizePath(".")
} else if (dir.exists("app_21.0") && file.exists("app_21.0/htcdi_engine.R")) {
  normalizePath("app_21.0")
} else stop("Cannot locate htcdi_engine.R")
setwd(root)

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("htcdi_config.R", local = TRUE, encoding = "UTF-8")
source("htcdi_engine.R", local = TRUE, encoding = "UTF-8")
source("htcdi_module.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}

cfg <- htcdi_load_config()
ids <- htcdi_issuer_ids(cfg)
check("universe 14", identical(length(ids), 14L))
check("OIL 93", identical(htcdi_find_issuer("OIL", cfg)$criticality_prior, 93))
check("OIL ticker USO", identical(htcdi_find_issuer("OIL", cfg)$tickers[[1]], "USO"))
check("ASML 95", identical(htcdi_find_issuer("ASML", cfg)$criticality_prior, 95))
check("meta destruction name", identical(cfg$meta$name, "Human Tech Civilization Destruction Index"))
check("oil litho channel", {
  ch <- cfg$contagion_channels
  any(vapply(ch, function(x) identical(x$id, "oil_litho"), logical(1)))
})
check("TSM 92", identical(htcdi_find_issuer("TSM", cfg)$criticality_prior, 92))
check("NVDA 65", identical(htcdi_find_issuer("NVDA", cfg)$criticality_prior, 65))
check("GOOG listed", "GOOG" %in% htcdi_find_issuer("GOOGL", cfg)$tickers)
check("factor w sum", abs(sum(cfg$criticality_factor_weights) - 1) < 1e-12)
yaml_path <- htcdi_config_yaml_path()
check("yaml exists", is.character(yaml_path) && file.exists(yaml_path))

# 1 AAPL no ADR
check("1 AAPL no ADR", is.null(htcdi_classify_alignment("USD", "USD", instrument_type = "ordinary",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = "USD")))
check("1 AAPL px", is.finite(htcdi_per_share(3.5e12, 1.5e10, "USD", "USD", usd_twd = NA_real_,
  instrument_type = "ordinary", statement_ccy = "USD")))

# 2 MSFT no FX when USD=USD
check("2 MSFT no FX", is.null(htcdi_classify_alignment("USD", "USD", instrument_type = "ordinary",
  usd_twd = NA_real_, statement_currency = "USD")))
check("2 MSFT px", is.finite(htcdi_per_share(1e12, 7e9, "USD", "USD", usd_twd = NA_real_,
  instrument_type = "ordinary", statement_ccy = "USD")))

# 3 TSM vs 2330 ADR+FX
check("3 TSM vs 2330 OK", is.null(htcdi_classify_alignment("TWD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = 32, adr_ratio = 5, shares = 5e9, statement_currency = "TWD")))
check("3 TSM px", is.finite(htcdi_per_share(250920e8, 25.932e9 / 5, "TWD", "USD", usd_twd = 32,
  instrument_type = "ADR", underlying_share_comparison_required = TRUE, statement_ccy = "TWD", adr_ratio = 5)))
check("3 TSM missing FX", identical(htcdi_classify_alignment("TWD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = NA_real_, statement_currency = "TWD"),
  "REQUIRED_FX_RATE_MISSING"))

# 4 TSM alone no ADR
check("4 TSM alone", is.null(htcdi_classify_alignment("USD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = "USD")))

# 5 GOOGL/GOOG
dedup <- htcdi_dedupe_tickers(c("GOOGL", "GOOG", "AAPL"), cfg)
check("5 one Alphabet", identical(sum(dedup$issuers == "GOOGL"), 1L))
check("5 keeps AAPL", "AAPL" %in% dedup$issuers)
check("5 dup code", identical(dedup$code, "DUPLICATE_ECONOMIC_ISSUER"))
base <- htcdi_score(cfg = cfg)
check("5 one row", identical(sum(vapply(base$issuers, function(r) identical(r$economic_issuer, "alphabet"), logical(1))), 1L))

# 6 missing optional per-share does not block return/vol
closes <- 100 + seq_len(80) / 10
dates <- seq.Date(as.Date("2024-01-02"), by = "day", length.out = 80)
sig <- htcdi_market_signals(data.frame(Date = dates, Close = closes, Volume = rep(1e6, 80)))
check("6 ret/vol", is.finite(sig$ret_1d) && is.finite(sig$vol_20d))
sc6 <- htcdi_score(list(AAPL = list(closes = closes, dates = dates, volume = rep(1e6, 80),
  alignment_code = "REQUIRED_PER_SHARE_VALUE_NON_FINITE", financial_resilience = 70, operational_continuity = 70)), cfg)
check("6 composite", is.finite(sc6$composite))
check("6 AAPL market", is.finite(sc6$issuers$AAPL$ret_1m) || is.finite(sc6$issuers$AAPL$vol_20d))

# 7 non-finite required blocks only that calc
sig7 <- htcdi_market_signals(data.frame(Date = seq.Date(as.Date("2024-06-01"), by = "day", length.out = 8), Close = 100 + 1:8))
check("7 beta blocked", sig7$failures$beta %in% c("BENCHMARK_DATA_MISSING", "INSUFFICIENT_ROLLING_WINDOW"))
check("7 ret ok", is.finite(sig7$ret_1d))
toast7 <- htcdi_failure_toast("INSUFFICIENT_ROLLING_WINDOW", "beta", c("returns", "vol"), "en")
check("7 toast blocked", grepl("rolling beta", toast7, ignore.case = TRUE))
check("7 toast remain", grepl("return", toast7, ignore.case = TRUE))

# 8 missing FX when currencies match
check("8 same ccy", is.null(htcdi_classify_alignment("USD", "USD", usd_twd = NA_real_, statement_currency = "USD")))

# 9 missing statement currency only when alignment required
check("9 ordinary N/A", is.null(htcdi_classify_alignment(NA, "USD", instrument_type = "ordinary",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = NA)))
check("9 ADR missing st", identical(htcdi_classify_alignment(NA, "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = 32, statement_currency = NA),
  "STATEMENT_CURRENCY_UNAVAILABLE"))

# 10 one weak name != stack contracting path
inp10 <- lapply(stats::setNames(nm = ids), function(id) list(
  rev_yoy = 0.08, gm_delta = 0.01, capex_vs_own = 0.01, data_confidence = 90,
  excess_1y = 0.05, ret_1y = 0.10, beta_60d = 1.05, price_vs_hist = 0.20, rev_yoy_vs_own = 0.02))
inp10$ASML$rev_yoy <- -0.25; inp10$ASML$excess_1y <- -0.20; inp10$ASML$price_vs_hist <- -0.15
sc10 <- htcdi_score(inp10, cfg)
check("10 not Contracting", !identical(sc10$alert, "Contracting"))
check("10 no channel", identical(length(sc10$contagion$channels), 0L))

# 11 linked stages cooling together
inp11 <- inp10
for (id in c("ASML", "TSM")) {
  inp11[[id]]$rev_yoy <- -0.28; inp11[[id]]$gm_delta <- -0.08; inp11[[id]]$capex_vs_own <- -0.04
  inp11[[id]]$excess_1y <- -0.35; inp11[[id]]$ret_1y <- -0.25; inp11[[id]]$beta_60d <- 0.6
  inp11[[id]]$price_vs_hist <- -0.40; inp11[[id]]$rev_yoy_vs_own <- -0.15
}
sc11 <- htcdi_score(inp11, cfg)
check("11 cooling path", length(sc11$contagion$channels) >= 1L)
check("11 litho_foundry", "litho_foundry" %in% sc11$contagion$channels)
check("11 penalty", is.finite(sc11$contagion$penalty) && sc11$contagion$penalty > 0)

# 12/13 price path vs benchmark lifts Market and can lift composite
n <- 80L
dates <- seq.Date(as.Date("2024-01-02"), by = "day", length.out = n)
px_down <- 120 - seq_len(n) * 0.6
px_flat <- rep(100, n)
px_up <- 80 + seq_len(n) * 0.8
bench <- 100 + seq_len(n) * 0.05
.mk_px_bench <- function(px) lapply(stats::setNames(nm = ids), function(id) list(
  closes = px, dates = dates, volume = rep(1e6, n),
  bench_df = data.frame(Date = dates, Close = bench)))
sc12 <- htcdi_score(.mk_px_bench(px_down), cfg)
sc12b <- htcdi_score(.mk_px_bench(px_flat), cfg)
sc13 <- htcdi_score(.mk_px_bench(px_up), cfg)
check("12 market lower when down", is.finite(sc12$indices$market_observation) &&
  is.finite(sc12b$indices$market_observation) &&
  sc12$indices$market_observation < sc12b$indices$market_observation)
check("13 market higher when up", is.finite(sc13$indices$market_observation) &&
  sc13$indices$market_observation > sc12b$indices$market_observation)
check("13 composite can rise with outperformance", is.finite(sc13$composite) && is.finite(sc12b$composite) &&
  sc13$composite > sc12b$composite)

# 14 caps
raw <- htcdi_raw_weights(cfg); wt <- htcdi_constrain_weights(raw, cfg)
check("14 sum 1", abs(sum(wt$constrained) - 1) < 1e-8)
check("14 issuer cap", {
  ok <- TRUE
  for (id in names(wt$constrained)) {
    cap <- if (id %in% c("ASML", "TSM", "OIL")) 0.15 else 0.12
    if (wt$constrained[[id]] > cap + 1e-8) ok <- FALSE
  }
  ok
})
check("14 layer cap", {
  ok <- TRUE
  for (ly in unique(unname(wt$layer))) {
    mem <- names(wt$constrained)[wt$layer == ly]
    if (sum(wt$constrained[mem]) > 0.25 + 1e-8) ok <- FALSE
  }
  ok
})
raw2 <- raw; raw2[] <- 0.01; raw2[["NVDA"]] <- 0.40; raw2 <- raw2 / sum(raw2)
wt2 <- htcdi_constrain_weights(raw2, cfg)
check("14 NVDA raw high", raw2[["NVDA"]] > 0.12)
check("14 NVDA capped", wt2$constrained[["NVDA"]] <= 0.12 + 1e-8)

# 15 reproducible
sc15a <- htcdi_score(.mk_px_bench(px_down), cfg); sc15b <- htcdi_score(.mk_px_bench(px_down), cfg)
check("15 composite", identical(sc15a$composite, sc15b$composite))
check("15 indices", identical(sc15a$indices, sc15b$indices))
check("15 formula", grepl("0.30", sc15a$formula, fixed = TRUE) && grepl("Stmt", sc15a$formula, fixed = TRUE))
check("15 role", identical(sc15a$role, "civilization_destruction_reading"))

check("stmt grows with revenue", {
  hi <- inp10; lo <- inp10
  for (id in ids) { hi[[id]]$rev_yoy <- 0.20; lo[[id]]$rev_yoy <- -0.10 }
  shi <- htcdi_score(hi, cfg); slo <- htcdi_score(lo, cfg)
  is.finite(shi$indices$statement_development) && is.finite(slo$indices$statement_development) &&
    shi$indices$statement_development > slo$indices$statement_development
})
check("beta lifts influence", {
  hi <- inp10; lo <- inp10
  for (id in ids) { hi[[id]]$beta_60d <- 1.6; lo[[id]]$beta_60d <- 0.7 }
  shi <- htcdi_score(hi, cfg); slo <- htcdi_score(lo, cfg)
  is.finite(shi$indices$influence_vs_market) && is.finite(slo$indices$influence_vs_market) &&
    shi$indices$influence_vs_market > slo$indices$influence_vs_market
})

.mk_px <- function(px) lapply(stats::setNames(nm = ids), function(id) list(
  closes = px, dates = dates, volume = rep(1e6, n)))
sc_po_up <- htcdi_score(.mk_px(px_up), cfg)
sc_po_flat <- htcdi_score(.mk_px(rep(100, n)), cfg)
check("price-only statement not from price", {
  hu <- sc_po_up$indices$systems_health; hf <- sc_po_flat$indices$systems_health
  (!is.finite(hu) && !is.finite(hf)) ||
    (is.finite(hu) && is.finite(hf) && abs(hu - hf) < 1e-8)
})
check("price-only M moves", is.finite(sc_po_up$indices$market_observation) &&
  is.finite(sc_po_flat$indices$market_observation) &&
  sc_po_up$indices$market_observation > sc_po_flat$indices$market_observation)
check("price-only composite can rise", is.finite(sc_po_up$composite) && is.finite(sc_po_flat$composite) &&
  sc_po_up$composite > sc_po_flat$composite)

sc0 <- htcdi_score(NULL, cfg)
check("missing history no composite", !is.finite(sc0$composite))
check("missing history not placeholder", !isTRUE(sc0$composite %in% c(0, 50, 100, 70, 72)))
check("missing history unavailable", identical(sc0$alert, "Unavailable"))
check("missing history H NA", !is.finite(sc0$indices$systems_health))
check("missing history S NA", !is.finite(sc0$indices$systemic_stress))
check("missing history M NA", !is.finite(sc0$indices$market_observation))
check("missing history code", identical(sc0$failures$composite, "SOURCE_HISTORY_UNAVAILABLE"))
check("prior not used as H", isTRUE(sc0$issuers$ASML$criticality_prior == 95) &&
  !is.finite(sc0$issuer_health[["ASML"]]))

c_fill <- htcdi_composite(70, 40, 50, 50, cfg)
c_omit <- htcdi_composite(70, 40, 50, NA, cfg)
check("renorm omits Traj", is.finite(c_omit) &&
  abs(as.numeric(c_omit)[1] - (0.30 * 70 + 0.25 * 40 + 0.25 * 50) / 0.80) < 1e-8)
check("renorm != fill", abs(as.numeric(c_omit)[1] - as.numeric(c_fill)[1]) > 1e-6)
check("renorm dropped Traj", isTRUE("trajectory_vs_history" %in% attr(c_omit, "dropped")))

inp_one <- inp11
inp_one$TSM$rev_yoy <- 0.10; inp_one$TSM$excess_1y <- 0.08; inp_one$TSM$price_vs_hist <- 0.12
inp_one$TSM$gm_delta <- 0.01; inp_one$TSM$beta_60d <- 1.1; inp_one$TSM$rev_yoy_vs_own <- 0.02
sc_short <- htcdi_score(inp_one, cfg)
check("one cooler no litho path", !isTRUE("litho_foundry" %in% sc_short$contagion$channels))

live0 <- htcdi_live_inputs_from_prices(NULL, NULL, cfg)
check("live no dc default", is.null(live0$AAPL$data_confidence) || !is.finite(live0$AAPL$data_confidence))
toast_h <- htcdi_failure_toast("SOURCE_HISTORY_UNAVAILABLE", "composite", c("fragility"), "en")
check("history toast specific", grepl("default|withheld|unavailable", toast_h, ignore.case = TRUE) &&
  !grepl("Something went wrong", toast_h, ignore.case = TRUE))

macro_txt <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
mod_txt <- paste(readLines("htcdi_module.R", warn = FALSE), collapse = "\n")
check("no lazy default score", !grepl("want_px", macro_txt, fixed = TRUE))
check("always fetch history", grepl("Always attempt live Yahoo/history", macro_txt, fixed = TRUE))
check("live fetches statements", grepl("cached_scrape_financials", macro_txt, fixed = TRUE))
check("live 5y window", grepl("fetch_price_history_df(tk, \"5y\")", macro_txt, fixed = TRUE))
check("UI flow helper", grepl("ynow-htcdi-flow", mod_txt, fixed = TRUE) && grepl("htcdi_score_span", mod_txt, fixed = TRUE))
check("UI Rf not flow", grepl("ynow-macro-rf__value", macro_txt, fixed = TRUE) &&
  !grepl("ynow-macro-rf__value ynow-htcdi-flow", macro_txt, fixed = TRUE))
check("CSS logo flow stops", grepl("ynow-logo-flow", ui_txt, fixed = TRUE) &&
  grepl("#0C5484", ui_txt, fixed = TRUE) && grepl("#249C60", ui_txt, fixed = TRUE) &&
  grepl("#1AA8B8", ui_txt, fixed = TRUE) && grepl("ynow-htcdi-flow", ui_txt, fixed = TRUE))
kf <- gregexpr("@keyframes ynow-logo-flow", ui_txt, fixed = TRUE)[[1]]
check("single logo-flow keyframes", length(kf) == 1L && kf[[1]] > 0)
check("Data-limited reuses flow class", grepl(".ynow-fund-profile-badge .ynow-htcdi-flow", ui_txt, fixed = TRUE))

if (!exists("tags", inherits = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
  tags <- htmltools::tags
}
box_ok <- tryCatch(htcdi_kpi_box(sc13, lite = TRUE, locale = "en"), error = function(e) NULL)
if (!is.null(box_ok)) {
  html_ok <- paste(as.character(box_ok), collapse = " ")
  check("KPI numeral flow class", grepl("ynow-htcdi-flow", html_ok, fixed = TRUE))
  check("KPI numeral not unavailable", !grepl("ynow-htcdi-unavailable", html_ok, fixed = TRUE))
}
box_na <- tryCatch(htcdi_kpi_box(sc0, lite = TRUE, locale = "en"), error = function(e) NULL)
if (!is.null(box_na)) {
  html_na <- paste(as.character(box_na), collapse = " ")
  check("KPI unavailable class", grepl("ynow-htcdi-unavailable", html_na, fixed = TRUE))
  check("KPI unavailable no flow", !grepl("ynow-htcdi-flow", html_na, fixed = TRUE))
}
four <- tryCatch(.htcdi_four_boxes(sc13, "en"), error = function(e) NULL)
if (!is.null(four)) {
  html_four <- paste(as.character(four), collapse = " ")
  check("four-index flow class", grepl("ynow-htcdi-flow", html_four, fixed = TRUE))
}
check("UI beside Rf", grepl("ynow-macro-kpi--htcdi", macro_txt, fixed = TRUE) && grepl("ynow-macro-kpi--rf", macro_txt, fixed = TRUE))
check("UI same row", grepl("htcdi_col", macro_txt, fixed = TRUE) && grepl("rf_col", macro_txt, fixed = TRUE) &&
  grepl("ynow_col", macro_txt, fixed = TRUE))
check("UI 2:1:1", grepl("rf_col, ynow_col, htcdi_col", macro_txt, fixed = TRUE) &&
  grepl("width = 6, class = \"col-xs-12 col-sm-6 col-md-6\"", macro_txt, fixed = TRUE) &&
  grepl("width = 3, class = \"col-xs-12 col-sm-3 col-md-3\"", macro_txt, fixed = TRUE))
check("UI TW+US", grepl("rf_col, ynow_col, htcdi_col", macro_txt, fixed = TRUE))
check("UI expand below", {
  pos_row <- regexpr("rf_signal_row", macro_txt, fixed = TRUE)[1]
  pos_exp <- regexpr("ynow_macro_htcdi_expand", macro_txt, fixed = TRUE)[1]
  is.finite(pos_row) && is.finite(pos_exp) && pos_exp > pos_row
})
check("UI Full-only expand", grepl("ynow-macro-htcdi-expand ynow-full-only", macro_txt, fixed = TRUE))
check("UI Lite no click", grepl("if (!isTRUE(lite))", mod_txt, fixed = TRUE))
check("UI Lite CSS", grepl("body.ynow-lite #ynow_macro_htcdi_expand", ui_txt, fixed = TRUE))
check("UI four indices", grepl("htcdi-four", mod_txt, fixed = TRUE))
check("four-index uses flow span", grepl("htcdi_score_span(it$val)", mod_txt, fixed = TRUE))
check("layer table uses flow span", grepl("htcdi_score_span(stmt_v)", mod_txt, fixed = TRUE) &&
  grepl("htcdi_score_span(mkt_v)", mod_txt, fixed = TRUE) &&
  grepl("htcdi_score_span(inf_v)", mod_txt, fixed = TRUE) &&
  grepl("htcdi_score_span(traj_v)", mod_txt, fixed = TRUE))
mod_lines <- strsplit(mod_txt, "\n", fixed = TRUE)[[1]]
ly_i <- grep("^\\.htcdi_layer_table <- function", mod_lines)
nw_i <- grep("^\\.htcdi_network_ui <- function", mod_lines)
layer_src <- if (length(ly_i) && length(nw_i) && nw_i[[1]] > ly_i[[1]]) {
  paste(mod_lines[ly_i[[1]]:(nw_i[[1]] - 1L)], collapse = "\n")
} else ""
check("layer table no substitutes header", !grepl("htcdi_col_substitutes", layer_src, fixed = TRUE))
check("layer table no replacement header", !grepl("htcdi_col_replacement", layer_src, fixed = TRUE))
check("layer table no rebuild-year mean", !grepl("replacement_time_years", layer_src, fixed = TRUE))
check("issuer pair not single constituent table", grepl(".htcdi_in_composite_block", mod_txt, fixed = TRUE) &&
  !grepl(".htcdi_constituent_table", mod_txt, fixed = TRUE) &&
  !grepl(".htcdi_issuer_pair", mod_txt, fixed = TRUE) &&
  !grepl(".htcdi_out_composite_table", mod_txt, fixed = TRUE))
check("in-composite table has live inputs", grepl("htcdi_col_rev_yoy", mod_txt, fixed = TRUE) &&
  grepl("htcdi_col_gm_delta", mod_txt, fixed = TRUE) && grepl("htcdi_col_capex_own", mod_txt, fixed = TRUE) &&
  grepl("htcdi_col_price_hist", mod_txt, fixed = TRUE))
check("stage/composite term bridge", grepl(".htcdi_term_bridge", mod_txt, fixed = TRUE) &&
  grepl("ynow-htcdi-term-groups", mod_txt, fixed = TRUE) &&
  grepl("htcdi_term_map_stmt", mod_txt, fixed = TRUE))
check("stage table shows Stmt Mkt Inf Traj", grepl('.htcdi_term_th("stmt"', layer_src, fixed = TRUE) &&
  grepl('.htcdi_term_th("mkt"', layer_src, fixed = TRUE) &&
  grepl('.htcdi_term_th("inf"', layer_src, fixed = TRUE) &&
  grepl('.htcdi_term_th("traj"', layer_src, fixed = TRUE))
check("stage table carries issuer role", grepl("htcdi_col_function", layer_src, fixed = TRUE) &&
  grepl("htcdi_col_issuer", layer_src, fixed = TRUE))
check("issuer tables drop cap theater", !grepl("htcdi_col_criticality", mod_txt, fixed = TRUE) &&
  !grepl("htcdi_col_weight_raw", mod_txt, fixed = TRUE) &&
  !grepl("htcdi_col_substitutes", mod_txt, fixed = TRUE) &&
  !grepl("htcdi_col_confidence", mod_txt, fixed = TRUE))
check("formula banner like WACC", grepl("ynow-htcdi-formula-banner", mod_txt, fixed = TRUE) &&
  grepl("htcdi_formula_eq", mod_txt, fixed = TRUE))
check("Rf not full-only", !grepl("ynow-macro-kpi--rf[^\\n]*ynow-full-only", macro_txt))

for (k in c("htcdi_title", "htcdi_title_full", "htcdi_disclosure", "htcdi_index_health",
            "htcdi_index_stress", "htcdi_index_fragility", "htcdi_index_market",
            "notif_htcdi_fx_missing", "htcdi_unavailable", "htcdi_dropped",
            "htcdi_dropped_none", "htcdi_alert_unavailable", "notif_htcdi_history_missing")) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("en full title destruction", {
  identical(ui_str("htcdi_title_full", "en"), "Human Tech Civilization Destruction Index (HTCDI)") &&
    identical(ui_str("htcdi_title", "en"), "HTCDI")
})
check("zh full title destruction", {
  identical(ui_str("htcdi_title_full", "zh-TW"), "人類科技文明毀滅指數（HTCDI）") &&
    identical(ui_str("htcdi_title", "zh-TW"), "人類科技文明毀滅指數")
})
check("config display name", {
  identical(HTCDI_DEFAULT_CONFIG$meta$id, "HTCDI") &&
    identical(HTCDI_DEFAULT_CONFIG$meta$name, "Human Tech Civilization Destruction Index")
})
check("destruction framing retained", {
  grepl("Destruction", ui_str("htcdi_title_full", "en"), fixed = TRUE) &&
    grepl("人類科技文明毀滅指數", ui_str("htcdi_title_full", "zh-TW"), fixed = TRUE) &&
    grepl("Destruction", HTCDI_DEFAULT_CONFIG$meta$name, fixed = TRUE)
})
check("oil layer labels", {
  identical(ui_str("htcdi_ly_energy_oil", "en"), "Oil / energy") &&
    identical(ui_str("htcdi_ly_energy_oil", "zh-TW"), "石油／能源") &&
    identical(ui_str("htcdi_fn_energy_oil", "zh-TW"), "石油指數（USO）") &&
    identical(.htcdi_named("fn", "energy_oil", "zh-TW"), "石油指數（USO）")
})
check("en name", identical(ui_str("htcdi_index_health", "en"), "Statement Development"))
check("zh name EN", identical(ui_str("htcdi_index_health", "zh-TW"), "Statement Development"))
check("zh gloss", grepl("財報發展", ui_str("htcdi_index_health_gloss", "zh-TW"), fixed = TRUE))
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("htcdi_disclosure", "zh-TW")))
htcdi_copy_keys <- grep("^htcdi_", names(.UI_STRINGS$en), value = TRUE)
check("htcdi keys in zh-TW", all(htcdi_copy_keys %in% names(.UI_STRINGS$`zh-TW`)))
check("zh stage is 環節", identical(ui_str("htcdi_col_layer", "zh-TW"), "環節"))
check("zh knock-on path", identical(ui_str("htcdi_contagion_paths", "zh-TW"), "連鎖降溫路徑"))
check("en stage not layer", identical(ui_str("htcdi_col_layer", "en"), "Stage"))
check("en knock-on path", identical(ui_str("htcdi_contagion_paths", "en"), "Linked stages cooling together"))
check("en term bridge", grepl("Same color", ui_str("htcdi_term_bridge", "en"), fixed = TRUE))
check("zh term bridge", grepl("同色", ui_str("htcdi_term_bridge", "zh-TW"), fixed = TRUE))
check("chain cooling bilingual", identical(ui_str("htcdi_chain_cooling", "en"), "Cooling") &&
  identical(ui_str("htcdi_chain_cooling", "zh-TW"), "降溫中"))
check("network above layers in expand source", {
  exp_i <- grep("^htcdi_expand_ui <- function", mod_lines)
  if (!length(exp_i)) return(FALSE)
  exp_src <- paste(mod_lines[exp_i[[1]]:min(length(mod_lines), exp_i[[1]] + 40L)], collapse = "\n")
  pos_net <- regexpr(".htcdi_network_ui", exp_src, fixed = TRUE)[1]
  pos_ly <- regexpr(".htcdi_layer_table", exp_src, fixed = TRUE)[1]
  pos_four <- regexpr(".htcdi_four_boxes", exp_src, fixed = TRUE)[1]
  is.finite(pos_net) && is.finite(pos_ly) && is.finite(pos_four) &&
    pos_four < pos_net && pos_net < pos_ly
})
check("term badges bilingual", {
  identical(ui_str("htcdi_term_stmt", "en"), "Stmt") &&
    identical(ui_str("htcdi_term_mkt", "zh-TW"), "Mkt") &&
    nzchar(ui_str("htcdi_term_map_inf", "en")) &&
    nzchar(ui_str("htcdi_in_composite_help", "zh-TW"))
})
check("zh expand avoids 傳染/濾鏡", {
  blob <- paste(vapply(
    c("htcdi_disclosure", "htcdi_index_stress_gloss", "htcdi_network_note",
      "htcdi_contagion_paths", "htcdi_contagion_none", "htcdi_layer_title",
      "htcdi_highest_risk_layer", "htcdi_network_title", "htcdi_method_limits"),
    function(k) ui_str(k, "zh-TW"), character(1)), collapse = " ")
  !grepl("傳染", blob, fixed = TRUE) && !grepl("濾鏡", blob, fixed = TRUE)
})
check("copy has no ticker examples", {
  keys <- c("htcdi_method_selection", "htcdi_method_weighting", "htcdi_method_fx_adr",
            "htcdi_network_note", "htcdi_disclosure", "notif_htcdi_dup_issuer",
            "htcdi_formula_parts", "htcdi_out_composite_note")
  blob <- paste(c(vapply(keys, function(k) ui_str(k, "en"), character(1)),
                  vapply(keys, function(k) ui_str(k, "zh-TW"), character(1))), collapse = " ")
  !grepl("GOOGL", blob, fixed = TRUE) && !grepl("GOOG", blob, fixed = TRUE) &&
    !grepl("ASML", blob, fixed = TRUE) && !grepl("2330", blob, fixed = TRUE) &&
    !grepl("TSM", blob, fixed = TRUE)
})
check("ly enterprise_dbs zh", identical(ui_str("htcdi_ly_enterprise_dbs", "zh-TW"), "企業資料庫"))
check("named helper zh", identical(.htcdi_named("ly", "enterprise_dbs", "zh-TW"), "企業資料庫"))
if (requireNamespace("htmltools", quietly = TRUE) && requireNamespace("shiny", quietly = TRUE)) {
  if (!exists("tags", inherits = TRUE)) tags <- htmltools::tags
  if (!exists("column", inherits = TRUE)) column <- shiny::column
  if (!exists("fluidRow", inherits = TRUE)) fluidRow <- shiny::fluidRow
  exp_zh <- tryCatch(htcdi_expand_ui(sc13, "zh-TW"), error = function(e) {
    cat("EXPAND ERR ", e$message, "\n", sep = ""); NULL
  })
  check("expand zh renders", !is.null(exp_zh))
  if (!is.null(exp_zh)) {
    html_zh <- paste(as.character(exp_zh), collapse = " ")
    check("expand zh uses 企業資料庫", grepl("企業資料庫", html_zh, fixed = TRUE))
    check("expand zh hides cooling when no path", !grepl("連鎖降溫", html_zh, fixed = TRUE) &&
      !grepl("相對歷史仍在降溫", html_zh, fixed = TRUE) &&
      !grepl("Issuers still cooling", html_zh, fixed = TRUE))
    exp_path <- tryCatch(htcdi_expand_ui(sc11, "zh-TW"), error = function(e) NULL)
    if (!is.null(exp_path)) {
      html_path <- paste(as.character(exp_path), collapse = " ")
      check("expand zh shows path when lit", grepl("連鎖降溫路徑", html_path, fixed = TRUE) &&
        grepl("連鎖降溫", html_path, fixed = TRUE) &&
        grepl("ynow-htcdi-chain-grid", html_path, fixed = TRUE) &&
        grepl("ynow-htcdi-chain-card", html_path, fixed = TRUE))
      check("expand zh path omits issuer list", !grepl("相對歷史仍在降溫", html_path, fixed = TRUE))
      pos_net <- regexpr("ynow_macro_htcdi_network_title", html_path, fixed = TRUE)[1]
      pos_ly <- regexpr("ynow_macro_htcdi_layer_title", html_path, fixed = TRUE)[1]
      pos_four <- regexpr("ynow-htcdi-four", html_path, fixed = TRUE)[1]
      check("cooling above Function stages", is.finite(pos_net) && is.finite(pos_ly) &&
        is.finite(pos_four) && pos_four < pos_net && pos_net < pos_ly)
    } else check("expand zh shows path when lit", FALSE)
    check("expand zh uses 環節", grepl("環節", html_zh, fixed = TRUE))
    check("expand zh no raw enterprise_dbs cell", !grepl(">enterprise_dbs<", html_zh, fixed = TRUE))
    check("expand zh formula banner", grepl("HTCDI = 0.30", html_zh, fixed = TRUE))
    check("expand zh in-composite title", grepl("進入複合分數", html_zh, fixed = TRUE))
    check("expand zh no out-composite title", !grepl("不進入複合分數", html_zh, fixed = TRUE))
    check("expand zh role in stages", grepl("在鏈上的角色", html_zh, fixed = TRUE))
    check("expand zh no Criticality header", !grepl(">Criticality<", html_zh, fixed = TRUE))
    check("expand zh no 可替代對象 header", !grepl(">可替代對象<", html_zh, fixed = TRUE))
    check("expand zh no 未受限權重", !grepl("未受限權重", html_zh, fixed = TRUE))
  }
  layer_en <- tryCatch(.htcdi_layer_table(sc13, "en"), error = function(e) NULL)
  layer_zh <- tryCatch(.htcdi_layer_table(sc13, "zh-TW"), error = function(e) NULL)
  in_en <- tryCatch(.htcdi_in_composite_table(sc13, "en"), error = function(e) NULL)
  banner_en <- tryCatch(.htcdi_formula_banner(sc13, "en"), error = function(e) NULL)
  check("issuer label drops dup ticker", identical(.htcdi_issuer_label(sc13$issuers$ASML), "ASML"))
  check("issuer label keeps extra class", identical(.htcdi_issuer_label(sc13$issuers$GOOGL), "GOOGL/GOOG"))
  if (!is.null(layer_en)) {
    html_ly <- paste(as.character(layer_en), collapse = " ")
    check("layer HTML no substitutes col", !grepl("What can replace it", html_ly, fixed = TRUE))
    check("layer HTML no rebuild col", !grepl("Years to rebuild", html_ly, fixed = TRUE))
    check("layer HTML has issuer", grepl("Issuer", html_ly, fixed = TRUE))
    check("layer HTML has role", grepl("Role in the chain", html_ly, fixed = TRUE))
    check("layer HTML term badges", grepl("data-htcdi-term=\"stmt\"", html_ly, fixed = TRUE) &&
      grepl("data-htcdi-term=\"mkt\"", html_ly, fixed = TRUE) &&
      grepl("data-htcdi-term=\"inf\"", html_ly, fixed = TRUE) &&
      grepl("data-htcdi-term=\"traj\"", html_ly, fixed = TRUE))
    check("layer HTML no spaced dup ticker", !grepl("ASML ASML", html_ly, fixed = TRUE) &&
      !grepl("GOOGL GOOGL", html_ly, fixed = TRUE))
  }
  if (!is.null(layer_zh)) {
    html_ly_zh <- paste(as.character(layer_zh), collapse = " ")
    check("layer zh HTML no 可替代對象", !grepl("可替代對象", html_ly_zh, fixed = TRUE))
    check("layer zh HTML no 重建年數", !grepl("重建年數", html_ly_zh, fixed = TRUE))
    check("layer zh HTML has 發行人", grepl("發行人", html_ly_zh, fixed = TRUE))
    check("layer zh HTML has 在鏈上的角色", grepl("在鏈上的角色", html_ly_zh, fixed = TRUE))
  }
  if (!is.null(in_en)) {
    html_in <- paste(as.character(in_en), collapse = " ")
    check("in-composite has Rev YoY", grepl("Rev YoY", html_in, fixed = TRUE))
    check("in-composite has ΔGM", grepl("ΔGM", html_in, fixed = TRUE))
    check("in-composite has CapEx vs own", grepl("CapEx vs own", html_in, fixed = TRUE))
    check("in-composite grouped term headers", grepl("ynow-htcdi-term-groups", html_in, fixed = TRUE) &&
      grepl("colspan=\"3\"", html_in, fixed = TRUE) &&
      grepl("data-htcdi-term=\"stmt\"", html_in, fixed = TRUE) &&
      grepl("data-htcdi-term=\"mkt\"", html_in, fixed = TRUE))
    check("in-composite no substitutes", !grepl("What can replace it", html_in, fixed = TRUE))
    check("in-composite no Uncapped", !grepl("Uncapped weight", html_in, fixed = TRUE))
    check("in-composite no Criticality", !grepl("Criticality", html_in, fixed = TRUE))
    check("in-composite no spaced dup ticker", !grepl("ASML ASML", html_in, fixed = TRUE) &&
      !grepl("GOOGL GOOGL", html_in, fixed = TRUE))
  }
  block_en <- tryCatch(.htcdi_in_composite_block(sc13, "en"), error = function(e) NULL)
  if (!is.null(block_en)) {
    html_blk <- paste(as.character(block_en), collapse = " ")
    check("composite block has term bridge", grepl("ynow-htcdi-term-bridge", html_blk, fixed = TRUE) &&
      grepl("Same color", html_blk, fixed = TRUE))
  } else {
    check("composite block has term bridge", FALSE)
  }
  if (!is.null(banner_en)) {
    html_bn <- paste(as.character(banner_en), collapse = " ")
    check("banner equation", grepl("HTCDI = 0.30·Stmt + 0.25·Mkt + 0.25·Inf + 0.20·Traj", html_bn, fixed = TRUE))
    check("banner WACC-like class", grepl("ynow-htcdi-formula-banner__eq", html_bn, fixed = TRUE))
  }
}

for (fn in c("htcdi_config.R", "htcdi_engine.R", "htcdi_module.R", "macro_market_module.R", "global.R", "ui_locale.R")) {
  parsed <- tryCatch({ parse(fn, keep.source = FALSE); TRUE }, error = function(e) { cat("PARSE ", fn, ": ", e$message, "\n"); FALSE })
  check(paste("parse", fn), isTRUE(parsed))
}

if (fail > 0L) { cat("\n", fail, " failure(s)\n", sep = ""); quit(status = 1) }
cat("\nAll HTCDI checks passed.\n")
