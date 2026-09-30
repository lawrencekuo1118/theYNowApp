#!/usr/bin/env Rscript
# HCCSI engine + Macro placement (no network)
# Run: cd app_19.0 && Rscript tests/test_hccsi.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "hccsi_engine.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("hccsi_engine.R")) {
  normalizePath(".")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/hccsi_engine.R")) {
  normalizePath("app_19.0")
} else stop("Cannot locate hccsi_engine.R")
setwd(root)

source("setup.R", local = TRUE, encoding = "UTF-8")
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("hccsi_config.R", local = TRUE, encoding = "UTF-8")
source("hccsi_engine.R", local = TRUE, encoding = "UTF-8")
source("hccsi_module.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}

cfg <- hccsi_load_config()
ids <- hccsi_issuer_ids(cfg)
check("universe 13", identical(length(ids), 13L))
check("ASML 95", identical(hccsi_find_issuer("ASML", cfg)$criticality_prior, 95))
check("TSM 92", identical(hccsi_find_issuer("TSM", cfg)$criticality_prior, 92))
check("NVDA 65", identical(hccsi_find_issuer("NVDA", cfg)$criticality_prior, 65))
check("GOOG listed", "GOOG" %in% hccsi_find_issuer("GOOGL", cfg)$tickers)
check("factor w sum", abs(sum(cfg$criticality_factor_weights) - 1) < 1e-12)
yaml_path <- hccsi_config_yaml_path()
check("yaml exists", is.character(yaml_path) && file.exists(yaml_path))

# 1 AAPL no ADR
check("1 AAPL no ADR", is.null(hccsi_classify_alignment("USD", "USD", instrument_type = "ordinary",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = "USD")))
check("1 AAPL px", is.finite(hccsi_per_share(3.5e12, 1.5e10, "USD", "USD", usd_twd = NA_real_,
  instrument_type = "ordinary", statement_ccy = "USD")))

# 2 MSFT no FX when USD=USD
check("2 MSFT no FX", is.null(hccsi_classify_alignment("USD", "USD", instrument_type = "ordinary",
  usd_twd = NA_real_, statement_currency = "USD")))
check("2 MSFT px", is.finite(hccsi_per_share(1e12, 7e9, "USD", "USD", usd_twd = NA_real_,
  instrument_type = "ordinary", statement_ccy = "USD")))

# 3 TSM vs 2330 ADR+FX
check("3 TSM vs 2330 OK", is.null(hccsi_classify_alignment("TWD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = 32, adr_ratio = 5, shares = 5e9, statement_currency = "TWD")))
check("3 TSM px", is.finite(hccsi_per_share(250920e8, 25.932e9 / 5, "TWD", "USD", usd_twd = 32,
  instrument_type = "ADR", underlying_share_comparison_required = TRUE, statement_ccy = "TWD", adr_ratio = 5)))
check("3 TSM missing FX", identical(hccsi_classify_alignment("TWD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = NA_real_, statement_currency = "TWD"),
  "REQUIRED_FX_RATE_MISSING"))

# 4 TSM alone no ADR
check("4 TSM alone", is.null(hccsi_classify_alignment("USD", "USD", instrument_type = "ADR",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = "USD")))

# 5 GOOGL/GOOG
dedup <- hccsi_dedupe_tickers(c("GOOGL", "GOOG", "AAPL"), cfg)
check("5 one Alphabet", identical(sum(dedup$issuers == "GOOGL"), 1L))
check("5 keeps AAPL", "AAPL" %in% dedup$issuers)
check("5 dup code", identical(dedup$code, "DUPLICATE_ECONOMIC_ISSUER"))
base <- hccsi_score(cfg = cfg)
check("5 one row", identical(sum(vapply(base$issuers, function(r) identical(r$economic_issuer, "alphabet"), logical(1))), 1L))

# 6 missing optional per-share does not block return/vol
closes <- 100 + seq_len(80) / 10
dates <- seq.Date(as.Date("2024-01-02"), by = "day", length.out = 80)
sig <- hccsi_market_signals(data.frame(Date = dates, Close = closes, Volume = rep(1e6, 80)))
check("6 ret/vol", is.finite(sig$ret_1d) && is.finite(sig$vol_20d))
sc6 <- hccsi_score(list(AAPL = list(closes = closes, dates = dates, volume = rep(1e6, 80),
  alignment_code = "REQUIRED_PER_SHARE_VALUE_NON_FINITE", financial_resilience = 70, operational_continuity = 70)), cfg)
check("6 composite", is.finite(sc6$composite))
check("6 AAPL market", is.finite(sc6$issuers$AAPL$ret_1m) || is.finite(sc6$issuers$AAPL$vol_20d))

# 7 non-finite required blocks only that calc
sig7 <- hccsi_market_signals(data.frame(Date = seq.Date(as.Date("2024-06-01"), by = "day", length.out = 8), Close = 100 + 1:8))
check("7 beta blocked", sig7$failures$beta %in% c("BENCHMARK_DATA_MISSING", "INSUFFICIENT_ROLLING_WINDOW"))
check("7 ret ok", is.finite(sig7$ret_1d))
toast7 <- hccsi_failure_toast("INSUFFICIENT_ROLLING_WINDOW", "beta", c("returns", "vol"), "en")
check("7 toast blocked", grepl("rolling beta", toast7, ignore.case = TRUE))
check("7 toast remain", grepl("return", toast7, ignore.case = TRUE))

# 8 missing FX when currencies match
check("8 same ccy", is.null(hccsi_classify_alignment("USD", "USD", usd_twd = NA_real_, statement_currency = "USD")))

# 9 missing statement currency only when alignment required
check("9 ordinary N/A", is.null(hccsi_classify_alignment(NA, "USD", instrument_type = "ordinary",
  underlying_share_comparison_required = FALSE, usd_twd = NA_real_, statement_currency = NA)))
check("9 ADR missing st", identical(hccsi_classify_alignment(NA, "USD", instrument_type = "ADR",
  underlying_share_comparison_required = TRUE, usd_twd = 32, statement_currency = NA),
  "STATEMENT_CURRENCY_UNAVAILABLE"))

# 10 one weak name != stack contracting path
inp10 <- lapply(stats::setNames(nm = ids), function(id) list(
  rev_yoy = 0.08, gm_delta = 0.01, capex_vs_own = 0.01, data_confidence = 90,
  excess_1y = 0.05, ret_1y = 0.10, beta_60d = 1.05, price_vs_hist = 0.20, rev_yoy_vs_own = 0.02))
inp10$ASML$rev_yoy <- -0.25; inp10$ASML$excess_1y <- -0.20; inp10$ASML$price_vs_hist <- -0.15
sc10 <- hccsi_score(inp10, cfg)
check("10 not Contracting", !identical(sc10$alert, "Contracting"))
check("10 no channel", identical(length(sc10$contagion$channels), 0L))

# 11 linked stages cooling together
inp11 <- inp10
for (id in c("ASML", "TSM")) {
  inp11[[id]]$rev_yoy <- -0.28; inp11[[id]]$gm_delta <- -0.08; inp11[[id]]$capex_vs_own <- -0.04
  inp11[[id]]$excess_1y <- -0.35; inp11[[id]]$ret_1y <- -0.25; inp11[[id]]$beta_60d <- 0.6
  inp11[[id]]$price_vs_hist <- -0.40; inp11[[id]]$rev_yoy_vs_own <- -0.15
}
sc11 <- hccsi_score(inp11, cfg)
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
sc12 <- hccsi_score(.mk_px_bench(px_down), cfg)
sc12b <- hccsi_score(.mk_px_bench(px_flat), cfg)
sc13 <- hccsi_score(.mk_px_bench(px_up), cfg)
check("12 market lower when down", is.finite(sc12$indices$market_observation) &&
  is.finite(sc12b$indices$market_observation) &&
  sc12$indices$market_observation < sc12b$indices$market_observation)
check("13 market higher when up", is.finite(sc13$indices$market_observation) &&
  sc13$indices$market_observation > sc12b$indices$market_observation)
check("13 composite can rise with outperformance", is.finite(sc13$composite) && is.finite(sc12b$composite) &&
  sc13$composite > sc12b$composite)

# 14 caps
raw <- hccsi_raw_weights(cfg); wt <- hccsi_constrain_weights(raw, cfg)
check("14 sum 1", abs(sum(wt$constrained) - 1) < 1e-8)
check("14 issuer cap", {
  ok <- TRUE
  for (id in names(wt$constrained)) {
    cap <- if (id %in% c("ASML", "TSM")) 0.15 else 0.12
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
wt2 <- hccsi_constrain_weights(raw2, cfg)
check("14 NVDA raw high", raw2[["NVDA"]] > 0.12)
check("14 NVDA capped", wt2$constrained[["NVDA"]] <= 0.12 + 1e-8)

# 15 reproducible
sc15a <- hccsi_score(.mk_px_bench(px_down), cfg); sc15b <- hccsi_score(.mk_px_bench(px_down), cfg)
check("15 composite", identical(sc15a$composite, sc15b$composite))
check("15 indices", identical(sc15a$indices, sc15b$indices))
check("15 formula", grepl("0.30", sc15a$formula, fixed = TRUE) && grepl("Stmt", sc15a$formula, fixed = TRUE))
check("15 role", identical(sc15a$role, "tech_development_expectation"))

check("stmt grows with revenue", {
  hi <- inp10; lo <- inp10
  for (id in ids) { hi[[id]]$rev_yoy <- 0.20; lo[[id]]$rev_yoy <- -0.10 }
  shi <- hccsi_score(hi, cfg); slo <- hccsi_score(lo, cfg)
  is.finite(shi$indices$statement_development) && is.finite(slo$indices$statement_development) &&
    shi$indices$statement_development > slo$indices$statement_development
})
check("beta lifts influence", {
  hi <- inp10; lo <- inp10
  for (id in ids) { hi[[id]]$beta_60d <- 1.6; lo[[id]]$beta_60d <- 0.7 }
  shi <- hccsi_score(hi, cfg); slo <- hccsi_score(lo, cfg)
  is.finite(shi$indices$influence_vs_market) && is.finite(slo$indices$influence_vs_market) &&
    shi$indices$influence_vs_market > slo$indices$influence_vs_market
})

.mk_px <- function(px) lapply(stats::setNames(nm = ids), function(id) list(
  closes = px, dates = dates, volume = rep(1e6, n)))
sc_po_up <- hccsi_score(.mk_px(px_up), cfg)
sc_po_flat <- hccsi_score(.mk_px(rep(100, n)), cfg)
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

sc0 <- hccsi_score(NULL, cfg)
check("missing history no composite", !is.finite(sc0$composite))
check("missing history not placeholder", !isTRUE(sc0$composite %in% c(0, 50, 100, 70, 72)))
check("missing history unavailable", identical(sc0$alert, "Unavailable"))
check("missing history H NA", !is.finite(sc0$indices$systems_health))
check("missing history S NA", !is.finite(sc0$indices$systemic_stress))
check("missing history M NA", !is.finite(sc0$indices$market_observation))
check("missing history code", identical(sc0$failures$composite, "SOURCE_HISTORY_UNAVAILABLE"))
check("prior not used as H", isTRUE(sc0$issuers$ASML$criticality_prior == 95) &&
  !is.finite(sc0$issuer_health[["ASML"]]))

c_fill <- hccsi_composite(70, 40, 50, 50, cfg)
c_omit <- hccsi_composite(70, 40, 50, NA, cfg)
check("renorm omits Traj", is.finite(c_omit) &&
  abs(as.numeric(c_omit)[1] - (0.30 * 70 + 0.25 * 40 + 0.25 * 50) / 0.80) < 1e-8)
check("renorm != fill", abs(as.numeric(c_omit)[1] - as.numeric(c_fill)[1]) > 1e-6)
check("renorm dropped Traj", isTRUE("trajectory_vs_history" %in% attr(c_omit, "dropped")))

inp_one <- inp11
inp_one$TSM$rev_yoy <- 0.10; inp_one$TSM$excess_1y <- 0.08; inp_one$TSM$price_vs_hist <- 0.12
inp_one$TSM$gm_delta <- 0.01; inp_one$TSM$beta_60d <- 1.1; inp_one$TSM$rev_yoy_vs_own <- 0.02
sc_short <- hccsi_score(inp_one, cfg)
check("one cooler no litho path", !isTRUE("litho_foundry" %in% sc_short$contagion$channels))

live0 <- hccsi_live_inputs_from_prices(NULL, NULL, cfg)
check("live no dc default", is.null(live0$AAPL$data_confidence) || !is.finite(live0$AAPL$data_confidence))
toast_h <- hccsi_failure_toast("SOURCE_HISTORY_UNAVAILABLE", "composite", c("fragility"), "en")
check("history toast specific", grepl("default|withheld|unavailable", toast_h, ignore.case = TRUE) &&
  !grepl("Something went wrong", toast_h, ignore.case = TRUE))

macro_txt <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
mod_txt <- paste(readLines("hccsi_module.R", warn = FALSE), collapse = "\n")
check("no lazy default score", !grepl("want_px", macro_txt, fixed = TRUE))
check("always fetch history", grepl("Always attempt live Yahoo/history", macro_txt, fixed = TRUE))
check("live fetches statements", grepl("cached_scrape_financials", macro_txt, fixed = TRUE))
check("live 5y window", grepl("fetch_price_history_df(tk, \"5y\")", macro_txt, fixed = TRUE))
check("UI flow helper", grepl("ynow-hccsi-flow", mod_txt, fixed = TRUE) && grepl("hccsi_score_span", mod_txt, fixed = TRUE))
check("UI Rf not flow", grepl("ynow-macro-rf__value", macro_txt, fixed = TRUE) &&
  !grepl("ynow-macro-rf__value ynow-hccsi-flow", macro_txt, fixed = TRUE))
check("CSS logo flow stops", grepl("ynow-logo-flow", ui_txt, fixed = TRUE) &&
  grepl("#0C5484", ui_txt, fixed = TRUE) && grepl("#249C60", ui_txt, fixed = TRUE) &&
  grepl("#1AA8B8", ui_txt, fixed = TRUE) && grepl("ynow-hccsi-flow", ui_txt, fixed = TRUE))
kf <- gregexpr("@keyframes ynow-logo-flow", ui_txt, fixed = TRUE)[[1]]
check("single logo-flow keyframes", length(kf) == 1L && kf[[1]] > 0)
check("Data-limited reuses flow class", grepl(".ynow-fund-profile-badge .ynow-hccsi-flow", ui_txt, fixed = TRUE))

if (!exists("tags", inherits = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
  tags <- htmltools::tags
}
box_ok <- tryCatch(hccsi_kpi_box(sc13, lite = TRUE, locale = "en"), error = function(e) NULL)
if (!is.null(box_ok)) {
  html_ok <- paste(as.character(box_ok), collapse = " ")
  check("KPI numeral flow class", grepl("ynow-hccsi-flow", html_ok, fixed = TRUE))
  check("KPI numeral not unavailable", !grepl("ynow-hccsi-unavailable", html_ok, fixed = TRUE))
}
box_na <- tryCatch(hccsi_kpi_box(sc0, lite = TRUE, locale = "en"), error = function(e) NULL)
if (!is.null(box_na)) {
  html_na <- paste(as.character(box_na), collapse = " ")
  check("KPI unavailable class", grepl("ynow-hccsi-unavailable", html_na, fixed = TRUE))
  check("KPI unavailable no flow", !grepl("ynow-hccsi-flow", html_na, fixed = TRUE))
}
four <- tryCatch(.hccsi_four_boxes(sc13, "en"), error = function(e) NULL)
if (!is.null(four)) {
  html_four <- paste(as.character(four), collapse = " ")
  check("four-index flow class", grepl("ynow-hccsi-flow", html_four, fixed = TRUE))
}
check("UI beside Rf", grepl("ynow-macro-kpi--hccsi", macro_txt, fixed = TRUE) && grepl("ynow-macro-kpi--rf", macro_txt, fixed = TRUE))
check("UI same row", grepl("hccsi_col", macro_txt, fixed = TRUE) && grepl("rf_col", macro_txt, fixed = TRUE))
check("UI 1:1", grepl("kpi_w <- if (is_tw) 4L else 6L", macro_txt, fixed = TRUE))
check("UI TW+US", grepl("cols <- list(rf_col, hccsi_col)", macro_txt, fixed = TRUE))
check("UI expand below", {
  pos_row <- regexpr("rf_signal_row", macro_txt, fixed = TRUE)[1]
  pos_exp <- regexpr("ynow_macro_hccsi_expand", macro_txt, fixed = TRUE)[1]
  is.finite(pos_row) && is.finite(pos_exp) && pos_exp > pos_row
})
check("UI Full-only expand", grepl("ynow-macro-hccsi-expand ynow-full-only", macro_txt, fixed = TRUE))
check("UI Lite no click", grepl("if (!isTRUE(lite))", mod_txt, fixed = TRUE))
check("UI Lite CSS", grepl("body.ynow-lite #ynow_macro_hccsi_expand", ui_txt, fixed = TRUE))
check("UI four indices", grepl("hccsi-four", mod_txt, fixed = TRUE))
check("four-index uses flow span", grepl("hccsi_score_span(it$val)", mod_txt, fixed = TRUE))
check("layer table uses flow span", grepl("hccsi_score_span(result$issuer_health[[id]])", mod_txt, fixed = TRUE))
mod_lines <- strsplit(mod_txt, "\n", fixed = TRUE)[[1]]
ly_i <- grep("^\\.hccsi_layer_table <- function", mod_lines)
nw_i <- grep("^\\.hccsi_network_ui <- function", mod_lines)
layer_src <- if (length(ly_i) && length(nw_i) && nw_i[[1]] > ly_i[[1]]) {
  paste(mod_lines[ly_i[[1]]:(nw_i[[1]] - 1L)], collapse = "\n")
} else ""
check("layer table no substitutes header", !grepl("hccsi_col_substitutes", layer_src, fixed = TRUE))
check("layer table no replacement header", !grepl("hccsi_col_replacement", layer_src, fixed = TRUE))
check("layer table no rebuild-year mean", !grepl("replacement_time_years", layer_src, fixed = TRUE))
check("issuer pair not single constituent table", grepl(".hccsi_in_composite_block", mod_txt, fixed = TRUE) &&
  !grepl(".hccsi_constituent_table", mod_txt, fixed = TRUE) &&
  !grepl(".hccsi_issuer_pair", mod_txt, fixed = TRUE) &&
  !grepl(".hccsi_out_composite_table", mod_txt, fixed = TRUE))
check("in-composite table has live inputs", grepl("hccsi_col_rev_yoy", mod_txt, fixed = TRUE) &&
  grepl("hccsi_col_gm_delta", mod_txt, fixed = TRUE) && grepl("hccsi_col_capex_own", mod_txt, fixed = TRUE) &&
  grepl("hccsi_col_price_hist", mod_txt, fixed = TRUE))
check("stage table carries issuer role", grepl("hccsi_col_function", layer_src, fixed = TRUE) &&
  grepl("hccsi_col_issuer", layer_src, fixed = TRUE))
check("issuer tables drop cap theater", !grepl("hccsi_col_criticality", mod_txt, fixed = TRUE) &&
  !grepl("hccsi_col_weight_raw", mod_txt, fixed = TRUE) &&
  !grepl("hccsi_col_substitutes", mod_txt, fixed = TRUE) &&
  !grepl("hccsi_col_confidence", mod_txt, fixed = TRUE))
check("formula banner like WACC", grepl("ynow-hccsi-formula-banner", mod_txt, fixed = TRUE) &&
  grepl("hccsi_formula_eq", mod_txt, fixed = TRUE))
check("Rf not full-only", !grepl("ynow-macro-kpi--rf[^\\n]*ynow-full-only", macro_txt))

for (k in c("hccsi_title", "hccsi_disclosure", "hccsi_index_health", "hccsi_index_stress",
            "hccsi_index_fragility", "hccsi_index_market", "notif_hccsi_fx_missing",
            "hccsi_unavailable", "hccsi_dropped", "hccsi_dropped_none",
            "hccsi_alert_unavailable", "notif_hccsi_history_missing")) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("en name", identical(ui_str("hccsi_index_health", "en"), "Statement Development"))
check("zh name EN", identical(ui_str("hccsi_index_health", "zh-TW"), "Statement Development"))
check("zh gloss", grepl("財報發展", ui_str("hccsi_index_health_gloss", "zh-TW"), fixed = TRUE))
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("hccsi_disclosure", "zh-TW")))
hccsi_copy_keys <- grep("^hccsi_", names(.UI_STRINGS$en), value = TRUE)
check("hccsi keys in zh-TW", all(hccsi_copy_keys %in% names(.UI_STRINGS$`zh-TW`)))
check("zh stage is 環節", identical(ui_str("hccsi_col_layer", "zh-TW"), "環節"))
check("zh knock-on path", identical(ui_str("hccsi_contagion_paths", "zh-TW"), "連鎖降溫路徑"))
check("en stage not layer", identical(ui_str("hccsi_col_layer", "en"), "Stage"))
check("en knock-on path", identical(ui_str("hccsi_contagion_paths", "en"), "Linked stages cooling together"))
check("zh expand avoids 傳染/濾鏡", {
  blob <- paste(vapply(
    c("hccsi_disclosure", "hccsi_index_stress_gloss", "hccsi_network_note",
      "hccsi_contagion_paths", "hccsi_contagion_none", "hccsi_layer_title",
      "hccsi_highest_risk_layer", "hccsi_network_title", "hccsi_method_limits"),
    function(k) ui_str(k, "zh-TW"), character(1)), collapse = " ")
  !grepl("傳染", blob, fixed = TRUE) && !grepl("濾鏡", blob, fixed = TRUE)
})
check("copy has no ticker examples", {
  keys <- c("hccsi_method_selection", "hccsi_method_weighting", "hccsi_method_fx_adr",
            "hccsi_network_note", "hccsi_disclosure", "notif_hccsi_dup_issuer",
            "hccsi_formula_parts", "hccsi_out_composite_note")
  blob <- paste(c(vapply(keys, function(k) ui_str(k, "en"), character(1)),
                  vapply(keys, function(k) ui_str(k, "zh-TW"), character(1))), collapse = " ")
  !grepl("GOOGL", blob, fixed = TRUE) && !grepl("GOOG", blob, fixed = TRUE) &&
    !grepl("ASML", blob, fixed = TRUE) && !grepl("2330", blob, fixed = TRUE) &&
    !grepl("TSM", blob, fixed = TRUE)
})
check("ly enterprise_dbs zh", identical(ui_str("hccsi_ly_enterprise_dbs", "zh-TW"), "企業資料庫"))
check("named helper zh", identical(.hccsi_named("ly", "enterprise_dbs", "zh-TW"), "企業資料庫"))
if (requireNamespace("htmltools", quietly = TRUE) && requireNamespace("shiny", quietly = TRUE)) {
  if (!exists("tags", inherits = TRUE)) tags <- htmltools::tags
  if (!exists("column", inherits = TRUE)) column <- shiny::column
  if (!exists("fluidRow", inherits = TRUE)) fluidRow <- shiny::fluidRow
  exp_zh <- tryCatch(hccsi_expand_ui(sc13, "zh-TW"), error = function(e) {
    cat("EXPAND ERR ", e$message, "\n", sep = ""); NULL
  })
  check("expand zh renders", !is.null(exp_zh))
  if (!is.null(exp_zh)) {
    html_zh <- paste(as.character(exp_zh), collapse = " ")
    check("expand zh uses 企業資料庫", grepl("企業資料庫", html_zh, fixed = TRUE))
    check("expand zh uses 連鎖降溫路徑", grepl("連鎖降溫路徑", html_zh, fixed = TRUE))
    check("expand zh uses 環節", grepl("環節", html_zh, fixed = TRUE))
    check("expand zh no raw enterprise_dbs cell", !grepl(">enterprise_dbs<", html_zh, fixed = TRUE))
    check("expand zh formula banner", grepl("HCCSI = 0.30", html_zh, fixed = TRUE))
    check("expand zh in-composite title", grepl("進入複合分數", html_zh, fixed = TRUE))
    check("expand zh no out-composite title", !grepl("不進入複合分數", html_zh, fixed = TRUE))
    check("expand zh role in stages", grepl("在鏈上的角色", html_zh, fixed = TRUE))
    check("expand zh no Criticality header", !grepl(">Criticality<", html_zh, fixed = TRUE))
    check("expand zh no 可替代對象 header", !grepl(">可替代對象<", html_zh, fixed = TRUE))
    check("expand zh no 未受限權重", !grepl("未受限權重", html_zh, fixed = TRUE))
  }
  layer_en <- tryCatch(.hccsi_layer_table(sc13, "en"), error = function(e) NULL)
  layer_zh <- tryCatch(.hccsi_layer_table(sc13, "zh-TW"), error = function(e) NULL)
  in_en <- tryCatch(.hccsi_in_composite_table(sc13, "en"), error = function(e) NULL)
  banner_en <- tryCatch(.hccsi_formula_banner(sc13, "en"), error = function(e) NULL)
  check("issuer label drops dup ticker", identical(.hccsi_issuer_label(sc13$issuers$ASML), "ASML"))
  check("issuer label keeps extra class", identical(.hccsi_issuer_label(sc13$issuers$GOOGL), "GOOGL/GOOG"))
  if (!is.null(layer_en)) {
    html_ly <- paste(as.character(layer_en), collapse = " ")
    check("layer HTML no substitutes col", !grepl("What can replace it", html_ly, fixed = TRUE))
    check("layer HTML no rebuild col", !grepl("Years to rebuild", html_ly, fixed = TRUE))
    check("layer HTML has issuer", grepl("Issuer", html_ly, fixed = TRUE))
    check("layer HTML has role", grepl("Role in the chain", html_ly, fixed = TRUE))
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
    check("in-composite no substitutes", !grepl("What can replace it", html_in, fixed = TRUE))
    check("in-composite no Uncapped", !grepl("Uncapped weight", html_in, fixed = TRUE))
    check("in-composite no Criticality", !grepl("Criticality", html_in, fixed = TRUE))
    check("in-composite no spaced dup ticker", !grepl("ASML ASML", html_in, fixed = TRUE) &&
      !grepl("GOOGL GOOGL", html_in, fixed = TRUE))
  }
  if (!is.null(banner_en)) {
    html_bn <- paste(as.character(banner_en), collapse = " ")
    check("banner equation", grepl("HCCSI = 0.30·Stmt + 0.25·Mkt + 0.25·Inf + 0.20·Traj", html_bn, fixed = TRUE))
    check("banner WACC-like class", grepl("ynow-hccsi-formula-banner__eq", html_bn, fixed = TRUE))
  }
}

for (fn in c("hccsi_config.R", "hccsi_engine.R", "hccsi_module.R", "macro_market_module.R", "global.R", "ui_locale.R")) {
  parsed <- tryCatch({ parse(fn, keep.source = FALSE); TRUE }, error = function(e) { cat("PARSE ", fn, ": ", e$message, "\n"); FALSE })
  check(paste("parse", fn), isTRUE(parsed))
}

if (fail > 0L) { cat("\n", fail, " failure(s)\n", sep = ""); quit(status = 1) }
cat("\nAll HCCSI checks passed.\n")
