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

# 10 single outage != Critical
inp10 <- lapply(stats::setNames(nm = ids), function(id) list(
  financial_resilience = 75, operational_continuity = 75, supply_chain_resilience = 75,
  market_stability = 75, data_confidence = 70, ops_incident = FALSE, stress_duration_days = 0))
inp10$ASML$ops_incident <- TRUE; inp10$ASML$ops_incident_persistent <- FALSE
inp10$ASML$ops_incident_stress <- 50; inp10$ASML$stress_duration_days <- 1; inp10$ASML$operational_continuity <- 55
sc10 <- hccsi_score(inp10, cfg)
check("10 not Critical", !identical(sc10$alert, "Critical"))
check("10 no channel", identical(length(sc10$contagion$channels), 0L))

# 11 persistent connected stress
inp11 <- inp10
for (id in c("ASML", "TSM")) {
  inp11[[id]]$ops_incident <- TRUE; inp11[[id]]$ops_incident_persistent <- TRUE
  inp11[[id]]$ops_incident_stress <- 80; inp11[[id]]$supply_disruption <- 80
  inp11[[id]]$issuer_stress <- 80; inp11[[id]]$stress_duration_days <- 25
  inp11[[id]]$operational_continuity <- 30
}
sc11 <- hccsi_score(inp11, cfg)
check("11 contagion", length(sc11$contagion$channels) >= 1L)
check("11 litho_foundry", "litho_foundry" %in% sc11$contagion$channels)
check("11 penalty", is.finite(sc11$contagion$penalty) && sc11$contagion$penalty > 0)

# 12 price decline = market stress only
n <- 80L; px_down <- 120 - seq_len(n) * 0.6
inp12 <- lapply(stats::setNames(nm = ids), function(id) list(
  financial_resilience = 75, operational_continuity = 75, supply_chain_resilience = 75,
  market_stability = 75, data_confidence = 70, closes = px_down, dates = dates, volume = rep(1e6, n)))
inp12b <- lapply(stats::setNames(nm = ids), function(id) list(
  financial_resilience = 75, operational_continuity = 75, supply_chain_resilience = 75,
  market_stability = 75, data_confidence = 70, closes = rep(100, n), dates = dates, volume = rep(1e6, n)))
sc12 <- hccsi_score(inp12, cfg); sc12b <- hccsi_score(inp12b, cfg)
check("12 health stable", abs(sc12$indices$systems_health - sc12b$indices$systems_health) < 1.5)
check("12 market lower", is.finite(sc12$indices$market_observation) && sc12$indices$market_observation < sc12b$indices$market_observation)
check("12 not Critical", !identical(sc12$alert, "Critical"))

# 13 higher prices do not raise Systems Health
px_up <- 80 + seq_len(n) * 0.8
inp13 <- lapply(stats::setNames(nm = ids), function(id) list(
  financial_resilience = 75, operational_continuity = 75, supply_chain_resilience = 75,
  market_stability = 75, data_confidence = 70, closes = px_up, dates = dates, volume = rep(1e6, n)))
sc13 <- hccsi_score(inp13, cfg)
check("13 health unchanged", abs(sc13$indices$systems_health - sc12b$indices$systems_health) < 1.5)
check("13 market higher", is.finite(sc13$indices$market_observation) && sc13$indices$market_observation > sc12b$indices$market_observation)
check("13 composite not lifted", is.finite(sc13$composite) && sc13$composite <= sc12b$composite + 0.75)

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
sc15a <- hccsi_score(inp12, cfg); sc15b <- hccsi_score(inp12, cfg)
check("15 composite", identical(sc15a$composite, sc15b$composite))
check("15 indices", identical(sc15a$indices, sc15b$indices))
check("15 formula", grepl("0.40", sc15a$formula, fixed = TRUE) && grepl("M*", sc15a$formula, fixed = TRUE))
check("15 role", identical(sc15a$role, "systemic_risk_observation"))

macro_txt <- paste(readLines("macro_market_module.R", warn = FALSE), collapse = "\n")
ui_txt <- paste(readLines("ynow_ui.R", warn = FALSE), collapse = "\n")
mod_txt <- paste(readLines("hccsi_module.R", warn = FALSE), collapse = "\n")
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
check("Rf not full-only", !grepl("ynow-macro-kpi--rf[^\\n]*ynow-full-only", macro_txt))

for (k in c("hccsi_title", "hccsi_disclosure", "hccsi_index_health", "hccsi_index_stress",
            "hccsi_index_fragility", "hccsi_index_market", "notif_hccsi_fx_missing")) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("en name", identical(ui_str("hccsi_index_health", "en"), "Systems Health Index"))
check("zh name EN", identical(ui_str("hccsi_index_health", "zh-TW"), "Systems Health Index"))
check("zh gloss", grepl("系統健康", ui_str("hccsi_index_health_gloss", "zh-TW"), fixed = TRUE))
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("hccsi_disclosure", "zh-TW")))

for (fn in c("hccsi_config.R", "hccsi_engine.R", "hccsi_module.R", "macro_market_module.R", "global.R", "ui_locale.R")) {
  parsed <- tryCatch({ parse(fn, keep.source = FALSE); TRUE }, error = function(e) { cat("PARSE ", fn, ": ", e$message, "\n"); FALSE })
  check(paste("parse", fn), isTRUE(parsed))
}

if (fail > 0L) { cat("\n", fail, " failure(s)\n", sep = ""); quit(status = 1) }
cat("\nAll HCCSI checks passed.\n")
