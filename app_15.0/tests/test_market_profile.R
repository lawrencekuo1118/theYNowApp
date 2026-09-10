# Tests for market_profile + Blue Chip detail row count invariant (no network).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

root <- normalizePath("..", mustWork = TRUE)
# Allow running from app_15.0/tests
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source(file.path(root, "market_profile.R"), local = TRUE, encoding = "UTF-8")
source(file.path(root, "ui_locale.R"), local = TRUE, encoding = "UTF-8")

check("US normalize AAPL", identical(normalize_ticker_for_market("aapl", "US"), "AAPL"))
check("TW normalize 2330", identical(normalize_ticker_for_market("2330", "TW"), "2330.TW"))
check("TW keep 2330.TW", identical(normalize_ticker_for_market("2330.TW", "TW"), "2330.TW"))
check("TW spaced bare", identical(normalize_ticker_for_market("  2330  ", "TW"), "2330.TW"))
check("TW spaced suffix", identical(normalize_ticker_for_market("2330 .TW", "TW"), "2330.TW"))
check("TW lowercase suffix", identical(normalize_ticker_for_market("2330.tw", "TW"), "2330.TW"))
check("TW label strip", identical(normalize_ticker_for_market("2330.TW — 台積電", "TW"), "2330.TW"))
check("TW alt helper", identical(tw_yahoo_alt_ticker("3105.TW"), "3105.TWO"))
check("TW profile tax 20", identical(as.integer(market_profile("TW")$wacc_tax), 20L))
check("US profile tax 21", identical(as.integer(market_profile("US")$wacc_tax), 21L))
check("TW default ticker", identical(market_profile("TW")$default_ticker, "2330.TW"))
check("TW hides SEC", isFALSE(market_profile("TW")$show_sec_lab))
check("US shows SEC", isTRUE(market_profile("US")$show_sec_lab))
check("TW bench 0050", identical(market_profile("TW")$beta_bench, "0050.TW"))

# Display vs fetch
check("TW display strips .TW", identical(display_ticker_for_market("2330.TW", "TW"), "2330"))
check("TW display strips .TWO", identical(display_ticker_for_market("3105.TWO", "TW"), "3105"))
check("TW display bare stays", identical(display_ticker_for_market("2330", "TW"), "2330"))
check("US display AAPL", identical(display_ticker_for_market("AAPL", "US"), "AAPL"))
check("US display keeps TSM", identical(display_ticker_for_market("TSM", "US"), "TSM"))
check(
  "fetch == normalize",
  identical(fetch_ticker_for_market("2330", "TW"), normalize_ticker_for_market("2330", "TW"))
)

# Locale
check("TW locale zh-TW", identical(locale_for_market("TW"), "zh-TW"))
check("US locale en", identical(locale_for_market("US"), "en"))
check("ui_str zh ticker", grepl("代號", ui_str("ticker_label", "zh-TW")))
check("ui_str en ticker", grepl("Ticker", ui_str("ticker_label", "en")))
check("no simplified 默认", !grepl("默认", ui_str("data_source_body", "zh-TW")))

# CJK alias / universe name search
check("query_has_cjk", isTRUE(query_has_cjk("台積")))
check("query_no_cjk digits", isFALSE(query_has_cjk("2330")))
alias_hits <- search_tw_universe_by_name("台積", max_results = 5L)
check("CJK 台積 hits 2330", "2330.TW" %in% unname(alias_hits))
honghai <- search_tw_universe_by_name("鴻海", max_results = 5L)
check("CJK 鴻海 hits 2317", "2317.TW" %in% unname(honghai))

# Universe cache（若存在）：上櫃純數字應解析為 .TWO
cache_path <- file.path(root, "data", "tw_universe.csv")
if (file.exists(cache_path)) {
  source(file.path(root, "lab_tw_universe.R"), local = TRUE, encoding = "UTF-8")
  check(
    "TW OTC bare 3105 → .TWO",
    identical(normalize_ticker_for_market("3105", "TW"), "3105.TWO")
  )
  check(
    "TW listed bare 2330 stays .TW",
    identical(normalize_ticker_for_market("2330", "TW"), "2330.TW")
  )
  uni_hits <- search_tw_universe_by_name("台灣積體", max_results = 8L)
  check(
    "universe CJK 台灣積體 → 2330",
    "2330.TW" %in% unname(uni_hits)
  )
} else {
  cat("SKIP: tw_universe.csv not present for OTC resolve checks\n")
}

# Minimal merge: detail path must keep all evaluated rows even if eq/gate would filter
source(file.path(root, "lab_industry_method.R"), local = TRUE, encoding = "UTF-8")

catlg <- data.frame(
  ticker = c("AAA", "BBB", "CCC"),
  industry_key = c("sc.Foundry", "sc.Foundry", "sc.Foundry"),
  industry_label = c("A", "B", "C"),
  primary = c("dcf", "dcf", "dcf"),
  secondary = c("pb", "pb", "pb"),
  stringsAsFactors = FALSE
)
scores <- data.frame(
  ticker = c("AAA", "BBB", "CCC"),
  ok = c(TRUE, TRUE, TRUE),
  f_score = c(8, 5, 9),
  quality_flag = c(1, 0, 1),
  is_quality = c(TRUE, FALSE, TRUE),
  is_quality_upside = c(TRUE, FALSE, TRUE),
  market_cap = c(1e11, 2e10, 5e10),
  size_band = c("large", "mid", "mid"),
  price = c(10, 20, 30),
  fv = c(15, 25, 40),
  upside_total_pct = c(50, 25, 33),
  upside_cagr_pct = c(8, 4, 6),
  n_years = c(5L, 5L, 5L),
  fv_note = c("", "", ""),
  method_used = c("dcf", "dcf", "dcf"),
  primary_live = c("dcf", "dcf", "dcf"),
  company_type = c("", "", ""),
  company_name = c("A", "B", "C"),
  error = c("", "", ""),
  stringsAsFactors = FALSE
)

detail <- lab_merge_catalog_scores(
  catlg, scores = scores,
  eq_only = FALSE, gate_only = FALSE, evaluated_only = TRUE
)
check("detail keeps N=3 evaluated", nrow(detail) == 3L)

gated <- lab_merge_catalog_scores(
  catlg, scores = scores,
  eq_only = TRUE, gate_only = TRUE, evaluated_only = TRUE
)
check("quality filters shrink leaderboard path", nrow(gated) < 3L)

if (fail > 0L) {
  cat("FAILED checks:", fail, "\n")
  quit(status = 1)
}
cat("All market_profile / detail-N checks passed.\n")
