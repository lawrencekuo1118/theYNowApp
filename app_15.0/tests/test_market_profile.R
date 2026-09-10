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

check("US normalize AAPL", identical(normalize_ticker_for_market("aapl", "US"), "AAPL"))
check("TW normalize 2330", identical(normalize_ticker_for_market("2330", "TW"), "2330.TW"))
check("TW keep 2330.TW", identical(normalize_ticker_for_market("2330.TW", "TW"), "2330.TW"))
check("TW profile tax 20", identical(as.integer(market_profile("TW")$wacc_tax), 20L))
check("US profile tax 21", identical(as.integer(market_profile("US")$wacc_tax), 21L))
check("TW default ticker", identical(market_profile("TW")$default_ticker, "2330.TW"))
check("TW hides SEC", isFALSE(market_profile("TW")$show_sec_lab))
check("US shows SEC", isTRUE(market_profile("US")$show_sec_lab))
check("TW bench 0050", identical(market_profile("TW")$beta_bench, "0050.TW"))

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
