#!/usr/bin/env Rscript
# Build default industry-chain mcap-weighted upside snapshot (US).
# Samples top market-cap names per chain, evaluates FV upside, aggregates all chains.
#
# Usage (from app_22.0/):
#   RETICULATE_PYTHON=./.ynow_venv/bin/python \
#     YNOW_IND_MCAP_PER_CHAIN=2 Rscript scripts/build_ind_mcap_chain_rank_snapshot.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
setwd(app_dir)
cat("app_dir=", app_dir, "\n", sep = "")

per_chain <- suppressWarnings(as.integer(Sys.getenv("YNOW_IND_MCAP_PER_CHAIN", "2")))
if (!is.finite(per_chain) || per_chain < 1L) per_chain <- 2L

py <- Sys.getenv("RETICULATE_PYTHON", unset = "")
if (!nzchar(py) && file.exists(".ynow_venv/bin/python")) {
  Sys.setenv(RETICULATE_PYTHON = normalizePath(".ynow_venv/bin/python"))
}

suppressPackageStartupMessages({
  pacman::p_load(
    tidyverse, dplyr, stringr, jsonlite, rvest, xml2,
    memoise, cachem, TTR, reticulate, shiny
  )
})
if (nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
  tryCatch(
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE),
    error = function(e) cat("WARN use_python:", conditionMessage(e), "\n")
  )
}

src <- function(f) {
  if (!file.exists(f)) return(invisible(FALSE))
  tryCatch({
    source(f, local = FALSE, encoding = "UTF-8")
    TRUE
  }, error = function(e) {
    cat("WARN source ", f, ": ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
src("debug_helpers.R")
src("setup.R")
src("market_profile.R")
src("ui_locale.R")
src("web_crawler.R")
src("industry_standards.R")
src("fundamental_profile.R")
src("fcf_projection_module.R")
src("ddm_module.R")
src("ri_module.R")
src("pb_asset_module.R")
src("nav_module.R")
src("relative_multiples_module.R")
src("sotp_module.R")
src("earnings_quality_module.R")
src("default_config.R")
src("lab_industry_method.R")
src("lab_us_universe.R")
src("lab_sp500_universe.R")
src("lab_concept_groups.R")

stopifnot(exists("cached_scrape_financials", mode = "function"))
stopifnot(exists("lab_build_industry_method_catalog", mode = "function"))

catlg <- lab_build_industry_method_catalog(market_mode = "US")
stopifnot(is.data.frame(catlg), nrow(catlg) > 0L)
catlg$chain_id <- vapply(as.character(catlg$industry_key), industry_chain_id, character(1))
catlg <- tryCatch(lab_attach_market_caps(catlg), error = function(e) catlg)
mcap <- suppressWarnings(as.numeric(catlg$market_cap))
if (!any(is.finite(mcap) & mcap > 0, na.rm = TRUE)) {
  stop("No market_cap on catalog — check universe_metrics_snapshot.csv")
}

chains <- unname(lab_industry_chain_picker_choices("zh-TW"))
pick <- lapply(chains, function(cid) {
  sub <- catlg[
    !is.na(catlg$chain_id) & catlg$chain_id == cid &
      is.finite(suppressWarnings(as.numeric(catlg$market_cap))) &
      suppressWarnings(as.numeric(catlg$market_cap)) > 0,
    ,
    drop = FALSE
  ]
  if (!nrow(sub)) return(NULL)
  sub <- sub[order(-suppressWarnings(as.numeric(sub$market_cap))), , drop = FALSE]
  head(sub, per_chain)
})
pool <- do.call(rbind, Filter(Negate(is.null), pick))
pool <- pool[!duplicated(toupper(as.character(pool$ticker))), , drop = FALSE]
cat("eval pool n=", nrow(pool), " chains_covered=", length(unique(pool$chain_id)), "\n", sep = "")

cols <- intersect(c("ticker", "industry_key", "primary"), names(pool))
scores <- lab_screen_tickers_fscore(
  pool[, cols, drop = FALSE],
  max_n = nrow(pool),
  progress_cb = function(i, n, tk) {
    if (i == 1L || i == n || i %% 5L == 0L) {
      cat(sprintf("  eval %d/%d %s\n", i, n, tk))
    }
  }
)
stopifnot(is.data.frame(scores), nrow(scores) >= 1L)
n_ok <- sum(isTRUE(scores$ok) | (is.finite(scores$upside_cagr_pct) & is.finite(scores$market_cap)), na.rm = TRUE)
cat("scores ok/finite upside+mcap ≈ ", n_ok, " / ", nrow(scores), "\n", sep = "")

merged <- lab_merge_catalog_scores(
  catlg,
  scores = scores,
  method_filter = character(0),
  industry_filter = character(0),
  eq_only = FALSE,
  gate_only = FALSE,
  evaluated_only = TRUE
)
# Ensure mcap on merged from scores/catalog
if (!"market_cap" %in% names(merged) ||
    !any(is.finite(suppressWarnings(as.numeric(merged$market_cap))), na.rm = TRUE)) {
  merged <- tryCatch(lab_attach_market_caps(merged), error = function(e) merged)
}
# Prefer market_cap from scores when present
if ("market_cap" %in% names(scores)) {
  idx <- match(toupper(merged$ticker), toupper(scores$ticker))
  hit <- which(is.finite(idx))
  sc_m <- suppressWarnings(as.numeric(scores$market_cap[idx[hit]]))
  use <- is.finite(sc_m) & sc_m > 0
  if (any(use)) merged$market_cap[hit[use]] <- sc_m[use]
}

options(ynow.ind_mcap_chain_snapshot = NULL)
built <- lab_build_ind_mcap_chain_snapshot(
  merged,
  market = "US",
  eq_only = FALSE,
  gate_only = FALSE,
  no_alert = FALSE,
  locale = "zh-TW",
  universe_df = catlg
)
dest <- file.path("data", "ind_mcap_chain_rank_snapshot_US.csv")
utils::write.csv(built$snapshot, dest, row.names = FALSE, fileEncoding = "UTF-8")
cat(
  "Wrote ", dest, " rows=", nrow(built$snapshot),
  " finite=", sum(is.finite(built$snapshot$wavg_cagr)), "\n",
  sep = ""
)
print(built$table)
cat("DONE\n")
