#!/usr/bin/env Rscript
# Rebuild TYNOW monthly basket with current gate rules (network scrape).
# Run from repo root: Rscript scripts/rebuild_tynow_index.R

args <- commandArgs(trailingOnly = TRUE)
scan_cap <- suppressWarnings(as.integer(args[1]))
if (!is.finite(scan_cap) || scan_cap < 10L) scan_cap <- 500L

root <- normalizePath(file.path(dirname(normalizePath(commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))][1], mustWork = FALSE)), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "app_21.0"))) {
  root <- normalizePath(getwd())
}
app_dir <- file.path(root, "app_21.0")
stopifnot(dir.exists(app_dir))
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "")

cat("== TYNOW rebuild ==\n")
cat("app_dir=", app_dir, "\n", sep = "")
cat("scan_cap=", scan_cap, "\n", sep = "")

source("global.R", local = TRUE, encoding = "UTF-8")

ranked <- ynow_index_tw_ranked(scan_cap)
cat("ranked=", nrow(ranked), "\n", sep = "")
if (!nrow(ranked)) stop("No TW ranked tickers in universe snapshot")

path <- ynow_index_cache_path("TW")
cat("cache=", path, "\n", sep = "")

t0 <- Sys.time()
sel <- ynow_index_current(
  as_of = Sys.time(),
  rebuild = TRUE,
  market = "TW",
  ranked = ranked,
  cache_path = path
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

cat("month=", sel$month %||% "", "\n", sep = "")
cat("n=", sel$n %||% 0L, " scanned=", sel$scanned %||% 0L,
    " complete=", isTRUE(sel$complete),
    " hit_scan_cap=", isTRUE(sel$hit_scan_cap), "\n", sep = "")
cat(sprintf("elapsed_min=%.1f\n", elapsed))
if (length(sel$members)) {
  for (m in sel$members) {
    cat(sprintf(
      "  %s  mcap=%s  f_score=%s  n_alert=%s  w=%.3f\n",
      m$ticker,
      format(m$market_cap, scientific = FALSE, trim = TRUE),
      as.character(m$f_score %||% NA),
      as.character(m$n_alert %||% NA),
      as.numeric(m$weight %||% NA)
    ))
  }
} else {
  cat("  (no members)\n")
}
cat("wrote ", path, "\n", sep = "")
invisible(sel)
