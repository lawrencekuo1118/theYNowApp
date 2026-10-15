#!/usr/bin/env Rscript
# Build data/us_industry_overlay.csv for non–S&P US primary listings.
# Ranks Unmapped names by market_cap (universe_metrics_snapshot), fetches Yahoo
# Sector/Industry, maps via resolve_industry_key_from_yahoo.
#
# Usage (from app_18.0/):
#   Rscript scripts/build_us_industry_overlay.R
#   Rscript scripts/build_us_industry_overlay.R --max-n 100
#   YNOW_DEBUG_SKIP_PY=1 Rscript ...   # dry: keep existing overlay only

args <- commandArgs(trailingOnly = TRUE)
max_n <- 300L
if ("--max-n" %in% args) {
  i <- match("--max-n", args)
  max_n <- suppressWarnings(as.integer(args[i + 1L])[1])
  if (!is.finite(max_n) || max_n < 10L) max_n <- 300L
}

root <- if (file.exists("lab_us_universe.R")) {
  getwd()
} else if (file.exists("app_18.0/lab_us_universe.R")) {
  file.path(getwd(), "app_18.0")
} else {
  stop("Run from repo root or app_18.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("industry_standards.R", local = TRUE, encoding = "UTF-8")
source("lab_adr.R", local = TRUE, encoding = "UTF-8")
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_us_universe.R", local = TRUE, encoding = "UTF-8")
if (file.exists("web_crawler.R")) {
  suppressWarnings(try(source("web_crawler.R", local = TRUE, encoding = "UTF-8"), silent = TRUE))
}

unmapped_key <- if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) LAB_UNMAPPED_KEY else "lab.Unmapped"
out_path <- file.path("data", "us_industry_overlay.csv")

u <- lab_get_us_universe(FALSE)
sp <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) NULL)
sp_tks <- if (is.data.frame(sp) && nrow(sp)) {
  unique(toupper(trimws(as.character(sp$ticker))))
} else {
  character(0)
}

need <- u[
  !(toupper(trimws(as.character(u$ticker))) %in% sp_tks) &
    (is.na(u$industry_key) | !nzchar(as.character(u$industry_key)) |
       as.character(u$industry_key) == unmapped_key),
  ,
  drop = FALSE
]
message("Unmapped non-S&P candidates: ", nrow(need))

caps <- rep(0, nrow(need))
snap_path <- file.path("data", "universe_metrics_snapshot.csv")
if (file.exists(snap_path)) {
  snap <- tryCatch(
    utils::read.csv(snap_path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.data.frame(snap) && all(c("ticker", "market_cap") %in% names(snap))) {
    idx <- match(
      toupper(trimws(as.character(need$ticker))),
      toupper(trimws(as.character(snap$ticker)))
    )
    caps <- ifelse(is.na(idx), 0, suppressWarnings(as.numeric(snap$market_cap[idx])))
    caps[!is.finite(caps)] <- 0
  }
}
need <- need[order(-caps, need$ticker), , drop = FALSE]
need <- utils::head(need, max_n)
message("Will fetch Yahoo industry for top ", nrow(need), " by market cap")

skip_py <- identical(Sys.getenv("YNOW_DEBUG_SKIP_PY", ""), "1")
rows <- list()

# Keep prior overlay rows not being refreshed
prior <- tryCatch(lab_us_load_industry_overlay(), error = function(e) NULL)
prior_keep <- NULL
if (is.data.frame(prior) && nrow(prior)) {
  refresh_set <- toupper(trimws(as.character(need$ticker)))
  prior_keep <- prior[!(prior$ticker %in% refresh_set), , drop = FALSE]
}

if (isTRUE(skip_py)) {
  message("YNOW_DEBUG_SKIP_PY=1 — not calling Yahoo; rewriting prior overlay only")
} else if (!exists("get_yahoo_industry", mode = "function")) {
  message("get_yahoo_industry missing — cannot enrich; keeping prior overlay")
} else {
  for (i in seq_len(nrow(need))) {
    tk <- as.character(need$ticker[[i]])
    info <- tryCatch(get_yahoo_industry(tk), error = function(e) NULL)
    sector <- as.character(info$sector %||% "")[1]
    industry <- as.character(info$industry %||% "")[1]
    if (!nzchar(sector) && !nzchar(industry) && nzchar(as.character(info$display_text %||% ""))) {
      # parse from display if present
    }
    key <- ""
    if (exists("resolve_industry_key_from_yahoo", mode = "function")) {
      key <- tryCatch(
        resolve_industry_key_from_yahoo(
          display_text = as.character(info$display_text %||% ""),
          sector = sector,
          industry = industry
        ),
        error = function(e) ""
      )
    }
    key <- as.character(key %||% "")[1]
    if (!nzchar(key) || identical(key, unmapped_key)) {
      message(sprintf("[%d/%d] %s → unmapped (%s | %s)", i, nrow(need), tk, sector, industry))
      Sys.sleep(0.15)
      next
    }
    raw <- as.character(info$display_text %||% "")[1]
    if (!nzchar(raw)) raw <- paste0("Sector: ", sector, " | Industry: ", industry)
    rows[[length(rows) + 1L]] <- data.frame(
      ticker = tk,
      sector = sector,
      industry = industry,
      industry_key = key,
      industry_raw = raw,
      source = "yahoo",
      fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      stringsAsFactors = FALSE
    )
    message(sprintf("[%d/%d] %s → %s", i, nrow(need), tk, key))
    Sys.sleep(0.12)
  }
}

new_df <- if (length(rows)) do.call(rbind, rows) else NULL
parts <- list(prior_keep, new_df)
parts <- parts[!vapply(parts, is.null, logical(1))]
parts <- parts[vapply(parts, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]
out <- if (length(parts)) {
  d <- do.call(rbind, parts)
  d <- d[!duplicated(d$ticker, fromLast = TRUE), , drop = FALSE]
  d[order(d$ticker), , drop = FALSE]
} else {
  data.frame(
    ticker = character(0), sector = character(0), industry = character(0),
    industry_key = character(0), industry_raw = character(0),
    source = character(0), fetched_at = character(0),
    stringsAsFactors = FALSE
  )
}

dir.create("data", showWarnings = FALSE)
utils::write.csv(out, out_path, row.names = FALSE, fileEncoding = "UTF-8")
message("Wrote ", nrow(out), " rows → ", normalizePath(out_path, mustWork = FALSE))
