#!/usr/bin/env Rscript
# Fill industry_key on data/us_industry_overlay.csv via resolve_industry_key_from_yahoo.
# Run after scripts/build_us_industry_overlay.py (no network).
#
# Usage (from app_18.0/): Rscript scripts/fill_us_industry_overlay_keys.R

root <- if (file.exists("industry_standards.R")) {
  getwd()
} else if (file.exists("app_18.0/industry_standards.R")) {
  file.path(getwd(), "app_18.0")
} else {
  stop("Run from repo root or app_18.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("industry_standards.R", local = TRUE, encoding = "UTF-8")
if (file.exists("lab_sp500_universe.R")) {
  suppressWarnings(try(source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8"), silent = TRUE))
}

path <- file.path("data", "us_industry_overlay.csv")
if (!file.exists(path)) stop("Missing ", path)

d <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
if (!nrow(d)) {
  message("Empty overlay")
  quit(save = "no", status = 0)
}
for (col in c("ticker", "sector", "industry", "industry_key", "industry_raw", "source", "fetched_at")) {
  if (!(col %in% names(d))) d[[col]] <- NA_character_
}

mapped <- 0L
for (i in seq_len(nrow(d))) {
  key <- tryCatch(
    resolve_industry_key_from_yahoo(
      display_text = as.character(d$industry_raw[[i]] %||% ""),
      sector = as.character(d$sector[[i]] %||% ""),
      industry = as.character(d$industry[[i]] %||% "")
    ),
    error = function(e) ""
  )
  key <- as.character(key %||% "")[1]
  if (!nzchar(key)) next
  if (exists("industry_standards", inherits = TRUE) &&
      is.list(industry_standards) &&
      !(key %in% names(industry_standards))) {
    next
  }
  d$industry_key[[i]] <- key
  mapped <- mapped + 1L
}

utils::write.csv(d, path, row.names = FALSE, fileEncoding = "UTF-8")
message("Mapped ", mapped, "/", nrow(d), " → ", normalizePath(path, mustWork = FALSE))
