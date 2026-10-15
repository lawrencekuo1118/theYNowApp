# Offline repro: why US ASX resolves to sc.IC_Design instead of sc.Packaging
# Usage (from repo root or app_24.0/):
#   Rscript app_24.0/scripts/repro_asx_industry_debug.R
#   cd app_24.0 && Rscript scripts/repro_asx_industry_debug.R
#
# Writes NDJSON to /opt/cursor/artifacts/asx_industry_debug.log

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("scripts/repro_asx_industry_debug.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(root)

log_path <- "/opt/cursor/artifacts/asx_industry_debug.log"
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
# Fresh run
tryCatch(unlink(log_path), error = function(e) NULL)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("industry_standards.R", local = TRUE, encoding = "UTF-8")
if (file.exists("lab_sp500_universe.R")) {
  suppressWarnings(try(source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8"), silent = TRUE))
}
if (file.exists("lab_adr.R")) {
  suppressWarnings(try(source("lab_adr.R", local = TRUE, encoding = "UTF-8"), silent = TRUE))
}
if (file.exists("lab_us_universe.R")) {
  suppressWarnings(try(source("lab_us_universe.R", local = TRUE, encoding = "UTF-8"), silent = TRUE))
}

.dbg <- function(hypothesisId, location, message, data) {
  payload <- list(
    hypothesisId = hypothesisId,
    location = location,
    message = message,
    data = data,
    timestamp = as.numeric(Sys.time()) * 1000
  )
  cat(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      "\n", file = log_path, append = TRUE)
}

# --- Static CSV evidence ---
uni_path <- file.path("data", "us_universe.csv")
ov_path <- file.path("data", "us_industry_overlay.csv")
uni_row <- NULL
ov_row <- NULL
if (file.exists(uni_path)) {
  u <- utils::read.csv(uni_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  uni_row <- u[toupper(u$ticker) == "ASX", , drop = FALSE]
}
if (file.exists(ov_path)) {
  o <- utils::read.csv(ov_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  ov_row <- o[toupper(o$ticker) == "ASX", , drop = FALSE]
}

adr_key <- ""
if (exists("lab_adr_industry_map", mode = "function")) {
  am <- lab_adr_industry_map()
  if ("ASX" %in% names(am)) adr_key <- unname(am[["ASX"]])
}

.dbg("A", "repro_asx_industry_debug.R:csv", "static CSV / ADR map for ASX", list(
  universe_path = uni_path,
  universe_key = if (nrow(uni_row %||% data.frame())) as.character(uni_row$industry_key[[1]]) else "",
  universe_raw = if (nrow(uni_row %||% data.frame()) && "industry_raw" %in% names(uni_row)) {
    as.character(uni_row$industry_raw[[1]])
  } else "",
  overlay_path = ov_path,
  overlay_key = if (nrow(ov_row %||% data.frame())) as.character(ov_row$industry_key[[1]]) else "",
  overlay_sector = if (nrow(ov_row %||% data.frame()) && "sector" %in% names(ov_row)) {
    as.character(ov_row$sector[[1]])
  } else "",
  overlay_industry = if (nrow(ov_row %||% data.frame()) && "industry" %in% names(ov_row)) {
    as.character(ov_row$industry[[1]])
  } else "",
  adr_map_key = adr_key,
  expected = "sc.Packaging"
))

# --- Live resolve (instrumented helpers also append) ---
yahoo_key <- resolve_industry_key_from_yahoo(
  sector = "Technology", industry = "Semiconductors"
)
us_lookup <- if (exists("lookup_us_ticker_industry_key", mode = "function")) {
  lookup_us_ticker_industry_key("ASX")
} else {
  ""
}
resolved <- resolve_industry_key_for_ticker(
  "ASX",
  market_mode = "US",
  sector = "Technology",
  industry = "Semiconductors",
  display_text = "Sector: Technology | Industry: Semiconductors"
)

zh <- ""
if (nzchar(resolved) && exists("industry_standards") && resolved %in% names(industry_standards)) {
  # Prefer labeled zh map if present at top of industry_standards.R
  labs <- tryCatch(get0("industry_labels_zh", ifnotfound = NULL), error = function(e) NULL)
  if (is.null(labs)) {
    # Common pattern: named character vector near file top — probe via grep of known labels
    zh <- switch(
      resolved,
      "sc.IC_Design" = "半導體｜IC 設計",
      "sc.Packaging" = "半導體｜封測",
      "sc.Foundry" = "半導體｜晶圓代工",
      resolved
    )
  }
}

.dbg("A", "repro_asx_industry_debug.R:resolve", "ASX resolve summary", list(
  yahoo_semiconductors_key = yahoo_key,
  lookup_us_ticker_industry_key = us_lookup,
  resolve_industry_key_for_ticker = resolved,
  snapshot_label_guess_zh = zh,
  mismatch_vs_packaging = !identical(resolved, "sc.Packaging")
))

cat("ASX us_lookup=", us_lookup, "\n", sep = "")
cat("ASX resolve=", resolved, " (", zh, ")\n", sep = "")
cat("Yahoo Semiconductors→", yahoo_key, "\n", sep = "")
cat("ADR map ASX→", adr_key, "\n", sep = "")
cat("Log → ", log_path, "\n", sep = "")
