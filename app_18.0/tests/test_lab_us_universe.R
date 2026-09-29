# Blue Chip / Search US universe (primary listings; SEC-backed)
# Run from app_18.0/: Rscript tests/test_lab_us_universe.R

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
source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_us_universe.R", local = TRUE, encoding = "UTF-8")
if (file.exists("lab_industry_method.R")) {
  # lab_us_quality_candidates lives here; source may pull heavy deps — guard
  suppressWarnings(
    try(source("lab_industry_method.R", local = TRUE, encoding = "UTF-8"), silent = TRUE)
  )
}

check <- function(msg, cond) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  cat("OK", msg, "\n")
}

u <- lab_get_us_universe(FALSE)
check("US universe nonempty", is.data.frame(u) && nrow(u) >= 1000L)
check("has NVDA", "NVDA" %in% u$ticker)
check("has JPM", "JPM" %in% u$ticker)
# Non–S&P example that is still primary-listed (may vary; skip if absent)
if ("SOFI" %in% u$ticker) check("has SOFI (non-SP500 typical)", TRUE)

sp <- tryCatch(lab_get_sp500_universe(FALSE), error = function(e) NULL)
if (is.data.frame(sp) && nrow(sp) > 0L) {
  check("US universe larger than S&P 500", nrow(u) > nrow(sp))
  non_sp <- setdiff(toupper(as.character(u$ticker)), toupper(as.character(sp$ticker)))
  check("has non-S&P primary listings", length(non_sp) >= 1000L)
}

meta <- lab_us_universe_meta()
check("meta n matches", identical(as.integer(meta$n), nrow(u)))
check("nasdaq+nyse cover most", (meta$n_nasdaq + meta$n_nyse) >= as.integer(0.9 * meta$n))

hits <- search_us_universe_by_name("NVIDIA", max_results = 5L)
check("search NVIDIA", length(hits) >= 1L && "NVDA" %in% unname(hits))
sofi_hits <- search_us_universe_by_name("SoFi", max_results = 5L)
check("search SoFi non-S&P", length(sofi_hits) >= 1L && "SOFI" %in% unname(sofi_hits))

# Industry overlay path (bundled CSV) maps some non-S&P names out of Unmapped
ov_path <- lab_us_existing_industry_overlay()
check("industry overlay file present", !is.na(ov_path) && file.exists(ov_path))
if ("SHOP" %in% u$ticker) {
  shop_key <- as.character(u$industry_key[match("SHOP", u$ticker)])
  check("SHOP mapped via overlay/ADR path", nzchar(shop_key) && !identical(shop_key, LAB_UNMAPPED_KEY))
}
if ("SOFI" %in% u$ticker) {
  sofi_key <- as.character(u$industry_key[match("SOFI", u$ticker)])
  check("SOFI mapped via overlay", nzchar(sofi_key) && !identical(sofi_key, LAB_UNMAPPED_KEY))
}

pool <- data.frame(ticker = u$ticker[seq_len(min(2000L, nrow(u)))], stringsAsFactors = FALSE)
pre <- lab_us_prescreen_eval_pool(pool, max_n = 100L)
check("prescreen size", nrow(pre) <= 100L && nrow(pre) >= 1L)

# Blue Chip candidates come from full US universe (with S&P fallback only if empty)
if (exists("lab_us_quality_candidates", mode = "function")) {
  cands <- tryCatch(lab_us_quality_candidates(), error = function(e) NULL)
  if (is.list(cands) && length(cands)) {
    all_tk <- unique(unlist(cands, use.names = FALSE))
    check("bluechip candidates nonempty", length(all_tk) >= 100L)
    if (is.data.frame(sp) && nrow(sp) > 0L) {
      check(
        "bluechip candidates include non-S&P",
        length(setdiff(toupper(all_tk), toupper(as.character(sp$ticker)))) >= 1L
      )
    }
  }
}

cat("PASS lab_us_universe\n")
