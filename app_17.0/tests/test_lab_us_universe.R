# Blue Chip / Search US universe (primary listings; SEC-backed)
# Run from app_17.0/: Rscript tests/test_lab_us_universe.R

root <- if (file.exists("lab_us_universe.R")) {
  getwd()
} else if (file.exists("app_17.0/lab_us_universe.R")) {
  file.path(getwd(), "app_17.0")
} else {
  stop("Run from repo root or app_17.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

source("market_profile.R", local = TRUE, encoding = "UTF-8")
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_us_universe.R", local = TRUE, encoding = "UTF-8")

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

meta <- lab_us_universe_meta()
check("meta n matches", identical(as.integer(meta$n), nrow(u)))
check("nasdaq+nyse cover most", (meta$n_nasdaq + meta$n_nyse) >= as.integer(0.9 * meta$n))

hits <- search_us_universe_by_name("NVIDIA", max_results = 5L)
check("search NVIDIA", length(hits) >= 1L && "NVDA" %in% unname(hits))

pool <- data.frame(ticker = u$ticker[seq_len(min(2000L, nrow(u)))], stringsAsFactors = FALSE)
pre <- lab_us_prescreen_eval_pool(pool, max_n = 100L)
check("prescreen size", nrow(pre) <= 100L && nrow(pre) >= 1L)

cat("PASS lab_us_universe\n")
