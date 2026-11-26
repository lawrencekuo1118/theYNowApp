#!/usr/bin/env Rscript
# Post-fetch statement cleaning consistency:
# - persistent cache hits stamp integrity_original_checked (no Search re-scrape)
# - stale cleaner version re-normalizes on read
# - ynow_soft_fill_statements_post_fetch shared across modes; ranking skips TPEx
# Run: cd app_28.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_statement_post_fetch_cleaning.R

root <- if (file.exists("web_crawler.R")) {
  getwd()
} else if (file.exists("../web_crawler.R")) {
  normalizePath("..")
} else if (dir.exists("app_28.0") && file.exists("app_28.0/web_crawler.R")) {
  normalizePath("app_28.0")
} else {
  stop("Cannot locate app_28.0 / web_crawler.R")
}
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

suppressPackageStartupMessages({
  library(cachem)
  library(memoise)
})
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <<- function(...) invisible(NULL)
}

source("setup.R", local = TRUE, encoding = "UTF-8")
source("web_crawler.R", local = TRUE, encoding = "UTF-8")
source("tpex_financial.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check(
  "cleaner version constant defined",
  exists("YNOW_STATEMENT_CLEANER_VERSION") &&
    is.finite(as.integer(YNOW_STATEMENT_CLEANER_VERSION)[1])
)

filled <- list(
  "Income Statement" = list(
    collapsed = data.frame(
      Breakdown = "Total Revenue", `12/31/2023` = "100", `12/31/2024` = "120",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    expanded = data.frame(
      Breakdown = "Total Revenue", `12/31/2023` = "100", `12/31/2024` = "120",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  ),
  "Balance Sheet" = list(
    collapsed = data.frame(
      Breakdown = "Total Assets", `12/31/2023` = "500", `12/31/2024` = "550",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    expanded = data.frame(
      Breakdown = "Total Assets", `12/31/2023` = "500", `12/31/2024` = "550",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  ),
  "Cash Flow" = list(
    collapsed = data.frame(
      Breakdown = "Free Cash Flow", `12/31/2023` = "40", `12/31/2024` = "45",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    expanded = data.frame(
      Breakdown = "Free Cash Flow", `12/31/2023` = "40", `12/31/2024` = "45",
      check.names = FALSE, stringsAsFactors = FALSE
    )
  ),
  `_meta` = list(currency = "USD", financialCurrency = "USD")
)

norm <- normalize_all_financials(filled)
check(
  "normalize stamps cleaner version",
  identical(
    as.integer(attr(norm, "statement_cleaner_version")[1]),
    as.integer(YNOW_STATEMENT_CLEANER_VERSION)[1]
  )
)
check(
  "normalize reorders FY newest-first",
  identical(names(norm[["Income Statement"]]$expanded)[-1], c("12/31/2024", "12/31/2023"))
)

# Persistent cache hit path: no integrity_original_checked on stored blob → stamped on read
store <- list()
ynow_market_cache_get <<- function(key, ttl) {
  store[[as.character(key)[1]]]
}
ynow_market_cache_put <<- function(key, tier, ticker, value) {
  store[[as.character(key)[1]]] <<- value
  invisible(TRUE)
}
ynow_market_cache_ttl <<- function(tier) 86400

# Stale hit: missing cleaner version → re-normalize + stamp integrity
stale <- filled
stale[["Income Statement"]]$expanded <- filled[["Income Statement"]]$expanded
attr(stale, "statement_cleaner_version") <- NULL
store[["stmt:AAPL"]] <- stale

tryCatch(memoise::forget(.cached_scrape_financials_memo), error = function(e) NULL)
.cached_scrape_financials_memo <<- memoise::memoise(
  .scrape_financials_uncached,
  cache = cachem::cache_mem(max_age = 3600)
)
.ensure_python_scraper <<- function() TRUE
scrape_all_financials <<- function(stock_code) stop("should not live-scrape on cache hit")

hit <- .scrape_financials_uncached("AAPL")
check(
  "cache hit stamps integrity_original_checked",
  isTRUE(attr(hit, "integrity_original_checked"))
)
check(
  "cache hit origin is persistent_cache",
  identical(attr(hit, "statement_cache_origin"), "persistent_cache")
)
check(
  "stale cache hit upgraded cleaner version",
  identical(
    as.integer(attr(hit, "statement_cleaner_version")[1]),
    as.integer(YNOW_STATEMENT_CLEANER_VERSION)[1]
  )
)
check(
  "stale hit re-ordered columns after re-normalize",
  identical(names(hit[["Income Statement"]]$expanded)[-1], c("12/31/2024", "12/31/2023"))
)
check(
  "upgraded payload written back to store",
  identical(
    as.integer(attr(store[["stmt:AAPL"]], "statement_cleaner_version")[1]),
    as.integer(YNOW_STATEMENT_CLEANER_VERSION)[1]
  )
)

# Soft-fill helper: ranking skip + gate
empty_yahoo <- list(
  "Income Statement" = list(expanded = data.frame(Breakdown = character(0))),
  "Balance Sheet" = list(expanded = data.frame(Breakdown = character(0))),
  "Cash Flow" = list(expanded = data.frame(Breakdown = character(0)))
)
tpex_calls <- 0L
apply_tpex_financial_fallback <<- function(yahoo_res, ticker) {
  tpex_calls <<- tpex_calls + 1L
  list(res = yahoo_res, used = TRUE, meta = list(board = "OTC"), error = NULL)
}

op_prev <- getOption("ynow.ranking_screen")
options(ynow.ranking_screen = TRUE)
tpex_calls <- 0L
fb_rank <- ynow_soft_fill_statements_post_fetch(empty_yahoo, "7772.TWO")
check("ranking_screen skips soft-fill TPEx", isFALSE(fb_rank$used) && identical(tpex_calls, 0L))

options(ynow.ranking_screen = FALSE)
tpex_calls <- 0L
fb_search <- ynow_soft_fill_statements_post_fetch(empty_yahoo, "7772.TWO")
check("non-ranking soft-fill invokes TPEx for .TWO", isTRUE(fb_search$used) && identical(tpex_calls, 1L))

tpex_calls <- 0L
fb_us <- ynow_soft_fill_statements_post_fetch(empty_yahoo, "AAPL")
check("US ticker soft-fill does not call TPEx", isFALSE(fb_us$used) && identical(tpex_calls, 0L))

tpex_calls <- 0L
fb_force <- ynow_soft_fill_statements_post_fetch(
  empty_yahoo, "7772.TWO", allow_tpex = TRUE
)
check("allow_tpex=TRUE forces TPEx even if ranking", {
  options(ynow.ranking_screen = TRUE)
  tpex_calls <<- 0L
  fb <- ynow_soft_fill_statements_post_fetch(empty_yahoo, "7772.TWO", allow_tpex = TRUE)
  isTRUE(fb$used) && identical(tpex_calls, 1L)
})
options(ynow.ranking_screen = op_prev)

# Call-site wiring: Search / Lab / BBLab / index / HTCDI / bluechip / HFV
src_files <- c(
  "ynow_server.R",
  "lab_im_quality.R",
  "lab_im_bluechip_eval.R",
  "business_breakdown_module.R",
  "ynow_index.R",
  "macro_market_module.R",
  "backtest_module.R"
)
for (f in src_files) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  check(
    paste0(f, " calls ynow_soft_fill_statements_post_fetch"),
    grepl("ynow_soft_fill_statements_post_fetch", txt, fixed = TRUE)
  )
}

# Fiscal cleaner remains Search-only for valuation frames (intentional)
srv <- paste(readLines("ynow_server.R", warn = FALSE), collapse = "\n")
check(
  "Search builds fiscal history frames after scrape",
  grepl("ynow_fiscal_history_frame", srv, fixed = TRUE)
)
lab_q <- paste(readLines("lab_im_quality.R", warn = FALSE), collapse = "\n")
check(
  "Lab quality intentionally skips fiscal history frame",
  !grepl("ynow_fiscal_history_frame", lab_q, fixed = TRUE)
)
bblab <- paste(readLines("business_breakdown_module.R", warn = FALSE), collapse = "\n")
check(
  "BBLab intentionally skips fiscal history frame",
  !grepl("ynow_fiscal_history_frame", bblab, fixed = TRUE)
)

# CapEx overlay keeps Yahoo sign on history frame; binding uses abs (intentional)
mi <- paste(readLines("model_integrity.R", warn = FALSE), collapse = "\n")
check(
  "integrity binding derives capex=abs(capital_expenditure)",
  grepl("capex=derived\\(id,'capital_expenditure',abs\\)", mi)
)
check(
  "fiscal frame overlays signed capital_expenditure label",
  grepl("capital_expenditure='Capital Expenditure'", mi, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All statement post-fetch cleaning checks passed.\n")
