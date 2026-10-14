#!/usr/bin/env Rscript
# Acceptance — live Yahoo / TPEx financial-statement HTTP matrix
# Covers Phase 2 LIVE-UNCOVERED-1 (Yahoo) and LIVE-UNCOVERED-2 (TPEx XLS).
#
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=0 Rscript tests/test_audit_live_statements.R
# Env:  YNOW_ACCEPTANCE_REQUIRE_LIVE_STATEMENTS=1 (default) fail if matrix incomplete
#       RETICULATE_PYTHON — prefer app_24.0/.ynow_venv when unset

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("web_crawler.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "0")

venv_py <- file.path(app_dir, ".ynow_venv", "bin", "python")
if (!nzchar(Sys.getenv("RETICULATE_PYTHON")) && file.exists(venv_py)) {
  Sys.setenv(RETICULATE_PYTHON = venv_py)
}
if (nzchar(Sys.getenv("RETICULATE_PYTHON")) && requireNamespace("reticulate", quietly = TRUE)) {
  tryCatch(
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE),
    error = function(e) NULL
  )
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

source("setup.R", local = FALSE)
source("web_crawler.R", local = FALSE)
source("tpex_financial.R", local = FALSE)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

.stmt_dims <- function(res, key) {
  d <- tryCatch(
    coerce_financial_df(res[[key]]$expanded),
    error = function(e) NULL
  )
  if (is.null(d) || !is.data.frame(d)) return(c(NA_integer_, NA_integer_))
  as.integer(dim(d))
}

.stmt_nonempty <- function(res, key) {
  d <- tryCatch(
    coerce_financial_df(res[[key]]$expanded),
    error = function(e) NULL
  )
  is.data.frame(d) && isFALSE(financial_df_is_empty(d))
}

.fetch_yahoo_triple <- function(ticker, attempts = 3L) {
  attempts <- max(1L, as.integer(attempts)[1])
  last <- NULL
  for (i in seq_len(attempts)) {
    tryCatch(memoise::forget(.cached_scrape_financials_memo), error = function(e) NULL)
    .drop_financials_cache_key(ticker)
    res <- tryCatch(cached_scrape_financials(ticker), error = function(e) e)
    last <- res
    if (!inherits(res, "error") && isFALSE(financials_is_bs_cf_all_empty(res))) {
      if (i > 1L) {
        cat("INFO ", ticker, " recovered on attempt ", i, "\n", sep = "")
      }
      return(res)
    }
    cat(
      "INFO ", ticker, " attempt ", i, "/", attempts,
      if (inherits(res, "error")) {
        paste0(" err=", conditionMessage(res))
      } else {
        " empty"
      },
      "\n",
      sep = ""
    )
    if (i < attempts) Sys.sleep(2 * i)
  }
  last
}

.check_yahoo_triple <- function(ticker, require_cf = TRUE) {
  res <- .fetch_yahoo_triple(ticker, attempts = 3L)
  check(paste0(ticker, " scrape ok"), !inherits(res, "error"))
  if (inherits(res, "error")) {
    cat("INFO ", ticker, " err=", conditionMessage(res), "\n", sep = "")
    return(invisible(NULL))
  }
  check(paste0(ticker, " not all-empty"), isFALSE(financials_is_bs_cf_all_empty(res)))
  check(paste0(ticker, " IS non-empty"), .stmt_nonempty(res, "Income Statement"))
  check(paste0(ticker, " BS non-empty"), .stmt_nonempty(res, "Balance Sheet"))
  if (isTRUE(require_cf)) {
    check(paste0(ticker, " CF non-empty"), .stmt_nonempty(res, "Cash Flow"))
  }
  for (nm in c("Income Statement", "Balance Sheet", "Cash Flow")) {
    dm <- .stmt_dims(res, nm)
    cat(
      "INFO ", ticker, " ", nm, " dim=", dm[1], "x", dm[2], "\n",
      sep = ""
    )
  }
  invisible(res)
}

check(
  "python scraper ready",
  isTRUE(.ensure_python_scraper()) && exists("scrape_all_financials", mode = "function")
)

# LIVE-UNCOVERED-1 — Yahoo HTTP statements (US + TW listed)
cat("== LIVE-UNCOVERED-1 Yahoo statements ==\n")
.check_yahoo_triple("AAPL", require_cf = TRUE)
.check_yahoo_triple("2330.TW", require_cf = TRUE)
# OTC quote may already have Yahoo statements; still prove HTTP path
.check_yahoo_triple("3363.TWO", require_cf = TRUE)

# LIVE-UNCOVERED-2 — TPEx official XLS summary (no invented CF)
cat("== LIVE-UNCOVERED-2 TPEx financial summary ==\n")
check(
  "TPEx python module ready",
  isTRUE(.ensure_tpex_financial_py()) &&
    exists("scrape_tpex_financials_fallback", envir = .tpex_fs_env, inherits = FALSE)
)

tpex_ticker <- "3363.TWO"
raw <- tryCatch(.fetch_tpex_financial_raw(tpex_ticker, board = "OTC"), error = function(e) e)
check(paste0("TPEx raw ", tpex_ticker, " ok object"), !inherits(raw, "error"))
if (!inherits(raw, "error")) {
  meta <- tryCatch(as.list(raw[["_meta"]]), error = function(e) list())
  check(paste0("TPEx meta ok ", tpex_ticker), isTRUE(meta$ok %||% FALSE))
  cat(
    "INFO TPEx board=", as.character(meta$board %||% "?")[1],
    " source=", as.character(meta$source %||% "?")[1], "\n",
    sep = ""
  )
  tpex_norm <- tryCatch(normalize_all_financials(raw), error = function(e) e)
  check("TPEx normalize ok", !inherits(tpex_norm, "error"))
  if (!inherits(tpex_norm, "error")) {
    check("TPEx IS non-empty", .stmt_nonempty(tpex_norm, "Income Statement"))
    check("TPEx BS non-empty", .stmt_nonempty(tpex_norm, "Balance Sheet"))
    # Honest contract: summary does not invent CapEx / FCF
    check(
      "TPEx CF empty (no invent)",
      isTRUE(.tpex_stmt_empty(tpex_norm, "Cash Flow"))
    )
    for (nm in c("Income Statement", "Balance Sheet", "Cash Flow")) {
      dm <- .stmt_dims(tpex_norm, nm)
      cat("INFO TPEx ", nm, " dim=", dm[1], "x", dm[2], "\n", sep = "")
    }
  }
}

# Merge policy: empty Yahoo + TPEx fills IS/BS only
empty_yahoo <- list(
  "Income Statement" = list(expanded = data.frame()),
  "Balance Sheet" = list(expanded = data.frame()),
  "Cash Flow" = list(expanded = data.frame())
)
fb <- tryCatch(apply_tpex_financial_fallback(empty_yahoo, tpex_ticker), error = function(e) e)
check("TPEx fallback apply ok", !inherits(fb, "error") && is.list(fb))
if (!inherits(fb, "error") && is.list(fb)) {
  check("TPEx fallback used", isTRUE(fb$used))
  check(
    "TPEx fallback filled IS",
    .stmt_nonempty(fb$res, "Income Statement")
  )
  check(
    "TPEx fallback filled BS",
    .stmt_nonempty(fb$res, "Balance Sheet")
  )
  check(
    "TPEx fallback did not invent CF",
    isTRUE(.tpex_stmt_empty(fb$res, "Cash Flow"))
  )
}

require_live <- !identical(Sys.getenv("YNOW_ACCEPTANCE_REQUIRE_LIVE_STATEMENTS", "1"), "0")
cat(
  "PASS=", pass, " FAIL=", fail,
  " require_live=", as.character(require_live), "\n",
  sep = ""
)
if (isTRUE(require_live) && fail > 0L) {
  quit(status = 1L)
}
if (!isTRUE(require_live) && fail > 0L) {
  cat("NOTE: failures present but YNOW_ACCEPTANCE_REQUIRE_LIVE_STATEMENTS=0\n")
}
quit(status = 0L)
