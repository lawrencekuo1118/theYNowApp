#!/usr/bin/env Rscript
# Acceptance — LIVE-UNCOVERED-3 / LIVE-UNCOVERED-4
# 3: multi-process shared Yahoo gate stamp + shared cache_disk under one root
# 4: fast quote refresh while slow statement cache stays warm
#
# Run: cd app_24.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_audit_live_cache_races.R

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

cache_root <- file.path(tempdir(), paste0("ynow_live_cache_", as.integer(Sys.time())))
gate_dir <- file.path(cache_root, "yahoo_gate")
dir.create(gate_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_YAHOO_GATE = "1",
  YNOW_YAHOO_MIN_INTERVAL_MS = "120",
  YNOW_YAHOO_GATE_DIR = gate_dir,
  YNOW_CACHE_ROOT = cache_root,
  YNOW_FORCE_SHARED_CACHE = "1"
)

suppressPackageStartupMessages({
  library(cachem)
  library(memoise)
})
if (!requireNamespace("callr", quietly = TRUE)) {
  stop("callr is required for multi-worker cache race evidence")
}
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
if (!exists(".ynow_log", mode = "function")) {
  .ynow_log <<- function(...) invisible(NULL)
}
if (!exists("normalize_all_financials", mode = "function")) {
  normalize_all_financials <<- function(x) x
}

source("web_crawler.R", local = FALSE)

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

info <- ynow_cache_tier_info()
check("shared cache root active", grepl(cache_root, info$cache_root, fixed = TRUE))
check("fast shared disk", isTRUE(info$fast_shared_disk))
check("slow dir under shared root", grepl(cache_root, info$slow_dir, fixed = TRUE))
check("gate dir under shared root", grepl(cache_root, info$yahoo_gate_dir, fixed = TRUE))

# =============================================================================
# LIVE-UNCOVERED-3 — concurrent workers share stamp + cache_disk
# =============================================================================
cat("== LIVE-UNCOVERED-3 multi-worker shared gate/cache ==\n")

n_workers <- 4L
acquires_each <- 5L
worker_script <- function(app_dir, cache_root, gate_dir, n_acq, worker_id) {
  Sys.setenv(
    YNOW_DEBUG_SKIP_PY = "1",
    YNOW_YAHOO_GATE = "1",
    YNOW_YAHOO_MIN_INTERVAL_MS = "120",
    YNOW_YAHOO_GATE_DIR = gate_dir,
    YNOW_CACHE_ROOT = cache_root,
    YNOW_FORCE_SHARED_CACHE = "1"
  )
  suppressPackageStartupMessages({
    library(cachem)
    library(memoise)
  })
  if (!exists("%||%", mode = "function")) {
    `%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
  }
  if (!exists(".ynow_log", mode = "function")) {
    .ynow_log <<- function(...) invisible(NULL)
  }
  if (!exists("normalize_all_financials", mode = "function")) {
    normalize_all_financials <<- function(x) x
  }
  setwd(app_dir)
  source("web_crawler.R", local = FALSE)

  times <- numeric(n_acq)
  ok <- logical(n_acq)
  for (i in seq_len(n_acq)) {
    t0 <- proc.time()[["elapsed"]]
    ok[i] <- isTRUE(.ynow_yahoo_gate_acquire(timeout_sec = 20))
    times[i] <- proc.time()[["elapsed"]] - t0
    # Shared slow cache write/read under contention
    # cachem keys: lowercase letters + digits only
    key <- paste0("w", worker_id, "k", i)
    .ynow_slow_cache$set(key, list(worker = worker_id, i = i, t = Sys.time()))
    got <- .ynow_slow_cache$get(key)
    if (!is.list(got) || !identical(as.integer(got$worker), as.integer(worker_id))) {
      ok[i] <- FALSE
    }
  }
  stamp_path <- file.path(gate_dir, "yahoo_gate.last")
  stamp <- tryCatch(readLines(stamp_path, n = 1L, warn = FALSE), error = function(e) NA_character_)
  list(
    worker = worker_id,
    ok = ok,
    times = times,
    stamp = suppressWarnings(as.numeric(stamp[1])),
    lock_exists = dir.exists(file.path(gate_dir, "yahoo_gate.lock.d"))
  )
}

jobs <- vector("list", n_workers)
for (w in seq_len(n_workers)) {
  jobs[[w]] <- callr::r_bg(
    worker_script,
    args = list(app_dir, cache_root, gate_dir, acquires_each, w),
    supervise = TRUE
  )
}
deadline <- Sys.time() + 90
while (Sys.time() < deadline && any(vapply(jobs, function(j) j$is_alive(), logical(1)))) {
  Sys.sleep(0.2)
}
results <- lapply(jobs, function(j) {
  if (isTRUE(j$is_alive())) {
    try(j$kill(), silent = TRUE)
    return(list(error = "timeout"))
  }
  tryCatch(j$get_result(), error = function(e) list(error = conditionMessage(e)))
})

alive_left <- sum(vapply(jobs, function(j) isTRUE(j$is_alive()), logical(1)))
check("all workers finished", identical(as.integer(alive_left), 0L))

errs <- vapply(results, function(r) !is.null(r$error), logical(1))
check("workers returned results", all(!errs))
if (any(errs)) {
  cat("INFO worker errors: ", paste(vapply(results[errs], function(r) r$error, ""), collapse = " | "), "\n", sep = "")
}

ok_mat <- lapply(results[!errs], function(r) r$ok)
check(
  "all gate acquires + cache writes ok",
  length(ok_mat) == n_workers && all(vapply(ok_mat, all, logical(1)))
)

stamps <- suppressWarnings(as.numeric(vapply(results[!errs], function(r) r$stamp %||% NA_real_, numeric(1))))
check("worker stamps finite", length(stamps) > 0 && all(is.finite(stamps)))

lock_left <- any(vapply(results[!errs], function(r) isTRUE(r$lock_exists), logical(1))) ||
  dir.exists(file.path(gate_dir, "yahoo_gate.lock.d"))
check("no stale gate lock left", isFALSE(lock_left))

final_stamp <- tryCatch(
  suppressWarnings(as.numeric(readLines(file.path(gate_dir, "yahoo_gate.last"), n = 1L, warn = FALSE)[1])),
  error = function(e) NA_real_
)
check("shared stamp file readable", is.finite(final_stamp))
cat("INFO final stamp=", final_stamp, " workers=", n_workers, " acquires_each=", acquires_each, "\n", sep = "")

# Cross-runtime: Python gate shares the same stamp under contention result
venv_py <- file.path(app_dir, ".ynow_venv", "bin", "python")
py <- if (file.exists(venv_py)) venv_py else Sys.which("python3")
if (nzchar(py) || file.exists(py)) {
  py_file <- file.path(cache_root, "gate_after_workers.py")
  writeLines(
    c(
      "import os, sys",
      sprintf("sys.path.insert(0, %s)", encodeString(app_dir, quote = "'")),
      "os.environ['YNOW_YAHOO_GATE'] = '1'",
      "os.environ['YNOW_YAHOO_MIN_INTERVAL_MS'] = '120'",
      sprintf("os.environ['YNOW_YAHOO_GATE_DIR'] = %s", encodeString(gate_dir, quote = "'")),
      "from deep_scraper import _yahoo_gate_acquire",
      "_yahoo_gate_acquire(timeout_sec=10)",
      "stamp = open(os.path.join(os.environ['YNOW_YAHOO_GATE_DIR'], 'yahoo_gate.last'), encoding='utf-8').read().strip()",
      "print(stamp)"
    ),
    py_file
  )
  out <- system2(py, args = py_file, stdout = TRUE, stderr = TRUE)
  py_stamp <- suppressWarnings(as.numeric(trimws(out[length(out)])))
  check(
    "Python gate advances shared stamp after R workers",
    is.finite(py_stamp) && is.finite(final_stamp) && py_stamp >= final_stamp
  )
} else {
  check("Python available for cross-runtime stamp", FALSE)
}

# =============================================================================
# LIVE-UNCOVERED-4 — quote fast tier refreshes; statements stay on slow tier
# =============================================================================
cat("== LIVE-UNCOVERED-4 quote-fresh vs statement-warm ==\n")

check(
  "TTL split 15m vs 24h",
  identical(as.integer(info$fast_ttl_sec), 900L) &&
    identical(as.integer(info$slow_ttl_sec), 86400L)
)

stmt_calls <- 0L
quote_calls <- 0L

.scrape_financials_uncached <<- function(stock_code) {
  stmt_calls <<- stmt_calls + 1L
  list(
    "Income Statement" = list(
      expanded = data.frame(Breakdown = "Total Revenue", `12/31/2024` = 100 + stmt_calls,
                            check.names = FALSE, stringsAsFactors = FALSE)
    ),
    "Balance Sheet" = list(
      expanded = data.frame(Breakdown = "Total Assets", `12/31/2024` = 200 + stmt_calls,
                            check.names = FALSE, stringsAsFactors = FALSE)
    ),
    "Cash Flow" = list(
      expanded = data.frame(Breakdown = "Operating Cash Flow", `12/31/2024` = 50 + stmt_calls,
                            check.names = FALSE, stringsAsFactors = FALSE)
    ),
    `_meta` = list(currency = "USD", financialCurrency = "USD")
  )
}
.cached_scrape_financials_memo <<- memoise::memoise(
  .scrape_financials_uncached,
  cache = .ynow_slow_cache
)

# Minimal quote fetcher on fast cache (mirrors summary tier wiring)
.fetch_quote_uncached <<- function(stock_code) {
  quote_calls <<- quote_calls + 1L
  data.frame(
    Item = c("Price", "gen"),
    Value = c(as.character(1000 + quote_calls), as.character(quote_calls)),
    stringsAsFactors = FALSE
  )
}
.cached_quote_memo <<- memoise::memoise(.fetch_quote_uncached, cache = .ynow_fast_cache)

tk <- "AAPL"
res1 <- .cached_scrape_financials_memo(tk)
q1 <- .cached_quote_memo(tk)
check("initial statement fetch once", identical(as.integer(stmt_calls), 1L))
check("initial quote fetch once", identical(as.integer(quote_calls), 1L))

# Refresh quote only (fast tier drop) — statements must stay warm on slow cache
tryCatch(memoise::drop_cache(.cached_quote_memo)(tk), error = function(e) NULL)
q2 <- .cached_quote_memo(tk)
res2 <- .cached_scrape_financials_memo(tk)

check("quote refreshed (2 fetches)", identical(as.integer(quote_calls), 2L))
check(
  "statement still warm (still 1 scrape)",
  identical(as.integer(stmt_calls), 1L)
)
check(
  "statement payload unchanged across quote refresh",
  identical(res1, res2)
)
check(
  "quote value changed after refresh",
  !identical(as.character(q1$Value[1]), as.character(q2$Value[1]))
)

# Opposite direction honesty: dropping slow statement cache does not reset quote memo
tryCatch(memoise::drop_cache(.cached_scrape_financials_memo)(tk), error = function(e) NULL)
res3 <- .cached_scrape_financials_memo(tk)
q3 <- .cached_quote_memo(tk)
check("statement re-fetched after slow drop", identical(as.integer(stmt_calls), 2L))
check("quote still warm after slow drop", identical(as.integer(quote_calls), 2L))
check(
  "quote unchanged when only statements dropped",
  identical(as.character(q2$Value[1]), as.character(q3$Value[1]))
)
check(
  "new statement payload after slow drop",
  !identical(res2, res3)
)

cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
quit(status = 0L)
