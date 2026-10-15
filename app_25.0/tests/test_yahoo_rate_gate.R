#!/usr/bin/env Rscript
# Yahoo cross-process rate gate + shared cache helpers (no network).
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_yahoo_rate_gate.R

root <- if (file.exists("web_crawler.R")) {
  getwd()
} else if (file.exists("../web_crawler.R")) {
  normalizePath("..")
} else if (dir.exists("app_21.0") && file.exists("app_21.0/web_crawler.R")) {
  normalizePath("app_21.0")
} else {
  stop("Cannot locate app_21.0 / web_crawler.R")
}
setwd(root)

gate_tmp <- file.path(tempdir(), paste0("ynow_gate_test_", as.integer(Sys.time())))
cache_tmp <- file.path(tempdir(), paste0("ynow_cache_test_", as.integer(Sys.time())))
dir.create(gate_tmp, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_tmp, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_YAHOO_GATE = "1",
  YNOW_YAHOO_MIN_INTERVAL_MS = "200",
  YNOW_YAHOO_GATE_DIR = gate_tmp,
  YNOW_CACHE_ROOT = cache_tmp,
  YNOW_FORCE_SHARED_CACHE = "1"
)

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
if (!exists("normalize_all_financials", mode = "function")) {
  normalize_all_financials <<- function(x) x
}

source("web_crawler.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

info <- ynow_cache_tier_info()
check("shared fast disk enabled", isTRUE(info$fast_shared_disk))
check("fast cache is cache_disk under shared root", grepl("cache_disk", info$fast_class, fixed = TRUE))
check("slow dir under cache root", grepl(cache_tmp, info$slow_dir, fixed = TRUE))
check("yahoo gate enabled", isTRUE(info$yahoo_gate_enabled))
check("yahoo min interval 200ms", identical(as.integer(info$yahoo_min_interval_ms), 200L))
check("gate dir override honored", identical(normalizePath(info$yahoo_gate_dir, mustWork = FALSE),
                                            normalizePath(gate_tmp, mustWork = FALSE)))

t0 <- proc.time()[["elapsed"]]
.ynow_yahoo_gate_acquire(timeout_sec = 5)
.ynow_yahoo_gate_acquire(timeout_sec = 5)
elapsed <- proc.time()[["elapsed"]] - t0
check("two acquires spaced by min interval", elapsed >= 0.15)

Sys.setenv(YNOW_YAHOO_GATE = "0")
t1 <- proc.time()[["elapsed"]]
.ynow_yahoo_gate_acquire(timeout_sec = 5)
.ynow_yahoo_gate_acquire(timeout_sec = 5)
elapsed_off <- proc.time()[["elapsed"]] - t1
check("gate disable skips sleep", elapsed_off < 0.12)
Sys.setenv(YNOW_YAHOO_GATE = "1")

# Python gate shares the same stamp file (no Yahoo network).
py <- Sys.which("python3")
if (!nzchar(py)) py <- Sys.which("python")
venv_py <- file.path(root, ".ynow_venv", "bin", "python")
if (file.exists(venv_py)) py <- venv_py
check("python available for gate parity", nzchar(py) || file.exists(py))
if (nzchar(py) || file.exists(py)) {
  gate_lit <- gsub("\\\\", "/", normalizePath(gate_tmp, winslash = "/", mustWork = FALSE))
  root_lit <- gsub("\\\\", "/", normalizePath(root, winslash = "/", mustWork = FALSE))
  py_file <- file.path(gate_tmp, "gate_spacing_test.py")
  writeLines(
    c(
      "import os, sys, time",
      sprintf("sys.path.insert(0, %s)", encodeString(root_lit, quote = "'")),
      "os.environ['YNOW_YAHOO_GATE'] = '1'",
      "os.environ['YNOW_YAHOO_MIN_INTERVAL_MS'] = '200'",
      sprintf("os.environ['YNOW_YAHOO_GATE_DIR'] = %s", encodeString(gate_lit, quote = "'")),
      "from deep_scraper import _yahoo_gate_acquire",
      "t0 = time.time()",
      "_yahoo_gate_acquire(timeout_sec=5)",
      "_yahoo_gate_acquire(timeout_sec=5)",
      "print(f'{time.time() - t0:.3f}')"
    ),
    py_file
  )
  out <- tryCatch(
    system2(py, args = py_file, stdout = TRUE, stderr = TRUE),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    check("python gate script runs", FALSE)
  } else {
    status <- attr(out, "status")
    if (!is.null(status) && !identical(as.integer(status), 0L)) {
      cat("python stderr/out:\n", paste(out, collapse = "\n"), "\n", sep = "")
      check("python gate script exit 0", FALSE)
    } else {
      nums <- suppressWarnings(as.numeric(out))
      nums <- nums[is.finite(nums)]
      py_elapsed <- if (length(nums)) nums[length(nums)] else NA_real_
      check("python gate spaces two acquires", is.finite(py_elapsed) && py_elapsed >= 0.15)
      # Cross-runtime: R + Python share the same stamp path (import time makes
      # wall-clock spacing flaky, so assert stamp rewrite instead).
      Sys.setenv(YNOW_YAHOO_GATE = "1", YNOW_YAHOO_MIN_INTERVAL_MS = "200")
      stamp_path <- file.path(gate_tmp, "yahoo_gate.last")
      .ynow_yahoo_gate_acquire(timeout_sec = 5)
      stamp_r <- tryCatch(readLines(stamp_path, n = 1L, warn = FALSE), error = function(e) "")
      py_file2 <- file.path(gate_tmp, "gate_cross_test.py")
      writeLines(
        c(
          "import os, sys",
          sprintf("sys.path.insert(0, %s)", encodeString(root_lit, quote = "'")),
          "os.environ['YNOW_YAHOO_GATE'] = '1'",
          "os.environ['YNOW_YAHOO_MIN_INTERVAL_MS'] = '200'",
          sprintf("os.environ['YNOW_YAHOO_GATE_DIR'] = %s", encodeString(gate_lit, quote = "'")),
          "from deep_scraper import _yahoo_gate_acquire, _yahoo_gate_dir",
          "assert _yahoo_gate_dir() == os.environ['YNOW_YAHOO_GATE_DIR']",
          "_yahoo_gate_acquire(timeout_sec=5)",
          "stamp = os.path.join(os.environ['YNOW_YAHOO_GATE_DIR'], 'yahoo_gate.last')",
          "print(open(stamp, encoding='utf-8').read().strip())"
        ),
        py_file2
      )
      out2 <- system2(py, args = py_file2, stdout = TRUE, stderr = TRUE)
      stamp_py <- trimws(as.character(out2[length(out2)]))
      stamp_r_n <- suppressWarnings(as.numeric(stamp_r[1]))
      stamp_py_n <- suppressWarnings(as.numeric(stamp_py[1]))
      check(
        "R then Python share stamp file",
        is.finite(stamp_r_n) && is.finite(stamp_py_n) && stamp_py_n >= stamp_r_n
      )
    }
  }
}

src <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("python defines _yahoo_gate_acquire", grepl("def _yahoo_gate_acquire", src, fixed = TRUE))
check(
  "summary quote acquires gate",
  grepl("def get_summary_quote[\\s\\S]*?_yahoo_gate_acquire\\(", src, perl = TRUE)
)
check(
  "financials acquire gate",
  grepl("def scrape_all_financials_yf[\\s\\S]*?_yahoo_gate_acquire\\(", src, perl = TRUE)
)

lab <- paste(readLines("lab_clustering.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "lab R path calls yahoo gate",
  grepl(".ynow_yahoo_gate_acquire", lab, fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
