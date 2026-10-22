#!/usr/bin/env Rscript
# Offline: peer-beta batch uses chunked Yahoo quote + quoteSummary gap-fill;
# cluster feature JSON lives under YNOW_CACHE_ROOT.
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_beta_unlever_batch_offline.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_26.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("deep_scraper.py")
setwd(root)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

src <- paste(readLines("deep_scraper.py", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "batch fn defined",
  grepl("def get_beta_unlever_inputs_batch", src, fixed = TRUE)
)
check(
  "batch uses chunked quote fields=beta",
  grepl('fields = "symbol,shortName,longName,displayName,marketCap,beta"', src, fixed = TRUE) ||
    grepl("fields=.*marketCap,beta", src)
)
check(
  "batch gap-fills via quoteSummary",
  grepl("def _beta_unlever_from_quote_summary", src, fixed = TRUE) &&
    grepl("_beta_unlever_from_quote_summary", src, fixed = TRUE)
)
check(
  "batch falls back to per-name get_beta_unlever_inputs",
  grepl(
    "beta-unlever serial fallback[\\s\\S]*?get_beta_unlever_inputs\\(sym\\)",
    src,
    perl = TRUE
  )
)
check(
  "batch is not serial-only wrapper",
  !grepl(
    "def get_beta_unlever_inputs_batch\\([\\s\\S]*?return \\[get_beta_unlever_inputs\\(t\\) for t in cleaned\\]\\s*$",
    src,
    perl = TRUE
  )
)

check(
  "cluster cache under _yahoo_cache_root",
  grepl(
    'Path\\(_yahoo_cache_root\\(\\)\\)\\s*/\\s*"cluster_feat_cache"',
    src,
    perl = TRUE
  ) || grepl(
    '_yahoo_cache_root()\\) / "cluster_feat_cache"',
    src,
    fixed = TRUE
  )
)
check(
  "cluster cache not tempfile-only",
  !grepl(
    'Path\\(tempfile\\.gettempdir\\(\\)\\)\\s*/\\s*"ynow_cluster_feat_cache"',
    src,
    perl = TRUE
  )
)

wc <- paste(readLines("web_crawler.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "R fetch_beta_unlever_inputs_batch wires Python batch",
  grepl("fetch_beta_unlever_inputs_batch", wc, fixed = TRUE) &&
    grepl("get_beta_unlever_inputs_batch", wc, fixed = TRUE)
)

# Pure helper checks via Python (no network)
py <- Sys.which("python3")
if (!nzchar(py)) py <- Sys.which("python")
if (nzchar(py)) {
  cache_tmp <- file.path(tempdir(), paste0("ynow_beta_cache_", as.integer(Sys.time())))
  dir.create(cache_tmp, recursive = TRUE, showWarnings = FALSE)
  py_file <- file.path(tempdir(), paste0("ynow_beta_helper_", as.integer(Sys.time()), ".py"))
  writeLines(
    c(
      "import os, sys",
      sprintf("os.environ['YNOW_CACHE_ROOT'] = %s", encodeString(cache_tmp, quote = "'")),
      sprintf("sys.path.insert(0, %s)", encodeString(root, quote = "'")),
      "from deep_scraper import (",
      "  _beta_unlever_apply_de, _beta_unlever_empty, _yahoo_cache_root,",
      ")",
      "from pathlib import Path",
      "root = _yahoo_cache_root()",
      "assert root == os.environ['YNOW_CACHE_ROOT'], root",
      "cluster = Path(root) / 'cluster_feat_cache'",
      "cluster.mkdir(parents=True, exist_ok=True)",
      "assert cluster.is_dir()",
      "row = _beta_unlever_empty('X')",
      "row['beta'] = 1.2",
      "row['market_cap'] = 100.0",
      "row['total_debt'] = 25.0",
      "_beta_unlever_apply_de(row)",
      "assert row['ok'] and abs(row['de_ratio'] - 0.25) < 1e-9",
      "assert row['de_source'] == 'market_D/E'",
      "row2 = _beta_unlever_empty('Y')",
      "row2['beta'] = 0.9",
      "_beta_unlever_apply_de(row2, de_raw=54.2)",
      "assert row2['ok'] and abs(row2['de_ratio'] - 0.542) < 1e-9",
      "print('PY_OK')"
    ),
    py_file
  )
  out <- tryCatch(
    system2(py, args = py_file, stdout = TRUE, stderr = TRUE),
    error = function(e) paste("error:", conditionMessage(e))
  )
  check("python helpers + cache root", any(grepl("PY_OK", out, fixed = TRUE)))
  if (!any(grepl("PY_OK", out, fixed = TRUE))) {
    cat(paste(out, collapse = "\n"), "\n")
  }
  unlink(py_file)
} else {
  cat("SKIP python helper checks (no python3)\n")
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
