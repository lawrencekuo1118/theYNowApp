#!/usr/bin/env Rscript
# Stock Prospecting Search path: catalog memo + honest search gates + QC bridge.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_lab_im_search_path.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
suppressPackageStartupMessages({
  source(file.path(app_dir, "global.R"), local = TRUE)
})

# --- Catalog memo (process-level): second build must reuse, not recompute ---
check(
  "catalog memo helpers exist",
  exists("lab_im_catalog_memo_key", mode = "function") &&
    exists("lab_im_catalog_memo_clear", mode = "function") &&
    exists("lab_get_industry_method_catalog", mode = "function") &&
    exists("lab_im_run_catalog_job", mode = "function")
)

lab_im_catalog_memo_clear()
fake <- data.frame(
  industry_key = "x",
  ticker = "AAA",
  stringsAsFactors = FALSE
)
key_us <- lab_im_catalog_memo_key("US", character(0))
lab_im_catalog_memo_set(key_us, fake)
hit <- lab_get_industry_method_catalog(
  market_mode = "US",
  boards = character(0),
  force = FALSE
)
check(
  "memo hit returns cached rows without rebuild",
  is.data.frame(hit) && identical(hit$ticker, "AAA")
)
lab_im_catalog_memo_clear()

# --- Search gate: classify hang/no-op states for the button path ---
check(
  "search gate helper exists",
  exists("ynow_lab_im_search_gate", mode = "function")
)
check(
  "gate: empty catalog → empty_catalog",
  identical(
    ynow_lab_im_search_gate(
      catlg = data.frame(),
      universe_busy = FALSE,
      catalog_busy = FALSE
    ),
    "empty_catalog"
  )
)
check(
  "gate: universe busy → wait_universe",
  identical(
    ynow_lab_im_search_gate(
      catlg = fake,
      universe_busy = TRUE,
      catalog_busy = FALSE
    ),
    "wait_universe"
  )
)
check(
  "gate: catalog busy → wait_catalog",
  identical(
    ynow_lab_im_search_gate(
      catlg = NULL,
      universe_busy = FALSE,
      catalog_busy = TRUE
    ),
    "wait_catalog"
  )
)
check(
  "gate: ready catalog → start",
  identical(
    ynow_lab_im_search_gate(
      catlg = fake,
      universe_busy = FALSE,
      catalog_busy = FALSE
    ),
    "start"
  )
)

# --- QC bridge must not wipe default methods when module reports none yet ---
check(
  "ynq_filters_to_lab_bridge exists",
  exists("ynq_filters_to_lab_bridge", mode = "function")
)
bridge_empty <- ynq_filters_to_lab_bridge(
  list(
    chain = "__ALL__",
    subindustry = "__ALL__",
    selected_models = character(0),
    earnings_quality = FALSE,
    piotroski = FALSE,
    no_warning = FALSE,
    include_adr = TRUE
  ),
  locale = "en"
)
defs <- as.character(APP_DEFAULTS$lab_im_methods %||% character(0))
check(
  "bridge empty models falls back to APP_DEFAULTS",
  length(bridge_empty$lab_im_methods) > 0L &&
    all(bridge_empty$lab_im_methods %in% defs) &&
    all(defs %in% bridge_empty$lab_im_methods)
)

# --- Server wiring: deferred catalog ensure + honest notifies; stats use cache ---
server_src <- paste(
  readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "server has deferred catalog ensure helper",
  grepl("\\.lab_im_ensure_catalog\\s*<-\\s*function", server_src, perl = TRUE)
)
check(
  "search pulse uses ynow_lab_im_search_gate",
  grepl("ynow_lab_im_search_gate\\(", server_src, perl = TRUE)
)
check(
  "pulse path notifies on empty/wait (no silent return-only)",
  grepl("lab_im_catalog_empty", server_src, fixed = TRUE) &&
    grepl("lab_im_catalog_preparing", server_src, fixed = TRUE)
)
check(
  "ynq stats read catalog cache reactiveVal (not live rebuild)",
  grepl("lab_im_catalog_df()", server_src, fixed = TRUE) &&
    grepl("universe_n = tryCatch\\(\\{[\\s\\S]*?lab_im_catalog_df\\(\\)", server_src, perl = TRUE)
)
check(
  "catalog ensure scheduled after bluechip mount flush",
  grepl(".lab_im_ensure_catalog", server_src, fixed = TRUE) &&
    grepl("lab_im_catalog_auto_armed", server_src, fixed = TRUE)
)
check(
  "server wires catalog ExtendedTask",
  grepl("lab_im_run_catalog_job", server_src, fixed = TRUE) &&
    grepl("lab_im_catalog_task", server_src, fixed = TRUE)
)
# Pure job returns a list shape the finish handler understands.
job_out <- lab_im_run_catalog_job(
  market_mode = "US",
  boards = character(0),
  force = FALSE,
  key = lab_im_catalog_memo_key("US", character(0))
)
check(
  "catalog job returns ok/catlg/key",
  is.list(job_out) &&
    isTRUE(job_out$ok) &&
    is.data.frame(job_out$catlg) &&
    nrow(job_out$catlg) > 0L &&
    nzchar(as.character(job_out$key %||% "")[1])
)

# Locale keys for honest empty/busy
loc_src <- paste(
  readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "en-US catalog preparing / empty keys",
  grepl("lab_im_catalog_preparing\\s*=", loc_src, perl = TRUE) &&
    grepl("lab_im_catalog_empty\\s*=", loc_src, perl = TRUE)
)
check(
  "zh-TW catalog preparing / empty keys",
  grepl("正在準備候選宇宙", loc_src, fixed = TRUE) &&
    grepl("候選宇宙為空", loc_src, fixed = TRUE)
)

cat("\nPassed: ", pass, "  Failed: ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
