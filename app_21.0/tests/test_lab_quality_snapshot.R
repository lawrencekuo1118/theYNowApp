# Quality snapshot: gates change the pool count; fresh rows are not recomputed.
# Run: cd app_21.0/tests && Rscript test_lab_quality_snapshot.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
Sys.setenv(YNOW_SLOW_CACHE_DIR = tempfile("ynow-quality-"))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
owd <- getwd()
setwd(app_dir)
on.exit(setwd(owd), add = TRUE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

now <- as.POSIXct("2026-10-11 10:00:00", tz = "UTC")
pool <- data.frame(
  ticker = c("AAA", "BBB", "CCC"),
  industry_key = c("semi", "soft", "bank"),
  stringsAsFactors = FALSE
)
compute <- function(ticker, industry_key = NULL) {
  ticker <- toupper(ticker)
  if (identical(ticker, "AAA")) {
    return(list(ticker = ticker, f_score = 8, quality_flag = 1, n_fs_alerts = 0L, ok = TRUE, error = NA_character_))
  }
  if (identical(ticker, "BBB")) {
    return(list(ticker = ticker, f_score = 4, quality_flag = 0, n_fs_alerts = 2L, ok = TRUE, error = NA_character_))
  }
  list(ticker = ticker, f_score = NA_real_, quality_flag = NA_real_, n_fs_alerts = NA_integer_, ok = FALSE, error = "no statements")
}

res <- lab_refresh_quality_snapshot(pool, market = "US", compute_fn = compute, now = now)
check("first refresh runs all three", identical(res$n_run, 3L) && identical(res$n_skip, 0L))
snap <- lab_load_quality_snapshot("US")
check("snapshot has AAA and BBB", all(c("AAA", "BBB") %in% snap$ticker))
check("CCC stored as not ok", isTRUE(!snap$ok[match("CCC", snap$ticker)]))

res2 <- lab_refresh_quality_snapshot(pool, market = "US", compute_fn = compute, now = now + 3600)
check("within 24h skips fresh ok rows", identical(res2$n_skip, 2L) && identical(res2$n_run, 1L))

gated <- lab_apply_quality_snapshot(
  pool$ticker, snap, eq_only = TRUE, gate_only = TRUE, no_alert = TRUE, now = now
)
check("all gates keep only AAA", identical(gated$n_show, 1L))
check("CCC counts as missing", identical(gated$n_missing, 1L))
check("covered is two", identical(gated$n_covered, 2L))

open <- lab_apply_quality_snapshot(
  pool$ticker, snap, eq_only = FALSE, gate_only = FALSE, no_alert = FALSE, now = now
)
check("gates off keeps structural pool", identical(open$n_show, 3L))

piot <- lab_apply_quality_snapshot(
  c(pool$ticker, "DDD"), snap, eq_only = FALSE, gate_only = TRUE, no_alert = FALSE, now = now
)
check("Piotroski gate drops BBB and unknowns", identical(piot$n_show, 1L))
check("unknown DDD is missing", identical(piot$n_missing, 2L))

old <- lab_apply_quality_snapshot(pool$ticker, snap, gate_only = TRUE, now = now + 3 * 86400)
check("snapshot older than 24h is stale", isTRUE(old$stale))

for (loc in c("en", "zh-TW")) {
  for (k in c(
    "btn_lab_im_update_quality", "lab_im_quality_snap_none", "lab_im_quality_snap_status",
    "lab_im_quality_snap_stale", "lab_im_quality_snap_missing", "lab_im_quality_done"
  )) {
    check(paste(loc, k), nzchar(ui_str(k, loc)))
  }
}
check("en status has placeholders", grepl("{when}", ui_str("lab_im_quality_snap_status", "en"), fixed = TRUE))
check("zh uses 檔", grepl("檔", ui_str("lab_im_quality_snap_status", "zh-TW"), fixed = TRUE))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("button mounted", grepl("lab_im_update_quality", ui, fixed = TRUE))
check("server refreshes snapshot", grepl("lab_refresh_quality_snapshot", srv, fixed = TRUE))
check("count reads snapshot", grepl("lab_apply_quality_snapshot", srv, fixed = TRUE))
check("search upserts snapshot", grepl("lab_upsert_quality_from_scores", srv, fixed = TRUE) ||
  grepl("lab_upsert_quality_rows", srv, fixed = TRUE))

if (fail > 0L) quit(status = 1L)
cat("All quality-snapshot checks passed.\n")
