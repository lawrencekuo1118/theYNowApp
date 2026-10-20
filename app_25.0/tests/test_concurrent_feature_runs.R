# test_concurrent_feature_runs.R — ExtendedTask / async concurrency wiring
pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

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
    file.path(wd, "app_25.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("ynow_server.R")
suppressMessages({
  source(file.path(root, "ui_locale.R"), local = TRUE, encoding = "UTF-8")
})
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glob <- paste(readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
async <- paste(readLines(file.path(root, "async_jobs.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
lab <- paste(readLines(file.path(root, "lab_industry_method.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
clu <- paste(readLines(file.path(root, "lab_clustering.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("async_jobs.R present", grepl(".ynow_make_extended_task", async, fixed = TRUE))
check("global sources async_jobs", grepl('source("async_jobs.R"', glob, fixed = TRUE))
check("global loads future+promises",
      grepl("\\bfuture\\b", glob) && grepl("\\bpromises\\b", glob))
check("lab_im_run_eval_job defined", grepl("lab_im_run_eval_job\\s*<-\\s*function", lab))
check("lab_cluster_run_job defined", grepl("lab_cluster_run_job\\s*<-\\s*function", clu))
check("server creates Blue Chip ExtendedTask",
      grepl("lab_im_bc_task", srv, fixed = TRUE) &&
        grepl(".ynow_make_extended_task(fn)", srv, fixed = TRUE) &&
        grepl('"lab_im_run_eval_job"', srv, fixed = TRUE))
check("server creates Cluster ExtendedTask",
      grepl("lab_cluster_task", srv, fixed = TRUE) &&
        grepl('"lab_cluster_run_job"', srv, fixed = TRUE) &&
        grepl("lab_cluster_task$invoke", srv, fixed = TRUE))
check("separate universe busy flag", grepl("lab_im_universe_busy", srv, fixed = TRUE))
check("separate cluster busy flag", grepl("lab_cluster_busy", srv, fixed = TRUE))
check("Blue Chip invoke uses ExtendedTask",
      grepl("lab_im_bc_task$invoke", srv, fixed = TRUE))
check("Cluster invoke uses ExtendedTask",
      grepl("lab_cluster_task$invoke", srv, fixed = TRUE))
# Universe refresh must not share Blue Chip busy exclusive of cluster
check("universe busy exclusive with eval only",
      grepl("lab_im_eval_busy())) || isTRUE(isolate(lab_im_universe_busy()", srv, fixed = TRUE) ||
        grepl("lab_im_eval_busy()) || isTRUE(isolate(lab_im_universe_busy()", srv, fixed = TRUE))
check("quality busy copy mentions parallel Clustering/HFV (en)",
      grepl("Clustering and HFV can still run in parallel",
            ui_str("lab_im_quality_busy", "en"), fixed = TRUE))
check("quality busy copy mentions 分群/HFV (zh-TW)",
      grepl("分群分析與 HFV 仍可同時運算",
            ui_str("lab_im_quality_busy", "zh-TW"), fixed = TRUE))
check("cluster busy i18n en",
      grepl("Clustering already running", ui_str("lab_cluster_busy", "en"), fixed = TRUE))
check("cluster busy i18n zh-TW",
      grepl("分群分析進行中", ui_str("lab_cluster_busy", "zh-TW"), fixed = TRUE))
check("no simplified Chinese in new zh-TW busy copy",
      !grepl("进行|已经|数据", ui_str("lab_im_quality_busy", "zh-TW")))

# Runtime: two future_promises overlap in wall time
async_ok <- FALSE
elapsed <- NA_real_
async_err <- NULL
tryCatch({
  .libPaths(c("/workspace/.R/library", .libPaths()))
  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("promises", quietly = TRUE) &&
      requireNamespace("later", quietly = TRUE)) {
    source(file.path(root, "async_jobs.R"), local = TRUE, encoding = "UTF-8")
    stopifnot(isTRUE(.ynow_async_init()))
    results <- new.env(parent = emptyenv())
    t0 <- Sys.time()
    p1 <- promises::future_promise({ Sys.sleep(0.55); "A" }, seed = TRUE)
    p2 <- promises::future_promise({ Sys.sleep(0.55); "B" }, seed = TRUE)
    promises::then(p1, onFulfilled = function(v) results$a <- v)
    promises::then(p2, onFulfilled = function(v) results$b <- v)
    for (i in seq_len(80L)) {
      later::run_now(timeoutSecs = 0.05)
      if (!is.null(results$a) && !is.null(results$b)) break
    }
    elapsed <<- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    async_ok <<- !is.null(results$a) && !is.null(results$b) &&
      is.finite(elapsed) && elapsed < 1.05
  } else {
    async_err <<- "future/promises/later not installed"
  }
}, error = function(e) {
  async_err <<- conditionMessage(e)
})
check(
  sprintf(
    "future_promise concurrency (elapsed=%s%s)",
    if (is.finite(elapsed)) sprintf("%.2fs", elapsed) else "NA",
    if (!is.null(async_err)) paste0("; err=", async_err) else ""
  ),
  async_ok
)

cat("\nPassed:", pass, " Failed:", fail, "\n")
if (fail > 0L) quit(status = 1L)
