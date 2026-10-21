# test_session_capacity.R — Starter-tier session / heavy-job helpers
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

root <- if (file.exists("../session_capacity.R")) {
  normalizePath("..")
} else if (file.exists("session_capacity.R")) {
  normalizePath(".")
} else {
  stop("Cannot locate session_capacity.R")
}

# Minimal %||% for helpers
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

# Hosted-safe detector stub used by session_capacity
.ynow_is_hosted_connect <- function() FALSE
.ynow_async_hosted_memory_safe <- function() FALSE

source(file.path(root, "session_capacity.R"), local = TRUE, encoding = "UTF-8")

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

# Force serialize mode for unit test
Sys.setenv(YNOW_HEAVY_JOB_SERIAL = "1")
check("serialize flag on", isTRUE(ynow_capacity_serialize_heavy_jobs()))
check("acquire first job", isTRUE(ynow_heavy_job_try_acquire("job_a")))
check("second job blocked", isFALSE(ynow_heavy_job_try_acquire("job_b")))
check("owner is job_a", identical(ynow_heavy_job_owner(), "job_a"))
check("wrong owner cannot release", {
  ynow_heavy_job_release("job_b")
  identical(ynow_heavy_job_owner(), "job_a")
})
check("release job_a", {
  ynow_heavy_job_release("job_a")
  !isTRUE(.ynow_heavy_job_state$busy)
})
check("acquire after release", isTRUE(ynow_heavy_job_try_acquire("job_c")))
ynow_heavy_job_release("job_c")

Sys.unsetenv("YNOW_HEAVY_JOB_SERIAL")
Sys.setenv(YNOW_HEAVY_JOB_PARALLEL = "1")
check("parallel override skips serialize", isFALSE(ynow_capacity_serialize_heavy_jobs()))
check("parallel allows dual acquire", {
  isTRUE(ynow_heavy_job_try_acquire("x")) && isTRUE(ynow_heavy_job_try_acquire("y"))
})
Sys.unsetenv("YNOW_HEAVY_JOB_PARALLEL")

# Hist cache: stub fetch
fetch_price_history_df <- function(ticker, period = "1y") {
  n <- 200L
  data.frame(
    Date = seq.Date(Sys.Date() - n + 1L, by = "day", length.out = n),
    Close = seq_len(n),
    Volume = seq_len(n),
    stringsAsFactors = FALSE
  )
}
.YNOW_CACHE_FAST_TTL_SEC <- 900L
ynow_hist_price_cache_clear()
a1 <- ynow_cached_hist_stock_trim("AAPL")
a2 <- ynow_cached_hist_stock_trim("AAPL")
check("hist trim returns data", is.data.frame(a1) && nrow(a1) > 30L)
check("hist trim shared cache hit", {
  identical(attr(a1, "fetched_at"), attr(a2, "fetched_at"))
})
check("hist trim <= 180d", {
  max(a1$Date) - min(a1$Date) <= 180
})

# Source wiring
glob <- paste(readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pa <- paste(readLines(file.path(root, "price_alert.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources session_capacity", grepl('source("session_capacity.R"', glob, fixed = TRUE))
check("server onSessionEnded cleanup", grepl("session$onSessionEnded", srv, fixed = TRUE))
check("server uses shared hist cache", grepl("ynow_cached_hist_stock_trim", srv, fixed = TRUE))
check("bluechip mounts deferred snap", grepl("lab_load_ind_mcap_chain_snapshot(mm)", srv, fixed = TRUE))
check("ind mcap snap starts empty", grepl("lab_ind_mcap_chain_snapshot_empty", srv, fixed = TRUE))
check("heavy job gate on Blue Chip", grepl('ynow_heavy_job_try_acquire("lab_im_eval")', srv, fixed = TRUE))
check("heavy job gate on Clustering", grepl('ynow_heavy_job_try_acquire("lab_cluster")', srv, fixed = TRUE))
check("heavy job gate on HFV", grepl('ynow_heavy_job_try_acquire("hfv_bt")', srv, fixed = TRUE))
check("price alert timer gated on armed", {
  grepl("armed <- isTRUE", pa, fixed = TRUE) || grepl("any(df$armed", pa, fixed = TRUE)
})

# i18n
suppressMessages({
  source(file.path(root, "ui_locale.R"), local = TRUE, encoding = "UTF-8")
})
check("capacity busy en", grepl("heavy job", ui_str("capacity_heavy_job_busy", "en"), ignore.case = TRUE))
check("capacity busy zh-TW", grepl("重型任務", ui_str("capacity_heavy_job_busy", "zh-TW"), fixed = TRUE))
check("no simplified in zh-TW busy", !grepl("服务器|已经|请稍", ui_str("capacity_heavy_job_busy", "zh-TW")))

cat("\nPassed: ", pass, " Failed: ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
invisible(TRUE)
