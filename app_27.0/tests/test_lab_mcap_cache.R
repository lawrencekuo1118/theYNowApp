#!/usr/bin/env Rscript
# Market-cap gap cache: US/TW files, TTL, and Ranking busy vs ind-mcap.
# No live Yahoo. Run: cd app_27.0/tests && Rscript test_lab_mcap_cache.R

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
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

root <- tempfile("ynow_mcap_")
dir.create(root, recursive = TRUE)
fixture <- file.path(root, "universe_metrics_snapshot.csv")
utils::write.csv(
  data.frame(
    ticker = "SNAP1",
    market_cap = 50,
    ret_1y = 0,
    paid_in_capital = NA_real_,
    stringsAsFactors = FALSE
  ),
  file = fixture,
  row.names = FALSE
)
Sys.setenv(YNOW_CACHE_ROOT = root)
Sys.setenv(YNOW_MCAP_CACHE_TTL_H = "24")
options(ynow.universe_metrics_snapshot = normalizePath(fixture, winslash = "/"))

source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
rm(list = ls(envir = .lab_universe_metrics_cache), envir = .lab_universe_metrics_cache)

yahoo_n <- new.env(parent = emptyenv())
yahoo_n$calls <- 0L
yahoo_n$mode <- "first"
assign(".ensure_python_scraper", function() TRUE, envir = .GlobalEnv)
assign("get_market_caps_batch", function(tickers) {
  yahoo_n$calls <- yahoo_n$calls + 1L
  tks <- toupper(trimws(as.character(tickers)))
  # Mirror Python get_market_caps_batch: float hits and NULL misses in one dict.
  # NA_real_ alone does not reproduce the unlist() length-mismatch bug.
  # Assign by name with [[ — logical out[cond] <- list(x) can scramble names.
  out <- stats::setNames(vector("list", length(tks)), tks)
  put <- function(sym, val) {
    if (sym %in% names(out)) out[[sym]] <<- val
  }
  if (identical(yahoo_n$mode, "first")) {
    put("ZZZUS", 100)
    put("ZZZB", 300)
    put("ZZZC", 50)
    put("9999.TW", 222)
  } else if (identical(yahoo_n$mode, "refresh")) {
    put("ZZZUS", 333)
  } else if (identical(yahoo_n$mode, "partial")) {
    put("PARTA", 111)
    put("PARTC", 333)
  } else if (identical(yahoo_n$mode, "empty")) {
    # all NULL — genuine empty pass
  }
  out
}, envir = .GlobalEnv)

caps <- lab_fetch_market_caps_usd(c("ZZZUS", "9999.TW", "SNAP1"), market = "US")
check("snapshot still wins for SNAP1", isTRUE(abs(caps[["SNAP1"]] - 50) < 1e-6))
check("US gap filled from stub", isTRUE(abs(caps[["ZZZUS"]] - 100) < 1e-6))
check("TW gap filled from stub", isTRUE(abs(caps[["9999.TW"]] - 222) < 1e-6))
check("stub called for the cold gaps", yahoo_n$calls >= 1L)

us_path <- file.path(root, "lab_mcap_cache_US.rds")
tw_path <- file.path(root, "lab_mcap_cache_TW.rds")
check("US cache file exists", file.exists(us_path))
check("TW cache file exists", file.exists(tw_path))
us_saved <- readRDS(us_path)
tw_saved <- readRDS(tw_path)
check("US file is US", identical(as.character(us_saved$market), "US"))
check("TW file is TW", identical(as.character(tw_saved$market), "TW"))
check("US file has ZZZUS", "ZZZUS" %in% names(us_saved$caps))
check("US file excludes TW ticker", !"9999.TW" %in% names(us_saved$caps))
check("TW file has 9999.TW", "9999.TW" %in% names(tw_saved$caps))
check("TW file excludes US ticker", !"ZZZUS" %in% names(tw_saved$caps))
check("cache stores fetched_at", is.numeric(us_saved$at) && "ZZZUS" %in% names(us_saved$at))

# Warm process: memory hit, no extra Yahoo.
calls_after_cold <- yahoo_n$calls
warm <- lab_fetch_market_caps_usd(c("ZZZUS", "9999.TW"), market = "US")
check("warm memory keeps US cap", isTRUE(abs(warm[["ZZZUS"]] - 100) < 1e-6))
check("warm memory keeps TW cap", isTRUE(abs(warm[["9999.TW"]] - 222) < 1e-6))
check("warm memory does not call Yahoo", identical(yahoo_n$calls, calls_after_cold))

# New process memory, same disk: still no Yahoo.
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
disk_hit <- lab_fetch_market_caps_usd("ZZZUS", market = "US")
check("cold memory reads US disk", isTRUE(abs(disk_hit[["ZZZUS"]] - 100) < 1e-6))
check("US disk hit does not call Yahoo", identical(yahoo_n$calls, calls_after_cold))
tw_hit <- lab_fetch_market_caps_usd("9999.TW", market = "TW")
check("TW disk is separate", isTRUE(abs(tw_hit[["9999.TW"]] - 222) < 1e-6))
check("TW disk hit does not call Yahoo", identical(yahoo_n$calls, calls_after_cold))

# Stale US entry is refetched. TW timestamp stays inside TTL.
us_saved$at[["ZZZUS"]] <- as.numeric(Sys.time()) - (48 * 3600)
saveRDS(us_saved, us_path)
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
yahoo_n$mode <- "refresh"
stale <- lab_fetch_market_caps_usd(c("ZZZUS", "9999.TW"), market = "US")
check("stale US cap is refetched", isTRUE(abs(stale[["ZZZUS"]] - 333) < 1e-6))
check("fresh TW cap is not refetched", isTRUE(abs(stale[["9999.TW"]] - 222) < 1e-6))
check("stale refresh called Yahoo", yahoo_n$calls > calls_after_cold)

# Same inputs, same truncate order (ranking equivalence, not a new formula).
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
unlink(us_path)
unlink(tw_path)
yahoo_n$mode <- "first"
yahoo_n$calls <- 0L
pool <- data.frame(ticker = c("ZZZC", "ZZZUS", "ZZZB"), stringsAsFactors = FALSE)
pool1 <- lab_attach_market_caps(pool)
ranked1 <- lab_select_eval_pool(pool1, max_n = 2L, mode = "mcap")
calls_rank <- yahoo_n$calls
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
pool2 <- lab_attach_market_caps(pool)
ranked2 <- lab_select_eval_pool(pool2, max_n = 2L, mode = "mcap")
check(
  "mcap truncate order is stable",
  identical(as.character(ranked1$ticker), c("ZZZB", "ZZZUS")) &&
    identical(as.character(ranked1$ticker), as.character(ranked2$ticker))
)
check("second truncate reuses disk", identical(yahoo_n$calls, calls_rank))

ok1 <- .lab_mcap_lock_acquire("US")
ok2 <- .lab_mcap_lock_acquire("US")
check("mcap fill lock is exclusive", isTRUE(ok1) && !isTRUE(ok2))
.lab_mcap_lock_release("US")
ok3 <- .lab_mcap_lock_acquire("US")
check("mcap fill lock releases", isTRUE(ok3))
.lab_mcap_lock_release("US")
tw_lock <- .lab_mcap_lock_acquire("TW")
us_lock <- .lab_mcap_lock_acquire("US")
check("US and TW locks are independent", isTRUE(tw_lock) && isTRUE(us_lock))
.lab_mcap_lock_release("TW")
.lab_mcap_lock_release("US")

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
busy <- regmatches(
  srv,
  regexpr(
    "Ranking tab busy:[\\s\\S]{0,400}?observe\\(\\{[\\s\\S]{0,240}?\\}\\)",
    srv,
    perl = TRUE
  )
)
check("ranking busy block present", length(busy) == 1L && nzchar(busy))
check(
  "ranking tab busy is catalog plus eval",
  grepl("lab_im_catalog_busy()", busy, fixed = TRUE) &&
    grepl("lab_im_eval_busy()", busy, fixed = TRUE)
)
check(
  "ranking tab busy ignores ind-mcap",
  !grepl("lab_im_ind_mcap_busy", busy, fixed = TRUE)
)
check(
  "ind-mcap refresh keeps its own busy button",
  grepl("lab_im_refresh_ind_mcap", srv, fixed = TRUE) &&
    grepl("lab_im_ind_mcap_busy()", srv, fixed = TRUE)
)
check(
  "market-cap fill stays inside eval, not a second busy task",
  grepl("Market-cap gap fill stays inside the eval worker", srv, fixed = TRUE) &&
    !grepl("lab_im_prefetch_mcap_job", srv, fixed = TRUE)
)

# Partial Yahoo success: mixed float/NULL must keep hits and must not cool.
cool_us <- file.path(root, "lab_mcap_yahoo_cool_US.rds")
if (file.exists(cool_us)) unlink(cool_us)
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
yahoo_n$mode <- "partial"
yahoo_n$calls <- 0L
partial_tks <- c("PARTA", "PARTB", "PARTC", sprintf("NOCAP%02d", 1:40))
partial_out <- lab_fetch_market_caps_usd(partial_tks, market = "US")
check("partial Yahoo keeps float caps", isTRUE(abs(partial_out[["PARTA"]] - 111) < 1e-6))
check("partial Yahoo keeps second float", isTRUE(abs(partial_out[["PARTC"]] - 333) < 1e-6))
check("partial Yahoo leaves NULL as NA", !is.finite(partial_out[["PARTB"]]))
check("partial Yahoo does not cool the market", !file.exists(cool_us))
check("partial Yahoo wrote PARTA to US disk", {
  us_partial <- readRDS(us_path)
  "PARTA" %in% names(us_partial$caps) && "PARTC" %in% names(us_partial$caps)
})

# Worker save must merge with a larger shared disk cache, not replace it.
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
shared_at <- as.numeric(Sys.time())
saveRDS(
  list(
    version = 2L,
    market = "US",
    caps = c(SHAREDA = 1e9, SHAREDB = 2e9, SHAREDC = 3e9),
    at = c(SHAREDA = shared_at, SHAREDB = shared_at, SHAREDC = shared_at)
  ),
  us_path
)
.lab_mcap_disk_load("US", force = TRUE)
# Simulate TTL-cleared local Yahoo entries while .disk_loaded_US stays set.
us_mem <- ls(envir = .lab_mcap_cache, all.names = FALSE)
us_mem <- us_mem[startsWith(us_mem, "US|")]
if (length(us_mem)) rm(list = us_mem, envir = .lab_mcap_cache)
check(
  "disk_loaded stays set after mem clear",
  isTRUE(get0(".disk_loaded_US", envir = .lab_mcap_cache, inherits = FALSE))
)
.lab_mcap_mem_put("US", "WORKER1", 9e9, src = "yahoo")
ok_save <- .lab_mcap_disk_save("US")
merged <- readRDS(us_path)
check("worker save succeeds", isTRUE(ok_save))
check(
  "worker save keeps shared disk hits",
  all(c("SHAREDA", "SHAREDB", "SHAREDC") %in% names(merged$caps))
)
check("worker save adds its own hit", "WORKER1" %in% names(merged$caps))
check(
  "worker save does not shrink shared cache",
  length(merged$caps) >= 4L
)

yahoo_n$mode <- "empty"
if (file.exists(cool_us)) unlink(cool_us)
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
calls_before_empty <- yahoo_n$calls
empty_tks <- sprintf("NOCAP%02d", 1:50)
invisible(lab_fetch_market_caps_usd(empty_tks, market = "US"))
calls_after_empty <- yahoo_n$calls
invisible(lab_fetch_market_caps_usd(empty_tks, market = "US"))
check("empty Yahoo pass calls the stub once", calls_after_empty > calls_before_empty)
check(
  "empty Yahoo pass cools the US file",
  identical(yahoo_n$calls, calls_after_empty) &&
    file.exists(cool_us)
)
invisible(lab_fetch_market_caps_usd("8888.TW", market = "TW"))
check("TW is not cooled by a US miss", yahoo_n$calls > calls_after_empty)

# Direct apply helper: unlist-shaped mixed NULL must not discard hits.
rm(list = ls(envir = .lab_mcap_cache, all.names = TRUE), envir = .lab_mcap_cache)
apply_out <- stats::setNames(rep(NA_real_, 3L), c("MIXA", "MIXB", "MIXC"))
applied <- .lab_fetch_mcap_apply_yahoo(
  apply_out,
  "US",
  list(MIXA = 10, MIXB = NULL, MIXC = 30)
)
check("apply keeps MIXA with NULL neighbor", isTRUE(abs(applied$out[["MIXA"]] - 10) < 1e-6))
check("apply keeps MIXC with NULL neighbor", isTRUE(abs(applied$out[["MIXC"]] - 30) < 1e-6))
check("apply reports two hits for mixed NULL", identical(applied$n_hit, 2L))

unlink(root, recursive = TRUE)
cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
