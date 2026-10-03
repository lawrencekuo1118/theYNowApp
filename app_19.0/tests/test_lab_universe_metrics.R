#!/usr/bin/env Rscript
# Offline tests: universe metrics snapshot → market_cap / ret_1y / paid_in_capital
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

# Isolate snapshot cache with a tiny fixture (no live Yahoo required)
fixture_dir <- tempfile("ynow_metrics_")
dir.create(fixture_dir, recursive = TRUE)
fixture_csv <- file.path(fixture_dir, "universe_metrics_snapshot.csv")
utils::write.csv(
  data.frame(
    ticker = c("AAA", "BBB", "1101.TW"),
    market_cap = c(1e9, 5e9, 2e9),
    ret_1y = c(0.10, -0.05, 0.20),
    paid_in_capital = c(NA_real_, NA_real_, 7.7e10),
    snapshot_at = "2026-09-21T00:00:00Z",
    source = c("test", "test", "test|mops_paid_in"),
    stringsAsFactors = FALSE
  ),
  file = fixture_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
options(ynow.universe_metrics_snapshot = normalizePath(fixture_csv, winslash = "/"))

rm(list = ls(envir = .lab_universe_metrics_cache), envir = .lab_universe_metrics_cache)
rm(list = ls(envir = .lab_mcap_cache), envir = .lab_mcap_cache)
rm(list = ls(envir = .lab_ret1y_cache), envir = .lab_ret1y_cache)

snap <- lab_load_universe_metrics_snapshot(force = TRUE)
check("snapshot rows", nrow(snap) == 3L)
check("snapshot has AAA mcap", isTRUE(abs(snap$market_cap[snap$ticker == "AAA"] - 1e9) < 1))

caps <- lab_fetch_market_caps_usd(c("BBB", "ZZZ"))
check("snapshot mcap for BBB", isTRUE(abs(caps[["BBB"]] - 5e9) < 1))
check("missing ticker stays NA", !is.finite(caps[["ZZZ"]]))

rets <- lab_fetch_returns_1y(c("AAA", "1101.TW"))
check("snapshot ret AAA", isTRUE(abs(rets[["AAA"]] - 0.10) < 1e-9))
check("snapshot ret TW", isTRUE(abs(rets[["1101.TW"]] - 0.20) < 1e-9))

pool <- data.frame(
  ticker = c("AAA", "BBB", "1101.TW"),
  stringsAsFactors = FALSE
)
pool_m <- lab_attach_market_caps(pool)
check("attach mcap BBB largest", identical(pool_m$ticker[order(-pool_m$market_cap)][1], "BBB"))
check(
  "attach paid_in_capital TW",
  isTRUE(is.finite(pool_m$paid_in_capital[pool_m$ticker == "1101.TW"]))
)
pool_r <- lab_attach_returns_1y(pool)
check("attach ret_1y", isTRUE(abs(pool_r$ret_1y[pool_r$ticker == "AAA"] - 0.10) < 1e-9))

ranked <- lab_select_eval_pool(pool_m, max_n = 2L, mode = "mcap")
check("rank uses snapshot mcap", identical(as.character(ranked$ticker), c("BBB", "1101.TW")))
check("used_market_cap TRUE", isTRUE(attr(ranked, "used_market_cap")))

# Bundled snapshot shipped with the app
options(ynow.universe_metrics_snapshot = NULL)
rm(list = ls(envir = .lab_universe_metrics_cache), envir = .lab_universe_metrics_cache)
rm(list = ls(envir = .lab_mcap_cache), envir = .lab_mcap_cache)
bundled <- file.path(app_dir, "data", "universe_metrics_snapshot.csv")
if (file.exists(bundled)) {
  big <- lab_load_universe_metrics_snapshot(force = TRUE)
  n_mcap <- sum(is.finite(big$market_cap) & big$market_cap > 0, na.rm = TRUE)
  n_ret <- sum(is.finite(big$ret_1y), na.rm = TRUE)
  n_paid <- sum(is.finite(big$paid_in_capital) & big$paid_in_capital > 0, na.rm = TRUE)
  check("bundled snapshot nonempty", nrow(big) > 100L)
  check("bundled mcap coverage", n_mcap > 100L)
  cat(sprintf(
    "INFO bundled rows=%d mcap=%d ret_1y=%d paid_in=%d\n",
    nrow(big), n_mcap, n_ret, n_paid
  ))
  if ("NVDA" %in% big$ticker) {
    check("NVDA mcap finite", isTRUE(is.finite(big$market_cap[big$ticker == "NVDA"])))
  }
  if ("2330.TW" %in% big$ticker) {
    check("2330.TW mcap finite", isTRUE(is.finite(big$market_cap[big$ticker == "2330.TW"])))
    check(
      "2330.TW paid_in present",
      isTRUE(is.finite(big$paid_in_capital[big$ticker == "2330.TW"]))
    )
  }
} else {
  cat("INFO bundled universe_metrics_snapshot.csv missing\n")
}

unlink(fixture_dir, recursive = TRUE)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_universe_metrics\n")
