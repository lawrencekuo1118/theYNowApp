#!/usr/bin/env Rscript
# Offline smoke: backtest-adjacent HFV scenario classifier (no network).
# Asserts current A/B/C/D/other codes — does not change thresholds/formulas.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_hfv_scenario_smoke.R

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
root <- ynow_test_app_dir("backtest_validation.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("backtest_validation.R", local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

thr <- hfv_scenario_threshold_defaults()
check("defaults expose mos_deep_cheap 0.20", identical(thr$mos_deep_cheap, 0.20))
check("defaults expose price_strong_up 0.05", identical(thr$price_strong_up, 0.05))

# A 錯殺黃金坑: FV↑, Price↓, Price ≪ FV (MOS ≥ 20%)
check(
  "scenario A deep cheap",
  identical(classify_hfv_scenario(100, 120, 90, 80), "A")
)
# B 戴維斯雙擊: FV↑, Price↑, |MOS| ≤ 10%
check(
  "scenario B davis double",
  identical(classify_hfv_scenario(100, 110, 100, 108), "B")
)
# C 價值陷阱: FV↓, Price↓, MOS > 0
check(
  "scenario C value trap",
  identical(classify_hfv_scenario(100, 90, 95, 85), "C")
)
# D 泡沫炒作: FV not up, Price strong↑, MOS ≤ −20%
check(
  "scenario D bubble",
  identical(classify_hfv_scenario(100, 100, 100, 130), "D")
)
# other: mild co-move without A–D triggers
check(
  "scenario other mild",
  identical(classify_hfv_scenario(100, 103, 100, 102), "other")
)
check(
  "scenario NA on bad inputs",
  is.na(classify_hfv_scenario(NA, 110, 100, 108)) &&
    is.na(classify_hfv_scenario(100, 110, 0, 108))
)

# Direction helper (flat band ±2%)
check("rel dir flat", identical(.hfv_rel_dir(100, 101.5, 0.02), "flat"))
check("rel dir up", identical(.hfv_rel_dir(100, 103, 0.02), "up"))
check("rel dir down", identical(.hfv_rel_dir(100, 97, 0.02), "down"))

# Pair builder: A then C (FV↑/Price↓ deep cheap → FV↓/Price↓ with MOS>0)
val <- data.frame(
  Date = as.Date(c("2024-01-02", "2024-02-01", "2024-03-01")),
  fair_value = c(100, 120, 90),
  hist_price = c(90, 80, 70),
  stringsAsFactors = FALSE
)
pairs <- build_hfv_scenario_pairs(val)
check("pairs has rows", is.data.frame(pairs) && nrow(pairs) == 2L)
check("pairs codes are A then C", {
  codes <- as.character(pairs$scenario)
  identical(codes[1], "A") && identical(codes[2], "C")
})

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All HFV scenario smoke checks passed.\n")
