#!/usr/bin/env Rscript
# Offline tests for lab_select_eval_pool / concept groups.
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
source(file.path(app_dir, "lab_concept_groups.R"), local = FALSE)
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

check("normalize mcap", identical(lab_normalize_pool_rank_mode("市值"), "mcap"))
check("normalize concept", identical(lab_normalize_pool_rank_mode("concept"), "concept"))
check("normalize ret", identical(lab_normalize_pool_rank_mode("ret_1y"), "ret_1y"))
check("normalize random", identical(lab_normalize_pool_rank_mode("random"), "random"))

us_ch <- lab_concept_group_choices("US", "en")
tw_ch <- lab_concept_group_choices("TW", "zh-TW")
check("US concepts nonempty", length(us_ch) >= 8L)
check("TW concepts nonempty", length(tw_ch) >= 14L)
check("mag7 has NVDA", "NVDA" %in% lab_concept_tickers("mag7", "US"))
check("TW ai has 2330.TW", "2330.TW" %in% lab_concept_tickers("ai_foundry", "TW"))
# Supplements from TW industry/concept maps (no duplicate theme keys)
tw_new <- c(
  "optical_comms", "robotics_auto", "defense_sat",
  "passive_components", "industrial_pc"
)
check("TW new concept keys present", all(tw_new %in% unname(tw_ch)))
check("TW optical has 2345", "2345.TW" %in% lab_concept_tickers("optical_comms", "TW"))
check("TW robotics has 1590", "1590.TW" %in% lab_concept_tickers("robotics_auto", "TW"))
check("TW defense has 2634", "2634.TW" %in% lab_concept_tickers("defense_sat", "TW"))
check("TW passive has 2327", "2327.TW" %in% lab_concept_tickers("passive_components", "TW"))
check("TW IPC has 2395", "2395.TW" %in% lab_concept_tickers("industrial_pc", "TW"))
check(
  "TW zh labels for new keys",
  identical(lab_concept_group_label("passive_components", "TW", "zh-TW"), "被動元件") &&
    identical(lab_concept_group_label("optical_comms", "TW", "zh-TW"), "光通訊／網通設備")
)
# All TW concept tickers resolve in bundled universe (when available)
u_path <- file.path(app_dir, "data", "tw_universe.csv")
if (file.exists(u_path)) {
  u <- utils::read.csv(u_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  all_tw <- unique(unlist(LAB_CONCEPT_GROUPS$TW, use.names = FALSE))
  miss <- setdiff(toupper(all_tw), toupper(as.character(u$ticker)))
  check("TW concept tickers in universe", length(miss) == 0L)
  if (length(miss)) cat("INFO missing concept tickers: ", paste(miss, collapse = ","), "\n", sep = "")
}

pool <- data.frame(
  ticker = c("AAA", "BBB", "CCC", "DDD", "EEE"),
  market_cap = c(5, 40, 10, NA, 20),
  stringsAsFactors = FALSE
)
mcap_p <- lab_select_eval_pool(pool, max_n = 2L, mode = "mcap")
check("mcap top2", identical(as.character(mcap_p$ticker), c("BBB", "EEE")))

rand_a <- lab_select_eval_pool(pool, max_n = 2L, mode = "random", seed = 1L)
rand_b <- lab_select_eval_pool(pool, max_n = 2L, mode = "random", seed = 1L)
check("random reproducible", identical(sort(rand_a$ticker), sort(rand_b$ticker)))
check("random size 2", nrow(rand_a) == 2L)

# Concept: only overlapping names kept
pool2 <- data.frame(
  ticker = c("AAPL", "MSFT", "ZZZ"),
  market_cap = c(100, 90, 1000),
  stringsAsFactors = FALSE
)
cp <- lab_select_eval_pool(
  pool2, max_n = 10L, mode = "concept",
  concept_keys = "mag7", market_mode = "US"
)
check("concept filters ZZZ", !("ZZZ" %in% cp$ticker) && all(cp$ticker %in% c("AAPL", "MSFT")))

# Rank entire pool by mcap first, then take N — even when pool is huge (no S&P pre-cut)
big <- data.frame(
  ticker = paste0("T", seq_len(900L)),
  market_cap = as.numeric(900:1),
  stringsAsFactors = FALSE
)
# Mock / stub: if lab_us_prescreen exists it must not drop the true top-cap names
# when market_cap is already present.
big_top <- lab_select_eval_pool(big, max_n = 3L, mode = "mcap")
check(
  "mcap sort-all-then-N on large pool",
  identical(as.character(big_top$ticker), c("T1", "T2", "T3"))
)

# Concept filter first, then mcap within, then N
pool3 <- data.frame(
  ticker = c("AAPL", "MSFT", "GOOGL", "ZZZ"),
  market_cap = c(50, 200, 100, 9999),
  stringsAsFactors = FALSE
)
cp_n <- lab_select_eval_pool(
  pool3, max_n = 2L, mode = "concept",
  concept_keys = "mag7", market_mode = "US"
)
check(
  "concept then mcap then N",
  identical(as.character(cp_n$ticker), c("MSFT", "GOOGL")) ||
    (nrow(cp_n) == 2L && !("ZZZ" %in% cp_n$ticker) && identical(cp_n$ticker[1], "MSFT"))
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_pool_rank\n")
