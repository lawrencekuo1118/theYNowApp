#!/usr/bin/env Rscript
# Tests: ADR industry map + include-ADR filter + truncate order helpers
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
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

amap <- lab_adr_industry_map()
check("ADR map nonempty", length(amap) >= 40L)
check("TSM Foundry", identical(unname(amap[["TSM"]]), "sc.Foundry"))
check("SKHY Memory", identical(unname(amap[["SKHY"]]), "sc.Memory"))
check("BABA ecommerce", identical(unname(amap[["BABA"]]), "ecr.Ecommerce_Retail"))
check("ASML equipment", identical(unname(amap[["ASML"]]), "sc.Equipment"))
miss_keys <- setdiff(unique(unname(amap)), names(industry_standards))
check("all ADR keys in industry_standards", length(miss_keys) == 0L)
if (length(miss_keys)) cat("  missing keys: ", paste(miss_keys, collapse = ", "), "\n", sep = "")

check("TSM is ADR", isTRUE(lab_is_us_adr("TSM", "TAIWAN SEMICONDUCTOR")))
check("SKHY is ADR", isTRUE(lab_is_us_adr("SKHY", "SK hynix Inc.")))
check("AAPL not ADR", !isTRUE(lab_is_us_adr("AAPL", "APPLE INC")))
check("HSBC foreign name", isTRUE(lab_is_us_adr("HSBC", "HSBC HOLDINGS PLC")))

u <- lab_get_us_universe(FALSE)
check("universe has is_adr", "is_adr" %in% names(u))
tsm <- u[u$ticker == "TSM", , drop = FALSE]
skhy <- u[u$ticker == "SKHY", , drop = FALSE]
check("TSM mapped Foundry", nrow(tsm) == 1L && identical(tsm$industry_key[[1]], "sc.Foundry"))
check("SKHY mapped Memory", nrow(skhy) == 1L && identical(skhy$industry_key[[1]], "sc.Memory"))
check("TSM is_adr TRUE", nrow(tsm) == 1L && isTRUE(tsm$is_adr[[1]]))
baba <- u[u$ticker == "BABA", , drop = FALSE]
check("BABA mapped", nrow(baba) == 1L && identical(baba$industry_key[[1]], "ecr.Ecommerce_Retail"))

n_adr <- sum(as.logical(u$is_adr), na.rm = TRUE)
check("ADR count > 50", n_adr > 50L)
cat(sprintf("INFO ADR flagged rows=%d / %d\n", n_adr, nrow(u)))

pool <- data.frame(
  ticker = c("AAPL", "TSM", "MSFT", "SKHY", "BABA"),
  is_adr = c(FALSE, TRUE, FALSE, TRUE, TRUE),
  market_cap = c(3e12, 5e11, 2e12, 1e11, 2e11),
  stringsAsFactors = FALSE
)
keep <- lab_filter_pool_adr(pool, include_adr = TRUE)
check("include ADR keeps 5", nrow(keep) == 5L)
drop <- lab_filter_pool_adr(pool, include_adr = FALSE)
check("exclude ADR drops 3", nrow(drop) == 2L && all(drop$ticker %in% c("AAPL", "MSFT")))

# Truncate after ADR exclude: mcap sort then N
ranked <- lab_select_eval_pool(
  lab_filter_pool_adr(pool, include_adr = FALSE),
  max_n = 2L, mode = "mcap"
)
check(
  "truncate after exclude ADR",
  identical(as.character(ranked$ticker), c("AAPL", "MSFT"))
)

# Catalog attaches is_adr
catlg <- lab_build_industry_method_catalog("US")
check("catalog has is_adr", "is_adr" %in% names(catlg))
if ("ticker" %in% names(catlg)) {
  hit <- catlg[!is.na(catlg$ticker) & toupper(as.character(catlg$ticker)) == "TSM", , drop = FALSE]
  check("catalog TSM Foundry", nrow(hit) >= 1L && identical(hit$industry_key[[1]], "sc.Foundry"))
  check("catalog TSM is_adr", nrow(hit) >= 1L && isTRUE(hit$is_adr[[1]]))
}

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("PASS lab_adr\n")
