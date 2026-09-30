#!/usr/bin/env Rscript
# YNOW index selection + equal-weight path (no network)
# Run: cd app_19.0 && Rscript tests/test_ynow_index.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) dirname(normalizePath(file_arg)) else getwd()
root <- if (file.exists(file.path(test_dir, "..", "ynow_index.R"))) {
  normalizePath(file.path(test_dir, ".."))
} else if (file.exists("ynow_index.R")) {
  normalizePath(".")
} else stop("Cannot locate ynow_index.R")
setwd(root)

source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("ynow_index.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "") else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("pass 9 and zero alerts", isTRUE(ynow_index_passes(9, 0)))
check("reject 8", !isTRUE(ynow_index_passes(8, 0)))
check("reject alert", !isTRUE(ynow_index_passes(9, 1)))
check("reject unknown alert", !isTRUE(ynow_index_passes(9, NA)))

screen <- function(tk) {
  pass <- tk %in% c("BBB", "DDD", "FFF", "HHH", "III", "JJJ", "LLL", "NNN", "PPP", "RRR", "TTT", "ZZZ")
  list(pass = pass, f_score = if (pass) 9 else 6, n_alert = if (pass) 0 else 1)
}
ranked <- c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "HHH", "III", "JJJ", "LLL", "NNN", "PPP", "RRR", "TTT", "UUU")
caps <- stats::setNames(rev(seq_along(ranked)) * 1e9, ranked)
sel <- ynow_index_select(ranked, screen, mcap = caps, n = 10L, scan_cap = 80L)
got <- vapply(sel$members, function(m) m$ticker, character(1))
check("takes ten in mcap order", identical(got, c("BBB", "DDD", "FFF", "HHH", "III", "JJJ", "LLL", "NNN", "PPP", "RRR")))
check("skips industry-agnostic failures", !"AAA" %in% got && !"CCC" %in% got)
check("equal weight", all(abs(vapply(sel$members, function(m) m$weight, numeric(1)) - 0.1) < 1e-12))
check("complete ten", isTRUE(sel$complete) && identical(sel$n, 10L))

fails <- paste0("X", seq_len(10))
short <- ynow_index_select(fails, screen, n = 10L, scan_cap = 10L)
check("scan cap stops", identical(short$n, 0L) && isTRUE(short$hit_scan_cap) && identical(short$scanned, 10L))

dates <- as.Date("2024-01-01") + 0:2
px <- list(
  AAA = data.frame(Date = dates, Close = c(10, 10, 20)),
  BBB = data.frame(Date = dates, Close = c(40, 40, 40))
)
ser <- ynow_index_bh_equal(px)
check("rebase 100", is.data.frame(ser) && abs(ser$Close[1] - 100) < 1e-8)
check("equal path day 3", abs(ser$Close[3] - 150) < 1e-8)

one <- list(ZZZ = data.frame(Date = dates, Close = c(5, 10, 15)))
ser1 <- ynow_index_bh_equal(one)
check("single name doubles", abs(ser1$Close[2] - 200) < 1e-8)

tmp <- tempfile(fileext = ".csv")
toy <- ynow_index_select(c("BBB", "DDD"), screen, mcap = caps, n = 2L, scan_cap = 5L)
toy$month <- "2026-10"
toy$built_at <- "2026-10-01T00:00:00Z"
check("write cache", isTRUE(ynow_index_write_cache(toy, tmp)))
back <- ynow_index_read_cache("2026-10", tmp)
check("read cache month", identical(vapply(back$members, function(m) m$ticker, character(1)), c("BBB", "DDD")))
check("other month miss", is.null(ynow_index_read_cache("2026-09", tmp)))
cur <- ynow_index_current(
  as_of = as.POSIXct("2026-10-15 12:00:00", tz = "Asia/Taipei"),
  rebuild = TRUE,
  screen_fn = screen,
  ranked = data.frame(
    ticker = c("ZZZ", "BBB", "AAA"),
    market_cap = c(1, 3, 9),
    stringsAsFactors = FALSE
  ),
  cache_path = tmp
)
check("current ranks by mcap", identical(vapply(cur$members, function(m) m$ticker, character(1)), c("BBB", "ZZZ")))
check("month taipei", identical(cur$month, "2026-10"))

check("en rule", grepl("equal weight", ui_str("ynow_index_rule", "en"), fixed = TRUE) &&
  grepl("9/9", ui_str("ynow_index_rule", "en"), fixed = TRUE))
check("zh rule", grepl("等權重", ui_str("ynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("不依產業", ui_str("ynow_index_rule", "zh-TW"), fixed = TRUE))
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("ynow_index_rule", "zh-TW")))
keys <- c("ynow_index_title", "ynow_index_rule", "ynow_index_chart_note", "ynow_index_waiting",
          "ynow_index_empty", "ynow_index_none", "ynow_index_build", "ynow_index_level",
          "ynow_index_col_ticker", "ynow_index_col_weight", "ynow_index_col_mcap", "ynow_index_col_fscore")
check("keys both locales", all(vapply(keys, function(k) {
  nzchar(ui_str(k, "en")) && nzchar(ui_str(k, "zh-TW"))
}, logical(1))))

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("panel mounted", grepl("ynow_index_panel", macro, fixed = TRUE))
check("build button", grepl("ynow_index_build", macro, fixed = TRUE))
check("US only gate", grepl('!identical(.mode(), "US")', macro, fixed = TRUE))
check("yahoo catalog untouched", !grepl('"YNOW" = "YNOW"', macro, fixed = TRUE))
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale wire", grepl("ynow_index_rule", ui_src, fixed = TRUE))

if (fail > 0L) {
  cat("\n", fail, " failure(s)\n", sep = "")
  quit(status = 1)
}
cat("\nAll YNOW index checks passed.\n")
