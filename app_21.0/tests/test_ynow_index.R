#!/usr/bin/env Rscript
# YNOW index selection + equal-weight path (no network)
# Run: cd app_21.0 && Rscript tests/test_ynow_index.R

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

check("pass 8 and zero alerts", isTRUE(ynow_index_passes(8, 0)))
check("pass 9 and zero alerts", isTRUE(ynow_index_passes(9, 0)))
check("reject 7", !isTRUE(ynow_index_passes(7, 0)))
check("reject alert US", !isTRUE(ynow_index_passes(8, 1, market = "US")))
check("reject alert 8 TW", !isTRUE(ynow_index_passes(8, 1, market = "TW")))
check("reject two alerts perfect TW", !isTRUE(ynow_index_passes(9, 2, market = "TW")))
check("pass perfect + one alert TW", isTRUE(ynow_index_passes(9, 1, market = "TW")))
check("reject perfect + one alert US", !isTRUE(ynow_index_passes(9, 1, market = "US")))
check("reject unknown alert", !isTRUE(ynow_index_passes(8, NA)))
tw_uni <- data.frame(
  ticker = c("2330.TW", "0050.TW", "6488.TWO", "3105.TWO"),
  exchange = c("TWSE", "TWSE", "TPEX", "ESB"),
  stringsAsFactors = FALSE
)
tw_keep <- ynow_index_filter_tw_rows(tw_uni)$ticker
check("TW keeps listed and OTC", identical(sort(tw_keep), c("2330.TW", "6488.TWO")))
check("TYNOW symbol", identical(ynow_index_symbol("TW"), "TYNOW"))
check("YNOW symbol", identical(ynow_index_symbol("US"), "YNOW"))
check("TW cache file", grepl("tynow_index_basket.csv", ynow_index_cache_path("TW"), fixed = TRUE))
check("US cache file", grepl("ynow_index_basket.csv", ynow_index_cache_path("US"), fixed = TRUE))

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
check("current US symbol", identical(cur$symbol, "YNOW"))
tw_cur <- ynow_index_current(
  as_of = as.POSIXct("2026-10-15 12:00:00", tz = "Asia/Taipei"),
  rebuild = TRUE,
  market = "TW",
  screen_fn = screen,
  ranked = data.frame(
    ticker = c("ZZZ", "BBB", "AAA"),
    market_cap = c(1, 3, 9),
    stringsAsFactors = FALSE
  ),
  cache_path = tempfile(fileext = ".csv")
)
check("current TW symbol", identical(tw_cur$symbol, "TYNOW") &&
  identical(vapply(tw_cur$members, function(m) m$ticker, character(1)), c("BBB", "ZZZ")))

check("en rule", grepl("equal weight", ui_str("ynow_index_rule", "en"), fixed = TRUE) &&
  grepl("8 or higher", ui_str("ynow_index_rule", "en"), fixed = TRUE) &&
  grepl("not re-screened", ui_str("ynow_index_rule", "en"), fixed = TRUE))
check("zh rule", grepl("等權重", ui_str("ynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("8 分以上", ui_str("ynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("不重新篩選", ui_str("ynow_index_rule", "zh-TW"), fixed = TRUE))
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("ynow_index_rule", "zh-TW")))
check("en TYNOW rule", grepl("TYNOW", ui_str("tynow_index_title", "en"), fixed = TRUE) &&
  grepl("8 or higher", ui_str("tynow_index_rule", "en"), fixed = TRUE) &&
  grepl("perfect F-Score (9)", ui_str("tynow_index_rule", "en"), fixed = TRUE) &&
  grepl("at most one statement alert", ui_str("tynow_index_rule", "en"), fixed = TRUE) &&
  grepl("emerging-board", ui_str("tynow_index_rule", "en"), fixed = TRUE))
check("zh TYNOW rule", grepl("上市與上櫃", ui_str("tynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("滿分（9）", ui_str("tynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("至多一項", ui_str("tynow_index_rule", "zh-TW"), fixed = TRUE) &&
  grepl("興櫃", ui_str("tynow_index_rule", "zh-TW"), fixed = TRUE) &&
  !grepl("默认|参数|数据|用户", ui_str("tynow_index_rule", "zh-TW")))
keys <- c("ynow_index_title", "ynow_index_rule", "ynow_index_chart_note", "ynow_index_waiting",
          "ynow_index_empty", "ynow_index_none", "ynow_index_level", "ynow_index_constituents",
          "ynow_index_col_ticker", "ynow_index_col_name", "ynow_index_col_weight",
          "ynow_index_col_last", "ynow_index_col_chg",
          "ynow_index_col_mcap", "ynow_index_col_fscore",
          "tynow_index_title", "tynow_index_rule", "tynow_index_waiting", "tynow_index_empty", "tynow_index_none")
check("keys both locales", all(vapply(keys, function(k) {
  nzchar(ui_str(k, "en")) && nzchar(ui_str(k, "zh-TW"))
}, logical(1))))

nm <- ynow_index_lookup_names("AAPL", "US")
check("lookup name", identical(unname(nm[["AAPL"]]), "Apple Inc."))

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("own index at page bottom", {
  pos_fx <- regexpr("ynow_macro_fx_lock", macro, fixed = TRUE)[1]
  pos_own <- regexpr("id = \"ynow_own_index\"", macro, fixed = TRUE)[1]
  pos_fx > 0 && pos_own > pos_fx
})
check("flow numeral", grepl("ynow-htcdi-flow", macro, fixed = TRUE))
check("YNOW KPI on Rf row", grepl(".own_index_kpi_card", macro, fixed = TRUE) &&
  grepl("ynow-macro-kpi--ynow", macro, fixed = TRUE) &&
  grepl("ynow_col", macro, fixed = TRUE))
check("own chart overlay opt-in", {
  grepl("own_index_overlay", macro, fixed = TRUE) &&
    grepl("own_index_overlays <- reactiveVal(character(0))", macro, fixed = TRUE) &&
    grepl("macro_align_rebase_100", macro, fixed = TRUE)
})
check("own chart expands in shared hist panel", {
  pos_rf <- regexpr("rf_signal_row", macro, fixed = TRUE)[1]
  pos_hist <- regexpr("ynow_macro_index_hist", macro, fixed = TRUE)[1]
  pos_rf > 0 && pos_hist > pos_rf &&
    grepl("Rule + chart + constituents all live in the shared expand card", macro, fixed = TRUE)
})
check("rule above hist chart in expand", {
  grepl('id = "ynow_index_rule"', macro, fixed = TRUE) &&
    grepl("index_hist_panel", macro, fixed = TRUE) &&
    grepl("rule above chart", macro, fixed = TRUE)
})
check("no mid-page YNOW rule chapter", {
  grepl("output\\$own_index_block <- renderUI", macro) &&
    grepl("Keep this output as NULL", macro, fixed = TRUE)
})
check("constituents under hist chart", {
  grepl(".own_index_constituents_ui", macro, fixed = TRUE) &&
    grepl("if (own) .own_index_constituents_ui()", macro, fixed = TRUE) &&
    !grepl("own_index_detail", macro, fixed = TRUE)
})
check("expand constituents", grepl("ynow_index_constituents", macro, fixed = TRUE))
check("mcap column after name", {
  pos_name <- regexpr('ynow_index_col_name", `data-i18n` = "ynow_index_col_name"', macro, fixed = TRUE)[1]
  pos_mcap <- regexpr('ynow_index_col_mcap", `data-i18n` = "ynow_index_col_mcap"', macro, fixed = TRUE)[1]
  pos_weight <- regexpr('ynow_index_col_weight", `data-i18n` = "ynow_index_col_weight"', macro, fixed = TRUE)[1]
  pos_name > 0 && pos_mcap > pos_name && pos_weight > pos_mcap &&
    grepl("format_money_abbr(mcap, mcap_ccy)", macro, fixed = TRUE) &&
    grepl("market_cap = suppressWarnings(as.numeric(m$market_cap)[1])", macro, fixed = TRUE)
})
check("weight percent", grepl('sprintf("%.2f%%", 100 * w)', macro, fixed = TRUE))
check("price recalc", grepl("ynow_index_series", macro, fixed = TRUE))
check("no live rescreen", !grepl("ynow_index_current(market = mode)", macro, fixed = TRUE))
check("no build button", !grepl("ynow_index_build", macro, fixed = TRUE))
check("TW market", grepl("TYNOW", macro, fixed = TRUE))
check("yahoo catalog untouched", !grepl('"YNOW" = "YNOW"', macro, fixed = TRUE))
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("locale wire", grepl("ynow_index_rule", ui_src, fixed = TRUE))

if (fail > 0L) {
  cat("\n", fail, " failure(s)\n", sep = "")
  quit(status = 1)
}
cat("\nAll YNOW index checks passed.\n")
