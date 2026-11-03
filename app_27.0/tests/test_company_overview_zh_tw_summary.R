#!/usr/bin/env Rscript
# Company Overview body: zh-TW localization path (offline; stubs network).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    message("PASS: ", label)
  } else {
    fail <<- fail + 1L
    message("FAIL: ", label)
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_company_overview_zh_tw_summary.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

if (!exists("normalize_ui_locale", mode = "function")) {
  source(file.path(root, "ui_locale.R"), local = TRUE)
}
source(file.path(root, "company_overview_locale.R"), local = TRUE)

en_sum <- paste(
  "Taiwan Semiconductor Manufacturing Company Limited manufactures,",
  "packages, tests, and sells integrated circuits and other semiconductor devices."
)
tw_native <- "依客戶之訂單與其提供之產品設計說明，以從事製造與銷售積體電路以及其他晶圓半導體裝置。"

check("needs zh body for English", isTRUE(.ynow_co_needs_zh_tw_body(en_sum)))
check("does not need zh body for TW native", isFALSE(.ynow_co_needs_zh_tw_body(tw_native)))
check(
  "desk filter 軟件→軟體",
  identical(.ynow_co_tw_desk_filter("該公司開發軟件產品"), "該公司開發軟體產品")
)
check(
  "desk filter 投資者→投資人",
  grepl("投資人", .ynow_co_tw_desk_filter("提醒投資者注意風險"), fixed = TRUE)
)
check(
  "normalize 2330 → 2330.TW",
  identical(.ynow_co_normalize_tw_symbol("2330"), "2330.TW")
)
check(
  "normalize AAPL → empty TW sym",
  identical(.ynow_co_normalize_tw_symbol("AAPL"), "")
)

# en locale: passthrough
check(
  "en locale keeps English",
  identical(
    localize_company_business_summary(en_sum, ticker = "2330", locale = "en", market_mode = "TW"),
    en_sum
  )
)

# zh-TW + TW ticker: prefer Yahoo TW stub
.cached_fetch_yahoo_tw_business_scope <<- function(ticker, timeout_sec = 12) {
  invisible(ticker); invisible(timeout_sec)
  tw_native
}
.cached_translate_en_to_zh_tw <<- function(text) {
  stop("translate must not run when TW native available: ", substr(text, 1, 40))
}
zh_tw_body <- localize_company_business_summary(
  en_sum, ticker = "2330", locale = "zh-TW", market_mode = "TW"
)
check("zh-TW TW ticker uses Yahoo TW scope", identical(zh_tw_body, tw_native))
check("zh-TW TW body has CJK", .ynow_co_cjk_count(zh_tw_body) >= 8L)

# zh-TW + US ticker: translate stub
.cached_fetch_yahoo_tw_business_scope <<- function(ticker, timeout_sec = 12) {
  invisible(ticker); invisible(timeout_sec)
  ""
}
.cached_translate_en_to_zh_tw <<- function(text) {
  invisible(text)
  "蘋果公司設計、製造及銷售智慧型手機、個人電腦與穿戴裝置。"
}
zh_us <- localize_company_business_summary(
  "Apple Inc. designs, manufactures, and sells smartphones.",
  ticker = "AAPL",
  locale = "zh-TW",
  market_mode = "US"
)
check("zh-TW US ticker uses translated body", grepl("蘋果公司", zh_us, fixed = TRUE))
check("zh-TW US translated has CJK", .ynow_co_cjk_count(zh_us) >= 8L)

# translate failure → English fallback
.cached_translate_en_to_zh_tw <<- function(text) ""
fb <- localize_company_business_summary(
  en_sum, ticker = "AAPL", locale = "zh-TW", market_mode = "US"
)
check("translate fail falls back to English", identical(fb, en_sum))

# Server wiring asserts
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE), collapse = "\n")
glob <- paste(readLines(file.path(root, "global.R"), warn = FALSE), collapse = "\n")
check(
  "server calls localize_company_business_summary",
  grepl("localize_company_business_summary\\(", srv)
)
check(
  "global sources company_overview_locale.R",
  grepl('source("company_overview_locale.R"', glob, fixed = TRUE)
)
check(
  "legacy overview test still present",
  file.exists(file.path(root, "tests", "test_company_overview.R"))
)

if (fail > 0L) {
  quit(status = 1L)
}
message("ALL PASS: test_company_overview_zh_tw_summary.R")
