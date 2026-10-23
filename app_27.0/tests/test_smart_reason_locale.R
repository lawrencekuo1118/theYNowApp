#!/usr/bin/env Rscript
# Smart Analysis / composite: pick reason_en under EN; confidence bilingual.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
# Minimal `%||%` + locale normalize used by setup helpers
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x[1]))) y else x
}
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check(".pick_rec_reason exists", exists(".pick_rec_reason", mode = "function"))

rec <- list(
  reason = "以財報屬性為主。主模型 dcf。",
  reason_en = "Statement attributes lead. Primary model dcf."
)
check("en picks reason_en", identical(
  .pick_rec_reason(rec, "en"),
  "Statement attributes lead. Primary model dcf."
))
check("zh-TW picks reason", identical(
  .pick_rec_reason(rec, "zh-TW"),
  "以財報屬性為主。主模型 dcf。"
))
check("en falls back to zh when reason_en empty", identical(
  .pick_rec_reason(list(reason = "僅中文", reason_en = ""), "en"),
  "僅中文"
))

empty_keys <- c(
  "model_rec_empty_summary", "model_rec_empty_reason",
  "model_rec_no_yahoo_summary", "model_rec_no_yahoo_reason",
  "model_rec_waiting_fs_summary", "model_rec_waiting_fs_reason",
  "model_rec_tpex_summary", "model_rec_tpex_reason",
  "conf_level_high", "conf_level_mid", "conf_level_low",
  "conf_fs_complete", "conf_fs_incomplete",
  "conf_fcf_stable", "conf_fcf_volatile",
  "conf_models_close", "conf_models_diverge", "conf_tv_weight_high"
)
for (k in empty_keys) {
  check(paste("en key", k), nzchar(ui_str(k, "en")))
  check(paste("zh key", k), nzchar(ui_str(k, "zh-TW")))
}

check("en empty reason not Chinese", !grepl("請先", ui_str("model_rec_empty_reason", "en")))
check("zh empty reason Traditional", grepl("請先", ui_str("model_rec_empty_reason", "zh-TW")))

conf_en <- score_valuation_confidence(
  confidence_inputs = list(data_complete = TRUE, has_fcf = TRUE, fcf_cv = 0.2),
  f_score = 9,
  primary_base = 100,
  secondary_point = 105,
  locale = "en"
)
conf_zh <- score_valuation_confidence(
  confidence_inputs = list(data_complete = TRUE, has_fcf = TRUE, fcf_cv = 0.2),
  f_score = 9,
  primary_base = 100,
  secondary_point = 105,
  locale = "zh-TW"
)
check("en confidence level High", identical(conf_en$level, "High"))
check("zh confidence level 高", identical(conf_zh$level, "高"))
check("en reasons English", any(grepl("Financial-statement|FCF volatility|F-Score stronger|aligned", conf_en$reasons)))
check("zh reasons Chinese", any(grepl("財報輸入完整|FCF 波動可控|F-Score 偏強|主副模型接近", conf_zh$reasons)))
check("en reasons not Chinese chars", !any(grepl("[\u4e00-\u9fff]", conf_en$reasons)))

# Smart Analysis render path must call .pick_rec_reason / reason_en
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("smart_analysis_reason uses .pick_rec_reason", grepl(
  "smart_analysis_reason[\\s\\S]{0,800}\\.pick_rec_reason",
  srv,
  perl = TRUE
))
check("recommend_valuation_models gets locale", grepl(
  "recommend_valuation_models\\([\\s\\S]{0,400}locale\\s*=\\s*loc_now",
  srv,
  perl = TRUE
))
idm <- paste(readLines(file.path(app_dir, "investment_decision_module.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("composite uses .pick_rec_reason", grepl("\\.pick_rec_reason\\(rec, loc\\)", idm))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
