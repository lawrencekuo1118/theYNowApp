#!/usr/bin/env Rscript
# Blue Chip Leaderboard ENG mode: American English only (no CJK in en helpers / keys).
# Run: cd app_21.0/tests && Rscript test_lab_im_eng_locale.R

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
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

has_cjk <- function(x) {
  grepl("[\u4e00-\u9fff]", paste(as.character(x), collapse = " "), perl = TRUE)
}

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
# minimal stubs used by lab helpers
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "lab_sp500_universe.R"), local = FALSE)

keys <- c(
  "lab_im_query_title", "btn_lab_im_download", "btn_lab_im_refresh",
  "lab_im_eval_hint", "lab_im_summary_empty", "lab_im_leader_note_ready",
  "lab_im_universe_us", "lab_im_detail_no_qual", "lab_im_lb_none"
)
for (k in keys) {
  en <- ui_str(k, "en")
  zh <- ui_str(k, "zh-TW")
  check(paste("en key", k), nzchar(en) && !has_cjk(en))
  check(paste("zh-TW key", k), nzchar(zh) && has_cjk(zh))
}

check("method labels en no CJK", !has_cjk(lab_method_labels("en")))
check("method labels zh has CJK", has_cjk(lab_method_labels("zh-TW")))
check("max_n en no CJK", !has_cjk(names(lab_im_max_n_select_choices("en"))))
check("max_n zh has CJK", has_cjk(names(lab_im_max_n_select_choices("zh-TW"))))
check("pool_rank en no CJK", !has_cjk(names(lab_im_pool_rank_choices("en"))))
check("pool_rank zh has CJK", has_cjk(names(lab_im_pool_rank_choices("zh-TW"))))
check("chain picker en no CJK", !has_cjk(names(lab_industry_chain_picker_choices("en"))))

sm_en <- lab_method_group_summary(NULL, include_all = TRUE, locale = "en")
check("summary en colnames", identical(
  names(sm_en)[1:3],
  c("Suggested valuation method", "Industries", "Candidates")
))
check("summary en method cells no CJK", !has_cjk(sm_en[["Suggested valuation method"]]))

sm_zh <- lab_method_group_summary(NULL, include_all = TRUE, locale = "zh-TW")
check("summary zh colnames", identical(
  names(sm_zh)[1:3],
  c("建議評價方法", "產業數", "候選檔數")
))

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI query title id", grepl("ynow_lab_im_query_title", ui, fixed = TRUE))
check("UI eval hint id", grepl("ynow_lab_im_eval_hint", ui, fixed = TRUE))
check("UI refresh label id", grepl("ynow_lab_im_refresh_label", ui, fixed = TRUE))
check("refresh button in query title row", {
  # Scope to the box title= block so CSS class names later in the file do not confuse order checks.
  m <- regexpr(
    "title\\s*=\\s*tags\\$div\\(\\s*class\\s*=\\s*\"ynow-lab-im-query-title-row\"[\\s\\S]{0,1200}?lab_im_refresh_universe",
    ui,
    perl = TRUE
  )
  isTRUE(m > 0L) && grepl("ynow-lab-im-refresh-in-title", ui, fixed = TRUE)
})
check("applyUiLocale download", grepl("btn_lab_im_download", ui, fixed = TRUE))
check("applyUiLocale query title", grepl("lab_im_query_title", ui, fixed = TRUE))

srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "server updates max_n choices",
  grepl("lab_im_max_n_select_choices\\(loc2?\\)", srv) ||
    grepl("lab_im_max_n_select_choices(loc", srv, fixed = TRUE)
)
check("server summary uses locale", grepl("lab_method_group_summary\\(.*locale\\s*=\\s*loc", srv))
check("server delayed locale remount", grepl("delay = 0.25", srv, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " / ", pass + fail, " checks\n", sep = "")
  quit(status = 1)
}
cat("All lab_im_eng_locale checks passed (", pass, ").\n", sep = "")
