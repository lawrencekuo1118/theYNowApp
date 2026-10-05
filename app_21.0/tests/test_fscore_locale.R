#!/usr/bin/env Rscript
# F-Score terminology: one glossary in en-US + zh-TW (quality screen / 品質檢核).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

keys <- c(
  "funnel_ch1_title", "funnel_ch1_lead", "funnel_fscore_list_title", "funnel_vbox_fscore",
  "funnel_kpi_jump_mos_aria", "funnel_kpi_jump_fscore_aria", "funnel_kpi_jump_alerts_aria",
  "dc_label_fscore", "dc_hint_fscore", "dc_cond_fscore_min", "dc_cond_hint_fscore_min",
  "lab_im_gate_label", "lab_im_gate_hint", "lab_im_run_title",
  "lab_im_progress", "lab_im_done_gate", "lab_im_gate_on", "lab_im_gate_off",
  "fscore_col_item", "fscore_col_score", "fscore_result_pass", "fscore_result_fail",
  "funnel_fscore_waiting", "funnel_fscore_n_pass", "funnel_fscore_n_fail",
  "fscore_item_roa_pos", "fscore_item_ocf_pos", "fscore_item_roa_up", "fscore_item_earn_quality",
  "fscore_item_leverage", "fscore_item_liquidity", "fscore_item_dilution",
  "fscore_item_margin", "fscore_item_turnover",
  "conf_fscore_strong", "conf_fscore_weak", "ann_fscore_crossref"
)
en_keys <- names(.UI_STRINGS$en)
zh_keys <- names(.UI_STRINGS$`zh-TW`)
for (k in keys) {
  check(paste("en key", k), isTRUE(k %in% en_keys) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), isTRUE(k %in% zh_keys) && nzchar(ui_str(k, "zh-TW")))
}

check("en term F-Score", identical(ui_str("funnel_vbox_fscore", "en"), "Quality screen (F-Score)"))
check("zh-TW term F-Score 品質檢核", identical(ui_str("funnel_vbox_fscore", "zh-TW"), "品質檢核 (F-Score)"))
check("zh-TW block 財報體質", identical(ui_str("funnel_ch1_title", "zh-TW"), "財報體質"))
check("en block Statement quality", identical(ui_str("funnel_ch1_title", "en"), "Statement quality"))
check("en first mention Piotroski", grepl("Piotroski F-Score", ui_str("funnel_ch1_lead", "en"), fixed = TRUE))
check("zh first mention Piotroski", grepl("Piotroski F-Score", ui_str("funnel_ch1_lead", "zh-TW"), fixed = TRUE))
check("en quality screen not buy", grepl("quality screen", ui_str("funnel_ch1_lead", "en"), ignore.case = TRUE) &&
        grepl("not a standalone buy", ui_str("funnel_ch1_lead", "en"), ignore.case = TRUE))
check("zh 通過／未達標 品質檢核", grepl("通過／未達標", ui_str("funnel_ch1_lead", "zh-TW"), fixed = TRUE) &&
        grepl("品質檢核", ui_str("funnel_ch1_lead", "zh-TW"), fixed = TRUE) &&
        grepl("不單獨構成買進", ui_str("funnel_ch1_lead", "zh-TW"), fixed = TRUE))
check("dc en quality-screen floor", identical(ui_str("dc_label_fscore", "en"), "F-Score quality-screen floor"))
check("dc zh 品質檢核下限", identical(ui_str("dc_label_fscore", "zh-TW"), "F-Score 品質檢核下限"))
check("lab gate en Piotroski high gate", identical(ui_str("lab_im_gate_label", "en"), "Piotroski high gate"))
check("lab gate zh Piotroski 高門檻", identical(ui_str("lab_im_gate_label", "zh-TW"), "Piotroski 高門檻"))
check("lab gate off uses F-Score", grepl("F-Score", ui_str("lab_im_gate_off", "en"), fixed = TRUE) &&
        grepl("F-Score", ui_str("lab_im_gate_off", "zh-TW"), fixed = TRUE))

# Banned competing glosses in F-Score *labels* (not HFV 體質 prose)
label_blob_zh <- paste(
  ui_str("funnel_fscore_list_title", "zh-TW"),
  ui_str("funnel_vbox_fscore", "zh-TW"),
  ui_str("dc_label_fscore", "zh-TW"),
  collapse = " "
)
label_blob_en <- paste(
  ui_str("funnel_fscore_list_title", "en"),
  ui_str("funnel_vbox_fscore", "en"),
  ui_str("dc_label_fscore", "en"),
  collapse = " "
)
check("zh labels avoid 派氏/F分/體質檢核", !grepl("派氏|F分|F 分|體質檢核|財報品質分數|基本面評分", label_blob_zh))
check("en labels avoid health check / completeness", !grepl("health check|completeness floor|quality filter|quality checklist", label_blob_en, ignore.case = TRUE))

# localize helper
check("localize helper exists", exists("localize_fscore_checklist", mode = "function"))
check("fscore cards helper exists", exists("fscore_results_ui", mode = "function"))
check("waiting en not buy", !grepl("buy", ui_str("funnel_fscore_waiting", "en"), ignore.case = TRUE))
check("waiting zh 財報", grepl("財報", ui_str("funnel_fscore_waiting", "zh-TW"), fixed = TRUE))
check("waiting zh not 数据", !grepl("数据", ui_str("funnel_fscore_waiting", "zh-TW"), fixed = TRUE))
df <- data.frame(
  `檢驗維度` = "獲利性 (ROA > 0)",
  `得分` = "通過",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
en_df <- localize_fscore_checklist(df, "en")
zh_df <- localize_fscore_checklist(df, "zh-TW")
check("en checklist col Quality item", "Quality item" %in% names(en_df))
check("en checklist Pass", identical(unname(en_df[[2]][1]), "Pass"))
check("en item ROA", grepl("ROA > 0", en_df[[1]][1], fixed = TRUE))
check("zh checklist 檢驗維度", "檢驗維度" %in% names(zh_df))
check("zh checklist 通過", identical(unname(zh_df[[2]][1]), "通過"))

if (requireNamespace("htmltools", quietly = TRUE)) {
  cards <- fscore_results_ui(df, "en")
  cards_html <- paste(as.character(cards), collapse = "")
  check("cards wrap", grepl("ynow-fscore-wrap", cards_html, fixed = TRUE))
  check("cards not details", !grepl("<details", cards_html, fixed = TRUE))
  check("cards keep F-Score English result Pass", grepl("Pass", cards_html, fixed = TRUE))
}

# nine-item key parity
item_keys <- grep("^fscore_item_", names(.UI_STRINGS$en), value = TRUE)
check("nine item keys", length(item_keys) == 9L)
check("item keys in zh-TW", all(item_keys %in% names(.UI_STRINGS$`zh-TW`)))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
