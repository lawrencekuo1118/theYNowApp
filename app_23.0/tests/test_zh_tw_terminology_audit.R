#!/usr/bin/env Rscript
# Audit zh-TW locale: Taiwan Traditional terminology (not Mainland / Simplified),
# and user-facing labels that must not remain English-only (except finance terms).
#
# Dangerous false positive: Traditional orthography ≠ Taiwan usage.
# Reject Mainland finance/stats/UI jargon that was merely converted to 繁體.

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

root <- if (file.exists("ui_locale.R")) "." else ".."
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source("ui_locale.R", local = FALSE, encoding = "UTF-8")

en <- .UI_STRINGS$en
zh <- .UI_STRINGS[["zh-TW"]]
keys <- names(en)
`%||%` <- function(a, b) if (is.null(a)) b else a

check("en/zh-TW key parity", identical(sort(keys), sort(names(zh))))

mainland <- c(
  "默认", "参数", "数据", "用户", "勾选", "周期", "阈值", "软件", "网络",
  "信息", "门限", "质量", "账户", "报表", "视频", "内存", "独立", "菜单"
)
# Traditional orthography but Mainland finance/stats/UI jargon (not Taiwan usage)
mainland_trad_traps <- c(
  "口徑", "統計口徑",
  "帶寬",           # IT bandwidth; TW finance uses 區間 for scenario bands
  "點擊",           # TW UI prefers 點選
  "可替代對象",     # CS「對象」; TW finance uses 可替代標的
  "一鍵",           # CN product-marketing; TW: 一次產出／快速套用
  "風險提示",       # CN broker boilerplate; TW securities: 風險警語
  "爬取", "抓取",   # TW desk/UI: 擷取／取得
  "同比", "環比",
  "市盈率", "市淨率",
  "倉位", "止損", "止盈",
  "杠桿",
  "投資者",         # TW: 投資人
  "賦能", "抓手", "閉環", "賽道", "顆粒度", "底層邏輯"
)
zh_blob <- paste(vapply(keys, function(k) as.character(zh[[k]] %||% "")[1], character(1)),
                 collapse = "\n")
for (w in mainland) {
  check(paste("no simplified/Mainland", w), !grepl(w, zh_blob, fixed = TRUE))
}
for (w in mainland_trad_traps) {
  check(paste("no Mainland-trad trap", w), !grepl(w, zh_blob, fixed = TRUE))
}
check("oos label uses 範圍 not 口徑", identical(as.character(zh$hfv_oos_mode_label)[1], "驗證樣本範圍"))
check(
  "oos realized label is natural TW",
  identical(as.character(zh$hfv_oos_realized)[1], "僅已實現下期（預設）")
)
check(
  "oos expanding label uses 擴張窗…命中率",
  identical(as.character(zh$hfv_oos_expanding)[1], "擴張窗樣本外命中率") &&
    !grepl("擴張視窗樣本外命中$", as.character(zh$hfv_oos_expanding)[1])
)
check(
  "oos insample label is natural TW",
  identical(as.character(zh$hfv_oos_insample)[1], "含未實現下期（樣本內）")
)
check(
  "en oos expanding uses hit rates",
  identical(as.character(en$hfv_oos_expanding)[1], "Expanding-window OOS hit rates")
)

# Scenario bands → 區間 (not 帶寬)
check(
  "scenario bands use 區間",
  grepl("情境區間", as.character(zh$hfv_scenario_thresh_note)[1], fixed = TRUE) &&
    grepl("A–D 區間", as.character(zh$hfv_scenario_concl_other)[1], fixed = TRUE)
)

# Lite toggle / report / risk copy
check(
  "lite toggle uses 點選",
  identical(as.character(zh$lite_toggle_title)[1], "點選即可切換簡化版／完整版")
)
check(
  "download report uses 一次產出",
  grepl("一次產出投資意見報告", as.character(zh$download_report_about)[1], fixed = TRUE)
)
check(
  "legal risk uses 風險警語",
  grepl("風險警語", as.character(zh$legal_risk_body)[1], fixed = TRUE)
)
check(
  "HTCDI substitutes use 可替代標的",
  identical(as.character(zh$htcdi_col_substitutes)[1], "可替代標的")
)
check(
  "macro Rf last-known uses 擷取",
  identical(as.character(zh$macro_rf_src_last)[1], "最近成功擷取")
)
check(
  "locale prefers 擷取 over 抓取",
  grepl("擷取", zh_blob, fixed = TRUE) && !grepl("抓取", zh_blob, fixed = TRUE)
)

# Taiwan prefers 佔比 / 網路 (not 占比 / 網絡)
check("uses 佔比 not 占比", grepl("佔比", zh_blob, fixed = TRUE) && !grepl("占比", zh_blob, fixed = TRUE))
check("uses 網路 not 網絡", !grepl("網絡", zh_blob, fixed = TRUE))

# 干擾: Taiwan MoE uses 干 (U+5E72), not 幹 (U+5E79)
gan_tw <- grepl("\u5e72\u64fe", zh_blob, fixed = TRUE)
gan_wrong_gan <- grepl("\u5e79\u64fe", zh_blob, fixed = TRUE)
check("干擾 uses Taiwan 干 (not 幹)", isTRUE(gan_tw) && !isTRUE(gan_wrong_gan))

# Previously English-only UI labels now have Taiwan Chinese
expect_zh <- list(
  lifecycle_opt_high_growth = "高成長",
  lifecycle_opt_declining = "衰退／有限存續",
  lifecycle_opt_financial = "金融機構",
  htcdi_alert_normal = "擴張",
  htcdi_alert_warning = "降溫",
  htcdi_alert_critical = "收縮",
  htcdi_alert_unavailable = "無法取得",
  htcdi_col_criticality = "關鍵程度",
  testing_page_title = "測試",
  bblab_ticker_label = "股票代號",
  bblab_gm_unestimable = "無法可靠估計",
  bblab_reval_unavailable = "重估比率無法取得",
  bblab_kpi_revenue = "營收",
  bblab_kpi_gp = "毛利",
  bblab_ch4_title = "五年結構佔比演進",
  htcdi_ly_payment_networks = "支付網路"
)
for (k in names(expect_zh)) {
  check(paste("zh label", k), identical(as.character(zh[[k]])[1], expect_zh[[k]]))
}

# Intentional English finance titles (kept with Chinese glosses elsewhere)
check(
  "HTCDI index title stays English by design",
  identical(as.character(zh$htcdi_index_health)[1], "Statement Development") &&
    grepl("財報發展", as.character(zh$htcdi_index_health_gloss)[1], fixed = TRUE)
)

# Substantial English prose identical to en (exclude formulas / short finance terms)
bad_ident <- character(0)
allow_prefix <- c(
  "hfv_scenario_cue_", "htcdi_index_", "bblab_formula_", "rel_formula_",
  "sotp_formula_", "ms_card_multiples_formula", "htcdi_formula_"
)
for (k in keys) {
  e <- as.character(en[[k]] %||% "")[1]
  z <- as.character(zh[[k]] %||% "")[1]
  if (!nzchar(e) || !identical(e, z)) next
  if (!grepl("[A-Za-z]{4,}", e) || !grepl(" ", e, fixed = TRUE) || nchar(e) <= 20L) next
  if (grepl("[=×÷]|Implied |P/E|EV/|FCFF|FCFE|WACC|HTCDI|Beta|Yahoo|FV|Price", e)) next
  if (any(startsWith(k, allow_prefix))) next
  # allow ALL-CAPS finance banners
  if (identical(e, toupper(e)) && nchar(e) <= 40L) next
  bad_ident <- c(bad_ident, k)
}
check(
  "no leftover English UI prose in zh-TW (non-finance)",
  length(bad_ident) == 0L || {
    cat(" leftover:", paste(head(bad_ident, 20), collapse = ", "), "\n")
    FALSE
  }
)

if (fail > 0L) {
  stop(sprintf("%d zh-TW terminology check(s) failed", fail), call. = FALSE)
}
cat("PASS zh_tw_terminology_audit\n")
