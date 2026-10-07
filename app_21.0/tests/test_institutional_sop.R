# Institutional SOP unlock logic
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_institutional_sop.R

root <- if (file.exists("decision_checklist_module.R")) {
  getwd()
} else if (file.exists("app_21.0/decision_checklist_module.R")) {
  file.path(getwd(), "app_21.0")
} else {
  stop("Cannot find app_21.0 root")
}
setwd(root)

source("decision_checklist_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

pass_row <- list(status = "pass")
fail_row <- list(status = "fail")
na_row <- list(status = "na")

mand_ok <- list(pass_row, pass_row, pass_row, pass_row)

check("locked without acks", !isTRUE(.dc_institutional_sop_unlock(FALSE, FALSE, FALSE, mand_ok)$unlocked))
check("locked with partial acks", !isTRUE(.dc_institutional_sop_unlock(TRUE, TRUE, FALSE, mand_ok)$unlocked))
check("unlocked with acks + pass gates", isTRUE(.dc_institutional_sop_unlock(TRUE, TRUE, TRUE, mand_ok)$unlocked))
check(
  "locked on gate fail",
  !isTRUE(.dc_institutional_sop_unlock(TRUE, TRUE, TRUE, list(pass_row, fail_row, pass_row, pass_row))$unlocked)
)
check(
  "locked on gate na",
  !isTRUE(.dc_institutional_sop_unlock(TRUE, TRUE, TRUE, list(pass_row, na_row, pass_row, pass_row))$unlocked)
)
check(
  "mandatory item set",
  identical(.DC_SOP_MANDATORY_ITEMS, c("g_sgr", "fscore", "bear_base", "hfv_veto"))
)

# Placement: compact SOP on YNOW; Decision Checklist only points there
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("investment_decision_module.R", local = TRUE, encoding = "UTF-8")
dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dc <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("SOP mount helper exported", exists("decision_sop_panel_ui", mode = "function"))
check("YNOW mounts SOP panel", grepl("decision_sop_panel_ui\\(\\)", dec))
check("YNOW SOP before recommendation", {
  p_sop <- regexpr("decision_sop_panel_ui", dec, fixed = TRUE)[1]
  p_rec <- regexpr('uiOutput\\(ns\\("ui_recommendation"\\)\\)', dec)[1]
  is.finite(p_sop) && p_sop > 0 && is.finite(p_rec) && p_rec > p_sop
})
check("Checklist no longer hosts SOP wizard box", {
  !grepl('id = "ynow_dc_box_sop"', substr(
    dc,
    regexpr("decision_checklist_tab_body_ui", dc, fixed = TRUE)[1],
    regexpr("decision_checklist_tab_ui", dc, fixed = TRUE)[1]
  ), fixed = TRUE)
})
check("Checklist has reloc note", grepl("ynow_dc_sop_reloc_note", dc, fixed = TRUE))
check("compact SOP class", grepl("ynow-dc-sop--compact", dc, fixed = TRUE))
check("en title simplified", identical(ui_str("dc_box_sop", "en"), "Decision SOP"))
check("zh title simplified", identical(ui_str("dc_box_sop", "zh-TW"), "決策 SOP"))
check("zh reloc no simplified", !grepl("默认|参数|数据|用户", ui_str("dc_sop_reloc_note", "zh-TW")))
check("locked hint points to SOP above", grepl("above|上方", ui_str("funnel_sop_locked_hint", "en")) ||
        grepl("上方", ui_str("funnel_sop_locked_hint", "zh-TW")))

cat("\nAll institutional SOP tests passed.\n")
