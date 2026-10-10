# Institutional SOP unlock logic
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_institutional_sop.R

root <- if (file.exists("decision_checklist_module.R")) {
  getwd()
} else if (file.exists(file.path("..", "decision_checklist_module.R"))) {
  normalizePath(file.path(".."))
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

# Placement: Decision SOP on Decision Checklist; YNOW recommendation shows locked hint only
source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("investment_decision_module.R", local = TRUE, encoding = "UTF-8")
dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dc <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("SOP mount helper exported", exists("decision_sop_panel_ui", mode = "function"))
check("YNOW no longer mounts SOP panel", !grepl("decision_sop_panel_ui\\(\\)", dec))
check("Checklist hosts SOP panel", {
  body_start <- regexpr("decision_checklist_tab_body_ui", dc, fixed = TRUE)[1]
  body_end <- regexpr("decision_checklist_tab_ui", dc, fixed = TRUE)[1]
  chunk <- if (body_start > 0 && body_end > body_start) substr(dc, body_start, body_end) else ""
  grepl("decision_sop_panel_ui()", chunk, fixed = TRUE) &&
    grepl('id = "ynow_dc_sop_panel"', dc, fixed = TRUE)
})
check("Checklist SOP before summary box", {
  body_start <- regexpr("decision_checklist_tab_body_ui", dc, fixed = TRUE)[1]
  body_end <- regexpr("decision_checklist_tab_ui", dc, fixed = TRUE)[1]
  chunk <- if (body_start > 0 && body_end > body_start) substr(dc, body_start, body_end) else ""
  p_sop <- regexpr("decision_sop_panel_ui", chunk, fixed = TRUE)[1]
  p_sum <- regexpr("ynow_dc_box_summary", chunk, fixed = TRUE)[1]
  is.finite(p_sop) && p_sop > 0 && is.finite(p_sum) && p_sum > p_sop
})
check("Checklist dropped reloc note", !grepl("ynow_dc_sop_reloc_note", dc, fixed = TRUE))
check("reloc locale key removed", is.null(.UI_STRINGS$en$dc_sop_reloc_note) &&
        is.null(.UI_STRINGS$`zh-TW`$dc_sop_reloc_note))
check("compact SOP class", grepl("ynow-dc-sop--compact", dc, fixed = TRUE))
check("en title simplified", identical(ui_str("dc_box_sop", "en"), "Decision SOP"))
check("zh title simplified", identical(ui_str("dc_box_sop", "zh-TW"), "決策 SOP"))
check(
  "locked hint points to Decision Checklist",
  grepl("Decision Checklist", ui_str("funnel_sop_locked_hint", "en"), fixed = TRUE) &&
    grepl("決策檢核", ui_str("funnel_sop_locked_hint", "zh-TW"), fixed = TRUE)
)
check(
  "unused locked title/body keys removed",
  is.null(.UI_STRINGS$en$funnel_sop_locked_title) &&
    is.null(.UI_STRINGS$en$funnel_sop_locked_body) &&
    is.null(.UI_STRINGS$`zh-TW`$funnel_sop_locked_title) &&
    is.null(.UI_STRINGS$`zh-TW`$funnel_sop_locked_body)
)
# Hint is for YNOW recommendation only — not the shared composite header on model pages.
rec_start <- regexpr("output\\$ui_recommendation", dec)[1]
cmp_start <- regexpr("output\\$ui_valuation_compare", dec)[1]
rec_chunk <- if (is.finite(rec_start) && rec_start > 0 && is.finite(cmp_start) && cmp_start > rec_start) {
  substr(dec, rec_start, cmp_start - 1L)
} else {
  ""
}
cmp_chunk <- if (is.finite(cmp_start) && cmp_start > 0) substr(dec, cmp_start, nchar(dec)) else ""
check(
  "YNOW recommendation uses locked hint",
  grepl("funnel_sop_locked_hint", rec_chunk, fixed = TRUE)
)
check(
  "composite valuation omits YNOW unlock hint",
  !grepl("funnel_sop_locked_hint", cmp_chunk, fixed = TRUE)
)
check(
  "composite still shows SOP locked status",
  grepl("composite_sop_locked", cmp_chunk, fixed = TRUE)
)

cat("\nAll institutional SOP tests passed.\n")
