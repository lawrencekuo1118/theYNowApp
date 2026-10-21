# test_ynow_no_prereq_gate.R — YNOW verdict/composite must not require SOP unlock
# Run: Rscript app_26.0/tests/test_ynow_no_prereq_gate.R

.app_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1L && nzchar(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
if (basename(.app_dir) == "tests") .app_dir <- dirname(.app_dir)
if (!file.exists(file.path(.app_dir, "investment_decision_module.R"))) {
  .app_dir <- normalizePath(file.path(getwd(), "app_26.0"), mustWork = FALSE)
}
setwd(.app_dir)

source("ui_locale.R", local = TRUE)

check <- function(lab, cond) {
  cat(if (isTRUE(cond)) "OK  " else "FAIL", lab, "\n")
  if (!isTRUE(cond)) quit(status = 1L)
}

dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# .sop_unlocked is a constant TRUE — never gates on sop_verdict_gate$unlocked
check(
  ".sop_unlocked always TRUE",
  grepl("\\.sop_unlocked <- function\\(\\) TRUE", dec)
)
check(
  "ui_recommendation has no lock hint",
  !grepl("funnel_sop_locked_hint", {
    rec_start <- regexpr("output\\$ui_recommendation", dec)[1]
    cmp_start <- regexpr("output\\$ui_valuation_compare", dec)[1]
    if (is.finite(rec_start) && rec_start > 0 && is.finite(cmp_start) && cmp_start > rec_start) {
      substr(dec, rec_start, cmp_start - 1L)
    } else {
      ""
    }
  }, fixed = TRUE)
)
check(
  "ui_recommendation has no ynow-sop-verdict-locked",
  !grepl("ynow-sop-verdict-locked", {
    rec_start <- regexpr("output\\$ui_recommendation", dec)[1]
    cmp_start <- regexpr("output\\$ui_valuation_compare", dec)[1]
    if (is.finite(rec_start) && rec_start > 0 && is.finite(cmp_start) && cmp_start > rec_start) {
      substr(dec, rec_start, cmp_start - 1L)
    } else {
      ""
    }
  }, fixed = TRUE)
)

cmp_start <- regexpr("output\\$ui_valuation_compare", dec)[1]
cmp_chunk <- if (is.finite(cmp_start) && cmp_start > 0) substr(dec, cmp_start, nchar(dec)) else ""
check(
  "composite does not gate status on SOP locked string",
  !grepl("composite_sop_locked", cmp_chunk, fixed = TRUE)
)
check(
  "composite waiting path uses composite_waiting_val",
  grepl("composite_waiting_val", cmp_chunk, fixed = TRUE)
)

# Locale: independent analysis, not unlock prerequisite
en_sub <- ui_str("funnel_page_sub", "en")
zh_sub <- ui_str("funnel_page_sub", "zh-TW")
en_lead <- ui_str("funnel_verdict_lead", "en")
zh_lead <- ui_str("funnel_verdict_lead", "zh-TW")
check("en page_sub no unlock-only wording", !grepl("unlocks only after", en_sub, ignore.case = TRUE))
check("en page_sub says independent / no SOP required", grepl("no Decision Checklist", en_sub, fixed = TRUE) ||
        grepl("no Decision Checklist / Decision SOP unlock is required", en_sub, fixed = TRUE))
check("zh page_sub 不需先完成", grepl("不需先完成", zh_sub, fixed = TRUE))
check("en verdict_lead no Full-mode unlock gate", !grepl("unlock via Decision Checklist", en_lead, fixed = TRUE))
check("zh verdict_lead 不需先解鎖", grepl("不需先解鎖", zh_lead, fixed = TRUE))

# Decision Checklist tab still exists (SOP workflow intact)
dc <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("Decision Checklist SOP panel retained", grepl("decision_sop_panel_ui", dc, fixed = TRUE))

cat("ALL PASS\n")
