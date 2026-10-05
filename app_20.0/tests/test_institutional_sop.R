# Institutional SOP unlock logic
# Run: cd app_20.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_institutional_sop.R

root <- if (file.exists("decision_checklist_module.R")) {
  getwd()
} else if (file.exists("app_20.0/decision_checklist_module.R")) {
  file.path(getwd(), "app_20.0")
} else {
  stop("Cannot find app_20.0 root")
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

cat("\nAll institutional SOP tests passed.\n")
