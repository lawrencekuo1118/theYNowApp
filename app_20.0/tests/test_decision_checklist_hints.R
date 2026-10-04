# Checklist & Conditions: investor-facing labels + brief hints (no code param ids).
# Run: cd app_20.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_decision_checklist_hints.R

root <- if (file.exists("decision_checklist_module.R")) {
  getwd()
} else if (file.exists("app_20.0/decision_checklist_module.R")) {
  file.path(getwd(), "app_20.0")
} else {
  stop("Cannot find app_20.0 root")
}
setwd(root)

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})
if (!exists("ynow_notes_block", mode = "function")) {
  ynow_notes_block <<- function(...) tags$div(class = "ynow-notes", ...)
}
source("ui_locale.R", local = FALSE)
source("decision_checklist_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

cond_keys <- c(
  "bear_mos_floor", "base_mos_floor", "g_sgr_gap_min",
  "sgr_wacc_buffer", "max_c_freq", "fscore_min", "user_primary"
)
item_keys <- names(.DC_ITEM_DEFS)

for (loc in c("en", "zh-TW")) {
  for (item in item_keys) {
    check(
      paste(loc, "gate hint", item),
      nzchar(ui_str(paste0("dc_hint_", item), loc)) &&
        !grepl(paste0("^", item, "$"), ui_str(paste0("dc_hint_", item), loc))
    )
    check(
      paste(loc, "gate label not code id", item),
      !identical(ui_str(paste0("dc_label_", item), loc), item)
    )
  }
  for (cn in cond_keys) {
    lab <- ui_str(paste0("dc_cond_", cn), loc)
    hint <- ui_str(paste0("dc_cond_hint_", cn), loc)
    check(paste(loc, "cond label", cn), nzchar(lab) && !identical(lab, cn))
    check(paste(loc, "cond hint", cn), nzchar(hint) && !identical(hint, cn))
    # No snake_case / formula-style code residue in visible labels
    check(
      paste(loc, "cond label investor wording", cn),
      !grepl("_", lab, fixed = TRUE) &&
        !grepl("\\|", lab) &&
        !grepl("Min \\(", lab)
    )
  }
}

# Initial UI must not pass raw condition ids as numericInput labels
ui_txt <- as.character(htmltools::doRenderTags(.dc_checks_ui()))
banned_labels <- c(
  "bear_mos_floor", "base_mos_floor", "g_sgr_gap_min",
  "sgr_wacc_buffer", "max_c_freq", "fscore_min"
)
# Labels appear as text nodes; input ids still contain cond_…_bear_mos_floor — that is OK.
# Detect bare label text by looking for >snake_case< style control-label content.
for (b in banned_labels) {
  check(
    paste("UI avoids raw control label", b),
    !grepl(sprintf(">[\\s]*%s[\\s]*<", b), ui_txt, perl = TRUE)
  )
}
check("UI ships gate hints", grepl("ynow-dc-hint", ui_txt, fixed = TRUE))
check("UI ships cond hints", grepl("ynow-dc-cond-hint", ui_txt, fixed = TRUE))
check(
  "baked gate hint not empty",
  grepl("Margin of Safety|Piotroski|Value Trap|Model Selector", ui_txt)
)
mod_src <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "module re-applies locale on tab visit",
  grepl('identical(as.character(input$sidebar_tabs)[1], "decision_checklist")', mod_src, fixed = TRUE)
)

cat("PASS test_decision_checklist_hints\n")
