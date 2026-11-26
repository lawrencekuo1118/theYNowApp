# DCF「二階段」sub-tab visibility: only when dcf_mode == two_stage.
# Static checks (no Shiny session): CSS first-paint hide + server class toggle +
# safe fallback to dcf_overview. Locale keys for the tab stay en-US / zh-TW.

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK  ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

ui <- paste(readLines("../ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines("../ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "CSS hides dcf_two_stage tab by default",
  grepl(
    'nav-tabs > li:has\\(> a\\[data-value=\\\\"dcf_two_stage\\\\"\\]\\)',
    ui,
    perl = TRUE
  ) || grepl(
    'a\\[data-value=\\"dcf_two_stage\\"\\]',
    ui,
    perl = TRUE
  )
)
check(
  "CSS shows tab when html.ynow-dcf-two-stage-on",
  grepl("ynow-dcf-two-stage-on", ui, fixed = TRUE)
)
check(
  "DCF shell still hosts two-stage tabPanel",
  grepl('value = "dcf_two_stage"', ui, fixed = TRUE)
)
check(
  "dcf_mode radio offers gordon + two_stage",
  grepl('choiceValues = list("gordon", "two_stage")', ui, fixed = TRUE)
)

check(
  "server toggles ynow-dcf-two-stage-on from dcf_mode",
  grepl("ynow-dcf-two-stage-on", srv, fixed = TRUE) &&
    grepl("observeEvent\\(input\\$dcf_mode", srv, perl = TRUE)
)
# Fallback pieces from the dcf_mode observer (R source keeps \" inside the JS string).
check(
  "server detects active dcf_two_stage before hide",
  grepl(
    "a[data-value=\\\"dcf_two_stage\\\"]').parent().hasClass('active')",
    srv,
    fixed = TRUE
  )
)
check(
  "server shows dcf_overview as safe fallback tab",
  grepl(
    "a[data-value=\\\"dcf_overview\\\"]').tab('show')",
    srv,
    fixed = TRUE
  )
)

source("../ui_locale.R", local = TRUE)
check(
  "en-US two-stage tab label",
  identical(ui_str("tab_dcf_two_stage", "en"), "Two-Stage")
)
check(
  "zh-TW two-stage tab label",
  identical(ui_str("tab_dcf_two_stage", "zh-TW"), "二階段")
)
check(
  "en-US two-stage mode radio label present",
  nzchar(ui_str("dcf_mode_two_stage", "en"))
)
check(
  "zh-TW two-stage mode radio uses 二階段成長法 wording",
  grepl("二階段", ui_str("dcf_mode_two_stage", "zh-TW"), fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All DCF two-stage tab gate checks passed.\n")
