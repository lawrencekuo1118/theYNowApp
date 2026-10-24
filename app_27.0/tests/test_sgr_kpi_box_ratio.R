# SGR KPI value boxes: equal 1:1 columns + square aspect-ratio CSS.
# Run: cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_sgr_kpi_box_ratio.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else if (file.exists("app_27.0/ynow_ui.R")) {
  file.path(getwd(), "app_27.0")
} else {
  stop("Cannot locate app_27.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Isolate .dcf_core_params_box body (SGR tabBox)
sgr_start <- regexpr("\\.dcf_core_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
sgr_end <- regexpr("\\.dcf_two_stage_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
check("SGR box function present", sgr_start > 0 && sgr_end > sgr_start)
sgr <- substr(ui, sgr_start, sgr_end - 1L)

check(
  "SGR KPI row class present",
  grepl('class\\s*=\\s*"ynow-sgr-kpi-row"', sgr, perl = TRUE)
)

# Lock on column host (FCF-style), not a bare tags$div wrapper around width=6 output
check(
  "output:sgr_pct lock on column host",
  grepl(
    'column\\(\\s*6\\s*,\\s*`data-ynow-lock`\\s*=\\s*"output:sgr_pct"',
    sgr,
    perl = TRUE
  )
)
check(
  "no bare tags$div lock wrapping vbx_sgr_pct width=6",
  !grepl(
    'tags\\$div\\(`data-ynow-lock`\\s*=\\s*"output:sgr_pct".*valueBoxOutput\\("vbx_sgr_pct",\\s*width\\s*=\\s*6\\)',
    sgr,
    perl = TRUE
  )
)
check(
  "vbx_sgr_pct width 12 inside column",
  grepl('valueBoxOutput\\("vbx_sgr_pct",\\s*width\\s*=\\s*12\\)', sgr, perl = TRUE)
)
check(
  "vbx_session_g width 12 inside column",
  grepl('valueBoxOutput\\("vbx_session_g",\\s*width\\s*=\\s*12\\)', sgr, perl = TRUE)
)

# Square aspect CSS scoped to SGR KPI row only
check(
  "CSS aspect-ratio 1/1 for ynow-sgr-kpi-row",
  grepl(".ynow-sgr-kpi-row .small-box", ui, fixed = TRUE) &&
    grepl("aspect-ratio:\\s*1\\s*/\\s*1\\s*!important", ui, perl = TRUE)
)
check(
  "SGR square CSS overrides height stretch",
  grepl(
    "[.]ynow-sgr-kpi-row [.]small-box[\\s\\S]{0,500}?height:\\s*clamp\\(",
    ui,
    perl = TRUE
  ) &&
    grepl(
      "[.]ynow-sgr-kpi-row [.]small-box[\\s\\S]{0,500}?width:\\s*auto\\s*!important",
      ui,
      perl = TRUE
    )
)

# KPI-grid golden ratio must not be the SGR rule
kpi_grid_ar <- regexpr(
  "\\.ynow-kpi-grid \\.small-box\\s*\\{[^}]*aspect-ratio:\\s*1\\.618",
  ui,
  perl = TRUE
)[1]
sgr_ar <- regexpr(
  "\\.ynow-sgr-kpi-row \\.small-box[\\s\\S]{0,200}?aspect-ratio:\\s*1\\s*/\\s*1",
  ui,
  perl = TRUE
)[1]
check("SGR uses 1/1 not golden-ratio grid rule", sgr_ar > 0 && kpi_grid_ar > 0)

cat("\nAll SGR KPI box ratio checks passed.\n")
