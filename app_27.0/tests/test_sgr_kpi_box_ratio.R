# SGR KPI value boxes: always 1:1 side-by-side equal height (desktop + mobile).
# SGR | Current configured g (near-term). Run:
#   cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_sgr_kpi_box_ratio.R

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
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Isolate .dcf_core_params_box body (SGR tabBox)
sgr_start <- regexpr("\\.dcf_core_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
sgr_end <- regexpr("\\.dcf_two_stage_params_box\\s*<-\\s*function", ui, perl = TRUE)[1]
check("SGR box function present", sgr_start > 0 && sgr_end > sgr_start)
sgr <- substr(ui, sgr_start, sgr_end - 1L)

check(
  "SGR KPI row class present",
  grepl('class\\s*=\\s*"ynow-sgr-kpi-row"', sgr, perl = TRUE)
)

# Both hosts are col-xs-6 so BS3 never stacks below 768px
check(
  "output:sgr_pct lock on col-xs-6 host",
  grepl(
    'class\\s*=\\s*"col-xs-6"[\\s\\S]{0,120}`data-ynow-lock`\\s*=\\s*"output:sgr_pct"',
    sgr,
    perl = TRUE
  ) || grepl(
    '`data-ynow-lock`\\s*=\\s*"output:sgr_pct"[\\s\\S]{0,120}class\\s*=\\s*"col-xs-6"',
    sgr,
    perl = TRUE
  )
)
check(
  "vbx_session_g host is col-xs-6 + uiOutput (not valueBoxOutput width=6)",
  grepl(
    'class\\s*=\\s*"col-xs-6"[\\s\\S]{0,160}uiOutput\\("vbx_session_g"\\)',
    sgr,
    perl = TRUE
  ) &&
    !grepl('valueBoxOutput\\("vbx_session_g"', sgr, perl = TRUE)
)
check(
  "vbx_sgr_pct is uiOutput inside col-xs-6",
  grepl('shiny::uiOutput\\("vbx_sgr_pct"\\)|uiOutput\\("vbx_sgr_pct"\\)', sgr, perl = TRUE)
)
check(
  "no bare tags$div lock wrapping vbx_sgr_pct width=6",
  !grepl(
    'tags\\$div\\(`data-ynow-lock`\\s*=\\s*"output:sgr_pct".*valueBoxOutput\\("vbx_sgr_pct",\\s*width\\s*=\\s*6\\)',
    sgr,
    perl = TRUE
  )
)

# Server: bare small-box (width=NULL) via renderUI for both
check(
  "vbx_sgr_pct renderUI with width=NULL",
  grepl("output\\$vbx_sgr_pct\\s*<-\\s*renderUI", srv, perl = TRUE) &&
    grepl(
      "output\\$vbx_sgr_pct\\s*<-\\s*renderUI\\([\\s\\S]{0,600}?width\\s*=\\s*NULL",
      srv,
      perl = TRUE
    )
)
check(
  "vbx_session_g renderUI with width=NULL",
  grepl("output\\$vbx_session_g\\s*<-\\s*renderUI", srv, perl = TRUE) &&
    grepl(
      "output\\$vbx_session_g\\s*<-\\s*renderUI\\([\\s\\S]{0,500}?width\\s*=\\s*NULL",
      srv,
      perl = TRUE
    )
)

# CSS: flex 1:1 nowrap + equal height at all breakpoints
check(
  "CSS row flex nowrap for ynow-sgr-kpi-row",
  grepl(
    "[.]row[.]ynow-sgr-kpi-row[\\s\\S]{0,200}?flex-wrap:\\s*nowrap\\s*!important",
    ui,
    perl = TRUE
  )
)
check(
  "CSS columns flex 1:1 width 50%",
  grepl(
    "[.]ynow-sgr-kpi-row > \\[class\\*='col-'\\][\\s\\S]{0,250}?flex:\\s*1 1 0\\s*!important[\\s\\S]{0,120}?width:\\s*50%\\s*!important",
    ui,
    perl = TRUE
  )
)
check(
  "CSS full-width for ynow-sgr-kpi-row small-box",
  grepl(".ynow-sgr-kpi-row .small-box", ui, fixed = TRUE) &&
    grepl(
      "[.]ynow-sgr-kpi-row [.]small-box[\\s\\S]{0,400}?width:\\s*100%\\s*!important",
      ui,
      perl = TRUE
    )
)
check(
  "SGR CSS does not shrink with width:auto + height clamp",
  !grepl(
    "[.]ynow-sgr-kpi-row [.]small-box[\\s\\S]{0,500}?width:\\s*auto\\s*!important",
    ui,
    perl = TRUE
  ) &&
    !grepl(
      "[.]ynow-sgr-kpi-row [.]small-box[\\s\\S]{0,500}?height:\\s*clamp\\(",
      ui,
      perl = TRUE
    )
)
check(
  "SGR columns stretch (not center-align mini tiles)",
  grepl(
    "[.]ynow-sgr-kpi-row > \\[class\\*='col-'\\][\\s\\S]{0,400}?align-items:\\s*stretch",
    ui,
    perl = TRUE
  )
)
check(
  "mobile stack rule excludes ynow-sgr-kpi-row",
  grepl(
    "row:not\\(\\.ynow-header-kpi-row\\):not\\(\\.ynow-sgr-kpi-row\\)\\s*>\\s*\\.col-sm-6:has\\(\\.small-box\\)",
    ui,
    perl = TRUE
  )
)
check(
  "mobile CSS keeps SGR cols at 50%",
  grepl(
    "@media\\s*\\(max-width:\\s*767px\\)\\s*\\{[\\s\\S]*?\\.ynow-sgr-kpi-row > \\[class\\*='col-'\\][\\s\\S]{0,200}?width:\\s*50%\\s*!important",
    ui,
    perl = TRUE
  )
)
check(
  "mobile CSS labels wrap without ellipsis on SGR row",
  grepl(
    "[.]ynow-sgr-kpi-row [.]small-box [.]inner p[\\s\\S]{0,250}?white-space:\\s*normal\\s*!important[\\s\\S]{0,120}?text-overflow:\\s*unset",
    ui,
    perl = TRUE
  )
)

cat("\nAll SGR KPI box ratio checks passed.\n")
