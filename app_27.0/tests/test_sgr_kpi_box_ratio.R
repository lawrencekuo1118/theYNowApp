# SGR KPI value boxes: full-width 1:1 side-by-side like BETA Overview
# (Summary β | 產業預設 β). Run:
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

# BETA Overview reference pattern: col-sm-6 host + valueBoxOutput(width=6)
beta_start <- regexpr("beta_overview_section_ui\\s*<-\\s*function", ui, perl = TRUE)[1]
beta_end <- regexpr("beta_unlever_section_ui\\s*<-\\s*function", ui, perl = TRUE)[1]
check("BETA overview function present", beta_start > 0 && beta_end > beta_start)
beta <- substr(ui, beta_start, beta_end - 1L)
check(
  "BETA Overview uses col-sm-6 + valueBoxOutput width 6",
  grepl('class\\s*=\\s*"col-sm-6"', beta, perl = TRUE) &&
    grepl('valueBoxOutput\\("vbx_beta_industry",\\s*width\\s*=\\s*6\\)', beta, perl = TRUE)
)

# SGR matches that host pattern (lock on col-sm-6; session_g via valueBoxOutput width=6)
check(
  "output:sgr_pct lock on col-sm-6 host",
  grepl(
    'class\\s*=\\s*"col-sm-6"[\\s\\S]{0,120}`data-ynow-lock`\\s*=\\s*"output:sgr_pct"',
    sgr,
    perl = TRUE
  ) || grepl(
    '`data-ynow-lock`\\s*=\\s*"output:sgr_pct"[\\s\\S]{0,120}class\\s*=\\s*"col-sm-6"',
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
  "vbx_sgr_pct is uiOutput inside col-sm-6 (BETA summary pattern)",
  grepl('shiny::uiOutput\\("vbx_sgr_pct"\\)|uiOutput\\("vbx_sgr_pct"\\)', sgr, perl = TRUE)
)
check(
  "vbx_session_g valueBoxOutput width 6 (BETA industry pattern)",
  grepl('valueBoxOutput\\("vbx_session_g",\\s*width\\s*=\\s*6\\)', sgr, perl = TRUE)
)

# Server: bare small-box (width=NULL) like Summary β
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
  "vbx_session_g valueBox width=NULL",
  grepl(
    "output\\$vbx_session_g\\s*<-\\s*renderValueBox\\([\\s\\S]{0,500}?width\\s*=\\s*NULL",
    srv,
    perl = TRUE
  )
)

# Full-width CSS (no centered mini-square clamp)
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
    "[.]ynow-sgr-kpi-row > \\[class\\*='col-'\\][\\s\\S]{0,200}?align-items:\\s*stretch",
    ui,
    perl = TRUE
  )
)

cat("\nAll SGR KPI box ratio checks passed.\n")
