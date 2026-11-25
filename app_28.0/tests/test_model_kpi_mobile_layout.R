# Valuation model Overview KPIs on phone (≤767px):
# white-outline hero → full-width row; secondary KPIs → 1:1 equal-height pair.
# Desktop Bootstrap column widths stay unchanged outside the media query.
# Run: cd app_28.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_model_kpi_mobile_layout.R

root <- if (file.exists("ynow_ui.R")) {
  getwd()
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else if (file.exists("app_28.0/ynow_ui.R")) {
  file.path(getwd(), "app_28.0")
} else {
  stop("Cannot locate app_28.0 root")
}
setwd(root)

check <- function(label, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  cat("OK:", label, "\n")
}

.css_path <- if (file.exists("www/ynow_widgets.css")) {
  "www/ynow_widgets.css"
} else {
  stop("Cannot locate www/ynow_widgets.css")
}
css <- paste(readLines(.css_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Modules that ship a white-outline hero KPI
mod_files <- c(
  "nav_module.R",
  "pb_asset_module.R",
  "ri_module.R",
  "sotp_module.R",
  "ynow_ui.R"
)
for (f in mod_files) {
  src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  check(
    paste(f, "has ynow-model-kpi-row + outline hero"),
    grepl("ynow-model-kpi-row", src, fixed = TRUE) &&
      grepl("ynow-model-kpi-result ynow-model-kpi-outline", src, fixed = TRUE)
  )
}

rel_src <- paste(readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "Multiples keep filled result (no outline) so mobile stack stays default",
  grepl("ynow-model-kpi-row", rel_src, fixed = TRUE) &&
    !grepl("ynow-model-kpi-outline", rel_src, fixed = TRUE)
)

# Phone CSS lives in the ≤767px block of ynow_widgets.css
check(
  "mobile media query hosts model-kpi-row outline layout",
  grepl(
    "@media\\s*\\(max-width:\\s*767px\\)\\s*\\{[\\s\\S]*?\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\)",
    css,
    perl = TRUE
  )
)
check(
  "mobile CSS uses 2-column grid for outline KPI rows",
  grepl(
    "\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\)[\\s\\S]{0,220}?grid-template-columns:\\s*repeat\\(2,\\s*minmax\\(0,\\s*1fr\\)\\)",
    css,
    perl = TRUE
  )
)
check(
  "mobile CSS hero spans full row (grid-column 1 / -1)",
  grepl(
    "\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\) > \\.ynow-model-kpi-outline[\\s\\S]{0,120}?grid-column:\\s*1\\s*/\\s*-1",
    css,
    perl = TRUE
  )
)
check(
  "mobile CSS odd last secondary spans full width",
  grepl(
    ":not\\(\\.ynow-model-kpi-outline\\):last-child:nth-child\\(even\\)[\\s\\S]{0,80}?grid-column:\\s*1\\s*/\\s*-1",
    css,
    perl = TRUE
  )
)
check(
  "mobile CSS kills Bootstrap clearfix on outline KPI rows",
  grepl(
    "\\.row\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\)::before[\\s\\S]{0,200}?content:\\s*none\\s*!important",
    css,
    perl = TRUE
  )
)
check(
  "mobile CSS stretches secondary boxes to equal height",
  grepl(
    "\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\) \\.small-box[\\s\\S]{0,200}?height:\\s*100%\\s*!important",
    css,
    perl = TRUE
  ) &&
    grepl(
      "\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\) \\.info-box[\\s\\S]{0,200}?height:\\s*100%\\s*!important",
      css,
      perl = TRUE
    )
)
check(
  "outline layout is phone-scoped (not a desktop-wide grid override)",
  {
    # First occurrence of the outline grid rule must sit after a 767px media query open
    # that has not been closed yet — approximate by requiring the media query before the rule
    # and that desktop/wide Bootstrap cols for model KPI rows are not forced to 50% outside.
    pos_mq <- regexpr("@media\\s*\\(max-width:\\s*767px\\)", css, perl = TRUE)[1]
    pos_rule <- regexpr(
      "\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\)[\\s\\S]{0,80}?display:\\s*grid\\s*!important",
      css,
      perl = TRUE
    )[1]
    is.finite(pos_mq) && is.finite(pos_rule) && pos_mq > 0 && pos_rule > pos_mq &&
      !grepl(
        "@media\\s*\\(min-width:\\s*768px\\)\\s*\\{[\\s\\S]*?\\.ynow-model-kpi-row:has\\(> \\.ynow-model-kpi-outline\\)[\\s\\S]{0,120}?display:\\s*grid",
        css,
        perl = TRUE
      )
  }
)

cat("\nAll model KPI mobile layout checks passed.\n")
