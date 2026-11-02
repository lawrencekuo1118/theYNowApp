# Mobile valuation 試算 + 回復預設: same row, equal height/width 1:1 (≤767px).
# Desktop Bootstrap layout unchanged (col-sm-6 / col-sm-4).
# Run:
#   cd app_27.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_mobile_calc_reset_buttons.R

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

setup_src <- paste(readLines("setup.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ri_src <- paste(readLines("ri_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
nav_src <- paste(readLines("nav_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pb_src <- paste(readLines("pb_asset_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sotp_src <- paste(readLines("sotp_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
rel_src <- paste(readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "ynow_calc_reset_row helper defined",
  grepl("ynow_calc_reset_row\\s*<-\\s*function", setup_src, perl = TRUE)
)
check(
  "helper uses col-xs-6 for calc/reset pair",
  grepl('class\\s*=\\s*"col-xs-6 ynow-calc-reset-col"', setup_src, perl = TRUE)
)
check(
  "helper with-sync uses col-xs-12 for sync",
  grepl('class\\s*=\\s*"col-xs-12 ynow-calc-reset-sync-col"', setup_src, perl = TRUE)
)

check(
  "mobile CSS media query present",
  grepl(
    "@media\\s*\\(max-width:\\s*767px\\)\\s*\\{[\\s\\S]*?\\.ynow-calc-reset-row",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS forces 50% width on calc/reset cols",
  grepl(
    "\\.ynow-calc-reset-row\\s*>\\s*\\.ynow-calc-reset-col\\s*\\{[\\s\\S]*?width:\\s*50%\\s*!important",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS equal-height flex on buttons",
  grepl(
    "\\.ynow-calc-reset-row\\s*>\\s*\\.ynow-calc-reset-col\\s*>\\s*\\.btn\\s*\\{[\\s\\S]*?height:\\s*100%",
    ui_src,
    perl = TRUE
  )
)
check(
  "mobile CSS sync full-width below",
  grepl(
    "ynow-calc-reset-row--with-sync\\s*>\\s*\\.ynow-calc-reset-sync-col\\s*\\{[\\s\\S]*?width:\\s*100%\\s*!important",
    ui_src,
    perl = TRUE
  )
)

check("DCF uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", ui_src) && grepl('ynow_calc_btn\\("calc"', ui_src))
check("DDM uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", ui_src) && grepl("mod_ddm-btn_calc_ddm", ui_src))
check("RI uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", ri_src))
check("NAV uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", nav_src))
check("P/B uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", pb_src))
check("SOTP uses ynow_calc_reset_row with sync_ui", grepl("ynow_calc_reset_row\\(", sotp_src) && grepl("sync_ui\\s*=", sotp_src))
check("Multiples .rel_run_row uses ynow_calc_reset_row", grepl("ynow_calc_reset_row\\(", rel_src) && grepl("sync_ui\\s*=", rel_src))

# No leftover bare fluidRow calc/reset pairs without the shared helper class
# (pair sites should go through ynow_calc_reset_row).
legacy_pair <- function(src) {
  grepl(
    "column\\(\\s*width\\s*=\\s*6[^)]*ynow_calc_btn[\\s\\S]{0,200}ynow_reset_defaults_btn",
    src,
    perl = TRUE
  )
}
check("no legacy DCF/DDM width-6 fluidRow pairs in ynow_ui.R", !legacy_pair(ui_src))
check("no legacy RI width-6 fluidRow pairs", !legacy_pair(ri_src))
check("no legacy NAV width-6 fluidRow pairs", !legacy_pair(nav_src))
check("no legacy P/B width-6 fluidRow pairs", !legacy_pair(pb_src))

cat("\nAll mobile calc/reset button checks passed.\n")
