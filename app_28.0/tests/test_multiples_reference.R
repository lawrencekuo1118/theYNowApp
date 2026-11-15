#!/usr/bin/env Rscript
# Multiples as universal Reference (never secondary).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) message("PASS: ", label) else {
    fail <<- fail + 1L
    message("FAIL: ", label)
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_multiples_reference.R", mustWork = FALSE)
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
source(file.path(root, "ui_locale.R"), local = TRUE)

ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE), collapse = "\n")
vg <- paste(readLines(file.path(root, "valuation_guard.R"), warn = FALSE), collapse = "\n")
# C-STRUCT-03: industry defaults live in lab_im_bluechip_eval.R (sourced by facade).
lab <- paste(
  c(
    readLines(file.path(root, "lab_im_bluechip_eval.R"), warn = FALSE),
    readLines(file.path(root, "lab_industry_method.R"), warn = FALSE)
  ),
  collapse = "\n"
)

check("locale Reference en", identical(ui_str("menu_badge_reference", "en"), "Reference"))
check("locale 參照 zh", identical(ui_str("menu_badge_reference", "zh-TW"), "參照"))
check("JS does not put reference on the sidebar", !grepl("roles.indexOf('reference')", ui, fixed = TRUE))
check("server does not assign a sidebar reference badge", !grepl('return("reference")', srv, fixed = TRUE))
check("model selector keeps a reference position strip", grepl("ms_pos_reference", srv, fixed = TRUE))
check("model selector folds P/B into the Multiples card", !grepl('make_card("P/B"', srv, fixed = TRUE) &&
        grepl("ms_family_book", srv, fixed = TRUE))
check("sec allowlist excludes multiples", grepl(
  'sec_auto_allowlist <- c("dcf", "ddm", "pb", "ri", "nav", "sotp")', srv, fixed = TRUE
))
check(
  "empty Fair Value primary does not fall through to DCF band",
  grepl("fv_ok <- prim %in% c(\"dcf\", \"ddm\", \"pb\", \"nav\", \"ri\")", srv, fixed = TRUE) &&
    grepl("return(.model_empty_band(\"\"))", srv, fixed = TRUE) &&
    !grepl("dcf_scenario_band()\n    )\n    if (is.null(band)) band <- list(bear = NA_real_", srv, fixed = TRUE)
)
check(
  "Lite auto-calc allows Multiples Reference without Fair Value primary",
  grepl("ref_only <- !isTRUE(fv_prim) && identical(ref, \"multiples\")", srv, fixed = TRUE) &&
    grepl("req(isTRUE(fv_prim) || isTRUE(ref_only))", srv, fixed = TRUE)
)
check("smart analysis labels insufficient Fair Value primary", {
  identical(ui_str("smart_no_fv_primary", "en"), "No usable Fair Value") &&
    identical(ui_str("smart_no_fv_primary", "zh-TW"), "無適用 Fair Value") &&
    grepl("smart_no_fv_primary", srv, fixed = TRUE)
})
check("guard excludes multiples from secondary pool", grepl('scores$model != "multiples"', vg, fixed = TRUE))
check("guard sets reference multiples", grepl('reference <- "multiples"', vg, fixed = TRUE))
check("guard removed SaaS multiples secondary override", !grepl(
  "Prefer it over same-family FV\n  # secondaries", vg
))
check("lab SaaS secondary RI", grepl(
  'list("saas.SaaS_Cloud", "dcf", "ri"', lab, fixed = TRUE
))
check("lab Internet secondary RI", grepl(
  'list("tech.Internet_Platform", "dcf", "ri"', lab, fixed = TRUE
))

if (fail > 0L) quit(status = 1L)
message("ALL PASS: test_multiples_reference.R")
