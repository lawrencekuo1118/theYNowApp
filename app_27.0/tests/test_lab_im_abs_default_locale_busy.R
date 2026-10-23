#!/usr/bin/env Rscript
# Blue Chip: Absolute defaults on / Relative off; locale switch keeps busy chrome.
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK" else "FAIL", ":", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
root <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
if (!file.exists(file.path(root, "default_config.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
# lab_industry_method.R sources sibling files with relative paths.
setwd(root)

# Minimal sources for APP_DEFAULTS
source(file.path(root, "industry_standards.R"), local = TRUE, encoding = "UTF-8")
# market_profile stub if needed
if (!exists("market_profile", mode = "function")) {
  market_profile <- function(mode = "US") list(rf_fallback = 5.0)
}
source(file.path(root, "default_config.R"), local = TRUE, encoding = "UTF-8")
source(file.path(root, "lab_industry_method.R"), local = TRUE, encoding = "UTF-8")

abs <- as.character(LAB_METHOD_ABS_KEYS)
rel <- as.character(LAB_METHOD_REL_KEYS)
defs <- as.character(APP_DEFAULTS$lab_im_methods)

check("defaults include all Absolute", identical(sort(defs), sort(abs)))
check("defaults exclude all Relative", length(intersect(defs, rel)) == 0L)
check("pb is Absolute and default-on", "pb" %in% defs && "pb" %in% abs)

ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("setMenuLabel preserves busy dots", grepl("ynow-tab-busy-dots", ui, fixed = TRUE) &&
        grepl("busyDotsClone", ui, fixed = TRUE))
check("reapply busy after applyUiLocale", grepl("ynowReapplyAllTabBusyVisuals", ui, fixed = TRUE))
check("schedule picker locale helper", grepl("\\.lab_im_schedule_picker_locale", srv))
check("defaults used when methods empty", grepl("APP_DEFAULTS\\$lab_im_methods", srv) &&
        grepl("Empty \\(pre-bind", srv))
check(
  "skip checkbox clear when available empty",
  grepl("Catalog not ready yet", srv, fixed = TRUE) &&
    grepl("keep HTML Absolute defaults", srv, fixed = TRUE)
)
check(
  "snap stale select-all away from Relative",
  grepl("Stale \"select all\"", srv, fixed = TRUE) ||
    grepl("Stale 'select all'", srv, fixed = TRUE) ||
    grepl("snap back to Absolute defaults", srv, fixed = TRUE)
)
check("defer while eval busy", grepl("lab_im_eval_busy", srv) &&
        grepl("later::later\\(run, delay = 0.35\\)", srv))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("ALL PASSED\n")
