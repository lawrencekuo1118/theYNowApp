#!/usr/bin/env Rscript
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) message("PASS: ", label) else {
    fail <<- fail + 1L; message("FAIL: ", label)
  }
}
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else normalizePath("test_gold_stardust.R", mustWork = FALSE)
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
ui <- paste(readLines(file.path(root, "ynow_ui.R"), warn = FALSE), collapse = "\n")
check("stardust canvas class CSS", grepl("ynow-gold-stardust", ui, fixed = TRUE))
check("stardust init JS", grepl("initYnowGoldStardust", ui, fixed = TRUE))
check("stardust sync hook", grepl("ynowSyncGoldStardust", ui, fixed = TRUE))
check("complete uses no shine sweep", {
  grepl("Complete: steady gold fill; motion comes from stardust", ui, fixed = TRUE) &&
    grepl("ynow-app-title.is-complete .ynow-app-title-fill-inner", ui, fixed = TRUE)
})
check("stardust paused on update-available", {
  grepl("body.ynow-update-available .main-header .logo .ynow-gold-stardust", ui, fixed = TRUE)
})
check("reduced motion hides stardust", grepl("prefers-reduced-motion: reduce", ui, fixed = TRUE) &&
  grepl("ynow-gold-stardust", ui, fixed = TRUE))
check("display version v24.20", grepl("v24.20", ui, fixed = TRUE))
if (fail > 0L) { message("FAILED: ", fail); quit(status = 1L) }
message("All gold-stardust checks passed.")
