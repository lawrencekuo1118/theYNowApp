#!/usr/bin/env Rscript
# Assert gold stardust is removed; load-complete title uses gold shine sweep again.
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
check("no stardust canvas class", !grepl("ynow-gold-stardust", ui, fixed = TRUE))
check("no stardust init JS", !grepl("initYnowGoldStardust", ui, fixed = TRUE))
check("no stardust sync hook", !grepl("ynowSyncGoldStardust", ui, fixed = TRUE))
check("complete uses gold shine sweep", {
  grepl(
    "ynow-app-title.is-complete .ynow-app-title-fill-inner",
    ui,
    fixed = TRUE
  ) && grepl(
    "animation: ynow-gold-shine 2.6s ease-in-out infinite;",
    ui,
    fixed = TRUE
  )
})
check("gold-shine keyframes present", grepl("@keyframes ynow-gold-shine", ui, fixed = TRUE))
build_path <- file.path(root, "www", "ynow_build.json")
build_ver <- if (file.exists(build_path)) {
  raw <- paste(readLines(build_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- regmatches(raw, regexpr('"version"\\s*:\\s*"([^"]+)"', raw, perl = TRUE))
  if (length(m)) sub('.*"([^"]+)".*', "\\1", m) else ""
} else {
  ""
}
check(
  "display version matches ynow_build.json",
  nzchar(build_ver) && grepl(build_ver, ui, fixed = TRUE)
)
if (fail > 0L) { message("FAILED: ", fail); quit(status = 1L) }
message("All gold-title (no-stardust) checks passed.")
