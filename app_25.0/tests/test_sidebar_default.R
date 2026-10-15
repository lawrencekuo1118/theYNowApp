# test_sidebar_default.R — first visit: sidebar starts collapsed
#
# Run: YNOW_DEBUG_SKIP_PY=1 Rscript app_21.0/tests/test_sidebar_default.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.script_dir <- if (length(.file_arg)) {
  dirname(normalizePath(sub("^--file=", "", .file_arg[[1]]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
.root <- normalizePath(file.path(.script_dir, ".."), mustWork = TRUE)
.fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) .fail <<- .fail + 1L
  invisible(ok)
}

ui <- paste(readLines(file.path(.root, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# dashboardSidebar(..., collapsed = TRUE) near the app chrome (not box collapsibles)
m <- regexpr("dashboardSidebar\\s*\\([\\s\\S]{0,200}?collapsed\\s*=\\s*(TRUE|FALSE)", ui, perl = TRUE)
chunk <- if (m[1] > 0) regmatches(ui, m)[[1]] else ""
check("dashboardSidebar present", grepl("dashboardSidebar", chunk, fixed = TRUE))
check("sidebar default collapsed TRUE", grepl("collapsed\\s*=\\s*TRUE", chunk))
check("sidebar not default expanded", !grepl("collapsed\\s*=\\s*FALSE", chunk))

if (.fail > 0L) {
  cat("\n", .fail, " check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("\nAll sidebar-default checks passed.\n")
