# test_page_width_unify.R — all tabs share Macro page max-width + responsive shell
#
# Run: YNOW_DEBUG_SKIP_PY=1 Rscript app_21.0/tests/test_page_width_unify.R

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

check("CSS var --ynow-page-max", grepl("--ynow-page-max:\\s*1200px", ui))
check("tab-pane uses page max", grepl(
  "tab-content > .tab-pane \\{[\\s\\S]*?max-width:\\s*var\\(--ynow-page-max",
  ui,
  perl = TRUE
))
check("tab-pane centered", grepl(
  "tab-content > .tab-pane \\{[\\s\\S]*?margin-left:\\s*auto[\\s\\S]*?margin-right:\\s*auto",
  ui,
  perl = TRUE
))
check("mobile tab-pane full width", grepl(
  "@media \\(max-width:\\s*767px\\) \\{[\\s\\S]*?tab-content > .tab-pane \\{[\\s\\S]*?max-width:\\s*100%",
  ui,
  perl = TRUE
))

shells <- c(
  ".ynow-macro-report",
  ".ynow-hfv-report",
  ".ynow-funnel-report",
  ".ynow-backtest-report",
  ".ynow-bblab--report",
  ".ynow-home"
)
for (sel in shells) {
  # Each shell should reference the shared var (not a narrower hard max)
  pat <- paste0(
    gsub("\\.", "\\\\.", sel),
    " \\{[\\s\\S]*?max-width:\\s*var\\(--ynow-page-max"
  )
  check(paste("shell", sel), grepl(pat, ui, perl = TRUE))
}

check("home no longer 760px max", !grepl("\\.ynow-home \\{[\\s\\S]*?max-width:\\s*760px", ui, perl = TRUE))
check("bblab no longer 980px max", !grepl("\\.ynow-bblab--report \\{[\\s\\S]*?max-width:\\s*980px", ui, perl = TRUE))
check("home responsive 3-col desktop", grepl(
  "@media \\(min-width:\\s*992px\\) \\{[\\s\\S]*?\\.ynow-home-grid \\{[^}]*grid-template-columns:\\s*1fr 1fr 1fr",
  ui,
  perl = TRUE
))
check("plotly/DT fluid in tab-pane", grepl(
  "tab-pane \\.plotly[\\s\\S]*?max-width:\\s*100%",
  ui,
  perl = TRUE
) && grepl("tab-pane .dataTables_wrapper", ui, fixed = TRUE))
check("mobile shell max-width after report rules", grepl(
  "After report-shell rules: force full bleed[\\s\\S]*?@media \\(max-width:\\s*767px\\) \\{[\\s\\S]*?\\.ynow-macro-report,[\\s\\S]*?max-width:\\s*100%",
  ui,
  perl = TRUE
))

if (.fail > 0L) {
  cat("\n", .fail, " check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("\nAll page-width unify checks passed.\n")
