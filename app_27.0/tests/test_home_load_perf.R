# Home first-paint: MathJax off critical path; gate demos do not autoplay-prefetch.
# Run: cd app_27.0 && Rscript tests/test_home_load_perf.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

app_dir <- if (file.exists("ynow_ui.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate ynow_ui.R")
}

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Extract .valuation_methodology_section_ui body (until next top-level assign at col 1)
meth_start <- regexpr("\\.valuation_methodology_section_ui\\s*<-\\s*function", ui, perl = TRUE)[1]
check("methodology section exists", meth_start > 0L)
meth_chunk <- if (meth_start > 0L) {
  substr(ui, meth_start, min(nchar(ui), meth_start + 25000L))
} else {
  ""
}
# Stop at next function assign that is clearly outside (heuristic: ".ynow_" or similar)
# Keep first 20k which covers the withMathJax call site.
meth_chunk <- substr(meth_chunk, 1L, 20000L)

check("methodology does not call shiny::withMathJax()", {
  # No executable call line (comments may still name withMathJax).
  !grepl("(^|\\n)\\s*withMathJax\\s*\\(", meth_chunk, perl = TRUE)
})
check("methodology marks deferred MathJax need", {
  grepl("ynow-needs-mathjax", meth_chunk, fixed = TRUE)
})
check("UI ships deferred MathJax loader", {
  grepl("ynowEnsureMathJax", ui, fixed = TRUE) &&
    grepl("ynowTypesetMathJax", ui, fixed = TRUE) &&
    grepl("MathJax.js?config=TeX-AMS-MML_HTMLorMML", ui, fixed = TRUE) &&
    grepl("s.async = true", ui, fixed = TRUE)
})
check("deferred loader does not block head via withMathJax singleton pattern", {
  # Loader creates script with async=true; classic withMathJax uses tags$script(src=) in head singleton
  grepl("createElement('script')", ui, fixed = TRUE) &&
    grepl("ynow-needs-mathjax", ui, fixed = TRUE)
})

# Lazy pages may still use withMathJax (not in first HTML).
check("HFV / Lab still request MathJax when mounted", {
  grepl("\\.ynow_page_ui_hfv\\s*<-\\s*function[\\s\\S]{0,400}?withMathJax\\(\\)", ui, perl = TRUE) &&
    grepl("\\.ynow_page_ui_lab_notes\\s*<-\\s*function[\\s\\S]{0,400}?withMathJax\\(\\)", ui, perl = TRUE)
})

check("gate demo videos preload=none and no autoplay", {
  grepl('preload = "none"', auth, fixed = TRUE) &&
    grepl("ynow-gate-demo-video", auth, fixed = TRUE) &&
    !grepl("autoplay = NA", auth, fixed = TRUE) &&
    grepl("ynowArmGateDemoVideos", ui, fixed = TRUE)
})

check("dashboardBody comment documents Home MathJax skip", {
  grepl("never on Home first paint", ui, fixed = TRUE) ||
    grepl("rather than on every first paint of Home", ui, fixed = TRUE)
})

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
quit(status = 0)
