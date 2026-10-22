# Chrome atmosphere: Balanced logo-blue sidebar foot glow + logo-green header leaves.
# Decorative only — no sidebar「資料來源」blurb; reduced-motion respected.

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

ui_path <- file.path("..", "ynow_ui.R")
stopifnot(file.exists(ui_path))
txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check <- function(label, ok) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  message("PASS: ", label)
}

check(
  "header leaves class + aria-hidden inject",
  grepl("ynow-chrome-header-leaves", txt, fixed = TRUE) &&
    grepl("ynowPlaceChromeLeaves", txt, fixed = TRUE) &&
    grepl("el.setAttribute('aria-hidden','true')", txt, fixed = TRUE)
)

check(
  "sidebar foot glow + chip wrappers",
  grepl("ynow-sidebar-foot-glow", txt, fixed = TRUE) &&
    grepl("ynow-sidebar-foot-chip", txt, fixed = TRUE) &&
    grepl("ynow-sidebar-snapshot-foot-links", txt, fixed = TRUE) &&
    grepl("ynow_sidebar_copyright", txt, fixed = TRUE)
)

chrome_vars <- regmatches(
  txt,
  regexpr(
    "--ynow-chrome-blue-glow:[\\s\\S]*?--ynow-chrome-blue-halo:[^;]+;",
    txt,
    perl = TRUE
  )
)
leaves_css <- regmatches(
  txt,
  regexpr(
    "\\.ynow-chrome-header-leaves \\{[\\s\\S]*?ynow-chrome-green-pulse",
    txt,
    perl = TRUE
  )
)
check(
  "Balanced uses logo blue/green tokens (not purple)",
  length(chrome_vars) == 1L &&
    grepl("rgba(12, 84, 132", chrome_vars, fixed = TRUE) &&
    grepl("rgba(36, 156, 96", chrome_vars, fixed = TRUE) &&
    length(leaves_css) == 1L &&
    grepl("stroke='%23249C60'", leaves_css, fixed = TRUE) &&
    !grepl("#7c3aed|#a855f7|#2ea8e6|#3ecf8e", paste(chrome_vars, leaves_css), ignore.case = TRUE)
)

check(
  "Balanced pulse timings match preview",
  grepl("ynow-chrome-green-pulse 4.8s", txt, fixed = TRUE) &&
    grepl("ynow-chrome-blue-pulse 3.6s", txt, fixed = TRUE) &&
    grepl("blur(6px)", txt, fixed = TRUE)
)

check(
  "prefers-reduced-motion disables chrome glow animations",
  grepl(
    "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-chrome-header-leaves[\\s\\S]*?animation:\\s*none",
    txt,
    perl = TRUE
  ) &&
    grepl(
      "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-sidebar-foot-glow[\\s\\S]*?animation:\\s*none",
      txt,
      perl = TRUE
    )
)

# Sidebar foot column must not reintroduce the moved-to-About data-sources blurb.
foot_start <- regexpr("ynow-sidebar-snapshot-foot", txt, fixed = TRUE)[1]
foot_chunk <- if (foot_start > 0) {
  substr(txt, foot_start, min(nchar(txt), foot_start + 4500L))
} else {
  ""
}
check(
  "sidebar foot has no「資料來源」blurb",
  nchar(foot_chunk) > 0 &&
    !grepl("資料來源", foot_chunk, fixed = TRUE) &&
    !grepl("Data sources", foot_chunk, ignore.case = TRUE) &&
    !grepl("DATA · LIVE", foot_chunk, fixed = TRUE)
)

message("All chrome glow checks passed.")
