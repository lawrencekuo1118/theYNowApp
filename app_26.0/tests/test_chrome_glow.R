# Chrome atmosphere A+B: line-stroke veins only (no filled wash).
# Header veins beside title box (left: 250px); circuit strokes on black chip.

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

ui_path <- file.path("..", "ynow_ui.R")
stopifnot(file.exists(ui_path))
txt <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

www_dir <- file.path("..", "www")
check <- function(label, ok) {
  if (!isTRUE(ok)) stop(sprintf("FAIL: %s", label), call. = FALSE)
  message("PASS: ", label)
}

check(
  "header leaves class + stroke PNG inject",
  grepl("ynow-chrome-header-leaves", txt, fixed = TRUE) &&
    grepl("ynow-chrome-header-veins", txt, fixed = TRUE) &&
    grepl("ynow-chrome-veins-branches.png", txt, fixed = TRUE) &&
    grepl("ynowPlaceChromeLeaves", txt, fixed = TRUE) &&
    grepl("el.setAttribute('aria-hidden','true')", txt, fixed = TRUE)
)

check(
  "sidebar foot glow + chip + circuit PNG",
  grepl("ynow-sidebar-foot-glow", txt, fixed = TRUE) &&
    grepl("ynow-sidebar-foot-chip", txt, fixed = TRUE) &&
    grepl("ynow-chrome-veins-circuit.png", txt, fixed = TRUE) &&
    file.exists(file.path(www_dir, "ynow-chrome-veins-branches.png")) &&
    file.exists(file.path(www_dir, "ynow-chrome-veins-circuit.png"))
)

check(
  "header veins placed outside title box (left 250px)",
  grepl("left:\\s*250px", txt) &&
    grepl("never inside \\.logo|outside the title", txt)
)

check(
  "no filled green/blue wash on header leaves",
  grepl(
    "\\.ynow-chrome-header-leaves \\{[\\s\\S]*?background:\\s*transparent",
    txt,
    perl = TRUE
  ) &&
    !grepl(
      "\\.ynow-chrome-header-leaves \\{[\\s\\S]*?radial-gradient\\(ellipse[^)]*ynow-chrome-green-glow",
      txt,
      perl = TRUE
    )
)

check(
  "chip is black with stroke overlay (not teal panel wash)",
  grepl("\\.ynow-sidebar-foot-chip \\{[\\s\\S]*?background:\\s*#0a0a0a", txt, perl = TRUE) &&
    grepl("ynow-chrome-veins-circuit.png", txt, fixed = TRUE) &&
    !grepl("inset 0 0 18px rgba\\(12, 84, 132", txt)
)

check(
  "Balanced pulse timings + reduced-motion",
  grepl("ynow-chrome-green-pulse 4.8s", txt, fixed = TRUE) &&
    grepl("ynow-chrome-blue-pulse 3.6s", txt, fixed = TRUE) &&
    grepl(
      "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-chrome-header-leaves[\\s\\S]*?animation:\\s*none",
      txt,
      perl = TRUE
    )
)

foot_start <- regexpr("ynow-sidebar-snapshot-foot", txt, fixed = TRUE)[1]
foot_chunk <- if (foot_start > 0) {
  substr(txt, foot_start, min(nchar(txt), foot_start + 5500L))
} else {
  ""
}
check(
  "sidebar foot has no「資料來源」blurb",
  nchar(foot_chunk) > 0 &&
    !grepl("資料來源", foot_chunk, fixed = TRUE) &&
    !grepl("Data sources", foot_chunk, ignore.case = TRUE)
)

message("All chrome glow line-stroke checks passed.")
