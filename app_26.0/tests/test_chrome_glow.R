# Chrome atmosphere A+B: line-stroke veins only (no filled wash).
# Header veins beside title box; sidebar foot keeps natural-size circuit PNG;
# longer upward traces are DRAWN (SVG paths) — never stretch the chip PNG.

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
  "full-sidebar circuit overlay inject + SVG longer traces",
  grepl("ynowPlaceChromeCircuit", txt, fixed = TRUE) &&
    grepl("ynow-chrome-sidebar-circuit", txt, fixed = TRUE) &&
    grepl("ynow-chrome-sidebar-circuit-strokes", txt, fixed = TRUE) &&
    grepl("ynow-chrome-sidebar-circuit-svg", txt, fixed = TRUE) &&
    grepl("ynow-chrome-sidebar-circuit-glow", txt, fixed = TRUE) &&
    grepl("ynowPlaceChromeAtmosphere", txt, fixed = TRUE) &&
    grepl("ynowCircuitStrokeGrad", txt, fixed = TRUE) &&
    grepl("<path class=\\\"ynow-ckt-p\\\"", txt, fixed = TRUE)
)

check(
  "header veins flush left (left 0) with transparent logo + wordmark hole mask",
  grepl("\\.ynow-chrome-header-leaves \\{[\\s\\S]*?left:\\s*0;", txt, perl = TRUE) &&
    grepl(
      "\\.skin-black \\.main-header \\.logo,[\\s\\S]*?background-color:\\s*transparent\\s*!important",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "\\.main-header \\.logo \\.ynow-app-title,[\\s\\S]*?background-color:\\s*transparent\\s*!important",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynow-chrome-header-leaves \\{[\\s\\S]*?mask-image:\\s*linear-gradient\\(\\s*90deg",
      txt,
      perl = TRUE
    ) &&
    grepl("transparent 248px", txt, fixed = TRUE) &&
    grepl("\\.main-header \\.logo \\.ynow-app-title \\{[\\s\\S]*?padding:\\s*0;", txt, perl = TRUE) &&
    grepl(
      "\\.ynow-app-title\\.is-complete \\.ynow-app-title-base \\{[\\s\\S]*?visibility:\\s*hidden",
      txt,
      perl = TRUE
    ) &&
    !grepl("left:\\s*250px", txt)
)

# Restrict to each .ynow-app-title { ... } block so unrelated padding: 4px 10px
# (buttons, cookie banner) cannot false-trigger.
title_blocks <- regmatches(
  txt,
  gregexpr(
    "\\.main-header \\.logo \\.ynow-app-title \\{[^}]*\\}",
    txt,
    perl = TRUE
  )
)[[1]]
check(
  "no title-pill padding that would ghost-offset the load fill",
  length(title_blocks) > 0 &&
    !any(grepl("padding:\\s*4px\\s+10px", title_blocks, perl = TRUE)) &&
    any(grepl("padding:\\s*0", title_blocks, perl = TRUE)) &&
    !grepl(
      "Logo strip transparent so far-left strokes show; wordmark gets opaque black pill",
      txt,
      fixed = TRUE
    )
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
  "chip is black with natural-size stroke PNG (not teal panel wash)",
  grepl("\\.ynow-sidebar-foot-chip \\{[\\s\\S]*?background:\\s*#000", txt, perl = TRUE) &&
    grepl(
      "\\.ynow-sidebar-foot-chip::before \\{[\\s\\S]*?background-image:\\s*url\\(\"ynow-chrome-veins-circuit\\.png\"\\)",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynow-sidebar-foot-chip::before \\{[\\s\\S]*?background-size:\\s*118%\\s+auto",
      txt,
      perl = TRUE
    ) &&
    !grepl("inset 0 0 18px rgba\\(12, 84, 132", txt)
)

check(
  "no full-panel PNG stretch / mask-size stretch of circuit asset",
  !grepl(
    "\\.ynow-chrome-sidebar-circuit-strokes \\{[\\s\\S]*?mask-image:\\s*url\\(\"ynow-chrome-veins-circuit\\.png\"\\)",
    txt,
    perl = TRUE
  ) &&
    !grepl("mask-size:\\s*155%\\s+auto", txt, perl = TRUE) &&
    grepl("never stretch|NATURAL-size|do NOT stretch|never scale the PNG", txt, ignore.case = TRUE)
)

check(
  "full-sidebar uses drawn SVG paths + upward fade",
  grepl(
    "\\.ynow-chrome-sidebar-circuit \\{[\\s\\S]*?mask-image:\\s*linear-gradient\\(\\s*to top",
    txt,
    perl = TRUE
  ) &&
    grepl("ynow-chrome-sidebar-circuit-svg", txt, fixed = TRUE) &&
    grepl("ynow-chrome-circuit-rise", txt, fixed = TRUE) &&
    grepl("ynowCircuitStrokeGrad", txt, fixed = TRUE) &&
    grepl("transform-origin:\\s*bottom center", txt, perl = TRUE)
)

grad_start <- regexpr("ynowCircuitStrokeGrad", txt, fixed = TRUE)[1]
grad_chunk <- if (grad_start > 0) {
  substr(txt, max(1L, grad_start - 80L), min(nchar(txt), grad_start + 900L))
} else {
  ""
}
check(
  "circuit strokes use logo blue + white dynamic (no neon accents)",
  grepl("ynowCircuitStrokeGrad", grad_chunk, fixed = TRUE) &&
    grepl("#0C5484", grad_chunk, fixed = TRUE) &&
    grepl("#ffffff", grad_chunk, fixed = TRUE) &&
    grepl("animate attributeName=\\\\\"stop-color\\\\\"", grad_chunk, perl = TRUE) &&
    !grepl(
      "purple|violet|#a0[0-9a-f]{2}ff|#7[cC]3[aA][eE][dD]",
      grad_chunk,
      ignore.case = TRUE
    )
)

check(
  "chip circuit fades out upward + rises from bottom",
  grepl(
    "\\.ynow-sidebar-foot-chip::before \\{[\\s\\S]*?linear-gradient\\(to top",
    txt,
    perl = TRUE
  ) &&
    grepl("ynow-chrome-circuit-rise", txt, fixed = TRUE) &&
    grepl("transform-origin:\\s*bottom center", txt, perl = TRUE)
)

check(
  "logo hover stays above veins with transparent bg (no branch flash)",
  grepl(
    "\\.main-header \\.logo,[\\s\\S]*?\\.main-header \\.logo:hover \\{[\\s\\S]*?z-index:\\s*5",
    txt,
    perl = TRUE
  ) &&
    grepl(
      "\\.skin-black \\.main-header \\.logo:hover[\\s\\S]*?background-color:\\s*transparent\\s*!important",
      txt,
      perl = TRUE
    ) &&
    grepl("z-index:\\s*5\\s*!important", txt, perl = TRUE) &&
    !grepl(
      "\\.skin-black \\.main-header \\.logo:hover[\\s\\S]{0,120}?background-color:\\s*#000\\s*!important",
      txt,
      perl = TRUE
    )
)

check(
  "wordmark not clipped by logo overflow / tight max-width",
  grepl(
    "\\.main-header \\.logo,[\\s\\S]*?\\.main-header \\.logo:hover \\{[\\s\\S]*?overflow:\\s*visible\\s*!important",
    txt,
    perl = TRUE
  ) &&
    grepl(
      "\\.main-header \\.logo \\.ynow-app-title,[\\s\\S]*?width:\\s*max-content",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "\\.ynow-app-title\\.is-complete \\.ynow-app-title-fill \\{[\\s\\S]*?overflow:\\s*visible",
      txt,
      perl = TRUE
    ) &&
    !grepl(
      "\\.main-header \\.logo \\.ynow-app-title,[\\s\\S]*?max-width:\\s*calc\\(100% - 16px\\)",
      txt,
      perl = TRUE
    )
)

check(
  "Balanced pulse timings + reduced-motion",
  grepl("ynow-chrome-green-pulse 4.8s", txt, fixed = TRUE) &&
    grepl("ynow-chrome-blue-pulse 3.6s", txt, fixed = TRUE) &&
    grepl("ynow-chrome-circuit-rise 3.6s", txt, fixed = TRUE) &&
    grepl(
      "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-chrome-header-leaves[\\s\\S]*?animation:\\s*none",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-sidebar-foot-chip::before[\\s\\S]*?animation:\\s*none",
      txt,
      perl = TRUE
    ) &&
    grepl(
      "prefers-reduced-motion: reduce[\\s\\S]*?\\.ynow-chrome-sidebar-circuit-svg[\\s\\S]*?animation:\\s*none",
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
