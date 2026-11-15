#!/usr/bin/env Rscript
# Vine/foliage focus-KPI border (transparent center) + legend above FINANCIAL REPORT.
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    message("PASS: ", label)
  } else {
    fail <<- fail + 1L
    message("FAIL: ", label)
  }
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
test_file <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
} else {
  normalizePath("test_vine_kpi_frame.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
www_png <- file.path(app_dir, "www", "ynow-vine-kpi-frame.png")
www_ref <- file.path(app_dir, "www", "ynow-vine-kpi-frame-ref.jpg")

check("PNG vine frame asset present", file.exists(www_png))
check("JPG visual reference present", file.exists(www_ref))
check("legacy gold SVG removed", !file.exists(file.path(app_dir, "www", "ynow-gold-kpi-frame.svg")))
check("legacy gold JPG ref removed", !file.exists(file.path(app_dir, "www", "ynow-gold-kpi-frame-ref.jpg")))

check("PNG is RGBA with transparent center", {
  if (!requireNamespace("png", quietly = TRUE)) {
    # Fallback: file magic + nonzero size; full alpha check when png avail.
    size <- file.info(www_png)$size
    con <- file(www_png, "rb")
    magic <- readBin(con, "raw", 8L)
    close(con)
    identical(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), magic) &&
      isTRUE(size > 1000)
  } else {
    img <- png::readPNG(www_png)
    # Expect alpha channel and hollow center.
    if (length(dim(img)) < 3L || dim(img)[[3]] < 4L) return(FALSE)
    h <- dim(img)[[1]]
    w <- dim(img)[[2]]
    cy <- max(1L, as.integer(h / 2))
    cx <- max(1L, as.integer(w / 2))
    # Center alpha near 0; some border pixels opaque.
    center_a <- img[cy, cx, 4]
    border_a <- max(img[, , 4], na.rm = TRUE)
    isTRUE(center_a < 0.05) && isTRUE(border_a > 0.5)
  }
})

check("UI loads vine PNG on focus boxes", grepl("ynow-vine-kpi-frame.png", ui, fixed = TRUE))
check("UI vine breathe animation", grepl("@keyframes ynow-focus-vine-breathe", ui, fixed = TRUE))
check("legacy gold ember removed", !grepl("@keyframes ynow-focus-gold-ember", ui, fixed = TRUE))
check("legacy gold frame-spin removed", !grepl("@keyframes ynow-focus-gold-frame-spin", ui, fixed = TRUE))
check("legacy gold SVG url removed", !grepl("ynow-gold-kpi-frame.svg", ui, fixed = TRUE))
check("static 3px gold border removed", !grepl("border: 3px solid var(--ynow-gold", ui, fixed = TRUE))
check("legacy pulse keyframes removed", !grepl("@keyframes ynow-focus-gold-pulse", ui, fixed = TRUE))

# Perimeter wrap: 9-slice border-image (not stretched full-bleed background).
check("focus frame uses border-image-source", grepl("border-image-source: url('ynow-vine-kpi-frame.png')", ui, fixed = TRUE))
check("focus frame uses border-image-slice", grepl("border-image-slice:", ui, fixed = TRUE))
check("focus frame uses border-image-outset", grepl("border-image-outset:", ui, fixed = TRUE))
check("no stretch full-bleed vine background", {
  !grepl("ynow-vine-kpi-frame.png') center / 100% 100%", ui, fixed = TRUE) &&
    !grepl('ynow-vine-kpi-frame.png") center / 100% 100%', ui, fixed = TRUE)
})
check("FS focus uses thinner vine band", {
  grepl("ynow-fs-card.ynow-focus-metric-box::before", ui, fixed = TRUE) &&
    grepl("border-image-width: 20px", ui, fixed = TRUE)
})

# Border-only: no ::after fill ring / gold wash.
check("focus ::after disabled (no fill wash)", {
  grepl("ynow-focus-metric-box::after", ui, fixed = TRUE) &&
    grepl("content: none !important", ui, fixed = TRUE)
})
check("no gold gradient fill on focus frame", {
  !grepl("#FF8C1A", ui, fixed = TRUE) ||
    !grepl("ynow-focus-metric-box::after[\\s\\S]{0,400}#FF8C1A", ui)
})
check("no inset / ember gold wash on focus box", {
  !grepl("inset 0 0 10px rgba\\(255, 201, 51", ui) &&
    !grepl("0 0 22px 4px rgba\\(255, 106, 0", ui)
})
check("focus frame overlay pointer-events none", {
  grepl("ynow-focus-metric-box::before", ui, fixed = TRUE) &&
    grepl("pointer-events: none", ui, fixed = TRUE)
})
# Vertical center: stacking must not force .icon-large into flex flow.
check("focus stacking elevates small-box .inner only", {
  grepl(".small-box.ynow-focus-metric-box > .inner", ui, fixed = TRUE) &&
    !grepl(".small-box.ynow-focus-metric-box > *", ui, fixed = TRUE)
})
check("FS focus stacking still elevates children", {
  grepl(".ynow-fs-card.ynow-focus-metric-box > *", ui, fixed = TRUE)
})

check("legend note helper", grepl(".ynow_focus_metric_legend_note_ui <- function", ui, fixed = TRUE))
check("legend note class above FS", grepl("ynow-focus-metric-note--above-fs", ui, fixed = TRUE))
check("legend note right-aligned CSS", {
  grepl("ynow-dashboard-fin-report-wrap > .ynow-focus-metric-note--above-fs", ui, fixed = TRUE) &&
    grepl("justify-content: flex-end", ui, fixed = TRUE)
})
check("legend note before FINANCIAL REPORT tabBox", {
  i_note <- regexpr("\\.ynow_focus_metric_legend_note_ui\\(\\)", ui)[1]
  i_fr <- regexpr('tabBox\\(title = "FINANCIAL REPORT"', ui)[1]
  i_note > 0 && i_fr > 0 && i_note < i_fr
})
check("KPI color legend no longer hosts focus note", {
  m <- regexpr(
    "(?s)\\.kpi_band_color_legend_ui <- function\\(\\) \\{.*?\\n\\}",
    ui,
    perl = TRUE
  )
  block <- if (m[1] > 0) regmatches(ui, m)[[1]] else ""
  nzchar(block) &&
    grepl("ynow_kpi_legend_blue", block, fixed = TRUE) &&
    !grepl("ynow_kpi_legend_focus", block, fixed = TRUE)
})

check("locale en vine legend", grepl(
  'kpi_legend_focus_metric = "Vine · Financial-report attribute key metric"',
  loc,
  fixed = TRUE
))
check("locale zh-TW vine legend", grepl(
  'kpi_legend_focus_metric = "藤蔓 · 本財報屬性關鍵指標"',
  loc,
  fixed = TRUE
))

if (fail > 0L) {
  quit(status = 1L)
}
message("ALL PASS: test_vine_kpi_frame.R")
