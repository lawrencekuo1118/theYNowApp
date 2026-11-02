#!/usr/bin/env Rscript
# Animated orange-gold focus-KPI frame + legend placement above FINANCIAL REPORT.
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
  normalizePath("test_gold_kpi_animated_frame.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
www_svg <- file.path(app_dir, "www", "ynow-gold-kpi-frame.svg")
www_ref <- file.path(app_dir, "www", "ynow-gold-kpi-frame-ref.jpg")

check("SVG frame asset present", file.exists(www_svg))
check("JPG visual reference present", file.exists(www_ref))
check("SVG has animated stroke", {
  svg <- paste(readLines(www_svg, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("<animate", svg, fixed = TRUE) && grepl("FF8C1A", svg, fixed = TRUE)
})

check("UI loads SVG frame on focus boxes", grepl("ynow-gold-kpi-frame.svg", ui, fixed = TRUE))
check("UI ember animation", grepl("@keyframes ynow-focus-gold-ember", ui, fixed = TRUE))
check("UI frame spin animation", grepl("@keyframes ynow-focus-gold-frame-spin", ui, fixed = TRUE))
check("static 3px gold border removed", !grepl("border: 3px solid var(--ynow-gold", ui, fixed = TRUE))
check("legacy pulse keyframes removed", !grepl("@keyframes ynow-focus-gold-pulse", ui, fixed = TRUE))

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

check("locale en gold legend", grepl(
  'kpi_legend_focus_metric = "Gold · Financial-report attribute key metric"',
  loc,
  fixed = TRUE
))
check("locale zh-TW gold legend", grepl(
  'kpi_legend_focus_metric = "金 · 本財報屬性關鍵指標"',
  loc,
  fixed = TRUE
))

if (fail > 0L) {
  quit(status = 1L)
}
message("ALL PASS: test_gold_kpi_animated_frame.R")
