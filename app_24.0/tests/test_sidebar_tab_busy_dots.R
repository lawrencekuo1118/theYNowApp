#!/usr/bin/env Rscript
# Sidebar tab busy dots: Loading → animated 「…」 on menu label
# Run: cd app_22.0/tests && Rscript test_sidebar_tab_busy_dots.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("CSS busy dots class", grepl("ynow-tab-busy-dots", ui, fixed = TRUE))
check("CSS is-busy class", grepl("ynow-tab-is-busy", ui, fixed = TRUE))
check("CSS keyframes pulse", grepl("ynow-tab-dot-pulse", ui, fixed = TRUE))
check("JS initYnowSidebarTabBusyDots", grepl("initYnowSidebarTabBusyDots", ui, fixed = TRUE))
check("JS custom handler ynowTabBusy", grepl("ynowTabBusy", ui, fixed = TRUE))
check("JS shiny:busy hook", grepl("shiny:busy", ui, fixed = TRUE) &&
        grepl("onShinyBusy", ui, fixed = TRUE))
check("JS shiny:idle clears", grepl("shiny:idle", ui, fixed = TRUE) &&
        grepl("onShinyIdle", ui, fixed = TRUE))
check("JS busy is immediate (no arm delay)", {
  busy_js <- regmatches(
    ui,
    regexpr("/\\* Sidebar tab label:[\\s\\S]*?window\\.ynowSetSidebarTabBusy = setTabBusy;", ui, perl = TRUE)
  )
  length(busy_js) == 1L &&
    grepl("Show 「…」 as soon as the loading screen", busy_js, fixed = TRUE) &&
    grepl("setTabBusy(tab, true, 'shiny')", busy_js, fixed = TRUE) &&
    !grepl(", 220)", busy_js, fixed = TRUE)
})
check("JS marks busy on sidebar tab click",
      grepl("onSidebarTabClick", ui, fixed = TRUE) &&
        grepl("setTabBusy(tab, true, 'nav')", ui, fixed = TRUE))
check("JS progress DOM watcher",
      grepl("watchProgressDom", ui, fixed = TRUE) &&
        grepl("setTabBusy(tab, true, 'progress')", ui, fixed = TRUE))
check("JS exposes ynowSetSidebarTabBusy", grepl("ynowSetSidebarTabBusy", ui, fixed = TRUE))
check("JS reapply busy after locale", grepl("ynowReapplyAllTabBusyVisuals", ui, fixed = TRUE))
check("setMenuLabel keeps busy dots", grepl("busyDotsClone", ui, fixed = TRUE))
check("JS exposes ynowCurrentSidebarTab", grepl("ynowCurrentSidebarTab", ui, fixed = TRUE))
check("JS creates three dot spans", grepl("<span>\\\\.<\\\\/span><span>\\\\.<\\\\/span><span>\\\\.<\\\\/span>", ui) ||
        grepl("innerHTML = '<span>.</span><span>.</span><span>.</span>'", ui, fixed = TRUE))

check("server .ynow_set_tab_busy", grepl("\\.ynow_set_tab_busy", srv))
check("server sendCustomMessage ynowTabBusy", grepl("ynowTabBusy", srv, fixed = TRUE))
check("server mirrors bluechip eval busy",
      grepl('\\.ynow_set_tab_busy\\(\"bluechip\"', srv) ||
        grepl("\\.ynow_set_tab_busy\\(\"bluechip\"", srv))
check("server company scrape marks tab",
      grepl("\\.ynow_busy_tab_now\\(\"dashboard\"\\)", srv) ||
        grepl('.ynow_busy_tab_now("dashboard")', srv, fixed = TRUE))
check("server cluster marks lab_notes",
      grepl('\\.ynow_set_tab_busy\\(\"lab_notes\"', srv) ||
        grepl('.ynow_set_tab_busy("lab_notes"', srv, fixed = TRUE))
check("server HFV marks hfv",
      grepl('.ynow_set_tab_busy("hfv"', srv, fixed = TRUE))

# No simplified Chinese in this feature's CSS/JS comments block
busy_block <- regmatches(
  ui,
  regexpr("/\\* Sidebar tab label:[\\s\\S]*?\\}\\)\\(\\);", ui, perl = TRUE)
)
check("busy JS block present", length(busy_block) == 1L && nzchar(busy_block))
check("no simplified 加载 in busy block",
      length(busy_block) == 0L || !grepl("加载", busy_block, fixed = TRUE))

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
