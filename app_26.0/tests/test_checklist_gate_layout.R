#!/usr/bin/env Rscript
# Decision Checklist guest gate: unlock + SOP demo 1:1 side-by-side;
# faded page surface under Sign-in overlay (Full guest path).
suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("test_checklist_gate_layout.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."))
setwd(app_dir)

source("ui_locale.R", encoding = "UTF-8")
ynow_auth_enabled <- function() TRUE
# tabItem comes from shinydashboard; stub when the package is unavailable offline.
if (!exists("tabItem", mode = "function")) {
  tabItem <- function(tabName, ...) {
    tags$div(class = "tab-pane", `data-value` = tabName, id = paste0("shiny-tab-", tabName), ...)
  }
}
if (!exists("icon", mode = "function")) {
  icon <- function(name, ...) tags$i(class = paste("fa fa-", name, sep = ""), ...)
}
if (!exists("box", mode = "function")) {
  box <- function(..., title = NULL, width = NULL, status = NULL, solidHeader = NULL) {
    tags$div(class = "box", tags$div(class = "box-header", title), tags$div(class = "box-body", ...))
  }
}
if (!exists("fluidRow", mode = "function")) {
  fluidRow <- function(...) tags$div(class = "row", ...)
}
if (!exists("column", mode = "function")) {
  column <- function(width, ...) tags$div(class = paste0("col-sm-", width), ...)
}
if (!exists("uiOutput", mode = "function")) {
  uiOutput <- function(outputId, ...) tags$div(id = outputId, class = "shiny-html-output", ...)
}
if (!exists("checkboxInput", mode = "function")) {
  checkboxInput <- function(inputId, label, value = FALSE, ...) {
    tags$div(tags$label(label), tags$input(id = inputId, type = "checkbox", checked = if (isTRUE(value)) NA else NULL))
  }
}
if (!exists("conditionalPanel", mode = "function")) {
  conditionalPanel <- function(condition, ...) tags$div(`data-display-if` = condition, ...)
}
suppressWarnings(source("auth_account.R", encoding = "UTF-8", local = FALSE))
suppressWarnings(source("decision_checklist_module.R", encoding = "UTF-8", local = FALSE))

n_ok <- 0L
n_fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    n_ok <<- n_ok + 1L
    message("PASS  ", label)
  } else {
    n_fail <<- n_fail + 1L
    message("FAIL  ", label)
  }
}

chk_html <- paste(as.character(ynow_member_gate_ui(
  "ynow_chk_guest_gate", "ynow_chk_gate_title", "ynow_chk_gate_body",
  "ynow_chk_gate_signin", feature = "checklist"
)), collapse = "\n")

lab_html <- paste(as.character(ynow_member_gate_ui(
  "ynow_lab_guest_gate", "ynow_lab_gate_title", "ynow_lab_gate_body",
  "ynow_lab_gate_signin", feature = "lab"
)), collapse = "\n")

check("checklist gate has surface-overlay class", {
  grepl("ynow-member-gate--surface-overlay", chk_html, fixed = TRUE)
})
check("checklist extras use promo-demo row + split class", {
  grepl("ynow-gate-extras--checklist-split", chk_html, fixed = TRUE) &&
    grepl("ynow-gate-promo-demo-row", chk_html, fixed = TRUE) &&
    grepl("ynow-gate-promo-kicker", chk_html, fixed = TRUE) &&
    grepl("ynow-gate-demo-caption", chk_html, fixed = TRUE) &&
    grepl("demos/checklist.mp4", chk_html, fixed = TRUE)
})
check("lab gate stays stacked (no checklist split)", {
  !grepl("ynow-gate-extras--checklist-split", lab_html, fixed = TRUE) &&
    !grepl("ynow-gate-promo-demo-row", lab_html, fixed = TRUE) &&
    !grepl("ynow-member-gate--surface-overlay", lab_html, fixed = TRUE) &&
    grepl("ynow-gate-promo", lab_html, fixed = TRUE) &&
    grepl("ynow-gate-demo", lab_html, fixed = TRUE)
})
check("en/zh demo captions match Decision Checklist copy", {
  identical(
    ui_str("auth_gate_checklist_demo_caption", "en"),
    "Decision Checklist demo — Decision SOP flow"
  ) &&
    identical(
      ui_str("auth_gate_checklist_demo_caption", "zh-TW"),
      "決策檢核示範——Decision SOP 流程"
    ) &&
    identical(ui_str("auth_gate_checklist_kicker", "zh-TW"), "解鎖決策檢核") &&
    identical(ui_str("auth_pa_gate_title", "zh-TW"), "需登入帳號")
})
check("fade backdrop helper renders skeleton", {
  bd <- paste(as.character(ynow_checklist_gate_fade_backdrop_ui()), collapse = "\n")
  grepl("ynow-member-gate-fade-backdrop", bd, fixed = TRUE) &&
    grepl("ynow-dc-fade-skel", bd, fixed = TRUE) &&
    grepl('aria-hidden="true"', bd, fixed = TRUE)
})

tab_html <- paste(as.character(decision_checklist_tab_ui()), collapse = "\n")
check("checklist tab wraps surface + fade + gate + body", {
  grepl("ynow-member-gate-surface--checklist", tab_html, fixed = TRUE) &&
    grepl("ynow-member-gate-fade-backdrop", tab_html, fixed = TRUE) &&
    grepl("ynow_chk_guest_gate", tab_html, fixed = TRUE) &&
    grepl("ynow_chk_body", tab_html, fixed = TRUE) &&
    grepl("ynow-member-gate--surface-overlay", tab_html, fixed = TRUE) &&
    grepl("ynow-gate-promo-demo-row", tab_html, fixed = TRUE)
})

ui_txt <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
check("UI CSS defines 1:1 row + surface overlay fade", {
  grepl("ynow-gate-promo-demo-row", ui_txt, fixed = TRUE) &&
    grepl("ynow-gate-extras--checklist-split", ui_txt, fixed = TRUE) &&
    grepl("flex: 1 1 0", ui_txt, fixed = TRUE) &&
    grepl("ynow-member-gate--surface-overlay", ui_txt, fixed = TRUE) &&
    grepl("ynow-member-gate-fade-backdrop", ui_txt, fixed = TRUE) &&
    grepl("rgba(255, 255, 255", ui_txt, fixed = TRUE)
})

message(sprintf("\n%d passed, %d failed", n_ok, n_fail))
quit(status = if (n_fail > 0L) 1L else 0L)
