#!/usr/bin/env Rscript
# Collapsible Notes / 附註 chrome: details markup, default collapsed, locales, Lite=Full
# Run: cd app_19.0 && Rscript tests/test_ynow_notes_collapse.R

root <- if (file.exists("ui_locale.R")) {
  normalizePath(".")
} else if (file.exists("../ui_locale.R")) {
  normalizePath("..")
} else if (dir.exists("app_19.0") && file.exists("app_19.0/ui_locale.R")) {
  normalizePath("app_19.0")
} else {
  stop("Cannot locate app_19.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
source("ui_locale.R", local = TRUE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("en Notes", identical(.UI_STRINGS$en$notes_title, "Notes"))
check("zh 附註", identical(.UI_STRINGS$`zh-TW`$notes_title, "附註"))
check("no 备注", !grepl("备注", .UI_STRINGS$`zh-TW`$notes_title, fixed = TRUE))
check("no 注释", !grepl("注释", .UI_STRINGS$`zh-TW`$notes_title, fixed = TRUE))
check("aria both locales", nzchar(ui_str("notes_toggle_aria", "en")) &&
        nzchar(ui_str("notes_toggle_aria", "zh-TW")))
check("helper exists", exists("ynow_notes_block", mode = "function"))

if (requireNamespace("htmltools", quietly = TRUE)) {
  collapsed <- ynow_notes_block(htmltools::tags$p(id = "demo_lead", "annotation only"))
  html <- paste(as.character(collapsed), collapse = "")
  check("markup details", grepl("<details", html, fixed = TRUE))
  check("markup class ynow-notes", grepl("ynow-notes", html, fixed = TRUE))
  check("markup summary", grepl("<summary", html, fixed = TRUE))
  check("markup body", grepl("ynow-notes__body", html, fixed = TRUE))
  check("default collapsed no open attr", !grepl("\\sopen(|=|>|\\s)", html))
  check("first-paint 附註", grepl("附註", html, fixed = TRUE))
  check("body keeps annotation", grepl("annotation only", html, fixed = TRUE))
  check("body keeps lead id", grepl("demo_lead", html, fixed = TRUE))

  opened <- ynow_notes_block(
    locale = "en",
    open = TRUE,
    htmltools::tags$p("opened")
  )
  html_open <- paste(as.character(opened), collapse = "")
  check("open=TRUE sets open", grepl("\\sopen", html_open))
  check("en title Notes", grepl(">Notes<", html_open, fixed = TRUE) ||
          grepl("Notes</span>", html_open, fixed = TRUE))
} else {
  check("htmltools available", FALSE)
}

dec <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ui_start <- regexpr("decision_ui <- function", dec, fixed = TRUE)[1]
ui_end <- regexpr("decision_momentum_panel_ui", dec, fixed = TRUE)[1]
ui_fn <- if (ui_start > 0 && ui_end > ui_start) substr(dec, ui_start, ui_end) else dec
check("Lite=Full no lite-only", !grepl("ynow-lite-only", ui_fn, fixed = TRUE))
check("Lite=Full no full-only", !grepl("ynow-full-only", ui_fn, fixed = TRUE))
.notes_calls <- function(txt) {
  calls <- character(0)
  remaining <- txt
  repeat {
    m <- regexpr("ynow_notes_block\\s*\\(", remaining)
    if (m < 1L) break
    start <- as.integer(m)
    depth <- 0L
    end <- nchar(remaining)
    for (i in seq.int(start, nchar(remaining))) {
      ch <- substr(remaining, i, i)
      if (identical(ch, "(")) depth <- depth + 1L
      if (identical(ch, ")")) {
        depth <- depth - 1L
        if (depth <= 0L) {
          end <- i
          break
        }
      }
    }
    calls <- c(calls, substr(remaining, start, end))
    remaining <- substr(remaining, end + 1L, nchar(remaining))
  }
  calls
}
notes_calls <- .notes_calls(ui_fn)
n_notes <- length(notes_calls)
check("YNOW has three chapter notes", n_notes >= 3L)
check("page sub present", grepl("ynow_funnel_page_sub", ui_fn, fixed = TRUE))
check("page sub outside notes", !any(grepl("ynow_funnel_page_sub", notes_calls, fixed = TRUE)))
pos_mh <- regexpr("ynow-funnel-report__masthead", ui_fn, fixed = TRUE)[1]
pos_kpi <- regexpr("ynow-funnel-kpi-jump-row", ui_fn, fixed = TRUE)[1]
masthead <- if (pos_mh > 0 && pos_kpi > pos_mh) substr(ui_fn, pos_mh, pos_kpi) else ""
check("page sub in masthead chrome", grepl("ynow_funnel_page_sub", masthead, fixed = TRUE))
check("masthead has no notes wrapper", !grepl("ynow_notes_block", masthead, fixed = TRUE))
check("KPI row outside notes", grepl("ynow-funnel-kpi-jump-row", ui_fn, fixed = TRUE))
check("F-Score boxes outside notes wrapper", {
  pos_box <- regexpr("fscore_panel", ui_fn, fixed = TRUE)[1]
  pos_lead <- regexpr("ynow_funnel_ch1_lead", ui_fn, fixed = TRUE)[1]
  pos_box > pos_lead && !any(grepl("fscore_panel", notes_calls, fixed = TRUE))
})
if (exists("fscore_results_ui", mode = "function") && requireNamespace("htmltools", quietly = TRUE)) {
  empty_html <- paste(as.character(fscore_results_ui(NULL, "en")), collapse = "")
  check("empty F-Score wrap visible", grepl("ynow-fscore-wrap", empty_html, fixed = TRUE))
  check("empty F-Score not in details", !grepl("<details", empty_html, fixed = TRUE))
  check("empty F-Score waiting copy", grepl("Waiting for statements", empty_html, fixed = TRUE))
  demo <- data.frame(
    `檢驗維度` = c("獲利性 (ROA > 0)", "獲利性 (OCF > 0)"),
    `得分` = c("通過", "未達標"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  cards_html <- paste(as.character(fscore_results_ui(demo, "en")), collapse = "")
  check("F-Score cards class", grepl("ynow-fscore-card", cards_html, fixed = TRUE))
  check("F-Score cards not in details", !grepl("<details", cards_html, fixed = TRUE))
  check("F-Score pass box", grepl("ynow-fscore-pass", cards_html, fixed = TRUE))
  check("F-Score fail box", grepl("ynow-fscore-fail", cards_html, fixed = TRUE))
} else {
  check("fscore_results_ui available", FALSE)
}

dc <- paste(readLines("decision_checklist_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("DC hint wrapped", grepl("ynow_notes_block", dc, fixed = TRUE) &&
        grepl("ynow_dc_panel_hint", dc, fixed = TRUE))
check("DC HFV footnote wrapped", grepl("dc_live_hfv_note", dc, fixed = TRUE) &&
        grepl("ynow_notes_block", dc, fixed = TRUE))

macro <- paste(readLines("macro_market_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
macro_ui_start <- regexpr("macro_market_ui <- function", macro, fixed = TRUE)[1]
macro_ui_end <- regexpr("# ---- Server ----", macro, fixed = TRUE)[1]
macro_ui <- if (macro_ui_start > 0 && macro_ui_end > macro_ui_start) {
  substr(macro, macro_ui_start, macro_ui_end)
} else {
  macro
}
theme_start <- regexpr("ynow_macro_theme_title", macro_ui, fixed = TRUE)[1]
theme_end <- regexpr("# Bubble & concentration", macro_ui, fixed = TRUE)[1]
theme_sec <- if (theme_start > 0 && theme_end > theme_start) {
  substr(macro_ui, theme_start, theme_end)
} else {
  macro_ui
}
theme_notes <- .notes_calls(theme_sec)
check("Macro theme help present", grepl("ynow_macro_theme_help", theme_sec, fixed = TRUE))
check("Macro theme help outside Notes", !any(grepl("ynow_macro_theme_help", theme_notes, fixed = TRUE)))
check("Macro bottom Notes is fx lock", {
  length(theme_notes) == 1L && grepl("ynow_macro_fx_lock", theme_notes[[1]], fixed = TRUE)
})
check("Macro no empty top Notes", {
  pos_pick <- regexpr("industry_key", theme_sec, fixed = TRUE)[1]
  pos_help <- regexpr("ynow_macro_theme_help", theme_sec, fixed = TRUE)[1]
  pos_help > 0 && pos_pick > pos_help &&
    !grepl("ynow_notes_block", substr(theme_sec, 1L, pos_pick), fixed = TRUE)
})
check("Macro Notes below chart", {
  pos_plot <- regexpr("overlay_plot", theme_sec, fixed = TRUE)[1]
  pos_notes <- regexpr("ynow_notes_block", theme_sec, fixed = TRUE)[1]
  pos_plot > 0 && pos_notes > pos_plot
})

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("applyUiLocale notes_title", grepl("ynow-notes__title", ui, fixed = TRUE))
check("applyUiLocale notes aria", grepl("notes_toggle_aria", ui, fixed = TRUE))

if (fail > 0L) {
  stop(sprintf("%d notes-collapse check(s) failed", fail), call. = FALSE)
}
cat("PASS ynow_notes_collapse\n")
