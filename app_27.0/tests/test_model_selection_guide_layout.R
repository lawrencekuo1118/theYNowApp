#!/usr/bin/env Rscript
# Model Selection Guide: under Fundamental Valuation Models chart, collapsed,
# tightened tables; formula sibling tabs stay in methodology section.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_model_selection_guide_layout.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_27.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("ynow_ui.R")
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

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("guide panel helper", grepl(
  "\\.model_selection_guide_panel_ui\\s*<-\\s*function",
  ui
))
check("matrix body helper", grepl(
  "\\.model_selection_guide_matrix_body\\s*<-\\s*function",
  ui
))

guide_start <- regexpr(
  "\\.model_selection_guide_panel_ui\\s*<-\\s*function",
  ui
)[1]
dims_ann <- regexpr(
  "\\.model_selector_dimensions_annotation_ui\\s*<-\\s*function",
  ui
)[1]
meth_start <- regexpr(
  "\\.valuation_methodology_section_ui\\s*<-\\s*function",
  ui
)[1]
check(
  "helper order: guide panel before dims annotation",
  guide_start > 0 && dims_ann > guide_start && meth_start > dims_ann
)

guide_slice <- if (guide_start > 0 && dims_ann > guide_start) {
  substr(ui, guide_start, dims_ann - 1L)
} else {
  ""
}
check(
  "guide details default collapsed",
  grepl("tags$details", guide_slice, fixed = TRUE) &&
    grepl("ynow_ms_guide_details", guide_slice, fixed = TRUE) &&
    !grepl("\\bopen\\s*=", guide_slice)
)

gs_m <- regexpr(
  'tabName = "get_started"[\\s\\S]*?tabName = "snapshot"',
  ui,
  perl = TRUE
)
gs_ok <- gs_m[1] > 0
gs_chunk <- if (gs_ok) {
  substr(ui, gs_m[1], gs_m[1] + attr(gs_m, "match.length") - 1L)
} else {
  ""
}
pos_title <- if (gs_ok) regexpr("ynow_gs_model_selector_title", gs_chunk, fixed = TRUE)[1] else -1L
pos_ui <- if (gs_ok) {
  regexpr('uiOutput("get_started_model_selector")', gs_chunk, fixed = TRUE)[1]
} else {
  -1L
}
pos_guide <- if (gs_ok) {
  regexpr(".model_selection_guide_panel_ui()", gs_chunk, fixed = TRUE)[1]
} else {
  -1L
}
pos_ann <- if (gs_ok) {
  regexpr(".model_selector_dimensions_annotation_ui()", gs_chunk, fixed = TRUE)[1]
} else {
  -1L
}
pos_method <- if (gs_ok) {
  regexpr(".valuation_methodology_section_ui(", gs_chunk, fixed = TRUE)[1]
} else {
  -1L
}
check(
  "mount order: chart UI → guide → dims → methodology",
  gs_ok &&
    pos_title > 0 &&
    pos_ui > pos_title &&
    pos_guide > pos_ui &&
    pos_ann > pos_guide &&
    pos_method > pos_ann
)

meth_slice <- if (meth_start > 0) {
  m_end <- regexpr("\\.shared_bridge_box\\s*<-\\s*function", ui)[1]
  if (m_end > meth_start) substr(ui, meth_start, m_end - 1L) else ""
} else {
  ""
}
check(
  "methodology has no Decision Matrix tab",
  nchar(meth_slice) > 0 &&
    !grepl("ynow_method_tab_matrix", meth_slice, fixed = TRUE) &&
    !grepl("ynow_method_matrix_fv_note", meth_slice, fixed = TRUE)
)
check(
  "methodology keeps formula siblings",
  grepl("Dividend Discount Model (DDM)", meth_slice, fixed = TRUE) &&
    grepl("Discounted Cash Flow (DCF)", meth_slice, fixed = TRUE) &&
    grepl("Residual Income (RI)", meth_slice, fixed = TRUE)
)
check(
  "methodology tabBox retitled Model Formulas",
  grepl('ynow_method_tabbox_title", "Model Formulas"', meth_slice, fixed = TRUE) ||
    grepl("ynow_method_tabbox_title\", \"Model Formulas\"", meth_slice, fixed = TRUE)
)

check("tightened EN FV header Item", grepl("<th>Item</th>", ui, fixed = TRUE))
check("tightened ZH FV header 項目", grepl("<th>項目</th>", ui, fixed = TRUE))
check("no old Comparison header in guide", {
  body_start <- regexpr(
    "\\.model_selection_guide_matrix_body\\s*<-\\s*function",
    ui
  )[1]
  body_end <- regexpr(
    "\\.model_selection_guide_panel_ui\\s*<-\\s*function",
    ui
  )[1]
  slice <- if (body_start > 0 && body_end > body_start) {
    substr(ui, body_start, body_end - 1L)
  } else {
    ""
  }
  !grepl("<th>Comparison</th>", slice, fixed = TRUE) &&
    grepl("<th>Item</th>", slice, fixed = TRUE)
})

check("pos chart height 72px", grepl("height:72px", srv, fixed = TRUE))
check("pos track class", grepl("ynow-ms-pos-track", srv, fixed = TRUE))
check("ms-guide CSS", grepl(".ynow-ms-guide", ui, fixed = TRUE))

check("applyUiLocale guide title", grepl("ynow_ms_guide_title", ui, fixed = TRUE) &&
        grepl("box_model_guide", ui, fixed = TRUE))
check("applyUiLocale guide aria", grepl("ynow_ms_guide_summary", ui, fixed = TRUE) &&
        grepl("gs_ms_guide_toggle_aria", ui, fixed = TRUE))
check("notes aria skips guide summary", grepl(
  "if (el.id === 'ynow_ms_guide_summary') return;",
  ui,
  fixed = TRUE
))
check("applyUiLocale formulas title", grepl("method_formulas_title", ui, fixed = TRUE))

check("en guide toggle aria", nzchar(.UI_STRINGS$en$gs_ms_guide_toggle_aria))
check("zh-TW guide toggle aria", nzchar(.UI_STRINGS$`zh-TW`$gs_ms_guide_toggle_aria))
check("en formulas title", identical(
  .UI_STRINGS$en$method_formulas_title,
  "Model Formulas"
))
check("zh-TW formulas title", identical(
  .UI_STRINGS$`zh-TW`$method_formulas_title,
  "模型公式"
))
check("zh-TW no simplified 选择", !grepl(
  "选择",
  .UI_STRINGS$`zh-TW`$box_model_guide,
  fixed = TRUE
))
check("callout points under chart en", grepl(
  "under the Fundamental Valuation Models chart",
  .UI_STRINGS$en$explain_core_callout_body,
  fixed = TRUE
))
check("callout points under chart zh", grepl(
  "基本面評價模型圖表下方",
  .UI_STRINGS$`zh-TW`$explain_core_callout_body,
  fixed = TRUE
))

if (requireNamespace("htmltools", quietly = TRUE)) {
  env <- new.env(parent = globalenv())
  env$tags <- htmltools::tags
  env$HTML <- htmltools::HTML
  env$tagList <- htmltools::tagList
  m_body <- regexpr(
    "\\.model_selection_guide_matrix_body\\s*<-\\s*function\\s*\\(\\)\\s*\\{[\\s\\S]*?\\n\\}",
    ui,
    perl = TRUE
  )
  m_panel <- regexpr(
    "\\.model_selection_guide_panel_ui\\s*<-\\s*function\\s*\\(\\)\\s*\\{[\\s\\S]*?\\n\\}",
    ui,
    perl = TRUE
  )
  check("parse matrix body", m_body[1] > 0)
  check("parse guide panel", m_panel[1] > 0)
  if (m_body[1] > 0 && m_panel[1] > 0) {
    eval(parse(text = substr(ui, m_body[1], m_body[1] + attr(m_body, "match.length") - 1L)), envir = env)
    eval(parse(text = substr(ui, m_panel[1], m_panel[1] + attr(m_panel, "match.length") - 1L)), envir = env)
    html <- paste(as.character(env$.model_selection_guide_panel_ui()), collapse = "")
    check("rendered details", grepl("<details", html, fixed = TRUE))
    check("rendered no open", !grepl("\\sopen(|=|>|\\s)", html))
    check("rendered title id", grepl("ynow_ms_guide_title", html, fixed = TRUE))
    check("rendered FV table", grepl("FCFF / FCFE", html, fixed = TRUE) || grepl("FCFF／FCFE", html, fixed = TRUE))
  }
}

if (fail > 0L) {
  stop(sprintf("%d model-selection-guide layout check(s) failed", fail), call. = FALSE)
}
cat("PASS test_model_selection_guide_layout\n")
