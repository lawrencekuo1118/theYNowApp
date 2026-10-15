#!/usr/bin/env Rscript
# Model Selector consideration-dimensions annotation: relocated, collapsed, i18n
# Run: cd app_21.0 && Rscript tests/test_ms_dims_annotation.R

root <- if (file.exists("ynow_ui.R")) {
  normalizePath(".")
} else if (file.exists("../ynow_ui.R")) {
  normalizePath("..")
} else if (dir.exists("app_21.0") && file.exists("app_21.0/ynow_ui.R")) {
  normalizePath("app_21.0")
} else {
  stop("Cannot locate app_21.0")
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

ui <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("helper annotation exists", grepl(
  "\\.model_selector_dimensions_annotation_ui\\s*<-\\s*function",
  ui
))
check("helper table exists", grepl(
  "\\.consideration_dimensions_table\\s*<-\\s*function",
  ui
))

ann_start <- regexpr(
  "\\.model_selector_dimensions_annotation_ui\\s*<-\\s*function",
  ui
)[1]
tbl_start <- regexpr(
  "\\.consideration_dimensions_table\\s*<-\\s*function",
  ui
)[1]
meth_start <- regexpr(
  "\\.valuation_methodology_section_ui\\s*<-\\s*function",
  ui
)[1]
ddm_tab <- regexpr(
  '"Dividend Discount Model \\(DDM\\)"',
  ui
)[1]
check(
  "helper order: table → annotation → methodology",
  tbl_start > 0 && ann_start > tbl_start && meth_start > ann_start
)

ann_slice <- if (ann_start > 0 && meth_start > ann_start) {
  substr(ui, ann_start, meth_start - 1L)
} else {
  ""
}
check(
  "details default collapsed",
  grepl("tags$details", ann_slice, fixed = TRUE) &&
    grepl("ynow_ms_dims_details", ann_slice, fixed = TRUE) &&
    !grepl("\\bopen\\s*=", ann_slice)
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
  "mounted under Model Selector box",
  gs_ok &&
    pos_title > 0 &&
    pos_ui > pos_title &&
    pos_ann > pos_ui &&
    pos_method > pos_ann
)

# Decision Matrix body inside methodology ends before the DDM tab panel
meth_matrix_slice <- if (meth_start > 0 && ddm_tab > meth_start) {
  substr(ui, meth_start, ddm_tab - 1L)
} else {
  ""
}
check(
  "methodology Decision Matrix has no 考慮維度 table",
  nchar(meth_matrix_slice) > 0 &&
    !grepl("<th>考慮維度</th>", meth_matrix_slice, fixed = TRUE) &&
    !grepl(".consideration_dimensions_table(", meth_matrix_slice, fixed = TRUE)
)
check("table helper still has 考慮維度 header", {
  # Header must live in the shared helper (before methodology), not only in comments
  helper_slice <- if (tbl_start > 0 && meth_start > tbl_start) {
    substr(ui, tbl_start, meth_start - 1L)
  } else {
    ""
  }
  grepl("<th>考慮維度</th>", helper_slice, fixed = TRUE)
})
check("applyUiLocale dims title", grepl("ynow_ms_dims_title", ui, fixed = TRUE) &&
        grepl("gs_ms_dims_title", ui, fixed = TRUE))
check("applyUiLocale dims aria", grepl("ynow_ms_dims_summary", ui, fixed = TRUE) &&
        grepl("gs_ms_dims_toggle_aria", ui, fixed = TRUE))
check("notes aria skips dims summary", grepl(
  "if (el.id === 'ynow_ms_dims_summary') return;",
  ui,
  fixed = TRUE
))
check("ms-dims CSS present", grepl(".ynow-ms-dims", ui, fixed = TRUE))

check("en dims title", identical(
  .UI_STRINGS$en$gs_ms_dims_title,
  "Consideration dimensions"
))
check("zh-TW dims title", identical(
  .UI_STRINGS$`zh-TW`$gs_ms_dims_title,
  "考慮維度"
))
check("en dims aria", nzchar(.UI_STRINGS$en$gs_ms_dims_toggle_aria))
check("zh-TW dims aria", nzchar(.UI_STRINGS$`zh-TW`$gs_ms_dims_toggle_aria))
check("zh-TW no simplified 维度", !grepl(
  "维度",
  .UI_STRINGS$`zh-TW`$gs_ms_dims_title,
  fixed = TRUE
))

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("model selector renderUI intact", grepl(
  "output\\$get_started_model_selector\\s*<-\\s*renderUI",
  srv
))
check("model_sidebar_rec still drives cards", grepl(
  "model_sidebar_rec\\(\\)",
  srv
))

if (requireNamespace("htmltools", quietly = TRUE)) {
  env <- new.env(parent = globalenv())
  env$tags <- htmltools::tags
  env$HTML <- htmltools::HTML
  m_tbl <- regexpr(
    "\\.consideration_dimensions_table\\s*<-\\s*function\\s*\\(\\)\\s*\\{[\\s\\S]*?\\n\\}",
    ui,
    perl = TRUE
  )
  m_ann2 <- regexpr(
    "\\.model_selector_dimensions_annotation_ui\\s*<-\\s*function\\s*\\(\\)\\s*\\{[\\s\\S]*?\\n\\}",
    ui,
    perl = TRUE
  )
  check("parse table helper", m_tbl[1] > 0)
  check("parse annotation helper", m_ann2[1] > 0)
  if (m_tbl[1] > 0 && m_ann2[1] > 0) {
    eval(parse(text = substr(ui, m_tbl[1], m_tbl[1] + attr(m_tbl, "match.length") - 1L)), envir = env)
    eval(parse(text = substr(ui, m_ann2[1], m_ann2[1] + attr(m_ann2, "match.length") - 1L)), envir = env)
    html <- paste(as.character(env$.model_selector_dimensions_annotation_ui()), collapse = "")
    check("rendered details", grepl("<details", html, fixed = TRUE))
    check("rendered no open", !grepl("\\sopen(|=|>|\\s)", html))
    check("rendered title id", grepl("ynow_ms_dims_title", html, fixed = TRUE))
    check("rendered table header", grepl("考慮維度", html, fixed = TRUE))
    check("rendered not notes_title class", !grepl("ynow-notes__title", html, fixed = TRUE))
  }
}

if (fail > 0L) {
  stop(sprintf("%d ms-dims annotation check(s) failed", fail), call. = FALSE)
}
cat("PASS test_ms_dims_annotation\n")
