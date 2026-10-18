#!/usr/bin/env Rscript
# Regression: Pages to capture catalog + grouped UI + locale keys.

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
app_dir <- normalizePath(file.path(".."), mustWork = TRUE)
setwd(app_dir)

source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("param_audit.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "PASS" else "FAIL", "-", label, "\n")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

pages <- ynow_param_audit_pdf_pages()
check("catalog is a data.frame", is.data.frame(pages) && nrow(pages) >= 20L)
need_cols <- c(
  "id", "tab", "inner", "annotate_tab", "locale_key",
  "category", "category_key", "selected_default"
)
check("catalog has required columns", all(need_cols %in% names(pages)))
check("ids unique", !anyDuplicated(pages$id))

# Legacy valuation defaults still selected
legacy <- c(
  "get_started", "dcf_calculator", "ddm_calculator",
  "ri_calculator", "pb_calculator", "nav_calculator"
)
check("legacy six remain default-selected", all(legacy %in% pages$id[pages$selected_default]))

# Fundamentals / backtest coverage
must_have <- c(
  "dashboard", "dashboard_is", "dashboard_bs", "dashboard_cf",
  "smart_analysis", "sensitivity", "company_advance",
  "rel_mult_ev", "rel_multiples_calculator", "rel_mult_rev", "sotp_calculator",
  "snapshot", "hfv", "lab_notes", "decision_checklist", "price_alerts", "bluechip"
)
check("expanded capture ids present", all(must_have %in% pages$id))

cats <- unique(pages$category)
check(
  "categories cover overview/statements/valuation/backtest/checklist",
  all(c("overview", "statements", "valuation", "backtest", "checklist") %in% cats)
)

# Statement rows target Company tab with an inner block
stmt <- pages[pages$category == "statements" & nzchar(pages$inner), , drop = FALSE]
check("statement blocks set tab=dashboard", all(stmt$tab == "dashboard"))
check("statement blocks have inner keys", nrow(stmt) >= 5L)

# Lab registry tab aligns with capture page (annotation boxes)
reg <- ynow_tracked_param_registry()
lab_tabs <- unique(reg$tab[startsWith(reg$input_id, "lab_im_")])
check("lab params annotate on bluechip tab", identical(lab_tabs, "bluechip"))

# UI helper emits grouped checkbox input
ui <- ynow_param_audit_pdf_pages_ui()
ui_html <- paste(as.character(ui), collapse = "\n")
check("UI input id param_audit_pdf_pages", grepl('id="param_audit_pdf_pages"', ui_html, fixed = TRUE))
check("UI uses category groups", grepl("ynow-pdf-pages-group", ui_html, fixed = TRUE))
check("UI has Decision Checklist option", grepl("param_audit_pdf_page_checklist", ui_html, fixed = TRUE))
check("UI has Quant Backtest Lab option", grepl("param_audit_pdf_page_lab", ui_html, fixed = TRUE))
check("UI has Income Statement block", grepl("param_audit_pdf_page_fs_is", ui_html, fixed = TRUE))

# Locale keys exist in both en and zh-TW
en <- .UI_STRINGS$en
zh <- .UI_STRINGS$`zh-TW`
keys <- unique(c(pages$locale_key, pages$category_key))
missing_en <- keys[!vapply(keys, function(k) nzchar(en[[k]] %||% ""), logical(1))]
missing_zh <- keys[!vapply(keys, function(k) nzchar(zh[[k]] %||% ""), logical(1))]
check(paste0("en locale keys present (", length(keys), ")"), !length(missing_en))
check(paste0("zh-TW locale keys present (", length(keys), ")"), !length(missing_zh))
if (length(missing_en)) cat("  missing en:", paste(missing_en, collapse = ", "), "\n")
if (length(missing_zh)) cat("  missing zh:", paste(missing_zh, collapse = ", "), "\n")

# zh-TW terminology: no Mainland wording / no 一鍵
zh_blob <- paste(unlist(zh[keys]), collapse = " ")
check("zh-TW avoids 一鍵", !grepl("一鍵", zh_blob, fixed = TRUE))
check("zh-TW avoids 数据", !grepl("数据", zh_blob, fixed = TRUE))

# Capture JS wires inner tabs + pages payload
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("JS defines ynowActivateInnerTab", grepl("function ynowActivateInnerTab", ui_src, fixed = TRUE))
check("JS waits for lazy panes", grepl("function ynowWaitForCapturePane", ui_src, fixed = TRUE))
check("JS consumes payload.pages", grepl("payload.pages", ui_src, fixed = TRUE))
check("server sends pages payload", {
  srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("pages = pages_payload", srv, fixed = TRUE)
})

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1L)
}
cat("All param_audit PDF pages checks passed.\n")
