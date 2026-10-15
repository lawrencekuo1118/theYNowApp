# Explainability as a core surface (not appendix)
# Run: cd app_24.0/tests && Rscript test_explainability_core.R

app_dir <- normalizePath("..", mustWork = TRUE)
pass <- 0L
fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  if (ok) {
    pass <<- pass + 1L
    cat("OK", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
  invisible(ok)
}

src_explain <- file.path(app_dir, "explainability.R")
src_locale <- file.path(app_dir, "ui_locale.R")
src_global <- file.path(app_dir, "global.R")
src_ui <- file.path(app_dir, "ynow_ui.R")
src_srv <- file.path(app_dir, "ynow_server.R")

check("explainability.R exists", file.exists(src_explain))
check("global sources explainability.R", {
  g <- paste(readLines(src_global, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl('source\\(\"explainability\\.R\"', g)
})

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists("ui_str", mode = "function")) {
  source(src_locale, local = FALSE, encoding = "UTF-8")
}
source(src_explain, local = FALSE, encoding = "UTF-8")

keys <- c(
  "explain_core_callout_title", "explain_core_callout_body",
  "explain_dcf_why_title", "explain_dcf_why_empty", "explain_dcf_why_foot",
  "explain_dcf_why_path", "explain_dcf_why_mode",
  "explain_dcf_why_path_fcff", "explain_dcf_why_path_fcfe",
  "explain_dcf_why_mode_gordon", "explain_dcf_why_mode_two",
  "explain_dcf_why_pv_explicit", "explain_dcf_why_pv_tv",
  "explain_dcf_why_disc_wacc", "explain_dcf_why_disc_ke",
  "explain_dcf_why_g", "explain_dcf_why_cash", "explain_dcf_why_debt",
  "explain_dcf_why_shares", "explain_dcf_why_price"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check("zh title 合理價", grepl("合理價", ui_str("explain_dcf_why_title", "zh-TW"), fixed = TRUE))
check("en title Fair Value", grepl("Fair Value", ui_str("explain_dcf_why_title", "en"), fixed = TRUE))
check("zh foot 核心", grepl("核心", ui_str("explain_dcf_why_foot", "zh-TW"), fixed = TRUE))
check("en foot core", grepl("core", ui_str("explain_dcf_why_foot", "en"), ignore.case = TRUE))
check("zh no simplified 默认", !grepl("默认", ui_str("explain_dcf_why_title", "zh-TW"), fixed = TRUE))

snap <- list(
  pv_explicit = 100, pv_tv = 400, discount = 0.09, g = 0.04,
  cash = 20, debt = 30, shares = 10, price = 49
)
rows_fcff <- ynow_dcf_why_rows(snap, claim = "fcff", mode = "gordon", locale = "en")
check("fcff rows nonempty", is.data.frame(rows_fcff) && nrow(rows_fcff) >= 6L)
check("fcff path mentions FCFF", any(grepl("FCFF", rows_fcff$value, fixed = TRUE)))
check("fcff has cash bridge", any(grepl("Cash", rows_fcff$label, fixed = TRUE)))

rows_fcfe <- ynow_dcf_why_rows(snap, claim = "fcfe", mode = "two_stage", locale = "zh-TW")
check("fcfe path mentions FCFE", any(grepl("FCFE", rows_fcfe$value, fixed = TRUE)))
check("fcfe omits EV cash bridge", !any(grepl("EV→", rows_fcfe$label, fixed = TRUE)) &&
        !any(grepl("EV→", rows_fcfe$value, fixed = TRUE) & grepl("Cash|Debt|現金（EV|負債（EV", rows_fcfe$label)))
check("empty snap empty rows", nrow(ynow_dcf_why_rows(NULL, locale = "en")) == 0L)

suppressPackageStartupMessages(library(htmltools))
panel <- ynow_value_why_panel(
  title = "Why",
  rows = rows_fcff,
  empty_message = "empty",
  footnote = "core"
)
check("panel is shiny tag", inherits(panel, "shiny.tag") || inherits(panel, "shiny.tag.list"))

ui <- paste(readLines(src_ui, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(src_srv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("UI mounts dcf_value_why", grepl('uiOutput\\(\"dcf_value_why\"\\)', ui))
check("UI why before dcf chart", {
  i_why <- regexpr("dcf_value_why", ui, fixed = TRUE)[1]
  i_chart <- regexpr("plt_dcf_trajectory", ui, fixed = TRUE)[1]
  i_why > 0 && i_chart > 0 && i_why < i_chart
})
check("param sensitivity default expanded", {
  grepl("collapsed = FALSE", ui, fixed = TRUE) &&
    grepl("Core explainability: keep open by default", ui, fixed = TRUE)
})
check("About zh 可解釋性是核心", grepl("可解釋性是核心功能，不是附註", ui, fixed = TRUE))
check("About en Explainability is a core feature", {
  grepl("Explainability is a core feature, not an appendix", ui, fixed = TRUE)
})
check("Model Dashboard explainability callout", grepl("ynow_explain_core_callout", ui, fixed = TRUE))
check("applyUiLocale wires explain callout", {
  grepl("ynow_explain_core_callout_title", ui, fixed = TRUE) &&
    grepl("explain_core_callout_title", ui, fixed = TRUE)
})
check("server renders dcf_value_why", grepl("output\\$dcf_value_why", srv) &&
        grepl("ynow_dcf_why_rows", srv, fixed = TRUE) &&
        grepl("ynow_value_why_panel", srv, fixed = TRUE))

cat(sprintf("PASS %d  FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
