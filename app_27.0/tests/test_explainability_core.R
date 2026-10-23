# Explainability as a core surface (not appendix)
# Run: cd app_27.0 && Rscript tests/test_explainability_core.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_explainability_core.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

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
src_about <- file.path(app_dir, "ynow_about_ui.R")
src_srv <- file.path(app_dir, "ynow_server.R")
src_ri <- file.path(app_dir, "ri_module.R")
src_pb <- file.path(app_dir, "pb_asset_module.R")
src_nav <- file.path(app_dir, "nav_module.R")
src_rel <- file.path(app_dir, "relative_multiples_module.R")
src_sotp <- file.path(app_dir, "sotp_module.R")

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
  "explain_dcf_why_shares", "explain_dcf_why_price",
  "explain_data_source", "explain_data_source_value", "explain_gate_rationale",
  "explain_ddm_why_title", "explain_ri_why_title", "explain_pb_why_title",
  "explain_nav_why_title", "explain_mult_why_title", "explain_sotp_why_title",
  "explain_mult_path", "explain_mult_role_value", "explain_sotp_path", "explain_sotp_role_value",
  "explain_smart_fv_why_title", "explain_gate_why_title",
  "explain_gate_item_hfv", "explain_gate_unlocked", "explain_gate_locked_fail"
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
check("HFV veto not buy (en)", grepl("not a buy", ui_str("explain_gate_item_hfv", "en"), ignore.case = TRUE))
check("HFV veto not buy (zh)", grepl("否決", ui_str("explain_gate_item_hfv", "zh-TW"), fixed = TRUE) &&
        grepl("非買訊", ui_str("explain_gate_item_hfv", "zh-TW"), fixed = TRUE))
check("callout mentions Smart Analysis", {
  grepl("Smart Analysis", ui_str("explain_core_callout_body", "en"), fixed = TRUE) &&
    grepl("Smart Analysis", ui_str("explain_core_callout_body", "zh-TW"), fixed = TRUE)
})
check("callout mentions Multiples + SOTP + Dashboard", {
  en_b <- ui_str("explain_core_callout_body", "en")
  zh_b <- ui_str("explain_core_callout_body", "zh-TW")
  grepl("Multiples", en_b, fixed = TRUE) && grepl("SOTP", en_b, fixed = TRUE) &&
    grepl("Model Dashboard", en_b, fixed = TRUE) &&
    grepl("Multiples", zh_b, fixed = TRUE) && grepl("SOTP", zh_b, fixed = TRUE) &&
    grepl("Model Dashboard", zh_b, fixed = TRUE)
})
check("mult Reference not FV (en)", grepl("Reference", ui_str("explain_mult_role_value", "en"), fixed = TRUE) &&
        grepl("not a Fair Value", ui_str("explain_mult_role_value", "en"), ignore.case = TRUE))
check("mult Reference not FV (zh)", grepl("Reference", ui_str("explain_mult_role_value", "zh-TW"), fixed = TRUE) &&
        grepl("非 Fair Value", ui_str("explain_mult_role_value", "zh-TW"), fixed = TRUE))

snap <- list(
  pv_explicit = 100, pv_tv = 400, discount = 0.09, g = 0.04,
  cash = 20, debt = 30, shares = 10, price = 49
)
rows_fcff <- ynow_dcf_why_rows(snap, claim = "fcff", mode = "gordon", locale = "en")
check("fcff rows nonempty", is.data.frame(rows_fcff) && nrow(rows_fcff) >= 6L)
check("fcff path mentions FCFF", any(grepl("FCFF", rows_fcff$value, fixed = TRUE)))
check("fcff has cash bridge", any(grepl("Cash", rows_fcff$label, fixed = TRUE)))
check("fcff has data source", any(grepl("Data source", rows_fcff$label, fixed = TRUE)))

rows_fcfe <- ynow_dcf_why_rows(snap, claim = "fcfe", mode = "two_stage", locale = "zh-TW")
check("fcfe path mentions FCFE", any(grepl("FCFE", rows_fcfe$value, fixed = TRUE)))
check("fcfe omits EV cash bridge", !any(grepl("EV→", rows_fcfe$label, fixed = TRUE)) &&
        !any(grepl("EV→", rows_fcfe$value, fixed = TRUE) & grepl("Cash|Debt|現金（EV|負債（EV", rows_fcfe$label)))
check("empty snap empty rows", nrow(ynow_dcf_why_rows(NULL, locale = "en")) == 0L)

ddm_rows <- ynow_ddm_why_rows(
  mode = "gordon", ke_pct = 9, g_pct = 3, d0 = 2, price = 40, locale = "en"
)
check("ddm rows path+source", nrow(ddm_rows) >= 5L &&
        any(grepl("Data source", ddm_rows$label, fixed = TRUE)))

ri_rows <- ynow_ri_why_rows(
  b0 = 20, ke_pct = 9, roe_pct = 15, g_pct = 3, pv_ri = 5, tv = 10, price = 35, locale = "en"
)
check("ri rows path", any(grepl("B0", ri_rows$value, fixed = TRUE) | grepl("B0", ri_rows$label, fixed = TRUE)))

pb_rows <- ynow_pb_why_rows(
  mode = "justified", book = 25, target_pb = 1.5, price = 37.5, locale = "en"
)
check("pb rows target", any(grepl("Target P/B", pb_rows$label, fixed = TRUE)))

nav_rows <- ynow_nav_why_rows(
  navps = 30, multiple = 0.9, holdco_pct = 15, price = 27, locale = "zh-TW"
)
check("nav rows NAVPS", any(grepl("NAVPS", nav_rows$label, fixed = TRUE)))

mult_rows <- ynow_multiples_why_rows(
  method_key = "pe", implied_price = 42, locale = "en"
)
check("mult rows path+role", nrow(mult_rows) >= 5L &&
        any(grepl("Metric", mult_rows$value, fixed = TRUE) | grepl("Multiple", mult_rows$value, fixed = TRUE)) &&
        any(grepl("Reference", mult_rows$value, fixed = TRUE)))
check("mult rows data source", any(grepl("Data source", mult_rows$label, fixed = TRUE)))

sotp_rows <- ynow_sotp_why_rows(
  n_segments = 3, implied_price = 55, locale = "en"
)
check("sotp rows segments+path", nrow(sotp_rows) >= 5L &&
        any(grepl("3", sotp_rows$value, fixed = TRUE)) &&
        any(grepl("EV/Sales", sotp_rows$value, fixed = TRUE)))
check("sotp rows data source", any(grepl("Data source", sotp_rows$label, fixed = TRUE)))

gate_rows <- ynow_decision_gate_why_rows(
  list(
    unlocked = FALSE,
    reason = "gates_fail",
    mandatory = list(
      list(item = "hfv_veto", status = "fail", detail = "C-freq 40% > 35%"),
      list(item = "bear_base", status = "pass", detail = "MOS ok")
    )
  ),
  locale = "en"
)
check("gate rows status", nrow(gate_rows) >= 2L)
check("gate HFV veto label", any(grepl("HFV veto", gate_rows$label, fixed = TRUE)))

suppressPackageStartupMessages(library(htmltools))
panel <- ynow_value_why_panel(
  title = "Why",
  rows = rows_fcff,
  empty_message = "empty",
  footnote = "core"
)
check("panel is shiny tag", inherits(panel, "shiny.tag") || inherits(panel, "shiny.tag.list"))
check("panel has admin-dev-explain class", {
  grepl("ynow-admin-dev-explain", as.character(panel), fixed = TRUE)
})
check("admin helper class constant", identical(ynow_admin_dev_explain_class(), "ynow-admin-dev-explain"))
check("admin helper wrap", {
  w <- ynow_admin_dev_explain(tags$span("x"))
  inherits(w, "shiny.tag") && grepl("ynow-admin-dev-explain", as.character(w), fixed = TRUE)
})
smart <- ynow_smart_explain_ui(
  fv_title = "FV", fv_rows = rows_fcff, fv_empty = "e", fv_foot = "f",
  gate_title = "Gate", gate_rows = gate_rows, gate_empty = "ge"
)
check("smart explain ui", inherits(smart, "shiny.tag.list") || inherits(smart, "shiny.tag"))
check("smart explain panels gated", {
  grepl("ynow-admin-dev-explain", as.character(smart), fixed = TRUE)
})
mult_panel <- ynow_value_why_panel(
  title = "Mult", rows = mult_rows, empty_message = "e", footnote = "f"
)
smart_extra <- ynow_smart_explain_ui(
  fv_title = "FV", fv_rows = rows_fcff, fv_empty = "e", fv_foot = "f",
  gate_title = "Gate", gate_rows = gate_rows, gate_empty = "ge",
  extra_panels = list(mult_panel)
)
check("smart explain ui accepts extra_panels", {
  inherits(smart_extra, "shiny.tag.list") || inherits(smart_extra, "shiny.tag")
})

ui <- paste(readLines(src_ui, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
about_ui <- if (file.exists(src_about)) {
  paste(readLines(src_about, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}
srv <- paste(readLines(src_srv, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ri_txt <- paste(readLines(src_ri, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pb_txt <- paste(readLines(src_pb, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
nav_txt <- paste(readLines(src_nav, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
rel_txt <- paste(readLines(src_rel, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sotp_txt <- paste(readLines(src_sotp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("UI mounts dcf_value_why", grepl('uiOutput\\(\"dcf_value_why\"\\)', ui))
check("UI mounts ddm_value_why", grepl('uiOutput\\(\"ddm_value_why\"\\)', ui))
check("UI mounts smart_value_why", grepl('uiOutput\\(\"smart_value_why\"\\)', ui))
check("UI mounts dashboard_value_why", grepl('uiOutput\\(\"dashboard_value_why\"\\)', ui))
check("UI why before dcf chart", {
  i_why <- regexpr("dcf_value_why", ui, fixed = TRUE)[1]
  i_chart <- regexpr("plt_dcf_trajectory", ui, fixed = TRUE)[1]
  i_why > 0 && i_chart > 0 && i_why < i_chart
})
check("Smart why before chart", {
  i_why <- regexpr("smart_value_why", ui, fixed = TRUE)[1]
  i_chart <- regexpr("smart_analysis_chart", ui, fixed = TRUE)[1]
  i_why > 0 && i_chart > 0 && i_why < i_chart
})
check("Dashboard why near explain callout", {
  i_call <- regexpr('id = "ynow_explain_core_callout"', ui, fixed = TRUE)[1]
  i_dash <- regexpr("dashboard_value_why", ui, fixed = TRUE)[1]
  i_call > 0 && i_dash > 0 && i_dash > i_call && (i_dash - i_call) < 2500
})
check("param sensitivity default expanded", {
  grepl("collapsed = FALSE", ui, fixed = TRUE) &&
    grepl("Core explainability: keep open by default", ui, fixed = TRUE)
})
check("About zh 可解釋性是核心", {
  grepl("可解釋性是核心功能，不是附註", about_ui, fixed = TRUE) ||
    grepl("可解釋性是核心功能，不是附註", ui, fixed = TRUE)
})
check("About en Explainability is a core feature", {
  grepl("Explainability is a core feature, not an appendix", about_ui, fixed = TRUE) ||
    grepl("Explainability is a core feature, not an appendix", ui, fixed = TRUE)
})
check("Model Dashboard explainability callout", grepl("ynow_explain_core_callout", ui, fixed = TRUE))
check("callout uses admin-dev-explain class", {
  grepl("ynow-explain-core-callout ynow-full-only ynow-admin-dev-explain", ui, fixed = TRUE)
})
check("CSS hides admin-dev-explain without sysadmin", {
  grepl("body:not(.ynow-sysadmin-on) .ynow-admin-dev-explain", ui, fixed = TRUE) &&
    grepl("ynow-admin-dev-explain", ui, fixed = TRUE)
})
check("applyUiLocale wires explain callout", {
  grepl("ynow_explain_core_callout_title", ui, fixed = TRUE) &&
    grepl("explain_core_callout_title", ui, fixed = TRUE)
})
check("server renders dcf_value_why", grepl("output\\$dcf_value_why", srv) &&
        grepl("ynow_dcf_why_rows", srv, fixed = TRUE) &&
        grepl("ynow_value_why_panel", srv, fixed = TRUE))
check("server renders smart_value_why", grepl("output\\$smart_value_why", srv) &&
        grepl("ynow_smart_explain_ui", srv, fixed = TRUE) &&
        grepl("ynow_decision_gate_why_rows", srv, fixed = TRUE))
check("server shared primary explain for dashboard", {
  grepl("output\\$dashboard_value_why", srv) &&
    grepl("\\.ynow_build_primary_explain_ui", srv) &&
    grepl("ynow_multiples_why_rows", srv, fixed = TRUE) &&
    grepl("ynow_sotp_why_rows", srv, fixed = TRUE) &&
    grepl("extra_panels", srv, fixed = TRUE)
})
check("server renders ddm_value_why", grepl("output\\$ddm_value_why", srv) &&
        grepl("ynow_ddm_why_rows", srv, fixed = TRUE))
check("RI module mounts value_why", grepl('uiOutput\\(ns\\(\"value_why\"\\)\\)', ri_txt) &&
        grepl("ynow_ri_why_rows", ri_txt, fixed = TRUE))
check("PB module mounts value_why", grepl('uiOutput\\(ns\\(\"value_why\"\\)\\)', pb_txt) &&
        grepl("ynow_pb_why_rows", pb_txt, fixed = TRUE))
check("NAV module mounts value_why", grepl('uiOutput\\(ns\\(\"value_why\"\\)\\)', nav_txt) &&
        grepl("ynow_nav_why_rows", nav_txt, fixed = TRUE))
check("Multiples module mounts value_why", grepl('uiOutput\\(ns\\(\"value_why\"\\)\\)', rel_txt) &&
        grepl("ynow_multiples_why_rows", rel_txt, fixed = TRUE))
check("SOTP module mounts value_why", grepl('uiOutput\\(ns\\(\"value_why\"\\)\\)', sotp_txt) &&
        grepl("ynow_sotp_why_rows", sotp_txt, fixed = TRUE))

cat(sprintf("PASS %d  FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
