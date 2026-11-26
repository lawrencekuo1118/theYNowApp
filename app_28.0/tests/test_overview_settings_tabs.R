# Rendered shell/body tab order: Overview is the leftmost Settings sub-tab.
# Asserts the HTML a user would get, not a source-order grep.

suppressPackageStartupMessages({
  if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny required")
  if (!requireNamespace("shinydashboard", quietly = TRUE)) stop("shinydashboard required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
  library(shiny)
  library(shinydashboard)
  if (requireNamespace("magrittr", quietly = TRUE)) library(magrittr)
})
if (!exists("%>%", mode = "function")) {
  `%>%` <- function(lhs, rhs) {
    call <- substitute(rhs)
    call[[length(call) + 1L]] <- lhs
    eval(call, envir = parent.frame())
  }
}

root <- if (file.exists("ri_module.R")) {
  "."
} else if (file.exists("../ri_module.R")) {
  ".."
} else {
  stop("Cannot locate ri_module.R")
}
setwd(root)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

if (!exists("ui_i18n", mode = "function")) {
  ui_i18n <- function(key, fallback = NULL) {
    shiny::tags$span(`data-i18n` = key, fallback %||% key)
  }
}
if (!exists("ui_str", mode = "function")) {
  ui_str <- function(key, locale = "en") key
}

icon <- function(...) shiny::tags$i(class = "fa")
withSpinner <- function(x, ...) x
plotlyOutput <- function(...) shiny::tags$div(class = "plotly-stub")
if (!requireNamespace("DT", quietly = TRUE)) {
  DT <- new.env()
  DT$dataTableOutput <- function(...) shiny::tags$div()
}

ynow_calc_btn <- function(input_id, label, block = TRUE) {
  shiny::actionButton(input_id, label, class = "ynow-btn-calc")
}
ynow_reset_defaults_btn <- function(input_id, label = "Reset", block = TRUE) {
  shiny::actionButton(input_id, label, class = "ynow-btn-reset")
}
ynow_calc_reset_row <- function(calc_ui, reset_ui, sync_ui = NULL) {
  shiny::tagList(calc_ui, reset_ui, sync_ui)
}
lock_numeric_input <- function(ui) ui
.beta_model_source_section_ui <- function(...) shiny::tags$div(id = "beta-stub")
.model_param_sensitivity_box <- function(...) shiny::tags$div(class = "param-sens-stub")
.dcf_two_stage_params_box <- function() shiny::tags$div(id = "dcf-two-stage-stub")
.ddm_two_stage_params_box <- function() shiny::tags$div(id = "ddm-two-stage-stub")
.ddm_formula_banner <- function() shiny::tags$div(id = "ddm-formula-stub")
capm_beta_settings_ui <- function(...) shiny::tags$div()
rd_estimate_settings_ui <- function(...) shiny::tags$div()
ddm_ke_bridge_settings_ui <- function(...) shiny::tags$div()
# Real FCF shell tab (hosts near-term g + terminal SGR metric cards).
source("fcf_projection_module.R", local = FALSE, encoding = "UTF-8")

APP_DEFAULTS <- list(
  ri_roe = 15, ri_years = 5, ddm_ke = 8, sgr = 2, ri_payout = 40,
  roe_method = "constant",
  ddm_mode = "gordon", ddm_d0 = 1, ddm_g = 2, ddm_sync_central_g = TRUE,
  use_est_re = FALSE,
  wacc_gordon = 8, wacc_re = 8, wacc_rd = 4, wacc_tax = 21,
  rel_ev_sales_multiple = 3, rel_mode = "equity",
  rel_ev_ebitda_multiple = 10, rel_ev_ebit_multiple = 12, rel_ev_fcf_multiple = 15,
  rel_pe_multiple = 18, rel_fwd_pe_multiple = 18, rel_ps_multiple = 3,
  rel_ev_arr_multiple = 10,
  nav_holdco_discount = 0, nav_low = 0.9, nav_mid = 1, nav_high = 1.05,
  pb_bvps = 10, pb_tbvps = 8, pb_holdco_discount = 0, pb_basis = "bvps",
  pb_low = 1, pb_mid = 1.5, pb_high = 2, pb_target_mode = "justified",
  pb_use_industry = FALSE
)

source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("ri_module.R", local = FALSE, encoding = "UTF-8")
source("nav_module.R", local = FALSE, encoding = "UTF-8")
source("sotp_module.R", local = FALSE, encoding = "UTF-8")
source("pb_asset_module.R", local = FALSE, encoding = "UTF-8")
source("relative_multiples_module.R", local = FALSE, encoding = "UTF-8")

# Pull only the DCF/DDM page builders. Sourcing ynow_ui.R builds the whole dashboard.
.extract_fn <- function(path, name) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  pat <- paste0("(?s)", name, "\\s*<-\\s*function\\b")
  m <- regexpr(pat, txt, perl = TRUE)
  if (m[1] < 1L) stop("missing function ", name)
  start <- as.integer(m[1])
  rest <- substring(txt, start, nchar(txt))
  depth <- 0L
  end <- NA_integer_
  chars <- strsplit(rest, "", fixed = TRUE)[[1]]
  in_str <- FALSE
  quote <- ""
  esc <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (in_str) {
      if (esc) {
        esc <- FALSE
      } else if (ch == "\\") {
        esc <- TRUE
      } else if (ch == quote) {
        in_str <- FALSE
      }
      next
    }
    if (ch == "'" || ch == "\"") {
      in_str <- TRUE
      quote <- ch
      next
    }
    if (ch == "{") depth <- depth + 1L
    if (ch == "}") {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i
        break
      }
    }
  }
  if (is.na(end)) stop("unbalanced function ", name)
  eval(parse(text = substring(rest, 1L, end)), envir = globalenv())
}

.extract_fn("ynow_ui.R", ".ynow_dcf_shell_ui")
.extract_fn("ynow_ui.R", ".ynow_dcf_body_ui")
.extract_fn("ynow_ui.R", ".ynow_ddm_shell_ui")
.extract_fn("ynow_ui.R", ".ynow_page_ui_ddm")

html_of <- function(ui) paste(as.character(ui), collapse = "\n")

# Anchor labels only (skip tab-pane data-value twins).
tab_anchors <- function(html) {
  m <- gregexpr("<a\\b[^>]*data-value=\"[^\"]+\"[^>]*>", html, perl = TRUE)
  tags <- regmatches(html, m)[[1]]
  if (!length(tags)) return(character())
  gsub("^.*data-value=\"([^\"]+)\".*$", "\\1", tags)
}

fail <- 0L
check <- function(name, ok) {
  if (isTRUE(ok)) {
    cat("PASS ", name, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", name, "\n", sep = "")
  }
}

expect_shell_order <- function(label, shell_html, body_html, overview, followers) {
  tabs <- tab_anchors(shell_html)
  cat(label, " shell tabs: ", paste(tabs, collapse = " | "), "\n", sep = "")
  ov <- match(overview, tabs)
  check(paste(label, "overview anchor in shell"), !is.na(ov) && ov == 1L)
  if (!is.na(ov)) {
    for (fol in followers) {
      j <- match(fol, tabs)
      check(
        paste(label, overview, "before", fol),
        !is.na(j) && ov < j
      )
    }
  }
  body_tabs <- tab_anchors(body_html)
  cat(label, " body tabs: ", paste(body_tabs, collapse = " | "), "\n", sep = "")
  check(paste(label, "overview not a body tab"), !(overview %in% body_tabs))
}

ri_shell <- html_of(ri_module_shell_ui("mod_ri"))
ri_body <- html_of(ri_module_body_ui("mod_ri"))
expect_shell_order("RI", ri_shell, ri_body, "ri_overview", c("RI Settings", "Beta (β)"))
check("RI run button is on the shell overview", grepl("mod_ri-btn_calc_ri", ri_shell, fixed = TRUE))
check("RI run button left the body", !grepl("mod_ri-btn_calc_ri", ri_body, fixed = TRUE))
check("RI sensitivity stays in the body", "Sensitivity Analysis" %in% tab_anchors(ri_body))

nav_shell <- html_of(nav_module_shell_ui("mod_nav"))
nav_body <- html_of(nav_module_body_ui("mod_nav"))
expect_shell_order("NAV", nav_shell, nav_body, "nav_overview", "NAV Settings")
check("NAV run button is on the shell", grepl("mod_nav-btn_calc_nav", nav_shell, fixed = TRUE))
check("NAV body has no tab strip", length(tab_anchors(nav_body)) == 0L)

pb_shell <- html_of(pb_asset_module_shell_ui("mod_pb"))
pb_body <- html_of(pb_asset_module_body_ui("mod_pb"))
expect_shell_order("P/B", pb_shell, pb_body, "pb_overview", c("pb_settings", "target_pb"))

sotp_shell <- html_of(sotp_module_shell_ui("mod_sotp"))
sotp_body <- html_of(sotp_module_body_ui("mod_sotp"))
expect_shell_order("SOTP", sotp_shell, sotp_body, "sotp_overview", character())
check(
  "SOTP overview span is in the shell",
  grepl("ynow_sotp_tab_overview", sotp_shell, fixed = TRUE)
)
check(
  "SOTP overview span left the body",
  !grepl("ynow_sotp_tab_overview", sotp_body, fixed = TRUE)
)
check(
  "SOTP segments stay in the body",
  grepl("ynow_sotp_tab_segments", sotp_body, fixed = TRUE) &&
    !grepl("ynow_sotp_tab_segments", sotp_shell, fixed = TRUE)
)
sotp_tabs <- tab_anchors(sotp_shell)
check(
  "SOTP overview precedes Bridge",
  match("sotp_overview", sotp_tabs) < match(TRUE, grepl("Bridge|bridge", sotp_tabs))
)

dcf_shell <- html_of(.ynow_dcf_shell_ui())
dcf_body <- html_of(.ynow_dcf_body_ui())
expect_shell_order(
  "DCF", dcf_shell, dcf_body, "dcf_overview",
  c("dcf_two_stage", "fcf_projection", "WACC", "Beta (β)")
)
check("DCF run button is on the shell", grepl('id="calc"', dcf_shell, fixed = TRUE))
check("DCF run button left the body", !grepl('id="calc"', dcf_body, fixed = TRUE))
check(
  "DCF details stay in the body",
  "DCF Calculation Details" %in% tab_anchors(dcf_body)
)
# Metric cards restored to affiliated shell sub-tabs (not Overview body clutter).
check("growth KPIs on FCF Projection shell", grepl("ibx_estimated_g", dcf_shell, fixed = TRUE) &&
  grepl("ibx_sgr", dcf_shell, fixed = TRUE))
check("growth KPIs left DCF body", !grepl("ibx_estimated_g", dcf_body, fixed = TRUE) &&
  !grepl("ibx_sgr", dcf_body, fixed = TRUE))
check("WACC KPIs on WACC shell tab", grepl("ynow-wacc-kpi-grid", dcf_shell, fixed = TRUE) &&
  grepl("ibx_wacc", dcf_shell, fixed = TRUE) &&
  grepl("ibx_rd", dcf_shell, fixed = TRUE) &&
  grepl("ibx_re", dcf_shell, fixed = TRUE))
check("WACC KPIs left DCF body", !grepl("ynow-wacc-kpi-grid", dcf_body, fixed = TRUE) &&
  !grepl("ibx_wacc", dcf_body, fixed = TRUE))
check(
  "WACC KPI tones match NAV/RI scale",
  grepl("ynow-model-kpi-result ynow-model-kpi-outline", dcf_shell, fixed = TRUE) &&
    grepl("ynow-model-kpi-tone-2", dcf_shell, fixed = TRUE) &&
    grepl("ynow-model-kpi-tone-3", dcf_shell, fixed = TRUE) &&
    grepl("ynow-model-kpi-tone-4", dcf_shell, fixed = TRUE)
)

ddm_shell <- html_of(.ynow_ddm_shell_ui())
ddm_body <- html_of(.ynow_page_ui_ddm())
ddm_tabs <- tab_anchors(ddm_shell)
cat("DDM shell tabs: ", paste(ddm_tabs, collapse = " | "), "\n", sep = "")
ov <- match("ddm_overview", ddm_tabs)
check("DDM overview anchor in shell", !is.na(ov))
check(
  "DDM overview before Growth, D0, Ke, Beta",
  !is.na(ov) && all(ov < match(c("ddm_growth", "D0", "Ke", "Beta (β)"), ddm_tabs))
)
check("DDM overview not a body tab", !("ddm_overview" %in% tab_anchors(ddm_body)))
check("DDM run button is on the shell", grepl("mod_ddm-btn_calc_ddm", ddm_shell, fixed = TRUE))
check("DDM run button left the body", !grepl("mod_ddm-btn_calc_ddm", ddm_body, fixed = TRUE))

rel <- relative_multiples_bodies_ui("mod_rel")
for (nm in names(rel)) {
  tabs <- tab_anchors(html_of(rel[[nm]]))
  cat("Multiples ", nm, " tabs: ", paste(tabs, collapse = " | "), "\n", sep = "")
  # Already one tabBox: Overview then Settings. Titles may be span HTML in data-value.
  is_ov <- function(v) grepl("Overview", v, fixed = TRUE)
  check(paste("Multiples", nm, "starts with Overview"), length(tabs) >= 1L && is_ov(tabs[[1]]))
  if (any(tabs == "Settings")) {
    check(
      paste("Multiples", nm, "Overview before Settings"),
      which(vapply(tabs, is_ov, logical(1)))[1] < match("Settings", tabs)
    )
  }
}
check(
  "DDM calculation details stay in the body",
  "DDM Calculation Details" %in% tab_anchors(ddm_body)
)

check(
  "zh-TW RI overview label",
  identical(ui_str("tab_ri_overview", "zh-TW"), "RI 總覽")
)
check(
  "zh-TW DCF two-stage label",
  identical(ui_str("tab_dcf_two_stage", "zh-TW"), "二階段")
)
check(
  "zh-TW DDM growth label",
  identical(ui_str("tab_ddm_growth", "zh-TW"), "成長 g")
)
check(
  "en-US DDM growth label",
  identical(ui_str("tab_ddm_growth", "en"), "Growth")
)
check(
  "tab map resolves ri_overview",
  identical(ui_tab_label_map("zh-TW")[["ri_overview"]], "RI 總覽")
)
check(
  "zh-TW RI settings hint names RI 總覽",
  grepl("RI 總覽", ui_str("ri_settings_recalc_hint", "zh-TW"), fixed = TRUE)
)
check(
  "zh-TW NAV reset hint names NAV 總覽",
  grepl("NAV 總覽", ui_str("nav_settings_reset_hint", "zh-TW"), fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All overview/settings tab checks passed.\n")
