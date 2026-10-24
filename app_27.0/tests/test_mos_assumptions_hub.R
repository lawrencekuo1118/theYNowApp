# test_mos_assumptions_hub.R — MOS Assumptions tab + tunable thr / ladder
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_mos_assumptions_hub.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
.source_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
`%||%` <- function(x, y) if (is.null(x)) y else x
suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
  library(shinydashboard)
})
source(file.path(.source_root, "industry_standards.R"), local = FALSE, encoding = "UTF-8")
source(file.path(.source_root, "market_profile.R"), local = FALSE, encoding = "UTF-8")
source(file.path(.source_root, "default_config.R"), local = FALSE, encoding = "UTF-8")
.safe_num <- function(x, default = NA_real_) {
  v <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(v)) default else v
}
.clip01 <- function(x, lo = 0, hi = 1) {
  x <- .safe_num(x, lo)
  max(lo, min(hi, x))
}
# Mirror live mos_hysteresis_target signature (avoid sourcing full backtest_module).
mos_hysteresis_target <- function(mos, w_vg = 0.7, max_exp = 0.90,
                                  deep = 0.30, mid = 0.10, fair = 0.00,
                                  mild_rich = -0.10) {
  max_exp <- .clip01(.safe_num(max_exp, 0.90), 0.5, 1)
  deep <- .safe_num(deep, 0.30)
  mid <- .safe_num(mid, 0.10)
  fair <- .safe_num(fair, 0.00)
  mild_rich <- .safe_num(mild_rich, -0.10)
  if (!is.finite(mos)) return(min(0.40, max_exp))
  base <- if (mos >= deep) max_exp
          else if (mos >= mid) max_exp * (0.65 / 0.90)
          else if (mos >= fair) max_exp * (0.40 / 0.90)
          else if (mos >= mild_rich) max_exp * (0.15 / 0.90)
          else 0.00
  w <- .clip01(w_vg, 0, 1)
  flat <- min(0.40, max_exp)
  target <- (1 - w) * flat + w * base
  .clip01(target, 0, max_exp)
}
source(file.path(.source_root, "backtest_validation.R"), local = TRUE, encoding = "UTF-8")
source(file.path(.source_root, "mos_assumptions_module.R"), local = TRUE, encoding = "UTF-8")
if (!exists("ynow_notes_block", mode = "function")) {
  ynow_notes_block <<- function(...) tags$div(class = "ynow-notes", ...)
}
if (!exists("ynow_member_gate_ui", mode = "function")) {
  ynow_member_gate_ui <<- function(...) NULL
}
# Match live auth_account.R signature. A positional (gate, body) stub
# hid the #467 shinyapps boot crash (body missing).
if (!exists("ynow_member_gate_surface_ui", mode = "function")) {
  ynow_member_gate_surface_ui <<- function(feature, gate, body, ..., fade = NULL) {
    if (missing(body)) stop('argument "body" is missing, with no default', call. = FALSE)
    tagList(gate, body)
  }
}

check <- function(msg, cond) {
  if (!isTRUE(cond)) stop(sprintf("FAIL: %s", msg), call. = FALSE)
  cat("OK:", msg, "\n")
}
approx_eq <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(as.numeric(a) - as.numeric(b)) <= tol
}

check("APP_DEFAULTS has MOS hub keys", {
  all(c(
    "mos_alert_default_pct", "mos_hfv_near_pct", "mos_hfv_deep_cheap_pct",
    "mos_hfv_deep_rich_pct", "mos_ladder_deep_pct", "mos_ladder_mild_rich_pct",
    "cond_bear_base_bear_mos_floor", "bt_w_vg"
  ) %in% names(APP_DEFAULTS))
})

thr0 <- mos_assumptions_hfv_thr(NULL)
check("default thr near 0.10", approx_eq(thr0$mos_near, 0.10))
check("default thr cheap 0.20", approx_eq(thr0$mos_deep_cheap, 0.20))
check("default thr rich -0.20", approx_eq(thr0$mos_deep_rich, -0.20))

fake_input <- list(
  mos_hfv_near_pct = 12,
  mos_hfv_deep_cheap_pct = 25,
  mos_hfv_deep_rich_pct = -18,
  mos_hfv_flat_band_pct = 3,
  mos_hfv_price_strong_up_pct = 6
)
thr1 <- mos_assumptions_hfv_thr(fake_input)
check("override thr near 0.12", approx_eq(thr1$mos_near, 0.12))
check("override thr cheap 0.25", approx_eq(thr1$mos_deep_cheap, 0.25))
check("classify A with custom cheap", {
  identical(
    classify_hfv_scenario(100, 120, 90, 80, thr = thr1),
    "A"
  )
})
check("classify B fails at |MOS|=0.11 when near=0.10", {
  identical(
    classify_hfv_scenario(100, 110, 100, 98.9, thr = thr0),
    "other"
  ) || identical(
    classify_hfv_scenario(100, 110, 100, 100, thr = thr0),
    "B"
  )
})

lad <- mos_assumptions_ladder(list(
  mos_ladder_deep_pct = 40,
  mos_ladder_mid_pct = 15,
  mos_ladder_fair_pct = 5,
  mos_ladder_mild_rich_pct = -5
))
check("ladder deep 0.40", approx_eq(lad$deep, 0.40))
exp_deep <- mos_hysteresis_target(
  0.42, w_vg = 1, max_exp = 0.90,
  deep = lad$deep, mid = lad$mid, fair = lad$fair, mild_rich = lad$mild_rich
)
check("custom deep cut maps to max_exp", approx_eq(exp_deep, 0.90))
exp_old_mid <- mos_hysteresis_target(0.35, w_vg = 1, max_exp = 0.90)
check("legacy deep 0.30 still max at 0.35", approx_eq(exp_old_mid, 0.90))

ui_html <- paste(as.character(mos_assumptions_tab_ui()), collapse = "\n")
check("tab ui has mos_assumptions tabName", grepl("mos_assumptions", ui_html, fixed = TRUE))
check("tab ui owns bear floor id", grepl("cond_bear_base_bear_mos_floor", ui_html, fixed = TRUE))
check("tab ui owns bt_w_vg", grepl("bt_w_vg", ui_html, fixed = TRUE))
check("tab ui owns ladder deep", grepl("mos_ladder_deep_pct", ui_html, fixed = TRUE))

source(file.path(.source_root, "decision_checklist_module.R"), local = TRUE, encoding = "UTF-8")
dc_html <- paste(as.character(decision_checklist_tab_body_ui()), collapse = "\n")
check("checklist no longer hosts bear floor input", {
  !grepl('id="cond_bear_base_bear_mos_floor"', dc_html, fixed = TRUE) &&
    grepl("dc_mos_assump_note", dc_html, fixed = TRUE) ||
    grepl("ynow_dc_mos_assump_note_bear_base", dc_html, fixed = TRUE)
})

cat("\nAll mos_assumptions hub checks passed.\n")
