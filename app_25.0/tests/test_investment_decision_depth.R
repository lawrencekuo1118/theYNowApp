#!/usr/bin/env Rscript
# Offline depth: investment_decision_module beyond layout/SOP adjacency.
# Asserts current verdict branches + module wiring — no formula/product changes.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_investment_decision_depth.R

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
    file.path(wd, "app_25.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("investment_decision_module.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(TTR)
  library(glue)
})
source("ui_locale.R", local = FALSE, encoding = "UTF-8")
source("investment_decision_module.R", local = FALSE, encoding = "UTF-8")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

# --- Module wiring (was only adjacent via layout/SOP smokes) ---
check("decision_ui exported", exists("decision_ui", mode = "function"))
check("decision_server exported", exists("decision_server", mode = "function"))
check("decision_momentum_panel_ui exported", exists("decision_momentum_panel_ui", mode = "function"))
check("decision_valuation_compare_ui exported", exists("decision_valuation_compare_ui", mode = "function"))
check("ynow_statement_verdict exported", exists("ynow_statement_verdict", mode = "function"))

ui_html <- as.character(decision_ui("depth"))
check("decision_ui mounts verdict block", grepl("ynow_funnel_verdict", ui_html, fixed = TRUE))
check("decision_ui mounts risk matrix", grepl("risk_matrix", ui_html, fixed = TRUE))
check("decision_ui mounts QoE KPI jump", grepl("ynow_kpi_jump_qoe", ui_html, fixed = TRUE))
mom_html <- as.character(decision_momentum_panel_ui("depth"))
check("momentum panel mounts", grepl("ynow_funnel_mom", mom_html, fixed = TRUE))
cmp_html <- as.character(decision_valuation_compare_ui("depth"))
check("valuation compare uiOutput", grepl("ui_valuation_compare", cmp_html, fixed = TRUE))

src <- paste(readLines("investment_decision_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "YNOW ignores Decision SOP gate (.sop_unlocked always TRUE)",
  grepl("\\.sop_unlocked\\s*<-\\s*function\\(\\)\\s*TRUE", src)
)
check(
  "server still calls ynow_statement_verdict",
  grepl("ynow_statement_verdict\\(", src)
)

# --- Verdict depth (branches beyond first-tranche smoke) ---
clean <- list(
  ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "低",
  red_flags = character(0)
)

check(
  "NULL when f_quality missing",
  is.null(ynow_statement_verdict(clean, f_score = 8, f_quality = NA, n_alert = 0L))
)

mixed <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "低", red_flags = character(0)),
  f_score = 5, f_quality = 1, n_alert = 0L
)
check("mid F-Score + clear flags → mixed", identical(mixed$title_key, "funnel_v_mixed_title"))
check("mixed class secondary", identical(mixed$class, "alert-secondary"))

fq0 <- ynow_statement_verdict(clean, f_score = 8, f_quality = 0, n_alert = 0L)
check("f_quality=0 → quality_weak", identical(fq0$title_key, "funnel_v_quality_weak_title"))

nred <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "低",
       red_flags = c("a", "b")),
  f_score = 9, f_quality = 1, n_alert = 0L
)
check("n_red>=2 → manip even without m_flag 警示", identical(nred$title_key, "funnel_v_manip_title"))

hi <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "通過", risk_level = "高",
       red_flags = character(0)),
  f_score = 9, f_quality = 1, n_alert = 0L
)
check("risk_level 高 → manip", identical(hi$title_key, "funnel_v_manip_title"))

qoe_watch <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "通過", qoe_flag = "觀察", risk_level = "低",
       red_flags = character(0)),
  f_score = 8, f_quality = 1, n_alert = 0L
)
check("qoe 觀察 → watch", identical(qoe_watch$title_key, "funnel_v_watch_title"))

# Priority: manip beats weak quality
pri <- ynow_statement_verdict(
  list(ok = TRUE, m_flag = "警示", qoe_flag = "警示", risk_level = "高",
       red_flags = c("x", "y")),
  f_score = 2, f_quality = 0, n_alert = 9L
)
check("manip priority over quality_weak", identical(pri$title_key, "funnel_v_manip_title"))

# Insufficient eq_pack still uses F-Score path (no ok list → empty flags)
bare <- ynow_statement_verdict(NULL, f_score = 8, f_quality = 1, n_alert = 0L)
check("NULL eq_pack + strong F → mixed (not clear)", identical(bare$title_key, "funnel_v_mixed_title"))

# Locale keys for every verdict title (en-US + zh-TW)
vkeys <- c(
  "funnel_v_manip_title", "funnel_v_quality_weak_title", "funnel_v_watch_title",
  "funnel_v_clear_title", "funnel_v_mixed_title"
)
for (k in vkeys) {
  check(paste0("locale en ", k), nzchar(ui_str(k, "en")))
  check(paste0("locale zh-TW ", k), nzchar(ui_str(k, "zh-TW")))
}
check(
  "clear copy vetoes buy framing (en)",
  grepl("never a buy signal", ui_str("funnel_v_clear_text", "en"), ignore.case = TRUE)
)
check(
  "clear copy vetoes buy framing (zh-TW)",
  grepl("絕非買進", ui_str("funnel_v_clear_text", "zh-TW"), fixed = TRUE)
)

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All investment_decision depth checks passed.\n")
