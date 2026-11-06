#!/usr/bin/env Rscript
# Stock Prospecting first-load: shell first, nested subtabs, staggered ind-mcap.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_bluechip_first_load.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

server_src <- paste(
  readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
ui_src <- paste(
  readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
dnd_src <- paste(
  readLines(file.path(app_dir, "www", "ynow_screener_dnd.js"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

# Source only the pure policy helpers before server() to avoid app startup.
prefix <- sub("server <- function\\(input, output, session\\) \\{[\\s\\S]*$", "", server_src, perl = TRUE)
env <- new.env(parent = baseenv())
eval(parse(text = prefix), envir = env)

check("first-load plan helper exists", exists("ynow_bluechip_first_load_plan", envir = env, inherits = FALSE))

plan_upside <- env$ynow_bluechip_first_load_plan(
  has_ind_mcap_upside = TRUE,
  snapshot_present = TRUE,
  eval_busy = FALSE
)
check("upside snapshot delays ind-mcap ≥ 2s", isTRUE(plan_upside$ind_mcap_auto_delay_s >= 2))
check("ranking eval stays near flush", isTRUE(abs(plan_upside$ranking_eval_delay_s - 0.05) < 1e-9))
check("nest cluster until subtab", isTRUE(plan_upside$nest_cluster_until_subtab))
check("nest screener until subtab", isTRUE(plan_upside$nest_screener_until_subtab))
check("defer ind-mcap while eval", isTRUE(plan_upside$defer_ind_mcap_while_eval))

plan_empty <- env$ynow_bluechip_first_load_plan(
  has_ind_mcap_upside = FALSE,
  snapshot_present = FALSE,
  eval_busy = FALSE
)
check("empty snapshot uses shorter delay", isTRUE(plan_empty$ind_mcap_auto_delay_s < plan_upside$ind_mcap_auto_delay_s))
check("empty delay still after Ranking flush", isTRUE(plan_empty$ind_mcap_auto_delay_s >= 0.5))

# UI: Ranking still eager; Clustering / Custom Screen are nested lazy hosts.
check("bluechip has nested cluster host", grepl('uiOutput("ynow_lazy_host_bc_cluster")', ui_src, fixed = TRUE))
check("bluechip has nested screener host", grepl('uiOutput("ynow_lazy_host_bc_screener")', ui_src, fixed = TRUE))
check(
  "cluster body extracted to builder",
  grepl("\\.ynow_page_ui_bluechip_cluster\\s*<-\\s*function", ui_src, perl = TRUE)
)
check(
  "screener body extracted to builder",
  grepl("\\.ynow_page_ui_bluechip_screener\\s*<-\\s*function", ui_src, perl = TRUE) &&
    grepl('screener_panel_ui\\(embed = "bluechip"\\)', ui_src, perl = TRUE)
)
check(
  "im_cluster host before im_screener host",
  {
    i_c <- regexpr('value = "im_cluster"', ui_src, fixed = TRUE)[1]
    i_s <- regexpr('value = "im_screener"', ui_src, fixed = TRUE)[1]
    i_hc <- regexpr("ynow_lazy_host_bc_cluster", ui_src, fixed = TRUE)[1]
    i_hs <- regexpr("ynow_lazy_host_bc_screener", ui_src, fixed = TRUE)[1]
    i_c > 0 && i_s > i_c && i_hc > i_c && i_hs > i_s
  }
)
# Industry DT host stays static (sibling owns Ajax/colnames tn/7).
check(
  "industry DT host stays dataTableOutput",
  grepl('DT::dataTableOutput("lab_im_ind_mcap_table")', ui_src, fixed = TRUE)
)
# Nested-shell CSS lives inside tags$style(HTML("…")); double-quoted attr
# selectors break R parse of ynow_ui.R (boot failure).
check(
  "nested-shell CSS attr selector uses single quotes",
  grepl("[id^='ynow_lazy_host_bc_']", ui_src, fixed = TRUE) &&
    !grepl('[id^="ynow_lazy_host_bc_"]', ui_src, fixed = TRUE)
)
check(
  "no industry DT remount wrapper",
  !grepl('uiOutput("lab_im_ind_mcap_table_ui")', ui_src, fixed = TRUE)
)

# Server wiring
check("server mounts nested hosts on bluechip_im_report",
      grepl("input\\$bluechip_im_report", server_src) &&
        grepl("ynow_lazy_host_bc_cluster", server_src, fixed = TRUE) &&
        grepl("ynow_lazy_host_bc_screener", server_src, fixed = TRUE))
check("ind-mcap auto uses first-load plan",
      grepl("ynow_bluechip_first_load_plan", server_src, fixed = TRUE) &&
        grepl("ind_mcap_auto_delay_s", server_src, fixed = TRUE))
check("ind-mcap requeues while Ranking eval busy",
      grepl("defer_ind_mcap_while_eval", server_src, fixed = TRUE) &&
        grepl("lab_im_eval_busy()", server_src, fixed = TRUE))
check("leaderboard busy copy key",
      grepl("lab_im_lb_busy", server_src, fixed = TRUE))

# DnD boots after nested screener host too
check("screener DnD listens for nested host",
      grepl("ynow_lazy_host_bc_screener", dnd_src, fixed = TRUE))

source(file.path(app_dir, "ui_locale.R"), local = FALSE)
for (k in c("lab_im_lb_busy", "bc_cluster_shell_hint", "bc_screener_shell_hint")) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}
check(
  "ind-mcap help snapshot-first (en)",
  grepl("snapshot first", ui_str("lab_im_ind_mcap_help", "en"), ignore.case = TRUE) ||
    grepl("shows the default snapshot first", ui_str("lab_im_ind_mcap_help", "en"), fixed = TRUE)
)
check(
  "ind-mcap help snapshot-first (zh-TW)",
  grepl("預設快照", ui_str("lab_im_ind_mcap_help", "zh-TW"), fixed = TRUE) &&
    !grepl("请", ui_str("lab_im_ind_mcap_help", "zh-TW"), fixed = TRUE)
)

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
