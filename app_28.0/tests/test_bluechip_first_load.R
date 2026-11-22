#!/usr/bin/env Rscript
# Stock Prospecting first-load: staggered ind-mcap plan + locale copy.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_bluechip_first_load.R

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
check(
  "catalog ensure before ranking eval",
  isTRUE(plan_upside$catalog_ensure_delay_s < plan_upside$ranking_eval_delay_s)
)
check(
  "ranking eval waits for catalog window",
  isTRUE(plan_upside$ranking_eval_delay_s >= 0.2)
)
check("nest cluster until subtab", isTRUE(plan_upside$nest_cluster_until_subtab))
check("nest screener until subtab", isTRUE(plan_upside$nest_screener_until_subtab))
check("defer ind-mcap while eval", isTRUE(plan_upside$defer_ind_mcap_while_eval))
check("defer ind-mcap while catalog", isTRUE(plan_upside$defer_ind_mcap_while_catalog))

plan_empty <- env$ynow_bluechip_first_load_plan(
  has_ind_mcap_upside = FALSE,
  snapshot_present = FALSE,
  eval_busy = FALSE
)
check("empty snapshot uses shorter delay", isTRUE(plan_empty$ind_mcap_auto_delay_s < plan_upside$ind_mcap_auto_delay_s))
check("empty delay still after Ranking flush", isTRUE(plan_empty$ind_mcap_auto_delay_s >= 0.5))

plan_snap_only <- env$ynow_bluechip_first_load_plan(
  has_ind_mcap_upside = FALSE,
  snapshot_present = TRUE,
  eval_busy = FALSE
)
check(
  "snapshot without upside uses mid delay",
  isTRUE(plan_snap_only$ind_mcap_auto_delay_s > plan_empty$ind_mcap_auto_delay_s) &&
    isTRUE(plan_snap_only$ind_mcap_auto_delay_s < plan_upside$ind_mcap_auto_delay_s)
)

check(
  "ind-mcap defer helper exists",
  exists("ynow_bluechip_ind_mcap_should_defer", envir = env, inherits = FALSE)
)
check(
  "defer while Ranking eval busy",
  isTRUE(env$ynow_bluechip_ind_mcap_should_defer(eval_busy = TRUE))
)
check(
  "defer while catalog busy",
  isTRUE(env$ynow_bluechip_ind_mcap_should_defer(catalog_busy = TRUE))
)
check(
  "defer while Ranking queued after catalog",
  isTRUE(env$ynow_bluechip_ind_mcap_should_defer(eval_queued_after_catalog = TRUE))
)
check(
  "no defer when idle",
  !isTRUE(env$ynow_bluechip_ind_mcap_should_defer())
)
check(
  "catalog defer flag can disable queue wait",
  !isTRUE(env$ynow_bluechip_ind_mcap_should_defer(
    catalog_busy = TRUE,
    defer_while_catalog = FALSE
  ))
)

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
