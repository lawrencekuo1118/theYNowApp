#!/usr/bin/env Rscript
# Industry mcap-weighted upside is a dedicated board above Candidate truncate.
# Run: cd app_21.0/tests && Rscript test_lab_ind_mcap_block.R

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

source(file.path(app_dir, "ui_locale.R"), local = FALSE)

keys <- c(
  "lab_im_ind_mcap_title", "lab_im_ind_mcap_badge", "lab_im_ind_mcap_help",
  "lab_im_ind_mcap_waiting", "lab_im_ind_mcap_empty", "lab_im_ind_mcap_status",
  "btn_lab_im_refresh_ind_mcap", "lab_im_refresh_ind_mcap_title"
)
for (k in keys) {
  check(paste("en key", k), k %in% names(.UI_STRINGS$en) && nzchar(ui_str(k, "en")))
  check(paste("zh-TW key", k), k %in% names(.UI_STRINGS$`zh-TW`) && nzchar(ui_str(k, "zh-TW")))
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("UI has ind-mcap block", grepl("ynow-lab-im-ind-mcap-block", ui, fixed = TRUE))
check("UI has ind-mcap table output", grepl("lab_im_ind_mcap_table", ui, fixed = TRUE))
check("UI has ind-mcap status output", grepl("lab_im_ind_mcap_status", ui, fixed = TRUE))
i_block <- regexpr("ynow-lab-im-ind-mcap-block", ui, fixed = TRUE)[1]
i_rank <- regexpr("\"lab_im_pool_rank\"", ui, fixed = TRUE)[1]
check("ind-mcap block above Candidate truncate", i_block > 0 && i_rank > 0 && i_block < i_rank)

m <- regmatches(
  ui,
  regexpr("radioButtons\\(\\s*\"lab_im_lb_mode\"[\\s\\S]*?inline = TRUE", ui, perl = TRUE)
)
check("Ranking radio present", length(m) == 1L && nzchar(m))
check("Ranking radio drops industry_avg", length(m) == 1L && !grepl("industry_avg", m, fixed = TRUE))
check("Ranking radio keeps undervalued", length(m) == 1L && grepl("undervalued", m, fixed = TRUE))

check("server auto board reactive", grepl("\\.lab_im_ind_mcap_board", srv))
check("server migrates legacy industry_avg", grepl("industry_avg", srv, fixed = TRUE) &&
        grepl("updateRadioButtons\\(session, \"lab_im_lb_mode\"", srv))
check("scope help mentions dedicated board (en)",
      grepl("dedicated board above Candidate truncate", ui_str("lab_im_lb_scope_help", "en"), fixed = TRUE))
check("scope help mentions 上方獨立區塊 (zh-TW)",
      grepl("上方獨立區塊", ui_str("lab_im_lb_scope_help", "zh-TW"), fixed = TRUE))

# Opening Blue Chip Leaderboard auto-starts the same runner as Search Blue Chips
check("server has lab_im_run_pulse", grepl("lab_im_run_pulse", srv, fixed = TRUE))
check("server has lab_im_eval_busy", grepl("lab_im_eval_busy", srv, fixed = TRUE))
check("button pulses shared runner",
      grepl("observeEvent(input$lab_im_run_fscore", srv, fixed = TRUE) &&
        grepl("lab_im_run_pulse(", srv, fixed = TRUE))
check("sidebar bluechip auto-triggers when empty",
      grepl('identical(tab, "bluechip")', srv, fixed = TRUE) &&
        grepl("lazy_mount_nonce()", srv, fixed = TRUE) &&
        grepl('mounted("bluechip")', srv, fixed = TRUE))
check("sidebar bluechip auto-start defers via later after flush",
      grepl("later::later(start_eval", srv, fixed = TRUE) &&
        grepl("lab_im_auto_start_armed", srv, fixed = TRUE) &&
        grepl("session$onFlushed(function()", srv, fixed = TRUE) &&
        grepl("delay = 0.05", srv, fixed = TRUE))
check("waiting copy mentions Refresh ranking (en)",
      grepl("Refresh ranking", ui_str("lab_im_ind_mcap_waiting", "en"), fixed = TRUE))
check("waiting copy mentions 更新排名 (zh-TW)",
      grepl("更新排名", ui_str("lab_im_ind_mcap_waiting", "zh-TW"), fixed = TRUE) &&
        !grepl("请按", ui_str("lab_im_ind_mcap_waiting", "zh-TW"), fixed = TRUE))
check("help mentions industry chain (en)",
      grepl("industry chain", ui_str("lab_im_ind_mcap_help", "en"), ignore.case = TRUE))
check("help mentions 全部產業鏈 (zh-TW)",
      grepl("全部產業鏈", ui_str("lab_im_ind_mcap_help", "zh-TW"), fixed = TRUE))
check("UI has Refresh ranking button", grepl("lab_im_refresh_ind_mcap", ui, fixed = TRUE))
check("server refresh observeEvent", grepl("lab_im_refresh_ind_mcap", srv, fixed = TRUE))

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
