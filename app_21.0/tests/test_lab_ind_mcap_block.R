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
  "lab_im_ind_mcap_waiting", "lab_im_ind_mcap_empty", "lab_im_ind_mcap_status"
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

cat("\nPASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
cat("ALL PASS\n")
