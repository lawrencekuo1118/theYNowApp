# Blue Chip Leaderboard tables: clickable column headers (client-side DT sort).
# Run: cd app_25.0/tests && Rscript test_lab_im_sortable_tables.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

ui <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# C-STRUCT-03: DT sort helpers live in lab_im_sidebar.R (sourced by facade).
src <- paste(
  c(
    readLines(file.path(app_dir, "lab_im_sidebar.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path(app_dir, "lab_industry_method.R"), warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)

for (id in c("lab_im_ind_mcap_table", "lab_im_leaderboard", "lab_im_summary", "lab_im_table")) {
  check(
    paste("UI dataTableOutput", id),
    grepl(sprintf('DT::dataTableOutput("%s")', id), ui, fixed = TRUE)
  )
}
check(
  "cluster table is dataTableOutput",
  grepl('DT::dataTableOutput("lab_cluster_table")', ui, fixed = TRUE) ||
    grepl('dataTableOutput("lab_cluster_table")', srv, fixed = TRUE)
)
check("helper forces ordering", grepl("opts$ordering <- TRUE", src, fixed = TRUE))
check("helper parses percent for sort", grepl("parseFloat(pct[1])", src, fixed = TRUE))
check("header cursor", grepl("ynow-sortable-dt thead th", ui, fixed = TRUE))

for (id in c(
  "lab_im_summary", "lab_im_ind_mcap_table", "lab_im_leaderboard",
  "lab_im_table", "lab_cluster_table"
)) {
  check(
    paste("server renderDT", id),
    grepl(sprintf("output$%s <- DT::renderDT", id), srv, fixed = TRUE)
  )
}

if (requireNamespace("DT", quietly = TRUE)) {
  owd <- getwd()
  setwd(app_dir)
  on.exit(setwd(owd), add = TRUE)
  source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
  df <- data.frame(
    排名 = c(1L, 2L),
    年化估值漲幅 = c("+12.5%", "-3.0%"),
    總市值 = c("1.20T", "800.00B"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  widget <- ynow_sortable_datatable(df)
  opts <- widget$x$options
  check("widget ordering on", isTRUE(opts$ordering))
  check("widget keeps incoming row order", identical(opts$order, list()))
  check("widget has sort render", !is.null(opts$columnDefs[[1]]$render))
} else {
  cat("NOTE: DT not installed; skipped widget check\n")
}

if (fail > 0L) quit(status = 1L)
cat("All sortable-table checks passed.\n")
