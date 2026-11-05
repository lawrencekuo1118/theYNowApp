# Stock Prospecting tables: clickable column headers (client-side DT sort).
# Default: offline static asserts (UI dataTableOutput, server renderDT, helper wiring).
# Optional env: DT widget runtime check when package DT is installed.
# Run: cd app_27.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_lab_im_sortable_tables.R
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
check("helper numericizes mcap for server sort",
      grepl("ynow_dt_numericize_sort_columns", src, fixed = TRUE))
check("helper coerces mcap display text",
      grepl("ynow_coerce_mcap_sort_value", src, fixed = TRUE))
check("helper formats mcap on display", grepl("formatMcap", src, fixed = TRUE))
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

# Load helpers without full app (sort coercers + format table)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
source(file.path(app_dir, "lab_im_sidebar.R"), local = FALSE)

# --- Total market cap: text sort is wrong; numeric sort is correct ---
mcap_txt <- c("1.20T", "800.00B", "98.70B", "5.10T", "$12.3M", "—")
mcap_num <- vapply(mcap_txt, ynow_coerce_mcap_sort_value, numeric(1), USE.NAMES = FALSE)
check("coerce 1.20T", isTRUE(all.equal(mcap_num[[1]], 1.2e12)))
check("coerce 800.00B", isTRUE(all.equal(mcap_num[[2]], 8e11)))
check("coerce 98.70B", isTRUE(all.equal(mcap_num[[3]], 9.87e10)))
check("coerce 5.10T", isTRUE(all.equal(mcap_num[[4]], 5.1e12)))
check("coerce $12.3M", isTRUE(all.equal(mcap_num[[5]], 1.23e7)))
check("coerce em dash NA", is.na(mcap_num[[6]]))

# Lexicographic order of formatted strings (the bug)
str_ord <- order(mcap_txt[!is.na(mcap_num)])
num_asc <- order(mcap_num, na.last = TRUE)
num_desc <- order(-mcap_num, na.last = TRUE)
check(
  "string order differs from numeric asc (bug shape)",
  !identical(
    mcap_txt[str_ord],
    mcap_txt[order(mcap_num, na.last = TRUE)]
  )
)
check(
  "numeric asc is true market-cap order",
  identical(
    mcap_txt[num_asc],
    c("$12.3M", "98.70B", "800.00B", "1.20T", "5.10T", "—")
  )
)
check(
  "numeric desc is true market-cap order",
  identical(
    mcap_txt[num_desc],
    c("5.10T", "1.20T", "800.00B", "98.70B", "$12.3M", "—")
  )
)

df_fmt <- data.frame(
  排名 = c(1L, 2L, 3L, 4L),
  市值加權年化估值漲幅 = c("+12.5%", "-3.0%", "+1.0%", "—"),
  總市值 = c("1.20T", "800.00B", "5.10T", "98.70B"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
prep <- ynow_dt_numericize_sort_columns(df_fmt)
check("numericize mcap column is numeric", is.numeric(prep$data[["總市值"]]))
check("numericize pct column is numeric", is.numeric(prep$data[["市值加權年化估值漲幅"]]))
check("numericize mcap idx 0-based col 2", identical(prep$mcap_idx, 2L))
check("numericize pct idx 0-based col 1", identical(prep$pct_idx, 1L))

# Server-side DT sorts the R column — simulate asc/desc on coerced mcap
m <- prep$data[["總市值"]]
check(
  "server-style asc order of mcap rows",
  identical(as.integer(prep$data[["排名"]][order(m)]), c(4L, 2L, 1L, 3L))
)
check(
  "server-style desc order of mcap rows",
  identical(as.integer(prep$data[["排名"]][order(-m)]), c(3L, 1L, 2L, 4L))
)

# en-US column titles after localize
df_en <- data.frame(
  Rank = 1:2,
  `Total market cap` = c("5.10T", "800.00B"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(df_en)[1] <- "排名"
prep_en <- ynow_dt_numericize_sort_columns(df_en)
check(
  "en Total market cap coerced",
  is.numeric(prep_en$data[["Total market cap"]]) &&
    isTRUE(all.equal(prep_en$data[["Total market cap"]], c(5.1e12, 8e11)))
)
check(
  "en mcap desc puts 5.10T first",
  identical(
    order(-prep_en$data[["Total market cap"]]),
    c(1L, 2L)
  )
)

# Industry chain format table keeps numeric Total market cap
if (file.exists(file.path(app_dir, "lab_im_ind_mcap.R"))) {
  if (!exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale <- function(x) {
      x <- as.character(x %||% "zh-TW")[1]
      if (identical(x, "en") || startsWith(tolower(x), "en")) "en" else "zh-TW"
    }
  }
  if (!exists("lab_ind_mcap_drop_unmapped_chains", mode = "function")) {
    source(file.path(app_dir, "lab_im_ind_mcap.R"), local = FALSE)
  }
  agg <- data.frame(
    chain_id = c("A", "B", "C"),
    wavg_cagr = c(10, 20, 5),
    total_mcap = c(1.2e12, 8e11, 5.1e12),
    n_names = c(3L, 2L, 4L),
    stringsAsFactors = FALSE
  )
  ft <- lab_format_ind_mcap_chain_table(agg, "zh-TW")
  check("format table 總市值 is numeric", is.numeric(ft[["總市值"]]))
  check(
    "format table mcap values preserved",
    isTRUE(all.equal(ft[["總市值"]], c(1.2e12, 8e11, 5.1e12)))
  )
  ft_en <- lab_format_ind_mcap_chain_table(agg, "en")
  check(
    "en format table Total market cap numeric",
    is.numeric(ft_en[["Total market cap"]]) &&
      isTRUE(all.equal(ft_en[["Total market cap"]][order(-ft_en[["Total market cap"]])],
                       c(5.1e12, 1.2e12, 8e11)))
  )
}

# Optional DT widget smoke — not required for default offline static path
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
  check("widget data mcap numeric", is.numeric(widget$x$data[["總市值"]]))
  check(
    "widget data mcap values",
    isTRUE(all.equal(as.numeric(widget$x$data[["總市值"]]), c(1.2e12, 8e11)))
  )
  render_js <- paste(as.character(opts$columnDefs[[1]]$render), collapse = "\n")
  check("widget render has formatMcap", grepl("formatMcap", render_js, fixed = TRUE))
  check("widget render lists mcap col", grepl("mcapCols", render_js, fixed = TRUE))
} else {
  cat("SKIP optional DT widget check (package DT not installed)\n")
}

if (fail > 0L) quit(status = 1L)
cat("All sortable-table offline checks passed.\n")
