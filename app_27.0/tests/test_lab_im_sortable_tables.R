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
check(
  "header cursor",
  {
    chrome <- paste(
      readLines(file.path(app_dir, "www", "ynow_chrome.css"), warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    grepl("ynow-sortable-dt thead th", chrome, fixed = TRUE) ||
      grepl("ynow-sortable-dt thead th", ui, fixed = TRUE)
  }
)
check(
  "virtual page helper present",
  grepl("ynow_dt_resolve_page_length", src, fixed = TRUE) &&
    grepl("YNOW_DT_VIRTUAL_PAGE_DEFAULT", src, fixed = TRUE)
)
check(
  "Detail no longer mounts all rows for large N",
  grepl("Viewport virtualization", srv, fixed = TRUE) &&
    !grepl("Unlimited / large N: show all rows", srv, fixed = TRUE) &&
    grepl("ynow_lab_im_detail_virt_hint", ui, fixed = TRUE)
)
check(
  "virtualization i18n keys in locale",
  {
    loc <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    grepl("lab_im_detail_virt_hint", loc, fixed = TRUE) &&
      grepl("dt_paginate_prev", loc, fixed = TRUE) &&
      grepl("dt_paginate_next", loc, fixed = TRUE)
  }
)

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

# --- Viewport virtualization (500+ mobile DOM) ---
check(
  "resolve: small table keeps all rows",
  identical(ynow_dt_resolve_page_length(10L, NULL), 10L)
)
check(
  "resolve: NULL page_length on 500 rows caps to virtual page",
  identical(ynow_dt_resolve_page_length(500L, NULL), as.integer(YNOW_DT_VIRTUAL_PAGE_DEFAULT))
)
check(
  "resolve: explicit pageLength=nrow on large table still caps",
  identical(ynow_dt_resolve_page_length(500L, 500L), as.integer(YNOW_DT_VIRTUAL_PAGE_DEFAULT))
)
check(
  "resolve: explicit 20 on mid-size table kept",
  identical(ynow_dt_resolve_page_length(80L, 20L), 20L)
)

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

# Ajax-stable keys: zh headers canonicalize to English (DT server filter uses names)
zh_df <- data.frame(
  訊息 = "ok",
  總市值 = "1.20T",
  年化估值漲幅 = "+1.0%",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
can <- lab_canonicalize_im_colnames(zh_df)
check(
  "canonicalize maps zh headers to English Ajax keys",
  identical(
    names(can),
    c("Message", "Total market cap", "Annualized upside")
  )
)
check(
  "display names round-trip zh under zh-TW",
  identical(
    lab_im_col_display_names(names(can), "zh-TW"),
    c("訊息", "總市值", "年化估值漲幅")
  )
)
check(
  "display names stay English under en",
  identical(lab_im_col_display_names(names(can), "en"), names(can))
)
# Simulate DT::dataTablesFilter name lookup across a locale flip: keys must match.
ajax_keys_zh_session <- names(lab_canonicalize_im_colnames(zh_df))
ajax_keys_en_session <- names(lab_canonicalize_im_colnames(
  lab_localize_im_colnames(zh_df, "en")
))
check(
  "locale flip keeps identical Ajax column keys (tn/7 guard)",
  identical(ajax_keys_zh_session, ajax_keys_en_session)
)

# First-load race guards (tn/7 on Table_3 / Table_4): do not rebuild the
# industry DT on busy flips or duplicate snapshot assigns; catalog off session.
check(
  "ind-mcap board isolates busy (no DT rebuild on job arm)",
  grepl("isolate(lab_im_ind_mcap_busy())", srv, fixed = TRUE) &&
    grepl("Isolate busy:", srv, fixed = TRUE)
)
check(
  "ind-mcap snap assign skips unchanged fingerprint",
  grepl(".lab_im_ind_mcap_set_snap_if_changed", srv, fixed = TRUE) &&
    grepl(".lab_im_ind_mcap_snap_fp", srv, fixed = TRUE)
)
check(
  "ind-mcap start passes catlg=NULL (build in worker)",
  grepl("catlg = NULL", srv, fixed = TRUE) &&
    grepl("can keep answering DT server=TRUE dataobj", srv, fixed = TRUE)
)

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
  widget <- ynow_sortable_datatable(df, locale = "zh-TW")
  opts <- widget$x$options
  check("widget ordering on", isTRUE(opts$ordering))
  check("widget keeps incoming row order", identical(opts$order, list()))
  check("widget has sort render", !is.null(opts$columnDefs[[1]]$render))
  check("small widget does not force paging chrome", isFALSE(isTRUE(opts$paging)))
  big <- data.frame(
    排名 = seq_len(120L),
    年化估值漲幅 = sprintf("%+.1f%%", seq_len(120L) / 10),
    總市值 = rep("1.00B", 120L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  w_big <- ynow_sortable_datatable(big, locale = "en", page_length = NULL)
  ob <- w_big$x$options
  check("large widget pages", isTRUE(ob$paging))
  check(
    "large widget pageLength capped",
    identical(as.integer(ob$pageLength)[1], as.integer(YNOW_DT_VIRTUAL_PAGE_DEFAULT))
  )
  check("large widget shows tip chrome", identical(as.character(ob$dom)[1], "tip"))
  check("large widget deferRender", isTRUE(ob$deferRender))
  check(
    "widget data uses stable English Ajax keys",
    identical(names(widget$x$data), c("Rank", "Annualized upside", "Total market cap"))
  )
  check("widget data mcap numeric", is.numeric(widget$x$data[["Total market cap"]]))
  check(
    "widget data mcap values",
    isTRUE(all.equal(as.numeric(widget$x$data[["Total market cap"]]), c(1.2e12, 8e11)))
  )
  check(
    "widget thead display stays zh-TW",
    grepl("<th>排名</th>", widget$x$container, fixed = TRUE) &&
      grepl("<th>年化估值漲幅</th>", widget$x$container, fixed = TRUE) &&
      grepl("<th>總市值</th>", widget$x$container, fixed = TRUE)
  )
  # columnDefs name fields must be the stable English keys (Ajax contract)
  name_defs <- Filter(function(d) !is.null(d$name), opts$columnDefs)
  ajax_names <- vapply(name_defs, function(d) as.character(d$name), character(1))
  check(
    "widget columnDefs Ajax names are English keys",
    identical(ajax_names, c("Rank", "Annualized upside", "Total market cap"))
  )
  render_js <- paste(as.character(opts$columnDefs[[1]]$render), collapse = "\n")
  check("widget render has formatMcap", grepl("formatMcap", render_js, fixed = TRUE))
  check("widget render lists mcap col", grepl("mcapCols", render_js, fixed = TRUE))

  w_en <- ynow_sortable_datatable(df, locale = "en")
  check(
    "en locale keeps same Ajax data names as zh",
    identical(names(w_en$x$data), names(widget$x$data))
  )
} else {
  cat("SKIP optional DT widget check (package DT not installed)\n")
}

if (fail > 0L) quit(status = 1L)
cat("All sortable-table offline checks passed.\n")
