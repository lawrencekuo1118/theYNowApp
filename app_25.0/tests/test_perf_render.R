# Performance: cache reuse + lean Plotly/DT paths (no network).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

suppressPackageStartupMessages({
  library(plotly)
})

root <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(root, "web_crawler.R"), local = TRUE)
source(file.path(root, "setup.R"), local = TRUE)

fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

# --- Downsample helper ---
df_big <- data.frame(
  Date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 2000),
  Value = seq_len(2000),
  stringsAsFactors = FALSE
)
ds <- .ynow_downsample_df(df_big, max_n = 600L)
check("downsample shrinks long series", nrow(ds) <= 600L && nrow(ds) >= 3L)
check("downsample keeps endpoints", identical(ds$Date[1], df_big$Date[1]) &&
        identical(ds$Date[nrow(ds)], df_big$Date[nrow(df_big)]))
check("downsample no-op when small", identical(nrow(.ynow_downsample_df(df_big[1:10, ], 600L)), 10L))

# --- IS plot uses native plot_ly (no ggplotly) ---
setup_src <- paste(readLines(file.path(root, "setup.R"), warn = FALSE), collapse = "\n")
check(
  "generate_safe_line_plot is native plot_ly",
  grepl("generate_safe_line_plot\\s*<-\\s*function", setup_src) &&
    grepl("plotly::plot_ly\\(\\)", setup_src) &&
    !grepl("ggplotly\\(", sub(
      ".*generate_safe_line_plot\\s*<-\\s*function",
      "generate_safe_line_plot <- function",
      setup_src
    ))
)

# Smoke: empty + tiny series
p0 <- generate_safe_line_plot(NULL, "AAPL", "Total Revenue")
check("empty IS plot returns plotly", inherits(p0, "plotly"))
tiny <- data.frame(
  Item = "Total Revenue",
  `2024` = "100B",
  `2023` = "90B",
  `2022` = "80B",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
p1 <- generate_safe_line_plot(tiny, "AAPL", "Total Revenue")
check("tiny IS plot returns plotly", inherits(p1, "plotly"))

# --- Server: statement DT + no double-normalize + HFV plot_ly ---
srv <- paste(readLines(file.path(root, "ynow_server.R"), warn = FALSE), collapse = "\n")
check(
  "statement tables use DT::renderDT server=TRUE",
  grepl("tbIncomeStatement\\s*<-\\s*DT::renderDT", srv) &&
    grepl("server\\s*=\\s*TRUE", srv) &&
    grepl("deferRender\\s*=\\s*TRUE", srv)
)
# After cached scrape, normalize_all_financials should not be called again in the same block
fin_block <- sub(
  ".*incProgress\\(0\\.5, detail = \"正在擷取財報明細",
  "incProgress(0.5, detail = \"正在擷取財報明細",
  srv
)
fin_block <- substr(fin_block, 1, 800)
check(
  "no double normalize after cached scrape",
  grepl("cached_scrape_financials\\(stock_code\\)", fin_block) &&
    !grepl("normalize_all_financials\\(res\\)", fin_block)
)
check(
  "HFV equity plot uses native plot_ly",
  grepl("bt_equity_plot\\s*<-\\s*renderPlotly", srv) &&
    grepl("bt_equity_plot[\\s\\S]{0,1200}plotly::plot_ly\\(\\)", srv, perl = TRUE) &&
    !grepl("bt_equity_plot[\\s\\S]{0,2000}ggplotly\\(", srv, perl = TRUE)
)
check(
  "HFV exposure plot uses native plot_ly",
  grepl("bt_exposure_plot[\\s\\S]{0,900}plotly::plot_ly\\(\\)", srv, perl = TRUE) &&
    !grepl("bt_exposure_plot[\\s\\S]{0,1200}ggplotly\\(", srv, perl = TRUE)
)
check(
  "hist price cache has TTL",
  grepl("fetched_at", srv, fixed = TRUE) &&
    grepl(".hist_price_cache_ttl_sec", srv, fixed = TRUE)
)

# --- Lab / BBLab route summary through fast cache ---
lab <- paste(readLines(file.path(root, "lab_industry_method.R"), warn = FALSE), collapse = "\n")
bb <- paste(readLines(file.path(root, "business_breakdown_module.R"), warn = FALSE), collapse = "\n")
check(
  "lab IM uses cached_get_summary_data",
  grepl("cached_get_summary_data", lab, fixed = TRUE)
)
check(
  "BBLab uses cached summary + industry",
  grepl("cached_get_summary_data", bb, fixed = TRUE) &&
    grepl("cached_get_yahoo_industry", bb, fixed = TRUE)
)

# --- Slow cache dir override hook ---
info <- ynow_cache_tier_info()
check("slow cache dir exists", is.character(info$slow_dir) && nzchar(info$slow_dir) && dir.exists(info$slow_dir))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("ALL OK\n")
