# tests/test_lab_clustering.R — Clustering Lab unit tests (no network)
suppressPackageStartupMessages({
  library(magrittr)
  library(plotly)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
if (!exists("normalize_market_mode", mode = "function")) {
  normalize_market_mode <- function(mode) {
    m <- toupper(trimws(as.character(mode %||% "US")[1]))
    if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
  }
}

source("lab_industry_method.R", encoding = "UTF-8")
source("beta_grouping_module.R", encoding = "UTF-8")
source("lab_clustering.R", encoding = "UTF-8")

df <- lab_cluster_synthetic_features(n = 24L, seed = 7L)
stopifnot(nrow(df) == 24L)
stopifnot(all(LAB_CLUSTER_FEATURES %in% names(df)))

res <- lab_run_stock_clustering(df, k_clusters = 4L, locale = "en", seed = 7L)
stopifnot(identical(as.integer(res$k), 4L))
stopifnot(nrow(res$data) == 24L)
stopifnot(all(c("Cluster_ID", "Cluster_Label") %in% names(res$data)))
stopifnot(length(unique(res$data$Cluster_ID)) == 4L)

x <- c(-1000, 1, 2, 3, 4, 1000)
w <- lab_cluster_winsorize(x, lo = 0.01, hi = 0.99)
stopifnot(max(w) < 1000, min(w) > -1000)

res_zh <- lab_run_stock_clustering(df, k_clusters = 4L, locale = "zh-TW", seed = 7L)
stopifnot(any(grepl("成長|價值|風險|防禦", res_zh$data$Cluster_Label)))

focus <- res$data$ticker[[1]]
peers <- lab_cluster_radar_peers(res, focus, n_peers = 3L)
stopifnot(length(peers) <= 3L, !focus %in% peers)

p1 <- lab_cluster_scatter_plotly(
  res, x_feat = "ROE", y_feat = "PE_Ratio", locale = "en", focus_ticker = focus
)
p2 <- lab_cluster_radar_plotly(res, focus_ticker = focus, locale = "en")
stopifnot(inherits(p1, "plotly"), inherits(p2, "plotly"))
p1b <- plotly::plotly_build(p1)
focus_trace <- NULL
for (tr in p1b$x$data) {
  nm <- as.character(tr$name %||% "")
  if (grepl("★", nm, fixed = TRUE)) {
    focus_trace <- tr
    break
  }
}
stopifnot(!is.null(focus_trace))
focus_col <- tolower(as.character(focus_trace$marker$color %||% "")[1])
stopifnot(identical(focus_col, "#e53935") || identical(focus_col, "rgb(229, 57, 53)"))

# Idle vs live panel mode (market-switch clear must destroy Plotly/DT hosts)
stopifnot(identical(lab_cluster_panel_mode(NULL), "idle"))
stopifnot(identical(lab_cluster_panel_mode(list(data = data.frame())), "idle"))
stopifnot(identical(lab_cluster_panel_mode(res), "live"))
stopifnot(isTRUE(lab_cluster_has_result(res)))
stopifnot(!isTRUE(lab_cluster_has_result(NULL)))

ph <- lab_cluster_idle_placeholder("Run clustering to see the map.", min_height = "420px")
stopifnot(inherits(ph, "shiny.tag"))
stopifnot(identical(ph$attribs$class, "ynow-lab-cluster-idle"))
html <- as.character(ph)
stopifnot(grepl("ynow-lab-cluster-idle", html, fixed = TRUE))
stopifnot(grepl("Run clustering to see the map", html, fixed = TRUE))
# Placeholder must NOT embed plotly/DT output bindings
stopifnot(!grepl("plotly-html-widget|htmlwidget-output|datatables", html, ignore.case = TRUE))

# Simulate market-switch clear: result → NULL flips idle (no live widget host)
sim <- res
stopifnot(identical(lab_cluster_panel_mode(sim), "live"))
sim <- NULL
stopifnot(identical(lab_cluster_panel_mode(sim), "idle"))

# Cluster assignments: numeric columns → 2 decimal places
fmt <- lab_cluster_format_assignments_df(res$data)
stopifnot(is.integer(fmt$Cluster_ID) || all(fmt$Cluster_ID == as.integer(fmt$Cluster_ID)))
for (nm in intersect(LAB_CLUSTER_FEATURES, names(fmt))) {
  vals <- fmt[[nm]][is.finite(fmt[[nm]])]
  if (length(vals)) {
    stopifnot(all(abs(vals - round(vals, 2)) < 1e-9))
  }
}
if ("market_cap" %in% names(fmt)) {
  vals <- fmt$market_cap[is.finite(fmt$market_cap)]
  if (length(vals)) stopifnot(all(abs(vals - round(vals, 2)) < 1e-9))
}

# Usable-row helper: all-NA shells must count as 0 (R Yahoo fallback failure mode)
empty_shell <- lab_cluster_features_to_df(lapply(1:4, function(i) {
  list(
    ticker = paste0("T", i, ".TW"),
    name = paste0("T", i),
    market_cap = NA_real_,
    ROE = NA_real_, Operating_Margin = NA_real_, Rev_YoY = NA_real_,
    OpInc_YoY = NA_real_, Debt_Ratio = NA_real_, PE_Ratio = NA_real_, PB_Ratio = NA_real_
  )
}))
stopifnot(identical(lab_cluster_usable_feature_rows(empty_shell), 0L))
stopifnot(lab_cluster_usable_feature_rows(res$data) >= 4L)

# Sparse ticker detection + merge prefer finite primary then secondary
sparse <- lab_cluster_sparse_tickers(empty_shell, c("T1.TW", "T9.TW"))
stopifnot(identical(sort(sparse), c("T1.TW", "T9.TW")))
partial_a <- lab_cluster_features_to_df(list(list(
  ticker = "2330.TW", name = "TSMC", market_cap = 1e12,
  ROE = 40, Operating_Margin = NA_real_, Rev_YoY = NA_real_,
  OpInc_YoY = NA_real_, Debt_Ratio = NA_real_, PE_Ratio = NA_real_, PB_Ratio = NA_real_
)))
partial_b <- lab_cluster_features_to_df(list(list(
  ticker = "2330.TW", name = "TSMC", market_cap = NA_real_,
  ROE = NA_real_, Operating_Margin = 55, Rev_YoY = 30,
  OpInc_YoY = NA_real_, Debt_Ratio = 20, PE_Ratio = 22, PB_Ratio = 8
)))
merged <- lab_cluster_merge_feature_dfs(partial_a, partial_b)
stopifnot(identical(merged$ticker[[1]], "2330.TW"))
stopifnot(isTRUE(abs(merged$ROE[[1]] - 40) < 1e-9))
stopifnot(isTRUE(abs(merged$Operating_Margin[[1]] - 55) < 1e-9))
stopifnot(lab_cluster_usable_feature_rows(merged) >= 1L)

# Ratio-dense but Beta-blank rows: default sparse stays empty; need_beta=TRUE gap-fills
dense_no_beta <- lab_cluster_features_to_df(list(
  list(
    ticker = "AAPL", name = "Apple", market_cap = 3e12,
    ROE = 150, Operating_Margin = 30, Rev_YoY = 8,
    OpInc_YoY = 10, Debt_Ratio = 150, PE_Ratio = 30, PB_Ratio = 40,
    Beta = NA_real_
  ),
  list(
    ticker = "MSFT", name = "Microsoft", market_cap = 3e12,
    ROE = 40, Operating_Margin = 42, Rev_YoY = 12,
    OpInc_YoY = 14, Debt_Ratio = 50, PE_Ratio = 35, PB_Ratio = 12,
    Beta = NA_real_
  )
))
stopifnot(identical(lab_cluster_sparse_tickers(dense_no_beta, c("AAPL", "MSFT")), character(0)))
stopifnot(identical(
  sort(lab_cluster_sparse_tickers(dense_no_beta, c("AAPL", "MSFT"), need_beta = TRUE)),
  c("AAPL", "MSFT")
))
stopifnot(identical(
  sort(lab_cluster_missing_beta_tickers(dense_no_beta, c("AAPL", "MSFT", "ZZZ"))),
  c("AAPL", "MSFT", "ZZZ")
))

# CAPM Summary / batch Beta gap-fill (existing crawlers — no new source)
assign("fetch_beta_unlever_inputs_batch", function(tickers) {
  lapply(as.character(tickers), function(tk) {
    list(ticker = toupper(tk), name = toupper(tk), beta = 1.05, market_cap = 1e9)
  })
}, envir = .GlobalEnv)
on.exit({
  if (exists("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv, inherits = FALSE)) {
    rm("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv)
  }
}, add = TRUE)
filled_beta <- lab_cluster_fill_beta_from_capm(dense_no_beta, c("AAPL", "MSFT"))
stopifnot(identical(lab_cluster_usable_beta_rows(filled_beta), 2L))
stopifnot(all(abs(filled_beta$Beta - 1.05) < 1e-9))
stopifnot(identical(lab_cluster_missing_beta_tickers(filled_beta, c("AAPL", "MSFT")), character(0)))
# Ratios preserved while Beta is filled from CAPM path
stopifnot(isTRUE(abs(filled_beta$ROE[filled_beta$ticker == "AAPL"][[1]] - 150) < 1e-9))

# Summary-metrics fallback when batch is unavailable
rm("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv)
assign("lab_fetch_summary_metrics", function(ticker) {
  list(
    market_cap = 2e9, price = 100, beta = 0.88,
    company_name = paste0(toupper(ticker), " Co")
  )
}, envir = .GlobalEnv)
on.exit({
  if (exists("lab_fetch_summary_metrics", envir = .GlobalEnv, inherits = FALSE)) {
    rm("lab_fetch_summary_metrics", envir = .GlobalEnv)
  }
}, add = TRUE)
filled_sum <- lab_cluster_fill_beta_from_capm(dense_no_beta, "AAPL")
stopifnot(identical(lab_cluster_usable_beta_rows(filled_sum), 1L))
stopifnot(isTRUE(abs(filled_sum$Beta[filled_sum$ticker == "AAPL"][[1]] - 0.88) < 1e-9))
rm("lab_fetch_summary_metrics", envir = .GlobalEnv)

# Offline snapshot must load and support US + TW without live Yahoo
snap_all <- lab_load_cluster_features_snapshot(NULL)
stopifnot(is.data.frame(snap_all), nrow(snap_all) >= 100L)
stopifnot(lab_cluster_usable_feature_rows(snap_all) >= 100L)
us_sample <- utils::head(snap_all$ticker[!grepl("\\.(TW|TWO)$", snap_all$ticker)], 25)
tw_sample <- utils::head(snap_all$ticker[grepl("\\.(TW|TWO)$", snap_all$ticker)], 25)
stopifnot(length(us_sample) >= 10L, length(tw_sample) >= 10L)
# Simulate Yahoo-blocked host: snapshot-only fetch for N≈25 US / TW
old_ensure <- if (exists(".ensure_python_scraper", mode = "function", inherits = TRUE)) {
  get(".ensure_python_scraper", mode = "function")
} else {
  NULL
}
assign(".ensure_python_scraper", function() FALSE, envir = .GlobalEnv)
on.exit({
  if (is.null(old_ensure)) {
    if (exists(".ensure_python_scraper", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".ensure_python_scraper", envir = .GlobalEnv)
    }
  } else {
    assign(".ensure_python_scraper", old_ensure, envir = .GlobalEnv)
  }
}, add = TRUE)
# Stub R crumb path to empty (rate_limited)
assign("lab_fetch_cluster_features_r", function(tickers, timeout_sec = 12) {
  out <- lab_cluster_features_to_df(NULL)
  attr(out, "yahoo_r_error") <- "rate_limited"
  out
}, envir = .GlobalEnv)
feats_us <- lab_fetch_cluster_features(us_sample)
feats_tw <- lab_fetch_cluster_features(tw_sample)
stopifnot(lab_cluster_usable_feature_rows(feats_us) >= 2L)
stopifnot(lab_cluster_usable_feature_rows(feats_tw) >= 2L)
stopifnot(isTRUE(attr(feats_us, "used_snapshot")) || lab_cluster_usable_feature_rows(feats_us) >= 2L)
res_off <- lab_run_stock_clustering(feats_us, k_clusters = 4L, locale = "en", seed = 3L)
stopifnot(identical(as.integer(res_off$k), 4L), nrow(res_off$data) >= 4L)

# β_L mode: snapshot ratios dense but Beta blank → CAPM batch gap-fill
assign("fetch_beta_unlever_inputs_batch", function(tickers) {
  lapply(as.character(tickers), function(tk) {
    list(
      ticker = toupper(as.character(tk)[1]),
      name = toupper(as.character(tk)[1]),
      beta = 1.12,
      market_cap = 1e9
    )
  })
}, envir = .GlobalEnv)
on.exit({
  if (exists("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv, inherits = FALSE)) {
    rm("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv)
  }
}, add = TRUE)
# K-Means path (need_beta=FALSE) still works offline without requiring Beta
feats_km <- lab_fetch_cluster_features(us_sample[seq_len(min(12L, length(us_sample)))], need_beta = FALSE)
stopifnot(lab_cluster_usable_feature_rows(feats_km) >= 2L)
res_km_off <- lab_run_stock_clustering(feats_km, k_clusters = 3L, locale = "en", seed = 5L)
stopifnot(identical(as.character(res_km_off$cluster_mode), "kmeans"))
# β_L path fills Beta via existing CAPM crawler even when R crumb is rate-limited
beta_sample <- us_sample[seq_len(min(12L, length(us_sample)))]
feats_beta_live <- lab_fetch_cluster_features(beta_sample, need_beta = TRUE)
stopifnot(lab_cluster_usable_beta_rows(feats_beta_live) >= 2L)
stopifnot(lab_cluster_usable_feature_rows(feats_beta_live) >= 2L)
res_beta_off <- lab_run_beta_clustering(feats_beta_live, locale = "en")
stopifnot(identical(as.character(res_beta_off$cluster_mode), "beta"))
stopifnot(nrow(res_beta_off$data) >= 2L)
rm("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv)

# Session ticker matching (TW suffix / BRK.B↔BRK-B)
stopifnot(identical(lab_cluster_match_ticker(c("AAPL", "MSFT"), "aapl"), "AAPL"))
stopifnot(identical(lab_cluster_match_ticker(c("2330.TW", "2317.TW"), "2330"), "2330.TW"))
stopifnot(identical(lab_cluster_match_ticker(c("BRK-B", "AAPL"), "BRK.B"), "BRK-B"))
stopifnot(is.na(lab_cluster_match_ticker(c("AAPL"), "MSFT")))

# Universe N must force-include Search ticker; clustering keeps sparse Search row
catlg <- data.frame(
  ticker = c("AAA", "BBB", "CCC", "DDD", "EEE"),
  industry_key = rep("tech", 5),
  industry_label = rep("Tech", 5),
  market_cap = c(5, 4, 3, 2, 1) * 1e9,
  stringsAsFactors = FALSE
)
pool_top <- catlg[order(-catlg$market_cap), , drop = FALSE][1:3, , drop = FALSE]
stopifnot(is.na(lab_cluster_match_ticker(pool_top$ticker, "EEE")))
pool_n <- lab_cluster_ensure_ticker_in_pool(pool_top, catlg, "EEE", max_n = 3L)
stopifnot(nrow(pool_n) <= 3L)
stopifnot(!is.na(lab_cluster_match_ticker(pool_n$ticker, "EEE")))

# Truncate-aware pool build (mcap) + force-include Search ticker
pool_built <- lab_cluster_build_pool(
  catlg, max_n = 3L, ensure_ticker = "EEE", rank_mode = "mcap"
)
stopifnot(nrow(pool_built) <= 3L)
stopifnot(!is.na(lab_cluster_match_ticker(pool_built$ticker, "EEE")))
stopifnot(identical(as.character(attr(pool_built, "pool_rank_mode")), "mcap"))

# Selected N (not hardcoded 25/50): pool size follows max_n; Inf keeps all
pool_n100 <- lab_cluster_build_pool(catlg, max_n = 100L, rank_mode = "mcap")
stopifnot(nrow(pool_n100) == min(100L, nrow(catlg)))
stopifnot(identical(as.integer(attr(pool_n100, "max_n")), 100L))
pool_n50 <- lab_cluster_build_pool(catlg, max_n = 50L, rank_mode = "mcap")
stopifnot(nrow(pool_n50) == min(50L, nrow(catlg)))
# Re-resolve Inf must not collapse to default 25
pool_all <- lab_cluster_build_pool(
  catlg, max_n = lab_resolve_im_max_n("all", NULL), rank_mode = "mcap"
)
stopifnot(nrow(pool_all) == nrow(lab_dedupe_eval_pool(catlg)) ||
            nrow(pool_all) >= nrow(pool_n100))
stopifnot(is.infinite(attr(pool_all, "max_n")))

# Result order: pin focus first, then mcap desc for the rest
ord_df <- data.frame(
  ticker = c("AAA", "BBB", "CCC", "EEE"),
  market_cap = c(5, 40, 10, 1) * 1e9,
  Cluster_ID = 1:4,
  stringsAsFactors = FALSE
)
ord_mcap <- lab_cluster_order_by_truncate(ord_df, rank_mode = "mcap", pin_ticker = "EEE")
stopifnot(identical(as.character(ord_mcap$ticker[[1]]), "EEE"))
stopifnot(identical(as.character(ord_mcap$ticker[-1]), c("BBB", "CCC", "AAA")))

ord_ret <- ord_df
ord_ret$ret_1y <- c(0.05, 0.40, 0.10, -0.20)
ord_ret1 <- lab_cluster_order_by_truncate(ord_ret, rank_mode = "ret_1y", pin_ticker = "CCC")
stopifnot(identical(as.character(ord_ret1$ticker[[1]]), "CCC"))
stopifnot(identical(as.character(ord_ret1$ticker[-1]), c("BBB", "AAA", "EEE")))

# Pool order preserved when provided (random / concept truncate order)
ord_pool <- lab_cluster_order_by_truncate(
  ord_df, rank_mode = "random", pin_ticker = "AAA",
  pool_ticker_order = c("CCC", "EEE", "BBB", "AAA")
)
stopifnot(identical(as.character(ord_pool$ticker), c("AAA", "CCC", "EEE", "BBB")))

# Force-include drop respects ret_1y truncate (drop lowest return, keep focus)
pool_ret <- data.frame(
  ticker = c("A", "B", "C", "D"),
  ret_1y = c(0.5, 0.4, 0.1, 0.05),
  market_cap = c(1, 2, 3, 4) * 1e9,
  stringsAsFactors = FALSE
)
pool_ret_n <- lab_cluster_ensure_ticker_in_pool(
  pool_ret[1:3, ], pool_ret, "D", max_n = 3L, rank_mode = "ret_1y"
)
stopifnot(nrow(pool_ret_n) == 3L)
stopifnot(!is.na(lab_cluster_match_ticker(pool_ret_n$ticker, "D")))
stopifnot(is.na(lab_cluster_match_ticker(pool_ret_n$ticker, "C")))

feats_shell <- lab_cluster_ensure_ticker_in_features(df[1:8, ], "ZZZ")
stopifnot(!is.na(lab_cluster_match_ticker(feats_shell$ticker, "ZZZ")))
res_keep <- lab_run_stock_clustering(
  feats_shell, k_clusters = 3L, locale = "en", seed = 1L, ensure_ticker = "ZZZ"
)
stopifnot(!is.na(lab_cluster_match_ticker(res_keep$data$ticker, "ZZZ")))
stopifnot("n_finite" %in% names(res_keep$data))
zz_i <- which(toupper(res_keep$data$ticker) == "ZZZ")[1]
stopifnot(is.finite(zz_i), res_keep$data$n_finite[[zz_i]] < 2L)


# Radar focus + Search: multi ensure injects NVS even when absent from catalog
stopifnot(identical(
  lab_cluster_normalize_ensure_tickers(c("TSM", "", "NVS", "tsm")),
  c("TSM", "NVS")
))
pool_focus <- lab_cluster_ensure_ticker_in_pool(
  pool_top, catlg, c("TSM", "NVS"), max_n = 3L, rank_mode = "mcap"
)
stopifnot(nrow(pool_focus) <= 3L)
stopifnot(!is.na(lab_cluster_match_ticker(pool_focus$ticker, "NVS")))
# Search-only ensure must NOT include NVS (repro of pre-fix radar failure)
pool_search_only <- lab_cluster_ensure_ticker_in_pool(
  pool_top, catlg, "TSM", max_n = 3L, rank_mode = "mcap"
)
stopifnot(is.na(lab_cluster_match_ticker(pool_search_only$ticker, "NVS")))
feats_multi <- lab_cluster_ensure_ticker_in_features(df[1:8, ], c("TSM", "NVS"))
stopifnot(!is.na(lab_cluster_match_ticker(feats_multi$ticker, "NVS")))
stopifnot(!is.na(lab_cluster_match_ticker(feats_multi$ticker, "TSM")))
res_multi <- lab_run_stock_clustering(
  feats_multi, k_clusters = 3L, locale = "en", seed = 2L,
  ensure_ticker = c("TSM", "NVS")
)
stopifnot(!is.na(lab_cluster_match_ticker(res_multi$data$ticker, "NVS")))
# Radar match succeeds for focus NVS after multi-ensure clustering
stopifnot(identical(
  lab_cluster_match_ticker(res_multi$data$ticker, "NVS"),
  "NVS"
))

# Pin Search ticker as first assignment row after truncate order
res_keep$data$market_cap <- seq_len(nrow(res_keep$data)) * 1e9
res_keep$data <- lab_cluster_order_by_truncate(
  res_keep$data, rank_mode = "mcap", pin_ticker = "ZZZ"
)
stopifnot(identical(toupper(as.character(res_keep$data$ticker[[1]])), "ZZZ"))

cov_en <- lab_cluster_coverage_labels(
  res_keep$data$n_finite, res_keep$data$ticker,
  search_ticker = "ZZZ", search_is_fallback = TRUE, locale = "en"
)
stopifnot(identical(cov_en[[1]], "Data-limited"))
cov_zh <- lab_cluster_coverage_labels(
  res_keep$data$n_finite, res_keep$data$ticker,
  search_ticker = "ZZZ", search_is_fallback = FALSE, locale = "zh-TW"
)
zz_i2 <- which(toupper(res_keep$data$ticker) == "ZZZ")[1]
stopifnot(identical(cov_zh[[zz_i2]], "資料受限"))
# Dense synthetic row stays OK when not search-fallback
cov_ok <- lab_cluster_coverage_labels(
  res$data$n_finite, res$data$ticker, locale = "en"
)
stopifnot(all(cov_ok == "OK"))
stopifnot("n_finite" %in% names(res$data))

flow_en <- lab_cluster_coverage_flow_html(c("Data-limited", "OK"))
stopifnot(grepl("ynow-htcdi-flow", flow_en[[1]], fixed = TRUE))
stopifnot(grepl("Data-limited", flow_en[[1]], fixed = TRUE))
stopifnot(!grepl("ynow-htcdi-flow", flow_en[[2]], fixed = TRUE))
stopifnot(identical(flow_en[[2]], "OK"))
flow_zh <- lab_cluster_coverage_flow_html("資料受限")
stopifnot(grepl("ynow-htcdi-flow", flow_zh[[1]], fixed = TRUE))
stopifnot(grepl("資料受限", flow_zh[[1]], fixed = TRUE))

# --- Beta grouping mode (levered equity β_L bins) ---
stopifnot(identical(lab_normalize_cluster_mode("kmeans"), "kmeans"))
stopifnot(identical(lab_normalize_cluster_mode("beta"), "beta"))
stopifnot(identical(lab_normalize_cluster_mode("比率"), "kmeans"))
stopifnot(identical(lab_normalize_cluster_mode("依 Beta"), "beta"))
stopifnot(identical(lab_normalize_cluster_mode("nope"), "kmeans"))

mode_en <- lab_cluster_mode_choices("en")
mode_zh <- lab_cluster_mode_choices("zh-TW")
stopifnot(identical(sort(unname(mode_en)), c("beta", "kmeans")))
stopifnot(identical(sort(unname(mode_zh)), c("beta", "kmeans")))
stopifnot(!any(grepl("K-Means|比率|分群|依", names(mode_en)) & grepl("[\u4e00-\u9fff]", names(mode_en))))
# en labels stay ASCII-ish English (allow β via "Beta" word only)
stopifnot(all(grepl("K-Means|Beta", names(mode_en))))
stopifnot(any(grepl("Beta|β", names(mode_zh))))

# K=3 equal-count terciles (retired absolute −Inf/0/0.8/1.0/1.3/Inf bins)
bin_probe <- c(0.1, 0.3, 0.5, 0.9, 1.1, 1.5, NA_real_)
asg <- lab_cluster_assign_beta_bins(
  bin_probe, locale = "en",
  symbol = c("A", "B", "C", "D", "E", "F", "G")
)
stopifnot(identical(
  as.integer(asg$Cluster_ID),
  as.integer(c(1, 1, 2, 2, 3, 3, 4))
))
stopifnot(grepl("Low", asg$Cluster_Label[[1]], fixed = TRUE))
stopifnot(grepl("Mid", asg$Cluster_Label[[3]], fixed = TRUE))
stopifnot(grepl("High", asg$Cluster_Label[[5]], fixed = TRUE))
stopifnot(grepl("Missing|not grouped", asg$Cluster_Label[[7]], ignore.case = TRUE))
asg_zh <- lab_cluster_assign_beta_bins(
  c(0.2, 0.5, 0.9, 1.1, 1.4, 1.8, NA_real_),
  locale = "zh-TW",
  symbol = sprintf("T%d", 1:7)
)
stopifnot(any(grepl("低|中|高|缺值", asg_zh$Cluster_Label)))

# Membership choices (fixed / dynamic)
mem_en <- lab_cluster_beta_membership_choices("en")
mem_zh <- lab_cluster_beta_membership_choices("zh-TW")
stopifnot(identical(sort(unname(mem_en)), c("dynamic", "fixed")))
stopifnot(identical(sort(unname(mem_zh)), c("dynamic", "fixed")))
stopifnot(identical(lab_normalize_beta_membership("逐期"), "dynamic"))

# Axis choices expose Beta (levered) for scatter (K-Means / fallback)
ax_en <- lab_cluster_axis_choices("en")
ax_zh <- lab_cluster_axis_choices("zh-TW")
stopifnot("Beta" %in% unname(ax_en), "Beta" %in% unname(ax_zh))
stopifnot(any(grepl("levered|β_L", names(ax_en), ignore.case = TRUE)))

# Beta clustering on synthetic frame; K-Means path still intact
df_beta <- lab_cluster_synthetic_features(n = 24L, seed = 11L)
stopifnot("Beta" %in% names(df_beta))
stopifnot(lab_cluster_usable_beta_rows(df_beta) >= 10L)
res_beta <- lab_run_beta_clustering(df_beta, locale = "en", membership = "fixed")
stopifnot(identical(as.character(res_beta$cluster_mode), "beta"))
stopifnot(identical(as.character(res_beta$beta_source), "summary_levered"))
stopifnot(identical(as.character(res_beta$beta_membership), "fixed"))
stopifnot(nrow(res_beta$data) == 24L)
stopifnot(all(c("Cluster_ID", "Cluster_Label", "Beta") %in% names(res_beta$data)))
stopifnot(all(res_beta$data$Cluster_ID %in% 1:4))
# Equal-count on 6 known Betas → 2 per tercile
df_fixed <- df_beta[1:6, , drop = FALSE]
df_fixed$ticker <- sprintf("B%d", 1:6)
df_fixed$Beta <- c(0.2, 0.4, 0.8, 1.0, 1.4, 1.9)
res_fixed <- lab_run_beta_clustering(df_fixed, locale = "en")
stopifnot(identical(
  as.integer(res_fixed$data$Cluster_ID[order(res_fixed$data$ticker)]),
  as.integer(c(1, 1, 2, 2, 3, 3))
))
# With monthly OLS panel attached (fixed membership)
syn <- bg_synthetic_returns(n_symbols = 24L, n_months = 90L, seed = 21L)
bg_fixed <- bg_calculate(
  syn$stock_returns, syn$market_returns,
  membership = "fixed", as_of = syn$as_of, years = 5L, locale = "en"
)
df_ols <- df_beta[seq_len(24L), , drop = FALSE]
df_ols$ticker <- syn$symbols
res_ols <- lab_run_beta_clustering(
  df_ols, locale = "en", membership = "fixed", beta_grouping = bg_fixed
)
stopifnot(identical(as.character(res_ols$beta_source), "rolling_ols_monthly"))
stopifnot(identical(as.character(res_ols$beta_membership), "fixed"))
stopifnot(!is.null(res_ols$beta_grouping))
# Dynamic membership path on same panel
bg_dyn <- bg_calculate(
  syn$stock_returns, syn$market_returns,
  membership = "dynamic", as_of = syn$as_of, years = 5L, locale = "en"
)
res_dyn <- lab_run_beta_clustering(
  df_ols, locale = "en", membership = "dynamic", beta_grouping = bg_dyn
)
stopifnot(identical(as.character(res_dyn$beta_membership), "dynamic"))
p_hist <- lab_cluster_scatter_plotly(res_dyn, locale = "en")
stopifnot(inherits(p_hist, "plotly"))

# Ensure ticker kept when Beta missing → missing bin = k+1 = 4
df_miss <- df_beta[1:8, , drop = FALSE]
df_miss$Beta[[1]] <- NA_real_
df_miss$ticker[[1]] <- "FOCUS1"
res_miss <- lab_run_beta_clustering(df_miss, locale = "en", ensure_ticker = "FOCUS1")
stopifnot(!is.na(lab_cluster_match_ticker(res_miss$data$ticker, "FOCUS1")))
focus_row <- res_miss$data[toupper(res_miss$data$ticker) == "FOCUS1", , drop = FALSE]
stopifnot(identical(as.integer(focus_row$Cluster_ID[[1]]), 4L))

# K-Means regression: still ignores Beta as a distance feature
res_km2 <- lab_run_stock_clustering(df_beta, k_clusters = 4L, locale = "en", seed = 7L)
stopifnot(identical(as.character(res_km2$cluster_mode), "kmeans"))
stopifnot(!("Beta" %in% res_km2$features))
stopifnot(identical(as.integer(res_km2$k), 4L))
stopifnot(setequal(res_km2$features, LAB_CLUSTER_FEATURES))

# Scatter / radar still build for Beta mode
p_beta <- lab_cluster_scatter_plotly(
  res_beta, x_feat = "Beta", y_feat = "PE_Ratio", locale = "en",
  focus_ticker = res_beta$data$ticker[[1]]
)
stopifnot(inherits(p_beta, "plotly"))
p_beta_radar <- lab_cluster_radar_plotly(
  res_beta, focus_ticker = res_beta$data$ticker[[1]], locale = "en"
)
stopifnot(inherits(p_beta_radar, "plotly"))

# Snapshot schema accepts Beta column (may be empty offline)
snap_beta <- lab_load_cluster_features_snapshot(NULL)
stopifnot("Beta" %in% names(snap_beta))

# Priority refill with need_beta uses CAPM Beta when ratios already dense
dense_focus <- lab_cluster_features_to_df(list(list(
  ticker = "FOCUSB", name = "Focus B", market_cap = 1e9,
  ROE = 20, Operating_Margin = 15, Rev_YoY = 5,
  OpInc_YoY = 6, Debt_Ratio = 40, PE_Ratio = 18, PB_Ratio = 3,
  Beta = NA_real_
)))
assign("fetch_beta_unlever_inputs_batch", function(tickers) {
  list(list(ticker = "FOCUSB", name = "Focus B", beta = 0.77, market_cap = 1e9))
}, envir = .GlobalEnv)
assign(".ensure_python_scraper", function() FALSE, envir = .GlobalEnv)
refill_b <- lab_cluster_priority_refill_features(dense_focus, "FOCUSB", need_beta = TRUE)
stopifnot(isTRUE(abs(refill_b$Beta[[1]] - 0.77) < 1e-9))
# Without need_beta, ratio-dense row is left alone (no CAPM call required)
refill_km <- lab_cluster_priority_refill_features(dense_focus, "FOCUSB", need_beta = FALSE)
stopifnot(!is.finite(refill_km$Beta[[1]]))
if (exists("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv, inherits = FALSE)) {
  rm("fetch_beta_unlever_inputs_batch", envir = .GlobalEnv)
}

# Regression: Clustering Lab is under Stock Prospecting (`bluechip`).
# Gating plotly/DT on `lab_notes` mounts empty hosts after 執行分群 (blank map/radar/table).
srv_lines <- readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8")
scatter_i <- grep("^\\s*output\\$lab_cluster_scatter <- plotly::renderPlotly", srv_lines)[1]
radar_i <- grep("^\\s*output\\$lab_cluster_radar <- plotly::renderPlotly", srv_lines)[1]
table_i <- grep("^\\s*output\\$lab_cluster_table <- DT::renderDT", srv_lines)[1]
stopifnot(is.finite(scatter_i), is.finite(radar_i), is.finite(table_i))
gate_near <- function(start_i) {
  chunk <- srv_lines[start_i:min(length(srv_lines), start_i + 12L)]
  any(grepl('req\\(identical\\(\\.ynow_sidebar_tab\\(\\), "bluechip"\\)\\)', chunk)) &&
    !any(grepl('req\\(identical\\(\\.ynow_sidebar_tab\\(\\), "lab_notes"\\)\\)', chunk))
}
stopifnot(isTRUE(gate_near(scatter_i)))
stopifnot(isTRUE(gate_near(radar_i)))
stopifnot(isTRUE(gate_near(table_i)))

parsed_cluster <- tryCatch({
  parse("lab_clustering.R", keep.source = FALSE)
  TRUE
}, error = function(e) {
  cat("PARSE lab_clustering.R: ", e$message, "\n")
  FALSE
})
stopifnot(isTRUE(parsed_cluster))
parsed_server <- tryCatch({
  parse("ynow_server.R", keep.source = FALSE)
  TRUE
}, error = function(e) {
  cat("PARSE ynow_server.R: ", e$message, "\n")
  FALSE
})
stopifnot(isTRUE(parsed_server))

cat("PASS lab_clustering\n")

