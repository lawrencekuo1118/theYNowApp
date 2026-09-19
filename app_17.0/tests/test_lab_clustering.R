# tests/test_lab_clustering.R — Clustering Lab unit tests (no network)
suppressPackageStartupMessages({
  library(magrittr)
  library(plotly)
})

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

p1 <- lab_cluster_scatter_plotly(res, x_feat = "ROE", y_feat = "PE_Ratio", locale = "en")
p2 <- lab_cluster_radar_plotly(res, focus_ticker = focus, locale = "en")
stopifnot(inherits(p1, "plotly"), inherits(p2, "plotly"))

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

cat("PASS lab_clustering\n")
