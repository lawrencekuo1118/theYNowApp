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

cat("PASS lab_clustering\n")
