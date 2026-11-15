# tests/test_beta_grouping.R — Level Beta grouping (fixed + dynamic)
# Offline: synthetic monthly returns; no network.
suppressPackageStartupMessages({
  if (requireNamespace("plotly", quietly = TRUE)) library(plotly)
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

source("beta_grouping_module.R", encoding = "UTF-8")

syn <- bg_synthetic_returns(
  n_symbols = 30L, n_months = 90L,
  as_of = as.Date("2026-10-01"), seed = 11L
)

# --- Reject duplicates ---
dup_st <- syn$stock_returns
dup_st <- rbind(dup_st[1, , drop = FALSE], dup_st)
ok_dup <- tryCatch({
  bg_validate_stock_returns(dup_st)
  FALSE
}, error = function(e) grepl("duplicate", conditionMessage(e), ignore.case = TRUE))
stopifnot(isTRUE(ok_dup))

dup_mk <- syn$market_returns
dup_mk <- rbind(dup_mk[1, , drop = FALSE], dup_mk)
ok_dup_m <- tryCatch({
  bg_validate_market_returns(dup_mk)
  FALSE
}, error = function(e) grepl("duplicate", conditionMessage(e), ignore.case = TRUE))
stopifnot(isTRUE(ok_dup_m))

# --- Trajectory mode refused ---
ok_traj <- tryCatch({
  bg_calculate(
    syn$stock_returns, syn$market_returns,
    mode = "trajectory", as_of = syn$as_of
  )
  FALSE
}, error = function(e) grepl("trajectory", conditionMessage(e), ignore.case = TRUE))
stopifnot(isTRUE(ok_traj))

# --- Rolling 24-mo window: incomplete left edge stays invalid ---
panel <- bg_estimate_beta_panel(
  syn$stock_returns, syn$market_returns,
  window = 24L, min_obs = 20L
)
stopifnot(nrow(panel) > 0L)
early <- min(panel$date)
early_rows <- panel[panel$date == early, , drop = FALSE]
stopifnot(all(!early_rows$valid))

# Month with full 24 calendar months should have valid betas for most symbols
full_month <- sort(unique(panel$date))[30L]
full_rows <- panel[panel$date == full_month & panel$valid %in% TRUE, , drop = FALSE]
stopifnot(nrow(full_rows) >= 20L)

# --- Missing month inside window does NOT stretch / fill ---
# Drop one interior month for one symbol; n_obs must fall by 1 vs complete peer.
sym0 <- syn$symbols[[1]]
st_gap <- syn$stock_returns
mid <- sort(unique(st_gap$date))[50L]
st_gap <- st_gap[!(st_gap$symbol == sym0 & st_gap$date == mid), , drop = FALSE]
panel_gap <- bg_estimate_beta_panel(st_gap, syn$market_returns, window = 24L, min_obs = 20L)
# as_of after the gap so window includes mid
asof_gap <- sort(unique(panel_gap$date))[
  which(sort(unique(panel_gap$date)) >= mid)[1] + 12L
]
row_gap <- panel_gap[panel_gap$symbol == sym0 & panel_gap$date == asof_gap, , drop = FALSE]
row_ok <- panel[panel$symbol == sym0 & panel$date == asof_gap, , drop = FALSE]
stopifnot(nrow(row_gap) == 1L, nrow(row_ok) == 1L)
stopifnot(as.integer(row_gap$n_obs[[1]]) == as.integer(row_ok$n_obs[[1]]) - 1L)

# --- Fixed membership ---
fixed <- bg_calculate(
  syn$stock_returns, syn$market_returns,
  mode = "level", k = 3L, window = 24L, min_obs = 20L,
  years = 5L, membership = "fixed", as_of = syn$as_of, locale = "en"
)
stopifnot(identical(fixed$meta$membership, "fixed"))
stopifnot(identical(fixed$meta$note, "ok"))
stopifnot(nrow(fixed$summary) > 0L, nrow(fixed$members) > 0L)
# As-of members fixed across history
asof_mem <- fixed$members[
  fixed$members$date == fixed$meta$as_of & !is.na(fixed$members$group_id),
  c("symbol", "group_id"),
  drop = FALSE
]
stopifnot(nrow(asof_mem) >= 6L)
# Same symbol keeps same group_id on every display month
for (i in seq_len(min(5L, nrow(asof_mem)))) {
  sym <- asof_mem$symbol[[i]]
  gid <- asof_mem$group_id[[i]]
  hist_g <- fixed$members$group_id[fixed$members$symbol == sym]
  stopifnot(all(hist_g == gid | is.na(hist_g)))
}
# Equal-count at as_of: sizes differ by at most 1
asof_counts <- table(asof_mem$group_id)
stopifnot(length(asof_counts) == 3L)
stopifnot(diff(range(as.integer(asof_counts))) <= 1L)

# Future data must not change past betas: truncate market after as_of-12 and recompute
cut <- bg_month_floor(seq(syn$as_of, by = "-12 months", length.out = 2L)[2])
st_past <- syn$stock_returns[syn$stock_returns$date <= cut, , drop = FALSE]
mk_past <- syn$market_returns[syn$market_returns$date <= cut, , drop = FALSE]
past_only <- bg_calculate(
  st_past, mk_past,
  mode = "level", membership = "fixed", as_of = cut, years = 3L
)
# Compare overlapping month betas for a symbol
sym_cmp <- syn$symbols[[5]]
b1 <- fixed$members$beta[
  fixed$members$symbol == sym_cmp & fixed$members$date == cut
]
b2 <- past_only$members$beta[
  past_only$members$symbol == sym_cmp & past_only$members$date == cut
]
stopifnot(length(b1) == 1L, length(b2) == 1L)
stopifnot(isTRUE(all.equal(b1, b2, tolerance = 1e-10)))

# --- Dynamic membership ---
dyn <- bg_calculate(
  syn$stock_returns, syn$market_returns,
  mode = "level", k = 3L, window = 24L, min_obs = 20L,
  years = 5L, membership = "dynamic", as_of = syn$as_of, locale = "zh-TW"
)
stopifnot(identical(dyn$meta$membership, "dynamic"))
stopifnot(identical(dyn$meta$note, "ok"))
stopifnot(nrow(dyn$summary) > 0L)
# Dynamic labels are zh-TW
stopifnot(any(grepl("低|中|高", dyn$meta$labels)))
# Membership can change over time for the same symbol
sym_d <- syn$symbols[[15]]
gids <- dyn$members$group_id[dyn$members$symbol == sym_d & !is.na(dyn$members$group_id)]
stopifnot(length(gids) >= 2L)
# Not asserting they must differ (stable mid can stay), but each month is re-ranked
# with n_assets summing near valid count
sm_last <- dyn$summary[dyn$summary$date == dyn$meta$as_of, , drop = FALSE]
stopifnot(sum(sm_last$n_assets) >= 6L)
stopifnot(all(sm_last$group_id %in% 1:3))

# --- Cross-section helper (Lab Summary β_L fallback) ---
xs <- bg_assign_level_from_betas(
  symbol = c("A", "B", "C", "D", "E", "F"),
  beta = c(0.2, 0.5, 0.9, 1.1, 1.4, 1.8),
  k = 3L, locale = "en"
)
stopifnot(identical(as.integer(xs$group_id), as.integer(c(1, 1, 2, 2, 3, 3))))
stopifnot(grepl("Low", xs$group_label[[1]], fixed = TRUE))
stopifnot(grepl("High", xs$group_label[[6]], fixed = TRUE))

# Missing → bin k+1
xs_na <- bg_assign_level_from_betas(
  c("A", "B", "C", "D", "E", "F", "G"),
  c(0.2, 0.5, 0.9, 1.1, 1.4, 1.8, NA_real_),
  k = 3L
)
stopifnot(identical(as.integer(xs_na$group_id[[7]]), 4L))

# --- CSV-shaped summary retains zero-n months when groups empty ---
# (insufficient universe at a month → n_assets = 0 rows still present for audit)
tiny <- bg_synthetic_returns(n_symbols = 4L, n_months = 60L, seed = 3L)
tiny_res <- bg_calculate(
  tiny$stock_returns, tiny$market_returns,
  membership = "dynamic", as_of = tiny$as_of, k = 3L, years = 2L
)
# 4 < 2K=6 → months should not form groups (empty or n_assets 0)
if (identical(tiny_res$meta$note, "ok")) {
  stopifnot(all(tiny_res$summary$n_assets == 0L) || nrow(tiny_res$summary) == 0L ||
              all(is.na(tiny_res$members$group_id)))
} else {
  stopifnot(tiny_res$meta$note %in% c("insufficient_valid_as_of", "ok", "empty_panel"))
}

# Plot helper does not error
if (requireNamespace("plotly", quietly = TRUE)) {
  p <- bg_summary_plotly(fixed, show_iqr = TRUE, locale = "en")
  stopifnot(inherits(p, "plotly"))
}

message("test_beta_grouping.R: PASS")
