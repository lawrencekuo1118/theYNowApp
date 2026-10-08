# lab_quality_leaderboard: by_industry within-industry ranks; industry_avg mcap weights

suppressPackageStartupMessages({
  if (!requireNamespace("testthat", quietly = TRUE)) stop("testthat required")
})

testthat::local_edition(3)

root <- if (file.exists("lab_industry_method.R")) {
  "."
} else if (file.exists("../lab_industry_method.R")) {
  ".."
} else {
  stop("Cannot locate lab_industry_method.R")
}
setwd(root)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
source("lab_industry_method.R", local = TRUE, encoding = "UTF-8")
if (file.exists("lab_tw_universe.R")) {
  tryCatch(source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8"), error = function(e) NULL)
}

.mk_row <- function(tk, ind_key, ind_lab, cagr, fs = 8, mcap = 1e9) {
  data.frame(
    ticker = tk,
    industry_key = ind_key,
    industry_label = ind_lab,
    primary = "dcf",
    method_used = "dcf",
    company_name = tk,
    upside_cagr_pct = cagr,
    upside_total_pct = cagr * 5,
    f_score = fs,
    quality_flag = 1,
    market_cap = mcap,
    stringsAsFactors = FALSE
  )
}

df <- rbind(
  .mk_row("A1", "semi", "Semiconductors", 40, mcap = 100),
  .mk_row("A2", "semi", "Semiconductors", 30, mcap = 50),
  .mk_row("A3", "semi", "Semiconductors", 20, mcap = 10),
  .mk_row("B1", "soft", "Software", 50, mcap = 80),
  .mk_row("B2", "soft", "Software", 10, mcap = 20),
  .mk_row("C1", "bank", "Banks", 60, mcap = 200)
)

testthat::test_that("by_industry Top-K keeps cross-industry order but Industry rank starts at 1 per industry", {
  lb <- lab_quality_leaderboard(
    df, top_n = 4L, eq_only = FALSE, gate_only = FALSE,
    scope = "by_industry",
    industry_filter = c("semi", "soft", "bank")
  )
  testthat::expect_equal(nrow(lb), 4L)
  # Global CAGR order: C1(60), B1(50), A1(40), A2(30)
  testthat::expect_identical(as.character(lb$代號), c("C1", "B1", "A1", "A2"))
  testthat::expect_true("產業內排名" %in% names(lb))
  # Each industry restarts at 1 — not a cross-industry serial 1..4
  testthat::expect_identical(as.integer(lb$產業內排名), c(1L, 1L, 1L, 2L))
})

testthat::test_that("overall is Top-K across the provided pool with serial ranks", {
  lb <- lab_quality_leaderboard(
    df, top_n = 3L, eq_only = FALSE, gate_only = FALSE,
    scope = "overall",
    industry_filter = character(0)
  )
  testthat::expect_equal(nrow(lb), 3L)
  testthat::expect_identical(as.character(lb$代號), c("C1", "B1", "A1"))
  testthat::expect_true("排名" %in% names(lb))
  testthat::expect_identical(as.integer(lb$排名), c(1L, 2L, 3L))
})

testthat::test_that("by_industry with one industry returns that industry's Top-K from 1", {
  lb <- lab_quality_leaderboard(
    df, top_n = 2L, eq_only = FALSE, gate_only = FALSE,
    scope = "by_industry",
    industry_filter = "semi"
  )
  testthat::expect_equal(nrow(lb), 2L)
  testthat::expect_identical(as.character(lb$代號), c("A1", "A2"))
  testthat::expect_true(all(lb$產業 == "Semiconductors"))
  testthat::expect_identical(as.integer(lb$產業內排名), c(1L, 2L))
})

testthat::test_that("industry_avg ranks by mcap-weighted average upside", {
  # Soft: (80*50 + 20*10) / 100 = 42
  # Semi: (100*40 + 50*30 + 10*20) / 160 = 35.625
  # Bank: 60
  lb <- lab_quality_leaderboard(
    df, top_n = 10L, eq_only = FALSE, gate_only = FALSE,
    scope = "industry_avg",
    industry_filter = character(0)
  )
  testthat::expect_equal(nrow(lb), 3L)
  testthat::expect_identical(as.character(lb$產業), c("Banks", "Software", "Semiconductors"))
  testthat::expect_true("市值加權年化估值漲幅" %in% names(lb))
  testthat::expect_identical(as.integer(lb$排名), c(1L, 2L, 3L))
  # Banks = +60.0%, Software = +42.0%
  testthat::expect_true(grepl("\\+60\\.0%", lb[["市值加權年化估值漲幅"]][[1]]))
  testthat::expect_true(grepl("\\+42\\.0%", lb[["市值加權年化估值漲幅"]][[2]]))
})

testthat::test_that("undervalued ranks by total FV–price gap, not CAGR", {
  # Same CAGR order would put C1 first (60); total gap order differs when horizon differs.
  uv <- rbind(
    .mk_row("X1", "semi", "Semiconductors", 20, mcap = 10),  # total 100
    .mk_row("X2", "soft", "Software", 40, mcap = 10),         # total 200
    .mk_row("X3", "bank", "Banks", 50, mcap = 10),            # total 250
    .mk_row("X4", "semi", "Semiconductors", -5, mcap = 10)    # overvalued — excluded
  )
  # Override totals so ranking ≠ CAGR order: X1 total largest despite lower CAGR
  uv$upside_total_pct <- c(300, 150, 100, -25)
  lb <- lab_quality_leaderboard(
    uv, top_n = 10L, eq_only = FALSE, gate_only = FALSE,
    scope = "undervalued",
    industry_filter = character(0)
  )
  testthat::expect_equal(nrow(lb), 3L)
  testthat::expect_identical(as.character(lb$代號), c("X1", "X2", "X3"))
  testthat::expect_true(grepl("\\+300\\.0%", lb[["總潛在漲幅"]][[1]], fixed = FALSE))
  testthat::expect_false("X4" %in% as.character(lb$代號))
})

testthat::test_that("TW board normalizer maps 上市／上櫃／興櫃", {
  testthat::skip_if_not(exists("lab_normalize_tw_boards", mode = "function"))
  testthat::expect_identical(
    lab_normalize_tw_boards(c("上市", "興櫃")),
    c("TWSE", "ESB")
  )
  testthat::expect_true(lab_tw_exchange_in_boards("TPEX", c("TPEX")))
  testthat::expect_false(lab_tw_exchange_in_boards("ESB", c("TWSE", "TPEX")))
})

cat("PASS test_lab_quality_leaderboard\n")
