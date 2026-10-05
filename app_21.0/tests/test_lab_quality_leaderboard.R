# lab_quality_leaderboard: by_industry = Top 10 within selected industries (one list)

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

.mk_row <- function(tk, ind_key, ind_lab, cagr, fs = 8) {
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
    stringsAsFactors = FALSE
  )
}

df <- rbind(
  .mk_row("A1", "semi", "Semiconductors", 40),
  .mk_row("A2", "semi", "Semiconductors", 30),
  .mk_row("A3", "semi", "Semiconductors", 20),
  .mk_row("B1", "soft", "Software", 50),
  .mk_row("B2", "soft", "Software", 10),
  .mk_row("C1", "bank", "Banks", 60)
)

testthat::test_that("by_industry is one Top-K within selected industries, not per-industry lists", {
  lb <- lab_quality_leaderboard(
    df, top_n = 2L, eq_only = FALSE, gate_only = FALSE,
    scope = "by_industry",
    industry_filter = c("semi", "soft")
  )
  testthat::expect_equal(nrow(lb), 2L)
  # Across selected industries: B1 (50) then A1 (40) — not 2 per industry
  testthat::expect_identical(as.character(lb$代號), c("B1", "A1"))
  testthat::expect_true("產業內排名" %in% names(lb))
  testthat::expect_false(nrow(lb) > 2L)
})

testthat::test_that("overall is Top-K across the provided pool", {
  lb <- lab_quality_leaderboard(
    df, top_n = 3L, eq_only = FALSE, gate_only = FALSE,
    scope = "overall",
    industry_filter = character(0)
  )
  testthat::expect_equal(nrow(lb), 3L)
  testthat::expect_identical(as.character(lb$代號), c("C1", "B1", "A1"))
  testthat::expect_true("排名" %in% names(lb))
})

testthat::test_that("by_industry with one industry returns that industry's Top-K", {
  lb <- lab_quality_leaderboard(
    df, top_n = 2L, eq_only = FALSE, gate_only = FALSE,
    scope = "by_industry",
    industry_filter = "semi"
  )
  testthat::expect_equal(nrow(lb), 2L)
  testthat::expect_identical(as.character(lb$代號), c("A1", "A2"))
  testthat::expect_true(all(lb$產業 == "Semiconductors"))
})

cat("PASS test_lab_quality_leaderboard\n")
