#!/usr/bin/env Rscript
# AMZN-like DCF edge cases: CapEx spike margins, D&A alias preference, mature-tech SGR.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) cat("OK ", label, "\n", sep = "")
  else { cat("FAIL ", label, "\n", sep = ""); fail <<- fail + 1L }
}
approx_eq <- function(a, b, tol = 1e-6) is.finite(a) && is.finite(b) && abs(a - b) <= tol

# avg_ratio_newest
check(
  "avg CapEx/Rev 3y",
  approx_eq(
    avg_ratio_newest(c(131.8, 83.0, 52.7), c(716.9, 638.0, 574.8), n = 3L),
    mean(c(131.8 / 716.9, 83.0 / 638.0, 52.7 / 574.8)),
    tol = 1e-9
  )
)

# DA_PATTERNS prefer broad line when both present
cf <- data.frame(
  Breakdown = c("Depreciation", "Depreciation And Amortization", "Capital Expenditure"),
  TTM = c("41860", "65756", "-131819"),
  Y2025 = c("41860", "65756", "-131819"),
  Y2024 = c("32067", "52795", "-82999"),
  stringsAsFactors = FALSE
)
dep <- select_clean_metric_row_any(cf, DA_PATTERNS, include_ttm = FALSE)
check("DA prefers And Amortization", approx_eq(dep[1], 65756))

# mature-tech SGR fallback
d_is <- data.frame(
  Breakdown = c("Net Income", "Total Revenue"),
  TTM = c("77670", "716924"),
  Y2025 = c("77670", "716924"),
  stringsAsFactors = FALSE
)
d_bs <- data.frame(
  Breakdown = "Stockholders Equity",
  Y2025 = "411065",
  stringsAsFactors = FALSE
)
raw <- calc_fundamental_sgr_pct(d_is, d_bs, NULL)
check("raw AMZN-like SGR high", is.finite(raw) && raw > 6)
est <- estimate_perpetual_g(
  method = "fundamental",
  rf_pct = 4.7,
  d_is = d_is,
  d_bs = d_bs,
  d_cf = NULL,
  ticker = "AMZN",
  wacc_pct = 9.0
)
# fundamental keeps Retention×ROE; WACC−2 clamp if g≥WACC (previous behavior)
check("AMZN fundamental SGR not forced to 2.75", !approx_eq(est$g_pct, 2.75, tol = 1e-9))
check("AMZN fundamental SGR clamped below WACC", is.finite(est$g_pct) && est$g_pct < 9)
check("reason stays Fundamental／SGR", grepl("Fundamental／SGR", est$reason, fixed = TRUE))

# Non-tech high ROE also uses fundamental then WACC clamp
est2 <- estimate_perpetual_g(
  method = "fundamental",
  rf_pct = 4.7,
  d_is = d_is,
  d_bs = d_bs,
  ticker = "XYZ",
  industry_text = "Packaged Foods",
  wacc_pct = 9.0
)
check("non-tech keeps high SGR then WACC clamp", is.finite(est2$g_pct) && est2$g_pct < 9)

# CapEx spike → hist avg of prior years; latest > 1.35× prior
latest <- 131.8 / 716.9
prior <- mean(c(83.0 / 638.0, 52.7 / 574.8))
check("AMZN CapEx spike vs prior", isTRUE(ratio_spike_vs_prior(
  c(131.8, 83.0, 52.7), c(716.9, 638.0, 574.8), prior_n = 2L, mult = 1.35
)))
check("prior mean below latest", latest > 1.35 * prior)

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All AMZN DCF edge checks passed.\n")
