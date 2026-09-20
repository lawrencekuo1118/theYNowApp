#!/usr/bin/env Rscript
# Lifecycle tier classification: objective criteria + terminal-g point estimates.
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
approx_eq <- function(a, b, tol = 1e-9) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# --- classification precedence ---
check(
  "bank → mature_sunset",
  identical(classify_lifecycle_stage("Sector: Financial | Industry: Banks - Diversified"), "mature_sunset")
)
check(
  "utility → mature_sunset",
  identical(classify_lifecycle_stage("Electric Utilities"), "mature_sunset")
)
check(
  "bank high CAGR still sunset (industry before growth)",
  identical(
    classify_lifecycle_stage("Banks - Diversified", rev_cagr = 20),
    "mature_sunset"
  )
)
check(
  "software high CAGR → growth_to_mature (growth before tech)",
  identical(
    classify_lifecycle_stage("Sector: Technology | Industry: Software", rev_cagr = 12),
    "growth_to_mature"
  )
)
check(
  "software low CAGR → mature_tech",
  identical(
    classify_lifecycle_stage("Software — Infrastructure", rev_cagr = 4),
    "mature_tech"
  )
)
check(
  "software NA CAGR → mature_tech",
  identical(classify_lifecycle_stage("Information Technology", rev_cagr = NA_real_), "mature_tech")
)
check(
  "CAGR exactly 8 → not growth (threshold is >8)",
  identical(classify_lifecycle_stage("Packaged Foods", rev_cagr = 8), "mature_general")
)
check(
  "CAGR 8.1 non-tech → growth_to_mature",
  identical(classify_lifecycle_stage("Packaged Foods", rev_cagr = 8.1), "growth_to_mature")
)
check(
  "default → mature_general",
  identical(classify_lifecycle_stage("Household Products", rev_cagr = 3), "mature_general")
)

# detail evidence present
d <- classify_lifecycle_stage("Semiconductors", rev_cagr = 3, detail = TRUE)
check("detail returns list", is.list(d) && identical(d$stage, "mature_tech"))
check("detail has rule", identical(d$rule, "industry_tech_keywords"))
check("detail evidence_zh non-empty", nzchar(d$evidence_zh))
check("detail evidence_en non-empty", nzchar(d$evidence_en))

# --- lifecycle g point estimates (band midpoints) ---
est_sun <- estimate_perpetual_g(
  method = "lifecycle", industry_text = "Gas Utilities", lifecycle_stage = "auto", locale = "en"
)
check("sunset g=1.75", approx_eq(est_sun$g_pct, 1.75))
check("sunset evidence in reason", grepl("Objective evidence|highly mature", est_sun$reason, ignore.case = TRUE))

est_tech <- estimate_perpetual_g(
  method = "lifecycle",
  industry_text = "Consumer Electronics",
  rev_cagr = 2,
  lifecycle_stage = "auto",
  locale = "zh-TW"
)
check("tech g=2.75", approx_eq(est_tech$g_pct, 2.75))
check("tech evidence zh", grepl("客觀依據", est_tech$reason, fixed = TRUE))

est_g2m <- estimate_perpetual_g(
  method = "lifecycle",
  industry_text = "Software",
  rev_cagr = 15,
  lifecycle_stage = "auto"
)
check("high-growth tech → growth_to_mature stage", identical(est_g2m$lifecycle_stage, "growth_to_mature"))
check("growth_to_mature g=2.5", approx_eq(est_g2m$g_pct, 2.5))
check("growth_to_mature suggests two-stage", isTRUE(est_g2m$suggest_two_stage))

est_gen <- estimate_perpetual_g(
  method = "lifecycle", industry_text = "Apparel Retail", rev_cagr = 2, lifecycle_stage = "auto"
)
check("general g=2.5", approx_eq(est_gen$g_pct, 2.5))
check("general auto stage", identical(est_gen$auto_lifecycle, "mature_general"))

# method recommend: growth software → lifecycle
rec_g <- recommend_perpetual_g_method(
  rf_pct = 4.5,
  d_is = data.frame(Breakdown = c("Net Income", "Total Revenue"), Y2025 = c("10", "100"), stringsAsFactors = FALSE),
  d_bs = data.frame(Breakdown = "Stockholders Equity", Y2025 = "50", stringsAsFactors = FALSE),
  industry_text = "Software",
  rev_cagr = 18,
  wacc_pct = 9
)
check("high-growth software recommends lifecycle", identical(rec_g$method, "lifecycle"))
check("high-growth software auto_lifecycle", identical(rec_g$auto_lifecycle, "growth_to_mature"))
check("recommend embeds evidence", grepl("客觀依據|Objective", rec_g$reason))

if (fail > 0L) {
  cat("FAILED:", fail, "\n")
  quit(status = 1)
}
cat("All lifecycle SGR checks passed.\n")
