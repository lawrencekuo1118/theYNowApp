#!/usr/bin/env Rscript
# Offline smoke: deepen FCF projection / CapEx sync beyond test_fcf_capex_sync.R.
# Asserts current outputs — does not change valuation formulas.
# Run: cd app_28.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_fcf_projection_smoke.R

local({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  cands <- character()
  if (length(f) == 1L && nzchar(f[[1]])) {
    cands <- c(
      cands,
      file.path(dirname(normalizePath(f[[1]], mustWork = FALSE)), "ynow_test_app_dir.R")
    )
  }
  wd <- normalizePath(getwd(), mustWork = FALSE)
  cands <- c(
    cands,
    file.path(wd, "ynow_test_app_dir.R"),
    file.path(wd, "tests", "ynow_test_app_dir.R"),
    file.path(wd, "app_28.0", "tests", "ynow_test_app_dir.R")
  )
  hit <- cands[nzchar(cands) & !is.na(cands) & file.exists(cands)]
  if (!length(hit)) stop("Cannot locate ynow_test_app_dir.R", call. = FALSE)
  source(hit[[1]], local = FALSE)
})
root <- ynow_test_app_dir("setup.R")
setwd(root)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

source("setup.R", local = FALSE)
source("fcf_projection_module.R", local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

check("fcf_projection_module_server exported", exists("fcf_projection_module_server", mode = "function"))
check("fcf_projection_module_ui exported", exists("fcf_projection_module_ui", mode = "function"))
check("fcf_sync_capex_revenue_margin exported", exists("fcf_sync_capex_revenue_margin", mode = "function"))
check(".dcf_unit_fcff_path exported", exists(".dcf_unit_fcff_path", mode = "function"))

# CapEx sync edge fixtures (complement test_fcf_capex_sync.R)
empty <- fcf_sync_capex_revenue_margin(numeric(0), numeric(0))
check("empty hist → not ok", isFALSE(empty$ok) && !isTRUE(empty$spike))

bad_rev <- fcf_sync_capex_revenue_margin(c(10, 10), c(0, 100))
check("zero latest rev → not ok", isFALSE(bad_rev$ok))

# Borderline spike: latest = exactly mult × prior mean → NOT spike (strict >)
# prior years 10/100=0.10, 10/100=0.10 → prior mean 0.10; latest 13.5/100=0.135 = 1.35×
border <- fcf_sync_capex_revenue_margin(
  c(13.5, 10, 10), c(100, 100, 100),
  list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("exact mult borderline not spike", isFALSE(border$spike) && approx_eq(border$pct, 13.5, 0.01))

# Just over threshold → spike → seeds 3y avg
over <- fcf_sync_capex_revenue_margin(
  c(14, 10, 10), c(100, 100, 100),
  list(enabled = TRUE, mult = 1.35, avg_n = 3L, prior_n = 2L)
)
check("over mult is spike", isTRUE(over$spike))
check("spike seeds 3y avg ~11.333%", approx_eq(over$pct, (14 + 10 + 10) / 3, 0.05))

# Unit FCFF path (projection engine under FCF tab)
fcff1 <- .dcf_unit_fcff_path(
  n = 1, g_near = 0.10, nopat_m = 0.20, depre_m = 0.05, capex_m = 0.08, nwc_m = 0.10
)
rev1 <- 1.10
fcff1_closed <- rev1 * (0.20 + 0.05 - 0.08) - (rev1 - 1) * 0.10
check("unit FCFF y1 NOPAT path", approx_eq(fcff1[1], fcff1_closed))

fcff3 <- .dcf_unit_fcff_path(
  n = 3, g_near = 0.05, nopat_m = 0.15, depre_m = 0.04, capex_m = 0.06, nwc_m = 0.02
)
check("unit FCFF length 3", length(fcff3) == 3L && all(is.finite(fcff3)))
# y1: rev=1.05, FCFF = 1.05*(0.15+0.04-0.06) - 0.05*0.02
check("unit FCFF y1 closed", approx_eq(fcff3[1], 1.05 * 0.13 - 0.05 * 0.02))

# Mirror fcf_projection_module::proj_table_data: when near-term g > 0 and
# NOPAT margin is held flat, both NOPAT and FCFF must rise together.
.proj_nopat_fcff <- function(n, g, base_rev, base_nopat, base_depre, base_capex, base_nwc) {
  nopat_m <- base_nopat / base_rev
  depre_m <- base_depre / base_rev
  capex_m <- base_capex / base_rev
  nwc_m <- base_nwc / base_rev
  rev <- nopat <- fcff <- numeric(n)
  for (i in seq_len(n)) {
    prev <- if (i == 1L) base_rev else rev[i - 1L]
    rev[i] <- prev * (1 + g)
    nopat[i] <- rev[i] * nopat_m
    dn <- (rev[i] - prev) * nwc_m
    fcff[i] <- nopat[i] + rev[i] * depre_m - rev[i] * capex_m - dn
  }
  list(nopat = nopat, fcff = fcff, rev = rev)
}
grow <- .proj_nopat_fcff(5L, 0.05, 100, 20, 5, 8, 2)
check("5y NOPAT grows with positive g", all(diff(grow$nopat) > 0))
check("5y FCFF grows with positive g", all(diff(grow$fcff) > 0))
check(
  "NOPAT CAGR matches revenue g (flat margin)",
  approx_eq((grow$nopat[5] / grow$nopat[1])^(1 / 4) - 1, 0.05, 1e-10)
)
check(
  "FCFF CAGR matches revenue g (flat margins)",
  approx_eq((grow$fcff[5] / grow$fcff[1])^(1 / 4) - 1, 0.05, 1e-10)
)
flat <- .proj_nopat_fcff(5L, 0, 100, 20, 5, 8, 2)
check("g=0 keeps NOPAT flat (not a bug)", all(abs(diff(flat$nopat)) < 1e-12))
check("g=0 keeps FCFF flat (not a bug)", all(abs(diff(flat$fcff)) < 1e-12))

# Trailing-FCFF hist-anchor must use near-term g (Gordon), not terminal SGR.
check(".fcff_trailing_growth_path exported", exists(".fcff_trailing_growth_path", mode = "function"))
anch_near <- .fcff_trailing_growth_path(100, 5L, mode = "gordon", g_near = 0.08)
anch_wrong_sgr <- 100 * (1.03)^seq_len(5) # old bug: grew at SGR=3%
check("Gordon trailing path uses near-term g=8%", {
  length(anch_near) == 5L &&
    approx_eq(anch_near[1], 100 * 1.08) &&
    approx_eq(anch_near[5], 100 * (1.08)^5)
})
check(
  "Gordon trailing path is not terminal SGR",
  is.null(anch_near) || abs(anch_near[5] - anch_wrong_sgr[5]) > 1
)
check(
  "Gordon trailing refuses missing near-term g",
  is.null(.fcff_trailing_growth_path(100, 5L, mode = "gordon", g_near = NA_real_))
)
anch_ts <- .fcff_trailing_growth_path(
  100, 5L, mode = "two_stage",
  g_near = 0.08, g_stage1 = 0.10, g_stage2 = 0.03, yr_stage1 = 2L
)
check("two-stage trailing uses g1 then g2", {
  length(anch_ts) == 5L &&
    approx_eq(anch_ts[1], 100 * 1.10) &&
    approx_eq(anch_ts[2], 100 * 1.10^2) &&
    approx_eq(anch_ts[3], 100 * 1.10^2 * 1.03)
})

# EV from projected FCFF + Gordon TV (same engine as DCF overview)
ev <- .dcf_formula_ev_from_fcff(fcff3, r1 = 0.10, g_term = 0.03)
tv <- fcff3[3] * 1.03 / (0.10 - 0.03)
dfs <- 1.10^seq_len(3)
ev_closed <- sum(fcff3 / dfs) + tv / dfs[3]
check("EV from FCFF path + TV", approx_eq(ev, ev_closed, 1e-8))
check("EV blocked when g>=r", !is.finite(.dcf_formula_ev_from_fcff(fcff3, 0.10, 0.10)))

src <- paste(readLines("fcf_projection_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("module exposes CapEx spike controls", grepl("apply_capex_spike_smooth", src, fixed = TRUE) &&
  grepl("capex_spike_mult", src, fixed = TRUE))
check("module calls fcf_sync_capex_revenue_margin", grepl("fcf_sync_capex_revenue_margin(", src, fixed = TRUE))

# FCFE formula banner: two lines, no full-width pipe on one line
fcfe_banner <- dcf_formula_banner_txt("fcfe")
fcff_banner <- dcf_formula_banner_txt("fcff")
fcfe_lines <- strsplit(fcfe_banner, "\n", fixed = TRUE)[[1]]
check("FCFE banner is two lines", length(fcfe_lines) == 2L)
check("FCFE banner line1 conversion", identical(
  fcfe_lines[[1]],
  "FCFE = FCFF − Interest×(1−T) + Net Borrowing"
))
check("FCFE banner line2 FCFF identity", identical(
  fcfe_lines[[2]],
  "FCFF = CFO + Interest×(1−T) − CapEx ＝ NOPAT + D&A − ΔNWC − CapEx"
))
check("FCFE banner has no pipe separator", !grepl("｜", fcfe_banner, fixed = TRUE))
check("FCFF banner stays single line", !grepl("\n", fcff_banner, fixed = TRUE) &&
  grepl("FCFF = CFO", fcff_banner, fixed = TRUE))
check("banner UI preserves newlines (pre-line)", grepl("white-space: pre-line", src, fixed = TRUE))

# Six FCF inputs: IS → reinvestment → IC stack (display order only; IDs unchanged)
check(
  "fcf param stack marks IS→reinvestment→IC order",
  grepl('data-ynow-fcf-param-order' , src, fixed = TRUE) &&
    grepl("revenue,nopat,da,capex,delta_nwc,ic", src, fixed = TRUE)
)
pos_rev <- regexpr('fcf_revenue"', src, fixed = TRUE)[1]
pos_nopat <- regexpr('fcf_nopat"', src, fixed = TRUE)[1]
pos_da <- regexpr('fcf_depreciation"', src, fixed = TRUE)[1]
pos_capex <- regexpr('fcf_capex"', src, fixed = TRUE)[1]
pos_nwc <- regexpr('fcf_delta_nwc"', src, fixed = TRUE)[1]
pos_ic <- regexpr('fcf_invested_capital"', src, fixed = TRUE)[1]
check(
  "UI source order is Revenue → NOPAT → D&A → CapEx → ΔNWC → IC",
  all(c(pos_rev, pos_nopat, pos_da, pos_capex, pos_nwc, pos_ic) > 0L) &&
    pos_rev < pos_nopat && pos_nopat < pos_da && pos_da < pos_capex &&
    pos_capex < pos_nwc && pos_nwc < pos_ic
)
check("CapEx appears before ΔNWC (statement / table flow)", pos_capex > 0L && pos_nwc > pos_capex)
check("formula-feel operators present", grepl("ynow-fcf-param-op--plus", src, fixed = TRUE) &&
  grepl("ynow-fcf-param-op--minus", src, fixed = TRUE))
check("FCFF identity label is display-only (no engine change)", grepl("fcf_param_identity", src, fixed = TRUE))

loc <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (k in c(
  "fcf_param_step_is", "fcf_param_step_reinvest", "fcf_param_step_ic",
  "fcf_param_revenue", "fcf_param_nopat", "fcf_param_da",
  "fcf_param_capex", "fcf_param_delta_nwc", "fcf_param_ic",
  "fcf_param_identity", "fcf_param_ic_note", "fcf_param_symbols",
  "fcf_sync_from_statements",
  "dcf_fcf_trend_title", "dcf_fcf_trend_nopat", "dcf_fcf_trend_fcff",
  "dcf_fcf_trend_fcfe", "dcf_fcf_trend_x", "dcf_fcf_trend_y"
)) {
  check(
    paste0("locale key ", k, " in en + zh-TW"),
    length(gregexpr(paste0(k, " ="), loc, fixed = TRUE)[[1]]) >= 2L
  )
}
ui_css <- paste(readLines("www/ynow_widgets.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("www CSS ships FCF param-stack rules", grepl(".ynow-fcf-param-stack", ui_css, fixed = TRUE))
mod_ui <- paste(readLines("fcf_projection_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("FCF module uses param-stack class", grepl("ynow-fcf-param-stack", mod_ui, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("All FCF projection smoke checks passed.\n")
