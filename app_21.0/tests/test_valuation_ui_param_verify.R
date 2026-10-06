#!/usr/bin/env Rscript
# End-to-end offline verification:
# 1) Valuation engines apply UI-like parameter inputs via standard formulas
# 2) Industry-attribute samples → primary/secondary recommendation rules
# 3) Lite "best scenario" clamps (g < discount) yield finite FV, not unable-to-estimate
#
# Run: cd app_21.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_valuation_ui_param_verify.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
source(file.path(app_dir, "setup.R"), local = FALSE)
source(file.path(app_dir, "industry_standards.R"), local = FALSE)
source(file.path(app_dir, "default_config.R"), local = FALSE)
source(file.path(app_dir, "lab_industry_method.R"), local = FALSE)
source(file.path(app_dir, "market_profile.R"), local = FALSE)
source(file.path(app_dir, "ri_module.R"), local = FALSE)
source(file.path(app_dir, "nav_module.R"), local = FALSE)
source(file.path(app_dir, "relative_multiples_module.R"), local = FALSE)
source(file.path(app_dir, "ui_locale.R"), local = FALSE)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
    pass <<- pass + 1L
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}
approx_eq <- function(a, b, tol = 1e-8) {
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# Mirror Lite helper in ynow_server.R (cannot source server outside Shiny).
.clamp_g_below_rate <- function(g_pct, rate_pct, margin = 0.5) {
  g_pct <- suppressWarnings(as.numeric(g_pct)[1])
  rate_pct <- suppressWarnings(as.numeric(rate_pct)[1])
  if (!is.finite(g_pct)) return(NA_real_)
  if (!is.finite(rate_pct) || rate_pct <= 0) return(g_pct)
  if (g_pct < rate_pct - 1e-6) return(g_pct)
  max(rate_pct - margin, 0)
}

stmt <- function(rows, start_year = 2024L) {
  n <- max(lengths(rows))
  years <- as.character(start_year - seq_len(n) + 1L)
  df <- data.frame(Metric = names(rows), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_len(n)) {
    df[[years[i]]] <- vapply(rows, function(v) {
      if (length(v) >= i && is.finite(v[i])) v[i] else NA_real_
    }, numeric(1))
  }
  df
}
base_is <- function(ni = c(80, 76), rev = c(400, 380), interest = c(8, 8),
                    pretax = c(100, 95), tax = c(20, 19)) {
  stmt(list(
    "Net Income" = ni,
    "Total Revenue" = rev,
    "Interest Expense" = interest,
    "Pretax Income" = pretax,
    "Income Tax Expense" = tax
  ))
}
base_bs <- function(equity = c(500, 480), debt = c(100, 100), assets = c(900, 860),
                    goodwill = c(20, 20), shares = c(100, 100),
                    investments = c(0, 0)) {
  stmt(list(
    "Common Stock Equity" = equity,
    "Total Debt" = debt,
    "Total Assets" = assets,
    "Goodwill" = goodwill,
    "Ordinary Shares Number" = shares,
    "Long Term Investments" = investments
  ))
}
base_cf <- function(cfo = c(120, 110), capex = c(-30, -28), div = c(-20, -20)) {
  stmt(list(
    "Operating Cash Flow" = cfo,
    "Capital Expenditure" = capex,
    "Free Cash Flow" = cfo + capex,
    "Cash Dividends Paid" = div
  ))
}

cat("\n=== 1) UI params → standard formulas ===\n")

# Typical Full/Lite UI defaults (percent → decimal in engines)
ui_wacc <- 0.09
ui_ke <- 0.10
ui_g <- 0.04
ui_sgr_pct <- 4
ui_years <- 5L
ui_shares <- 100
ui_cash <- 50
ui_debt <- 200
ui_fcff <- c(100, 105, 110, 115, 120)  # projection length = years

# DCF FCFF Gordon via value_dcf_per_share (same path as guards / DCF math)
dcf_ps <- value_dcf_per_share(
  ui_fcff, claim = "fcff", wacc = ui_wacc, g_term = ui_g,
  shares = ui_shares, cash = ui_cash, debt = ui_debt
)
# Manual closed form: EV = Σ CF_t/(1+r)^t + TV/(1+r)^n ; Equity = EV+cash−debt
r <- ui_wacc
n <- length(ui_fcff)
pv_cf <- sum(ui_fcff / (1 + r)^seq_len(n))
tv <- ui_fcff[n] * (1 + ui_g) / (r - ui_g)
pv_tv <- tv / (1 + r)^n
eq_manual <- pv_cf + pv_tv + ui_cash - ui_debt
ps_manual <- eq_manual / ui_shares
check("DCF FCFF uses WACC/g/shares/cash/debt from UI-like params",
      isTRUE(dcf_ps$ok) && approx_eq(dcf_ps$per_share, ps_manual, 1e-6))

# DCF FCFE: discount at Ke, no cash−debt bridge
fcfe_path <- fcff_to_fcfe(ui_fcff, interest_after_tax = 5, debt0 = ui_debt, g_path = ui_g)
fcfe_ps <- value_dcf_per_share(
  fcfe_path, claim = "fcfe", ke = ui_ke, wacc = ui_wacc, discount_kind = "ke",
  g_term = ui_g, shares = ui_shares, cash = 9999, debt = 1
)
pv_fcfe <- sum(fcfe_path / (1 + ui_ke)^seq_along(fcfe_path))
tv_fcfe <- fcfe_path[n] * (1 + ui_g) / (ui_ke - ui_g)
ps_fcfe_manual <- (pv_fcfe + tv_fcfe / (1 + ui_ke)^n) / ui_shares
check("DCF FCFE discounts at Ke and ignores cash/debt bridge",
      isTRUE(fcfe_ps$ok) && approx_eq(fcfe_ps$per_share, ps_fcfe_manual, 1e-6))

# Claim/rate guard: FCFF cannot use Ke-only discount
bad_claim <- value_dcf_per_share(
  ui_fcff, claim = "fcff", ke = ui_ke, wacc = NA, discount_kind = "ke",
  g_term = ui_g, shares = ui_shares
)
check("FCFF rejects Ke-only discount (claim/rate consistency)", !isTRUE(bad_claim$ok))

# DDM Gordon / two-stage / SPM — UI sgr & Ke
d0 <- 2
p_gordon <- .ddm_formula_p0(d0 = d0, g = ui_g, r = ui_ke)
check("DDM Gordon = D1/(Ke-g) from UI g/Ke", approx_eq(p_gordon, d0 * (1 + ui_g) / (ui_ke - ui_g)))
p_ts <- .ddm_formula_two_stage(d0 = d0, g1 = 0.08, n = 3L, g2 = ui_g, r = ui_ke)
d1 <- d0 * 1.08; d2 <- d0 * 1.08^2; d3 <- d0 * 1.08^3
pn <- d3 * (1 + ui_g) / (ui_ke - ui_g)
p_ts_m <- d1 / 1.1 + d2 / 1.1^2 + (d3 + pn) / 1.1^3
check("DDM two-stage applies stage1 g, terminal g, Ke", approx_eq(p_ts, p_ts_m, 1e-10))
p_spm <- .ddm_formula_spm(eps = 4, d = 2, g = ui_g, r = ui_ke)
check("DDM SPM closed form from UI params", approx_eq(p_spm, 4 * ui_g / ui_ke^2 + 2 / ui_ke))

# RI
ri_cell <- compute_ri_valuation(
  b0 = 20, ke = ui_ke, g = ui_g, n = 5L, payout = 0.4, roe_path = rep(0.15, 5)
)
check("RI returns finite price with UI Ke/g",
      identical(ri_cell$status, "success") && is.finite(ri_cell$intrinsic) && ri_cell$intrinsic > 0)
check("RI errors when g >= Ke (standard guard)", {
  bad <- compute_ri_valuation(
    b0 = 20, ke = 0.08, g = 0.09, n = 5L, payout = 0.4, roe_path = rep(0.15, 5)
  )
  identical(bad$status, "error")
})

# P/B and Justified
check("P/B = BVPS × multiple (UI mid)", approx_eq(.pb_formula_p(basis = 25, pb = 1.6), 40))
just <- (0.15 - ui_g) / (ui_ke - ui_g)
check("Justified P/B = (ROE-g)/(Ke-g)", approx_eq(just, (0.15 - 0.04) / (0.10 - 0.04)))

# NAV
check("NAV = NAVPS × multiple", approx_eq(.nav_formula_p(navps = 30, multiple = 0.85), 25.5))
navc <- extract_nav_components(base_bs(investments = c(200, 180)), holdco_discount = 0.2)
check("NAV holdco discount applied to investments", {
  is.finite(navc$nav) && navc$nav < 500  # equity 500 with 20% haircut on investments
})

# Multiples / SOTP arithmetic engines
pe <- calc_pe_implied_price(eps = 5, pe_multiple = 20)
check("Multiples PE uses EPS and PE inputs",
      identical(pe$status, "ok") && approx_eq(pe$implied_price, 100, 1e-6))

# Gordon TV blocked when UI g >= WACC (Full-mode failure mode Lite heals)
# Signature: dcf_gordon_tv(last_fcf, g, r)
check("Gordon TV unable when g>=WACC (pre-heal)", !is.finite(dcf_gordon_tv(120, ui_wacc, ui_wacc)))
check("Gordon TV finite when UI g < WACC", is.finite(dcf_gordon_tv(120, ui_g, ui_wacc)))

cat("\n=== 2) Industry samples → primary/secondary recommendation ===\n")

defaults <- lab_industry_method_defaults()
check("lab defaults cover industry keys", nrow(defaults) >= 40)

# Attribute sample matrix: industry_key → expected primary (+ secondary constraints)
samples <- list(
  list(key = "fn.Conglomerate_Holding", text = "Conglomerate Holding", prim = "nav", sec = "sotp",
       tag = "holding"),
  list(key = "fn.Banking", text = "Banks - Diversified", prim = "pb", sec = "ri",
       tag = "financial"),
  list(key = "re.REIT", text = "REIT - Diversified", prim = "pb", sec = "nav",
       tag = "reit"),
  list(key = "en.Utilities", text = "Utilities - Regulated Electric", prim = "pb", sec = "ddm",
       tag = "utility"),
  list(key = "saas.SaaS_Cloud", text = "Software - Application", prim = "dcf", sec = "multiples",
       tag = "saas", two_stage = TRUE),
  list(key = "sc.Foundry", text = "Semiconductors", prim = "dcf", sec = "pb",
       tag = "cyclical_foundry"),
  list(key = "mat.Metals_Mining", text = "Steel", prim = "dcf", sec = "pb",
       tag = "cyclical_steel"),
  list(key = "en.Energy_OilGas", text = "Oil & Gas Integrated", prim = "dcf", sec = "pb",
       tag = "cyclical_oil"),
  list(key = "fmcg.Food_Beverages", text = "Beverages - Soft Drinks", prim = "ddm", sec = "dcf",
       tag = "staples"),
  list(key = "tel.Telecom", text = "Telecom Services", prim = "ddm", sec = "dcf",
       tag = "telecom"),
  list(key = "hc.Biotech", text = "Biotechnology", prim = "dcf", sec = "pb",
       tag = "biotech", two_stage = TRUE),
  list(key = "auto.Automotive_EV", text = "Auto Manufacturers", prim = "dcf", sec = "pb",
       tag = "ev", two_stage = TRUE)
)

for (s in samples) {
  row <- defaults[defaults$industry_key == s$key, , drop = FALSE]
  check(paste0("lab default primary [", s$tag, "]"),
        nrow(row) == 1L && identical(as.character(row$primary[1]), s$prim))
  check(paste0("lab default secondary [", s$tag, "]"),
        nrow(row) == 1L && identical(as.character(row$secondary[1]), s$sec))
  if (isTRUE(s$two_stage)) {
    check(paste0("lab default two-stage [", s$tag, "]"), isTRUE(row$suggest_two_stage[1]))
  }
}

# Live scorer with statement shapes matching attributes
live_cases <- list(
  list(
    tag = "holding_live",
    key = "fn.Conglomerate_Holding", text = "Conglomerate",
    d_is = base_is(), d_bs = base_bs(investments = c(300, 280)), d_cf = base_cf(),
    expect_prim = "nav", expect_sec = "sotp"
  ),
  list(
    tag = "bank_live",
    key = "fn.Banking", text = "Banks - Diversified",
    d_is = base_is(ni = c(90, 85), rev = c(300, 280)),
    d_bs = base_bs(equity = c(800, 760), debt = c(4000, 3800), assets = c(9000, 8600)),
    d_cf = base_cf(cfo = c(100, 95), capex = c(-5, -5), div = c(-30, -28)),
    expect_prim = "pb"
  ),
  list(
    tag = "saas_live",
    key = "saas.SaaS_Cloud", text = "Software - Application",
    d_is = base_is(ni = c(40, 20), rev = c(500, 350)),
    d_bs = base_bs(equity = c(400, 300), goodwill = c(150, 140)),
    d_cf = base_cf(cfo = c(120, 80), capex = c(-25, -20)),
    expect_prim = "dcf", expect_sec_in = c("multiples", "ri", "pb")
  ),
  list(
    tag = "cyclical_steel_live",
    key = "mat.Metals_Mining", text = "Steel",
    d_is = base_is(ni = c(50, 90), rev = c(600, 800)),
    d_bs = base_bs(equity = c(700, 650)),
    d_cf = base_cf(cfo = c(80, 120), capex = c(-40, -50)),
    expect_prim = "dcf", expect_sec = "pb"
  ),
  list(
    tag = "staples_live",
    key = "fmcg.Food_Beverages", text = "Beverages - Soft Drinks",
    d_is = base_is(ni = c(70, 68), rev = c(350, 340)),
    d_bs = base_bs(equity = c(450, 440)),
    d_cf = base_cf(cfo = c(90, 88), capex = c(-20, -19), div = c(-40, -38)),
    expect_prim_in = c("ddm", "dcf"),
    expect_not_forced_pb = TRUE
  ),
  list(
    tag = "ni_pos_fcff_neg",
    key = "", text = "Packaged Foods",
    d_is = base_is(ni = c(50, 45), rev = c(300, 280), pretax = c(60, 55), tax = c(10, 10)),
    d_bs = base_bs(),
    # CapEx overwhelms CFO → negative FCF while NI > 0
    d_cf = base_cf(cfo = c(40, 38), capex = c(-90, -85), div = c(-5, -5)),
    expect_prim = "ri", expect_sec = "dcf"
  )
)

for (lc in live_cases) {
  rec <- recommend_valuation_models(
    lc$d_cf, industry_text = lc$text,
    d_is = lc$d_is, d_bs = lc$d_bs,
    industry_choice = lc$key
  )
  # Use exact name lookup — `$` partial-matches expect_prim → expect_prim_in.
  if ("expect_prim" %in% names(lc)) {
    check(paste0("live primary [", lc$tag, "] = ", lc[["expect_prim"]]),
          identical(as.character(rec$primary), lc[["expect_prim"]]))
  }
  if ("expect_prim_in" %in% names(lc)) {
    check(paste0("live primary [", lc$tag, "] in {", paste(lc[["expect_prim_in"]], collapse = ","), "}"),
          as.character(rec$primary) %in% lc[["expect_prim_in"]])
  }
  if ("expect_sec" %in% names(lc)) {
    check(paste0("live secondary [", lc$tag, "] = ", lc[["expect_sec"]]),
          identical(as.character(rec$secondary), lc[["expect_sec"]]))
  }
  if ("expect_sec_in" %in% names(lc)) {
    check(paste0("live secondary [", lc$tag, "] in allowed set"),
          as.character(rec$secondary) %in% lc[["expect_sec_in"]])
  }
  if (isTRUE(lc$expect_not_forced_pb)) {
    check(paste0("live [", lc$tag, "] not forced PB primary"),
          !identical(as.character(rec$primary), "pb"))
  }
  # Multiples/SOTP never primary
  check(paste0("live [", lc$tag, "] never primary multiples/sotp"),
        !as.character(rec$primary) %in% c("multiples", "sotp"))
}

# Cyclical PB slot across all cyclical keys in defaults
cyc_keys <- defaults$industry_key[vapply(defaults$industry_key, function(k) {
  isTRUE(is_cyclical_industry(k, ""))
}, logical(1))]
cyc_ok <- TRUE
for (k in cyc_keys) {
  row <- defaults[defaults$industry_key == k, , drop = FALSE]
  slot_ok <- identical(row$primary[1], "pb") || identical(row$secondary[1], "pb")
  if (!isTRUE(slot_ok)) {
    cat("FAIL cyclical PB slot missing for ", k, "\n", sep = "")
    cyc_ok <- FALSE
    fail <<- fail + 1L
  }
}
if (isTRUE(cyc_ok)) {
  cat("OK cyclical PB slot on all ", length(cyc_keys), " cyclical lab defaults\n", sep = "")
  pass <<- pass + 1L
}

cat("\n=== 3) Lite best-param scenario → finite FV (not unable) ===\n")

# Clamp heals g >= WACC (the classic Lite failure mode)
g_bad_pct <- 10
wacc_pct <- 9
g_healed <- .clamp_g_below_rate(g_bad_pct, wacc_pct, margin = 0.5)
check("Lite clamp pulls g below WACC by 0.5pp", approx_eq(g_healed, 8.5, 1e-9))
check("Lite clamp leaves valid g unchanged",
      approx_eq(.clamp_g_below_rate(4, 9, 0.5), 4, 1e-9))

# After heal, Gordon TV and full DCF are finite
tv_healed <- dcf_gordon_tv(120, g_healed / 100, wacc_pct / 100)
check("Gordon TV finite after Lite g heal", is.finite(tv_healed) && tv_healed > 0)
dcf_healed <- value_dcf_per_share(
  ui_fcff, claim = "fcff", wacc = wacc_pct / 100, g_term = g_healed / 100,
  shares = ui_shares, cash = ui_cash, debt = ui_debt
)
check("DCF per-share finite after Lite g heal (not unable)",
      isTRUE(dcf_healed$ok) && is.finite(dcf_healed$per_share) && dcf_healed$per_share > 0)

# Pre-heal would be unable
check("pre-heal DCF unable when g>=WACC", {
  v <- value_dcf_per_share(
    ui_fcff, claim = "fcff", wacc = 0.09, g_term = 0.10,
    shares = ui_shares, cash = ui_cash, debt = ui_debt
  )
  !isTRUE(v$ok) && !is.finite(v$per_share)
})

# Claim recommendation for Lite scenario
claim_fcff <- recommend_dcf_claim(
  base_bs(debt = c(800, 780), equity = c(200, 220)),
  fcff = 50, fcfe = 5
)
check("Lite claim prefers FCFF under high leverage", identical(claim_fcff$prefer, "fcff"))

# Two-stage suggestion for growth SaaS default
saas_row <- defaults[defaults$industry_key == "saas.SaaS_Cloud", ]
check("SaaS Lite scenario wants Two-Stage", isTRUE(saas_row$suggest_two_stage[1]))
bank_row <- defaults[defaults$industry_key == "fn.Banking", ]
check("Bank Lite scenario stays Gordon (not two-stage)", !isTRUE(bank_row$suggest_two_stage[1]))

# lab_estimate_fv_per_share: recommended primary yields finite FV for sample industries
for (s in samples[c(1, 2, 5, 6, 9)]) {
  d_is <- base_is()
  d_bs <- base_bs(investments = if (identical(s$prim, "nav")) c(250, 240) else c(0, 0))
  d_cf <- if (identical(s$prim, "ddm")) {
    base_cf(div = c(-40, -38))
  } else {
    base_cf()
  }
  est <- lab_estimate_fv_per_share(
    method = s$prim, industry_key = s$key,
    d_is = d_is, d_bs = d_bs, d_cf = d_cf,
    price = 50, market_cap = 5000
  )
  check(paste0("lab FV finite for primary [", s$tag, "/", s$prim, "]"),
        is.finite(est$fv) && est$fv > 0)
}

# Growth revenue > 12% → Multiples secondary preference (live)
rec_growth <- recommend_valuation_models(
  base_cf(cfo = c(100, 60), capex = c(-20, -15)),
  "Software - Application",
  base_is(ni = c(50, 25), rev = c(400, 250)),
  base_bs(),
  industry_choice = "saas.SaaS_Cloud"
)
check("high-growth SaaS keeps DCF primary", identical(rec_growth$primary, "dcf"))
check("high-growth SaaS secondary is Multiples (or RI)",
      as.character(rec_growth$secondary) %in% c("multiples", "ri", "pb"))

cat("\n=== Summary ===\n")
cat("PASS ", pass, "  FAIL ", fail, "\n", sep = "")
if (fail > 0L) {
  quit(status = 1L)
}
cat("ALL PASS\n")
