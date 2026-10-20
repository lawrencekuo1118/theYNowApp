# Phase 1 — Independent valuation oracles (hand / closed-form only).
# CRITICAL: This file must NOT source app_25.0 engines and must NOT call
# .dcf_formula_*, .ddm_formula_*, compute_ri_valuation, calc_* multiples, etc.
# Expected values are pure algebra so engine comparisons are non-circular.
#
# Source: source("tests/audit_independent_oracles.R", local = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

.oracle_approx_eq <- function(a, b, tol = 1e-8) {
  a <- suppressWarnings(as.numeric(a)[1])
  b <- suppressWarnings(as.numeric(b)[1])
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

# =============================================================================
# DCF oracles
# =============================================================================

#' Single-rate Gordon DCF on an explicit CF path.
#' PV_path + PV(TV); TV = CF_n(1+g)/(r−g); equity = EV+cash−debt (FCFF) or EV (FCFE).
oracle_dcf_path <- function(cf, r, g, cash = 0, debt = 0, shares = 1,
                            bridge = c("ev", "none")) {
  bridge <- match.arg(bridge)
  cf <- suppressWarnings(as.numeric(cf))
  stopifnot(length(cf) >= 1L, all(is.finite(cf)))
  r <- as.numeric(r)[1]; g <- as.numeric(g)[1]
  cash <- as.numeric(cash)[1]; debt <- as.numeric(debt)[1]
  shares <- as.numeric(shares)[1]
  n <- length(cf)
  disc <- (1 + r)^seq_len(n)
  pv_cf <- sum(cf / disc)
  tv <- cf[n] * (1 + g) / (r - g)
  pv_tv <- tv / disc[n]
  enterprise <- pv_cf + pv_tv
  equity <- if (identical(bridge, "ev")) enterprise + cash - debt else enterprise
  list(
    n = n, r = r, g = g,
    pv_cf = pv_cf, tv = tv, pv_tv = pv_tv,
    enterprise = enterprise, equity = equity,
    per_share = equity / shares,
    bridge = bridge
  )
}

#' Two-rate discount path: years 1..y1 at r1, remaining at r2; Gordon uses r2.
oracle_dcf_two_rate <- function(cf, r1, r2, g, yr_stage1, cash = 0, debt = 0, shares = 1) {
  cf <- suppressWarnings(as.numeric(cf))
  n <- length(cf)
  y1 <- as.integer(yr_stage1)[1]
  rs <- c(rep(as.numeric(r1)[1], min(y1, n)), rep(as.numeric(r2)[1], max(0L, n - y1)))
  disc <- cumprod(1 + rs)
  pv_cf <- sum(cf / disc)
  tv <- cf[n] * (1 + g) / (as.numeric(r2)[1] - g)
  pv_tv <- tv / disc[n]
  enterprise <- pv_cf + pv_tv
  equity <- enterprise + as.numeric(cash)[1] - as.numeric(debt)[1]
  list(
    rs = rs, disc = disc, pv_cf = pv_cf, tv = tv, pv_tv = pv_tv,
    enterprise = enterprise, equity = equity, per_share = equity / as.numeric(shares)[1]
  )
}

#' Unit growth path EV: F1=f0, Ft grows at g_near then optional g_stage2; TV at g_term/r2.
oracle_dcf_unit_gordon <- function(n, r1, g_term, g_near = 0, f0 = 1, r2 = NULL,
                                   yr_stage1 = NULL, g_stage2 = NULL) {
  n <- as.integer(n)[1]
  r1 <- as.numeric(r1)[1]
  r2 <- if (is.null(r2)) r1 else as.numeric(r2)[1]
  g_term <- as.numeric(g_term)[1]
  g_near <- as.numeric(g_near)[1]
  f0 <- as.numeric(f0)[1]
  two_stage <- !is.null(yr_stage1) && is.finite(as.numeric(yr_stage1)[1]) && n > 1L &&
    !is.null(g_stage2) && is.finite(as.numeric(g_stage2)[1])
  if (two_stage) {
    y1 <- as.integer(yr_stage1)[1]
    rs <- c(rep(r1, min(y1, n)), rep(r2, max(0L, n - y1)))
    g2 <- as.numeric(g_stage2)[1]
    gs <- c(rep(g_near, max(0L, y1 - 1L)), rep(g2, max(0L, n - y1)))
    if (length(gs) < n - 1L) {
      pad <- if (length(gs) == 0L) g_near else gs[length(gs)]
      gs <- c(gs, rep(pad, n - 1L - length(gs)))
    }
    gs <- gs[seq_len(max(0L, n - 1L))]
  } else {
    rs <- rep(r1, n)
    gs <- rep(g_near, max(0L, n - 1L))
  }
  f <- numeric(n)
  f[1] <- f0
  if (n >= 2L && length(gs) >= 1L) {
    for (t in seq_len(n - 1L)) f[t + 1L] <- f[t] * (1 + gs[t])
  }
  disc <- cumprod(1 + rs)
  pv <- sum(f / disc)
  tv <- f[n] * (1 + g_term) / (r2 - g_term)
  list(f = f, rs = rs, disc = disc, pv = pv, tv = tv, enterprise = pv + tv / disc[n])
}

#' FCFF → FCFE: FCFE_t = FCFF_t − IAT + g_t × beginning_debt; debt rolls with net borrowing.
oracle_fcff_to_fcfe <- function(fcff, interest_after_tax = 0, debt0 = 0, g_path = 0) {
  fcff <- as.numeric(fcff)
  n <- length(fcff)
  iat <- as.numeric(interest_after_tax)[1]
  if (!is.finite(iat)) iat <- 0
  debt <- as.numeric(debt0)[1]
  if (!is.finite(debt) || debt < 0) debt <- 0
  gp <- as.numeric(g_path)
  if (length(gp) == 1L) gp <- rep(gp, n)
  if (length(gp) < n) gp <- c(gp, rep(utils::tail(gp, 1), n - length(gp)))
  gp[!is.finite(gp)] <- 0
  fcfe <- numeric(n)
  beg <- numeric(n)
  nb <- numeric(n)
  for (i in seq_len(n)) {
    beg[i] <- debt
    nb[i] <- gp[i] * debt
    fcfe[i] <- fcff[i] - iat + nb[i]
    debt <- debt + nb[i]
  }
  list(FCFE = fcfe, BeginningDebt = beg, NetBorrowing = nb, InterestAfterTax = rep(iat, n))
}

oracle_gordon_tv <- function(last_cf, g, r) {
  as.numeric(last_cf)[1] * (1 + as.numeric(g)[1]) / (as.numeric(r)[1] - as.numeric(g)[1])
}

oracle_ev_to_equity <- function(ev, cash = 0, debt = 0) {
  as.numeric(ev)[1] + as.numeric(cash)[1] - as.numeric(debt)[1]
}

# =============================================================================
# DDM oracles
# =============================================================================

oracle_ddm_gordon <- function(d0, g, ke) {
  d0 <- as.numeric(d0)[1]; g <- as.numeric(g)[1]; ke <- as.numeric(ke)[1]
  if (!is.finite(d0) || !is.finite(g) || !is.finite(ke) || ke <= g) return(NA_real_)
  d1 <- d0 * (1 + g)
  list(d1 = d1, p0 = d1 / (ke - g))
}

#' SPM (Brown & Abraham): V0 = E·g / r² + D / r  (r > 0; no r>g required).
oracle_ddm_spm <- function(eps, d, g, ke) {
  eps <- as.numeric(eps)[1]; d <- as.numeric(d)[1]
  g <- as.numeric(g)[1]; ke <- as.numeric(ke)[1]
  if (!is.finite(eps) || !is.finite(d) || !is.finite(g) || !is.finite(ke) || ke <= 0) {
    return(NA_real_)
  }
  if (d < 0) d <- 0
  (eps * g) / (ke * ke) + d / ke
}

oracle_ddm_two_stage <- function(d0, g1, n, g2, ke) {
  d0 <- as.numeric(d0)[1]; g1 <- as.numeric(g1)[1]
  g2 <- as.numeric(g2)[1]; ke <- as.numeric(ke)[1]
  n <- as.integer(n)[1]
  if (!is.finite(d0) || !is.finite(g1) || !is.finite(g2) || !is.finite(ke) || ke <= g2) {
    return(list(p0 = NA_real_, dts = numeric(0), pn = NA_real_))
  }
  dts <- d0 * (1 + g1)^seq_len(n)
  dfs <- (1 + ke)^seq_len(n)
  pv_div <- sum(dts / dfs)
  pn <- dts[n] * (1 + g2) / (ke - g2)
  list(dts = dts, pn = pn, pv_div = pv_div, p0 = pv_div + pn / dfs[n])
}

# =============================================================================
# Residual Income oracles
# =============================================================================

#' RI with optional ROE path; BV rolls: End = Beg + NI − NI·payout.
oracle_ri <- function(b0, ke, g, n, payout, roe_path) {
  b0 <- as.numeric(b0)[1]; ke <- as.numeric(ke)[1]; g <- as.numeric(g)[1]
  n <- as.integer(n)[1]; payout <- max(0, min(1, as.numeric(payout)[1]))
  roe_path <- as.numeric(roe_path)
  stopifnot(length(roe_path) == n)
  beg <- numeric(n); ni <- numeric(n); div <- numeric(n)
  end <- numeric(n); ri <- numeric(n); pv <- numeric(n)
  curr <- b0
  for (i in seq_len(n)) {
    beg[i] <- curr
    ni[i] <- curr * roe_path[i]
    div[i] <- ni[i] * payout
    ri[i] <- (roe_path[i] - ke) * curr
    pv[i] <- ri[i] / ((1 + ke)^i)
    end[i] <- curr + ni[i] - div[i]
    curr <- end[i]
  }
  tv <- ri[n] * (1 + g) / (ke - g)
  pv_tv <- tv / ((1 + ke)^n)
  list(
    beg_bv = beg, net_income = ni, dividend = div, end_bv = end,
    ri = ri, pv_ri_year = pv, pv_ri = sum(pv),
    tv = tv, pv_terminal = pv_tv,
    intrinsic = b0 + sum(pv) + pv_tv
  )
}

oracle_roe_linear <- function(roe_start, roe_end, n) {
  n <- as.integer(n)[1]
  if (n == 1L) return(as.numeric(roe_start)[1])
  as.numeric(seq(as.numeric(roe_start)[1], as.numeric(roe_end)[1], length.out = n))
}

# =============================================================================
# MULT / relative valuation oracles
# =============================================================================

oracle_pe_price <- function(eps, pe) {
  eps <- as.numeric(eps)[1]; pe <- as.numeric(pe)[1]
  if (!is.finite(eps) || eps <= 0 || !is.finite(pe) || pe <= 0) return(NA_real_)
  eps * pe
}

oracle_forward_eps <- function(trailing_eps, growth_pct) {
  t <- as.numeric(trailing_eps)[1]; g <- as.numeric(growth_pct)[1]
  if (!is.finite(t) || t <= 0 || !is.finite(g)) return(NA_real_)
  t * (1 + g / 100)
}

oracle_forward_pe_price <- function(trailing_eps, growth_pct, fwd_pe) {
  feps <- oracle_forward_eps(trailing_eps, growth_pct)
  oracle_pe_price(feps, fwd_pe)
}

oracle_peg <- function(pe, growth_pct) {
  pe <- as.numeric(pe)[1]; g <- as.numeric(growth_pct)[1]
  if (!is.finite(pe) || pe <= 0 || !is.finite(g) || g <= 0) return(NA_real_)
  pe / g
}

#' EV multiple → equity → price. P/S must use oracle_ps_price (no cash−debt).
oracle_ev_multiple_price <- function(metric, multiple, cash, debt, shares) {
  metric <- as.numeric(metric)[1]; multiple <- as.numeric(multiple)[1]
  cash <- as.numeric(cash)[1]; debt <- as.numeric(debt)[1]
  shares <- as.numeric(shares)[1]
  if (!is.finite(metric) || metric <= 0 || !is.finite(multiple) || multiple <= 0) {
    return(list(status = "unavailable", implied_ev = NA_real_,
                equity = NA_real_, price = NA_real_))
  }
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  ev <- metric * multiple
  if (!is.finite(shares) || shares <= 0) {
    return(list(status = "unavailable", implied_ev = ev, equity = NA_real_, price = NA_real_))
  }
  eq <- ev + cash - debt
  list(status = "ok", implied_ev = ev, equity = eq, price = eq / shares)
}

oracle_ps_price <- function(revenue, ps_multiple, shares) {
  rev <- as.numeric(revenue)[1]; m <- as.numeric(ps_multiple)[1]
  shares <- as.numeric(shares)[1]
  if (!is.finite(rev) || rev <= 0 || !is.finite(m) || m <= 0) {
    return(list(status = "unavailable", equity = NA_real_, price = NA_real_))
  }
  eq <- rev * m
  if (!is.finite(shares) || shares <= 0) {
    return(list(status = "unavailable", equity = eq, price = NA_real_))
  }
  list(status = "ok", equity = eq, price = eq / shares)
}

oracle_sotp_revenue <- function(revenues, multiples, cash, debt, shares, non_op = 0) {
  revenues <- as.numeric(revenues)
  multiples <- as.numeric(multiples)
  stopifnot(length(revenues) == length(multiples), length(revenues) >= 2L)
  vals <- revenues * multiples
  ev <- sum(vals) + as.numeric(non_op)[1]
  eq <- ev + as.numeric(cash)[1] - as.numeric(debt)[1]
  sh <- as.numeric(shares)[1]
  list(
    segment_values = vals, implied_ev = ev, equity = eq,
    price = if (is.finite(sh) && sh > 0) eq / sh else NA_real_
  )
}

oracle_mos <- function(fv, price) {
  fv <- as.numeric(fv)[1]; price <- as.numeric(price)[1]
  (fv - price) / fv
}

oracle_wacc <- function(re, rd, tax, we, wd) {
  we * re + wd * rd * (1 - tax)
}

oracle_capm_ke <- function(rf, beta, rm) {
  rf + beta * (rm - rf)
}
