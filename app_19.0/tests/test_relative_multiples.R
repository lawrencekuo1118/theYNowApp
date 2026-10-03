# Relative multiples: P/E, Fwd P/E, PEG, EV/* , P/S, EV/ARR, SOTP (offline)
# Run: cd app_19.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_relative_multiples.R

root <- if (file.exists("relative_multiples_module.R")) {
  getwd()
} else if (file.exists("app_19.0/relative_multiples_module.R")) {
  file.path(getwd(), "app_19.0")
} else {
  stop("Run from repo root or app_19.0")
}
setwd(root)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Minimal stubs used by helpers
dcf_ev_to_equity <- function(ev, cash = 0, debt = 0) {
  ev <- as.numeric(ev)[1]; cash <- as.numeric(cash)[1]; debt <- as.numeric(debt)[1]
  if (!is.finite(ev)) return(NA_real_)
  if (!is.finite(cash)) cash <- 0
  if (!is.finite(debt)) debt <- 0
  ev + cash - debt
}

source("ui_locale.R", local = TRUE, encoding = "UTF-8")
APP_DEFAULTS <- list(
  rel_pe_multiple = 18,
  rel_fwd_pe_multiple = 18,
  rel_ev_fcf_multiple = 15,
  rel_ev_ebit_multiple = 12,
  rel_ev_ebitda_multiple = 10,
  rel_ev_sales_multiple = 3,
  rel_ps_multiple = 3,
  rel_ev_arr_multiple = 10
)
source("relative_multiples_module.R", local = TRUE, encoding = "UTF-8")

check <- function(msg, cond) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  cat("OK", msg, "\n")
}

# --- P/E ---
ok <- calc_pe_implied_price(2, 15)
check("PE positive", identical(ok$status, "ok") && abs(ok$implied_price - 30) < 1e-9)
z <- calc_pe_implied_price(0, 15)
check("PE zero EPS N/A", identical(z$status, "unavailable") && !is.finite(z$implied_price))
neg <- calc_pe_implied_price(-1, 15)
check("PE neg EPS N/A", identical(neg$status, "unavailable"))
na_eps <- calc_pe_implied_price(NA_real_, 15)
check("PE missing EPS N/A", identical(na_eps$status, "unavailable"))

# --- Forward EPS invert ---
feps <- calc_forward_eps_from_price_pe(150, 25)
check("forward EPS invert", abs(feps - 6) < 1e-9)
check("forward EPS invert bad", !is.finite(calc_forward_eps_from_price_pe(150, NA_real_)))

# --- PEG ---
peg_ok <- calc_peg(30, 15, growth_definition = "SGR", growth_period = "terminal")
check("PEG ok", identical(peg_ok$status, "ok") && abs(peg_ok$peg - 2) < 1e-9)
check("PEG labels", identical(peg_ok$growth_definition, "SGR"))
peg_zg <- calc_peg(30, 0)
check("PEG zero growth N/A", identical(peg_zg$status, "unavailable"))
peg_ng <- calc_peg(30, -5)
check("PEG neg growth N/A", identical(peg_ng$status, "unavailable"))

# --- EV/FCF ---
evf <- calc_ev_fcf_implied_price(
  fcf = 100, ev_fcf_multiple = 10, cash = 20, debt = 50, shares = 10
)
check("EV/FCF ok", identical(evf$status, "ok"))
check("EV/FCF EV", abs(evf$implied_ev - 1000) < 1e-9)
check("EV/FCF equity", abs(evf$equity_value - 970) < 1e-9)
check("EV/FCF price", abs(evf$implied_price - 97) < 1e-9)
evf0 <- calc_ev_fcf_implied_price(0, 10, 20, 50, 10)
check("EV/FCF zero FCF N/A", identical(evf0$status, "unavailable"))
evf_neg <- calc_ev_fcf_implied_price(-5, 10, 20, 50, 10)
check("EV/FCF neg FCF N/A", identical(evf_neg$status, "unavailable"))
evf_sh <- calc_ev_fcf_implied_price(100, 10, 20, 50, NA_real_)
check("EV/FCF missing shares N/A", identical(evf_sh$status, "unavailable"))

# --- EV/EBIT, EV/EBITDA, EV/Sales ---
eve <- calc_ev_metric_implied_price(80, 12, cash = 20, debt = 50, shares = 10, "EBIT")
check("EV/EBIT ok", identical(eve$status, "ok") && abs(eve$implied_price - 93) < 1e-9)
eveda <- calc_ev_metric_implied_price(120, 10, cash = 20, debt = 50, shares = 10, "EBITDA")
check("EV/EBITDA ok", identical(eveda$status, "ok") && abs(eveda$implied_ev - 1200) < 1e-9)
evs <- calc_ev_metric_implied_price(500, 3, cash = 20, debt = 50, shares = 10, "Revenue")
check("EV/Sales ok", identical(evs$status, "ok") && abs(evs$implied_price - 147) < 1e-9)
check("EV/EBIT zero N/A", identical(
  calc_ev_metric_implied_price(0, 12, 20, 50, 10, "EBIT")$status, "unavailable"
))

# --- P/S (equity-side, no debt bridge) ---
ps <- calc_ps_implied_price(500, 3, shares = 10)
check("P/S ok", identical(ps$status, "ok") && abs(ps$implied_price - 150) < 1e-9)
check("P/S zero rev N/A", identical(calc_ps_implied_price(0, 3, 10)$status, "unavailable"))
check("P/S missing shares N/A", identical(calc_ps_implied_price(500, 3, NA_real_)$status, "unavailable"))

# --- EV/ARR: blank / missing → N/A (Missing ≠ 0) ---
arr_na <- calc_ev_metric_implied_price(NA_real_, 10, 20, 50, 10, "ARR")
check("EV/ARR missing N/A", identical(arr_na$status, "unavailable"))
arr_ok <- calc_ev_metric_implied_price(200, 10, 20, 50, 10, "ARR")
check("EV/ARR manual ok", identical(arr_ok$status, "ok") && abs(arr_ok$implied_price - 197) < 1e-9)

# --- SOTP revenue ---
segs2 <- data.frame(
  name = c("A", "B"), revenue = c(100, 200), stringsAsFactors = FALSE
)
sotp <- calc_sotp_revenue_implied(segs2, 3, cash = 20, debt = 50, shares = 10)
check("SOTP ok", identical(sotp$status, "ok"))
check("SOTP EV", abs(sotp$implied_ev - 900) < 1e-9)
check("SOTP equity", abs(sotp$equity_value - 870) < 1e-9)
check("SOTP price", abs(sotp$implied_price - 87) < 1e-9)
check("SOTP n_segments", identical(as.integer(sotp$n_segments), 2L))
segs1 <- data.frame(name = "Only", revenue = 100, stringsAsFactors = FALSE)
check("SOTP single segment N/A", identical(
  calc_sotp_revenue_implied(segs1, 3, 20, 50, 10)$status, "unavailable"
))
check("SOTP empty N/A", identical(
  calc_sotp_revenue_implied(data.frame(name = character(0), revenue = numeric(0)), 3, 20, 50, 10)$status,
  "unavailable"
))
sotp_nonop <- calc_sotp_revenue_implied(segs2, 3, cash = 20, debt = 50, shares = 10, non_operating = 100)
check("SOTP non-op", abs(sotp_nonop$implied_ev - 1000) < 1e-9)

# --- Locale ---
for (k in c(
  "menu_rel_multiples", "rel_multiples_implied_price", "rel_multiples_disclaimer",
  "rel_multiples_growth_sgr", "rel_multiples_vbx_pe", "rel_multiples_vbx_evebit",
  "rel_multiples_vbx_sotp", "rel_multiples_status_arr", "rel_multiples_status_sotp",
  "rel_multiples_tab_sotp", "rel_multiples_ev_heading", "rel_multiples_sotp_need_segments"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("rel_multiples_lead_body", "zh-TW")))
check("menu Multiples SOTP en", grepl("SOTP", ui_str("menu_rel_multiples", "en"), fixed = TRUE))
check("menu Multiples SOTP zh", grepl("SOTP", ui_str("menu_rel_multiples", "zh-TW"), fixed = TRUE))

# --- UI mounts ---
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("sidebar mounts rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE))
check("module UI call", grepl('relative_multiples_module_ui("mod_rel")', ui_src, fixed = TRUE))
check("lite hides rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .sidebar-menu li:has(a[data-value=\"rel_multiples_calculator\"])", ui_src, fixed = TRUE))
check("applyUiLocale SOTP tab", grepl("ynow_rel_multiples_tab_sotp", ui_src, fixed = TRUE))
check("applyUiLocale EV heading", grepl("ynow_rel_multiples_ev_heading", ui_src, fixed = TRUE))

g_src <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources module", grepl("relative_multiples_module.R", g_src, fixed = TRUE))

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server mounts module", grepl("relative_multiples_module_server", srv, fixed = TRUE))

# --- Defaults in config source ---
cfg <- paste(readLines("default_config.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("default PE multiple in config", grepl("rel_pe_multiple", cfg, fixed = TRUE))
check("default EV/FCF multiple in config", grepl("rel_ev_fcf_multiple", cfg, fixed = TRUE))
check("default EV/EBIT multiple in config", grepl("rel_ev_ebit_multiple", cfg, fixed = TRUE))
check("default EV/Sales multiple in config", grepl("rel_ev_sales_multiple", cfg, fixed = TRUE))
check("default P/S multiple in config", grepl("rel_ps_multiple", cfg, fixed = TRUE))
check("default EV/ARR multiple in config", grepl("rel_ev_arr_multiple", cfg, fixed = TRUE))
check("dcf_ev_to_equity bridge", abs(dcf_ev_to_equity(100, 10, 30) - 80) < 1e-9)

cat("PASS relative_multiples\n")
