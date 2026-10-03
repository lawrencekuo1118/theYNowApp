# Relative multiples: P/E, Forward P/E, PEG, EV/FCF (offline)
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
  rel_ev_fcf_multiple = 15
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

# --- Locale ---
for (k in c(
  "menu_rel_multiples", "rel_multiples_implied_price", "rel_multiples_disclaimer",
  "rel_multiples_growth_sgr", "rel_multiples_vbx_pe"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("rel_multiples_lead_body", "zh-TW")))

# --- UI mounts ---
ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("sidebar mounts rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE))
check("module UI call", grepl('relative_multiples_module_ui("mod_rel")', ui_src, fixed = TRUE))
check("lite hides rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .sidebar-menu li:has(a[data-value=\"rel_multiples_calculator\"])", ui_src, fixed = TRUE))

g_src <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources module", grepl("relative_multiples_module.R", g_src, fixed = TRUE))

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server mounts module", grepl("relative_multiples_module_server", srv, fixed = TRUE))

# --- Defaults in config source ---
cfg <- paste(readLines("default_config.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("default PE multiple in config", grepl("rel_pe_multiple", cfg, fixed = TRUE))
check("default EV/FCF multiple in config", grepl("rel_ev_fcf_multiple", cfg, fixed = TRUE))
check("dcf_ev_to_equity bridge", abs(dcf_ev_to_equity(100, 10, 30) - 80) < 1e-9)

cat("PASS relative_multiples\n")
