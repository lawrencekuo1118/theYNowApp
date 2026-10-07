# Relative multiples: P/E, Fwd P/E, PEG, EV/* , P/S, EV/ARR, SOTP (offline)
# Run: cd app_21.0 && YNOW_DEBUG_SKIP_PY=1 Rscript tests/test_relative_multiples.R

root <- if (file.exists("relative_multiples_module.R")) {
  getwd()
} else if (file.exists("app_21.0/relative_multiples_module.R")) {
  file.path(getwd(), "app_21.0")
} else {
  stop("Run from repo root or app_21.0")
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
  rel_mode = "earnings",
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

# --- Forward EPS invert (deprecated pure math; sync must NOT call this) ---
feps <- calc_forward_eps_from_price_pe(150, 25)
check("forward EPS invert math", abs(feps - 6) < 1e-9)
check("forward EPS invert bad", !is.finite(calc_forward_eps_from_price_pe(150, NA_real_)))

# --- Sync policy: statements + App defaults; no market PE / price back-solve ---
mults <- .rel_app_default_multiples(APP_DEFAULTS)
check("default PE from APP_DEFAULTS", abs(mults$pe_multiple - 18) < 1e-9)
check("default Fwd PE from APP_DEFAULTS", abs(mults$fwd_pe_multiple - 18) < 1e-9)
check(
  "forward EPS from trailing×g not price",
  abs(.rel_forward_eps_from_trailing(2, 10) - 2.2) < 1e-9
)
check(
  "forward EPS missing growth stays NA (no 回推)",
  !is.finite(.rel_forward_eps_from_trailing(2, NA_real_))
)
# Chart method label (family-aware pick)
fake_res <- list(
  pe = list(status = "ok", implied_price = 100),
  forward_pe = list(status = "ok", implied_price = 110),
  ev_fcf = list(status = "ok", implied_price = 90),
  ev_ebit = list(status = "unavailable", implied_price = NA_real_),
  ev_ebitda = list(status = "ok", implied_price = 95),
  ev_sales = list(status = "ok", implied_price = 80),
  ps = list(status = "ok", implied_price = 70),
  ev_arr = list(status = "unavailable", implied_price = NA_real_)
)
pick_e <- .rel_pick_active_implied(fake_res, "earnings")
check("earnings family picks Trailing P/E first", identical(pick_e$key, "pe") && abs(pick_e$implied_price - 100) < 1e-9)
pick_ent <- .rel_pick_active_implied(fake_res, "enterprise")
check("enterprise family picks EV/FCF first", identical(pick_ent$key, "ev_fcf"))
pick_ps <- .rel_pick_active_implied(fake_res, "ps")
check("ps family picks P/S", identical(pick_ps$key, "ps"))
check("chart label Multiples · P/E", identical(.rel_chart_label("pe"), "Multiples · P/E"))
check("chart label Multiples · EV/EBITDA", identical(.rel_chart_label("ev_ebitda"), "Multiples · EV/EBITDA"))
check("chart label bare Multiples when unknown", identical(.rel_chart_label(NA_character_), "Multiples"))
check(
  "forward EPS nonpositive trailing NA",
  !is.finite(.rel_forward_eps_from_trailing(0, 10))
)
# Statement-first EPS: NI / shares when IS+BS present
d_is_eps <- data.frame(
  Breakdown = c("Net Income Common Stockholders", "Total Revenue"),
  `12/31/2024` = c("200", "1000"),
  check.names = FALSE, stringsAsFactors = FALSE
)
d_bs_eps <- data.frame(
  Breakdown = c("Ordinary Shares Number", "Cash And Cash Equivalents"),
  `12/31/2024` = c("100", "50"),
  check.names = FALSE, stringsAsFactors = FALSE
)
# Stubs if setup helpers absent in offline test
if (!exists("select_current_metric", mode = "function")) {
  select_current_metric <<- function(df, pattern, kind = NULL) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NA_real_)
    hit <- grepl(pattern, df[[1]], ignore.case = TRUE, perl = TRUE)
    if (!any(hit)) return(NA_real_)
    suppressWarnings(as.numeric(df[which(hit)[1], 2]))
  }
}
teps_stmt <- .rel_trailing_eps_from_statements(
  sum_df = data.frame(Item = "Trailing P/E", Value = "99", stringsAsFactors = FALSE),
  d_is = d_is_eps, d_bs = d_bs_eps
)
check("trailing EPS prefers NI/shares over summary", abs(teps_stmt - 2) < 1e-9)

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
segs_pm <- data.frame(
  name = c("A", "B"), revenue = c(100, 200), multiple = c(2, 4), stringsAsFactors = FALSE
)
sotp_pm <- calc_sotp_revenue_implied(segs_pm, 3, cash = 20, debt = 50, shares = 10)
check("SOTP per-segment multiples", identical(sotp_pm$status, "ok"))
check("SOTP per-segment EV", abs(sotp_pm$implied_ev - 1000) < 1e-9) # 100*2 + 200*4

# --- Locale ---
for (k in c(
  "menu_rel_multiples", "menu_sotp", "rel_multiples_implied_price", "rel_multiples_disclaimer",
  "rel_multiples_growth_sgr", "rel_multiples_vbx_pe", "rel_multiples_vbx_evebit",
  "rel_multiples_status_arr", "rel_mode_label", "rel_mode_earnings", "rel_mode_enterprise",
  "rel_mode_ps", "rel_multiples_ev_heading", "rel_multiples_ps_heading",
  "rel_multiples_tab_earnings", "rel_multiples_tab_enterprise", "rel_multiples_tab_ps",
  "rel_multiples_tab_bridge", "rel_formula_earnings", "rel_formula_enterprise", "rel_formula_ps",
  "sotp_lead_title", "sotp_need_segments", "sotp_vbx_price", "sotp_col_multiple",
  "sotp_tab_bridge", "sotp_formula_banner", "sotp_settings_seg_note",
  "sotp_status_shares_missing", "sotp_status_multiple_invalid",
  "sotp_status_equity_invalid", "sotp_status_unavailable"
)) {
  check(paste("en", k), nzchar(ui_str(k, "en")))
  check(paste("zh", k), nzchar(ui_str(k, "zh-TW")))
}
check("shares missing not bare N/A", {
  !identical(ui_str("sotp_status_shares_missing", "en"), "N/A") &&
    grepl("shares", ui_str("sotp_status_shares_missing", "en"), ignore.case = TRUE)
})
check("zh no simplified", !grepl("默认|参数|数据|用户", ui_str("rel_multiples_lead_body", "zh-TW")))
check("menu Multiples en", identical(ui_str("menu_rel_multiples", "en"), "Multiples"))
check("menu SOTP en", identical(ui_str("menu_sotp", "en"), "SOTP"))
check("no sotp in multiples radio choices en", !grepl("SOTP", ui_str("rel_mode_earnings", "en"), fixed = TRUE))

# --- UI mounts / mode radio ---
mod_src <- paste(readLines("relative_multiples_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("sync omits Yahoo market P/E seed", !grepl("mkt_pe", mod_src, fixed = TRUE))
check("sync omits Forward P/E market seed", !grepl("fpe_mkt", mod_src, fixed = TRUE))
check(
  "sync never calls price÷P/E back-solve",
  !grepl("calc_forward_eps_from_price_pe\\(", mod_src) ||
    grepl("@deprecated[\\s\\S]*?calc_forward_eps_from_price_pe", mod_src)
)
# Ensure the only calc_forward_eps_from_price_pe occurrence is the function definition, not a call site in sync
check(
  "no back-solve call in sync_from_statements",
  {
    # Strip the function definition body roughly; fail if .quote_px() feeds forward EPS
    !grepl("feps <- calc_forward_eps_from_price_pe", mod_src, fixed = TRUE) &&
      !grepl("calc_forward_eps_from_price_pe\\(\\.quote_px", mod_src) &&
      grepl(".rel_forward_eps_from_trailing", mod_src, fixed = TRUE) &&
      grepl(".rel_app_default_multiples", mod_src, fixed = TRUE) &&
      grepl(".rel_trailing_eps_from_statements", mod_src, fixed = TRUE)
  }
)
check(
  "locale pe_help rejects back-solve",
  {
    en <- ui_str("rel_multiples_pe_help", "en")
    zh <- ui_str("rel_multiples_pe_help", "zh-TW")
    grepl("back-solve|industry defaults", en, ignore.case = TRUE) &&
      grepl("回推|產業預設", zh) &&
      !grepl("price ÷ Forward P/E|股價 ÷ Forward P/E", en) &&
      !grepl("股價 ÷ Forward P/E 反推", zh)
  }
)
check("mode radio in module", grepl('ns("rel_mode")', mod_src, fixed = TRUE))
check("mode earnings family", grepl("earnings", mod_src, fixed = TRUE) && grepl("enterprise", mod_src, fixed = TRUE))
check("mode conditional panels", grepl("mod_rel-rel_mode", mod_src, fixed = TRUE))
check("multiples radio omits sotp choice", !grepl('= "sotp"', mod_src, fixed = TRUE))
check("multiples earnings settings tab", grepl("ynow_rel_multiples_tab_earnings", mod_src, fixed = TRUE))
check("multiples enterprise settings tab", grepl("ynow_rel_multiples_tab_enterprise", mod_src, fixed = TRUE))
check("multiples ps settings tab", grepl("ynow_rel_multiples_tab_ps", mod_src, fixed = TRUE))
check("multiples bridge settings tab", grepl("ynow_rel_multiples_tab_bridge", mod_src, fixed = TRUE))
check("multiples formula banners", grepl("ynow_rel_formula_earnings", mod_src, fixed = TRUE) &&
        grepl("ynow_rel_formula_enterprise", mod_src, fixed = TRUE) &&
        grepl("ynow_rel_formula_ps", mod_src, fixed = TRUE))
check("multiples earnings params", grepl('ns("trailing_eps")', mod_src, fixed = TRUE) &&
        grepl('ns("pe_multiple")', mod_src, fixed = TRUE) &&
        grepl('ns("peg_growth_pct")', mod_src, fixed = TRUE))
check("multiples enterprise params", grepl('ns("fcff")', mod_src, fixed = TRUE) &&
        grepl('ns("ev_fcf_multiple")', mod_src, fixed = TRUE) &&
        grepl('ns("ev_arr_multiple")', mod_src, fixed = TRUE))
check("multiples bridge params", grepl('ns("cash")', mod_src, fixed = TRUE) &&
        grepl('ns("debt")', mod_src, fixed = TRUE) &&
        grepl('ns("shares")', mod_src, fixed = TRUE))

sotp_src <- paste(readLines("sotp_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("sotp module UI", grepl("sotp_module_ui", sotp_src, fixed = TRUE))
check("sotp per-segment inputs", grepl("seg_mult_", sotp_src, fixed = TRUE))
check("sotp bridge tab", grepl("ynow_sotp_tab_bridge", sotp_src, fixed = TRUE))
check("sotp formula banner", grepl("ynow_sotp_formula_banner", sotp_src, fixed = TRUE))
check("sotp segment params", grepl('ns("default_ev_sales")', sotp_src, fixed = TRUE) &&
        grepl('ns("nonop")', sotp_src, fixed = TRUE))
check("sotp bridge params", grepl('ns("cash")', sotp_src, fixed = TRUE) &&
        grepl('ns("debt")', sotp_src, fixed = TRUE) &&
        grepl('ns("shares")', sotp_src, fixed = TRUE))
check("sotp sync uses segment notes", {
  grepl(".rel_sotp_fetch_segment_notes", sotp_src, fixed = TRUE) &&
    grepl("notes = notes", sotp_src, fixed = TRUE) &&
    grepl(".sotp_unavailable_msg", sotp_src, fixed = TRUE)
})
check("sotp helper wires notes into BB Lab", {
  grepl("notes = notes", mod_src, fixed = TRUE) &&
    grepl("segment_tables = segment_tables", mod_src, fixed = TRUE) &&
    grepl(".rel_sotp_fetch_segment_notes", mod_src, fixed = TRUE)
})

# Offline: segment_tables supply ≥2 revenues when IS has only consolidated rows
source("business_breakdown_schema.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_structure.R", local = TRUE, encoding = "UTF-8")
source("business_breakdown_engine.R", local = TRUE, encoding = "UTF-8")
note_tbl <- list(list(
  short_name = "Segment Information",
  kind = "operating_segment",
  headers = c("Segment", "Revenue", "Cost of Revenue"),
  rows = list(
    c("Business A", "120", "40"),
    c("Business B", "80", "40"),
    c("Total", "200", "80")
  )
))
is_consol <- data.frame(
  Breakdown = c("Total Revenue", "Cost Of Revenue", "Gross Profit"),
  `12/31/2024` = c("200", "80", "120"),
  check.names = FALSE, stringsAsFactors = FALSE
)
segs_notes <- .rel_sotp_segments_from_is(
  is_consol, ticker = "FIXT", statement_currency = "USD",
  segment_tables = note_tbl
)
check(
  "SOTP segments from BB Lab segment notes",
  is.data.frame(segs_notes) && nrow(segs_notes) >= 2L &&
    all(segs_notes$revenue > 0)
)
segs_no_notes <- .rel_sotp_segments_from_is(
  is_consol, ticker = "FIXT", statement_currency = "USD"
)
check(
  "SOTP without notes stays empty on consolidated-only IS",
  is.data.frame(segs_no_notes) && nrow(segs_no_notes) == 0L
)

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("sidebar mounts rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE))
check("sidebar mounts sotp", grepl("sotp_calculator", ui_src, fixed = TRUE))
check("module UI call", grepl('relative_multiples_module_ui("mod_rel")', ui_src, fixed = TRUE))
check("sotp UI call", grepl('sotp_module_ui("mod_sotp")', ui_src, fixed = TRUE))
check("lite hides rel multiples", grepl("rel_multiples_calculator", ui_src, fixed = TRUE) &&
        grepl("body.ynow-lite .sidebar-menu li:has(a[data-value=\"rel_multiples_calculator\"])", ui_src, fixed = TRUE))
check("lite hides sotp", grepl('body.ynow-lite .sidebar-menu li:has(a[data-value="sotp_calculator"])', ui_src, fixed = TRUE))
check("applyUiLocale mode help", grepl("ynow_rel_mode_help", ui_src, fixed = TRUE))
check("applyUiLocale sotp lead", grepl("ynow_sotp_lead_title", ui_src, fixed = TRUE))

g_src <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("global sources module", grepl("relative_multiples_module.R", g_src, fixed = TRUE))
check("global sources sotp", grepl("sotp_module.R", g_src, fixed = TRUE))

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server mounts module", grepl("relative_multiples_module_server", srv, fixed = TRUE))
check("server mounts sotp", grepl("sotp_module_server", srv, fixed = TRUE))

# --- Defaults in config source ---
cfg <- paste(readLines("default_config.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("default rel_mode in config", grepl("rel_mode", cfg, fixed = TRUE))
check("default PE multiple in config", grepl("rel_pe_multiple", cfg, fixed = TRUE))
check("default EV/FCF multiple in config", grepl("rel_ev_fcf_multiple", cfg, fixed = TRUE))
check("default EV/EBIT multiple in config", grepl("rel_ev_ebit_multiple", cfg, fixed = TRUE))
check("default EV/Sales multiple in config", grepl("rel_ev_sales_multiple", cfg, fixed = TRUE))
check("default P/S multiple in config", grepl("rel_ps_multiple", cfg, fixed = TRUE))
check("default EV/ARR multiple in config", grepl("rel_ev_arr_multiple", cfg, fixed = TRUE))
check("dcf_ev_to_equity bridge", abs(dcf_ev_to_equity(100, 10, 30) - 80) < 1e-9)

srv <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("server locale-pushes rel_mode", grepl("mod_rel-rel_mode", srv, fixed = TRUE))
check("server wires Multiples chart_label to composite", grepl("chart_label", srv, fixed = TRUE))
check("server smart chart uses Multiples method suffix", grepl("smart_multiples_bar_suffix", srv, fixed = TRUE))
check(
  "decision module accepts model_point_labels",
  grepl("model_point_labels", paste(readLines("investment_decision_module.R", warn = FALSE), collapse = "\n"), fixed = TRUE)
)

cat("PASS relative_multiples\n")
