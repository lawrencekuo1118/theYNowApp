# Asset transmission map — graph, shocks, locale (no network).
# Run: cd app_20.0 && Rscript tests/test_asset_transmission.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
root <- if (file.exists("asset_transmission_module.R")) {
  normalizePath(".")
} else if (file.exists("../asset_transmission_module.R")) {
  normalizePath("..")
} else if (dir.exists("app_20.0") && file.exists("app_20.0/asset_transmission_module.R")) {
  normalizePath("app_20.0")
} else {
  stop("Cannot locate asset_transmission_module.R")
}
setwd(root)

source("ui_locale.R", local = TRUE, encoding = "UTF-8")
source("asset_transmission_module.R", local = TRUE, encoding = "UTF-8")

check <- function(label, ok) {
  if (!isTRUE(ok)) stop("FAIL: ", label)
  cat("OK ", label, "\n", sep = "")
}

catg <- asset_tx_catalog()
ids <- catg$nodes$id
n_nodes <- length(ids)
check(
  "node fields aligned",
  n_nodes == length(catg$nodes$yahoo) &&
    n_nodes == length(catg$nodes$x) &&
    n_nodes == length(catg$nodes$y) &&
    n_nodes == length(catg$nodes$kind) &&
    n_nodes == length(catg$nodes$band) &&
    n_nodes == length(catg$nodes$textposition)
)
check("35 public nodes", n_nodes == 35L && !anyDuplicated(ids))
check("yahoo symbols unique", !anyDuplicated(catg$nodes$yahoo))
check(
  "§1 driver band is inflation only",
  identical(as.character(catg$nodes$id[catg$nodes$band == "driver"]), "infl")
)
check(
  "§2 hubs are rate / fx / liq",
  identical(sort(unique(as.character(catg$nodes$band[catg$nodes$band %in% c("rate", "fx", "liq")]))), c("fx", "liq", "rate")) &&
    identical(sort(as.character(catg$nodes$id[catg$nodes$band == "liq"])), c("move", "vix"))
)
check(
  "§4 commodities sit in cmdty (not driver)",
  all(c("oil", "brent", "ng", "copper", "wheat") %in% catg$nodes$id[catg$nodes$band == "cmdty"]) &&
    !any(c("oil", "brent", "ng", "copper", "wheat") %in% catg$nodes$id[catg$nodes$band == "driver"])
)
check(
  "§4 bonds include HYG (not liquidity)",
  identical(as.character(catg$nodes$band[catg$nodes$id == "hyg"]), "bond") &&
    all(c("tip", "ief", "tlt", "hyg") %in% catg$nodes$id[catg$nodes$band == "bond"])
)
check(
  "§4 asset-class bands cover equities / metals / crypto / other",
  all(c("spx", "taiex") %in% catg$nodes$id[catg$nodes$band == "equity"]) &&
    all(c("gold", "silver") %in% catg$nodes$id[catg$nodes$band == "metal"]) &&
    identical(as.character(catg$nodes$band[catg$nodes$id == "btc"]), "crypto") &&
    identical(as.character(catg$nodes$band[catg$nodes$id == "vnq"]), "other")
)
check(
  "§1/2/4 layer headers present",
  all(c(
    "atx_head_driver", "atx_head_rate", "atx_head_fxhub", "atx_head_liq",
    "atx_head_bond", "atx_head_equity", "atx_head_metal",
    "atx_head_cmdty", "atx_head_crypto", "atx_head_other"
  ) %in% names(.UI_STRINGS$en)) &&
    grepl("§1", ui_str("atx_head_driver", "en"), fixed = TRUE) &&
    grepl("§2", ui_str("atx_head_rate", "zh-TW"), fixed = TRUE) &&
    grepl("債券", ui_str("atx_head_bond", "zh-TW"), fixed = TRUE)
)
check(
  "47 channels",
  nrow(catg$edges) == 47L &&
    length(catg$edges$from) == length(catg$edges$to) &&
    length(catg$edges$from) == length(catg$edges$prior)
)
check(
  "edge endpoints exist",
  all(catg$edges$from %in% ids) && all(catg$edges$to %in% ids)
)
check("priors are -1, 0, or 1", all(catg$edges$prior %in% c(-1L, 0L, 1L)))
prior_of <- function(from, to) {
  hit <- catg$edges$from == from & catg$edges$to == to
  catg$edges$prior[hit][1]
}
check("bill feeds 10Y same direction", identical(prior_of("us_bill", "us10y"), 1L))
check("10Y pressures Nasdaq", identical(prior_of("us10y", "nasdaq"), -1L))
check("10Y and dollar are regime-dependent", identical(prior_of("us10y", "dxy"), 0L))
check("10Y and gold are regime-dependent", identical(prior_of("us10y", "gold"), 0L))
check("10Y opposite long-bond price", identical(prior_of("us10y", "tlt"), -1L))
check("DXY opposite gold", identical(prior_of("dxy", "gold"), -1L))
check("DXY and USD/TWD same way", identical(prior_of("dxy", "usdtwd"), 1L))
check("SOX feeds TAIEX", identical(prior_of("sox", "taiex"), 1L))
check("2Y futures yield feeds the 10Y", identical(prior_of("us2y", "us10y"), 1L))
check("Nasdaq still feeds Bitcoin", identical(prior_of("nasdaq", "btc"), 1L))
check("Nasdaq-100 feeds SOX", identical(prior_of("ndx", "sox"), 1L))
check("dollar vs oil is regime-dependent", identical(prior_of("dxy", "oil"), 0L))
check("TIPS vs gold is regime-dependent", identical(prior_of("tip", "gold"), 0L))
check("Henry Hub vs WTI is regime-dependent", identical(prior_of("ng", "oil"), 0L))
check(
  "curve is derived, not a Yahoo symbol",
  identical(catg$nodes$yahoo[catg$nodes$id == "curve"], "") &&
    identical(catg$nodes$kind[catg$nodes$id == "curve"], "spread") &&
    identical(catg$nodes$transform[catg$nodes$id == "curve"], "diff")
)
check("US 2Y is the yield future", identical(catg$nodes$yahoo[catg$nodes$id == "us2y"], "2YY=F"))
check("Henry Hub symbol", identical(catg$nodes$yahoo[catg$nodes$id == "ng"], "NG=F"))
check("Brent symbol", identical(catg$nodes$yahoo[catg$nodes$id == "brent"], "BZ=F"))
check("Nasdaq-100 symbol", identical(catg$nodes$yahoo[catg$nodes$id == "ndx"], "^NDX"))
check(
  "TIPS is an ETF price",
  identical(catg$nodes$yahoo[catg$nodes$id == "tip"], "TIP") &&
    identical(catg$nodes$kind[catg$nodes$id == "tip"], "etf")
)
check("TIPS gloss is not a real-yield percent", grepl("not a 10-year real-yield", ui_str("atx_gloss_tip", "en"), fixed = TRUE))
check("gas gloss names Henry Hub and TTF", grepl("Henry Hub", ui_str("atx_gloss_ng", "zh-TW"), fixed = TRUE) && grepl("TTF", ui_str("atx_gloss_ng", "zh-TW"), fixed = TRUE))
catalog_keys <- unique(c(catg$nodes$label_key, catg$nodes$gloss_key, catg$edges$channel_key))
check(
  "catalog keys in both locales",
  all(catalog_keys %in% names(.UI_STRINGS$en)) &&
    all(catalog_keys %in% names(.UI_STRINGS[["zh-TW"]]))
)

atx_keys <- grep("^atx_", names(.UI_STRINGS$en), value = TRUE)
check("atx keys exist", length(atx_keys) >= 70L)
for (k in atx_keys) {
  check(paste("zh key", k), nzchar(ui_str(k, "zh-TW")) && ui_str(k, "zh-TW") != k)
  check(paste("en key", k), nzchar(ui_str(k, "en")))
}
zh_blob <- paste(vapply(atx_keys, function(k) ui_str(k, "zh-TW"), character(1)), collapse = "\n")
for (bad in c("默认", "参数", "数据", "用户", "信息", "软件", "网络", "质量", "窗口", "视频")) {
  check(paste("no simplified", bad), !grepl(bad, zh_blob, fixed = TRUE))
}
check("zh node TAIEX", identical(ui_str("atx_node_taiex", "zh-TW"), "台灣加權指數"))
check("en node T-bill", identical(ui_str("atx_node_us_bill", "en"), "US T-bill"))
check("testing title zh", identical(ui_str("testing_box_title", "zh-TW"), "資產傳導"))
check("testing title en", identical(ui_str("testing_box_title", "en"), "Asset transmission"))
check(
  "zh curve up is bear steepening",
  identical(ui_str("atx_lean_curve_up", "zh-TW"), "2s10s：長端升得比較快就是熊市陡峭化（bear steepener）")
)
check(
  "zh curve down is bull steepening",
  identical(ui_str("atx_lean_curve_down", "zh-TW"), "2s10s：前端掉得比較快就是牛市陡峭化（bull steepener）")
)
check(
  "en curve leans stay American English",
  identical(ui_str("atx_lean_curve_up", "en"), "2s10s: bear steepener if the long end leads") &&
    identical(ui_str("atx_lean_curve_down", "en"), "2s10s: bull steepener if the front end falls faster")
)

check("yield level", identical(asset_tx_fmt_level(4.2, "yield"), "4.20%"))
check("yield bp", identical(asset_tx_fmt_shock(0.062, "yield"), "+6.2 bp"))
check("yield bp down", identical(asset_tx_fmt_shock(-0.01, "yield"), "-1.0 bp"))
check("ret pct", identical(asset_tx_fmt_shock(-0.01234, "index"), "-1.23%"))
check("spread level", identical(asset_tx_fmt_level(0.50, "spread"), "+50.0 bp"))
check("spread level inverted", identical(asset_tx_fmt_level(-0.25, "spread"), "-25.0 bp"))
check("spread shock", identical(asset_tx_fmt_shock(0.02, "spread"), "+2.0 bp"))
check("fx level", identical(asset_tx_fmt_level(32.1254, "fx"), "32.125"))
check("missing level", identical(asset_tx_fmt_level(NA_real_, "index"), "—"))
check(
  "missing print stays in place",
  identical(.asset_tx_num_vec(list(1, NULL, 3.5), 3), c(1, NA_real_, 3.5))
)
check("window choice null", identical(.asset_tx_window_choice(NULL), "60"))
check("window choice empty", identical(.asset_tx_window_choice(character(0)), "60"))
check("window choice 20", identical(.asset_tx_window_choice("20"), "20"))
check("yahoo range defaults to 6mo", identical(.asset_tx_yahoo_range(NULL), "6mo"))
check("yahoo range keeps 1y", identical(.asset_tx_yahoo_range("1y"), "1y"))
cache_key <- .asset_tx_panel_cache_key(c("B", "A"), "6mo")
check("panel cache key sorts symbols", identical(cache_key, "6mo::A|B"))
.asset_tx_panel_cache_put(cache_key, list(fetched_at = Sys.time(), n_ok = 1L, marker = "hit"))
cache_hit <- .asset_tx_panel_cache_get(cache_key)
check("panel cache hit within TTL", is.list(cache_hit) && identical(cache_hit$marker, "hit"))
.asset_tx_panel_cache_put(cache_key, list(fetched_at = Sys.time() - 120, n_ok = 1L, marker = "stale"))
check("panel cache misses after TTL", is.null(.asset_tx_panel_cache_get(cache_key)))
rm(list = cache_key, envir = .asset_tx_panel_cache)
chart_fix <- .asset_tx_parse_chart(list(
  chart = list(result = list(list(
    timestamp = list(1725148800, 1725235200),
    meta = list(exchangeTimezoneName = "America/New_York"),
    indicators = list(quote = list(list(close = list(4.2, NULL))))
  )))
))
check(
  "chart parser keeps a finite close",
  length(chart_fix) == 1L && is.finite(unname(chart_fix)[[1]])
)
# Regression: simplifyVector=TRUE turns result into a data.frame and used to
# yield empty series → "empty Yahoo history" → blank Testing map.
yahoo_txt <- paste0(
  '{"chart":{"result":[{"meta":{"exchangeTimezoneName":"UTC"},',
  '"timestamp":[1704067200,1704153600],',
  '"indicators":{"quote":[{"close":[100.5,101.25]}]}}]}}'
)
parsed_txt <- .asset_tx_parse_chart_text(yahoo_txt)
check(
  "parse_chart_text survives Yahoo JSON (not empty)",
  length(parsed_txt) == 2L &&
    isTRUE(all(is.finite(unname(parsed_txt)))) &&
    grepl("simplifyVector = FALSE", paste(readLines("asset_transmission_module.R", warn = FALSE), collapse = "\n"), fixed = TRUE)
)

us_col <- asset_tx_move_colors("US")
tw_col <- asset_tx_move_colors("TW")
check("US up green", identical(us_col$up, "#1e7a46"))
check("TW up red", identical(tw_col$up, "#c0392b"))
check(
  "z color follows market",
  asset_tx_z_color(2.5, us_col) != asset_tx_z_color(2.5, tw_col)
)
check("aligned edge teal", identical(asset_tx_edge_color("aligned"), "#2ec4c6"))
check("diverged edge amber", identical(asset_tx_edge_color("diverged"), "#c47b14"))
rr <- .asset_tx_round_rect(1, 2, 1.40, 0.64, 0.12)
check("round rect is a closed SVG path", grepl("^M ", rr) && grepl("Z$", rr) && grepl("Q ", rr))
seg_h <- .asset_tx_segment(0, 0, 2, 0)
check("horizontal arrow clears the card", seg_h$x1 >= 0.62 && seg_h$x2 <= 2 - 0.62)
seg_v <- .asset_tx_segment(0, 0, 0, 2)
check("vertical arrow clears the card", seg_v$y1 >= 0.30 && seg_v$y2 <= 2 - 0.30)

# Planted daily changes: bill → 10Y positive, 10Y → S&P negative, 10Y → gold positive
# (gold prior is opposite, so that channel diverges), SOX → TAIEX positive.
set.seed(7)
n <- 120L
bill_d <- rnorm(n, sd = 0.03)
us10_d <- 0.95 * bill_d
spx_r <- rep(0, n)
gold_r <- rep(0, n)
sox_r <- rnorm(n, sd = 0.01)
taiex_r <- rep(0, n)
spx_r[-1] <- -0.02 * as.numeric(scale(us10_d[-1]))
gold_r[-1] <- 0.02 * as.numeric(scale(us10_d[-1]))
taiex_r[-1] <- 0.95 * sox_r[-1]
to_diff <- function(d, start) {
  lvl <- numeric(length(d))
  lvl[1] <- start
  if (length(d) >= 2L) lvl[-1] <- start + cumsum(d[-1])
  lvl
}
to_ret <- function(r, start) start * cumprod(1 + r)
noise <- function(sd) {
  r <- rnorm(n, sd = sd)
  r[1] <- 0
  to_ret(r, 100)
}
panel <- data.frame(
  Date = as.Date("2025-01-01") + seq_len(n) - 1L,
  oil = noise(0.01),
  us_bill = to_diff(bill_d, 4),
  us10y = to_diff(us10_d, 4.2),
  dxy = noise(0.004),
  vix = noise(0.02),
  gold = to_ret(gold_r, 2300),
  spx = to_ret(spx_r, 5000),
  nasdaq = noise(0.01),
  usdtwd = noise(0.002) / 100 * 32,
  sox = to_ret(sox_r, 4000),
  taiex = to_ret(taiex_r, 18000),
  stringsAsFactors = FALSE
)
# usdtwd noise/100*32 can be tiny; force a positive FX level path.
panel$usdtwd <- 32 + cumsum(c(0, rnorm(n - 1, sd = 0.02)))

snap <- asset_tx_snapshot(panel, window = 60L)
check("snapshot dated", identical(snap$as_of, max(panel$Date)))
edge_state <- function(from, to) {
  hit <- snap$edges$from == from & snap$edges$to == to
  snap$edges[hit, , drop = FALSE]
}
bill <- edge_state("us_bill", "us10y")
check("bill-10Y corr high", is.finite(bill$corr) && bill$corr > 0.85)
check("bill-10Y aligned", identical(bill$state, "aligned"))
eq <- edge_state("us10y", "spx")
check("10Y-SPX corr negative", is.finite(eq$corr) && eq$corr < -0.85)
check("10Y-SPX aligned", identical(eq$state, "aligned"))
gld <- edge_state("us10y", "gold")
check("10Y-gold corr positive", is.finite(gld$corr) && gld$corr > 0.85)
check("10Y-gold mixed", identical(gld$state, "mixed"))
semi <- edge_state("sox", "taiex")
check("SOX-TAIEX aligned", is.finite(semi$corr) && semi$corr > 0.85 && identical(semi$state, "aligned"))

# Known 1D / 5D yield change. Other nodes left missing on purpose.
tiny <- data.frame(
  Date = as.Date("2025-06-01") + 0:5,
  us10y = c(4, 4.01, 4.02, 4.03, 4.04, 4.10),
  stringsAsFactors = FALSE
)
tiny_snap <- asset_tx_snapshot(tiny, window = 20L)
us10 <- tiny_snap$nodes[tiny_snap$nodes$id == "us10y", , drop = FALSE]
check("1D is 6.0 bp", identical(asset_tx_fmt_shock(us10$shock_1d, "yield"), "+6.0 bp"))
check("5D is 10.0 bp", identical(asset_tx_fmt_shock(us10$shock_5d, "yield"), "+10.0 bp"))
path_up <- asset_tx_yield_path(tiny_snap, "en")
check("yield path bias up", identical(path_up$bias, "up") && grepl("+6.0 bp", path_up$title, fixed = TRUE))
check(
  "yield path has curve and TIPS leans",
  nrow(path_up$rows) == 11L &&
    any(path_up$rows$asset == "Gold") &&
    any(path_up$rows$asset == "2s10s") &&
    any(path_up$rows$asset == "TIPS")
)
quiet_panel <- tiny
quiet_panel$us10y <- c(4, 4, 4, 4, 4, 4.002)
quiet_snap <- asset_tx_snapshot(quiet_panel, window = 20L)
path_quiet <- asset_tx_yield_path(quiet_snap, "en")
check("quiet yield path stays unsigned", identical(path_quiet$bias, "quiet") && !nrow(path_quiet$rows))
down_panel <- tiny
down_panel$us10y <- c(4.10, 4.08, 4.06, 4.04, 4.02, 4.00)
down_snap <- asset_tx_snapshot(down_panel, window = 20L)
check("yield path bias down", identical(asset_tx_yield_path(down_snap, "zh-TW")$bias, "down"))
check("missing oil stays NA", !is.finite(tiny_snap$nodes$level[tiny_snap$nodes$id == "oil"]))
check("quiet tape is an unsigned regime", identical(asset_tx_regime(quiet_snap, "en")$id, "quiet"))
check("zh quiet regime", identical(asset_tx_regime(quiet_snap, "zh-TW")$title, "平靜"))

curve_panel <- data.frame(
  Date = as.Date("2025-06-01") + 0:5,
  us10y = c(4, 4, 4, 4, 4, 4.10),
  us2y = c(3.5, 3.5, 3.5, 3.5, 3.5, 3.40),
  stringsAsFactors = FALSE
)
curve_snap <- asset_tx_snapshot(curve_panel, window = 20L)
curve_row <- curve_snap$nodes[curve_snap$nodes$id == "curve", , drop = FALSE]
check("curve level is 10Y minus 2Y", abs(curve_row$level - 0.70) < 1e-8)
check("curve shock is the spread change", abs(curve_row$shock_1d - 0.20) < 1e-8)
check("curve level prints in bp", identical(asset_tx_fmt_level(curve_row$level, "spread"), "+70.0 bp"))
check("curve shock prints in bp", identical(asset_tx_fmt_shock(curve_row$shock_1d, "spread"), "+20.0 bp"))
check(
  "curve symbol is the formula",
  identical(asset_tx_node_table(curve_snap, "en")[asset_tx_node_table(curve_snap, "en")[[1]] == "2s10s", 2][[1]], "10Y\u22122Y fut")
)

plant_move <- function(prev, last) {
  n <- 6L
  df <- data.frame(Date = as.Date("2025-07-01") + seq_len(n) - 1L, stringsAsFactors = FALSE)
  ids <- union(names(prev), names(last))
  for (id in ids) {
    start <- if (id %in% names(prev)) prev[[id]] else last[[id]]
    end <- if (id %in% names(last)) last[[id]] else start
    v <- rep(start, n)
    v[n] <- end
    df[[id]] <- v
  }
  asset_tx_snapshot(df, window = 20L)
}
regime_id <- function(snap) asset_tx_regime(snap, "en")$id
check(
  "regime expansion",
  identical(regime_id(plant_move(
    list(copper = 100, oil = 80, spx = 5000, us10y = 4.20),
    list(copper = 101, oil = 80.8, spx = 5050, us10y = 4.30)
  )), "expansion")
)
check(
  "inflationary expansion beats expansion",
  identical(regime_id(plant_move(
    list(copper = 100, oil = 80, spx = 5000, us10y = 4.20, dxy = 100, nasdaq = 100, btc = 100),
    list(copper = 101, oil = 80.8, spx = 5050, us10y = 4.30, dxy = 101, nasdaq = 99, btc = 100)
  )), "inflationary_expansion")
)
check(
  "regime stagflation",
  identical(regime_id(plant_move(
    list(oil = 80, gold = 2000, copper = 100, spx = 5000),
    list(oil = 81, gold = 2020, copper = 99, spx = 4950)
  )), "stagflation")
)
check(
  "stagflation allows flat copper",
  identical(regime_id(plant_move(
    list(oil = 80, gold = 2000, copper = 100, spx = 5000),
    list(oil = 81, gold = 2020, copper = 100.02, spx = 4950)
  )), "stagflation")
)
check(
  "regime soft landing",
  identical(regime_id(plant_move(
    list(infl = 33, us2y = 4.50, tlt = 90, gold = 2000, nasdaq = 100, btc = 100),
    list(infl = 32.5, us2y = 4.40, tlt = 91, gold = 2020, nasdaq = 100, btc = 100)
  )), "soft_landing")
)
check(
  "regime risk-off",
  identical(regime_id(plant_move(
    list(hyg = 75, spx = 5000, oil = 80, copper = 100, dxy = 100, tlt = 90),
    list(hyg = 74, spx = 4950, oil = 79, copper = 100, dxy = 101, tlt = 90)
  )), "risk_off")
)
check(
  "regime mixed",
  identical(regime_id(plant_move(
    list(spx = 100, gold = 100, dxy = 100),
    list(spx = 101, gold = 99, dxy = 99)
  )), "mixed")
)
check(
  "zh regime names the lean",
  grepl("供給衝擊", asset_tx_regime(plant_move(
    list(oil = 80, gold = 2000, copper = 100, spx = 5000),
    list(oil = 81, gold = 2020, copper = 99, spx = 4950)
  ), "zh-TW")$body, fixed = TRUE)
)
oil_edge <- tiny_snap$edges[tiny_snap$edges$from == "oil", , drop = FALSE]
check("missing upstream is n/a", all(oil_edge$state == "na"))

mid <- asset_tx_snapshot(panel, window = 60L, as_of = panel$Date[40])
check("as_of respected", identical(mid$as_of, panel$Date[40]))
check("earlier 10Y level", isTRUE(all.equal(mid$nodes$level[mid$nodes$id == "us10y"], panel$us10y[40])))
gap <- panel
gap <- rbind(gap, gap[nrow(gap), , drop = FALSE])
gap$Date[nrow(gap)] <- max(panel$Date) + 10
gap[nrow(gap), setdiff(names(gap), "Date")] <- NA_real_
gap_snap <- asset_tx_snapshot(gap, window = 60L)
check("blank calendar day is not the as-of", identical(gap_snap$as_of, max(panel$Date)))

en_sum <- asset_tx_summary_text(snap, "en")
zh_sum <- asset_tx_summary_text(snap, "zh-TW")
check("en summary has date", grepl(format(snap$as_of, "%Y-%m-%d"), en_sum, fixed = TRUE))
check("zh summary has 通道", grepl("通道", zh_sum, fixed = TRUE))
check("en summary names a link", grepl("Strongest trailing link", en_sum, fixed = TRUE))

en_nodes <- asset_tx_node_table(snap, "en")
zh_nodes <- asset_tx_node_table(snap, "zh-TW")
check("en table header", "Asset" %in% names(en_nodes) && "1D" %in% names(en_nodes))
check("zh table has TAIEX label", any(zh_nodes[[1]] == "台灣加權指數"))
en_edges <- asset_tx_edge_table(snap, "en")
check(
  "en edge state words",
  any(en_edges$State == "Aligned") && any(en_edges$State == "Diverged") && any(en_edges$State == "Regime")
)
zh_edges <- asset_tx_edge_table(snap, "zh-TW")
check("zh edge state words", any(grepl("一致", zh_edges[[ncol(zh_edges)]], fixed = TRUE)))
check("zh mixed state", any(zh_edges[[ncol(zh_edges)]] == "視情境"))

tape <- asset_tx_tape(panel, window = 60L, n = 12L)
check("tape length 12", length(tape) == 12L)
check("tape ends on last date", identical(tape[[length(tape)]]$as_of, max(panel$Date)))
tape_month <- asset_tx_tape(panel, window = 60L, n = as.integer(.asset_tx_tape_frames))
check(
  "default play tape is ~1 trading month",
  length(tape_month) == as.integer(.asset_tx_tape_frames) &&
    as.integer(.asset_tx_tape_frames) >= 20L
)
tape24 <- asset_tx_tape(panel, window = 60L, n = 24L)
check("tape can still request 24", length(tape24) == 24L)

if (requireNamespace("plotly", quietly = TRUE)) {
  fig <- asset_tx_figure(tape, "zh-TW", "TW")
  check("figure is plotly", inherits(fig, "plotly"))
  check("tape frames", length(fig$x$frames) == 12L)
  check("frame name is a date", grepl("^\\d{4}-\\d{2}-\\d{2}$", fig$x$frames[[1]]$name))
  built <- plotly::plotly_build(fig)
  blob <- paste(unlist(lapply(built$x$data, function(tr) c(tr$text, tr$hovertext, tr$customdata))), collapse = "\n")
  node_text <- unlist(lapply(built$x$data, function(tr) c(tr$text, tr$customdata)), use.names = FALSE)
  node_text <- node_text[!is.na(node_text) & nzchar(as.character(node_text))]
  check("zh label on figure", grepl("台灣加權指數", blob, fixed = TRUE))
  check(
    "zh TAIEX node shows level and percent shock",
    any(grepl("台灣加權指數<br>[0-9,]{3,}  [+-][0-9.]+%", node_text))
  )
  check(
    "zh US 10Y node shows percent level and bp shock",
    any(grepl("美國 10 年債<br>[0-9]+\\.[0-9]{2}%  [+-][0-9]+\\.[0-9] bp", node_text))
  )
  check(
    "zh hover still has last, 1D, and 5D",
    grepl("最新", blob, fixed = TRUE) &&
      grepl("1 日", blob, fixed = TRUE) &&
      grepl("5 日", blob, fixed = TRUE)
  )
  curve_fig <- plotly::plotly_build(asset_tx_figure(list(curve_snap), "zh-TW", "US"))
  curve_text <- unlist(lapply(curve_fig$x$data, function(tr) c(tr$text, tr$customdata)), use.names = FALSE)
  check(
    "zh 2s10s node shows spread level and bp change",
    any(grepl("2s10s<br>\\+70\\.0 bp  \\+20\\.0 bp", curve_text))
  )
  nodes_tr <- NULL
  for (tr in built$x$data) {
    if (identical(tr$name, "nodes")) {
      nodes_tr <- tr
      break
    }
  }
  check("nodes value trace present", !is.null(nodes_tr))
  val_cols <- unlist(nodes_tr$textfont$color, use.names = FALSE)
  check(
    "node numbers use move colors",
    length(val_cols) >= 2L && !all(tolower(val_cols) %in% c("#ffffff", "#f4f6fb", "white"))
  )
  arr <- .asset_tx_arrow(c(0, 1), c(0, 0), head = 0.11)
  check(
    "arrow head is a slim chevron",
    length(arr$x) >= 5L && any(is.na(arr$x))
  )
  bent <- .asset_tx_smooth_poly(c(0, 0, 1, 1), c(0, 1, 1, 0), n_per = 10L)
  check(
    "routed shaft is densified for smooth bends",
    length(bent$x) > 8L && all(is.finite(bent$x)) && all(is.finite(bent$y))
  )
  check("play control present", !is.null(built$x$layout$updatemenus) || !is.null(fig$x$layout$updatemenus))
  play_menu <- built$x$layout$updatemenus[[1]]
  if (is.null(play_menu)) play_menu <- fig$x$layout$updatemenus[[1]]
  slider0 <- built$x$layout$sliders[[1]]
  if (is.null(slider0)) slider0 <- fig$x$layout$sliders[[1]]
  check(
    "Play stays inside paper (left-anchored)",
    !is.null(play_menu) &&
      identical(as.character(play_menu$xanchor)[1], "left") &&
      isTRUE(as.numeric(play_menu$x)[1] <= 0.05)
  )
  check(
    "session slider spans nearly full width",
    !is.null(slider0) &&
      isTRUE(as.numeric(slider0$x)[1] <= 0.12) &&
      isTRUE(as.numeric(slider0$len)[1] >= 0.85)
  )
  marg_b <- built$x$layout$margin$b
  if (is.null(marg_b)) marg_b <- fig$x$layout$margin$b
  check(
    "bottom margin leaves room for slider/Play",
    isTRUE(as.numeric(marg_b)[1] >= 80)
  )
  frame_n <- vapply(fig$x$frames, function(fr) length(fr$data), integer(1))
  check("frames share a trace count", length(unique(frame_n)) == 1L && frame_n[1] > 40L)
  has_dots <- vapply(fig$x$frames, function(fr) {
    any(vapply(fr$data, function(tr) identical(tr$name, "dots"), logical(1)))
  }, logical(1))
  check("no dotted canvas behind cards", !any(has_dots))
  paper <- fig$x$layout$paper_bgcolor
  if (is.null(paper)) paper <- built$x$layout$paper$bgcolor
  check("black canvas", identical(paper, "#000000"))
  # Card fills should stay deep enough for white names + move-colored prints.
  card_fills <- character()
  shapes <- built$x$layout$shapes
  if (is.null(shapes)) shapes <- fig$x$layout$shapes
  if (length(shapes)) {
    card_fills <- vapply(shapes, function(sh) {
      if (!identical(sh$type, "path")) return(NA_character_)
      as.character(sh$fillcolor %||% NA_character_)[1]
    }, character(1))
    card_fills <- card_fills[!is.na(card_fills)]
  }
  check(
    "card fills use deep hub/charcoal tones",
    length(card_fills) >= 2L &&
      all(card_fills %in% c("#1f3f7a", "#161c2e"))
  )
  ui_mod <- paste(readLines("asset_transmission_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  check(
    "map CSS has no white-dot background",
    grepl("background-image: none", ui_mod, fixed = TRUE) &&
      !grepl("radial-gradient(rgba(244, 246, 251", ui_mod, fixed = TRUE)
  )
  check(
    "map height is extended",
    grepl("min(86vh, 980px)", ui_mod, fixed = TRUE) &&
      grepl('height = "900px"', ui_mod, fixed = TRUE)
  )
  check(
    "map fits panel width so timeline stays visible",
    grepl("overflow-x: auto", ui_mod, fixed = TRUE) &&
      grepl("min-width: 100%", ui_mod, fixed = TRUE) &&
      grepl("max-width: 100%", ui_mod, fixed = TRUE) &&
      !grepl("min-width: 1560px", ui_mod, fixed = TRUE) &&
      !grepl("overflow-x: hidden", ui_mod, fixed = TRUE)
  )
  check(
    "figure enables pan and zoom",
    isFALSE(isTRUE(built$x$layout$xaxis$fixedrange)) &&
      isFALSE(isTRUE(built$x$layout$yaxis$fixedrange)) &&
      identical(built$x$layout$dragmode, "pan")
  )
  check(
    "figure does not letterbox with scaleanchor",
    is.null(built$x$layout$yaxis$scaleanchor) &&
      is.null(built$x$layout$yaxis$scaleratio) &&
      !grepl("scaleanchor\\s*=", ui_mod)
  )
  xr <- as.numeric(unlist(built$x$layout$xaxis$range))
  yr <- as.numeric(unlist(built$x$layout$yaxis$range))
  nx <- as.numeric(catg$nodes$x)
  ny <- as.numeric(catg$nodes$y)
  x_mid <- mean(range(nx, na.rm = TRUE))
  y_mid <- mean(range(ny, na.rm = TRUE))
  check(
    "axis ranges center nodes with equal pads",
    length(xr) == 2L && length(yr) == 2L &&
      all(is.finite(xr)) && all(is.finite(yr)) &&
      isTRUE(all.equal(mean(xr), x_mid, tolerance = 1e-9)) &&
      isTRUE(all.equal(mean(yr), y_mid, tolerance = 1e-9)) &&
      isTRUE(all.equal((x_mid - xr[[1]]), (xr[[2]] - x_mid), tolerance = 1e-9)) &&
      isTRUE(all.equal((y_mid - yr[[1]]), (yr[[2]] - y_mid), tolerance = 1e-9))
  )
  check(
    "map scroll keeps Play/timeline start visible",
    grepl("centerAtxMap", ui_mod, fixed = TRUE) &&
      grepl("scrollLeft = 0", ui_mod, fixed = TRUE) &&
      !grepl("scrollLeft = maxX / 2", ui_mod, fixed = TRUE)
  )
  cfg <- built$x$config
  if (is.null(cfg)) cfg <- fig$x$config
  check(
    "plotly scrollZoom enabled",
    isTRUE(cfg$scrollZoom) || isTRUE(cfg[["scrollZoom"]])
  )
} else {
  stop("FAIL: plotly namespace missing")
}

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glb_src <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("testing page mounts map", grepl('asset_transmission_ui("atx")', ui_src, fixed = TRUE))
check("testing title id kept", grepl("ynow_testing_page_title", ui_src, fixed = TRUE))
check(
  "Asset transmission panel has no black box border",
  grepl("ynow-atx-panel", ui_src, fixed = TRUE) &&
    grepl("box.box-solid.box-primary.ynow-atx-panel", ui_src, fixed = TRUE) &&
    grepl("border: none !important", ui_src, fixed = TRUE)
)
check(
  "Testing tab forest theme mirrors Blue Chip toggle",
  grepl("ynow-theme-testing", ui_src, fixed = TRUE) &&
    grepl("tab === 'testing'", ui_src, fixed = TRUE) &&
    grepl("--ynow-testing-moss: #464704", ui_src, fixed = TRUE) &&
    grepl("--ynow-testing-sage: #9CA889", ui_src, fixed = TRUE) &&
    grepl("--ynow-testing-ivory: #F3F5E7", ui_src, fixed = TRUE) &&
    grepl("--ynow-testing-khaki: #B7A78C", ui_src, fixed = TRUE) &&
    grepl("--ynow-testing-brown: #423926", ui_src, fixed = TRUE)
)
check(
  "ATX move colors stay green-up / red-down (US)",
  identical(asset_tx_move_colors("US")$up, "#1e7a46") &&
    identical(asset_tx_move_colors("US")$down, "#c0392b")
)
check("server mounts module", grepl('asset_transmission_server(', srv_src, fixed = TRUE))
check("global sources module", grepl('source("asset_transmission_module.R"', glb_src, fixed = TRUE))
check(
  "global defers shinyapps Python install",
  grepl(".ynow_ensure_python", glb_src, fixed = TRUE) &&
    grepl("py_require(.ynow_py_pkgs)", glb_src, fixed = TRUE) &&
    grepl("UV_PYTHON_DOWNLOADS", glb_src, fixed = TRUE) &&
    grepl("RETICULATE_USE_MANAGED_VENV", glb_src, fixed = TRUE) &&
    grepl(".ynow_is_hosted_connect", glb_src, fixed = TRUE) &&
    grepl(".ynow_python_install_armed", glb_src, fixed = TRUE)
)
app_src <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "app blocks uv downloads before global.R",
  grepl("UV_PYTHON_DOWNLOADS", app_src, fixed = TRUE) &&
    grepl('Sys.setenv(', app_src, fixed = TRUE) &&
    grepl("RETICULATE_USE_MANAGED_VENV", app_src, fixed = TRUE) &&
    grepl(".ynow_is_hosted_connect", app_src, fixed = TRUE) &&
    grepl("/srv/connect/apps", app_src, fixed = TRUE) &&
    grepl("onStart", app_src, fixed = TRUE) &&
    grepl(".ynow_arm_python_install", app_src, fixed = TRUE)
)
cfg_src <- paste(readLines("default_config.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "default_config does not fetch live Rf at source",
  !grepl("cached_get_risk_free_rate()", cfg_src, fixed = TRUE) &&
    grepl("rf_fallback", cfg_src, fixed = TRUE)
)
rprofile <- if (file.exists(".Rprofile")) {
  paste(readLines(".Rprofile", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}
check(
  "Rprofile blocks managed uv before app.R",
  grepl("UV_PYTHON_DOWNLOADS", rprofile, fixed = TRUE) &&
    grepl("RETICULATE_USE_MANAGED_VENV", rprofile, fixed = TRUE)
)
mod_src <- paste(readLines("asset_transmission_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "figure avoids slow plotly animation_slider rebuild",
  grepl(".asset_tx_attach_play_frames", mod_src, fixed = TRUE) &&
    !grepl("plotly::animation_slider", mod_src, fixed = TRUE) &&
    !grepl("plotly::animation_button", mod_src, fixed = TRUE)
)
check(
  "fetch skips Python on shinyapps",
  grepl(".asset_tx_on_shinyapps", mod_src, fixed = TRUE) &&
    grepl("!isTRUE(.asset_tx_on_shinyapps())", mod_src, fixed = TRUE) &&
    grepl("/srv/connect/apps", mod_src, fixed = TRUE)
)
check(
  "panel load isolates reactiveVal reads for later()",
  grepl("shiny::isolate(pack_rv())", mod_src, fixed = TRUE) &&
    grepl("later::later(run, delay = 0)", mod_src, fixed = TRUE)
)

# Runtime: reading a reactiveVal inside later() without isolate() throws the
# exact shinyapps error; with isolate() the deferred load path stays safe.
if (requireNamespace("shiny", quietly = TRUE) && requireNamespace("later", quietly = TRUE)) {
  pack_rv <- shiny::reactiveVal(list(fetched_at = Sys.time(), n_ok = 1L))
  bad <- NULL
  later::later(function() {
    bad <<- tryCatch(pack_rv(), error = function(e) conditionMessage(e))
  }, delay = 0)
  later::run_now(timeoutSecs = 1)
  check(
    "later without isolate hits reactive-context error",
    is.character(bad) && grepl("active reactive context", bad, fixed = TRUE)
  )
  good <- NULL
  later::later(function() {
    good <<- tryCatch(shiny::isolate(pack_rv()), error = function(e) conditionMessage(e))
  }, delay = 0)
  later::run_now(timeoutSecs = 1)
  check(
    "later with isolate can read pack_rv",
    is.list(good) && identical(good$n_ok, 1L)
  )
} else {
  check("later/shiny available for reactive-context check", FALSE)
}
# Hosted-connect detector must recognize Posit Connect paths (opaque hostnames).
if (exists(".ynow_is_hosted_connect", mode = "function")) {
  old_wd <- getwd()
  tmp_connect <- file.path(tempdir(), "srv", "connect", "apps", "TheYNowApp")
  dir.create(tmp_connect, recursive = TRUE, showWarnings = FALSE)
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_connect)
  check("connect path counts as hosted", isTRUE(.ynow_is_hosted_connect()))
  setwd(old_wd)
} else {
  # Sourced module defines detector via global; fall back to source snippet check.
  check("connect path detector present", grepl("/srv/connect/apps", glb_src, fixed = TRUE))
}
dbg_paths <- c(
  "ynow_ui.R", "ynow_server.R", "macro_market_module.R", "backtest_module.R"
)
dbg_hit <- vapply(dbg_paths, function(fn) {
  src <- paste(readLines(fn, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl("debug-ef0f33\\.log", src) || grepl("127\\.0\\.0\\.1:7302/ingest", src)
}, logical(1))
check("no laptop debug log path left in app", !any(dbg_hit))
check("regime card wired", grepl('ns("regime")', mod_src, fixed = TRUE) && grepl("asset_tx_regime(", mod_src, fixed = TRUE))
check(
  "panel cache TTL exceeds 90s poll",
  grepl("\\.asset_tx_panel_cache_ttl_sec\\s*<-\\s*105", mod_src) ||
    (exists(".asset_tx_panel_cache_ttl_sec") && isTRUE(.asset_tx_panel_cache_ttl_sec >= 90))
)
check(
  "map has loading bar overlay",
  grepl("ynow-atx-load-bar", mod_src, fixed = TRUE) &&
    grepl('ns("map_loading")', mod_src, fixed = TRUE) &&
    grepl("atx_loading_bar", mod_src, fixed = TRUE)
)
check(
  "map wraps plotly with spinner",
  grepl("shinycssloaders::withSpinner", mod_src, fixed = TRUE) &&
    grepl('plotlyOutput(ns("map")', mod_src, fixed = TRUE)
)
check(
  "progressive tape then full frames",
  grepl("tape_n_rv", mod_src, fixed = TRUE) &&
    grepl("\\.asset_tx_tape_frames", mod_src) &&
    grepl("loading_rv", mod_src, fixed = TRUE)
)
check(
  "figure cache helpers present",
  grepl(".asset_tx_fig_cache", mod_src, fixed = TRUE) &&
    grepl(".asset_tx_fig_cache_get", mod_src, fixed = TRUE)
)
check(
  "alt downloads batched",
  grepl("need_alt", mod_src, fixed = TRUE) &&
    grepl("uniq_alt", mod_src, fixed = TRUE)
)
loc_src <- paste(readLines("ui_locale.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "pan hint localized en+zh-TW",
  grepl("atx_pan_hint", loc_src, fixed = TRUE) &&
    grepl("Swipe or drag to pan the map", loc_src, fixed = TRUE) &&
    grepl("可左右滑動或拖曳平移圖表", loc_src, fixed = TRUE)
)
check(
  "loading bar localized en+zh-TW",
  grepl("atx_loading_bar", loc_src, fixed = TRUE) &&
    grepl("Loading Asset transmission map", loc_src, fixed = TRUE) &&
    grepl("正在載入 Asset transmission 圖表", loc_src, fixed = TRUE)
)
check(
  "play tape covers at least one trading month",
  exists(".asset_tx_tape_frames") && isTRUE(as.integer(.asset_tx_tape_frames) >= 20L)
)
check(
  "play hint matches 22-session (~1 month) tape",
  grepl("last 22 sessions", loc_src, fixed = TRUE) &&
    grepl("about one month", loc_src, fixed = TRUE) &&
    grepl("近 22 個交易日", loc_src, fixed = TRUE) &&
    grepl("約一個月", loc_src, fixed = TRUE)
)

if (requireNamespace("shiny", quietly = TRUE)) {
  ui <- asset_transmission_ui("atx")
  check("module ui", inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
}

cat("ALL PASS\n")
