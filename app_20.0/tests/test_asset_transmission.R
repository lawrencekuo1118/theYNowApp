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

us_col <- asset_tx_move_colors("US")
tw_col <- asset_tx_move_colors("TW")
check("US up green", identical(us_col$up, "#1e7a46"))
check("TW up red", identical(tw_col$up, "#c0392b"))
check(
  "z color follows market",
  asset_tx_z_color(2.5, us_col) != asset_tx_z_color(2.5, tw_col)
)
check("aligned edge teal", identical(asset_tx_edge_color("aligned"), "#0f6e6e"))
check("diverged edge amber", identical(asset_tx_edge_color("diverged"), "#c47b14"))

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

tape <- asset_tx_tape(panel, window = 60L, n = 24L)
check("tape length 24", length(tape) == 24L)
check("tape ends on last date", identical(tape[[length(tape)]]$as_of, max(panel$Date)))

if (requireNamespace("plotly", quietly = TRUE)) {
  fig <- asset_tx_figure(tape, "zh-TW", "TW")
  check("figure is plotly", inherits(fig, "plotly"))
  check("tape frames", length(fig$x$frames) == 24L)
  check("frame name is a date", grepl("^\\d{4}-\\d{2}-\\d{2}$", fig$x$frames[[1]]$name))
  built <- plotly::plotly_build(fig)
  blob <- paste(unlist(lapply(built$x$data, function(tr) c(tr$text, tr$hovertext))), collapse = "\n")
  check("zh label on figure", grepl("台灣加權指數", blob, fixed = TRUE))
  check("play control present", !is.null(built$x$layout$updatemenus) || !is.null(fig$x$layout$updatemenus))
} else {
  stop("FAIL: plotly namespace missing")
}

ui_src <- paste(readLines("ynow_ui.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
srv_src <- paste(readLines("ynow_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glb_src <- paste(readLines("global.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("testing page mounts map", grepl('asset_transmission_ui("atx")', ui_src, fixed = TRUE))
check("testing title id kept", grepl("ynow_testing_page_title", ui_src, fixed = TRUE))
check("server mounts module", grepl('asset_transmission_server(', srv_src, fixed = TRUE))
check("global sources module", grepl('source("asset_transmission_module.R"', glb_src, fixed = TRUE))
mod_src <- paste(readLines("asset_transmission_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("regime card wired", grepl('ns("regime")', mod_src, fixed = TRUE) && grepl("asset_tx_regime(", mod_src, fixed = TRUE))

if (requireNamespace("shiny", quietly = TRUE)) {
  ui <- asset_transmission_ui("atx")
  check("module ui", inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
}

cat("ALL PASS\n")
