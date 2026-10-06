# ==========================================
# asset_transmission_module.R — Asset transmission sidebar tab (Lite + Full)
#
# Live intermarket transmission map.
# Order: inflation and the cycle → policy rate and the curve → dollar and
# liquidity → bonds, commodities, precious metals, energy, equities, crypto.
# Nodes are Yahoo daily bars in native quotes, plus a curve computed in R.
# No session FX conversion. The 2s10s level is not a Yahoo symbol.
# Arrow = structural channel (upstream → downstream), not "both rise".
# Width = |Pearson corr| of daily changes over the selected window.
# Color = whether that corr matches the structural prior.
# Yields and the curve use daily differences (percentage points, shown in bp).
# Other nodes use % changes.
# Priors and the regime card are surveillance, not a forecast and not WACC.
# ==========================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

# ---- Graph ----------------------------------------------------------------

asset_tx_catalog <- function() {
  # Drivers → policy rate / curve → dollar → liquidity → asset classes.
  # prior 0 = that link flips by regime (drawn purple once |corr| clears 0.15).
  # `curve` has a blank Yahoo symbol: the level is US 10Y minus the 2Y futures yield.
  asset_x <- c(0.40, 2.25, 4.10, 5.95, 7.80, 9.65, 11.50, 13.35)
  nodes <- data.frame(
    id = c(
      "infl", "copper", "oil", "brent", "ng",
      "us2y", "us_bill", "us5y", "us10y", "us30y", "curve",
      "dxy", "eurusd", "usdjpy", "usdcny", "usdtwd",
      "move", "vix", "hyg",
      "tlt", "ief", "tip", "spx", "nasdaq", "ndx", "sox", "stoxx",
      "nikkei", "hsi", "taiex", "gold", "silver", "btc", "vnq", "wheat"
    ),
    yahoo = c(
      "RINF", "HG=F", "CL=F", "BZ=F", "NG=F",
      "2YY=F", "^IRX", "^FVX", "^TNX", "^TYX", "",
      "DX-Y.NYB", "EURUSD=X", "JPY=X", "CNY=X", "TWD=X",
      "^MOVE", "^VIX", "HYG",
      "TLT", "IEF", "TIP", "^GSPC", "^IXIC", "^NDX", "^SOX", "^STOXX50E",
      "^N225", "^HSI", "^TWII", "GC=F", "SI=F", "BTC-USD", "VNQ", "ZW=F"
    ),
    x = c(
      rep(0.60, 5),
      rep(3.25, 6),
      rep(6.00, 5),
      rep(8.70, 3),
      asset_x,
      asset_x
    ),
    y = c(
      c(9.2, 8.0, 6.8, 5.6, 4.4),
      c(9.2, 8.2, 7.2, 6.2, 5.2, 4.2),
      c(9.2, 8.0, 6.8, 5.6, 4.4),
      c(8.6, 6.5, 4.4),
      rep(2.35, 8),
      rep(-0.25, 8)
    ),
    textposition = c(
      rep("middle right", 5),
      rep("middle right", 6),
      rep("middle right", 5),
      rep("middle left", 3),
      rep(c("top center", "bottom center"), 4),
      rep("bottom center", 8)
    ),
    kind = c(
      "etf", "commodity", "commodity", "commodity", "commodity",
      "yield", "yield", "yield", "yield", "yield", "spread",
      "index", "fx", "fx", "fx", "fx",
      "vol", "vol", "etf",
      "etf", "etf", "etf", "index", "index", "index", "index", "index",
      "index", "index", "index", "commodity", "commodity", "crypto", "etf", "commodity"
    ),
    band = c(
      rep("driver", 5),
      rep("rate", 6),
      rep("fx", 5),
      rep("liq", 3),
      rep("asset", 16)
    ),
    stringsAsFactors = FALSE
  )
  nodes$yahoo_alt <- ""
  nodes$symbol_show <- ifelse(nodes$id == "curve", "10Y\u22122Y fut", "")
  nodes$transform <- ifelse(nodes$kind %in% c("yield", "spread"), "diff", "ret")
  nodes$label_key <- paste0("atx_node_", nodes$id)
  nodes$gloss_key <- paste0("atx_gloss_", nodes$id)

  edges <- data.frame(
    from = c(
      "oil", "infl", "copper",
      "us_bill", "us5y", "us10y",
      "us2y", "us2y",
      "us10y", "us10y", "us10y", "us10y", "us10y", "us10y", "us10y",
      "dxy", "dxy", "dxy", "dxy", "dxy", "dxy", "dxy", "dxy",
      "dxy", "dxy",
      "vix", "vix", "vix", "vix", "vix", "move",
      "spx", "nasdaq", "sox", "spx", "nasdaq", "nasdaq", "nasdaq",
      "nasdaq", "ndx",
      "spx", "gold",
      "brent", "ng", "tip", "copper", "hyg"
    ),
    to = c(
      "us10y", "us10y", "spx",
      "us10y", "us10y", "us30y",
      "us5y", "us10y",
      "dxy", "spx", "nasdaq", "gold", "tlt", "vnq", "btc",
      "eurusd", "usdjpy", "usdcny", "usdtwd", "gold", "hsi", "taiex", "btc",
      "oil", "copper",
      "spx", "nasdaq", "taiex", "hyg", "gold", "vix",
      "nasdaq", "sox", "taiex", "stoxx", "nikkei", "hsi", "btc",
      "ndx", "sox",
      "tlt", "silver",
      "us10y", "oil", "gold", "silver", "spx"
    ),
    prior = c(
      1, 1, 1,
      1, 1, 1,
      1, 1,
      0, -1, -1, 0, -1, -1, -1,
      -1, 1, 1, 1, -1, -1, -1, -1,
      0, -1,
      -1, -1, -1, -1, 0, 1,
      1, 1, 1, 1, 1, 1, 1,
      1, 1,
      -1, 1,
      1, 0, 0, 1, 1
    ),
    stringsAsFactors = FALSE
  )
  edges$channel_key <- paste0("atx_ch_", edges$from, "_", edges$to)
  edges$prior <- as.integer(edges$prior)
  list(nodes = nodes, edges = edges)
}

asset_tx_window <- function(x) {
  w <- suppressWarnings(as.integer(x)[1])
  if (!is.finite(w) || w < 10L) return(60L)
  as.integer(min(250L, w))
}

asset_tx_move_colors <- function(market_mode = "US") {
  tw <- identical(toupper(trimws(as.character(market_mode %||% "US")[1])), "TW")
  if (tw) {
    list(up = "#c0392b", down = "#1e7a46", flat = "#8d9399")
  } else {
    list(up = "#1e7a46", down = "#c0392b", flat = "#8d9399")
  }
}

.asset_tx_hex_rgb <- function(hex) {
  hex <- gsub("#", "", as.character(hex)[1])
  if (nchar(hex) < 6L) return(c(0, 0, 0))
  c(
    strtoi(substr(hex, 1L, 2L), 16L),
    strtoi(substr(hex, 3L, 4L), 16L),
    strtoi(substr(hex, 5L, 6L), 16L)
  )
}

.asset_tx_mix <- function(a, b, t) {
  t <- max(0, min(1, as.numeric(t)[1]))
  if (!is.finite(t)) t <- 0
  ra <- .asset_tx_hex_rgb(a)
  rb <- .asset_tx_hex_rgb(b)
  v <- round(ra * (1 - t) + rb * t)
  v[!is.finite(v)] <- 0
  sprintf("#%02x%02x%02x", v[1], v[2], v[3])
}

asset_tx_z_color <- function(z, colors) {
  if (!is.finite(z)) return(colors$flat)
  t <- tanh(z / 1.25)
  if (abs(t) < 0.08) return(colors$flat)
  if (t > 0) .asset_tx_mix(colors$flat, colors$up, t) else .asset_tx_mix(colors$flat, colors$down, -t)
}

asset_tx_edge_color <- function(state) {
  switch(
    as.character(state %||% "na")[1],
    aligned = "#2ec4c6",
    diverged = "#c47b14",
    mixed = "#6a5acd",
    weak = "#a7adb4",
    "#c5c9ce"
  )
}

# ---- Formatting ------------------------------------------------------------

asset_tx_fmt_level <- function(level, kind = "index") {
  if (!is.finite(level)) return("—")
  kind <- as.character(kind %||% "index")[1]
  if (kind == "yield") return(sprintf("%.2f%%", level))
  # Spread level is percentage points (10Y minus the short yield). Positive = upward sloping.
  if (kind == "spread") return(sprintf("%+.1f bp", level * 100))
  if (kind == "fx") return(sprintf("%.3f", level))
  if (kind == "vol") return(sprintf("%.2f", level))
  if (kind %in% c("commodity", "etf")) {
    return(format(round(level, 2), nsmall = 2, big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  if (abs(level) >= 1000) {
    return(format(round(level, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  format(round(level, 2), nsmall = 2, big.mark = ",", scientific = FALSE, trim = TRUE)
}

asset_tx_fmt_shock <- function(shock, kind = "index") {
  if (!is.finite(shock)) return("—")
  kind <- as.character(kind %||% "index")[1]
  if (kind %in% c("yield", "spread")) return(sprintf("%+.1f bp", shock * 100))
  sprintf("%+.2f%%", shock * 100)
}

asset_tx_fmt_corr <- function(corr) {
  if (!is.finite(corr)) return("—")
  sprintf("%+.2f", corr)
}

.asset_tx_label <- function(key, locale) {
  if (exists("ui_str", mode = "function")) return(ui_str(key, locale))
  key
}

# ---- Snapshot --------------------------------------------------------------

# Curve level = long yield minus the 2Y futures yield, in the same percentage-point units.
# Recomputed even if the caller passed a column, so the node is never a fake Yahoo print.
.asset_tx_apply_derived <- function(panel, nodes) {
  if (is.null(panel) || !is.data.frame(panel)) return(panel)
  if ("curve" %in% nodes$id && all(c("us10y", "us2y") %in% names(panel))) {
    panel$curve <- panel$us10y - panel$us2y
  }
  panel
}

.asset_tx_symbol_label <- function(meta, sym_attr = NULL, id = NULL) {
  if (!is.null(sym_attr) && !is.null(id) && id %in% names(sym_attr) && nzchar(sym_attr[[id]])) {
    return(as.character(sym_attr[[id]])[1])
  }
  show <- if ("symbol_show" %in% names(meta)) as.character(meta$symbol_show[1]) else ""
  if (nzchar(show)) return(show)
  y <- as.character(meta$yahoo[1])
  if (nzchar(y)) y else "\u2014"
}

.asset_tx_move_threshold <- function(kind) {
  if (as.character(kind %||% "index")[1] %in% c("yield", "spread")) 0.005 else 0.0005
}

.asset_tx_dir <- function(snap, id) {
  row <- snap$nodes[snap$nodes$id == id, , drop = FALSE]
  if (!nrow(row) || !is.finite(row$shock_1d[1])) return("na")
  thr <- .asset_tx_move_threshold(row$kind[1])
  sh <- row$shock_1d[1]
  if (sh >= thr) "up" else if (sh <= -thr) "down" else "flat"
}

.asset_tx_prepare_panel <- function(panel, nodes) {
  if (is.null(panel) || !is.data.frame(panel) || !nrow(panel) || !"Date" %in% names(panel)) {
    return(NULL)
  }
  out <- data.frame(Date = as.Date(panel$Date), stringsAsFactors = FALSE)
  out <- out[!is.na(out$Date), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  for (id in nodes$id) {
    if (id %in% names(panel)) {
      out[[id]] <- suppressWarnings(as.numeric(panel[[id]]))
    } else {
      out[[id]] <- NA_real_
    }
  }
  out <- out[order(out$Date), , drop = FALSE]
  # Last print wins if a calendar day is repeated.
  out <- out[!duplicated(out$Date, fromLast = TRUE), , drop = FALSE]
  out <- .asset_tx_apply_derived(out, nodes)
  value_cols <- setdiff(names(out), "Date")
  any_print <- Reduce(`|`, lapply(value_cols, function(col) is.finite(out[[col]])))
  if (!length(any_print) || !any(any_print)) return(NULL)
  out <- out[any_print, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# reticulate drops Python None when unlist() is applied to a mixed list.
.asset_tx_num_vec <- function(x, n = NULL) {
  if (is.null(x)) return(rep(NA_real_, n %||% 0L))
  if (is.numeric(x) && !is.list(x)) {
    v <- suppressWarnings(as.numeric(x))
  } else if (is.list(x)) {
    v <- vapply(x, function(el) {
      if (is.null(el) || length(el) == 0L) return(NA_real_)
      out <- suppressWarnings(as.numeric(el)[1])
      if (length(out) != 1L || !is.finite(out)) NA_real_ else out
    }, numeric(1))
  } else {
    v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  }
  if (!is.null(n) && length(v) != n) {
    v2 <- rep(NA_real_, n)
    m <- min(length(v), n)
    if (m > 0L) v2[seq_len(m)] <- v[seq_len(m)]
    v <- v2
  }
  v
}

asset_tx_snapshot <- function(panel, catalog = NULL, window = 60L, as_of = NULL) {
  catalog <- catalog %||% asset_tx_catalog()
  nodes <- catalog$nodes
  edges <- catalog$edges
  panel <- .asset_tx_prepare_panel(panel, nodes)
  if (is.null(panel)) return(NULL)
  window <- asset_tx_window(window)
  as_of <- if (is.null(as_of)) max(panel$Date) else as.Date(as_of)
  if (!length(as_of) || is.na(as_of)) return(NULL)
  panel <- panel[panel$Date <= as_of, , drop = FALSE]
  if (!nrow(panel)) return(NULL)
  n <- nrow(panel)
  idx <- n
  shock <- lapply(seq_len(nrow(nodes)), function(i) rep(NA_real_, n))
  names(shock) <- nodes$id
  for (i in seq_len(nrow(nodes))) {
    id <- nodes$id[i]
    lvl <- panel[[id]]
    sh <- rep(NA_real_, n)
    finite_idx <- which(is.finite(lvl))
    if (length(finite_idx) >= 2L) {
      prev_i <- finite_idx[-length(finite_idx)]
      cur_i <- finite_idx[-1L]
      prev <- lvl[prev_i]
      cur <- lvl[cur_i]
      if (identical(nodes$transform[i], "diff")) {
        sh[cur_i] <- cur - prev
      } else {
        sh[cur_i] <- ifelse(is.finite(prev) & prev != 0, cur / prev - 1, NA_real_)
      }
    }
    shock[[id]] <- sh
  }
  win_idx <- seq.int(max(1L, n - window + 1L), n)
  node_rows <- lapply(seq_len(nrow(nodes)), function(i) {
    id <- nodes$id[i]
    lvl <- panel[[id]]
    sh <- shock[[id]]
    finite_idx <- which(is.finite(lvl) & seq_along(lvl) <= idx)
    level <- NA_real_
    shock_1d <- NA_real_
    shock_5d <- NA_real_
    if (length(finite_idx)) {
      j <- finite_idx[length(finite_idx)]
      level <- lvl[j]
      shock_1d <- sh[j]
      if (length(finite_idx) >= 6L) {
        j5 <- finite_idx[length(finite_idx) - 5L]
        if (identical(nodes$transform[i], "diff")) {
          shock_5d <- lvl[j] - lvl[j5]
        } else if (is.finite(lvl[j5]) && lvl[j5] != 0) {
          shock_5d <- lvl[j] / lvl[j5] - 1
        }
      }
    }
    hist <- sh[win_idx]
    hist <- hist[is.finite(hist)]
    sdv <- if (length(hist) >= 10L) stats::sd(hist) else NA_real_
    z <- if (is.finite(sdv) && sdv > 0 && is.finite(shock_1d)) shock_1d / sdv else NA_real_
    data.frame(
      id = id,
      kind = nodes$kind[i],
      transform = nodes$transform[i],
      level = level,
      shock_1d = shock_1d,
      shock_5d = shock_5d,
      sd = sdv,
      z = z,
      stringsAsFactors = FALSE
    )
  })
  node_df <- do.call(rbind, node_rows)
  rownames(node_df) <- NULL

  edge_rows <- lapply(seq_len(nrow(edges)), function(i) {
    a <- edges$from[i]
    b <- edges$to[i]
    sa <- shock[[a]][win_idx]
    sb <- shock[[b]][win_idx]
    ok <- is.finite(sa) & is.finite(sb)
    n_ok <- sum(ok)
    corr <- if (n_ok >= 20L) suppressWarnings(stats::cor(sa[ok], sb[ok])) else NA_real_
    if (!is.finite(corr)) corr <- NA_real_
    state <- "na"
    if (is.finite(corr)) {
      state <- if (abs(corr) < 0.15) {
        "weak"
      } else if (edges$prior[i] == 0L) {
        "mixed"
      } else if (sign(corr) == edges$prior[i]) {
        "aligned"
      } else {
        "diverged"
      }
    }
    za <- node_df$z[match(a, node_df$id)]
    zb <- node_df$z[match(b, node_df$id)]
    sa1 <- node_df$shock_1d[match(a, node_df$id)]
    sb1 <- node_df$shock_1d[match(b, node_df$id)]
    tiny <- function(v, z) {
      !is.finite(v) || abs(v) < 1e-12 || (is.finite(z) && abs(z) < 0.05)
    }
    today <- "flat"
    if (!tiny(sa1, za) && !tiny(sb1, zb)) {
      today <- if (edges$prior[i] == 0L) {
        "mixed"
      } else if (as.integer(sign(sa1) * sign(sb1)) == edges$prior[i]) {
        "with_prior"
      } else {
        "against_prior"
      }
    }
    data.frame(
      from = a,
      to = b,
      prior = edges$prior[i],
      channel_key = edges$channel_key[i],
      corr = corr,
      n = as.integer(n_ok),
      state = state,
      today = today,
      stringsAsFactors = FALSE
    )
  })
  edge_df <- do.call(rbind, edge_rows)
  rownames(edge_df) <- NULL
  list(
    as_of = panel$Date[idx],
    window = window,
    nodes = node_df,
    edges = edge_df,
    catalog = catalog
  )
}

asset_tx_tape <- function(panel, catalog = NULL, window = 60L, n = 22L) {
  catalog <- catalog %||% asset_tx_catalog()
  prepared <- .asset_tx_prepare_panel(panel, catalog$nodes)
  if (is.null(prepared)) return(list())
  # Allow n=1 for progressive first paint; Play uses n>=2.
  dts <- tail(sort(unique(prepared$Date)), max(1L, as.integer(n)[1]))
  snaps <- lapply(dts, function(d) {
    asset_tx_snapshot(prepared, catalog = catalog, window = window, as_of = d)
  })
  snaps[!vapply(snaps, is.null, logical(1))]
}

.asset_tx_state_key <- function(state) {
  switch(
    as.character(state %||% "na")[1],
    aligned = "atx_state_aligned",
    diverged = "atx_state_diverged",
    mixed = "atx_state_mixed",
    weak = "atx_state_weak",
    "atx_state_na"
  )
}

.asset_tx_today_key <- function(today) {
  switch(
    as.character(today %||% "flat")[1],
    with_prior = "atx_today_with",
    against_prior = "atx_today_against",
    mixed = "atx_today_mixed",
    "atx_today_flat"
  )
}

.asset_tx_prior_key <- function(prior) {
  if (!is.finite(prior) || prior == 0) return("atx_prior_mixed")
  if (prior < 0) "atx_prior_opp" else "atx_prior_same"
}

asset_tx_summary_text <- function(snap, locale = "en") {
  if (is.null(snap)) return(.asset_tx_label("atx_empty", locale))
  loc <- locale
  nodes <- snap$nodes
  edges <- snap$edges
  lab <- function(id) .asset_tx_label(paste0("atx_node_", id), loc)
  scored <- edges[edges$today %in% c("with_prior", "against_prior"), , drop = FALSE]
  n_scored <- nrow(scored)
  n_with <- sum(scored$today == "with_prior")
  finite_corr <- edges[is.finite(edges$corr), , drop = FALSE]
  strongest <- ""
  if (nrow(finite_corr)) {
    k <- which.max(abs(finite_corr$corr))
    strongest <- .ynow_fmt_str(
      .asset_tx_label("atx_summary_link", loc),
      from = lab(finite_corr$from[k]),
      to = lab(finite_corr$to[k]),
      corr = asset_tx_fmt_corr(finite_corr$corr[k])
    )
  }
  if (n_scored == 0L) {
    base <- .ynow_fmt_str(
      .asset_tx_label("atx_summary_flat", loc),
      asof = format(snap$as_of, "%Y-%m-%d")
    )
  } else {
    base <- .ynow_fmt_str(
      .asset_tx_label("atx_summary", loc),
      asof = format(snap$as_of, "%Y-%m-%d"),
      n_with = n_with,
      n_scored = n_scored
    )
  }
  leader <- ""
  if (any(is.finite(nodes$z))) {
    j <- which.max(abs(nodes$z))
    leader <- .ynow_fmt_str(
      .asset_tx_label("atx_leader", loc),
      name = lab(nodes$id[j]),
      chg = asset_tx_fmt_shock(nodes$shock_1d[j], nodes$kind[j]),
      z = sprintf("%.1f", nodes$z[j])
    )
  }
  paste(c(base, strongest, leader)[nzchar(c(base, strongest, leader))], collapse = " ")
}

asset_tx_node_table <- function(snap, locale = "en") {
  loc <- locale
  empty <- data.frame(
    Asset = character(), Symbol = character(), Last = character(),
    `1D` = character(), `5D` = character(), check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (is.null(snap)) return(empty)
  nodes <- snap$catalog$nodes
  sym_attr <- attr(snap, "symbols")
  rows <- lapply(seq_len(nrow(snap$nodes)), function(i) {
    id <- snap$nodes$id[i]
    meta <- nodes[nodes$id == id, , drop = FALSE]
    sym <- .asset_tx_symbol_label(meta, sym_attr, id)
    data.frame(
      a = .asset_tx_label(meta$label_key[1], loc),
      b = sym,
      c = asset_tx_fmt_level(snap$nodes$level[i], meta$kind[1]),
      d = asset_tx_fmt_shock(snap$nodes$shock_1d[i], meta$kind[1]),
      e = asset_tx_fmt_shock(snap$nodes$shock_5d[i], meta$kind[1]),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  names(df) <- c(
    .asset_tx_label("atx_col_asset", loc),
    .asset_tx_label("atx_col_symbol", loc),
    .asset_tx_label("atx_col_last", loc),
    .asset_tx_label("atx_col_1d", loc),
    .asset_tx_label("atx_col_5d", loc)
  )
  df
}

asset_tx_edge_table <- function(snap, locale = "en") {
  loc <- locale
  empty_names <- c(
    .asset_tx_label("atx_col_from", loc),
    .asset_tx_label("atx_col_to", loc),
    .asset_tx_label("atx_col_channel", loc),
    .asset_tx_label("atx_col_prior", loc),
    .asset_tx_label("atx_col_corr", loc),
    .asset_tx_label("atx_col_today", loc),
    .asset_tx_label("atx_col_state", loc)
  )
  if (is.null(snap)) {
    df <- as.data.frame(setNames(replicate(length(empty_names), character(), simplify = FALSE), empty_names), stringsAsFactors = FALSE)
    return(df)
  }
  lab <- function(id) .asset_tx_label(paste0("atx_node_", id), loc)
  df <- data.frame(
    from = vapply(snap$edges$from, lab, character(1)),
    to = vapply(snap$edges$to, lab, character(1)),
    channel = vapply(snap$edges$channel_key, .asset_tx_label, character(1), locale = loc),
    prior = vapply(snap$edges$prior, function(p) .asset_tx_label(.asset_tx_prior_key(p), loc), character(1)),
    corr = vapply(snap$edges$corr, asset_tx_fmt_corr, character(1)),
    today = vapply(snap$edges$today, function(t) .asset_tx_label(.asset_tx_today_key(t), loc), character(1)),
    state = vapply(snap$edges$state, function(s) .asset_tx_label(.asset_tx_state_key(s), loc), character(1)),
    stringsAsFactors = FALSE
  )
  names(df) <- empty_names
  df
}

# ---- Figure ----------------------------------------------------------------

# Card size in data units. Slightly taller than the arrow-routing spacing so
# name + print lines keep comfortable line gap inside the rounded body.
.asset_tx_card_geom <- function() {
  list(w = 1.48, h = 0.74, r = 0.13, hw = 0.74, hh = 0.37, pad = 0.06)
}

# Hub bands use a deeper blue; other cards stay charcoal. Both keep white names
# and move-colored prints readable (avoid pale fills that wash out the type).
.asset_tx_band_fill <- function(band) {
  if (as.character(band %||% "")[1] %in% c("rate", "fx", "liq")) "#1f3f7a" else "#161c2e"
}

# SVG path for a rounded rectangle centered at (x, y). Data y increases upward.
.asset_tx_round_rect <- function(x, y, w, h, r) {
  x <- as.numeric(x)[1]
  y <- as.numeric(y)[1]
  w <- abs(as.numeric(w)[1])
  h <- abs(as.numeric(h)[1])
  r <- as.numeric(r)[1]
  if (!is.finite(x) || !is.finite(y) || !is.finite(w) || !is.finite(h)) return("")
  if (!is.finite(r) || r < 0) r <- 0
  hw <- w / 2
  hh <- h / 2
  r <- min(r, hw, hh)
  x0 <- x - hw
  x1 <- x + hw
  y0 <- y - hh
  y1 <- y + hh
  sprintf(
    paste0(
      "M %.4f,%.4f L %.4f,%.4f Q %.4f,%.4f %.4f,%.4f ",
      "L %.4f,%.4f Q %.4f,%.4f %.4f,%.4f ",
      "L %.4f,%.4f Q %.4f,%.4f %.4f,%.4f ",
      "L %.4f,%.4f Q %.4f,%.4f %.4f,%.4f Z"
    ),
    x0 + r, y1,
    x1 - r, y1,
    x1, y1, x1, y1 - r,
    x1, y0 + r,
    x1, y0, x1 - r, y0,
    x0 + r, y0,
    x0, y0, x0, y0 + r,
    x0, y1 - r,
    x0, y1, x0 + r, y1
  )
}

# Points of the same rounded rect, so a per-frame border trace sits on the shape.
.asset_tx_round_poly <- function(x, y, w, h, r, n_arc = 6L) {
  x <- as.numeric(x)[1]
  y <- as.numeric(y)[1]
  w <- abs(as.numeric(w)[1])
  h <- abs(as.numeric(h)[1])
  r <- as.numeric(r)[1]
  hw <- w / 2
  hh <- h / 2
  if (!is.finite(r) || r < 0) r <- 0
  r <- min(r, hw, hh)
  x0 <- x - hw
  x1 <- x + hw
  y0 <- y - hh
  y1 <- y + hh
  quad <- function(p0, p1, p2) {
    t <- seq(0, 1, length.out = n_arc)
    list(
      x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
      y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2]
    )
  }
  corners <- list(
    quad(c(x1 - r, y1), c(x1, y1), c(x1, y1 - r)),
    quad(c(x1, y0 + r), c(x1, y0), c(x1 - r, y0)),
    quad(c(x0 + r, y0), c(x0, y0), c(x0, y0 + r)),
    quad(c(x0, y1 - r), c(x0, y1), c(x0 + r, y1))
  )
  xs <- unlist(lapply(corners, `[[`, "x"), use.names = FALSE)
  ys <- unlist(lapply(corners, `[[`, "y"), use.names = FALSE)
  list(x = c(xs, xs[1]), y = c(ys, ys[1]))
}

.asset_tx_rect_hit <- function(ux, uy, hw, hh, pad) {
  ax <- abs(ux)
  ay <- abs(uy)
  tx <- if (ax < 1e-8) Inf else hw / ax
  ty <- if (ay < 1e-8) Inf else hh / ay
  d <- min(tx, ty)
  if (!is.finite(d)) d <- max(hw, hh)
  d + pad
}

.asset_tx_segment <- function(x1, y1, x2, y2, inset_start = NULL, inset_end = NULL) {
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx * dx + dy * dy)
  if (!is.finite(len) || len < 1e-6) {
    return(list(x1 = x1, y1 = y1, x2 = x2, y2 = y2, ux = 1, uy = 0))
  }
  ux <- dx / len
  uy <- dy / len
  geom <- .asset_tx_card_geom()
  if (is.null(inset_start)) inset_start <- .asset_tx_rect_hit(ux, uy, geom$hw, geom$hh, geom$pad)
  if (is.null(inset_end)) inset_end <- .asset_tx_rect_hit(ux, uy, geom$hw, geom$hh, geom$pad)
  # Keep a visible shaft when two cards are closer than the two insets.
  if (inset_start + inset_end > len * 0.86) {
    scale <- (len * 0.86) / (inset_start + inset_end)
    inset_start <- inset_start * scale
    inset_end <- inset_end * scale
  }
  list(
    x1 = x1 + ux * inset_start,
    y1 = y1 + uy * inset_start,
    x2 = x2 - ux * inset_end,
    y2 = y2 - uy * inset_end,
    ux = ux,
    uy = uy
  )
}

# Densify a control polyline into a smooth shaft. Multi-bend routes lose the
# sharp parallelogram corners; true two-point shafts stay straight.
.asset_tx_smooth_poly <- function(xs, ys, n_per = 14L) {
  xs <- as.numeric(xs)
  ys <- as.numeric(ys)
  ok <- is.finite(xs) & is.finite(ys)
  xs <- xs[ok]
  ys <- ys[ok]
  n <- length(xs)
  if (n < 2L) return(list(x = xs, y = ys))
  if (n == 2L) return(list(x = xs, y = ys))
  d <- c(0, cumsum(sqrt(diff(xs)^2 + diff(ys)^2)))
  total <- d[length(d)]
  if (!is.finite(total) || total < 1e-6) return(list(x = xs, y = ys))
  # fmm spline rounds the corners without overshooting as hard as natural.
  n_out <- max(18L, (n - 1L) * as.integer(n_per))
  tt <- seq(0, total, length.out = n_out)
  list(
    x = stats::spline(d, xs, xout = tt, method = "fmm")$y,
    y = stats::spline(d, ys, xout = tt, method = "fmm")$y
  )
}

.asset_tx_arrow <- function(xs, ys, head = 0.11) {
  sm <- .asset_tx_smooth_poly(xs, ys)
  xs <- sm$x
  ys <- sm$y
  n <- length(xs)
  if (n < 2L) return(list(x = xs, y = ys))
  dx <- xs[n] - xs[n - 1L]
  dy <- ys[n] - ys[n - 1L]
  seglen <- sqrt(dx * dx + dy * dy)
  if (!is.finite(seglen) || seglen < 1e-5) return(list(x = xs, y = ys))
  if (seglen < head * 2.4) head <- max(0.05, seglen * 0.38)
  ux <- dx / seglen
  uy <- dy / seglen
  px <- -uy
  py <- ux
  tipx <- xs[n]
  tipy <- ys[n]
  # Pull the shaft tip back so the chevron sits cleanly on the end.
  shaft_end_x <- tipx - ux * (head * 0.72)
  shaft_end_y <- tipy - uy * (head * 0.72)
  bx <- tipx - ux * head
  by <- tipy - uy * head
  w <- head * 0.42
  list(
    x = c(xs[-n], shaft_end_x, NA, bx + px * w, tipx, bx - px * w),
    y = c(ys[-n], shaft_end_y, NA, by + py * w, tipy, by - py * w)
  )
}

.asset_tx_chevron <- function(seg, head = 0.11) {
  .asset_tx_arrow(c(seg$x1, seg$x2), c(seg$y1, seg$y2), head = head)
}

.asset_tx_poly_hits <- function(xs, ys, nodes, id_from, id_to) {
  geom <- .asset_tx_card_geom()
  others <- nodes[nodes$id != id_from & nodes$id != id_to, , drop = FALSE]
  if (!nrow(others) || length(xs) < 2L) return(0L)
  n <- 0L
  for (k in seq_len(length(xs) - 1L)) {
    ts <- seq(0.12, 0.88, length.out = 14L)
    px <- xs[k] + (xs[k + 1L] - xs[k]) * ts
    py <- ys[k] + (ys[k + 1L] - ys[k]) * ts
    for (j in seq_len(nrow(others))) {
      if (any(abs(px - others$x[j]) < geom$hw - 0.03 & abs(py - others$y[j]) < geom$hh - 0.03)) {
        n <- n + 1L
      }
    }
  }
  n
}

# If the straight shaft would cross another card, slide it sideways and
# reconnect to the inset points. Geometry is static, so cache the polyline.
.asset_tx_route_cache <- new.env(parent = emptyenv())

.asset_tx_routed_arrow <- function(x1, y1, x2, y2, nodes, id_from, id_to) {
  key <- sprintf("%s|%s", id_from, id_to)
  if (exists(key, envir = .asset_tx_route_cache, inherits = FALSE)) {
    return(get(key, envir = .asset_tx_route_cache, inherits = FALSE))
  }
  seg <- .asset_tx_segment(x1, y1, x2, y2)
  straight <- .asset_tx_chevron(seg)
  dx <- x2 - x1
  dy <- y2 - y1
  len <- sqrt(dx * dx + dy * dy)
  result <- straight
  if (is.finite(len) && len > 1e-4) {
    ux <- dx / len
    uy <- dy / len
    px <- -uy
    py <- ux
    base_hits <- .asset_tx_poly_hits(c(seg$x1, seg$x2), c(seg$y1, seg$y2), nodes, id_from, id_to)
    best_hits <- base_hits
    if (base_hits > 0L) {
      for (step in seq(0.35, 1.55, by = 0.12)) {
        for (sign in c(1, -1)) {
          sh <- sign * step
          # Soft control polygon: pull the mid handles toward the ends so the
          # densified spline bends instead of drawing a hard parallelogram.
          xs <- c(
            seg$x1,
            seg$x1 + (seg$x2 - seg$x1) * 0.28 + px * sh,
            seg$x1 + (seg$x2 - seg$x1) * 0.72 + px * sh,
            seg$x2
          )
          ys <- c(
            seg$y1,
            seg$y1 + (seg$y2 - seg$y1) * 0.28 + py * sh,
            seg$y1 + (seg$y2 - seg$y1) * 0.72 + py * sh,
            seg$y2
          )
          hits <- .asset_tx_poly_hits(xs, ys, nodes, id_from, id_to)
          if (hits < best_hits) {
            best_hits <- hits
            result <- .asset_tx_arrow(xs, ys, head = 0.11)
            if (best_hits == 0L) break
          }
        }
        if (best_hits == 0L) break
      }
    }
  }
  assign(key, result, envir = .asset_tx_route_cache)
  result
}

.asset_tx_edge_hover <- function(snap, i, locale) {
  e <- snap$edges[i, , drop = FALSE]
  nodes <- snap$catalog$nodes
  lab <- function(id) .asset_tx_label(nodes$label_key[nodes$id == id][1], locale)
  paste(
    sprintf("<b>%s → %s</b>", lab(e$from), lab(e$to)),
    .asset_tx_label(e$channel_key, locale),
    paste0(
      .asset_tx_label("atx_hover_prior", locale), ": ",
      .asset_tx_label(.asset_tx_prior_key(e$prior), locale)
    ),
    paste0(.asset_tx_label("atx_hover_corr", locale), ": ", asset_tx_fmt_corr(e$corr)),
    paste0(
      .asset_tx_label("atx_hover_today", locale), ": ",
      .asset_tx_label(.asset_tx_today_key(e$today), locale)
    ),
    sep = "<br>"
  )
}

.asset_tx_node_hover <- function(snap, i, locale) {
  row <- snap$nodes[i, , drop = FALSE]
  meta <- snap$catalog$nodes[snap$catalog$nodes$id == row$id, , drop = FALSE]
  sym_attr <- attr(snap, "symbols")
  sym <- .asset_tx_symbol_label(meta, sym_attr, row$id)
  paste(
    sprintf("<b>%s</b> (%s)", .asset_tx_label(meta$label_key[1], locale), sym),
    .asset_tx_label(meta$gloss_key[1], locale),
    paste0(.asset_tx_label("atx_hover_last", locale), ": ", asset_tx_fmt_level(row$level, meta$kind[1])),
    paste0(.asset_tx_label("atx_hover_d1", locale), ": ", asset_tx_fmt_shock(row$shock_1d, meta$kind[1])),
    paste0(.asset_tx_label("atx_hover_d5", locale), ": ", asset_tx_fmt_shock(row$shock_5d, meta$kind[1])),
    sep = "<br>"
  )
}

.asset_tx_dot_trace <- function(nodes) {
  # Intentionally empty: dotted canvas hurt readability. Kept as a named no-op
  # so older call sites / tests that look for the helper still resolve.
  invisible(NULL)
}

.asset_tx_card_border_trace <- function(snap, i, colors) {
  geom <- .asset_tx_card_geom()
  nodes <- snap$catalog$nodes
  id <- snap$nodes$id[i]
  meta <- nodes[nodes$id == id, , drop = FALSE]
  poly <- .asset_tx_round_poly(meta$x[1], meta$y[1], geom$w, geom$h, geom$r)
  list(
    x = poly$x,
    y = poly$y,
    type = "scatter",
    mode = "lines",
    fill = "none",
    line = list(color = asset_tx_z_color(snap$nodes$z[i], colors), width = 2.2, shape = "linear"),
    hoverinfo = "none",
    text = "",
    showlegend = FALSE,
    inherit = FALSE,
    name = paste0("card_", id)
  )
}

.asset_tx_traces <- function(snap, locale, market_mode) {
  colors <- asset_tx_move_colors(market_mode)
  nodes <- snap$catalog$nodes
  edges <- snap$edges
  # Solid black canvas only — no dot grid behind the cards.
  traces <- list()
  for (i in seq_len(nrow(snap$nodes))) {
    traces[[length(traces) + 1L]] <- .asset_tx_card_border_trace(snap, i, colors)
  }
  for (i in seq_len(nrow(edges))) {
    a <- nodes[nodes$id == edges$from[i], , drop = FALSE]
    b <- nodes[nodes$id == edges$to[i], , drop = FALSE]
    ch <- .asset_tx_routed_arrow(a$x, a$y, b$x, b$y, nodes, edges$from[i], edges$to[i])
    # Thin shafts: weak links stay hairline; strong corr only gently thickens.
    width <- if (is.finite(edges$corr[i])) 1.05 + 1.85 * abs(edges$corr[i]) else 1.0
    col <- asset_tx_edge_color(edges$state[i])
    dash <- if (edges$state[i] %in% c("weak", "na")) "dot" else "solid"
    traces[[length(traces) + 1L]] <- list(
      x = ch$x,
      y = ch$y,
      type = "scatter",
      mode = "lines",
      line = list(color = col, width = width, dash = dash),
      hoverinfo = "text",
      hovertext = .asset_tx_edge_hover(snap, i, locale),
      text = "",
      showlegend = FALSE,
      inherit = FALSE,
      name = sprintf("ch%02d", i)
    )
  }
  n_i <- seq_len(nrow(snap$nodes))
  xy <- nodes[match(snap$nodes$id, nodes$id), , drop = FALSE]
  move_cols <- vapply(
    n_i,
    function(i) asset_tx_z_color(snap$nodes$z[i], colors),
    character(1)
  )
  labels <- vapply(n_i, function(i) {
    meta <- nodes[nodes$id == snap$nodes$id[i], , drop = FALSE]
    .asset_tx_label(meta$label_key[1], locale)
  }, character(1))
  # Name stays light; level + 1D change share the same move color as the border.
  values <- vapply(n_i, function(i) {
    meta <- nodes[nodes$id == snap$nodes$id[i], , drop = FALSE]
    kind <- meta$kind[1]
    paste0(
      asset_tx_fmt_level(snap$nodes$level[i], kind), "  ",
      asset_tx_fmt_shock(snap$nodes$shock_1d[i], kind)
    )
  }, character(1))
  # Combined label kept for tests / hover-adjacent scrape of the nodes trace text.
  combined <- paste0(labels, "<br>", values)
  # Extra vertical gap between name and prints (= clearer line spacing inside the card).
  traces[[length(traces) + 1L]] <- list(
    x = xy$x,
    y = xy$y + 0.14,
    type = "scatter",
    mode = "text",
    text = labels,
    textposition = "middle center",
    textfont = list(
      size = 12.5,
      color = "#ffffff",
      family = "Arial, Helvetica, sans-serif"
    ),
    hoverinfo = "skip",
    showlegend = FALSE,
    inherit = FALSE,
    name = "node_names",
    cliponaxis = FALSE
  )
  traces[[length(traces) + 1L]] <- list(
    x = xy$x,
    y = xy$y - 0.16,
    type = "scatter",
    mode = "markers+text",
    marker = list(
      size = 36,
      color = "rgba(255,255,255,0)",
      opacity = 0,
      line = list(width = 0)
    ),
    text = values,
    textposition = "middle center",
    textfont = list(
      size = 12,
      color = move_cols,
      family = "Arial, Helvetica, sans-serif"
    ),
    # Keep the classic three-line scrape string on this trace for offline tests.
    customdata = combined,
    hoverinfo = "text",
    hovertext = vapply(n_i, function(i) .asset_tx_node_hover(snap, i, locale), character(1)),
    cliponaxis = FALSE,
    showlegend = FALSE,
    inherit = FALSE,
    name = "nodes"
  )
  traces
}

.asset_tx_band_shapes <- function(nodes) {
  spec <- list(
    driver = list(fill = "rgba(28, 42, 82, 0.28)"),
    rate = list(fill = "rgba(36, 56, 108, 0.22)"),
    fx = list(fill = "rgba(24, 44, 92, 0.22)"),
    liq = list(fill = "rgba(32, 48, 96, 0.20)"),
    asset = list(fill = "rgba(18, 32, 68, 0.18)")
  )
  shapes <- list()
  for (band in names(spec)) {
    sub <- nodes[nodes$band == band, , drop = FALSE]
    if (!nrow(sub)) next
    pad_x <- if (band == "asset") 0.85 else 0.95
    pad_y <- if (band == "asset") 0.55 else 0.48
    shapes[[length(shapes) + 1L]] <- list(
      type = "rect",
      xref = "x",
      yref = "y",
      x0 = min(sub$x) - pad_x,
      x1 = max(sub$x) + pad_x,
      y0 = min(sub$y) - pad_y,
      y1 = max(sub$y) + if (band == "fx") 0.42 else pad_y,
      fillcolor = spec[[band]]$fill,
      line = list(color = "rgba(244, 246, 251, 0.05)", width = 1),
      layer = "below"
    )
  }
  shapes
}

# Fill is static by band. The move-colored stroke is a per-frame line trace,
# so Play can update the border; this shape only paints the card body.
.asset_tx_card_shapes <- function(snap) {
  geom <- .asset_tx_card_geom()
  nodes <- snap$catalog$nodes
  lapply(seq_len(nrow(snap$nodes)), function(i) {
    meta <- nodes[nodes$id == snap$nodes$id[i], , drop = FALSE]
    list(
      type = "path",
      xref = "x",
      yref = "y",
      path = .asset_tx_round_rect(meta$x[1], meta$y[1], geom$w, geom$h, geom$r),
      fillcolor = .asset_tx_band_fill(meta$band[1]),
      line = list(color = "rgba(0,0,0,0)", width = 0),
      layer = "below"
    )
  })
}

.asset_tx_headers <- function(locale) {
  heads <- data.frame(
    x = c(0.60, 3.25, 6.00, 8.70, 0.15),
    y = c(10.25, 10.25, 10.25, 10.25, 3.45),
    xanchor = c("center", "center", "center", "center", "left"),
    key = c(
      "atx_head_driver", "atx_head_rate", "atx_head_fxhub",
      "atx_head_liq", "atx_head_asset"
    ),
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(heads)), function(i) {
    list(
      x = heads$x[i],
      y = heads$y[i],
      xanchor = heads$xanchor[i],
      text = .asset_tx_label(heads$key[i], locale),
      showarrow = FALSE,
      xref = "x",
      yref = "y",
      font = list(size = 14, color = "#ffffff", family = "Arial, Helvetica, sans-serif")
    )
  })
}

# Latest-session co-movement label. Surveillance only — not a forecast and not WACC.
# Thresholds match the yield-path card: yields under 0.5 bp and prices under 0.05% are noise.
# inflationary_expansion is checked before expansion so a dollar-up, growth-down tape
# is not labeled a clean expansion.
asset_tx_regime <- function(snap, locale = "en") {
  loc <- locale
  pack <- function(id) {
    list(
      id = id,
      title = .asset_tx_label(paste0("atx_regime_", id), loc),
      body = .asset_tx_label(paste0("atx_regime_", id, "_body"), loc),
      note = .asset_tx_label("atx_regime_note", loc)
    )
  }
  if (is.null(snap)) return(pack("quiet"))
  watch <- c(
    "oil", "copper", "gold", "spx", "nasdaq", "btc", "us10y", "dxy",
    "hyg", "infl", "tlt", "us2y", "us_bill"
  )
  dirs <- stats::setNames(lapply(watch, function(id) .asset_tx_dir(snap, id)), watch)
  active <- sum(vapply(dirs, function(d) d %in% c("up", "down"), logical(1)))
  if (active < 3L) return(pack("quiet"))
  up <- function(id) identical(dirs[[id]], "up")
  down <- function(id) identical(dirs[[id]], "down")
  not_up <- function(id) dirs[[id]] %in% c("down", "flat")
  # Prefer the 2Y futures node when it actually printed; otherwise the T-bill.
  short_down <- if (!identical(dirs[["us2y"]], "na")) down("us2y") else down("us_bill")
  growth_down <- down("nasdaq") || down("btc")
  soft_bids <- sum(c(up("tlt"), up("gold"), up("nasdaq"), up("btc")))
  infl_exp <- up("oil") && up("copper") && up("us10y") && up("dxy") && growth_down
  stag <- up("oil") && up("gold") && not_up("copper") && down("spx")
  expansion <- up("copper") && up("oil") && up("spx") && up("us10y")
  soft <- down("infl") && short_down && soft_bids >= 2L
  risk <- down("hyg") && down("spx") && (down("oil") || down("copper")) && (up("dxy") || up("tlt"))
  id <- if (infl_exp) {
    "inflationary_expansion"
  } else if (stag) {
    "stagflation"
  } else if (expansion) {
    "expansion"
  } else if (soft) {
    "soft_landing"
  } else if (risk) {
    "risk_off"
  } else {
    "mixed"
  }
  pack(id)
}

asset_tx_yield_path <- function(snap, locale = "en") {
  quiet <- list(bias = "quiet", title = .asset_tx_label("atx_path_quiet", locale), rows = data.frame())
  if (is.null(snap)) return(quiet)
  hub <- snap$nodes[snap$nodes$id == "us10y", , drop = FALSE]
  if (!nrow(hub) || !is.finite(hub$shock_1d[1])) return(quiet)
  shock <- hub$shock_1d[1]
  bias <- if (shock >= 0.005) "up" else if (shock <= -0.005) "down" else "quiet"
  title <- .ynow_fmt_str(
    .asset_tx_label(
      if (bias == "up") "atx_path_up" else if (bias == "down") "atx_path_down" else "atx_path_quiet",
      locale
    ),
    chg = asset_tx_fmt_shock(shock, "yield")
  )
  if (bias == "quiet") {
    return(list(bias = "quiet", title = title, rows = data.frame()))
  }
  # Textbook lean beside the actual print. Curve and TIPS rows state the real-rate point
  # without inventing a cash real-yield level.
  lean_up <- c(
    curve = "atx_lean_curve_up",
    dxy = "atx_lean_dxy_up", gold = "atx_lean_gold_up", tip = "atx_lean_tip_up",
    spx = "atx_lean_eq_up",
    nasdaq = "atx_lean_eq_up", tlt = "atx_lean_bond_up", hyg = "atx_lean_credit_up",
    btc = "atx_lean_crypto_up", taiex = "atx_lean_tw_up", vnq = "atx_lean_reit_up"
  )
  lean_down <- c(
    curve = "atx_lean_curve_down",
    dxy = "atx_lean_dxy_down", gold = "atx_lean_gold_down", tip = "atx_lean_tip_down",
    spx = "atx_lean_eq_down",
    nasdaq = "atx_lean_eq_down", tlt = "atx_lean_bond_down", hyg = "atx_lean_credit_down",
    btc = "atx_lean_crypto_down", taiex = "atx_lean_tw_down", vnq = "atx_lean_reit_down"
  )
  leans <- if (bias == "down") lean_down else lean_up
  rows <- lapply(names(leans), function(id) {
    meta <- snap$catalog$nodes[snap$catalog$nodes$id == id, , drop = FALSE]
    row <- snap$nodes[snap$nodes$id == id, , drop = FALSE]
    data.frame(
      asset = .asset_tx_label(paste0("atx_node_", id), locale),
      lean = .asset_tx_label(unname(leans[[id]]), locale),
      actual = if (nrow(row)) asset_tx_fmt_shock(row$shock_1d[1], if (nrow(meta)) meta$kind[1] else "index") else "—",
      stringsAsFactors = FALSE
    )
  })
  list(
    bias = bias,
    title = title,
    rows = do.call(rbind, rows)
  )
}

.asset_tx_show_latest <- function(p) {
  nfr <- length(p$x$frames)
  if (nfr < 2L) return(p)
  # Open on the newest session. Play restarts at the first session on the tape.
  p$x$data <- p$x$frames[[nfr]]$data
  if (length(p$x$layout$sliders)) {
    p$x$layout$sliders[[1]]$active <- nfr - 1L
  }
  menus <- p$x$layout$updatemenus
  if (length(menus)) {
    for (i in seq_along(menus)) {
      btns <- menus[[i]]$buttons
      if (!length(btns) || !identical(btns[[1]]$method, "animate")) next
      if (length(btns[[1]]$args) >= 2L && is.list(btns[[1]]$args[[2]])) {
        p$x$layout$updatemenus[[i]]$buttons[[1]]$args[[2]]$fromcurrent <- FALSE
      }
    }
  }
  p
}

# Name traces and drop helpers that must not ship in frame payloads.
.asset_tx_named_traces <- function(snap, locale, market_mode) {
  traces <- .asset_tx_traces(snap, locale, market_mode)
  lapply(seq_along(traces), function(i) {
    tr <- traces[[i]]
    if (is.null(tr$name) || !nzchar(as.character(tr$name)[1])) {
      tr$name <- if (i == length(traces)) "nodes" else sprintf("ch%02d", i)
    }
    tr$inherit <- NULL
    tr
  })
}

# Attach Play frames without plotly's frame= regroup (that path is ~15s for
# 22 sessions × ~84 traces). Build the latest session as the base plot, then
# hang precomputed frame payloads + slider / Play button on the built object.
.asset_tx_attach_play_frames <- function(p, snaps, locale, market_mode) {
  names_fr <- vapply(snaps, function(sn) format(sn$as_of, "%Y-%m-%d"), character(1))
  trace_lists <- lapply(snaps, function(sn) .asset_tx_named_traces(sn, locale, market_mode))
  n_tr <- length(trace_lists[[1]])
  if (!n_tr) return(p)
  idx <- as.list(as.integer(seq_len(n_tr) - 1L))
  frames <- lapply(seq_along(snaps), function(i) {
    list(name = names_fr[[i]], data = trace_lists[[i]], traces = idx)
  })
  anim_args <- list(
    mode = "immediate",
    transition = list(duration = 180, easing = "cubic-in-out"),
    frame = list(duration = 420, redraw = FALSE)
  )
  steps <- lapply(names_fr, function(nm) {
    list(
      method = "animate",
      args = list(list(nm), anim_args),
      label = nm,
      value = nm
    )
  })
  p <- plotly::plotly_build(p)
  p$x$frames <- frames
  # Room for the session slider + Play under the map (tight b-margin clips them).
  p$x$layout$margin <- list(l = 10, r = 10, t = 12, b = 100)
  # Full-width timeline inside the paper; Play sits left-inside so it never
  # falls outside x=0 (xanchor=right) or gets scrolled off-screen.
  p$x$layout$sliders <- list(list(
    active = length(names_fr) - 1L,
    steps = steps,
    x = 0.10,
    len = 0.88,
    y = 0,
    pad = list(t = 48, b = 12),
    currentvalue = list(
      prefix = .asset_tx_label("atx_session_prefix", locale),
      font = list(color = "#f4f6fb", size = 13)
    ),
    bgcolor = "#12182e",
    bordercolor = "#3a4668",
    tickcolor = "#f4f6fb",
    font = list(color = "#d5dced", size = 10)
  ))
  p$x$layout$updatemenus <- list(structure(
    list(
      type = "buttons",
      direction = "right",
      showactive = FALSE,
      y = 0,
      x = 0,
      yanchor = "top",
      xanchor = "left",
      pad = list(t = 70, l = 8),
      bgcolor = "#3a6fe0",
      font = list(color = "#ffffff", size = 12),
      buttons = list(list(
        label = .asset_tx_label("atx_play", locale),
        method = "animate",
        args = list(
          NULL,
          modifyList(anim_args, list(fromcurrent = FALSE), keep.null = TRUE)
        )
      ))
    ),
    class = "aniButton"
  ))
  p
}

.asset_tx_fig_cache_key <- function(snaps, locale, market_mode) {
  if (!length(snaps)) return("")
  last <- snaps[[length(snaps)]]
  fa <- attr(last, "fetched_at")
  stamp <- if (!is.null(fa)) {
    format(fa, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  } else {
    paste(vapply(snaps, function(s) format(s$as_of, "%Y-%m-%d"), character(1)), collapse = "|")
  }
  paste(
    stamp,
    length(snaps),
    last$window %||% "",
    as.character(locale %||% "en")[1],
    toupper(as.character(market_mode %||% "US")[1]),
    sep = "::"
  )
}

.asset_tx_fig_cache_get <- function(key) {
  if (!nzchar(key) || !exists(key, envir = .asset_tx_fig_cache, inherits = FALSE)) return(NULL)
  get(key, envir = .asset_tx_fig_cache, inherits = FALSE)
}

.asset_tx_fig_cache_put <- function(key, value) {
  if (!nzchar(key) || is.null(value)) return(invisible(NULL))
  keys <- ls(envir = .asset_tx_fig_cache, all.names = TRUE)
  if (length(keys) >= .asset_tx_fig_cache_max) {
    # Drop oldest entries by insertion order (env listing order is fine enough).
    drop_n <- max(1L, length(keys) - .asset_tx_fig_cache_max + 1L)
    rm(list = keys[seq_len(drop_n)], envir = .asset_tx_fig_cache)
  }
  assign(key, value, envir = .asset_tx_fig_cache)
  invisible(value)
}

asset_tx_figure <- function(snaps, locale = "en", market_mode = "US") {
  snaps <- snaps[!vapply(snaps, is.null, logical(1))]
  if (!length(snaps)) return(NULL)
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  locale <- if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(locale) else locale
  cache_key <- .asset_tx_fig_cache_key(snaps, locale, market_mode)
  cached <- .asset_tx_fig_cache_get(cache_key)
  if (!is.null(cached)) return(cached)
  last <- snaps[[length(snaps)]]
  multi <- length(snaps) >= 2L
  # Rebuild arrow routes once per figure, not once per frame.
  rm(list = ls(envir = .asset_tx_route_cache, all.names = TRUE), envir = .asset_tx_route_cache)
  # Base plot = latest session only. Play frames are attached afterward so we
  # never pay plotly's O(frames × traces) frame= regroup (~15s at n=22).
  p <- plotly::plot_ly()
  for (tr in .asset_tx_named_traces(last, locale, market_mode)) {
    args <- tr
    args$p <- p
    args$inherit <- FALSE
    p <- do.call(plotly::add_trace, args)
  }
  nodes_xy <- last$catalog$nodes
  date_note <- format(last$as_of, "%Y-%m-%d")
  # Equal pads so the graph sits centered on the black paper (L/R and T/B).
  x_lo <- min(nodes_xy$x, na.rm = TRUE)
  x_hi <- max(nodes_xy$x, na.rm = TRUE)
  y_lo <- min(nodes_xy$y, na.rm = TRUE)
  y_hi <- max(nodes_xy$y, na.rm = TRUE)
  x_pad <- 1.60
  y_pad <- 2.05
  x_c <- (x_lo + x_hi) / 2
  y_c <- (y_lo + y_hi) / 2
  x_half <- (x_hi - x_lo) / 2 + x_pad
  y_half <- (y_hi - y_lo) / 2 + y_pad
  range_x <- c(x_c - x_half, x_c + x_half)
  range_y <- c(y_c - y_half, y_c + y_half)
  # #region agent log
  tryCatch({
    line <- jsonlite::toJSON(list(
      sessionId = "f77c57",
      runId = "center-pre",
      hypothesisId = "H1-axis",
      location = "asset_transmission_module.R:.asset_tx_build_figure",
      message = "atx axis ranges centered",
      data = list(
        x_lo = x_lo, x_hi = x_hi, y_lo = y_lo, y_hi = y_hi,
        x_c = x_c, y_c = y_c, x_pad = x_pad, y_pad = y_pad,
        range_x0 = range_x[[1]], range_x1 = range_x[[2]],
        range_y0 = range_y[[1]], range_y1 = range_y[[2]],
        pad_left = x_lo - range_x[[1]],
        pad_right = range_x[[2]] - x_hi,
        pad_bottom = y_lo - range_y[[1]],
        pad_top = range_y[[2]] - y_hi,
        equal_x = isTRUE(all.equal(x_lo - range_x[[1]], range_x[[2]] - x_hi, tolerance = 1e-9)),
        equal_y = isTRUE(all.equal(y_lo - range_y[[1]], range_y[[2]] - y_hi, tolerance = 1e-9))
      ),
      timestamp = as.numeric(Sys.time()) * 1000
    ), auto_unbox = TRUE)
    cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
  }, error = function(e) invisible(NULL))
  # #endregion
  anns <- c(
    .asset_tx_headers(locale),
    list(list(
      x = x_lo - 0.4,
      y = y_lo - 1.75,
      text = paste0("Yahoo Finance · ", date_note),
      showarrow = FALSE,
      xref = "x",
      yref = "y",
      xanchor = "left",
      font = list(size = 11, color = "#c5cce0", family = "Arial, Helvetica, sans-serif")
    ))
  )
  p <- plotly::layout(
    p,
    autosize = TRUE,
    # Pan/zoom stay on so narrow viewports can explore without compressing cards.
    # Do NOT set scaleanchor/scaleratio: a 1:1 lock letterboxes the plot domain
    # inside the paper and leaves a large empty block on one side.
    dragmode = "pan",
    xaxis = list(
      visible = FALSE,
      range = range_x,
      fixedrange = FALSE,
      zeroline = FALSE,
      constrain = "domain"
    ),
    yaxis = list(
      visible = FALSE,
      range = range_y,
      fixedrange = FALSE,
      zeroline = FALSE,
      constrain = "domain"
    ),
    margin = list(l = 2, r = 2, t = 8, b = 2),
    paper_bgcolor = "#000000",
    plot_bgcolor = "#000000",
    hovermode = "closest",
    annotations = anns,
    shapes = c(.asset_tx_band_shapes(nodes_xy), .asset_tx_card_shapes(last)),
    showlegend = FALSE
  )
  if (multi) {
    p <- .asset_tx_attach_play_frames(p, snaps, locale, market_mode)
    p <- .asset_tx_show_latest(p)
  }
  out <- plotly::config(
    p,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE,
    doubleClick = "reset",
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
      "hoverCompareCartesian", "toggleSpikelines", "toImage"
    )
  )
  .asset_tx_fig_cache_put(cache_key, out)
  out
}

# ---- Fetch -----------------------------------------------------------------

.asset_tx_yahoo_range <- function(period) {
  switch(as.character(period %||% "6mo")[1], "1y" = "1y", "2y" = "2y", "5y" = "5y", "6mo")
}

# Process-level panel cache. The Testing tab polls every 90s; TTL must exceed
# that interval so auto-refresh reuses the panel instead of re-hitting Yahoo.
.asset_tx_panel_cache <- new.env(parent = emptyenv())
.asset_tx_panel_cache_ttl_sec <- 105
# Replay depth for Play (~1 trading month). Progressive paint: 1 frame, then full tape.
.asset_tx_tape_frames <- 22L
# Figure cache: same panel + window + locale + market should not rebuild Plotly.
.asset_tx_fig_cache <- new.env(parent = emptyenv())
.asset_tx_fig_cache_max <- 8L

.asset_tx_window_choice <- function(sel) {
  sel <- as.character(sel %||% "")
  sel <- if (length(sel) >= 1L) sel[[1]] else ""
  if (!nzchar(sel) || !sel %in% c("20", "60", "120")) "60" else sel
}

# Yahoo chart JSON. Prefer simplifyVector=TRUE payloads (faster parse); still
# accept the nested simplifyVector=FALSE shape used by older call sites.
.asset_tx_parse_chart <- function(j) {
  empty <- numeric()
  if (!is.list(j) || is.null(j$chart) || is.null(j$chart$result) || !length(j$chart$result)) {
    return(empty)
  }
  res <- j$chart$result[[1]]
  if (!is.list(res)) return(empty)
  ts <- res$timestamp
  quote <- res$indicators$quote
  if (is.null(ts) || !length(ts) || is.null(quote) || !length(quote)) return(empty)
  close <- quote[[1]]$close
  if (is.null(close)) return(empty)
  ts <- suppressWarnings(as.numeric(unlist(ts, use.names = FALSE)))
  # Nested lists (simplifyVector=FALSE) vs atomic vectors (TRUE).
  if (is.list(close) && !is.data.frame(close)) {
    vals <- vapply(close, function(el) {
      if (is.null(el) || length(el) < 1L) return(NA_real_)
      out <- suppressWarnings(as.numeric(el[[1]]))
      if (length(out) != 1L || !is.finite(out)) NA_real_ else out
    }, numeric(1))
  } else {
    vals <- suppressWarnings(as.numeric(unlist(close, use.names = FALSE)))
  }
  n <- min(length(ts), length(vals))
  if (n < 1L) return(empty)
  ts <- ts[seq_len(n)]
  vals <- vals[seq_len(n)]
  tz <- "UTC"
  meta_tz <- res$meta$exchangeTimezoneName
  if (is.character(meta_tz) && length(meta_tz) >= 1L && nzchar(meta_tz[[1]])) tz <- meta_tz[[1]]
  keys <- tryCatch(
    format(as.POSIXct(ts, origin = "1970-01-01", tz = tz), "%Y-%m-%d"),
    error = function(e) format(as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
  )
  ok <- !is.na(keys) & nzchar(keys) & is.finite(vals)
  if (!any(ok)) return(empty)
  stats::setNames(vals[ok], keys[ok])
}

.asset_tx_chart_url <- function(sym, range = "6mo") {
  paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    utils::URLencode(as.character(sym)[1], reserved = TRUE),
    "?interval=1d&range=", range
  )
}

.asset_tx_parse_chart_text <- function(txt) {
  if (!nzchar(txt %||% "")) return(numeric())
  # IMPORTANT: simplifyVector=TRUE turns chart$result into a data.frame and
  # breaks .asset_tx_parse_chart (returns empty → "empty Yahoo history").
  # Always parse nested lists first; fall back only if that fails.
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  path <- "false"
  if (is.null(j)) {
    j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
    path <- "true_fallback"
  }
  if (is.null(j)) return(numeric())
  # Data.frame result from simplifyVector=TRUE is unusable for this parser.
  if (is.data.frame(j$chart$result)) {
    j2 <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(j2)) {
      j <- j2
      path <- "false_after_df"
    }
  }
  out <- .asset_tx_parse_chart(j)
  # #region agent log
  if (identical(Sys.getenv("YNOW_ATX_DEBUG"), "1") || file.exists("/Users/lawrencekuo/coding/theYNowApp/.cursor")) {
    tryCatch({
      line <- jsonlite::toJSON(list(
        sessionId = "f77c57",
        hypothesisId = "A",
        location = "asset_transmission_module.R:parse_chart_text",
        message = "yahoo chart parse",
        data = list(
          path = path,
          result_is_df = is.data.frame(j$chart$result),
          n_points = length(out),
          txt_bytes = nchar(txt)
        ),
        timestamp = as.numeric(Sys.time()) * 1000
      ), auto_unbox = TRUE)
      cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
    }, error = function(e) NULL)
  }
  # #endregion
  out
}

.asset_tx_chart_one <- function(sym, range = "6mo") {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(numeric())
  }
  resp <- tryCatch(
    httr::GET(.asset_tx_chart_url(sym, range), httr::user_agent("Mozilla/5.0"), httr::timeout(20)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) >= 400L) return(numeric())
  txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  .asset_tx_parse_chart_text(txt)
}

# Concurrent Yahoo chart pulls via libcurl. Safe on shinyapps (no forking).
.asset_tx_curl_download <- function(symbols, range = "6mo") {
  out <- vector("list", length(symbols))
  names(out) <- symbols
  if (!length(symbols)) return(out)
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }
  pool <- curl::new_pool()
  for (i in seq_along(symbols)) {
    local({
      sym <- symbols[[i]]
      h <- curl::new_handle()
      curl::handle_setopt(h, useragent = "Mozilla/5.0", timeout = 20, connecttimeout = 10)
      curl::curl_fetch_multi(
        .asset_tx_chart_url(sym, range),
        done = function(res) {
          out[[sym]] <<- tryCatch({
            if (is.null(res) || isTRUE(res$status_code >= 400L)) return(numeric())
            txt <- rawToChar(res$content)
            Encoding(txt) <- "UTF-8"
            .asset_tx_parse_chart_text(txt)
          }, error = function(e) numeric())
        },
        fail = function(err) {
          out[[sym]] <<- numeric()
        },
        pool = pool,
        handle = h
      )
    })
  }
  curl::multi_run(pool = pool)
  # #region agent log
  tryCatch({
    n_ok <- sum(vapply(out, function(x) length(x) > 0, logical(1)))
    line <- jsonlite::toJSON(list(
      sessionId = "f77c57",
      hypothesisId = "B",
      location = "asset_transmission_module.R:curl_download",
      message = "curl multi done",
      data = list(n_sym = length(symbols), n_ok = n_ok),
      timestamp = as.numeric(Sys.time()) * 1000
    ), auto_unbox = TRUE)
    cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
  }, error = function(e) NULL)
  # #endregion
  out
}

.asset_tx_r_download <- function(symbols, period = "6mo") {
  symbols <- unique(as.character(symbols))
  symbols <- symbols[nzchar(symbols)]
  if (!length(symbols)) stop("empty Yahoo history")
  rng <- .asset_tx_yahoo_range(period)
  series <- .asset_tx_curl_download(symbols, rng)
  if (is.null(series)) {
    series <- lapply(symbols, function(sym) .asset_tx_chart_one(sym, rng))
    names(series) <- symbols
  }
  dates <- sort(unique(unlist(lapply(series, names), use.names = FALSE)))
  if (!length(dates)) stop("empty Yahoo history")
  values <- lapply(symbols, function(sym) {
    m <- series[[sym]]
    v <- rep(NA_real_, length(dates))
    if (is.null(m) || !length(m)) return(v)
    hit <- match(names(m), dates)
    ok <- !is.na(hit)
    v[hit[ok]] <- as.numeric(unname(m[ok]))
    v
  })
  list(Date = dates, symbols = symbols, values = values)
}

.asset_tx_py_download <- function(symbols, period = "6mo") {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("reticulate missing")
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) stop("Python skipped")
  if (exists(".ynow_ensure_python", mode = "function")) {
    if (!isTRUE(.ynow_ensure_python())) stop("Python unavailable")
  }
  reticulate::py_run_string("
def _ynow_atx_download(tickers, period):
    import math
    from datetime import datetime, timezone
    import requests
    tickers = [str(t) for t in list(tickers)]
    rng = {'6mo': '6mo', '1y': '1y', '2y': '2y', '5y': '5y'}.get(str(period), '1y')
    headers = {'User-Agent': 'Mozilla/5.0'}
    try:
        from zoneinfo import ZoneInfo
    except Exception:
        ZoneInfo = None

    def _chart(sym):
        url = (
            'https://query1.finance.yahoo.com/v8/finance/chart/'
            + requests.utils.quote(sym, safe='')
            + '?interval=1d&range=' + rng
        )
        r = requests.get(url, headers=headers, timeout=20)
        j = r.json()
        res = ((j.get('chart') or {}).get('result') or [None])[0]
        if not res:
            return {}
        ts = res.get('timestamp') or []
        quote = (((res.get('indicators') or {}).get('quote') or [{}])[0]).get('close') or []
        tzname = ((res.get('meta') or {}).get('exchangeTimezoneName')) or 'UTC'
        tz = timezone.utc
        if ZoneInfo is not None:
            try:
                tz = ZoneInfo(tzname)
            except Exception:
                tz = timezone.utc
        dmap = {}
        for t, v in zip(ts, quote):
            if v is None or t is None:
                continue
            try:
                fv = float(v)
            except Exception:
                continue
            if math.isnan(fv):
                continue
            key = datetime.fromtimestamp(int(t), tz).strftime('%Y-%m-%d')
            dmap[key] = fv
        return dmap

    def _yfinance_one(sym):
        try:
            import yfinance as yf
            import pandas as pd
            h = yf.Ticker(sym).history(period=rng, auto_adjust=True)
        except Exception:
            return {}
        if h is None or len(h) == 0 or 'Close' not in getattr(h, 'columns', []):
            return {}
        dmap = {}
        for idx, val in h['Close'].items():
            try:
                key = pd.Timestamp(idx).strftime('%Y-%m-%d')
                fv = float(val)
            except Exception:
                continue
            if math.isnan(fv):
                continue
            dmap[key] = fv
        return dmap

    series = {}
    for sym in tickers:
        dmap = {}
        try:
            dmap = _chart(sym)
        except Exception:
            dmap = {}
        if len(dmap) < 30:
            ymap = _yfinance_one(sym)
            if len(ymap) > len(dmap):
                dmap = ymap
        series[sym] = dmap
    dates = sorted({d for m in series.values() for d in m.keys()})
    values = []
    for sym in tickers:
        m = series.get(sym) or {}
        values.append([m.get(d) for d in dates])
    return {'Date': dates, 'symbols': tickers, 'values': values}
")
  reticulate::py$`_ynow_atx_download`(as.list(symbols), period)
}

.asset_tx_series_map <- function(raw) {
  if (is.null(raw)) return(list(dates = as.Date(character()), series = list()))
  dates <- as.Date(as.character(unlist(raw$Date, use.names = FALSE)))
  symbols <- as.character(unlist(raw$symbols, use.names = FALSE))
  values <- raw$values
  series <- list()
  if (is.matrix(values) || is.data.frame(values)) {
    for (i in seq_along(symbols)) {
      series[[symbols[i]]] <- .asset_tx_num_vec(values[, i], length(dates))
    }
  } else if (is.list(values)) {
    for (i in seq_along(symbols)) {
      series[[symbols[i]]] <- .asset_tx_num_vec(values[[i]], length(dates))
    }
  }
  keep <- !is.na(dates)
  dates <- dates[keep]
  series <- lapply(series, function(v) {
    if (length(v) == length(keep)) v[keep] else v
  })
  list(dates = dates, series = series)
}

.asset_tx_align <- function(dates, series, nodes) {
  if (!length(dates)) return(NULL)
  out <- data.frame(Date = dates, stringsAsFactors = FALSE)
  used <- stats::setNames(rep("", nrow(nodes)), nodes$id)
  for (i in seq_len(nrow(nodes))) {
    id <- nodes$id[i]
    sym <- nodes$yahoo[i]
    v <- series[[sym]]
    if (is.null(v) || !any(is.finite(v))) {
      # Yahoo sometimes returns the symbol without the caret.
      alt_name <- sub("^\\^", "", sym)
      if (!is.null(series[[alt_name]]) && any(is.finite(series[[alt_name]]))) {
        v <- series[[alt_name]]
        sym <- alt_name
      }
    }
    if (is.null(v)) v <- rep(NA_real_, length(dates))
    if (length(v) != length(dates)) {
      v2 <- rep(NA_real_, length(dates))
      n <- min(length(v), length(dates))
      if (n > 0L) v2[seq_len(n)] <- v[seq_len(n)]
      v <- v2
    }
    out[[id]] <- v
    if (any(is.finite(v))) used[[id]] <- sym
  }
  attr(out, "symbols") <- used
  out
}

.asset_tx_panel_cache_key <- function(syms, period) {
  paste0(as.character(period %||% "6mo")[1], "::", paste(sort(unique(as.character(syms))), collapse = "|"))
}

.asset_tx_panel_cache_get <- function(key) {
  if (!exists(key, envir = .asset_tx_panel_cache, inherits = FALSE)) return(NULL)
  hit <- get(key, envir = .asset_tx_panel_cache, inherits = FALSE)
  if (!is.list(hit) || is.null(hit$fetched_at)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), hit$fetched_at, units = "secs"))
  if (!is.finite(age) || age > .asset_tx_panel_cache_ttl_sec) return(NULL)
  hit
}

.asset_tx_panel_cache_put <- function(key, value) {
  assign(key, value, envir = .asset_tx_panel_cache)
  invisible(value)
}

.asset_tx_on_shinyapps <- function() {
  if (exists("on_shinyapps", inherits = TRUE)) {
    v <- get("on_shinyapps", inherits = TRUE)
    if (isTRUE(v)) return(TRUE)
  }
  if (exists(".ynow_is_hosted_connect", mode = "function", inherits = TRUE)) {
    return(isTRUE(.ynow_is_hosted_connect()))
  }
  wd <- tryCatch(normalizePath(getwd(), mustWork = FALSE), error = function(e) getwd())
  nzchar(Sys.getenv("SHINY_SERVER_VERSION")) ||
    grepl("shinyapps", Sys.getenv("HOSTNAME"), ignore.case = TRUE) ||
    grepl("shinyapps", Sys.getenv("R_CONFIG_ACTIVE"), ignore.case = TRUE) ||
    identical(Sys.getenv("FORCE_SHINYAPPS_PYTHON"), "1") ||
    grepl("/srv/connect/apps", wd, fixed = TRUE) ||
    (identical(Sys.getenv("USER"), "shiny") && dir.exists("/srv/connect"))
}

asset_tx_fetch_panel <- function(catalog = NULL, period = "6mo", force = FALSE) {
  catalog <- catalog %||% asset_tx_catalog()
  nodes <- catalog$nodes
  syms <- unique(nodes$yahoo)
  syms <- syms[nzchar(syms)]
  cache_key <- .asset_tx_panel_cache_key(syms, period)
  if (!isTRUE(force)) {
    cached <- .asset_tx_panel_cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }
  # R chart API first. shinyapps.io has reticulate but no Python, so the
  # Python downloader cannot be the only path — and on shinyapps we skip it
  # entirely so a failed Python install cannot wedge the Testing map.
  raw <- tryCatch(.asset_tx_r_download(syms, period), error = function(e) NULL)
  n_dates <- if (is.null(raw) || is.null(raw$Date)) 0L else length(raw$Date)
  if (n_dates < 30L && !isTRUE(.asset_tx_on_shinyapps())) {
    py <- tryCatch(.asset_tx_py_download(syms, period), error = function(e) NULL)
    if (!is.null(py) && length(py$Date) > n_dates) raw <- py
  }
  if (is.null(raw)) stop("empty Yahoo history")
  parsed <- .asset_tx_series_map(raw)
  panel <- .asset_tx_align(parsed$dates, parsed$series, nodes)
  if (is.null(panel)) stop("empty Yahoo history")
  used <- attr(panel, "symbols")
  # Collect short series once, then pull all alts in one concurrent curl pool
  # instead of one Yahoo round-trip per node.
  need_alt <- character(0)
  need_ids <- character(0)
  for (i in seq_len(nrow(nodes))) {
    alt <- nodes$yahoo_alt[i]
    id <- nodes$id[i]
    if (!nzchar(alt)) next
    n_fin <- sum(is.finite(panel[[id]]))
    if (n_fin >= 30L) next
    need_alt <- c(need_alt, alt)
    need_ids <- c(need_ids, id)
  }
  if (length(need_alt)) {
    uniq_alt <- unique(need_alt)
    alt_raw <- tryCatch(.asset_tx_r_download(uniq_alt, period), error = function(e) NULL)
    alt_n <- if (is.null(alt_raw) || is.null(alt_raw$Date)) 0L else length(alt_raw$Date)
    if (alt_n < 30L && !isTRUE(.asset_tx_on_shinyapps())) {
      py_alt <- tryCatch(.asset_tx_py_download(uniq_alt, period), error = function(e) NULL)
      if (!is.null(py_alt) && length(py_alt$Date) > alt_n) alt_raw <- py_alt
    }
    alt_parsed <- .asset_tx_series_map(alt_raw)
    if (length(alt_parsed$dates)) {
      for (j in seq_along(need_ids)) {
        id <- need_ids[[j]]
        alt <- need_alt[[j]]
        n_fin <- sum(is.finite(panel[[id]]))
        v <- alt_parsed$series[[alt]]
        if (is.null(v) || !any(is.finite(v))) next
        extra <- data.frame(Date = alt_parsed$dates, v = as.numeric(v), stringsAsFactors = FALSE)
        all_dates <- sort(unique(c(panel$Date, extra$Date)))
        rebuilt <- data.frame(Date = all_dates, stringsAsFactors = FALSE)
        for (col in setdiff(names(panel), "Date")) {
          rebuilt[[col]] <- panel[[col]][match(all_dates, panel$Date)]
        }
        rebuilt[[id]] <- extra$v[match(all_dates, extra$Date)]
        if (sum(is.finite(rebuilt[[id]])) > n_fin) {
          panel <- rebuilt
          used[[id]] <- alt
        }
      }
    }
  }
  panel <- .asset_tx_apply_derived(panel, nodes)
  if ("curve" %in% names(panel) && any(is.finite(panel$curve))) {
    used[["curve"]] <- "10Y\u22122Y fut"
  }
  attr(panel, "symbols") <- used
  n_ok <- sum(nzchar(used))
  if (n_ok < 1L) stop("no public series returned")
  out <- list(
    panel = panel,
    symbols = used,
    fetched_at = Sys.time(),
    n_ok = as.integer(n_ok),
    n_nodes = nrow(nodes)
  )
  .asset_tx_panel_cache_put(cache_key, out)
  # #region agent log
  tryCatch({
    line <- jsonlite::toJSON(list(
      sessionId = "f77c57",
      hypothesisId = "E",
      location = "asset_transmission_module.R:fetch_panel",
      message = "fetch ok",
      data = list(n_ok = out$n_ok, n_nodes = out$n_nodes, nrow = nrow(out$panel)),
      timestamp = as.numeric(Sys.time()) * 1000
    ), auto_unbox = TRUE)
    cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
  }, error = function(e) NULL)
  # #endregion
  out
}

.asset_tx_with_symbols <- function(snap, symbols) {
  if (is.null(snap)) return(NULL)
  attr(snap, "symbols") <- symbols
  snap
}

# ---- Shiny -----------------------------------------------------------------

asset_transmission_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
      .ynow-atx-summary { margin: 8px 0 4px; font-size: 14px; line-height: 1.45; color: #1c1915; }
      .ynow-atx-legend { display: flex; flex-wrap: wrap; gap: 10px 18px; margin: 6px 0 12px; font-size: 12px; color: #3f3a33; }
      .ynow-atx-legend span { display: inline-flex; align-items: center; gap: 6px; }
      .ynow-atx-swatch { width: 18px; height: 4px; border-radius: 2px; display: inline-block; }
      .ynow-atx-cardswatch { width: 16px; height: 11px; border-radius: 3px; display: inline-block; border: 2px solid #1e7a46; box-sizing: border-box; }
      .ynow-atx-note { margin: 0 0 8px; font-size: 12px; color: #5c5346; }
      .ynow-atx-status { font-size: 12px; color: #5c5346; margin-top: 28px; }
      .ynow-atx-map {
        overflow-x: auto;
        overflow-y: auto;
        -webkit-overflow-scrolling: touch;
        overscroll-behavior: contain;
        width: 100%;
        max-width: 100%;
        background-color: #000000;
        background-image: none;
        border-radius: 8px;
        padding: 0;
        box-sizing: border-box;
        /* Keep the transmission canvas at a readable intrinsic width; swipe to explore. */
        touch-action: pan-x pan-y;
        position: relative;
        z-index: 1;
        isolation: isolate;
      }
      .ynow-atx-map .plotly,
      .ynow-atx-map .html-widget {
        /* Fit the panel so the session slider + Play stay fully visible by default. */
        width: 100% !important;
        min-width: 100% !important;
        max-width: 100% !important;
        height: min(86vh, 980px) !important;
      }
      .ynow-atx-map .js-plotly-plot,
      .ynow-atx-map .plot-container,
      .ynow-atx-map .svg-container {
        width: 100% !important;
        min-width: 100% !important;
        max-width: 100% !important;
      }
      .ynow-atx-map .modebar {
        top: 8px !important;
        right: 8px !important;
      }
      .ynow-atx-path { margin: 8px 0 10px; padding: 8px 10px; background: #fff; border: 1px solid #e4dccb; border-radius: 6px; }
      .ynow-atx-path h4 { margin: 0 0 6px; font-size: 14px; }
      .ynow-atx-regime { margin: 8px 0 10px; padding: 8px 10px; background: #f4f0e6; border: 1px solid #e4dccb; border-radius: 6px; }
      .ynow-atx-regime h4 { margin: 0 0 4px; font-size: 14px; }
      .ynow-atx-regime p { margin: 0 0 4px; font-size: 13px; line-height: 1.45; color: #1c1915; }
      .ynow-atx-path table { width: 100%; border-collapse: collapse; font-size: 12px; }
      .ynow-atx-path th, .ynow-atx-path td { text-align: left; padding: 3px 8px 3px 0; border-bottom: 1px solid #f0ebe3; }
      .ynow-atx h4 { margin: 14px 0 6px; font-size: 15px; }
      .ynow-atx-load {
        position: absolute;
        inset: 0;
        z-index: 6;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 12px;
        background: rgba(0, 0, 0, 0.82);
        border-radius: 8px;
        pointer-events: none;
      }
      .ynow-atx-load[hidden] { display: none !important; }
      .ynow-atx-load-label {
        color: #f4f6fb;
        font-size: 14px;
        letter-spacing: 0.01em;
        font-family: Arial, Helvetica, sans-serif;
      }
      .ynow-atx-load-track {
        width: min(72%, 420px);
        height: 8px;
        border-radius: 999px;
        background: rgba(244, 246, 251, 0.14);
        overflow: hidden;
        box-shadow: inset 0 0 0 1px rgba(244, 246, 251, 0.08);
      }
      .ynow-atx-load-bar {
        height: 100%;
        width: 38%;
        border-radius: 999px;
        background: linear-gradient(90deg, #2ec4c6 0%, #3a6fe0 55%, #6a5acd 100%);
        animation: ynow-atx-load-slide 1.15s ease-in-out infinite;
      }
      @keyframes ynow-atx-load-slide {
        0% { transform: translateX(-120%); }
        100% { transform: translateX(320%); }
      }
    ")),
    htmltools::tags$div(
      class = "ynow-atx",
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectInput(
            ns("window"),
            label = "Correlation window",
            choices = c("20 sessions" = "20", "60 sessions" = "60", "120 sessions" = "120"),
            selected = "60",
            width = "100%"
          )
        ),
        shiny::column(
          width = 3,
          htmltools::tags$div(
            style = "margin-top: 25px;",
            shiny::actionButton(ns("refresh"), "Refresh prints", icon = shiny::icon("sync"), class = "btn-default")
          )
        ),
        shiny::column(
          width = 5,
          shiny::uiOutput(ns("status"))
        )
      ),
      shiny::uiOutput(ns("summary")),
      shiny::uiOutput(ns("regime")),
      shiny::uiOutput(ns("path")),
      htmltools::tags$div(
        class = "ynow-atx-map",
        shiny::uiOutput(ns("map_loading")),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("map"), height = "900px", width = "100%"),
          type = 4,
          color = "#9CA889",
          proxy.height = "420px"
        )
      ),
      shiny::uiOutput(ns("legend")),
      shiny::uiOutput(ns("nodes_title")),
      DT::DTOutput(ns("nodes")),
      shiny::uiOutput(ns("edges_title")),
      DT::DTOutput(ns("edges")),
      shiny::uiOutput(ns("method"))
    ),
    htmltools::tags$script(htmltools::HTML("
      (function() {
        function centerAtxMap(map) {
          if (!map) return;
          var plot = map.querySelector('.js-plotly-plot') || map.querySelector('.html-widget');
          if (!plot) return;
          var maxX = Math.max(0, plot.offsetWidth - map.clientWidth);
          var maxY = Math.max(0, plot.offsetHeight - map.clientHeight);
          // Keep scrollLeft at 0 so the Play control + timeline start stay visible.
          map.scrollLeft = 0;
          map.scrollTop = maxY / 2;
          // #region agent log
          fetch('http://127.0.0.1:7302/ingest/e3a0dcdf-71e1-4bba-855e-f942118bd315',{method:'POST',headers:{'Content-Type':'application/json','X-Debug-Session-Id':'f77c57'},body:JSON.stringify({sessionId:'f77c57',runId:'center-pre',hypothesisId:'H-center',location:'asset_transmission_module.R:centerAtxMap',message:'atx map scroll center',data:{scrollLeft:map.scrollLeft,scrollTop:map.scrollTop,maxX:maxX,maxY:maxY,mapW:map.clientWidth,mapH:map.clientHeight,plotW:plot.offsetWidth,plotH:plot.offsetHeight},timestamp:Date.now()})}).catch(function(){});
          // #endregion
        }
        function bind(map) {
          if (!map || map.__ynowAtxCenterBound) return;
          map.__ynowAtxCenterBound = true;
          var attachPlot = function(plot) {
            if (!plot || plot.__ynowAtxAfterPlot) return;
            plot.__ynowAtxAfterPlot = true;
            plot.on('plotly_afterplot', function() { centerAtxMap(map); });
            centerAtxMap(map);
          };
          var existing = map.querySelector('.js-plotly-plot');
          if (existing) attachPlot(existing);
          var mo = new MutationObserver(function() {
            var plot = map.querySelector('.js-plotly-plot');
            if (plot) attachPlot(plot);
          });
          mo.observe(map, { childList: true, subtree: true });
        }
        function boot() {
          document.querySelectorAll('.ynow-atx-map').forEach(bind);
        }
        if (window.jQuery) {
          $(document).on('shiny:value shiny:visualchange', function() { boot(); });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', boot);
        } else {
          boot();
        }
      })();
    "))
  )
}

asset_transmission_server <- function(id, ui_locale_rv = NULL, market_mode_rv = NULL, tab_active_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    locale <- shiny::reactive({
      loc <- if (is.function(ui_locale_rv)) {
        tryCatch(ui_locale_rv(), error = function(e) "en")
      } else {
        "en"
      }
      if (exists("normalize_ui_locale", mode = "function")) normalize_ui_locale(loc) else "en"
    })
    market <- shiny::reactive({
      m <- if (is.function(market_mode_rv)) {
        tryCatch(market_mode_rv(), error = function(e) "US")
      } else {
        "US"
      }
      toupper(trimws(as.character(m %||% "US")[1]))
    })
    active <- shiny::reactive({
      if (!is.function(tab_active_rv)) return(TRUE)
      isTRUE(tryCatch(tab_active_rv(), error = function(e) TRUE))
    })

    pack_rv <- shiny::reactiveVal(NULL)
    err_rv <- shiny::reactiveVal(NULL)
    loading_rv <- shiny::reactiveVal(TRUE)
    # Progressive paint: latest session first, then full Play tape.
    tape_n_rv <- shiny::reactiveVal(1L)
    inflight <- FALSE

    load_panel <- function(force = FALSE) {
      if (isTRUE(inflight)) return(invisible(NULL))
      inflight <<- TRUE
      on.exit({ inflight <<- FALSE }, add = TRUE)
      loading_rv(TRUE)
      loc <- tryCatch(shiny::isolate(locale()), error = function(e) "en")
      run_fetch <- function() {
        res <- tryCatch(
          asset_tx_fetch_panel(force = isTRUE(force)),
          error = function(e) structure(list(message = conditionMessage(e)), class = "asset_tx_fetch_error")
        )
        if (inherits(res, "asset_tx_fetch_error")) {
          # Keep the last good panel so a failed refresh does not blank the map.
          # Writes are safe outside a reactive consumer; reads are not.
          err_rv(res$message)
          loading_rv(FALSE)
          return(invisible(NULL))
        }
        # later::later() runs this off the reactive flush so the UI can paint while
        # Yahoo downloads. Reading a reactiveVal there needs isolate().
        old <- shiny::isolate(pack_rv())
        if (
          !isTRUE(force) &&
            !is.null(old) &&
            identical(old$fetched_at, res$fetched_at) &&
            identical(old$n_ok, res$n_ok)
        ) {
          err_rv(NULL)
          loading_rv(FALSE)
          return(invisible(res))
        }
        err_rv(NULL)
        # Paint the latest session immediately, then expand to the Play tape.
        tape_n_rv(1L)
        pack_rv(res)
        loading_rv(FALSE)
        target_n <- as.integer(.asset_tx_tape_frames)
        if (target_n > 1L) {
          upgrade <- function() {
            if (!identical(shiny::isolate(tape_n_rv()), target_n)) {
              tape_n_rv(target_n)
            }
          }
          if (requireNamespace("later", quietly = TRUE)) {
            later::later(upgrade, delay = 0.05)
          } else {
            upgrade()
          }
        }
        invisible(res)
      }
      # Header progress fill tracks withProgress when a session is attached.
      if (requireNamespace("shiny", quietly = TRUE)) {
        tryCatch(
          shiny::withProgress(
            message = .asset_tx_label("atx_loading", loc),
            value = 0.12,
            session = session,
            {
              shiny::incProgress(0.35, detail = .asset_tx_label("atx_loading_bar", loc))
              out <- run_fetch()
              shiny::incProgress(0.95)
              out
            }
          ),
          error = function(e) run_fetch()
        )
      } else {
        run_fetch()
      }
    }

    shiny::observeEvent(input$refresh, {
      tryCatch(load_panel(force = TRUE), error = function(e) {
        err_rv(conditionMessage(e))
        loading_rv(FALSE)
      })
    }, ignoreInit = TRUE)

    shiny::observe({
      shiny::req(isTRUE(active()))
      shiny::invalidateLater(90000, session)
      run <- function() {
        tryCatch(load_panel(force = FALSE), error = function(e) {
          err_rv(conditionMessage(e))
          loading_rv(FALSE)
        })
      }
      # Defer off the reactive flush so the UI can paint while Yahoo downloads.
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(run, delay = 0)
      } else {
        run()
      }
    })

    shiny::observe({
      loc <- locale()
      sel <- .asset_tx_window_choice(shiny::isolate(input$window))
      shiny::updateSelectInput(
        session,
        "window",
        label = .asset_tx_label("atx_window", loc),
        choices = stats::setNames(
          c("20", "60", "120"),
          c(
            .asset_tx_label("atx_win_20", loc),
            .asset_tx_label("atx_win_60", loc),
            .asset_tx_label("atx_win_120", loc)
          )
        ),
        selected = sel
      )
      shiny::updateActionButton(session, "refresh", label = .asset_tx_label("atx_refresh", loc))
    })

    snaps <- shiny::reactive({
      pack <- pack_rv()
      shiny::req(pack)
      tryCatch({
        window <- asset_tx_window(.asset_tx_window_choice(input$window))
        n_fr <- as.integer(tape_n_rv() %||% 1L)
        if (!is.finite(n_fr) || n_fr < 1L) n_fr <- 1L
        tape <- asset_tx_tape(pack$panel, window = window, n = n_fr)
        lapply(tape, function(sn) {
          sn <- .asset_tx_with_symbols(sn, symbols = pack$symbols)
          attr(sn, "fetched_at") <- pack$fetched_at
          sn
        })
      }, error = function(e) {
        err_rv(conditionMessage(e))
        list()
      })
    })

    output$map_loading <- shiny::renderUI({
      loc <- locale()
      show <- isTRUE(loading_rv()) || is.null(pack_rv())
      if (!show) return(NULL)
      htmltools::tags$div(
        class = "ynow-atx-load",
        role = "progressbar",
        `aria-busy` = "true",
        `aria-label` = .asset_tx_label("atx_loading_bar", loc),
        htmltools::tags$div(class = "ynow-atx-load-track", htmltools::tags$div(class = "ynow-atx-load-bar")),
        htmltools::tags$div(class = "ynow-atx-load-label", .asset_tx_label("atx_loading_bar", loc))
      )
    })

    output$status <- shiny::renderUI({
      loc <- locale()
      err <- err_rv()
      pack <- pack_rv()
      if (!is.null(err) && is.null(pack)) {
        return(htmltools::tags$div(class = "ynow-atx-status", style = "color:#8a3b12;", err))
      }
      if (is.null(pack) || isTRUE(loading_rv())) {
        return(htmltools::tags$div(class = "ynow-atx-status", .asset_tx_label("atx_loading", loc)))
      }
      stamp <- format(pack$fetched_at, "%Y-%m-%d %H:%M UTC", tz = "UTC")
      msg <- .ynow_fmt_str(
        .asset_tx_label("atx_status", loc),
        n_ok = pack$n_ok,
        n_nodes = pack$n_nodes,
        stamp = stamp
      )
      if (!is.null(err)) msg <- paste(msg, err)
      htmltools::tags$div(class = "ynow-atx-status", msg)
    })

    output$summary <- shiny::renderUI({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      htmltools::tags$p(class = "ynow-atx-summary", asset_tx_summary_text(tape[[length(tape)]], locale()))
    })

    output$regime <- shiny::renderUI({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      loc <- locale()
      reg <- asset_tx_regime(tape[[length(tape)]], loc)
      htmltools::tags$div(
        class = "ynow-atx-regime",
        htmltools::tags$h4(paste0(.asset_tx_label("atx_regime_kicker", loc), " \u00b7 ", reg$title)),
        htmltools::tags$p(reg$body),
        htmltools::tags$p(class = "ynow-atx-note", style = "margin:0;", reg$note)
      )
    })

    output$path <- shiny::renderUI({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      loc <- locale()
      path <- asset_tx_yield_path(tape[[length(tape)]], loc)
      real_note <- htmltools::tags$p(class = "ynow-atx-note", style = "margin:6px 0 0;", .asset_tx_label("atx_path_real_note", loc))
      if (!nrow(path$rows)) {
        return(htmltools::tags$div(class = "ynow-atx-path", htmltools::tags$h4(path$title), real_note))
      }
      htmltools::tags$div(
        class = "ynow-atx-path",
        htmltools::tags$h4(path$title),
        htmltools::tags$table(
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th(.asset_tx_label("atx_col_asset", loc)),
              htmltools::tags$th(.asset_tx_label("atx_path_lean", loc)),
              htmltools::tags$th(.asset_tx_label("atx_path_actual", loc))
            )
          ),
          htmltools::tags$tbody(
            lapply(seq_len(nrow(path$rows)), function(i) {
              htmltools::tags$tr(
                htmltools::tags$td(path$rows$asset[i]),
                htmltools::tags$td(path$rows$lean[i]),
                htmltools::tags$td(path$rows$actual[i])
              )
            })
          )
        ),
        real_note
      )
    })

    output$map <- plotly::renderPlotly({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      fig <- tryCatch(
        asset_tx_figure(tape, locale(), market()),
        error = function(e) {
          # #region agent log
          tryCatch({
            line <- jsonlite::toJSON(list(
              sessionId = "f77c57",
              hypothesisId = "C",
              location = "asset_transmission_module.R:renderPlotly",
              message = "figure error",
              data = list(err = conditionMessage(e), tape_n = length(tape)),
              timestamp = as.numeric(Sys.time()) * 1000
            ), auto_unbox = TRUE)
            cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
          }, error = function(e2) NULL)
          # #endregion
          err_rv(conditionMessage(e))
          NULL
        }
      )
      # #region agent log
      tryCatch({
        line <- jsonlite::toJSON(list(
          sessionId = "f77c57",
          hypothesisId = "D",
          location = "asset_transmission_module.R:renderPlotly",
          message = "map render",
          data = list(
            tape_n = length(tape),
            fig_null = is.null(fig),
            n_data = if (!is.null(fig)) length(fig$x$data) else 0L,
            n_frames = if (!is.null(fig)) length(fig$x$frames) else 0L
          ),
          timestamp = as.numeric(Sys.time()) * 1000
        ), auto_unbox = TRUE)
        cat(as.character(line), "\n", file = "/Users/lawrencekuo/coding/theYNowApp/.cursor/debug-f77c57.log", append = TRUE)
      }, error = function(e2) NULL)
      # #endregion
      shiny::validate(shiny::need(!is.null(fig), .asset_tx_label("atx_empty", locale())))
      fig
    })

    output$legend <- shiny::renderUI({
      loc <- locale()
      node_key <- if (identical(market(), "TW")) "atx_legend_node_tw" else "atx_legend_node_us"
      up_hex <- asset_tx_move_colors(market())$up
      htmltools::tagList(
        htmltools::tags$div(
          class = "ynow-atx-legend",
          htmltools::tags$span(
            htmltools::tags$i(
              class = "ynow-atx-cardswatch",
              style = sprintf("background:#1f3f7a;border-color:%s;", up_hex)
            ),
            .asset_tx_label("atx_legend_fill_hub", loc)
          ),
          htmltools::tags$span(
            htmltools::tags$i(
              class = "ynow-atx-cardswatch",
              style = sprintf("background:#161c2e;border-color:%s;", up_hex)
            ),
            .asset_tx_label("atx_legend_fill_other", loc)
          ),
          htmltools::tags$span(
            htmltools::tags$i(class = "ynow-atx-swatch", style = "background:#2ec4c6;"),
            .asset_tx_label("atx_legend_align", loc)
          ),
          htmltools::tags$span(
            htmltools::tags$i(class = "ynow-atx-swatch", style = "background:#c47b14;"),
            .asset_tx_label("atx_legend_diverge", loc)
          ),
          htmltools::tags$span(
            htmltools::tags$i(class = "ynow-atx-swatch", style = "background:#6a5acd;"),
            .asset_tx_label("atx_legend_mixed", loc)
          ),
          htmltools::tags$span(
            htmltools::tags$i(class = "ynow-atx-swatch", style = "background:#a7adb4;"),
            .asset_tx_label("atx_legend_weak", loc)
          )
        ),
        htmltools::tags$p(class = "ynow-atx-note", .asset_tx_label("atx_arrow_note", loc)),
        htmltools::tags$p(class = "ynow-atx-note", .asset_tx_label(node_key, loc)),
        htmltools::tags$p(class = "ynow-atx-note", .asset_tx_label("atx_pan_hint", loc)),
        htmltools::tags$p(class = "ynow-atx-note", .asset_tx_label("atx_play_hint", loc))
      )
    })

    output$nodes_title <- shiny::renderUI({
      htmltools::tags$h4(.asset_tx_label("atx_nodes_title", locale()))
    })
    output$edges_title <- shiny::renderUI({
      htmltools::tags$h4(.asset_tx_label("atx_edges_title", locale()))
    })

    output$nodes <- DT::renderDT({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      df <- asset_tx_node_table(tape[[length(tape)]], locale())
      DT::datatable(
        df,
        rownames = FALSE,
        escape = TRUE,
        class = "compact stripe",
        options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = TRUE)
      )
    }, server = FALSE)

    output$edges <- DT::renderDT({
      tape <- snaps()
      shiny::req(length(tape) > 0L)
      loc <- locale()
      df <- asset_tx_edge_table(tape[[length(tape)]], loc)
      state_col <- .asset_tx_label("atx_col_state", loc)
      dt <- DT::datatable(
        df,
        rownames = FALSE,
        escape = TRUE,
        class = "compact stripe",
        options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = TRUE, pageLength = 50)
      )
      DT::formatStyle(
        dt,
          state_col,
          color = DT::styleEqual(
            c(
              .asset_tx_label("atx_state_aligned", loc),
              .asset_tx_label("atx_state_diverged", loc),
              .asset_tx_label("atx_state_mixed", loc),
              .asset_tx_label("atx_state_weak", loc),
              .asset_tx_label("atx_state_na", loc)
            ),
            c("#0f6e6e", "#a35b00", "#5b4b8a", "#6b7280", "#9aa0a6")
          )
        )
    }, server = FALSE)

    output$method <- shiny::renderUI({
      loc <- locale()
      if (!exists("ynow_notes_block", mode = "function")) {
        return(htmltools::tags$p(.asset_tx_label("atx_method_body", loc)))
      }
      ynow_notes_block(
        htmltools::tags$p(.asset_tx_label("atx_method_body", loc)),
        locale = loc,
        open = FALSE
      )
    })
  })
}
