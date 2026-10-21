# ==========================================
# lab_im_ind_mcap.R — extracted from lab_industry_method.R (C-STRUCT-03)
# Behavior-preserving structural split; sourced by lab_industry_method.R.
# ==========================================

# ---- Industry-chain mcap-weighted upside ranking (snapshot + all chains) ----

.lab_ind_mcap_chain_cache <- new.env(parent = emptyenv())

#' Bundled / override path for industry-chain mcap-weighted rank snapshot
lab_ind_mcap_chain_snapshot_path <- function(market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  opt <- getOption("ynow.ind_mcap_chain_snapshot", default = NULL)
  if (!is.null(opt) && length(opt) >= 1L) {
    p <- as.character(opt)[1]
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  candidates <- c(
    file.path("data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm)),
    file.path("data", "ind_mcap_chain_rank_snapshot.csv"),
    file.path("app_26.0", "data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm)),
    file.path("app_26.0", "data", "ind_mcap_chain_rank_snapshot.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  # Prefer writable cache for saves when bundled file is missing
  cache_dir <- tryCatch({
    if (exists("lab_quality_snapshot_dir", mode = "function")) {
      lab_quality_snapshot_dir()
    } else {
      file.path(tempdir(), "ynow_cache", "slow")
    }
  }, error = function(e) file.path(tempdir(), "ynow_cache", "slow"))
  file.path(cache_dir, sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm))
}

lab_ind_mcap_chain_snapshot_empty <- function() {
  data.frame(
    chain_id = character(0),
    wavg_cagr = numeric(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    snapshot_at = character(0),
    market = character(0),
    stringsAsFactors = FALSE
  )
}

#' Load industry-chain rank snapshot (bundled default or prior 更新排名 result)
lab_load_ind_mcap_chain_snapshot <- function(market = "US", force = FALSE) {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  cache_key <- paste0("df_", mm)
  if (!isTRUE(force) &&
      exists(cache_key, envir = .lab_ind_mcap_chain_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .lab_ind_mcap_chain_cache, inherits = FALSE))
  }
  path <- lab_ind_mcap_chain_snapshot_path(mm)
  empty <- lab_ind_mcap_chain_snapshot_empty()
  if (!nzchar(path) || is.na(path) || !file.exists(path)) {
    assign(cache_key, empty, envir = .lab_ind_mcap_chain_cache)
    attr(empty, "snapshot_source") <- "missing"
    return(empty)
  }
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0L || !"chain_id" %in% names(raw)) {
    assign(cache_key, empty, envir = .lab_ind_mcap_chain_cache)
    attr(empty, "snapshot_source") <- "empty"
    return(empty)
  }
  df <- data.frame(
    chain_id = as.character(raw$chain_id),
    wavg_cagr = if ("wavg_cagr" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$wavg_cagr))
    } else {
      NA_real_
    },
    total_mcap = if ("total_mcap" %in% names(raw)) {
      suppressWarnings(as.numeric(raw$total_mcap))
    } else {
      NA_real_
    },
    n_names = if ("n_names" %in% names(raw)) {
      as.integer(suppressWarnings(as.numeric(raw$n_names)))
    } else {
      NA_integer_
    },
    snapshot_at = if ("snapshot_at" %in% names(raw)) {
      as.character(raw$snapshot_at)
    } else {
      rep(NA_character_, nrow(raw))
    },
    market = if ("market" %in% names(raw)) {
      as.character(raw$market)
    } else {
      rep(mm, nrow(raw))
    },
    stringsAsFactors = FALSE
  )
  df <- df[nzchar(df$chain_id) & !is.na(df$chain_id), , drop = FALSE]
  df <- lab_ind_mcap_drop_unmapped_chains(df)
  rownames(df) <- NULL
  attr(df, "snapshot_source") <- path
  if (nrow(df) && any(nzchar(df$snapshot_at), na.rm = TRUE)) {
    attr(df, "snapshot_at") <- as.character(df$snapshot_at[nzchar(df$snapshot_at)][[1]])
  }
  assign(cache_key, df, envir = .lab_ind_mcap_chain_cache)
  df
}

#' Persist industry-chain rank snapshot (bundled data/ when writable; else slow cache)
lab_save_ind_mcap_chain_snapshot <- function(snapshot, market = "US") {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  snap <- if (is.data.frame(snapshot)) snapshot else lab_ind_mcap_chain_snapshot_empty()
  snap <- lab_ind_mcap_drop_unmapped_chains(snap)
  if (!is.data.frame(snap) || !nrow(snap)) snap <- lab_ind_mcap_chain_snapshot_empty()
  if (!"market" %in% names(snap)) snap$market <- mm
  snap$market[!nzchar(as.character(snap$market))] <- mm
  # Option path wins when set (tests). Prefer writable cache over bundled data/
  # so a failed / no-Yahoo refresh cannot wipe the shipped snapshot (all-NA wavg).
  opt <- getOption("ynow.ind_mcap_chain_snapshot", default = NULL)
  bundled <- file.path("data", sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm))
  has_upside <- any(
    is.finite(suppressWarnings(as.numeric(snap$wavg_cagr))),
    na.rm = TRUE
  )
  path <- if (!is.null(opt) && length(opt) >= 1L && nzchar(as.character(opt)[1])) {
    as.character(opt)[1]
  } else if (isTRUE(has_upside) && dir.exists("data") && file.access("data", 2) == 0L) {
    bundled
  } else {
    # Writable cache — keeps packaged CSV intact when upside is all NA
    cache_dir <- tryCatch({
      if (exists("lab_quality_snapshot_dir", mode = "function")) {
        lab_quality_snapshot_dir()
      } else {
        file.path(tempdir(), "ynow_cache", "slow")
      }
    }, error = function(e) file.path(tempdir(), "ynow_cache", "slow"))
    file.path(cache_dir, sprintf("ind_mcap_chain_rank_snapshot_%s.csv", mm))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(snap, tmp, row.names = FALSE, fileEncoding = "UTF-8")
  file.rename(tmp, path)
  cache_key <- paste0("df_", mm)
  out_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  attr(snap, "snapshot_source") <- out_path
  if (nrow(snap) && any(nzchar(as.character(snap$snapshot_at)), na.rm = TRUE)) {
    attr(snap, "snapshot_at") <- as.character(
      snap$snapshot_at[nzchar(as.character(snap$snapshot_at))][[1]]
    )
  }
  assign(cache_key, snap, envir = .lab_ind_mcap_chain_cache)
  out_path
}

#' Chain ids excluded from Industry cap-weighted upside ranking (Unmapped bucket).
lab_ind_mcap_unmapped_chain_ids <- function() {
  ids <- c("未對應", "Unmapped", "unmapped")
  if (exists("LAB_UNMAPPED_KEY", inherits = TRUE)) {
    ids <- c(ids, as.character(LAB_UNMAPPED_KEY)[1])
  }
  unique(ids[nzchar(ids) & !is.na(ids)])
}

#' TRUE when `chain_id` is the Unmapped industry-chain bucket.
lab_ind_mcap_is_unmapped_chain <- function(chain_id) {
  cid <- trimws(as.character(chain_id %||% "")[1])
  if (!nzchar(cid) || is.na(cid)) return(TRUE)
  drop <- lab_ind_mcap_unmapped_chain_ids()
  if (cid %in% drop) return(TRUE)
  identical(tolower(cid), "unmapped") ||
    identical(tolower(cid), "unmapped industry")
}

#' Drop Unmapped rows from a chain aggregate / snapshot frame.
lab_ind_mcap_drop_unmapped_chains <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  if (!"chain_id" %in% names(df)) return(df)
  keep <- !vapply(df$chain_id, lab_ind_mcap_is_unmapped_chain, logical(1))
  out <- df[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Per-chain universe market-cap totals (ALL names in catalog, not eval sample).
#' Unmapped／未對應 chains are dropped (same rule as the upside board).
lab_chain_mcap_universe_totals <- function(universe_df) {
  empty <- data.frame(
    chain_id = character(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(universe_df) || !is.data.frame(universe_df) || nrow(universe_df) < 1L) {
    return(empty)
  }
  df <- universe_df
  if (!"market_cap" %in% names(df) ||
      !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
             suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
    if (exists("lab_attach_market_caps", mode = "function")) {
      df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
    }
  }
  mcap <- suppressWarnings(as.numeric(df$market_cap))
  ok <- is.finite(mcap) & mcap > 0
  df <- df[ok, , drop = FALSE]
  mcap <- mcap[ok]
  if (!nrow(df)) return(empty)
  chain_ids <- if ("industry_key" %in% names(df) &&
                   exists("industry_chain_id", mode = "function")) {
    vapply(as.character(df$industry_key), function(k) {
      cid <- tryCatch(industry_chain_id(k), error = function(e) "")
      as.character(cid %||% "")[1]
    }, character(1))
  } else if ("chain_id" %in% names(df)) {
    as.character(df$chain_id)
  } else {
    rep("", nrow(df))
  }
  miss <- is.na(chain_ids) | !nzchar(chain_ids)
  if (any(miss) && "industry_label" %in% names(df)) {
    chain_ids[miss] <- as.character(df$industry_label[miss])
  }
  keep <- !vapply(chain_ids, lab_ind_mcap_is_unmapped_chain, logical(1))
  df <- df[keep, , drop = FALSE]
  mcap <- mcap[keep]
  chain_ids <- chain_ids[keep]
  if (!nrow(df)) return(empty)
  keys <- unique(chain_ids)
  rows <- lapply(keys, function(k) {
    hit <- which(chain_ids == k)
    data.frame(
      chain_id = k,
      total_mcap = sum(mcap[hit]),
      n_names = length(hit),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  lab_ind_mcap_drop_unmapped_chains(out)
}

#' Format raw chain snapshot / aggregate rows into the DT display table.
lab_format_ind_mcap_chain_table <- function(agg_df, locale = "zh-TW") {
  empty <- data.frame(
    排名 = integer(0),
    產業鏈 = character(0),
    市值加權年化估值漲幅 = character(0),
    總市值 = character(0),
    檔數 = integer(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  agg_df <- lab_ind_mcap_drop_unmapped_chains(agg_df)
  if (is.null(agg_df) || !is.data.frame(agg_df) || nrow(agg_df) == 0L) {
    return(lab_localize_im_colnames(empty, locale))
  }
  .fmt_mcap <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x) || x <= 0) return("—")
    if (x >= 1e12) sprintf("%.2fT", x / 1e12)
    else if (x >= 1e9) sprintf("%.2fB", x / 1e9)
    else if (x >= 1e6) sprintf("%.2fM", x / 1e6)
    else sprintf("%.0f", x)
  }
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  n <- nrow(agg_df)
  chain_ids <- as.character(agg_df$chain_id %||% rep("", n))
  labs <- vapply(seq_len(n), function(i) {
    cid <- chain_ids[[i]]
    if (nzchar(cid) && exists("industry_chain_label", mode = "function")) {
      lab <- tryCatch(industry_chain_label(cid, locale = loc), error = function(e) NA_character_)
      if (is.character(lab) && length(lab) && nzchar(lab[[1]]) && !is.na(lab[[1]])) {
        return(as.character(lab[[1]]))
      }
    }
    if ("industry_label" %in% names(agg_df)) {
      alt <- as.character(agg_df$industry_label[[i]] %||% "")[1]
      if (nzchar(alt)) return(alt)
    }
    if (nzchar(cid)) cid else "—"
  }, character(1))
  wavg <- suppressWarnings(as.numeric(agg_df$wavg_cagr))
  wavg_txt <- ifelse(
    is.finite(wavg),
    sprintf("%+.1f%%", wavg),
    "—"
  )
  mcap_txt <- vapply(agg_df$total_mcap, .fmt_mcap, character(1))
  n_names <- if ("n_names" %in% names(agg_df)) {
    as.integer(suppressWarnings(as.numeric(agg_df$n_names)))
  } else {
    rep(NA_integer_, n)
  }
  lab_localize_im_colnames(
    data.frame(
      排名 = seq_len(n),
      產業鏈 = labs,
      市值加權年化估值漲幅 = wavg_txt,
      總市值 = mcap_txt,
      檔數 = n_names,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    loc
  )
}

#' Market-aware colorscale for industry mcap treemap (valuation upside %).
#'
#' US: green up / red down. TW: red up / green down (台灣投資慣例).
lab_ind_mcap_treemap_colorscale <- function(market = "US") {
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market)
    } else {
      m <- toupper(trimws(as.character(market)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")
  # Soft block-heatmap palette (close to common market treemap UIs).
  if (identical(mm, "TW")) {
    list(
      list(0, "#1b7a4a"),
      list(0.35, "#8fd6a8"),
      list(0.5, "#d8dde3"),
      list(0.65, "#f0a2a2"),
      list(1, "#c0392b")
    )
  } else {
    list(
      list(0, "#c0392b"),
      list(0.35, "#f0a2a2"),
      list(0.5, "#d8dde3"),
      list(0.65, "#8fd6a8"),
      list(1, "#1b7a4a")
    )
  }
}

#' Prepare all industry-chain rows for the block treemap (size = total mcap).
#'
#' Pads missing picker chains so **全部產業** always appear; Unmapped excluded.
lab_ind_mcap_prepare_treemap_df <- function(snap_df, locale = "zh-TW") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  empty <- data.frame(
    chain_id = character(0),
    label = character(0),
    wavg_cagr = numeric(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    stringsAsFactors = FALSE
  )
  snap_df <- lab_ind_mcap_drop_unmapped_chains(snap_df)
  picker <- character(0)
  if (exists("lab_industry_chain_picker_choices", mode = "function")) {
    picker <- tryCatch(
      unname(lab_industry_chain_picker_choices(loc)),
      error = function(e) character(0)
    )
  }
  picker <- as.character(picker %||% character(0))
  picker <- picker[nzchar(picker) & !is.na(picker)]
  if (exists("lab_ind_mcap_is_unmapped_chain", mode = "function")) {
    picker <- picker[!vapply(picker, lab_ind_mcap_is_unmapped_chain, logical(1))]
  }
  if (!length(picker) && (is.null(snap_df) || !nrow(snap_df))) return(empty)

  by_id <- list()
  if (is.data.frame(snap_df) && nrow(snap_df) > 0L) {
    for (i in seq_len(nrow(snap_df))) {
      cid <- as.character(snap_df$chain_id[[i]] %||% "")[1]
      if (!nzchar(cid)) next
      by_id[[cid]] <- list(
        wavg = suppressWarnings(as.numeric(snap_df$wavg_cagr[[i]])),
        mcap = suppressWarnings(as.numeric(snap_df$total_mcap[[i]])),
        n_names = if ("n_names" %in% names(snap_df)) {
          as.integer(suppressWarnings(as.numeric(snap_df$n_names[[i]])))
        } else {
          NA_integer_
        }
      )
    }
  }
  ids <- unique(c(picker, names(by_id)))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(empty)

  .chain_lab <- function(cid) {
    if (exists("industry_chain_label", mode = "function")) {
      lab <- tryCatch(industry_chain_label(cid, locale = loc), error = function(e) NA_character_)
      if (is.character(lab) && length(lab) && nzchar(lab[[1]]) && !is.na(lab[[1]])) {
        return(as.character(lab[[1]]))
      }
    }
    cid
  }
  rows <- lapply(ids, function(cid) {
    hit <- by_id[[cid]]
    data.frame(
      chain_id = cid,
      label = .chain_lab(cid),
      wavg_cagr = if (is.null(hit)) NA_real_ else as.numeric(hit$wavg)[1],
      total_mcap = if (is.null(hit)) NA_real_ else as.numeric(hit$mcap)[1],
      n_names = if (is.null(hit)) NA_integer_ else as.integer(hit$n_names)[1],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  # Prefer largest mcap first for stable packing.
  ord <- order(
    -ifelse(is.finite(out$total_mcap), out$total_mcap, -Inf),
    out$label
  )
  out[ord, , drop = FALSE]
}

#' Plotly block treemap for industry mcap-weighted upside (all chains).
#'
#' Area = total market cap; color = mcap-weighted annualized valuation upside.
#' @param market US → green-up; TW → red-up.
lab_ind_mcap_treemap_plotly <- function(snap_df,
                                        market = "US",
                                        locale = "zh-TW",
                                        color_abs_max = 20) {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  mm <- tryCatch({
    if (exists("normalize_market_mode", mode = "function")) {
      normalize_market_mode(market)
    } else {
      m <- toupper(trimws(as.character(market)[1]))
      if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) "TW" else "US"
    }
  }, error = function(e) "US")
  df <- lab_ind_mcap_prepare_treemap_df(snap_df, locale = loc)
  empty_plot <- function(msg) {
    p0 <- plotly::plotly_empty(type = "scatter")
    p0 <- plotly::layout(
      p0,
      annotations = list(
        list(
          text = msg, showarrow = FALSE, xref = "paper", yref = "paper",
          x = 0.5, y = 0.5, font = list(size = 13, color = "#888")
        )
      ),
      margin = list(l = 8, r = 8, t = 8, b = 8),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
    plotly::config(p0, displayModeBar = FALSE)
  }
  if (!is.data.frame(df) || nrow(df) < 1L) {
    msg <- tryCatch(
      ui_str("lab_im_ind_mcap_treemap_empty", loc),
      error = function(e) if (identical(loc, "en")) {
        "No industry-chain blocks yet."
      } else {
        "尚無產業鏈區塊可顯示。"
      }
    )
    return(empty_plot(msg))
  }

  mcap <- suppressWarnings(as.numeric(df$total_mcap))
  # Tiny floor so zero/NA mcap chains still appear (全部產業).
  pos <- is.finite(mcap) & mcap > 0
  floor_v <- if (any(pos)) max(min(mcap[pos]) * 0.02, 1) else 1
  values <- ifelse(pos, mcap, floor_v)

  wavg <- suppressWarnings(as.numeric(df$wavg_cagr))
  cabsm <- suppressWarnings(as.numeric(color_abs_max)[1])
  if (!is.finite(cabsm) || cabsm <= 0) cabsm <- 20
  # Midpoint grey for missing upside; clip extremes for a readable scale.
  z <- ifelse(is.finite(wavg), pmax(-cabsm, pmin(cabsm, wavg)), 0)

  wavg_txt <- ifelse(is.finite(wavg), sprintf("%+.1f%%", wavg), "—")
  labels <- paste0(df$label, "\n", wavg_txt)
  n_names <- suppressWarnings(as.integer(df$n_names))
  hover_chain <- tryCatch(
    ui_str("lab_im_ind_mcap_treemap_hover_chain", loc),
    error = function(e) if (identical(loc, "en")) "Industry chain" else "產業鏈"
  )
  hover_upside <- tryCatch(
    ui_str("lab_im_ind_mcap_treemap_hover_upside", loc),
    error = function(e) if (identical(loc, "en")) {
      "Mcap-weighted annualized upside"
    } else {
      "市值加權年化估值漲幅"
    }
  )
  hover_mcap <- tryCatch(
    ui_str("lab_im_ind_mcap_treemap_hover_mcap", loc),
    error = function(e) if (identical(loc, "en")) "Total market cap" else "總市值"
  )
  hover_n <- tryCatch(
    ui_str("lab_im_ind_mcap_treemap_hover_n", loc),
    error = function(e) if (identical(loc, "en")) "Names" else "檔數"
  )
  cbar_title <- tryCatch(
    ui_str("lab_im_ind_mcap_treemap_colorbar", loc),
    error = function(e) if (identical(loc, "en")) "Upside %" else "漲幅 %"
  )
  .fmt_mcap <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x) || x <= 0) return("—")
    if (x >= 1e12) sprintf("%.2fT", x / 1e12)
    else if (x >= 1e9) sprintf("%.2fB", x / 1e9)
    else if (x >= 1e6) sprintf("%.2fM", x / 1e6)
    else sprintf("%.0f", x)
  }
  mcap_txt <- vapply(mcap, .fmt_mcap, character(1))
  n_txt <- ifelse(is.finite(n_names), as.character(n_names), "—")
  custom <- paste0(
    hover_chain, ": ", df$label, "<br>",
    hover_upside, ": ", wavg_txt, "<br>",
    hover_mcap, ": ", mcap_txt, "<br>",
    hover_n, ": ", n_txt
  )

  p <- plotly::plot_ly(
    type = "treemap",
    labels = labels,
    parents = rep("", nrow(df)),
    values = values,
    ids = as.character(df$chain_id),
    text = labels,
    textinfo = "label",
    hovertext = custom,
    hoverinfo = "text",
    marker = list(
      colors = z,
      colorscale = lab_ind_mcap_treemap_colorscale(mm),
      cmin = -cabsm,
      cmax = cabsm,
      cmid = 0,
      line = list(width = 1.5, color = "#ffffff"),
      pad = list(t = 2, l = 2, r = 2, b = 2),
      showscale = TRUE,
      colorbar = list(
        title = list(text = cbar_title, font = list(size = 11)),
        thickness = 12,
        len = 0.55,
        y = 0.5,
        tickfont = list(size = 10),
        ticksuffix = "%"
      )
    ),
    tiling = list(packing = "squarify", pad = 2),
    pathbar = list(visible = FALSE)
  )
  p <- plotly::layout(
    p,
    margin = list(l = 4, r = 4, t = 4, b = 4),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    uniformtext = list(minsize = 10, mode = "hide"),
    font = list(family = "Helvetica Neue, Arial, sans-serif", size = 12, color = "#222")
  )
  plotly::config(p, displayModeBar = FALSE, responsive = TRUE)
}

#' All industry chains ranked by mcap-weighted annualized valuation upside.
#' Includes every mapped chain from the picker (missing data → — / NA), sorted by
#' wavg desc. Unmapped（未對應）is excluded from ranking and display.
#' @param universe_df Full catalog/universe for 總市值／檔數 (all names, not eval sample).
lab_industry_chain_mcap_upside_leaderboard <- function(merged_df,
                                                      eq_only = FALSE,
                                                      gate_only = FALSE,
                                                      no_alert = FALSE,
                                                      locale = "zh-TW",
                                                      include_all_chains = TRUE,
                                                      universe_df = NULL) {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else {
    as.character(locale %||% "zh-TW")[1]
  }
  all_ids <- character(0)
  if (isTRUE(include_all_chains) &&
      exists("lab_industry_chain_picker_choices", mode = "function")) {
    all_ids <- unname(lab_industry_chain_picker_choices(loc))
  } else if (isTRUE(include_all_chains) &&
             exists("industry_chain_picker_choices", mode = "function")) {
    all_ids <- unname(industry_chain_picker_choices(loc))
  }
  all_ids <- unique(as.character(all_ids %||% character(0)))
  all_ids <- all_ids[nzchar(all_ids) & !is.na(all_ids)]
  # Never pad / rank the Unmapped bucket on this board
  all_ids <- all_ids[!vapply(all_ids, lab_ind_mcap_is_unmapped_chain, logical(1))]

  empty_agg <- data.frame(
    chain_id = character(0),
    wavg_cagr = numeric(0),
    total_mcap = numeric(0),
    n_names = integer(0),
    stringsAsFactors = FALSE
  )

  df <- tryCatch(
    lab_leaderboard_pool(
      merged_df, eq_only = eq_only, gate_only = gate_only, no_alert = no_alert
    ),
    error = function(e) NULL
  )
  agg <- empty_agg
  if (is.data.frame(df) && nrow(df) > 0L) {
    if (!"market_cap" %in% names(df) ||
        !any(is.finite(suppressWarnings(as.numeric(df$market_cap))) &
               suppressWarnings(as.numeric(df$market_cap)) > 0, na.rm = TRUE)) {
      if (exists("lab_attach_market_caps", mode = "function")) {
        df <- tryCatch(lab_attach_market_caps(df), error = function(e) df)
      }
    }
    mcap <- suppressWarnings(as.numeric(df$market_cap))
    ups <- suppressWarnings(as.numeric(df$upside_cagr_pct))
    ok <- is.finite(mcap) & mcap > 0 & is.finite(ups)
    df <- df[ok, , drop = FALSE]
    mcap <- mcap[ok]
    ups <- ups[ok]
    if (nrow(df) > 0L) {
      chain_ids <- if ("industry_key" %in% names(df) &&
                       exists("industry_chain_id", mode = "function")) {
        vapply(as.character(df$industry_key), function(k) {
          cid <- tryCatch(industry_chain_id(k), error = function(e) "")
          if (!nzchar(cid) && "industry_label" %in% names(df)) {
            # fall through
          }
          as.character(cid %||% "")[1]
        }, character(1))
      } else {
        rep("", nrow(df))
      }
      miss <- !nzchar(chain_ids) | is.na(chain_ids)
      if (any(miss) && "industry_label" %in% names(df)) {
        chain_ids[miss] <- as.character(df$industry_label[miss])
      }
      # Drop unmapped / blank chains — do not create an Unmapped rank row
      keep_ch <- !vapply(chain_ids, lab_ind_mcap_is_unmapped_chain, logical(1))
      df <- df[keep_ch, , drop = FALSE]
      mcap <- mcap[keep_ch]
      ups <- ups[keep_ch]
      chain_ids <- chain_ids[keep_ch]
      if (nrow(df) > 0L) {
        keys <- unique(chain_ids)
        rows <- lapply(keys, function(k) {
          hit <- which(chain_ids == k)
          w <- mcap[hit]
          u <- ups[hit]
          data.frame(
            chain_id = k,
            wavg_cagr = sum(w * u) / sum(w),
            # Placeholder — overwritten by full-universe totals below
            total_mcap = sum(w),
            n_names = length(hit),
            stringsAsFactors = FALSE
          )
        })
        agg <- do.call(rbind, rows)
      }
    }
  }

  # 總市值／檔數 = ALL names in the chain universe (not eval / Top-N sample)
  uni_src <- if (is.data.frame(universe_df) && nrow(universe_df) > 0L) {
    universe_df
  } else {
    NULL
  }
  totals <- tryCatch(
    lab_chain_mcap_universe_totals(uni_src),
    error = function(e) NULL
  )
  if (is.data.frame(totals) && nrow(totals) > 0L) {
    if (!nrow(agg)) {
      agg <- data.frame(
        chain_id = totals$chain_id,
        wavg_cagr = NA_real_,
        total_mcap = totals$total_mcap,
        n_names = as.integer(totals$n_names),
        stringsAsFactors = FALSE
      )
    } else {
      idx <- match(agg$chain_id, totals$chain_id)
      hit <- which(is.finite(idx))
      if (length(hit)) {
        agg$total_mcap[hit] <- totals$total_mcap[idx[hit]]
        agg$n_names[hit] <- as.integer(totals$n_names[idx[hit]])
      }
      extra <- setdiff(totals$chain_id, agg$chain_id)
      if (length(extra)) {
        j <- match(extra, totals$chain_id)
        agg <- rbind(
          agg,
          data.frame(
            chain_id = totals$chain_id[j],
            wavg_cagr = NA_real_,
            total_mcap = totals$total_mcap[j],
            n_names = as.integer(totals$n_names[j]),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  agg <- lab_ind_mcap_drop_unmapped_chains(agg)
  if (is.null(agg) || !is.data.frame(agg)) agg <- empty_agg

  if (length(all_ids)) {
    missing <- setdiff(all_ids, agg$chain_id)
    if (length(missing)) {
      agg <- rbind(
        agg,
        data.frame(
          chain_id = missing,
          wavg_cagr = NA_real_,
          total_mcap = NA_real_,
          n_names = 0L,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (!nrow(agg)) {
    return(list(
      aggregate = empty_agg,
      table = lab_format_ind_mcap_chain_table(empty_agg, loc)
    ))
  }
  ord <- order(
    !is.finite(agg$wavg_cagr),
    -ifelse(is.finite(agg$wavg_cagr), agg$wavg_cagr, -Inf),
    agg$chain_id,
    method = "radix"
  )
  agg <- agg[ord, , drop = FALSE]
  rownames(agg) <- NULL
  list(
    aggregate = agg,
    table = lab_format_ind_mcap_chain_table(agg, loc)
  )
}

#' Build / refresh chain-rank snapshot from a merged evaluated frame.
#' @param universe_df Full catalog for 總市值 (all chain names).
lab_build_ind_mcap_chain_snapshot <- function(merged_df,
                                             market = "US",
                                             eq_only = FALSE,
                                             gate_only = FALSE,
                                             no_alert = FALSE,
                                             locale = "zh-TW",
                                             snapshot_at = NULL,
                                             universe_df = NULL) {
  mm <- tryCatch(
    normalize_market_mode(market),
    error = function(e) toupper(trimws(as.character(market %||% "US")[1]))
  )
  if (!mm %in% c("US", "TW")) mm <- "US"
  built <- lab_industry_chain_mcap_upside_leaderboard(
    merged_df,
    eq_only = eq_only,
    gate_only = gate_only,
    no_alert = no_alert,
    locale = locale,
    include_all_chains = TRUE,
    universe_df = universe_df
  )
  agg <- built$aggregate
  if (!is.data.frame(agg) || nrow(agg) == 0L) {
    agg <- lab_ind_mcap_chain_snapshot_empty()
  }
  ts <- as.character(snapshot_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]
  snap <- data.frame(
    chain_id = as.character(agg$chain_id),
    wavg_cagr = suppressWarnings(as.numeric(agg$wavg_cagr)),
    total_mcap = suppressWarnings(as.numeric(agg$total_mcap)),
    n_names = as.integer(suppressWarnings(as.numeric(agg$n_names))),
    snapshot_at = rep(ts, nrow(agg)),
    market = rep(mm, nrow(agg)),
    stringsAsFactors = FALSE
  )
  path <- tryCatch(lab_save_ind_mcap_chain_snapshot(snap, mm), error = function(e) NA_character_)
  list(snapshot = snap, table = built$table, path = path)
}

#' Independent industry-chain mcap-weighted upside job (all chains).
#'
#' Decoupled from Blue Chip page filters (truncate / concepts / N / boards /
#' methods / quality gates). Samples top `per_chain` mcap names per chain,
#' evaluates FV upside, builds + saves the default snapshot.
#'
#' Pure inputs only (safe for ExtendedTask / future workers).
lab_im_run_ind_mcap_job <- function(catlg,
                                    market_mode = "US",
                                    locale = "zh-TW",
                                    per_chain = 2L) {
  mm <- tryCatch(normalize_market_mode(market_mode), error = function(e) "US")
  tryCatch(set_market_mode(mm), error = function(e) NULL)
  loc <- tryCatch(normalize_ui_locale(locale), error = function(e) "zh-TW")
  pc <- suppressWarnings(as.integer(per_chain)[1])
  if (!is.finite(pc) || pc < 1L) pc <- 2L

  empty_out <- function(reason) {
    list(
      ok = FALSE,
      reason = as.character(reason)[1],
      snapshot = lab_ind_mcap_chain_snapshot_empty(),
      table = NULL,
      n_eval = 0L,
      n_chains = 0L,
      path = NA_character_,
      market_mode = mm,
      locale = loc
    )
  }

  if (!is.data.frame(catlg) || nrow(catlg) < 1L) {
    return(empty_out("empty_catalog"))
  }
  if (!exists("industry_chain_id", mode = "function") ||
      !exists("lab_industry_chain_picker_choices", mode = "function") ||
      !exists("lab_screen_tickers_fscore", mode = "function")) {
    return(empty_out("missing_helpers"))
  }

  catlg <- catlg
  if (!"chain_id" %in% names(catlg) ||
      !any(nzchar(as.character(catlg$chain_id)), na.rm = TRUE)) {
    catlg$chain_id <- vapply(
      as.character(catlg$industry_key %||% rep(NA_character_, nrow(catlg))),
      function(k) tryCatch(industry_chain_id(k), error = function(e) ""),
      character(1)
    )
  }
  catlg <- tryCatch(lab_attach_market_caps(catlg), error = function(e) catlg)
  mcap <- suppressWarnings(as.numeric(catlg$market_cap))
  if (!any(is.finite(mcap) & mcap > 0, na.rm = TRUE)) {
    return(empty_out("no_market_cap"))
  }

  chains <- unname(lab_industry_chain_picker_choices(loc))
  chains <- chains[nzchar(as.character(chains)) & !is.na(chains)]
  chains <- chains[!vapply(chains, lab_ind_mcap_is_unmapped_chain, logical(1))]
  if (!length(chains)) return(empty_out("no_chains"))

  pick <- lapply(chains, function(cid) {
    sub <- catlg[
      !is.na(catlg$chain_id) & as.character(catlg$chain_id) == as.character(cid) &
        is.finite(suppressWarnings(as.numeric(catlg$market_cap))) &
        suppressWarnings(as.numeric(catlg$market_cap)) > 0,
      ,
      drop = FALSE
    ]
    if (!nrow(sub)) return(NULL)
    sub <- sub[order(-suppressWarnings(as.numeric(sub$market_cap))), , drop = FALSE]
    utils::head(sub, pc)
  })
  pool <- do.call(rbind, Filter(Negate(is.null), pick))
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) < 1L) {
    return(empty_out("empty_pool"))
  }
  pool <- pool[!duplicated(toupper(as.character(pool$ticker))), , drop = FALSE]

  cols <- intersect(c("ticker", "industry_key", "primary"), names(pool))
  scores <- tryCatch(
    lab_screen_tickers_fscore(
      pool[, cols, drop = FALSE],
      max_n = nrow(pool),
      progress_cb = NULL
    ),
    error = function(e) NULL
  )
  if (!is.data.frame(scores) || nrow(scores) < 1L) {
    return(empty_out("empty_scores"))
  }

  merged <- lab_merge_catalog_scores(
    catlg,
    scores = scores,
    method_filter = character(0),
    industry_filter = character(0),
    eq_only = FALSE,
    gate_only = FALSE,
    evaluated_only = TRUE
  )
  if (!is.data.frame(merged) || nrow(merged) < 1L) {
    return(empty_out("empty_merged"))
  }
  if (!"market_cap" %in% names(merged) ||
      !any(is.finite(suppressWarnings(as.numeric(merged$market_cap))), na.rm = TRUE)) {
    merged <- tryCatch(lab_attach_market_caps(merged), error = function(e) merged)
  }
  if ("market_cap" %in% names(scores)) {
    idx <- match(toupper(merged$ticker), toupper(scores$ticker))
    hit <- which(is.finite(idx))
    sc_m <- suppressWarnings(as.numeric(scores$market_cap[idx[hit]]))
    use <- is.finite(sc_m) & sc_m > 0
    if (any(use)) merged$market_cap[hit[use]] <- sc_m[use]
  }

  built <- tryCatch(
    lab_build_ind_mcap_chain_snapshot(
      merged,
      market = mm,
      eq_only = FALSE,
      gate_only = FALSE,
      no_alert = FALSE,
      locale = loc,
      universe_df = catlg
    ),
    error = function(e) NULL
  )
  if (is.null(built) || !is.data.frame(built$snapshot) || nrow(built$snapshot) < 1L) {
    return(empty_out("build_failed"))
  }

  list(
    ok = TRUE,
    reason = "ok",
    snapshot = built$snapshot,
    table = built$table,
    n_eval = as.integer(nrow(scores)),
    n_chains = as.integer(nrow(built$snapshot)),
    path = as.character(built$path %||% NA_character_)[1],
    market_mode = mm,
    locale = loc
  )
}

