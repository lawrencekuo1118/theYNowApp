# ==========================================
# beta_grouping_module.R — Level Beta grouping (v1)
#
# Research / education risk layering from monthly returns.
# mode = "level" only (K equal-count bins). Not trajectory clustering.
# Does NOT auto-write group medians into CAPM / WACC.
# Spec: docs/beta-clustering-readme-spec.md (Project store)
# ==========================================

BG_DEFAULT_WINDOW <- 24L
BG_DEFAULT_MIN_OBS <- 20L
BG_DEFAULT_YEARS <- 5L
BG_DEFAULT_K <- 3L
BG_SE_WARN <- 0.50
BG_R2_WARN <- 0.10
BG_VAR_EPS <- 1e-12

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.bg_bind_rows <- function(xs) {
  xs <- Filter(Negate(is.null), xs)
  if (!length(xs)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(dplyr::bind_rows(xs))
  }
  do.call(rbind, lapply(xs, as.data.frame, stringsAsFactors = FALSE))
}

#' Floor dates to first calendar day of month
bg_month_floor <- function(dates) {
  d <- as.Date(dates)
  as.Date(format(d, "%Y-%m-01"))
}

#' Locale-aware Low / Mid / High labels (plus optional missing)
bg_group_labels <- function(k = BG_DEFAULT_K, locale = "en", include_missing = FALSE) {
  k <- max(2L, as.integer(k)[1])
  loc <- as.character(locale %||% "en")[1]
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  if (k == 3L) {
    labs <- if (is_zh) {
      c("低 Beta（分位）", "中 Beta（分位）", "高 Beta（分位）")
    } else {
      c("Low Beta (tercile)", "Mid Beta (tercile)", "High Beta (tercile)")
    }
  } else {
    labs <- if (is_zh) {
      paste0("Beta 分位群 ", seq_len(k))
    } else {
      paste0("Beta quantile ", seq_len(k))
    }
  }
  if (isTRUE(include_missing)) {
    labs <- c(
      labs,
      if (is_zh) "β_L 缺值／未入群（資料受限）" else "Missing / not grouped (data-limited)"
    )
  }
  labs
}

bg_normalize_membership <- function(x, default = "fixed") {
  s <- tolower(trimws(as.character(x %||% default)[1]))
  if (!nzchar(s)) s <- default
  if (s %in% c("fixed", "fix", "ex_post", "ex-post", "as_of", "cutoff")) return("fixed")
  if (s %in% c("dynamic", "dyn", "rolling", "period", "逐期", "逐期重分")) return("dynamic")
  default
}

bg_normalize_mode <- function(x, default = "level") {
  s <- tolower(trimws(as.character(x %||% default)[1]))
  if (s %in% c("level", "levels", "bin", "tercile", "quantile")) return("level")
  if (s %in% c("trajectory", "traj", "ts", "path", "cluster_ts")) {
    stop(
      "trajectory Beta clustering is not implemented in v1; use mode = \"level\".",
      call. = FALSE
    )
  }
  default
}

#' Reject duplicate keys and coerce contract columns
bg_validate_stock_returns <- function(stock_returns) {
  if (!is.data.frame(stock_returns)) {
    stop("stock_returns must be a data.frame", call. = FALSE)
  }
  need <- c("date", "symbol", "ret")
  miss <- setdiff(need, names(stock_returns))
  if (length(miss)) {
    stop(paste0("stock_returns missing columns: ", paste(miss, collapse = ", ")), call. = FALSE)
  }
  df <- data.frame(
    date = bg_month_floor(stock_returns$date),
    symbol = as.character(stock_returns$symbol),
    ret = suppressWarnings(as.numeric(stock_returns$ret)),
    stringsAsFactors = FALSE
  )
  df$symbol <- toupper(trimws(df$symbol))
  df <- df[!is.na(df$date) & nzchar(df$symbol), , drop = FALSE]
  key <- paste(df$symbol, as.character(df$date), sep = "\t")
  if (any(duplicated(key))) {
    stop("stock_returns has duplicate (symbol, date) rows", call. = FALSE)
  }
  df
}

bg_validate_market_returns <- function(market_returns) {
  if (!is.data.frame(market_returns)) {
    stop("market_returns must be a data.frame", call. = FALSE)
  }
  need <- c("date", "ret_m")
  miss <- setdiff(need, names(market_returns))
  if (length(miss)) {
    stop(paste0("market_returns missing columns: ", paste(miss, collapse = ", ")), call. = FALSE)
  }
  df <- data.frame(
    date = bg_month_floor(market_returns$date),
    ret_m = suppressWarnings(as.numeric(market_returns$ret_m)),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$date), , drop = FALSE]
  if (any(duplicated(df$date))) {
    stop("market_returns has duplicate date rows", call. = FALSE)
  }
  df[order(df$date), , drop = FALSE]
}

#' OLS slope beta with intercept on paired (ret, ret_m); NA months stay NA (no gap fill)
bg_ols_beta_window <- function(ret, ret_m, min_obs = BG_DEFAULT_MIN_OBS,
                               se_warn = BG_SE_WARN, r2_warn = BG_R2_WARN) {
  ok <- is.finite(ret) & is.finite(ret_m)
  n <- sum(ok)
  out <- list(
    beta = NA_real_, se_beta = NA_real_, r2 = NA_real_,
    n_obs = as.integer(n), valid = FALSE, warnings = character(0)
  )
  if (n < as.integer(min_obs)[1]) return(out)
  y <- ret[ok]
  x <- ret_m[ok]
  vx <- stats::var(x)
  if (!is.finite(vx) || vx <= BG_VAR_EPS) return(out)
  fit <- tryCatch(
    stats::lm(y ~ x),
    error = function(e) NULL
  )
  if (is.null(fit)) return(out)
  cf <- stats::coef(fit)
  beta <- suppressWarnings(as.numeric(cf[["x"]])[1])
  if (!is.finite(beta)) return(out)
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  se <- NA_real_
  r2 <- NA_real_
  warns <- character(0)
  if (!is.null(sm)) {
    ct <- sm$coefficients
    if (!is.null(ct) && "x" %in% rownames(ct)) {
      se <- suppressWarnings(as.numeric(ct["x", "Std. Error"])[1])
    }
    r2 <- suppressWarnings(as.numeric(sm$r.squared)[1])
  }
  if (is.finite(se) && se > se_warn) warns <- c(warns, "high_se")
  if (is.finite(r2) && r2 < r2_warn) warns <- c(warns, "low_r2")
  if (!is.finite(r2)) warns <- c(warns, "r2_unavailable")
  list(
    beta = beta,
    se_beta = if (is.finite(se)) se else NA_real_,
    r2 = if (is.finite(r2)) r2 else NA_real_,
    n_obs = as.integer(n),
    valid = TRUE,
    warnings = warns
  )
}

#' Rolling monthly Beta panel for every symbol × month
#' Window is a fixed calendar span of `window` months ending at t (inclusive).
#' Missing months inside the window stay NA and reduce n_obs (no stretch / future fill).
bg_estimate_beta_panel <- function(stock_returns, market_returns,
                                   window = BG_DEFAULT_WINDOW,
                                   min_obs = BG_DEFAULT_MIN_OBS,
                                   se_warn = BG_SE_WARN,
                                   r2_warn = BG_R2_WARN) {
  st <- bg_validate_stock_returns(stock_returns)
  mk <- bg_validate_market_returns(market_returns)
  window <- as.integer(window)[1]
  min_obs <- as.integer(min_obs)[1]
  if (!is.finite(window) || window < 2L) stop("window must be >= 2", call. = FALSE)
  if (!is.finite(min_obs) || min_obs < 2L) stop("min_obs must be >= 2", call. = FALSE)

  months <- sort(unique(mk$date))
  if (!length(months)) {
    return(data.frame(
      date = as.Date(character()),
      symbol = character(),
      beta = numeric(),
      se_beta = numeric(),
      r2 = numeric(),
      n_obs = integer(),
      valid = logical(),
      warn_flags = character(),
      stringsAsFactors = FALSE
    ))
  }

  symbols <- sort(unique(st$symbol))
  # Wide ret matrix: rows = months, cols = symbols (NA where missing)
  ret_mat <- matrix(NA_real_, nrow = length(months), ncol = length(symbols),
                    dimnames = list(as.character(months), symbols))
  for (i in seq_along(symbols)) {
    sub <- st[st$symbol == symbols[[i]], , drop = FALSE]
    idx <- match(sub$date, months)
    ok <- !is.na(idx)
    ret_mat[idx[ok], i] <- sub$ret[ok]
  }
  ret_m <- mk$ret_m[match(months, mk$date)]

  rows <- vector("list", length(months) * length(symbols))
  ri <- 0L
  for (ti in seq_along(months)) {
    # Calendar window [t - (window-1), t] by position on market calendar
    i0 <- ti - window + 1L
    if (i0 < 1L) {
      # Left edge incomplete: still estimate with available months; min_obs gates validity
      i0 <- 1L
    }
    # Spec: full 24 calendar-month window required conceptually; if start truncated,
    # n_obs reflects only paired months present (no extension past available history).
    # When ti < window, calendar span is shorter than window → typically invalid via min_obs.
    span_ok <- (ti - i0 + 1L) >= window
    for (si in seq_along(symbols)) {
      ri <- ri + 1L
      if (!span_ok) {
        rows[[ri]] <- data.frame(
          date = months[[ti]],
          symbol = symbols[[si]],
          beta = NA_real_,
          se_beta = NA_real_,
          r2 = NA_real_,
          n_obs = 0L,
          valid = FALSE,
          warn_flags = "",
          stringsAsFactors = FALSE
        )
        next
      }
      y <- ret_mat[i0:ti, si]
      x <- ret_m[i0:ti]
      est <- bg_ols_beta_window(y, x, min_obs = min_obs, se_warn = se_warn, r2_warn = r2_warn)
      rows[[ri]] <- data.frame(
        date = months[[ti]],
        symbol = symbols[[si]],
        beta = est$beta,
        se_beta = est$se_beta,
        r2 = est$r2,
        n_obs = est$n_obs,
        valid = isTRUE(est$valid),
        warn_flags = paste(est$warnings, collapse = ","),
        stringsAsFactors = FALSE
      )
    }
  }
  .bg_bind_rows(rows)
}

#' Equal-count K-bin assignment; ties broken by symbol sort; identical beta → warning
bg_assign_quantile_groups <- function(symbol, beta, k = BG_DEFAULT_K) {
  symbol <- as.character(symbol)
  beta <- suppressWarnings(as.numeric(beta))
  k <- as.integer(k)[1]
  n <- length(beta)
  group_id <- rep(NA_integer_, n)
  tie_warn <- FALSE
  ok <- is.finite(beta) & nzchar(symbol)
  if (sum(ok) < 2L * k) {
    return(list(group_id = group_id, tie_warn = FALSE, n_valid = as.integer(sum(ok))))
  }
  ord <- order(beta[ok], symbol[ok], method = "radix")
  idx <- which(ok)[ord]
  nv <- length(idx)
  # Near-equal counts: assign ranks 1..nv then cut into k buckets
  ranks <- seq_len(nv)
  # Use floor breaks so sizes differ by at most 1
  breaks <- floor(seq(0, nv, length.out = k + 1L))
  gid <- integer(nv)
  for (g in seq_len(k)) {
    lo <- breaks[[g]] + 1L
    hi <- breaks[[g + 1L]]
    if (lo <= hi) gid[lo:hi] <- g
  }
  group_id[idx] <- gid
  # Tie across boundary: same beta on both sides of a cut
  if (nv >= 2L) {
    for (g in seq_len(k - 1L)) {
      cut_at <- breaks[[g + 1L]]
      if (cut_at >= 1L && cut_at < nv) {
        if (isTRUE(all.equal(beta[idx[cut_at]], beta[idx[cut_at + 1L]]))) {
          tie_warn <- TRUE
        }
      }
    }
  }
  list(group_id = group_id, tie_warn = tie_warn, n_valid = as.integer(nv))
}

.bg_empty_summary <- function() {
  data.frame(
    date = as.Date(character()),
    group_id = integer(),
    median_beta = numeric(),
    q25 = numeric(),
    q75 = numeric(),
    n_assets = integer(),
    stringsAsFactors = FALSE
  )
}

.bg_empty_members <- function() {
  data.frame(
    date = as.Date(character()),
    symbol = character(),
    beta = numeric(),
    se_beta = numeric(),
    r2 = numeric(),
    n_obs = integer(),
    valid = logical(),
    warn_flags = character(),
    group_id = integer(),
    stringsAsFactors = FALSE
  )
}

.bg_summarize_month <- function(members_m, k) {
  out <- .bg_empty_summary()
  if (!nrow(members_m)) return(out)
  d <- members_m$date[[1]]
  rows <- lapply(seq_len(k), function(g) {
    sub <- members_m[!is.na(members_m$group_id) & members_m$group_id == g, , drop = FALSE]
    n <- nrow(sub)
    if (n < 1L) {
      return(data.frame(
        date = d, group_id = as.integer(g),
        median_beta = NA_real_, q25 = NA_real_, q75 = NA_real_,
        n_assets = 0L, stringsAsFactors = FALSE
      ))
    }
    b <- sub$beta
    data.frame(
      date = d,
      group_id = as.integer(g),
      median_beta = stats::median(b, na.rm = TRUE),
      q25 = as.numeric(stats::quantile(b, 0.25, na.rm = TRUE, names = FALSE, type = 7)),
      q75 = as.numeric(stats::quantile(b, 0.75, na.rm = TRUE, names = FALSE, type = 7)),
      n_assets = as.integer(n),
      stringsAsFactors = FALSE
    )
  })
  .bg_bind_rows(rows)
}

#' Pure compute API — level Beta grouping
#'
#' @param stock_returns data.frame(date, symbol, ret)
#' @param market_returns data.frame(date, ret_m)
#' @param mode only "level" in v1
#' @param membership "fixed" (default) or "dynamic"
#' @param as_of Date month for fixed membership + display end
bg_calculate <- function(stock_returns, market_returns,
                         mode = "level",
                         k = BG_DEFAULT_K,
                         window = BG_DEFAULT_WINDOW,
                         min_obs = BG_DEFAULT_MIN_OBS,
                         years = BG_DEFAULT_YEARS,
                         membership = "fixed",
                         as_of = NULL,
                         se_warn = BG_SE_WARN,
                         r2_warn = BG_R2_WARN,
                         locale = "en") {
  mode <- bg_normalize_mode(mode)
  membership <- bg_normalize_membership(membership)
  k <- as.integer(k)[1]
  years <- as.integer(years)[1]
  if (!is.finite(k) || k < 2L) stop("k must be >= 2", call. = FALSE)

  panel <- bg_estimate_beta_panel(
    stock_returns, market_returns,
    window = window, min_obs = min_obs,
    se_warn = se_warn, r2_warn = r2_warn
  )
  if (!nrow(panel)) {
    return(list(
      summary = .bg_empty_summary(),
      members = .bg_empty_members(),
      meta = list(
        mode = mode, membership = membership, k = k, window = window,
        min_obs = min_obs, years = years, as_of = as.Date(NA),
        n_excluded = 0L, tie_warn = FALSE, labels = bg_group_labels(k, locale),
        note = "empty_panel"
      )
    ))
  }

  all_months <- sort(unique(panel$date))
  if (is.null(as_of) || !length(as_of) || is.na(as.Date(as_of)[1])) {
    as_of <- max(all_months)
  } else {
    as_of <- bg_month_floor(as.Date(as_of)[1])
  }
  if (!as_of %in% all_months) {
    # Snap to latest market month on or before as_of
    cand <- all_months[all_months <= as_of]
    if (!length(cand)) {
      stop("as_of precedes all available Beta months", call. = FALSE)
    }
    as_of <- max(cand)
  }

  display_start <- bg_month_floor(seq(as_of, by = paste0("-", years, " years"), length.out = 2L)[2])
  display_months <- all_months[all_months >= display_start & all_months <= as_of]

  members_out <- .bg_empty_members()
  summary_out <- .bg_empty_summary()
  tie_any <- FALSE
  n_excl_asof <- 0L

  if (identical(membership, "fixed")) {
    asof_rows <- panel[panel$date == as_of, , drop = FALSE]
    valid_asof <- asof_rows[asof_rows$valid %in% TRUE, , drop = FALSE]
    n_excl_asof <- nrow(asof_rows) - nrow(valid_asof)
    asg <- bg_assign_quantile_groups(valid_asof$symbol, valid_asof$beta, k = k)
    tie_any <- isTRUE(asg$tie_warn)
    if (asg$n_valid < 2L * k) {
      return(list(
        summary = .bg_empty_summary(),
        members = .bg_empty_members(),
        meta = list(
          mode = mode, membership = membership, k = k, window = window,
          min_obs = min_obs, years = years, as_of = as_of,
          n_excluded = as.integer(n_excl_asof),
          tie_warn = FALSE,
          labels = bg_group_labels(k, locale),
          note = "insufficient_valid_as_of",
          n_valid_as_of = asg$n_valid
        )
      ))
    }
    map <- stats::setNames(asg$group_id, valid_asof$symbol)
    # Fixed: same members across history; beta from each month's panel
    mem_list <- lapply(display_months, function(d) {
      m <- panel[panel$date == d & panel$symbol %in% names(map), , drop = FALSE]
      if (!nrow(m)) return(NULL)
      m$group_id <- unname(map[m$symbol])
      # Keep only as-of valid members; historical invalid beta stays with NA group stats
      m
    })
    members_out <- .bg_bind_rows(mem_list)
    # Summary uses only finite beta rows that are valid that month
    sum_list <- lapply(display_months, function(d) {
      m <- members_out[members_out$date == d, , drop = FALSE]
      m_ok <- m[isTRUE(m$valid) | m$valid %in% TRUE, , drop = FALSE]
      m_ok <- m[m$valid %in% TRUE & is.finite(m$beta) & !is.na(m$group_id), , drop = FALSE]
      .bg_summarize_month(m_ok, k)
    })
    summary_out <- .bg_bind_rows(sum_list)
  } else {
    # dynamic: re-rank each month independently (no future info)
    mem_list <- list()
    sum_list <- list()
    for (d in display_months) {
      m <- panel[panel$date == d, , drop = FALSE]
      valid_m <- m[m$valid %in% TRUE, , drop = FALSE]
      asg <- bg_assign_quantile_groups(valid_m$symbol, valid_m$beta, k = k)
      if (isTRUE(asg$tie_warn)) tie_any <- TRUE
      m$group_id <- NA_integer_
      if (asg$n_valid >= 2L * k) {
        m$group_id[match(valid_m$symbol, m$symbol)] <- asg$group_id
      }
      mem_list[[length(mem_list) + 1L]] <- m
      m_ok <- m[!is.na(m$group_id) & m$valid %in% TRUE, , drop = FALSE]
      sum_list[[length(sum_list) + 1L]] <- .bg_summarize_month(m_ok, k)
      if (identical(d, as_of)) {
        n_excl_asof <- nrow(m) - nrow(valid_m)
      }
    }
    members_out <- .bg_bind_rows(mem_list)
    summary_out <- .bg_bind_rows(sum_list)
  }

  if (nrow(summary_out)) {
    summary_out <- summary_out[order(summary_out$date, summary_out$group_id), , drop = FALSE]
  }
  if (nrow(members_out)) {
    members_out <- members_out[order(members_out$date, members_out$group_id, members_out$symbol), , drop = FALSE]
  }

  list(
    summary = summary_out,
    members = members_out,
    meta = list(
      mode = mode,
      membership = membership,
      k = k,
      window = window,
      min_obs = min_obs,
      years = years,
      as_of = as_of,
      display_start = if (length(display_months)) min(display_months) else as.Date(NA),
      n_excluded = as.integer(n_excl_asof),
      tie_warn = isTRUE(tie_any),
      labels = bg_group_labels(k, locale),
      note = "ok",
      forward_looking = identical(membership, "fixed"),
      not_tradable_backtest = TRUE
    )
  )
}

#' Equal-count tercile assignment from a single cross-section of β_L
#' (Lab fallback when monthly returns are unavailable).
bg_assign_level_from_betas <- function(symbol, beta, k = BG_DEFAULT_K, locale = "en") {
  symbol <- as.character(symbol)
  beta <- suppressWarnings(as.numeric(beta))
  asg <- bg_assign_quantile_groups(symbol, beta, k = k)
  labs <- bg_group_labels(k, locale, include_missing = TRUE)
  miss_id <- as.integer(k + 1L)
  gid <- asg$group_id
  gid[is.na(gid)] <- miss_id
  list(
    group_id = as.integer(gid),
    group_label = labs[gid],
    tie_warn = isTRUE(asg$tie_warn),
    n_valid = asg$n_valid,
    k = as.integer(k),
    labels = labs
  )
}

#' Build synthetic monthly stock + market returns for tests / offline demos
bg_synthetic_returns <- function(n_symbols = 30L, n_months = 90L,
                                 as_of = as.Date("2026-10-01"),
                                 seed = 42L,
                                 true_betas = NULL) {
  set.seed(as.integer(seed)[1])
  as_of <- bg_month_floor(as_of)
  months <- seq(as_of, by = "-1 month", length.out = as.integer(n_months)[1])
  months <- sort(bg_month_floor(months))
  n_symbols <- as.integer(n_symbols)[1]
  symbols <- sprintf("S%02d", seq_len(n_symbols))
  if (is.null(true_betas)) {
    true_betas <- seq(0.4, 1.8, length.out = n_symbols)
  }
  true_betas <- as.numeric(true_betas)
  if (length(true_betas) != n_symbols) {
    stop("true_betas length must equal n_symbols", call. = FALSE)
  }
  ret_m <- stats::rnorm(length(months), mean = 0.008, sd = 0.04)
  market_returns <- data.frame(date = months, ret_m = ret_m, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(n_symbols), function(i) {
    eps <- stats::rnorm(length(months), sd = 0.03)
    data.frame(
      date = months,
      symbol = symbols[[i]],
      ret = true_betas[[i]] * ret_m + eps,
      stringsAsFactors = FALSE
    )
  })
  stock_returns <- .bg_bind_rows(rows)
  list(
    stock_returns = stock_returns,
    market_returns = market_returns,
    true_betas = stats::setNames(true_betas, symbols),
    as_of = as_of,
    symbols = symbols
  )
}

#' Plotly median + optional IQR bands for Lab map panel
bg_summary_plotly <- function(grouped, show_iqr = FALSE, locale = "en") {
  if (is.null(grouped) || !is.list(grouped) || is.null(grouped$summary)) {
    return(plotly::plotly_empty())
  }
  sm <- grouped$summary
  meta <- grouped$meta %||% list()
  labs <- as.character(meta$labels %||% bg_group_labels(meta$k %||% 3L, locale))
  loc <- as.character(locale %||% "en")[1]
  is_zh <- grepl("^zh", tolower(loc), perl = TRUE)
  if (!is.data.frame(sm) || nrow(sm) < 1L) {
    msg <- if (is_zh) "尚無 Beta 分群時間序列" else "No Beta grouping time series yet"
    return(
      plotly::plotly_empty() %>%
        plotly::layout(annotations = list(list(
          text = msg, showarrow = FALSE, font = list(size = 14)
        )))
    )
  }
  sm$label <- labs[pmax(1L, pmin(length(labs), as.integer(sm$group_id)))]
  colors <- c("#2c7bb6", "#fdae61", "#d7191c", "#7b3294", "#1a9641")
  p <- plotly::plot_ly()
  for (g in sort(unique(as.integer(sm$group_id)))) {
    sub <- sm[as.integer(sm$group_id) == g, , drop = FALSE]
    col <- colors[((g - 1L) %% length(colors)) + 1L]
    nm <- sub$label[[1]]
    if (isTRUE(show_iqr)) {
      p <- plotly::add_ribbons(
        p, data = sub, x = ~date, ymin = ~q25, ymax = ~q75,
        name = paste0(nm, " IQR"),
        line = list(color = "transparent"),
        fillcolor = paste0(col, "33"),
        showlegend = TRUE,
        hoverinfo = "skip"
      )
    }
    p <- plotly::add_lines(
      p, data = sub, x = ~date, y = ~median_beta,
      name = nm,
      line = list(color = col, width = 2),
      text = ~paste0("n=", n_assets),
      hovertemplate = paste0(
        "%{x|%Y-%m}<br>", nm, "<br>median β=%{y:.3f}<br>%{text}<extra></extra>"
      )
    )
  }
  ylab <- if (is_zh) "組內 Beta 中位數" else "Group median Beta"
  xlab <- if (is_zh) "月份" else "Month"
  title <- if (is_zh) {
    mem <- if (identical(meta$membership, "dynamic")) "逐期重分" else "固定群組（截止月）"
    paste0("Beta 分位群中位數（", mem, "）")
  } else {
    mem <- if (identical(meta$membership, "dynamic")) "dynamic membership" else "fixed membership (as-of)"
    paste0("Beta quantile medians (", mem, ")")
  }
  plotly::layout(
    p,
    title = list(text = title, font = list(size = 14)),
    xaxis = list(title = xlab),
    yaxis = list(title = ylab, zeroline = TRUE),
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 40, b = 60)
  )
}

# --- Optional standalone Shiny helpers (do not auto-mount in Lab) ---

betaGroupingUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          ns("membership"), "Membership",
          choices = c("Fixed (as-of)" = "fixed", "Dynamic (re-rank)" = "dynamic"),
          selected = "fixed"
        )
      ),
      shiny::column(
        3,
        shiny::dateInput(ns("as_of"), "As-of month", value = bg_month_floor(Sys.Date()))
      ),
      shiny::column(
        3,
        shiny::checkboxInput(ns("show_iqr"), "Show Q25–Q75 band", value = FALSE)
      ),
      shiny::column(
        3,
        shiny::downloadButton(ns("dl_csv"), "Download CSV")
      )
    ),
    shiny::plotOutput(ns("plot"), height = "420px"),
    shiny::verbatimTextOutput(ns("meta"))
  )
}

betaGroupingServer <- function(id, stock_returns, market_returns,
                               k = BG_DEFAULT_K, window = BG_DEFAULT_WINDOW,
                               min_obs = BG_DEFAULT_MIN_OBS, years = BG_DEFAULT_YEARS) {
  shiny::moduleServer(id, function(input, output, session) {
    grouped <- shiny::reactive({
      st <- if (is.function(stock_returns)) stock_returns() else stock_returns
      mk <- if (is.function(market_returns)) market_returns() else market_returns
      bg_calculate(
        st, mk,
        mode = "level", k = k, window = window, min_obs = min_obs,
        years = years,
        membership = bg_normalize_membership(input$membership),
        as_of = input$as_of
      )
    })
    output$plot <- shiny::renderPlot({
      g <- grouped()
      sm <- g$summary
      if (!nrow(sm)) {
        plot.new()
        title("No Beta grouping summary")
        return(invisible())
      }
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(sm$date, sm$median_beta, type = "n", xlab = "Month", ylab = "Median Beta")
        for (gid in unique(sm$group_id)) {
          sub <- sm[sm$group_id == gid, , drop = FALSE]
          lines(sub$date, sub$median_beta)
        }
        return(invisible())
      }
      labs <- g$meta$labels
      sm$label <- factor(labs[sm$group_id], levels = labs)
      p <- ggplot2::ggplot(sm, ggplot2::aes(date, median_beta, color = label, fill = label)) +
        ggplot2::geom_line(linewidth = 1)
      if (isTRUE(input$show_iqr)) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA
        )
      }
      p +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = "Month", y = "Group median Beta", color = NULL, fill = NULL)
    })
    output$meta <- shiny::renderPrint({
      str(grouped()$meta)
    })
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0("beta_grouping_", format(Sys.Date()), ".csv"),
      content = function(file) {
        utils::write.csv(grouped()$summary, file, row.names = FALSE, na = "")
      }
    )
    list(grouped = grouped)
  })
}
