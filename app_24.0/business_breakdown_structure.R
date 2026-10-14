# Business & Profitability Structure Analysis
# Separates reportable BUSINESS segments from accounting / consolidation adjustments.
# Never invents Segment Operating Income or allocates Corporate expenses by revenue share.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

BBLAB_ECONOMIC_ROLES <- c(
  "BUSINESS",
  "OTHER_OPERATING",
  "CORPORATE",
  "ELIMINATION",
  "RECONCILIATION",
  "NON_OPERATING",
  "ACCOUNTING_ADJUSTMENT"
)

#' Classify a disclosure line by economic role (not merely by amount existence).
#' Ask first: "Is this an actual operating business the company runs?"
bblab_classify_economic_role <- function(name,
                                         classification = NA_character_,
                                         is_reported_segment = FALSE,
                                         source_type = NA_character_) {
  s <- if (exists(".bblab_norm_label", mode = "function")) {
    .bblab_norm_label(name)
  } else {
    tolower(trimws(.bblab_chr(name)))
  }
  cls <- toupper(.bblab_chr(classification, ""))

  if (cls %in% c("ELIMINATION") ||
      grepl("\\bintersegment\\b|\\bintercompany\\b|\\beliminations?\\b|\\belimination of\\b", s)) {
    return("ELIMINATION")
  }
  if (grepl(paste(
    "purchase accounting", "acquisition related", "acquisition-related",
    "fair value adjustment", "mark to market", "mark-to-market",
    "accounting adjustment", "restructuring adjustment",
    "one time gain", "one-time gain", "one time loss", "one-time loss",
    "asset sale gain", "asset sale loss", "impairment",
    sep = "|"
  ), s)) {
    return("ACCOUNTING_ADJUSTMENT")
  }
  if (cls %in% c("RECONCILIATION", "ROUNDING") ||
      grepl(paste(
        "reconcil", "consolidation adjustment", "consolidating adjustment",
        "segment reconcil", "reconciling item", "bridge to consolidated",
        sep = "|"
      ), s)) {
    return("RECONCILIATION")
  }
  if (cls %in% c("UNALLOCATED", "SHARED_CORPORATE") ||
      grepl(paste(
        "^corporate$", "^corporate /", "corporate and other", "corporate other",
        "corporate expenses?", "corporate unallocated", "unallocated",
        "shared services", "headquarters", "hq costs?",
        sep = "|"
      ), s)) {
    return("CORPORATE")
  }
  if (grepl(paste(
    "investment income", "investment gain", "investment loss",
    "interest income", "interest expense", "non operating", "non-operating",
    "other non operating", "other income expense", "equity in earnings",
    "gain on sale of", "loss on sale of",
    sep = "|"
  ), s) && !isTRUE(is_reported_segment)) {
    return("NON_OPERATING")
  }
  if (cls %in% c("OTHER") ||
      grepl("^other$|^others$|^all other|^other segments?$|^other businesses?$|^other operations?$", s)) {
    # "Other" alone is ambiguous — treat as OTHER_OPERATING (may be a catch-all
    # operating bucket) unless already tagged as recon/corporate above.
    return("OTHER_OPERATING")
  }
  if (cls %in% c("MAJOR") || isTRUE(is_reported_segment) ||
      identical(.bblab_chr(source_type), "operating_segment") ||
      identical(.bblab_chr(source_type), "segment_note")) {
    return("BUSINESS")
  }
  if (nzchar(s)) return("BUSINESS")
  "RECONCILIATION"
}

.bblab_role_type_label <- function(role) {
  switch(as.character(role %||% ""),
    BUSINESS = "Business segment",
    OTHER_OPERATING = "Other operating",
    CORPORATE = "Corporate / unallocated",
    ELIMINATION = "Intersegment elimination",
    RECONCILIATION = "Segment reconciliation",
    NON_OPERATING = "Non-operating",
    ACCOUNTING_ADJUSTMENT = "Accounting adjustment",
    "Adjustment"
  )
}

.bblab_role_explanation <- function(role, name = "") {
  base <- switch(as.character(role %||% ""),
    BUSINESS = "Reportable / operating business used in the main economic structure.",
    OTHER_OPERATING = paste0(
      "Operating activity disclosed by the filer but not treated as a principal ",
      "reportable segment in the main business table."
    ),
    CORPORATE = "Company-level / unallocated items; not a separate operating business.",
    ELIMINATION = "Intersegment or intercompany elimination in consolidation.",
    RECONCILIATION = "Bridge item between segment totals and consolidated statements.",
    NON_OPERATING = "Non-operating item; not core product/service operating revenue.",
    ACCOUNTING_ADJUSTMENT = "Accounting, fair-value, acquisition, or one-time adjustment.",
    "Not classified as a principal operating business."
  )
  nm <- .bblab_chr(name)
  if (nzchar(nm)) paste0(base, " Label: ", nm, ".") else base
}

.bblab_collect_structure_items <- function(res) {
  items <- list()
  push <- function(comp, default_cls = "MAJOR") {
    if (is.null(comp) || !is.list(comp)) return()
    role <- .bblab_chr(comp$economic_role, "")
    if (!nzchar(role) || !role %in% BBLAB_ECONOMIC_ROLES) {
      role <- bblab_classify_economic_role(
        comp$name %||% comp$id,
        classification = comp$classification %||% default_cls,
        is_reported_segment = isTRUE(comp$is_reported_segment),
        source_type = comp$source_type
      )
    }
    items[[length(items) + 1L]] <<- list(
      id = .bblab_chr(comp$id, .bblab_slug(comp$name %||% "item", "struct")),
      name = .bblab_chr(comp$name, comp$id),
      role = role,
      classification = .bblab_chr(comp$classification, default_cls),
      revenue = .bblab_num(comp$revenue),
      operating_income = .bblab_num(comp$operating_income),
      is_reported_segment = isTRUE(comp$is_reported_segment),
      oi_disclosed = .bblab_finite(comp$operating_income)
    )
  }
  for (c in res$businesses %||% list()) push(c, "MAJOR")
  if (!is.null(res$other)) {
    if (is.list(res$other$members) && length(res$other$members)) {
      for (m in res$other$members) push(m, "OTHER")
    } else {
      push(res$other, "OTHER")
    }
  }
  for (nm in c("unallocated", "eliminations", "rounding", "recon_amount")) {
    comp <- res[[nm]]
    if (!is.null(comp) && is.list(comp$members) && length(comp$members)) {
      for (m in comp$members) push(m, comp$classification %||% "RECONCILIATION")
    } else {
      push(comp, switch(nm,
        unallocated = "UNALLOCATED",
        eliminations = "ELIMINATION",
        rounding = "ROUNDING",
        recon_amount = "RECONCILIATION",
        "RECONCILIATION"
      ))
    }
  }
  for (c in res$adjustment_members %||% list()) push(c, "RECONCILIATION")
  for (c in res$shared_corporate %||% list()) {
    if (is.list(c) && !is.null(c$name)) {
      push(c, "SHARED_CORPORATE")
    }
  }
  items
}

.bblab_pick_extremes <- function(rows, value_fn, share_fn = NULL) {
  if (!length(rows)) return(character(0))
  vals <- vapply(rows, value_fn, numeric(1))
  ok <- which(is.finite(vals))
  if (!length(ok)) return(character(0))
  rows[ok[order(-vals[ok])]]
}

#' Build Business / Profit / Adjustment structure from a bblab_analyze result.
#' Never fabricates Segment Operating Income.
bblab_build_structure_analysis <- function(res, locale = "en") {
  empty <- list(
    ok = FALSE,
    businesses = list(),
    adjustments = list(),
    conclusions = list(),
    summary_sentence = "",
    missing = character(0)
  )
  if (is.null(res) || !is.list(res)) return(empty)
  cons <- res$consolidated %||% list()
  cons_rev <- .bblab_num(cons$revenue)
  cons_oi <- .bblab_num(cons$operating_income)

  items <- .bblab_collect_structure_items(res)
  if (!length(items)) {
    empty$missing <- c(
      "No reportable business segments or product/service revenue split was disclosed ",
      "in the retrieved statements."
    )
    empty$summary_sentence <- paste(
      "Cannot determine this company's economic operating structure from the",
      "reported financial statements; segment / business disclosures are insufficient."
    )
    return(empty)
  }

  biz <- Filter(function(x) identical(x$role, "BUSINESS"), items)
  adj <- Filter(function(x) !identical(x$role, "BUSINESS"), items)

  # Prefer reportable-segment businesses when present; keep all BUSINESS otherwise.
  reported <- Filter(function(x) isTRUE(x$is_reported_segment), biz)
  if (length(reported) >= 1L) biz <- reported

  business_rows <- lapply(biz, function(x) {
    rev <- x$revenue
    rev_pct <- if (.bblab_finite(rev) && .bblab_finite(cons_rev) && cons_rev != 0) {
      rev / cons_rev
    } else NA_real_
    oi <- x$operating_income
    margin <- if (.bblab_finite(oi) && .bblab_finite(rev) && rev != 0) oi / rev else NA_real_
    profit_pct <- if (.bblab_finite(oi) && .bblab_finite(cons_oi) && cons_oi != 0) {
      oi / cons_oi
    } else NA_real_
    list(
      business = x$name,
      revenue = rev,
      revenue_pct = rev_pct,
      operating_income = oi,
      operating_income_disclosed = isTRUE(x$oi_disclosed),
      margin = margin,
      profit_contribution = profit_pct,
      profit_status = if (isTRUE(x$oi_disclosed)) "REPORTED" else "NOT_DISCLOSED"
    )
  })

  adjustment_rows <- lapply(adj, function(x) {
    amt <- if (.bblab_finite(x$revenue)) x$revenue else x$operating_income
    list(
      adjustment = x$name,
      amount = amt,
      type = x$role,
      type_label = .bblab_role_type_label(x$role),
      explanation = .bblab_role_explanation(x$role, x$name)
    )
  })

  missing <- character(0)
  if (!.bblab_finite(cons_rev)) {
    missing <- c(missing, "Consolidated revenue not available for contribution %.")
  }
  any_oi <- any(vapply(business_rows, function(r) isTRUE(r$operating_income_disclosed), logical(1)))
  if (!any_oi) {
    missing <- c(missing, "Segment profitability not disclosed.")
  }
  if (!.bblab_finite(cons_oi) && any_oi) {
    missing <- c(
      missing,
      "Consolidated operating income not available; profit contribution % uses segment OI only when disclosed."
    )
  }

  # Conclusions — only from disclosed figures; never invent.
  by_rev <- .bblab_pick_extremes(business_rows, function(r) .bblab_num(r$revenue, -Inf))
  by_oi <- .bblab_pick_extremes(
    Filter(function(r) isTRUE(r$operating_income_disclosed), business_rows),
    function(r) .bblab_num(r$operating_income, -Inf)
  )
  names_of <- function(rows, n = 3L) {
    if (!length(rows)) return(character(0))
    vapply(rows[seq_len(min(n, length(rows)))], function(r) r$business, character(1))
  }
  high_rev_low_profit <- character(0)
  high_profit_low_rev <- character(0)
  if (length(business_rows) >= 2L && any_oi) {
    rev_ok <- Filter(function(r) .bblab_finite(r$revenue_pct), business_rows)
    oi_ok <- Filter(function(r) isTRUE(r$operating_income_disclosed) &&
                      .bblab_finite(r$profit_contribution), business_rows)
    if (length(rev_ok) && length(oi_ok)) {
      for (r in rev_ok) {
        if (!isTRUE(r$operating_income_disclosed)) next
        if (.bblab_finite(r$revenue_pct) && .bblab_finite(r$margin) &&
            r$revenue_pct >= 0.20 && r$margin < 0.10) {
          high_rev_low_profit <- c(high_rev_low_profit, r$business)
        }
        if (.bblab_finite(r$revenue_pct) && .bblab_finite(r$profit_contribution) &&
            r$profit_contribution >= 0.20 && r$revenue_pct < 0.15) {
          high_profit_low_rev <- c(high_profit_low_rev, r$business)
        }
      }
    }
  }

  adj_corp <- Filter(function(a) a$type %in% c("CORPORATE", "ELIMINATION", "RECONCILIATION"),
                     adjustment_rows)
  adj_nonop <- Filter(function(a) a$type %in% c("NON_OPERATING", "ACCOUNTING_ADJUSTMENT"),
                      adjustment_rows)

  nd <- function(x) if (!length(x)) "Not disclosed" else paste(unique(x), collapse = "; ")

  conclusions <- list(
    primary_revenue = nd(names_of(by_rev)),
    primary_profit = if (!any_oi) "Segment profitability not disclosed" else nd(names_of(by_oi)),
    high_revenue_low_profit = nd(high_rev_low_profit),
    high_profit_low_revenue = nd(high_profit_low_rev),
    corporate_consolidation = nd(vapply(adj_corp, function(a) a$adjustment, character(1))),
    non_operating = nd(vapply(adj_nonop, function(a) a$adjustment, character(1)))
  )

  biz_names <- vapply(business_rows, function(r) r$business, character(1))
  adj_names <- vapply(adjustment_rows, function(a) a$adjustment, character(1))
  if (length(biz_names) && length(adj_names)) {
    summary_sentence <- sprintf(
      paste(
        "This company's economic operating structure is primarily composed of %s,",
        "while %s are accounting / consolidation / non-operating adjustments and",
        "should not be interpreted as separate operating businesses."
      ),
      paste(biz_names, collapse = ", "),
      paste(adj_names, collapse = ", ")
    )
  } else if (length(biz_names)) {
    summary_sentence <- sprintf(
      paste(
        "This company's economic operating structure is primarily composed of %s.",
        "No material corporate / consolidation / non-operating adjustment lines",
        "were separated from the main businesses in the retrieved disclosures."
      ),
      paste(biz_names, collapse = ", ")
    )
  } else {
    summary_sentence <- paste(
      "Cannot determine this company's economic operating structure from the",
      "reported financial statements; disclosed lines appear to be adjustments only."
    )
  }

  # Bridge note when segment OI sum may not equal consolidated OI
  bridge_note <- NULL
  if (any_oi) {
    sum_oi <- sum(vapply(business_rows, function(r) {
      if (isTRUE(r$operating_income_disclosed)) .bblab_num(r$operating_income, 0) else 0
    }, numeric(1)))
    if (.bblab_finite(cons_oi) && is.finite(sum_oi) &&
        abs(sum_oi - cons_oi) > max(1, abs(cons_oi) * 0.005)) {
      bridge_note <- paste(
        "Sum of disclosed Segment Operating Income does not equal Consolidated",
        "Operating Income; corporate / eliminations / other reconciliation items explain the bridge."
      )
    } else if (!.bblab_finite(cons_oi)) {
      bridge_note <- paste(
        "Segment Operating Income is shown when disclosed; Consolidated Operating",
        "Income was not available to complete the profit bridge."
      )
    }
  }

  list(
    ok = length(business_rows) >= 1L || length(adjustment_rows) >= 1L,
    businesses = business_rows,
    adjustments = adjustment_rows,
    conclusions = conclusions,
    summary_sentence = summary_sentence,
    bridge_note = bridge_note,
    missing = missing,
    consolidated_revenue = cons_rev,
    consolidated_operating_income = cons_oi
  )
}

# =============================================================================
# Classic BCG matrix (internal portfolio adaptation)
# X = relative revenue share vs largest business (proxy for relative market share)
# Y = segment revenue CAGR from multi-year disclosures (proxy for market growth)
# Bubble size = current revenue. Never invents growth when history is insufficient.
# =============================================================================

.BBLAB_BCG_QUADRANTS <- c("star", "cash_cow", "question_mark", "dog")

.bblab_bcg_cagr <- function(revenues_named) {
  if (is.null(revenues_named) || !length(revenues_named)) return(NA_real_)
  yrs <- suppressWarnings(as.integer(names(revenues_named)))
  vals <- suppressWarnings(as.numeric(unname(revenues_named)))
  ok <- is.finite(yrs) & is.finite(vals) & vals > 0
  if (sum(ok) < 2L) return(NA_real_)
  yrs <- yrs[ok]
  vals <- vals[ok]
  ord <- order(yrs)
  yrs <- yrs[ord]
  vals <- vals[ord]
  # Prefer first→last span; skip if years identical
  n <- length(yrs)
  y0 <- yrs[[1]]
  y1 <- yrs[[n]]
  span <- as.numeric(y1 - y0)
  if (!is.finite(span) || span <= 0) return(NA_real_)
  (vals[[n]] / vals[[1]])^(1 / span) - 1
}

.bblab_bcg_match_history_series <- function(business_name, history) {
  if (is.null(history) || !is.list(history) || !length(history$series)) return(NULL)
  key_fn <- if (exists(".bblab_history_match_key", mode = "function")) {
    .bblab_history_match_key
  } else {
    function(nm) tolower(trimws(as.character(nm %||% "")[1]))
  }
  target <- key_fn(business_name)
  for (s in history$series) {
    if (identical(key_fn(s$name %||% ""), target)) return(s)
  }
  NULL
}

.bblab_bcg_assign_quadrant <- function(rel_share, growth, x_mid = 0.5, y_mid = 0.05) {
  if (!is.finite(rel_share) || !is.finite(growth)) return(NA_character_)
  high_share <- rel_share >= x_mid
  high_growth <- growth >= y_mid
  if (isTRUE(high_share) && isTRUE(high_growth)) return("star")
  if (isTRUE(high_share) && !isTRUE(high_growth)) return("cash_cow")
  if (!isTRUE(high_share) && isTRUE(high_growth)) return("question_mark")
  "dog"
}

#' Build classic BCG matrix inputs from structure analysis + history.
#' @param structure_analysis result of bblab_build_structure_analysis()
#' @param history res$history from bblab_analyze() (optional)
#' @return list(ok, eligible, points, x_mid, y_mid, method_note, missing)
bblab_build_bcg_analysis <- function(structure_analysis = NULL, history = NULL) {
  empty <- list(
    ok = FALSE,
    eligible = FALSE,
    points = list(),
    x_mid = 0.5,
    y_mid = NA_real_,
    method_note = "bcg_need_structure",
    missing = character(0),
    quadrant_counts = setNames(rep(0L, 4L), .BBLAB_BCG_QUADRANTS)
  )
  sa <- structure_analysis
  if (is.null(sa) || !is.list(sa)) return(empty)
  biz <- sa$businesses %||% list()
  biz <- Filter(function(r) {
    .bblab_finite(r$revenue) && as.numeric(r$revenue)[1] > 0
  }, biz)
  if (length(biz) < 2L) {
    empty$missing <- "Fewer than two operating businesses with positive revenue."
    empty$method_note <- "bcg_need_two_businesses"
    return(empty)
  }

  revs <- vapply(biz, function(r) as.numeric(r$revenue)[1], numeric(1))
  max_rev <- max(revs, na.rm = TRUE)
  if (!is.finite(max_rev) || max_rev <= 0) {
    empty$missing <- "No positive business revenue for relative share."
    empty$method_note <- "bcg_need_structure"
    return(empty)
  }

  hist_ok <- isTRUE(history$eligible) && length(history$series) >= 1L
  points <- lapply(seq_along(biz), function(i) {
    r <- biz[[i]]
    name <- as.character(r$business %||% "")[1]
    rev <- as.numeric(r$revenue)[1]
    rel <- rev / max_rev
    growth <- NA_real_
    growth_source <- "none"
    if (isTRUE(hist_ok)) {
      ser <- .bblab_bcg_match_history_series(name, history)
      if (!is.null(ser)) {
        growth <- .bblab_bcg_cagr(ser$revenues)
        if (is.finite(growth)) growth_source <- "segment_revenue_cagr"
      }
    }
    list(
      business = name,
      revenue = rev,
      revenue_pct = if (.bblab_finite(r$revenue_pct)) as.numeric(r$revenue_pct)[1] else NA_real_,
      relative_share = rel,
      growth = growth,
      growth_source = growth_source,
      margin = if (.bblab_finite(r$margin)) as.numeric(r$margin)[1] else NA_real_,
      quadrant = NA_character_
    )
  })

  growths <- vapply(points, function(p) as.numeric(p$growth)[1], numeric(1))
  n_growth <- sum(is.finite(growths))
  if (n_growth < 1L) {
    # Structure exists but classic BCG growth axis cannot be estimated.
    empty$ok <- TRUE
    empty$eligible <- FALSE
    empty$points <- points
    empty$missing <- "Multi-year segment revenue not available for growth (Y-axis)."
    empty$method_note <- "bcg_need_history"
    return(empty)
  }

  # Classic divider: relative share = 0.5× leader; growth midline = median of known CAGRs
  x_mid <- 0.5
  y_mid <- stats::median(growths[is.finite(growths)], na.rm = TRUE)
  if (!is.finite(y_mid)) y_mid <- 0.05

  for (i in seq_along(points)) {
    points[[i]]$quadrant <- .bblab_bcg_assign_quadrant(
      points[[i]]$relative_share, points[[i]]$growth, x_mid = x_mid, y_mid = y_mid
    )
  }

  counts <- setNames(rep(0L, 4L), .BBLAB_BCG_QUADRANTS)
  for (p in points) {
    q <- p$quadrant
    if (!is.na(q) && q %in% names(counts)) counts[[q]] <- counts[[q]] + 1L
  }

  list(
    ok = TRUE,
    eligible = TRUE,
    points = points,
    x_mid = x_mid,
    y_mid = y_mid,
    method_note = "bcg_internal_proxy",
    missing = character(0),
    quadrant_counts = counts
  )
}

#' Classic BCG quadrant scatter (plotly). High relative share on the left.
#' @param bcg result of bblab_build_bcg_analysis()
#' @param labels named list of axis/quadrant strings (locale)
bblab_bcg_plotly <- function(bcg, labels = list()) {
  lab <- function(k, fallback) {
    v <- labels[[k]]
    if (is.null(v) || !nzchar(as.character(v)[1])) fallback else as.character(v)[1]
  }
  empty <- plotly::layout(
    plotly::plot_ly(type = "scatter", mode = "markers"),
    showlegend = FALSE,
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )
  if (is.null(bcg) || !isTRUE(bcg$eligible) || !length(bcg$points)) return(empty)

  pts <- Filter(function(p) {
    is.finite(p$relative_share) && is.finite(p$growth)
  }, bcg$points)
  if (!length(pts)) return(empty)

  q_colors <- c(
    star = "#F5C518",
    cash_cow = "#249C60",
    question_mark = "#1AA8B8",
    dog = "#8A8A8A"
  )
  q_labels <- c(
    star = lab("star", "Stars"),
    cash_cow = lab("cash_cow", "Cash cows"),
    question_mark = lab("question_mark", "Question marks"),
    dog = lab("dog", "Dogs")
  )

  x <- vapply(pts, function(p) p$relative_share, numeric(1))
  y <- vapply(pts, function(p) p$growth * 100, numeric(1))
  rev_all <- vapply(pts, function(p) as.numeric(p$revenue)[1], numeric(1))
  rev_max <- max(rev_all[is.finite(rev_all) & rev_all > 0], na.rm = TRUE)
  if (!is.finite(rev_max) || rev_max <= 0) rev_max <- 1
  size <- vapply(rev_all, function(r) {
    if (!is.finite(r) || r <= 0) return(18)
    s <- 14 + 28 * sqrt(r / rev_max)
    max(16, min(48, s))
  }, numeric(1))
  names <- vapply(pts, function(p) p$business, character(1))
  quads <- vapply(pts, function(p) {
    q <- as.character(p$quadrant %||% "dog")[1]
    if (!q %in% names(q_colors)) "dog" else q
  }, character(1))
  colors <- unname(q_colors[quads])
  qlab <- unname(q_labels[quads])

  hover <- sprintf(
    "%s<br>%s: %.2f×<br>%s: %.1f%%<br>%s: %s",
    names,
    lab("x_axis_short", "Relative share"),
    x,
    lab("y_axis_short", "Revenue CAGR"),
    y,
    lab("quadrant", "Quadrant"),
    qlab
  )

  x_mid <- as.numeric(bcg$x_mid %||% 0.5)[1]
  y_mid_pct <- as.numeric(bcg$y_mid %||% 0.05)[1] * 100
  x_max <- max(1.05, max(x, na.rm = TRUE) * 1.08)
  y_pad <- max(2, diff(range(c(y, y_mid_pct), na.rm = TRUE)) * 0.15)
  y_lo <- min(y, y_mid_pct, na.rm = TRUE) - y_pad
  y_hi <- max(y, y_mid_pct, na.rm = TRUE) + y_pad

  fig <- plotly::plot_ly(
    type = "scatter",
    mode = "markers+text",
    x = x,
    y = y,
    text = names,
    textposition = "top center",
    hovertext = hover,
    hoverinfo = "text",
    marker = list(
      size = size,
      color = colors,
      opacity = 0.88,
      line = list(color = "#222222", width = 1)
    )
  )

  # Quadrant labels for reversed X (high relative share on the LEFT = larger x).
  # Top-left Stars · Top-right Question marks · Bottom-left Cash cows · Bottom-right Dogs
  x_left <- x_mid + (x_max - x_mid) * 0.55   # high-share side (plots left)
  x_right <- max(0.05, x_mid * 0.45)         # low-share side (plots right)
  y_top <- y_mid_pct + (y_hi - y_mid_pct) * 0.65
  y_bot <- y_mid_pct - (y_mid_pct - y_lo) * 0.65
  ann <- list(
    list(x = x_left, y = y_top, text = q_labels[["star"]], showarrow = FALSE,
         font = list(size = 12, color = "#C9A227"), xref = "x", yref = "y"),
    list(x = x_right, y = y_top, text = q_labels[["question_mark"]], showarrow = FALSE,
         font = list(size = 12, color = "#1AA8B8"), xref = "x", yref = "y"),
    list(x = x_left, y = y_bot, text = q_labels[["cash_cow"]], showarrow = FALSE,
         font = list(size = 12, color = "#249C60"), xref = "x", yref = "y"),
    list(x = x_right, y = y_bot, text = q_labels[["dog"]], showarrow = FALSE,
         font = list(size = 12, color = "#666666"), xref = "x", yref = "y")
  )

  plotly::layout(
    fig,
    title = NULL,
    margin = list(l = 64, r = 24, t = 28, b = 56),
    xaxis = list(
      title = lab("x_axis", "Relative share (vs largest business)"),
      range = c(x_max, 0.02), # reverse: high share on the left (classic BCG)
      zeroline = FALSE,
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.08)"
    ),
    yaxis = list(
      title = lab("y_axis", "Revenue CAGR (%)"),
      range = c(y_lo, y_hi),
      zeroline = TRUE,
      zerolinecolor = "rgba(0,0,0,0.15)",
      showgrid = TRUE,
      gridcolor = "rgba(0,0,0,0.08)"
    ),
    shapes = list(
      list(type = "line", x0 = x_mid, x1 = x_mid, y0 = y_lo, y1 = y_hi,
           line = list(color = "rgba(0,0,0,0.35)", width = 1, dash = "dot")),
      list(type = "line", x0 = 0.02, x1 = x_max, y0 = y_mid_pct, y1 = y_mid_pct,
           line = list(color = "rgba(0,0,0,0.35)", width = 1, dash = "dot"))
    ),
    annotations = ann,
    showlegend = FALSE,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(255,255,255,0.96)"
  )
}
