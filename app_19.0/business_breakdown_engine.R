# Business Breakdown Lab engine — company-agnostic decomposition.
# No ticker, issuer name, platform, node, or statement-layout branches.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.bblab_has <- function(flags, name) {
  name %in% as.character(flags %||% character(0))
}

.bblab_kind_rank <- function(kind, cfg) {
  pri <- cfg$disclosure_priority %||% BBLAB_DEFAULT_CONFIG$disclosure_priority
  i <- match(kind, pri)
  if (is.na(i)) length(pri) + 8L else as.integer(i)
}

#' Score one disclosure dimension. Geography that is only customer location is vetoed.
bblab_score_dimension <- function(dim, consolidated = NULL, cfg = NULL) {
  cfg <- cfg %||% bblab_load_config()
  w <- cfg$dimension_score_weights
  flags <- unique(c(as.character(dim$flags %||% character(0))))
  comps <- dim$components %||% list()
  n_rev <- sum(vapply(comps, function(c) .bblab_finite(c$revenue), logical(1)))
  n_cor <- sum(vapply(comps, function(c) .bblab_finite(c$cor) || .bblab_finite(c$gp), logical(1)))
  cons_rev <- .bblab_num(consolidated$revenue)
  sum_rev <- sum(vapply(comps, function(c) {
    v <- .bblab_num(c$revenue, 0)
    if (is.finite(v) && v > 0) v else 0
  }, numeric(1)))
  recon_ok <- is.finite(cons_rev) && cons_rev != 0 && is.finite(sum_rev) &&
    abs(sum_rev - cons_rev) <= max(cfg$thresholds$reconciliation_abs_tol,
                                   abs(cons_rev) * cfg$thresholds$reconciliation_rel_tol)
  auto_flags <- c(
    if (isTRUE(dim$mutually_exclusive)) "mutually_exclusive",
    if (n_rev >= 1L) "separate_revenue",
    if (n_cor >= 1L) "attributable_cor_gp",
    if (isTRUE(recon_ok) || .bblab_has(flags, "reconciles")) "reconciles",
    if (isTRUE(dim$filed_audited)) "filed_audited"
  )
  flags <- unique(c(flags, auto_flags))
  veto <- isTRUE(cfg$flags$geography_customer_location_only_veto) &&
    identical(dim$kind, "geography") && isTRUE(dim$is_customer_location_only) &&
    !identical(dim$kind, "operating_segment") && !identical(dim$kind, "segment_note")
  if (isTRUE(veto)) {
    return(list(score = -Inf, veto = TRUE, flags = flags,
                code = "BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY",
                kind_rank = .bblab_kind_rank(dim$kind, cfg)))
  }
  score <- 0
  for (nm in names(w)) {
    if (.bblab_has(flags, nm)) score <- score + as.numeric(w[[nm]])
  }
  score <- score + (length(BBLAB_DIMENSION_KINDS) - .bblab_kind_rank(dim$kind, cfg)) * 0.15
  if (n_rev >= 2L) score <- score + 0.4
  list(score = score, veto = FALSE, flags = flags, code = NA_character_,
       kind_rank = .bblab_kind_rank(dim$kind, cfg), n_rev = n_rev, n_cor = n_cor)
}

#' Pick exactly one primary dimension. Never combine overlapping dimensions.
bblab_select_primary_dimension <- function(dimensions, consolidated = NULL, cfg = NULL,
                                           override_id = NULL) {
  cfg <- cfg %||% bblab_load_config()
  if (!length(dimensions)) {
    return(list(dimension = NULL, scores = list(),
                codes = "BUSINESS_DISCLOSURE_INSUFFICIENT"))
  }
  dims <- dimensions
  if (!is.null(names(dims)) && !is.list(dims[[1]])) dims <- list(dims)
  scores <- lapply(seq_along(dims), function(i) {
    sc <- bblab_score_dimension(dims[[i]], consolidated, cfg)
    sc$id <- dims[[i]]$id
    sc$index <- i
    sc$overlaps_with <- as.character(dims[[i]]$overlaps_with %||% character(0))
    sc
  })
  codes <- unique(unlist(lapply(scores, function(s) if (isTRUE(s$veto)) s$code else NULL)))
  ov_ids <- unique(unlist(lapply(scores, function(s) s$overlaps_with)))
  if (length(ov_ids) && isTRUE(cfg$flags$never_combine_overlapping_dimensions)) {
    codes <- unique(c(codes, "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED"))
  }
  if (!is.null(override_id) && nzchar(.bblab_chr(override_id))) {
    hit <- which(vapply(dims, function(d) identical(d$id, .bblab_chr(override_id)), logical(1)))
    if (length(hit) == 1L && !isTRUE(scores[[hit]]$veto)) {
      return(list(dimension = dims[[hit]], scores = scores, codes = codes, override = TRUE))
    }
  }
  eligible <- which(vapply(scores, function(s) !isTRUE(s$veto) && is.finite(s$score), logical(1)))
  if (!length(eligible)) {
    return(list(dimension = NULL, scores = scores,
                codes = unique(c(codes, "BUSINESS_DISCLOSURE_INSUFFICIENT"))))
  }
  ord <- eligible[order(
    -vapply(scores[eligible], function(s) s$score, numeric(1)),
    vapply(scores[eligible], function(s) s$kind_rank, numeric(1))
  )]
  chosen <- dims[[ord[1]]]
  list(dimension = chosen, scores = scores, codes = codes, override = FALSE)
}

.bblab_is_major <- function(comp, cons_rev, cfg) {
  if (isTRUE(comp$qualitative_only)) return(FALSE)
  share <- NA_real_
  if (.bblab_finite(comp$revenue_pct)) {
    share <- as.numeric(comp$revenue_pct)[1]
    if (is.finite(share) && share > 1) share <- share / 100
  }
  if (!is.finite(share) && .bblab_finite(comp$revenue) && .bblab_finite(cons_rev) && cons_rev != 0) {
    share <- as.numeric(comp$revenue)[1] / as.numeric(cons_rev)[1]
  }
  isTRUE(comp$is_reported_segment) ||
    (is.finite(share) && share >= cfg$thresholds$major_share) ||
    isTRUE(comp$is_principal_activity) ||
    isTRUE(comp$needed_to_explain_material_share) ||
    (isTRUE(comp$distinct_economics) &&
       (isTRUE(comp$distinct_customers) || isTRUE(comp$distinct_production)) &&
       is.finite(share) && share >= cfg$thresholds$major_share / 2)
}

#' Identify 2–6 major businesses (or 1). Immaterial → Other Businesses. Never fabricate.
bblab_identify_major <- function(components, consolidated, cfg = NULL) {
  cfg <- cfg %||% bblab_load_config()
  cons_rev <- .bblab_num(consolidated$revenue)
  comps <- components %||% list()
  if (!length(comps)) {
    return(list(major = list(), other = list(), leftover = list(),
                single = FALSE, fabricated = FALSE))
  }
  special <- vapply(comps, function(c) {
    c$classification %in% c("UNALLOCATED", "ELIMINATION", "ROUNDING", "SHARED_CORPORATE", "RECONCILIATION")
  }, logical(1))
  work <- comps[!special]
  leftover_special <- comps[special]
  major_idx <- which(vapply(work, function(c) .bblab_is_major(c, cons_rev, cfg), logical(1)))
  if (!length(major_idx) && length(work) >= 1L &&
      all(vapply(work, function(c) isTRUE(c$qualitative_only), logical(1)))) {
    major_idx <- seq_along(work)
  }
  if (!length(major_idx) && length(work) == 1L && !isTRUE(work[[1]]$qualitative_only)) {
    major_idx <- 1L
  }
  max_n <- as.integer(cfg$thresholds$max_major_businesses)
  if (length(major_idx) > max_n) {
    shares <- vapply(work[major_idx], function(c) {
      if (.bblab_finite(c$revenue)) as.numeric(c$revenue)[1] else if (.bblab_finite(c$revenue_pct)) {
        p <- as.numeric(c$revenue_pct)[1]; if (p > 1) p <- p / 100
        p * (if (is.finite(cons_rev)) cons_rev else 0)
      } else 0
    }, numeric(1))
    keep <- major_idx[order(-shares)][seq_len(max_n)]
    major_idx <- sort(keep)
  }
  other_idx <- setdiff(seq_along(work), major_idx)
  major <- work[major_idx]
  for (i in seq_along(major)) major[[i]]$classification <- "MAJOR"
  other_items <- work[other_idx]
  list(
    major = major,
    other = other_items,
    leftover = leftover_special,
    single = length(major) == 1L && !length(other_items),
    fabricated = FALSE
  )
}

.bblab_status_rank <- function(method) {
  switch(as.character(method %||% ""),
    reported = 1L, percent_times_consolidated = 2L, mapped_product = 3L,
    operational_driver = 4L, unallocated = 5L,
    reported_cor = 1L, reported_gp = 2L, company_allocation = 3L,
    observable_driver = 4L, justified_proxy = 5L, unallocated_cost = 6L,
    revenue_share = 7L,
    9L
  )
}

#' Assign revenue. Rounding stays an explicit line; disclosed % is never silently rewritten.
bblab_assign_revenue <- function(businesses, other_items, leftover, consolidated,
                                 cfg = NULL, period = NA_character_, currency = NA_character_) {
  cfg <- cfg %||% bblab_load_config()
  cons_rev <- .bblab_num(consolidated$revenue)
  apply_one <- function(comp) {
    ev_notes <- NA_character_
    method <- "unallocated"
    status <- "UNALLOCATED"
    conf <- "UNAVAILABLE"
    amt <- NA_real_
    pct <- .bblab_num(comp$revenue_pct)
    if (is.finite(pct) && pct > 1) pct <- pct / 100
    if (.bblab_finite(comp$revenue)) {
      amt <- as.numeric(comp$revenue)[1]
      method <- "reported"
      status <- "REPORTED"
      conf <- "HIGH"
    } else if (is.finite(pct) && is.finite(cons_rev)) {
      amt <- pct * cons_rev
      method <- "percent_times_consolidated"
      status <- "DERIVED"
      conf <- "MEDIUM"
      ev_notes <- "Revenue derived as disclosed percentage times consolidated revenue; disclosed % unchanged."
    } else if (.bblab_finite(comp$mapped_product_revenue)) {
      amt <- as.numeric(comp$mapped_product_revenue)[1]
      method <- "mapped_product"
      status <- "DERIVED"
      conf <- "MEDIUM"
    } else if (.bblab_finite(comp$operational_driver_revenue)) {
      amt <- as.numeric(comp$operational_driver_revenue)[1]
      method <- "operational_driver"
      status <- "ESTIMATED"
      conf <- "LOW"
    }
    comp$revenue <- amt
    if (!is.finite(pct) && is.finite(amt) && is.finite(cons_rev) && cons_rev != 0) {
      pct <- amt / cons_rev
    }
    comp$revenue_pct <- pct
    comp$revenue_method <- method
    comp$revenue_evidence <- bblab_evidence(
      comp$id, "revenue", amt, currency, period, status, conf,
      source_type = comp$source_type, allocation_method = method,
      notes = ev_notes, recon_role = comp$classification
    )
    comp
  }
  businesses <- lapply(businesses, apply_one)
  other_items <- lapply(other_items, apply_one)
  other <- NULL
  if (length(other_items)) {
    o_rev <- sum(vapply(other_items, function(c) .bblab_num(c$revenue, 0), numeric(1)))
    other <- bblab_component(
      "other_businesses", "Other Businesses", "OTHER",
      revenue = if (is.finite(o_rev)) o_rev else NA_real_,
      period = period, currency = currency
    )
    other$members <- other_items
    other$revenue_method <- "derived_sum"
    other$revenue_evidence <- bblab_evidence(
      "other_businesses", "revenue", other$revenue, currency, period,
      "DERIVED", "MEDIUM", recon_role = "OTHER"
    )
  }
  assigned <- sum(vapply(businesses, function(c) .bblab_num(c$revenue, 0), numeric(1)))
  if (!is.null(other) && .bblab_finite(other$revenue)) assigned <- assigned + other$revenue
  elim <- Filter(function(c) identical(c$classification, "ELIMINATION"), leftover)
  rnd_in <- Filter(function(c) identical(c$classification, "ROUNDING"), leftover)
  una_in <- Filter(function(c) identical(c$classification, "UNALLOCATED"), leftover)
  elim_rev <- if (length(elim)) sum(vapply(elim, function(c) .bblab_num(c$revenue, 0), numeric(1))) else 0
  una_rev <- if (length(una_in)) sum(vapply(una_in, function(c) .bblab_num(c$revenue, 0), numeric(1))) else NA_real_
  rounding_amt <- if (length(rnd_in)) {
    sum(vapply(rnd_in, function(c) .bblab_num(c$revenue, 0), numeric(1)))
  } else if (is.finite(cons_rev) && is.finite(assigned)) {
    cons_rev - assigned - (if (is.finite(elim_rev)) elim_rev else 0) -
      (if (is.finite(una_rev)) una_rev else 0)
  } else NA_real_
  rounding <- NULL
  # Only tiny residuals become an explicit rounding line. Material leftover is
  # left for reconciliation (Unallocated / Reconciliation Amount) — never spread.
  rnd_cap <- if (is.finite(cons_rev)) {
    max(cfg$thresholds$reconciliation_abs_tol, abs(cons_rev) * cfg$thresholds$reconciliation_rel_tol)
  } else cfg$thresholds$reconciliation_abs_tol
  if (is.finite(rounding_amt) && abs(rounding_amt) > 0 && abs(rounding_amt) <= rnd_cap) {
    rounding <- bblab_component(
      "revenue_rounding_adjustment", "Revenue Rounding Adjustment", "ROUNDING",
      revenue = rounding_amt, period = period, currency = currency
    )
    rounding$revenue_evidence <- bblab_evidence(
      "revenue_rounding_adjustment", "revenue", rounding_amt, currency, period,
      "DERIVED", "HIGH", notes = "Explicit rounding line; disclosed percentages were not rewritten.",
      recon_role = "ROUNDING"
    )
  }
  unallocated <- NULL
  if (length(una_in) || (is.finite(cons_rev) && is.finite(assigned) &&
                         is.finite(una_rev) && abs(una_rev) > 0)) {
    amt <- if (is.finite(una_rev)) una_rev else NA_real_
    unallocated <- bblab_component(
      "unallocated", "Unallocated", "UNALLOCATED",
      revenue = amt, period = period, currency = currency
    )
    unallocated$revenue_evidence <- bblab_evidence(
      "unallocated", "revenue", amt, currency, period, "UNALLOCATED", "LOW",
      recon_role = "UNALLOCATED"
    )
  }
  eliminations <- NULL
  if (length(elim)) {
    eliminations <- bblab_component(
      "eliminations", "Eliminations", "ELIMINATION",
      revenue = elim_rev, period = period, currency = currency
    )
    eliminations$revenue_evidence <- bblab_evidence(
      "eliminations", "revenue", elim_rev, currency, period, "REPORTED", "HIGH",
      recon_role = "ELIMINATION"
    )
  }
  list(
    businesses = businesses, other = other, unallocated = unallocated,
    eliminations = eliminations, rounding = rounding
  )
}

#' Assign Cost of Revenue. Revenue-share is the final fallback only.
bblab_assign_cost <- function(rev_pack, consolidated, cfg = NULL,
                              use_consolidated_gm_fallback = FALSE,
                              period = NA_character_, currency = NA_character_) {
  cfg <- cfg %||% bblab_load_config()
  cons_rev <- .bblab_num(consolidated$revenue)
  cons_cor <- .bblab_num(consolidated$cor)
  cons_gp <- .bblab_num(consolidated$gp)
  cons_gm <- if (is.finite(cons_rev) && cons_rev != 0 && is.finite(cons_gp)) cons_gp / cons_rev else NA_real_
  codes <- character(0)
  notices <- character(0)
  apply_one <- function(comp) {
    if (is.null(comp) || isTRUE(comp$qualitative_only)) return(comp)
    method <- "unallocated_cost"
    status <- "UNALLOCATED"
    conf <- "UNAVAILABLE"
    cor <- NA_real_
    gp <- NA_real_
    note <- NA_character_
    if (.bblab_finite(comp$cor)) {
      cor <- as.numeric(comp$cor)[1]
      method <- "reported_cor"
      status <- "REPORTED"
      conf <- "HIGH"
    } else if (.bblab_finite(comp$gp) && .bblab_finite(comp$revenue) &&
               !( .bblab_finite(comp$operating_income) &&
                  !isTRUE(comp$gp_reported) &&
                  abs(as.numeric(comp$gp)[1] - as.numeric(comp$operating_income)[1]) < 1e-8)) {
      # Reported Gross Profit only. Operating income is never a GP stand-in.
      gp <- as.numeric(comp$gp)[1]
      cor <- as.numeric(comp$revenue)[1] - gp
      method <- "reported_gp"
      status <- "DERIVED"
      conf <- "HIGH"
      note <- "Cost of Revenue derived from reported Gross Profit."
    } else if (.bblab_finite(comp$company_allocated_cor)) {
      cor <- as.numeric(comp$company_allocated_cor)[1]
      method <- "company_allocation"
      status <- "ALLOCATED"
      conf <- "MEDIUM"
      note <- "Cost of Revenue is a company allocation / estimate."
      codes <<- unique(c(codes, "BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE"))
    } else if (.bblab_finite(comp$observable_driver_cor)) {
      cor <- as.numeric(comp$observable_driver_cor)[1]
      method <- "observable_driver"
      status <- "ESTIMATED"
      conf <- "MEDIUM"
    } else if (.bblab_finite(comp$justified_proxy_cor)) {
      cor <- as.numeric(comp$justified_proxy_cor)[1]
      method <- "justified_proxy"
      status <- "ESTIMATED"
      conf <- "LOW"
    } else if (isTRUE(use_consolidated_gm_fallback) && is.finite(cons_gm) &&
               .bblab_finite(comp$revenue)) {
      gp <- as.numeric(comp$revenue)[1] * cons_gm
      cor <- as.numeric(comp$revenue)[1] - gp
      method <- "consolidated_gm_fallback"
      status <- "CONSOLIDATED_PROXY"
      conf <- "LOW"
      note <- "Low-confidence fallback: consolidated Gross Margin applied by user opt-in."
      codes <<- unique(c(codes, "BUSINESS_COST_NOT_RELIABLY_ESTIMABLE"))
    } else if (isTRUE(use_consolidated_gm_fallback) &&
               isTRUE(cfg$flags$revenue_share_cost_is_final_fallback) &&
               .bblab_finite(comp$revenue) && is.finite(cons_rev) && cons_rev != 0 &&
               is.finite(cons_cor)) {
      cor <- as.numeric(comp$revenue)[1] / cons_rev * cons_cor
      method <- "revenue_share"
      status <- "ALLOCATED"
      conf <- "LOW"
      note <- "ALLOCATED_LOW_CONFIDENCE: Cost of Revenue uses revenue-share as a final fallback. This is not a reported business cost."
      codes <<- unique(c(codes, "BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE"))
      notices <<- unique(c(notices, "rev_share_cost"))
    } else {
      codes <<- unique(c(codes, "BUSINESS_COST_NOT_RELIABLY_ESTIMABLE"))
    }
    comp$cor <- cor
    comp$cor_method <- method
    comp$cor_evidence <- bblab_evidence(
      comp$id, "cor", cor, currency, period, status, conf,
      allocation_method = method, notes = note, recon_role = comp$classification
    )
    if (identical(status, "ALLOCATED") && identical(conf, "LOW")) {
      comp$cor_label <- "ALLOCATED_LOW_CONFIDENCE"
    } else if (status %in% c("ALLOCATED", "ESTIMATED", "CONSOLIDATED_PROXY")) {
      comp$cor_label <- "allocated/estimated"
    } else {
      comp$cor_label <- status
    }
    comp
  }
  pack <- rev_pack
  pack$businesses <- lapply(pack$businesses, apply_one)
  if (!is.null(pack$other)) {
    if (is.list(pack$other$members)) {
      pack$other$members <- lapply(pack$other$members, apply_one)
      pack$other$cor <- sum(vapply(pack$other$members, function(c) .bblab_num(c$cor, 0), numeric(1)))
    }
    pack$other <- apply_one(pack$other)
  }
  for (nm in c("unallocated", "eliminations", "rounding")) {
    if (!is.null(pack[[nm]])) pack[[nm]] <- apply_one(pack[[nm]])
  }
  pack$cost_codes <- unique(codes)
  pack$cost_notices <- unique(notices)
  pack
}

#' GP = Rev − CoR; GM = GP / Rev. Shared opex never enters GP cards.
bblab_compute_gp <- function(pack, level_hint = NULL) {
  apply_one <- function(comp) {
    if (is.null(comp)) return(comp)
    rev <- .bblab_num(comp$revenue)
    cor <- .bblab_num(comp$cor)
    if (.bblab_finite(comp$gp) && identical(comp$cor_method %||% "", "reported_cor") == FALSE &&
        isTRUE(comp$gp_reported)) {
      gp <- as.numeric(comp$gp)[1]
    } else if (is.finite(rev) && is.finite(cor)) {
      gp <- rev - cor
    } else {
      gp <- NA_real_
    }
    gm <- if (is.finite(gp) && is.finite(rev) && rev != 0) gp / rev else NA_real_
    unestimable <- !is.finite(cor) && is.finite(rev)
    comp$gp <- gp
    comp$gm <- gm
    if (isTRUE(unestimable)) {
      comp$gp_display <- "Not reliably estimable"
      comp$gm_display <- "Not reliably estimable"
      comp$gp_evidence <- bblab_evidence(
        comp$id, "gp", NA_real_, comp$currency, comp$period, "UNAVAILABLE", "UNAVAILABLE",
        notes = "Cost of Revenue not reliably estimable; Gross Profit / Gross Margin withheld."
      )
    } else {
      st <- if (is.finite(gp) && identical(comp$cor_method %||% "", "reported_cor")) "DERIVED" else
        if (is.finite(gp) && identical(comp$revenue_method %||% "", "reported") &&
            identical(comp$cor_method %||% "", "reported_gp")) "REPORTED" else
          if (is.finite(gp)) "DERIVED" else "UNAVAILABLE"
      cf <- if (!is.null(comp$cor_evidence)) comp$cor_evidence$confidence else "UNAVAILABLE"
      if (identical(comp$revenue_method, "reported") && identical(comp$cor_method, "reported_cor")) cf <- "HIGH"
      comp$gp_evidence <- bblab_evidence(
        comp$id, "gp", gp, comp$currency, comp$period, st, cf, recon_role = comp$classification
      )
    }
    comp
  }
  pack$businesses <- lapply(pack$businesses, apply_one)
  if (!is.null(pack$other)) pack$other <- apply_one(pack$other)
  for (nm in c("unallocated", "eliminations", "rounding")) {
    if (!is.null(pack[[nm]])) pack[[nm]] <- apply_one(pack[[nm]])
  }
  pack
}

#' Levels A–D from the weakest major-business evidence.
bblab_classify_level <- function(pack) {
  comps <- pack$businesses %||% list()
  if (!length(comps)) return("D")
  if (all(vapply(comps, function(c) isTRUE(c$qualitative_only), logical(1)))) return("D")
  levels <- vapply(comps, function(c) {
    if (isTRUE(c$qualitative_only)) return("D")
    rev_ok <- identical(c$revenue_method %||% "", "reported") ||
      identical(c$revenue_evidence$status %||% "", "REPORTED")
    cor_rep <- identical(c$cor_method %||% "", "reported_cor") ||
      identical(c$cor_method %||% "", "reported_gp")
    cor_alloc <- (c$cor_method %||% "") %in% c(
      "company_allocation", "observable_driver", "justified_proxy",
      "revenue_share", "consolidated_gm_fallback"
    )
    if (isTRUE(rev_ok) && isTRUE(cor_rep)) return("A")
    if (isTRUE(rev_ok) && isTRUE(cor_alloc) && .bblab_finite(c$cor)) return("B")
    if (isTRUE(rev_ok) || .bblab_finite(c$revenue)) return("C")
    "D"
  }, character(1))
  if (any(levels == "D") && all(levels %in% c("D"))) return("D")
  if (any(levels == "C")) return("C")
  if (any(levels == "B")) return("B")
  "A"
}

.bblab_line_sum <- function(pack, field) {
  bits <- c(pack$businesses, list(pack$other, pack$unallocated, pack$eliminations,
                                  pack$rounding, pack$recon_amount))
  bits <- Filter(Negate(is.null), bits)
  sum(vapply(bits, function(c) .bblab_num(c[[field]], 0), numeric(1)))
}

#' Reconcile to reported consolidated. Do not spread unexplained diffs. Do not plug NI.
bblab_reconcile <- function(pack, consolidated, cfg = NULL) {
  cfg <- cfg %||% bblab_load_config()
  tol_rel <- cfg$thresholds$reconciliation_rel_tol
  tol_abs <- cfg$thresholds$reconciliation_abs_tol
  check_one <- function(field, reported) {
    recast <- .bblab_line_sum(pack, field)
    ok <- is.finite(reported) && is.finite(recast) &&
      abs(recast - reported) <= max(tol_abs, abs(reported) * tol_rel)
    leftover <- if (is.finite(reported) && is.finite(recast)) reported - recast else NA_real_
    list(field = field, reported = reported, recast = recast, leftover = leftover,
         pass = isTRUE(ok), tolerance = max(tol_abs, if (is.finite(reported)) abs(reported) * tol_rel else tol_abs))
  }
  rev <- check_one("revenue", .bblab_num(consolidated$revenue))
  cor <- check_one("cor", .bblab_num(consolidated$cor))
  gp <- check_one("gp", .bblab_num(consolidated$gp))
  codes <- character(0)
  # Unexplained leftover → Unallocated or Reconciliation Amount. Never spread, never NI.
  apply_leftover <- function(chk, field) {
    if (isTRUE(chk$pass) || !is.finite(chk$leftover) || abs(chk$leftover) < 1e-12) return(chk)
    # Leftover is booked once as Reconciliation Amount (not spread, not NI).
    if (is.null(pack$recon_amount)) {
      pack$recon_amount <<- bblab_component(
        "reconciliation_amount", "Reconciliation Amount", "RECONCILIATION"
      )
    }
    pack$recon_amount[[field]] <<- chk$leftover
    check_one(field, chk$reported)
  }
  # Do not auto-spread: expose leftover as Unallocated / Reconciliation Amount then re-check.
  if (!isTRUE(rev$pass) && is.finite(rev$leftover) && abs(rev$leftover) > 0) {
    rev <- apply_leftover(rev, "revenue")
  }
  if (!isTRUE(cor$pass) && is.finite(cor$leftover) && abs(cor$leftover) > 0) {
    cor <- apply_leftover(cor, "cor")
  }
  if (!isTRUE(gp$pass) && is.finite(gp$leftover) && abs(gp$leftover) > 0) {
    gp <- apply_leftover(gp, "gp")
  }
  # Recompute after leftover booking
  rev <- check_one("revenue", .bblab_num(consolidated$revenue))
  cor <- check_one("cor", .bblab_num(consolidated$cor))
  gp <- check_one("gp", .bblab_num(consolidated$gp))
  pass <- isTRUE(rev$pass) && (isTRUE(cor$pass) || !is.finite(.bblab_num(consolidated$cor))) &&
    (isTRUE(gp$pass) || !is.finite(.bblab_num(consolidated$gp)))
  if (!isTRUE(pass)) codes <- "BUSINESS_RECONCILIATION_FAIL"
  pack$reconciliation <- list(
    pass = isTRUE(pass),
    revenue = rev, cor = cor, gp = gp,
    used_ni_plug = FALSE
  )
  pack$recon_codes <- codes
  pack
}

#' Revaluation ratio. Never fabricate. Default view is Reported.
bblab_revaluation <- function(pack, inputs = NULL, cfg = NULL) {
  cfg <- cfg %||% bblab_load_config()
  inputs <- inputs %||% list()
  by_id <- if (length(inputs)) {
    stats::setNames(inputs, vapply(inputs, function(x) .bblab_chr(x$component_id %||% x$id), character(1)))
  } else list()
  apply_one <- function(comp) {
    if (is.null(comp)) return(comp)
    inp <- by_id[[comp$id]]
    reported_basis <- .bblab_num(inp$reported_basis %||% inp$reportedBasis %||% comp$revenue)
    adjusted_basis <- .bblab_num(inp$adjusted_basis %||% inp$adjustedBasis)
    method <- .bblab_chr(inp$method, NA_character_)
    reason <- .bblab_chr(inp$reason, NA_character_)
    src_date <- .bblab_chr(inp$source_date %||% inp$sourceDate, NA_character_)
    user_ov <- isTRUE(inp$user_override %||% inp$userOverride)
    is_est <- isTRUE(inp$is_estimated %||% inp$isEstimated)
    status <- "UNAVAILABLE"
    conf <- "UNAVAILABLE"
    ratio <- NA_real_
    pct <- NA_real_
    if (is.finite(reported_basis) && is.finite(adjusted_basis) && abs(reported_basis) > 0) {
      ratio <- adjusted_basis / reported_basis
      pct <- ratio - 1
      status <- if (!is.null(inp$status) && toupper(.bblab_chr(inp$status)) %in% BBLAB_VALUE_STATUS) {
        toupper(.bblab_chr(inp$status))
      } else if (isTRUE(user_ov)) {
        "REPORTED"
      } else if (identical(toupper(method), "CONSOLIDATED_PROXY")) {
        "CONSOLIDATED_PROXY"
      } else if (isTRUE(is_est)) {
        "ESTIMATED"
      } else {
        "DERIVED"
      }
      conf <- if (identical(status, "CONSOLIDATED_PROXY")) "LOW" else
        if (identical(status, "ESTIMATED") || identical(status, "ALLOCATED")) "LOW" else
          if (identical(status, "REPORTED")) "HIGH" else "MEDIUM"
    } else if (identical(toupper(method), "CONSOLIDATED_PROXY") && is.finite(reported_basis)) {
      status <- "CONSOLIDATED_PROXY"
      conf <- "LOW"
    }
    if (!is.finite(ratio)) {
      status <- "UNAVAILABLE"
      conf <- "UNAVAILABLE"
    }
    changes_prod <- isTRUE(inp$changes_production_costs_or_da)
    comp$revaluation <- list(
      reportedBasis = reported_basis,
      adjustedBasis = adjusted_basis,
      revaluationRatio = ratio,
      revaluationPercentage = pct,
      method = method,
      reason = reason,
      confidence = conf,
      sourceDate = src_date,
      userOverride = user_ov,
      isEstimated = is_est,
      status = status,
      changesProductionCostsOrDA = changes_prod
    )
    comp$revaluation_available <- is.finite(ratio)
    comp
  }
  pack$businesses <- lapply(pack$businesses, apply_one)
  if (!is.null(pack$other)) pack$other <- apply_one(pack$other)
  any_avail <- any(vapply(pack$businesses, function(c) isTRUE(c$revaluation_available), logical(1)))
  any_proxy <- any(vapply(pack$businesses, function(c) {
    identical(c$revaluation$status %||% "", "CONSOLIDATED_PROXY")
  }, logical(1)))
  codes <- character(0)
  if (!isTRUE(any_avail)) codes <- c(codes, "BUSINESS_REVALUATION_UNAVAILABLE")
  if (isTRUE(any_proxy)) codes <- c(codes, "BUSINESS_REVALUATION_CONSOLIDATED_PROXY")
  pack$reval_codes <- unique(codes)
  pack$reval_any <- isTRUE(any_avail)
  pack
}

#' Chart eligibility. Failures never block statement cards.
#' One supportable business is eligible (single slice at 100%, or that
#' business plus Other / Unallocated / Rounding recon slices). Component
#' count is never a gate: BUSINESS_CHART_SINGLE_COMPONENT and
#' BUSINESS_CHART_INSUFFICIENT_COMPONENTS are not emitted.
bblab_chart_eligibility <- function(pack, consolidated, cfg = NULL, level = NULL) {
  cfg <- cfg %||% bblab_load_config()
  codes <- character(0)
  cons_rev <- .bblab_num(consolidated$revenue)
  period <- unique(na.omit(c(
    consolidated$period,
    vapply(pack$businesses %||% list(), function(c) .bblab_chr(c$period, NA_character_), character(1))
  )))
  ccy <- unique(na.omit(c(
    consolidated$currency,
    vapply(pack$businesses %||% list(), function(c) .bblab_chr(c$currency, NA_character_), character(1))
  )))
  n_valid <- sum(vapply(pack$businesses %||% list(), function(c) {
    .bblab_finite(c$revenue) && !isTRUE(c$qualitative_only)
  }, logical(1)))
  if (!is.null(pack$other) && .bblab_finite(pack$other$revenue) && abs(pack$other$revenue) > 0) {
    n_valid <- n_valid + 1L
  }
  if (identical(level, "D")) codes <- c(codes, "BUSINESS_CHART_LEVEL_D")
  # Finite 0.0%-rounded shares (display) never fail eligibility.
  # One component is not a failure; do not gate on n_valid.
  if (!is.finite(cons_rev) || cons_rev == 0) codes <- c(codes, "BUSINESS_CHART_CONSOLIDATED_REVENUE_MISSING")
  if (length(period) > 1L) codes <- c(codes, "BUSINESS_CHART_PERIOD_MISMATCH")
  if (length(ccy) > 1L) codes <- c(codes, "BUSINESS_CHART_CURRENCY_MISMATCH")
  if (!isTRUE(pack$reconciliation$pass)) codes <- c(codes, "BUSINESS_CHART_RECONCILIATION_FAIL")
  eligible <- !length(codes)
  list(eligible = isTRUE(eligible), codes = unique(codes),
       n_valid = n_valid, consolidated_revenue = cons_rev)
}

#' Donut slices. Share denominator = reported consolidated revenue.
#' Negative eliminations stay in the legend/bridge, never as positive slices.
#' minDisplayPercentage groups tiny non-material items for display only.
bblab_chart_slices <- function(pack, consolidated, cfg = NULL, elig = NULL) {
  cfg <- cfg %||% bblab_load_config()
  elig <- elig %||% bblab_chart_eligibility(pack, consolidated, cfg)
  cons_rev <- .bblab_num(consolidated$revenue)
  min_p <- cfg$thresholds$min_display_percentage
  major_p <- cfg$thresholds$major_share
  rows <- list()
  add_row <- function(id, name, amt, classification, confidence, period, currency,
                      is_reported_segment = FALSE, source_keep = TRUE, display_group = FALSE) {
    rows[[length(rows) + 1L]] <<- list(
      id = id, name = name, revenue = amt,
      share = if (is.finite(cons_rev) && cons_rev != 0 && is.finite(amt)) amt / cons_rev else NA_real_,
      classification = classification,
      confidence = confidence,
      period = period, currency = currency,
      is_reported_segment = isTRUE(is_reported_segment),
      source_keep = isTRUE(source_keep),
      display_group = isTRUE(display_group),
      negative_elim = is.finite(amt) && amt < 0 && identical(classification, "ELIMINATION")
    )
  }
  for (c in pack$businesses %||% list()) {
    add_row(c$id, c$name, .bblab_num(c$revenue), c$classification,
            c$revenue_evidence$confidence %||% "UNAVAILABLE",
            c$period, c$currency, isTRUE(c$is_reported_segment))
  }
  if (!is.null(pack$other) && .bblab_finite(pack$other$revenue)) {
    add_row(pack$other$id, pack$other$name, pack$other$revenue, "OTHER",
            pack$other$revenue_evidence$confidence %||% "MEDIUM",
            pack$other$period, pack$other$currency)
  }
  if (!is.null(pack$unallocated) && .bblab_finite(pack$unallocated$revenue) &&
      abs(pack$unallocated$revenue) > 0) {
    add_row(pack$unallocated$id, pack$unallocated$name, pack$unallocated$revenue, "UNALLOCATED",
            "LOW", pack$unallocated$period, pack$unallocated$currency)
  }
  if (!is.null(pack$rounding) && .bblab_finite(pack$rounding$revenue) &&
      abs(pack$rounding$revenue) > 0) {
    add_row(pack$rounding$id, pack$rounding$name, pack$rounding$revenue, "ROUNDING",
            "HIGH", pack$rounding$period, pack$rounding$currency)
  }
  if (!is.null(pack$recon_amount) && .bblab_finite(pack$recon_amount$revenue) &&
      abs(pack$recon_amount$revenue) > 0) {
    add_row(pack$recon_amount$id, pack$recon_amount$name, pack$recon_amount$revenue,
            "RECONCILIATION", "LOW", NA_character_, consolidated$currency)
  }
  elim_legend <- NULL
  if (!is.null(pack$eliminations) && .bblab_finite(pack$eliminations$revenue)) {
    elim_legend <- list(
      id = pack$eliminations$id, name = pack$eliminations$name,
      revenue = pack$eliminations$revenue, classification = "ELIMINATION",
      in_pie = FALSE
    )
  }
  source_rows <- rows
  pie_rows <- Filter(function(r) {
    is.finite(r$revenue) && r$revenue > 0 && !isTRUE(r$negative_elim)
  }, rows)
  grouped <- list()
  display <- list()
  for (r in pie_rows) {
    tiny <- is.finite(r$share) && r$share < min_p
    material <- isTRUE(r$is_reported_segment) || (is.finite(r$share) && r$share >= major_p) ||
      identical(r$classification, "MAJOR")
    if (isTRUE(tiny) && !isTRUE(material)) {
      grouped[[length(grouped) + 1L]] <- r
    } else {
      display[[length(display) + 1L]] <- r
    }
  }
  if (length(grouped)) {
    g_amt <- sum(vapply(grouped, function(r) r$revenue, numeric(1)))
    display[[length(display) + 1L]] <- list(
      id = "display_other_group",
      name = "Other (display grouping)",
      revenue = g_amt,
      share = if (is.finite(cons_rev) && cons_rev != 0) g_amt / cons_rev else NA_real_,
      classification = "OTHER",
      confidence = "MEDIUM",
      display_group = TRUE,
      source_keep = FALSE,
      grouped_ids = vapply(grouped, function(r) r$id, character(1))
    )
  }
  list(
    eligible = isTRUE(elig$eligible),
    codes = elig$codes,
    denominator = cons_rev,
    slices = if (isTRUE(elig$eligible)) display else list(),
    source_rows = source_rows,
    eliminations_legend = elim_legend,
    grouped_source = grouped
  )
}

.bblab_overall_confidence <- function(pack, level, recon_pass) {
  if (identical(level, "D")) return("UNAVAILABLE")
  bits <- c(
    vapply(pack$businesses %||% list(), function(c) c$revenue_evidence$confidence %||% "UNAVAILABLE", character(1)),
    vapply(pack$businesses %||% list(), function(c) c$cor_evidence$confidence %||% "UNAVAILABLE", character(1))
  )
  if (!isTRUE(recon_pass)) return("LOW")
  if (identical(level, "A") && all(bits %in% c("HIGH", "MEDIUM"))) return("HIGH")
  if (identical(level, "B")) return("MEDIUM")
  if (identical(level, "C")) return("LOW")
  "MEDIUM"
}

.bblab_shared_only <- function(items) {
  items <- items %||% list()
  keep <- c("sm", "ga", "rd", "opex", "interest", "tax", "ni", "assets", "liabilities",
            "selling_marketing", "general_admin", "central_rd", "shared_opex")
  nms <- names(items)
  if (is.null(nms)) return(list())
  items[intersect(nms, keep)]
}

#' FX / ADR classifiers for the lab. ADR never blocks statement-currency GP cards.
bblab_classify_fx_adr <- function(statement_currency, display_currency,
                                  usd_twd = NULL, instrument_type = "ordinary",
                                  adr_ratio = NULL, per_adr_display = FALSE) {
  codes <- character(0)
  st <- if (exists("normalize_ccy", mode = "function")) {
    normalize_ccy(statement_currency)
  } else toupper(.bblab_chr(statement_currency, NA_character_))
  disp <- if (exists("normalize_ccy", mode = "function")) {
    normalize_ccy(display_currency)
  } else toupper(.bblab_chr(display_currency, NA_character_))
  need_fx <- if (exists("fx_conversion_required", mode = "function")) {
    isTRUE(fx_conversion_required(st, disp))
  } else {
    !is.na(st) && !is.na(disp) && !identical(st, disp)
  }
  if (isTRUE(need_fx)) {
    if (is.na(st) || !nzchar(.bblab_chr(st))) {
      codes <- c(codes, "STATEMENT_CURRENCY_UNAVAILABLE")
    } else {
      fx <- suppressWarnings(as.numeric(usd_twd)[1])
      if (!is.finite(fx) || fx <= 0) codes <- c(codes, "REQUIRED_FX_RATE_MISSING")
    }
  }
  need_adr <- isTRUE(per_adr_display) &&
    identical(toupper(.bblab_chr(instrument_type, "ordinary")), "ADR")
  if (isTRUE(need_adr)) {
    ar <- suppressWarnings(as.numeric(adr_ratio)[1])
    if (!is.finite(ar) || ar <= 0) codes <- c(codes, "APPLICABLE_ADR_RATIO_MISSING",
                                             "BUSINESS_ADR_MISSING_NONBLOCKING")
  }
  list(
    codes = unique(codes),
    decompose_currency = st,
    display_currency = disp,
    fx_required = isTRUE(need_fx),
    adr_required = isTRUE(need_adr),
    adr_blocks_business_gp = FALSE
  )
}

bblab_convert_amount <- function(amount, from_ccy, to_ccy, usd_twd = NULL) {
  amt <- .bblab_num(amount)
  if (!is.finite(amt)) return(amt)
  if (exists("fx_factor", mode = "function")) {
    mult <- fx_factor(from_ccy, to_ccy, usd_twd)
    if (is.finite(mult)) return(amt * mult)
  }
  fr <- toupper(.bblab_chr(from_ccy))
  to <- toupper(.bblab_chr(to_ccy))
  fx <- suppressWarnings(as.numeric(usd_twd)[1])
  if (identical(fr, to) || !nzchar(fr) || !nzchar(to)) return(amt)
  if (!is.finite(fx) || fx <= 0) {
    attr(amt, "alignment_failure") <- "REQUIRED_FX_RATE_MISSING"
    return(amt)
  }
  if (identical(fr, "USD") && identical(to, "TWD")) return(amt * fx)
  if (identical(fr, "TWD") && identical(to, "USD")) return(amt / fx)
  amt
}

#' Specific toast payload: what failed, what is blocked, what remains.
bblab_toast_payload <- function(codes, recon_pass = NULL, allocation_used = FALSE,
                                chart_eligible = FALSE) {
  codes <- unique(as.character(codes %||% character(0)))
  lapply(codes, function(code) {
    blocked <- character(0)
    remain <- c("business_cards", "analysis_summary", "reconciliation_table")
    chart_count_codes <- c("BUSINESS_CHART_SINGLE_COMPONENT",
                           "BUSINESS_CHART_INSUFFICIENT_COMPONENTS")
    if (code %in% chart_count_codes) {
      # Repurposed: one / few components never hide a valid recon donut.
      blocked <- character(0)
      remain <- unique(c(remain, "composition_chart"))
    } else if (grepl("^BUSINESS_CHART_", code)) {
      blocked <- "composition_chart"
    } else if (identical(code, "BUSINESS_COST_NOT_RELIABLY_ESTIMABLE") ||
               identical(code, "BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE")) {
      blocked <- character(0)
      remain <- unique(c(remain, "revenue"))
    } else if (identical(code, "BUSINESS_REVALUATION_UNAVAILABLE") ||
               identical(code, "BUSINESS_REVALUATION_CONSOLIDATED_PROXY")) {
      # Revaluation never hides reported / derived Rev, CoR, or GP cards.
      blocked <- "revaluation_ratio"
    } else if (identical(code, "REQUIRED_FX_RATE_MISSING") ||
               identical(code, "REQUIRED_FX_RATE_INVALID")) {
      blocked <- "display_currency_conversion"
      remain <- unique(c(remain, "statement_currency_analysis"))
    } else if (identical(code, "BUSINESS_ADR_MISSING_NONBLOCKING") ||
               identical(code, "APPLICABLE_ADR_RATIO_MISSING")) {
      blocked <- "per_adr_display"
      remain <- unique(c(remain, "statement_currency_analysis", "business_cards"))
    } else if (identical(code, "BUSINESS_RECONCILIATION_FAIL")) {
      blocked <- "recast_totals"
      remain <- unique(c(remain, "reported_consolidated_totals"))
    } else if (identical(code, "BUSINESS_STATEMENTS_UNAVAILABLE")) {
      blocked <- c("business_cards", "composition_chart", "reconciliation_table")
      remain <- "search"
    }
    if (identical(code, "BUSINESS_REVALUATION_UNAVAILABLE") ||
        identical(code, "BUSINESS_REVALUATION_CONSOLIDATED_PROXY")) {
      remain <- unique(c(remain, "revenue", "cost_of_revenue", "gross_profit",
                         "business_cards", "composition_chart", "search"))
    }
    list(
      code = code,
      failed = code,
      blocked_outputs = blocked,
      remaining_outputs = remain,
      allocation_or_proxy_used = isTRUE(allocation_used) ||
        code %in% c("BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE", "BUSINESS_REVALUATION_CONSOLIDATED_PROXY"),
      recon_pass = isTRUE(recon_pass),
      chart_eligible = isTRUE(chart_eligible),
      page_error = FALSE
    )
  })
}

.bblab_parse_amount <- function(x) {
  if (is.null(x) || length(x) < 1L || (is.atomic(x) && is.na(x[1]))) return(NA_real_)
  if (is.numeric(x) && is.finite(x[1])) return(as.numeric(x)[1])
  raw <- trimws(as.character(x)[1])
  if (!nzchar(raw) || raw %in% c("—", "-", "--", "n/a", "N/A", "NA", "nm", "NM")) {
    return(NA_real_)
  }
  paren <- grepl("^\\(.*\\)$", gsub("\\s", "", raw))
  if (exists("parse_financial_number", mode = "function")) {
    n <- parse_financial_number(gsub("[()]", "", raw))[1]
  } else {
    s <- gsub("[, $£€¥]", "", raw)
    s <- gsub("%", "", s)
    n <- suppressWarnings(as.numeric(s)[1])
  }
  if (!is.finite(n)) return(NA_real_)
  if (isTRUE(paren) && n > 0) n <- -n
  n
}

.bblab_slug <- function(name, prefix = "biz") {
  s <- tolower(gsub("[^A-Za-z0-9]+", "_", .bblab_chr(name, "component")))
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) s <- "component"
  paste0(prefix, "_", substr(s, 1L, 48L))
}

.bblab_norm_label <- function(lab) {
  s <- tolower(trimws(.bblab_chr(lab)))
  s <- gsub("&", " and ", s)
  s <- gsub("[^a-z0-9]+", " ", s)
  trimws(gsub("\\s+", " ", s))
}

# Standard consolidated IS lines — never treat as a separate business.
.bblab_is_standard_is_line <- function(lab) {
  s <- .bblab_norm_label(lab)
  skip <- c(
    "total revenue", "operating revenue", "net sales", "net revenue",
    "total net sales", "total net revenue", "revenue", "sales",
    "total operating revenue", "total sales",
    "cost of revenue", "cost of goods sold", "cost of sales",
    "cost of goods", "reconciled cost of revenue",
    "gross profit", "gross margin", "gross profit ratio",
    "operating expense", "operating expenses", "operating income",
    "operating profit", "operating earnings",
    "research and development", "selling general and administration",
    "selling general administrative", "general and administrative",
    "selling and marketing", "selling marketing",
    "ebit", "ebitda", "normalized ebitda", "normalized ebit",
    "pretax income", "tax provision", "net income",
    "net income common stockholders", "net income continuous operations",
    "diluted ni availto com stockholders", "diluted average shares",
    "basic average shares", "diluted eps", "basic eps",
    "interest expense", "interest income", "net interest income",
    "net non operating interest income expense",
    "other income expense", "other non operating income expenses",
    "total expenses", "total unusual items",
    "reconciled depreciation", "depreciation and amortization",
    "special income charges", "other special charges",
    "tax effect of unusual items", "tax rate for calcs",
    "normalized income", "net income including noncontrolling interests",
    "net income from continuing operation net minority interest",
    "net income from continuing and discontinued operation",
    "total unusual items excluding goodwill"
  )
  s %in% skip || grepl("^(diluted|basic) (eps|ni|average)", s) ||
    grepl("^tax (effect|rate)", s) || grepl("^ebitda|^ebit$", s)
}

.bblab_is_total_or_recon_name <- function(lab) {
  s <- .bblab_norm_label(lab)
  # "Net sales" is a metric label under a segment, not a recon line.
  grepl("^(total|sum|subtotal|grand total)\\b", s) ||
    grepl("\\b(eliminations?|elimination of intersegment|reconcil|unallocated|corporate and other)\\b", s) ||
    identical(s, "consolidated") || identical(s, "total revenue") ||
    identical(s, "total net sales") || identical(s, "total net revenue")
}

.bblab_metric_field <- function(lab) {
  s <- .bblab_norm_label(lab)
  if (!nzchar(s)) return(NA_character_)
  if (grepl("gross profit", s) && !grepl("margin|%", s)) return("gp")
  if (grepl("cost of (sales|revenue|goods)", s)) return("cor")
  if (grepl("operating income|operating profit", s)) return("operating_income")
  if (grepl("^operating expenses?$", s)) return("operating_expenses")
  if (s %in% c("net sales", "net revenue", "revenue", "sales",
               "total revenue", "total net sales", "total net revenue") ||
      grepl("^net sales\\b", s)) {
    return("revenue")
  }
  NA_character_
}

.bblab_looks_like_country_name <- function(lab) {
  s <- .bblab_norm_label(lab)
  grepl(paste(
    "united states", "^u s a$", "^usa$", "^u s$",
    "united kingdom", "^u k$", "great britain",
    "germany", "japan", "china", "france", "canada", "taiwan",
    "korea", "australia", "india", "brazil", "mexico", "italy", "spain",
    "netherlands", "switzerland", "sweden", "ireland", "singapore",
    "hong kong", "rest of world", "rest of the world", "other countries",
    sep = "|"
  ), s)
}

.bblab_df_current_amount <- function(df, i) {
  if (!is.data.frame(df) || i < 1L || i > nrow(df) || ncol(df) < 2L) return(NA_real_)
  period_cols <- colnames(df)[-1]
  ttm <- grepl("^ttm$", period_cols, ignore.case = TRUE)
  order_idx <- if (any(ttm)) c(which(ttm), which(!ttm)) else seq_along(period_cols)
  for (j in order_idx) {
    v <- .bblab_parse_amount(df[i, period_cols[j]])
    if (is.finite(v)) return(v)
  }
  NA_real_
}

.bblab_df_period <- function(df) {
  if (!is.data.frame(df) || ncol(df) < 2L) return(NA_character_)
  period_cols <- colnames(df)[-1]
  ttm <- grepl("^ttm$", period_cols, ignore.case = TRUE)
  hit <- if (any(ttm)) period_cols[which(ttm)[1]] else period_cols[1]
  .bblab_chr(hit, NA_character_)
}

.bblab_single_entity_dimension <- function(cons, entity = list(), payload = list()) {
  nm <- .bblab_chr(entity$name %||% payload$entity_name, "Reporting entity")
  single <- bblab_component(
    "consolidated_entity", nm, "MAJOR",
    revenue = cons$revenue, cor = cons$cor, gp = cons$gp,
    is_principal_activity = TRUE,
    period = cons$period, currency = cons$currency,
    source_type = "consolidated_statements"
  )
  bblab_dimension(
    "entity", "official_description", list(single),
    mutually_exclusive = TRUE, filed_audited = TRUE,
    period = cons$period, currency = cons$currency,
    flags = c("relevant", "reconciles", "mutually_exclusive", "separate_revenue",
              if (.bblab_finite(cons$cor) || .bblab_finite(cons$gp)) "attributable_cor_gp" else NULL,
              "filed_audited")
  )
}

.bblab_header_col <- function(headers, patterns, exclude = NULL) {
  h <- as.character(headers %||% character(0))
  if (!length(h)) return(NA_integer_)
  for (p in patterns) {
    hit <- grepl(p, h, ignore.case = TRUE)
    if (!is.null(exclude) && nzchar(exclude)) {
      hit <- hit & !grepl(exclude, h, ignore.case = TRUE)
    }
    if (any(hit)) return(which(hit)[1])
  }
  NA_integer_
}

.bblab_rel_diff <- function(a, b) {
  if (!is.finite(a) || !is.finite(b) || b == 0) return(Inf)
  abs(a - b) / abs(b)
}

#' Pick a multiplier that puts `amounts` into the same units as reported consolidated revenue.
#' Drops a table total/consolidated row (largest amount ≈ sum of the rest) so millions
#' vs unscaled dollars is not misread as "no matching scale" and 0.0% shares.
.bblab_infer_amount_scale <- function(amounts, cons_rev, prior = 1, rel_tol = 0.05) {
  amts <- as.numeric(amounts)
  amts <- amts[is.finite(amts) & amts != 0]
  prior <- if (.bblab_finite(prior) && as.numeric(prior)[1] > 0) as.numeric(prior)[1] else 1
  if (!length(amts) || !is.finite(cons_rev) || cons_rev == 0) return(prior)
  cands <- unique(c(prior, 1, 1e3, 1e6, 1e9, 1e-3, 1e-6, 1e-9))
  pick <- function(sm) {
    if (!is.finite(sm) || sm == 0) return(NA_real_)
    ok <- vapply(cands, function(m) .bblab_rel_diff(sm * m, cons_rev) <= rel_tol, logical(1))
    if (!any(ok)) return(NA_real_)
    cands[which(ok)[1]]
  }
  mx <- max(abs(amts))
  rest <- amts[abs(abs(amts) - mx) / mx > 0.02]
  sum_rest <- if (length(rest)) sum(rest) else NA_real_
  looks_total <- length(rest) >= 2L && is.finite(sum_rest) &&
    .bblab_rel_diff(abs(sum_rest), mx) <= rel_tol
  parts <- if (isTRUE(looks_total)) rest else amts
  m_parts <- if (length(parts) >= 1L) pick(sum(parts)) else NA_real_
  m_max <- pick(mx)
  m_all <- pick(sum(amts))
  for (m in c(m_parts, m_max, m_all, prior)) {
    if (is.finite(m) && m > 0) return(m)
  }
  prior
}

.bblab_money_fields <- c(
  "revenue", "cor", "gp", "operating_income", "operating_expenses",
  "mapped_product_revenue", "operational_driver_revenue",
  "company_allocated_cor", "observable_driver_cor", "justified_proxy_cor"
)

.bblab_scale_one_comp <- function(comp, m) {
  if (is.null(comp) || !is.finite(m) || abs(m - 1) < 1e-15) return(comp)
  for (nm in .bblab_money_fields) {
    if (.bblab_finite(comp[[nm]])) comp[[nm]] <- as.numeric(comp[[nm]])[1] * m
  }
  if (is.list(comp$extra)) {
    for (nm in names(comp$extra)) {
      if (nm %in% .bblab_money_fields && .bblab_finite(comp$extra[[nm]])) {
        comp$extra[[nm]] <- as.numeric(comp$extra[[nm]])[1] * m
      }
    }
  }
  if (is.list(comp$members)) {
    comp$members <- lapply(comp$members, function(x) .bblab_scale_one_comp(x, m))
  }
  comp
}

#' Align disclosed component money amounts to reported consolidated units.
.bblab_align_component_units <- function(comps, cons, prior = 1) {
  cons_rev <- .bblab_num(cons$revenue)
  if (!length(comps) || !is.finite(cons_rev) || cons_rev == 0) return(comps)
  special <- vapply(comps, function(c) {
    identical(c$classification, "UNALLOCATED") ||
      identical(c$classification, "ELIMINATION") ||
      identical(c$classification, "ROUNDING") ||
      identical(c$classification, "SHARED_CORPORATE") ||
      identical(c$classification, "RECONCILIATION")
  }, logical(1))
  work <- if (any(!special)) comps[!special] else comps
  revs <- vapply(work, function(c) .bblab_num(c$revenue), numeric(1))
  m <- .bblab_infer_amount_scale(revs, cons_rev, prior = prior)
  if (!is.finite(m) || abs(m - 1) < 1e-15) return(comps)
  lapply(comps, function(c) .bblab_scale_one_comp(c, m))
}

#' Copy CoR / GP from another disclosure only when the business name matches.
#' Never invents a cost and never treats Operating income as Gross Profit.
.bblab_map_cor_from_disclosures <- function(comps, dims, primary_id) {
  if (!length(comps) || !length(dims)) return(comps)
  by_name <- list()
  for (d in dims) {
    if (identical(.bblab_chr(d$id), .bblab_chr(primary_id))) next
    for (c in d$components %||% list()) {
      key <- .bblab_norm_label(c$name)
      if (!nzchar(key)) next
      if (.bblab_finite(c$cor) || isTRUE(c$gp_reported) && .bblab_finite(c$gp)) {
        by_name[[key]] <- c
      }
    }
  }
  if (!length(by_name)) return(comps)
  lapply(comps, function(comp) {
    if (.bblab_finite(comp$cor) || isTRUE(comp$gp_reported)) return(comp)
    hit <- by_name[[.bblab_norm_label(comp$name)]]
    if (is.null(hit)) return(comp)
    if (.bblab_finite(hit$cor) && !.bblab_finite(comp$cor)) {
      comp$cor <- as.numeric(hit$cor)[1]
      comp$cor_mapped_from <- hit$id
    }
    if (isTRUE(hit$gp_reported) && .bblab_finite(hit$gp) && !.bblab_finite(comp$gp)) {
      comp$gp <- as.numeric(hit$gp)[1]
      comp$gp_reported <- TRUE
    }
    comp
  })
}

.bblab_table_scale <- function(blob, amounts, cons_rev, explicit = NA_real_) {
  blob <- .bblab_chr(blob)
  text_scale <- 1
  if (grepl("in\\s+billions|\\$\\s*billion", blob, ignore.case = TRUE)) text_scale <- 1e9
  else if (grepl("in\\s+millions|\\$\\s*million|nt\\$\\s*million|\\$\\s*in\\s+millions",
                 blob, ignore.case = TRUE)) text_scale <- 1e6
  else if (grepl("in\\s+thousands|\\$\\s*thousand", blob, ignore.case = TRUE)) text_scale <- 1e3
  prior <- if (.bblab_finite(explicit) && as.numeric(explicit)[1] > 1) {
    as.numeric(explicit)[1]
  } else text_scale
  .bblab_infer_amount_scale(amounts, cons_rev, prior = prior)
}

.bblab_segment_year <- function(segment_tables, notes = NULL) {
  tables <- .bblab_coerce_segment_tables(segment_tables, notes)
  yrs <- integer(0)
  for (tbl in tables) {
    h <- as.character(tbl$headers %||% tbl$header %||% character(0))
    if (!length(h)) next
    y <- suppressWarnings(as.integer(sub(".*?((?:19|20)\\d{2}).*", "\\1", h)))
    yrs <- c(yrs, y[is.finite(y) & y >= 1900L & y <= 2100L])
  }
  if (!length(yrs)) return(NA_integer_)
  as.integer(max(yrs))
}

.bblab_df_metric_for_year <- function(df, patterns, year) {
  if (!is.data.frame(df) || !nrow(df) || !is.finite(year)) return(NA_real_)
  lab <- as.character(df[[1]])
  hit <- grepl(paste(patterns, collapse = "|"), lab, ignore.case = TRUE)
  if (!any(hit)) return(NA_real_)
  cols <- colnames(df)[-1]
  ystr <- as.character(as.integer(year))
  col <- which(grepl(ystr, cols, fixed = TRUE))
  if (!length(col)) return(NA_real_)
  .bblab_parse_amount(df[which(hit)[1], cols[col[1]]])
}

.bblab_kind_from_label <- function(label) {
  s <- tolower(.bblab_chr(label))
  # Country / customer-location footnotes, even when nested under a Segment title.
  if (grepl("geograph|by country|by region|customer location|attributed to countries|countries representing", s)) {
    return("geography")
  }
  if (grepl("disaggregat|revenue by|net sales by|net revenue by|groups of similar|platform", s)) {
    return("revenue_disaggregation")
  }
  if (grepl("product|service", s) && !grepl("segment", s)) return("product_service")
  if (grepl("segment", s)) return("operating_segment")
  if (grepl("m\\s*and\\s*a|md&a|management.s discussion", s)) return("mda")
  NA_character_
}

.bblab_row_amounts <- function(r, skip_i = 1L) {
  if (!length(r)) return(numeric(0))
  out <- numeric(0)
  idx <- seq_along(r)
  if (is.finite(skip_i) && skip_i >= 1L && skip_i <= length(r)) {
    idx <- idx[idx != skip_i]
  }
  for (j in idx) {
    t <- trimws(as.character(r[[j]]))
    if (!nzchar(t) || t %in% c("$", "€", "£", "¥")) next
    if (grepl("^(19|20)\\d{2}$", t)) next
    n <- .bblab_parse_amount(t)
    if (is.finite(n)) out <- c(out, n)
  }
  out
}

.bblab_year_header_idx <- function(headers) {
  h <- as.character(headers %||% character(0))
  yrs <- suppressWarnings(as.integer(sub(".*?((?:19|20)\\d{2}).*", "\\1", h)))
  hit <- which(is.finite(yrs) & yrs >= 1900L & yrs <= 2100L)
  if (!length(hit)) return(integer(0))
  hit[order(yrs[hit])]
}

.bblab_promote_year_header <- function(headers, rows) {
  headers <- as.character(headers %||% character(0))
  if (!length(rows)) return(list(headers = headers, rows = rows))
  probe <- as.character(rows[[1]])
  toks <- trimws(probe[nzchar(trimws(probe))])
  toks <- toks[!toks %in% c("$", "€", "£", "¥")]
  if (length(toks) >= 2L && all(grepl("^(19|20)\\d{2}$", toks))) {
    headers <- c(if (length(headers)) headers[[1]] else "", toks)
    rows <- rows[-1]
  }
  list(headers = headers, rows = rows)
}

.bblab_table_is_non_revenue <- function(headers, rows, source_label = "") {
  labs <- vapply(rows, function(r) {
    if (!length(r)) return("")
    .bblab_norm_label(r[[1]])
  }, character(1))
  has_rev <- any(vapply(labs, function(s) identical(.bblab_metric_field(s), "revenue"), logical(1)))
  if (isTRUE(has_rev)) return(FALSE)
  blob <- paste(c(source_label, headers, labs), collapse = " ")
  country_hits <- sum(vapply(labs, .bblab_looks_like_country_name, logical(1)))
  date_banner <- any(grepl("^(year ended|december)\\b", labs))
  has_corporate <- any(grepl("^corporate\\b", labs))
  grepl("depreciation and amortization|property and equipment|goodwill|net additions to property|segment assets",
        blob, ignore.case = TRUE) ||
    (isTRUE(date_banner) && country_hits < 2L) ||
    isTRUE(has_corporate) ||
    (grepl("december", paste(headers, collapse = " "), ignore.case = TRUE) &&
       !grepl("year ended", paste(headers, collapse = " "), ignore.case = TRUE))
}

#' Map already-retrieved income-statement child rows into a disclosure dimension.
bblab_extract_dimension_from_income_statement <- function(d_is, consolidated = NULL,
                                                         period = NA_character_,
                                                         currency = NA_character_) {
  if (!is.data.frame(d_is) || nrow(d_is) < 3L) return(NULL)
  labs <- trimws(as.character(d_is[[1]]))
  cons_rev <- .bblab_num(consolidated$revenue)
  stop_re <- paste(
    "cost of revenue", "cost of goods", "gross profit",
    "operating expense", "operating income", "operating profit",
    sep = "|"
  )
  start_i <- NA_integer_
  end_i <- nrow(d_is)
  for (i in seq_along(labs)) {
    n <- .bblab_norm_label(labs[[i]])
    if (is.na(start_i) && grepl("revenue|net sales", n) && .bblab_is_standard_is_line(labs[[i]])) {
      start_i <- i
    }
    if (!is.na(start_i) && i > start_i && grepl(stop_re, n)) {
      end_i <- i - 1L
      break
    }
  }
  if (is.na(start_i) || end_i <= start_i) return(NULL)
  comps <- list()
  seen <- character(0)
  for (i in seq.int(start_i, end_i)) {
    lab <- labs[[i]]
    if (!nzchar(lab) || .bblab_is_standard_is_line(lab) || .bblab_is_total_or_recon_name(lab)) {
      next
    }
    amt <- .bblab_df_current_amount(d_is, i)
    if (!is.finite(amt) || amt == 0) next
    id <- .bblab_slug(lab, "is")
    if (id %in% seen) id <- paste0(id, "_", i)
    seen <- c(seen, id)
    cls <- if (grepl("eliminat", .bblab_norm_label(lab))) "ELIMINATION" else "MAJOR"
    comps[[length(comps) + 1L]] <- bblab_component(
      id, lab, cls, revenue = amt,
      is_reported_segment = FALSE,
      separately_disclosed_revenue = TRUE,
      distinct_economics = TRUE,
      period = period %||% .bblab_df_period(d_is),
      currency = currency,
      source_type = "income_statement"
    )
  }
  if (length(comps) < 2L) return(NULL)
  flags <- c("separate_revenue", "relevant", "mutually_exclusive")
  if (is.finite(cons_rev) && cons_rev != 0) {
    sm <- sum(vapply(comps, function(c) .bblab_num(c$revenue, 0), numeric(1)))
    if (abs(sm - cons_rev) <= max(1, abs(cons_rev) * 0.05)) {
      flags <- c(flags, "reconciles")
    }
  }
  bblab_dimension(
    "is_product_service", "product_service", comps,
    mutually_exclusive = TRUE, filed_audited = TRUE,
    period = period %||% .bblab_df_period(d_is),
    currency = currency,
    flags = flags,
    label = "Income statement product / service revenue"
  )
}

.bblab_pick_current_amount <- function(amts, year_i, headers, r) {
  if (length(year_i) >= 1L) {
    col <- year_i[length(year_i)]
    if (length(r) >= col) {
      v <- .bblab_parse_amount(r[[col]])
      if (is.finite(v)) return(v)
    }
  }
  if (length(amts) >= 1L) return(amts[length(amts)])
  NA_real_
}

.bblab_components_from_grouped_metrics <- function(rows, name_i, year_i, headers,
                                                   scale, kind, period, currency) {
  comps <- list()
  seen <- character(0)
  current <- NULL
  flush_current <- function() {
    if (is.null(current) || !.bblab_finite(current$revenue)) return()
    id <- .bblab_slug(current$name, "note")
    if (id %in% seen) id <- paste0(id, "_", length(comps) + 1L)
    seen <<- c(seen, id)
    cls <- current$cls %||% "MAJOR"
    comps[[length(comps) + 1L]] <<- bblab_component(
      id, current$name, cls,
      revenue = current$revenue, cor = current$cor, gp = current$gp,
      is_reported_segment = identical(kind, "operating_segment") ||
        identical(kind, "segment_note"),
      separately_disclosed_revenue = TRUE,
      distinct_economics = TRUE,
      period = period, currency = currency,
      source_type = kind,
      extra = if (.bblab_finite(current$operating_income)) {
        list(operating_income = current$operating_income)
      } else NULL
    )
  }
  for (k in seq_along(rows)) {
    r <- as.character(rows[[k]])
    if (!length(r) || length(r) < name_i) next
    nm <- trimws(r[[name_i]])
    amts <- .bblab_row_amounts(r, skip_i = name_i)
    field <- .bblab_metric_field(nm)
    if (!nzchar(nm)) next
    if (grepl("^(year ended|december)\\b", .bblab_norm_label(nm))) next
    if (is.na(field) && !length(amts)) {
      flush_current()
      cls <- if (.bblab_is_total_or_recon_name(nm)) {
        if (grepl("eliminat", .bblab_norm_label(nm))) "ELIMINATION" else
          if (grepl("unallocated|corporate", .bblab_norm_label(nm))) "UNALLOCATED" else NA_character_
      } else "MAJOR"
      if (is.na(cls) && .bblab_is_total_or_recon_name(nm)) {
        current <- NULL
        next
      }
      current <- list(name = nm, cls = cls %||% "MAJOR",
                      revenue = NA_real_, cor = NA_real_, gp = NA_real_,
                      operating_income = NA_real_)
      next
    }
    if (is.na(field)) next
    amt <- .bblab_pick_current_amount(amts, year_i, headers, r)
    if (!is.finite(amt)) next
    amt <- amt * scale
    if (is.null(current)) next
    if (identical(field, "revenue")) current$revenue <- amt
    else if (identical(field, "cor")) current$cor <- amt
    else if (identical(field, "gp")) current$gp <- amt
    else if (identical(field, "operating_income")) current$operating_income <- amt
    # operating_expenses: shared/full opex — never written onto GP cards.
  }
  flush_current()
  comps
}

.bblab_components_from_table <- function(tbl, cons = NULL, period = NA_character_,
                                         currency = NA_character_, kind = "segment_note",
                                         source_label = "") {
  headers <- tbl$headers %||% tbl$header %||% character(0)
  rows <- tbl$rows %||% tbl$data %||% list()
  if (is.data.frame(rows)) {
    if (!length(headers)) headers <- colnames(rows)
    rows <- lapply(seq_len(nrow(rows)), function(i) as.character(unlist(rows[i, ], use.names = FALSE)))
  }
  if (!length(rows)) return(NULL)
  promoted <- .bblab_promote_year_header(headers, rows)
  headers <- promoted$headers
  rows <- promoted$rows
  if (!length(rows)) return(NULL)
  if (!length(headers) && length(rows[[1]])) {
    headers <- paste0("c", seq_along(rows[[1]]))
  }
  if (isTRUE(.bblab_table_is_non_revenue(headers, rows, source_label))) return(NULL)
  name_i <- .bblab_header_col(headers, c("segment", "product", "business", "description",
                                        "platform", "category", "line"))
  if (is.na(name_i)) name_i <- 1L
  year_i <- .bblab_year_header_idx(headers)
  labs <- vapply(rows, function(r) {
    if (length(r) < name_i) return("")
    trimws(as.character(r[[name_i]]))
  }, character(1))
  fields <- vapply(labs, .bblab_metric_field, character(1))
  grouped <- identical(.bblab_chr(tbl$layout), "grouped_metrics") ||
    (any(fields == "revenue", na.rm = TRUE) &&
       any(fields == "operating_income", na.rm = TRUE))
  blob <- paste(c(source_label, headers, tbl$caption %||% ""), collapse = " ")
  if (isTRUE(grouped)) {
    raw_amts <- unlist(lapply(seq_along(rows), function(k) {
      if (!identical(fields[[k]], "revenue")) return(numeric(0))
      .bblab_pick_current_amount(.bblab_row_amounts(as.character(rows[[k]]), name_i),
                                 year_i, headers, as.character(rows[[k]]))
    }))
    scale <- .bblab_table_scale(blob, raw_amts, .bblab_num(cons$revenue), tbl$scale)
    comps <- .bblab_components_from_grouped_metrics(
      rows, name_i, year_i, headers, scale, kind, period, currency
    )
    if (length(comps) < 2L) return(NULL)
    flags <- c("separate_revenue", "relevant", "mutually_exclusive", "filed_audited",
               "distinct_economics", "management_major")
    if (any(vapply(comps, function(c) .bblab_finite(c$cor) || .bblab_finite(c$gp), logical(1)))) {
      flags <- c(flags, "attributable_cor_gp")
    }
    return(list(components = comps, flags = flags, is_customer_location_only = FALSE,
                kind = "operating_segment"))
  }
  rev_i <- .bblab_header_col(
    headers,
    c("net sales", "net revenue", "total revenue", "(^|\\b)revenue($|\\b)", "(^|\\b)sales($|\\b)"),
    exclude = "cost|percent|%|margin|expense"
  )
  if (is.na(rev_i) && length(year_i)) rev_i <- year_i[length(year_i)]
  if (is.na(rev_i)) {
    probe <- rows[[1]]
    for (j in seq_along(probe)) {
      if (j == name_i) next
      t <- trimws(as.character(probe[[j]]))
      if (grepl("^(19|20)\\d{2}$", t)) next
      if (is.finite(.bblab_parse_amount(t))) {
        rev_i <- j
        break
      }
    }
  }
  if (is.na(rev_i) && length(rows) >= 1L) {
    amts <- .bblab_row_amounts(as.character(rows[[1]]), name_i)
    if (length(amts)) rev_i <- name_i
  }
  if (is.na(rev_i)) return(NULL)
  cor_i <- .bblab_header_col(headers, c("cost of revenue", "cost of sales", "cost of goods",
                                       "cost of good"))
  gp_i <- .bblab_header_col(headers, c("gross profit"), exclude = "margin|%")
  pct_i <- .bblab_header_col(headers, c("% of", "percent of", "percentage of", "% of net",
                                       "% of total"))
  raw_amts <- vapply(rows, function(r) {
    r <- as.character(r)
    if (length(year_i)) {
      v <- .bblab_pick_current_amount(.bblab_row_amounts(r, name_i), year_i, headers, r)
      if (is.finite(v)) return(v)
    }
    if (length(r) < rev_i) {
      am <- .bblab_row_amounts(r, name_i)
      return(if (length(am)) am[length(am)] else NA_real_)
    }
    v <- .bblab_parse_amount(r[[rev_i]])
    if (is.finite(v)) v else {
      am <- .bblab_row_amounts(r, name_i)
      if (length(am)) am[length(am)] else NA_real_
    }
  }, numeric(1))
  scale <- .bblab_table_scale(blob, raw_amts, .bblab_num(cons$revenue), tbl$scale)
  comps <- list()
  seen <- character(0)
  for (k in seq_along(rows)) {
    r <- as.character(rows[[k]])
    if (!length(r) || length(r) < name_i) next
    nm <- trimws(r[[name_i]])
    if (!nzchar(nm)) next
    if (grepl("^(year ended|december)\\b", .bblab_norm_label(nm))) next
    if (!is.na(.bblab_metric_field(nm))) next
    if (.bblab_is_total_or_recon_name(nm)) {
      cls_guess <- if (grepl("eliminat", .bblab_norm_label(nm))) "ELIMINATION" else
        if (grepl("unallocated|corporate", .bblab_norm_label(nm))) "UNALLOCATED" else NA_character_
      if (is.na(cls_guess)) next
    } else {
      cls_guess <- "MAJOR"
    }
    amt <- raw_amts[[k]]
    pct <- if (!is.na(pct_i) && length(r) >= pct_i) {
      p <- .bblab_parse_amount(r[[pct_i]])
      if (is.finite(p) && grepl("%", r[[pct_i]])) p / 100 else if (is.finite(p) && p > 1) p / 100 else p
    } else NA_real_
    if (!is.finite(amt) && is.finite(pct) && .bblab_finite(cons$revenue)) {
      amt <- pct * as.numeric(cons$revenue)[1]
    }
    if (is.finite(amt)) amt <- amt * scale
    if (!is.finite(amt) && !is.finite(pct)) next
    if (is.finite(amt) && amt == 0 && !is.finite(pct)) next
    cor <- if (!is.na(cor_i) && length(r) >= cor_i) .bblab_parse_amount(r[[cor_i]]) * scale else NA_real_
    gp <- if (!is.na(gp_i) && length(r) >= gp_i) .bblab_parse_amount(r[[gp_i]]) * scale else NA_real_
    id <- .bblab_slug(nm, "note")
    if (id %in% seen) id <- paste0(id, "_", k)
    seen <- c(seen, id)
    comps[[length(comps) + 1L]] <- bblab_component(
      id, nm, cls_guess %||% "MAJOR",
      revenue = amt, cor = cor, gp = gp, revenue_pct = pct,
      is_reported_segment = identical(kind, "operating_segment") || identical(kind, "segment_note"),
      separately_disclosed_revenue = TRUE,
      distinct_economics = TRUE,
      period = period, currency = currency,
      source_type = kind
    )
  }
  if (length(comps) < 2L) return(NULL)
  flags <- c("separate_revenue", "relevant", "mutually_exclusive", "filed_audited")
  if (any(vapply(comps, function(c) .bblab_finite(c$cor) || .bblab_finite(c$gp), logical(1)))) {
    flags <- c(flags, "attributable_cor_gp")
  }
  nms <- vapply(comps, function(c) c$name, character(1))
  keep <- vapply(comps, function(c) identical(c$classification, "MAJOR"), logical(1))
  nms <- nms[keep]
  geo_from_names <- length(nms) >= 2L &&
    mean(vapply(nms, .bblab_looks_like_country_name, logical(1))) >= 0.8
  geo_only <- isTRUE(tbl$is_customer_location_only) ||
    identical(kind, "geography") ||
    grepl("customer location|by country|by region|attributed to countries", blob, ignore.case = TRUE) ||
    isTRUE(geo_from_names)
  if (identical(kind, "operating_segment") || identical(kind, "segment_note")) {
    geo_only <- FALSE
  }
  list(components = comps, flags = flags, is_customer_location_only = isTRUE(geo_only),
       kind = if (isTRUE(geo_only)) "geography" else kind)
}

.bblab_coerce_segment_tables <- function(segment_tables, notes = NULL) {
  tables <- list()
  if (is.list(segment_tables) && length(segment_tables)) {
    tables <- segment_tables
  }
  if (!length(tables) && is.list(notes)) {
    js <- notes$segment_tables_json %||% notes$segment_tables
    if (is.character(js) && nzchar(js[1])) {
      parsed <- tryCatch(jsonlite::fromJSON(js[1], simplifyVector = FALSE),
                         error = function(e) NULL)
      if (is.list(parsed) && length(parsed)) tables <- parsed
    } else if (is.list(js) && length(js)) {
      tables <- js
    }
  }
  if (!is.null(names(tables)) && !is.list(tables[[1]]) && !is.null(tables$headers)) {
    tables <- list(tables)
  }
  tables
}

#' Company-agnostic extraction from already-retrieved statements / filed notes.
#' Never invents amounts. Empty result means "do not fabricate a second business".
bblab_extract_dimensions_from_filings <- function(d_is = NULL, notes = NULL,
                                                 segment_tables = NULL,
                                                 consolidated = NULL,
                                                 period = NA_character_,
                                                 currency = NA_character_) {
  dims <- list()
  is_dim <- bblab_extract_dimension_from_income_statement(
    d_is, consolidated, period, currency
  )
  if (!is.null(is_dim)) dims[[length(dims) + 1L]] <- is_dim
  tables <- .bblab_coerce_segment_tables(segment_tables, notes)
  tbl_i <- 0L
  for (tbl in tables) {
    if (!is.list(tbl)) next
    lab <- .bblab_chr(tbl$short_name %||% tbl$label %||% tbl$caption, "segment_note")
    kind <- .bblab_chr(tbl$kind %||% .bblab_kind_from_label(lab), "segment_note")
    if (!kind %in% BBLAB_DIMENSION_KINDS) kind <- "segment_note"
    packed <- .bblab_components_from_table(
      tbl, cons = consolidated, period = period, currency = currency,
      kind = kind, source_label = lab
    )
    if (is.null(packed) || length(packed$components) < 2L) next
    kind <- .bblab_chr(packed$kind %||% kind, kind)
    if (!kind %in% BBLAB_DIMENSION_KINDS) kind <- "segment_note"
    geo_only <- isTRUE(packed$is_customer_location_only)
    if (kind %in% c("operating_segment", "segment_note")) geo_only <- FALSE
    tbl_i <- tbl_i + 1L
    dims[[length(dims) + 1L]] <- bblab_dimension(
      .bblab_slug(paste(lab, kind, tbl_i), "dim"), kind, packed$components,
      mutually_exclusive = TRUE,
      is_customer_location_only = geo_only,
      flags = packed$flags,
      filed_audited = TRUE,
      period = period, currency = currency,
      label = lab
    )
  }
  if (length(dims) >= 2L) {
    ids <- vapply(dims, function(d) d$id, character(1))
    for (i in seq_along(dims)) {
      dims[[i]]$overlaps_with <- unique(c(dims[[i]]$overlaps_with, ids[-i]))
    }
  }
  dims
}

.bblab_shared_from_is <- function(d_is) {
  if (!is.data.frame(d_is) || !nrow(d_is)) return(list())
  grab <- function(patterns) {
    if (exists("select_current_metric_any", mode = "function")) {
      v <- tryCatch(select_current_metric_any(d_is, patterns, "flow"), error = function(e) NA_real_)
      if (.bblab_finite(v)) return(as.numeric(v)[1])
    }
    NA_real_
  }
  out <- list(
    rd = grab(c("^Research And Development$", "Research And Development", "Research & Development")),
    ga = grab(c("Selling General And Administration", "General And Administrative")),
    sm = grab(c("^Selling And Marketing$", "Selling And Marketing")),
    opex = grab(c("^Operating Expense$", "Operating Expense"))
  )
  out <- out[vapply(out, function(v) .bblab_finite(v), logical(1))]
  out
}

#' Main entry. Payload is generic; no issuer-specific branches.
bblab_analyze <- function(payload, options = list(), cfg = NULL) {
  cfg <- cfg %||% bblab_load_config()
  opt <- options %||% list()
  progress <- character(0)
  mark <- function(s) { progress <<- unique(c(progress, s)); s }
  codes <- character(0)
  limitations <- character(0)
  mark("resolve_issuer")
  entity <- payload$entity %||% payload$issuer %||% list()
  if (is.null(payload) || (!nzchar(.bblab_chr(payload$ticker)) &&
                           !nzchar(.bblab_chr(entity$name)) &&
                           is.null(payload$consolidated) &&
                           !length(payload$dimensions))) {
    out <- bblab_empty_result("BUSINESS_ISSUER_UNRESOLVED")
    out$progress <- list(stage = "resolve_issuer", completed = progress)
    out$toasts <- bblab_toast_payload(out$codes)
    return(out)
  }
  mark("retrieve_statements")
  cons <- payload$consolidated
  if (is.null(cons) || (!.bblab_finite(cons$revenue) && !length(payload$dimensions))) {
    codes <- c(codes, "BUSINESS_STATEMENTS_UNAVAILABLE")
  }
  fx <- bblab_classify_fx_adr(
    payload$statement_currency %||% cons$currency,
    opt$display_currency %||% payload$display_currency %||% payload$statement_currency,
    usd_twd = opt$usd_twd %||% payload$usd_twd,
    instrument_type = payload$instrument_type %||% "ordinary",
    adr_ratio = payload$adr_ratio,
    per_adr_display = isTRUE(opt$per_adr_display)
  )
  codes <- unique(c(codes, fx$codes))
  mark("parse_disclosures")
  dims <- payload$dimensions %||% list()
  if (!length(dims)) {
    dims <- bblab_extract_dimensions_from_filings(
      payload$income_statement %||% payload$d_is,
      notes = payload$notes,
      segment_tables = payload$segment_tables,
      consolidated = cons,
      period = payload$period %||% cons$period,
      currency = payload$statement_currency %||% cons$currency
    )
  }
  entity_dim <- NULL
  if (.bblab_finite(cons$revenue)) {
    entity_dim <- .bblab_single_entity_dimension(cons, entity, payload)
  }
  if (!length(dims) && !is.null(entity_dim)) {
    dims <- list(entity_dim)
    limitations <- c(limitations, "no_multi_business_split")
  }
  mark("detect_dimension")
  sel <- bblab_select_primary_dimension(
    dims, cons, cfg, override_id = opt$dimension_override
  )
  codes <- unique(c(codes, sel$codes))
  dim <- sel$dimension
  if (is.null(dim) && !is.null(entity_dim)) {
    # Vetoed geography (or other non-eligible dims) must not hide reported cards.
    dim <- entity_dim
    limitations <- unique(c(limitations, "no_multi_business_split", "no_primary_dimension"))
  }
  if (is.null(dim)) {
    out <- bblab_empty_result(unique(c(codes, "BUSINESS_DISCLOSURE_INSUFFICIENT")),
                              c(limitations, "no_primary_dimension"))
    out$progress <- list(stage = "detect_dimension", completed = progress)
    out$consolidated <- cons
    out$toasts <- bblab_toast_payload(out$codes)
    out$fx <- fx
    out$ok <- FALSE
    return(out)
  }
  mark("identify_businesses")
  dim$components <- .bblab_align_component_units(dim$components, cons)
  dim$components <- .bblab_map_cor_from_disclosures(dim$components, dims, dim$id)
  ident <- bblab_identify_major(dim$components, cons, cfg)
  if (isTRUE(ident$single) || length(ident$major) <= 1L) {
    limitations <- unique(c(limitations, "no_multi_business_split"))
  }
  mark("assign_revenue")
  pack <- bblab_assign_revenue(
    ident$major, ident$other, ident$leftover, cons, cfg,
    period = cons$period %||% dim$period,
    currency = cons$currency %||% dim$currency
  )
  mark("assign_cost_gp")
  use_fb <- isTRUE(opt$use_consolidated_gm_fallback)
  pack <- bblab_assign_cost(
    pack, cons, cfg, use_consolidated_gm_fallback = use_fb,
    period = cons$period, currency = cons$currency
  )
  pack <- bblab_compute_gp(pack)
  shared <- .bblab_shared_only(payload$shared_corporate %||% payload$shared %||% list())
  pack$shared_corporate <- shared
  level <- bblab_classify_level(pack)
  if (identical(level, "D")) {
    for (i in seq_along(pack$businesses)) {
      pack$businesses[[i]]$revenue <- NA_real_
      pack$businesses[[i]]$cor <- NA_real_
      pack$businesses[[i]]$gp <- NA_real_
      pack$businesses[[i]]$gm <- NA_real_
    }
    limitations <- unique(c(limitations, "level_d_qualitative_only"))
  }
  mark("reconcile_revalue")
  pack <- bblab_reconcile(pack, cons, cfg)
  pack <- bblab_revaluation(pack, payload$revaluation_inputs %||% opt$revaluation_inputs, cfg)
  codes <- unique(c(codes, pack$cost_codes, pack$recon_codes, pack$reval_codes))
  mark("chart_cards")
  elig <- bblab_chart_eligibility(pack, cons, cfg, level = level)
  chart <- bblab_chart_slices(pack, cons, cfg, elig)
  codes <- unique(c(codes, elig$codes))
  overall <- .bblab_overall_confidence(pack, level, pack$reconciliation$pass)
  view <- if (identical(.bblab_chr(opt$view, cfg$flags$default_view), "adjusted")) "adjusted" else "reported"
  fold_adj <- identical(view, "adjusted") &&
    any(vapply(pack$businesses, function(c) isTRUE(c$revaluation$changesProductionCostsOrDA), logical(1)))
  if (!isTRUE(fold_adj)) {
    # Reported view never overwrites reported Rev/CoR/GP with revaluation.
    view <- if (identical(view, "adjusted") && !isTRUE(fold_adj)) "reported_keep_production" else view
  }
  toasts <- bblab_toast_payload(
    codes,
    recon_pass = pack$reconciliation$pass,
    allocation_used = "rev_share_cost" %in% (pack$cost_notices %||% character(0)),
    chart_eligible = chart$eligible
  )
  list(
    ok = TRUE,
    level = level,
    primary_dimension = list(
      id = dim$id, kind = dim$kind, label = dim$label,
      source_priority = dim$source_priority, filed_audited = dim$filed_audited
    ),
    dimension_scores = sel$scores,
    businesses = pack$businesses,
    other = pack$other,
    unallocated = pack$unallocated,
    eliminations = pack$eliminations,
    rounding = pack$rounding,
    recon_amount = pack$recon_amount,
    shared_corporate = shared,
    reconciliation = pack$reconciliation,
    revaluation_available = isTRUE(pack$reval_any),
    chart = chart,
    evidence = lapply(pack$businesses, function(c) {
      list(revenue = c$revenue_evidence, cor = c$cor_evidence, gp = c$gp_evidence,
           revaluation = c$revaluation)
    }),
    confidence = list(
      identification = if (length(pack$businesses) >= 1L) "HIGH" else "UNAVAILABLE",
      revenue = if (identical(level, "D")) "UNAVAILABLE" else if (identical(level, "A")) "HIGH" else "MEDIUM",
      cost = if (level %in% c("C", "D")) "UNAVAILABLE" else if (identical(level, "A")) "HIGH" else "LOW",
      gp = if (level %in% c("C", "D")) "UNAVAILABLE" else if (identical(level, "A")) "HIGH" else "LOW",
      revaluation = if (isTRUE(pack$reval_any)) "MEDIUM" else "UNAVAILABLE",
      recon = if (isTRUE(pack$reconciliation$pass)) "HIGH" else "LOW",
      overall = overall
    ),
    progress = list(stage = "chart_cards", completed = progress),
    codes = codes,
    toasts = toasts,
    limitations = unique(limitations),
    view = cfg$flags$default_view,
    view_requested = opt$view %||% cfg$flags$default_view,
    fold_revaluation_into_gp = isTRUE(fold_adj),
    consolidated = cons,
    fx = fx,
    cost_notices = pack$cost_notices %||% character(0),
    entity = entity,
    ticker = .bblab_chr(payload$ticker, NA_character_),
    statement_currency = payload$statement_currency %||% cons$currency,
    period = payload$period %||% cons$period,
    frequency = payload$frequency %||% "annual"
  )
}

#' Pull consolidated IS totals into a generic payload. Optional disclosures attached as-is.
#' When disclosures are omitted, auto-detect product / segment / disaggregation rows
#' from the already-retrieved statement and any filed note tables (no invented amounts).
bblab_payload_from_statements <- function(d_is, ticker = "", entity_name = "",
                                          statement_currency = NA_character_,
                                          period = NA_character_, frequency = "annual",
                                          disclosures = NULL, shared_corporate = NULL,
                                          instrument_type = "ordinary", adr_ratio = NA_real_,
                                          usd_twd = NULL, display_currency = NULL,
                                          notes = NULL, segment_tables = NULL) {
  seg_year <- .bblab_segment_year(segment_tables, notes)
  if (!nzchar(.bblab_chr(period))) {
    if (is.finite(seg_year)) {
      period <- as.character(seg_year)
    } else if (is.data.frame(d_is)) {
      period <- .bblab_df_period(d_is)
    }
  }
  grab <- function(patterns) {
    if (is.finite(seg_year) && is.data.frame(d_is)) {
      yv <- .bblab_df_metric_for_year(d_is, patterns, seg_year)
      if (.bblab_finite(yv)) return(as.numeric(yv)[1])
    }
    if (exists("select_current_metric_any", mode = "function") && is.data.frame(d_is)) {
      v <- tryCatch(select_current_metric_any(d_is, patterns, "flow"), error = function(e) NA_real_)
      if (.bblab_finite(v)) return(as.numeric(v)[1])
    }
    if (!is.data.frame(d_is) || !nrow(d_is)) return(NA_real_)
    lab <- as.character(d_is[[1]])
    hit <- grepl(paste(patterns, collapse = "|"), lab, ignore.case = TRUE)
    if (!any(hit)) return(NA_real_)
    nums <- .bblab_parse_amount(d_is[which(hit)[1], 2])
    if (.bblab_finite(nums)) as.numeric(nums)[1] else NA_real_
  }
  rev <- grab(c("^Total Revenue$", "Total Revenue", "Operating Revenue", "^Revenue$"))
  gp <- grab(c("^Gross Profit$", "Gross Profit"))
  cor <- grab(c("^Cost Of Revenue$", "Cost of Revenue", "Cost Of Goods", "Cost of Goods Sold"))
  if (!.bblab_finite(cor) && .bblab_finite(rev) && .bblab_finite(gp)) cor <- rev - gp
  if (!.bblab_finite(gp) && .bblab_finite(rev) && .bblab_finite(cor)) gp <- rev - cor
  ni <- grab(c("^Net Income$", "Net Income Common Stockholders", "Net Income"))
  cons <- bblab_consolidated(rev, cor, gp, statement_currency, period, ni = ni)
  dims <- list()
  if (is.list(disclosures) && length(disclosures)) {
    dims <- lapply(disclosures, function(d) {
      if (is.null(d$kind)) d else {
        comps <- lapply(d$components %||% list(), function(c) {
          if (inherits(c, "list") && !is.null(c$id)) c else do.call(bblab_component, c)
        })
        bblab_dimension(
          d$id %||% d$kind, d$kind, comps,
          mutually_exclusive = isTRUE(d$mutually_exclusive %||% TRUE),
          is_customer_location_only = isTRUE(d$is_customer_location_only),
          overlaps_with = d$overlaps_with %||% character(0),
          flags = d$flags %||% character(0),
          source_priority = d$source_priority,
          filed_audited = isTRUE(d$filed_audited),
          period = d$period %||% period,
          currency = d$currency %||% statement_currency,
          label = d$label
        )
      }
    })
  } else {
    dims <- bblab_extract_dimensions_from_filings(
      d_is, notes = notes, segment_tables = segment_tables,
      consolidated = cons, period = period, currency = statement_currency
    )
  }
  shared <- shared_corporate
  if (is.null(shared) || !length(shared)) shared <- .bblab_shared_from_is(d_is)
  list(
    ticker = ticker,
    entity = list(name = entity_name),
    statement_currency = statement_currency,
    display_currency = display_currency %||% statement_currency,
    period = period,
    frequency = frequency,
    instrument_type = instrument_type,
    adr_ratio = adr_ratio,
    usd_twd = usd_twd,
    consolidated = cons,
    dimensions = dims,
    shared_corporate = shared %||% list(),
    revaluation_inputs = list(),
    notes = notes,
    segment_tables = segment_tables,
    income_statement = if (is.data.frame(d_is)) d_is else NULL
  )
}
