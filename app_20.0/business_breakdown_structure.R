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
