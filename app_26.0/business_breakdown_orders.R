# Business Breakdown — company orders / concentration (markets + suppliers).
# Orthogonal to the primary business-dimension split: never becomes primary_dimension.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

BBLAB_ORDER_TABLE_KINDS <- c(
  "customer_concentration", "supplier_concentration", "market_geography"
)

.bblab_orders_empty_pack <- function(codes = character(0), source = NA_character_) {
  list(
    eligible = FALSE,
    codes = unique(as.character(codes %||% character(0))),
    source = .bblab_chr(source, NA_character_),
    period = NA_character_,
    currency = NA_character_,
    denominator = NA_real_,
    slices = list(),
    rows = list(),
    top1_share = NA_real_,
    top3_share = NA_real_,
    method = NA_character_,
    threshold_note = NA_character_,
    is_customer_location_only = NA
  )
}

.bblab_orders_empty <- function(codes = character(0)) {
  first_or <- function(x, fallback) {
    x <- as.character(x %||% character(0))
    if (length(x) >= 1L && nzchar(x[[1]])) x[[1]] else fallback
  }
  list(
    market_geography = .bblab_orders_empty_pack(
      first_or(codes, "ORDERS_GEOGRAPHY_DISCLOSURE_MISSING")
    ),
    customer_concentration = .bblab_orders_empty_pack(
      first_or(codes, "ORDERS_CUSTOMER_DISCLOSURE_MISSING")
    ),
    supplier_concentration = .bblab_orders_empty_pack(
      first_or(codes, "ORDERS_SUPPLIER_DISCLOSURE_MISSING")
    )
  )
}

.bblab_orders_first_code <- function(pack, fallback = "") {
  codes <- as.character(pack$codes %||% character(0))
  if (length(codes) >= 1L && nzchar(codes[[1]])) codes[[1]] else fallback
}

.bblab_orders_is_concentration_kind <- function(kind) {
  k <- .bblab_chr(kind, "")
  k %in% c("customer_concentration", "supplier_concentration")
}

.bblab_orders_skip_as_business_dim <- function(kind) {
  k <- .bblab_chr(kind, "")
  k %in% c("customer_concentration", "supplier_concentration", "market_geography")
}

#' Pick the best geography dimension already extracted from filings (customer location).
.bblab_orders_pick_geography_dim <- function(dims) {
  if (!length(dims)) return(NULL)
  geos <- Filter(function(d) {
    is.list(d) && identical(.bblab_chr(d$kind), "geography") &&
      length(d$components %||% list()) >= 2L
  }, dims)
  if (!length(geos)) {
    geos <- Filter(function(d) {
      is.list(d) && isTRUE(d$is_customer_location_only) &&
        length(d$components %||% list()) >= 2L
    }, dims)
  }
  if (!length(geos)) return(NULL)
  # Prefer filed/audited with the most positive revenue rows.
  score <- vapply(geos, function(d) {
    comps <- d$components %||% list()
    n_pos <- sum(vapply(comps, function(c) {
      .bblab_finite(c$revenue) && as.numeric(c$revenue)[1] > 0
    }, logical(1)))
    filed <- if (isTRUE(d$filed_audited)) 10L else 0L
    as.numeric(n_pos + filed)
  }, numeric(1))
  geos[[which.max(score)]]
}

.bblab_orders_slice_from_comp <- function(comp, cons_rev, classification = "GEOGRAPHY") {
  amt <- .bblab_num(comp$revenue)
  share <- if (.bblab_finite(comp$revenue_pct)) {
    p <- as.numeric(comp$revenue_pct)[1]
    if (is.finite(p) && abs(p) > 1.0000001) p / 100 else p
  } else if (.bblab_finite(amt) && .bblab_finite(cons_rev) && cons_rev != 0) {
    amt / cons_rev
  } else {
    NA_real_
  }
  list(
    id = .bblab_chr(comp$id, .bblab_slug(comp$name %||% "row", "ord")),
    name = .bblab_chr(comp$name, "—"),
    revenue = amt,
    share = share,
    classification = classification,
    confidence = comp$revenue_evidence$confidence %||%
      if (isTRUE(comp$separately_disclosed_revenue)) "HIGH" else "MEDIUM",
    period = .bblab_chr(comp$period, NA_character_),
    currency = .bblab_chr(comp$currency, NA_character_),
    status = "REPORTED",
    rank = NA_integer_
  )
}

.bblab_orders_rank_rows <- function(rows) {
  if (!length(rows)) return(list())
  shares <- vapply(rows, function(r) {
    s <- suppressWarnings(as.numeric(r$share)[1])
    if (is.finite(s)) s else -Inf
  }, numeric(1))
  ord <- order(shares, decreasing = TRUE, na.last = TRUE)
  out <- rows[ord]
  for (i in seq_along(out)) out[[i]]$rank <- as.integer(i)
  out
}

.bblab_orders_top_shares <- function(rows) {
  shares <- vapply(rows, function(r) {
    s <- suppressWarnings(as.numeric(r$share)[1])
    if (is.finite(s)) s else NA_real_
  }, numeric(1))
  shares <- shares[is.finite(shares)]
  list(
    top1 = if (length(shares) >= 1L) shares[[1]] else NA_real_,
    top3 = if (length(shares) >= 1L) sum(shares[seq_len(min(3L, length(shares)))]) else NA_real_
  )
}

#' Build market / geography pack from a geography dimension (not a business split).
bblab_orders_market_from_dimension <- function(dim, consolidated = NULL,
                                              source = "sec_segment_note") {
  empty <- .bblab_orders_empty_pack("ORDERS_GEOGRAPHY_DISCLOSURE_MISSING")
  if (is.null(dim) || !is.list(dim)) return(empty)
  cons_rev <- .bblab_num(consolidated$revenue)
  comps <- Filter(function(c) {
    if (is.null(c) || !is.list(c)) return(FALSE)
    cls <- .bblab_chr(c$classification, "MAJOR")
    if (cls %in% c("ELIMINATION", "RECONCILIATION", "ROUNDING")) return(FALSE)
    .bblab_finite(c$revenue) || .bblab_finite(c$revenue_pct)
  }, dim$components %||% list())
  if (length(comps) < 2L) return(empty)
  slices <- lapply(comps, function(c) .bblab_orders_slice_from_comp(c, cons_rev, "GEOGRAPHY"))
  slices <- Filter(function(s) {
    (is.finite(s$share) && s$share > 0) || (is.finite(s$revenue) && s$revenue > 0)
  }, slices)
  if (length(slices) < 2L) {
    return(.bblab_orders_empty_pack("ORDERS_GEOGRAPHY_DISCLOSURE_MISSING"))
  }
  slices <- .bblab_orders_rank_rows(slices)
  tops <- .bblab_orders_top_shares(slices)
  list(
    eligible = TRUE,
    codes = character(0),
    source = .bblab_chr(source, "sec_segment_note"),
    period = .bblab_chr(dim$period %||% consolidated$period, NA_character_),
    currency = .bblab_chr(dim$currency %||% consolidated$currency, NA_character_),
    denominator = cons_rev,
    slices = slices,
    rows = slices,
    top1_share = tops$top1,
    top3_share = tops$top3,
    method = "geography_customer_location",
    threshold_note = NA_character_,
    is_customer_location_only = isTRUE(dim$is_customer_location_only) ||
      identical(.bblab_chr(dim$kind), "geography")
  )
}

.bblab_orders_anon_customer_name <- function(nm) {
  s <- .bblab_norm_label(nm)
  grepl("^(customer|client|buyer)( [a-z0-9]+)?$", s) ||
    grepl("^customer [a-z]$", s) ||
    grepl("^significant customer", s) ||
    grepl("^major customer", s)
}

.bblab_orders_anon_supplier_name <- function(nm) {
  s <- .bblab_norm_label(nm)
  grepl("^(supplier|vendor)( [a-z0-9]+)?$", s) ||
    grepl("^supplier [a-z]$", s) ||
    grepl("^significant supplier", s) ||
    grepl("^major supplier", s)
}

#' Lightweight row parse for concentration tables (allows a single counterparty).
.bblab_orders_rows_from_table <- function(tbl, consolidated = NULL,
                                         period = NA_character_,
                                         currency = NA_character_) {
  if (!is.list(tbl)) return(list())
  headers <- as.character(tbl$headers %||% character(0))
  rows <- tbl$rows %||% list()
  if (!length(rows)) return(list())
  name_i <- 1L
  year_i <- if (exists(".bblab_year_header_idx", mode = "function")) {
    .bblab_year_header_idx(headers)
  } else {
    integer(0)
  }
  pct_i <- if (exists(".bblab_header_col", mode = "function")) {
    .bblab_header_col(headers, c("% of", "percent of", "percentage of", "% of net",
                                 "% of total", "percent"))
  } else {
    NA_integer_
  }
  rev_i <- if (exists(".bblab_header_col", mode = "function")) {
    .bblab_header_col(
      headers,
      c("net sales", "net revenue", "total revenue", "(^|\\b)revenue($|\\b)",
        "(^|\\b)sales($|\\b)", "amount"),
      exclude = "cost|percent|%|margin|expense"
    )
  } else {
    NA_integer_
  }
  if (is.na(rev_i) && length(year_i)) rev_i <- year_i[length(year_i)]
  if (is.na(rev_i) && length(headers) >= 2L) rev_i <- length(headers)
  cons_rev <- .bblab_num(consolidated$revenue)
  scale <- suppressWarnings(as.numeric(tbl$scale)[1])
  if (!is.finite(scale) || scale <= 0) scale <- 1
  out <- list()
  for (k in seq_along(rows)) {
    r <- as.character(rows[[k]])
    if (!length(r) || length(r) < name_i) next
    nm <- trimws(r[[name_i]])
    if (!nzchar(nm)) next
    if (grepl("^(year ended|december)\\b", .bblab_norm_label(nm))) next
    if (grepl("^(19|20)\\d{2}$", nm)) next
    if (.bblab_is_consolidated_total_name(nm)) next
    if (grepl("^(total|consolidated|all other|other customers|other suppliers)\\b",
              .bblab_norm_label(nm))) {
      next
    }
    amt <- NA_real_
    if (!is.na(rev_i) && length(r) >= rev_i) {
      amt <- .bblab_parse_amount(r[[rev_i]])
    }
    if (!is.finite(amt) && length(year_i)) {
      am <- .bblab_row_amounts(r, name_i)
      amt <- .bblab_pick_current_amount(am, year_i, headers, r)
    }
    if (is.finite(amt)) amt <- amt * scale
    pct <- if (!is.na(pct_i) && length(r) >= pct_i) {
      p <- .bblab_parse_amount(r[[pct_i]])
      if (is.finite(p) && grepl("%", r[[pct_i]])) p / 100 else if (is.finite(p) && p > 1) p / 100 else p
    } else {
      NA_real_
    }
    if (!is.finite(amt) && is.finite(pct) && .bblab_finite(cons_rev)) {
      amt <- pct * as.numeric(cons_rev)[1]
    }
    if (!is.finite(amt) && !is.finite(pct)) next
    out[[length(out) + 1L]] <- list(
      id = .bblab_slug(nm, "ord"),
      name = nm,
      revenue = amt,
      revenue_pct = pct,
      separately_disclosed_revenue = TRUE,
      period = period,
      currency = currency,
      classification = "MAJOR"
    )
  }
  out
}

#' Parse concentration tables already attached on notes / segment_tables_json.
.bblab_orders_pack_from_tables <- function(tables, kind, consolidated = NULL,
                                          source = "sec_concentration_note") {
  miss_code <- if (identical(kind, "supplier_concentration")) {
    "ORDERS_SUPPLIER_DISCLOSURE_MISSING"
  } else {
    "ORDERS_CUSTOMER_DISCLOSURE_MISSING"
  }
  empty <- .bblab_orders_empty_pack(miss_code)
  if (!length(tables)) return(empty)
  cons_rev <- .bblab_num(consolidated$revenue)
  period <- .bblab_chr(consolidated$period, NA_character_)
  currency <- .bblab_chr(consolidated$currency, NA_character_)
  best <- NULL
  best_n <- 0L
  for (tbl in tables) {
    if (!is.list(tbl)) next
    tk <- .bblab_chr(tbl$kind %||% "", "")
    lab <- .bblab_chr(tbl$short_name %||% tbl$label, "")
    kind_ok <- identical(tk, kind)
    if (!kind_ok && identical(kind, "customer_concentration")) {
      kind_ok <- grepl("customer|credit risk|concentration of credit", lab, ignore.case = TRUE) &&
        !grepl("supplier|vendor", lab, ignore.case = TRUE)
    }
    if (!kind_ok && identical(kind, "supplier_concentration")) {
      kind_ok <- grepl("supplier|vendor", lab, ignore.case = TRUE)
    }
    if (!isTRUE(kind_ok)) next
    comps <- .bblab_orders_rows_from_table(
      tbl, consolidated = consolidated, period = period, currency = currency
    )
    # Fall back to the shared segment-table parser when stub rows need scale help.
    if (!length(comps) && exists(".bblab_components_from_table", mode = "function")) {
      packed <- tryCatch(
        .bblab_components_from_table(
          tbl, cons = consolidated, period = period, currency = currency,
          kind = "revenue_disaggregation",
          source_label = lab
        ),
        error = function(e) NULL
      )
      comps <- packed$components %||% list()
      comps <- Filter(function(c) {
        nm <- .bblab_chr(c$name, "")
        if (!nzchar(nm)) return(FALSE)
        if (.bblab_is_consolidated_total_name(nm)) return(FALSE)
        if (grepl("^(total|consolidated|all other|other customers|other suppliers)\\b",
                  .bblab_norm_label(nm))) {
          return(FALSE)
        }
        .bblab_finite(c$revenue) || .bblab_finite(c$revenue_pct)
      }, comps)
    }
    if (length(comps) > best_n) {
      best <- comps
      best_n <- length(comps)
    }
  }
  if (best_n < 1L) return(empty)
  classification <- if (identical(kind, "supplier_concentration")) {
    "SUPPLIER"
  } else {
    "CUSTOMER"
  }
  rows <- lapply(best, function(c) {
    row <- .bblab_orders_slice_from_comp(c, cons_rev, classification)
    anon <- if (identical(kind, "supplier_concentration")) {
      .bblab_orders_anon_supplier_name(c$name)
    } else {
      .bblab_orders_anon_customer_name(c$name)
    }
    row$status <- if (isTRUE(anon)) "ANONYMIZED" else "REPORTED"
    row
  })
  rows <- Filter(function(r) {
    (is.finite(r$share) && r$share > 0) || (is.finite(r$revenue) && r$revenue > 0)
  }, rows)
  if (!length(rows)) return(empty)
  rows <- .bblab_orders_rank_rows(rows)
  tops <- .bblab_orders_top_shares(rows)
  method <- if (any(vapply(rows, function(r) identical(r$status, "ANONYMIZED"), logical(1)))) {
    "top_n_anonymous"
  } else {
    "named_counterparties"
  }
  list(
    eligible = TRUE,
    codes = character(0),
    source = .bblab_chr(source, "sec_concentration_note"),
    period = period,
    currency = currency,
    denominator = cons_rev,
    slices = rows,
    rows = rows,
    top1_share = tops$top1,
    top3_share = tops$top3,
    method = method,
    threshold_note = if (identical(kind, "customer_concentration")) {
      "ASC 280 / IFRS 8 major-customer disclosures when filed (≥10% of revenue when reported)."
    } else {
      "Filed major-supplier concentration when disclosed in notes."
    },
    is_customer_location_only = NA
  )
}

.bblab_orders_scan_note_text <- function(notes, kind = "customer") {
  texts <- character(0)
  if (is.list(notes)) {
    for (nm in c("excerpts", "full_texts")) {
      v <- notes[[nm]]
      if (is.character(v) && length(v)) texts <- c(texts, v)
    }
    sn <- notes$short_names
    if (is.character(sn) && length(sn)) texts <- c(texts, sn)
  }
  if (!length(texts)) return(list(signal = "none", codes = character(0)))
  blob <- paste(texts, collapse = "\n")
  low <- tolower(blob)
  if (identical(kind, "customer")) {
    if (grepl("no (single )?(customer|client) accounted for more than", low) ||
        grepl("no (significant|major) customer", low) ||
        grepl("did not have any customer.*10\\s*%", low) ||
        grepl("no customer representing 10", low)) {
      return(list(signal = "none_above_threshold",
                  codes = "ORDERS_NO_MAJOR_CUSTOMER_ABOVE_THRESHOLD"))
    }
  } else if (identical(kind, "supplier")) {
    if (grepl("no (single )?(supplier|vendor) accounted for more than", low) ||
        grepl("no (significant|major) supplier", low) ||
        grepl("did not have any supplier.*10\\s*%", low)) {
      return(list(signal = "none_above_threshold",
                  codes = "ORDERS_NO_MAJOR_SUPPLIER_ABOVE_THRESHOLD"))
    }
  }
  list(signal = "none", codes = character(0))
}

#' Build the full orders / concentration block for BBLab analyze results.
#' @param dims Candidate dimensions already extracted (may include geography).
#' @param notes SEC / filing notes list (segment_tables_json, excerpts).
#' @param consolidated Reported consolidated totals.
#' @param segment_tables Optional pre-parsed tables list.
bblab_build_order_concentration <- function(dims = list(),
                                           notes = NULL,
                                           consolidated = NULL,
                                           segment_tables = NULL,
                                           ticker = NULL) {
  out <- .bblab_orders_empty()
  tables <- .bblab_coerce_segment_tables(segment_tables, notes)
  # Geography / major markets — prefer filed geography dimensions.
  geo_dim <- .bblab_orders_pick_geography_dim(dims)
  if (is.null(geo_dim) && length(tables)) {
    geo_tbls <- Filter(function(t) {
      identical(.bblab_chr(t$kind), "geography") || isTRUE(t$is_customer_location_only)
    }, tables)
    if (length(geo_tbls)) {
      packed <- tryCatch(
        .bblab_components_from_table(
          geo_tbls[[1]], cons = consolidated,
          period = consolidated$period, currency = consolidated$currency,
          kind = "geography",
          source_label = .bblab_chr(geo_tbls[[1]]$short_name, "geography")
        ),
        error = function(e) NULL
      )
      if (!is.null(packed) && length(packed$components) >= 2L) {
        geo_dim <- bblab_dimension(
          "orders_geo", "geography", packed$components,
          is_customer_location_only = TRUE,
          filed_audited = TRUE,
          period = consolidated$period, currency = consolidated$currency,
          label = .bblab_chr(geo_tbls[[1]]$short_name, "Geography")
        )
      }
    }
  }
  if (!is.null(geo_dim)) {
    out$market_geography <- bblab_orders_market_from_dimension(
      geo_dim, consolidated = consolidated,
      source = if (isTRUE(geo_dim$filed_audited)) "sec_segment_note" else "filing"
    )
  }

  cust_tables <- Filter(function(t) {
    identical(.bblab_chr(t$kind), "customer_concentration")
  }, tables)
  out$customer_concentration <- .bblab_orders_pack_from_tables(
    if (length(cust_tables)) cust_tables else tables,
    kind = "customer_concentration",
    consolidated = consolidated
  )
  if (!isTRUE(out$customer_concentration$eligible)) {
    scan <- .bblab_orders_scan_note_text(notes, "customer")
    if (identical(scan$signal, "none_above_threshold")) {
      out$customer_concentration <- .bblab_orders_empty_pack(scan$codes)
      out$customer_concentration$source <- "sec_concentration_note"
      out$customer_concentration$method <- "threshold_none_above"
      out$customer_concentration$threshold_note <-
        "Filer disclosed that no customer exceeded the major-customer threshold."
    }
  }

  supp_tables <- Filter(function(t) {
    identical(.bblab_chr(t$kind), "supplier_concentration")
  }, tables)
  out$supplier_concentration <- .bblab_orders_pack_from_tables(
    if (length(supp_tables)) supp_tables else list(),
    kind = "supplier_concentration",
    consolidated = consolidated
  )
  if (!isTRUE(out$supplier_concentration$eligible)) {
    scan <- .bblab_orders_scan_note_text(notes, "supplier")
    if (identical(scan$signal, "none_above_threshold")) {
      out$supplier_concentration <- .bblab_orders_empty_pack(scan$codes)
      out$supplier_concentration$source <- "sec_concentration_note"
      out$supplier_concentration$method <- "threshold_none_above"
      out$supplier_concentration$threshold_note <-
        "Filer disclosed that no supplier exceeded the major-supplier threshold."
    }
  }

  # Honest TW note: no MOPS major-customer/supplier parser yet.
  tk <- .bblab_chr(ticker, "")
  if (grepl("\\.(TW|TWO)$", tk, ignore.case = TRUE)) {
    if (!isTRUE(out$customer_concentration$eligible) &&
        identical(.bblab_orders_first_code(out$customer_concentration),
                  "ORDERS_CUSTOMER_DISCLOSURE_MISSING")) {
      out$customer_concentration$codes <- "ORDERS_TW_FILING_PARSER_PENDING"
    }
    if (!isTRUE(out$supplier_concentration$eligible) &&
        identical(.bblab_orders_first_code(out$supplier_concentration),
                  "ORDERS_SUPPLIER_DISCLOSURE_MISSING")) {
      out$supplier_concentration$codes <- "ORDERS_TW_FILING_PARSER_PENDING"
    }
    if (!isTRUE(out$market_geography$eligible) &&
        identical(.bblab_orders_first_code(out$market_geography),
                  "ORDERS_GEOGRAPHY_DISCLOSURE_MISSING")) {
      out$market_geography$codes <- "ORDERS_TW_FILING_PARSER_PENDING"
    }
  }
  out
}
