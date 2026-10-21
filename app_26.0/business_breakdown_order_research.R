# Order research pack — disclosed facts with a TW-first source stack.
# One builder. Missing backlog / bookings stay empty. Never invent amounts.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# Priority registry. can_fill_amount=FALSE means corroboration / estimate only.
BBLAB_ORDER_SOURCE_STACK <- list(
  list(id = "mops_material", priority = 1L, credibility = "confirmed", can_fill_amount = TRUE),
  list(id = "mops_filing", priority = 2L, credibility = "confirmed", can_fill_amount = TRUE),
  list(id = "sec_filing", priority = 2L, credibility = "confirmed", can_fill_amount = TRUE),
  list(id = "company_ir", priority = 3L, credibility = "management", can_fill_amount = TRUE),
  list(id = "earnings_call", priority = 4L, credibility = "management", can_fill_amount = TRUE),
  list(id = "statement_proxy", priority = 5L, credibility = "proxy", can_fill_amount = FALSE),
  list(id = "broker_estimate", priority = 6L, credibility = "estimate", can_fill_amount = FALSE),
  list(id = "industry_proxy", priority = 7L, credibility = "estimate", can_fill_amount = FALSE)
)

BBLAB_ORDER_RESEARCH_LAYER_KEYS <- c(
  "scale", "quality", "conversion", "profit", "cash", "valuation"
)

BBLAB_ORDER_RESEARCH_KPI_IDS <- c(
  "backlog_yoy", "book_to_bill", "backlog_revenue_cover",
  "order_conversion", "est_order_gross_profit", "customer_concentration"
)

# Industry lens is a lookup, not a ticker branch. Prefixes are industry_standards keys.
BBLAB_ORDER_RESEARCH_PROFILES <- list(
  semi_ai = list(
    primary = c("book_to_bill", "backlog_revenue_cover"),
    key_prefix = c("sc."),
    text = c("semiconductor", "foundry", "\\bgpu\\b", "artificial intelligence")
  ),
  saas_software = list(
    primary = c("backlog_revenue_cover", "order_conversion"),
    key_prefix = c("saas.", "tech.Software", "tech.IT_Services", "tech.Internet"),
    text = c("\\bsaas\\b", "software", "cloud computing")
  ),
  eng_epc = list(
    primary = c("backlog_revenue_cover", "order_conversion"),
    key_prefix = c("ind.Construction", "ind.Aerospace"),
    text = c("\\bepc\\b", "engineering", "construction", "infrastructure", "aerospace")
  ),
  manufacturing = list(
    primary = c("book_to_bill", "customer_concentration"),
    key_prefix = c(
      "ind.Machinery", "ind.Conglomerate", "auto.",
      "ec.Hardware", "tech.Hardware", "tech.Optoelectronics",
      "tech.Electronics"
    ),
    text = c("manufactur", "machinery", "industrial")
  ),
  other = list(primary = character(0), key_prefix = character(0), text = character(0))
)

.or_num <- function(x) {
  if (is.null(x) || length(x) < 1L) return(NA_real_)
  v <- suppressWarnings(as.numeric(x)[1])
  if (is.finite(v)) v else NA_real_
}

.or_chr <- function(x, default = "") {
  if (exists(".bblab_chr", mode = "function")) return(.bblab_chr(x, default))
  if (is.null(x) || length(x) < 1L || is.na(x[1])) return(default)
  as.character(x[1])
}

.or_ratio <- function(num, den) {
  n <- .or_num(num)
  d <- .or_num(den)
  if (!is.finite(n) || !is.finite(d) || d == 0) return(NA_real_)
  n / d
}

.or_metric <- function(id, value = NA_real_, unit = "amount", status = NULL,
                       gap = NA_character_, source = NA_character_,
                       formula = NA_character_) {
  v <- .or_num(value)
  if (!is.finite(v)) v <- NA_real_
  st <- .or_chr(status, "")
  if (!nzchar(st)) st <- if (is.finite(v)) "REPORTED" else "UNAVAILABLE"
  if (is.finite(v)) gap <- NA_character_
  list(
    id = .or_chr(id),
    value = v,
    unit = .or_chr(unit, "amount"),
    status = st,
    gap = if (is.finite(v)) NA_character_ else .or_chr(gap, NA_character_),
    source = .or_chr(source, NA_character_),
    formula = .or_chr(formula, NA_character_)
  )
}

.or_kpi <- function(id, value, unit, formula, gap, source = NA_character_,
                    status = NULL, callout = FALSE) {
  m <- .or_metric(id, value, unit, status, gap, source, formula)
  m$callout <- isTRUE(callout)
  m
}

#' Map an industry key or free text onto one lens. Unknown stays "other".
bblab_order_research_industry_profile <- function(industry_key = NULL,
                                                  industry_text = NULL) {
  key <- .or_chr(industry_key, "")
  text <- tolower(.or_chr(industry_text, ""))
  order <- c("semi_ai", "saas_software", "eng_epc", "manufacturing", "other")
  for (id in order) {
    if (identical(id, "other")) next
    spec <- BBLAB_ORDER_RESEARCH_PROFILES[[id]]
    if (nzchar(key) && any(startsWith(key, spec$key_prefix))) return(id)
    if (nzchar(text) && length(spec$text) &&
        any(vapply(spec$text, function(p) grepl(p, text, ignore.case = TRUE, perl = TRUE), logical(1)))) {
      return(id)
    }
  }
  "other"
}

.or_series <- function(df, patterns) {
  if (!is.data.frame(df) || !nrow(df) || ncol(df) < 2L) return(numeric(0))
  lab <- as.character(df[[1]])
  hit <- which(grepl(paste(patterns, collapse = "|"), lab, ignore.case = TRUE))
  if (!length(hit)) return(numeric(0))
  vals <- suppressWarnings(as.numeric(unlist(df[hit[1], -1, drop = TRUE])))
  names(vals) <- colnames(df)[-1]
  vals
}

# Annual column t-1 is four quarters back. A quarterly print uses the 5th newest column.
.or_yoy_prior <- function(vals, frequency = "annual") {
  if (!length(vals)) return(NA_real_)
  ttm <- grepl("^ttm$", names(vals), ignore.case = TRUE)
  annual <- vals[!ttm]
  annual <- annual[is.finite(annual)]
  if (!length(annual)) return(NA_real_)
  freq <- tolower(.or_chr(frequency, "annual"))
  if (freq %in% c("quarter", "quarterly") && length(annual) >= 5L) {
    return(as.numeric(annual[5]))
  }
  if (length(annual) >= 2L) return(as.numeric(annual[2]))
  NA_real_
}

.or_current <- function(vals, prefer_ttm = FALSE) {
  if (!length(vals)) return(NA_real_)
  ttm <- grepl("^ttm$", names(vals), ignore.case = TRUE)
  if (isTRUE(prefer_ttm) && any(ttm) && is.finite(vals[which(ttm)[1]])) {
    return(as.numeric(vals[which(ttm)[1]]))
  }
  rest <- vals[!ttm]
  rest <- rest[is.finite(rest)]
  if (length(rest)) return(as.numeric(rest[1]))
  finite <- vals[is.finite(vals)]
  if (length(finite)) as.numeric(finite[1]) else NA_real_
}

.or_pick <- function(df, patterns, frequency = "annual", which = c("current", "prior", "ttm")) {
  which <- match.arg(which)
  vals <- .or_series(df, patterns)
  if (identical(which, "prior")) return(.or_yoy_prior(vals, frequency))
  if (identical(which, "ttm")) {
    ttm <- grepl("^ttm$", names(vals), ignore.case = TRUE)
    if (any(ttm) && is.finite(vals[which(ttm)[1]])) return(as.numeric(vals[which(ttm)[1]]))
    return(NA_real_)
  }
  .or_current(vals, prefer_ttm = FALSE)
}

.or_table_amounts <- function(tbl) {
  if (!is.list(tbl)) return(numeric(0))
  rows <- tbl$rows %||% list()
  headers <- as.character(tbl$headers %||% character(0))
  scale <- .or_num(tbl$scale)
  if (!is.finite(scale) || scale <= 0) scale <- 1
  year_i <- integer(0)
  if (length(headers)) {
    y <- suppressWarnings(as.integer(sub(".*?((?:19|20)\\d{2}).*", "\\1", headers)))
    year_i <- which(is.finite(y) & y >= 1900L & y <= 2100L)
  }
  out <- numeric(0)
  for (r in rows) {
    r <- as.character(r)
    if (length(r) < 2L) next
    idx <- if (length(year_i)) year_i[length(year_i)] else length(r)
    if (idx < 1L || idx > length(r)) next
    amt <- if (exists(".bblab_parse_amount", mode = "function")) {
      .bblab_parse_amount(r[[idx]])
    } else {
      .or_num(gsub("[^0-9.\\-]", "", r[[idx]]))
    }
    if (is.finite(amt)) out <- c(out, amt * scale)
  }
  out
}

.or_table_latest_pair <- function(tbl) {
  if (!is.list(tbl)) return(list(current = NA_real_, prior = NA_real_))
  rows <- tbl$rows %||% list()
  if (!length(rows)) return(list(current = NA_real_, prior = NA_real_))
  r <- as.character(rows[[1]])
  headers <- as.character(tbl$headers %||% character(0))
  scale <- .or_num(tbl$scale)
  if (!is.finite(scale) || scale <= 0) scale <- 1
  parse_cell <- function(cell) {
    if (exists(".bblab_parse_amount", mode = "function")) {
      .bblab_parse_amount(cell)
    } else {
      .or_num(gsub("[^0-9.\\-]", "", cell))
    }
  }
  # Year headers pick current vs t-1. Without years, the rightmost amount is current.
  dated <- list()
  plain <- numeric(0)
  for (j in seq_along(r)) {
    if (j == 1L && length(r) > 1L) next
    if (length(headers) >= j && grepl("%", headers[[j]])) next
    amt <- parse_cell(r[[j]])
    if (!is.finite(amt)) next
    amt <- amt * scale
    year <- NA_integer_
    if (length(headers) >= j) {
      y <- suppressWarnings(as.integer(sub(".*?((?:19|20)\\d{2}).*", "\\1", headers[[j]])))
      if (is.finite(y) && y >= 1900L && y <= 2100L) year <- y
    }
    if (is.finite(year)) dated[[length(dated) + 1L]] <- list(year = year, amt = amt)
    else plain <- c(plain, amt)
  }
  if (length(dated)) {
    ord <- order(vapply(dated, function(d) d$year, integer(1)), decreasing = TRUE)
    dated <- dated[ord]
    return(list(
      current = dated[[1]]$amt,
      prior = if (length(dated) >= 2L) dated[[2]]$amt else NA_real_
    ))
  }
  list(
    current = if (length(plain) >= 1L) plain[[length(plain)]] else NA_real_,
    prior = if (length(plain) >= 2L) plain[[length(plain) - 1L]] else NA_real_
  )
}

.or_source_tier <- function(raw) {
  s <- tolower(.or_chr(raw, ""))
  if (!nzchar(s)) return("mops_filing")
  if (grepl("mops_material|material_info|重大訊息", s)) return("mops_material")
  if (grepl("mops", s)) return("mops_filing")
  if (grepl("sec", s)) return("sec_filing")
  if (s %in% c("company_ir", "ir", "ir_deck") || grepl("\\bir\\b", s)) return("company_ir")
  if (s %in% c("earnings", "earnings_call", "transcript", "management")) return("earnings_call")
  if (grepl("broker|estimate|research", s)) return("broker_estimate")
  if (grepl("industry|customs|supply.?chain", s)) return("industry_proxy")
  if (grepl("statement|proxy", s)) return("statement_proxy")
  if (identical(s, "filing")) return("mops_filing")
  "mops_filing"
}

.or_tier_meta <- function(tier_id) {
  for (t in BBLAB_ORDER_SOURCE_STACK) {
    if (identical(t$id, tier_id)) return(t)
  }
  list(id = tier_id, priority = 99L, credibility = "estimate", can_fill_amount = FALSE)
}

.or_table_source <- function(tbl) {
  raw <- if (is.list(tbl)) tbl$source %||% tbl$provenance else tbl
  .or_source_tier(raw)
}

.or_disclosures_from_tables <- function(tables) {
  out <- list()
  if (!length(tables)) return(out)
  for (tbl in tables) {
    if (!is.list(tbl)) next
    kind <- tolower(.or_chr(tbl$kind, ""))
    src <- .or_table_source(tbl)
    if (!isTRUE(.or_tier_meta(src)$can_fill_amount)) next
    if (kind %in% c("backlog", "rpo", "remaining_performance")) {
      pair <- .or_table_latest_pair(tbl)
      if (is.finite(pair$current)) out$backlog <- pair$current
      if (is.finite(pair$prior)) out$backlog_prior <- pair$prior
      out$backlog_source <- src
    } else if (kind %in% c("bookings", "orders_received")) {
      pair <- .or_table_latest_pair(tbl)
      if (is.finite(pair$current)) {
        out$bookings <- pair$current
        out$bookings_source <- src
      }
    } else if (kind %in% c("orders", "order_concentration")) {
      amts <- .or_table_amounts(tbl)
      if (length(amts)) {
        out$order_amounts <- amts
        out$order_amounts_source <- src
      }
    }
  }
  out
}

.or_focus_fact <- function(name, value, unit, source, label_key = NA_character_,
                           order_type = "unknown", url = NA_character_) {
  v <- .or_num(value)
  tier <- .or_source_tier(source)
  meta <- .or_tier_meta(tier)
  list(
    name = .or_chr(name, ""),
    label_key = .or_chr(label_key, NA_character_),
    value = if (is.finite(v)) v else NA_real_,
    unit = unit,
    source = tier,
    credibility = .or_chr(meta$credibility, "estimate"),
    order_type = .or_chr(order_type, "unknown"),
    url = .or_chr(url, NA_character_),
    status = if (is.finite(v) || nzchar(.or_chr(name, ""))) "REPORTED" else "UNAVAILABLE"
  )
}

.or_dedupe_facts <- function(facts) {
  if (!length(facts)) return(list())
  seen <- character(0)
  out <- list()
  for (f in facts) {
    if (!is.finite(.or_num(f$value))) next
    key <- paste(
      tolower(.or_chr(f$name, "")),
      sprintf("%.8g", .or_num(f$value)),
      .or_chr(f$unit, ""),
      .or_chr(f$order_type, "unknown"),
      sep = "|"
    )
    if (key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- f
  }
  out
}

.or_named_amount_facts <- function(tbl, source = NULL) {
  if (!is.list(tbl)) return(list())
  rows <- tbl$rows %||% list()
  headers <- as.character(tbl$headers %||% character(0))
  scale <- .or_num(tbl$scale)
  if (!is.finite(scale) || scale <= 0) scale <- 1
  src <- if (!is.null(source)) source else .or_table_source(tbl)
  year_i <- integer(0)
  pct_i <- integer(0)
  if (length(headers)) {
    y <- suppressWarnings(as.integer(sub(".*?((?:19|20)\\d{2}).*", "\\1", headers)))
    year_i <- which(is.finite(y) & y >= 1900L & y <= 2100L)
    pct_i <- which(grepl("%|share|pct|占比|佔比", headers, ignore.case = TRUE))
  }
  facts <- list()
  for (r in rows) {
    r <- as.character(r)
    if (length(r) < 2L) next
    nm <- r[[1]]
    if (!nzchar(nm) || grepl("^(total|consolidated)\\b", nm, ignore.case = TRUE)) next
    if (length(pct_i) && pct_i[[1]] <= length(r)) {
      pct <- if (exists(".bblab_parse_amount", mode = "function")) {
        .bblab_parse_amount(r[[pct_i[[1]]]])
      } else {
        .or_num(gsub("[^0-9.\\-]", "", r[[pct_i[[1]]]]))
      }
      if (is.finite(pct) && grepl("%", r[[pct_i[[1]]]])) pct <- pct / 100
      if (is.finite(pct) && pct > 1) pct <- pct / 100
      if (is.finite(pct)) {
        facts[[length(facts) + 1L]] <- .or_focus_fact(nm, pct, "ratio", src)
        next
      }
    }
    idx <- if (length(year_i)) year_i[length(year_i)] else length(r)
    if (idx < 1L || idx > length(r)) next
    amt <- if (exists(".bblab_parse_amount", mode = "function")) {
      .bblab_parse_amount(r[[idx]])
    } else {
      .or_num(gsub("[^0-9.\\-]", "", r[[idx]]))
    }
    if (is.finite(amt)) {
      facts[[length(facts) + 1L]] <- .or_focus_fact(nm, amt * scale, "amount", src)
    }
  }
  facts
}

.or_facts_from_slices <- function(node) {
  if (!is.list(node) || !isTRUE(node$eligible)) return(list())
  rows <- node$rows %||% node$slices %||% list()
  src <- .or_table_source(list(source = node$source))
  facts <- list()
  for (r in rows) {
    if (!is.list(r)) next
    nm <- .or_chr(r$name, "")
    if (!nzchar(nm)) next
    share <- .or_num(r$share %||% r$revenue_pct)
    rev <- .or_num(r$revenue)
    if (is.finite(share)) {
      facts[[length(facts) + 1L]] <- .or_focus_fact(nm, share, "ratio", src)
    } else if (is.finite(rev)) {
      facts[[length(facts) + 1L]] <- .or_focus_fact(nm, rev, "amount", src)
    }
  }
  facts
}

.or_call_party_facts <- function(rows, source = "earnings_call") {
  if (!is.list(rows)) return(list())
  facts <- list()
  for (r in rows) {
    if (!is.list(r)) next
    nm <- .or_chr(r$name %||% r$label, "")
    share <- .or_num(r$share %||% r$revenue_pct)
    amt <- .or_num(r$revenue %||% r$amount %||% r$value)
    if (!nzchar(nm)) next
    if (is.finite(share)) {
      facts[[length(facts) + 1L]] <- .or_focus_fact(nm, share, "ratio", source)
    } else if (is.finite(amt)) {
      facts[[length(facts) + 1L]] <- .or_focus_fact(nm, amt, "amount", source)
    }
  }
  facts
}

.or_filter_amount_tiers <- function(facts) {
  Filter(function(f) {
    meta <- .or_tier_meta(f$source)
    isTRUE(meta$can_fill_amount)
  }, facts %||% list())
}

.or_material_facts <- function(rows) {
  if (!is.list(rows)) return(list())
  facts <- list()
  for (r in rows) {
    if (!is.list(r)) next
    nm <- .or_chr(r$name %||% r$label %||% r$title, "")
    amt <- .or_num(r$amount %||% r$value %||% r$revenue)
    if (!is.finite(amt)) next
    facts[[length(facts) + 1L]] <- .or_focus_fact(
      if (nzchar(nm)) nm else "",
      amt, "amount", "mops_material",
      label_key = if (!nzchar(nm)) "bblab_or_metric_bookings" else NA_character_,
      order_type = .or_chr(r$order_type, "award"),
      url = .or_chr(r$url, NA_character_)
    )
  }
  facts
}

#' Three disclosed buckets: order quantity, major suppliers, major-customer revenue mix.
.or_build_focus <- function(inputs, order_concentration, disclosures, tables) {
  inputs <- inputs %||% list()
  disclosures <- disclosures %||% list()
  call <- disclosures$earnings_call %||% list()
  ir <- disclosures$company_ir %||% list()
  orders <- list()
  push_order <- function(label_key, value, source, order_type = "unknown") {
    v <- .or_num(value)
    if (!is.finite(v)) return()
    orders[[length(orders) + 1L]] <<- .or_focus_fact(
      "", v, "amount", source, label_key = label_key, order_type = order_type
    )
  }
  push_order("bblab_or_metric_backlog", inputs$backlog, inputs$backlog_source %||% "mops_filing")
  push_order("bblab_or_metric_bookings", inputs$bookings, inputs$bookings_source %||% "mops_filing")
  if (is.finite(.or_num(call$backlog))) {
    push_order("bblab_or_metric_backlog", call$backlog, "earnings_call", "guidance")
  }
  if (is.finite(.or_num(call$bookings))) {
    push_order("bblab_or_metric_bookings", call$bookings, "earnings_call", "guidance")
  }
  if (is.finite(.or_num(ir$backlog))) {
    push_order("bblab_or_metric_backlog", ir$backlog, "company_ir", "guidance")
  }
  if (is.finite(.or_num(ir$bookings))) {
    push_order("bblab_or_metric_bookings", ir$bookings, "company_ir", "guidance")
  }
  orders <- c(orders, .or_material_facts(disclosures$mops_material %||% disclosures$material_awards))
  for (tbl in tables %||% list()) {
    if (!is.list(tbl)) next
    kind <- tolower(.or_chr(tbl$kind, ""))
    tier <- .or_table_source(tbl)
    if (!isTRUE(.or_tier_meta(tier)$can_fill_amount)) next
    if (kind %in% c("orders", "order_concentration", "material", "award")) {
      orders <- c(orders, .or_named_amount_facts(tbl))
    }
  }
  orders <- c(orders, .or_call_party_facts(call$order_lines, "earnings_call"))
  orders <- .or_dedupe_facts(.or_filter_amount_tiers(orders))

  suppliers <- .or_facts_from_slices(order_concentration$supplier_concentration)
  customers <- .or_facts_from_slices(order_concentration$customer_concentration)
  for (tbl in tables %||% list()) {
    if (!is.list(tbl)) next
    tier <- .or_table_source(tbl)
    if (!isTRUE(.or_tier_meta(tier)$can_fill_amount)) next
    kind <- tolower(.or_chr(tbl$kind, ""))
    if (kind %in% c("supplier", "supplier_concentration", "suppliers")) {
      suppliers <- c(suppliers, .or_named_amount_facts(tbl))
    } else if (kind %in% c("customer", "customer_concentration", "customers")) {
      customers <- c(customers, .or_named_amount_facts(tbl))
    }
  }
  suppliers <- c(suppliers, .or_call_party_facts(call$suppliers, "earnings_call"))
  customers <- c(customers, .or_call_party_facts(call$customers, "earnings_call"))
  suppliers <- .or_dedupe_facts(.or_filter_amount_tiers(suppliers))
  customers <- .or_dedupe_facts(.or_filter_amount_tiers(customers))

  bucket <- function(id, facts, gap) {
    finite <- vapply(facts, function(f) is.finite(.or_num(f$value)), logical(1))
    list(
      id = id,
      status = if (any(finite)) "disclosed" else "gap",
      facts = facts,
      gap = if (any(finite)) NA_character_ else gap
    )
  }
  list(
    bucket("orders", orders, "ORDERS_BACKLOG_UNDISCLOSED"),
    bucket("suppliers", suppliers, "ORDERS_SUPPLIER_DISCLOSURE_MISSING"),
    bucket("customers", customers, "ORDERS_CUSTOMER_DISCLOSURE_MISSING")
  )
}

.or_build_source_stack <- function(focus) {
  used <- unique(unlist(lapply(focus %||% list(), function(b) {
    vapply(b$facts %||% list(), function(f) .or_chr(f$source, ""), character(1))
  }), use.names = FALSE))
  used <- used[nzchar(used)]
  lapply(BBLAB_ORDER_SOURCE_STACK, function(t) {
    list(
      id = t$id,
      priority = t$priority,
      credibility = t$credibility,
      can_fill_amount = isTRUE(t$can_fill_amount),
      contributed = t$id %in% used
    )
  })
}

.or_build_cross_check <- function(inputs) {
  inputs <- inputs %||% list()
  item <- function(id, value) {
    v <- .or_num(value)
    list(
      id = id,
      value = if (is.finite(v)) v else NA_real_,
      unit = "amount",
      source = "statement_proxy",
      credibility = "proxy",
      status = if (is.finite(v)) "REPORTED" else "UNAVAILABLE",
      gap = if (is.finite(v)) NA_character_ else "ORDERS_CASH_ITEM_UNDISCLOSED",
      note_key = paste0("bblab_or_xcheck_note_", id)
    )
  }
  list(
    item("contract_liability", inputs$deferred_revenue),
    item("rpo", inputs$backlog),
    item("contract_assets", inputs$contract_assets),
    item("inventory", inputs$inventory),
    item("accounts_receivable", inputs$accounts_receivable),
    item("operating_cash_flow", inputs$operating_cash_flow)
  )
}

.or_top5_share <- function(amounts) {
  amts <- .or_num_vec(amounts)
  amts <- amts[is.finite(amts) & amts > 0]
  if (!length(amts)) return(NA_real_)
  amts <- sort(amts, decreasing = TRUE)
  tot <- sum(amts)
  if (!is.finite(tot) || tot == 0) return(NA_real_)
  sum(amts[seq_len(min(5L, length(amts)))]) / tot
}

.or_num_vec <- function(x) {
  if (is.null(x) || !length(x)) return(numeric(0))
  if (is.list(x) && !is.numeric(x)) {
    x <- vapply(x, function(el) {
      if (is.list(el)) .or_num(el$amount %||% el$revenue) else .or_num(el)
    }, numeric(1))
  }
  suppressWarnings(as.numeric(x))
}

bblab_order_research_empty <- function(gaps = character(0)) {
  blank_kpi <- function(id, unit, formula, gap, callout = FALSE) {
    .or_kpi(id, NA_real_, unit, formula, gap, callout = callout)
  }
  kpis <- list(
    backlog_yoy = blank_kpi(
      "backlog_yoy", "ratio", "backlog_t / backlog_t-4 - 1",
      "ORDERS_BACKLOG_UNDISCLOSED"
    ),
    book_to_bill = blank_kpi(
      "book_to_bill", "multiple", "bookings / revenue",
      "ORDERS_BOOKINGS_UNDISCLOSED", callout = TRUE
    ),
    backlog_revenue_cover = blank_kpi(
      "backlog_revenue_cover", "multiple", "backlog / ttm_revenue",
      "ORDERS_BACKLOG_UNDISCLOSED", callout = TRUE
    ),
    order_conversion = blank_kpi(
      "order_conversion", "ratio", "recognized_from_cohort / cohort_original",
      "ORDERS_CONVERSION_UNDISCLOSED"
    ),
    est_order_gross_profit = blank_kpi(
      "est_order_gross_profit", "amount",
      "est_order_revenue - est_fulfillment_cost",
      "ORDERS_ORDER_PROFIT_UNDISCLOSED"
    ),
    customer_concentration = blank_kpi(
      "customer_concentration", "ratio", "top5_order_dollars / total_order_dollars",
      "ORDERS_ORDER_DOLLARS_UNDISCLOSED"
    )
  )
  layer <- function(id, key) {
    list(
      id = as.integer(id),
      key = key,
      status = "gap",
      metrics = list(),
      note_key = paste0("bblab_or_note_", key),
      evidence = list()
    )
  }
  focus <- .or_build_focus(list(), NULL, NULL, list())
  list(
    layers = lapply(seq_along(BBLAB_ORDER_RESEARCH_LAYER_KEYS), function(i) {
      layer(i, BBLAB_ORDER_RESEARCH_LAYER_KEYS[[i]])
    }),
    kpis = kpis,
    industry_profile = "other",
    industry_primary = character(0),
    sources = list(),
    gaps = unique(as.character(gaps)),
    focus = focus,
    source_stack = .or_build_source_stack(focus),
    cross_check = .or_build_cross_check(list()),
    chart = list(multiples = list(), rates = list(), orders = list(), customers = list())
  )
}

.or_layer_status <- function(metrics) {
  if (!length(metrics)) return("gap")
  finite <- vapply(metrics, function(m) is.finite(.or_num(m$value)), logical(1))
  if (!any(finite)) return("gap")
  if (all(finite)) return("disclosed")
  "partial"
}

.or_push_source <- function(sources, metric, provenance, detail = NA_character_) {
  if (!nzchar(.or_chr(provenance, ""))) return(sources)
  c(sources, list(list(
    metric = metric,
    provenance = provenance,
    detail = .or_chr(detail, NA_character_)
  )))
}

.or_chart_points <- function(kpis) {
  multiples <- list()
  rates <- list()
  for (id in c("book_to_bill", "backlog_revenue_cover")) {
    v <- .or_num(kpis[[id]]$value)
    if (is.finite(v)) multiples[[length(multiples) + 1L]] <- list(id = id, value = v)
  }
  for (id in c("backlog_yoy", "order_conversion", "customer_concentration")) {
    v <- .or_num(kpis[[id]]$value)
    if (is.finite(v)) rates[[length(rates) + 1L]] <- list(id = id, value = v)
  }
  list(multiples = multiples, rates = rates)
}

#' Pure KPI math. Each value is NA unless every input in the formula is finite.
bblab_order_research_kpis <- function(inputs = list()) {
  inputs <- inputs %||% list()
  backlog <- .or_num(inputs$backlog)
  backlog_prior <- .or_num(inputs$backlog_prior)
  bookings <- .or_num(inputs$bookings)
  revenue <- .or_num(inputs$revenue)
  ttm <- .or_num(inputs$ttm_revenue)
  cover_den <- ttm
  cover_gap <- if (!is.finite(backlog)) {
    "ORDERS_BACKLOG_UNDISCLOSED"
  } else if (!is.finite(cover_den)) {
    "ORDERS_TTM_REVENUE_UNDISCLOSED"
  } else {
    "ORDERS_BACKLOG_UNDISCLOSED"
  }
  yoy <- .or_ratio(backlog, backlog_prior) 
  if (is.finite(yoy)) yoy <- yoy - 1
  btb_gap <- if (!is.finite(bookings)) "ORDERS_BOOKINGS_UNDISCLOSED" else "ORDERS_TTM_REVENUE_UNDISCLOSED"
  if (!is.finite(bookings) && !is.finite(revenue)) btb_gap <- "ORDERS_BOOKINGS_UNDISCLOSED"
  recognized <- .or_num(inputs$recognized_from_cohort)
  cohort <- .or_num(inputs$cohort_original)
  est_rev <- .or_num(inputs$est_order_revenue)
  est_cost <- .or_num(inputs$est_fulfillment_cost)
  # Both legs must be disclosed. Consolidated gross margin times backlog is not an order cost.
  est_gp <- if (is.finite(est_rev) && is.finite(est_cost)) est_rev - est_cost else NA_real_
  conc <- .or_top5_share(inputs$order_amounts)
  src <- function(nm, fallback = NA_character_) .or_chr(inputs[[nm]], fallback)
  list(
    backlog_yoy = .or_kpi(
      "backlog_yoy", yoy, "ratio", "backlog_t / backlog_t-4 - 1",
      "ORDERS_BACKLOG_UNDISCLOSED", src("backlog_source", "filing")
    ),
    book_to_bill = .or_kpi(
      "book_to_bill", .or_ratio(bookings, revenue), "multiple", "bookings / revenue",
      if (is.finite(.or_ratio(bookings, revenue))) NA_character_ else btb_gap,
      src("bookings_source", "filing"),
      callout = TRUE
    ),
    backlog_revenue_cover = .or_kpi(
      "backlog_revenue_cover", .or_ratio(backlog, cover_den), "multiple",
      "backlog / ttm_revenue",
      if (is.finite(.or_ratio(backlog, cover_den))) NA_character_ else cover_gap,
      src("backlog_source", "filing"),
      callout = TRUE
    ),
    order_conversion = .or_kpi(
      "order_conversion", .or_ratio(recognized, cohort), "ratio",
      "recognized_from_cohort / cohort_original",
      "ORDERS_CONVERSION_UNDISCLOSED", src("conversion_source", "filing")
    ),
    est_order_gross_profit = .or_kpi(
      "est_order_gross_profit", est_gp, "amount",
      "est_order_revenue - est_fulfillment_cost",
      "ORDERS_ORDER_PROFIT_UNDISCLOSED", src("profit_source", "filing")
    ),
    customer_concentration = .or_kpi(
      "customer_concentration", conc, "ratio",
      "top5_order_dollars / total_order_dollars",
      "ORDERS_ORDER_DOLLARS_UNDISCLOSED", src("order_amounts_source", "filing")
    )
  )
}

.or_collect_inputs <- function(disclosures, consolidated, income_statement,
                               balance_sheet, cash_flow, tables, frequency) {
  d <- disclosures %||% list()
  from_tbl <- .or_disclosures_from_tables(tables)
  for (nm in names(from_tbl)) {
    if (is.null(d[[nm]])) d[[nm]] <- from_tbl[[nm]]
  }
  freq <- .or_chr(frequency, "annual")
  if (!is.finite(.or_num(d$backlog))) {
    d$backlog <- .or_pick(
      income_statement,
      c("remaining performance obligation", "\\brpo\\b", "order backlog", "\\bbacklog\\b"),
      freq, "current"
    )
    if (!is.finite(.or_num(d$backlog))) {
      d$backlog <- .or_pick(
        balance_sheet,
        c("remaining performance obligation", "\\brpo\\b", "order backlog", "\\bbacklog\\b"),
        freq, "current"
      )
    }
    if (is.finite(.or_num(d$backlog)) && !nzchar(.or_chr(d$backlog_source, ""))) {
      d$backlog_source <- "statement"
    }
  }
  if (!is.finite(.or_num(d$backlog_prior)) && is.finite(.or_num(d$backlog))) {
    d$backlog_prior <- .or_pick(
      income_statement,
      c("remaining performance obligation", "\\brpo\\b", "order backlog", "\\bbacklog\\b"),
      freq, "prior"
    )
    if (!is.finite(.or_num(d$backlog_prior))) {
      d$backlog_prior <- .or_pick(
        balance_sheet,
        c("remaining performance obligation", "\\brpo\\b", "order backlog", "\\bbacklog\\b"),
        freq, "prior"
      )
    }
  }
  if (!is.finite(.or_num(d$bookings))) {
    d$bookings <- .or_pick(
      income_statement,
      c("^bookings$", "orders received", "new orders"),
      freq, "current"
    )
    if (is.finite(.or_num(d$bookings))) d$bookings_source <- "statement"
  }
  rev <- .or_num(consolidated$revenue)
  if (!is.finite(rev)) {
    rev <- .or_pick(income_statement, c("^Total Revenue$", "^Revenue$", "Operating Revenue"), freq, "current")
  }
  d$revenue <- rev
  ttm <- .or_num(d$ttm_revenue)
  if (!is.finite(ttm)) {
    ttm <- .or_pick(
      income_statement,
      c("^Total Revenue$", "^Revenue$", "Operating Revenue"),
      freq, "ttm"
    )
  }
  # An annual revenue column is the trailing-year denominator the coverage formula asks for.
  if (!is.finite(ttm) && tolower(freq) %in% c("annual", "year", "yearly") && is.finite(rev)) {
    ttm <- rev
    d$ttm_source <- "statement_annual"
  } else if (is.finite(ttm)) {
    d$ttm_source <- "statement_ttm"
  }
  d$ttm_revenue <- ttm
  gp <- .or_num(consolidated$gp)
  if (!is.finite(gp)) {
    gp <- .or_pick(income_statement, c("^Gross Profit$"), freq, "current")
  }
  d$gross_profit <- gp
  d$gross_margin <- .or_ratio(gp, rev)
  if (is.finite(d$gross_margin)) d$gross_margin_source <- "statement"
  d$accounts_receivable <- .or_num(d$accounts_receivable)
  if (!is.finite(d$accounts_receivable)) {
    d$accounts_receivable <- .or_pick(
      balance_sheet,
      c("^Accounts Receivable$", "^Gross Accounts Receivable$", "Net Accounts Receivable"),
      freq, "current"
    )
  }
  d$inventory <- .or_num(d$inventory)
  if (!is.finite(d$inventory)) {
    d$inventory <- .or_pick(balance_sheet, c("^Inventory$", "^Inventories$"), freq, "current")
  }
  d$contract_assets <- .or_num(d$contract_assets)
  if (!is.finite(d$contract_assets)) {
    d$contract_assets <- .or_pick(
      balance_sheet,
      c("Contract Asset", "Unbilled Receivable", "Unbilled Revenue"),
      freq, "current"
    )
  }
  d$deferred_revenue <- .or_num(d$deferred_revenue)
  if (!is.finite(d$deferred_revenue)) {
    d$deferred_revenue <- .or_pick(
      balance_sheet,
      c("^Current Deferred Revenue$", "^Deferred Revenue$", "Unearned Revenue", "Contract Liabilit"),
      freq, "current"
    )
  }
  d$operating_cash_flow <- .or_num(d$operating_cash_flow)
  if (!is.finite(d$operating_cash_flow)) {
    d$operating_cash_flow <- .or_pick(cash_flow, c("^Operating Cash Flow$"), freq, "current")
  }
  d
}

.or_revenue_evidence <- function(order_concentration) {
  ev <- list()
  if (!is.list(order_concentration)) return(ev)
  cust <- order_concentration$customer_concentration
  if (is.list(cust) && isTRUE(cust$eligible) && is.finite(.or_num(cust$top1_share))) {
    ev[[length(ev) + 1L]] <- list(
      metric = "revenue_customer_top1",
      value = .or_num(cust$top1_share),
      unit = "ratio",
      basis = "revenue_not_orders",
      source = .or_chr(cust$source, "filing"),
      status = "REPORTED"
    )
  }
  if (is.list(cust) && isTRUE(cust$eligible) && is.finite(.or_num(cust$top3_share))) {
    ev[[length(ev) + 1L]] <- list(
      metric = "revenue_customer_top3",
      value = .or_num(cust$top3_share),
      unit = "ratio",
      basis = "revenue_not_orders",
      source = .or_chr(cust$source, "filing"),
      status = "REPORTED"
    )
  }
  supp <- order_concentration$supplier_concentration
  if (is.list(supp) && isTRUE(supp$eligible) && is.finite(.or_num(supp$top1_share))) {
    ev[[length(ev) + 1L]] <- list(
      metric = "revenue_supplier_top1",
      value = .or_num(supp$top1_share),
      unit = "ratio",
      basis = "revenue_not_orders",
      source = .or_chr(supp$source, "filing"),
      status = "REPORTED"
    )
  }
  ev
}

#' Build one OrderResearchPack for the current ticker.
bblab_build_order_research_pack <- function(order_concentration = NULL,
                                            consolidated = NULL,
                                            disclosures = NULL,
                                            income_statement = NULL,
                                            balance_sheet = NULL,
                                            cash_flow = NULL,
                                            industry_key = NULL,
                                            industry_text = NULL,
                                            frequency = "annual",
                                            notes = NULL,
                                            segment_tables = NULL) {
  tables <- list()
  if (is.list(segment_tables) && length(segment_tables)) tables <- segment_tables
  if (exists(".bblab_coerce_segment_tables", mode = "function")) {
    coerced <- tryCatch(.bblab_coerce_segment_tables(segment_tables, notes), error = function(e) NULL)
    if (is.list(coerced) && length(coerced)) tables <- coerced
  }
  inputs <- .or_collect_inputs(
    disclosures, consolidated, income_statement, balance_sheet, cash_flow, tables, frequency
  )
  kpis <- bblab_order_research_kpis(inputs)
  profile <- bblab_order_research_industry_profile(industry_key, industry_text)
  primary <- BBLAB_ORDER_RESEARCH_PROFILES[[profile]]$primary %||% character(0)

  stmt <- "statement"
  scale_metrics <- list(
    .or_metric("backlog", inputs$backlog, "amount", gap = "ORDERS_BACKLOG_UNDISCLOSED",
               source = .or_chr(inputs$backlog_source, NA_character_)),
    .or_metric("bookings", inputs$bookings, "amount", gap = "ORDERS_BOOKINGS_UNDISCLOSED",
               source = .or_chr(inputs$bookings_source, NA_character_)),
    kpis$backlog_yoy
  )
  growth <- .or_chr(disclosures$growth_source, "")
  if (nzchar(growth)) {
    scale_metrics[[length(scale_metrics) + 1L]] <- list(
      id = "growth_source", value = 1, unit = "text", status = "REPORTED",
      gap = NA_character_, source = .or_chr(disclosures$growth_source_provenance, "management"),
      formula = NA_character_, text = growth
    )
  } else {
    scale_metrics[[length(scale_metrics) + 1L]] <- .or_metric(
      "growth_source", NA_real_, "text", gap = "ORDERS_GROWTH_SOURCE_UNDISCLOSED"
    )
  }

  quality_terms <- disclosures$quality_terms
  quality_metrics <- list(
    .or_metric(
      "quality_terms",
      if (is.character(quality_terms) && length(quality_terms) && nzchar(quality_terms[[1]])) 1 else NA_real_,
      "flag",
      gap = "ORDERS_QUALITY_TERMS_UNDISCLOSED",
      source = if (is.character(quality_terms) && nzchar(quality_terms[[1]])) "filing" else NA_character_
    ),
    kpis$customer_concentration
  )
  evidence <- .or_revenue_evidence(order_concentration)

  # Cohort conversion only. A next-four-quarter rate is an assumption, so it is not a metric.
  conv_metrics <- list(kpis$order_conversion)
  profit_metrics <- list(
    .or_metric("gross_margin", inputs$gross_margin, "ratio",
               gap = "ORDERS_GROSS_MARGIN_UNDISCLOSED",
               source = .or_chr(inputs$gross_margin_source, stmt)),
    kpis$est_order_gross_profit
  )
  cash_item <- function(id, value) {
    .or_metric(id, value, "amount", gap = "ORDERS_CASH_ITEM_UNDISCLOSED", source = stmt)
  }
  cash_metrics <- list(
    cash_item("accounts_receivable", inputs$accounts_receivable),
    cash_item("contract_assets", inputs$contract_assets),
    cash_item("inventory", inputs$inventory),
    cash_item("deferred_revenue", inputs$deferred_revenue),
    cash_item("operating_cash_flow", inputs$operating_cash_flow)
  )
  # Coverage is a filed status multiple. EPS, FCFF, and MOS are not estimated here.
  val_metrics <- if (is.finite(.or_num(kpis$backlog_revenue_cover$value))) {
    list(kpis$backlog_revenue_cover)
  } else {
    list()
  }

  layers_metrics <- list(
    scale_metrics, quality_metrics, conv_metrics, profit_metrics, cash_metrics, val_metrics
  )
  layers <- lapply(seq_along(BBLAB_ORDER_RESEARCH_LAYER_KEYS), function(i) {
    key <- BBLAB_ORDER_RESEARCH_LAYER_KEYS[[i]]
    ms <- layers_metrics[[i]]
    list(
      id = as.integer(i),
      key = key,
      status = .or_layer_status(ms),
      metrics = ms,
      note_key = paste0("bblab_or_note_", key),
      evidence = if (identical(key, "quality")) evidence else list()
    )
  })

  sources <- list()
  for (id in BBLAB_ORDER_RESEARCH_KPI_IDS) {
    kpi <- kpis[[id]]
    if (is.finite(.or_num(kpi$value))) {
      sources <- .or_push_source(sources, id, kpi$source, kpi$formula)
    }
  }
  for (ev in evidence) {
    sources <- .or_push_source(sources, ev$metric, ev$source, ev$basis)
  }
  if (is.finite(.or_num(inputs$gross_margin))) {
    sources <- .or_push_source(sources, "gross_margin", inputs$gross_margin_source, "gp / revenue")
  }
  for (id in c("accounts_receivable", "inventory", "deferred_revenue",
               "contract_assets", "operating_cash_flow")) {
    if (is.finite(.or_num(inputs[[id]]))) sources <- .or_push_source(sources, id, stmt, NA_character_)
  }

  gaps <- character(0)
  collect_gap <- function(node) {
    g <- .or_chr(node$gap, "")
    if (nzchar(g) && !is.na(g)) gaps <<- c(gaps, g)
  }
  for (kpi in kpis) collect_gap(kpi)
  for (layer in layers) for (m in layer$metrics) collect_gap(m)
  focus <- .or_build_focus(inputs, order_concentration, disclosures, tables)
  chart <- .or_chart_points(kpis)
  chart_pts <- function(facts) {
    pts <- list()
    for (f in facts %||% list()) {
      if (!is.finite(.or_num(f$value))) next
      label <- if (nzchar(.or_chr(f$name, ""))) f$name else NA_character_
      id <- if (nzchar(.or_chr(f$label_key, ""))) {
        sub("^bblab_or_metric_", "", f$label_key)
      } else {
        .or_chr(f$name, "fact")
      }
      pts[[length(pts) + 1L]] <- list(
        id = id, label = label, value = f$value, unit = f$unit, source = f$source
      )
    }
    pts
  }
  chart$orders <- chart_pts(Filter(function(f) identical(f$unit, "amount"), focus[[1]]$facts))
  chart$customers <- chart_pts(Filter(function(f) identical(f$unit, "ratio"), focus[[3]]$facts))

  list(
    layers = layers,
    kpis = kpis,
    industry_profile = profile,
    industry_primary = as.character(primary),
    sources = sources,
    gaps = unique(gaps),
    focus = focus,
    source_stack = .or_build_source_stack(focus),
    cross_check = .or_build_cross_check(inputs),
    chart = chart
  )
}

bblab_attach_order_research <- function(result, payload = list()) {
  if (!is.list(result)) return(result)
  payload <- payload %||% list()
  result$order_research <- tryCatch(
    bblab_build_order_research_pack(
      order_concentration = result$order_concentration,
      consolidated = result$consolidated %||% payload$consolidated,
      disclosures = payload$order_disclosures,
      income_statement = payload$income_statement,
      balance_sheet = payload$balance_sheet,
      cash_flow = payload$cash_flow,
      industry_key = payload$industry_key,
      industry_text = payload$industry_text,
      frequency = payload$frequency %||% result$frequency %||% "annual",
      notes = payload$notes,
      segment_tables = payload$segment_tables
    ),
    error = function(e) bblab_order_research_empty("ORDERS_BACKLOG_UNDISCLOSED")
  )
  result
}
