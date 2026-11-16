# Group cost attribution & segment profitability (FSA / Business Breakdown).
# Direct / Shared (disclosed bases only) / Corporate-Unallocated / Intercompany.
# Never invents Segment Gross Margin without Segment COGS.
# Never allocates all group costs by bare revenue share without a disclosed basis.
# Corporate / Elimination / Reconciliation are never operating segments.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

BBLAB_COST_CLASSES <- c(
  "DIRECT",
  "SHARED",
  "CORPORATE_UNALLOCATED",
  "INTERCOMPANY",
  "UNAVAILABLE"
)

BBLAB_PROFIT_STATUS_LADDER <- c(
  "REPORTED",
  "DERIVED",
  "ESTIMATED",
  "ALLOCATED",
  "UNAVAILABLE"
)

BBLAB_COST_PROFIT_INDUSTRY_PROFILES <- list(
  manufacturing = list(
    id = "manufacturing",
    primary_cost = "COGS",
    allow_gp = TRUE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    key_prefix = c("ind.Machinery", "ind.Industrial", "sc.Foundry", "sc.IDM", "sc.OSAT"),
    text = c("manufactur", "foundry", "semiconductor", "industrial", "machinery", "automotive")
  ),
  saas = list(
    id = "saas",
    primary_cost = "COST_OF_REVENUE_OR_COGS",
    allow_gp = TRUE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    # SaaS OpEx (S&M / R&D / G&A) stays below Gross Profit.
    key_prefix = c("saas.", "tech.Software", "tech.Internet"),
    text = c("saas", "software", "cloud", "subscription")
  ),
  retail = list(
    id = "retail",
    primary_cost = "COGS",
    allow_gp = TRUE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    key_prefix = c("ret.", "cons.Retail"),
    text = c("retail", "store", "merchandise", "e-?commerce", "supermarket")
  ),
  holding = list(
    id = "holding",
    primary_cost = "CORPORATE_AND_INVESTMENT",
    allow_gp = FALSE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    key_prefix = c("fin.Holding", "cong."),
    text = c("holding company", "conglomerate", "investment holding")
  ),
  financial = list(
    id = "financial",
    primary_cost = "INTEREST_AND_PROVISION",
    allow_gp = FALSE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    key_prefix = c("fin.Bank", "fin.Insurance", "fin.Broker"),
    text = c("\\bbank\\b", "insurance", "broker", "financial services", "nii", "net interest")
  ),
  other = list(
    id = "other",
    primary_cost = "COGS_OR_OPEX",
    allow_gp = TRUE,
    mix_opex_into_gp = FALSE,
    mix_investment_into_opinc = FALSE,
    key_prefix = character(0),
    text = character(0)
  )
)

#' Map industry key / free text onto a cost-profit lens. Unknown stays "other".
bblab_cost_profit_industry_profile <- function(industry_key = NULL,
                                               industry_text = NULL) {
  key <- .bblab_chr(industry_key, "")
  text <- tolower(.bblab_chr(industry_text, ""))
  order <- c("financial", "holding", "saas", "retail", "manufacturing", "other")
  for (id in order) {
    if (identical(id, "other")) next
    spec <- BBLAB_COST_PROFIT_INDUSTRY_PROFILES[[id]]
    if (nzchar(key) && length(spec$key_prefix) &&
        any(vapply(spec$key_prefix, function(p) startsWith(key, p), logical(1)))) {
      return(id)
    }
    if (nzchar(text) && length(spec$text) &&
        any(vapply(spec$text, function(p) grepl(p, text, ignore.case = TRUE, perl = TRUE),
                   logical(1)))) {
      return(id)
    }
  }
  "other"
}

.bblab_disclosed_allocation_basis <- function(comp) {
  basis <- .bblab_chr(
    comp$allocation_basis %||% comp$allocationBasis %||%
      comp$disclosed_allocation_basis %||% "",
    ""
  )
  if (!nzchar(basis)) return(NA_character_)
  basis
}

#' Classify one cost line into Direct / Shared / Corporate-Unallocated / Intercompany.
#' Shared requires a company-disclosed allocation basis; bare revenue-share invent is forbidden.
bblab_classify_cost_attribution <- function(comp) {
  if (is.null(comp) || !is.list(comp)) return("UNAVAILABLE")
  role <- .bblab_chr(comp$economic_role, "")
  cls <- toupper(.bblab_chr(comp$classification, ""))
  method <- .bblab_chr(comp$cor_method, "")
  basis <- .bblab_disclosed_allocation_basis(comp)

  if (identical(cls, "ELIMINATION") || identical(role, "ELIMINATION") ||
      identical(method, "intercompany_elimination")) {
    return("INTERCOMPANY")
  }
  if (identical(cls, "UNALLOCATED") || identical(cls, "SHARED_CORPORATE") ||
      identical(role, "CORPORATE") ||
      identical(method, "corporate_unallocated")) {
    return("CORPORATE_UNALLOCATED")
  }
  if (method %in% c("reported_cor", "reported_gp")) {
    return("DIRECT")
  }
  if (method %in% c("company_allocation", "observable_driver", "justified_proxy")) {
    # Shared only when the filer disclosed an allocation / driver basis.
    if (nzchar(.bblab_chr(basis, "")) ||
        .bblab_finite(comp$company_allocated_cor) ||
        .bblab_finite(comp$observable_driver_cor) ||
        .bblab_finite(comp$justified_proxy_cor)) {
      return("SHARED")
    }
    return("UNAVAILABLE")
  }
  if (identical(method, "revenue_share")) {
    # Bare revenue-share invent without disclosed basis is not a valid attribution.
    if (nzchar(.bblab_chr(basis, ""))) return("SHARED")
    return("UNAVAILABLE")
  }
  if (identical(method, "consolidated_gm_fallback")) {
    return("UNAVAILABLE")
  }
  if (.bblab_finite(comp$cor)) return("DIRECT")
  "UNAVAILABLE"
}

#' Normalize a metric status onto the Reported → Derived → Estimated/Allocated → Unavailable ladder.
bblab_normalize_profit_status <- function(status, method = NA_character_) {
  st <- toupper(.bblab_chr(status, "UNAVAILABLE"))
  if (st %in% c("REPORTED", "DERIVED", "ESTIMATED", "ALLOCATED", "UNAVAILABLE")) return(st)
  if (st %in% c("CONSOLIDATED_PROXY", "UNALLOCATED")) return("UNAVAILABLE")
  meth <- .bblab_chr(method, "")
  if (meth %in% c("reported", "reported_cor", "reported_oi")) return("REPORTED")
  if (meth %in% c("reported_gp", "percent_times_consolidated", "mapped_product",
                  "derived_sum", "rev_minus_cor", "oi_minus_shared")) return("DERIVED")
  if (meth %in% c("company_allocation", "revenue_share")) return("ALLOCATED")
  if (meth %in% c("observable_driver", "justified_proxy", "operational_driver",
                  "consolidated_gm_fallback")) return("ESTIMATED")
  "UNAVAILABLE"
}

.bblab_status_label_with_method <- function(status, method = NA_character_) {
  st <- bblab_normalize_profit_status(status, method)
  meth <- .bblab_chr(method, "")
  if (st %in% c("ESTIMATED", "ALLOCATED") && nzchar(meth)) {
    paste0(st, " (", meth, ")")
  } else st
}

#' Segment Gross Margin only when Segment COGS (or reported GP) exists.
.bblab_segment_gp_pack <- function(comp, industry_profile = "other") {
  prof <- BBLAB_COST_PROFIT_INDUSTRY_PROFILES[[industry_profile]] %||%
    BBLAB_COST_PROFIT_INDUSTRY_PROFILES$other
  rev <- .bblab_num(comp$revenue)
  cor <- .bblab_num(comp$cor)
  gp_raw <- .bblab_num(comp$gp)
  cor_method <- .bblab_chr(comp$cor_method, "")
  cor_status <- bblab_normalize_profit_status(
    comp$cor_evidence$status %||% NA_character_, cor_method
  )

  # Holdings / financials: do not invent Gross Margin from OpEx or investment lines.
  if (!isTRUE(prof$allow_gp)) {
    return(list(
      cor = NA_real_, gp = NA_real_, gm = NA_real_,
      cor_status = "UNAVAILABLE", gp_status = "UNAVAILABLE", gm_status = "UNAVAILABLE",
      cor_method = cor_method, cost_class = bblab_classify_cost_attribution(comp),
      note = "Industry lens withholds Segment Gross Margin (not a COGS business model)."
    ))
  }

  has_cogs <- is.finite(cor) ||
    (isTRUE(comp$gp_reported) && is.finite(gp_raw) && is.finite(rev))
  if (!isTRUE(has_cogs)) {
    return(list(
      cor = NA_real_, gp = NA_real_, gm = NA_real_,
      cor_status = if (identical(cor_status, "UNAVAILABLE")) "UNAVAILABLE" else cor_status,
      gp_status = "UNAVAILABLE", gm_status = "UNAVAILABLE",
      cor_method = cor_method, cost_class = bblab_classify_cost_attribution(comp),
      note = "Segment Gross Margin withheld: Segment COGS not disclosed or reliably attributable."
    ))
  }

  if (!is.finite(cor) && is.finite(gp_raw) && is.finite(rev)) {
    cor <- rev - gp_raw
    cor_status <- "DERIVED"
    cor_method <- if (!nzchar(cor_method)) "reported_gp" else cor_method
  }
  gp <- if (is.finite(gp_raw)) gp_raw else if (is.finite(rev) && is.finite(cor)) rev - cor else NA_real_
  gm <- if (is.finite(gp) && is.finite(rev) && rev != 0) gp / rev else NA_real_
  gp_status <- if (isTRUE(comp$gp_reported) && is.finite(gp_raw)) {
    "REPORTED"
  } else if (is.finite(gp)) {
    "DERIVED"
  } else {
    "UNAVAILABLE"
  }
  # Never upgrade estimates to Reported.
  if (cor_status %in% c("ESTIMATED", "ALLOCATED") && identical(gp_status, "REPORTED")) {
    gp_status <- cor_status
  }
  gm_status <- if (is.finite(gm)) gp_status else "UNAVAILABLE"

  list(
    cor = cor, gp = gp, gm = gm,
    cor_status = cor_status,
    gp_status = gp_status,
    gm_status = gm_status,
    cor_method = cor_method,
    cost_class = bblab_classify_cost_attribution(comp),
    note = NA_character_
  )
}

.bblab_segment_oi_pack <- function(comp) {
  oi <- .bblab_num(comp$operating_income)
  rev <- .bblab_num(comp$revenue)
  if (!is.finite(oi)) {
    return(list(
      operating_income = NA_real_,
      oi_margin = NA_real_,
      oi_status = "UNAVAILABLE",
      oi_method = NA_character_,
      note = "Segment Operating Income not disclosed; not estimated."
    ))
  }
  # OI is only Reported when filed; never invent from GP − allocated OpEx here.
  st <- if (isTRUE(comp$oi_reported) || isTRUE(comp$operating_income_reported) ||
            identical(.bblab_chr(comp$oi_method), "reported") ||
            identical(.bblab_chr(comp$oi_method), "reported_oi")) {
    "REPORTED"
  } else if (identical(.bblab_chr(comp$oi_method), "derived")) {
    "DERIVED"
  } else {
    # Default: disclosed numeric on the segment note → Reported.
    "REPORTED"
  }
  list(
    operating_income = oi,
    oi_margin = if (is.finite(rev) && rev != 0) oi / rev else NA_real_,
    oi_status = st,
    oi_method = .bblab_chr(comp$oi_method, "reported_oi"),
    note = NA_character_
  )
}

#' Build per-segment profitability with status ladder + cost class.
#' Corporate / Unallocated stay separate; estimates are never labeled disclosed.
bblab_build_segment_profitability <- function(res, industry_profile = "other") {
  empty <- list(
    ok = FALSE,
    industry_profile = industry_profile,
    segments = list(),
    corporate_unallocated = list(),
    consolidated = list(),
    gaps = character(0)
  )
  if (is.null(res) || !is.list(res)) return(empty)
  prof_id <- .bblab_chr(industry_profile, "other")
  if (!prof_id %in% names(BBLAB_COST_PROFIT_INDUSTRY_PROFILES)) prof_id <- "other"
  cons <- res$consolidated %||% list()
  cons_rev <- .bblab_num(cons$revenue)
  cons_oi <- .bblab_num(cons$operating_income)
  cons_gp <- .bblab_num(cons$gp)
  cons_cor <- .bblab_num(cons$cor)

  rows <- list()
  gaps <- character(0)
  for (comp in res$businesses %||% list()) {
    if (is.null(comp) || !is.list(comp)) next
    role <- .bblab_chr(comp$economic_role, "BUSINESS")
    if (role %in% c("CORPORATE", "ELIMINATION", "RECONCILIATION",
                    "NON_OPERATING", "ACCOUNTING_ADJUSTMENT")) next
    gp_pack <- .bblab_segment_gp_pack(comp, prof_id)
    oi_pack <- .bblab_segment_oi_pack(comp)
    rev <- .bblab_num(comp$revenue)
    rev_st <- bblab_normalize_profit_status(
      comp$revenue_evidence$status %||% NA_character_,
      comp$revenue_method
    )
    if (identical(gp_pack$gm_status, "UNAVAILABLE") &&
        nzchar(.bblab_chr(gp_pack$note, ""))) {
      gaps <- c(gaps, paste0(comp$name %||% comp$id, ": ", gp_pack$note))
    }
    if (identical(oi_pack$oi_status, "UNAVAILABLE")) {
      gaps <- c(gaps, paste0(comp$name %||% comp$id, ": ", oi_pack$note))
    }
    rows[[length(rows) + 1L]] <- list(
      id = .bblab_chr(comp$id),
      name = .bblab_chr(comp$name, comp$id),
      role = "BUSINESS",
      revenue = rev,
      revenue_status = rev_st,
      revenue_method = .bblab_chr(comp$revenue_method, NA_character_),
      cor = gp_pack$cor,
      cor_status = gp_pack$cor_status,
      cor_method = .bblab_status_label_with_method(gp_pack$cor_status, gp_pack$cor_method),
      cost_class = gp_pack$cost_class,
      gp = gp_pack$gp,
      gp_status = gp_pack$gp_status,
      gm = gp_pack$gm,
      gm_status = gp_pack$gm_status,
      operating_income = oi_pack$operating_income,
      oi_status = oi_pack$oi_status,
      oi_method = .bblab_status_label_with_method(oi_pack$oi_status, oi_pack$oi_method),
      oi_margin = oi_pack$oi_margin,
      period = .bblab_chr(comp$period, cons$period),
      currency = .bblab_chr(comp$currency, cons$currency),
      provenance = list(
        revenue = comp$revenue_evidence,
        cor = comp$cor_evidence,
        gp = comp$gp_evidence
      )
    )
  }

  corp_items <- list()
  push_corp <- function(comp, default_name) {
    if (is.null(comp) || !is.list(comp)) return()
    amt_rev <- .bblab_num(comp$revenue)
    amt_oi <- .bblab_num(comp$operating_income)
    corp_items[[length(corp_items) + 1L]] <<- list(
      id = .bblab_chr(comp$id, default_name),
      name = .bblab_chr(comp$name, default_name),
      role = .bblab_chr(comp$economic_role, "CORPORATE"),
      cost_class = bblab_classify_cost_attribution(comp),
      revenue = amt_rev,
      operating_income = amt_oi,
      status = if (.bblab_finite(amt_rev) || .bblab_finite(amt_oi)) "REPORTED" else "UNAVAILABLE"
    )
  }
  push_corp(res$unallocated, "Corporate / Unallocated")
  for (c in res$shared_corporate %||% list()) {
    if (is.list(c) && !is.null(c$name)) push_corp(c, "Shared / Corporate")
  }

  list(
    ok = length(rows) >= 1L || length(corp_items) >= 1L,
    industry_profile = prof_id,
    segments = rows,
    corporate_unallocated = corp_items,
    consolidated = list(
      revenue = cons_rev,
      cor = cons_cor,
      gp = cons_gp,
      operating_income = cons_oi,
      gm = if (is.finite(cons_gp) && is.finite(cons_rev) && cons_rev != 0) {
        cons_gp / cons_rev
      } else NA_real_,
      status = if (is.finite(cons_rev)) "REPORTED" else "UNAVAILABLE"
    ),
    gaps = unique(gaps)
  )
}

.bblab_bridge_step <- function(label, amount, role, status = "REPORTED",
                               sign = c("plus", "minus", "equals", "start")) {
  sign <- match.arg(sign)
  list(
    label = .bblab_chr(label),
    amount = .bblab_num(amount),
    role = .bblab_chr(role, "BUSINESS"),
    status = bblab_normalize_profit_status(status),
    sign = sign,
    is_operating_segment = identical(role, "BUSINESS") || identical(role, "OTHER_OPERATING")
  )
}

#' Revenue Bridge: segment revenues → other → eliminations → corporate/unalloc → consolidated.
bblab_build_revenue_bridge <- function(res) {
  empty <- list(ok = FALSE, steps = list(), pass = FALSE, leftover = NA_real_,
                consolidated = NA_real_, note = "Revenue bridge unavailable.")
  if (is.null(res) || !is.list(res)) return(empty)
  cons <- res$consolidated %||% list()
  cons_rev <- .bblab_num(cons$revenue)
  steps <- list()
  running <- 0

  for (comp in res$businesses %||% list()) {
    rev <- .bblab_num(comp$revenue)
    if (!is.finite(rev)) next
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      comp$name %||% comp$id, rev, "BUSINESS",
      comp$revenue_evidence$status %||% "REPORTED",
      if (!length(steps)) "start" else "plus"
    )
    running <- running + rev
  }
  if (!is.null(res$other) && .bblab_finite(res$other$revenue)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$other$name %||% "Other Businesses", res$other$revenue, "OTHER_OPERATING",
      "DERIVED", "plus"
    )
    running <- running + as.numeric(res$other$revenue)[1]
  }
  if (!is.null(res$eliminations) && .bblab_finite(res$eliminations$revenue)) {
    # Intercompany: show as elimination (typically negative / subtract on consol).
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$eliminations$name %||% "Intercompany eliminations",
      res$eliminations$revenue, "ELIMINATION", "REPORTED", "minus"
    )
    running <- running + as.numeric(res$eliminations$revenue)[1]
  }
  if (!is.null(res$unallocated) && .bblab_finite(res$unallocated$revenue)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$unallocated$name %||% "Corporate / Unallocated",
      res$unallocated$revenue, "CORPORATE", "REPORTED", "plus"
    )
    running <- running + as.numeric(res$unallocated$revenue)[1]
  }
  if (!is.null(res$rounding) && .bblab_finite(res$rounding$revenue)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$rounding$name %||% "Rounding", res$rounding$revenue, "RECONCILIATION",
      "DERIVED", "plus"
    )
    running <- running + as.numeric(res$rounding$revenue)[1]
  }
  if (!is.null(res$recon_amount) && .bblab_finite(res$recon_amount$revenue)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$recon_amount$name %||% "Reconciliation",
      res$recon_amount$revenue, "RECONCILIATION", "DERIVED", "plus"
    )
    running <- running + as.numeric(res$recon_amount$revenue)[1]
  }

  leftover <- if (is.finite(cons_rev) && is.finite(running)) cons_rev - running else NA_real_
  pass <- is.finite(leftover) && abs(leftover) <= max(
    1, if (is.finite(cons_rev)) abs(cons_rev) * 0.005 else 1
  )
  if (is.finite(cons_rev)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      "Consolidated Revenue", cons_rev, "CONSOLIDATED", "REPORTED", "equals"
    )
  }
  note <- if (!length(steps)) {
    "No disclosed segment revenues to bridge."
  } else if (isTRUE(pass)) {
    "Revenue bridge reconciles to consolidated Revenue within tolerance."
  } else if (is.finite(leftover)) {
    paste0(
      "Revenue bridge leftover ", format(leftover, scientific = FALSE),
      " remains unexplained; amounts are not spread by revenue share."
    )
  } else {
    "Consolidated Revenue unavailable; bridge incomplete."
  }
  list(
    ok = length(steps) >= 1L,
    steps = steps,
    pass = isTRUE(pass),
    leftover = leftover,
    consolidated = cons_rev,
    note = note
  )
}

#' Operating Profit Bridge: segment OI → other → corporate → elim/recon → consolidated OI.
#' Never fabricates missing Segment Operating Income.
bblab_build_operating_profit_bridge <- function(res) {
  empty <- list(ok = FALSE, steps = list(), pass = FALSE, leftover = NA_real_,
                consolidated = NA_real_,
                note = "Operating Profit bridge unavailable.",
                any_segment_oi = FALSE)
  if (is.null(res) || !is.list(res)) return(empty)
  cons <- res$consolidated %||% list()
  cons_oi <- .bblab_num(cons$operating_income)
  steps <- list()
  running <- 0
  any_oi <- FALSE

  for (comp in res$businesses %||% list()) {
    oi <- .bblab_num(comp$operating_income)
    if (!is.finite(oi)) next
    any_oi <- TRUE
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      comp$name %||% comp$id, oi, "BUSINESS", "REPORTED",
      if (!length(steps)) "start" else "plus"
    )
    running <- running + oi
  }
  if (!is.null(res$other) && .bblab_finite(res$other$operating_income)) {
    any_oi <- TRUE
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$other$name %||% "Other operating", res$other$operating_income,
      "OTHER_OPERATING", "REPORTED", "plus"
    )
    running <- running + as.numeric(res$other$operating_income)[1]
  }
  if (!is.null(res$unallocated) && .bblab_finite(res$unallocated$operating_income)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$unallocated$name %||% "Corporate / Unallocated",
      res$unallocated$operating_income, "CORPORATE", "REPORTED", "minus"
    )
    running <- running + as.numeric(res$unallocated$operating_income)[1]
  }
  if (!is.null(res$eliminations) && .bblab_finite(res$eliminations$operating_income)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$eliminations$name %||% "Intercompany eliminations",
      res$eliminations$operating_income, "ELIMINATION", "REPORTED", "minus"
    )
    running <- running + as.numeric(res$eliminations$operating_income)[1]
  }
  if (!is.null(res$recon_amount) && .bblab_finite(res$recon_amount$operating_income)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      res$recon_amount$name %||% "Reconciliation",
      res$recon_amount$operating_income, "RECONCILIATION", "DERIVED", "plus"
    )
    running <- running + as.numeric(res$recon_amount$operating_income)[1]
  }

  if (!isTRUE(any_oi)) {
    empty$note <- paste(
      "Segment Operating Income not disclosed; Operating Profit bridge is not estimated",
      "and is not labeled as disclosed."
    )
    empty$consolidated <- cons_oi
    return(empty)
  }

  leftover <- if (is.finite(cons_oi) && is.finite(running)) cons_oi - running else NA_real_
  pass <- is.finite(leftover) && abs(leftover) <= max(
    1, if (is.finite(cons_oi)) abs(cons_oi) * 0.005 else 1
  )
  if (is.finite(cons_oi)) {
    steps[[length(steps) + 1L]] <- .bblab_bridge_step(
      "Consolidated Operating Income", cons_oi, "CONSOLIDATED", "REPORTED", "equals"
    )
  }
  note <- if (isTRUE(pass)) {
    "Operating Profit bridge reconciles to consolidated Operating Income within tolerance."
  } else if (is.finite(leftover)) {
    paste0(
      "Operating Profit bridge leftover ", format(leftover, scientific = FALSE),
      "; Corporate / Elimination / Reconciliation explain the gap when disclosed.",
      " No HQ cost was spread by revenue share."
    )
  } else {
    "Consolidated Operating Income unavailable; bridge incomplete."
  }
  list(
    ok = length(steps) >= 1L,
    steps = steps,
    pass = isTRUE(pass),
    leftover = leftover,
    consolidated = cons_oi,
    note = note,
    any_segment_oi = TRUE
  )
}

#' Attach cost-attribution classes, segment profitability, and bridges onto an analyze result.
#' Estimated segment profit is never treated as disclosed for valuation hooks.
bblab_attach_cost_profit_analysis <- function(res, payload = NULL, industry_key = NULL,
                                              industry_text = NULL) {
  if (is.null(res) || !is.list(res)) return(res)
  ik <- industry_key %||% payload$industry_key %||% payload$industry %||% NULL
  it <- industry_text %||% payload$industry_text %||% payload$entity$industry %||% NULL
  if (is.list(ik)) ik <- ik$key %||% ik$id %||% NULL
  prof <- bblab_cost_profit_industry_profile(ik, it)

  # Tag cost class on each business / recon component after assign_cost.
  tag <- function(comp) {
    if (is.null(comp) || !is.list(comp)) return(comp)
    if (!nzchar(.bblab_chr(comp$cost_class, ""))) {
      comp$cost_class <- bblab_classify_cost_attribution(comp)
    }
    if (is.list(comp$members)) comp$members <- lapply(comp$members, tag)
    # Valuation / FCF hook: estimated or allocated segment profit is not disclosed.
    oi_st <- bblab_normalize_profit_status(
      if (.bblab_finite(comp$operating_income)) "REPORTED" else "UNAVAILABLE",
      comp$oi_method
    )
    gp_st <- bblab_normalize_profit_status(
      comp$gp_evidence$status %||% NA_character_, comp$cor_method
    )
    comp$valuation_safe <- list(
      use_segment_gp = identical(gp_st, "REPORTED") || identical(gp_st, "DERIVED"),
      use_segment_oi = identical(oi_st, "REPORTED") || identical(oi_st, "DERIVED"),
      use_segment_fcf = FALSE,
      note = "Estimated / Allocated segment profit and FCF are never treated as disclosed."
    )
    comp
  }
  res$businesses <- lapply(res$businesses %||% list(), tag)
  for (nm in c("other", "unallocated", "eliminations", "rounding", "recon_amount")) {
    if (!is.null(res[[nm]])) res[[nm]] <- tag(res[[nm]])
  }

  res$industry_cost_profile <- prof
  res$segment_profitability <- bblab_build_segment_profitability(res, prof)
  res$revenue_bridge <- bblab_build_revenue_bridge(res)
  res$operating_profit_bridge <- bblab_build_operating_profit_bridge(res)
  res$cost_attribution_summary <- list(
    forbid_bare_revenue_share = TRUE,
    classes_present = unique(vapply(res$businesses %||% list(), function(c) {
      .bblab_chr(c$cost_class, "UNAVAILABLE")
    }, character(1))),
    note = paste(
      "Direct costs use disclosed Segment COGS/GP.",
      "Shared costs require a company-disclosed allocation basis.",
      "Corporate-Unallocated stays separate.",
      "Intercompany eliminates on consolidation.",
      "Bare revenue-share allocation without basis is forbidden."
    )
  )
  res
}
