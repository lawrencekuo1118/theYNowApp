# Business Breakdown Lab — generic component + evidence schemas.
# Company-agnostic: no ticker, issuer, platform, node, or layout branches.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

BBLAB_VALUE_STATUS <- c(
  "REPORTED", "DERIVED", "ALLOCATED", "ESTIMATED",
  "UNALLOCATED", "UNAVAILABLE", "CONSOLIDATED_PROXY"
)
BBLAB_CONFIDENCE <- c("HIGH", "MEDIUM", "LOW", "UNAVAILABLE")
BBLAB_LEVELS <- c("A", "B", "C", "D")
BBLAB_COMPONENT_CLASS <- c(
  "MAJOR", "OTHER", "UNALLOCATED", "ELIMINATION", "ROUNDING",
  "SHARED_CORPORATE", "RECONCILIATION"
)
BBLAB_DIMENSION_KINDS <- c(
  "operating_segment", "segment_note", "product_service",
  "revenue_disaggregation", "mda", "earnings", "ir_deck",
  "official_description", "geography", "technology", "platform"
)
BBLAB_PROGRESS_STAGES <- c(
  "resolve_issuer",
  "retrieve_statements",
  "parse_disclosures",
  "detect_dimension",
  "identify_businesses",
  "assign_revenue",
  "assign_cost_gp",
  "reconcile_revalue",
  "chart_cards"
)
BBLAB_ERROR_CODES <- c(
  "REQUIRED_FX_RATE_MISSING",
  "REQUIRED_FX_RATE_INVALID",
  "STATEMENT_CURRENCY_UNAVAILABLE",
  "APPLICABLE_ADR_RATIO_MISSING",
  "APPLICABLE_ADR_RATIO_INVALID",
  "FX_DATE_MISMATCH",
  "BUSINESS_ISSUER_UNRESOLVED",
  "BUSINESS_STATEMENTS_UNAVAILABLE",
  "BUSINESS_DISCLOSURE_INSUFFICIENT",
  "BUSINESS_OVERLAPPING_DIMENSIONS_BLOCKED",
  "BUSINESS_GEOGRAPHY_CUSTOMER_LOCATION_ONLY",
  "BUSINESS_COST_NOT_RELIABLY_ESTIMABLE",
  "BUSINESS_COST_ALLOCATED_LOW_CONFIDENCE",
  "BUSINESS_REVALUATION_UNAVAILABLE",
  "BUSINESS_REVALUATION_CONSOLIDATED_PROXY",
  "BUSINESS_RECONCILIATION_FAIL",
  "BUSINESS_CHART_INSUFFICIENT_COMPONENTS",
  "BUSINESS_CHART_CONSOLIDATED_REVENUE_MISSING",
  "BUSINESS_CHART_PERIOD_MISMATCH",
  "BUSINESS_CHART_CURRENCY_MISMATCH",
  "BUSINESS_CHART_DIMENSION_MIXED",
  "BUSINESS_CHART_RECONCILIATION_FAIL",
  "BUSINESS_CHART_SINGLE_COMPONENT",
  "BUSINESS_CHART_LEVEL_D",
  "BUSINESS_ADR_MISSING_NONBLOCKING"
)

BBLAB_DEFAULT_CONFIG <- list(
  meta = list(
    id = "BUSINESS_BREAKDOWN_LAB",
    role = "experimental_business_decomposition"
  ),
  thresholds = list(
    major_share = 0.10,
    min_display_percentage = 0.03,
    reconciliation_rel_tol = 0.005,
    reconciliation_abs_tol = 1.0,
    max_major_businesses = 6L,
    min_major_businesses = 1L
  ),
  disclosure_priority = c(
    "operating_segment", "segment_note", "product_service",
    "revenue_disaggregation", "mda", "earnings", "ir_deck",
    "official_description"
  ),
  dimension_score_weights = c(
    distinct_economics = 1.0,
    separate_revenue = 1.0,
    attributable_cor_gp = 1.2,
    management_major = 0.8,
    mutually_exclusive = 1.0,
    reconciles = 1.5,
    relevant = 0.6,
    filed_audited = 0.8
  ),
  flags = list(
    geography_customer_location_only_veto = TRUE,
    never_combine_overlapping_dimensions = TRUE,
    revenue_share_cost_is_final_fallback = TRUE,
    consolidated_gm_fallback_default = FALSE,
    default_view = "reported"
  )
)

bblab_config_yaml_path <- function() {
  candidates <- c(
    "business_breakdown_config.yaml",
    file.path("app_19.0", "business_breakdown_config.yaml")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) normalizePath(hit[[1]], mustWork = TRUE) else NA_character_
}

bblab_load_config <- function(path = NULL) {
  cfg <- BBLAB_DEFAULT_CONFIG
  p <- path %||% bblab_config_yaml_path()
  if (is.character(p) && length(p) == 1L && !is.na(p) && nzchar(p) && file.exists(p) &&
      requireNamespace("yaml", quietly = TRUE)) {
    raw <- tryCatch(yaml::read_yaml(p), error = function(e) NULL)
    if (is.list(raw) && is.list(raw$thresholds)) {
      for (nm in names(raw$thresholds)) {
        cfg$thresholds[[nm]] <- raw$thresholds[[nm]]
      }
    }
    if (is.list(raw) && length(raw$disclosure_priority)) {
      cfg$disclosure_priority <- unlist(raw$disclosure_priority, use.names = FALSE)
    }
    if (is.list(raw) && length(raw$dimension_score_weights)) {
      w <- unlist(raw$dimension_score_weights)
      cfg$dimension_score_weights[names(w)] <- as.numeric(w)
    }
    if (is.list(raw) && is.list(raw$flags)) {
      for (nm in names(raw$flags)) cfg$flags[[nm]] <- raw$flags[[nm]]
    }
  }
  cfg
}

.bblab_finite <- function(x) {
  num <- suppressWarnings(as.numeric(x)[1])
  length(num) == 1L && is.finite(num)
}

.bblab_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) < 1L) return(default)
  num <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(num)) default else num
}

.bblab_chr <- function(x, default = "") {
  if (is.null(x) || length(x) < 1L || is.na(x[1])) return(default)
  as.character(x[1])
}

#' Evidence / provenance for one metric on one component.
bblab_evidence <- function(component_id, metric, amount = NA_real_,
                           currency = NA_character_, period = NA_character_,
                           status = "UNAVAILABLE", confidence = "UNAVAILABLE",
                           source_type = NA_character_, source_document = NA_character_,
                           source_page = NA_character_, source_date = NA_character_,
                           allocation_method = NA_character_, allocation_basis = NA_character_,
                           recon_role = NA_character_, user_override = FALSE,
                           notes = NA_character_) {
  st <- toupper(.bblab_chr(status, "UNAVAILABLE"))
  if (!st %in% BBLAB_VALUE_STATUS) st <- "UNAVAILABLE"
  cf <- toupper(.bblab_chr(confidence, "UNAVAILABLE"))
  if (!cf %in% BBLAB_CONFIDENCE) cf <- "UNAVAILABLE"
  list(
    componentId = .bblab_chr(component_id),
    metric = .bblab_chr(metric),
    amount = .bblab_num(amount),
    currency = .bblab_chr(currency, NA_character_),
    period = .bblab_chr(period, NA_character_),
    status = st,
    confidence = cf,
    sourceType = .bblab_chr(source_type, NA_character_),
    sourceDocument = .bblab_chr(source_document, NA_character_),
    sourcePage = .bblab_chr(source_page, NA_character_),
    sourceDate = .bblab_chr(source_date, NA_character_),
    allocationMethod = .bblab_chr(allocation_method, NA_character_),
    allocationBasis = .bblab_chr(allocation_basis, NA_character_),
    reconRole = .bblab_chr(recon_role, NA_character_),
    userOverride = isTRUE(user_override),
    notes = .bblab_chr(notes, NA_character_)
  )
}

#' One disclosed or derived business / recon line.
bblab_component <- function(id, name, classification = "MAJOR",
                            revenue = NA_real_, cor = NA_real_, gp = NA_real_,
                            revenue_pct = NA_real_,
                            is_reported_segment = FALSE,
                            separately_disclosed_revenue = FALSE,
                            is_principal_activity = FALSE,
                            distinct_economics = FALSE,
                            distinct_customers = FALSE,
                            distinct_production = FALSE,
                            needed_to_explain_material_share = FALSE,
                            period = NA_character_, currency = NA_character_,
                            source_type = NA_character_,
                            qualitative_only = FALSE,
                            description = NA_character_,
                            extra = NULL, ...) {
  cls <- toupper(.bblab_chr(classification, "MAJOR"))
  if (!cls %in% BBLAB_COMPONENT_CLASS) cls <- "MAJOR"
  out <- list(
    id = .bblab_chr(id),
    name = .bblab_chr(name, id),
    classification = cls,
    revenue = .bblab_num(revenue),
    cor = .bblab_num(cor),
    gp = .bblab_num(gp),
    revenue_pct = .bblab_num(revenue_pct),
    is_reported_segment = isTRUE(is_reported_segment),
    separately_disclosed_revenue = isTRUE(separately_disclosed_revenue) || .bblab_finite(revenue),
    is_principal_activity = isTRUE(is_principal_activity),
    distinct_economics = isTRUE(distinct_economics),
    distinct_customers = isTRUE(distinct_customers),
    distinct_production = isTRUE(distinct_production),
    needed_to_explain_material_share = isTRUE(needed_to_explain_material_share),
    period = .bblab_chr(period, NA_character_),
    currency = .bblab_chr(currency, NA_character_),
    source_type = .bblab_chr(source_type, NA_character_),
    qualitative_only = isTRUE(qualitative_only),
    description = .bblab_chr(description, NA_character_)
  )
  if (is.list(extra) && length(extra)) out <- c(out, extra)
  dots <- list(...)
  if (length(dots)) out <- c(out, dots)
  out
}

#' One candidate reporting dimension (never mixed with an overlapping one).
bblab_dimension <- function(id, kind, components = list(),
                            mutually_exclusive = TRUE,
                            is_customer_location_only = FALSE,
                            overlaps_with = character(0),
                            flags = character(0),
                            source_priority = NA_integer_,
                            filed_audited = FALSE,
                            period = NA_character_,
                            currency = NA_character_,
                            label = NULL) {
  kd <- .bblab_chr(kind, "official_description")
  if (!kd %in% BBLAB_DIMENSION_KINDS) kd <- "official_description"
  comps <- if (is.null(components)) list() else components
  if (!is.null(names(comps)) && !is.list(comps[[1]])) comps <- list(comps)
  list(
    id = .bblab_chr(id, kd),
    kind = kd,
    label = .bblab_chr(label, kd),
    components = comps,
    mutually_exclusive = isTRUE(mutually_exclusive),
    is_customer_location_only = isTRUE(is_customer_location_only),
    overlaps_with = unique(as.character(overlaps_with %||% character(0))),
    flags = unique(as.character(flags %||% character(0))),
    source_priority = suppressWarnings(as.integer(source_priority)[1]),
    filed_audited = isTRUE(filed_audited),
    period = .bblab_chr(period, NA_character_),
    currency = .bblab_chr(currency, NA_character_)
  )
}

bblab_consolidated <- function(revenue = NA_real_, cor = NA_real_, gp = NA_real_,
                               currency = NA_character_, period = NA_character_,
                               ni = NA_real_) {
  list(
    revenue = .bblab_num(revenue),
    cor = .bblab_num(cor),
    gp = .bblab_num(gp),
    ni = .bblab_num(ni),
    currency = .bblab_chr(currency, NA_character_),
    period = .bblab_chr(period, NA_character_)
  )
}

#' Empty analysis skeleton used by the engine and UI.
bblab_empty_result <- function(codes = character(0), limitations = character(0)) {
  list(
    ok = FALSE,
    level = NA_character_,
    primary_dimension = NULL,
    businesses = list(),
    other = NULL,
    unallocated = NULL,
    eliminations = NULL,
    rounding = NULL,
    recon_amount = NULL,
    shared_corporate = list(),
    reconciliation = list(pass = FALSE, revenue = NULL, cor = NULL, gp = NULL),
    revaluation = list(),
    chart = list(eligible = FALSE, codes = character(0), slices = list()),
    evidence = list(),
    confidence = list(overall = "UNAVAILABLE"),
    progress = list(stage = NA_character_, completed = character(0)),
    codes = unique(as.character(codes)),
    toasts = list(),
    limitations = unique(as.character(limitations)),
    view = "reported"
  )
}
