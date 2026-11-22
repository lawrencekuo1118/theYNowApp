# financial_narrative_schema.R — FNI finding schema, dimensions, evidence rules
# Phase 1: qualitative narrative + optional param *suggestions* only.
# Never invent page numbers; evidence_quote must appear in extracted text.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.YNOW_FNI_PROMPT_VERSION <- "fni-v1"

.YNOW_FNI_DIMENSIONS <- c(
  "growth_drivers",
  "profitability_outlook",
  "capital_allocation",
  "risk_factors",
  "management_guidance",
  "narrative_change"
)

.YNOW_FNI_DIMENSION_LABELS <- list(
  en = list(
    growth_drivers = "Growth Drivers",
    profitability_outlook = "Profitability Outlook",
    capital_allocation = "Capital Allocation",
    risk_factors = "Risk Factors",
    management_guidance = "Management Guidance",
    narrative_change = "Narrative Change"
  ),
  `zh-TW` = list(
    growth_drivers = "成長動能",
    profitability_outlook = "獲利展望",
    capital_allocation = "資本配置",
    risk_factors = "風險因子",
    management_guidance = "管理層指引",
    narrative_change = "敘事變化"
  )
)

ynow_fni_dimension_label <- function(dim, locale = "en") {
  loc <- if (exists("normalize_ui_locale", mode = "function")) {
    normalize_ui_locale(locale)
  } else if (identical(as.character(locale)[1], "zh-TW")) {
    "zh-TW"
  } else {
    "en"
  }
  labs <- .YNOW_FNI_DIMENSION_LABELS[[loc]] %||% .YNOW_FNI_DIMENSION_LABELS$en
  as.character(labs[[as.character(dim)[1]]] %||% dim)[1]
}

#' Empty finding shell (JSON-Schema-aligned list).
ynow_fni_empty_finding <- function(dimension = "growth_drivers") {
  list(
    dimension = as.character(dimension)[1],
    claim = "",
    polarity = "neutral", # positive | negative | neutral | mixed
    confidence = 0,
    evidence_quote = "",
    evidence_source = "",
    evidence_locator = "", # page/section/speaker when known; never invent
    period = "",
    notes = "",
    param_suggestion = NULL # optional; never auto-applied to WACC/Ke/g
  )
}

#' Validate one finding against extracted text (evidence must exist verbatim).
#' Returns list(ok=, finding=, reason=).
ynow_fni_validate_finding <- function(finding, extracted_text = "") {
  f <- finding
  if (!is.list(f)) {
    return(list(ok = FALSE, finding = NULL, reason = "not_a_list"))
  }
  dim <- as.character(f$dimension %||% "")[1]
  if (!nzchar(dim) || !(dim %in% .YNOW_FNI_DIMENSIONS)) {
    return(list(ok = FALSE, finding = NULL, reason = "bad_dimension"))
  }
  quote <- as.character(f$evidence_quote %||% "")[1]
  text <- as.character(extracted_text %||% "")[1]
  if (!nzchar(quote)) {
    return(list(ok = FALSE, finding = f, reason = "missing_evidence_quote"))
  }
  # Require a non-trivial substring match (normalize whitespace lightly).
  q_norm <- gsub("\\s+", " ", quote, perl = TRUE)
  t_norm <- gsub("\\s+", " ", text, perl = TRUE)
  if (!nzchar(text) || !grepl(q_norm, t_norm, fixed = TRUE)) {
    return(list(ok = FALSE, finding = f, reason = "evidence_not_in_text"))
  }
  # Drop invented locator heuristics: blank is honest; numeric-only "page" without source is cleared.
  loc <- as.character(f$evidence_locator %||% "")[1]
  if (nzchar(loc) && grepl("(?i)^page\\s*\\d+$", loc, perl = TRUE) &&
      !nzchar(as.character(f$evidence_source %||% "")[1])) {
    f$evidence_locator <- ""
  }
  conf <- suppressWarnings(as.numeric(f$confidence)[1])
  if (!is.finite(conf)) conf <- 0
  f$confidence <- max(0, min(1, conf))
  pol <- tolower(as.character(f$polarity %||% "neutral")[1])
  if (!(pol %in% c("positive", "negative", "neutral", "mixed"))) pol <- "neutral"
  f$polarity <- pol
  f$claim <- as.character(f$claim %||% "")[1]
  f$evidence_quote <- quote
  list(ok = TRUE, finding = f, reason = "")
}

#' Filter a list of findings; drop those without grounded evidence.
ynow_fni_filter_findings <- function(findings, extracted_text = "") {
  if (!length(findings)) return(list())
  out <- list()
  for (f in findings) {
    v <- ynow_fni_validate_finding(f, extracted_text)
    if (isTRUE(v$ok)) out[[length(out) + 1L]] <- v$finding
  }
  out
}

#' Cross-quarter compare: never treat "not mentioned" as "resolved".
#' periods: named list of period_id -> list(findings=..., text=...)
ynow_fni_cross_quarter_compare <- function(period_packs) {
  if (!is.list(period_packs) || length(period_packs) < 2L) {
    return(list(
      ok = FALSE,
      reason = "need_at_least_two_periods",
      changes = list()
    ))
  }
  ids <- names(period_packs)
  if (is.null(ids) || !all(nzchar(ids))) {
    ids <- paste0("P", seq_along(period_packs))
    names(period_packs) <- ids
  }
  changes <- list()
  for (dim in .YNOW_FNI_DIMENSIONS) {
    by_period <- lapply(period_packs, function(pk) {
      finds <- pk$findings %||% list()
      keep <- Filter(function(f) identical(as.character(f$dimension %||% "")[1], dim), finds)
      claims <- vapply(keep, function(f) as.character(f$claim %||% "")[1], character(1))
      quotes <- vapply(keep, function(f) as.character(f$evidence_quote %||% "")[1], character(1))
      list(claims = claims, quotes = quotes, n = length(keep))
    })
    present <- vapply(by_period, function(x) isTRUE(x$n > 0L), logical(1))
    # Status vocabulary: introduced | continued | changed | not_mentioned (never "resolved")
    status <- if (all(present)) {
      # Heuristic change detection on first claim text
      firsts <- vapply(by_period, function(x) {
        if (!length(x$claims)) return("")
        x$claims[[1]]
      }, character(1))
      if (length(unique(firsts)) == 1L) "continued" else "changed"
    } else if (any(present) && !all(present)) {
      "not_mentioned_in_some_periods"
    } else {
      "not_mentioned"
    }
    changes[[length(changes) + 1L]] <- list(
      dimension = dim,
      status = status,
      by_period = by_period,
      note = if (identical(status, "not_mentioned_in_some_periods")) {
        "Absence in a later period is not treated as resolved."
      } else {
        ""
      }
    )
  }
  list(ok = TRUE, reason = "", period_ids = ids, changes = changes)
}

#' JSON Schema document describing the LLM response shape (for Ollama format).
ynow_fni_json_schema <- function() {
  list(
    type = "object",
    properties = list(
      summary = list(type = "string"),
      findings = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            dimension = list(
              type = "string",
              enum = as.list(.YNOW_FNI_DIMENSIONS)
            ),
            claim = list(type = "string"),
            polarity = list(
              type = "string",
              enum = list("positive", "negative", "neutral", "mixed")
            ),
            confidence = list(type = "number"),
            evidence_quote = list(type = "string"),
            evidence_source = list(type = "string"),
            evidence_locator = list(type = "string"),
            period = list(type = "string"),
            notes = list(type = "string"),
            param_suggestion = list(
              type = c("object", "null"),
              properties = list(
                param = list(type = "string"),
                direction = list(type = "string"),
                rationale = list(type = "string")
              )
            )
          ),
          required = list(
            "dimension", "claim", "polarity", "confidence", "evidence_quote"
          )
        )
      )
    ),
    required = list("summary", "findings")
  )
}

#' Normalize a parsed LLM / mock payload into a standard result list.
ynow_fni_normalize_result <- function(payload, extracted_text = "", meta = list()) {
  if (!is.list(payload)) payload <- list()
  findings <- payload$findings %||% list()
  if (is.data.frame(findings)) {
    findings <- lapply(seq_len(nrow(findings)), function(i) as.list(findings[i, , drop = FALSE]))
  }
  findings <- ynow_fni_filter_findings(findings, extracted_text)
  list(
    ok = TRUE,
    summary = as.character(payload$summary %||% "")[1],
    findings = findings,
    extracted_text = as.character(extracted_text %||% "")[1],
    meta = meta,
    prompt_version = .YNOW_FNI_PROMPT_VERSION
  )
}
