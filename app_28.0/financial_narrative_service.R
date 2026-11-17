# financial_narrative_service.R — FNI pipeline orchestration
# Import → Docling/plain → chunk → Ollama → evidence recheck → cache → compare.
# User must trigger analysis; no background LLM/Docling on page load.

ynow_fni_max_concurrent <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("YNOW_AI_MAX_CONCURRENT", unset = "1"))[1])
  if (!is.finite(v) || v < 1L) v <- 1L
  max(1L, min(4L, v))
}

#' Combined service readiness (Docling + Ollama) for UI banner.
ynow_fni_service_status <- function() {
  mock_on <- .ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")
  enabled <- isTRUE(ynow_fni_ai_enabled()) || isTRUE(mock_on)
  doc <- ynow_fni_docling_status()
  llm <- ynow_fni_ollama_status()
  model_missing <- identical(llm$status, "reachable_model_missing")
  # ready requires a present model (unless mock). Do not green-banner on model-missing.
  ready <- isTRUE(enabled) &&
    isTRUE(doc$ok) &&
    isTRUE(llm$ok) &&
    !isTRUE(model_missing)
  msg <- if (!isTRUE(enabled) && !isTRUE(mock_on)) {
    "AI service not configured. Set YNOW_AI_QUAL_ENABLED=1 and run Docling Serve + Ollama locally."
  } else if (!isTRUE(doc$ok)) {
    doc$message %||% "AI service not configured (Docling)."
  } else if (isTRUE(model_missing) || !isTRUE(llm$ok)) {
    llm$message %||% "AI service not configured (Ollama)."
  } else {
    "Ready"
  }
  list(
    enabled = enabled,
    ready = ready,
    message = msg,
    docling = doc,
    ollama = llm,
    model = ynow_fni_ollama_model(),
    prompt_version = .YNOW_FNI_PROMPT_VERSION
  )
}

#' Build a provenance record for an imported source.
ynow_fni_provenance <- function(kind,
                                ticker = "",
                                period = "",
                                language = "en",
                                filename = "",
                                catalog_id = "",
                                content_hash = "",
                                market = "") {
  list(
    kind = as.character(kind %||% "upload")[1], # upload | paste | catalog | search
    ticker = toupper(trimws(as.character(ticker %||% "")[1])),
    period = as.character(period %||% "")[1],
    language = as.character(language %||% "en")[1],
    filename = as.character(filename %||% "")[1],
    catalog_id = as.character(catalog_id %||% "")[1],
    content_hash = as.character(content_hash %||% "")[1],
    market = as.character(market %||% "")[1],
    imported_at = as.character(Sys.time())
  )
}

#' Ingest from upload path, paste text, or catalog-fetched text.
ynow_fni_ingest <- function(source = list()) {
  kind <- as.character(source$kind %||% "paste")[1]
  if (identical(kind, "upload") || identical(kind, "file")) {
    path <- as.character(source$path %||% "")[1]
    fname <- as.character(source$filename %||% basename(path))[1]
    # Text-like uploads skip Docling when already plain.
    ext <- tolower(tools::file_ext(fname))
    if (ext %in% c("txt", "md", "csv", "json")) {
      raw <- tryCatch(
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
        error = function(e) ""
      )
      doc <- ynow_fni_ingest_plain_text(raw, source_label = fname)
    } else {
      doc <- ynow_fni_docling_convert_file(path, filename = fname)
    }
    h <- ynow_fni_content_hash(path = path, text = doc$text)
  } else if (identical(kind, "catalog")) {
    text <- as.character(source$text %||% "")[1]
    doc <- ynow_fni_ingest_plain_text(text, source_label = source$filename %||% "catalog")
    h <- ynow_fni_content_hash(text = text)
  } else {
    text <- as.character(source$text %||% "")[1]
    doc <- ynow_fni_ingest_plain_text(text, source_label = "paste")
    h <- ynow_fni_content_hash(text = text)
  }
  prov <- ynow_fni_provenance(
    kind = kind,
    ticker = source$ticker %||% "",
    period = source$period %||% "",
    language = source$language %||% "en",
    filename = source$filename %||% doc$meta$filename %||% "",
    catalog_id = source$catalog_id %||% "",
    content_hash = h,
    market = source$market %||% ""
  )
  list(doc = doc, provenance = prov, content_hash = h)
}

#' Run analysis on one ingested document (chunk → analyze → merge → cache).
#' force=TRUE skips cache read.
ynow_fni_analyze_document <- function(source = list(),
                                      locale = "en",
                                      force = FALSE,
                                      max_chunks = 8L) {
  locale <- if (identical(as.character(locale)[1], "zh-TW")) "zh-TW" else "en"
  ingested <- ynow_fni_ingest(source)
  doc <- ingested$doc
  if (!isTRUE(doc$ok)) {
    return(list(
      ok = FALSE,
      error = doc$error %||% "ingest_failed",
      message = doc$message %||% "Could not extract text from the document.",
      provenance = ingested$provenance,
      result = NULL,
      cache_hit = FALSE
    ))
  }
  model <- ynow_fni_ollama_model()
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) model <- "mock"
  key <- ynow_fni_cache_key(
    content_hash = ingested$content_hash,
    model = model,
    prompt_version = .YNOW_FNI_PROMPT_VERSION,
    locale = locale,
    ticker = source$ticker %||% "",
    period = source$period %||% ""
  )
  if (!isTRUE(force)) {
    cached <- ynow_fni_cache_get(key)
    if (is.list(cached) && isTRUE(cached$ok)) {
      cached$cache_hit <- TRUE
      cached$cache_key <- key
      return(cached)
    }
  }
  # Service gate before LLM work
  st <- ynow_fni_service_status()
  if (!isTRUE(st$ready) && !.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(list(
      ok = FALSE,
      error = "ai_service_not_configured",
      message = st$message,
      provenance = ingested$provenance,
      result = NULL,
      cache_hit = FALSE,
      status = st
    ))
  }
  chunks <- ynow_fni_chunk_text(doc$text)
  if (!length(chunks)) {
    return(list(
      ok = FALSE,
      error = "empty_chunks",
      message = "No text chunks to analyze.",
      provenance = ingested$provenance,
      result = NULL,
      cache_hit = FALSE
    ))
  }
  max_chunks <- as.integer(max_chunks)[1]
  if (!is.finite(max_chunks) || max_chunks < 1L) max_chunks <- 8L
  chunks <- head(chunks, max_chunks)
  meta <- list(
    ticker = source$ticker %||% "",
    period = source$period %||% "",
    source_label = ingested$provenance$filename,
    filename = ingested$provenance$filename
  )
  payloads <- list()
  errors <- character()
  for (ch in chunks) {
    resp <- ynow_fni_ollama_chat_json(ch$text, meta = meta, locale = locale)
    if (!isTRUE(resp$ok)) {
      errors <- c(errors, resp$message %||% resp$error %||% "chunk_failed")
      next
    }
    payloads[[length(payloads) + 1L]] <- resp$payload
  }
  if (!length(payloads)) {
    return(list(
      ok = FALSE,
      error = "llm_failed",
      message = paste(unique(errors), collapse = "; "),
      provenance = ingested$provenance,
      result = NULL,
      cache_hit = FALSE
    ))
  }
  merged <- ynow_fni_merge_chunk_payloads(payloads, full_text = doc$text)
  result <- ynow_fni_normalize_result(
    merged,
    extracted_text = doc$text,
    meta = list(
      provenance = ingested$provenance,
      model = model,
      chunk_count = length(chunks),
      used_chunks = length(payloads)
    )
  )
  out <- list(
    ok = TRUE,
    error = "",
    message = "",
    provenance = ingested$provenance,
    result = result,
    cache_hit = FALSE,
    cache_key = key,
    extracted_text = doc$text
  )
  ynow_fni_cache_put(key, out)
  out
}

#' Merge two per-document analysis outputs that share a period (findings + text).
ynow_fni_merge_period_results <- function(existing, incoming) {
  if (!is.list(existing) || !isTRUE(existing$ok)) return(incoming)
  if (!is.list(incoming) || !isTRUE(incoming$ok)) return(existing)
  text_a <- as.character(existing$extracted_text %||% existing$result$extracted_text %||% "")[1]
  text_b <- as.character(incoming$extracted_text %||% incoming$result$extracted_text %||% "")[1]
  full_text <- paste(c(text_a, text_b)[nzchar(c(text_a, text_b))], collapse = "\n\n")
  payloads <- list(
    list(
      summary = existing$result$summary %||% "",
      findings = existing$result$findings %||% list()
    ),
    list(
      summary = incoming$result$summary %||% "",
      findings = incoming$result$findings %||% list()
    )
  )
  merged_payload <- ynow_fni_merge_chunk_payloads(payloads, full_text = full_text)
  meta_a <- existing$result$meta %||% list()
  meta_b <- incoming$result$meta %||% list()
  provs <- list(existing$provenance, incoming$provenance)
  provs <- Filter(function(p) is.list(p), provs)
  merged_result <- ynow_fni_normalize_result(
    merged_payload,
    extracted_text = full_text,
    meta = list(
      provenance = provs,
      model = meta_a$model %||% meta_b$model %||% "",
      chunk_count = sum(
        as.integer(meta_a$chunk_count %||% 0L)[1],
        as.integer(meta_b$chunk_count %||% 0L)[1],
        na.rm = TRUE
      ),
      used_chunks = sum(
        as.integer(meta_a$used_chunks %||% 0L)[1],
        as.integer(meta_b$used_chunks %||% 0L)[1],
        na.rm = TRUE
      ),
      merged_sources = length(provs)
    )
  )
  list(
    ok = TRUE,
    error = "",
    message = "",
    provenance = provs,
    result = merged_result,
    cache_hit = isTRUE(existing$cache_hit) && isTRUE(incoming$cache_hit),
    cache_key = paste(
      existing$cache_key %||% "",
      incoming$cache_key %||% "",
      sep = "+"
    ),
    extracted_text = full_text
  )
}

#' Analyze multiple period packs and return cross-quarter compare.
#' sources: list of source lists (each with period).
#' Same-period sources merge findings instead of silently overwriting.
ynow_fni_analyze_and_compare <- function(sources,
                                         locale = "en",
                                         force = FALSE) {
  if (!is.list(sources) || !length(sources)) {
    return(list(ok = FALSE, error = "no_sources", message = "No sources provided.",
                results = list(), compare = NULL))
  }
  results <- list()
  packs <- list()
  for (i in seq_along(sources)) {
    src <- sources[[i]]
    if (!is.list(src)) next
    period <- as.character(src$period %||% paste0("P", i))[1]
    if (!nzchar(period)) period <- paste0("P", i)
    one <- ynow_fni_analyze_document(src, locale = locale, force = force)
    if (!is.null(results[[period]])) {
      one <- ynow_fni_merge_period_results(results[[period]], one)
    }
    results[[period]] <- one
    if (isTRUE(one$ok) && is.list(one$result)) {
      packs[[period]] <- list(
        findings = one$result$findings %||% list(),
        text = one$extracted_text %||% one$result$extracted_text %||% ""
      )
    }
  }
  compare <- NULL
  if (length(packs) >= 2L) {
    compare <- ynow_fni_cross_quarter_compare(packs)
  } else {
    compare <- list(
      ok = FALSE,
      reason = "need_at_least_two_periods",
      changes = list()
    )
  }
  list(
    ok = any(vapply(results, function(r) isTRUE(r$ok), logical(1))),
    error = "",
    message = "",
    results = results,
    compare = compare
  )
}

#' Pull catalog text for a ticker/selection when earnings-call helpers exist.
ynow_fni_fetch_catalog_source <- function(ticker,
                                          selected_ids = NULL,
                                          period = "",
                                          language = "en") {
  ticker <- as.character(ticker %||% "")[1]
  if (!nzchar(ticker)) {
    return(list(ok = FALSE, error = "empty_ticker", message = "Ticker required.", sources = list()))
  }
  if (!exists("ynow_earnings_call_catalog", mode = "function") ||
      !exists("ynow_ec_fetch_selected", mode = "function")) {
    return(list(
      ok = FALSE,
      error = "catalog_unavailable",
      message = "Earnings-call catalog helpers are not loaded.",
      sources = list()
    ))
  }
  cat <- tryCatch(ynow_earnings_call_catalog(ticker, max_rows = 12L), error = function(e) NULL)
  if (!is.list(cat) || !length(cat$rows)) {
    return(list(
      ok = FALSE,
      error = "catalog_empty",
      message = "No earnings-call catalog rows for this ticker.",
      catalog = cat,
      sources = list()
    ))
  }
  fetched <- tryCatch(
    ynow_ec_fetch_selected(ticker, cat, selected_ids = selected_ids),
    error = function(e) NULL
  )
  # Build a readable text blob from known disclosure fields.
  text_parts <- character()
  if (is.list(fetched)) {
    for (nm in c("excerpt", "outlook_notes", "order_notes", "supply_chain_notes",
                 "themes", "raw_text", "text", "transcript")) {
      v <- fetched[[nm]]
      if (is.character(v) && any(nzchar(v))) {
        text_parts <- c(text_parts, paste(v[nzchar(v)], collapse = "\n"))
      } else if (is.list(v) && length(v)) {
        text_parts <- c(text_parts, paste(unlist(v), collapse = "\n"))
      }
    }
    if (is.list(fetched$primary) && length(fetched$primary)) {
      for (nm in c("excerpt", "text", "raw_text", "outlook_notes")) {
        v <- fetched$primary[[nm]]
        if (is.character(v) && any(nzchar(v))) {
          text_parts <- c(text_parts, paste(v[nzchar(v)], collapse = "\n"))
        }
      }
    }
  }
  text <- paste(unique(text_parts[nzchar(text_parts)]), collapse = "\n\n")
  if (!nzchar(trimws(text))) {
    return(list(
      ok = FALSE,
      error = "catalog_no_text",
      message = "Catalog row(s) returned no usable text body.",
      catalog = cat,
      fetched = fetched,
      sources = list()
    ))
  }
  market <- if (grepl("\\.TW$|\\.TWO$|^[0-9]{4}$", ticker)) "TW" else "US"
  src <- list(
    kind = "catalog",
    text = text,
    ticker = ticker,
    period = period,
    language = language,
    filename = paste0("catalog:", ticker),
    catalog_id = paste(selected_ids %||% cat$default_id %||% "", collapse = ","),
    market = market
  )
  list(ok = TRUE, error = "", message = "", catalog = cat, fetched = fetched, sources = list(src))
}

#' Job entry for ExtendedTask (plain args only; named for worker lookup).
ynow_fni_run_analysis_job <- function(sources_json = "[]",
                                      locale = "en",
                                      force = FALSE,
                                      key = "") {
  sources <- tryCatch(
    jsonlite::fromJSON(as.character(sources_json)[1], simplifyVector = FALSE),
    error = function(e) list()
  )
  if (!is.list(sources)) sources <- list()
  # Single-object JSON may parse as a named list rather than list-of-lists.
  if (length(sources) && !is.null(sources$kind)) sources <- list(sources)
  out <- ynow_fni_analyze_and_compare(
    sources,
    locale = locale,
    force = isTRUE(force) || identical(as.character(force)[1], "TRUE")
  )
  out$job_key <- as.character(key %||% "")[1]
  out
}
attr(ynow_fni_run_analysis_job, "ynow_job_name") <- "ynow_fni_run_analysis_job"

#' Flatten findings to a data.frame for CSV download.
ynow_fni_findings_to_df <- function(findings) {
  if (!length(findings)) {
    return(data.frame(
      dimension = character(),
      claim = character(),
      polarity = character(),
      confidence = numeric(),
      evidence_quote = character(),
      evidence_source = character(),
      evidence_locator = character(),
      period = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    dimension = vapply(findings, function(f) as.character(f$dimension %||% "")[1], character(1)),
    claim = vapply(findings, function(f) as.character(f$claim %||% "")[1], character(1)),
    polarity = vapply(findings, function(f) as.character(f$polarity %||% "")[1], character(1)),
    confidence = vapply(findings, function(f) {
      v <- suppressWarnings(as.numeric(f$confidence)[1])
      if (!is.finite(v)) NA_real_ else v
    }, numeric(1)),
    evidence_quote = vapply(findings, function(f) as.character(f$evidence_quote %||% "")[1], character(1)),
    evidence_source = vapply(findings, function(f) as.character(f$evidence_source %||% "")[1], character(1)),
    evidence_locator = vapply(findings, function(f) as.character(f$evidence_locator %||% "")[1], character(1)),
    period = vapply(findings, function(f) as.character(f$period %||% "")[1], character(1)),
    notes = vapply(findings, function(f) as.character(f$notes %||% "")[1], character(1)),
    stringsAsFactors = FALSE
  )
}
