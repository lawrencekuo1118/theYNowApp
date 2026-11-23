# financial_narrative_service.R — FNI pipeline orchestration
# Import → local extract / Cloudflare Markdown → chunk → Workers AI → evidence recheck → cache → compare.
# User must trigger analysis; no background LLM or conversion on page load.
# Cloud Run Docling is not on this path (see ynow_fni_docling_cloudrun_convert).

ynow_fni_max_concurrent <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("YNOW_AI_MAX_CONCURRENT", unset = "1"))[1])
  if (!is.finite(v) || v < 1L) v <- 1L
  max(1L, min(4L, v))
}

ynow_fni_max_runs_session <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("YNOW_AI_MAX_RUNS", unset = "8"))[1])
  if (!is.finite(v) || v < 1L) v <- 8L
  max(1L, min(200L, v))
}

ynow_fni_max_runs_host <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("YNOW_AI_MAX_RUNS_HOST", unset = "32"))[1])
  if (!is.finite(v) || v < 1L) v <- 32L
  max(1L, min(5000L, v))
}

#' Seconds a session must wait between live Cloudflare runs. 0 disables.
ynow_fni_min_interval_sec <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_AI_MIN_INTERVAL_SEC", unset = "15"))[1])
  if (!is.finite(v) || v < 0) v <- 15
  min(3600, v)
}

# In-process nest count so chunk calls under one analysis do not each take a run.
.ynow_fni_budget_rt <- new.env(parent = emptyenv())
.ynow_fni_budget_rt$depth <- 0L

.ynow_fni_budget_session_id <- function(session_key) {
  raw <- trimws(as.character(session_key %||% "")[1])
  if (!nzchar(raw)) return("anonymous")
  if (requireNamespace("digest", quietly = TRUE)) {
    return(substr(digest::digest(raw, algo = "sha256", serialize = FALSE), 1, 16))
  }
  gsub("[^A-Za-z0-9]", "", substr(raw, 1, 24))
}

.ynow_fni_budget_message <- function(reason, locale = "en") {
  loc <- if (identical(as.character(locale)[1], "zh-TW")) "zh-TW" else "en"
  key <- switch(
    reason,
    host = "fni_budget_host",
    concurrent = "fni_budget_concurrent",
    cooldown = "fni_budget_cooldown",
    "fni_budget_session"
  )
  if (exists("ui_str", mode = "function")) {
    msg <- tryCatch(ui_str(key, locale = loc), error = function(e) "")
    if (nzchar(msg %||% "") && !identical(msg, key)) return(msg)
  }
  if (identical(loc, "zh-TW")) {
    switch(
      reason,
      host = "此主機今日的 AI 分析次數已達上限（YNOW_AI_MAX_RUNS_HOST）。請明日再試。Lite 與完整版共用此上限，用來保護 Cloudflare 帳戶。",
      concurrent = "已有分析進行中（YNOW_AI_MAX_CONCURRENT）。請稍後再按「執行分析」。",
      cooldown = "此工作階段距上次分析太近（YNOW_AI_MIN_INTERVAL_SEC）。請稍候再按「執行分析」。Lite 與完整版共用此間隔。",
      "此工作階段已達 AI 分析次數上限（YNOW_AI_MAX_RUNS）。請稍後再試。Lite 與完整版共用此上限，用來保護 Cloudflare 帳戶。"
    )
  } else {
    switch(
      reason,
      host = "This host reached today's AI analysis limit (YNOW_AI_MAX_RUNS_HOST). Try again tomorrow. Lite and Full share this limit so a public host cannot spend the Cloudflare account without a bound.",
      concurrent = "Another analysis is already running (YNOW_AI_MAX_CONCURRENT). Press Run analysis again in a moment.",
      cooldown = "This session must wait before another analysis (YNOW_AI_MIN_INTERVAL_SEC). Press Run analysis again after the interval. Lite and Full share this cooldown.",
      "This session reached the AI analysis limit (YNOW_AI_MAX_RUNS). Try again later. Lite and Full share this limit so a public host cannot spend the Cloudflare account without a bound."
    )
  }
}

.ynow_fni_budget_paths <- function() {
  dir <- ynow_fni_cache_dir()
  list(
    dir = dir,
    state = file.path(dir, "cf_spend.json"),
    guard = file.path(dir, "cf_spend.lockdir")
  )
}

.ynow_fni_budget_read <- function(path) {
  empty <- list(
    day = "", host_runs = 0L, inflight = 0L, inflight_at = 0,
    sessions = list(), last_run = list()
  )
  if (!file.exists(path)) return(empty)
  parsed <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(parsed)) return(empty)
  sessions <- parsed$sessions
  if (!is.list(sessions)) sessions <- list()
  last_run <- parsed$last_run
  if (!is.list(last_run)) last_run <- list()
  list(
    day = as.character(parsed$day %||% "")[1],
    host_runs = as.integer(parsed$host_runs %||% 0L)[1],
    inflight = as.integer(parsed$inflight %||% 0L)[1],
    inflight_at = suppressWarnings(as.numeric(parsed$inflight_at %||% 0)[1]),
    sessions = sessions,
    last_run = last_run
  )
}

.ynow_fni_budget_write <- function(path, state) {
  jsonlite::write_json(state, path, auto_unbox = TRUE, pretty = FALSE, null = "null")
}

.ynow_fni_budget_mutate <- function(fn) {
  paths <- .ynow_fni_budget_paths()
  if (dir.exists(paths$guard)) {
    info <- file.info(paths$guard)
    age <- Sys.time() - info$mtime
    if (is.finite(as.numeric(age)) && age > 30) unlink(paths$guard, recursive = TRUE)
  }
  got <- FALSE
  for (i in seq_len(40)) {
    got <- isTRUE(dir.create(paths$guard, showWarnings = FALSE))
    if (got) break
    Sys.sleep(0.05)
  }
  if (!got) {
    return(list(
      ok = FALSE,
      error = "ai_budget_busy",
      reason = "concurrent",
      message = .ynow_fni_budget_message("concurrent", "en")
    ))
  }
  on.exit(unlink(paths$guard, recursive = TRUE), add = TRUE)
  state <- .ynow_fni_budget_read(paths$state)
  today <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  if (!identical(state$day, today)) {
    state$day <- today
    state$host_runs <- 0L
    state$sessions <- list()
    state$last_run <- list()
  }
  if (!is.finite(state$inflight)) state$inflight <- 0L
  if (state$inflight < 0L) state$inflight <- 0L
  if (state$inflight > 0L && is.finite(state$inflight_at) && state$inflight_at > 0 &&
      (as.numeric(Sys.time()) - state$inflight_at) > 900) {
    state$inflight <- 0L
    state$inflight_at <- 0
  }
  out <- fn(state)
  if (is.list(out$state)) .ynow_fni_budget_write(paths$state, out$state)
  out$result
}

ynow_fni_cf_budget_begin <- function(session_key = "", locale = "en") {
  sid <- .ynow_fni_budget_session_id(session_key)
  loc <- if (identical(as.character(locale)[1], "zh-TW")) "zh-TW" else "en"
  result <- .ynow_fni_budget_mutate(function(state) {
    host_cap <- ynow_fni_max_runs_host()
    session_cap <- ynow_fni_max_runs_session()
    conc_cap <- ynow_fni_max_concurrent()
    used_s <- as.integer(state$sessions[[sid]] %||% 0L)[1]
    if (!is.finite(used_s) || used_s < 0L) used_s <- 0L
    if (!is.list(state$last_run)) state$last_run <- list()
    interval <- ynow_fni_min_interval_sec()
    last_at <- suppressWarnings(as.numeric(state$last_run[[sid]] %||% 0)[1])
    if (!is.finite(last_at)) last_at <- 0
    cooling <- interval > 0 && last_at > 0 && (as.numeric(Sys.time()) - last_at) < interval
    reason <- if (state$host_runs >= host_cap) {
      "host"
    } else if (used_s >= session_cap) {
      "session"
    } else if (state$inflight >= conc_cap) {
      "concurrent"
    } else if (isTRUE(cooling)) {
      "cooldown"
    } else {
      ""
    }
    if (nzchar(reason)) {
      return(list(
        state = state,
        result = list(
          ok = FALSE,
          error = "ai_budget_exhausted",
          reason = reason,
          message = .ynow_fni_budget_message(reason, loc)
        )
      ))
    }
    state$host_runs <- state$host_runs + 1L
    state$sessions[[sid]] <- used_s + 1L
    state$last_run[[sid]] <- as.numeric(Sys.time())
    state$inflight <- state$inflight + 1L
    if (!is.finite(state$inflight_at) || state$inflight_at <= 0) {
      state$inflight_at <- as.numeric(Sys.time())
    }
    list(
      state = state,
      result = list(
        ok = TRUE,
        error = "",
        reason = "",
        message = "",
        sid = sid,
        counted = TRUE
      )
    )
  })
  if (isTRUE(result$ok)) {
    .ynow_fni_budget_rt$depth <- .ynow_fni_budget_rt$depth + 1L
  }
  result
}

ynow_fni_cf_budget_end <- function(lease) {
  if (!is.list(lease) || !isTRUE(lease$counted)) return(invisible(FALSE))
  if (isTRUE(lease$ended)) return(invisible(FALSE))
  .ynow_fni_budget_mutate(function(state) {
    state$inflight <- max(0L, as.integer(state$inflight %||% 0L)[1] - 1L)
    if (state$inflight == 0L) state$inflight_at <- 0
    list(state = state, result = TRUE)
  })
  .ynow_fni_budget_rt$depth <- max(0L, .ynow_fni_budget_rt$depth - 1L)
  invisible(TRUE)
}

ynow_fni_cf_budget_refund <- function(lease) {
  if (!is.list(lease) || !isTRUE(lease$counted) || isTRUE(lease$refunded)) {
    return(invisible(FALSE))
  }
  sid <- as.character(lease$sid %||% "")[1]
  .ynow_fni_budget_mutate(function(state) {
    state$host_runs <- max(0L, as.integer(state$host_runs %||% 0L)[1] - 1L)
    if (nzchar(sid) && !is.null(state$sessions[[sid]])) {
      state$sessions[[sid]] <- max(0L, as.integer(state$sessions[[sid]] %||% 0L)[1] - 1L)
    }
    list(state = state, result = TRUE)
  })
  lease$refunded <- TRUE
  invisible(TRUE)
}

#' Take one host/session run if this call is not already inside a lease.
#' Live HTTP helpers use this so a direct call cannot skip the cap.
ynow_fni_cf_budget_guard <- function(session_key = "", locale = "en") {
  if (.ynow_fni_budget_rt$depth >= 1L) {
    return(list(ok = TRUE, counted = FALSE, owned = FALSE, error = "", message = ""))
  }
  lease <- ynow_fni_cf_budget_begin(session_key, locale = locale)
  lease$owned <- isTRUE(lease$ok)
  lease
}

#' Combined service readiness (local parse + Cloudflare Workers AI) for UI banner.
ynow_fni_service_status <- function() {
  mock_on <- .ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")
  enabled <- isTRUE(ynow_fni_ai_enabled()) || isTRUE(mock_on)
  doc <- ynow_fni_document_status()
  llm <- ynow_fni_llm_status()
  model_missing <- identical(llm$status, "reachable_model_missing")
  # ready requires Workers AI (unless mock). Do not green-banner on model-missing.
  # Local extract can be ok while the LLM is still unconfigured.
  ready <- isTRUE(enabled) &&
    isTRUE(doc$ok) &&
    isTRUE(llm$ok) &&
    !isTRUE(model_missing)
  msg <- if (!isTRUE(enabled) && !isTRUE(mock_on)) {
    .ynow_fni_cf_not_configured_message()
  } else if (!isTRUE(doc$ok)) {
    doc$message %||% "AI service not configured (document parse)."
  } else if (isTRUE(model_missing) || !isTRUE(llm$ok)) {
    llm$message %||% "AI service not configured (Cloudflare Workers AI)."
  } else {
    "Ready"
  }
  list(
    enabled = enabled,
    ready = ready,
    message = msg,
    document = doc,
    llm = llm,
    # Compatibility fields for older banner readers.
    docling = doc,
    ollama = llm,
    model = if (isTRUE(mock_on)) "mock" else ynow_fni_llm_model(),
    prompt_version = .YNOW_FNI_PROMPT_VERSION,
    parse_backend = ynow_fni_parse_backend()
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
    # Text-like uploads stay on the local reader. PDF / other binaries go through
    # ynow_fni_convert_file (local PDF text, else Cloudflare Markdown Conversion).
    ext <- tolower(tools::file_ext(fname))
    if (ext %in% c("txt", "md", "csv", "json")) {
      raw <- tryCatch(
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
        error = function(e) ""
      )
      doc <- ynow_fni_ingest_plain_text(raw, source_label = fname)
    } else {
      doc <- ynow_fni_convert_file(path, filename = fname)
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
                                      max_chunks = 8L,
                                      session_key = "") {
  locale <- if (identical(as.character(locale)[1], "zh-TW")) "zh-TW" else "en"
  # Live Cloudflare calls (LLM and Markdown Conversion) share one run lease.
  # Mock and unconfigured hosts do not touch the counter.
  live_cf <- !.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0") &&
    isTRUE(.ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0")) &&
    isTRUE(ynow_fni_cf_credentials_present())
  lease <- NULL
  if (isTRUE(live_cf)) {
    lease <- ynow_fni_cf_budget_begin(session_key, locale = locale)
    if (!isTRUE(lease$ok)) {
      return(list(
        ok = FALSE,
        error = lease$error %||% "ai_budget_exhausted",
        message = lease$message %||% .ynow_fni_budget_message(lease$reason %||% "session", locale),
        provenance = NULL,
        result = NULL,
        cache_hit = FALSE
      ))
    }
    on.exit(ynow_fni_cf_budget_end(lease), add = TRUE)
  }
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
  model <- ynow_fni_llm_model()
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
      if (is.list(lease)) ynow_fni_cf_budget_refund(lease)
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
    resp <- ynow_fni_llm_chat_json(ch$text, meta = meta, locale = locale)
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
                                         force = FALSE,
                                         session_key = "") {
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
    one <- ynow_fni_analyze_document(
      src,
      locale = locale,
      force = force,
      session_key = session_key
    )
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
                                      key = "",
                                      session_key = "") {
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
    force = isTRUE(force) || identical(as.character(force)[1], "TRUE"),
    session_key = session_key
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
