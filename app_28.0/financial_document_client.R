# financial_document_client.R — Docling Serve client (free/open document parse)
# Defaults to localhost. Never auto-starts Docling; honest empty when offline.

.ynow_fni_env_flag <- function(name, default = "0") {
  v <- tolower(trimws(Sys.getenv(name, unset = as.character(default))))
  v %in% c("1", "true", "yes", "on")
}

ynow_fni_docling_url <- function() {
  u <- trimws(Sys.getenv("YNOW_DOCLING_URL", unset = "http://127.0.0.1:5001"))
  if (!nzchar(u)) u <- "http://127.0.0.1:5001"
  sub("/+$", "", u)
}

ynow_fni_docling_api_key <- function() {
  trimws(Sys.getenv("YNOW_DOCLING_API_KEY", unset = ""))
}

ynow_fni_request_timeout <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_AI_REQUEST_TIMEOUT", unset = "120")))
  if (!is.finite(v) || v <= 0) v <- 120
  v
}

ynow_fni_max_file_mb <- function() {
  v <- suppressWarnings(as.numeric(Sys.getenv("YNOW_AI_MAX_FILE_MB", unset = "25")))
  if (!is.finite(v) || v <= 0) v <- 25
  v
}

#' Whether Docling is configured/enabled for qualitative analysis.
ynow_fni_ai_enabled <- function() {
  .ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0") ||
    .ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")
}

#' Probe Docling health without converting a document.
#' Returns list(ok=, status=, message=, url=).
ynow_fni_docling_status <- function(timeout = 3) {
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(list(
      ok = TRUE,
      status = "mock",
      message = "Mock Docling enabled (YNOW_AI_QUAL_MOCK=1).",
      url = "mock://docling"
    ))
  }
  if (!isTRUE(ynow_fni_ai_enabled()) &&
      !nzchar(Sys.getenv("YNOW_DOCLING_URL", unset = ""))) {
    # Explicit URL still allows probe even when master flag is off.
  }
  base <- ynow_fni_docling_url()
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE,
      status = "unavailable",
      message = "httr package is required for Docling HTTP calls.",
      url = base
    ))
  }
  # Docling Serve commonly exposes /health or /docs; try a few.
  paths <- c("/health", "/openapi.json", "/")
  last_msg <- "AI service not configured"
  for (p in paths) {
    url <- paste0(base, p)
    res <- tryCatch(
      {
        hdrs <- list()
        key <- ynow_fni_docling_api_key()
        if (nzchar(key)) hdrs <- c(hdrs, httr::add_headers(Authorization = paste("Bearer", key)))
        httr::GET(url, httr::timeout(timeout), hdrs)
      },
      error = function(e) e
    )
    if (inherits(res, "error")) {
      last_msg <- paste0("Docling unreachable at ", base, ": ", conditionMessage(res))
      next
    }
    code <- tryCatch(httr::status_code(res), error = function(e) NA_integer_)
    if (is.finite(code) && code >= 200 && code < 500) {
      return(list(
        ok = TRUE,
        status = "reachable",
        message = paste0("Docling reachable (", code, ") at ", base),
        url = base,
        http_status = code
      ))
    }
    last_msg <- paste0("Docling HTTP ", code, " at ", url)
  }
  list(ok = FALSE, status = "offline", message = last_msg, url = base)
}

#' Convert a local file via Docling Serve (or mock).
#' Returns list(ok=, text=, markdown=, pages=, meta=, error=).
ynow_fni_docling_convert_file <- function(path,
                                          filename = NULL,
                                          timeout = NULL) {
  path <- as.character(path %||% "")[1]
  if (!nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, text = "", markdown = "", pages = list(),
                meta = list(), error = "file_not_found"))
  }
  info <- file.info(path)
  max_b <- ynow_fni_max_file_mb() * 1024^2
  if (isTRUE(info$size > max_b)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(), meta = list(),
      error = sprintf("file_too_large_max_%s_mb", ynow_fni_max_file_mb())
    ))
  }
  fname <- as.character(filename %||% basename(path))[1]
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(.ynow_fni_docling_mock_convert(path, fname))
  }
  if (!isTRUE(ynow_fni_ai_enabled())) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(), meta = list(),
      error = "ai_service_not_configured",
      message = "AI service not configured. Set YNOW_AI_QUAL_ENABLED=1 and start Docling Serve."
    ))
  }
  st <- ynow_fni_docling_status(timeout = min(3, ynow_fni_request_timeout()))
  if (!isTRUE(st$ok)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(), meta = list(),
      error = "ai_service_not_configured",
      message = st$message %||% "AI service not configured"
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(ok = FALSE, text = "", markdown = "", pages = list(),
                meta = list(), error = "httr_missing"))
  }
  to <- timeout %||% ynow_fni_request_timeout()
  base <- ynow_fni_docling_url()
  # Docling Serve v1-style convert endpoint (file upload).
  url <- paste0(base, "/v1/convert/file")
  key <- ynow_fni_docling_api_key()
  hdrs <- list()
  if (nzchar(key)) hdrs <- c(hdrs, httr::add_headers(Authorization = paste("Bearer", key)))
  res <- tryCatch(
    httr::POST(
      url,
      body = list(files = httr::upload_file(path)),
      encode = "multipart",
      httr::timeout(to),
      hdrs
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    # Fallback path name used by some Docling Serve builds
    url2 <- paste0(base, "/convert")
    res <- tryCatch(
      httr::POST(
        url2,
        body = list(file = httr::upload_file(path)),
        encode = "multipart",
        httr::timeout(to),
        hdrs
      ),
      error = function(e) e
    )
  }
  if (inherits(res, "error")) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(), meta = list(),
      error = "docling_request_failed",
      message = conditionMessage(res)
    ))
  }
  code <- httr::status_code(res)
  body_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(body_txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !(code >= 200 && code < 300)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(http_status = code),
      error = "docling_bad_response",
      message = substr(body_txt %||% "", 1, 400)
    ))
  }
  # Normalize common Docling response shapes.
  md <- ""
  txt <- ""
  pages <- list()
  if (!is.null(parsed$document)) {
    md <- as.character(parsed$document$md_content %||%
                         parsed$document$markdown %||% "")[1]
    txt <- as.character(parsed$document$text_content %||%
                          parsed$document$text %||% md)[1]
  } else {
    md <- as.character(parsed$md_content %||% parsed$markdown %||% "")[1]
    txt <- as.character(parsed$text_content %||% parsed$text %||% md)[1]
  }
  if (!nzchar(txt) && nzchar(md)) txt <- md
  list(
    ok = nzchar(txt),
    text = txt,
    markdown = md,
    pages = pages,
    meta = list(
      filename = fname,
      source = "docling",
      url = base,
      http_status = code
    ),
    error = if (nzchar(txt)) "" else "empty_extraction"
  )
}

#' Convert plain pasted text (no Docling round-trip).
ynow_fni_ingest_plain_text <- function(text, source_label = "paste") {
  text <- as.character(text %||% "")[1]
  list(
    ok = nzchar(trimws(text)),
    text = text,
    markdown = text,
    pages = list(),
    meta = list(source = source_label, filename = source_label),
    error = if (nzchar(trimws(text))) "" else "empty_text"
  )
}

.ynow_fni_docling_mock_convert <- function(path, fname) {
  # Prefer reading UTF-8 text fixtures; otherwise wrap a short stub.
  raw <- tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) ""
  )
  if (!nzchar(raw)) {
    raw <- paste0(
      "Mock Docling extraction for ", fname, ".\n",
      "Management expects revenue growth from AI servers in the next year.\n",
      "Gross margin outlook remains under pricing pressure.\n",
      "CapEx will focus on capacity expansion; dividends stay stable.\n",
      "Key risks include customer concentration and FX volatility.\n",
      "CFO guidance: mid-single-digit growth; no change to long-term WACC assumptions.\n"
    )
  }
  list(
    ok = TRUE,
    text = raw,
    markdown = raw,
    pages = list(list(page = 1L, text = raw)),
    meta = list(filename = fname, source = "mock_docling"),
    error = ""
  )
}

#' Section-aware chunking for long documents.
#' Returns list of list(id=, title=, text=, start=, end=).
ynow_fni_chunk_text <- function(text, max_chars = 3500L, overlap = 200L) {
  text <- as.character(text %||% "")[1]
  if (!nzchar(text)) return(list())
  max_chars <- as.integer(max_chars)[1]
  if (!is.finite(max_chars) || max_chars < 500L) max_chars <- 3500L
  overlap <- as.integer(overlap)[1]
  if (!is.finite(overlap) || overlap < 0L) overlap <- 200L

  # Prefer markdown / filing section headers when present.
  parts <- unlist(strsplit(text, "(?m)(?=^#{1,3}\\s+|^(ITEM|Item)\\s+\\d+)", perl = TRUE))
  parts <- parts[nzchar(trimws(parts))]
  if (!length(parts)) parts <- text

  chunks <- list()
  buf <- ""
  title <- "body"
  flush_buf <- function() {
    if (!nzchar(trimws(buf))) return()
    # Further split oversized buffer
    while (nchar(buf) > max_chars) {
      cut <- max_chars
      # try break on paragraph
      window <- substr(buf, 1, max_chars)
      br <- max(gregexpr("\n\n", window, fixed = TRUE)[[1]])
      if (is.finite(br) && br > max_chars * 0.4) cut <- br
      piece <- substr(buf, 1, cut)
      chunks[[length(chunks) + 1L]] <<- list(
        id = sprintf("c%03d", length(chunks) + 1L),
        title = title,
        text = piece,
        start = NA_integer_,
        end = NA_integer_
      )
      buf <<- substr(buf, max(1L, cut - overlap + 1L), nchar(buf))
    }
    if (nzchar(trimws(buf))) {
      chunks[[length(chunks) + 1L]] <<- list(
        id = sprintf("c%03d", length(chunks) + 1L),
        title = title,
        text = buf,
        start = NA_integer_,
        end = NA_integer_
      )
      buf <<- ""
    }
  }

  for (p in parts) {
    hdr <- sub("\n.*$", "", p, perl = TRUE)
    hdr <- trimws(substr(hdr, 1, 80))
    if (!nzchar(hdr)) hdr <- "body"
    if (nchar(buf) + nchar(p) > max_chars && nzchar(buf)) {
      flush_buf()
      title <- hdr
      buf <- p
    } else {
      if (!nzchar(buf)) title <- hdr
      buf <- paste0(buf, if (nzchar(buf)) "\n" else "", p)
    }
  }
  flush_buf()
  chunks
}
