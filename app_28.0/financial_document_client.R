# financial_document_client.R — local extract + Cloudflare Markdown Conversion
# Default parse path: plain text / text-layer PDF in-process; other binaries via
# POST /accounts/{account_id}/ai/tomarkdown.
# Never calls a model or conversion on page load — only when the user runs analysis.
#
# Extension hook (NOT implemented in this build): Google Cloud Run Docling.
# If local extract + Cloudflare Markdown Conversion cannot recover complex finance
# tables, a later change may route YNOW_FNI_PARSE_BACKEND=docling to
# ynow_fni_docling_cloudrun_convert() with YNOW_DOCLING_URL set to that Cloud Run
# service. Do not treat Docling as configured today.

.ynow_fni_env_flag <- function(name, default = "0") {
  v <- tolower(trimws(Sys.getenv(name, unset = as.character(default))))
  v %in% c("1", "true", "yes", "on")
}

.ynow_fni_cf_probe_cache <- new.env(parent = emptyenv())

ynow_fni_cf_account_id <- function() {
  id <- trimws(Sys.getenv("YNOW_CF_ACCOUNT_ID", unset = ""))
  if (!nzchar(id)) id <- trimws(Sys.getenv("CLOUDFLARE_ACCOUNT_ID", unset = ""))
  id
}

ynow_fni_cf_api_token <- function() {
  tok <- trimws(Sys.getenv("YNOW_CF_API_TOKEN", unset = ""))
  if (!nzchar(tok)) tok <- trimws(Sys.getenv("CLOUDFLARE_API_TOKEN", unset = ""))
  tok
}

ynow_fni_cf_credentials_present <- function() {
  nzchar(ynow_fni_cf_account_id()) && nzchar(ynow_fni_cf_api_token())
}

ynow_fni_cf_api_root <- function() {
  id <- utils::URLencode(ynow_fni_cf_account_id(), reserved = TRUE)
  paste0("https://api.cloudflare.com/client/v4/accounts/", id)
}

ynow_fni_cf_markdown_url <- function() {
  paste0(ynow_fni_cf_api_root(), "/ai/tomarkdown")
}

#' httr config objects for Cloudflare calls (timeout + Bearer).
#' Always return a list of config objects — never c() add_headers into a plain list
#' (that strips the request class and drops auth).
.ynow_fni_cf_httr_config <- function(timeout) {
  list(
    httr::timeout(timeout),
    httr::add_headers(
      Authorization = paste("Bearer", ynow_fni_cf_api_token())
    )
  )
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

#' local_cf (default) or docling (Cloud Run hook; not implemented).
ynow_fni_parse_backend <- function() {
  b <- tolower(trimws(Sys.getenv("YNOW_FNI_PARSE_BACKEND", unset = "local_cf")))
  if (!nzchar(b)) b <- "local_cf"
  if (b %in% c("docling", "docling_cloudrun", "cloud_run_docling")) return("docling")
  "local_cf"
}

#' Whether live Workers AI / Markdown Conversion calls are allowed.
ynow_fni_ai_enabled <- function() {
  .ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0") ||
    .ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")
}

.ynow_fni_cf_not_configured_message <- function() {
  paste(
    "AI service not configured.",
    "Set YNOW_AI_QUAL_ENABLED=1 and provide YNOW_CF_ACCOUNT_ID plus YNOW_CF_API_TOKEN",
    "(Cloudflare Workers AI). Text and text-layer PDFs extract locally;",
    "other files use Cloudflare Markdown Conversion."
  )
}

#' Lightweight token check: GET /ai/tomarkdown/supported (no conversion, no LLM).
#' Cached ~60s so a banner render does not hammer the API. Never logs the token.
.ynow_fni_cf_probe <- function(timeout = 3) {
  if (!isTRUE(ynow_fni_cf_credentials_present())) {
    return(list(
      ok = FALSE,
      status = "not_configured",
      message = .ynow_fni_cf_not_configured_message(),
      http_status = NA_integer_
    ))
  }
  tok <- ynow_fni_cf_api_token()
  checksum <- sum(utf8ToInt(tok)) %% 100000L
  key <- paste(ynow_fni_cf_account_id(), nchar(tok), checksum, sep = ":")
  now <- as.numeric(Sys.time())
  hit <- .ynow_fni_cf_probe_cache[[key]]
  if (is.list(hit) && is.finite(hit$expires %||% NA_real_) && hit$expires > now && is.list(hit$result)) {
    return(hit$result)
  }
  result <- .ynow_fni_cf_probe_uncached(timeout)
  .ynow_fni_cf_probe_cache[[key]] <- list(expires = now + 60, result = result)
  result
}

.ynow_fni_cf_probe_uncached <- function(timeout = 3) {
  base <- ynow_fni_cf_markdown_url()
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE,
      status = "unavailable",
      message = "httr package is required for Cloudflare HTTP calls.",
      http_status = NA_integer_
    ))
  }
  url <- paste0(base, "/supported")
  res <- tryCatch(
    do.call(httr::GET, c(list(url), .ynow_fni_cf_httr_config(timeout))),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(
      ok = FALSE,
      status = "offline",
      message = paste0(
        "Cloudflare Workers AI unreachable: ",
        conditionMessage(res)
      ),
      http_status = NA_integer_
    ))
  }
  code <- tryCatch(httr::status_code(res), error = function(e) NA_integer_)
  body_txt <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
  parsed <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)
  success <- is.list(parsed) && isTRUE(parsed$success)
  if (is.finite(code) && code >= 200 && code < 300 && (is.null(parsed) || success || is.null(parsed$success))) {
    return(list(
      ok = TRUE,
      status = "reachable",
      message = "Cloudflare Workers AI credentials accepted.",
      http_status = code
    ))
  }
  err <- ""
  if (is.list(parsed) && length(parsed$errors)) {
    e1 <- parsed$errors[[1]]
    err <- as.character(e1$message %||% "")[1]
  }
  status <- if (is.finite(code) && code %in% c(401L, 403L)) "auth_error" else "error"
  list(
    ok = FALSE,
    status = status,
    message = paste0(
      "Cloudflare Workers AI HTTP ", code,
      if (nzchar(err)) paste0(": ", err) else ""
    ),
    http_status = code
  )
}

#' Probe document-parse readiness without converting a file.
#' Local extract is always available; Cloudflare Markdown Conversion is optional
#' until a binary file needs it. ok=TRUE means the local path is usable.
ynow_fni_document_status <- function(timeout = 3) {
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(list(
      ok = TRUE,
      status = "mock",
      message = "Mock document extract enabled (YNOW_AI_QUAL_MOCK=1).",
      url = "mock://document"
    ))
  }
  local_note <- "Local text and text-layer PDF extraction is available."
  if (!isTRUE(.ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0")) ||
      !isTRUE(ynow_fni_cf_credentials_present())) {
    return(list(
      ok = TRUE,
      status = "local_only",
      message = paste(local_note, "Cloudflare Markdown Conversion is not configured."),
      url = ""
    ))
  }
  probe <- .ynow_fni_cf_probe(timeout)
  if (!isTRUE(probe$ok)) {
    return(list(
      ok = TRUE,
      status = "local_only",
      message = paste(local_note, probe$message %||% ""),
      url = ynow_fni_cf_markdown_url()
    ))
  }
  list(
    ok = TRUE,
    status = "cf_markdown",
    message = paste(local_note, "Cloudflare Markdown Conversion is reachable."),
    url = ynow_fni_cf_markdown_url()
  )
}

# Compatibility alias (older tests / callers). Prefer ynow_fni_document_status.
ynow_fni_docling_status <- function(...) ynow_fni_document_status(...)

.ynow_fni_text_usable <- function(text, min_chars = 40L) {
  text <- as.character(text %||% "")[1]
  squashed <- gsub("[[:space:]]+", "", text, perl = TRUE)
  nchar(squashed) >= min_chars && grepl("[[:alpha:]]", text, perl = TRUE)
}

.ynow_fni_read_text_file <- function(path) {
  tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) ""
  )
}

.ynow_fni_pdf_local_text <- function(path) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    txt <- tryCatch(
      paste(pdftools::pdf_text(path), collapse = "\n"),
      error = function(e) ""
    )
    if (.ynow_fni_text_usable(txt)) return(txt)
  }
  bin <- Sys.which("pdftotext")
  if (!nzchar(bin)) return("")
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(out), add = TRUE)
  rc <- tryCatch(
    system2(
      bin,
      c("-enc", "UTF-8", "-layout", path, out),
      stdout = FALSE,
      stderr = FALSE
    ),
    error = function(e) 1L
  )
  if (!identical(as.integer(rc)[1], 0L) || !file.exists(out)) return("")
  txt <- .ynow_fni_read_text_file(out)
  if (.ynow_fni_text_usable(txt)) txt else ""
}

#' In-process extract for plain text and text-layer PDFs.
#' Returns list(ok=, text=, kind=, ext=). ok=FALSE means the caller should try
#' Cloudflare Markdown Conversion (or report that it is not configured).
ynow_fni_try_local_extract <- function(path, filename = NULL) {
  fname <- as.character(filename %||% basename(path))[1]
  ext <- tolower(tools::file_ext(fname))
  plain_ext <- c("txt", "md", "csv", "json", "text", "log")
  text <- ""
  kind <- ""
  if (ext %in% plain_ext) {
    text <- .ynow_fni_read_text_file(path)
    kind <- "local_text"
  } else if (identical(ext, "pdf")) {
    text <- .ynow_fni_pdf_local_text(path)
    kind <- "local_pdf"
  }
  ok <- if (ext %in% plain_ext) nzchar(trimws(text)) else .ynow_fni_text_usable(text)
  list(ok = isTRUE(ok), text = text, kind = kind, ext = ext)
}

#' Deferred Cloud Run Docling seam. Always fails closed until a later PR
#' implements the Cloud Run client. Not used by the default local_cf backend.
ynow_fni_docling_cloudrun_convert <- function(path,
                                             filename = NULL,
                                             timeout = NULL) {
  list(
    ok = FALSE,
    text = "",
    markdown = "",
    pages = list(),
    meta = list(
      filename = as.character(filename %||% basename(as.character(path %||% "")))[1],
      source = "docling_cloudrun",
      deferred = TRUE
    ),
    error = "docling_cloudrun_not_implemented",
    message = paste(
      "Cloud Run Docling is not implemented.",
      "The default parse path is R local extract plus Cloudflare Markdown Conversion.",
      "Add a Cloud Run client here only if complex finance-table quality is insufficient.",
      "Intended env: YNOW_FNI_PARSE_BACKEND=docling and YNOW_DOCLING_URL=<Cloud Run origin>."
    )
  )
}

ynow_fni_cf_markdown_convert <- function(path, filename = NULL, timeout = NULL) {
  fname <- as.character(filename %||% basename(path))[1]
  if (!isTRUE(ynow_fni_cf_credentials_present()) ||
      !isTRUE(.ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0"))) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname, source = "cf_markdown"),
      error = "ai_service_not_configured",
      message = .ynow_fni_cf_not_configured_message()
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname),
      error = "httr_missing",
      message = "httr package is required for Cloudflare HTTP calls."
    ))
  }
  to <- timeout %||% ynow_fni_request_timeout()
  url <- ynow_fni_cf_markdown_url()
  res <- tryCatch(
    do.call(
      httr::POST,
      c(
        list(
          url,
          body = list(
            files = httr::upload_file(path),
            conversionOptions = '{"output":{"format":"markdown"}}'
          ),
          encode = "multipart"
        ),
        .ynow_fni_cf_httr_config(to)
      )
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname, source = "cf_markdown", url = url),
      error = "cf_markdown_request_failed",
      message = conditionMessage(res)
    ))
  }
  code <- httr::status_code(res)
  body_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)
  item <- NULL
  if (is.list(parsed)) {
    item <- parsed$result
    if (is.list(item) && is.null(item$data) && length(item) >= 1L) item <- item[[1]]
  }
  err <- ""
  if (is.list(item)) err <- as.character(item$error %||% "")[1]
  if (is.list(parsed) && isTRUE(parsed$success == FALSE) && !nzchar(err) && length(parsed$errors)) {
    err <- as.character(parsed$errors[[1]]$message %||% "")[1]
  }
  md <- ""
  fmt <- ""
  if (is.list(item)) {
    md <- as.character(item$data %||% "")[1]
    fmt <- as.character(item$format %||% "")[1]
  }
  bad <- !(is.finite(code) && code >= 200 && code < 300) ||
    identical(fmt, "error") ||
    (nzchar(err) && !nzchar(md))
  if (bad || !nzchar(md)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname, source = "cf_markdown", url = url, http_status = code),
      error = "cf_markdown_bad_response",
      message = if (nzchar(err)) err else substr(body_txt %||% "", 1, 400)
    ))
  }
  list(
    ok = TRUE,
    text = md,
    markdown = md,
    pages = list(),
    meta = list(
      filename = fname,
      source = "cf_markdown",
      url = url,
      http_status = code,
      format = fmt
    ),
    error = ""
  )
}

#' Convert a local file (mock, local extract, Cloudflare Markdown, or deferred Docling).
#' Returns list(ok=, text=, markdown=, pages=, meta=, error=, message=).
ynow_fni_convert_file <- function(path, filename = NULL, timeout = NULL) {
  path <- as.character(path %||% "")[1]
  if (!nzchar(path) || !file.exists(path)) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(), error = "file_not_found"
    ))
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
    return(.ynow_fni_mock_convert(path, fname))
  }
  # Explicit opt-in only. Default backend never calls this.
  if (identical(ynow_fni_parse_backend(), "docling")) {
    return(ynow_fni_docling_cloudrun_convert(path, filename = fname, timeout = timeout))
  }
  local <- ynow_fni_try_local_extract(path, filename = fname)
  if (isTRUE(local$ok)) {
    return(list(
      ok = TRUE,
      text = local$text,
      markdown = local$text,
      pages = list(),
      meta = list(
        filename = fname,
        source = "local_extract",
        kind = local$kind
      ),
      error = ""
    ))
  }
  ext <- tolower(tools::file_ext(fname))
  needs_cf <- !(ext %in% c("txt", "md", "csv", "json", "text", "log"))
  if (!needs_cf) {
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname, source = "local_extract"),
      error = "empty_extraction",
      message = "Local text extract returned empty text."
    ))
  }
  if (!isTRUE(.ynow_fni_env_flag("YNOW_AI_QUAL_ENABLED", "0")) ||
      !isTRUE(ynow_fni_cf_credentials_present())) {
    why <- if (identical(ext, "pdf")) {
      "Local PDF text extraction returned no usable text."
    } else {
      "This file type is not extracted locally."
    }
    return(list(
      ok = FALSE, text = "", markdown = "", pages = list(),
      meta = list(filename = fname, source = "cf_markdown"),
      error = "ai_service_not_configured",
      message = paste(why, .ynow_fni_cf_not_configured_message())
    ))
  }
  ynow_fni_cf_markdown_convert(path, filename = fname, timeout = timeout)
}

# Compatibility alias. Prefer ynow_fni_convert_file.
ynow_fni_docling_convert_file <- function(...) ynow_fni_convert_file(...)

#' Convert plain pasted text (no Cloudflare round-trip).
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

.ynow_fni_mock_convert <- function(path, fname) {
  raw <- .ynow_fni_read_text_file(path)
  if (!nzchar(raw)) {
    raw <- paste0(
      "Mock document extraction for ", fname, ".\n",
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
    meta = list(filename = fname, source = "mock"),
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
