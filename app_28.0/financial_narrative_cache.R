# financial_narrative_cache.R — disk cache for FNI results
# Cache key: file hash + prompt version + model (+ locale/ticker/period).

ynow_fni_cache_dir <- function() {
  override <- trimws(Sys.getenv("YNOW_AI_CACHE_DIR", unset = ""))
  if (nzchar(override)) {
    dir.create(override, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(override, winslash = "/", mustWork = FALSE))
  }
  root <- ""
  if (exists(".ynow_cache_root", mode = "function")) {
    root <- tryCatch(.ynow_cache_root(), error = function(e) "")
  }
  if (!nzchar(root)) {
    root <- file.path(tempdir(), "ynow_cache")
  }
  path <- file.path(root, "fni")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' SHA-256 (or fallback) content hash for a file or string.
ynow_fni_content_hash <- function(path = NULL, text = NULL) {
  if (nzchar(as.character(path %||% "")[1]) && file.exists(path)) {
    if (requireNamespace("digest", quietly = TRUE)) {
      return(digest::digest(file = path, algo = "sha256"))
    }
    # Fallback: size + mtime + basename
    info <- file.info(path)
    return(sprintf(
      "fb_%s_%s_%s",
      basename(path),
      as.character(info$size %||% 0),
      as.character(as.numeric(info$mtime) %||% 0)
    ))
  }
  text <- as.character(text %||% "")[1]
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(text, algo = "sha256", serialize = FALSE))
  }
  sprintf("fb_%d_%s", nchar(text), substr(gsub("\\s+", "", text), 1, 32))
}

ynow_fni_cache_key <- function(content_hash,
                               model = NULL,
                               prompt_version = NULL,
                               locale = "en",
                               ticker = "",
                               period = "",
                               extra = "") {
  model <- as.character(model %||% ynow_fni_ollama_model())[1]
  prompt_version <- as.character(prompt_version %||% .YNOW_FNI_PROMPT_VERSION)[1]
  locale <- as.character(locale %||% "en")[1]
  raw <- paste(
    content_hash,
    prompt_version,
    model,
    locale,
    toupper(trimws(as.character(ticker %||% "")[1])),
    trimws(as.character(period %||% "")[1]),
    as.character(extra %||% "")[1],
    sep = "|"
  )
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(raw, algo = "sha256", serialize = FALSE)
  } else {
    gsub("[^A-Za-z0-9]", "_", raw)
  }
}

.ynow_fni_cache_path <- function(key) {
  file.path(ynow_fni_cache_dir(), paste0(key, ".rds"))
}

ynow_fni_cache_get <- function(key) {
  key <- as.character(key %||% "")[1]
  if (!nzchar(key)) return(NULL)
  path <- .ynow_fni_cache_path(key)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

ynow_fni_cache_put <- function(key, value) {
  key <- as.character(key %||% "")[1]
  if (!nzchar(key) || is.null(value)) return(invisible(FALSE))
  path <- .ynow_fni_cache_path(key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({
    saveRDS(value, path)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

ynow_fni_cache_clear <- function(key = NULL) {
  if (nzchar(as.character(key %||% "")[1])) {
    path <- .ynow_fni_cache_path(key)
    if (file.exists(path)) unlink(path)
    return(invisible(TRUE))
  }
  dir <- ynow_fni_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(TRUE)
}
