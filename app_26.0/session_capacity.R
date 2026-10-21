# ==========================================
# session_capacity.R — Starter-tier session / memory helpers
# ==========================================
# Target host: shinyapps.io Starter (1 instance × 1 worker × ≤1 GB).
# Goals: shrink per-session RSS, share process caches, serialize heavy jobs
# so ~6–8 concurrent browse/search sessions do not OOM the single worker.

.ynow_heavy_job_state <- new.env(parent = emptyenv())
.ynow_heavy_job_state$busy <- FALSE
.ynow_heavy_job_state$owner <- NULL
.ynow_heavy_job_state$started_at <- NA_real_

#' TRUE when this process should serialize Lab / HFV / Clustering jobs.
ynow_capacity_serialize_heavy_jobs <- function() {
  if (identical(Sys.getenv("YNOW_HEAVY_JOB_PARALLEL"), "1")) return(FALSE)
  if (identical(Sys.getenv("YNOW_HEAVY_JOB_SERIAL"), "1")) return(TRUE)
  if (exists(".ynow_async_hosted_memory_safe", mode = "function")) {
    return(isTRUE(.ynow_async_hosted_memory_safe()))
  }
  if (exists(".ynow_is_hosted_connect", mode = "function")) {
    return(isTRUE(.ynow_is_hosted_connect()))
  }
  FALSE
}

#' Try to acquire the process-wide heavy-job slot (Lab eval / cluster / HFV).
#' @return TRUE if caller may start; FALSE if another heavy job is running.
ynow_heavy_job_try_acquire <- function(job_id = "job") {
  job_id <- as.character(job_id %||% "job")[1]
  if (!isTRUE(ynow_capacity_serialize_heavy_jobs())) return(TRUE)
  # Stale lock (>45 min): recover from killed ExtendedTask / crashed path.
  started <- suppressWarnings(as.numeric(.ynow_heavy_job_state$started_at %||% NA_real_)[1])
  if (isTRUE(.ynow_heavy_job_state$busy) && is.finite(started) &&
      (as.numeric(Sys.time()) - started) > 45 * 60) {
    .ynow_heavy_job_state$busy <- FALSE
    .ynow_heavy_job_state$owner <- NULL
  }
  if (isTRUE(.ynow_heavy_job_state$busy)) return(FALSE)
  .ynow_heavy_job_state$busy <- TRUE
  .ynow_heavy_job_state$owner <- job_id
  .ynow_heavy_job_state$started_at <- as.numeric(Sys.time())
  TRUE
}

#' Release the heavy-job slot (safe to call from any session).
ynow_heavy_job_release <- function(job_id = NULL) {
  if (!isTRUE(ynow_capacity_serialize_heavy_jobs())) return(invisible(FALSE))
  if (!is.null(job_id)) {
    cur <- as.character(.ynow_heavy_job_state$owner %||% "")[1]
    want <- as.character(job_id)[1]
    if (nzchar(cur) && nzchar(want) && !identical(cur, want)) {
      return(invisible(FALSE))
    }
  }
  .ynow_heavy_job_state$busy <- FALSE
  .ynow_heavy_job_state$owner <- NULL
  .ynow_heavy_job_state$started_at <- NA_real_
  invisible(TRUE)
}

ynow_heavy_job_owner <- function() {
  as.character(.ynow_heavy_job_state$owner %||% "")[1]
}

# --- Process-global hist price trim cache (shared across sessions on one worker) ---
.ynow_hist_price_global <- new.env(parent = emptyenv())

ynow_hist_price_ttl_sec <- function() {
  ttl <- tryCatch(as.integer(.YNOW_CACHE_FAST_TTL_SEC)[1], error = function(e) NA_integer_)
  if (!is.finite(ttl) || ttl < 60L) 900L else ttl
}

#' Fetch ~180d Close history with process-global TTL (Starter multi-session reuse).
ynow_cached_hist_stock_trim <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk)) return(NULL)
  now_ts <- as.numeric(Sys.time())
  if (exists(tk, envir = .ynow_hist_price_global, inherits = FALSE)) {
    hit <- get(tk, envir = .ynow_hist_price_global, inherits = FALSE)
    fetched_at <- suppressWarnings(as.numeric(attr(hit, "fetched_at") %||% NA_real_)[1])
    if (is.finite(fetched_at) && (now_ts - fetched_at) <= ynow_hist_price_ttl_sec()) {
      return(hit)
    }
  }
  if (!exists("fetch_price_history_df", mode = "function")) return(NULL)
  df_final <- tryCatch({
    hist <- fetch_price_history_df(tk, "1y")
    if (is.null(hist) || nrow(hist) < 30) stop("insufficient history")
    cutoff <- Sys.Date() - 180
    hist <- hist[hist$Date >= cutoff, , drop = FALSE]
    data.frame(
      Date = hist$Date,
      Open = NA_real_, High = NA_real_, Low = NA_real_,
      Close = hist$Close,
      Volume = if ("Volume" %in% names(hist)) hist$Volume else NA_real_,
      Adjusted = hist$Close,
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
  if (!is.null(df_final)) {
    attr(df_final, "fetched_at") <- now_ts
    assign(tk, df_final, envir = .ynow_hist_price_global)
  }
  df_final
}

#' Drop one ticker (or all) from the shared hist cache.
ynow_hist_price_cache_clear <- function(ticker = NULL) {
  if (is.null(ticker) || !nzchar(as.character(ticker)[1])) {
    rm(list = ls(envir = .ynow_hist_price_global), envir = .ynow_hist_price_global)
  } else {
    tk <- toupper(trimws(as.character(ticker)[1]))
    if (exists(tk, envir = .ynow_hist_price_global, inherits = FALSE)) {
      rm(list = tk, envir = .ynow_hist_price_global)
    }
  }
  invisible(TRUE)
}

#' Best-effort session teardown for Starter memory reclaim.
#' Call from session$onSessionEnded; pass a list of clearer functions / reactiveVals.
ynow_session_capacity_cleanup <- function(clearers = list()) {
  for (fn in clearers) {
    tryCatch({
      if (is.function(fn)) fn()
    }, error = function(e) NULL)
  }
  tryCatch(gc(verbose = FALSE, full = TRUE), error = function(e) NULL)
  invisible(TRUE)
}
