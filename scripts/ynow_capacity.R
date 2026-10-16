# ==========================================
# ynow_capacity.R — shinyapps.io concurrency planning
# ==========================================
# Pure helpers (no network) plus optional live audit / apply.
# Target: ≥500 concurrent browser sessions without worker OOM.
#
# Capacity formula (Posit):
#   concurrent ≈ instances × max_workers × max_connections
# Memory budget (engineering heuristic for this heavy valuation app):
#   MB_per_conn ≈ instance_MB / (max_workers × max_connections)
# Prefer ≥120 MB/conn after worker base RSS; lower risks crash under Lab/HFV.

.YNOW_CAPACITY_TARGET <- 500L

.YNOW_INSTANCE_MB <- c(
  small = 256L,
  medium = 512L,
  large = 1024L,
  xlarge = 2048L,
  xxlarge = 4096L,
  xxxlarge = 8192L
)

#' Theoretical max concurrent connections.
ynow_capacity_connections <- function(instances, workers, max_connections) {
  as.integer(instances) * as.integer(workers) * as.integer(max_connections)
}

#' Usable session RAM (MB) after reserving base RSS per worker process.
ynow_capacity_usable_mb <- function(instance_size, workers, base_worker_mb = 280) {
  size <- tolower(as.character(instance_size)[1])
  mb <- .YNOW_INSTANCE_MB[[size]]
  if (is.null(mb) || !is.finite(mb)) return(NA_real_)
  w <- max(1L, as.integer(workers)[1])
  max(0, as.numeric(mb) - w * as.numeric(base_worker_mb))
}

#' Memory per connection (MB) after worker base RSS reservation.
ynow_capacity_mb_per_conn <- function(instance_size, workers, max_connections,
                                      base_worker_mb = 280) {
  usable <- ynow_capacity_usable_mb(instance_size, workers, base_worker_mb)
  if (!is.finite(usable)) return(NA_real_)
  denom <- as.integer(workers) * as.integer(max_connections)
  if (!is.finite(denom) || denom < 1L) return(NA_real_)
  usable / denom
}

#' Recommended settings for a target concurrent count given plan ceilings.
#' @param target desired concurrent sessions (default 500)
#' @param max_instances plan MaxInstancesLimit
#' @param max_workers plan MaxApplicationWorkersLimit
#' @param allowed_sizes character vector of instance templates allowed by plan
#' @param min_mb_per_conn soft floor for memory/connection (default 120)
#' @return list(ok=, settings=, capacity=, mb_per_conn=, reason=)
ynow_capacity_recommend <- function(
    target = .YNOW_CAPACITY_TARGET,
    max_instances = 1L,
    max_workers = 1L,
    allowed_sizes = c("small", "medium", "large"),
    min_mb_per_conn = 120,
    max_connections_cap = 50L
) {
  target <- as.integer(target)[1]
  max_instances <- max(1L, as.integer(max_instances)[1])
  max_workers <- max(1L, as.integer(max_workers)[1])
  max_connections_cap <- max(1L, as.integer(max_connections_cap)[1])
  allowed_sizes <- unique(tolower(as.character(allowed_sizes)))
  allowed_sizes <- allowed_sizes[allowed_sizes %in% names(.YNOW_INSTANCE_MB)]
  if (!length(allowed_sizes)) allowed_sizes <- "large"

  # Prefer largest allowed instance; search workers × connections from dense → sparse.
  size <- allowed_sizes[which.max(.YNOW_INSTANCE_MB[allowed_sizes])]
  best <- NULL
  for (w in seq.int(max_workers, 1L)) {
    # connections needed if we use all instances and this worker count
    need_per_inst <- ceiling(target / max_instances / w)
    if (need_per_inst > max_connections_cap) next
    for (c in seq.int(max(need_per_inst, 1L), max_connections_cap)) {
      cap <- ynow_capacity_connections(max_instances, w, c)
      if (cap < target) next
      mb <- ynow_capacity_mb_per_conn(size, w, c)
      if (!is.finite(mb) || mb < min_mb_per_conn) next
      cand <- list(
        instances = max_instances,
        workers = w,
        max_connections = c,
        size = size,
        capacity = cap,
        mb_per_conn = mb
      )
      # Prefer fewer connections per worker (isolation), then higher MB/conn.
      score <- c(-c, mb, -w)
      if (is.null(best) ||
          score[1] > best$score[1] ||
          (score[1] == best$score[1] && score[2] > best$score[2]) ||
          (score[1] == best$score[1] && score[2] == best$score[2] && score[3] > best$score[3])) {
        best <- c(cand, list(score = score))
      }
    }
  }

  if (is.null(best)) {
    # Fall back: max theoretical on this plan (may be < target and/or low MB).
    w <- max_workers
    c <- max_connections_cap
    cap <- ynow_capacity_connections(max_instances, w, c)
    mb <- ynow_capacity_mb_per_conn(size, w, c)
    reason <- sprintf(
      "Plan ceiling cannot meet target=%d with ≥%.0f MB/conn (max theoretical=%d @ %s, workers=%d, conn=%d, ~%.0f MB/conn). Upgrade shinyapps.io plan (Professional recommended for 500).",
      target, min_mb_per_conn, cap, size, w, c, mb
    )
    return(list(
      ok = FALSE,
      target = target,
      settings = list(
        instances = max_instances,
        workers = w,
        max_connections = c,
        size = size,
        instances_start = min(2L, max_instances),
        worker_load_factor = 0.5,
        instance_load_factor = 0.5,
        timeout_conn = 600L,
        timeout_idle = 60L,
        timeout_init = 300L
      ),
      capacity = cap,
      mb_per_conn = mb,
      reason = reason
    ))
  }

  list(
    ok = TRUE,
    target = target,
    settings = list(
      instances = best$instances,
      workers = best$workers,
      max_connections = best$max_connections,
      size = best$size,
      # Warm a couple of instances so flash traffic does not wait on cold start.
      instances_start = min(as.integer(ceiling(best$instances / 3)), best$instances),
      worker_load_factor = 0.5,
      instance_load_factor = 0.5,
      timeout_conn = 600L,
      timeout_idle = 60L,
      timeout_init = 300L
    ),
    capacity = best$capacity,
    mb_per_conn = best$mb_per_conn,
    reason = sprintf(
      "Meets target=%d with capacity=%d (instances=%d × workers=%d × conn=%d on %s, ~%.0f MB/conn).",
      target, best$capacity, best$instances, best$workers, best$max_connections, best$size, best$mb_per_conn
    )
  )
}

#' Map live shinyapps entitlements → recommend() inputs.
ynow_capacity_from_entitlements <- function(entitlements, target = .YNOW_CAPACITY_TARGET) {
  if (!is.list(entitlements)) stop("entitlements must be a list")
  max_inst <- tryCatch(
    as.integer(entitlements$MaxInstancesLimit$limit),
    error = function(e) 1L
  )
  max_workers <- tryCatch(
    as.integer(entitlements$MaxApplicationWorkersLimit$limit),
    error = function(e) 1L
  )
  sizes <- tryCatch(
    unlist(entitlements$ApplicationInstanceType$items, use.names = FALSE),
    error = function(e) c("small", "medium", "large")
  )
  if (!length(sizes)) sizes <- c("small", "medium", "large")
  ynow_capacity_recommend(
    target = target,
    max_instances = max_inst,
    max_workers = max_workers,
    allowed_sizes = sizes
  )
}

#' Best-effort settings for the *current* plan (stability first when below target).
#' On Starter (1×1×50 @ large): lower max_connections so 1 GB does not OOM.
ynow_capacity_stable_for_plan <- function(entitlements) {
  rec <- ynow_capacity_from_entitlements(entitlements, target = .YNOW_CAPACITY_TARGET)
  max_inst <- tryCatch(as.integer(entitlements$MaxInstancesLimit$limit), error = function(e) 1L)
  max_workers <- tryCatch(as.integer(entitlements$MaxApplicationWorkersLimit$limit), error = function(e) 1L)
  sizes <- tryCatch(
    unlist(entitlements$ApplicationInstanceType$items, use.names = FALSE),
    error = function(e) "large"
  )
  size <- sizes[which.max(.YNOW_INSTANCE_MB[tolower(sizes)])]
  if (!length(size) || is.na(size)) size <- "large"
  mb <- .YNOW_INSTANCE_MB[[tolower(size)]] %||% 1024L

  if (isTRUE(rec$ok)) {
    return(rec)
  }

  # Stability path: shrink connections so residual RAM/conn stays ≥ 120 MB.
  # Prefer fewer workers when a single large instance would otherwise OOM.
  w <- max(1L, max_workers)
  usable <- ynow_capacity_usable_mb(size, w)
  if (!is.finite(usable) || usable < 120) {
    # Drop workers until base RSS fits.
    while (w > 1L && (ynow_capacity_usable_mb(size, w) < 120 || !is.finite(ynow_capacity_usable_mb(size, w)))) {
      w <- w - 1L
    }
    usable <- ynow_capacity_usable_mb(size, w)
  }
  c_max <- max(1L, min(50L, as.integer(floor(max(usable, 0) / 120))))
  list(
    ok = FALSE,
    target = .YNOW_CAPACITY_TARGET,
    settings = list(
      instances = max(1L, max_inst),
      workers = w,
      max_connections = c_max,
      size = as.character(size),
      instances_start = 1L,
      worker_load_factor = 0.5,
      instance_load_factor = 0.5,
      timeout_conn = 600L,
      timeout_idle = 60L,
      timeout_init = 300L
    ),
    capacity = ynow_capacity_connections(max(1L, max_inst), w, c_max),
    mb_per_conn = ynow_capacity_mb_per_conn(size, w, c_max),
    reason = rec$reason
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
