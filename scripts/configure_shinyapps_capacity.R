#!/usr/bin/env Rscript
# ==========================================
# configure_shinyapps_capacity.R
# ==========================================
# Audit + apply shinyapps.io scaling so TheYNowApp can hold ≥500 concurrent
# sessions without worker OOM (when the subscription plan allows it).
#
# Usage:
#   Rscript scripts/configure_shinyapps_capacity.R           # audit + apply
#   Rscript scripts/configure_shinyapps_capacity.R --dry-run # audit only
#   Rscript scripts/configure_shinyapps_capacity.R --audit   # alias of dry-run
#
# Exit codes:
#   0  capacity ≥ 500 after apply (or already)
#   1  applied / audited but plan cannot reach 500 (upgrade required)
#   2  missing credentials / API failure

args <- commandArgs(trailingOnly = TRUE)
dry_run <- any(args %in% c("--dry-run", "--audit", "-n"))

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

root <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) == 1L && nzchar(file_arg)) {
    normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
  } else {
    normalizePath(getwd(), mustWork = TRUE)
  }
}
source(file.path(root, "scripts", "ynow_capacity.R"), local = FALSE)

acct <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
tok <- Sys.getenv("SHINYAPPS_TOKEN", "")
sec <- Sys.getenv("SHINYAPPS_SECRET", "")
use_env <- nzchar(acct) && nzchar(tok) && nzchar(sec)
if (use_env) {
  rsconnect::setAccountInfo(name = acct, token = tok, secret = sec)
} else {
  acc <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
  if (is.null(acc) || !nrow(acc)) {
    message("Missing shinyapps credentials (SHINYAPPS_* or rsconnect::accounts()).")
    quit(status = 2)
  }
  acct <- as.character(acc$name[[1]])
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "TheYNowApp")
app_id <- suppressWarnings(as.integer(Sys.getenv("SHINYAPPS_APP_ID", "10907657")))

ns <- asNamespace("rsconnect")
account_details <- rsconnect::accountInfo(acct)
client <- get("clientForAccount", envir = ns)(account_details)

accts <- client$accountsForUser()
acct_row <- accts[[1]]
for (a in accts) {
  if (identical(as.character(a$name), acct) ||
      identical(as.integer(a$id), as.integer(account_details$accountId))) {
    acct_row <- a
    break
  }
}
plan_name <- tryCatch(acct_row$subscription$plan$name, error = function(e) "unknown")
plan_type <- tryCatch(acct_row$subscription$type$name, error = function(e) "unknown")
ents <- tryCatch(acct_row$subscription$entitlements, error = function(e) NULL)
if (is.null(ents)) {
  message("Could not read subscription entitlements.")
  quit(status = 2)
}

#' Merge deployment defaults with explicit application property overrides.
#' listApplicationProperties holds the effective overrides; getApplication
#' deployment.properties often still shows platform defaults.
ynow_effective_props <- function(client, app_id) {
  app <- client$getApplication(app_id)
  props <- app$deployment$properties %||% list()
  listed <- tryCatch(client$listApplicationProperties(app_id), error = function(e) NULL)
  if (!is.null(listed) && is.list(listed$properties)) {
    for (row in listed$properties) {
      if (!is.null(row$name)) props[[row$name]] <- row$value
    }
  }
  props
}

props <- ynow_effective_props(client, app_id)

cur_size <- as.character(props[["application.instances.template"]] %||% "large")[1]
cur_workers <- as.integer(props[["application.shiny.scheduler.max.processes"]] %||% 1)
cur_conn <- as.integer(props[["application.shiny.scheduler.max.requests"]] %||% 50)
cur_inst_start <- as.integer(props[["application.instances.start"]] %||% 1)
cur_inst <- max(1L, cur_inst_start)
cur_cap <- ynow_capacity_connections(cur_inst, cur_workers, cur_conn)
cur_mb <- ynow_capacity_mb_per_conn(cur_size, cur_workers, cur_conn)

rec <- ynow_capacity_stable_for_plan(ents)
ideal <- ynow_capacity_from_entitlements(ents, target = .YNOW_CAPACITY_TARGET)

cat("=== TheYNowApp capacity audit ===\n")
cat(sprintf("Account: %s | plan: %s (%s)\n", acct, plan_name, plan_type))
cat(sprintf(
  "Entitlements: max_instances=%s max_workers=%s sizes=%s\n",
  ents$MaxInstancesLimit$limit %||% "?",
  ents$MaxApplicationWorkersLimit$limit %||% "?",
  paste(unlist(ents$ApplicationInstanceType$items %||% list("?")), collapse = ",")
))
cat(sprintf(
  "Current (effective): size=%s instances≈%d workers=%d max_conn=%d → capacity≈%d (≈%.0f MB/conn usable)\n",
  cur_size, cur_inst, cur_workers, cur_conn, cur_cap, cur_mb
))
cat(sprintf(
  "Recommended for plan: size=%s instances=%d workers=%d max_conn=%d start=%d → capacity=%d (≈%.0f MB/conn)\n",
  rec$settings$size, rec$settings$instances, rec$settings$workers,
  rec$settings$max_connections, rec$settings$instances_start,
  rec$capacity, rec$mb_per_conn
))
cat(sprintf("Target 500 reachable on this plan: %s\n", if (isTRUE(ideal$ok)) "YES" else "NO"))
cat(ideal$reason, "\n")
if (!isTRUE(ideal$ok)) {
  cat(
    "Action required: upgrade shinyapps.io to Professional ($349/mo)\n",
    "  https://www.shinyapps.io/admin/#/account/", acct, "/billing\n",
    "  Then re-run: Rscript scripts/configure_shinyapps_capacity.R\n",
    sep = ""
  )
}

# Coerce to JSON-friendly types (API rejects quoted strings for numbers).
as_prop_value <- function(name, value) {
  if (grepl("load\\.factor$", name)) return(as.numeric(value))
  if (grepl("template$|visibility$|version$|tag$", name)) return(as.character(value)[1])
  # timeouts, counts, processes, requests, thresholds
  v <- suppressWarnings(as.integer(value))
  if (is.finite(v)) return(v)
  as.character(value)[1]
}

set_prop <- function(name, value) {
  value <- as_prop_value(name, value)
  old <- props[[name]]
  old_cmp <- if (is.null(old)) "" else as.character(old)[1]
  new_cmp <- as.character(value)[1]
  if (identical(old_cmp, new_cmp)) {
    cat(sprintf("  keep %s=%s\n", name, new_cmp))
    return(invisible(FALSE))
  }
  if (isTRUE(dry_run)) {
    cat(sprintf("  DRY-RUN would set %s: %s -> %s\n", name, old_cmp, new_cmp))
    return(invisible(TRUE))
  }
  cat(sprintf("  set %s: %s -> %s\n", name, old_cmp, new_cmp))
  ok <- tryCatch(
    {
      client$setApplicationProperty(app_id, name, value, force = TRUE)
      TRUE
    },
    error = function(e) {
      cat(sprintf("  WARN failed %s: %s\n", name, conditionMessage(e)))
      FALSE
    }
  )
  invisible(isTRUE(ok))
}

s <- rec$settings
cat(if (dry_run) "=== Dry-run changes ===\n" else "=== Applying settings (no bundle redeploy) ===\n")
# NOTE: shinyapps API rejects `application.instances.count`. Extra instances
# must be added in the dashboard ([+] Add instance) after a plan upgrade.
set_prop("application.instances.template", s$size)
set_prop("application.instances.start", s$instances_start)
set_prop("application.instances.load.factor", s$instance_load_factor)
set_prop("application.shiny.scheduler.max.processes", s$workers)
set_prop("application.shiny.scheduler.min.processes", 0L)
set_prop("application.shiny.scheduler.max.requests", s$max_connections)
set_prop("application.shiny.scheduler.load.factor", s$worker_load_factor)
set_prop("application.shiny.timeout.conn", s$timeout_conn)
set_prop("application.shiny.timeout.idle", s$timeout_idle)
set_prop("application.shiny.timeout.init", s$timeout_init)

plan_max_inst <- tryCatch(as.integer(ents$MaxInstancesLimit$limit), error = function(e) 1L)
if (!isTRUE(dry_run) && isTRUE(ideal$ok) && plan_max_inst > 1L) {
  cat(sprintf(
    "MANUAL STEP: In shinyapps dashboard → TheYNowApp → Settings, click [+] Add instance until you have %d instances (API cannot set instance count).\n",
    s$instances
  ))
  cat(sprintf("  https://www.shinyapps.io/admin/#/application/%s\n", app_id))
}

if (!dry_run) {
  Sys.sleep(1)
  props2 <- ynow_effective_props(client, app_id)
  n_size <- as.character(props2[["application.instances.template"]] %||% s$size)[1]
  n_workers <- as.integer(props2[["application.shiny.scheduler.max.processes"]] %||% s$workers)
  n_conn <- as.integer(props2[["application.shiny.scheduler.max.requests"]] %||% s$max_connections)
  n_start <- as.integer(props2[["application.instances.start"]] %||% 1L)
  n_inst_assumed <- if (isTRUE(ideal$ok)) s$instances else max(1L, n_start)
  n_cap <- ynow_capacity_connections(n_inst_assumed, n_workers, n_conn)
  n_cap_now <- ynow_capacity_connections(max(1L, n_start), n_workers, n_conn)
  cat(sprintf(
    "After apply (effective): size=%s start=%d workers=%d max_conn=%d → capacity_now≈%d (capacity_if_%d_instances≈%d)\n",
    n_size, n_start, n_workers, n_conn, n_cap_now, n_inst_assumed, n_cap
  ))
  if (isTRUE(ideal$ok) && n_cap >= .YNOW_CAPACITY_TARGET) {
    cat("OK: settings ready for ≥ 500 after dashboard instance add.\n")
    quit(status = 0)
  }
  if (n_cap_now >= .YNOW_CAPACITY_TARGET) {
    cat("OK: capacity ≥ 500.\n")
    quit(status = 0)
  }
  cat("WARN: capacity still < 500 — subscription upgrade required.\n")
  quit(status = 1)
}

if (rec$capacity >= .YNOW_CAPACITY_TARGET && isTRUE(ideal$ok)) {
  cat("OK (dry-run): plan can reach ≥ 500 with recommended settings.\n")
  quit(status = 0)
}
cat("WARN (dry-run): plan cannot reach 500 yet.\n")
quit(status = 1)
