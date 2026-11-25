#!/usr/bin/env Rscript
# Install every package required for a complete rsconnect shinyapps manifest.
# Used by Actions Tip-ship before deploy_app_21.R. Safe to re-run (idempotent).
#
# Usage:
#   Rscript scripts/install_deploy_app_packages.R

root <- tryCatch(
  {
    args <- commandArgs(trailingOnly = FALSE)
    f <- grep("^--file=", args, value = TRUE)
    if (length(f)) {
      normalizePath(file.path(dirname(sub("^--file=", "", f[[1]])), ".."))
    } else {
      normalizePath(".")
    }
  },
  error = function(e) normalizePath(".")
)
pkg_src <- file.path(root, "scripts", "ynow_deploy_required_packages.R")
if (!file.exists(pkg_src)) {
  stop("Missing ", pkg_src, call. = FALSE)
}
source(pkg_src, local = TRUE)

pkgs <- unique(ynow_deploy_required_packages())
missing <- ynow_deploy_missing_packages(pkgs)
if (length(missing)) {
  message("Installing ", length(missing), " deploy packages: ", paste(missing, collapse = ", "))
  install.packages(missing, quiet = TRUE, repos = "https://cloud.r-project.org")
}

still <- ynow_deploy_missing_packages(pkgs)
if (length(still)) {
  stop(
    "Failed to install required deploy packages:\n  ",
    paste(still, collapse = ", "),
    call. = FALSE
  )
}

# reticulate managed-Python path needs >= 1.47.0 (same gate as deploy_app_21.R).
if (utils::packageVersion("reticulate") < "1.47.0") {
  message("Upgrading reticulate to >= 1.47.0…")
  install.packages("reticulate", quiet = TRUE, repos = "https://cloud.r-project.org")
  if (utils::packageVersion("reticulate") < "1.47.0") {
    stop("reticulate >= 1.47.0 required after upgrade attempt.", call. = FALSE)
  }
}

# Load dynlib-sensitive packages once so apt lib mismatches fail here, not mid-upload.
for (p in c("rsconnect", "RPostgres", "sodium", "shiny")) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    stop("Failed to load package ", p, " after install — check system libs", call. = FALSE)
  }
}

crit_missing <- ynow_deploy_missing_packages(ynow_deploy_boot_critical_packages())
if (length(crit_missing)) {
  stop(
    "Boot-critical packages still missing after install:\n  ",
    paste(crit_missing, collapse = ", "),
    call. = FALSE
  )
}

message(
  "Deploy app packages OK (", length(pkgs), " required; reticulate ",
  utils::packageVersion("reticulate"), ")"
)
