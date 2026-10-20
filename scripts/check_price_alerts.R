#!/usr/bin/env Rscript
# Check saved price/condition rules and send mail when one is met.
# Run from the repo root: Rscript scripts/check_price_alerts.R
# Uses the live app directory (app_25.0) and its .Renviron.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

app <- file.path(root, "app_25.0")
if (!dir.exists(app)) {
  stop("live app directory not found: ", app)
}
renv <- file.path(app, ".Renviron")
if (file.exists(renv)) readRenviron(renv)
setwd(app)
source("global.R", local = FALSE, encoding = "UTF-8")
out <- price_alert_check_store()
message(
  "price alerts: rules=", nrow(out$df),
  " met=", out$met,
  " sent=", out$sent
)
