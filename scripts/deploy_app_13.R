#!/usr/bin/env Rscript
# Deploy app_13.0 → shinyapps.io TheYNowApp
#
# Requires env:
#   SHINYAPPS_ACCOUNT  (e.g. hopesmasher1118)
#   SHINYAPPS_TOKEN
#   SHINYAPPS_SECRET
# Optional:
#   SHINYAPPS_APP_NAME (default TheYNowApp)
#   SHINYAPPS_APP_ID   (default 10907657)

acct <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
tok  <- Sys.getenv("SHINYAPPS_TOKEN", "")
sec  <- Sys.getenv("SHINYAPPS_SECRET", "")
if (!nzchar(acct) || !nzchar(tok) || !nzchar(sec)) {
  message(paste(
    "Missing shinyapps credentials.",
    "Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, SHINYAPPS_SECRET",
    "(from https://www.shinyapps.io/admin/#/tokens) then re-run:",
    "  Rscript scripts/deploy_app_13.R",
    sep = "\n"
  ))
  quit(status = 2)
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
app_dir <- file.path(root, "app_13.0")
if (!dir.exists(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
  stop("app_13.0/app.R not found under ", root)
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "TheYNowApp")
app_id <- suppressWarnings(as.integer(Sys.getenv("SHINYAPPS_APP_ID", "10907657")))

message("Configuring account: ", acct)
rsconnect::setAccountInfo(name = acct, token = tok, secret = sec)

message("Deploying ", app_dir, " → ", app_name, " (appId=", app_id, ")")
res <- rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  appId = if (is.finite(app_id)) app_id else NULL,
  account = acct,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)

message("Deploy finished.")
print(res)
invisible(res)
