#!/usr/bin/env Rscript
# Restore / merge gitignored app_27.0/.Renviron from the live shinyapps bundle.
# Prevents Cloud deploys from dropping Supabase/Neon keys that only exist on the
# server (shinyapps has no Secrets UI).
#
# Usage (repo root):
#   Rscript scripts/restore_renviron_from_shinyapps.R
#   Rscript scripts/restore_renviron_from_shinyapps.R --merge   # keep local-only keys
#
# Never prints secret values. Never commits .Renviron.

args <- commandArgs(trailingOnly = TRUE)
do_merge <- "--merge" %in% args || identical(Sys.getenv("YNOW_RENVIRON_MERGE"), "1")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("rsconnect required", call. = FALSE)
}
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("curl required", call. = FALSE)
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
if (length(file_arg) == 1L && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}

app_dir <- if (dir.exists(file.path(root, "app_27.0"))) {
  file.path(root, "app_27.0")
} else {
  stop("app_27.0 not found under ", root, call. = FALSE)
}
dest <- file.path(app_dir, ".Renviron")

.parse_renviron_keys <- function(path) {
  if (!file.exists(path)) return(character())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  kv <- lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=", lines)]
  keys <- sub("=.*", "", kv)
  nonempty <- nchar(trimws(sub("^[^=]*=", "", kv))) > 0
  keys[nonempty]
}

.merge_renviron <- function(base_path, overlay_path, out_path) {
  # Prefer overlay (local) values when both define a key; keep comments from base.
  base_lines <- if (file.exists(base_path)) {
    readLines(base_path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }
  over_lines <- if (file.exists(overlay_path)) {
    readLines(overlay_path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }
  parse_map <- function(lines) {
    out <- list()
    for (ln in lines) {
      if (!grepl("^[A-Za-z_][A-Za-z0-9_]*=", ln)) next
      k <- sub("=.*", "", ln)
      out[[k]] <- ln
    }
    out
  }
  bm <- parse_map(base_lines)
  om <- parse_map(over_lines)
  keys <- unique(c(names(bm), names(om)))
  # Stable order: Neon / OAuth / points / rest
  prefer <- c(
    "YNOW_DB_HOST", "YNOW_DB_NAME", "YNOW_DB_USER", "YNOW_DB_PASSWORD",
    "YNOW_DB_PORT", "YNOW_DB_SSLMODE", "YNOW_DATABASE_URL",
    "YNOW_ENABLE_ACCOUNT_LOGIN",
    "YNOW_SUPABASE_URL", "YNOW_SUPABASE_ANON_KEY", "YNOW_SUPABASE_SERVICE_ROLE",
    "YNOW_GOOGLE_CLIENT_ID", "YNOW_GOOGLE_CLIENT_SECRET",
    "YNOW_OAUTH_REDIRECT_URI", "YNOW_OAUTH_STATE_SECRET",
    "YNOW_POINTS_ENABLED", "YNOW_POINTS_LOGIN_GRANT",
    "YNOW_POINTS_SIGNUP_BONUS", "YNOW_POINTS_DAILY_CHECKIN"
  )
  keys <- c(prefer[prefer %in% keys], setdiff(keys, prefer))
  out <- vapply(keys, function(k) {
    if (!is.null(om[[k]])) om[[k]] else bm[[k]]
  }, character(1))
  writeLines(c(
    "# Restored/merged by scripts/restore_renviron_from_shinyapps.R — gitignored; never commit.",
    out
  ), out_path, useBytes = TRUE)
  invisible(out_path)
}

info <- tryCatch(
  rsconnect::accountInfo(name = rsconnect::accounts()$name[[1]], server = "shinyapps.io"),
  error = function(e) NULL
)
if (is.null(info)) stop("No rsconnect shinyapps account configured", call. = FALSE)

authInfo <- list(token = info$token, secret = info$secret)
GET <- getFromNamespace("GET", "rsconnect")
signatureHeaders <- getFromNamespace("signatureHeaders", "rsconnect")
service <- list(
  protocol = "https", host = "api.shinyapps.io", port = 443,
  account_name = info$name
)
app_id <- suppressWarnings(as.integer(Sys.getenv("SHINYAPPS_APP_ID", "10907657")))
app <- GET(service, authInfo, paste0("/v1/applications/", app_id))
bid <- app$deployment$image$bundle_id
if (is.null(bid) || !nzchar(as.character(bid))) {
  stop("Could not resolve live bundle id", call. = FALSE)
}
message("Live bundleId=", bid, " status=", app$status)

path <- paste0("/v1/bundles/", bid, "/download")
headers <- signatureHeaders(authInfo, "GET", path, file = NULL)
tmp_tar <- tempfile(fileext = ".tar.gz")
h <- curl::new_handle()
curl::handle_setheaders(
  h,
  Date = headers$Date,
  `X-Auth-Token` = headers$`X-Auth-Token`,
  `X-Auth-Signature` = headers$`X-Auth-Signature`,
  `X-Content-Checksum` = headers$`X-Content-Checksum`
)
curl::handle_setopt(h, followlocation = TRUE)
res <- curl::curl_fetch_disk(paste0("https://api.shinyapps.io", path), tmp_tar, handle = h)
if (!identical(as.integer(res$status_code), 200L)) {
  stop("Bundle download failed status=", res$status_code, call. = FALSE)
}
files <- untar(tmp_tar, list = TRUE)
ren_files <- files[grepl("(^|/)\\.Renviron$", files)]
if (!length(ren_files)) {
  stop("Live bundle ", bid, " has no .Renviron to restore", call. = FALSE)
}
exdir <- tempfile("renv_live_")
dir.create(exdir, recursive = TRUE)
untar(tmp_tar, files = ren_files, exdir = exdir)
ff <- list.files(exdir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
ff <- ff[basename(ff) == ".Renviron"]
if (!length(ff)) stop("Extracted .Renviron not found", call. = FALSE)

live_keys <- .parse_renviron_keys(ff[[1]])
message(
  "Live .Renviron keys (names only): ",
  paste(live_keys, collapse = ", ")
)
has_neon <- any(grepl("^YNOW_DB_|^YNOW_DATABASE_URL$", live_keys))
has_oauth <- all(c("YNOW_SUPABASE_URL", "YNOW_SUPABASE_ANON_KEY") %in% live_keys) ||
  all(c("YNOW_GOOGLE_CLIENT_ID", "YNOW_GOOGLE_CLIENT_SECRET") %in% live_keys)
message("Live has_oauth=", has_oauth, " has_neon=", has_neon)

if (isTRUE(do_merge) && file.exists(dest)) {
  tmp_out <- tempfile("renv_merged_")
  .merge_renviron(ff[[1]], dest, tmp_out)
  file.copy(tmp_out, dest, overwrite = TRUE)
  message("Merged live → local .Renviron (local keys win on conflict)")
} else {
  file.copy(ff[[1]], dest, overwrite = TRUE)
  message("Wrote ", dest, " from live bundle ", bid)
}
Sys.chmod(dest, mode = "0600")
final_keys <- .parse_renviron_keys(dest)
message(
  "Local .Renviron now has keys: ",
  paste(final_keys, collapse = ", ")
)
invisible(list(
  bundle_id = bid,
  path = dest,
  keys = final_keys,
  has_oauth = has_oauth,
  has_neon = any(grepl("^YNOW_DB_|^YNOW_DATABASE_URL$", final_keys))
))
