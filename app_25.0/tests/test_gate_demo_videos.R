# Gate demo clips: every ynow_gate_features() id has mp4+webm ≈10s.
# Run: Rscript app_25.0/tests/test_gate_demo_videos.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_gate_demo_videos.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
demos_dir <- file.path(app_dir, "www", "demos")

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    bad <<- bad + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
  invisible(isTRUE(cond))
}

source(file.path(app_dir, "auth_account.R"), local = TRUE, encoding = "UTF-8")
feats <- ynow_gate_features()
check(
  "gate features include lab..atx",
  identical(
    sort(feats),
    sort(c("lab", "checklist", "alerts", "account", "pdf", "snap_defaults", "inner_lock", "atx"))
  )
)

ffprobe <- Sys.which("ffprobe")
.has_probe <- nzchar(ffprobe)

.duration <- function(path) {
  if (!isTRUE(.has_probe) || !file.exists(path)) return(NA_real_)
  out <- tryCatch(
    system2(
      ffprobe,
      c("-v", "error", "-show_entries", "format=duration", "-of", "default=nk=1:nw=1", path),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) NA_character_
  )
  suppressWarnings(as.numeric(out[[1]]))
}

for (f in feats) {
  mp4 <- file.path(demos_dir, paste0(f, ".mp4"))
  webm <- file.path(demos_dir, paste0(f, ".webm"))
  check(paste0(f, " mp4 exists"), file.exists(mp4) && file.info(mp4)$size > 20000)
  check(paste0(f, " webm exists"), file.exists(webm) && file.info(webm)$size > 20000)
  if (isTRUE(.has_probe)) {
    d_mp4 <- .duration(mp4)
    d_webm <- .duration(webm)
    # Target ≈10s (allow 8–11 for encoder rounding)
    check(paste0(f, " mp4 ~10s"), is.finite(d_mp4) && d_mp4 >= 8 && d_mp4 <= 11)
    check(paste0(f, " webm ~10s"), is.finite(d_webm) && d_webm >= 8 && d_webm <= 11)
  }
}

auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("promo UI wires demos/<feature>.mp4", grepl('paste0(demo_base, ".mp4")', auth, fixed = TRUE))
check("promo UI wires demos/<feature>.webm", grepl('paste0(demo_base, ".webm")', auth, fixed = TRUE))
check("generator script present", file.exists(file.path(app_dir, "..", "scripts", "generate_gate_demo_videos.py")))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
