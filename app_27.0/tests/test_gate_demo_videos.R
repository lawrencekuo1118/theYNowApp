# Gate demo clips: product ynow_gate_demo_features() ids have mp4+webm ≈10s
# from live App UI. Non-product gate ids ship no teaser.
# Run: Rscript app_27.0/tests/test_gate_demo_videos.R

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("test_gate_demo_videos.R", mustWork = FALSE)
}
app_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
demos_dir <- file.path(app_dir, "www", "demos")
setwd(app_dir)

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
demo_feats <- ynow_gate_demo_features()
non_demo <- setdiff(feats, demo_feats)
check(
  "gate features include lab..atx",
  identical(
    sort(feats),
    sort(c("lab", "checklist", "alerts", "account", "pdf", "snap_defaults", "inner_lock", "atx"))
  )
)
check(
  "demo features are product-only",
  identical(
    sort(demo_feats),
    sort(c("lab", "checklist", "alerts", "pdf", "atx"))
  )
)
check(
  "non-product gates include account + snap_defaults + inner_lock",
  identical(sort(non_demo), sort(c("account", "snap_defaults", "inner_lock")))
)
check(
  "account has no promo page",
  isFALSE(ynow_gate_has_promo("account")) &&
    is.null(ynow_gate_promo_faq_ui(feature = "account", include_faq = FALSE))
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
  if (length(out) < 1L) return(NA_real_)
  suppressWarnings(as.numeric(out[[1]]))
}

for (f in demo_feats) {
  mp4 <- file.path(demos_dir, paste0(f, ".mp4"))
  webm <- file.path(demos_dir, paste0(f, ".webm"))
  check(paste0(f, " mp4 exists"), file.exists(mp4) && file.info(mp4)$size > 20000)
  check(paste0(f, " webm exists"), file.exists(webm) && file.info(webm)$size > 20000)
  if (isTRUE(.has_probe)) {
    d_mp4 <- .duration(mp4)
    d_webm <- .duration(webm)
    check(paste0(f, " mp4 ~10s"), is.finite(d_mp4) && d_mp4 >= 8 && d_mp4 <= 11)
    check(paste0(f, " webm ~10s"), is.finite(d_webm) && d_webm >= 8 && d_webm <= 11)
  }
  check(paste0(f, " has_demo TRUE"), isTRUE(ynow_gate_has_demo(f)))
}

for (f in non_demo) {
  mp4 <- file.path(demos_dir, paste0(f, ".mp4"))
  webm <- file.path(demos_dir, paste0(f, ".webm"))
  check(paste0(f, " mp4 absent"), !file.exists(mp4))
  check(paste0(f, " webm absent"), !file.exists(webm))
  check(paste0(f, " has_demo FALSE"), isFALSE(ynow_gate_has_demo(f)))
  check(paste0(f, " has_promo FALSE"), isFALSE(ynow_gate_has_promo(f)))
  check(
    paste0(f, " promo helper NULL"),
    is.null(ynow_gate_promo_faq_ui(feature = f, include_faq = FALSE))
  )
}

auth <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check("promo UI wires demos/<feature>.mp4", grepl('paste0(demo_base, ".mp4")', auth, fixed = TRUE))
check("promo UI wires demos/<feature>.webm", grepl('paste0(demo_base, ".webm")', auth, fixed = TRUE))
check("capture seed documents local-only", grepl("YNOW_DEMO_CAPTURE", auth, fixed = TRUE))
check("demo allowlist helper present", grepl("ynow_gate_demo_features", auth, fixed = TRUE))

gen <- file.path(app_dir, "..", "scripts", "generate_gate_demo_videos.py")
check("generator script present", file.exists(gen))
gen_txt <- paste(readLines(gen, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "generator is live App UI capture (not fixture/PIL)",
  grepl("running App UI", gen_txt, fixed = TRUE) &&
    grepl("capture_live", gen_txt, fixed = TRUE) &&
    grepl("_require_authed", gen_txt, fixed = TRUE) &&
    !grepl("ImageDraw", gen_txt, fixed = TRUE) &&
    !grepl("gate_demo_pages", gen_txt, fixed = TRUE) &&
    !grepl("capture_fixture", gen_txt, fixed = TRUE)
)
check(
  "generator skips non-product snap_defaults / inner_lock",
  !grepl('"snap_defaults"', gen_txt, fixed = TRUE) &&
    !grepl('"inner_lock"', gen_txt, fixed = TRUE) &&
    !grepl("foot_snapshot", gen_txt, fixed = TRUE)
)
check(
  "generator captures product demo ids",
  grepl('"lab"', gen_txt, fixed = TRUE) &&
    grepl('"checklist"', gen_txt, fixed = TRUE) &&
    grepl('"alerts"', gen_txt, fixed = TRUE) &&
    grepl('"pdf"', gen_txt, fixed = TRUE) &&
    grepl('"atx"', gen_txt, fixed = TRUE) &&
    !grepl('"account"', gen_txt, fixed = TRUE)
)
orch <- file.path(app_dir, "..", "scripts", "record_gate_demos.sh")
check("record_gate_demos.sh present", file.exists(orch))
orch_txt <- paste(readLines(orch, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "orchestrator sets YNOW_DEMO_CAPTURE locally",
  grepl("YNOW_DEMO_CAPTURE=1", orch_txt, fixed = TRUE) &&
    grepl("Never set YNOW_DEMO_CAPTURE", orch_txt, fixed = TRUE) &&
    grepl("shinyapps", orch_txt, fixed = TRUE)
)
check(
  "orchestrator documents product-only",
  grepl("product", orch_txt, ignore.case = TRUE) &&
    grepl("snap_defaults", orch_txt, fixed = TRUE)
)
fix_dir <- file.path(app_dir, "..", "scripts", "gate_demo_pages")
check("fixture HTML directory removed", !dir.exists(fix_dir))

message("PASS ", ok, " FAIL ", bad)
if (bad > 0L) quit(status = 1)
invisible(NULL)
