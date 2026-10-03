#!/usr/bin/env Rscript
# Arithmetic and file-rewrite checks for the +0.01 display-version bump.
# Does not deploy and does not rewrite the live app tree.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
source(file.path(script_dir, "bump_display_version.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

check("v17 -> v17.01", identical(ynow_bump_display_label("v17"), "v17.01"))
check("v17.01 -> v17.02", identical(ynow_bump_display_label("v17.01"), "v17.02"))
check("v17.94 -> v17.95", identical(ynow_bump_display_label("v17.94"), "v17.95"))
check("v17.99 -> v18.00", identical(ynow_bump_display_label("v17.99"), "v18.00"))
check("v17.9 -> v17.91", identical(ynow_bump_display_label("v17.9"), "v17.91"))

tmp <- tempfile("ynow-bump-")
dir.create(file.path(tmp, "app_19.0", "www"), recursive = TRUE)
dir.create(file.path(tmp, "app_19.0", "tests"), recursive = TRUE)
dir.create(file.path(tmp, "scripts"), recursive = TRUE)
writeLines(
  c(
    "title The YNow App v17.94",
    "about The YNow App (v17.94) stays",
    "lite The YNow App Lite (v17.94) stays"
  ),
  file.path(tmp, "app_19.0", "ynow_ui.R")
)
writeLines(
  c("---", "  app_version: \"v17.94\"", "---"),
  file.path(tmp, "app_19.0", "report_template.Rmd")
)
writeLines(
  c(
    "# The YNow App v17.94 — Valuation Methodology",
    "",
    "## v17.94 重點",
    "- **目錄**：`app_19.0/`；顯示版號 **v17.94**"
  ),
  file.path(tmp, "app_19.0", "README.md")
)
writeLines(
  "display_version=v17.94\n",
  file.path(tmp, "scripts", "DEPLOY_BASELINE.txt")
)
jsonlite::write_json(
  list(version = "v17.91", note = "keep"),
  file.path(tmp, "app_19.0", "www", "ynow_build.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
writeLines(
  "testthat::expect_true(grepl(\"The YNow App v17.94\", txt, fixed = TRUE))",
  file.path(tmp, "app_19.0", "tests", "test_lite_mode_ui.R")
)

git_tmp <- tempfile("ynow-git-")
dir.create(git_tmp)
writeLines("hello", file.path(git_tmp, "note.txt"))
system2("git", c("-C", git_tmp, "init", "-q"))
init_msg <- tempfile(pattern = "ynow-init-", fileext = ".txt")
writeLines("init", init_msg)
system2("git", c("-C", git_tmp, "add", "note.txt"))
system2("git", c("-C", git_tmp, "commit", "-q", "-F", init_msg))
writeLines("hello v2", file.path(git_tmp, "note.txt"))
commit_ok <- ynow_git_commit_paths(
  git_tmp,
  file.path(git_tmp, "note.txt"),
  "chore: bump display version to v17.95",
  must = FALSE
)
logged <- system2("git", c("-C", git_tmp, "log", "-1", "--format=%s"), stdout = TRUE)
check("commit message keeps spaces", isTRUE(commit_ok) && identical(logged[[1]], "chore: bump display version to v17.95"))
unlink(git_tmp, recursive = TRUE)
unlink(init_msg)

Sys.setenv(YNOW_VERSION_BUMP_NO_COMMIT = "1")
on.exit(Sys.unsetenv("YNOW_VERSION_BUMP_NO_COMMIT"), add = TRUE)
out <- ynow_prepare_deploy_version(tmp, commit = TRUE)
check("prepare bumps once", isTRUE(out$bumped) && identical(out$version, "v17.95"))
ui <- paste(readLines(file.path(tmp, "app_19.0", "ynow_ui.R")), collapse = "\n")
check("ui rewritten", grepl("v17.95", ui, fixed = TRUE) && !grepl("v17.94", ui, fixed = TRUE))
readme <- readLines(file.path(tmp, "app_19.0", "README.md"))
check("readme title bumped", grepl("v17.95", readme[[1]], fixed = TRUE))
check("readme history kept", grepl("v17.94", readme[[3]], fixed = TRUE) && grepl("v17.94", readme[[4]], fixed = TRUE))
rmd <- paste(readLines(file.path(tmp, "app_19.0", "report_template.Rmd")), collapse = "\n")
check("report app_version bumped", grepl("v17.95", rmd, fixed = TRUE))
build <- jsonlite::fromJSON(file.path(tmp, "app_19.0", "www", "ynow_build.json"))
check("build json synced", identical(build$version, "v17.95") && identical(build$note, "keep"))
test_txt <- paste(readLines(file.path(tmp, "app_19.0", "tests", "test_lite_mode_ui.R")), collapse = "\n")
check("lite test expectation bumped", grepl("v17.95", test_txt, fixed = TRUE))

out2 <- ynow_prepare_deploy_version(tmp, commit = TRUE)
check("second prepare does not bump again", isTRUE(!out2$bumped) && identical(out2$version, "v17.95"))
base_lines <- readLines(file.path(tmp, "scripts", "DEPLOY_BASELINE.txt"))
check("baseline unchanged by bump", any(grepl("^display_version=v17.94$", base_lines)))

ynow_record_shipped_version(tmp, "v17.95")
base_after <- readLines(file.path(tmp, "scripts", "DEPLOY_BASELINE.txt"))
check("shipped version recorded", any(grepl("^display_version=v17.95$", base_after)))
out3 <- ynow_prepare_deploy_version(tmp, commit = TRUE)
check("next deploy bumps another 0.01", isTRUE(out3$bumped) && identical(out3$version, "v17.96"))

if (fail > 0L) {
  stop(fail, " bump checks failed")
}
cat("All display-version bump checks passed.\n")
