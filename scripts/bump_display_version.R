#!/usr/bin/env Rscript
# Display-version bump for the live line (app_19.0).
#
# Each successful deploy_app_19.R run advances the display version by +0.01
# and ships that version. Folder name stays app_19.0/ until a full +1 cut.
#
#   Rscript scripts/bump_display_version.R --dry-run
#   Rscript scripts/bump_display_version.R
#
# Skip from deploy with --no-bump or YNOW_SKIP_VERSION_BUMP=1.
# Skip the git commit with YNOW_VERSION_BUMP_NO_COMMIT=1.

ynow_parse_display_version <- function(label) {
  label <- as.character(label)[1]
  if (!is.character(label) || !nzchar(label) || is.na(label)) {
    stop("Unrecognized display version: ", label)
  }
  if (!grepl("^v[0-9]+(\\.[0-9]{1,2})?$", label)) {
    stop("Unrecognized display version: ", label)
  }
  body <- sub("^v", "", label)
  parts <- strsplit(body, ".", fixed = TRUE)[[1]]
  major <- as.integer(parts[[1]])
  if (length(parts) == 1L) {
    minor <- 0L
  } else {
    digits <- parts[[2]]
    minor <- as.integer(digits) * as.integer(10^(2L - nchar(digits)))
  }
  if (is.na(major) || is.na(minor) || minor < 0L || minor > 99L) {
    stop("Unrecognized display version: ", label)
  }
  major * 100L + minor
}

ynow_format_display_version <- function(hundredths) {
  hundredths <- as.integer(hundredths)[1]
  if (is.na(hundredths) || hundredths < 0L) stop("Invalid version hundredths")
  sprintf("v%d.%02d", hundredths %/% 100L, hundredths %% 100L)
}

ynow_bump_display_label <- function(label) {
  ynow_format_display_version(ynow_parse_display_version(label) + 1L)
}

ynow_repo_root_from_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(file_arg) == 1L && nzchar(file_arg)) {
    return(normalizePath(file.path(dirname(file_arg), ".."), mustWork = TRUE))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

ynow_read_display_version <- function(app_dir) {
  ui_path <- file.path(app_dir, "ynow_ui.R")
  if (!file.exists(ui_path)) stop("ynow_ui.R not found: ", ui_path)
  ui <- readLines(ui_path, warn = FALSE, encoding = "UTF-8")
  hit <- grep("The YNow App v[0-9]+(\\.[0-9]{1,2})?", ui, value = TRUE)
  if (!length(hit)) stop("Header version not found in ", ui_path)
  m <- regmatches(hit[[1]], regexpr("v[0-9]+(\\.[0-9]{1,2})?", hit[[1]]))
  if (!length(m) || !nzchar(m[[1]])) stop("Header version not found in ", ui_path)
  m[[1]]
}

ynow_read_baseline_version <- function(root) {
  path <- file.path(root, "scripts", "DEPLOY_BASELINE.txt")
  if (!file.exists(path)) return(NA_character_)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit <- grep("^display_version=", lines, value = TRUE)
  if (!length(hit)) return(NA_character_)
  sub("^display_version=", "", hit[[1]])
}

ynow_replace_label <- function(text, old_label, new_label) {
  gsub(old_label, new_label, text, fixed = TRUE)
}

ynow_write_lines <- function(path, lines) {
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = FALSE)
}

ynow_apply_display_version <- function(root, old_label, new_label) {
  app_dir <- file.path(root, "app_19.0")
  changed <- character(0)

  ui_path <- file.path(app_dir, "ynow_ui.R")
  ui <- readLines(ui_path, warn = FALSE, encoding = "UTF-8")
  ui_new <- ynow_replace_label(ui, old_label, new_label)
  if (!identical(ui, ui_new)) {
    ynow_write_lines(ui_path, ui_new)
    changed <- c(changed, ui_path)
  }

  rmd_path <- file.path(app_dir, "report_template.Rmd")
  if (file.exists(rmd_path)) {
    rmd <- readLines(rmd_path, warn = FALSE, encoding = "UTF-8")
    rmd_new <- rmd
    idx <- grep("^\\s*app_version:", rmd)
    if (length(idx)) {
      rmd_new[idx] <- ynow_replace_label(rmd[idx], old_label, new_label)
    }
    if (!identical(rmd, rmd_new)) {
      ynow_write_lines(rmd_path, rmd_new)
      changed <- c(changed, rmd_path)
    }
  }

  readme_path <- file.path(app_dir, "README.md")
  if (file.exists(readme_path)) {
    readme <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")
    if (length(readme) && grepl(old_label, readme[[1]], fixed = TRUE)) {
      readme[[1]] <- ynow_replace_label(readme[[1]], old_label, new_label)
      ynow_write_lines(readme_path, readme)
      changed <- c(changed, readme_path)
    }
  }

  json_path <- file.path(app_dir, "www", "ynow_build.json")
  if (file.exists(json_path)) {
    build <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
    build$version <- new_label
    jsonlite::write_json(build, json_path, auto_unbox = TRUE, pretty = TRUE)
    changed <- c(changed, json_path)
  }

  test_path <- file.path(app_dir, "tests", "test_lite_mode_ui.R")
  if (file.exists(test_path)) {
    test_lines <- readLines(test_path, warn = FALSE, encoding = "UTF-8")
    test_new <- ynow_replace_label(test_lines, old_label, new_label)
    if (!identical(test_lines, test_new)) {
      ynow_write_lines(test_path, test_new)
      changed <- c(changed, test_path)
    }
  }

  invisible(unique(changed))
}

ynow_sync_build_json <- function(app_dir, label) {
  json_path <- file.path(app_dir, "www", "ynow_build.json")
  if (!file.exists(json_path)) return(invisible(FALSE))
  build <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
  if (identical(as.character(build$version)[1], label)) return(invisible(FALSE))
  build$version <- label
  jsonlite::write_json(build, json_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

ynow_git_commit_paths <- function(root, paths, message, must = TRUE) {
  if (identical(Sys.getenv("YNOW_VERSION_BUMP_NO_COMMIT"), "1")) {
    return(invisible(FALSE))
  }
  if (!length(paths)) return(invisible(FALSE))
  repo <- tryCatch(
    system2("git", c("-C", root, "rev-parse", "--is-inside-work-tree"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (!length(repo) || !identical(repo[[1]], "true")) return(invisible(FALSE))

  rel <- substring(normalizePath(paths, mustWork = TRUE), nchar(normalizePath(root, mustWork = TRUE)) + 2L)
  # A host git wrapper splits an unquoted -m message on spaces (":" starts a new token).
  msg_file <- tempfile(pattern = "ynow-commit-", fileext = ".txt")
  on.exit(unlink(msg_file), add = TRUE)
  ynow_write_lines(msg_file, message)
  status <- system2("git", c("-C", root, "commit", "-F", msg_file, "--", rel))
  if (!identical(status, 0L)) {
    if (must) stop("git commit failed (status ", status, "): ", message)
    message("git commit failed (status ", status, "): ", message)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

ynow_commit_version_bump <- function(root, paths, new_label) {
  ynow_git_commit_paths(
    root, paths,
    paste0("chore: bump display version to ", new_label),
    must = TRUE
  )
}

ynow_record_shipped_version <- function(root, version) {
  path <- file.path(root, "scripts", "DEPLOY_BASELINE.txt")
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  repl <- paste0("display_version=", version)
  idx <- grep("^display_version=", lines)
  if (length(idx)) {
    if (identical(lines[[idx[[1]]]], repl)) return(invisible(FALSE))
    lines[[idx[[1]]]] <- repl
  } else {
    lines <- c(lines, repl)
  }
  ynow_write_lines(path, lines)
  ynow_git_commit_paths(
    root, path,
    paste0("chore: record shipped display version ", version),
    must = FALSE
  )
  invisible(TRUE)
}

ynow_prepare_deploy_version <- function(root, commit = TRUE) {
  app_dir <- file.path(root, "app_19.0")
  current <- ynow_read_display_version(app_dir)
  baseline <- ynow_read_baseline_version(root)
  pending <- !is.na(baseline) && nzchar(baseline) && identical(current, ynow_bump_display_label(baseline))
  if (pending) {
    message(
      "Display version ", current,
      " is already +0.01 ahead of deployed baseline ", baseline,
      ". This deploy ships that bump and does not add another 0.01."
    )
    if (isTRUE(ynow_sync_build_json(app_dir, current))) {
      message("Synced www/ynow_build.json to ", current)
    }
    return(list(version = current, previous = current, bumped = FALSE))
  }
  new_label <- ynow_bump_display_label(current)
  changed <- ynow_apply_display_version(root, current, new_label)
  message("Display version ", current, " -> ", new_label)
  if (commit && length(changed)) {
    ynow_commit_version_bump(root, changed, new_label)
  }
  list(version = new_label, previous = current, bumped = TRUE, changed = changed)
}

ynow_bump_cli <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  dry <- "--dry-run" %in% args
  root <- ynow_repo_root_from_script()
  app_dir <- file.path(root, "app_19.0")
  current <- ynow_read_display_version(app_dir)
  baseline <- ynow_read_baseline_version(root)
  new_label <- ynow_bump_display_label(current)
  message("root: ", root)
  message("current: ", current)
  message("baseline: ", if (is.na(baseline)) "(none)" else baseline)
  message("next: ", new_label)
  if (dry) {
    message("dry-run: no files written")
    return(invisible(new_label))
  }
  ynow_prepare_deploy_version(root, commit = TRUE)
}

if (sys.nframe() == 0L) {
  ynow_bump_cli()
}
