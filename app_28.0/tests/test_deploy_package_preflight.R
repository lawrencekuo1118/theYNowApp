#!/usr/bin/env Rscript
# Regression: tip-ship / deploy must install the full boot package set before
# rsconnect::deployApp(). Missing local packages are omitted from the shinyapps
# manifest in non-interactive CI → Listen exit status 1 (Tip-ship #3 / 12867372).
suppressPackageStartupMessages({
  app_root <- if (file.exists("global.R")) {
    normalizePath(".")
  } else if (file.exists("../global.R")) {
    normalizePath("..")
  } else {
    normalizePath(file.path("..", "app_28.0"))
  }
  repo_root <- normalizePath(file.path(app_root, ".."))
})

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

req_src <- file.path(repo_root, "scripts", "ynow_deploy_required_packages.R")
install_src <- file.path(repo_root, "scripts", "install_deploy_app_packages.R")
deploy_src <- file.path(repo_root, "scripts", "deploy_app_21.R")
tip_ship <- file.path(repo_root, ".github", "workflows", "tip-ship.yml")
docs_tip <- file.path(repo_root, "docs", "cloud-tip-ship.md")

check("ynow_deploy_required_packages.R exists", file.exists(req_src))
check("install_deploy_app_packages.R exists", file.exists(install_src))
check("tip-ship.yml exists", file.exists(tip_ship))
check("docs/cloud-tip-ship.md exists", file.exists(docs_tip))

source(req_src, local = TRUE)
pkgs <- ynow_deploy_required_packages()
crit <- ynow_deploy_boot_critical_packages()

check("required package list is non-empty", length(pkgs) >= 20L)
check(
  "boot-critical includes shiny + shinydashboard + pacman",
  all(c("shiny", "shinydashboard", "pacman") %in% crit)
)
check(
  "required includes shiny UI stack",
  all(c(
    "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "DT", "plotly",
    "tidyverse", "dplyr", "reticulate", "RPostgres", "pool", "sodium"
  ) %in% pkgs)
)
check(
  "pagedown is installed for snapshot but not a boot-critical attach gate",
  "pagedown" %in% pkgs && !"pagedown" %in% crit
)

glb <- paste(readLines(file.path(app_root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# Extract bare names inside pacman::p_load(...)
no_comments <- gsub("(?m)^\\s*#.*$", "", glb, perl = TRUE)
no_comments <- gsub("#[^\n]*", "", no_comments)
m <- regexpr("pacman::p_load\\(", no_comments, perl = TRUE)
pload_names <- character(0)
if (m[1] >= 0L) {
  start <- as.integer(m[1])
  depth <- 0L
  end <- start
  chars <- strsplit(no_comments, "", fixed = TRUE)[[1]]
  i <- start
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (identical(ch, "(")) depth <- depth + 1L
    if (identical(ch, ")")) {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i
        break
      }
    }
    i <- i + 1L
  }
  block <- paste(chars[start:end], collapse = "")
  # Tokens that look like package names (skip pacman::p_load itself)
  toks <- unique(unlist(regmatches(
    block,
    gregexpr("\\b[A-Za-z][A-Za-z0-9.]*\\b", block)
  )))
  skip <- c("pacman", "p_load")
  pload_names <- setdiff(toks, skip)
}

check(
  "every global.R p_load package is in deploy required list",
  length(pload_names) > 0L && all(pload_names %in% pkgs)
)

deploy_txt <- paste(readLines(deploy_src, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
tip_txt <- paste(readLines(tip_ship, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "deploy_app_21.R sources ynow_deploy_required_packages.R",
  grepl("ynow_deploy_required_packages\\.R", deploy_txt)
)
check(
  "deploy_app_21.R fails on missing required packages",
  grepl("ynow_deploy_missing_packages", deploy_txt) &&
    grepl("install_deploy_app_packages\\.R", deploy_txt)
)
check(
  "tip-ship.yml runs install_deploy_app_packages.R",
  grepl("install_deploy_app_packages\\.R", tip_txt)
)
check(
  "tip-ship.yml no longer installs only the thin Neon/rsconnect set",
  !grepl(
    'pkgs <- c\\("rsconnect","curl","RPostgres","pool","sodium","future","DBI","reticulate"\\)',
    tip_txt
  )
)
land_src <- file.path(repo_root, "scripts", "tip_ship_land_commits.sh")
check("tip_ship_land_commits.sh exists", file.exists(land_src))
check(
  "tip-ship.yml lands bump/baseline via tip_ship_land_commits.sh (no direct master push)",
  grepl("tip_ship_land_commits\\.sh", tip_txt) &&
    !grepl("git push origin \"HEAD:", tip_txt, fixed = TRUE)
)
check(
  "tip-ship.yml does not require TIP_SHIP_GH_PAT; Release after land merge",
  !grepl("secrets\\.TIP_SHIP_GH_PAT", tip_txt) &&
    grepl("Publish GitHub Release \\(after land merge\\)", tip_txt) &&
    grepl("steps\\.land\\.outputs\\.merged_sha", tip_txt)
)
land_txt <- paste(readLines(land_src, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "land script: GITHUB_TOKEN PR + CI dispatch + auto-merge wait (no PAT)",
  grepl("gh pr create", land_txt, fixed = TRUE) &&
    grepl("gh workflow run ci.yml", land_txt, fixed = TRUE) &&
    grepl("gh pr merge", land_txt, fixed = TRUE) &&
    grepl("--auto", land_txt, fixed = TRUE) &&
    grepl("MERGED", land_txt, fixed = TRUE) &&
    grepl("tip-ship/baseline-", land_txt, fixed = TRUE) &&
    grepl("superseded", land_txt) &&
    grepl("Resource not accessible by integration", land_txt, fixed = TRUE) &&
    grepl('git push origin "HEAD:refs/heads/', land_txt, fixed = TRUE) &&
    !grepl("(?m)^[[:space:]]*git push -u", land_txt, perl = TRUE) &&
    !grepl("secrets\\.TIP_SHIP_GH_PAT", land_txt) &&
    !grepl("TIP_SHIP_GH_PAT:-", land_txt, fixed = TRUE)
)
docs_txt <- paste(readLines(docs_tip, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
check(
  "docs: GITHUB_TOKEN land PR + auto-merge; no PAT; no ruleset bypass; Actions create-PR setting",
  grepl("tip_ship_land_commits\\.sh", docs_txt) &&
    grepl("GITHUB_TOKEN", docs_txt, fixed = TRUE) &&
    grepl("auto-merge", docs_txt) &&
    grepl("Allow GitHub Actions to create and approve pull requests", docs_txt, fixed = TRUE) &&
    grepl("Do \\*\\*not\\*\\* add `TIP_SHIP_GH_PAT`", docs_txt) &&
    grepl("Do \\*\\*not\\*\\* require `github-actions\\[bot\\]` ruleset bypass", docs_txt) &&
    !grepl("| `TIP_SHIP_GH_PAT` | **yes**", docs_txt, fixed = TRUE)
)
install_txt <- paste(
  readLines(install_src, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
check(
  "installer prefers getOption('repos') / RSPM (no hardcoded cloud.r-project.org)",
  grepl("getOption\\(\"repos\"", install_txt) &&
    !grepl('repos = "https://cloud.r-project.org"', install_txt, fixed = TRUE)
)

cat("\nPASS ", pass, " / FAIL ", fail, "\n", sep = "")
quit(status = if (fail > 0L) 1L else 0L)
