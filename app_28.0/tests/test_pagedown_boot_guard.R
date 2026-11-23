#!/usr/bin/env Rscript
# Regression: shinyapps boot must not search for / launch headless Chrome.
# pagedown stays off pacman::p_load; chrome_print only via render_report_pdf with
# bounded CDP retries (see setup_report.R).
suppressPackageStartupMessages({
  root <- if (file.exists("global.R")) {
    normalizePath(".")
  } else if (file.exists("../global.R")) {
    normalizePath("..")
  } else {
    normalizePath(file.path("..", "app_28.0"))
  }
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

glb <- paste(readLines(file.path(root, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
setup_rep <- paste(
  readLines(file.path(root, "setup_report.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
forced <- paste(
  readLines(file.path(root, "ynow_forced_deps.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

check(
  "global.R does not p_load pagedown on the boot path",
  {
    # Strip full-line and trailing comments, then require no bare pagedown token
    # inside pacman::p_load(...).
    no_comments <- gsub("(?m)^\\s*#.*$", "", glb, perl = TRUE)
    no_comments <- gsub("#[^\n]*", "", no_comments)
    m <- regexpr("pacman::p_load\\(", no_comments, perl = TRUE)
    if (m[1] < 0L) return(FALSE)
    start <- as.integer(m[1])
    # Match balanced (...) from p_load(
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
    !grepl("(^|[^A-Za-z0-9_.])pagedown([^A-Za-z0-9_.]|$)", block)
  }
)

check(
  "forced deps do not library/require pagedown at boot",
  !grepl("library\\(\\s*pagedown\\s*\\)", forced) &&
    !grepl("require\\(\\s*pagedown\\s*\\)", forced) &&
    !grepl("requireNamespace\\(\\s*[\"']pagedown[\"']", forced)
)

check(
  "setup_report defines Chrome resolve + hosted PDF helpers",
  grepl(".ynow_resolve_pagedown_chrome", setup_rep, fixed = TRUE) &&
    grepl(".ynow_hosted_connect_for_pdf", setup_rep, fixed = TRUE) &&
    grepl("pagedown.remote.maxattempts", setup_rep, fixed = TRUE)
)

check(
  "render_report_pdf passes an explicit browser and bounds CDP attempts",
  grepl("browser = browser", setup_rep, fixed = TRUE) &&
    grepl("pagedown.remote.maxattempts = if (hosted) 3L else 5L", setup_rep, fixed = TRUE)
)

# Runtime: helpers must fail fast without entering the 20-attempt loop when
# Chrome is forced missing.
old_wd <- getwd()
setwd(root)
on.exit(setwd(old_wd), add = TRUE)
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
suppressMessages(source(file.path(root, "setup.R"), local = TRUE, encoding = "UTF-8"))

check(
  "resolve helper returns empty when PAGEDOWN_CHROME points at a missing path",
  {
    old <- Sys.getenv("PAGEDOWN_CHROME", unset = NA_character_)
    on.exit({
      if (is.na(old)) Sys.unsetenv("PAGEDOWN_CHROME")
      else Sys.setenv(PAGEDOWN_CHROME = old)
    }, add = TRUE)
    Sys.setenv(PAGEDOWN_CHROME = file.path(tempdir(), "ynow-no-such-chrome-binary"))
    identical(.ynow_resolve_pagedown_chrome(), "")
  }
)

check(
  "render_report_pdf fails immediately when no browser (no 20-attempt message)",
  {
    old <- Sys.getenv("PAGEDOWN_CHROME", unset = NA_character_)
    on.exit({
      if (is.na(old)) Sys.unsetenv("PAGEDOWN_CHROME")
      else Sys.setenv(PAGEDOWN_CHROME = old)
    }, add = TRUE)
    Sys.setenv(PAGEDOWN_CHROME = file.path(tempdir(), "ynow-no-such-chrome-binary"))
    err <- tryCatch(
      render_report_pdf(tempfile(fileext = ".html"), tempfile(fileext = ".pdf")),
      error = function(e) conditionMessage(e)
    )
    is.character(err) &&
      grepl("No usable browser", err, fixed = TRUE) &&
      !grepl("after 20 attempts", err, fixed = TRUE)
  }
)

check(
  "hosted detector is true under FORCE_SHINYAPPS_PYTHON",
  {
    old <- Sys.getenv("FORCE_SHINYAPPS_PYTHON", unset = NA_character_)
    on.exit({
      if (is.na(old)) Sys.unsetenv("FORCE_SHINYAPPS_PYTHON")
      else Sys.setenv(FORCE_SHINYAPPS_PYTHON = old)
    }, add = TRUE)
    Sys.setenv(FORCE_SHINYAPPS_PYTHON = "1")
    isTRUE(.ynow_hosted_connect_for_pdf())
  }
)

# Boot smoke: sourcing global under hosted flags must not call find_chrome /
# chrome_print. Trace via a temporary override if pagedown is already loaded.
check(
  "hosted global.R source does not invoke pagedown::find_chrome",
  {
    called <- FALSE
    if (requireNamespace("pagedown", quietly = TRUE)) {
      ns <- asNamespace("pagedown")
      if (is.function(ns$find_chrome)) {
        unlockBinding("find_chrome", ns)
        orig <- ns$find_chrome
        assign("find_chrome", function(...) {
          called <<- TRUE
          orig(...)
        }, envir = ns)
        lockBinding("find_chrome", ns)
        on.exit({
          unlockBinding("find_chrome", ns)
          assign("find_chrome", orig, envir = ns)
          lockBinding("find_chrome", ns)
        }, add = TRUE)
      }
    }
    Sys.setenv(
      FORCE_SHINYAPPS_PYTHON = "1",
      YNOW_DEBUG_SKIP_PY = "1",
      UV_PYTHON_DOWNLOADS = "never"
    )
    ok <- tryCatch({
      env <- new.env(parent = globalenv())
      # Mirror app.R hosted detector before global.R reads it.
      env$.ynow_is_hosted_connect <- function() TRUE
      source(file.path(root, "global.R"), local = env, encoding = "UTF-8")
      TRUE
    }, error = function(e) {
      message("global.R source error: ", conditionMessage(e))
      FALSE
    })
    isTRUE(ok) && isFALSE(called)
  }
)

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1)
