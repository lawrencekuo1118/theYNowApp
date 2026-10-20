# C-STRUCT-02 seam 2: cookie notice markup/JS live in ynow_cookie_notice_ui.R
# (sourced by ynow_ui.R). CSS + applyUiLocale setBtText stay on the facade.
# Run: cd app_25.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_ynow_cookie_notice_ui.R
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("[PASS] %s\n", label))
  } else {
    cat(sprintf("[FAIL] %s\n", label))
    fail <<- fail + 1L
  }
}

facade_path <- file.path(app_dir, "ynow_ui.R")
sibling_path <- file.path(app_dir, "ynow_cookie_notice_ui.R")
check("ynow_cookie_notice_ui.R exists", file.exists(sibling_path))
check("ynow_ui.R exists", file.exists(facade_path))

facade <- paste(readLines(facade_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sibling <- paste(readLines(sibling_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check(
  "facade sources cookie sibling",
  grepl(
    'source("ynow_cookie_notice_ui.R", local = TRUE, encoding = "UTF-8")',
    facade,
    fixed = TRUE
  )
)
check(
  "facade mounts .ynow_cookie_notice_ui()",
  grepl(".ynow_cookie_notice_ui()", facade, fixed = TRUE)
)
check(
  "facade does not redefine cookie builder",
  !grepl(".ynow_cookie_notice_ui <- function()", facade, fixed = TRUE)
)
check(
  "sibling defines .ynow_cookie_notice_ui",
  grepl(".ynow_cookie_notice_ui <- function()", sibling, fixed = TRUE)
)

# Markup + prefs JS moved to sibling; CSS + locale apply stay on facade.
check("sibling banner id", grepl('id = "ynow_cookie_banner"', sibling, fixed = TRUE))
check("sibling settings btn", grepl('id = "ynow_cookie_settings_btn"', sibling, fixed = TRUE))
check(
  "sibling necessary locked",
  grepl('id = "ynow_cookie_necessary"', sibling, fixed = TRUE) &&
    grepl('disabled = "disabled"', sibling, fixed = TRUE)
)
check("sibling analytics toggle", grepl('id = "ynow_cookie_analytics"', sibling, fixed = TRUE))
check("sibling preferences toggle", grepl('id = "ynow_cookie_preferences"', sibling, fixed = TRUE))
check("sibling reopen control", grepl('id = "ynow_cookie_reopen"', sibling, fixed = TRUE))
check("sibling persist localStorage", grepl("localStorage.setItem(KEY, raw)", sibling, fixed = TRUE))
check("sibling persist cookie", grepl("document.cookie = KEY", sibling, fixed = TRUE))
check(
  "sibling has no login gate on banner",
  !grepl("ynow_cookie_banner[\\s\\S]{0,400}ynow-menu-lock", sibling, perl = TRUE)
)

check(
  "facade keeps cookie CSS",
  grepl(".ynow-cookie-banner", facade, fixed = TRUE) &&
    grepl(".ynow-cookie-reopen", facade, fixed = TRUE)
)
check(
  "facade keeps applyUiLocale cookie setBtText",
  grepl("setBtText('ynow_cookie_banner_body', 'cookie_banner_body')", facade, fixed = TRUE)
)
check(
  "facade no longer embeds banner markup id",
  !grepl('id = "ynow_cookie_banner"', facade, fixed = TRUE)
)

parse_ok <- tryCatch({
  parse(sibling_path)
  TRUE
}, error = function(e) {
  cat("parse error:", conditionMessage(e), "\n")
  FALSE
})
check("ynow_cookie_notice_ui.R parses", parse_ok)

# Source into an isolated env (htmltools/shiny stubs for structure only).
env <- new.env(parent = baseenv())
env$tags <- list(
  div = function(...) structure(list(...), class = "shiny.tag"),
  p = function(...) structure(list(...), class = "shiny.tag"),
  button = function(...) structure(list(...), class = "shiny.tag"),
  label = function(...) structure(list(...), class = "shiny.tag"),
  input = function(...) structure(list(...), class = "shiny.tag"),
  span = function(...) structure(list(...), class = "shiny.tag"),
  script = function(...) structure(list(...), class = "shiny.tag")
)
env$tagList <- function(...) structure(list(...), class = c("shiny.tag.list", "list"))
env$HTML <- function(x) x
env$icon <- function(...) structure(list(...), class = "shiny.tag")
src_ok <- tryCatch({
  source(sibling_path, local = env, encoding = "UTF-8")
  is.function(env$.ynow_cookie_notice_ui)
}, error = function(e) {
  cat("source error:", conditionMessage(e), "\n")
  FALSE
})
check("sibling sources and exports builder", src_ok)

if (isTRUE(src_ok)) {
  built <- tryCatch(env$.ynow_cookie_notice_ui(), error = function(e) {
    cat("build error:", conditionMessage(e), "\n")
    NULL
  })
  check("builder returns tagList", !is.null(built) && inherits(built, "shiny.tag.list"))
}

if (fail > 0L) {
  cat(sprintf("FAILED %d\n", fail))
  quit(status = 1)
}
cat("ALL PASS\n")
