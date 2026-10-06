# test_deploy_refresh.R — new deploy: click The YNow App title to refresh (no auto-reload)
#
# Run: cd app_21.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_deploy_refresh.R

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
options(warn = 1)

.args <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args, value = TRUE)
.script_dir <- if (length(.file_arg)) {
  dirname(normalizePath(sub("^--file=", "", .file_arg[[1]]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
.root <- normalizePath(file.path(.script_dir, ".."), mustWork = TRUE)
.fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "OK " else "FAIL ", label, "\n", sep = "")
  if (!ok) .fail <<- .fail + 1L
  invisible(ok)
}

ui_path <- file.path(.root, "ynow_ui.R")
loc_path <- file.path(.root, "ui_locale.R")
build_path <- file.path(.root, "www", "ynow_build.json")
ui <- paste(readLines(ui_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc <- paste(readLines(loc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
build_raw <- paste(readLines(build_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

check("ynow_build.json has version", grepl('"version"', build_raw, fixed = TRUE))
check("build note documents click-to-refresh", grepl("click", build_raw, ignore.case = TRUE) &&
  grepl("never auto-reload", build_raw, ignore.case = TRUE))

check("UI reads build version helper", grepl(".ynow_read_build_version", ui, fixed = TRUE) &&
  grepl(".YNOW_BUILD_VERSION", ui, fixed = TRUE))
check("title has data-ynow-build", grepl('data-ynow-build="', ui, fixed = TRUE) &&
  grepl(".YNOW_BUILD_VERSION", ui, fixed = TRUE))

check("deploy refresh poller present", grepl("initYnowDeployRefresh", ui, fixed = TRUE))
check("polls ynow_build.json", grepl("ynow_build.json", ui, fixed = TRUE))
check("sets ynow-update-available class", grepl("ynow-update-available", ui, fixed = TRUE))
check("click reloads only when update available", {
  grepl("ynow-update-available", ui, fixed = TRUE) &&
    grepl("reloadForUpdate", ui, fixed = TRUE) &&
    grepl("onTitleActivate", ui, fixed = TRUE) &&
    grepl("!document.body.classList.contains('ynow-update-available')", ui, fixed = TRUE)
})
check("no auto-reload on poll success path", {
  # fetchBuild must not call reloadForUpdate; only click/keydown paths do.
  m <- regexpr("function fetchBuild\\(\\) \\{[\\s\\S]*?\\n\\s*\\}", ui, perl = TRUE)
  chunk <- if (m[1] < 0) "" else regmatches(ui, m)[[1]]
  nzchar(chunk) &&
    !grepl("reloadForUpdate", chunk, fixed = TRUE) &&
    !grepl("location.reload", chunk, fixed = TRUE) &&
    !grepl("location.replace", chunk, fixed = TRUE)
})
check("poll on shiny reconnect without auto-reload", {
  grepl("shiny:connected shiny:reconnected", ui, fixed = TRUE) &&
    grepl("still no auto-reload", ui, fixed = TRUE)
})
check("update chrome uses flame flow", grepl("ynow-update-available", ui, fixed = TRUE) &&
  grepl("body.ynow-update-available .main-header .logo .ynow-app-title-fill-inner", ui, fixed = TRUE) &&
  grepl("--ynow-htcdi-flame-gradient", ui, fixed = TRUE) &&
  grepl("ynow-htcdi-flame-flow", ui, fixed = TRUE) &&
  !grepl("ynow-update-pulse", ui, fixed = TRUE))

check("en update_available_click", grepl(
  'update_available_click = "New version available — click The YNow App to refresh"',
  loc, fixed = TRUE
))
check("zh-TW update_available_click", grepl(
  'update_available_click = "有新版本 — 請點一下 The YNow App 重新整理"',
  loc, fixed = TRUE
))
check("applyUiLocale wires update tip", grepl("ynowSetUpdateLocaleStrings", ui, fixed = TRUE))

# Runtime: helper resolves www/ynow_build.json when sourced from app root
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(.root)
# Extract and eval only the helper (avoid sourcing full UI / shiny)
helper_src <- paste(
  "ynow_read_build_version <- function() {",
  "  paths <- c(file.path(\"www\", \"ynow_build.json\"), \"ynow_build.json\")",
  "  for (p in paths) {",
  "    if (!file.exists(p)) next",
  "    raw <- paste(readLines(p, warn = FALSE, encoding = \"UTF-8\"), collapse = \"\\n\")",
  "    m <- regmatches(raw, regexpr('\\\"version\\\"\\\\s*:\\\\s*\\\"[^\\\"]+\\\"', raw))",
  "    if (length(m) == 1L && nzchar(m[[1]])) {",
  "      return(sub('^\\\"version\\\"\\\\s*:\\\\s*\\\"([^\\\"]+)\\\".*$', \"\\\\1\", m[[1]]))",
  "    }",
  "  }",
  "  \"missing\"",
  "}",
  sep = "\n"
)
# Prefer parsing from actual file helper via regex extract
block <- sub(
  "(?s).*?(\\.ynow_read_build_version <- function\\(\\) \\{.*?\\n\\})",
  "\\1",
  ui,
  perl = TRUE
)
# Fallback: jsonlite if available
ver <- tryCatch({
  j <- jsonlite::fromJSON(file.path("www", "ynow_build.json"))
  as.character(j$version)[1]
}, error = function(e) NA_character_)
check("build version readable", is.character(ver) && grepl("^v[0-9]", ver))

if (.fail > 0L) {
  cat("\n", .fail, " check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("\nAll deploy-refresh checks passed.\n")
