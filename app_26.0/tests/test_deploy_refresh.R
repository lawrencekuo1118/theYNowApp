# test_deploy_refresh.R — deploy refresh: flame click only; major must-click reminder
#
# Run: cd app_26.0/tests && YNOW_DEBUG_SKIP_PY=1 Rscript test_deploy_refresh.R

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
check("build note documents never force-logout", grepl("force-logout", build_raw, ignore.case = TRUE) ||
  grepl("force logout", build_raw, ignore.case = TRUE))
check("build note documents major reminder", grepl("Major", build_raw, fixed = TRUE) &&
  grepl("signed-in", build_raw, ignore.case = TRUE))

check("UI reads build version helper", grepl(".ynow_read_build_version", ui, fixed = TRUE) &&
  grepl(".YNOW_BUILD_VERSION", ui, fixed = TRUE))
check("title has data-ynow-build", grepl('data-ynow-build="', ui, fixed = TRUE) &&
  grepl(".YNOW_BUILD_VERSION", ui, fixed = TRUE))
check("R major helpers present", grepl(".ynow_display_major", ui, fixed = TRUE) &&
  grepl(".ynow_is_major_display_bump", ui, fixed = TRUE))

check("deploy refresh poller present", grepl("initYnowDeployRefresh", ui, fixed = TRUE))
check("polls ynow_build.json", grepl("ynow_build.json", ui, fixed = TRUE))
check("sets ynow-update-available class", grepl("ynow-update-available", ui, fixed = TRUE))
check("sets ynow-update-major class", grepl("ynow-update-major", ui, fixed = TRUE))
check("JS major parser matches display major", grepl("function displayMajor", ui, fixed = TRUE) &&
  grepl("function isMajorBump", ui, fixed = TRUE) &&
  grepl("ynowIsMajorDisplayBump", ui, fixed = TRUE))
check("major reminder for signed-in only", {
  grepl("ynow-authed.ynow-update-major", ui, fixed = TRUE) &&
    grepl("ynow_major_update_reminder", ui, fixed = TRUE) &&
    grepl("isSignedIn", ui, fixed = TRUE) &&
    grepl("classList.contains('ynow-authed')", ui, fixed = TRUE)
})
check("auth observer re-applies major chrome", {
  grepl("bindAuthObserver", ui, fixed = TRUE) &&
    grepl("MutationObserver", ui, fixed = TRUE) &&
    grepl("attributeFilter: ['class']", ui, fixed = TRUE)
})
check("reminder does not auto-reload", {
  grepl("do not reload here", ui, fixed = TRUE) ||
    grepl("Refresh stays on flame title click", ui, ignore.case = TRUE) ||
    grepl("refresh stays on flame title click", ui, ignore.case = TRUE)
})
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
  grepl("Pending deploy: flame title must override gold", ui, fixed = TRUE) &&
  grepl("body.ynow-update-available .main-header .logo .ynow-app-title.is-complete .ynow-app-title-fill-inner", ui, fixed = TRUE) &&
  grepl("--ynow-htcdi-flame-gradient", ui, fixed = TRUE) &&
  grepl("ynow-htcdi-flame-flow", ui, fixed = TRUE) &&
  !grepl("ynow-update-pulse", ui, fixed = TRUE))
check("header stays above major overlay", grepl(
  "body.ynow-authed.ynow-update-major .main-header",
  ui, fixed = TRUE
) && grepl("z-index: 11050", ui, fixed = TRUE))

check("en update_available_click", grepl(
  'update_available_click = "New version available — click The YNow App to refresh"',
  loc, fixed = TRUE
))
check("zh-TW update_available_click", grepl(
  'update_available_click = "有新版本 — 請點一下 The YNow App 重新整理"',
  loc, fixed = TRUE
))
check("en update_major strings", {
  grepl('update_major_click = "Major update — click The YNow App to refresh"', loc, fixed = TRUE) &&
    grepl('update_major_title = "Major update available"', loc, fixed = TRUE) &&
    grepl("update_major_body = paste0(", loc, fixed = TRUE) &&
    grepl("never auto-reload or force sign-out", loc, fixed = TRUE) &&
    grepl('update_major_hint = "Click the flaming The YNow App title to refresh"', loc, fixed = TRUE)
})
check("zh-TW update_major strings", {
  grepl('update_major_click = "重大版本更新 — 請點一下 The YNow App 重新整理"', loc, fixed = TRUE) &&
    grepl('update_major_title = "重大版本更新"', loc, fixed = TRUE) &&
    grepl("不會自動重新載入，也不會強制登出", loc, fixed = TRUE) &&
    grepl("請點選標題列會火焰動畫的 The YNow App 重新整理", loc, fixed = TRUE) &&
    !grepl("默认|参数|数据|用户|点击|一键", loc)
})
check("applyUiLocale wires update tip", grepl("ynowSetUpdateLocaleStrings", ui, fixed = TRUE))

# Runtime helpers
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(.root)
ver <- tryCatch({
  j <- jsonlite::fromJSON(file.path("www", "ynow_build.json"))
  as.character(j$version)[1]
}, error = function(e) NA_character_)
check("build version readable", is.character(ver) && grepl("^v[0-9]", ver))

# Eval major helpers from UI source (avoid sourcing full shiny UI)
.ynow_display_major <- function(label) {
  label <- as.character(label)[1]
  if (!is.character(label) || !nzchar(label) || is.na(label)) return(NA_integer_)
  m <- regmatches(label, regexpr("^[vV]?([0-9]+)", label))
  if (!length(m) || !nzchar(m[[1]])) return(NA_integer_)
  as.integer(sub("^[vV]?", "", m[[1]]))
}
.ynow_is_major_display_bump <- function(session_label, remote_label) {
  a <- .ynow_display_major(session_label)
  b <- .ynow_display_major(remote_label)
  is.finite(a) && is.finite(b) && as.integer(b) > as.integer(a)
}
# Keep local copies aligned with ynow_ui.R definitions (string presence already checked).
ui_major_fn <- regmatches(
  ui,
  regexpr("\\.ynow_display_major <- function\\(label\\) \\{[\\s\\S]*?\\n\\}", ui, perl = TRUE)
)
ui_bump_fn <- regmatches(
  ui,
  regexpr(
    "\\.ynow_is_major_display_bump <- function\\(session_label, remote_label\\) \\{[\\s\\S]*?\\n\\}",
    ui,
    perl = TRUE
  )
)
check("extracted major helpers", length(ui_major_fn) == 1L && length(ui_bump_fn) == 1L)
if (length(ui_major_fn) == 1L && length(ui_bump_fn) == 1L) {
  eval(parse(text = ui_major_fn[[1]]), envir = environment())
  eval(parse(text = ui_bump_fn[[1]]), envir = environment())
}
check("major v25.85 -> 25", identical(.ynow_display_major("v25.85"), 25L))
check("major v26.00 -> 26", identical(.ynow_display_major("v26.00"), 26L))
check("major v26 -> 26", identical(.ynow_display_major("v26"), 26L))
check("patch bump not major", isFALSE(.ynow_is_major_display_bump("v25.85", "v25.86")))
check("same version not major", isFALSE(.ynow_is_major_display_bump("v25.85", "v25.85")))
check("major bump v25 -> v26", isTRUE(.ynow_is_major_display_bump("v25.85", "v26.00")))
check("major bump v25.99 -> v26.00", isTRUE(.ynow_is_major_display_bump("v25.99", "v26.00")))
check("downgrade not major", isFALSE(.ynow_is_major_display_bump("v26.00", "v25.99")))

if (.fail > 0L) {
  cat("\n", .fail, " check(s) failed.\n", sep = "")
  quit(status = 1L)
}
cat("\nAll deploy-refresh checks passed.\n")
