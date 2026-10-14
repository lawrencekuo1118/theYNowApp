#!/usr/bin/env Rscript
# Acceptance — chromote full-app browser E2E (closes P1D-NO-SHINYTEST deferral)
# Starts app_24.0 Shiny, drives Chrome via CDP (dedicated headless profile/port).
#
# Run: cd app_24.0 && Rscript tests/test_audit_chromote_e2e.R
# Env: YNOW_CHROMOTE_TICKER=AAPL (default)
#      YNOW_CHROMOTE_ARTIFACT_DIR=/opt/cursor/artifacts/chromote_e2e

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else if (dir.exists("tests") && file.exists("app.R")) {
  normalizePath("tests")
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
setwd(app_dir)

ticker <- toupper(trimws(Sys.getenv("YNOW_CHROMOTE_TICKER", unset = "AAPL")))
artifact_dir <- Sys.getenv(
  "YNOW_CHROMOTE_ARTIFACT_DIR",
  unset = "/opt/cursor/artifacts/chromote_e2e"
)
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

fail <- 0L
pass <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("PASS ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

need_pkgs <- c("chromote", "callr", "httpuv", "jsonlite", "websocket")
for (p in need_pkgs) {
  check(paste0("package ", p), requireNamespace(p, quietly = TRUE))
}
if (fail > 0L) {
  cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
  quit(status = 1L)
}

chrome_bin <- Sys.getenv("CHROMOTE_CHROME", unset = "")
if (!nzchar(chrome_bin)) {
  chrome_bin <- tryCatch(chromote::find_chrome(), error = function(e) "")
}
if (!nzchar(chrome_bin) || !file.exists(chrome_bin)) {
  chrome_bin <- "/usr/local/bin/google-chrome"
}
check("chrome binary exists", file.exists(chrome_bin))

# --- helpers ---------------------------------------------------------------
.find_free_port <- function() {
  httpuv::randomPort()
}

.wait_http <- function(url, timeout_sec = 120) {
  deadline <- Sys.time() + timeout_sec
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      con <- url(url, open = "rb", timeout = 2)
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(0.5)
  }
  FALSE
}

.wait_chrome_debug <- function(port, timeout_sec = 30) {
  deadline <- Sys.time() + timeout_sec
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      u <- sprintf("http://127.0.0.1:%d/json/version", as.integer(port))
      con <- url(u, open = "rb", timeout = 1)
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}

.js_eval <- function(session, expr) {
  session$Runtime$evaluate(expr, returnByValue = TRUE)$result$value
}

.js_await <- function(session, expr, timeout_sec = 60, interval = 0.5) {
  deadline <- Sys.time() + timeout_sec
  last <- NULL
  while (Sys.time() < deadline) {
    last <- tryCatch(.js_eval(session, expr), error = function(e) NULL)
    if (isTRUE(last) || (is.character(last) && nzchar(last)) ||
        (is.numeric(last) && is.finite(last) && last > 0)) {
      return(last)
    }
    Sys.sleep(interval)
  }
  last
}

.screenshot <- function(session, path) {
  png <- session$Page$captureScreenshot(format = "png", fromSurface = TRUE)
  raw <- jsonlite::base64_dec(png$data)
  writeBin(raw, path)
  file.exists(path) && file.info(path)$size > 1000
}

# --- start Shiny -----------------------------------------------------------
shiny_port <- .find_free_port()
shiny_url <- sprintf("http://127.0.0.1:%d/", shiny_port)
cat("INFO shiny_url=", shiny_url, "\n", sep = "")

shiny_proc <- callr::r_bg(
  function(app_dir, port) {
    setwd(app_dir)
    # Prefer venv for live Yahoo during E2E
    venv <- file.path(app_dir, ".ynow_venv", "bin", "python")
    if (file.exists(venv) && !nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
      Sys.setenv(RETICULATE_PYTHON = venv)
    }
    Sys.setenv(YNOW_DEBUG_SKIP_PY = "0")
    shiny::runApp(appDir = app_dir, host = "127.0.0.1", port = port, launch.browser = FALSE)
  },
  args = list(app_dir, shiny_port),
  stdout = file.path(artifact_dir, "shiny_stdout.log"),
  stderr = file.path(artifact_dir, "shiny_stderr.log"),
  supervise = TRUE
)
on.exit({
  try(shiny_proc$kill(), silent = TRUE)
}, add = TRUE)

shiny_up <- .wait_http(shiny_url, timeout_sec = 180)
check("shiny listening", isTRUE(shiny_up))
if (!isTRUE(shiny_up)) {
  cat("INFO shiny stderr tail:\n")
  try(cat(paste(utils::tail(readLines(file.path(artifact_dir, "shiny_stderr.log"), warn = FALSE), 40), collapse = "\n"), "\n"), silent = TRUE)
  cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
  quit(status = 1L)
}

# --- start dedicated headless Chrome --------------------------------------
chrome_port <- .find_free_port()
chrome_profile <- tempfile("ynow-chromote-profile-")
dir.create(chrome_profile, recursive = TRUE, showWarnings = FALSE)
chrome_log <- file.path(artifact_dir, "chrome_stderr.log")
chrome_args <- c(
  "--headless=new",
  "--no-sandbox",
  "--disable-gpu",
  "--disable-dev-shm-usage",
  "--mute-audio",
  "--disable-extensions",
  paste0("--user-data-dir=", chrome_profile),
  paste0("--remote-debugging-port=", chrome_port),
  "about:blank"
)
chrome_cmd <- paste(c(
  shQuote(chrome_bin),
  vapply(chrome_args, shQuote, character(1)),
  ">", shQuote(chrome_log), "2>&1", "&",
  "echo $!"
), collapse = " ")
chrome_pid <- suppressWarnings(as.integer(system(chrome_cmd, intern = TRUE)[1]))
on.exit({
  if (is.finite(as.numeric(chrome_pid)[1])) {
    try(tools::pskill(as.integer(chrome_pid)[1]), silent = TRUE)
  }
}, add = TRUE)

chrome_up <- .wait_chrome_debug(chrome_port, timeout_sec = 30)
check("chrome remote debug up", isTRUE(chrome_up))
if (!isTRUE(chrome_up)) {
  cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
  quit(status = 1L)
}

b <- chromote::Chromote$new(
  browser = chromote::ChromeRemote$new(host = "127.0.0.1", port = as.integer(chrome_port))
)
s <- b$new_session()
on.exit({
  try(s$close(), silent = TRUE)
  try(b$close(), silent = TRUE)
}, add = TRUE)

# --- drive UI --------------------------------------------------------------
s$Page$navigate(shiny_url)
s$Page$loadEventFired(timeout_ = 60)
Sys.sleep(2)

home_png <- file.path(artifact_dir, "01_home.png")
check("screenshot home", .screenshot(s, home_png))

has_sc <- .js_await(
  s,
  "!!document.getElementById('sc') && !!document.getElementById('search')",
  timeout_sec = 90
)
check("search controls present", isTRUE(has_sc))

# Fill ticker and search
.js_eval(
  s,
  sprintf(
    paste0(
      "(function(){",
      "var el=document.getElementById('sc');",
      "el.value=%s;",
      "el.dispatchEvent(new Event('input',{bubbles:true}));",
      "el.dispatchEvent(new Event('change',{bubbles:true}));",
      "Shiny.setInputValue('sc', %s, {priority:'event'});",
      "document.getElementById('search').click();",
      "return true;",
      "})()"
    ),
    jsonlite::toJSON(ticker, auto_unbox = TRUE),
    jsonlite::toJSON(ticker, auto_unbox = TRUE)
  )
)

corp_ok <- .js_await(
  s,
  sprintf(
    paste0(
      "(function(){",
      "var t=(document.body && document.body.innerText) || '';",
      "return t.indexOf(%s)>=0 || t.indexOf('Apple')>=0 || t.indexOf('Fair')>=0 || t.indexOf('WACC')>=0;",
      "})()"
    ),
    jsonlite::toJSON(ticker, auto_unbox = TRUE)
  ),
  timeout_sec = 120
)
check(paste0("page shows ticker/valuation after search (", ticker, ")"), isTRUE(corp_ok))

after_png <- file.path(artifact_dir, "02_after_search.png")
check("screenshot after search", .screenshot(s, after_png))

# Try to open DCF tab via sidebar / menu text when present
.js_eval(
  s,
  paste0(
    "(function(){",
    "var nodes=Array.from(document.querySelectorAll('a,button,li,span,div'));",
    "var n=nodes.find(function(x){var t=(x.textContent||'').trim(); return /^DCF\\b/i.test(t) || t==='DCF';});",
    "if(n){n.click(); return true;} return false;",
    "})()"
  )
)
Sys.sleep(2)
dcf_png <- file.path(artifact_dir, "03_dcf_tab.png")
check("screenshot dcf/nav attempt", .screenshot(s, dcf_png))

body_len <- .js_eval(s, "(document.body && document.body.innerText || '').length")
check("body has substantial text", is.finite(as.numeric(body_len)[1]) && as.numeric(body_len)[1] > 200)

# Write a small evidence note
writeLines(
  c(
    paste0("ticker=", ticker),
    paste0("shiny_url=", shiny_url),
    paste0("chrome_port=", chrome_port),
    paste0("home=", home_png),
    paste0("after_search=", after_png),
    paste0("dcf=", dcf_png),
    paste0("PASS=", pass, " FAIL=", fail)
  ),
  file.path(artifact_dir, "e2e_summary.txt")
)

cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
quit(status = 0L)
