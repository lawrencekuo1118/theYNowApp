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
# Prefer the real Chromium binary — some cloud wrappers inject a shared
# --remote-debugging-port=9222 / user-data-dir that fights dedicated profiles.
candidates <- c(
  chrome_bin,
  "/opt/google/chrome/chrome",
  "/usr/local/bin/google-chrome",
  tryCatch(chromote::find_chrome(), error = function(e) ""),
  "/usr/bin/google-chrome-stable"
)
chrome_bin <- candidates[nzchar(candidates) & file.exists(candidates)][1]
check("chrome binary exists", nzchar(chrome_bin %||% "") && file.exists(chrome_bin))
cat("INFO chrome_bin=", chrome_bin, "\n", sep = "")

# --- helpers ---------------------------------------------------------------
.find_free_port <- function() {
  httpuv::randomPort()
}

.port_open <- function(host, port) {
  port <- as.integer(port)[1]
  tryCatch({
    con <- socketConnection(host = host, port = port, open = "r", blocking = TRUE, timeout = 1)
    close(con)
    TRUE
  }, error = function(e) FALSE)
}

.wait_http <- function(url, timeout_sec = 180, proc = NULL) {
  # Prefer TCP listen check — shiny::runApp can be up before HTTP body is ready.
  host <- sub("^https?://([^:/]+).*$", "\\1", url)
  port <- suppressWarnings(as.integer(sub("^https?://[^:/]+:(\\d+).*$", "\\1", url)))
  if (!is.finite(port)) port <- 80L
  deadline <- Sys.time() + timeout_sec
  while (Sys.time() < deadline) {
    if (isTRUE(.port_open(host, port))) {
      http_ok <- tryCatch({
        if (nzchar(Sys.which("curl"))) {
          rc <- system2(
            "curl",
            c("-sf", "-o", "/dev/null", "-m", "3", "-w", "%{http_code}", url),
            stdout = TRUE, stderr = FALSE
          )
          code <- suppressWarnings(as.integer(rc[1]))
          is.finite(code) && code >= 200 && code < 500
        } else {
          TRUE
        }
      }, error = function(e) TRUE)
      if (isTRUE(http_ok)) return(TRUE)
    }
    if (!is.null(proc) && !isTRUE(proc$is_alive())) return(FALSE)
    Sys.sleep(0.5)
  }
  FALSE
}

.wait_chrome_debug <- function(port, timeout_sec = 45) {
  deadline <- Sys.time() + timeout_sec
  u <- sprintf("http://127.0.0.1:%d/json/version", as.integer(port))
  while (Sys.time() < deadline) {
    ok <- tryCatch({
      if (nzchar(Sys.which("curl"))) {
        rc <- system2(
          "curl",
          c("-sf", "-m", "2", u),
          stdout = TRUE, stderr = FALSE
        )
        length(rc) > 0 && any(grepl("webSocketDebuggerUrl|Browser", rc))
      } else {
        isTRUE(.port_open("127.0.0.1", port))
      }
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

.screenshot <- function(session, path, tries = 3L) {
  for (i in seq_len(max(1L, as.integer(tries)[1]))) {
    ok <- tryCatch({
      png <- session$Page$captureScreenshot(format = "png", fromSurface = TRUE)
      raw <- jsonlite::base64_dec(png$data)
      writeBin(raw, path)
      file.exists(path) && isTRUE(file.info(path)$size > 1000)
    }, error = function(e) {
      cat("INFO screenshot error: ", conditionMessage(e), "\n", sep = "")
      FALSE
    })
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(0.4)
  }
  FALSE
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

shiny_up <- .wait_http(shiny_url, timeout_sec = 240, proc = shiny_proc)
check("shiny listening", isTRUE(shiny_up))
if (!isTRUE(shiny_up)) {
  cat("INFO shiny alive=", tryCatch(shiny_proc$is_alive(), error = function(e) NA), "\n")
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
home_brand <- .js_eval(
  s,
  "!!document.querySelector('.ynow-home') && ((document.body.innerText||'').indexOf('YNow')>=0)"
)
check("home brand visible", isTRUE(home_brand))

# Leave landing via Company card → dashboard (exposes #sc / #search)
clicked_company <- .js_eval(
  s,
  paste0(
    "(function(){",
    "var card=document.querySelector('.ynow-home-card[data-tab=\"dashboard\"]');",
    "if(!card) return false;",
    "card.click();",
    "return true;",
    "})()"
  )
)
check("clicked Company home card", isTRUE(clicked_company))
Sys.sleep(1.5)

has_sc <- .js_await(
  s,
  paste0(
    "(function(){",
    "var el=document.getElementById('sc');",
    "if(!el) return false;",
    "var st=window.getComputedStyle(el);",
    "return st && st.visibility!=='hidden' && st.display!=='none';",
    "})()"
  ),
  timeout_sec = 90
)
check("search controls present on Company tab", isTRUE(has_sc))
company_png <- file.path(artifact_dir, "02_company_tab.png")
check("screenshot company tab", .screenshot(s, company_png))

# Fill ticker and search (must not treat home-card 'Fair value' copy as success)
filled <- .js_eval(
  s,
  sprintf(
    paste0(
      "(function(){",
      "var el=document.getElementById('sc');",
      "if(!el) return false;",
      "el.focus();",
      "el.value='';",
      "el.value=%s;",
      "['input','keyup','change'].forEach(function(ev){",
      "  el.dispatchEvent(new Event(ev,{bubbles:true}));",
      "});",
      "if(window.jQuery){ try { jQuery(el).val(%s).trigger('change'); } catch(e) {} }",
      "if(window.Shiny&&Shiny.setInputValue){",
      "  Shiny.setInputValue('sc', %s, {priority:'event'});",
      "}",
      "var btn=document.getElementById('search');",
      "if(btn){",
      "  if(window.jQuery){ try { jQuery(btn).click(); } catch(e) { btn.click(); } }",
      "  else { btn.click(); }",
      "  if(window.Shiny&&Shiny.setInputValue){",
      "    Shiny.setInputValue('search', Math.random(), {priority:'event'});",
      "  }",
      "}",
      "return el.value;",
      "})()"
    ),
    jsonlite::toJSON(ticker, auto_unbox = TRUE),
    jsonlite::toJSON(ticker, auto_unbox = TRUE),
    jsonlite::toJSON(ticker, auto_unbox = TRUE)
  )
)
check(paste0("ticker typed into #sc (", as.character(filled), ")"), identical(toupper(as.character(filled)[1]), ticker))
Sys.sleep(1)

# Strong UI assertion (not landing marketing copy): left home, ticker lodged in #sc,
# and Company surfaces (summary / statements chrome) are visible. Live Yahoo numeric
# fill is covered by test_audit_live_statements.R — E2E may show N/A under rate-limit.
corp_ok <- .js_await(
  s,
  sprintf(
    paste0(
      "(function(){",
      "var el=document.getElementById('sc');",
      "var sc=(el && el.value) || '';",
      "var t=(document.body && document.body.innerText) || '';",
      # Home markup may remain in DOM; require it is not the visible surface.
      "var home=document.querySelector('.ynow-home-grid');",
      "var homeVisible=false;",
      "if(home){",
      "  var st=window.getComputedStyle(home);",
      "  homeVisible = !!(home.offsetParent) && st.display!=='none' && st.visibility!=='hidden';",
      "}",
      "var tickerOk = sc.toUpperCase()===%s;",
      "var companyChrome = /Finance Summary|Income Statement|Balance Sheet|Cash Flow|Industry info from Yahoo|Sector:/i.test(t);",
      "return !homeVisible && tickerOk && companyChrome;",
      "})()"
    ),
    jsonlite::toJSON(ticker, auto_unbox = TRUE)
  ),
  timeout_sec = 120
)
if (!isTRUE(corp_ok)) {
  dbg <- .js_eval(
    s,
    paste0(
      "(function(){",
      "var el=document.getElementById('sc');",
      "return {",
      "sc: el?el.value:null,",
      "home: !!document.querySelector('.ynow-home-grid'),",
      "snippet: ((document.body&&document.body.innerText)||'').slice(0,240)",
      "};",
      "})()"
    )
  )
  cat("INFO company-surface debug: ", paste(capture.output(str(dbg)), collapse = " "), "\n", sep = "")
}
check(paste0("company surface after search (", ticker, ")"), isTRUE(corp_ok))

after_png <- file.path(artifact_dir, "03_after_search.png")
check("screenshot after search", .screenshot(s, after_png))

# Open Fundamental Value / Smart Analysis surface
.js_eval(
  s,
  paste0(
    "(function(){",
    "var a=document.querySelector('.sidebar-menu a[data-value=\"get_started\"]');",
    "if(a){a.click(); return 'get_started';}",
    "var a2=document.querySelector('.sidebar-menu a[data-value=\"smart_analysis\"]');",
    "if(a2){a2.click(); return 'smart_analysis';}",
    "return false;",
    "})()"
  )
)
Sys.sleep(3)
value_png <- file.path(artifact_dir, "04_value_tab.png")
check("screenshot value tab", .screenshot(s, value_png))
value_ok <- .js_await(
  s,
  paste0(
    "(function(){",
    "var t=(document.body && document.body.innerText) || '';",
    "return /DCF|WACC|MOS|Fair Value|Smart Analysis|Bearing|Bear|Base/i.test(t);",
    "})()"
  ),
  timeout_sec = 60
)
check("value/DCF surface text present", isTRUE(value_ok))

body_len <- .js_eval(s, "(document.body && document.body.innerText || '').length")
check("body has substantial text", is.finite(as.numeric(body_len)[1]) && as.numeric(body_len)[1] > 200)

# Write a small evidence note
writeLines(
  c(
    paste0("ticker=", ticker),
    paste0("shiny_url=", shiny_url),
    paste0("chrome_port=", chrome_port),
    paste0("home=", home_png),
    paste0("company=", company_png),
    paste0("after_search=", after_png),
    paste0("value=", value_png),
    paste0("PASS=", pass, " FAIL=", fail)
  ),
  file.path(artifact_dir, "e2e_summary.txt")
)

cat("PASS=", pass, " FAIL=", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
quit(status = 0L)
