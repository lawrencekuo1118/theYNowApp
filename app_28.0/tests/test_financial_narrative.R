#!/usr/bin/env Rscript
# Financial Narrative Intelligence — offline unit + wiring checks.
# Run: cd app_28.0 && YNOW_AI_QUAL_MOCK=1 Rscript tests/test_financial_narrative.R
Sys.setenv(
  YNOW_DEBUG_SKIP_PY = "1",
  YNOW_AI_QUAL_MOCK = "1",
  YNOW_AI_QUAL_ENABLED = "0"
)

app_dir <- if (file.exists("financial_narrative_schema.R")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (file.exists("../financial_narrative_schema.R")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Cannot locate financial_narrative_schema.R")
}
setwd(app_dir)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

ok <- 0L
bad <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    message("OK  ", label)
  } else {
    bad <<- bad + 1L
    message("FAIL ", label)
  }
}

source("financial_narrative_schema.R", local = TRUE, encoding = "UTF-8")
source("financial_document_client.R", local = TRUE, encoding = "UTF-8")
source("financial_llm_client.R", local = TRUE, encoding = "UTF-8")
source("financial_narrative_cache.R", local = TRUE, encoding = "UTF-8")
source("financial_narrative_service.R", local = TRUE, encoding = "UTF-8")
source("financial_narrative_module.R", local = TRUE, encoding = "UTF-8")

fix_q3 <- file.path(app_dir, "tests/fixtures/fni_transcript_q3.txt")
fix_q2 <- file.path(app_dir, "tests/fixtures/fni_transcript_q2.txt")
check("fixtures present", file.exists(fix_q3) && file.exists(fix_q2))

# --- Schema / evidence ---
text_q3 <- paste(readLines(fix_q3, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
good <- ynow_fni_empty_finding("management_guidance")
good$claim <- "Mid-single-digit growth guided"
good$evidence_quote <- "CFO guidance: mid-single-digit growth; no change to long-term WACC assumptions."
good$polarity <- "neutral"
good$confidence <- 0.8
v_ok <- ynow_fni_validate_finding(good, text_q3)
check("grounded evidence validates", isTRUE(v_ok$ok))

bad_f <- good
bad_f$evidence_quote <- "We guide 40% revenue growth and cut WACC to 3%."
v_bad <- ynow_fni_validate_finding(bad_f, text_q3)
check("fabricated guidance rejected", !isTRUE(v_bad$ok))

# --- Chunking ---
chunks <- ynow_fni_chunk_text(paste(rep(text_q3, 20), collapse = "\n\n"), max_chars = 400L)
check("chunker produces multiple chunks for long text", length(chunks) >= 2L)

# --- Mock Docling + Ollama pipeline ---
tmp_cache <- file.path(tempdir(), paste0("fni_test_cache_", as.integer(Sys.time())))
Sys.setenv(YNOW_AI_CACHE_DIR = tmp_cache)
ynow_fni_cache_clear()

st <- ynow_fni_service_status()
check("mock services report ready", isTRUE(st$ready) && identical(st$docling$status, "mock"))

doc <- ynow_fni_docling_convert_file(fix_q3, filename = "fni_transcript_q3.txt")
check("mock Docling extracts fixture text", isTRUE(doc$ok) && grepl("AI servers", doc$text, fixed = TRUE))

one <- ynow_fni_analyze_document(
  list(
    kind = "upload",
    path = fix_q3,
    filename = "fni_transcript_q3.txt",
    ticker = "EXAM",
    period = "2024Q3",
    language = "en"
  ),
  locale = "en",
  force = TRUE
)
check("mock analyze returns ok + findings", {
  isTRUE(one$ok) &&
    is.list(one$result) &&
    length(one$result$findings) >= 1L &&
    !isTRUE(one$cache_hit)
})
check("findings evidence all in text", {
  all(vapply(one$result$findings, function(f) {
    grepl(gsub("\\s+", " ", f$evidence_quote, perl = TRUE),
          gsub("\\s+", " ", one$extracted_text, perl = TRUE),
          fixed = TRUE)
  }, logical(1)))
})

two <- ynow_fni_analyze_document(
  list(
    kind = "upload",
    path = fix_q3,
    filename = "fni_transcript_q3.txt",
    ticker = "EXAM",
    period = "2024Q3",
    language = "en"
  ),
  locale = "en",
  force = FALSE
)
check("second run is cache hit", isTRUE(two$ok) && isTRUE(two$cache_hit))

# --- Cross-quarter: not mentioned ≠ resolved ---
cmp <- ynow_fni_analyze_and_compare(
  list(
    list(kind = "upload", path = fix_q3, filename = "q3.txt", ticker = "EXAM", period = "2024Q3"),
    list(kind = "upload", path = fix_q2, filename = "q2.txt", ticker = "EXAM", period = "2024Q2")
  ),
  locale = "en",
  force = TRUE
)
check("compare ok with two periods", isTRUE(cmp$ok) && isTRUE(cmp$compare$ok))
check("compare never uses resolved status", {
  statuses <- vapply(cmp$compare$changes, function(ch) as.character(ch$status %||% "")[1], character(1))
  !any(grepl("resolved", statuses, ignore.case = TRUE))
})

# Offline services (master off, mock off) → honest message
Sys.setenv(YNOW_AI_QUAL_MOCK = "0", YNOW_AI_QUAL_ENABLED = "0")
st_off <- ynow_fni_service_status()
check("offline services not ready", !isTRUE(st_off$ready))
check("offline message mentions not configured", {
  grepl("not configured", st_off$message %||% "", ignore.case = TRUE) ||
    grepl("unreachable", st_off$message %||% "", ignore.case = TRUE)
})
# Restore mock for any later checks
Sys.setenv(YNOW_AI_QUAL_MOCK = "1")

# --- UI / wiring greps (Lite-free) ---
ui_src <- paste(readLines(file.path(app_dir, "ynow_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
server_src <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
glob_src <- paste(readLines(file.path(app_dir, "global.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
auth_src <- paste(readLines(file.path(app_dir, "auth_account.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js_src <- paste(readLines(file.path(app_dir, "www/ynow_app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
loc_src <- paste(readLines(file.path(app_dir, "ui_locale.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
renviron <- paste(readLines(file.path(app_dir, ".Renviron.example"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
doc_path <- file.path(app_dir, "..", "docs", "financial-narrative-free-api-integration.md")

check("global sources FNI stack", {
  grepl("financial_narrative_schema.R", glob_src, fixed = TRUE) &&
    grepl("financial_narrative_module.R", glob_src, fixed = TRUE)
})
check("sidebar menu + lazy host wired", {
  grepl('tabName = "financial_narrative"', ui_src, fixed = TRUE) &&
    grepl("ynow_menu_financial_narrative", ui_src, fixed = TRUE) &&
    grepl("ynow_lazy_host_financial_narrative", ui_src, fixed = TRUE) &&
    grepl(".ynow_page_ui_financial_narrative", ui_src, fixed = TRUE)
})
check("server registers module + lazy builder", {
  grepl("financial_narrative_server", server_src, fixed = TRUE) &&
    grepl("financial_narrative = function()", server_src, fixed = TRUE)
})
check("Lite does NOT omit financial_narrative", {
  # Extract full_only function body roughly
  m <- regmatches(
    auth_src,
    regexpr("ynow_sidebar_full_only_tabs <- function\\(\\) \\{[\\s\\S]*?\\n\\}", auth_src, perl = TRUE)
  )
  length(m) == 1L && !grepl("financial_narrative", m, fixed = TRUE)
})
check("no ynow-full-only on FNI page host", {
  grepl('id = "ynow_financial_narrative_host"', ui_src, fixed = TRUE) &&
    !grepl('ynow-full-only[\\s\\S]{0,80}ynow_financial_narrative_host', ui_src, perl = TRUE)
})
check("locale en + zh-TW menu keys", {
  grepl('menu_financial_narrative = "Financial Narrative Analysis"', loc_src, fixed = TRUE) &&
    grepl('menu_financial_narrative = "財務質性分析"', loc_src, fixed = TRUE) &&
    grepl("fni_service_not_configured", loc_src, fixed = TRUE)
})
check("JS applyUiLocale maps financial_narrative", {
  grepl("financial_narrative: s.menu_financial_narrative", js_src, fixed = TRUE) &&
    grepl("ynow_menu_financial_narrative", js_src, fixed = TRUE) &&
    grepl("ynow_fni_page_title", js_src, fixed = TRUE)
})
check(".Renviron.example documents AI vars", {
  grepl("YNOW_AI_QUAL_ENABLED", renviron, fixed = TRUE) &&
    grepl("YNOW_DOCLING_URL", renviron, fixed = TRUE) &&
    grepl("YNOW_OLLAMA_MODEL", renviron, fixed = TRUE)
})
check("integration doc present", file.exists(doc_path))

# Parse check for module file
check("module parses", inherits(tryCatch(parse(file.path(app_dir, "financial_narrative_module.R")), error = function(e) e), "expression"))
check("service parses", inherits(tryCatch(parse(file.path(app_dir, "financial_narrative_service.R")), error = function(e) e), "expression"))

message("")
message(sprintf("Passed %d  Failed %d", ok, bad))
if (bad > 0L) quit(status = 1L)
quit(status = 0L)
