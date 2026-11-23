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

# --- Mock local extract + Workers AI pipeline ---
tmp_cache <- file.path(tempdir(), paste0("fni_test_cache_", as.integer(Sys.time())))
Sys.setenv(YNOW_AI_CACHE_DIR = tmp_cache)
ynow_fni_cache_clear()

st <- ynow_fni_service_status()
check("mock services report ready", isTRUE(st$ready) && identical(st$document$status, "mock"))

doc <- ynow_fni_convert_file(fix_q3, filename = "fni_transcript_q3.txt")
check("mock extract reads fixture text", isTRUE(doc$ok) && grepl("AI servers", doc$text, fixed = TRUE))

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

# reachable_model_missing must not green-banner / ready=TRUE
ynow_fni_llm_status <- function(...) {
  list(
    ok = FALSE,
    status = "reachable_model_missing",
    message = "Workers AI model is not available.",
    url = "https://api.cloudflare.com/client/v4/accounts/example/ai/run/@cf/meta/llama-3.3-70b-instruct-fp8-fast",
    model = "@cf/meta/llama-3.3-70b-instruct-fp8-fast"
  )
}
ynow_fni_document_status <- function(...) {
  list(ok = TRUE, status = "cf_markdown", message = "Cloudflare Markdown Conversion reachable", url = "https://api.cloudflare.com")
}
Sys.setenv(YNOW_AI_QUAL_MOCK = "0", YNOW_AI_QUAL_ENABLED = "1")
st_miss <- ynow_fni_service_status()
check("reachable_model_missing not ready", !isTRUE(st_miss$ready))
check("reachable_model_missing message retained", {
  grepl("not available", st_miss$message %||% "", fixed = TRUE) ||
    grepl("model", st_miss$message %||% "", ignore.case = TRUE)
})
# Restore mock clients + env for remaining checks
source("financial_document_client.R", local = TRUE, encoding = "UTF-8")
source("financial_llm_client.R", local = TRUE, encoding = "UTF-8")
source("financial_narrative_service.R", local = TRUE, encoding = "UTF-8")
Sys.setenv(YNOW_AI_QUAL_MOCK = "1", YNOW_AI_QUAL_ENABLED = "0")

# --- Cloudflare wiring (no network) ---
cf_prev <- list(
  mock = Sys.getenv("YNOW_AI_QUAL_MOCK", unset = ""),
  enabled = Sys.getenv("YNOW_AI_QUAL_ENABLED", unset = ""),
  account = Sys.getenv("YNOW_CF_ACCOUNT_ID", unset = ""),
  token = Sys.getenv("YNOW_CF_API_TOKEN", unset = ""),
  model = Sys.getenv("YNOW_CF_AI_MODEL", unset = ""),
  backend = Sys.getenv("YNOW_FNI_PARSE_BACKEND", unset = ""),
  cf_account = Sys.getenv("CLOUDFLARE_ACCOUNT_ID", unset = ""),
  cf_token = Sys.getenv("CLOUDFLARE_API_TOKEN", unset = "")
)
Sys.setenv(
  YNOW_AI_QUAL_MOCK = "0",
  YNOW_AI_QUAL_ENABLED = "0",
  YNOW_CF_ACCOUNT_ID = "",
  YNOW_CF_API_TOKEN = "",
  CLOUDFLARE_ACCOUNT_ID = "",
  CLOUDFLARE_API_TOKEN = "",
  YNOW_CF_AI_MODEL = "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
  YNOW_FNI_PARSE_BACKEND = "local_cf"
)
local_txt <- ynow_fni_convert_file(fix_q3, filename = "fni_transcript_q3.txt")
check("txt extracts locally without Cloudflare", {
  isTRUE(local_txt$ok) &&
    identical(local_txt$meta$source, "local_extract") &&
    grepl("AI servers", local_txt$text, fixed = TRUE)
})
pdf_tmp <- tempfile(fileext = ".pdf")
writeBin(charToRaw("%PDF-1.1\nnot-a-text-layer"), pdf_tmp)
pdf_off <- ynow_fni_convert_file(pdf_tmp, filename = "scan.pdf")
check("image-like PDF without credentials names Cloudflare", {
  !isTRUE(pdf_off$ok) &&
    identical(pdf_off$error, "ai_service_not_configured") &&
    grepl("Cloudflare", pdf_off$message %||% "", fixed = TRUE) &&
    !grepl("Docling Serve", pdf_off$message %||% "", fixed = TRUE)
})
unlink(pdf_tmp)
Sys.setenv(YNOW_FNI_PARSE_BACKEND = "docling")
deferred <- ynow_fni_convert_file(fix_q3, filename = "fni_transcript_q3.txt")
check("docling backend stays on the Cloud Run hook", {
  !isTRUE(deferred$ok) &&
    identical(deferred$error, "docling_cloudrun_not_implemented")
})
hook <- ynow_fni_docling_cloudrun_convert(fix_q3, filename = "x.txt")
check("Cloud Run Docling hook is explicit", identical(hook$error, "docling_cloudrun_not_implemented"))
Sys.setenv(YNOW_FNI_PARSE_BACKEND = "local_cf", YNOW_AI_QUAL_ENABLED = "1")
llm_off <- ynow_fni_llm_status()
check("Workers AI without token is not configured", {
  !isTRUE(llm_off$ok) &&
    grepl("YNOW_CF_ACCOUNT_ID", llm_off$message %||% "", fixed = TRUE)
})
Sys.setenv(YNOW_CF_ACCOUNT_ID = "abc123", YNOW_AI_QUAL_ENABLED = "0")
run_url <- ynow_fni_cf_ai_run_url()
check("Workers AI run URL uses account and JSON-mode model", {
  grepl(
    "accounts/abc123/ai/run/@cf/meta/llama-3.3-70b-instruct-fp8-fast",
    run_url,
    fixed = TRUE
  ) &&
    grepl("/ai/tomarkdown", ynow_fni_cf_markdown_url(), fixed = TRUE)
})
body <- ynow_fni_llm_request_body("excerpt", list(ticker = "EXAM", period = "2024Q3"), "en")
check("Workers AI body keeps JSON schema", {
  identical(body$response_format$type, "json_schema") &&
    is.list(body$response_format$json_schema$properties$findings) &&
    length(body$messages) == 2L
})
coerced <- ynow_fni_coerce_llm_payload(list(
  result = list(response = list(summary = "from-run", findings = list()))
))
coerced_chat <- ynow_fni_coerce_llm_payload(list(
  choices = list(list(message = list(content = "{\"summary\":\"from-chat\",\"findings\":[]}")))
))
check("payload coerce accepts run and chat shapes", {
  identical(coerced$summary, "from-run") && identical(coerced_chat$summary, "from-chat")
})
restore_env <- function(name, value) {
  if (nzchar(value)) {
    args <- stats::setNames(list(value), name)
    do.call(Sys.setenv, args)
  } else {
    Sys.unsetenv(name)
  }
}
restore_env("YNOW_AI_QUAL_MOCK", "1")
restore_env("YNOW_AI_QUAL_ENABLED", "0")
restore_env("YNOW_CF_ACCOUNT_ID", cf_prev$account)
restore_env("YNOW_CF_API_TOKEN", cf_prev$token)
restore_env("YNOW_CF_AI_MODEL", cf_prev$model)
restore_env("YNOW_FNI_PARSE_BACKEND", cf_prev$backend)
restore_env("CLOUDFLARE_ACCOUNT_ID", cf_prev$cf_account)
restore_env("CLOUDFLARE_API_TOKEN", cf_prev$cf_token)
rm(restore_env)

# Same-period sources must merge findings (not overwrite)
same_period <- ynow_fni_analyze_and_compare(
  list(
    list(
      kind = "paste",
      text = paste(readLines(fix_q3, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      ticker = "EXAM",
      period = "2024Q3",
      filename = "paste_a"
    ),
    list(
      kind = "paste",
      text = paste(readLines(fix_q2, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      ticker = "EXAM",
      period = "2024Q3",
      filename = "paste_b"
    )
  ),
  locale = "en",
  force = TRUE
)
check("same-period sources keep one period key", {
  identical(sort(names(same_period$results)), "2024Q3")
})
check("same-period sources merge findings", {
  isTRUE(same_period$ok) &&
    is.list(same_period$results[["2024Q3"]]) &&
    isTRUE(same_period$results[["2024Q3"]]$ok) &&
    length(same_period$results[["2024Q3"]]$result$findings) >= 1L &&
    grepl("paste_a|paste_b|Q2|Q3|AI servers|guidance",
          same_period$results[["2024Q3"]]$extracted_text %||% "",
          ignore.case = TRUE)
})
check("module avoids sync fallback on async invoke error", {
  mod <- paste(readLines(file.path(app_dir, "financial_narrative_module.R"),
                          warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Async path queues latest press; sync analyze only when ExtendedTask is unavailable.
  n_analyze <- length(gregexpr("ynow_fni_analyze_and_compare", mod, fixed = TRUE)[[1]])
  grepl("fni_pending", mod, fixed = TRUE) &&
    grepl("fni_run_gen", mod, fixed = TRUE) &&
    grepl("fni_inflight_gen", mod, fixed = TRUE) &&
    grepl(".fni_finish_async", mod, fixed = TRUE) &&
    grepl(".fni_try_start_async", mod, fixed = TRUE) &&
    grepl("task_invoke_failed", mod, fixed = TRUE) &&
    !grepl("Fallback sync path", mod, fixed = TRUE) &&
    identical(n_analyze, 1L)
})
check("Cloudflare httr config preserves add_headers", {
  doc <- paste(readLines(file.path(app_dir, "financial_document_client.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  grepl(".ynow_fni_cf_httr_config", doc, fixed = TRUE) &&
    grepl("ynow_fni_docling_cloudrun_convert", doc, fixed = TRUE) &&
    !grepl(
      "hdrs\\s*<-\\s*c\\(hdrs,\\s*httr::add_headers",
      doc
    )
})

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
check(".Renviron.example documents Cloudflare AI vars", {
  grepl("YNOW_AI_QUAL_ENABLED", renviron, fixed = TRUE) &&
    grepl("YNOW_CF_ACCOUNT_ID", renviron, fixed = TRUE) &&
    grepl("YNOW_CF_API_TOKEN", renviron, fixed = TRUE) &&
    grepl("YNOW_CF_AI_MODEL", renviron, fixed = TRUE) &&
    grepl("not implemented", renviron, ignore.case = TRUE) &&
    !grepl("YNOW_OLLAMA_MODEL", renviron, fixed = TRUE)
})
check("locale copy describes Cloudflare setup", {
  grepl("Cloudflare Workers AI", loc_src, fixed = TRUE) &&
    grepl("fni_service_offline_detail", loc_src, fixed = TRUE) &&
    grepl("YNOW_CF_ACCOUNT_ID", loc_src, fixed = TRUE) &&
    !grepl("Docling + Ollama", loc_src, fixed = TRUE) &&
    !grepl("本機設定 Docling", loc_src, fixed = TRUE)
})
check("integration doc present", file.exists(doc_path))

# Parse check for module file
check("module parses", inherits(tryCatch(parse(file.path(app_dir, "financial_narrative_module.R")), error = function(e) e), "expression"))
check("service parses", inherits(tryCatch(parse(file.path(app_dir, "financial_narrative_service.R")), error = function(e) e), "expression"))

message("")
message(sprintf("Passed %d  Failed %d", ok, bad))
if (bad > 0L) quit(status = 1L)
quit(status = 0L)
