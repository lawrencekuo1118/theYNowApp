# financial_llm_client.R — Ollama chat client (qwen3.5:9b default)
# Non-stream /api/chat with JSON Schema. Never auto-downloads models on boot.

ynow_fni_ollama_url <- function() {
  u <- trimws(Sys.getenv("YNOW_OLLAMA_URL", unset = "http://127.0.0.1:11434"))
  if (!nzchar(u)) u <- "http://127.0.0.1:11434"
  sub("/+$", "", u)
}

ynow_fni_ollama_model <- function() {
  m <- trimws(Sys.getenv("YNOW_OLLAMA_MODEL", unset = "qwen3.5:9b"))
  if (!nzchar(m)) m <- "qwen3.5:9b"
  m
}

#' Probe Ollama without loading/pulling a model.
ynow_fni_ollama_status <- function(timeout = 3) {
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(list(
      ok = TRUE,
      status = "mock",
      message = "Mock Ollama enabled (YNOW_AI_QUAL_MOCK=1).",
      url = "mock://ollama",
      model = ynow_fni_ollama_model()
    ))
  }
  base <- ynow_fni_ollama_url()
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(
      ok = FALSE, status = "unavailable",
      message = "httr package is required for Ollama HTTP calls.",
      url = base, model = ynow_fni_ollama_model()
    ))
  }
  res <- tryCatch(
    httr::GET(paste0(base, "/api/tags"), httr::timeout(timeout)),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(
      ok = FALSE,
      status = "offline",
      message = paste0(
        "AI service not configured / Ollama unreachable at ", base, ": ",
        conditionMessage(res)
      ),
      url = base,
      model = ynow_fni_ollama_model()
    ))
  }
  code <- httr::status_code(res)
  if (!(code >= 200 && code < 300)) {
    return(list(
      ok = FALSE, status = "error",
      message = paste0("Ollama HTTP ", code, " at ", base),
      url = base, model = ynow_fni_ollama_model()
    ))
  }
  body <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  models <- character()
  if (is.list(parsed) && length(parsed$models)) {
    models <- vapply(parsed$models, function(m) as.character(m$name %||% "")[1], character(1))
  }
  want <- ynow_fni_ollama_model()
  has <- any(models == want) || any(startsWith(models, paste0(sub(":.*$", "", want), ":")))
  list(
    ok = TRUE,
    status = if (has) "model_present" else "reachable_model_missing",
    message = if (has) {
      paste0("Ollama reachable; model ", want, " listed.")
    } else {
      paste0(
        "Ollama reachable at ", base,
        " but model ", want,
        " is not listed. Pull it manually (do not auto-download from the app)."
      )
    },
    url = base,
    model = want,
    models = models
  )
}

.ynow_fni_system_prompt <- function(locale = "en") {
  loc <- if (identical(as.character(locale)[1], "zh-TW")) "zh-TW" else "en"
  if (identical(loc, "zh-TW")) {
    paste(
      "你是台灣投資研究助理，只根據提供的文件原文做質性分析。",
      "必須輸出符合 JSON Schema 的物件，含 summary 與 findings。",
      "六個維度：growth_drivers, profitability_outlook, capital_allocation,",
      "risk_factors, management_guidance, narrative_change。",
      "每筆 finding 的 evidence_quote 必須是文件中可逐字找到的原文片段；",
      "不可捏造頁碼或未出現的數字／指引。",
      "若文件未提及某主題，不要硬寫；可省略該維度。",
      "param_suggestion 僅供參考，不可當成已套用的 WACC/Ke/g/價格。",
      "正式財經名詞保留英文（WACC、FCFF、CapEx、MOS 等）。",
      sep = " "
    )
  } else {
    paste(
      "You are an investment-research assistant. Analyze only the provided document text.",
      "Return a JSON object matching the schema with summary and findings.",
      "Six dimensions: growth_drivers, profitability_outlook, capital_allocation,",
      "risk_factors, management_guidance, narrative_change.",
      "Each finding's evidence_quote MUST be a verbatim substring of the source text.",
      "Do not invent page numbers, guidance figures, or claims without quotes.",
      "If a theme is absent, omit it — never invent.",
      "param_suggestion is advisory only; never treat it as applied WACC/Ke/g/price.",
      "Keep formal finance terms in English (WACC, FCFF, CapEx, MOS, etc.).",
      sep = " "
    )
  }
}

.ynow_fni_user_prompt <- function(chunk_text, meta = list(), locale = "en") {
  ticker <- as.character(meta$ticker %||% "")[1]
  period <- as.character(meta$period %||% "")[1]
  source <- as.character(meta$source_label %||% meta$filename %||% "")[1]
  paste0(
    "Ticker: ", ticker, "\n",
    "Period: ", period, "\n",
    "Source: ", source, "\n",
    "Prompt-version: ", .YNOW_FNI_PROMPT_VERSION, "\n\n",
    "--- DOCUMENT TEXT START ---\n",
    chunk_text,
    "\n--- DOCUMENT TEXT END ---\n"
  )
}

#' Call Ollama /api/chat (non-stream) with JSON Schema format.
#' Returns list(ok=, payload=, raw=, error=, message=, model=).
ynow_fni_ollama_chat_json <- function(chunk_text,
                                      meta = list(),
                                      locale = "en",
                                      timeout = NULL) {
  if (.ynow_fni_env_flag("YNOW_AI_QUAL_MOCK", "0")) {
    return(.ynow_fni_ollama_mock_chat(chunk_text, meta = meta, locale = locale))
  }
  if (!isTRUE(ynow_fni_ai_enabled())) {
    return(list(
      ok = FALSE, payload = NULL, raw = "",
      error = "ai_service_not_configured",
      message = "AI service not configured. Set YNOW_AI_QUAL_ENABLED=1 and start Ollama.",
      model = ynow_fni_ollama_model()
    ))
  }
  st <- ynow_fni_ollama_status(timeout = min(3, ynow_fni_request_timeout()))
  if (!isTRUE(st$ok)) {
    return(list(
      ok = FALSE, payload = NULL, raw = "",
      error = "ai_service_not_configured",
      message = st$message %||% "AI service not configured",
      model = ynow_fni_ollama_model()
    ))
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(ok = FALSE, payload = NULL, raw = "", error = "httr_missing",
                message = "httr missing", model = ynow_fni_ollama_model()))
  }
  to <- timeout %||% ynow_fni_request_timeout()
  model <- ynow_fni_ollama_model()
  body <- list(
    model = model,
    stream = FALSE,
    format = ynow_fni_json_schema(),
    messages = list(
      list(role = "system", content = .ynow_fni_system_prompt(locale)),
      list(role = "user", content = .ynow_fni_user_prompt(chunk_text, meta, locale))
    ),
    options = list(temperature = 0.2)
  )
  url <- paste0(ynow_fni_ollama_url(), "/api/chat")
  res <- tryCatch(
    httr::POST(
      url,
      body = body,
      encode = "json",
      httr::timeout(to),
      httr::add_headers(`Content-Type` = "application/json")
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(
      ok = FALSE, payload = NULL, raw = "",
      error = "ollama_request_failed",
      message = conditionMessage(res),
      model = model
    ))
  }
  code <- httr::status_code(res)
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  if (!(code >= 200 && code < 300)) {
    return(list(
      ok = FALSE, payload = NULL, raw = raw,
      error = "ollama_http_error",
      message = paste0("Ollama HTTP ", code),
      model = model
    ))
  }
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  content <- ""
  if (is.list(parsed)) {
    content <- as.character(parsed$message$content %||% parsed$response %||% "")[1]
  }
  payload <- tryCatch(jsonlite::fromJSON(content, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload) && is.list(parsed$message) && is.list(parsed$message$content)) {
    payload <- parsed$message$content
  }
  if (is.null(payload)) {
    return(list(
      ok = FALSE, payload = NULL, raw = raw,
      error = "ollama_bad_json",
      message = "Model response was not valid JSON for the FNI schema.",
      model = model
    ))
  }
  list(ok = TRUE, payload = payload, raw = raw, error = "", message = "", model = model)
}

.ynow_fni_ollama_mock_chat <- function(chunk_text, meta = list(), locale = "en") {
  text <- as.character(chunk_text %||% "")[1]
  # Pull short grounded quotes from the text itself when possible.
  pick_quote <- function(patterns, fallback) {
    for (pat in patterns) {
      m <- regexpr(pat, text, perl = TRUE, ignore.case = TRUE)
      if (m[1] > 0) {
        s <- substr(text, m[1], m[1] + attr(m, "match.length") - 1L)
        if (nchar(s) > 12) return(substr(s, 1, 180))
      }
    }
    # Fallback: first non-empty line that is long enough and present in text
    lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
    lines <- lines[nzchar(lines) & nchar(lines) >= 20]
    if (length(lines)) return(substr(lines[[1]], 1, 180))
    fallback
  }
  q_growth <- pick_quote(
    c("revenue growth[^\\n]{0,80}", "成長[^\\n]{0,60}", "AI servers?[^\\n]{0,60}"),
    "Management expects revenue growth from AI servers in the next year."
  )
  q_margin <- pick_quote(
    c("Gross margin[^\\n]{0,80}", "毛利率[^\\n]{0,60}", "pricing pressure[^\\n]{0,60}"),
    "Gross margin outlook remains under pricing pressure."
  )
  q_capex <- pick_quote(
    c("CapEx[^\\n]{0,80}", "資本支出[^\\n]{0,60}", "dividends?[^\\n]{0,60}"),
    "CapEx will focus on capacity expansion; dividends stay stable."
  )
  q_risk <- pick_quote(
    c("risks? include[^\\n]{0,100}", "風險[^\\n]{0,60}", "customer concentration[^\\n]{0,60}"),
    "Key risks include customer concentration and FX volatility."
  )
  q_guide <- pick_quote(
    c("guidance[:\\s][^\\n]{0,100}", "指引[^\\n]{0,60}", "mid-single-digit[^\\n]{0,60}"),
    "CFO guidance: mid-single-digit growth; no change to long-term WACC assumptions."
  )
  # Only keep quotes that actually appear in text (schema validator will also check).
  mk <- function(dim, claim, pol, quote) {
    list(
      dimension = dim,
      claim = claim,
      polarity = pol,
      confidence = 0.7,
      evidence_quote = quote,
      evidence_source = as.character(meta$source_label %||% meta$filename %||% "mock")[1],
      evidence_locator = "",
      period = as.character(meta$period %||% "")[1],
      notes = "mock",
      param_suggestion = NULL
    )
  }
  findings <- list(
    mk("growth_drivers",
       if (identical(locale, "zh-TW")) "文件提及營收成長動能（如 AI 伺服器相關）。"
       else "Document cites revenue growth drivers (e.g. AI servers).",
       "positive", q_growth),
    mk("profitability_outlook",
       if (identical(locale, "zh-TW")) "毛利率展望受定價壓力影響。"
       else "Gross margin outlook is under pricing pressure.",
       "negative", q_margin),
    mk("capital_allocation",
       if (identical(locale, "zh-TW")) "CapEx 聚焦產能；股利政策偏穩定。"
       else "CapEx focuses on capacity; dividend stance is stable.",
       "neutral", q_capex),
    mk("risk_factors",
       if (identical(locale, "zh-TW")) "風險包含客戶集中與匯率波動。"
       else "Risks include customer concentration and FX volatility.",
       "negative", q_risk),
    mk("management_guidance",
       if (identical(locale, "zh-TW")) "管理層指引為中個位數成長；未改長期 WACC 假設。"
       else "Management guides mid-single-digit growth; no change to long-term WACC assumptions.",
       "neutral", q_guide)
  )
  # Drop any whose quote is not in text (honest mock).
  findings <- Filter(function(f) {
    grepl(gsub("\\s+", " ", f$evidence_quote, perl = TRUE),
          gsub("\\s+", " ", text, perl = TRUE),
          fixed = TRUE)
  }, findings)
  payload <- list(
    summary = if (identical(locale, "zh-TW")) {
      "（模擬）依文件原文整理之質性摘要；證據句皆來自擷取文字。"
    } else {
      "(Mock) Qualitative summary grounded in extracted text; quotes are verbatim."
    },
    findings = findings
  )
  list(
    ok = TRUE,
    payload = payload,
    raw = jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
    error = "",
    message = "",
    model = "mock"
  )
}

#' Merge chunk-level LLM payloads: concatenate findings, join summaries, recheck evidence.
ynow_fni_merge_chunk_payloads <- function(payloads, full_text = "") {
  payloads <- Filter(function(p) is.list(p), payloads)
  if (!length(payloads)) {
    return(list(summary = "", findings = list()))
  }
  summaries <- vapply(payloads, function(p) as.character(p$summary %||% "")[1], character(1))
  summaries <- summaries[nzchar(summaries)]
  findings <- list()
  for (p in payloads) {
    fs <- p$findings %||% list()
    if (is.data.frame(fs)) {
      fs <- lapply(seq_len(nrow(fs)), function(i) as.list(fs[i, , drop = FALSE]))
    }
    for (f in fs) findings[[length(findings) + 1L]] <- f
  }
  findings <- ynow_fni_filter_findings(findings, full_text)
  # De-dupe by dimension + evidence_quote
  keys <- vapply(findings, function(f) {
    paste(f$dimension %||% "", f$evidence_quote %||% "", sep = "::")
  }, character(1))
  findings <- findings[!duplicated(keys)]
  list(
    summary = paste(unique(summaries), collapse = " "),
    findings = findings
  )
}
