# Financial Narrative Intelligence — Cloudflare Workers AI

Live line: `app_28.0/`. Sidebar: **財務質性分析** / **Financial Narrative Analysis** (`tabName = financial_narrative`).  
Lite + Full: **open** (not in `ynow_sidebar_full_only_tabs` / Lite omit; no membership lock). Workers AI usage is billed on the Cloudflare account that owns the API token. That is separate from an app membership fee.

## Architecture

```
Upload / paste / earnings-call catalog
        │
        ▼
 financial_document_client.R   ← R local text / text-layer PDF
        │                        else Cloudflare Markdown Conversion
        │                        (POST /accounts/{id}/ai/tomarkdown)
        │  chunk (section-aware)
        ▼
 financial_llm_client.R        ← Workers AI /ai/run/{model} + JSON schema
        │                        (or offline mock)
        │  merge + evidence recheck
        ▼
 financial_narrative_schema.R  ← 6 dimensions, evidence rules, cross-quarter
        │
 financial_narrative_cache.R   ← key = hash + prompt_version + model (+ locale/ticker/period)
        │
 financial_narrative_module.R  ← Shiny UI (user-triggered Run only)
```

Phase 1 is **qualitative** plus optional `param_suggestion` fields. The app **never** auto-overwrites WACC / Ke / g / price. Evidence quotes must be verbatim substrings of extracted text. The model is not allowed to invent quotes; the evidence filter drops any quote that is not in the extracted text.

### Six dimensions

| id | en-US | zh-TW |
|---|---|---|
| `growth_drivers` | Growth Drivers | 成長動能 |
| `profitability_outlook` | Profitability Outlook | 獲利展望 |
| `capital_allocation` | Capital Allocation | 資本配置 |
| `risk_factors` | Risk Factors | 風險因子 |
| `management_guidance` | Management Guidance | 管理層指引 |
| `narrative_change` | Narrative Change | 敘事變化 |

### Evidence rule

Every finding’s `evidence_quote` must be a **verbatim substring** of the extracted text. Invented page numbers are cleared. Cross-quarter compare **never** treats “not mentioned” as “resolved”.

## Environment

Documented in `app_28.0/.Renviron.example`:

| Variable | Default | Role |
|---|---|---|
| `YNOW_AI_QUAL_ENABLED` | off | Master switch for live Workers AI and Markdown Conversion |
| `YNOW_AI_QUAL_MOCK` | off | Offline mock path for tests/demos (no network) |
| `YNOW_CF_ACCOUNT_ID` | empty | Cloudflare account ID. `CLOUDFLARE_ACCOUNT_ID` is a fallback |
| `YNOW_CF_API_TOKEN` | empty | Workers AI API token (Read + Edit). `CLOUDFLARE_API_TOKEN` is a fallback |
| `YNOW_CF_AI_MODEL` | `@cf/meta/llama-3.3-70b-instruct-fp8-fast` | Documented JSON-mode text model |
| `YNOW_CF_AI_MAX_TOKENS` | `2048` | Completion cap. Cloudflare's default of 256 truncates the findings JSON; values below 2048 are raised to 2048 |
| `YNOW_FNI_PARSE_BACKEND` | `local_cf` | `docling` selects the deferred Cloud Run hook (not implemented) |
| `YNOW_DOCLING_URL` | empty | Reserved for a future Cloud Run Docling origin. Unused today |
| `YNOW_AI_REQUEST_TIMEOUT` | `120` | HTTP timeout seconds |
| `YNOW_AI_MAX_FILE_MB` | `25` | Upload size cap |
| `YNOW_AI_MAX_CONCURRENT` | `1` | Live analyses in flight on this host (enforced) |
| `YNOW_AI_MAX_RUNS` | `8` | Live analyses per browser session per UTC day |
| `YNOW_AI_MAX_RUNS_HOST` | `32` | Live analyses for the whole host per UTC day |
| `YNOW_AI_MIN_INTERVAL_SEC` | `15` | Per-session cooldown between live runs. `0` disables it |
| `YNOW_AI_CACHE_DIR` | `<cache>/fni` | Result cache directory |

**Honest offline copy:** when the master switch is off or the account ID / API token is missing, the UI banner shows **AI service not configured** / **尚未設定 AI 服務**, with setup text that names Cloudflare Workers AI. No model call runs on page load.

Default model: [`@cf/meta/llama-3.3-70b-instruct-fp8-fast`](https://developers.cloudflare.com/workers-ai/features/json-mode/) (Workers AI JSON mode). Override with `YNOW_CF_AI_MODEL` if you need another JSON-mode model, such as `@cf/meta/llama-3.1-8b-instruct`.

### Document parse

1. `.txt`, `.md`, `.csv`, `.json` — read locally as UTF-8. No Cloudflare call.
2. `.pdf` — try `pdftools` when installed, then `pdftotext` when it is on `PATH`. A usable text layer stays local.
3. Anything else, or a PDF with no usable text — `POST https://api.cloudflare.com/client/v4/accounts/{account_id}/ai/tomarkdown` (multipart field `files`, `conversionOptions` asking for markdown).

### LLM

`POST https://api.cloudflare.com/client/v4/accounts/{account_id}/ai/run/{model}`

Body: `messages` (system + user) and `response_format.type = json_schema` using `ynow_fni_json_schema()` (summary + six-dimension findings). The client also accepts a Chat Completions `choices[0].message.content` JSON string if the gateway returns that shape.

Readiness probe (only when the master switch and credentials are set): `GET .../ai/tomarkdown/supported`. It checks the token. It does not run a completion. The result is cached for about 60 seconds. `YNOW_AI_QUAL_MOCK=1` skips the probe.

## Deferred: Google Cloud Run Docling

Not implemented. `ynow_fni_docling_cloudrun_convert()` is the extension seam. It returns `docling_cloudrun_not_implemented`.

Use it only if local extract plus Cloudflare Markdown Conversion cannot handle complex finance tables. A later change would set `YNOW_FNI_PARSE_BACKEND=docling` and `YNOW_DOCLING_URL` to the Cloud Run origin and implement the HTTP client inside that function. The default backend stays `local_cf`.

## Setup

1. Create a Cloudflare API token with **Workers AI Read** and **Workers AI Edit**. Copy the account ID from Workers & Pages.
2. Copy `app_28.0/.Renviron.example` → `app_28.0/.Renviron` and set `YNOW_AI_QUAL_ENABLED=1`, `YNOW_CF_ACCOUNT_ID`, and `YNOW_CF_API_TOKEN`.
3. Start the Shiny app; open **Financial Narrative Analysis**; paste or upload; press **Run analysis**.

No LLM or Markdown Conversion runs on page load — only after the user opens the surface and triggers Run.

## Example JSON result (shape)

```json
{
  "summary": "Qualitative summary grounded in extracted text.",
  "findings": [
    {
      "dimension": "management_guidance",
      "claim": "Management guides mid-single-digit growth.",
      "polarity": "neutral",
      "confidence": 0.7,
      "evidence_quote": "CFO guidance: mid-single-digit growth; no change to long-term WACC assumptions.",
      "evidence_source": "paste",
      "evidence_locator": "",
      "period": "2024Q3",
      "notes": "",
      "param_suggestion": null
    }
  ]
}
```

## Tests

```bash
cd app_28.0
YNOW_DEBUG_SKIP_PY=1 YNOW_AI_QUAL_MOCK=1 Rscript tests/test_financial_narrative.R
```

Covered offline: schema validation, evidence filter, chunking, mock pipeline, cache hit, cross-quarter “not resolved”, local text extract without credentials, PDF-without-text asks for Cloudflare, Cloud Run Docling hook stays unimplemented, Workers AI URL and JSON-schema body, UI wiring (menu / lazy host / Lite-free), locale key parity.

**Not claimed as live e2e:** Workers AI completion, Markdown Conversion of a real scanned PDF, or finance-table quality versus Docling.

## Limits & risks

- Long filings are chunked (`max_chunks` default 8); overflow sections are skipped with honest partial results.
- Model hallucinations are mitigated by evidence recheck, not eliminated.
- Catalog fetch reuses existing earnings-call helpers; empty catalog → honest empty.
- Text sent to Workers AI and non-local files sent to Markdown Conversion leave the machine. Do not enable the master switch on shinyapps until the host env holds the token.
- When the master switch and credentials are set, every visitor (Lite and Full, no membership check) can press Run. That spends the operator's Cloudflare account. Treat enabling credentials on public shinyapps as an accepted cost risk. Bounds, shared by Lite and Full: `YNOW_AI_MAX_FILE_MB`, `YNOW_AI_MAX_CONCURRENT`, `YNOW_AI_MAX_RUNS` (per session token, per UTC day), `YNOW_AI_MAX_RUNS_HOST` (per host, per UTC day), and `YNOW_AI_MIN_INTERVAL_SEC` (per-session cooldown). Cache hits do not consume a run. This is not a login wall.
- Each Workers AI request sends `max_tokens` from `YNOW_CF_AI_MAX_TOKENS` (floor 2048). The model default of 256 is not used, because it truncates the six-dimension JSON.
- Scanned PDFs and complex tables depend on Cloudflare Markdown Conversion. If that quality is not enough for finance tables, the Cloud Run Docling hook above is the planned next step — it is not built in this change.

## Files

| File | Role |
|---|---|
| `app_28.0/financial_narrative_schema.R` | Dimensions, JSON schema, evidence, cross-quarter |
| `app_28.0/financial_document_client.R` | Local extract, Markdown Conversion, Docling hook |
| `app_28.0/financial_llm_client.R` | Workers AI client + merge |
| `app_28.0/financial_narrative_cache.R` | Disk cache |
| `app_28.0/financial_narrative_service.R` | Pipeline + job entry |
| `app_28.0/financial_narrative_module.R` | Shiny UI/server |
| `app_28.0/tests/test_financial_narrative.R` | Targeted tests |
| `app_28.0/tests/fixtures/fni_*.txt` | Mock transcripts |
