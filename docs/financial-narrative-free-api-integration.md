# Financial Narrative Intelligence — free/open API integration

Live line: `app_28.0/`. Sidebar: **財務質性分析** / **Financial Narrative Analysis** (`tabName = financial_narrative`).  
Lite + Full: **free** (not in `ynow_sidebar_full_only_tabs` / Lite omit; no membership lock).

## Architecture

```
Upload / paste / earnings-call catalog
        │
        ▼
 financial_document_client.R   ← Docling Serve (or plain text / mock)
        │  chunk (section-aware)
        ▼
 financial_llm_client.R        ← Ollama /api/chat + JSON Schema (or mock)
        │  merge + evidence recheck
        ▼
 financial_narrative_schema.R  ← 6 dimensions, evidence rules, cross-quarter
        │
 financial_narrative_cache.R   ← key = hash + prompt_version + model (+ locale/ticker/period)
        │
 financial_narrative_module.R  ← Shiny UI (user-triggered Run only)
```

Phase 1 is **qualitative** plus optional `param_suggestion` fields. The app **never** auto-overwrites WACC / Ke / g / price.

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
| `YNOW_AI_QUAL_ENABLED` | off | Master switch for live Docling/Ollama |
| `YNOW_AI_QUAL_MOCK` | off | Offline mock path for tests/demos |
| `YNOW_DOCLING_URL` | `http://127.0.0.1:5001` | Docling Serve |
| `YNOW_DOCLING_API_KEY` | empty | Optional Bearer token |
| `YNOW_OLLAMA_URL` | `http://127.0.0.1:11434` | Ollama |
| `YNOW_OLLAMA_MODEL` | `qwen3.5:9b` | Chat model (pull manually) |
| `YNOW_AI_REQUEST_TIMEOUT` | `120` | HTTP timeout seconds |
| `YNOW_AI_MAX_FILE_MB` | `25` | Upload size cap |
| `YNOW_AI_MAX_CONCURRENT` | `1` | Soft concurrency hint |
| `YNOW_AI_CACHE_DIR` | `<cache>/fni` | Result cache directory |

**Honest offline copy:** when services are down or the master switch is off, the UI banner shows **AI service not configured** / **尚未設定 AI 服務**.

Free **software** (Docling, Ollama, qwen weights under their licenses) is not the same as free **cloud compute**. Localhost defaults do not imply shinyapps.io can host GPU inference for free.

## Local install notes

1. Install and run [Docling Serve](https://github.com/docling-project/docling-serve) on `127.0.0.1:5001` (or set `YNOW_DOCLING_URL`).
2. Install [Ollama](https://ollama.com/), then **manually** `ollama pull qwen3.5:9b` (the app does not auto-download models on Shiny boot).
3. Copy `app_28.0/.Renviron.example` → `app_28.0/.Renviron` and set `YNOW_AI_QUAL_ENABLED=1`.
4. Start the Shiny app; open **Financial Narrative Analysis**; paste or upload; press **Run analysis**.

No LLM/Docling work runs on page load — only after the user opens the surface and triggers Run.

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
YNOW_AI_QUAL_MOCK=1 Rscript tests/test_financial_narrative.R
```

Covered offline: schema validation, evidence filter, chunking, mock Docling/Ollama pipeline, cache hit, cross-quarter “not resolved”, UI wiring greps (menu / lazy host / Lite-free), locale key parity.

**Not claimed as e2e without hardware:** live Docling PDF convert, live Ollama `qwen3.5:9b` inference, TW FinmoConf PDF OCR quality, multi-GB model VRAM needs.

## Limits & risks

- Long filings are chunked (`max_chunks` default 8); overflow sections are skipped with honest partial results.
- Model hallucinations are mitigated by evidence recheck, not eliminated.
- Catalog fetch reuses existing earnings-call helpers; empty catalog → honest empty.
- Do not tip-ship / shinyapps-deploy this feature as a cloud LLM dependency.

## License posture

Docling, Ollama, and model weights each carry their own licenses. This module only HTTP-calls local endpoints you operate. User uploads stay on the machine running Shiny + those services; they are not sent to paid third-party LLM APIs by this code path.

## Files

| File | Role |
|---|---|
| `app_28.0/financial_narrative_schema.R` | Dimensions, JSON schema, evidence, cross-quarter |
| `app_28.0/financial_document_client.R` | Docling client + chunking |
| `app_28.0/financial_llm_client.R` | Ollama client + merge |
| `app_28.0/financial_narrative_cache.R` | Disk cache |
| `app_28.0/financial_narrative_service.R` | Pipeline + job entry |
| `app_28.0/financial_narrative_module.R` | Shiny UI/server |
| `app_28.0/tests/test_financial_narrative.R` | Targeted tests |
| `app_28.0/tests/fixtures/fni_*.txt` | Mock transcripts |
