#!/usr/bin/env Rscript
# Offline tests: earnings-call keyword lexicon + framework extractors/scorers.
# YNOW_DEBUG_SKIP_PY=1 — no reticulate.

Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")
app_dir <- normalizePath(file.path(".."), mustWork = TRUE)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
source(file.path(app_dir, "earnings_call_keyword_lexicon.R"), local = FALSE, encoding = "UTF-8")
source(file.path(app_dir, "earnings_call_keyword_framework.R"), local = FALSE, encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
    cat("OK ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

lex <- ynow_ec_keyword_lexicon()
check("lexicon has skipped steps 1 and 5", {
  all(c("1_full_text_map_scan", "5_equity_story_narrative") %in% (lex$skipped_steps %||% character(0)))
})
check("lexicon product focus is supplier/order/customer", {
  all(c("major_suppliers", "orders_backlog_bookings", "major_customers") %in%
        (lex$product_focus %||% character(0)))
})
check("lexicon encodes steps 2–4 + order focus + traps", {
  enc <- lex$encoded_steps %||% character(0)
  all(c(
    "2_presentation_deep_read",
    "3_qa_close_read",
    "4_tone_and_wording_delta",
    "order_focus_extract",
    "keyword_db_template",
    "reading_traps"
  ) %in% enc)
})
check("order/supplier/customer roles present", {
  all(c("orders", "suppliers", "customers", "positive", "warning", "capex", "bridging") %in% names(lex))
})

empty <- ynow_ec_keyword_analyze("")
check("blank text → honest empty", identical(empty$status, "empty") && !isTRUE(empty$ok))
check("empty keyword_db tone is NA", is.na(empty$keyword_db$tone_score %||% NA))

sample_zh <- paste(
  "本季毛利率為47.5%，主要受惠於先進製程出貨比例提升。",
  "我們預期下季毛利率將維持在47%至49%的區間。",
  "今年計畫資本支出上調，擴產重心在先進製程。",
  "在手訂單能見度約兩個季度；新訂單較上季改善。",
  "主要供應商供貨穩定，供應鏈缺料壓力緩解。",
  "主要客戶占營收約三成；新客戶導入中。",
  "客戶庫存持續調整中。需求持續強勁。",
  "分析師問答：定價方面我們持續維持紀律。但我更想強調的是產品組合優化。",
  "能否給我們更多量化的細節？",
  sep = ""
)

a <- ynow_ec_keyword_analyze(sample_zh, meta = list(
  source_id = "fugle_earnings_memo",
  credibility = "estimate",
  company = "2330",
  date = "2026-01-01"
))
check("analyze ok on zh sample", isTRUE(a$ok) && identical(a$status, "ok"))
check("skipped steps exposed on analysis", {
  all(c("1_full_text_map_scan", "5_equity_story_narrative") %in% (a$skipped_steps %||% character(0)))
})
check("presentation forward layer hit", length(a$presentation$number_layers$forward) >= 1L)
check("CapEx notes extracted", length(a$presentation$capex_notes) >= 1L)
check("CapEx raised question cue", "capex_raised" %in% (a$presentation$capex_questions_raised %||% character(0)))
check("bridging detected", length(a$qa$bridging_hits) >= 1L)
check("follow-up cue detected", length(a$qa$followup_hits) >= 1L)
check("tone score in 1–5", {
  s <- a$tone$tone_score %||% NA_integer_
  is.finite(s) && s >= 1L && s <= 5L
})
check("keyword_db has guidance_update", nzchar(a$keyword_db$guidance_update %||% ""))
check("positive hits non-empty", length(a$positive_hits) >= 1L)
check("themes include tone/capex/qa", {
  th <- a$themes %||% character(0)
  any(c("tone_scored", "capex_plan", "analyst_qa", "cfo_guidance") %in% th)
})

# Prior-period wording delta (inventory deepen).
prior <- "客戶庫存持續調整中，預計下半年恢復正常。"
cur <- "客戶庫存調整比原預期更深，正常化時程延後至明年上半年。我們目前預計毛利率為47.3%。"
td <- ynow_ec_kw_score_tone(cur, prior_text = prior)
check("inventory wording delta flagged", {
  "inventory_or_timeline_worse_than_prior" %in% (td$wording_deltas$flags %||% character(0))
})

# Trap 2: beat without guide upper bound.
trap2 <- ynow_ec_kw_detect_traps(
  "Results beat analyst expectations this quarter.",
  meta = list(source_id = "fugle_earnings_memo")
)
check("trap2 beat without guide upper", isTRUE(trap2$trap2_beat_without_guide_upper))

trap2_ok <- ynow_ec_kw_detect_traps(
  "Results beat analyst expectations and exceeded the high end of guidance.",
  meta = list(source_id = "fugle_earnings_memo")
)
check("trap2 clear when guide upper present", !isTRUE(trap2_ok$trap2_beat_without_guide_upper))

# Trap 3: precise number + hedge.
trap3 <- ynow_ec_kw_detect_traps(
  "我們目前預計下季毛利率為47.3%。"
)
check("trap3 precise number with hedge", isTRUE(trap3$trap3_number_without_tone))

# Trap 1: short summary-like text.
trap1 <- ynow_ec_kw_detect_traps(
  "法說會摘要：業績強勁。",
  meta = list(source_id = "media_recap")
)
check("trap1 summary-only", isTRUE(trap1$trap1_summary_only))

# Order-focus extract (surface buckets only).
focus <- ynow_ec_kw_extract_order_focus(sample_zh)
check("order-focus harvests order notes", length(focus$order_notes) >= 1L)
check("order-focus harvests supplier notes", length(focus$supplier_notes) >= 1L)
check("order-focus harvests customer notes", length(focus$customer_notes) >= 1L)

# Attach onto a fetch-shaped result without inventing amounts.
base <- list(
  ok = TRUE,
  status = "ok",
  source_id = "finmoconf_presentation_pdf",
  credibility = "estimate",
  backlog = NA_real_,
  bookings = NA_real_,
  capex = NA_real_,
  themes = c("order_status"),
  excerpt = substr(sample_zh, 1L, 80L)
)
attached <- ynow_ec_attach_keyword_analysis(base, text = sample_zh)
check("attach keeps backlog NA", !is.finite(attached$backlog %||% NA_real_))
check("attach surfaces order_focus theme", "order_focus" %in% (attached$themes %||% character(0)))
check("attach surfaces keyword_order_notes", length(attached$keyword_order_notes %||% character(0)) >= 1L)
check("attach surfaces keyword_supplier_notes", length(attached$keyword_supplier_notes %||% character(0)) >= 1L)
check("attach surfaces keyword_customer_notes", length(attached$keyword_customer_notes %||% character(0)) >= 1L)
check("attach does not surface tone score", is.null(attached$keyword_tone_score))
check("attach does not surface CapEx notes field", is.null(attached$keyword_capex_notes))
check("attach does not surface bridging notes field", is.null(attached$keyword_bridging_notes))
check("attach keeps tone internally", {
  is.list(attached$keyword_analysis) &&
    is.finite(attached$keyword_analysis$tone$tone_score %||% NA_integer_)
})
check("attach stamps info_updated_at", nzchar(attached$info_updated_at %||% ""))
check("parse event date YYYY-MM-DD", {
  identical(as.character(ynow_ec_parse_event_date("2026-10-21")), "2026-10-21")
})
check("rank newest first", {
  ranked <- ynow_ec_rank_by_event_date(
    list(
      list(published = "2026-03-01", source_id = "a"),
      list(published = "2026-10-21", source_id = "b")
    ),
    source_rank = c(1L, 2L)
  )
  identical(ranked[[1]]$source_id, "b")
})
check("stamp prefers source event date", {
  stamped <- ynow_ec_stamp_info_updated(list(published = "2026-10-21"))
  identical(stamped$info_updated_at, "2026-10-21") &&
    identical(stamped$info_updated_kind, "source_event")
})

# Pre-read prep optional fields.
prep <- ynow_ec_preread_prep_normalize(list(
  market_prior = "neutral",
  must_answer_questions = c("定價壓力", ""),
  prior_commitments = c("上季指引區間")
))
check("preread keeps market_prior", identical(prep$market_prior, "neutral"))
check("preread drops blank must-answer", identical(prep$must_answer_questions, "定價壓力"))

en_sample <- paste(
  "Gross margin was 47.5%, primarily due to mix. We expect next quarter margin in a 47% to 49% range.",
  "CapEx plan raised for capacity expansion. Inventory digestion continues.",
  "Question-and-Answer Session. What I would rather emphasize is product mix.",
  "Can you quantify the pricing pressure?",
  sep = " "
)
en_a <- ynow_ec_keyword_analyze(en_sample, meta = list(source_id = "licensed_api", credibility = "management"))
check("en sample analyzes ok", isTRUE(en_a$ok))
check("en bridging / follow-up", {
  length(en_a$qa$bridging_hits) >= 1L || length(en_a$qa$followup_hits) >= 1L
})

cat("\nPassed: ", pass, " Failed: ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1L)
quit(status = 0L)
