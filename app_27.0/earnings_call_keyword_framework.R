# ==========================================
# Earnings-call keyword framework (extractors)
# ==========================================
# Encodes reading steps 2–4 + pre-read prep + keyword-DB template + traps.
# Explicitly skips step 1 (full-text map scan) and step 5 (equity story /
# smile-curve narrative). Lexicon lives in earnings_call_keyword_lexicon.R.
# Status-only; never invent amounts. Prefer primary transcript/memo text.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

.ynow_ec_kw_chr <- function(x, default = "") {
  v <- as.character(x %||% default)[1]
  if (!nzchar(v) || is.na(v)) default else v
}

.ynow_ec_kw_split_sentences <- function(text) {
  txt <- as.character(text %||% "")[1]
  if (!nzchar(trimws(txt))) return(character(0))
  parts <- unlist(strsplit(txt, "[。！？.!?\n\r]+", perl = TRUE))
  parts <- trimws(gsub("\\s+", " ", parts))
  parts[nzchar(parts) & nchar(parts) >= 6L & nchar(parts) <= 400L]
}

.ynow_ec_kw_pick_sentences <- function(parts, patterns, max_n = 5L) {
  if (!length(parts) || !length(patterns)) return(character(0))
  hit <- parts[vapply(parts, function(s) {
    any(vapply(patterns, function(p) grepl(p, s, perl = TRUE, ignore.case = TRUE), logical(1)))
  }, logical(1))]
  unique(utils::head(hit, max_n))
}

#' Optional pre-read prep fields (priors / 必答題 / prior commitments).
#' Empty strings/lists are honest empty — never invent.
ynow_ec_preread_prep_empty <- function() {
  list(
    market_prior = "",           # optimistic / neutral / pessimistic / ""
    must_answer_questions = character(0),
    prior_commitments = character(0),
    prior_transcript_excerpt = "",
    competitor_notes = character(0)
  )
}

#' Normalize optional prior list; drop blank junk.
ynow_ec_preread_prep_normalize <- function(prior = NULL) {
  base <- ynow_ec_preread_prep_empty()
  if (!is.list(prior) || !length(prior)) return(base)
  base$market_prior <- .ynow_ec_kw_chr(prior$market_prior)
  mq <- as.character(unlist(prior$must_answer_questions %||% character(0)))
  base$must_answer_questions <- unique(trimws(mq[nzchar(trimws(mq))]))
  pc <- as.character(unlist(prior$prior_commitments %||% character(0)))
  base$prior_commitments <- unique(trimws(pc[nzchar(trimws(pc))]))
  base$prior_transcript_excerpt <- .ynow_ec_kw_chr(prior$prior_transcript_excerpt)
  cn <- as.character(unlist(prior$competitor_notes %||% character(0)))
  base$competitor_notes <- unique(trimws(cn[nzchar(trimws(cn))]))
  base
}

#' Step 2 — presentation deep-read: number layers, segment mix, CapEx cues.
ynow_ec_kw_analyze_presentation <- function(text, lex = NULL) {
  lex <- lex %||% (if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  })
  parts <- .ynow_ec_kw_split_sentences(text)
  empty <- list(
    number_layers = list(raw = character(0), explain = character(0), forward = character(0)),
    segment_mix_notes = character(0),
    capex_notes = character(0),
    capex_questions_raised = character(0)
  )
  if (!length(parts)) return(empty)

  nl <- lex$number_layer %||% list()
  raw_pat <- c(nl$raw$zh %||% character(0), nl$raw$en %||% character(0))
  exp_pat <- c(nl$explain$zh %||% character(0), nl$explain$en %||% character(0))
  fwd_pat <- c(nl$forward$zh %||% character(0), nl$forward$en %||% character(0))

  # Prefer sentences that contain a digit for "raw" layer.
  with_num <- parts[grepl("[0-9]", parts)]
  raw_hits <- .ynow_ec_kw_pick_sentences(with_num, raw_pat, 4L)
  explain_hits <- .ynow_ec_kw_pick_sentences(parts, exp_pat, 4L)
  forward_hits <- .ynow_ec_kw_pick_sentences(parts, fwd_pat, 4L)

  seg_pat <- c(
    (lex$segment_cues$zh %||% character(0)),
    (lex$segment_cues$en %||% character(0))
  )
  capex_pat <- c(
    (lex$capex$zh %||% character(0)),
    (lex$capex$en %||% character(0))
  )
  cue_pat <- c(
    (lex$capex_question_cues$zh %||% character(0)),
    (lex$capex_question_cues$en %||% character(0))
  )

  capex_notes <- .ynow_ec_kw_pick_sentences(parts, capex_pat, 5L)
  raised <- character(0)
  if (length(capex_notes)) {
    # Surface which CapEx analytical questions the text touches (not invented answers).
    if (any(grepl("今年|this year|年度", capex_notes, ignore.case = TRUE, perl = TRUE))) {
      raised <- c(raised, "capex_plan_amount_yoy")
    }
    if (any(grepl("分配|重心|allocation|expansion|擴產|產能", capex_notes, ignore.case = TRUE, perl = TRUE))) {
      raised <- c(raised, "capex_allocation_focus")
    }
    if (any(grepl("削減|cut|discipline|資本紀律|不如預期", capex_notes, ignore.case = TRUE, perl = TRUE))) {
      raised <- c(raised, "capex_cut_explanation")
    }
    if (any(grepl("上調|raise|increase|上修", capex_notes, ignore.case = TRUE, perl = TRUE))) {
      raised <- c(raised, "capex_raised")
    }
    if (any(grepl(paste(cue_pat, collapse = "|"), paste(capex_notes, collapse = " "),
                  ignore.case = TRUE, perl = TRUE))) {
      # already covered by above; keep unique
    }
  }

  list(
    number_layers = list(
      raw = raw_hits,
      explain = explain_hits,
      forward = forward_hits
    ),
    segment_mix_notes = .ynow_ec_kw_pick_sentences(parts, seg_pat, 4L),
    capex_notes = capex_notes,
    capex_questions_raised = unique(raised)
  )
}

#' Step 3 — Q&A close-read: priority cues, bridging, follow-up value.
ynow_ec_kw_analyze_qa <- function(text, lex = NULL) {
  lex <- lex %||% (if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  })
  txt <- as.character(text %||% "")[1]
  parts <- .ynow_ec_kw_split_sentences(txt)
  empty <- list(
    has_qa_section = FALSE,
    bridging_hits = character(0),
    followup_hits = character(0),
    avoided_or_short_answers = character(0),
    analyst_focus_cues = character(0),
    priority_order = c(
      "multi_analyst_topic",
      "management_extended_answer",
      "numeric_answer",
      "routine_question"
    )
  )
  if (!nzchar(txt)) return(empty)

  has_qa <- grepl(
    "(?i)question[- ]and[- ]answer|\\bQ\\s*&\\s*A\\b|analyst\\s+question|問答|分析師",
    txt,
    perl = TRUE
  )
  bridge_pat <- c(lex$bridging$zh %||% character(0), lex$bridging$en %||% character(0))
  follow_pat <- c(lex$qa_followup$zh %||% character(0), lex$qa_followup$en %||% character(0))
  warn_pat <- c(lex$warning$zh %||% character(0), lex$warning$en %||% character(0))

  bridging <- .ynow_ec_kw_pick_sentences(parts, bridge_pat, 5L)
  followups <- .ynow_ec_kw_pick_sentences(parts, follow_pat, 5L)

  # Short answers after a specific question mark / ？ cue — heuristic only.
  avoided <- character(0)
  if (length(parts) >= 2L) {
    for (i in seq_len(length(parts) - 1L)) {
      q <- parts[[i]]
      a <- parts[[i + 1L]]
      if (grepl("\\?|？|能否|可否|how much|can you", q, ignore.case = TRUE, perl = TRUE) &&
            nchar(a) <= 40L &&
            grepl(paste(bridge_pat, collapse = "|"), a, ignore.case = TRUE, perl = TRUE)) {
        avoided <- c(avoided, a)
      }
    }
  }
  avoided <- unique(utils::head(avoided, 4L))

  focus <- .ynow_ec_kw_pick_sentences(parts, warn_pat, 4L)
  if (length(followups)) {
    focus <- unique(c(utils::head(followups, 2L), focus))
  }

  list(
    has_qa_section = isTRUE(has_qa),
    bridging_hits = bridging,
    followup_hits = followups,
    avoided_or_short_answers = avoided,
    analyst_focus_cues = utils::head(focus, 5L),
    priority_order = empty$priority_order
  )
}

#' Step 4 — tone score 1–5 + optional cross-period wording delta.
#' @param text current period text
#' @param prior_text optional prior-period text for delta
ynow_ec_kw_score_tone <- function(text, prior_text = NULL, lex = NULL) {
  lex <- lex %||% (if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  })
  txt <- as.character(text %||% "")[1]
  out <- list(
    tone_score = NA_integer_,
    tone_label = "",
    tone_hits = list(),
    wording_deltas = list(),
    guidance_band_notes = character(0),
    inventory_language_notes = character(0)
  )
  if (!nzchar(txt)) return(out)

  anchors <- lex$tone_anchors %||% list()
  scores <- integer(0)
  hits <- list()
  for (lvl in c("5", "4", "3", "2", "1")) {
    node <- anchors[[lvl]]
    if (is.null(node)) next
    pats <- c(node$zh %||% character(0), node$en %||% character(0))
    matched <- character(0)
    for (p in pats) {
      if (nzchar(p) && grepl(p, txt, perl = TRUE, ignore.case = TRUE)) {
        matched <- c(matched, p)
      }
    }
    if (length(matched)) {
      scores <- c(scores, as.integer(lvl))
      hits[[lvl]] <- unique(matched)
    }
  }
  if (length(scores)) {
    # Prefer extremes when both present; else mean rounded.
    if (any(scores <= 2L) && any(scores >= 4L)) {
      # Mixed → pull toward 3 unless one side dominates.
      out$tone_score <- as.integer(round(mean(scores)))
    } else {
      out$tone_score <- as.integer(round(mean(scores)))
    }
  } else {
    # Fallback: positive vs warning token balance.
    pos_n <- if (exists("ynow_ec_lexicon_hit_count", mode = "function")) {
      ynow_ec_lexicon_hit_count(txt, "positive", lex)
    } else {
      0L
    }
    warn_n <- if (exists("ynow_ec_lexicon_hit_count", mode = "function")) {
      ynow_ec_lexicon_hit_count(txt, "warning", lex)
    } else {
      0L
    }
    if (pos_n + warn_n > 0L) {
      ratio <- pos_n / (pos_n + warn_n)
      out$tone_score <- as.integer(max(1L, min(5L, round(1 + 4 * ratio))))
    }
  }
  labels <- c(
    "1" = "pessimistic",
    "2" = "cautious",
    "3" = "neutral",
    "4" = "optimistic",
    "5" = "very_optimistic"
  )
  if (is.finite(out$tone_score)) {
    out$tone_label <- labels[[as.character(out$tone_score)]] %||% ""
  }
  out$tone_hits <- hits

  parts <- .ynow_ec_kw_split_sentences(txt)
  guide_pat <- c(lex$guidance_verbs$zh %||% character(0), lex$guidance_verbs$en %||% character(0))
  inv_pat <- c(
    lex$inventory_pricing$zh %||% character(0),
    lex$inventory_pricing$en %||% character(0)
  )
  out$guidance_band_notes <- .ynow_ec_kw_pick_sentences(parts, guide_pat, 4L)
  out$inventory_language_notes <- .ynow_ec_kw_pick_sentences(parts, inv_pat, 4L)

  prior <- .ynow_ec_kw_chr(prior_text)
  if (nzchar(prior)) {
    deltas <- list()
    # Guidance band narrowing / shift heuristics (status strings only).
    cur_guide <- paste(out$guidance_band_notes, collapse = " | ")
    prior_parts <- .ynow_ec_kw_split_sentences(prior)
    prior_guide <- .ynow_ec_kw_pick_sentences(prior_parts, guide_pat, 4L)
    if (length(out$guidance_band_notes) && length(prior_guide)) {
      deltas$guidance <- list(
        prior = utils::head(prior_guide, 2L),
        current = utils::head(out$guidance_band_notes, 2L)
      )
    }
    prior_inv <- .ynow_ec_kw_pick_sentences(prior_parts, inv_pat, 4L)
    if (length(out$inventory_language_notes) && length(prior_inv)) {
      deltas$inventory <- list(
        prior = utils::head(prior_inv, 2L),
        current = utils::head(out$inventory_language_notes, 2L)
      )
    }
    # Soften / deepen language flags.
    deepen <- grepl("比原預期更深|deeper than expected|延後|push(?:ed)? out",
                    txt, ignore.case = TRUE, perl = TRUE)
    if (isTRUE(deepen)) {
      deltas$flags <- c(deltas$flags %||% character(0), "inventory_or_timeline_worse_than_prior")
    }
    out$wording_deltas <- deltas
  }
  out
}

#' Trap flags from the reading framework (do not invent facts).
ynow_ec_kw_detect_traps <- function(text, meta = NULL, lex = NULL) {
  lex <- lex %||% (if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  })
  txt <- as.character(text %||% "")[1]
  meta <- meta %||% list()
  flags <- list(
    trap1_summary_only = FALSE,
    trap2_beat_without_guide_upper = FALSE,
    trap3_number_without_tone = FALSE,
    notes = character(0)
  )
  if (!nzchar(txt)) return(flags)

  # Trap 1: source looks like a media summary, not primary memo/transcript/PDF.
  src <- tolower(.ynow_ec_kw_chr(meta$source_id))
  summary_pat <- c(
    lex$traps$summary_only$zh %||% character(0),
    lex$traps$summary_only$en %||% character(0)
  )
  if (nzchar(src) && grepl("summary|recap|highlights|media", src, perl = TRUE)) {
    flags$trap1_summary_only <- TRUE
    flags$notes <- c(flags$notes, "prefer_primary_transcript_or_memo")
  } else if (length(summary_pat) &&
               grepl(paste(summary_pat, collapse = "|"), txt, ignore.case = TRUE, perl = TRUE) &&
               nchar(txt) < 800L) {
    flags$trap1_summary_only <- TRUE
    flags$notes <- c(flags$notes, "short_summary_like_text")
  }

  # Trap 2: "beat analyst expectations" without management guide upper-bound context.
  beat_pat <- c(
    lex$traps$beat_vs_guide$zh %||% character(0),
    lex$traps$beat_vs_guide$en %||% character(0)
  )
  has_beat <- length(beat_pat) &&
    grepl(paste(beat_pat, collapse = "|"), txt, ignore.case = TRUE, perl = TRUE)
  has_guide_upper <- grepl(
    "(?i)(?:above|exceed(?:ed|s)?)\\s+(?:the\\s+)?(?:high\\s+end|upper\\s+(?:end|bound)|guidance)|財測上緣|指引上緣|上緣",
    txt,
    perl = TRUE
  )
  if (isTRUE(has_beat) && !isTRUE(has_guide_upper)) {
    flags$trap2_beat_without_guide_upper <- TRUE
    flags$notes <- c(flags$notes, "check_vs_management_guide_upper_bound")
  }

  # Trap 3: precise forward number with hedge language nearby.
  has_precise <- grepl(
    "(?i)(?:毛利率|gross margin|EPS|營收|revenue)[^。\\n]{0,40}[0-9]+(?:\\.[0-9]+)?\\s*%?",
    txt,
    perl = TRUE
  )
  hedge_pat <- c(lex$hedge$zh %||% character(0), lex$hedge$en %||% character(0))
  has_hedge <- length(hedge_pat) &&
    grepl(paste(hedge_pat, collapse = "|"), txt, ignore.case = TRUE, perl = TRUE)
  if (isTRUE(has_precise) && isTRUE(has_hedge)) {
    flags$trap3_number_without_tone <- TRUE
    flags$notes <- c(flags$notes, "pair_precise_number_with_hedging_language")
  }
  flags
}

#' Keyword-DB template fields (status-only excerpts; never invent amounts).
ynow_ec_keyword_db_template <- function(analysis) {
  a <- analysis %||% list()
  tone <- a$tone %||% list()
  qa <- a$qa %||% list()
  pos <- as.character(a$positive_hits %||% character(0))
  risk <- as.character(a$warning_hits %||% character(0))
  guide_notes <- as.character(tone$guidance_band_notes %||% character(0))
  guide_update <- ""
  if (length(guide_notes)) {
    joined <- paste(guide_notes, collapse = " ")
    if (grepl("上修|上調|raise|raised|increase(?:d)?\\s+guidance", joined, ignore.case = TRUE, perl = TRUE)) {
      guide_update <- "raised"
    } else if (grepl("下修|下調|lower|lowered|cut\\s+guidance", joined, ignore.case = TRUE, perl = TRUE)) {
      guide_update <- "lowered"
    } else if (grepl("維持|reaffirm|maintain|unchanged", joined, ignore.case = TRUE, perl = TRUE)) {
      guide_update <- "maintained"
    } else {
      guide_update <- "stated"
    }
  }
  delta_txt <- ""
  wd <- tone$wording_deltas %||% list()
  if (length(wd)) {
    bits <- character(0)
    if (!is.null(wd$guidance)) {
      bits <- c(bits, paste0(
        "guidance: ",
        paste(wd$guidance$prior %||% character(0), collapse = " / "),
        " → ",
        paste(wd$guidance$current %||% character(0), collapse = " / ")
      ))
    }
    if (!is.null(wd$inventory)) {
      bits <- c(bits, paste0(
        "inventory: ",
        paste(wd$inventory$prior %||% character(0), collapse = " / "),
        " → ",
        paste(wd$inventory$current %||% character(0), collapse = " / ")
      ))
    }
    if (length(wd$flags)) bits <- c(bits, paste(wd$flags, collapse = ","))
    delta_txt <- paste(bits, collapse = "; ")
  }
  list(
    company = .ynow_ec_kw_chr(a$company),
    date = .ynow_ec_kw_chr(a$date),
    guidance_update = guide_update,
    top_positives = utils::head(pos, 3L),
    top_risks = utils::head(risk, 3L),
    wording_delta = delta_txt,
    analyst_focus = utils::head(as.character(qa$analyst_focus_cues %||% character(0)), 3L),
    avoided_questions = utils::head(as.character(qa$avoided_or_short_answers %||% character(0)), 3L),
    tone_score = if (is.finite(tone$tone_score %||% NA_real_)) as.integer(tone$tone_score) else NA_integer_
  )
}

#' Full keyword-framework analysis on primary transcript / memo / deck text.
#' @param text primary source text (prefer memo/transcript/PDF extract)
#' @param prior optional list from ynow_ec_preread_prep_normalize / prior_text
#' @param meta optional list(source_id, credibility, company, date, …)
#' @return structured list for extractors / 業務拆解; honest empty when blank
ynow_ec_keyword_analyze <- function(text, prior = NULL, meta = NULL) {
  lex <- if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  }
  txt <- as.character(text %||% "")[1]
  meta <- meta %||% list()
  prep <- ynow_ec_preread_prep_normalize(prior)
  prior_text <- .ynow_ec_kw_chr(
    if (nzchar(prep$prior_transcript_excerpt)) prep$prior_transcript_excerpt
    else prior$prior_text %||% ""
  )

  empty <- list(
    ok = FALSE,
    status = "empty",
    framework_version = .ynow_ec_kw_chr(lex$version, "1"),
    skipped_steps = lex$skipped_steps %||% c("1_full_text_map_scan", "5_equity_story_narrative"),
    encoded_steps = lex$encoded_steps %||% character(0),
    preread = prep,
    presentation = ynow_ec_kw_analyze_presentation("", lex),
    qa = ynow_ec_kw_analyze_qa("", lex),
    tone = ynow_ec_kw_score_tone("", NULL, lex),
    traps = ynow_ec_kw_detect_traps("", meta, lex),
    positive_hits = character(0),
    warning_hits = character(0),
    keyword_db = NULL,
    themes = character(0),
    credibility = .ynow_ec_kw_chr(meta$credibility, "estimate"),
    source_id = .ynow_ec_kw_chr(meta$source_id),
    company = .ynow_ec_kw_chr(meta$company),
    date = .ynow_ec_kw_chr(meta$date %||% meta$published)
  )
  if (!nzchar(trimws(txt))) {
    empty$keyword_db <- ynow_ec_keyword_db_template(empty)
    return(empty)
  }

  presentation <- ynow_ec_kw_analyze_presentation(txt, lex)
  qa <- ynow_ec_kw_analyze_qa(txt, lex)
  tone <- ynow_ec_kw_score_tone(txt, prior_text, lex)
  traps <- ynow_ec_kw_detect_traps(txt, meta, lex)
  pos <- if (exists("ynow_ec_lexicon_hits", mode = "function")) {
    ynow_ec_lexicon_hits(txt, "positive", lex, 8L)
  } else {
    character(0)
  }
  warn <- if (exists("ynow_ec_lexicon_hits", mode = "function")) {
    ynow_ec_lexicon_hits(txt, "warning", lex, 8L)
  } else {
    character(0)
  }

  themes <- character(0)
  if (length(presentation$capex_notes)) themes <- c(themes, "capex_plan")
  if (length(presentation$number_layers$forward) || length(tone$guidance_band_notes)) {
    themes <- c(themes, "cfo_guidance")
  }
  if (isTRUE(qa$has_qa_section) || length(qa$followup_hits) || length(qa$bridging_hits)) {
    themes <- c(themes, "analyst_qa")
  }
  if (length(presentation$segment_mix_notes)) themes <- c(themes, "segment_mix")
  if (is.finite(tone$tone_score %||% NA_real_)) themes <- c(themes, "tone_scored")
  if (isTRUE(traps$trap1_summary_only) || isTRUE(traps$trap2_beat_without_guide_upper) ||
        isTRUE(traps$trap3_number_without_tone)) {
    themes <- c(themes, "reading_trap_flag")
  }

  analysis <- list(
    ok = TRUE,
    status = "ok",
    framework_version = .ynow_ec_kw_chr(lex$version, "1"),
    skipped_steps = lex$skipped_steps %||% c("1_full_text_map_scan", "5_equity_story_narrative"),
    encoded_steps = lex$encoded_steps %||% character(0),
    preread = prep,
    presentation = presentation,
    qa = qa,
    tone = tone,
    traps = traps,
    positive_hits = pos,
    warning_hits = warn,
    keyword_db = NULL,
    themes = unique(themes),
    credibility = .ynow_ec_kw_chr(meta$credibility, "estimate"),
    source_id = .ynow_ec_kw_chr(meta$source_id),
    company = .ynow_ec_kw_chr(meta$company),
    date = .ynow_ec_kw_chr(meta$date %||% meta$published)
  )
  analysis$keyword_db <- ynow_ec_keyword_db_template(analysis)
  analysis
}

#' Safe attach for fetch builders (no-op if analyzer missing / errors).
.ynow_ec_finalize_with_keywords <- function(result, text = NULL, prior = NULL) {
  if (!is.list(result)) return(result)
  if (!exists("ynow_ec_attach_keyword_analysis", mode = "function")) return(result)
  tryCatch(
    ynow_ec_attach_keyword_analysis(result, text = text, prior = prior),
    error = function(e) result
  )
}

#' Parse a source event / published date to Date (NA if unknown).
#' Accepts YYYY-MM-DD, YYYY/MM/DD, YYYYMMDD, ISO datetime prefixes.
ynow_ec_parse_event_date <- function(x) {
  s <- trimws(as.character(x %||% "")[1])
  if (!nzchar(s) || is.na(s)) return(as.Date(NA))
  s <- sub("T.*$", "", s)
  s <- sub(" .*", "", s)
  s <- gsub("/", "-", s, fixed = TRUE)
  if (grepl("^[0-9]{8}$", s)) {
    s <- paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-", substr(s, 7, 8))
  }
  d <- suppressWarnings(as.Date(s))
  if (length(d) != 1L || is.na(d)) as.Date(NA) else d
}

#' Rank earnings-call fetch results newest-first by published/event date.
#' Unknown dates sort last. Optional source_rank breaks ties (lower = preferred).
ynow_ec_rank_by_event_date <- function(results, source_rank = NULL) {
  if (!is.list(results) || !length(results)) return(list())
  n <- length(results)
  dates <- vapply(results, function(r) {
    d <- ynow_ec_parse_event_date(r$published %||% r$date %||% r$event_date %||% "")
    if (is.na(d)) -Inf else as.numeric(d)
  }, numeric(1))
  ranks <- if (is.null(source_rank)) {
    rep(99L, n)
  } else {
    as.integer(source_rank)[seq_len(n)]
  }
  ranks[!is.finite(ranks)] <- 99L
  ord <- order(-dates, ranks, seq_len(n))
  results[ord]
}

#' Stamp 資訊更新日期 fields: prefer source event date; else fetch/analysis time.
ynow_ec_stamp_info_updated <- function(result, fetched_at = NULL) {
  if (!is.list(result)) return(result)
  event <- .ynow_ec_kw_chr(result$published %||% result$event_date %||% result$date)
  event_d <- ynow_ec_parse_event_date(event)
  fetched <- .ynow_ec_kw_chr(fetched_at)
  if (!nzchar(fetched)) {
    fetched <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  result$fetched_at <- fetched
  if (!is.na(event_d)) {
    result$info_updated_at <- format(event_d, "%Y-%m-%d")
    result$info_updated_kind <- "source_event"
  } else if (nzchar(event)) {
    result$info_updated_at <- event
    result$info_updated_kind <- "source_event_raw"
  } else {
    # Honest: show analysis/fetch day when event date unknown.
    result$info_updated_at <- substr(fetched, 1L, 10L)
    result$info_updated_kind <- "fetched_at"
  }
  result$analysis_axis <- "latest_earnings_call"
  result
}

#' Status-only sentence harvest for 訂單／主要供應商／主要客戶 (never invent amounts).
ynow_ec_kw_extract_order_focus <- function(text, lex = NULL, max_n = 6L) {
  lex <- lex %||% (if (exists("ynow_ec_keyword_lexicon", mode = "function")) {
    ynow_ec_keyword_lexicon()
  } else {
    list()
  })
  empty <- list(
    order_notes = character(0),
    supplier_notes = character(0),
    customer_notes = character(0)
  )
  parts <- .ynow_ec_kw_split_sentences(text)
  if (!length(parts)) return(empty)
  role_pats <- function(role) {
    node <- lex[[role]]
    if (is.null(node)) return(character(0))
    c(as.character(node$zh %||% character(0)), as.character(node$en %||% character(0)))
  }
  list(
    order_notes = .ynow_ec_kw_pick_sentences(parts, role_pats("orders"), max_n),
    supplier_notes = .ynow_ec_kw_pick_sentences(parts, role_pats("suppliers"), max_n),
    customer_notes = .ynow_ec_kw_pick_sentences(parts, role_pats("customers"), max_n)
  )
}

#' Trap flags kept only when they matter for supplier / order / customer reading.
.ynow_ec_kw_order_focus_traps <- function(analysis, focus) {
  traps <- analysis$traps %||% list()
  flags <- character(0)
  # Trap1 always relevant: prefer primary transcript/memo for order facts.
  if (isTRUE(traps$trap1_summary_only)) flags <- c(flags, "trap1_summary_only")
  has_order_topic <- length(focus$order_notes) > 0L ||
    length(focus$supplier_notes) > 0L ||
    length(focus$customer_notes) > 0L
  # Trap2/3 only when the text is also about orders/customers/suppliers.
  if (isTRUE(has_order_topic) && isTRUE(traps$trap2_beat_without_guide_upper)) {
    flags <- c(flags, "trap2_beat_without_guide_upper")
  }
  if (isTRUE(has_order_topic) && isTRUE(traps$trap3_number_without_tone)) {
    flags <- c(flags, "trap3_number_without_tone")
  }
  unique(flags)
}

#' Attach keyword analysis onto an earnings-call facts / fetch result.
#' Product surface: only 訂單／主要供應商／主要客戶 (+ order-relevant traps).
#' Tone / CapEx / bridging / token walls stay inside keyword_analysis (internal).
#' Does not overwrite numeric amounts. Honest no-op if blank.
ynow_ec_attach_keyword_analysis <- function(result, text = NULL, prior = NULL) {
  if (!is.list(result)) return(result)
  txt <- .ynow_ec_kw_chr(text)
  if (!nzchar(txt)) {
    txt <- .ynow_ec_kw_chr(result$excerpt)
    # Prefer longer primary body if callers stash it.
    body <- .ynow_ec_kw_chr(result$`_content` %||% result$text %||% result$body)
    if (nchar(body) > nchar(txt)) txt <- body
  }
  meta <- list(
    source_id = result$source_id,
    credibility = result$credibility,
    company = result$company %||% result$ticker %||% result$title,
    date = result$published %||% result$date
  )
  analysis <- ynow_ec_keyword_analyze(txt, prior = prior, meta = meta)
  focus <- ynow_ec_kw_extract_order_focus(txt)
  analysis$order_focus <- focus
  result$keyword_analysis <- analysis

  # Surface fields for 業務拆解 → order-research (status-only).
  result$keyword_order_notes <- focus$order_notes
  result$keyword_supplier_notes <- focus$supplier_notes
  result$keyword_customer_notes <- focus$customer_notes
  result$keyword_trap_flags <- .ynow_ec_kw_order_focus_traps(analysis, focus)

  # Enrich existing note buckets without inventing amounts / overwriting parties.
  result$order_notes <- unique(c(
    .ynow_ec_kw_chr_vec_safe(result$order_notes),
    focus$order_notes
  ))
  result$supply_chain_notes <- unique(c(
    .ynow_ec_kw_chr_vec_safe(result$supply_chain_notes),
    focus$supplier_notes
  ))
  # Customer concentration status lives beside outlook when no dedicated bucket.
  if (length(focus$customer_notes)) {
    result$customer_notes <- unique(c(
      .ynow_ec_kw_chr_vec_safe(result$customer_notes),
      focus$customer_notes
    ))
  }

  themes <- as.character(result$themes %||% character(0))
  if (length(focus$order_notes)) themes <- c(themes, "order_status")
  if (length(focus$supplier_notes)) themes <- c(themes, "supply_chain_status")
  if (length(focus$customer_notes)) themes <- c(themes, "customer_concentration_status")
  if (length(result$keyword_trap_flags)) themes <- c(themes, "reading_trap_flag")
  result$themes <- unique(c(themes, "order_focus"))

  # Do not surface general tone / token / CapEx / bridging on the result object.
  result$keyword_tone_score <- NULL
  result$keyword_guidance_update <- NULL
  result$keyword_top_positives <- NULL
  result$keyword_top_risks <- NULL
  result$keyword_bridging_notes <- NULL
  result$keyword_capex_notes <- NULL

  # 資訊更新日期: source event date preferred; fetch time as fallback.
  result <- ynow_ec_stamp_info_updated(result)
  result
}

.ynow_ec_kw_chr_vec_safe <- function(x) {
  if (is.null(x)) return(character(0))
  v <- as.character(unlist(x))
  v <- trimws(v)
  v[nzchar(v) & !is.na(v)]
}
