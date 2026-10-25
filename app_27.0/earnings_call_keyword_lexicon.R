# ==========================================
# Earnings-call keyword lexicon (internal)
# ==========================================
# Machine-readable zh-TW + en-US token lists for transcript / presentation
# extractors and scorers. Not a user-facing wall of text.
# Derived methodology notes from a public reading framework; do not republish
# third-party article prose. Skip framework steps 1 (full-text map scan) and
# 5 (equity-story / smile-curve narrative). Encode steps 2–4 + keyword DB
# template + reading traps.
# Never invent amounts. Status-only facts feed 業務拆解 / order-research.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' Internal lexicon: paired zh / en tokens by role.
#' @return named list of character vectors (regex-friendly fragments).
ynow_ec_keyword_lexicon <- function() {
  list(
    version = "1",
    # Framework coverage note (machine metadata, not UI copy).
    skipped_steps = c("1_full_text_map_scan", "5_equity_story_narrative"),
    encoded_steps = c(
      "pre_read_prep",
      "2_presentation_deep_read",
      "3_qa_close_read",
      "4_tone_and_wording_delta",
      "keyword_db_template",
      "reading_traps"
    ),
    positive = list(
      zh = c(
        "創紀錄", "加速", "超預期", "強勁", "份額提升", "市占提升",
        "定價能力", "定價紀律", "新客戶", "新市場", "加快", "提前",
        "優於預期", "優化", "改善", "回溫", "復甦", "可見度提升"
      ),
      en = c(
        "record", "record[- ]high", "accelerat", "above expectations",
        "beat expectations", "strong demand", "share gain", "pricing power",
        "new customer", "new market", "pull[- ]?forward", "ahead of schedule",
        "better than expected", "improv", "recover", "visibility improv"
      )
    ),
    warning = list(
      zh = c(
        "挑戰", "壓力", "不確定", "調整中", "庫存", "砍單", "延遲",
        "一次性", "費用增加", "謹慎", "保守", "觀察", "疲弱", "下滑",
        "能見度受限", "能見度降低", "延後", "比原預期更深"
      ),
      en = c(
        "challeng", "headwind", "pressure", "uncertain", "inventory",
        "order cut", "cancel", "delay", "one[- ]time", "expense increase",
        "cautious", "conservative", "monitor", "weak", "soft",
        "limited visibility", "push(?:ed)? out", "deeper than expected"
      )
    ),
    capex = list(
      zh = c(
        "資本支出", "CapEx", "capex", "產能擴充", "擴產", "設備投資",
        "資本紀律", "資本配置", "資本計畫"
      ),
      en = c(
        "capital expenditures?", "\\bCapEx\\b", "capex",
        "capacity expansion", "fab build", "equipment investment",
        "capital discipline", "capital allocation", "capex plan"
      )
    ),
    inventory_pricing = list(
      zh = c(
        "庫存調整", "客戶庫存", "通路庫存", "去化", "定價壓力",
        "價格壓力", "ASP", "毛利率", "產品組合"
      ),
      en = c(
        "inventory digestion", "channel inventory", "customer inventory",
        "pricing pressure", "ASP", "gross margin", "product mix"
      )
    ),
    guidance_verbs = list(
      zh = c(
        "預期", "展望", "指引", "財測", "上修", "下修", "維持",
        "區間", "目標", "我們預計", "我們目前預計", "我們有信心"
      ),
      en = c(
        "guid(?:e|ance|ing)", "outlook", "expect(?:s|ed|ing)?",
        "raise(?:d|s)?", "lower(?:ed|s)?", "reaffirm", "maintain",
        "range", "we expect", "we currently expect", "confident"
      )
    ),
    bridging = list(
      zh = c(
        "但我更想強調", "更重要的是", "讓我換個角度", "先說一下",
        "回到我們的重點", "我想強調的是", "另一方面"
      ),
      en = c(
        "what I(?:'d| would) rather emphasize", "more importantly",
        "let me pivot", "stepping back", "the bigger picture",
        "I(?:'d| would) highlight", "on the other hand"
      )
    ),
    hedge = list(
      zh = c(
        "我們目前預計", "密切關注", "視情況", "取決於", "仍有變數",
        "初步看來", "大致", "大約", "可能", "或許"
      ),
      en = c(
        "we currently expect", "closely monitor", "depending on",
        "subject to", "preliminary", "roughly", "approximately",
        "may", "might", "could"
      )
    ),
    qa_followup = list(
      zh = c(
        "能否給我們更多", "量化", "能否確認", "時間點", "這是您第一次",
        "追問", "再請問", "可否具體"
      ),
      en = c(
        "can you quantify", "more color", "can you confirm",
        "timing", "first time you(?:'ve| have) mentioned",
        "follow[- ]up", "just to clarify", "more specifically"
      )
    ),
    # Tone scale anchors (1=bearish … 5=very bullish). Used by scorer only.
    tone_anchors = list(
      `5` = list(
        zh = c("強烈的信心", "創紀錄的需求", "非常樂觀"),
        en = c("strong(?:ly)? confident", "record demand", "very optimistic")
      ),
      `4` = list(
        zh = c("對展望感到滿意", "需求持續強勁", "樂觀"),
        en = c("comfortable with the outlook", "demand remain(?:s|ed)? strong", "optimistic")
      ),
      `3` = list(
        zh = c("密切關注情況", "市場狀況混合", "中性"),
        en = c("closely watch(?:ing)?", "mixed (?:market )?conditions", "neutral")
      ),
      `2` = list(
        zh = c("能見度受限", "採取保守立場", "謹慎"),
        en = c("limited visibility", "conservative stance", "cautious")
      ),
      `1` = list(
        zh = c("重大不確定性", "需求明顯疲弱", "悲觀"),
        en = c("significant uncertainty", "demand (?:is |was )?clearly weak", "pessimistic")
      )
    ),
    # Themes for CapEx question prompts (status cues; no invented amounts).
    capex_question_cues = list(
      zh = c("今年計畫", "與去年相比", "分配重心", "削減", "上調", "下調"),
      en = c("this year(?:'s)? plan", "versus last year", "allocation", "cut", "raise", "lower")
    ),
    # Segment-mix cues (step 2).
    segment_cues = list(
      zh = c("分部", "事業部", "業務別", "產品線", "各部門", "按產品"),
      en = c("segment", "business unit", "by product", "division", "product line")
    ),
    # Number-layer cues: raw figure vs explanation vs forward.
    number_layer = list(
      raw = list(
        zh = c("本季", "本期", "為", "%", "億", "百萬"),
        en = c("this quarter", "reported", "%", "billion", "million")
      ),
      explain = list(
        zh = c("主要受惠", "主要因", "受", "帶動", "反映", "歸因於"),
        en = c("driven by", "primarily due", "reflect(?:s|ing)?", "attributable")
      ),
      forward = list(
        zh = c("預期", "展望", "下季", "明年", "區間", "指引"),
        en = c("expect", "outlook", "next quarter", "next year", "range", "guidance")
      )
    ),
    # Trap detectors (reading mistakes to flag).
    traps = list(
      summary_only = list(
        zh = c("重點整理", "法說會摘要", "媒體摘要"),
        en = c("earnings highlights", "call summary", "media recap")
      ),
      beat_vs_guide = list(
        zh = c("超越分析師預期", "優於市場預期", "beat"),
        en = c("beat(?:s|ing)? (?:analyst |street )?expectations?", "above consensus")
      ),
      precise_number_hedge = list(
        zh = c("我們目前預計", "初步", "大約"),
        en = c("we currently expect", "preliminary", "approximately")
      )
    )
  )
}

#' Flatten lexicon role into a single regex alternation (zh ∪ en).
#' @param role character scalar key under ynow_ec_keyword_lexicon().
#' @param lex optional lexicon list.
ynow_ec_lexicon_pattern <- function(role, lex = NULL) {
  lex <- lex %||% ynow_ec_keyword_lexicon()
  node <- lex[[role]]
  if (is.null(node)) return("")
  toks <- character(0)
  if (is.list(node) && !is.null(node$zh)) {
    toks <- c(as.character(node$zh %||% character(0)), as.character(node$en %||% character(0)))
  } else if (is.list(node) && !is.null(node$`5`)) {
    # tone_anchors: flatten all levels
    for (k in names(node)) {
      toks <- c(toks, as.character(node[[k]]$zh %||% character(0)),
                as.character(node[[k]]$en %||% character(0)))
    }
  } else if (is.character(node)) {
    toks <- node
  }
  toks <- unique(toks[nzchar(toks)])
  if (!length(toks)) return("")
  paste(toks, collapse = "|")
}

#' Count case-insensitive regex hits for a role in text.
ynow_ec_lexicon_hit_count <- function(text, role, lex = NULL) {
  txt <- as.character(text %||% "")[1]
  if (!nzchar(txt)) return(0L)
  pat <- ynow_ec_lexicon_pattern(role, lex)
  if (!nzchar(pat)) return(0L)
  m <- gregexpr(pat, txt, perl = TRUE, ignore.case = TRUE)
  if (m[[1]][1] < 0) return(0L)
  length(m[[1]])
}

#' List matching token strings (unique, capped) for a role.
ynow_ec_lexicon_hits <- function(text, role, lex = NULL, max_n = 12L) {
  txt <- as.character(text %||% "")[1]
  empty <- character(0)
  if (!nzchar(txt)) return(empty)
  lex <- lex %||% ynow_ec_keyword_lexicon()
  node <- lex[[role]]
  if (is.null(node)) return(empty)
  pairs <- list()
  if (is.list(node) && !is.null(node$zh)) {
    for (t in c(as.character(node$zh), as.character(node$en))) {
      if (nzchar(t) && grepl(t, txt, perl = TRUE, ignore.case = TRUE)) {
        pairs[[length(pairs) + 1L]] <- t
      }
    }
  }
  unique(utils::head(unlist(pairs, use.names = FALSE), max_n))
}
