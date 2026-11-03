# ==========================================
# Earnings-call keyword lexicon (internal)
# ==========================================
# Machine-readable zh-TW + en-US token lists for transcript / presentation
# extractors and scorers. Not a user-facing wall of text.
# Derived methodology notes from a public reading framework; do not republish
# third-party article prose. Skip framework steps 1 (full-text map scan) and
# 5 (equity-story / smile-curve narrative). Encode steps 2–4 + traps.
# Product surface focus (post-v27.41): only 主要供應商 / 訂單 / 主要客戶 for
# 業務拆解 → order-research. Tone / CapEx / bridging stay internal scorers.
# Never invent amounts. Status-only.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' Internal lexicon. Scorer roles stay paired zh / en lists.
#' Surface roles (orders, suppliers, customers) are item lists: pat + kind.
#' @return named list.
ynow_ec_keyword_lexicon <- function() {
  item <- function(pat, kind) list(pat = pat, kind = kind)
  list(
    version = "3",
    # Framework coverage note (machine metadata, not UI copy).
    skipped_steps = c("1_full_text_map_scan", "5_equity_story_narrative"),
    product_focus = c("major_suppliers", "orders_backlog_bookings", "major_customers"),
    encoded_steps = c(
      "pre_read_prep",
      "2_presentation_deep_read",
      "3_qa_close_read",
      "4_tone_and_wording_delta",
      "order_focus_extract",
      "keyword_db_template",
      "reading_traps"
    ),
    # Order-research surface roles. kind is mention or constraint.
    # Customers have mention items only, so stance cannot become constrained.
    orders = list(
      items = list(
        item("在手訂單", "mention"),
        item("新訂單", "mention"),
        item("接單", "mention"),
        item("訂單能見度", "mention"),
        item("訂單展望", "mention"),
        item("訂單金額", "mention"),
        item("bookings", "mention"),
        item("backlog", "mention"),
        item("可見度", "mention"),
        item("\\bbacklog\\b", "mention"),
        item("bookings?", "mention"),
        item("book[- ]to[- ]bill", "mention"),
        item("order visibility", "mention"),
        item("order outlook", "mention"),
        item("急單", "mention"),
        item("提前拉貨", "mention"),
        item("長約訂單", "mention"),
        item("框架合約", "mention"),
        item("得標", "mention"),
        item("決標", "mention"),
        item("pull[- ]in", "mention"),
        item("pull[- ]forward", "mention"),
        item("framework agreement", "mention"),
        item("blanket PO", "mention"),
        item("annual PO", "mention"),
        item("contract win", "mention"),
        item("砍單", "constraint"),
        item("延期出貨", "constraint"),
        item("order cut", "constraint"),
        item("cancel(?:led)? orders?", "constraint"),
        item("push(?:ed)? out", "constraint"),
        item("遞延出貨", "constraint"),
        item("延後認列", "constraint"),
        item("訂單取消", "constraint"),
        item("客戶取消", "constraint"),
        item("雙重下單", "constraint"),
        item("重複下單", "constraint"),
        item("能見度受限", "constraint"),
        item("能見度降低", "constraint"),
        item("deferred shipment", "constraint"),
        item("delayed recognition", "constraint"),
        item("order cancellation", "constraint"),
        item("customer cancellation", "constraint"),
        item("double ordering", "constraint"),
        item("duplicate orders", "constraint"),
        item("limited visibility", "constraint")
      )
    ),
    suppliers = list(
      items = list(
        item("供應商", "mention"),
        item("主要供應商", "mention"),
        item("供應鏈", "mention"),
        item("供貨", "mention"),
        item("長約", "mention"),
        item("採購", "mention"),
        item("上游", "mention"),
        item("supplier", "mention"),
        item("supply[- ]chain", "mention"),
        item("key supplier", "mention"),
        item("sourcing", "mention"),
        item("long[- ]term agreement", "mention"),
        item("upstream", "mention"),
        item("交期", "mention"),
        item("交貨週期", "mention"),
        item("交貨時間", "mention"),
        item("交期改善", "mention"),
        item("產能利用率", "mention"),
        item("第二來源", "mention"),
        item("進貨占比", "mention"),
        item("配額", "mention"),
        item("供給分配", "mention"),
        item("lead time", "mention"),
        item("delivery cycle", "mention"),
        item("capacity utilization", "mention"),
        item("second source", "mention"),
        item("dual source", "mention"),
        item("dual-sourcing", "mention"),
        item("supply allocation", "mention"),
        item("缺料", "constraint"),
        item("material shortage", "constraint"),
        item("拉長交期", "constraint"),
        item("產能瓶頸", "constraint"),
        item("產能吃緊", "constraint"),
        item("供給受限", "constraint"),
        item("單一供應商", "constraint"),
        item("lead-time extension", "constraint"),
        item("capacity bottleneck", "constraint"),
        item("tight capacity", "constraint"),
        item("constrained supply", "constraint"),
        item("single-source", "constraint")
      )
    ),
    customers = list(
      items = list(
        item("客戶", "mention"),
        item("主要客戶", "mention"),
        item("重大客戶", "mention"),
        item("客戶集中", "mention"),
        item("占營收", "mention"),
        item("佔營收", "mention"),
        item("新客戶", "mention"),
        item("終端客戶", "mention"),
        item("品牌客戶", "mention"),
        item("終端品牌", "mention"),
        item("銷貨占比", "mention"),
        item("customer", "mention"),
        item("major customer", "mention"),
        item("key customer", "mention"),
        item("concentration", "mention"),
        item("% of (?:net )?revenue", "mention"),
        item("new customer", "mention"),
        item("end customer", "mention"),
        item("OEM", "mention"),
        item("\\bODM\\b", "mention"),
        item("\\bEMS\\b", "mention")
      )
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

#' One token list for harvest and for stance.
#' kind = "any" | "mention" | "constraint".
#' Surface roles read item$pat. Scorer roles return zh ∪ en only when kind is "any".
#' @param role lexicon role
#' @param kind which items
#' @param lex optional lexicon
ynow_ec_lexicon_tokens <- function(role, kind = "any", lex = NULL) {
  lex <- lex %||% ynow_ec_keyword_lexicon()
  node <- lex[[role]]
  if (is.null(node)) return(character(0))
  kind <- as.character(kind %||% "any")[1]
  if (!kind %in% c("any", "mention", "constraint")) return(character(0))
  if (is.list(node) && !is.null(node$items)) {
    pats <- character(0)
    for (it in node$items) {
      pat <- as.character(it$pat %||% "")[1]
      ik <- as.character(it$kind %||% "")[1]
      if (!nzchar(pat)) next
      if (identical(kind, "any") || identical(kind, ik)) pats <- c(pats, pat)
    }
    return(unique(pats))
  }
  if (!identical(kind, "any")) return(character(0))
  if (is.list(node) && !is.null(node$zh)) {
    toks <- c(as.character(node$zh %||% character(0)), as.character(node$en %||% character(0)))
    return(unique(toks[nzchar(toks)]))
  }
  if (is.list(node) && !is.null(node[["5"]])) {
    toks <- character(0)
    for (k in names(node)) {
      toks <- c(
        toks,
        as.character(node[[k]]$zh %||% character(0)),
        as.character(node[[k]]$en %||% character(0))
      )
    }
    return(unique(toks[nzchar(toks)]))
  }
  if (is.character(node)) return(unique(node[nzchar(node)]))
  character(0)
}

#' Flatten a role into one regex alternation. Derived from ynow_ec_lexicon_tokens.
#' @param role character scalar key under ynow_ec_keyword_lexicon().
#' @param lex optional lexicon list.
ynow_ec_lexicon_pattern <- function(role, lex = NULL) {
  toks <- ynow_ec_lexicon_tokens(role, kind = "any", lex = lex)
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
  toks <- ynow_ec_lexicon_tokens(role, kind = "any", lex = lex)
  if (!length(toks)) return(empty)
  hit <- toks[vapply(toks, function(t) {
    grepl(t, txt, perl = TRUE, ignore.case = TRUE)
  }, logical(1))]
  unique(utils::head(hit, max_n))
}
