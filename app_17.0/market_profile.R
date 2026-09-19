# ==========================================
# market_profile.R — 美股／台股市場模式（App 15）
# 標題列「市場切換」與 USD｜TWD 顯示幣別分開。
# ==========================================

.ynow_market_ctx <- new.env(parent = emptyenv())
.ynow_market_ctx$mode <- "US"

normalize_market_mode <- function(mode) {
  m <- toupper(trimws(as.character(mode %||% "US")[1]))
  if (identical(m, "TW") || identical(m, "TWN") || identical(m, "TAIWAN")) return("TW")
  "US"
}

get_market_mode <- function() normalize_market_mode(.ynow_market_ctx$mode)

set_market_mode <- function(mode) {
  .ynow_market_ctx$mode <- normalize_market_mode(mode)
  invisible(.ynow_market_ctx$mode)
}

#' 依市場回傳預設／基準／文案設定
market_profile <- function(mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  if (identical(mode, "TW")) {
    list(
      mode = "TW",
      label_zh = "台股",
      session_currency = "TWD",
      default_ticker = "2330.TW",
      wacc_tax = 20,
      rf_symbol = "TPEX_CURVE_10Y",
      rf_label_zh = "台灣 10 年期公債（櫃買 TPEx 殖利率曲線，即時）",
      # 僅在即時抓取與 last-known 皆失敗時使用；非「標準 Rf」
      rf_fallback = 1.8,
      data_source_note_zh = paste0(
        "台股估值主源 Yahoo；Rf＝櫃買 TPEx 公債殖利率曲線最近交易日 10 年期",
        "（失敗則最近成功值；再失敗才工程 fallback 1.8%，非固定公債）。法定 T＝20%。"
      ),
      beta_bench = "0050.TW",
      beta_bench_choices = c(
        "0050.TW（元大台灣50）" = "0050.TW",
        "^TWII（加權指數）" = "^TWII"
      ),
      backtest_bench = "0050.TW",
      show_sec_lab = FALSE,
      bluechip_title = "績優股排行（台股）",
      # Intro copy lives in ui_locale.R (bluechip_blurb_tw); kept for profile completeness
      bluechip_blurb = paste0(
        "依據臺灣證券交易所與櫃買中心公開名單，篩選台股績優候選標的（範圍僅含上市與上櫃；不含興櫃）。",
        "先依產業與評價模型篩選，再以 Piotroski F-Score≥7 過濾並依隱含年化估值漲幅排序。"
      ),
      universe_label = "上市／上櫃／興櫃",
      # 顯示標籤用不含 .TW 的代號；值仍為 Yahoo fetch symbol
      ticker_presets = c(
        "2330 — 台積電" = "2330.TW",
        "2317 — 鴻海" = "2317.TW",
        "2454 — 聯發科" = "2454.TW",
        "2308 — 台達電" = "2308.TW",
        "2382 — 廣達" = "2382.TW",
        "2303 — 聯電" = "2303.TW",
        "2881 — 富邦金" = "2881.TW",
        "2882 — 國泰金" = "2882.TW",
        "2891 — 中信金" = "2891.TW",
        "2886 — 兆豐金" = "2886.TW",
        "2412 — 中華電" = "2412.TW",
        "1301 — 台塑" = "1301.TW",
        "1303 — 南亞" = "1303.TW",
        "2002 — 中鋼" = "2002.TW",
        "0050 — 元大台灣50" = "0050.TW"
      )
    )
  } else {
    list(
      mode = "US",
      label_zh = "美股",
      session_currency = "USD",
      default_ticker = "TSM",
      wacc_tax = 21,
      rf_symbol = "^TNX",
      rf_label_zh = "美國 10 年期公債（Yahoo ^TNX，即時）",
      # 僅在即時抓取與 last-known 皆失敗時使用；非「標準 Rf」
      rf_fallback = 5.0,
      data_source_note_zh = paste0(
        "美股估值主源 Yahoo；Rf＝即時 ^TNX（失敗則最近成功值；",
        "再失敗才工程 fallback 5%，非固定公債）。法定 T＝21%。"
      ),
      beta_bench = "SPY",
      beta_bench_choices = c(
        "SPY (S&P 500 ETF)" = "SPY",
        "QQQ (Nasdaq 100 ETF)" = "QQQ",
        "IWM (Russell 2000 ETF)" = "IWM"
      ),
      backtest_bench = "SPY",
      show_sec_lab = TRUE,
      bluechip_title = "Blue Chip Ranking (US)",
      # Intro copy lives in ui_locale.R (bluechip_blurb_us); kept for profile completeness
      bluechip_blurb = paste0(
        "Screen US blue-chip candidates from the S&P 500 constituent list ",
        "(Nasdaq and NYSE listings): ",
        "industry × valuation-model filters, then Piotroski F-Score≥7 and implied annualized valuation appreciation."
      ),
      universe_label = "S&P 500 (Nasdaq + NYSE)",
      ticker_presets = c(
        "AMZN — Amazon.com" = "AMZN",
        "AAPL — Apple" = "AAPL",
        "MSFT — Microsoft" = "MSFT",
        "GOOGL — Alphabet" = "GOOGL",
        "META — Meta Platforms" = "META",
        "NVDA — NVIDIA" = "NVDA",
        "TSLA — Tesla" = "TSLA",
        "BRK-B — Berkshire Hathaway" = "BRK-B",
        "JPM — JPMorgan Chase" = "JPM",
        "V — Visa" = "V",
        "JNJ — Johnson & Johnson" = "JNJ",
        "XOM — Exxon Mobil" = "XOM",
        "KO — Coca-Cola" = "KO",
        "TSM — Taiwan Semiconductor (ADR)" = "TSM",
        "SPY — S&P 500 ETF" = "SPY",
        "QQQ — Nasdaq 100 ETF" = "QQQ"
      )
    )
  }
}

#' Yahoo / SEC exchange → Nasdaq listing family (NMS, NasdaqGS, …)
is_us_nasdaq_exchange <- function(exch) {
  e <- toupper(trimws(as.character(exch %||% "")[1]))
  if (!nzchar(e)) return(FALSE)
  grepl("NASDAQ", e, fixed = TRUE) || e %in% c("NMS", "NGM", "NCM", "NAS")
}

#' Yahoo / SEC exchange → NYSE listing family (NYQ, NYSE, NYSE American)
#' Note: NYSEArca / BATS are venues for many ETFs — not counted as NYSE equities here.
is_us_nyse_exchange <- function(exch) {
  e <- toupper(trimws(as.character(exch %||% "")[1]))
  if (!nzchar(e)) return(FALSE)
  if (grepl("ARCA|BATS|CBOE", e)) return(FALSE)
  e %in% c("NYSE", "NYQ", "NYA", "ASE", "AMEX", "NYSE AMERICAN", "NYSE MKT", "NEW YORK STOCK EXCHANGE")
}

#' US primary listing venues we keep in 美股 search / universe meta
is_us_primary_listing_exchange <- function(exch) {
  is_us_nasdaq_exchange(exch) || is_us_nyse_exchange(exch)
}

#' Normalize to NASDAQ / NYSE for universe CSV (else NA)
normalize_us_listing_exchange <- function(exch) {
  if (is_us_nasdaq_exchange(exch)) return("NASDAQ")
  if (is_us_nyse_exchange(exch)) return("NYSE")
  NA_character_
}

#' 自上市／上櫃／興櫃宇宙解析純數字代號 → Yahoo 後綴（優先 .TW／TWSE，其次上櫃，再興櫃）
#' 無宇宙或未命中時回傳 NULL（呼叫端預設 .TW）。
resolve_tw_bare_code <- function(code) {
  code <- toupper(gsub("\\s+", "", as.character(code %||% "")[1]))
  if (!nzchar(code) || !grepl("^[0-9]{4,6}[A-Z]?$", code)) return(NULL)
  u <- tryCatch({
    if (exists("lab_get_tw_universe", mode = "function")) {
      lab_get_tw_universe(FALSE)
    } else if (exists("lab_read_tw_cache", mode = "function")) {
      lab_read_tw_cache()
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) < 1L) return(NULL)
  tks <- toupper(as.character(u$ticker))
  bases <- sub("\\.(TW|TWO)$", "", tks, ignore.case = TRUE)
  idx <- which(bases == code)
  if (!length(idx)) return(NULL)
  if ("exchange" %in% names(u) && exists("lab_tw_exchange_rank", mode = "function")) {
    ranks <- vapply(as.character(u$exchange[idx]), lab_tw_exchange_rank, integer(1))
    idx <- idx[order(ranks)]
  } else {
    # 無 exchange 欄時：.TW 優先於 .TWO
    tw_first <- grepl("\\.TW$", tks[idx])
    idx <- c(idx[tw_first], idx[!tw_first])
  }
  tks[[idx[[1]]]]
}

#' 依市場正規化使用者輸入代號
#' TW：使用者只需輸入純數字（如 2330）；自動補 .TW。
#' 優先查宇宙：上市→.TW；僅上櫃／興櫃→.TWO。已含 .TW／.TWO 會去空白並正規化大小寫。
normalize_ticker_for_market <- function(sym, mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  raw <- trimws(as.character(sym %||% "")[1])
  if (!nzchar(raw) || identical(toupper(raw), "NA")) return(NA_character_)
  # strip common labels "2330.TW — 台積電"
  raw <- sub("\\s+[—\\-–].*$", "", raw)
  raw <- trimws(raw)
  if (identical(mode, "TW")) {
    if (grepl("^\\^", raw)) return(toupper(gsub("\\s+", "", raw)))
    # collapse spaces so "2330 .TW" / " 2330 " still work
    u <- toupper(gsub("\\s+", "", raw))
    # peel accidental suffix, then re-resolve (universe may prefer .TWO)
    bare <- sub("\\.(TW|TWO)$", "", u)
    if (grepl("^[0-9]{4,6}[A-Z]?$", bare)) {
      hit <- tryCatch(resolve_tw_bare_code(bare), error = function(e) NULL)
      if (!is.null(hit) && nzchar(hit)) return(hit)
      # keep explicit .TWO if user typed it and universe miss; else prefer .TW
      if (grepl("\\.TWO$", u)) return(paste0(bare, ".TWO"))
      return(paste0(bare, ".TW"))
    }
    if (grepl("\\.(TW|TWO)$", u)) return(u)
    return(u)
  }
  # US：保留 Yahoo 慣例（BRK.B → BRK-B 由上游處理）
  gsub("\\.", "-", toupper(gsub("\\s+", "", raw)))
}

#' TW：.TW 抓取失敗時改試 .TWO（上櫃 Yahoo 後綴）
tw_yahoo_alt_ticker <- function(sym) {
  u <- toupper(trimws(as.character(sym %||% "")[1]))
  if (!grepl("\\.TW$", u)) return(NA_character_)
  sub("\\.TW$", ".TWO", u)
}

is_tw_yahoo_ticker <- function(sym) {
  grepl("\\.(TW|TWO)$", as.character(sym %||% "")[1], ignore.case = TRUE)
}

#' 內部／Yahoo 抓取用代號（= normalize）
fetch_ticker_for_market <- function(sym, mode = get_market_mode()) {
  normalize_ticker_for_market(sym, mode)
}

#' UI 顯示用代號：台股模式剝除 .(TW|TWO)；美股維持原樣
display_ticker_for_market <- function(sym, mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  raw <- trimws(as.character(sym %||% "")[1])
  if (!nzchar(raw) || identical(toupper(raw), "NA")) return("")
  raw <- sub("\\s+[—\\-–].*$", "", raw)
  raw <- trimws(raw)
  if (!identical(mode, "TW")) {
    return(toupper(gsub("\\s+", "", raw)))
  }
  u <- gsub("\\s+", "", raw)
  if (grepl("^\\^", u)) return(toupper(u))
  sub("\\.(TW|TWO)$", "", u, ignore.case = TRUE)
}

#' 向量版顯示代號
display_tickers_for_market <- function(syms, mode = get_market_mode()) {
  mode <- normalize_market_mode(mode)
  vapply(
    as.character(syms %||% character(0)),
    function(s) display_ticker_for_market(s, mode),
    character(1),
    USE.NAMES = FALSE
  )
}

#' 查詢是否含中日韓漢字（CJK）
query_has_cjk <- function(q) {
  grepl("[\u4e00-\u9fff]", as.character(q %||% "")[1])
}

#' 清理公司名稱候選（去空白／NA／代號本身）
.clean_company_name_cands <- function(cands, ticker = "") {
  cands <- unlist(cands, use.names = FALSE)
  cands <- trimws(as.character(cands))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (nzchar(tk)) {
    cands <- cands[toupper(cands) != tk]
    bare <- sub("\\.(TW|TWO)$", "", tk, ignore.case = TRUE)
    if (nzchar(bare)) cands <- cands[toupper(cands) != bare]
  }
  unique(cands)
}

#' 自候選拆出中文／英文公司全稱（台股首頁雙語顯示用）
#' @return list(zh=, en=)；僅有一種語言時另一欄為 ""
split_corp_names_zh_en <- function(..., ticker = "", prefer_zh = NULL) {
  cands <- .clean_company_name_cands(list(...), ticker = ticker)
  pref <- trimws(as.character(prefer_zh %||% "")[1])
  if (nzchar(pref) && isTRUE(query_has_cjk(pref))) {
    cands <- unique(c(pref, cands))
  }
  if (!length(cands)) return(list(zh = "", en = ""))
  is_zh <- vapply(cands, query_has_cjk, logical(1))
  zh_pool <- cands[is_zh]
  en_pool <- cands[!is_zh]
  pick_longest <- function(pool) {
    if (!length(pool)) return("")
    as.character(pool[[which.max(nchar(pool))]])
  }
  zh <- if (nzchar(pref) && isTRUE(query_has_cjk(pref))) pref else pick_longest(zh_pool)
  en <- pick_longest(en_pool)
  # 避免中英相同字串重複兩行
  if (nzchar(zh) && nzchar(en) && identical(zh, en)) en <- ""
  list(zh = zh, en = en)
}

#' 字元子序列：query 的每個字依序出現在 text（台積 ⊂ 台灣積體…）
.cjk_chars_in_order <- function(query, text) {
  q <- as.character(query %||% "")[1]
  t <- as.character(text %||% "")[1]
  if (!nzchar(q) || !nzchar(t)) return(FALSE)
  qchars <- strsplit(q, "", fixed = TRUE)[[1]]
  pos <- 0L
  for (ch in qchars) {
    rest <- substr(t, pos + 1L, nchar(t))
    idx <- regexpr(ch, rest, fixed = TRUE)[1]
    if (is.na(idx) || idx < 1L) return(FALSE)
    pos <- pos + as.integer(idx)
  }
  TRUE
}

#' 台股常見簡稱 → Yahoo fetch symbol（補強宇宙全名匹配）
tw_short_name_aliases <- function() {
  c(
    "台積" = "2330.TW", "台積電" = "2330.TW", "TSMC" = "2330.TW",
    "鴻海" = "2317.TW", "富士康" = "2317.TW",
    "聯發科" = "2454.TW", "聯發" = "2454.TW",
    "台達電" = "2308.TW", "台達" = "2308.TW",
    "廣達" = "2382.TW",
    "聯電" = "2303.TW",
    "富邦金" = "2881.TW", "國泰金" = "2882.TW",
    "中信金" = "2891.TW", "兆豐金" = "2886.TW",
    "中華電" = "2412.TW",
    "台塑" = "1301.TW", "南亞" = "1303.TW", "中鋼" = "2002.TW",
    "元大台灣50" = "0050.TW", "台灣50" = "0050.TW"
  )
}

#' 自上市／上櫃／興櫃宇宙／簡稱別名搜尋（CJK／公司名 fallback）
#' @return named character：names = 「代號 — 名稱」, values = Yahoo fetch symbol
search_tw_universe_by_name <- function(query, max_results = 12L) {
  q <- trimws(as.character(query %||% "")[1])
  if (!nzchar(q)) return(character(0))
  max_results <- max(1L, as.integer(max_results)[1])

  hits <- character(0)
  labs <- character(0)
  add_hit <- function(sym, lab) {
    sym <- as.character(sym)[1]
    if (!nzchar(sym) || sym %in% hits) return()
    hits <<- c(hits, sym)
    labs <<- c(labs, lab)
  }

  # 1) 簡稱別名（台積／鴻海…）
  aliases <- tw_short_name_aliases()
  q_up <- toupper(q)
  for (i in seq_along(aliases)) {
    an <- names(aliases)[[i]]
    if (grepl(q, an, fixed = TRUE) || grepl(an, q, fixed = TRUE) ||
        identical(toupper(an), q_up)) {
      sym <- aliases[[i]]
      disp <- display_ticker_for_market(sym, "TW")
      add_hit(sym, paste0(disp, " — ", an))
    }
  }

  # 2) 宇宙全名／代號
  u <- tryCatch({
    if (exists("lab_get_tw_universe", mode = "function")) {
      lab_get_tw_universe(FALSE)
    } else if (exists("lab_read_tw_cache", mode = "function")) {
      lab_read_tw_cache()
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(u) && is.data.frame(u) && nrow(u) > 0L &&
      all(c("ticker", "name") %in% names(u))) {
    tks <- as.character(u$ticker)
    nms <- as.character(u$name)
    bare <- sub("\\.(TW|TWO)$", "", tks, ignore.case = TRUE)
    q_u <- toupper(gsub("\\s+", "", q))
    score <- rep(0L, length(tks))
    for (i in seq_along(tks)) {
      nm <- nms[[i]]
      if (identical(toupper(bare[[i]]), q_u) || identical(toupper(tks[[i]]), q_u)) {
        score[[i]] <- 100L
      } else if (nzchar(nm) && grepl(q, nm, fixed = TRUE)) {
        score[[i]] <- 80L
      } else if (nzchar(nm) && query_has_cjk(q) && .cjk_chars_in_order(q, nm)) {
        score[[i]] <- 40L
      } else if (grepl(q_u, toupper(bare[[i]]), fixed = TRUE)) {
        score[[i]] <- 60L
      }
    }
    ord <- order(-score, bare, na.last = TRUE)
    for (i in ord) {
      if (score[[i]] <= 0L) next
      if (length(hits) >= max_results) break
      disp <- display_ticker_for_market(tks[[i]], "TW")
      # 顯示法定全稱（勿剝「股份有限公司」）；搜尋仍可依子字串／簡稱別名命中
      disp_nm <- trimws(as.character(nms[[i]]))
      board_tag <- ""
      if ("exchange" %in% names(u)) {
        ex_i <- toupper(as.character(u$exchange[[i]])[1])
        if (ex_i %in% c("ESB", "EMERGING", "TPEX_ESB")) {
          board_tag <- "（興櫃）"
        } else if (ex_i %in% c("TPEX", "TWO", "OTC", "ROTC") &&
                   grepl("\\.TWO$", tks[[i]], ignore.case = TRUE)) {
          board_tag <- "（上櫃）"
        }
      }
      add_hit(tks[[i]], paste0(disp, " — ", disp_nm, board_tag))
    }
  }

  if (!length(hits)) return(character(0))
  stats::setNames(hits, labs)
}

#' Search S&P 500 universe by ticker / company name (offline; Nasdaq + NYSE constituents)
#' @return named character：names = 「代號 — 名稱」, values = Yahoo fetch symbol
search_us_universe_by_name <- function(query, max_results = 12L) {
  q <- trimws(as.character(query %||% "")[1])
  if (!nzchar(q)) return(character(0))
  max_results <- max(1L, as.integer(max_results)[1])

  u <- tryCatch({
    if (exists("lab_get_sp500_universe", mode = "function")) {
      lab_get_sp500_universe(FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  if (is.null(u) || !is.data.frame(u) || nrow(u) < 1L ||
      !all(c("ticker", "name") %in% names(u))) {
    return(character(0))
  }

  tks <- as.character(u$ticker)
  nms <- as.character(u$name)
  q_u <- toupper(gsub("\\s+", "", q))
  score <- rep(0L, length(tks))
  for (i in seq_along(tks)) {
    tk_u <- toupper(gsub("\\s+", "", tks[[i]]))
    nm <- nms[[i]]
    if (identical(tk_u, q_u)) {
      score[[i]] <- 100L
    } else if (nzchar(nm) && grepl(q, nm, ignore.case = TRUE, fixed = TRUE)) {
      score[[i]] <- 80L
    } else if (grepl(q_u, tk_u, fixed = TRUE)) {
      score[[i]] <- 60L
    }
  }
  ord <- order(-score, tks, na.last = TRUE)
  hits <- character(0)
  labs <- character(0)
  for (i in ord) {
    if (score[[i]] <= 0L) next
    if (length(hits) >= max_results) break
    board_tag <- ""
    if ("exchange" %in% names(u)) {
      ex_norm <- normalize_us_listing_exchange(u$exchange[[i]])
      if (identical(ex_norm, "NASDAQ")) board_tag <- " (Nasdaq)"
      else if (identical(ex_norm, "NYSE")) board_tag <- " (NYSE)"
    }
    nm_disp <- trimws(as.character(nms[[i]]))
    lab <- if (nzchar(nm_disp)) paste0(tks[[i]], " — ", nm_disp, board_tag) else
      paste0(tks[[i]], board_tag)
    hits <- c(hits, tks[[i]])
    labs <- c(labs, lab)
  }
  if (!length(hits)) return(character(0))
  stats::setNames(hits, labs)
}
