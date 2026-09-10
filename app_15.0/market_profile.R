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
      rf_symbol = "TW_GOV_APPROX",
      rf_label_zh = "台灣公債近似（文件化 fallback；Yahoo 無穩定台債指數時）",
      rf_fallback = 1.8,
      beta_bench = "0050.TW",
      beta_bench_choices = c(
        "0050.TW（元大台灣50）" = "0050.TW",
        "^TWII（加權指數）" = "^TWII"
      ),
      backtest_bench = "0050.TW",
      show_sec_lab = FALSE,
      bluechip_title = "BLUE CHIP（台股績優）",
      bluechip_blurb = paste0(
        "自臺灣證券交易所／櫃買中心 OpenAPI 上市櫃名單篩選台股績優候選：",
        "先套用規模 × 產業 × 評價模型，再以 Piotroski 高門檻與年化估值漲幅排序。"
      ),
      universe_label = "上市櫃",
      ticker_presets = c(
        "2330.TW — 台積電" = "2330.TW",
        "2317.TW — 鴻海" = "2317.TW",
        "2454.TW — 聯發科" = "2454.TW",
        "2308.TW — 台達電" = "2308.TW",
        "2382.TW — 廣達" = "2382.TW",
        "2303.TW — 聯電" = "2303.TW",
        "2881.TW — 富邦金" = "2881.TW",
        "2882.TW — 國泰金" = "2882.TW",
        "2891.TW — 中信金" = "2891.TW",
        "2886.TW — 兆豐金" = "2886.TW",
        "2412.TW — 中華電" = "2412.TW",
        "1301.TW — 台塑" = "1301.TW",
        "1303.TW — 南亞" = "1303.TW",
        "2002.TW — 中鋼" = "2002.TW",
        "0050.TW — 元大台灣50" = "0050.TW"
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
      rf_label_zh = "美國 10 年期公債（^TNX）",
      rf_fallback = 4.0,
      beta_bench = "SPY",
      beta_bench_choices = c(
        "SPY (S&P 500 ETF)" = "SPY",
        "QQQ (Nasdaq 100 ETF)" = "QQQ",
        "IWM (Russell 2000 ETF)" = "IWM"
      ),
      backtest_bench = "SPY",
      show_sec_lab = TRUE,
      bluechip_title = "BLUE CHIP（美股績優）",
      bluechip_blurb = paste0(
        "自 Wikipedia 的 S&P 500 成分名單篩選美股績優候選：",
        "先套用規模 × 產業 × 評價模型，再以 Piotroski 高門檻與年化估值漲幅排序。"
      ),
      universe_label = "S&P 500",
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
        "TSM — Taiwan Semiconductor (ADR)" = "TSM",
        "SPY — S&P 500 ETF" = "SPY",
        "QQQ — Nasdaq 100 ETF" = "QQQ"
      )
    )
  }
}

#' 自上市櫃宇宙解析純數字代號 → Yahoo 後綴（優先 .TW／TWSE）
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
  hits <- tks[bases == code]
  if (!length(hits)) return(NULL)
  tw <- hits[grepl("\\.TW$", hits)]
  if (length(tw)) return(tw[[1]])
  hits[[1]]
}

#' 依市場正規化使用者輸入代號
#' TW：使用者只需輸入純數字（如 2330）；自動補 .TW。
#' 優先查上市櫃宇宙；僅上櫃者給 .TWO。已含 .TW／.TWO 會去空白並正規化大小寫。
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
