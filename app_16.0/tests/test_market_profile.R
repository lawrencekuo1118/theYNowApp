# Tests for market_profile + Blue Chip detail row count invariant (no network).
Sys.setenv(YNOW_DEBUG_SKIP_PY = "1")

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK:", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL:", label, "\n")
  }
}

root <- normalizePath("..", mustWork = TRUE)
# Allow running from app_16.0/tests
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
if (!file.exists(file.path(root, "market_profile.R"))) {
  root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

source(file.path(root, "market_profile.R"), local = TRUE, encoding = "UTF-8")
source(file.path(root, "ui_locale.R"), local = TRUE, encoding = "UTF-8")

check("US normalize AAPL", identical(normalize_ticker_for_market("aapl", "US"), "AAPL"))
check("TW normalize 2330", identical(normalize_ticker_for_market("2330", "TW"), "2330.TW"))
check("TW keep 2330.TW", identical(normalize_ticker_for_market("2330.TW", "TW"), "2330.TW"))
check("TW spaced bare", identical(normalize_ticker_for_market("  2330  ", "TW"), "2330.TW"))
check("TW spaced suffix", identical(normalize_ticker_for_market("2330 .TW", "TW"), "2330.TW"))
check("TW lowercase suffix", identical(normalize_ticker_for_market("2330.tw", "TW"), "2330.TW"))
check("TW label strip", identical(normalize_ticker_for_market("2330.TW — 台積電", "TW"), "2330.TW"))
check("TW alt helper", identical(tw_yahoo_alt_ticker("3105.TW"), "3105.TWO"))
check("TW profile tax 20", identical(as.integer(market_profile("TW")$wacc_tax), 20L))
check("US profile tax 21", identical(as.integer(market_profile("US")$wacc_tax), 21L))
check(
  "TW rf_label notes Rf fallback + T",
  grepl("1\\.8", market_profile("TW")$rf_label_zh) &&
    grepl("20%", market_profile("TW")$rf_label_zh, fixed = TRUE)
)
check(
  "TW data_source_note mentions Yahoo",
  grepl("Yahoo", market_profile("TW")$data_source_note_zh %||% "", fixed = TRUE)
)
check("TW default ticker", identical(market_profile("TW")$default_ticker, "2330.TW"))
check("TW hides SEC", isFALSE(market_profile("TW")$show_sec_lab))
check("US shows SEC", isTRUE(market_profile("US")$show_sec_lab))
check("TW bench 0050", identical(market_profile("TW")$beta_bench, "0050.TW"))

# Display vs fetch
check("TW display strips .TW", identical(display_ticker_for_market("2330.TW", "TW"), "2330"))
check("TW display strips .TWO", identical(display_ticker_for_market("3105.TWO", "TW"), "3105"))
check("TW display bare stays", identical(display_ticker_for_market("2330", "TW"), "2330"))
check("US display AAPL", identical(display_ticker_for_market("AAPL", "US"), "AAPL"))
check("US display keeps TSM", identical(display_ticker_for_market("TSM", "US"), "TSM"))
check(
  "fetch == normalize",
  identical(fetch_ticker_for_market("2330", "TW"), normalize_ticker_for_market("2330", "TW"))
)

# Locale
check("TW locale zh-TW", identical(locale_for_market("TW"), "zh-TW"))
check("US locale en", identical(locale_for_market("US"), "en"))
check("ui_str zh ticker", grepl("代號", ui_str("ticker_label", "zh-TW")))
check("ui_str en ticker", grepl("Ticker", ui_str("ticker_label", "en")))
check("no simplified 默认", !grepl("默认", ui_str("data_source_body", "zh-TW")))
check("tab income zh", identical(ui_str("tab_income_statement", "zh-TW"), "損益表"))
check("tab income en", identical(ui_str("tab_income_statement", "en"), "Income Statement"))
check("tab wacc kept EN", identical(ui_str("tab_wacc", "zh-TW"), "WACC"))
check("box fin report zh", identical(ui_str("box_financial_report", "zh-TW"), "財務報表"))
check("tab map has Finance Summary", identical(ui_tab_label_map("zh-TW")[["Finance Summary"]], "財務摘要"))
check("tab map sec_notes", identical(ui_tab_label_map("zh-TW")[["sec_notes"]], "財報附註 (SEC)"))
check("no simplified in tab map", !any(grepl("数据|默认|用户", unlist(ui_tab_label_map("zh-TW")))))
box_specs <- ui_box_header_specs("zh-TW")
check("box specs non-empty", length(box_specs) >= 5L)
check(
  "box specs rematch EN header",
  any(vapply(box_specs, function(sp) {
    "FINANCIAL REPORT" %in% sp$match && identical(sp$label, "財務報表")
  }, logical(1)))
)

# CJK alias / universe name search
check("query_has_cjk", isTRUE(query_has_cjk("台積")))
check("query_no_cjk digits", isFALSE(query_has_cjk("2330")))

# 台股雙語全稱：中文／英文拆分與 fallback
parts_both <- split_corp_names_zh_en(
  "Taiwan Semiconductor Manufacturing Company Limited",
  "台灣積體電路製造股份有限公司",
  ticker = "2330.TW",
  prefer_zh = "台灣積體電路製造股份有限公司"
)
check("split zh+en has zh", identical(parts_both$zh, "台灣積體電路製造股份有限公司"))
check(
  "split zh+en has en",
  identical(parts_both$en, "Taiwan Semiconductor Manufacturing Company Limited")
)
parts_zh_only <- split_corp_names_zh_en(
  "台灣積體電路製造股份有限公司",
  ticker = "2330.TW"
)
check("split zh-only no empty en line", identical(parts_zh_only$en, "") && nzchar(parts_zh_only$zh))
parts_en_only <- split_corp_names_zh_en(
  "Taiwan Semiconductor Manufacturing Company Limited",
  ticker = "2330.TW"
)
check("split en-only no empty zh", identical(parts_en_only$zh, "") && nzchar(parts_en_only$en))

alias_hits <- search_tw_universe_by_name("台積", max_results = 5L)
check("CJK 台積 hits 2330", "2330.TW" %in% unname(alias_hits))
honghai <- search_tw_universe_by_name("鴻海", max_results = 5L)
check("CJK 鴻海 hits 2317", "2317.TW" %in% unname(honghai))

# Universe cache（若存在）：上櫃／興櫃純數字應解析為 .TWO
cache_path <- file.path(root, "data", "tw_universe.csv")
if (file.exists(cache_path)) {
  source(file.path(root, "lab_tw_universe.R"), local = TRUE, encoding = "UTF-8")
  check(
    "TW OTC bare 3105 → .TWO",
    identical(normalize_ticker_for_market("3105", "TW"), "3105.TWO")
  )
  check(
    "TW listed bare 2330 stays .TW",
    identical(normalize_ticker_for_market("2330", "TW"), "2330.TW")
  )
  uni_hits <- search_tw_universe_by_name("台灣積體", max_results = 8L)
  check(
    "universe CJK 台灣積體 → 2330",
    "2330.TW" %in% unname(uni_hits)
  )
  zh2330 <- lookup_tw_universe_company_name("2330.TW")
  check(
    "lookup 2330 Chinese name",
    nzchar(zh2330) && grepl("積體", zh2330, fixed = TRUE)
  )
  u <- lab_get_tw_universe(FALSE)
  has_otc <- any(toupper(as.character(u$exchange)) %in% c("TPEX", "TWO", "OTC"))
  check("universe has 上櫃 (TPEX)", isTRUE(has_otc))
  # 耀穎 7772（上櫃，2026/05 掛牌）：純數字與中文皆應命中
  if (any(grepl("^7772\\.TWO$", as.character(u$ticker), ignore.case = TRUE))) {
    check(
      "TW OTC 7772 → .TWO",
      identical(normalize_ticker_for_market("7772", "TW"), "7772.TWO")
    )
    hits7772 <- search_tw_universe_by_name("7772", max_results = 5L)
    check("search bare 7772 resolves", "7772.TWO" %in% unname(hits7772))
    hits_yaoying <- search_tw_universe_by_name("耀穎", max_results = 5L)
    check("CJK 耀穎 hits 7772", "7772.TWO" %in% unname(hits_yaoying))
    zh7772 <- lookup_tw_universe_company_name("7772.TWO")
    check(
      "7772 universe name is full legal 公司型態",
      nzchar(zh7772) && grepl("股份有限公司|(股)公司", zh7772)
    )
    check(
      "7772 name is 耀穎光電…全稱",
      grepl("耀穎光電", zh7772, fixed = TRUE)
    )
    # suggest 標籤應保留全稱（勿剝股份有限公司）
    lab7772 <- names(hits7772)[match("7772.TWO", unname(hits7772))]
    check(
      "suggest label 7772 keeps full name",
      nzchar(lab7772) && grepl("股份有限公司|(股)公司", lab7772)
    )
    # 上市／上櫃抽樣：全稱應含公司型態
    zh2330_full <- lookup_tw_universe_company_name("2330.TW")
    check(
      "2330 listed full legal name",
      grepl("股份有限公司", zh2330_full, fixed = TRUE)
    )
    zh6488 <- lookup_tw_universe_company_name("6488.TWO")
    if (nzchar(zh6488)) {
      check(
        "6488 OTC full legal name",
        grepl("股份有限公司|(股)公司", zh6488)
      )
    }
    # search_ticker_choices 不得因 R≥4.3 &&/vector grepl 而崩潰
    crawler <- file.path(root, "web_crawler.R")
    if (file.exists(crawler)) {
      source(crawler, local = TRUE, encoding = "UTF-8")
      ch7772 <- tryCatch(
        search_ticker_choices("7772", market = "TW"),
        error = function(e) structure(character(0), err = conditionMessage(e))
      )
      check(
        "suggest 7772 does not crash",
        is.null(attr(ch7772, "err")) && length(ch7772) > 0L
      )
      check("suggest 7772 includes 7772.TWO", "7772.TWO" %in% unname(ch7772))
      ch_name <- tryCatch(
        search_ticker_choices("耀穎", market = "TW"),
        error = function(e) structure(character(0), err = conditionMessage(e))
      )
      check(
        "suggest 耀穎 does not crash",
        is.null(attr(ch_name, "err")) && length(ch_name) > 0L
      )
      check("suggest 耀穎 includes 7772.TWO", "7772.TWO" %in% unname(ch_name))
    }
  } else {
    cat("NOTE: tw_universe.csv missing 7772.TWO — refresh OTC board\n")
  }
  has_esb <- any(toupper(as.character(u$exchange)) %in% c("ESB", "EMERGING", "TPEX_ESB"))
  # 興櫃列可能尚未寫入舊快取；有則驗證解析與搜尋
  if (isTRUE(has_esb)) {
    esb_rows <- u[toupper(as.character(u$exchange)) %in% c("ESB", "EMERGING", "TPEX_ESB"), , drop = FALSE]
    sample_tk <- as.character(esb_rows$ticker[[1]])
    sample_bare <- sub("\\.(TW|TWO)$", "", sample_tk, ignore.case = TRUE)
    check(
      "TW ESB bare → .TWO",
      identical(normalize_ticker_for_market(sample_bare, "TW"), paste0(sample_bare, ".TWO"))
    )
    # 已知樣本：富味鄉 1260（興櫃）；若在宇宙中則驗證
    if (any(grepl("^1260\\.TWO$", as.character(u$ticker), ignore.case = TRUE))) {
      check(
        "TW ESB 1260 → .TWO",
        identical(normalize_ticker_for_market("1260", "TW"), "1260.TWO")
      )
      hits1260 <- search_tw_universe_by_name("1260", max_results = 5L)
      check("search bare 1260 resolves", "1260.TWO" %in% unname(hits1260))
      hits_name <- search_tw_universe_by_name("富味鄉", max_results = 5L)
      check("CJK 富味鄉 hits 1260", "1260.TWO" %in% unname(hits_name))
    }
    qc <- lab_tw_quality_candidates()
    qc_tks <- unique(unlist(qc, use.names = FALSE))
    check(
      "Blue Chip candidates exclude ESB",
      !any(toupper(as.character(u$exchange[match(qc_tks, u$ticker)])) %in%
             c("ESB", "EMERGING", "TPEX_ESB"))
    )
    check(
      "is_tw_esb_ticker true for sample ESB",
      isTRUE(is_tw_esb_ticker(sample_tk))
    )
    check(
      "is_tw_esb_exchange helper",
      isTRUE(is_tw_esb_exchange("ESB")) && isFALSE(is_tw_esb_exchange("TWSE"))
    )
  } else {
    cat("NOTE: tw_universe.csv has no ESB rows yet — refresh to include 興櫃\n")
  }
  # 測試 fixture（若存在）可補強離線興櫃列
  fixture <- file.path(root, "tests", "data", "tw_universe.csv")
  if (file.exists(fixture)) {
    fx <- utils::read.csv(fixture, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    if (any(toupper(as.character(fx$exchange)) %in% c("ESB", "EMERGING"))) {
      check("fixture contains ESB sample", TRUE)
    }
  }
} else {
  cat("SKIP: tw_universe.csv not present for OTC/ESB resolve checks\n")
}

# P0 helper：IS／BS／CF 全空偵測（不需網路）
empty_stmt <- list(
  collapsed = data.frame(Breakdown = character(0), stringsAsFactors = FALSE),
  expanded = data.frame(Breakdown = character(0), stringsAsFactors = FALSE)
)
# Minimal coerce / empty helpers (mirror setup.R; avoid full setup.R side effects)
coerce_financial_df <- function(df) {
  if (is.null(df)) return(NULL)
  if (is.data.frame(df)) return(df)
  if (is.list(df) && !is.null(df$columns) && !is.null(df$data)) {
    cols <- as.character(unlist(df$columns, use.names = FALSE))
    rows <- df$data
    if (is.null(rows) || length(rows) == 0) {
      out <- as.data.frame(matrix(nrow = 0, ncol = length(cols)), stringsAsFactors = FALSE)
      names(out) <- cols
      return(out)
    }
  }
  tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) NULL)
}
financial_df_is_empty <- function(df) {
  df <- coerce_financial_df(df)
  is.null(df) || !is.data.frame(df) || nrow(df) < 1L || ncol(df) < 2L
}
financials_is_bs_cf_all_empty <- function(res) {
  if (is.null(res)) return(TRUE)
  .stmt_empty <- function(key) {
    stmt <- tryCatch(res[[key]], error = function(e) NULL)
    if (is.null(stmt)) return(TRUE)
    exp <- stmt$expanded
    if (is.null(exp)) exp <- stmt$collapsed
    financial_df_is_empty(coerce_financial_df(exp))
  }
  .stmt_empty("Income Statement") &&
    .stmt_empty("Balance Sheet") &&
    .stmt_empty("Cash Flow")
}
check(
  "empty IS/BS/CF detected",
  isTRUE(financials_is_bs_cf_all_empty(list(
    `Income Statement` = empty_stmt,
    `Balance Sheet` = empty_stmt,
    `Cash Flow` = empty_stmt
  )))
)
nonempty <- list(
  collapsed = data.frame(Breakdown = "Revenue", `12/31/2024` = "1", check.names = FALSE),
  expanded = data.frame(Breakdown = "Revenue", `12/31/2024` = "1", check.names = FALSE)
)
check(
  "non-empty CF means not all-empty",
  isFALSE(financials_is_bs_cf_all_empty(list(
    `Income Statement` = empty_stmt,
    `Balance Sheet` = empty_stmt,
    `Cash Flow` = nonempty
  )))
)

# Minimal merge: detail path must keep all evaluated rows even if eq/gate would filter
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(root)
source(file.path(root, "lab_industry_method.R"), local = TRUE, encoding = "UTF-8")

catlg <- data.frame(
  ticker = c("AAA", "BBB", "CCC"),
  industry_key = c("sc.Foundry", "sc.Foundry", "sc.Foundry"),
  industry_label = c("A", "B", "C"),
  primary = c("dcf", "dcf", "dcf"),
  secondary = c("pb", "pb", "pb"),
  stringsAsFactors = FALSE
)
scores <- data.frame(
  ticker = c("AAA", "BBB", "CCC"),
  ok = c(TRUE, TRUE, TRUE),
  f_score = c(8, 5, 9),
  quality_flag = c(1, 0, 1),
  is_quality = c(TRUE, FALSE, TRUE),
  is_quality_upside = c(TRUE, FALSE, TRUE),
  market_cap = c(1e11, 2e10, 5e10),
  size_band = c("large", "mid", "mid"),
  price = c(10, 20, 30),
  fv = c(15, 25, 40),
  upside_total_pct = c(50, 25, 33),
  upside_cagr_pct = c(8, 4, 6),
  n_years = c(5L, 5L, 5L),
  fv_note = c("", "", ""),
  method_used = c("dcf", "dcf", "dcf"),
  primary_live = c("dcf", "dcf", "dcf"),
  company_type = c("", "", ""),
  company_name = c("A", "B", "C"),
  error = c("", "", ""),
  stringsAsFactors = FALSE
)

detail <- lab_merge_catalog_scores(
  catlg, scores = scores,
  eq_only = FALSE, gate_only = FALSE, evaluated_only = TRUE
)
check("detail keeps N=3 evaluated", nrow(detail) == 3L)

gated <- lab_merge_catalog_scores(
  catlg, scores = scores,
  eq_only = TRUE, gate_only = TRUE, evaluated_only = TRUE
)
check("quality filters shrink leaderboard path", nrow(gated) < 3L)

if (fail > 0L) {
  cat("FAILED checks:", fail, "\n")
  quit(status = 1)
}
cat("All market_profile / detail-N checks passed.\n")
