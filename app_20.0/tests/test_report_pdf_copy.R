#!/usr/bin/env Rscript
# Ticker PDF report helpers (broker-style copy pack; no network).
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
test_dir <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  getwd()
}
app_dir <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)
source(file.path(app_dir, "setup.R"), local = FALSE)

fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("OK ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

pack_zh <- build_ticker_report_copy(
  locale = "zh-TW",
  stock_code = "AAPL",
  company_name = "Apple Inc.",
  current_price = 100,
  target_price = 120,
  primary_method = "DCF／FCFF",
  method_rationale = "FCF 穩健，採 DCF 為主。",
  margin_of_safety = (120 - 100) / 120 * 100,
  upside_pct = 20,
  dcf_price = 118,
  pb_value = 110,
  primary_bear = 90,
  primary_base = 120,
  primary_bull = 150,
  wacc = "8%",
  terminal_growth = "2.5%",
  forecast_years = 5,
  dcf_mode = "明確預測期 + Gordon 終值",
  eps = 6,
  bvps = 4,
  roe_pct = 25,
  rev_growth_pct = 8,
  capex_rev_pct = 3,
  capex_avg_pct = 2.5,
  capex_n_years = 4L,
  fscore_total = 7,
  money_prefix = "$"
)
check("zh section1 title", identical(pack_zh$titles$section1, "壹、投資建議"))
check("zh doc_kicker US default", identical(pack_zh$titles$doc_kicker, "美股個股報告"))
check("MOS formula present", grepl("MOS", pack_zh$mos_formula, fixed = TRUE))
check("target formula has 120", grepl("120", pack_zh$target_formula, fixed = TRUE))
check("DCF bullet", any(grepl("DCF", pack_zh$investment_bullets)))
check("Bear/Base/Bull bullet", any(grepl("Bear", pack_zh$investment_bullets)))
check("no peer/lab narrative in bullets", !any(grepl("同業排名|Lab 宇宙|peer ranking", pack_zh$investment_bullets, ignore.case = TRUE)))
check("CapEx growth bullet", any(grepl("CapEx", pack_zh$growth_bullets)))
check("implied P/B = 30", is.finite(pack_zh$implied_pb) && abs(pack_zh$implied_pb - 30) < 1e-6)
check("disclaimer excludes peers", grepl("同業排名|Lab", pack_zh$titles$disclaimer))
check("zh disclaimer self-risk", grepl("自行承擔", pack_zh$titles$disclaimer, fixed = TRUE))
check("zh disclaimer points to About", grepl("關於", pack_zh$titles$disclaimer, fixed = TRUE))
check("analysis paragraphs count >= 6", length(pack_zh$analysis_paragraphs) >= 6L)
check("analysis para1 has MOS", {
  p1 <- pack_zh$analysis_paragraphs[[1]]
  grepl("MOS", p1$body, fixed = TRUE)
})
check("analysis para title 投資立場", {
  identical(pack_zh$analysis_paragraphs[[1]]$title, "投資立場與目標價")
})
check("analysis excludes buy-signal wording misuse", {
  bodies <- vapply(pack_zh$analysis_paragraphs, function(p) p$body, character(1))
  !any(grepl("建議買進|強力買進|Buy now", bodies, ignore.case = TRUE)) &&
    any(grepl("非買進訊號|never a buy signal|不作券商", bodies))
})
check("zh F-Score quality screen section", identical(pack_zh$titles$fscore, "二、F-Score 品質檢核"))
check("zh F-Score bullet uses 品質檢核", any(grepl("品質檢核", pack_zh$investment_bullets)))
check("zh analysis title 品質檢核", {
  titles <- vapply(pack_zh$analysis_paragraphs, function(p) p$title, character(1))
  any(grepl("品質檢核", titles, fixed = TRUE))
})
check("zh no 體質檢核 label", {
  titles <- vapply(pack_zh$analysis_paragraphs, function(p) p$title, character(1))
  !any(grepl("體質檢核", titles, fixed = TRUE)) &&
    !grepl("體質檢核", pack_zh$titles$fscore, fixed = TRUE)
})
check("zh F-Score quality screen section", identical(pack_zh$titles$fscore, "二、F-Score 品質檢核"))
check("zh F-Score bullet uses 品質檢核", any(grepl("品質檢核", pack_zh$investment_bullets)))
check("zh analysis title 品質檢核", {
  titles <- vapply(pack_zh$analysis_paragraphs, function(p) p$title, character(1))
  any(grepl("品質檢核", titles, fixed = TRUE))
})
check("zh no 體質檢核 label", {
  titles <- vapply(pack_zh$analysis_paragraphs, function(p) p$title, character(1))
  !any(grepl("體質檢核", titles, fixed = TRUE)) &&
    !grepl("體質檢核", pack_zh$titles$fscore, fixed = TRUE)
})

pack_en <- build_ticker_report_copy(
  locale = "en",
  current_price = 50,
  target_price = 40,
  primary_method = "P/B",
  margin_of_safety = (40 - 50) / 40 * 100,
  upside_pct = -20,
  money_prefix = "$"
)
check("en section1 title", identical(pack_en$titles$section1, "I. Investment view"))
check("en doc_kicker US default", identical(pack_en$titles$doc_kicker, "US Company Report"))
check("en disclaimer mentions peer exclusion", grepl("peer ranking|lab-universe", pack_en$titles$disclaimer, ignore.case = TRUE))
check("en disclaimer self-risk", grepl("alone bear", pack_en$titles$disclaimer, ignore.case = TRUE))
check("en disclaimer points to About", grepl("About page", pack_en$titles$disclaimer, fixed = TRUE))
check("en analysis paragraphs >= 6", length(pack_en$analysis_paragraphs) >= 6L)
check("en analysis heading key", identical(pack_en$titles$analysis_heading, "Analysis notes (numbered)"))
pack_en_fs <- build_ticker_report_copy(
  locale = "en",
  current_price = 50,
  target_price = 40,
  primary_method = "P/B",
  margin_of_safety = (40 - 50) / 40 * 100,
  upside_pct = -20,
  fscore_total = 6,
  money_prefix = "$"
)
check("en F-Score quality screen section", identical(pack_en_fs$titles$fscore, "F-Score quality screen"))
check("en F-Score not health check", !grepl("health check", pack_en_fs$titles$fscore, ignore.case = TRUE))
check("en F-Score bullet quality screen", any(grepl("quality screen", pack_en_fs$investment_bullets, ignore.case = TRUE)))
pack_en_fs <- build_ticker_report_copy(
  locale = "en",
  current_price = 50,
  target_price = 40,
  primary_method = "P/B",
  margin_of_safety = (40 - 50) / 40 * 100,
  upside_pct = -20,
  fscore_total = 6,
  money_prefix = "$"
)
check("en F-Score quality screen section", identical(pack_en_fs$titles$fscore, "F-Score quality screen"))
check("en F-Score not health check", !grepl("health check", pack_en_fs$titles$fscore, ignore.case = TRUE))
check("en F-Score bullet quality screen", any(grepl("quality screen", pack_en_fs$investment_bullets, ignore.case = TRUE)))

m <- matrix(
  c(100, 110, 120, 105, 115, 125),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("WACC 8%", "WACC 9%"), c("g 1%", "g 2%", "g 3%"))
)
df <- .report_sensitivity_df(m)
check("sensitivity df rows", is.data.frame(df) && nrow(df) == 2L)

tpl <- file.path(app_dir, "report_template.Rmd")
txt <- paste(readLines(tpl, warn = FALSE), collapse = "\n")
check("template has report_copy", grepl("report_copy", txt, fixed = TRUE))
check("template has sensitivity", grepl("sensitivity_df", txt, fixed = TRUE))
check("template no peer-comp section", !grepl("同業比較（相對比較法）", txt, fixed = TRUE))
check("template logo blue", grepl("#0C5484", txt, fixed = TRUE))
check("template logo green", grepl("#249C60", txt, fixed = TRUE))
check("template analysis-box", grepl("analysis-box", txt, fixed = TRUE))
check("template report_condensed", grepl("report_condensed", txt, fixed = TRUE))
check("template has market_mode", grepl("market_mode", txt, fixed = TRUE))
check("template mode-badge", grepl("mode-badge", txt, fixed = TRUE))
check("template gold accent", grepl("#C9A227", txt, fixed = TRUE) || grepl("ynow-gold", txt, fixed = TRUE))
check("template black accent", grepl("#0b0d10", txt, fixed = TRUE) || grepl("ynow-black", txt, fixed = TRUE))
check("template TW sector labels", grepl("次產業", txt, fixed = TRUE) && grepl("產業", txt, fixed = TRUE))
check("template lite FCF gate", grepl("lite_fcf_note", txt, fixed = TRUE))
check("locale keys download_report_about exist", {
  loc_path <- file.path(app_dir, "ui_locale.R")
  loc_txt <- paste(readLines(loc_path, warn = FALSE), collapse = "\n")
  grepl("download_report_about", loc_txt, fixed = TRUE)
})

# Smoke-render HTML (no chrome) with minimal params
tmp_html <- tempfile(fileext = ".html")
ok_render <- FALSE
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  ok_render <- tryCatch({
    rmarkdown::render(
      input = tpl,
      output_file = basename(tmp_html),
      output_dir = dirname(tmp_html),
      intermediates_dir = tempdir(),
      clean = TRUE,
      quiet = TRUE,
      params = list(
        stock_code = "AAPL",
        company_name = "Apple Inc.",
        sector = "Technology",
        industry = "Consumer Electronics",
        report_date = "2026/09/22",
        rating = "未評等（僅列 MOS／價差）",
        rating_en = "NR",
        rating_color = "#6c757d",
        current_price = 100,
        target_price = 120,
        upside_pct = 20,
        dcf_price = 118,
        ddm_value = NA_real_,
        pb_value = 110,
        ri_value = NA_real_,
        ev_value = 2e12,
        margin_of_safety = 16.6667,
        primary_method = "DCF／FCFF",
        method_rationale = "FCF 穩健。",
        primary_bear = 90,
        primary_base = 120,
        primary_bull = 150,
        secondary_point = NA_real_,
        confidence_level = "Medium",
        confidence_score = 0.6,
        wacc = "8%",
        terminal_growth = "2.5%",
        forecast_years = 5,
        dcf_mode = "明確預測期 + Gordon 終值",
        market_cap = "3T",
        pe_ratio = "28",
        beta = "1.1",
        dividend_yield = "0.5%",
        eps = 6,
        bvps = 4,
        kpi_df = data.frame(指標 = "ROE", 數值 = "25.0%", check.names = FALSE),
        fcf_plot_path = NA_character_,
        warnings = "",
        fscore_total = 7,
        fscore_quality = 1,
        fscore_checklist = data.frame(`檢驗維度` = "ROA > 0", `得分` = "通過", check.names = FALSE),
        investment_highlights = pack_zh$investment_bullets,
        summary_df = data.frame(Item = "Market Cap", Value = "3T", stringsAsFactors = FALSE),
        income_df = NULL,
        balance_df = NULL,
        cashflow_df = NULL,
        session_currency = "USD",
        fx_usd_twd = 32,
        report_locale = "zh-TW",
        report_copy = pack_zh,
        sensitivity_df = df,
        app_version = "v20.66",
        report_condensed = FALSE,
        market_mode = "US"
      ),
      envir = new.env(parent = globalenv())
    )
    out_html <- file.path(dirname(tmp_html), basename(tmp_html))
    exists_ok <- file.exists(out_html) || file.exists(tmp_html)
    if (exists_ok) {
      html_body <- paste(readLines(if (file.exists(out_html)) out_html else tmp_html, warn = FALSE), collapse = "\n")
      check("rendered has logo blue", grepl("#0C5484", html_body, fixed = TRUE))
      check("rendered has gold accent", grepl("#C9A227", html_body, fixed = TRUE) || grepl("ynow-gold", html_body))
      check("rendered has mode badge US Full", grepl("US", html_body, fixed = TRUE) && grepl("Full", html_body, fixed = TRUE))
      check("rendered has analysis notes", grepl("分析說明|投資立場與目標價", html_body))
      check("rendered has analysis-box", grepl("analysis-box", html_body, fixed = TRUE))
    }
    exists_ok
  }, error = function(e) {
    cat("RENDER_ERR ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
check("rmarkdown HTML smoke render", isTRUE(ok_render))

# Condensed (Lite) smoke — appendix / FCF / sensitivity omitted
tmp_html_lite <- tempfile(fileext = ".html")
ok_lite <- FALSE
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  pack_lite <- build_ticker_report_copy(
    locale = "zh-TW",
    stock_code = "AAPL",
    company_name = "Apple Inc.",
    current_price = 100,
    target_price = 120,
    primary_method = "DCF／FCFF",
    margin_of_safety = (120 - 100) / 120 * 100,
    upside_pct = 20,
    primary_bear = 90,
    primary_base = 120,
    primary_bull = 150,
    fscore_total = 7,
    money_prefix = "$",
    market_mode = "US",
    report_condensed = TRUE
  )
  check("lite pack truncates analysis to <=4", length(pack_lite$analysis_paragraphs) <= 4L)
  check("lite pack title marks Lite", grepl("Lite", pack_lite$titles$doc_title, fixed = TRUE))
  ok_lite <- tryCatch({
    rmarkdown::render(
      input = tpl,
      output_file = basename(tmp_html_lite),
      output_dir = dirname(tmp_html_lite),
      intermediates_dir = tempdir(),
      clean = TRUE,
      quiet = TRUE,
      params = list(
        stock_code = "AAPL",
        company_name = "Apple Inc.",
        sector = "Technology",
        industry = "Consumer Electronics",
        report_date = "2026/09/22",
        rating = "NR",
        rating_en = "NR",
        rating_color = "#6c757d",
        current_price = 100,
        target_price = 120,
        upside_pct = 20,
        dcf_price = 118,
        ddm_value = NA_real_,
        pb_value = 110,
        ri_value = NA_real_,
        ev_value = 2e12,
        margin_of_safety = 16.6667,
        primary_method = "DCF／FCFF",
        method_rationale = "FCF 穩健。",
        primary_bear = 90,
        primary_base = 120,
        primary_bull = 150,
        secondary_point = NA_real_,
        confidence_level = "Medium",
        confidence_score = 0.6,
        wacc = "8%",
        terminal_growth = "2.5%",
        forecast_years = 5,
        dcf_mode = "明確預測期 + Gordon 終值",
        market_cap = "3T",
        pe_ratio = "28",
        beta = "1.1",
        dividend_yield = "0.5%",
        eps = 6,
        bvps = 4,
        kpi_df = NULL,
        fcf_plot_path = NA_character_,
        warnings = "",
        fscore_total = 7,
        fscore_quality = 1,
        fscore_checklist = data.frame(`檢驗維度` = "ROA > 0", `得分` = "通過", check.names = FALSE),
        investment_highlights = pack_lite$investment_bullets,
        summary_df = data.frame(Item = "Market Cap", Value = "3T", stringsAsFactors = FALSE),
        income_df = data.frame(Item = "Revenue", `2024` = "1", check.names = FALSE),
        balance_df = NULL,
        cashflow_df = NULL,
        session_currency = "USD",
        fx_usd_twd = 32,
        report_locale = "zh-TW",
        report_copy = pack_lite,
        sensitivity_df = df,
        app_version = "v20.66",
        report_condensed = TRUE,
        market_mode = "US"
      ),
      envir = new.env(parent = globalenv())
    )
    out_l <- file.path(dirname(tmp_html_lite), basename(tmp_html_lite))
    path_l <- if (file.exists(out_l)) out_l else tmp_html_lite
    if (!file.exists(path_l)) return(FALSE)
    body_l <- paste(readLines(path_l, warn = FALSE), collapse = "\n")
    check("lite badge present", grepl("Lite", body_l, fixed = TRUE) || grepl("lite-badge", body_l, fixed = TRUE))
    check("lite omits income appendix table", !grepl(">Revenue<", body_l, fixed = TRUE))
    check("lite appendix note", grepl("精簡版省略財報附錄|omits statement", body_l))
    check("lite omits FCF chart lead", !grepl("FCFF path \\(ticker DCF only\\)", body_l))
    check("lite omits sensitivity grid lead", !grepl("WACC × terminal g sensitivity", body_l, fixed = TRUE))
    check("lite omits fscore checklist row", !grepl("ROA &gt; 0|ROA > 0", body_l))
    TRUE
  }, error = function(e) {
    cat("LITE_RENDER_ERR ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
check("rmarkdown Lite condensed smoke render", isTRUE(ok_lite))

# TW Full smoke — industry labels 產業／次產業
tmp_html_tw <- tempfile(fileext = ".html")
ok_tw <- FALSE
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  pack_tw <- build_ticker_report_copy(
    locale = "zh-TW",
    stock_code = "2330",
    company_name = "台積電",
    current_price = 900,
    target_price = 1000,
    primary_method = "DCF／FCFF",
    margin_of_safety = 10,
    upside_pct = 11.1,
    money_prefix = "NT$",
    market_mode = "TW",
    report_condensed = FALSE
  )
  check("tw pack doc_kicker", identical(pack_tw$titles$doc_kicker, "台股個股報告"))
  ok_tw <- tryCatch({
    rmarkdown::render(
      input = tpl,
      output_file = basename(tmp_html_tw),
      output_dir = dirname(tmp_html_tw),
      intermediates_dir = tempdir(),
      clean = TRUE,
      quiet = TRUE,
      params = list(
        stock_code = "2330",
        company_name = "台積電",
        sector = "半導體",
        industry = "晶圓代工",
        report_date = "2026/10/05",
        rating = "NR",
        rating_en = "NR",
        rating_color = "#6c757d",
        current_price = 900,
        target_price = 1000,
        upside_pct = 11.1,
        dcf_price = 980,
        ddm_value = NA_real_,
        pb_value = NA_real_,
        ri_value = NA_real_,
        ev_value = NA_real_,
        margin_of_safety = 10,
        primary_method = "DCF／FCFF",
        method_rationale = "FCF 穩健。",
        primary_bear = 800,
        primary_base = 1000,
        primary_bull = 1200,
        secondary_point = NA_real_,
        confidence_level = "Medium",
        confidence_score = 0.6,
        wacc = "8%",
        terminal_growth = "2.5%",
        forecast_years = 5,
        dcf_mode = "明確預測期 + Gordon 終值",
        market_cap = "20T",
        pe_ratio = "20",
        beta = "1.0",
        dividend_yield = "2%",
        eps = 40,
        bvps = 80,
        kpi_df = NULL,
        fcf_plot_path = NA_character_,
        warnings = "",
        fscore_total = 7,
        fscore_quality = 1,
        fscore_checklist = NULL,
        investment_highlights = pack_tw$investment_bullets,
        summary_df = NULL,
        income_df = NULL,
        balance_df = NULL,
        cashflow_df = NULL,
        session_currency = "TWD",
        fx_usd_twd = 32,
        report_locale = "zh-TW",
        report_copy = pack_tw,
        sensitivity_df = NULL,
        app_version = "v20.66",
        report_condensed = FALSE,
        market_mode = "TW"
      ),
      envir = new.env(parent = globalenv())
    )
    out_t <- file.path(dirname(tmp_html_tw), basename(tmp_html_tw))
    path_t <- if (file.exists(out_t)) out_t else tmp_html_tw
    if (!file.exists(path_t)) return(FALSE)
    body_t <- paste(readLines(path_t, warn = FALSE), collapse = "\n")
    check("tw mode badge", grepl("TW", body_t, fixed = TRUE) && grepl("Full", body_t, fixed = TRUE))
    check("tw industry labels", grepl("產業", body_t, fixed = TRUE) && grepl("次產業", body_t, fixed = TRUE))
    TRUE
  }, error = function(e) {
    cat("TW_RENDER_ERR ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
}
check("rmarkdown TW Full smoke render", isTRUE(ok_tw))

# Server wiring smoke (static source checks)
srv <- paste(readLines(file.path(app_dir, "ynow_server.R"), warn = FALSE), collapse = "\n")
check("server passes market_mode to report", grepl("market_mode = rep_mm", srv, fixed = TRUE))
check("server filename has market+edition", grepl("YNow_Report_", srv, fixed = TRUE) && grepl("\"Lite\"", srv, fixed = TRUE))
check("server reads ynow_build version", grepl("ynow_build.json", srv, fixed = TRUE))

if (fail > 0L) {
  cat("FAILED ", fail, " checks\n", sep = "")
  quit(status = 1L)
}
cat("ALL OK\n")
