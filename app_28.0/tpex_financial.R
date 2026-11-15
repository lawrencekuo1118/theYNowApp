# ==========================================
# tpex_financial.R — 櫃買上櫃／興櫃財務資料簡報 fallback
# ==========================================
# 官方來源（非 FinMind、非 Selenium）：
#   索引 GET https://www.tpex.org.tw/www/zh-tw/statistics/financial?date=YYYY
#   上櫃 XLS /storage/statistic/financial/O_YYYYQn.xls
#   興櫃 XLS /storage/statistic/financial/U_YYYYQn.xls
# 僅摘要欄位；CF 維持空。見 tpex_financial_summary.py。

YNOW_TPEX_FINANCIAL_SUMMARY_URL <-
  "https://www.tpex.org.tw/zh-tw/mainboard/listed/financial/summary.html"

# Persistent env for reticulate exports. source_python() defaults to
# envir=parent.frame(); calling it from inside .ensure_* would bind wrappers
# into a throwaway evaluation frame and they vanish on return →
# "could not find function scrape_tpex_financials_fallback" (blank OTC/ESB FS).
.tpex_fs_env <- environment()
.tpex_fs_py_ready <- FALSE

.tpex_fs_fn_ready <- function() {
  exists(
    "scrape_tpex_financials_fallback",
    envir = .tpex_fs_env,
    inherits = FALSE,
    mode = "function"
  )
}

.ensure_tpex_financial_py <- function() {
  if (isTRUE(.tpex_fs_py_ready) && isTRUE(.tpex_fs_fn_ready())) {
    return(TRUE)
  }
  if (identical(Sys.getenv("YNOW_DEBUG_SKIP_PY"), "1")) return(FALSE)
  ok <- FALSE
  tryCatch({
    if (exists(".ynow_ensure_python", mode = "function")) {
      tryCatch(.ynow_ensure_python(), error = function(e) FALSE)
    }
    if (exists(".ensure_python_scraper", mode = "function")) {
      # Prefer shared Python init; ignore failure and still load this module.
      tryCatch(.ensure_python_scraper(), error = function(e) FALSE)
    }
    if (!isTRUE(reticulate::py_available(initialize = FALSE))) {
      suppressMessages(reticulate::py_config())
    }
    py_file <- "tpex_financial_summary.py"
    if (!file.exists(py_file)) {
      return(FALSE)
    }
    # MUST bind into .tpex_fs_env (this sourced module), not parent.frame().
    reticulate::source_python(py_file, envir = .tpex_fs_env)
    .tpex_fs_py_ready <<- TRUE
    ok <- TRUE
  }, error = function(e) {
    if (exists(".ynow_log", mode = "function")) {
      .ynow_log("⚠️ TPEx 財報模組載入失敗: ", e$message)
    }
    ok <<- FALSE
  })
  isTRUE(ok) && isTRUE(.tpex_fs_fn_ready())
}

#' 是否應嘗試櫃買季報摘要 fallback（.TWO 且 Yahoo IS／BS 空或缺）
should_try_tpex_financial_fallback <- function(ticker, yahoo_res = NULL) {
  tk <- toupper(trimws(as.character(ticker %||% "")[1]))
  if (!nzchar(tk) || !grepl("\\.TWO$", tk)) return(FALSE)
  if (is.null(yahoo_res)) return(TRUE)
  if (!exists("financial_df_is_empty", mode = "function") ||
      !exists("coerce_financial_df", mode = "function")) {
    if (exists("financials_is_bs_cf_all_empty", mode = "function")) {
      return(isTRUE(financials_is_bs_cf_all_empty(yahoo_res)))
    }
    return(TRUE)
  }
  is_empty <- financial_df_is_empty(
    coerce_financial_df(yahoo_res[["Income Statement"]]$expanded)
  )
  bs_empty <- financial_df_is_empty(
    coerce_financial_df(yahoo_res[["Balance Sheet"]]$expanded)
  )
  # CF 空不擋 TPEx：摘要本來就不提供 CF，仍可用來補 IS／BS
  isTRUE(is_empty) || isTRUE(bs_empty)
}

#' 抓取櫃買財務資料簡報並轉成 Yahoo 形狀三表（CF 通常為空）
#' @param board "OTC" | "ESB" | NULL（先上櫃再興櫃）
#' @return list(res=, meta=)
.fetch_tpex_financial_raw <- function(ticker, board = NULL) {
  if (!isTRUE(.ensure_tpex_financial_py())) {
    stop("TPEx 財報模組未載入（請確認 tpex_financial_summary.py／xlrd）")
  }
  b <- board
  if (!is.null(b) && !nzchar(as.character(b)[1])) b <- NULL
  if (is.null(b) && exists("is_tw_esb_ticker", mode = "function") &&
      isTRUE(is_tw_esb_ticker(ticker))) {
    b <- "ESB"
  }
  scrape_tpex_financials_fallback(ticker, b)
}

fetch_tpex_financial_fallback <- function(ticker, board = NULL) {
  raw <- .fetch_tpex_financial_raw(ticker, board = board)
  normalize_all_financials(raw)
}

tpex_financial_meta <- function(res) {
  if (is.null(res)) return(NULL)
  list(
    source = attr(res, "tpex_source") %||% NULL,
    board = attr(res, "tpex_board") %||% NULL,
    ok = isTRUE(attr(res, "tpex_ok")),
    note = attr(res, "tpex_note") %||% NULL,
    url = attr(res, "tpex_url") %||% YNOW_TPEX_FINANCIAL_SUMMARY_URL
  )
}

#' Per-statement empty check for Yahoo／TPEx merge.
.tpex_stmt_empty <- function(res, key) {
  if (is.null(res)) return(TRUE)
  stmt <- tryCatch(res[[key]], error = function(e) NULL)
  if (is.null(stmt)) return(TRUE)
  exp <- stmt$expanded
  if (is.null(exp)) exp <- stmt$collapsed
  if (!exists("financial_df_is_empty", mode = "function") ||
      !exists("coerce_financial_df", mode = "function")) {
    return(TRUE)
  }
  financial_df_is_empty(coerce_financial_df(exp))
}

#' Merge TPEx summary into Yahoo: fill empty IS／BS only; never invent CF.
#' Keeps any non-empty Yahoo statement (including Cash Flow) intact.
.merge_yahoo_tpex_statements <- function(yahoo_res, tpex_res) {
  out <- if (is.null(yahoo_res)) list() else yahoo_res
  filled <- character(0)
  for (key in c("Income Statement", "Balance Sheet")) {
    if (isTRUE(.tpex_stmt_empty(out, key)) && !isTRUE(.tpex_stmt_empty(tpex_res, key))) {
      out[[key]] <- tpex_res[[key]]
      filled <- c(filled, key)
    }
  }
  # Do not copy empty TPEx CF over a non-empty Yahoo CF.
  if (isTRUE(.tpex_stmt_empty(out, "Cash Flow")) &&
      !isTRUE(.tpex_stmt_empty(tpex_res, "Cash Flow"))) {
    out[["Cash Flow"]] <- tpex_res[["Cash Flow"]]
  } else if (is.null(out[["Cash Flow"]]) && !is.null(tpex_res[["Cash Flow"]])) {
    out[["Cash Flow"]] <- tpex_res[["Cash Flow"]]
  }
  list(res = out, filled = filled)
}

#' 合併／覆寫：Yahoo IS／BS 缺漏時以 TPEx 摘要補齊（不捏造 CF）
apply_tpex_financial_fallback <- function(yahoo_res, ticker) {
  if (!should_try_tpex_financial_fallback(ticker, yahoo_res)) {
    return(list(res = yahoo_res, used = FALSE, meta = NULL, error = NULL))
  }
  raw <- tryCatch(
    .fetch_tpex_financial_raw(ticker),
    error = function(e) e
  )
  if (inherits(raw, "error")) {
    return(list(
      res = yahoo_res, used = FALSE, meta = NULL,
      error = conditionMessage(raw)
    ))
  }
  py_meta <- tryCatch(as.list(raw[["_meta"]]), error = function(e) list())
  if (!isTRUE(py_meta$ok %||% FALSE)) {
    err <- as.character(py_meta$error %||% "櫃買季報彙總無此代號或解析失敗")[1]
    return(list(res = yahoo_res, used = FALSE, meta = py_meta, error = err))
  }

  tpex <- normalize_all_financials(raw)
  if (isTRUE(.tpex_stmt_empty(tpex, "Income Statement")) &&
      isTRUE(.tpex_stmt_empty(tpex, "Balance Sheet"))) {
    return(list(
      res = yahoo_res, used = FALSE, meta = py_meta,
      error = "櫃買季報彙總解析後 IS／BS 仍為空"
    ))
  }

  merged <- .merge_yahoo_tpex_statements(yahoo_res, tpex)
  if (!length(merged$filled)) {
    return(list(
      res = yahoo_res, used = FALSE, meta = py_meta,
      error = "Yahoo IS／BS 已有內容，無需櫃買摘要覆寫"
    ))
  }
  out <- merged$res

  board <- as.character(py_meta$board %||% "")[1]
  if (!nzchar(board)) {
    board <- if (exists("is_tw_esb_ticker", mode = "function") &&
                 isTRUE(is_tw_esb_ticker(ticker))) "ESB" else "OTC"
  }
  filled_lab <- paste(merged$filled, collapse = "／")
  note <- paste0(
    "已以櫃買「財務資料簡報」（",
    if (identical(board, "ESB")) "興櫃" else "上櫃",
    "季報彙總）補齊缺漏的 ", filled_lab, "；",
    "此為截至當季累計摘要，非完整三表。現金流量表仍空（未提供不捏造）；",
    "未提供之 CapEx／FCF／現金／負債等不會捏造。",
    "資料頁：", YNOW_TPEX_FINANCIAL_SUMMARY_URL
  )
  attr(out, "tpex_source") <- "tpex_financial_summary"
  attr(out, "tpex_ok") <- TRUE
  attr(out, "tpex_board") <- board
  attr(out, "tpex_note") <- note
  attr(out, "tpex_filled") <- merged$filled
  attr(out, "tpex_url") <- YNOW_TPEX_FINANCIAL_SUMMARY_URL
  attr(out, "currency") <- attr(out, "currency") %||% "TWD"
  attr(out, "financialCurrency") <- attr(out, "financialCurrency") %||% "TWD"

  list(
    res = out, used = TRUE,
    meta = list(
      source = "tpex_financial_summary",
      board = board,
      note = note,
      filled = merged$filled,
      url = YNOW_TPEX_FINANCIAL_SUMMARY_URL,
      unmapped = py_meta$unmapped,
      equity_derivation_zh = py_meta$equity_derivation_zh
    ),
    error = NULL
  )
}
