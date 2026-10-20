# ==========================================
# lab_industry_method.R — 實驗區：產業 × 評價方法 × 績優候選
#
# 宇宙依市場模式：美股＝Nasdaq／NYSE 主要上市（lab_us_universe.R；S&P GICS 疊加）；台股＝上市／上櫃／興櫃（lab_tw_universe.R；績優候選不含興櫃）。
# 績優原則：在 Piotroski 高門檻（F-Score≥7；不含盈餘品質）後，選「模型合理價相對現價」、
# 並依各標的財報推算之預測年數 n（生命週期建議；非一體適用 APP_DEFAULTS$years）換算年化漲幅最大者。
# 「宇宙檔數（N）」（lab_im_max_n；預設 25）＝分析後明細／排行最終顯示上限（非 Yahoo 撈取檔數）。
# 撈取／評估檔數＝lab_resolve_im_eval_n(N)（通常大於 N）；明細＝合格池取前 N，不足不湊滿。
# 盈餘品質／Piotroski 高門檻勾選套用於排行榜與明細顯示池（同一合格定義）。
# 流程：宇宙池先依「候選截斷邏輯」全市排序／篩選（市值／概念股／近一年漲幅／隨機），
# 再取評估檔數 eval_n 的前段 → Yahoo 評估 → 合格者最多顯示 N 檔。
# 市值模式：對整池排序後再取 eval_n（已有市值時不做 S&P 預篩，避免打亂排序）。
# 排行榜＝同一批合格者的 Top 10（預設 F-Score≥7；gate_only=FALSE 時不設 F 門檻；不足 10 不湊滿）。
# 產業建議方法對齊 recommend_valuation_models 的產業層規則（簡化估值）。
#
# C-STRUCT-03: implementation split across sibling modules (sourced below).
# Nested universe sources stay here. No formula / Lite / auth semantic change.
# ==========================================

# ---- Sidebar taxonomy / DT / company names ----
source("lab_im_sidebar.R", local = TRUE, encoding = "UTF-8")

# ---- Pool ranking / max-N / ticker params ----
source("lab_im_pool.R", local = TRUE, encoding = "UTF-8")

# S&P 500（GICS）＋美股全市場主要上市宇宙；須在候選目錄／名稱查詢之前載入
source("lab_sp500_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_adr.R", local = TRUE, encoding = "UTF-8")
source("lab_us_universe.R", local = TRUE, encoding = "UTF-8")
source("lab_tw_universe.R", local = TRUE, encoding = "UTF-8")

# ---- Blue Chip eval / catalog / F-Score screen ----
source("lab_im_bluechip_eval.R", local = TRUE, encoding = "UTF-8")

# ---- Quality snapshot boards / leaderboards ----
source("lab_im_quality.R", local = TRUE, encoding = "UTF-8")

# ---- Industry-chain mcap ranking ----
source("lab_im_ind_mcap.R", local = TRUE, encoding = "UTF-8")

#' Models the query filter can unlock (sidebar order).
#' Absolute Justified P/B (`pb`) unlocks from Fair Value primary/secondary.
#' Relative families (including Book multiples) unlock from Multiples reference.
lab_im_available_primary_methods <- function(catalog) {
  if (is.null(catalog) || !is.data.frame(catalog) || !nrow(catalog)) {
    return(character(0))
  }
  keys <- character(0)
  if ("primary" %in% names(catalog)) {
    p <- unique(as.character(catalog$primary))
    p <- p[nzchar(p) & !is.na(p)]
    p <- intersect(p, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
    keys <- c(keys, p)
  }
  if ("secondary" %in% names(catalog)) {
    s <- unique(as.character(catalog$secondary))
    s <- s[nzchar(s) & !is.na(s)]
    s <- intersect(s, c("nav", "dcf", "ddm", "ri", "pb", "sotp"))
    keys <- c(keys, s)
  }
  if ("rel_family" %in% names(catalog)) {
    f <- unique(as.character(catalog$rel_family))
    f <- f[nzchar(f) & !is.na(f)]
    f <- intersect(f, c("enterprise", "earnings", "revenue", "book"))
    keys <- c(keys, f)
  }
  lab_order_methods_like_sidebar(unique(keys))
}

#' 顯示用摘要：依主方法分組的產業數／候選檔數
#' Row order follows sidebar model order
#' (NAV → Justified P/B → DCF → DDM → RI → Enterprise → Earnings → Revenue → Book multiples → SOTP).
#' @param include_all TRUE：側欄模型列都顯示（無主模型者產業數／候選檔數為 0）
#' @param locale UI locale (`en` / `zh-TW`); display labels and column headers follow locale.
lab_method_group_summary <- function(catalog, include_all = FALSE, locale = "zh-TW") {
  labels <- lab_method_labels(locale)
  empty_row <- function(m) {
    data.frame(
      建議評價方法 = as.character(labels[[m]] %||% m),
      產業數 = 0L,
      候選檔數 = 0L,
      method_key = m,
      has_primary = FALSE,
      stringsAsFactors = FALSE
    )
  }
  out <- if (is.null(catalog) || !nrow(catalog)) {
    if (isTRUE(include_all)) {
      do.call(rbind, lapply(LAB_SIDEBAR_METHOD_ORDER, empty_row))
    } else {
      data.frame(
        建議評價方法 = character(0),
        產業數 = integer(0),
        候選檔數 = integer(0),
        method_key = character(0),
        has_primary = logical(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    methods <- if (isTRUE(include_all)) {
      as.character(LAB_SIDEBAR_METHOD_ORDER)
    } else {
      lab_order_methods_like_sidebar(catalog$primary)
    }
    do.call(rbind, lapply(methods, function(m) {
      sub <- if (m %in% c("enterprise", "earnings", "revenue", "book")) {
        if (!"rel_family" %in% names(catalog)) {
          catalog[0, , drop = FALSE]
        } else {
          catalog[as.character(catalog$rel_family) == m, , drop = FALSE]
        }
      } else {
        catalog[as.character(catalog$primary) == m, , drop = FALSE]
      }
      n_tk <- length(unique(stats::na.omit(sub$ticker)))
      data.frame(
        建議評價方法 = as.character(labels[[m]] %||% m),
        產業數 = if (n_tk > 0L) length(unique(sub$industry_key)) else 0L,
        候選檔數 = as.integer(n_tk),
        method_key = m,
        has_primary = n_tk > 0L,
        stringsAsFactors = FALSE
      )
    }))
  }
  lab_localize_im_colnames(out, locale)
}

#' 明細表：F-Score 著色膠囊（盈餘品質／Piotroski 高門檻改由工具列勾選過濾）
lab_html_fscore_pill <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(v),
    '<span class="ynow-lab-pill ynow-lab-pill-muted">—</span>',
    sprintf('<span class="ynow-lab-pill ynow-lab-pill-fs">%s</span>', as.integer(round(v)))
  )
}

lab_df_to_csv_text <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return("(無資料)\n")
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)
  utils::write.csv(df, tf, row.names = FALSE, fileEncoding = "UTF-8")
  paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

lab_lab_report_use_zip <- function() {
  nzchar(Sys.which("zip"))
}

#' 本頁報告：markdown 表頭 + 摘要／排行／明細 CSV（zip；無 zip 則單一 .md）
lab_write_lab_page_report <- function(dest, header_lines,
                                      summary_df = NULL,
                                      leaderboard_df = NULL,
                                      detail_df = NULL,
                                      as_zip = NULL) {
  if (is.null(as_zip)) as_zip <- lab_lab_report_use_zip()
  hdr <- as.character(header_lines %||% character(0))
  if (isTRUE(as_zip)) {
    td <- tempfile("ynow_lab_report_")
    dir.create(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    writeLines(hdr, file.path(td, "00_header.md"), useBytes = TRUE)
    utils::write.csv(
      if (is.null(summary_df)) data.frame(訊息 = "無摘要") else summary_df,
      file.path(td, "01_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    utils::write.csv(
      if (is.null(leaderboard_df)) data.frame(訊息 = "尚未評估") else leaderboard_df,
      file.path(td, "02_leaderboard.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    utils::write.csv(
      if (is.null(detail_df)) data.frame(訊息 = "無明細") else detail_df,
      file.path(td, "03_detail.csv"), row.names = FALSE, fileEncoding = "UTF-8"
    )
    files <- list.files(td)
    if (file.exists(dest)) unlink(dest)
    owd <- getwd()
    setwd(td)
    on.exit(setwd(owd), add = TRUE)
    zip_ok <- tryCatch({
      utils::zip(dest, files = files, flags = "-r9Xq")
      file.exists(dest) && isTRUE(file.info(dest)$size > 20)
    }, error = function(e) FALSE)
    if (isTRUE(zip_ok)) return(invisible(dest))
  }
  body <- c(
    hdr,
    "",
    "## 摘要（候選檔數）",
    "```csv",
    lab_df_to_csv_text(summary_df),
    "```",
    "",
    "## 績優排行榜",
    "```csv",
    lab_df_to_csv_text(leaderboard_df),
    "```",
    "",
    "## 明細",
    "```csv",
    lab_df_to_csv_text(detail_df),
    "```",
    ""
  )
  writeLines(body, dest, useBytes = TRUE)
  invisible(dest)
}
