# ==========================================
# fs_locale_zh_tw.R — 財報科目標籤（Yahoo English → 台灣正體）
# 僅供「顯示層」使用；估值／KPI 仍以英文科目比對。
# 台股模式（zh-TW）自動套用；美股維持英文。
# ==========================================

#' Yahoo Finance 常見科目 → 台灣會計／財報用語
.FS_LABEL_ZH_TW <- c(
  # ---- Income Statement ----
  "Total Revenue" = "營業收入總額",
  "Operating Revenue" = "營業收入",
  "Cost Of Revenue" = "營業成本",
  "Reconciled Cost Of Revenue" = "營業成本（調整後）",
  "Gross Profit" = "毛利",
  "Operating Expense" = "營業費用",
  "Operating Expenses" = "營業費用",
  "Selling General And Administration" = "銷管費用",
  "Selling And Marketing Expense" = "銷售費用",
  "General And Administrative Expense" = "管理費用",
  "Other Gand A" = "其他銷管費用",
  "Research And Development" = "研究發展費用",
  "Other Operating Expenses" = "其他營業費用",
  "Depreciation Income Statement" = "折舊（損益表）",
  "Depreciation And Amortization In Income Statement" = "折舊與攤銷（損益表）",
  "Depreciation Amortization Depletion Income Statement" = "折舊攤銷耗竭（損益表）",
  "Operating Income" = "營業利益",
  "Total Operating Income As Reported" = "帳列營業利益",
  "Net Non Operating Interest Income Expense" = "營業外淨利息收支",
  "Net Interest Income" = "淨利息收入",
  "Interest Income" = "利息收入",
  "Interest Expense" = "利息費用",
  "Interest Income Non Operating" = "營業外利息收入",
  "Total Other Finance Cost" = "其他財務成本",
  "Net Non Operating Interest Income Expense" = "營業外淨利息收支",
  "Other Income Expense" = "其他損益",
  "Other Non Operating Income Expenses" = "其他營業外收支",
  "Special Income Charges" = "特殊損益",
  "Gain On Sale Of Business" = "處分事業利益",
  "Write Off" = "沖銷",
  "Earnings From Equity Interest" = "權益法認列損益",
  "Gain On Sale Of Security" = "處分證券利益",
  "Pretax Income" = "稅前淨利",
  "Tax Provision" = "所得稅費用",
  "Tax Effect Of Unusual Items" = "非常項目稅額影響",
  "Tax Rate For Calcs" = "計算用稅率",
  "Net Income Continuous Operations" = "繼續營業單位淨利",
  "Net Income From Continuing Operation Net Minority Interest" = "繼續營業淨利（歸屬母公司）",
  "Net Income From Continuing And Discontinued Operation" = "繼續及停業單位淨利",
  "Net Income Including Noncontrolling Interests" = "合併淨利（含非控制權益）",
  "Minority Interests" = "非控制權益損益",
  "Net Income" = "淨利",
  "Net Income Common Stockholders" = "普通股股東淨利",
  "Diluted NI Availto Com Stockholders" = "稀釋後普通股可分配淨利",
  "Basic EPS" = "基本每股盈餘",
  "Diluted EPS" = "稀釋每股盈餘",
  "Basic Average Shares" = "基本加權平均股數",
  "Diluted Average Shares" = "稀釋加權平均股數",
  "Total Expenses" = "費用合計",
  "Normalized Income" = "常續性淨利",
  "EBIT" = "EBIT（營業利益＋利息稅前）",
  "EBITDA" = "EBITDA",
  "Normalized EBITDA" = "常續性 EBITDA",
  "Reconciled Depreciation" = "折舊攤銷（調節）",
  "Total Unusual Items" = "非常項目合計",
  "Total Unusual Items Excluding Goodwill" = "非常項目（不含商譽）",

  # ---- Balance Sheet ----
  "Total Assets" = "資產總額",
  "Current Assets" = "流動資產",
  "Cash Cash Equivalents And Short Term Investments" = "現金、約當現金及短期投資",
  "Cash And Cash Equivalents" = "現金及約當現金",
  "Cash Equivalents" = "約當現金",
  "Cash Financial" = "庫存現金／銀行存款",
  "Other Short Term Investments" = "其他短期投資",
  "Receivables" = "應收款項",
  "Accounts Receivable" = "應收帳款",
  "Gross Accounts Receivable" = "應收帳款總額",
  "Allowance For Doubtful Accounts Receivable" = "備抵呆帳",
  "Notes Receivable" = "應收票據",
  "Duefrom Related Parties Current" = "應收關係人款（流動）",
  "Inventory" = "存貨",
  "Raw Materials" = "原料",
  "Work In Process" = "在製品",
  "Finished Goods" = "製成品",
  "Other Current Assets" = "其他流動資產",
  "Restricted Cash" = "受限制現金",
  "Hedging Assets Current" = "避險資產（流動）",
  "Total Non Current Assets" = "非流動資產",
  "Net PPE" = "不動產廠房及設備淨額",
  "Gross PPE" = "不動產廠房及設備總額",
  "Accumulated Depreciation" = "累計折舊",
  "Land And Improvements" = "土地及改良物",
  "Buildings And Improvements" = "房屋及建築",
  "Machinery Furniture Equipment" = "機器設備及生財器具",
  "Construction In Progress" = "未完工程",
  "Other Properties" = "其他不動產",
  "Properties" = "不動產",
  "Goodwill And Other Intangible Assets" = "商譽及其他無形資產",
  "Goodwill" = "商譽",
  "Other Intangible Assets" = "其他無形資產",
  "Investments And Advances" = "投資及墊款",
  "Long Term Equity Investment" = "長期股權投資",
  "Investmentsin Associatesat Cost" = "採權益法之投資",
  "Investmentin Financial Assets" = "金融資產投資",
  "Available For Sale Securities" = "備供出售金融資產",
  "Held To Maturity Securities" = "持有至到期日金融資產",
  "Financial Assets" = "金融資產",
  "Non Current Deferred Taxes Assets" = "遞延所得稅資產",
  "Non Current Deferred Assets" = "遞延資產",
  "Non Current Prepaid Assets" = "預付款項（非流動）",
  "Other Non Current Assets" = "其他非流動資產",
  "Total Liabilities Net Minority Interest" = "負債總額",
  "Current Liabilities" = "流動負債",
  "Accounts Payable" = "應付帳款",
  "Payables" = "應付款項",
  "Other Payable" = "其他應付款",
  "Payables And Accrued Expenses" = "應付及應計費用",
  "Current Accrued Expenses" = "流動應計費用",
  "Income Tax Payable" = "應付所得稅",
  "Total Tax Payable" = "應付稅款合計",
  "Dividends Payable" = "應付股利",
  "Dueto Related Parties Current" = "應付關係人款（流動）",
  "Current Debt And Capital Lease Obligation" = "流動借款及租賃負債",
  "Current Debt" = "流動借款",
  "Other Current Borrowings" = "其他流動借款",
  "Other Current Liabilities" = "其他流動負債",
  "Pensionand Other Post Retirement Benefit Plans Current" = "流動退休金負債",
  "Total Non Current Liabilities Net Minority Interest" = "非流動負債",
  "Long Term Debt And Capital Lease Obligation" = "長期借款及租賃負債",
  "Long Term Debt" = "長期借款",
  "Long Term Capital Lease Obligation" = "長期租賃負債",
  "Capital Lease Obligations" = "租賃負債",
  "Non Current Deferred Taxes Liabilities" = "遞延所得稅負債",
  "Non Current Deferred Liabilities" = "遞延負債",
  "Non Current Pension And Other Postretirement Benefit Plans" = "非流動退休金負債",
  "Employee Benefits" = "員工福利負債",
  "Other Non Current Liabilities" = "其他非流動負債",
  "Total Debt" = "附息負債合計",
  "Net Debt" = "淨負債",
  "Stockholders Equity" = "股東權益",
  "Common Stock Equity" = "普通股權益",
  "Total Equity Gross Minority Interest" = "權益總額（含非控制權益）",
  "Minority Interest" = "非控制權益",
  "Capital Stock" = "股本",
  "Common Stock" = "普通股股本",
  "Additional Paid In Capital" = "資本公積",
  "Retained Earnings" = "保留盈餘",
  "Treasury Stock" = "庫藏股",
  "Treasury Shares Number" = "庫藏股數",
  "Ordinary Shares Number" = "普通股股數",
  "Share Issued" = "已發行股數",
  "Gains Losses Not Affecting Retained Earnings" = "其他權益",
  "Other Equity Adjustments" = "其他權益調整",
  "Foreign Currency Translation Adjustments" = "國外營運機構財務報表換算差額",
  "Unrealized Gain Loss" = "未實現損益",
  "Other Equity Interest" = "其他權益",
  "Tangible Book Value" = "有形帳面價值",
  "Net Tangible Assets" = "有形淨資產",
  "Working Capital" = "營運資金",
  "Invested Capital" = "投入資本",
  "Total Capitalization" = "資本總額",
  "Total Equity Gross Minority Interest" = "權益總額（含非控制權益）",

  # ---- Cash Flow ----
  "Operating Cash Flow" = "營業活動之現金流量",
  "Cash Flow From Continuing Operating Activities" = "繼續營業之營業現金流量",
  "Investing Cash Flow" = "投資活動之現金流量",
  "Cash Flow From Continuing Investing Activities" = "繼續營業之投資現金流量",
  "Financing Cash Flow" = "籌資活動之現金流量",
  "Cash Flow From Continuing Financing Activities" = "繼續營業之籌資現金流量",
  "Free Cash Flow" = "自由現金流",
  "Capital Expenditure" = "資本支出",
  "Purchase Of PPE" = "購置不動產廠房及設備",
  "Sale Of PPE" = "處分不動產廠房及設備",
  "Net PPE Purchase And Sale" = "不動產廠房及設備淨變動",
  "Purchase Of Intangibles" = "購置無形資產",
  "Sale Of Intangibles" = "處分無形資產",
  "Net Intangibles Purchase And Sale" = "無形資產淨變動",
  "Purchase Of Business" = "購併支出",
  "Net Business Purchase And Sale" = "事業購售淨額",
  "Purchase Of Investment" = "購買投資",
  "Sale Of Investment" = "出售投資",
  "Net Investment Purchase And Sale" = "投資淨變動",
  "Net Income From Continuing Operations" = "繼續營業單位淨利（現金流量）",
  "Depreciation" = "折舊",
  "Depreciation And Amortization" = "折舊與攤銷",
  "Depreciation Amortization Depletion" = "折舊攤銷耗竭",
  "Amortization Cash Flow" = "攤銷（現金流量）",
  "Amortization Of Intangibles" = "無形資產攤銷",
  "Stock Based Compensation" = "股份基礎給付",
  "Asset Impairment Charge" = "資產減損",
  "Other Non Cash Items" = "其他非現金項目",
  "Change In Working Capital" = "營運資金變動",
  "Change In Receivables" = "應收款項變動",
  "Changes In Account Receivables" = "應收帳款變動",
  "Change In Inventory" = "存貨變動",
  "Change In Account Payable" = "應付帳款變動",
  "Change In Payable" = "應付款變動",
  "Change In Accrued Expense" = "應計費用變動",
  "Change In Payables And Accrued Expense" = "應付及應計費用變動",
  "Change In Other Current Assets" = "其他流動資產變動",
  "Change In Other Current Liabilities" = "其他流動負債變動",
  "Change In Other Working Capital" = "其他營運資金變動",
  "Taxes Refund Paid" = "退（付）稅款",
  "Cash Dividends Paid" = "發放現金股利",
  "Common Stock Dividend Paid" = "普通股現金股利",
  "Repurchase Of Capital Stock" = "庫藏股買回",
  "Net Common Stock Issuance" = "普通股發行淨額",
  "Common Stock Payments" = "普通股相關支付",
  "Issuance Of Debt" = "舉借借款",
  "Repayment Of Debt" = "償還借款",
  "Net Issuance Payments Of Debt" = "借款淨變動",
  "Net Long Term Debt Issuance" = "長期借款淨變動",
  "Long Term Debt Issuance" = "舉借長期借款",
  "Long Term Debt Payments" = "償還長期借款",
  "Net Short Term Debt Issuance" = "短期借款淨變動",
  "Short Term Debt Issuance" = "舉借短期借款",
  "Short Term Debt Payments" = "償還短期借款",
  "Interest Paid Cff" = "支付利息（籌資）",
  "Interest Received Cfi" = "收取利息（投資）",
  "Dividends Received Cfi" = "收取股利（投資）",
  "Net Other Financing Charges" = "其他籌資項目",
  "Net Other Investing Changes" = "其他投資項目",
  "Changes In Cash" = "現金增（減）數",
  "Effect Of Exchange Rate Changes" = "匯率影響數",
  "Beginning Cash Position" = "期初現金",
  "End Cash Position" = "期末現金",
  "Operating Gains Losses" = "營業相關損益（非現金）",
  "Gain Loss On Sale Of PPE" = "處分不動產損益",
  "Gain Loss On Sale Of Business" = "處分事業損益",
  "Gain Loss On Investment Securities" = "投資證券損益",
  "Earnings Losses From Equity Investments" = "權益法投資損益",
  "Net Foreign Currency Exchange Gain Loss" = "外幣兌換損益",

  # ---- Finance Summary（Yahoo quote；含常見別名）----
  "Previous Close" = "前一日收盤",
  "Open" = "開盤",
  "Bid" = "買價",
  "Ask" = "賣價",
  "Day's Range" = "當日股價區間",
  "52 Week Range" = "52 週股價區間",
  "Volume" = "成交量",
  "Avg. Volume" = "平均成交量",
  "Market Cap (intraday)" = "市值（盤中）",
  "Beta (5Y Monthly)" = "Beta（五年月資料）",
  "PE Ratio (TTM)" = "本益比（TTM）",
  "EPS (TTM)" = "每股盈餘（TTM）",
  "Earnings Date" = "財報公布日",
  "Forward Dividend & Yield" = "預期股利與殖利率",
  "Dividend" = "股利",
  "Yield" = "殖利率",
  "Ex-Dividend Date" = "除息日",
  "1y Target Est" = "一年目標價（預估）",
  "Target Est" = "目標價（預估）"
)

.FS_SECTION_ZH_TW <- c(
  Price = "股價",
  Volume = "成交量",
  Valuation = "評價",
  Dividend = "股利",
  Other = "其他"
)

.FS_COL_ZH_TW <- c(
  Breakdown = "科目",
  Item = "項目",
  Value = "數值",
  TTM = "近十二個月（TTM）",
  ttm = "近十二個月（TTM）"
)

#' 正規化科目鍵（去空白、統一 And）
.normalize_fs_label_key <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("&", "And", x, fixed = TRUE)
  x
}

#' 單一科目標籤 → 台灣正體（無對照則保留原文）
localize_fs_label_zh_tw <- function(label) {
  raw <- as.character(label %||% "")[1]
  if (!nzchar(raw)) return(raw)
  key <- .normalize_fs_label_key(raw)
  keys <- names(.FS_LABEL_ZH_TW)
  if (key %in% keys) {
    hit <- unname(.FS_LABEL_ZH_TW[[key]])
    if (nzchar(hit)) return(hit)
  }
  # 大小寫不敏感備援
  idx <- match(tolower(key), tolower(keys))
  if (!is.na(idx)) {
    hit <- unname(.FS_LABEL_ZH_TW[[keys[idx]]])
    if (nzchar(hit)) return(hit)
  }
  raw
}

#' 向量化科目標籤
localize_fs_labels_zh_tw <- function(labels) {
  vapply(as.character(labels), localize_fs_label_zh_tw, character(1), USE.NAMES = FALSE)
}

#' 是否應對財報顯示套用 zh-TW 科目
should_localize_fs_zh_tw <- function(mode = NULL, locale = NULL) {
  if (!is.null(locale) && identical(normalize_ui_locale(locale), "zh-TW")) return(TRUE)
  if (!is.null(mode) && identical(normalize_market_mode(mode), "TW")) return(TRUE)
  if (exists("get_market_mode", mode = "function") &&
      identical(normalize_market_mode(get_market_mode()), "TW")) {
    return(TRUE)
  }
  FALSE
}

#' 顯示用：財報表第一欄科目翻成台灣正體；欄名 Breakdown→科目
#' 不變更數值欄；估值用的原始英文資料請勿覆寫。
localize_financial_df_zh_tw <- function(df, enabled = TRUE) {
  if (!isTRUE(enabled) || is.null(df) || !is.data.frame(df) || nrow(df) < 1 || ncol(df) < 1) {
    return(df)
  }
  out <- df
  out[[1]] <- localize_fs_labels_zh_tw(out[[1]])
  cn <- colnames(out)
  for (i in seq_along(cn)) {
    k <- as.character(cn[i])
    if (k %in% names(.FS_COL_ZH_TW)) {
      cn[i] <- unname(.FS_COL_ZH_TW[[k]])
    } else if (identical(i, 1L) && !grepl("[\\u4e00-\\u9fff]", k, perl = TRUE)) {
      # 常見第一欄別名
      if (grepl("breakdown|item|metric|account|line", k, ignore.case = TRUE)) {
        cn[i] <- "科目"
      }
    }
  }
  colnames(out) <- cn
  out
}

#' Finance Summary Item 顯示名
localize_summary_item_zh_tw <- function(item, enabled = TRUE) {
  if (!isTRUE(enabled)) return(as.character(item))
  localize_fs_label_zh_tw(item)
}

#' Finance Summary 區塊標題
localize_summary_section_zh_tw <- function(section, enabled = TRUE) {
  sec <- as.character(section %||% "")[1]
  if (!isTRUE(enabled)) return(sec)
  hit <- .FS_SECTION_ZH_TW[[sec]]
  if (!is.null(hit) && nzchar(hit)) return(unname(hit))
  sec
}

#' Income Statement 圖表選單：值維持英文（計算用），標籤可 zh-TW
is_metric_choices_for_locale <- function(locale_or_mode = "en") {
  en_vals <- c("Total Revenue", "Gross Profit", "EBITDA")
  use_zh <- should_localize_fs_zh_tw(
    mode = locale_or_mode,
    locale = locale_or_mode
  )
  if (!use_zh) {
    return(stats::setNames(en_vals, en_vals))
  }
  labs <- localize_fs_labels_zh_tw(en_vals)
  stats::setNames(en_vals, labs)
}
