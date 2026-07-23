# ==========================================
# default_config.R - 應用程式全域預設值設定檔
# 說明：統一管理 UI 與 Server 的初始數值，並動態綁定產業 KPI
# ==========================================

# 預設產業：消費電子硬體（與 demo TSM 對齊）
DEFAULT_IND <- "tech.Hardware"
ind_kpi <- industry_standards[[DEFAULT_IND]]

# ---------- 產業綁定基準（防呆）----------
default_beta <- if (!is.null(ind_kpi$beta_avg)) ind_kpi$beta_avg else 1.0
default_rm   <- if (!is.null(ind_kpi$rm_avg))   ind_kpi$rm_avg   else 8.0
default_debt <- if (!is.null(ind_kpi$debt_ratio_avg)) ind_kpi$debt_ratio_avg else 0.25

# 產業營收成長中位，但預設值封頂 10%（避免一開 App 就塞進過高 short-term g）
raw_g <- if (!is.null(ind_kpi$rev_growth)) mean(ind_kpi$rev_growth) else 6
default_g <- round(max(2, min(raw_g, 10)), 2)

# ---------- 利率／CAPM（與即時 Rf 連動）----------
default_rf <- tryCatch(cached_get_risk_free_rate(), error = function(e) 4.0)
if (is.null(default_rf) || is.na(default_rf) || default_rf <= 0) default_rf <- 4.0
default_rf <- round(as.numeric(default_rf), 2)

# 若產業 Rm 低於 Rf+3，自動抬升為 Rf+5（合理股權風險溢酬）
if (is.na(default_rm) || default_rm < default_rf + 3) {
  default_rm <- round(default_rf + 5, 2)
}

# Ke = Rf + Beta × (Rm − Rf)
default_re <- round(default_rf + default_beta * (default_rm - default_rf), 2)

# 債務成本 ≈ Rf + 信用利差（投資級約 1.0%~2.0%）
default_rd <- round(default_rf + 1.5, 2)

# 粗估 WACC（權益／負債權重）；稅率採美國企業稅常見 21%
default_tax <- 21
we <- max(0.05, min(0.95, 1 - default_debt))
wd <- 1 - we
default_wacc <- round(we * default_re + wd * default_rd * (1 - default_tax / 100), 2)

# 永續成長率 SGR：預設採 Macro（直接套用 Rf）；仍須明顯低於 WACC
default_sgr <- round(as.numeric(default_rf), 2)
if (is.na(default_sgr) || default_sgr <= 0) default_sgr <- 4.0
default_sgr <- min(default_sgr, max(0.5, default_wacc - 2))

# P/B 預設：優先產業 pb_band，否則用保守通用區間
if (!is.null(ind_kpi$pb_band) && length(ind_kpi$pb_band) >= 2) {
  pb_lo  <- ind_kpi$pb_band[1]
  pb_hi  <- ind_kpi$pb_band[2]
  pb_mid <- if (length(ind_kpi$pb_band) >= 3) ind_kpi$pb_band[3] else mean(c(pb_lo, pb_hi))
} else {
  pb_lo <- 1.0; pb_mid <- 1.4; pb_hi <- 1.8
}

# ---------- 全域參數表 ----------
APP_DEFAULTS <- list(

  # --- 1. 基本設定 ---
  stock_code      = "TSM",
  industry_choice = DEFAULT_IND,
  years           = 5,

  # --- 2. DDM ---
  ddm_d0          = NA,                 # 由財報／Summary 自動帶入
  ddm_g           = default_sgr,        # 股利 g；預設對齊中央 SGR，可覆寫
  ddm_ke          = default_re,
  ddm_sync_central_g = TRUE,            # 與 Get Started SGR 同步

  # --- 3. Gordon DCF ---
  dcf_mode        = "gordon",
  dcf_chart_mode  = "simple",
  g_growth_method = "fundamental",
  custom_g        = default_g,          # 自訂短期成長（已封頂）
  perpetual_g_method = "fundamental",   # v13：基本面優先（非 Macro=Rf）
  lifecycle_stage = "auto",             # auto = 依產業／成長自動分類
  sgr             = default_sgr,        # DCF／RI 終值 g < WACC
  wacc_gordon     = default_wacc,

  # --- 4. Two-Stage ---
  yr_stage1       = 3,
  g_stage1        = default_g,          # 高速期（封頂後）
  g_stage2        = default_sgr,        # 與 SGR 一致
  wacc_stage1     = default_wacc,
  wacc_stage2     = default_wacc,

  # --- 5. WACC / CAPM ---
  wacc_re         = default_re,
  wacc_rd         = default_rd,
  wacc_tax        = default_tax,

  use_est_re      = TRUE,

  capm_rf         = default_rf,
  capm_beta       = round(default_beta, 2),  # 啟動時占位；搜尋後預設改跟 Finance Summary β
  use_industry_beta = FALSE,                 # FALSE = 跟 Summary β；TRUE = 產業平均
  capm_rm         = round(default_rm, 2),

  # --- 6. P/B／資產法 ---
  pb_bvps         = NA,
  pb_tbvps        = NA,
  pb_low          = round(pb_lo, 2),
  pb_mid          = round(pb_mid, 2),
  pb_high         = round(pb_hi, 2),
  pb_basis        = "bvps",
  pb_use_industry = TRUE,
  # 例外：雙重股權／股數級距校正（預設關閉，由使用者決定是否套用）
  pb_adjust_share_class = FALSE
)
