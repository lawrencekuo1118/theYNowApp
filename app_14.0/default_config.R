# ==========================================
# default_config.R - 應用程式全域預設值設定檔
# 說明：統一管理 UI 與 Server 的初始數值，並動態綁定產業 KPI
# ==========================================

# 預設產業：半導體｜晶圓代工（與 demo TSM 對齊）
DEFAULT_IND <- "sc.Foundry"
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

# 債務成本 rᵈ：不設全域預設；載入財報後以利息費用／總負債覆寫（見 ynow_server）

# 粗估 WACC 啟動占位（僅 Ke）；有負債後由實際 rᵈ 重算
default_tax <- 21
we <- max(0.05, min(0.95, 1 - default_debt))
wd <- 1 - we
default_wacc <- round(default_re, 2)

# 永續成長率 SGR：啟動值錨在 Rf，執行基本面／生命週期法後由 central_perpetual_g 覆寫；須明顯低於 WACC
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
  ddm_mode        = "gordon",           # gordon | two_stage
  ddm_g_stage1    = default_g,          # 二階段高速期股利成長
  ddm_yr_stage1   = 5,                  # 二階段高速期年數

  # --- 3. Gordon DCF ---
  dcf_mode        = "gordon",
  dcf_claim       = "fcff",             # fcff (WACC+EV 橋接) | fcfe (Ke 直接股權)
  dcf_chart_mode  = "simple",
  # Dashboard Cash Flow：固定顯示營業／投資／融資三線疊圖（融資 FCF ≠ 自由現金流）
  cf_flow_series  = c("ocf", "icf", "fcf"),
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
  wacc_rd         = NA_real_,           # 由財報利息／負債覆寫；無預設
  wacc_rd_min     = 0,                  # 財報推估 rᵈ 下限（可調）
  wacc_rd_max     = 40,                 # 財報推估 rᵈ 上限（可調）
  wacc_tax        = default_tax,

  use_est_re      = TRUE,

  capm_rf         = default_rf,
  capm_beta       = round(default_beta, 2),  # 啟動占位；估值路徑就緒後改寫入選定來源（預設 Summary β）
  sync_gs_beta    = TRUE,                    # TRUE = WACC/CAPM β 跟隨 Get Started「套用至 CAPM」
  capm_rm         = round(default_rm, 2),
  beta_bench      = "SPY",
  beta_lookback_months = 60,                 # Rolling 僅對照用（不寫入 CAPM）
  beta_min_obs    = 24,
  # Rolling 估計不得寫入 CAPM；Summary β 可為預設套用來源
  beta_purpose    = "valuation",
  # 去槓桿化 βᵤ = Hamada(β_L,T,D/E)；β_L 預設 Summary
  beta_bl_source  = "summary",
  beta_peers      = "",
  beta_bottomup_agg = "mean",                # mean | median
  # 舊版再槓桿設定已移除（隱藏相容）；不再提供目標 D/E UI
  beta_relever_de_mode = "current",
  beta_target_de  = NA,
  # 套用至 CAPM：summary | industry | bottomup | unlever_firm | manual
  beta_u_apply_source = "summary",
  beta_u_manual   = NA,                      # 手動 β（直接寫入 CAPM）

  # --- 6. Residual Income ---
  ri_years        = 5,
  ri_roe          = 15,                      # 財報載入前占位；載入後覆寫
  ri_payout       = 40,                      # 財報載入前占位；載入後覆寫
  roe_method      = "constant",              # constant / linear / industry / custom

  # --- 7. P/B／資產法 ---
  pb_bvps         = NA,
  pb_tbvps        = NA,
  pb_low          = round(pb_lo, 2),
  pb_mid          = round(pb_mid, 2),
  pb_high         = round(pb_hi, 2),
  pb_basis        = "bvps",
  pb_use_industry = TRUE,
  pb_holdco_discount = 0                 # 控股折價套用在已辨識投資科目（0–50%）
)
