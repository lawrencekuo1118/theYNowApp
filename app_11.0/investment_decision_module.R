# =========================================================================
# 投資決策權威模組 (Investment Decision Scorecard) - 決策漏斗完整版
# 維度：財務質量 (Quality) -> 估值空間 (Value) -> 回歸動能 (Timing)
# =========================================================================

library(shiny)
library(shinydashboard)
library(TTR)
library(glue)

# -------------------------------------------
# 1. UI 模組：視覺化決策看板
# -------------------------------------------
decision_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 第一層：三大核心指標
    fluidRow(
      valueBoxOutput(ns("vbox_fscore"), width = 4),   # Quality First
      valueBoxOutput(ns("vbox_mos"), width = 4),      # Value Second
      valueBoxOutput(ns("vbox_momentum"), width = 4)  # Timing Third
    ),
    
    # 第二層：綜合決策（Model Selector 已移出 Sensitivity；推薦文案改在矩陣旁顯示）
    fluidRow(
      box(
        title = "智能決策矩陣 The Decision Funnel", width = 12, status = "primary", solidHeader = TRUE,
        column(width = 7, 
               h4("決策建議："),
               uiOutput(ns("ui_recommendation"))
        ),
        column(width = 5,
               h4("F-Score 體質檢核清單"),
               tableOutput(ns("table_checklist"))
        )
      )
    ),
    
    # 第三層：綜合估值水位圖（含 DCF / DDM / P/B）
    uiOutput(ns("ui_valuation_compare"))
  )
}

# -------------------------------------------
# 2. 獨立邏輯運算區塊 (Pure Functions)
# -------------------------------------------

# 🧠 智能估值決策樹模型 (The Great Filter)
get_valuation_recommendation <- function(net_margin, rev_growth, eps_growth, is_saas = FALSE, is_financial = FALSE) {
  if (isTRUE(is_financial)) {
    return(list(model = "P/B（本淨比／資產法）", reason = "金融／保險／控股企業：帳面淨值與合理本淨比通常優於 DCF／DDM。請至 P/B-Asset 分頁試算。"))
  }
  if(is.na(net_margin) || is.na(rev_growth)) {
    return(list(model = "資料不足", reason = "無法取得完整損益表數據，請確認財報年份。"))
  }
  
  if(net_margin <= 0.02) { 
    if(rev_growth >= 0.20) {
      if(is_saas) {
        return(list(model = "EV/Forward Sales", reason = "SaaS 產業且高成長，請套用 Rule of 40 檢視其擴張效率。"))
      } else {
        return(list(model = "P/S (市銷率)", reason = "公司尚未穩定獲利但具備高成長動能 (>20%)，建議以營收作為估值基準。"))
      }
    } else {
      return(list(model = "P/B (市淨率)", reason = "公司獲利微薄且無顯著成長，建議以淨資產／本淨比為主，並提防價值陷阱。"))
    }
  } else {
    if(!is.na(eps_growth) && eps_growth >= 0.15) {
      return(list(model = "PEG (市盈增長比)", reason = sprintf("具備實質獲利且進入高速成長期 (EPS 增長 %.1f%%)，單用 P/E 會產生高估錯覺，建議引入 PEG。", eps_growth * 100)))
    } else {
      return(list(model = "DCF 或 DDM", reason = "獲利穩定、成長放緩 (<15%)，屬於成熟型企業，適合絕對估值模型；金融股請改用 P/B。"))
    }
  }
}

# 🛡️ 安全計算 CAGR
safe_cagr <- function(row_data) {
  vals <- na.omit(parse_financial_number(row_data))
  if(length(vals) < 2) return(NA)
  newest <- vals[1]
  oldest <- vals[length(vals)]
  if(oldest <= 0 || newest <= 0) return(NA) 
  return((newest / oldest)^(1 / (length(vals) - 1)) - 1)
}

# -------------------------------------------
# 3. Server 模組：核心邏輯封裝
# -------------------------------------------
decision_server <- function(id, d_is, d_bs, d_cf, intrinsic_val_dcf, intrinsic_val_ddm, current_price, hist_price_data, industry_text,
                            intrinsic_val_pb = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    
    # 內部防呆取值函數
    get_row_safe <- function(df, label) {
      res <- select_clean_metric_row(df, label, include_ttm = FALSE)
      if (length(res) < 2) return(c(NA, NA))
      return(as.numeric(res[1:2]))
    }
    
    # --- 1. 財務質量 (Piotroski F-Score) 計算 ---
    f_score_eval <- reactive({
      req(d_is(), d_bs(), d_cf())
      
      net_inc  <- get_row_safe(d_is(), "Net Income Common Stockholders|Net Income$")
      revenue  <- get_row_safe(d_is(), "Total Revenue")
      gp       <- get_row_safe(d_is(), "Gross Profit")
      assets   <- get_row_safe(d_bs(), "Total Assets")
      lt_debt  <- get_row_safe(d_bs(), "Long Term Debt|Total Non Current Liabilities")
      cur_ast  <- get_row_safe(d_bs(), "Total Current Assets")
      cur_liab <- get_row_safe(d_bs(), "Total Current Liabilities")
      shares   <- get_row_safe(d_bs(), "Ordinary Shares Number")
      ocf      <- get_row_safe(d_cf(), "Operating Cash Flow")
      
      tryCatch({
        p1 <- ifelse(!is.na(net_inc[1]) && !is.na(assets[1]) && (net_inc[1]/assets[1]) > 0, 1, 0) # ROA > 0
        p2 <- ifelse(!is.na(ocf[1]) && ocf[1] > 0, 1, 0) # OCF > 0
        p3 <- ifelse(all(!is.na(net_inc[1:2]), !is.na(assets[1:2])) && (net_inc[1]/assets[1]) > (net_inc[2]/assets[2]), 1, 0) # ROA 提升
        p4 <- ifelse(!is.na(ocf[1]) && !is.na(net_inc[1]) && ocf[1] > net_inc[1], 1, 0) # 盈餘品質 (OCF > Net Income)
        
        p5 <- ifelse(all(!is.na(lt_debt[1:2]), !is.na(assets[1:2])) && (lt_debt[1]/assets[1]) <= (lt_debt[2]/assets[2]), 1, 0) # 槓桿下降
        p6 <- ifelse(all(!is.na(cur_ast[1:2]), !is.na(cur_liab[1:2])) && (cur_ast[1]/cur_liab[1]) > (cur_ast[2]/cur_liab[2]), 1, 0) # 流動比提升
        p7 <- ifelse(!is.na(shares[1]) && !is.na(shares[2]) && shares[1] <= (shares[2]*1.02), 1, 0) # 未大幅增資
        
        p8 <- ifelse(all(!is.na(gp[1:2]), !is.na(revenue[1:2])) && (gp[1]/revenue[1]) > (gp[2]/revenue[2]), 1, 0) # 毛利率提升
        p9 <- ifelse(all(!is.na(revenue[1:2]), !is.na(assets[1:2])) && (revenue[1]/assets[1]) > (revenue[2]/assets[2]), 1, 0) # 週轉率提升
        
        total <- sum(p1, p2, p3, p4, p5, p6, p7, p8, p9)
        list(
          total = total,
          quality_flag = p4, # 標記盈餘品質
          checklist = data.frame(
            `檢驗維度` = c("獲利性 (ROA > 0)", "獲利性 (OCF > 0)", "獲利性 (ROA 成長)", "獲利性 (盈餘品質)", 
                       "安全性 (槓桿下降)", "安全性 (流動比提升)", "安全性 (未大幅增資)", 
                       "效率 (毛利率提升)", "效率 (資產週轉率提升)"),
            `得分` = c(p1, p2, p3, p4, p5, p6, p7, p8, p9),
            check.names = FALSE
          )
        )
      }, error = function(e) list(total = 0, quality_flag = 0, checklist = data.frame()))
    })
    
    # --- 2. 安全邊際 (MOS) 計算 ---
    # 金融／保險優先 P/B；否則 DCF → DDM → P/B
    mos_calc <- reactive({
      is_financial <- grepl("Bank|Insurance|Financial|Conglomerate|fn\\.", industry_text(), ignore.case = TRUE)
      pb_v  <- tryCatch(intrinsic_val_pb(), error = function(e) NA)
      dcf_v <- tryCatch(intrinsic_val_dcf(), error = function(e) NA)
      ddm_v <- tryCatch(intrinsic_val_ddm(), error = function(e) NA)
      
      pick <- function(x) {
        if (length(x) == 1 && !is.null(x) && !is.na(x) && is.finite(x) && x != 0) x else NA
      }
      pb_v <- pick(pb_v); dcf_v <- pick(dcf_v); ddm_v <- pick(ddm_v)
      
      val <- if (is_financial) {
        if (!is.na(pb_v)) pb_v else if (!is.na(dcf_v)) dcf_v else ddm_v
      } else {
        if (!is.na(dcf_v)) dcf_v else if (!is.na(ddm_v)) ddm_v else pb_v
      }
      
      curr_p <- current_price()
      if (is.na(val) || is.na(curr_p) || val == 0) return(NA)
      (val - curr_p) / val
    })
    
    # --- 3. 趨勢動能 (Momentum) 計算 ---
    mom_status <- reactive({
      req(hist_price_data())
      prices <- hist_price_data()$Close
      vol <- hist_price_data()$Volume
      
      ma20 <- tail(SMA(prices, 20), 1)
      ma60 <- tail(SMA(prices, 60), 1)
      curr_p <- tail(prices, 1)
      
      cond1 <- curr_p > ma20 && curr_p > ma60 # 站上均線
      cond2 <- ma20 > ma60                    # 多頭排列
      
      list(
        triggered = (isTRUE(cond1) && isTRUE(cond2)), 
        dist_to_ma20 = if(is.na(ma20) || is.na(curr_p)) NA else (curr_p - ma20)/ma20
      )
    })
    
    # --- 4. 漏斗決策引擎 (Recommendation Logic) ---
    final_recommendation <- reactive({
      f_score <- f_score_eval()$total
      f_quality <- f_score_eval()$quality_flag
      mos <- mos_calc()
      mom <- mom_status()
      
      if (f_score < 4 || f_quality == 0) {
        return(list(class="alert-danger", icon="skull-crossbones", title="價值陷阱警訊", 
                    text="財務質量偏弱或經營現金流無法支撐淨利。就算估值再低，也不建議貿然摸底。"))
      }
      
      if (!is.na(mos) && mos < 0) {
        if (mom$triggered) {
          return(list(class="alert-warning", icon="fire", title="動能強勁但估值偏高", 
                      text="右側趨勢良好，但價格已超過內在價值。若持有可續抱，空手者不建議此時追高。"))
        } else {
          return(list(class="alert-warning", icon="hourglass-half", title="估值偏高且動能轉弱", 
                      text="好公司但目前價格太貴，且趨勢尚未轉強，建議耐心等待拉回再行佈局。"))
        }
      }
      
      if (!is.na(mos) && mos >= 0.2) {
        if (mom$triggered) {
          return(list(class="alert-success", icon="rocket", title="強烈建議：戴維斯雙擊點", 
                      text="低估、高質量、且技術面動能已開啟！勝率極高的絕佳擊球點。"))
        } else {
          return(list(class="alert-info", icon="anchor", title="左側潛伏區塊", 
                      text="具備極高投資價值，但市場資金尚未關注。可分批建倉，等待趨勢反轉。"))
        }
      }
      
      return(list(class="alert-secondary", icon="balance-scale", title="觀望中立", text="估值處於合理區間，體質穩健，可依據個人資產配置決定是否介入。"))
    })
    
    # --- Output 渲染 ---
    
    output$vbox_fscore <- renderValueBox({
      score <- f_score_eval()$total
      color <- if(score >= 7) "green" else if(score >= 4) "yellow" else "red"
      valueBox(paste0(score, " / 9"), "體質過濾 (F-Score)", icon = icon("gem"), color = color)
    })
    
    output$vbox_mos <- renderValueBox({
      val <- mos_calc()
      if(is.na(val)) {
        valueBox("N/A", "安全邊際 (MOS)", icon = icon("shield-halved"), color = "gray")
      } else {
        v_pct <- round(val * 100, 1)
        color <- if(v_pct >= 20) "green" else if(v_pct >= 0) "yellow" else "red"
        valueBox(paste0(v_pct, "%"), "安全邊際 (MOS)", icon = icon("shield-halved"), color = color)
      }
    })
    
    output$vbox_momentum <- renderValueBox({
      status <- mom_status()
      color <- if(isTRUE(status$triggered)) "green" else "navy"
      txt <- if(isTRUE(status$triggered)) "多頭確認" else "盤整/偏空"
      valueBox(txt, "趨勢動能", icon = icon("chart-line"), color = color)
    })
    
    output$ui_recommendation <- renderUI({
      rec <- final_recommendation()
      div(class = paste("alert", rec$class), 
          h4(icon(rec$icon), " ", rec$title), 
          p(rec$text))
    })
    
    output$table_checklist <- renderTable({
      df <- f_score_eval()$checklist
      if(nrow(df) > 0) {
        df$`得分` <- ifelse(df$`得分` == 1, "✅ 通過", "❌ 未達標")
      }
      df
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    # 智能估值導航渲染
    output$ui_smart_valuation <- renderUI({
      req(d_is())
      net_inc <- select_clean_metric_row(d_is(), "Net Income Common Stockholders|Net Income$", include_ttm = FALSE)
      revenue <- select_clean_metric_row(d_is(), "Total Revenue", include_ttm = FALSE)
      eps     <- select_clean_metric_row(d_is(), "Basic EPS|Diluted EPS", include_ttm = FALSE)
      
      n_margin <- if(length(net_inc)>0 && length(revenue)>0 && !is.na(revenue[1]) && revenue[1]!=0) net_inc[1]/revenue[1] else NA
      rev_g <- safe_cagr(revenue)
      eps_g <- safe_cagr(eps)
      is_saas <- grepl("Software|Technology", industry_text(), ignore.case = TRUE)
      is_financial <- grepl("Bank|Insurance|Financial|Conglomerate|fn\\.", industry_text(), ignore.case = TRUE)
      
      rec <- get_valuation_recommendation(n_margin, rev_g, eps_g, is_saas, is_financial)
      
      HTML(glue::glue("
        <div style='background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 15px; border-radius: 5px;'>
          <h4 style='margin-top: 0; color: #007bff; font-weight: bold;'><i class='fa fa-map-signs'></i> 模型選擇器 (Model Selector)</h4>
          <p style='font-size: 16px; margin-bottom: 5px;'><strong>推薦主體：</strong> <span style='color: #d9534f; font-weight: bold;'>{rec$model}</span></p>
          <p style='font-size: 14px; color: #555; margin-bottom: 0;'><strong>背後邏輯：</strong> {rec$reason}</p>
        </div>
      "))
    })
    
    # ==========================================
    # 📊 綜合估值對比水位圖 (你提供的邏輯整合版)
    # ==========================================
    output$ui_valuation_compare <- renderUI({
      # 1. 取得各項估值指標
      p_dcf  <- tryCatch(intrinsic_val_dcf(), error = function(e) NA)
      p_ddm  <- tryCatch(intrinsic_val_ddm(), error = function(e) NA)
      p_pb   <- tryCatch(intrinsic_val_pb(), error = function(e) NA)
      p_curr <- current_price()
      
      is_valid <- function(x) { length(x) == 1 && !is.na(x) && is.numeric(x) && is.finite(x) }
      if (!is_valid(p_curr)) return(div(class="alert alert-info", "正在等待市場資料..."))
      
      val_dcf <- if (is_valid(p_dcf)) p_dcf else NA
      val_ddm <- if (is_valid(p_ddm)) p_ddm else NA
      val_pb  <- if (is_valid(p_pb))  p_pb  else NA
      
      is_financial <- grepl("Bank|Insurance|Financial|Conglomerate|fn\\.", industry_text(), ignore.case = TRUE)
      rec_nav <- recommend_valuation_models(d_cf(), industry_text())
      if (isTRUE(rec_nav$dcf) && isTRUE(rec_nav$ddm)) {
        rec_title <- "雙模型皆適用 (DCF & DDM 交叉驗證)"
        rec_desc <- rec_nav$reason
        rec_color <- "#f39c12"; rec_icon <- "balance-scale"
      } else if (isTRUE(rec_nav$ddm)) {
        rec_title <- "推薦首選：股利折現模型 (DDM)"
        rec_desc <- rec_nav$reason
        rec_color <- "#3498db"; rec_icon <- "hand-holding-usd"
      } else if (isTRUE(rec_nav$dcf)) {
        rec_title <- "推薦首選：自由現金流模型 (DCF)"
        rec_desc <- rec_nav$reason
        rec_color <- "#9b59b6"; rec_icon <- "seedling"
      } else {
        rec_title <- "推薦首選：P/B／資產估值法"
        rec_desc <- rec_nav$reason
        rec_color <- if (is_financial) "#2980b9" else "#d9534f"
        rec_icon <- "landmark"
      }
      
      plot_dcf <- if (!is.na(val_dcf)) val_dcf else p_curr
      plot_ddm <- if (!is.na(val_ddm)) val_ddm else p_curr
      plot_pb  <- if (!is.na(val_pb))  val_pb  else p_curr
      
      all_vals <- na.omit(c(p_curr, val_dcf, val_ddm, val_pb))
      min_val <- min(all_vals) * 0.8; max_val <- max(all_vals) * 1.2
      range_val <- max(max_val - min_val, 1)
      
      pos_curr <- (p_curr - min_val) / range_val * 100
      pos_dcf  <- (plot_dcf - min_val) / range_val * 100
      pos_ddm  <- (plot_ddm - min_val) / range_val * 100
      pos_pb   <- (plot_pb  - min_val) / range_val * 100
      
      anchor <- if (is_financial && !is.na(val_pb)) val_pb else if (!is.na(val_dcf)) val_dcf else if (!is.na(val_pb)) val_pb else val_ddm
      status_text <- "合理區間"; status_color <- "#f39c12"
      if (!is.na(anchor) && p_curr < anchor * 0.8) { status_text <- "低估 (顯著安全邊際)"; status_color <- "#00a65a" }
      else if (!is.na(anchor) && p_curr > anchor * 1.2) { status_text <- "高估 (溢價過高)"; status_color <- "#d9534f" }
      
      HTML(glue::glue("
        <div style='background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; border-top: 3px solid {status_color};'>
          <div style='background: {rec_color}15; border-left: 5px solid {rec_color}; padding: 12px; border-radius: 4px; margin-bottom: 25px;'>
            <h5 style='color: {rec_color}; margin-top: 0; font-weight: bold;'>
              <i class='fa fa-{rec_icon}'></i> {rec_title}
            </h5>
            <p style='margin-bottom: 0; font-size: 13px; color: #555;'>{rec_desc}</p>
          </div>
          <h4 style='margin-top: 0; font-weight: bold;'>
            <i class='fa fa-balance-scale'></i> 綜合估值狀態：<span style='color: {status_color};'>{status_text}</span>
          </h4>
          <div style='position: relative; height: 70px; margin-top: 35px;'>
            <div style='position: absolute; top: 25px; left: 0; right: 0; height: 10px; background: #ecf0f1; border-radius: 5px;'></div>
            <div style='position: absolute; top: 0; left: {pos_ddm}%; transform: translateX(-50%); opacity: {if(!is.na(val_ddm)) 1 else 0};'>
              <div style='font-size: 11px; color: #7f8c8d;'>DDM</div>
              <div style='width: 3px; height: 35px; background: #3498db; margin: 0 auto;'></div>
              <div style='font-size: 12px; color: #3498db; font-weight: bold;'>${round(val_ddm, 2)}</div>
            </div>
            <div style='position: absolute; top: 0; left: {pos_dcf}%; transform: translateX(-50%); opacity: {if(!is.na(val_dcf)) 1 else 0};'>
              <div style='font-size: 11px; color: #7f8c8d;'>DCF</div>
              <div style='width: 3px; height: 35px; background: #9b59b6; margin: 0 auto;'></div>
              <div style='font-size: 12px; color: #9b59b6; font-weight: bold;'>${round(val_dcf, 2)}</div>
            </div>
            <div style='position: absolute; top: 0; left: {pos_pb}%; transform: translateX(-50%); opacity: {if(!is.na(val_pb)) 1 else 0};'>
              <div style='font-size: 11px; color: #7f8c8d;'>P/B</div>
              <div style='width: 3px; height: 35px; background: #2980b9; margin: 0 auto;'></div>
              <div style='font-size: 12px; color: #2980b9; font-weight: bold;'>${round(val_pb, 2)}</div>
            </div>
            <div style='position: absolute; top: -10px; left: {pos_curr}%; transform: translateX(-50%); z-index: 10;'>
              <div style='font-size: 12px; color: white; background: #2c3e50; padding: 2px 6px; border-radius: 4px;'>目前市價</div>
              <div style='width: 12px; height: 12px; background: #2c3e50; border: 2px solid white; border-radius: 50%; margin: 2px auto;'></div>
              <div style='font-size: 15px; color: #2c3e50; font-weight: bold;'>${round(p_curr, 2)}</div>
            </div>
          </div>
        </div>
      "))
    })
    
  })
}

# -------------------------------------------
# 4. 主程式呼叫範例 (放在你的 Server 內)
# -------------------------------------------
# decision_server(
#   id = "main_decision",
#   d_is = reactive({ scraped_financials()$income_statement }),
#   d_bs = reactive({ scraped_financials()$balance_sheet }),
#   d_cf = reactive({ scraped_financials()$cash_flow }),
#   intrinsic_val_dcf = reactive({ stock_price_estimate_val() }),
#   intrinsic_val_ddm = reactive({ tryCatch(ddm_results$ddm_price(), error=function(e) NA) }),
#   current_price = reactive({ as.numeric(gsub("[^0-9.]", "", isolate(input$txt_current_price))) }),
#   hist_price_data = reactive({ get_historical_data() }), # 需回傳包含 Close, Volume 的 data.frame/xts
#   industry_text = reactive({ tryCatch(corp_industry_text(), error=function(e) "") })
# )
