# 📦 fcf_projection_module.R 
fcf_projection_module_server <- function(id, 
                                         d_cash_flow, 
                                         input_years,      
                                         calc_trigger,     
                                         input_mode,       
                                         g_gordon,         
                                         g_stage1,         
                                         g_stage2,         
                                         yr_stage1,        
                                         discount_rate_g,   # Gordon 模式 WACC
                                         discount_rate_s1,  # Two-Stage 模式 WACC 1
                                         discount_rate_s2,  # Two-Stage 模式 WACC 2
                                         share_outstanding 
) {
  moduleServer(id, function(input, output, session) {
    
    valuation_results <- eventReactive(calc_trigger(), {
      req(d_cash_flow())
      fcf_vec <- select_clean_metric_row(d_cash_flow(), "Free Cash Flow")
      req(length(fcf_vec) > 0)
      fcf_base <- fcf_vec[1] # 取最新一期 FCF
      
      n <- input_years()
      
      if (input_mode() == "gordon") {
        # --- Gordon Growth 邏輯 ---
        g <- g_gordon() / 100
        r <- discount_rate_g() / 100
        fcf_projections <- fcf_base * (1 + g)^(1:n)
        terminal_value <- (fcf_projections[n] * (1 + g)) / (r - g)
        ev <- sum(fcf_projections / (1 + r)^(1:n)) + (terminal_value / (1 + r)^n)
        mode_label <- "Gordon Growth"
        
      } else {
        # --- Two-Stage Growth 邏輯 ---
        g1 <- g_stage1() / 100
        g2 <- g_stage2() / 100
        r1 <- discount_rate_s1() / 100
        r2 <- discount_rate_s2() / 100
        y1 <- yr_stage1()
        
        fcf_projections <- numeric(n)
        for(i in 1:n) {
          if(i <= y1) fcf_projections[i] <- fcf_base * (1 + g1)^i
          else fcf_projections[i] <- fcf_projections[y1] * (1 + g2)^(i - y1)
        }
        terminal_value <- (fcf_projections[n] * (1 + g2)) / (r2 - g2)
        # 第一階段折現使用 r1，終值折現綜合考慮 (簡化處理)
        ev <- sum(fcf_projections[1:y1] / (1 + r1)^(1:y1)) + 
          sum(fcf_projections[(y1+1):n] / (1 + r2)^( (y1+1):n )) + 
          (terminal_value / (1 + r2)^n)
        mode_label <- "Two-Stage"
      }
      
      shares <- share_outstanding()
      list(ev = ev, price = ev/shares, fcf_vector = fcf_projections, mode = mode_label)
    })
    
    return(valuation_results)
  })
}
