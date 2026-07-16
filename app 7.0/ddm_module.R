ddm_module_server <- function(id, dcf_g = reactive(NULL), dcf_ke = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # ==========================================
    # 🔄 1. 正向同步：接收 DCF 傳來的變數
    # ==========================================
    observeEvent(dcf_g(), {
      req(dcf_g())
      # 避免浮點數誤差造成的無限迴圈
      if (is.null(input$g) || abs(dcf_g() - input$g) > 1e-4) {
        updateNumericInput(session, "g", value = dcf_g())
      }
    })
    
    observeEvent(dcf_ke(), {
      req(dcf_ke())
      if (is.null(input$ke) || abs(dcf_ke() - input$ke) > 1e-4) {
        updateNumericInput(session, "ke", value = dcf_ke())
      }
    })
    
    # ==========================================
    # 🧮 2. DDM 核心計算邏輯
    # ==========================================
    ddm_calc <- eventReactive(input$btn_calc_ddm, {
      req(input$d0, input$g, input$ke)
      
      d0 <- input$d0
      g_dec <- input$g / 100
      ke_dec <- input$ke / 100
      
      # print(input$d0) # for debug use
      
      if (ke_dec <= g_dec) {
        return(list(
          status = "error", 
          message = "⚠️ 計算無效：要求報酬率 (Ke) 必須嚴格大於永續成長率 (g)！"
        ))
      }
      
      d1 <- d0 * (1 + g_dec)
      p0 <- d1 / (ke_dec - g_dec)
      
      return(list(status = "success", value = round(p0, 2), d1 = round(d1, 2)))
      
    }, ignoreNULL = FALSE) 
    
    # ==========================================
    # 📊 3. 渲染結果到 UI
    # ==========================================
    output$ui_ddm_result <- renderUI({
      res <- ddm_calc()
      if (res$status == "error") {
        div(style = "color: #d9534f; font-weight: bold; padding: 10px; background-color: #fdf2f2; border-left: 4px solid #d9534f;", res$message)
      } else {
        div(style = "font-size: 32px; color: #27ae60; font-weight: bold;", 
            paste0("$ ", formatC(res$value, format = "f", big.mark = ",", digits = 2)))
      }
    })
    
    # ==========================================
    # 📤 4. 匯出 DDM 的參數 (🟢 加入 debounce 延遲防呆機制)
    # ==========================================
    # 使用 debounce(500) 代表：使用者打完字停下半秒鐘後，才把變數送出
    return(list(
      ddm_g  = reactive(input$g) %>% debounce(500),
      ddm_ke = reactive(input$ke) %>% debounce(500),
      calc_result = ddm_calc
    ))
  })
}
