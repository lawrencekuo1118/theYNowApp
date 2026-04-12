library(shiny)

ui <- fluidPage(
  titlePanel("WACC 與 CAPM 估算 Re 同步範例"),
  
  sidebarLayout(
    sidebarPanel(
      h4("CAPM 參數"),
      numericInput("rf", "無風險利率 r_f (%)", value = 2),
      numericInput("beta", "Beta 值", value = 1.2),
      numericInput("rm", "市場報酬率 r_m (%)", value = 8),
      
      checkboxInput("use_capm", "✅ 使用估算的 rₑ 替代手動輸入", value = TRUE),
      
      h4("WACC 參數"),
      numericInput("re_manual", "股權成本 rₑ (%)", value = 10),
      numericInput("rd", "負債成本 r_d (%)", value = 5),
      numericInput("tc", "稅率 (%)", value = 20),
      numericInput("E", "股權資本 E", value = 600),
      numericInput("D", "負債資本 D", value = 400)
    ),
    
    mainPanel(
      h4("計算結果"),
      verbatimTextOutput("capm_result"),
      verbatimTextOutput("wacc_result")
    )
  )
)

server <- function(input, output, session) {
  # 計算 CAPM 的 rₑ
  re_capm <- reactive({
    input$rf + input$beta * (input$rm - input$rf)
  })
  
  # 根據勾選選項選擇 rₑ 來源
  re_used <- reactive({
    if (input$use_capm) {
      re_capm()
    } else {
      input$re_manual
    }
  })
  
  # 顯示 CAPM 計算結果
  output$capm_result <- renderPrint({
    cat("CAPM 計算股權成本 rₑ =", round(re_capm(), 2), "%")
  })
  
  # 計算 WACC 並顯示
  output$wacc_result <- renderPrint({
    E <- input$E
    D <- input$D
    V <- E + D
    re <- re_used()
    rd <- input$rd
    tc <- input$tc / 100
    
    wacc <- (E/V)*re + (D/V)*rd*(1 - tc)
    cat("使用的 rₑ =", round(re, 2), "%\n")
    cat("WACC =", round(wacc, 2), "%")
  })
}

shinyApp(ui, server)
