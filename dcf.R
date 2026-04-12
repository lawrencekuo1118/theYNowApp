# app.R
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("DCF估值計算器"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("wacc", "折現率 WACC (%)", value = 10),
      numericInput("growth", "永續成長率 g (%)", value = 3),
      numericInput("years", "預測年數 n", value = 5, min = 1, max = 20),
      textInput("fcf", "預測FCF（用逗號分隔）", value = "100,110,120,130,140"),
      actionButton("calc", "計算DCF")
    ),
    
    mainPanel(
      h4("計算結果："),
      verbatimTextOutput("dcf_result"),
      plotOutput("fcf_plot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$calc, {
    req(input$fcf)
    
    fcf_values <- as.numeric(unlist(strsplit(input$fcf, ",")))
    n <- input$years
    r <- input$wacc / 100
    g <- input$growth / 100
    
    if (length(fcf_values) < n) {
      output$dcf_result <- renderText("請提供足夠的FCF預測值。")
      return()
    }
    
    fcf_values <- fcf_values[1:n]
    pv_fcf <- sum(fcf_values / (1 + r)^(1:n))
    fcf_terminal <- fcf_values[n] * (1 + g)
    terminal_value <- fcf_terminal / (r - g)
    pv_terminal <- terminal_value / (1 + r)^n
    
    dcf_value <- pv_fcf + pv_terminal
    
    output$dcf_result <- renderText({
      paste0("企業價值估計：$", round(dcf_value, 2),
             "\n\n預測期現金流現值：$", round(pv_fcf, 2),
             "\n終值現值：$", round(pv_terminal, 2))
    })
    
    plot_df <- data.frame(Year = 1:n, Value = fcf_values)
    
    output$fcf_plot <- renderPlot({
      ggplot(plot_df, aes(x = Year, y = Value)) +
        geom_line(color = "black", size = 1.2) +
        geom_point(aes(color = Value < 0), size = 3) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
        theme_bw() +
        labs(title = "預測自由現金流", x = "年份", y = "FCF") +
        theme(plot.title = element_text(size = 14, face = "bold"))
    })
  })
}

shinyApp(ui, server)