# Assignment 6 - Technical Analysis Dashboard
# Student: Imad Ramadan
# Course: BDA400

library(shiny)
library(ggplot2)
library(quantmod)

ui <- fluidPage(
  titlePanel("Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01",
                     end = "2023-07-01")
    ),
    
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)

server <- function(input, output) {
  output$stock_chart <- renderPlot({
    stock_data <- getSymbols(input$symbol,
                             src = "yahoo",
                             from = input$date_range[1],
                             to = input$date_range[2],
                             auto.assign = FALSE)
    
    stock_df <- data.frame(
      Date = index(stock_data),
      Close = as.numeric(Cl(stock_data))
    )
    
    ggplot(stock_df, aes(x = Date, y = Close)) +
      geom_line() +
      labs(title = paste(input$symbol, "Stock Price"),
           x = "Date",
           y = "Closing Price")
  })
}

shinyApp(ui, server)
