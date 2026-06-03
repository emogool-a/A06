# Assignment 6 - Technical Analysis Dashboard
# Student: Imad Ramadan
# Course: BDA400

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel("Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", value = "AAPL"),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      checkboxGroupInput(
        "indicators",
        "Technical Indicators:",
        choices = c("SMA", "RSI", "MACD"),
        selected = c("SMA")
      )
    ),
    
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    
    stock_data <- getSymbols(
      input$symbol,
      src = "yahoo",
      from = input$date_range[1],
      to = input$date_range[2],
      auto.assign = FALSE
    )
    
    close_prices <- Cl(stock_data)
    
    sma20 <- SMA(close_prices, n = 20)
    sma50 <- SMA(close_prices, n = 50)
    rsi14 <- RSI(close_prices, n = 14)
    macd_values <- MACD(close_prices, nFast = 12, nSlow = 26, nSig = 9)
    
    stock_df <- data.frame(
      Date = index(stock_data),
      Close = as.numeric(close_prices),
      SMA20 = as.numeric(sma20),
      SMA50 = as.numeric(sma50),
      RSI = as.numeric(rsi14),
      MACD = as.numeric(macd_values[, 1]),
      SignalLine = as.numeric(macd_values[, 2])
    )
    
    stock_df$Signal <- "Hold"
    stock_df$Signal[stock_df$SMA20 > stock_df$SMA50] <- "Buy"
    stock_df$Signal[stock_df$SMA20 < stock_df$SMA50] <- "Sell"
    
    p <- ggplot(stock_df, aes(x = Date)) +
      geom_line(aes(y = Close), color = "black")
    
    if ("SMA" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = SMA20), color = "blue", linewidth = 1) +
        geom_line(aes(y = SMA50), color = "red", linewidth = 1)
    }
    
    p <- p +
      geom_point(
        data = subset(stock_df, Signal == "Buy"),
        aes(y = Close),
        color = "green",
        size = 2
      ) +
      geom_point(
        data = subset(stock_df, Signal == "Sell"),
        aes(y = Close),
        color = "red",
        size = 2
      )
    
    if ("RSI" %in% input$indicators) {
      p <- p +
        annotate(
          "text",
          x = min(stock_df$Date, na.rm = TRUE),
          y = max(stock_df$Close, na.rm = TRUE),
          label = "RSI enabled",
          hjust = 0,
          color = "purple"
        )
    }
    
    if ("MACD" %in% input$indicators) {
      p <- p +
        annotate(
          "text",
          x = min(stock_df$Date, na.rm = TRUE),
          y = max(stock_df$Close, na.rm = TRUE) * 0.97,
          label = "MACD enabled",
          hjust = 0,
          color = "orange"
        )
    }
    
    p +
      labs(
        title = paste(input$symbol, "- Technical Analysis with Signals"),
        x = "Date",
        y = "Price"
      )
  })
}

shinyApp(ui, server)