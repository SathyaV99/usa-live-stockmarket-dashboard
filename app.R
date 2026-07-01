# Libraries
library(shiny)
library(shinydashboard)
library(quantmod)
library(TTR)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xgboost)
library(DBI)
library(RPostgres)

# Load database credentials securely from an external, git-ignored file
source("credentials.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "USA Live Stockmarket"),
  dashboardSidebar(
    br(),
    div(style = "padding: 10px;",
        selectInput("global_symbol", "Select Stock Symbol:",
                    choices = c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT"),
                    selected = "AAPL"),
        uiOutput("global_globe_link")
    )
  ),
  dashboardBody(
    # Row for Metrics (Value Boxes)
    fluidRow(
      valueBoxOutput("profit_box", width = 4),
      valueBoxOutput("accurate_box", width = 4),
      valueBoxOutput("inaccurate_box", width = 4)
    ),
    
    # Row for Graphs
    fluidRow(
      box(width = 6, status = "primary", solidHeader = TRUE, title = "Actual vs Predicted (Last 5 Days)",
          plotlyOutput("stock_plot")
      ),
      box(width = 6, status = "warning", solidHeader = TRUE, title = "60-Day Macro Trend (Candlestick)",
          plotlyOutput("candlestick_plot")
      )
    ),
    
    # Row for Prediction Data Table
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE, title = "Historical Prediction Logs",
          fluidRow(
            column(4, dateRangeInput("date_filter", "Filter Dates:", start = Sys.Date() - 30, end = Sys.Date() + 1)),
            column(8, align = "right", downloadButton("download_predictions", "Download Predictions (.csv)"))
          ),
          br(),
          dataTableOutput("stock_table")
      )
    )
  )
)

server <- function(input, output, session){
  # Define reactive values to store data for each stock
  stocks_data <- reactiveValues(
    AAPL = NULL, AMZN = NULL, NVDA = NULL, GOOGL = NULL, MSFT = NULL
  )
  
  # Reactive value for metrics
  metrics_data <- reactiveValues(
    profit = 0, accurate = 0, inaccurate = 0
  )
  
  # Reactive value for candlestick data
  candlestick_xts <- reactiveVal(NULL)

  # Database connection helper
  get_db_connection <- function() {
    tryCatch({
      DBI::dbConnect(
        RPostgres::Postgres(),
        host = SUPABASE_HOST,
        port = SUPABASE_PORT,
        dbname = SUPABASE_DBNAME,
        user = SUPABASE_USER,
        password = SUPABASE_PASSWORD
      )
    }, error = function(e) { NULL })
  }

  init_db <- function() {
    con <- get_db_connection()
    if (!is.null(con)) {
      query <- "
      CREATE TABLE IF NOT EXISTS predictions_log (
        timestamp TEXT,
        open NUMERIC,
        close NUMERIC,
        \"MA_seven\" NUMERIC,
        \"MA_fourteen\" NUMERIC,
        \"MA_twenty\" NUMERIC,
        \"SD_seven\" NUMERIC,
        \"SD_fourteen\" NUMERIC,
        \"SD_twenty\" NUMERIC,
        predicted_close NUMERIC,
        \"Symbol\" TEXT
      );
      "
      tryCatch({ DBI::dbExecute(con, query) }, error = function(e) { warning("Failed to initialize table: ", e$message) })
      DBI::dbDisconnect(con)
    }
  }
  init_db()
  
  # Create Function to update symbol data
  retrieveData <- function(symbol) {
    if(is.null(symbol) || symbol == "") return()
    
    # Retrieve data from Yahoo API for 90 days
    getSymbols(symbol, src = "yahoo", from = Sys.Date()-90, to = Sys.Date())
    
    data_xts <- get(symbol)
    # Store for candlestick
    candlestick_xts(tail(data_xts, 60))
    
    open_price <- Op(data_xts)
    close_price <- Cl(data_xts)
    timestamp <- index(data_xts)
    
    # Feature engineering
    MA_seven <- SMA(open_price, n = 7)
    MA_fourteen <- SMA(open_price, n = 14)
    MA_twenty <- SMA(open_price, n = 20)
    SD_seven <- runSD(open_price, n = 7)
    SD_fourteen <- runSD(open_price, n = 14)
    SD_twenty <- runSD(open_price, n = 20)
    
    # Create data frame with calculated features
    latest_opening <- data.frame(
      timestamp = as.character(timestamp),
      open = as.numeric(open_price),
      close = as.numeric(close_price),
      MA_seven = as.numeric(MA_seven),
      MA_fourteen = as.numeric(MA_fourteen),
      MA_twenty = as.numeric(MA_twenty),
      SD_seven = as.numeric(SD_seven),
      SD_fourteen = as.numeric(SD_fourteen),
      SD_twenty = as.numeric(SD_twenty)
    )
    
    # Drop NA values (MAs introduce NAs at the start)
    latest_opening <- na.omit(latest_opening)
    
    # Select the latest 30 dates for evaluation and prediction
    latest_opening <- tail(latest_opening, 30)
    colnames(latest_opening) <- c("timestamp", "open", "close", "MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")
    
    # Load the saved model weights for XGBoost
    xgb_model_file <- paste0(symbol, "_XGB.rds")
    latest_opening$predicted_close <- NA
    
    if (file.exists(xgb_model_file)) {
      model <- readRDS(xgb_model_file)
      
      # We predict the entire 30-day window to evaluate performance
      X_pred <- as.matrix(latest_opening[, c("MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")])
      preds <- predict(model, newdata = X_pred)
      
      # Shift predictions: preds[i] is the prediction for day i+1
      latest_opening$predicted_close[2:nrow(latest_opening)] <- preds[1:(length(preds)-1)]
      
      # The very last prediction (preds[30]) is for TOMORROW
      last_date <- as.Date(latest_opening$timestamp[nrow(latest_opening)])
      next_date <- last_date + 1
      if(wday(next_date) == 7) next_date <- next_date + 2 # skip saturday
      if(wday(next_date) == 1) next_date <- next_date + 1 # skip sunday
      
      future_row <- data.frame(
        timestamp = as.character(next_date),
        open = NA, close = NA,
        MA_seven = NA, MA_fourteen = NA, MA_twenty = NA,
        SD_seven = NA, SD_fourteen = NA, SD_twenty = NA,
        predicted_close = preds[length(preds)]
      )
      latest_opening <- rbind(latest_opening, future_row)
      
      # === CALCULATE METRICS (Over the historical 30 days) ===
      cum_profit <- 0
      accurate_count <- 0
      inaccurate_count <- 0
      
      for(i in 2:(nrow(latest_opening)-1)) {
        today_close <- latest_opening$close[i]
        yesterday_close <- latest_opening$close[i-1]
        predicted_today <- latest_opening$predicted_close[i]
        
        predicted_direction <- predicted_today > yesterday_close
        actual_direction <- today_close > yesterday_close
        
        if(predicted_direction == actual_direction) {
          accurate_count <- accurate_count + 1
        } else {
          inaccurate_count <- inaccurate_count + 1
        }
        
        # Simulated Strategy: Long/Short
        # If we predict UP, we go long. Profit = daily_return
        # If we predict DOWN, we go short. Profit = -daily_return
        daily_return_pct <- ((today_close - yesterday_close) / yesterday_close) * 100
        
        if(predicted_direction == TRUE) {
          cum_profit <- cum_profit + daily_return_pct
        } else {
          cum_profit <- cum_profit - daily_return_pct
        }
      }
      
      metrics_data$profit <- round(cum_profit, 2)
      metrics_data$accurate <- accurate_count
      metrics_data$inaccurate <- inaccurate_count
    }
    
    # Append the newly calculated FUTURE row to the cloud (Avoid duplicates)
    con <- get_db_connection()
    if (!is.null(con)) {
      latest_opening$Symbol <- symbol
      future_row_to_insert <- tail(latest_opening, 1)
      
      check_query <- sprintf(
        "SELECT COUNT(*) FROM predictions_log WHERE \"Symbol\" = '%s' AND timestamp = '%s'", 
        symbol, future_row_to_insert$timestamp
      )
      
      tryCatch({ 
        count_res <- DBI::dbGetQuery(con, check_query)
        if (count_res[[1]] == 0) {
          DBI::dbAppendTable(con, "predictions_log", future_row_to_insert) 
        }
      }, error = function(e) {})
      DBI::dbDisconnect(con)
    }
    
    # Convert timestamp back to Date for plotting
    latest_opening$timestamp <- as.Date(latest_opening$timestamp)
    stocks_data[[symbol]] <- latest_opening
  }
  
  # Map the global selectInput to fetch data and refresh every 5 minutes (300,000 ms)
  observe({ 
    invalidateLater(300000, session)
    retrieveData(input$global_symbol) 
  })
  
  output$global_globe_link <- renderUI({
    tags$a(href = paste0("https://finance.yahoo.com/quote/", input$global_symbol),
           icon("globe"), target="_blank", style = "color: white;")
  })
  
  # === METRICS UI ===
  output$profit_box <- renderValueBox({
    valueBox(
      paste0(metrics_data$profit, "%"),
      "30-Day Cumulative Profit (Strategy)",
      icon = icon("piggy-bank"),
      color = if(metrics_data$profit >= 0) "green" else "red"
    )
  })
  
  output$accurate_box <- renderValueBox({
    valueBox(
      metrics_data$accurate,
      "Accurate Predictions",
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$inaccurate_box <- renderValueBox({
    valueBox(
      metrics_data$inaccurate,
      "Inaccurate Predictions",
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  # === PLOTS ===
  output$stock_plot <- renderPlotly({
    stock_data <- stocks_data[[input$global_symbol]]
    if(is.null(stock_data)) return()
    
    # Show last 5 days
    prediction_week <- tail(stock_data, 6)
    
    # Calculate UP/DOWN text for predictions
    prediction_week$pred_text <- ""
    for(i in 2:nrow(prediction_week)) {
      if(prediction_week$predicted_close[i] > prediction_week$close[i-1]) {
        prediction_week$pred_text[i] <- "UP"
      } else {
        prediction_week$pred_text[i] <- "DOWN"
      }
    }
    
    plot_data <- data.frame(
      x = prediction_week$timestamp,
      y_actual = prediction_week$close,
      y_predicted = prediction_week$predicted_close,
      pred_text = prediction_week$pred_text
    )
    plot_ly(data = plot_data, x = ~x) %>%
      add_lines(y = ~y_actual, name = 'Actual', line = list(color = "blue", width = 3)) %>%
      add_trace(y = ~y_predicted, name = 'Predicted', type = 'scatter', mode = 'lines+text+markers',
                text = ~pred_text, textposition = 'top center', textfont = list(color = 'red', size = 12, weight = "bold"),
                line = list(color = "red", width = 3, dash = 'dash')) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Close Price"),
        legend = list(x = 0.1, y = 0.9)
      )
  })
  
  output$candlestick_plot <- renderPlotly({
    df_xts <- candlestick_xts()
    if(is.null(df_xts)) return()
    
    df <- data.frame(Date = index(df_xts), coredata(df_xts))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    
    plot_ly(data = df, x = ~Date, type="candlestick",
            open = ~Open, close = ~Close,
            high = ~High, low = ~Low) %>%
      layout(xaxis = list(rangeslider = list(visible = F)),
             yaxis = list(title = "Price"))
  })
  
  # === TABLES ===
  # Displays data from Supabase backend directly on the dashboard
  output$stock_table <- renderDataTable({
    con <- get_db_connection()
    if (!is.null(con)) {
      query <- sprintf("SELECT timestamp, open, close, predicted_close FROM predictions_log WHERE \"Symbol\" = '%s' ORDER BY timestamp ASC", input$global_symbol)
      tryCatch({
        stock_data <- DBI::dbGetQuery(con, query)
        DBI::dbDisconnect(con)
      }, error = function(e) { 
        DBI::dbDisconnect(con) 
        stock_data <- NULL
      })
    } else {
      stock_data <- NULL
    }
    
    # Fallback if Supabase is disconnected/not setup yet
    if(is.null(stock_data)) {
      stock_data <- stocks_data[[input$global_symbol]]
      if(is.null(stock_data)) return()
      stock_data <- stock_data[, c('timestamp', 'open', 'close', 'predicted_close')]
    }
    
    # Sort chronologically to calculate lags correctly
    stock_data <- stock_data[order(as.Date(stock_data$timestamp)), ]
    
    prev_close <- dplyr::lag(stock_data$close, 1)
    
    # Calculations
    actual_return <- (stock_data$close - prev_close) / prev_close * 100
    error_val <- abs(stock_data$predicted_close - stock_data$close)
    error_pct <- (error_val / stock_data$close) * 100
    
    pred_signal <- ifelse(stock_data$predicted_close > prev_close, "Up", "Down")
    actual_signal <- ifelse(stock_data$close > prev_close, "Up", "Down")
    correct <- ifelse(pred_signal == actual_signal, "✅", "❌")
    
    # Strategy Return % (Long/Short Strategy)
    strategy_return <- ifelse(pred_signal == "Up", actual_return, -actual_return)
    strategy_return[is.na(strategy_return)] <- 0
    cum_profit <- cumsum(strategy_return)
    
    # Formatting the dataframe
    formatted_data <- data.frame(
      Date = stock_data$timestamp,
      Open = round(stock_data$open, 2),
      `Actual Close` = round(stock_data$close, 2),
      `Pred Close` = round(stock_data$predicted_close, 2),
      `Daily Profit %` = sprintf("%.2f%%", strategy_return),
      Error = sprintf("%.2f", error_val),
      `Error %` = sprintf("%.2f%%", error_pct),
      `Pred Signal` = pred_signal,
      `Actual Signal` = actual_signal,
      Correct = correct,
      `Cum. Profit %` = sprintf("%.2f%%", cum_profit),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Handle NA for future prediction row (Tomorrow)
    is_future <- is.na(stock_data$close)
    formatted_data$`Daily Profit %`[is_future] <- ""
    formatted_data$Error[is_future] <- ""
    formatted_data$`Error %`[is_future] <- ""
    formatted_data$`Actual Signal`[is_future] <- ""
    formatted_data$Correct[is_future] <- "⏳"
    formatted_data$`Cum. Profit %`[is_future] <- ""
    
    # Handle NA for the very first row due to lag
    formatted_data$`Pred Signal`[is.na(prev_close)] <- ""
    formatted_data$`Actual Signal`[is.na(prev_close)] <- ""
    formatted_data$Correct[is.na(prev_close)] <- ""
    formatted_data$`Daily Profit %`[is.na(prev_close)] <- ""
    
    # Filter by Date Range
    filter_dates <- as.Date(formatted_data$Date)
    mask <- filter_dates >= input$date_filter[1] & filter_dates <= input$date_filter[2]
    formatted_data <- formatted_data[mask, ]
    
    # Sort descending to show newest first
    formatted_data <- formatted_data[order(as.Date(formatted_data$Date), decreasing = TRUE), ]
    
    formatted_data
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Download Handlers
  output$download_predictions <- downloadHandler(
    filename = function() { paste0(input$global_symbol, "_predictions.csv") },
    content = function(file) { write.csv(stocks_data[[input$global_symbol]], file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)