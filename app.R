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
source("config/credentials.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "USA Live Stockmarket"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Live Monitoring", tabName = "live", icon = icon("dashboard")),
      menuItem("Historical Backtester", tabName = "backtest", icon = icon("history"))
    ),
    br(),
    div(style = "padding: 10px;",
        selectInput("global_symbol", "Select Stock Symbol:",
                    choices = c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT"),
                    selected = "AAPL"),
        uiOutput("global_globe_link")
    )
  ),
  dashboardBody(
    tabItems(
      # --- LIVE MONITORING TAB ---
      tabItem(tabName = "live",
        # Dynamic Stock Header (Name, Price, Date)
    uiOutput("stock_header"),
    
    # Row for Metrics (Value Boxes)
    fluidRow(
      valueBoxOutput("profit_box", width = 4),
      valueBoxOutput("accurate_box", width = 4),
      valueBoxOutput("inaccurate_box", width = 4)
    ),
    fluidRow(
      valueBoxOutput("win_rate_box", width = 4),
      valueBoxOutput("avg_profit_box", width = 4),
      valueBoxOutput("pending_box", width = 4)
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
            column(12, align = "right", downloadButton("download_predictions", "Download Predictions (.csv)"))
          ),
          br(),
          tags$div(style = "color: red; font-weight: bold; margin-bottom: 10px;", "➡️ More columns to the right, scroll to view"),
          dataTableOutput("stock_table")
      )
    )
      ),
      
      # --- HISTORICAL BACKTESTER TAB ---
      tabItem(tabName = "backtest",
        fluidRow(
          box(width = 12, status = "primary", title = "Custom Backtester Sandbox", solidHeader = TRUE,
            p("Test the AI on any custom historical dates. This runs entirely in a local sandbox and will NOT affect your live Neon database."),
            fluidRow(
              column(4, dateInput("bt_start_date", "Select Start Date (Tests next 15 days):", 
                                  value = Sys.Date() - 15,
                                  max = Sys.Date() - 15)),
              column(2, actionButton("reset_date", "Reset to Current", class="btn-secondary", style="margin-top: 25px;")),
              column(4, actionButton("run_bt", "Run AI Simulation", class="btn-primary", style="margin-top: 25px;"))
            )
          )
        ),
        fluidRow(
          valueBoxOutput("bt_profit_box", width = 4),
          valueBoxOutput("bt_accurate_box", width = 4),
          valueBoxOutput("bt_inaccurate_box", width = 4)
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE, title = "Actual vs Predicted",
              plotlyOutput("bt_stock_plot")
          ),
          box(width = 6, status = "warning", solidHeader = TRUE, title = "Candlestick Trend",
              plotlyOutput("bt_candlestick_plot")
          )
        ),
        fluidRow(
          box(width = 12, status = "success", title = "Backtest Results", solidHeader = TRUE,
              tags$div(style = "color: red; font-weight: bold; margin-bottom: 10px;", "➡️ More columns to the right, scroll to view"),
              dataTableOutput("bt_table")
          )
        )
      )
    )
  )
)

# Global timer to track the last background sync across all user sessions
global_last_sync <- Sys.time() - as.difftime(12, units = "hours")

server <- function(input, output, session){
  # Define reactive values to store data for each stock
  stocks_data <- reactiveValues(
    AAPL = NULL, AMZN = NULL, NVDA = NULL, GOOGL = NULL, MSFT = NULL
  )
  
  # Reactive value for metrics
  metrics_data <- reactiveValues(
    profit = 0, accurate = 0, inaccurate = 0,
    win_rate = 0, avg_profit = 0, pending = 0
  )
  
  # Reactive value for candlestick data
  candlestick_xts <- reactiveVal(NULL)
  
  # Reactive value for backtester results
  bt_results <- reactiveVal(NULL)
  bt_candlestick_xts <- reactiveVal(NULL)

  # Database connection helper
  get_db_connection <- function() {
    tryCatch({
      # Database Connection (Neon PostgreSQL)
      con <- dbConnect(RPostgres::Postgres(),
                       dbname = DB_NAME,
                       host = DB_HOST,
                       port = DB_PORT,
                       user = DB_USER,
                       password = DB_PASSWORD,
                       sslmode = "require")
    }, error = function(e) { NULL })
  }

  # Fast UI Loader (No XGBoost, No DB Upserts, just reads data)
  loadDataForUI <- function(symbol) {
    if(is.null(symbol) || symbol == "") return()
    
    # 1. Fetch basic Yahoo data just for the Candlestick chart
    tryCatch({
      getSymbols(symbol, src = "yahoo", from = Sys.Date()-60, to = Sys.Date())
      data_xts <- get(symbol)
      candlestick_xts(data_xts)
    }, error = function(e) {})
    
    # 2. Fetch the pre-calculated AI predictions directly from Neon Cloud
    con <- get_db_connection()
    if(!is.null(con)) {
      db_data <- tryCatch({ DBI::dbGetQuery(con, paste0("SELECT * FROM ", symbol, "_predictions ORDER BY timestamp DESC LIMIT 30")) }, error = function(e) { NULL })
      if (!is.null(db_data) && nrow(db_data) > 0) {
        db_data <- db_data[order(as.Date(db_data$timestamp)), ]
        db_data$timestamp <- as.Date(db_data$timestamp)
        stocks_data[[symbol]] <- db_data
        
        # Compute UI metrics from the loaded database rows
        if(nrow(db_data) > 1) {
          metrics_data$accurate <- sum(db_data$correct == "✅", na.rm = TRUE)
          metrics_data$inaccurate <- sum(db_data$correct == "❌", na.rm = TRUE)
          total_resolved <- metrics_data$accurate + metrics_data$inaccurate
          metrics_data$win_rate <- if(total_resolved > 0) round((metrics_data$accurate / total_resolved) * 100, 1) else 0
          
          strat_executed <- db_data$actual_return[!is.na(db_data$correct) & db_data$correct %in% c("✅", "❌")]
          metrics_data$profit <- if(length(strat_executed) > 0) round(sum(strat_executed, na.rm = TRUE), 2) else 0
          metrics_data$avg_profit <- if(length(strat_executed) > 0) round(mean(strat_executed, na.rm = TRUE), 2) else 0
          metrics_data$pending <- sum(db_data$correct == "⏳", na.rm = TRUE)
        }
      }
      DBI::dbDisconnect(con)
    }
  }
  
  # Create Function to run the heavy AI sync and UPSERT to Cloud
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
    
    # Load the saved model weights for XGBoost from models folder
    xgb_model_file <- paste0("models/", symbol, "_XGB.rds")
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
      
      # === CALCULATE METRICS (Vectorized for DB) ===
      prev_close <- dplyr::lag(latest_opening$close, 1)
      
      # 1. Daily Return
      latest_opening$actual_return <- (latest_opening$close - prev_close) / prev_close * 100
      
      # 2. Errors
      latest_opening$error_val <- abs(latest_opening$predicted_close - latest_opening$close)
      latest_opening$error_pct <- (latest_opening$error_val / latest_opening$close) * 100
      
      # 3. Signals (Only if previous close exists and predicted close exists)
      latest_opening$pred_signal <- ifelse(!is.na(latest_opening$predicted_close) & !is.na(prev_close),
                                           ifelse(latest_opening$predicted_close > prev_close, "Up", "Down"), 
                                           NA)
      
      latest_opening$actual_signal <- ifelse(!is.na(latest_opening$close) & !is.na(prev_close),
                                             ifelse(latest_opening$close > prev_close, "Up", "Down"), 
                                             NA)
      
      # 4. Correct/Incorrect Verdict (Fixing the absent prediction bug!)
      latest_opening$correct <- ifelse(!is.na(latest_opening$pred_signal) & !is.na(latest_opening$actual_signal),
                                       ifelse(latest_opening$pred_signal == latest_opening$actual_signal, "✅", "❌"),
                                       "⏳")
      
      # 5. Cumulative Profit Strategy
      # If predicted Up, we gain/lose the actual return. If Down, we short.
      strategy_return <- ifelse(latest_opening$pred_signal == "Up", latest_opening$actual_return, -latest_opening$actual_return)
      strategy_return[is.na(strategy_return)] <- 0
      latest_opening$cum_profit_pct <- cumsum(strategy_return)
      
      # Compute totals for value boxes
      metrics_data$profit <- round(tail(latest_opening$cum_profit_pct, 1), 2)
      metrics_data$accurate <- sum(latest_opening$correct == "✅", na.rm = TRUE)
      metrics_data$inaccurate <- sum(latest_opening$correct == "❌", na.rm = TRUE)
      
      # Additional new metrics
      total_completed <- metrics_data$accurate + metrics_data$inaccurate
      metrics_data$win_rate <- ifelse(total_completed > 0, round((metrics_data$accurate / total_completed) * 100, 2), 0)
      strat_executed <- strategy_return[latest_opening$correct %in% c("✅", "❌")]
      metrics_data$avg_profit <- if(length(strat_executed) > 0) round(mean(strat_executed, na.rm = TRUE), 2) else 0
      metrics_data$pending <- sum(latest_opening$correct == "⏳", na.rm = TRUE)
    }
    
    # Upsert the entire 30-day block (and tomorrow's future prediction) to the cloud
    con <- get_db_connection()
    if (!is.null(con)) {
      table_name <- paste0(symbol, "_predictions")
      
      # Alter table if it already exists to add the new columns (Ignore error if they exist)
      tryCatch({
        DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN actual_return NUMERIC, ADD COLUMN error_val NUMERIC, ADD COLUMN error_pct NUMERIC, ADD COLUMN pred_signal TEXT, ADD COLUMN actual_signal TEXT, ADD COLUMN correct TEXT, ADD COLUMN cum_profit_pct NUMERIC;", table_name))
      }, error = function(e) {})
      
      # Ensure the table exists with timestamp as PRIMARY KEY for UPSERT support
      query_create <- sprintf("
        CREATE TABLE IF NOT EXISTS %s (
          timestamp TEXT PRIMARY KEY,
          open NUMERIC,
          close NUMERIC,
          \"MA_seven\" NUMERIC,
          \"MA_fourteen\" NUMERIC,
          \"MA_twenty\" NUMERIC,
          \"SD_seven\" NUMERIC,
          \"SD_fourteen\" NUMERIC,
          \"SD_twenty\" NUMERIC,
          predicted_close NUMERIC,
          actual_return NUMERIC,
          error_val NUMERIC,
          error_pct NUMERIC,
          pred_signal TEXT,
          actual_signal TEXT,
          correct TEXT,
          cum_profit_pct NUMERIC
        );
      ", table_name)
      tryCatch({ DBI::dbExecute(con, query_create) }, error = function(e) {})
      
      # UPSERT Loop
      # We UPSERT all 31 recent days so the database always has a full 30-day historical log table.
      # This updates yesterday's NULL close and inserts tomorrow's prediction.
      recent_opening <- latest_opening
      for(i in 1:nrow(recent_opening)) {
        # Using COALESCE on predicted_close ensures we NEVER overwrite an older historical prediction
        # with a new prediction, preserving the true historical backtest log perfectly!
        query_upsert <- sprintf("
          INSERT INTO %s (timestamp, open, close, \"MA_seven\", \"MA_fourteen\", \"MA_twenty\", \"SD_seven\", \"SD_fourteen\", \"SD_twenty\", predicted_close, actual_return, error_val, error_pct, pred_signal, actual_signal, correct, cum_profit_pct)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
          ON CONFLICT (timestamp) DO UPDATE SET
          open = EXCLUDED.open,
          close = EXCLUDED.close,
          \"MA_seven\" = EXCLUDED.\"MA_seven\",
          \"MA_fourteen\" = EXCLUDED.\"MA_fourteen\",
          \"MA_twenty\" = EXCLUDED.\"MA_twenty\",
          \"SD_seven\" = EXCLUDED.\"SD_seven\",
          \"SD_fourteen\" = EXCLUDED.\"SD_fourteen\",
          \"SD_twenty\" = EXCLUDED.\"SD_twenty\",
          predicted_close = COALESCE(%s.predicted_close, EXCLUDED.predicted_close),
          actual_return = EXCLUDED.actual_return,
          error_val = EXCLUDED.error_val,
          error_pct = EXCLUDED.error_pct,
          pred_signal = EXCLUDED.pred_signal,
          actual_signal = EXCLUDED.actual_signal,
          correct = EXCLUDED.correct,
          cum_profit_pct = EXCLUDED.cum_profit_pct;
        ", table_name, table_name)
        
        tryCatch({
          DBI::dbExecute(con, query_upsert, params = list(
            recent_opening$timestamp[i],
            ifelse(is.na(recent_opening$open[i]), NA, recent_opening$open[i]),
            ifelse(is.na(recent_opening$close[i]), NA, recent_opening$close[i]),
            ifelse(is.na(recent_opening$MA_seven[i]), NA, recent_opening$MA_seven[i]),
            ifelse(is.na(recent_opening$MA_fourteen[i]), NA, recent_opening$MA_fourteen[i]),
            ifelse(is.na(recent_opening$MA_twenty[i]), NA, recent_opening$MA_twenty[i]),
            ifelse(is.na(recent_opening$SD_seven[i]), NA, recent_opening$SD_seven[i]),
            ifelse(is.na(recent_opening$SD_fourteen[i]), NA, recent_opening$SD_fourteen[i]),
            ifelse(is.na(recent_opening$SD_twenty[i]), NA, recent_opening$SD_twenty[i]),
            ifelse(is.na(recent_opening$predicted_close[i]), NA, recent_opening$predicted_close[i]),
            ifelse(is.na(recent_opening$actual_return[i]), NA, recent_opening$actual_return[i]),
            ifelse(is.na(recent_opening$error_val[i]), NA, recent_opening$error_val[i]),
            ifelse(is.na(recent_opening$error_pct[i]), NA, recent_opening$error_pct[i]),
            ifelse(is.na(recent_opening$pred_signal[i]), NA, recent_opening$pred_signal[i]),
            ifelse(is.na(recent_opening$actual_signal[i]), NA, recent_opening$actual_signal[i]),
            ifelse(is.na(recent_opening$correct[i]), NA, recent_opening$correct[i]),
            ifelse(is.na(recent_opening$cum_profit_pct[i]), NA, recent_opening$cum_profit_pct[i])
          ))
        }, error = function(e) {})
      }
      DBI::dbDisconnect(con)
    }
    
    # Convert timestamp back to Date for plotting
    latest_opening$timestamp <- as.Date(latest_opening$timestamp)
    stocks_data[[symbol]] <- latest_opening
  }
  
  # Background Sync: STRICTLY only runs once every 12 hours globally, never on page refresh
  observe({
    invalidateLater(43200000, session)
    
    # Check if 12 hours have passed since the LAST global sync (using 11.9 to be safe with execution times)
    time_since_sync <- as.numeric(difftime(Sys.time(), global_last_sync, units = "hours"))
    
    if (time_since_sync > 11.9) {
      global_last_sync <<- Sys.time() # Instantly lock it so other users/refreshes don't trigger it
      
      symbols_list <- c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT")
      withProgress(message = 'Syncing Live Markets to Cloud...', value = 0, {
        for(i in seq_along(symbols_list)) {
          sym <- symbols_list[i]
          incProgress(1/length(symbols_list), detail = paste("Processing", sym))
          tryCatch({ retrieveData(sym) }, error = function(e) {})
        }
      })
      
      # Reload the UI with the fresh data
      loadDataForUI(input$global_symbol)
    }
  })
  
  # Ensure the active symbol selected in the UI is ALWAYS loaded into memory quickly
  observeEvent(input$global_symbol, {
    withProgress(message = paste("Loading", input$global_symbol), value = 0.5, {
      loadDataForUI(input$global_symbol)
    })
  })
  
  output$global_globe_link <- renderUI({
    tags$a(href = paste0("https://finance.yahoo.com/quote/", input$global_symbol),
           icon("globe"), target="_blank", style = "color: white;")
  })
  
  # === STOCK HEADER UI ===
  output$stock_header <- renderUI({
    symbol <- input$global_symbol
    stock_data <- stocks_data[[symbol]]
    if(is.null(stock_data)) return()
    
    # Get the latest actual data (second to last row since last row is the future prediction)
    latest_actual_row <- stock_data[nrow(stock_data) - 1, ]
    latest_price <- round(latest_actual_row$close, 2)
    latest_date <- latest_actual_row$timestamp
    
    company_names <- c(
      "AAPL" = "Apple Inc.",
      "AMZN" = "Amazon.com Inc.",
      "NVDA" = "NVIDIA Corporation",
      "GOOGL" = "Alphabet Inc.",
      "MSFT" = "Microsoft Corporation"
    )
    company_name <- company_names[[symbol]]
    
    div(style = "padding-bottom: 20px;",
        h2(strong(paste0(company_name, " (", symbol, ")")), style = "margin-top: 0px;"),
        h4(paste0("Current Price: $", latest_price, "  |  As of: ", latest_date), style = "color: #555;")
    )
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
  
  output$win_rate_box <- renderValueBox({
    valueBox(
      paste0(metrics_data$win_rate, "%"),
      "Model Win Rate",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  output$avg_profit_box <- renderValueBox({
    valueBox(
      paste0(metrics_data$avg_profit, "%"),
      "Avg Daily Strategy Return",
      icon = icon("chart-line"),
      color = "aqua"
    )
  })
  
  output$pending_box <- renderValueBox({
    valueBox(
      metrics_data$pending,
      "Pending Future Predictions",
      icon = icon("hourglass-half"),
      color = "yellow"
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
      # Use a subquery to fetch only the 31 most recent days
      query <- sprintf("SELECT timestamp, open, close, predicted_close, actual_return, error_val, error_pct, pred_signal, actual_signal, correct, cum_profit_pct FROM (SELECT * FROM %s_predictions ORDER BY timestamp DESC LIMIT 31) sub ORDER BY timestamp ASC", input$global_symbol)
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
    
    # Fallback if DB disconnected
    if(is.null(stock_data)) {
      stock_data <- stocks_data[[input$global_symbol]]
      if(is.null(stock_data)) return()
    }
    
    # Formatting the dataframe perfectly to match your request
    formatted_data <- data.frame(
      Date = stock_data$timestamp,
      Open = round(stock_data$open, 2),
      `Actual Close` = round(stock_data$close, 2),
      `Pred Close` = round(stock_data$predicted_close, 2),
      `Daily Profit %` = ifelse(is.na(stock_data$actual_return), "", sprintf("%.2f%%", stock_data$actual_return)),
      Error = ifelse(is.na(stock_data$error_val), "", sprintf("%.2f", stock_data$error_val)),
      `Error %` = ifelse(is.na(stock_data$error_pct), "", sprintf("%.2f%%", stock_data$error_pct)),
      `Pred Signal` = ifelse(is.na(stock_data$pred_signal), "", stock_data$pred_signal),
      `Actual Signal` = ifelse(is.na(stock_data$actual_signal), "", stock_data$actual_signal),
      Correct = ifelse(is.na(stock_data$correct), "", stock_data$correct),
      `Cum. Profit %` = ifelse(is.na(stock_data$cum_profit_pct), "", sprintf("%.2f%%", stock_data$cum_profit_pct)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Sort descending to show newest first
    formatted_data <- formatted_data[order(as.Date(formatted_data$Date), decreasing = TRUE), ]
    
    formatted_data
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # === BACKTESTER SERVER LOGIC ===
  # Update the minimum allowable date based on the specific stock's IPO + 90 days
  observeEvent(input$global_symbol, {
    ipo_dates <- list(
      "AAPL" = as.Date("1980-12-12") + 90,
      "AMZN" = as.Date("1997-05-15") + 90,
      "MSFT" = as.Date("1986-03-13") + 90,
      "GOOGL" = as.Date("2004-08-19") + 90,
      "NVDA" = as.Date("1999-01-22") + 90
    )
    updateDateInput(session, "bt_start_date", min = ipo_dates[[input$global_symbol]])
  })
  
  observeEvent(input$reset_date, {
    updateDateInput(session, "bt_start_date", value = Sys.Date() - 15)
  })
  
  observeEvent(input$run_bt, {
    symbol <- input$global_symbol
    start_date <- as.Date(input$bt_start_date)
    end_date <- start_date + 15
    if (end_date > Sys.Date()) end_date <- Sys.Date()
    
    withProgress(message = 'Running Historical Sandbox', value = 0, {
      
      incProgress(0.2, detail = "Fetching Historical Data...")
      # Fetch 400 days early to ensure enough data for a robust 1-year (252 trading days) training set
    tryCatch({
      getSymbols(symbol, src = "yahoo", from = start_date - 400, to = end_date)
      data_xts <- get(symbol)
      bt_candlestick_xts(tail(data_xts, 60))
      
      open_price <- Op(data_xts)
      close_price <- Cl(data_xts)
      timestamp <- index(data_xts)
      
      bt_data <- data.frame(
        timestamp = as.Date(timestamp),
        open = as.numeric(open_price),
        close = as.numeric(close_price),
        MA_seven = as.numeric(SMA(open_price, n = 7)),
        MA_fourteen = as.numeric(SMA(open_price, n = 14)),
        MA_twenty = as.numeric(SMA(open_price, n = 20)),
        SD_seven = as.numeric(runSD(open_price, n = 7)),
        SD_fourteen = as.numeric(runSD(open_price, n = 14)),
        SD_twenty = as.numeric(runSD(open_price, n = 20))
      )
      bt_data$Target_Close <- dplyr::lead(bt_data$close, 1)
      bt_data <- na.omit(bt_data)
      
      incProgress(0.5, detail = "Starting Sliding Window AI Simulation...")
      
      bt_data$predicted_close <- NA
      test_dates <- bt_data$timestamp[bt_data$timestamp >= start_date & bt_data$timestamp <= end_date]
      
      # True Walk-Forward Sliding Window (Mimics Live Monitoring exactly!)
      for (i in seq_along(test_dates)) {
        current_date <- test_dates[i]
        
        # Step 1: Extract 30 days strictly prior to current_date
        train_candidates <- bt_data[bt_data$timestamp < current_date, ]
        if(nrow(train_candidates) < 30) next
        train_data <- tail(train_candidates, 30)
        
        # Step 2: Train exact replica of Live AI (30 rows, nrounds=10)
        X_train <- as.matrix(train_data[, c("MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")])
        Y_train <- train_data$Target_Close
        temp_model <- xgboost(data = X_train, label = Y_train, nrounds = 10, objective = "reg:squarederror", verbose = 0)
        
        # Step 3: Predict using ONLY current_date features
        current_row <- bt_data[bt_data$timestamp == current_date, ]
        X_pred <- as.matrix(current_row[, c("MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")])
        pred <- predict(temp_model, newdata = X_pred)
        
        # Step 4: Strict 1-Day Forward Log (Features from today predict tomorrow)
        row_idx <- which(bt_data$timestamp == current_date)
        if(row_idx < nrow(bt_data)) {
          bt_data$predicted_close[row_idx + 1] <- pred
        }
      }
        
      # Vectorized Metrics Calculation
        prev_close <- dplyr::lag(bt_data$close, 1)
        bt_data$actual_return <- (bt_data$close - prev_close) / prev_close * 100
        bt_data$error_val <- abs(bt_data$predicted_close - bt_data$close)
        bt_data$error_pct <- (bt_data$error_val / bt_data$close) * 100
        bt_data$pred_signal <- ifelse(!is.na(bt_data$predicted_close) & !is.na(prev_close), ifelse(bt_data$predicted_close > prev_close, "Up", "Down"), NA)
        bt_data$actual_signal <- ifelse(!is.na(bt_data$close) & !is.na(prev_close), ifelse(bt_data$close > prev_close, "Up", "Down"), NA)
        bt_data$correct <- ifelse(!is.na(bt_data$pred_signal) & !is.na(bt_data$actual_signal), ifelse(bt_data$pred_signal == bt_data$actual_signal, "✅", "❌"), "⏳")
        
        # Filter explicitly to the custom user dates
        bt_data <- bt_data[bt_data$timestamp >= start_date & bt_data$timestamp <= end_date, ]
        
        # Recalculate cum_profit_pct strictly for this window so it cleanly starts at 0
        strat_ret_filtered <- ifelse(bt_data$pred_signal == "Up", bt_data$actual_return, -bt_data$actual_return)
        strat_ret_filtered[is.na(strat_ret_filtered)] <- 0
        bt_data$cum_profit_pct <- cumsum(strat_ret_filtered)
        
        incProgress(1.0, detail = "Done!")
        
        bt_results(bt_data)
    }, error = function(e) { print(e) })
    
    }) # End withProgress
  })
  
  # === BACKTESTER UI OUTPUTS ===
  output$bt_profit_box <- renderValueBox({
    res <- bt_results()
    if(is.null(res)) return(valueBox("0%", "Period Cumulative Profit", icon=icon("piggy-bank"), color="blue"))
    prof <- round(tail(res$cum_profit_pct, 1), 2)
    valueBox(paste0(prof, "%"), "Period Cumulative Profit", icon=icon("piggy-bank"), color=ifelse(prof>=0, "green", "red"))
  })
  
  output$bt_accurate_box <- renderValueBox({
    res <- bt_results()
    if(is.null(res)) return(valueBox(0, "Accurate Predictions", icon=icon("thumbs-up"), color="green"))
    valueBox(sum(res$correct == "✅", na.rm=TRUE), "Accurate Predictions", icon=icon("thumbs-up"), color="green")
  })
  
  output$bt_inaccurate_box <- renderValueBox({
    res <- bt_results()
    if(is.null(res)) return(valueBox(0, "Inaccurate Predictions", icon=icon("thumbs-down"), color="red"))
    valueBox(sum(res$correct == "❌", na.rm=TRUE), "Inaccurate Predictions", icon=icon("thumbs-down"), color="red")
  })
  
  output$bt_stock_plot <- renderPlotly({
    res <- bt_results()
    if(is.null(res)) return(NULL)
    
    # Calculate UP/DOWN text for predictions
    res$pred_text <- ""
    for(i in 2:nrow(res)) {
      if(!is.na(res$predicted_close[i]) && !is.na(res$close[i-1])) {
        if(res$predicted_close[i] > res$close[i-1]) res$pred_text[i] <- "UP" else res$pred_text[i] <- "DOWN"
      }
    }
    
    plot_data <- data.frame(
      x = res$timestamp,
      y_actual = res$close,
      y_predicted = res$predicted_close,
      pred_text = res$pred_text
    )
    plot_ly(data = plot_data, x = ~x) %>%
      add_lines(y = ~y_actual, name = 'Actual', line = list(color = "blue", width = 3)) %>%
      add_trace(y = ~y_predicted, name = 'Predicted', type = 'scatter', mode = 'lines+text+markers',
                text = ~pred_text, textposition = 'top center', textfont = list(color = 'red', size = 12, weight = "bold"),
                line = list(color = "red", width = 3, dash = 'dash')) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Close Price"), legend = list(x = 0.1, y = 0.9))
  })
  
  output$bt_candlestick_plot <- renderPlotly({
    df_xts <- bt_candlestick_xts()
    if(is.null(df_xts)) return()
    df <- data.frame(Date = index(df_xts), coredata(df_xts))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    plot_ly(data = df, x = ~Date, type="candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
      layout(xaxis = list(rangeslider = list(visible = F)), yaxis = list(title = "Price"))
  })
  
  output$bt_table <- renderDataTable({
    res <- bt_results()
    if(is.null(res)) return(NULL)
    
    formatted_data <- data.frame(
      Date = res$timestamp,
      Open = round(res$open, 2),
      `Actual Close` = round(res$close, 2),
      `Pred Close` = round(res$predicted_close, 2),
      `Daily Profit %` = ifelse(is.na(res$actual_return), "", sprintf("%.2f%%", res$actual_return)),
      Error = ifelse(is.na(res$error_val), "", sprintf("%.2f", res$error_val)),
      `Error %` = ifelse(is.na(res$error_pct), "", sprintf("%.2f%%", res$error_pct)),
      `Pred Signal` = ifelse(is.na(res$pred_signal), "", res$pred_signal),
      `Actual Signal` = ifelse(is.na(res$actual_signal), "", res$actual_signal),
      Correct = ifelse(is.na(res$correct), "", res$correct),
      `Cum. Profit %` = ifelse(is.na(res$cum_profit_pct), "", sprintf("%.2f%%", res$cum_profit_pct)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
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