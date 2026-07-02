# scripts/sync_data.R
# This script should be scheduled to run periodically (e.g., via a cron job)
# to keep the database updated with the latest live market data and predictions.

library(quantmod)
library(TTR)
library(xgboost)
library(DBI)
library(RPostgres)
library(lubridate)
library(dplyr)

# Load database credentials
# Attempt to source regardless of whether ran from root or from scripts/ directory
tryCatch({
  source("config/credentials.R")
}, error = function(e) {
  source("../config/credentials.R")
})

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
    return(con)
  }, error = function(e) { 
    print(paste("Database connection failed:", e))
    return(NULL) 
  })
}

# Function to run the heavy AI sync and UPSERT to Cloud
syncDataForSymbol <- function(symbol) {
  print(paste("Syncing data for:", symbol))
  if(is.null(symbol) || symbol == "") return()
  
  # Retrieve data from Yahoo API for 90 days
  tryCatch({
    getSymbols(symbol, src = "yahoo", from = Sys.Date()-90, to = Sys.Date())
  }, error = function(e) {
    print(paste("Failed to fetch Yahoo data for", symbol))
    return()
  })
  
  data_xts <- get(symbol)
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
  
  # Drop NA values
  latest_opening <- na.omit(latest_opening)
  
  # Select the latest 30 dates
  latest_opening <- tail(latest_opening, 30)
  colnames(latest_opening) <- c("timestamp", "open", "close", "MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")
  
  # Load the saved model weights for XGBoost
  xgb_model_file <- paste0(ifelse(dir.exists("models"), "models/", "../models/"), symbol, "_XGB.rds")
  latest_opening$predicted_close <- NA
  
  if (file.exists(xgb_model_file)) {
    model <- readRDS(xgb_model_file)
    
    X_pred <- as.matrix(latest_opening[, c("MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")])
    preds <- predict(model, newdata = X_pred)
    
    latest_opening$predicted_close[2:nrow(latest_opening)] <- preds[1:(length(preds)-1)]
    
    last_date <- as.Date(latest_opening$timestamp[nrow(latest_opening)])
    next_date <- last_date + 1
    if(wday(next_date) == 7) next_date <- next_date + 2
    if(wday(next_date) == 1) next_date <- next_date + 1
    
    future_row <- data.frame(
      timestamp = as.character(next_date),
      open = NA, close = NA,
      MA_seven = NA, MA_fourteen = NA, MA_twenty = NA,
      SD_seven = NA, SD_fourteen = NA, SD_twenty = NA,
      predicted_close = preds[length(preds)]
    )
    latest_opening <- rbind(latest_opening, future_row)
    
    prev_close <- dplyr::lag(latest_opening$close, 1)
    
    latest_opening$actual_return <- (latest_opening$close - prev_close) / prev_close * 100
    latest_opening$error_val <- abs(latest_opening$predicted_close - latest_opening$close)
    latest_opening$error_pct <- (latest_opening$error_val / latest_opening$close) * 100
    
    latest_opening$pred_signal <- ifelse(!is.na(latest_opening$predicted_close) & !is.na(prev_close),
                                         ifelse(latest_opening$predicted_close > prev_close, "Up", "Down"), NA)
    
    latest_opening$actual_signal <- ifelse(!is.na(latest_opening$close) & !is.na(prev_close),
                                           ifelse(latest_opening$close > prev_close, "Up", "Down"), NA)
    
    latest_opening$correct <- ifelse(!is.na(latest_opening$pred_signal) & !is.na(latest_opening$actual_signal),
                                     ifelse(latest_opening$pred_signal == latest_opening$actual_signal, "✅", "❌"),
                                     "⏳")
    
    strategy_return <- ifelse(latest_opening$pred_signal == "Up", latest_opening$actual_return, -latest_opening$actual_return)
    strategy_return[is.na(strategy_return)] <- 0
    latest_opening$cum_profit_pct <- cumsum(strategy_return)
  }
  
  con <- get_db_connection()
  if (!is.null(con)) {
    table_name <- paste0(symbol, "_predictions")
    
    tryCatch({
      DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS actual_return NUMERIC, ADD COLUMN IF NOT EXISTS error_val NUMERIC, ADD COLUMN IF NOT EXISTS error_pct NUMERIC, ADD COLUMN IF NOT EXISTS pred_signal TEXT, ADD COLUMN IF NOT EXISTS actual_signal TEXT, ADD COLUMN IF NOT EXISTS correct TEXT, ADD COLUMN IF NOT EXISTS cum_profit_pct NUMERIC;", table_name))
    }, error = function(e) {})
    
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
    
    for(i in 1:nrow(latest_opening)) {
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
          latest_opening$timestamp[i],
          ifelse(is.na(latest_opening$open[i]), NA, latest_opening$open[i]),
          ifelse(is.na(latest_opening$close[i]), NA, latest_opening$close[i]),
          ifelse(is.na(latest_opening$MA_seven[i]), NA, latest_opening$MA_seven[i]),
          ifelse(is.na(latest_opening$MA_fourteen[i]), NA, latest_opening$MA_fourteen[i]),
          ifelse(is.na(latest_opening$MA_twenty[i]), NA, latest_opening$MA_twenty[i]),
          ifelse(is.na(latest_opening$SD_seven[i]), NA, latest_opening$SD_seven[i]),
          ifelse(is.na(latest_opening$SD_fourteen[i]), NA, latest_opening$SD_fourteen[i]),
          ifelse(is.na(latest_opening$SD_twenty[i]), NA, latest_opening$SD_twenty[i]),
          ifelse(is.na(latest_opening$predicted_close[i]), NA, latest_opening$predicted_close[i]),
          ifelse(is.na(latest_opening$actual_return[i]), NA, latest_opening$actual_return[i]),
          ifelse(is.na(latest_opening$error_val[i]), NA, latest_opening$error_val[i]),
          ifelse(is.na(latest_opening$error_pct[i]), NA, latest_opening$error_pct[i]),
          ifelse(is.na(latest_opening$pred_signal[i]), NA, latest_opening$pred_signal[i]),
          ifelse(is.na(latest_opening$actual_signal[i]), NA, latest_opening$actual_signal[i]),
          ifelse(is.na(latest_opening$correct[i]), NA, latest_opening$correct[i]),
          ifelse(is.na(latest_opening$cum_profit_pct[i]), NA, latest_opening$cum_profit_pct[i])
        ))
      }, error = function(e) {})
    }
    DBI::dbDisconnect(con)
  }
}

# Run sync for all symbols
symbols_list <- c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT")
for(sym in symbols_list) {
  syncDataForSymbol(sym)
}

# Record the sync completion time
con <- get_db_connection()
if (!is.null(con)) {
  tryCatch({
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS sync_status (id INTEGER PRIMARY KEY, last_sync TIMESTAMP)")
    DBI::dbExecute(con, "INSERT INTO sync_status (id, last_sync) VALUES (1, CURRENT_TIMESTAMP) ON CONFLICT (id) DO UPDATE SET last_sync = CURRENT_TIMESTAMP")
  }, error = function(e) { print(e) })
  DBI::dbDisconnect(con)
}

print("Sync complete.")
