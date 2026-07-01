# Script to train advanced XGBoost models for Stock Prediction
# Make sure to install xgboost: install.packages("xgboost")
library(quantmod)
library(TTR)
library(xgboost)
library(dplyr)

symbols <- c("AAPL", "AMZN", "NVDA", "GOOGL", "MSFT")

for (symbol in symbols) {
  cat("Training XGBoost model for", symbol, "...\n")
  
  # Fetch last 90 days of data (need buffer for 20-day moving average, to get exactly 30 clean rows)
  tryCatch({
    getSymbols(symbol, src = "yahoo", from = Sys.Date() - 90, to = Sys.Date())
  }, error = function(e) {
    cat("Error fetching data for", symbol, "\n")
  })
  
  data_xts <- get(symbol)
  open_price <- Op(data_xts)
  close_price <- Cl(data_xts)
  
  # Feature Engineering
  MA_seven <- SMA(open_price, n = 7)
  MA_fourteen <- SMA(open_price, n = 14)
  MA_twenty <- SMA(open_price, n = 20)
  SD_seven <- runSD(open_price, n = 7)
  SD_fourteen <- runSD(open_price, n = 14)
  SD_twenty <- runSD(open_price, n = 20)
  
  # Create data frame with calculated features
  dataset <- data.frame(
    open = as.numeric(open_price),
    close = as.numeric(close_price),
    MA_seven = as.numeric(MA_seven),
    MA_fourteen = as.numeric(MA_fourteen),
    MA_twenty = as.numeric(MA_twenty),
    SD_seven = as.numeric(SD_seven),
    SD_fourteen = as.numeric(SD_fourteen),
    SD_twenty = as.numeric(SD_twenty)
  )
  
  # Shift the target variable! 
  # We want to predict TOMORROW'S close using TODAY'S features.
  # So Target_Close for row 'i' is the close price of row 'i+1'
  dataset$Target_Close <- lead(dataset$close, 1)
  
  # Drop NA values (this removes the first ~20 rows due to MAs, and the very last row due to 'lead')
  dataset <- na.omit(dataset)
  
  # Restrict to only the most recent 30 days of training data as requested!
  dataset <- tail(dataset, 30)
  
  # Predictors and shifted target
  X <- as.matrix(dataset[, c("MA_seven", "MA_fourteen", "MA_twenty", "SD_seven", "SD_fourteen", "SD_twenty")])
  Y <- dataset$Target_Close
  
  # Train XGBoost model
  # nrounds reduced since 30 rows is extremely tiny
  xgb_model <- xgboost(data = X, label = Y, 
                       nrounds = 10, 
                       objective = "reg:squarederror", 
                       verbose = 0)
  
  # Save the model
  saveRDS(xgb_model, paste0("../models/", symbol, "_XGB.rds"))
  cat("Saved", paste0("../models/", symbol, "_XGB.rds"), "\n")
}

cat("All models trained successfully.\n")
