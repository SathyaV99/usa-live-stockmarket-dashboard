# API-Driven US Stock Market Prediction App

This Shiny application fetches real-time stock data via the **Yahoo Finance API**, performs **feature engineering**, and applies an advanced **XGBoost machine learning model** to predict the closing price of top US stocks (AAPL, AMZN, MSFT, GOOGL, NVDA) for the **next trading day**.

It also simulates a live trading strategy, tracks cumulative profit, and permanently logs predictions to a secure **Neon PostgreSQL cloud database**.

----------------------------------------------------------------------------------------------------------------------
# Live App (CLICK HERE): 
[https://sathyav99.shinyapps.io/API_US_stock_prediction/](https://sathyav99.shinyapps.io/API_US_stock_prediction/)
----------------------------------------------------------------------------------------------------------------------

---

## Key Features

The app is a highly focused, **Single-Page Dashboard** containing the following core elements:

### 1. Live Trading Metrics (Value Boxes)
The dashboard continuously backtests the model's predictions over the last 30 days and dynamically calculates:
- **30-Day Cumulative Profit**: Simulates a Long/Short strategy based on the model's signals (Buy if Up, Short if Down).
- **Accurate vs Inaccurate**: A strict count of how many times the model correctly predicted the market's direction.

### 2. Interactive Charts
- **Prediction Chart (5 Days)**: A line chart comparing the actual close prices to the model's predicted close prices, complete with explicit **UP** and **DOWN** prediction markers.
- **Macro Trend Chart (60 Days)**: A classic Candlestick chart (Open, High, Low, Close) to provide visual context on the recent momentum leading up to the predictions.

### 3. Historical Prediction Logs
A dynamic data table that secretly connects to a **Neon PostgreSQL** backend to fetch all historical predictions. 
- Automatically calculates `Actual Return %`, `Daily Profit %`, and `Cum. Profit %`.
- Fully filterable by custom Date Ranges.
- Highlights accurate predictions with ✅ and inaccurate with ❌.

---

## How True Forecasting Works

Unlike standard lagging models, this architecture is a true forecaster (predicting t+1):

1. **Live Data**: Fetch 90 days of live stock data using `quantmod::getSymbols`.
2. **Feature Engineering**: Calculate 7, 14, and 20-day Moving Averages and Standard Deviations based on the Opening price.
3. **Time-Shifting**: Align "Today's" features with "Tomorrow's" Target Close. 
4. **Strict Constraint**: The XGBoost model (`_XGB.rds`) is trained *strictly* on a rolling 30-day window to capture only the most recent market regime.
5. **Inference**: Pass today's live data into the model to predict **Tomorrow's Close**.
6. **Cloud Sync**: Append the generated prediction for tomorrow to the Neon database to track accuracy when tomorrow's actual price closes.

---

## Libraries Used

```r
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
```

---

## Automated Deployment

Deploying the app to shinyapps.io is entirely automated for users without RStudio:
- A `shinyapps_auth.R` file securely holds deployment credentials (git-ignored).
- Double-clicking `deploy_dashboard.bat` automatically authenticates the machine, bundles the app, and pushes it to the live server.

---

## How to Run Locally

1. Clone the repository.
2. Ensure you have the `_XGB.rds` models in the root folder.
3. Create a `credentials.R` file with your Neon database credentials.
4. Double-click `run_dashboard.bat` to launch the app locally!
