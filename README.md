# API-Driven US Stock Market Prediction App

This Shiny application fetches real-time stock data via **Yahoo Finance API**, performs **feature engineering**, and applies a **linear regression model** to predict the closing price of top US stocks: **AAPL, AMZN, MSFT, GOOGL, and NVDA**.

Check live predictions from just yesterday and the past few days for Apple, Amazon, Microsoft, Google, and Nvidia ([AAPL, AMZN, MSFT, GOOGL, NVDA]).

# Live App: 
----------------------------------------------------------------------------------------------------------------------
[https://sathyav99.shinyapps.io/API_US_stock_prediction/](https://sathyav99.shinyapps.io/API_US_stock_prediction/)
----------------------------------------------------------------------------------------------------------------------
---

## Key Features

The app is divided into four tabs:

### 1. Home Tab  
- Provides a brief intro.
- Walkthrough on how the app works.
- Explains how the model predicts using API and pretrained weights.

---

### 2. Data Tab  
Lets users explore **historical datasets**.

- `Dataset`: View raw stock data (Open, Close, Volume, etc.).
- `Summary`: Get statistics (mean, median, standard deviation, etc.).
- `Structure`: See the structure (columns, data types).

Supports: `AAPL`, `AMZN`, `GOOGL`, `NVDA`, `MSFT`

---

### 3. Visualization Tab  
Visualize stock behavior over the **last 30 days** using:

- `Histogram`: Volume distribution  
- `Boxplot`: Closing price spread  
- `LineGraph`: Trend over time  
- `ScatterPlot`: Close price over dates  

---

### 4. Prediction Tab  
- Runs linear regression predictions using recent data.
- Predicts next 5 days' closing prices.
- Shows comparison between **actual vs. predicted**.
- Displays plot + table output.

---

## How Prediction Works

1. Fetch **60 days of stock data** using:

```r
getSymbols(symbol, src = "yahoo", from = Sys.Date()-60, to = Sys.Date())
```

2. Engineer features for regression:

```r
MA_seven     <- SMA(open, n = 7)
MA_fourteen  <- SMA(open, n = 14)
MA_twenty    <- SMA(open, n = 20)
SD_seven     <- runSD(open, n = 7)
SD_fourteen  <- runSD(open, n = 14)
SD_twenty    <- runSD(open, n = 20)
```

3. Load pretrained model:

```r
re_LR_model <- readRDS(paste0(symbol, "_LR.rds"))
```

4. Update model with latest 30-day data and predict last 5 days:

```r
retrained_model <- update(re_LR_model, data = last_30_days)
predicted_close <- predict(retrained_model, newdata = tail(latest_opening, 5))
```

5. Display output in a **plotly line chart** and **data table**.

---

## Libraries Used

```r
library(shiny)
library(quantmod)
library(TTR)
library(datasets)
library(plotly)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(lubridate)
```

---

## Sync Across Tabs

The selected stock symbol in any tab is synced across all tabs using:

```r
observe({
  updateSelectInput(session, "dashboard_symbol", selected = input$symbol)
  updateSelectInput(session, "stock_symbol", selected = input$symbol)
  updateSelectInput(session, "symbol", selected = input$symbol)
  retrieveData(input$symbol)
})
```

---

## Yahoo Finance Link

Each stock links directly to Yahoo Finance:

```r
tags$a(href = paste0("https://finance.yahoo.com/quote/", input$symbol),
       icon("globe"), target="_blank")
```

---

## Visualization Preview

### Example Boxplot (Last 30 Days)

```r
boxplot(stock_data_2$Close, col ='red',
        main = "Boxplot of Closing Value for 30 days", 
        ylab = "Close Value")
```

### Example LineGraph

```r
plot(stock_data_2$timestamp, stock_data_2$Close, 
     type = "l", col = "red", main = "LineGraph")
```

---

## Model Strategy

- Linear regression with selected features
- Only last 30 days used to keep predictions fresh
- Model updated at runtime with `update()` and new inputs

---

## How to Run Locally

1. Clone the repo  
2. Place `*.csv` and `*.rds` model files in root  
3. Run in RStudio or R console:

```r
shiny::runApp()
```

---
