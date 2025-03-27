#Predict chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022-2023 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)
library(reticulate)

#Load prediction functions
source("./code/function_library/predict/LSTM.R")

#Read in data
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")
pred_dates <- seq.Date(from = as.Date("2023-08-01"), to = as.Date("2023-11-26"), by = "day")
forecast_horizon = 20

# pred_LSTM <- LSTM(data = dat_LSTM,
#                   pred_dates = pred_dates,
#                   forecast_horizon = forecast_horizon)

LSTM(data = dat_LSTM,
     pred_dates = pred_dates,
     forecast_horizon = forecast_horizon)

#write.csv(pred_LSTM, "./model_output/LSTM.csv", row.names = FALSE)

