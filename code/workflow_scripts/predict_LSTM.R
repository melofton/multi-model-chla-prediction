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

#Set variables
AGGREGATE = FALSE

#Read in data
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")
pred_dates <- seq.Date(from = as.Date("2023-04-07"), to = as.Date("2023-05-07"), by = "day")
forecast_horizon = 20

LSTM(data = dat_LSTM,
     pred_dates = pred_dates,
     forecast_horizon = forecast_horizon)

if(AGGREGATE == TRUE){
  csv_fils <- list.files("model_output/LSTM_predictions", full.names = TRUE) 
  dat <- map_df(csv_fils, read_csv, .id = "pred_id") %>%
    select(-pred_id)
  write.csv(dat, "./model_output/LSTM.csv", row.names = FALSE)
}
