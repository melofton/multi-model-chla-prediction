#Tune LSTM hyperparameters
#Author: Mary Lofton
#Date last updated: 03SEP24

#Purpose: Tune hyperparameters for LSTM model to predict chl-a
library(tidyverse)
library(lubridate)
library(reticulate)

#Load prediction functions
source("./code/function_library/fit_models/fit_LSTM.R")

#Read in data
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")

params_list <- list(epochs = c(100,200),
                    dropout = c(0, 0.0001, 0.0005, 0.001, 0.002, 0.01),
                    num_layers = c(1,2,3),
                    hidden_feature_size = c(8,16),
                    weight_decay = c(0, 0.0001, 0.0005, 0.001, 0.002, 0.01))
fit_LSTM <- fit_LSTM(data = dat_LSTM, cal_dates = c("2018-08-06","2021-12-31"), forecast_horizon = 20,
                     input_window = 42, params_list = params_list)

write.csv(fit_LSTM$out, "./model_output/LSTM_tuning.csv", row.names = FALSE)
save(fit_LSTM$plot, file = "./figures/LSTM_tuning.rdata")

