#Predict chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022-2023 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)

#Load prediction functions
predict.model.functions <- list.files("./code/function_library/predict")
sapply(paste0("./code/function_library/predict/",predict.model.functions),source,.GlobalEnv)

#Read in data
dat_persistence <- read_csv("./data/data_processed/persistence.csv")
dat_historicalMean <- read_csv("./data/data_processed/historicalMean.csv")
dat_DOY <- read_csv("./data/data_processed/DOY.csv")
dat_ETS <- read_csv("./data/data_processed/ETS.csv")
dat_ARIMA <- read_csv("./data/data_processed/ARIMA.csv")
dat_TSLM <- read_csv("./data/data_processed/TSLM.csv")
dat_processModels <- read_csv("./data/data_processed/processModels.csv")
dat_XGBoost <- read_csv("./data/data_processed/XGBoost.csv")
dat_prophet <- read_csv("./data/data_processed/prophet.csv")
dat_NNETAR <- read_csv("./data/data_processed/NNETAR.csv")

#Set prediction window and forecast horizon
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")
forecast_horizon = 35

#Predict chl-a
#Each function should take processed data, pred_dates, and forecast_horizon
#Each function should subset to pred_dates, run a forecast with max horizon of
#forecast_horizon for each date, and return a dataframe with the following structure:
#Model: name of model
#referenceDate: forecast issue date (will be list of pred_dates)
#Date: date of forecast (will go from pred_date[i] + 1 to pred_date[i] + 7 for each
#referenceDate)
#Chla_ugL: predicted value of chl-a

pred_persistence <- persistence(data = dat_persistence,
                                pred_dates = pred_dates,
                                forecast_horizon = forecast_horizon)

pred_historicalMean <- historicalMean(data = dat_historicalMean,
                                pred_dates = pred_dates,
                                forecast_horizon = forecast_horizon)

pred_DOY <- DOY(data = dat_DOY,
                pred_dates = pred_dates,
                forecast_horizon = forecast_horizon)

pred_ETS <- fableETS(data = dat_ETS,
                     pred_dates = pred_dates,
                     forecast_horizon = forecast_horizon)

pred_ARIMA <- fableARIMA(data = dat_ARIMA,
                pred_dates = pred_dates,
                forecast_horizon = forecast_horizon)

pred_TSLM <- fableTSLM(data = dat_TSLM,
                         pred_dates = pred_dates,
                         forecast_horizon = forecast_horizon)

pred_prophet <- pred_prophet(data = dat_prophet,
                     pred_dates = pred_dates,
                     forecast_horizon = forecast_horizon)

pred_XGBoost <- parsnipXGBoost(data = dat_XGBoost,
                               pred_dates = pred_dates,
                               forecast_horizon = forecast_horizon)

pred_NNETAR <- fableNNETAR(data = dat_NNETAR,
                         pred_dates = pred_dates,
                         forecast_horizon = forecast_horizon)

#Stack model output and write to file
mod_output <- bind_rows(pred_persistence, pred_historicalMean, pred_DOY, pred_ETS, pred_ARIMA, pred_TSLM, pred_prophet, pred_XGBoost, pred_NNETAR)

#OR if you only want to run one model
mod_output <- read_csv("./model_output/validation_output.csv") %>%
  filter(!model_id == "ARIMA") %>%
  bind_rows(.,pred_ARIMA)
unique(mod_output$model_id)

# #OR if you are reading in LSTM output
# LSTM_output <- read_csv("./multi-model-ensemble/model_output/LSTM_output.csv") %>%
#   add_column(horizon = c(1:21)) %>%
#   gather(-horizon,key = "reference_datetime", value = "prediction") %>%
#   mutate(datetime = as.Date(reference_datetime) + horizon,
#          reference_datetime = as.Date(reference_datetime)) %>%
#   add_column(variable = "chlorophyll-a",
#              model_id = "LSTM") %>%
#   select(model_id, reference_datetime, datetime, variable, prediction)
# mod_output <- read_csv("./multi-model-ensemble/model_output/validation_output.csv") %>%
#   bind_rows(.,LSTM_output)
# unique(mod_output$model_id)

write.csv(mod_output, "./model_output/validation_output.csv", row.names = FALSE)

