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
dat_GLMAED <- read_csv("./data/data_processed/GLMAED.csv")
dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")

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

pred_GLMAED <- GLMAED(spinup_folder = "./code/model_files/GLM-AED/spinup",
                      prediction_folder = "./code/model_files/GLM-AED/prediction",
                      rerun_spinup = TRUE,
                      spinup_dates = c('2018-04-20 12:00:00','2022-01-01 12:00:00'),
                      pred_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-01-03"), by = "day"),
                      forecast_horizon = 35,
                      wq_vars = c('OXY_oxy','CAR_dic','CAR_pH','CAR_ch4','SIL_rsi','NIT_amm','NIT_nit','PHS_frp','OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop','OGM_docr','OGM_donr','OGM_dopr','OGM_cpom','PHY_hot','PHY_cold','PHY_Nfixer'),
                      data = dat_GLMAED,
                      phyto_nml_file = "/aed/aed2_phyto_pars_16APR24_MEL.nml")

pred_OneDProcessModel <- OneDProcessModel(data = dat_1DProcessModel,
                                parms = c(-0.001, #w_p (negative is down, positive is up)
                                          4.5, #R_growth
                                          1.02,#1.1, #theta_growth
                                          1, #light_extinction
                                          60, #I_K
                                          0, #N_o
                                          2.5, #K_N
                                          0, #P_o
                                          0.0001, #K_P
                                          0.1, #f_pr
                                          0.13, #R_resp
                                          1.02, #theta_resp
                                          10, #T_std
                                          12,#20, #T_opt
                                          30,#35, #T_max
                                          0.02, #N_C_ratio
                                          0.002, #P_C_ratio
                                          0, #phyto_flux_top
                                          9.5,# lake_depth
                                          38,# num_boxes
                                          0.005,#KePHYTO
                                          0.01, #D_temp
                                          0,#phyto_flux_bottom
                                          10), #Xcc
                                pred_dates = c("2022-01-01","2023-11-26"),
                                forecast_horizon = 35)

pred_LSTM <- LSTM(data = dat_LSTM,
                  pred_dates = pred_dates,
                  forecast_horizon = forecast_horizon)

#Stack model output and write to file
mod_output <- bind_rows(pred_persistence, pred_historicalMean, pred_DOY, pred_ETS, pred_ARIMA, pred_TSLM, pred_prophet, pred_XGBoost, pred_NNETAR)

#append GLM-AED to model output
pred_GLMAED <- read_csv("./model_output/GLMAED.csv")
pred_GLMAED <- pred_GLMAED %>%
  mutate(reference_datetime = date(reference_datetime))

#OR if you only want to run one model
mod_output <- read_csv("./model_output/validation_output.csv") %>%
  filter(!model_id == "GLM-AED") %>%
  bind_rows(.,pred_GLMAED)

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

