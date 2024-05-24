#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)

#Load model fitting functions
fit.model.functions <- list.files("./code/function_library/fit_models")
sapply(paste0("./code/function_library/fit_models/",fit.model.functions),source,.GlobalEnv)

#Read in data
dat_persistence <- read_csv("./data/data_processed/persistence.csv")
dat_historicalMean <- read_csv("./data/data_processed/historicalMean.csv")
dat_DOY <- read_csv("./data/data_processed/DOY.csv")
dat_ETS <- read_csv("./data/data_processed/ETS.csv")
dat_ARIMA <- read_csv("./data/data_processed/ARIMA.csv")
dat_ARIMA_noDrivers <- read_csv("./data/data_processed/ARIMAnoDrivers.csv")
dat_TSLM <- read_csv("./data/data_processed/TSLM.csv")
dat_processModels <- read_csv("./data/data_processed/processModels.csv")
dat_XGBoost <- read_csv("./data/data_processed/XGBoost.csv")
dat_prophet <- read_csv("./data/data_processed/prophet.csv")
dat_NNETAR <- read_csv("./data/data_processed/NNETAR.csv")
dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")


#Set sim folder (for GLM-AED)
sim_folder <- "./code/model_files/GLM-AED/calibration"

#Fit models 
fit_persistence <- fit_persistence(data = dat_persistence, cal_dates = c("2018-08-06","2021-12-31"))
fit_persistence$plot

fit_historicalMean <- fit_historicalMean(data = dat_historicalMean, cal_dates = c("2018-08-06","2021-12-31"))
fit_historicalMean$plot

fit_DOY <- fit_DOY_chla(data = dat_DOY, cal_dates = c("2018-08-06","2021-12-31"))
fit_DOY$plot

fit_ETS <- fit_ETS(data = dat_ETS, cal_dates = c("2018-08-06","2021-12-31"))
fit_ETS$plot

fit_ARIMA <- fit_ARIMA(data = dat_ARIMA, cal_dates = c("2018-08-06","2021-12-31"))
fit_ARIMA$plot

fit_ARIMA_noDrivers <- fit_ARIMA(data = dat_ARIMA_noDrivers, cal_dates = c("2018-08-06","2021-12-31"), include_drivers = FALSE)
fit_ARIMA$plot

fit_TSLM <- fit_TSLM(data = dat_TSLM, cal_dates = c("2018-08-06","2021-12-31"))
fit_TSLM$plot

fit_XGBoost <- fit_XGBoost(data = dat_XGBoost, cal_dates = c("2018-08-06","2021-12-31"))
fit_XGBoost$plot

fit_prophet <- fit_prophet(data = dat_prophet, cal_dates = c("2018-08-06","2021-12-31"))
fit_prophet$plot

fit_NNETAR <- fit_NNETAR(data = dat_NNETAR, cal_dates = c("2018-08-06","2021-12-31"))
fit_NNETAR$plot

fit_LSTM <- fit_LSTM(data = dat_LSTM, cal_dates = c("2018-08-06","2021-12-31"), forecast_horizon = 20)
fit_LSTM$plot

#Calibrate process models (this completes one run + diagnostics + assessment
# metrics for GLM-AED) - you must be in a container to run this!
GLMAED_run <- calibrate_GLMAED(sim_folder = sim_folder, save_plot = FALSE)
OneDProcessModel_run <- calibrate_1DProcessModel(
  data = dat_1DProcessModel,
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
            10), #Xcc,
  cal_dates = c("2018-08-06","2022-01-01"),
  save_plots = TRUE,
  inputs = NULL
)
write.csv(OneDProcessModel_run$output_df, "./code/model_files/1DProcessModel/output.csv", row.names = FALSE)

OneDProcessModel_run$out <- OneDProcessModel_run$output_df %>%
  filter(depth == 1.5 & variable == "chla") %>%
  add_column(model_id = "OneDProcessModel") %>%
  mutate(depth = 1.6,
         variable = "chlorophyll-a") %>%
  select(model_id, datetime, variable, prediction)


#Stack model predictions and write to file (not applicable for persistence model
#and currently not supported for models fit in JAGS)
# mod_output <- bind_rows(fit_DOY$out, fit_ARIMA$out, fit_ETS$out, fit_TSLM$out,
#                         fit_XGBoost$out, fit_prophet$out, fit_NNETAR$out)
# 
#OR if you only want to run (or re-run) one or a few models
mod_output <- read_csv("./model_output/calibration_output.csv") %>%
  #filter(!model_id %in% c("OptimumMonod")) %>% #names of re-run models if applicable
  bind_rows(.,fit_LSTM$out) # %>% #bind rows with models to add/replace if applicable

write.csv(mod_output, "./model_output/calibration_output.csv", row.names = FALSE)
unique(mod_output$model_id)
