#Predict chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022-2023 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)

#Load prediction functions
source("./code/function_library/predict/GLMAED_ncdf.R")

#Read in data
dat_GLMAED <- read_csv("./data/data_processed/GLMAED.csv")

n_trials = 5

for(i in 1:n_trials){
pred_GLMAED <- GLMAED(spinup_folder = "./code/model_files/GLM-AED/spinup",
                      prediction_folder = "./code/model_files/GLM-AED/prediction",
                      rerun_spinup = TRUE,
                      spinup_dates = c('2018-04-20 12:00:00','2022-01-01 12:00:00'),
                      start_from_spinup = TRUE,
                      pred_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                      forecast_horizon = 35,
                      wq_vars = c('OXY_oxy','CAR_dic','CAR_pH','CAR_ch4','SIL_rsi','NIT_amm','NIT_nit','PHS_frp','OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop','OGM_docr','OGM_donr','OGM_dopr','OGM_cpom','PHY_hot','PHY_cold','PHY_Nfixer'),
                      data = dat_GLMAED,
                      phyto_nml_file = "/aed/aed2_phyto_pars_24MAY24_MEL.nml")

filename = paste0("./model_output/GLMAED_20240926_trial",i,".csv")
write.csv(pred_GLMAED, filename, row.names = FALSE)
}

