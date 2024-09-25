#Predict chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022-2023 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)

#Load prediction functions
source("./code/function_library/predict/GLMAED.R")


#Read in data
dat_GLMAED <- read_csv("./data/data_processed/GLMAED.csv")

pred_GLMAED <- GLMAED(spinup_folder = "./code/model_files/GLM-AED/spinup",
                      prediction_folder = "./code/model_files/GLM-AED/prediction",
                      rerun_spinup = TRUE,
                      spinup_dates = c('2018-04-20 12:00:00','2021-12-31 12:00:00'),
                      start_from_spinup = TRUE,
                      pred_dates = seq.Date(from = as.Date("2021-12-31"), to = as.Date("2022-01-07"), by = "day"),
                      forecast_horizon = 35,
                      wq_vars = c('OXY_oxy','CAR_dic','CAR_pH','CAR_ch4','SIL_rsi','NIT_amm','NIT_nit','PHS_frp','OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop','OGM_docr','OGM_donr','OGM_dopr','OGM_cpom','PHY_hot','PHY_cold','PHY_Nfixer'),
                      data = dat_GLMAED,
                      phyto_nml_file = "/aed/aed2_phyto_pars_24MAY24_MEL.nml",
                      container = "rocker-flare_4.4")

write.csv(pred_GLMAED, "./model_output/GLMAED_20240903.csv", row.names = FALSE)

