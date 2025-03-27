#Predict chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022-2023 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)
library(deSolve)

# source model functions
source("./code/function_library/predict/1DProcessModel.R")

dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")

AGGREGATE = TRUE

OneDProcessModel(data = dat_1DProcessModel,
                parms = c(-0.03, #w_p (negative is down, positive is up)
                          0.8, #R_growth
                          1.02,#1.1, #theta_growth
                          0.5, #light_extinction
                          5, #I_K
                          1, #N_o
                          1, #K_N
                          0.05, #P_o
                          0.05, #K_P
                          0.005, #f_pr
                          0.17, #R_resp
                          1.04, #theta_resp
                          10, #T_std
                          12,#20, #T_opt
                          35,#35, #T_max
                          0.02, #N_C_ratio
                          0.002, #P_C_ratio
                          0, #phyto_flux_top
                          9.5,# lake_depth
                          38,# num_boxes
                          0.02,#KePHYTO
                          0.1, #K (diffusivity)
                          0.1,#p0
                          10), #Xcc
                pred_dates = c("2021-12-31","2023-11-26"),
                forecast_horizon = 35)

if(AGGREGATE == TRUE){
  csv_fils <- list.files("model_output/OneDProcessModel", full.names = TRUE) 
  dat <- map_df(csv_fils, read_csv, .id = "pred_id") %>%
    select(-pred_id)
  write.csv(dat, "./model_output/OneDProcessModel.csv", row.names = FALSE)
}
