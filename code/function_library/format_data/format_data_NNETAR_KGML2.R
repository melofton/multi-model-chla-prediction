# Format data to run ETS model to predict GLM-AED residuals
# Author: Mary Lofton
# Date: 10OCT24

# Purpose: use GLM-AED chl-a as input, predict residuals of GLM-AED chl-a compared 
# compared to observations as output

library(tidyverse)
library(lubridate)

format_data_NNETAR_KGML2 <- function(filepath_GLMAED = "./model_output/validation_output.csv",
                                 filepath_obs = "./data/data_processed/NNETAR.csv",
                                 cal_val_dates = c("2022-01-01","2023-12-31")){
  
  # pull GLM_AED chlorophyll-a predictions for 1.6 m and calculate residuals
  GLMAED_pred <- read_csv(filepath_GLMAED) %>%
    filter(model_id == "GLM-AED")
  
  obs_dates <- data.frame(datetime = seq.Date(from = as.Date(cal_val_dates[1]), to = as.Date(cal_val_dates[2]), by = "day"))
  
  obs <- read_csv(filepath_obs) %>%
    right_join(.,obs_dates, by = "datetime") %>%
    filter(datetime %in% obs_dates$datetime) %>%
    select(datetime, Chla_ugL_mean)
  
  resid_df <- left_join(GLMAED_pred, obs, by = "datetime") %>%
    mutate(residuals = prediction - Chla_ugL_mean) %>%
    mutate(residuals = ifelse(year(reference_datetime) == 2023, NA, residuals)) %>%
    mutate(horizon = datetime - reference_datetime) 
  
  # pull GLM-AED met driver data
  met <- read_csv("./code/model_files/GLM-AED/prediction/inputs/met.csv") %>%
    arrange(time) %>%
    mutate(date = date(time)) %>%
    select(-time) %>%
    group_by(date) %>%
    summarise(across(AirTemp:Snow, ~ mean(.x, na.rm = TRUE))) %>%
    rename(datetime = date) %>%
    filter(datetime >= cal_val_dates[1] & datetime <= cal_val_dates[2])
  
  # pull GLM-AED inflow/outflow driver data
  inf <- read_csv("./code/model_files/GLM-AED/calibration/inputs/FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
    arrange(time) %>%
    select(-c(TRC_tr1:NCS_ss2,CAR_ch4_bub,PHY_Nfixer:BIV_filtfrac)) %>%
    rename(INFLOW = FLOW,
           datetime = time) %>%
    filter(datetime >= cal_val_dates[1] & datetime <= cal_val_dates[2])
  
  NNETAR_KGML_data <- resid_df %>%
    right_join(., met) %>%
    right_join(., inf)
  return(NNETAR_KGML_data)
}

