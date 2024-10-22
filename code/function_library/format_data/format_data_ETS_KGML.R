# Format data to run ETS model to predict GLM-AED residuals
# Author: Mary Lofton
# Date: 10OCT24

# Purpose: use GLM-AED chl-a as input, predict residuals of GLM-AED chl-a compared 
# compared to observations as output

library(tidyverse)
library(lubridate)

format_data_ETS_KGML <- function(filepath_GLMAED = "./code/model_files/GLM-AED/calibration/output/output.nc",
                                 filepath_obs = "./data/data_processed/ETS.csv",
                                 cal_dates = c("2020-01-01","2021-12-31"),
                                 GLM_run_dates = c("2018-04-20","2021-12-31")){
  
  nc_filepath <- file.path(filepath_GLMAED)
  
  nc_file <- ncdf4::nc_open(nc_filepath)
  
  message("the_depths")
  all_elevs <- ncdf4::ncvar_get(nc_file, var = "H")
  closest <- function(x){which.min(abs(x - 7.7))}
  EXO_elevs_index <- apply(all_elevs, 2, closest)

  message("chlorophyll-a")
  all_chl <- ncdf4::ncvar_get(nc_file, var = "PHY_tchla")
  GLM_EXO_chl = all_chl[cbind(EXO_elevs_index, seq_along(EXO_elevs_index))] 
  
  GLM_dates <- seq.Date(from = as.Date(GLM_run_dates[1]), to = as.Date(GLM_run_dates[2]), by = "day")
  obs_dates <- data.frame(datetime = seq.Date(from = as.Date(cal_dates[1]), to = as.Date(cal_dates[2]), by = "day"))
  
  GLM_chl_indices <- c(which(GLM_dates == cal_dates[1]),which(GLM_dates == cal_dates[2]))
  GLM_chl_for_residuals <- GLM_EXO_chl[GLM_chl_indices[1]:GLM_chl_indices[2]]
  
  obs <- read_csv(filepath_obs) %>%
    right_join(.,obs_dates, by = "datetime") %>%
    filter(datetime %in% obs_dates$datetime)
  
  residuals <- GLM_chl_for_residuals - obs$Chla_ugL_mean
  
  ETS_KGML_data <- data.frame(datetime = obs$datetime,
                              GLMAED_Chla_ugL = GLM_chl_for_residuals,
                              Chla_ugL_mean = obs$Chla_ugL_mean,
                              Chla_residuals_ugL = residuals)
  return(ETS_KGML_data)
}

# some free code to think about residuals
pred <- read_csv("./model_output/validation_output.csv") %>%
  filter(model_id == "GLM-AED") %>%
  mutate(horizon = datetime - reference_datetime) %>%
  filter(horizon == 35)

#source internal functions
source("./code/function_library/format_data/interpolation.R")

res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
start_date = "2022-01-01"
end_date = "2023-11-26"
  
#get list of dates
start_date = as.Date(start_date)
end_date = as.Date(end_date)
daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
colnames(daily_dates)[1] <- "datetime"
  
  ##EXO----
  
  #message
  message("interpolating exo")
  chla <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable == "Chla_ugL_mean" & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, observation) %>%
    rename(Chla_ugL_mean = observation) %>%
    filter(datetime %in% daily_dates$datetime)
  
  #interpolation
  chla2 <- interpolate(daily_dates = daily_dates,
                       data = chla,
                       variables = c("Chla_ugL_mean"),
                       method = "linear")
  
  #format final data frame
  df.out <- chla2 %>%
    select(datetime, Chla_ugL_mean, Flag_Chla_ugL_mean)
  
h35_residuals <- pred$prediction - df.out$Chla_ugL_mean
hist(residuals, xlim = c(-40, 40))
hist(h35_residuals, xlim = c(-40, 40))
mean(residuals)
mean(h35_residuals)
