#Fit random forest model for chl-a
#Author: Mary Lofton
#Date: 08OCT24

#Purpose: fit random forest model for chla from 2018-2021

pacman::p_load(tidyverse, lubridate, randomForest)
set.seed(100)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_randomForest <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  dates <- as_tibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    select(datetime)
  
  df <- as_tibble(data) %>%
    mutate(lag_Chla_ugL_mean = stats::lag(Chla_ugL_mean, k = 1)) %>%
    filter(datetime >= start_cal & datetime <= stop_cal) 
  
  rf_data <- df %>%
    select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean, lag_Chla_ugL_mean) 

  #run random forest
  rf <- randomForest(Chla_ugL_mean ~ ., data = rf_data, mtry = 3)
  fitted_values <- data.frame(datetime = df$datetime,
                              pred = rf$predicted)

  randomForest_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = datetime, y = pred, color = "randomForest"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #build output df
  df.out <- data.frame(model_id = "randomForest",
                       datetime = df$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$pred)

  
  #return output + model with best fit + plot
  return(list(out = df.out, plot = randomForest_plot))
}
