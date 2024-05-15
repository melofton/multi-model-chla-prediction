#Fit LSTM model for chl-a
#Author: Mary Lofton
#Date: 13MAY24

#Purpose: fit LSTM model for chla from 2018-2021

library(fable)
library(moments)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_LSTM <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #fit LSTM
  system('python scriptname data_frame', wait=FALSE)
  
  LSTM_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = datetime, y = .fitted, color = "ARIMA"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "ARIMA",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)

  
  #return output + model with best fit + plot
  return(list(out = df.out, ARIMA = my.arima, plot = ARIMA_plot))
}
