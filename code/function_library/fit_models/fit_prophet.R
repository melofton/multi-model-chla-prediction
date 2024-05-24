#Fit prophet model for chl-a
#Author: Mary Lofton
#Date last updated: 24MAY24

#Purpose: fit prophet model for chla from 2018-2021

library(prophet)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_Prophet <- function(data, cal_dates, include_drivers = TRUE){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    rename(ds = datetime,
           y = Chla_ugL_mean)
  
  #fit prophet model 
  my.prophet <- prophet(df, fit = FALSE) 
  
  if(include_drivers == TRUE){
    
    # initialize model
    my.init.prophet <- prophet(df, fit = FALSE) %>%
      add_regressor(name = "AirTemp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "PAR_umolm2s_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "WindSpeed_ms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "Flow_cms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "Temp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "SRP_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "DIN_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
      add_regressor(name = "LightAttenuation_Kd", prior.scale = NULL, standardize = "auto", mode = "additive") 
    
    # fit model
    my.prophet <- fit.prophet(m = my.init.prophet, df = df)
    
    # initialize prediction dataframe
    future_data <- rename(.data = data, ds = datetime)
    future <- make_future_dataframe(my.prophet, periods = 365) %>%
      left_join(future_data, by = "ds") %>%
      select(ds, AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean,
             Temp_C_mean, SRP_ugL, DIN_ugL, LightAttenuation_Kd)
    
  } else {
    
    # initialize model
    my.init.prophet <- prophet(df, fit = FALSE) 
    
    # fit model
    my.prophet <- fit.prophet(m = my.init.prophet, df = df)
    
    # initialize prediction dataframe
    future <- make_future_dataframe(my.prophet, periods = 365) 
  }
  
  
  forecast <- predict(my.prophet, future) %>%
    filter(ds <= "2021-12-31")
  
  prophet_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = ds, y = y, fill = "obs"))+
    geom_line(data = forecast, aes(x = as.Date(ds), y = yhat, color = "prophet"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "Prophet",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = forecast$yhat)
  
  if(include_drivers == FALSE){
    df.out <- df.out %>%
      mutate(model_id = "ProphetnoDrivers")
  }

  
  #return output + model with best fit + plot
  return(list(out = df.out, prophet = my.prophet, plot = prophet_plot))
}
