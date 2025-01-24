#Fit NNETAR model for chl-a
#Author: Mary Lofton
#Date: 15APR24

#Purpose: fit ARIMA model for chla from 2018-2021

library(fable)
library(moments)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_NNETAR_KGML2 <- function(data, cal_dates, target){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  # #define scaling function
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  # 
  # #define vars
  # vars <- c("AirTemp_C","Shortwave_Wm2","Windspeed_ms","Inflow_cms", "WaterTemp_C" ,"LightAttenuation_Kd", "DIN_ugL", "SRP_ugL")
  # 
  #assign target and predictors
  df <- data %>%
    select(-datetime) %>%
    filter(reference_datetime >= start_cal & reference_datetime <= stop_cal & !horizon == 0) %>%
    as_tsibble(., index = reference_datetime, key = horizon)
  
  # fit NNETARs from fable package
  if(target == "residuals"){
  my.nnetar <- df %>%
    model(`KGML NNETAR2` = fable::NNETAR(formula = residuals ~ AirTemp + ShortWave + LongWave + RelHum + 
                                          WindSpeed + Rain +
                                          NIT_amm + NIT_nit + PHS_frp + OGM_doc + prediction,
                                              n_networks = 20)) 
  }
  if(target == "observations"){
  my.nnetar <- df %>%
    model(`KGML NNETAR2` = fable::NNETAR(formula = Chla_ugL_mean ~ AirTemp + ShortWave + LongWave + RelHum + 
                                           WindSpeed + Rain +
                                           NIT_amm + NIT_nit + PHS_frp + OGM_doc + prediction,
                                         n_networks = 20))
  }
  
  # get fitted values
  fitted_values <- fitted(my.nnetar)
  
  if(target == "residuals"){
  KGML_NNETAR_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = reference_datetime, y = residuals, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = reference_datetime, y = .fitted, group = .model, color = .model))+
    labs(color = NULL, fill = NULL)+
    theme_classic()
  }
  if(target == "observations"){
    KGML_NNETAR_plot <- ggplot()+
      xlab("")+
      ylab("Chla (ug/L)")+
      geom_point(data = df, aes(x = reference_datetime, y = Chla_ugL_mean, fill = "obs"))+
      geom_line(data = fitted_values, aes(x = reference_datetime, y = .fitted, group = .model, color = .model))+
      labs(color = NULL, fill = NULL)+
      theme_classic()
  }
  
  #return output + model with best fit + plot
  return(list(KGML_NNETAR = my.nnetar, plot = KGML_NNETAR_plot))
}
