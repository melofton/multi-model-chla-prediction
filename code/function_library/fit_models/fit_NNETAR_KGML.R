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

fit_NNETAR_KGML <- function(data, cal_dates){
  
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
  df <- as_tsibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal)# %>%
    #mutate_at(vars, scale2)
  
  # fit NNETARs from fable package
  my.nnetar <- df %>%
    model(`KGML NNETAR` = fable::NNETAR(formula = Chla_residuals_ugL ~ AirTemp + ShortWave + LongWave + RelHum + 
                                          WindSpeed + Rain +
                                          NIT_amm + NIT_nit + PHS_frp + OGM_doc + GLMAED_Chla_ugL,
                                              n_networks = 20)) 
  
  # get fitted values
  fitted_values <- fitted(my.nnetar)
  
  KGML_NNETAR_plot <- ggplot()+
    xlab("")+
    ylab("Chla residuals (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_residuals_ugL, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  #build output df
  df.out <- data.frame(model_id = "KGML_NNETAR",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)
  
  #return output + model with best fit + plot
  return(list(out = df.out, KGML_NNETAR = my.nnetar, plot = KGML_NNETAR_plot))
}
