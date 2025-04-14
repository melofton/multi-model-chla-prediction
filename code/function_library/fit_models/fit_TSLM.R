#Fit TSLM model for chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: fit TSLM model for chla from 2018-2021

library(fable)
library(feasts)

#'Function to fit TSLM model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period
#'

fit_TSLM <- function(data, cal_dates){
  
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
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
    slice(-1)
    
  #fit TSLM from fable package
  my.tslm <- df %>%
    model(tslm.no.lag = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL)) 
  
  my.tslm.w.lag <- df %>%
    model(tslm = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL + lag_Chla_ugL_mean)) 
  
  my.tslm.w.trend <- df %>%
    model(tslm.w.trend = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL + lag_Chla_ugL_mean + trend())) 
  
  my.tslm.lag.only <- df %>%
    model(tslm.lag.only = fable::TSLM(formula = Chla_ugL_mean ~ lag_Chla_ugL_mean)) 
  
  diagnostics_no_lag <- gg_tsresiduals(my.tslm)
  diagnostics_lag <- gg_tsresiduals(my.tslm.w.lag)
  diagnostics_no_drivers <- gg_tsresiduals(my.tslm.lag.only)
  
  t1 <- glance(my.tslm)
  t2 <- glance(my.tslm.w.lag)
  t3 <- glance(my.tslm.lag.only)
  stat <- bind_rows(t1,t2,t3)
  
  fitted_values_tslm <- fitted(my.tslm.w.lag)
  fitted_values_tslm_no_lag <- fitted(my.tslm)
  fitted_values_tslm_no_drivers <- fitted(my.tslm.lag.only)

  TSLM_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values_tslm_no_lag, aes(x = datetime, y = .fitted, color = "TSLM (no lag)"))+
    geom_line(data = fitted_values_tslm_no_drivers, aes(x = datetime, y = .fitted, color = "TSLM (no drivers)"))+
    geom_line(data = fitted_values_tslm, aes(x = datetime, y = .fitted, color = "TSLM"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "TSLM",
                       datetime = dates$datetime[-1],
                       variable = "chlorophyll-a",
                       prediction = fitted_values_tslm$.fitted)

  
  #return output + model with best fit + plot
  return(list(out = df.out, TSLM = my.tslm.w.lag, plot = TSLM_plot, stats = stat, diagnostics = diagnostics_lag, diagnostics_no_lag = diagnostics_no_lag, diagnostics_no_drivers = diagnostics_no_drivers))
}
