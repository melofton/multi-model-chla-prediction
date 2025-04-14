#Fit MARS model for chl-a
#Author: Mary Lofton
#Date: 09SEP24

#Purpose: fit MARS model for chla from 2018-2021

library(earth)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period
#'

fit_MARS <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)%>%
    mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
    slice(-1)
  
  #fit MARS model from earth package
  earth.mod <- earth(Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL + lag_Chla_ugL_mean, data = df)
  earth.mod.no.lag <- earth(Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL, data = df)
  earth.mod.no.drivers <- earth(Chla_ugL_mean ~ lag_Chla_ugL_mean, data = df)
  
  basis.matrix <- model.matrix(earth.mod)
  basis.matrix.no.lag <- model.matrix(earth.mod.no.lag)
  basis.matrix.no.drivers <- model.matrix(earth.mod.no.drivers)
  
  basis.functions <- data.frame(basis.functions = colnames(basis.matrix)) %>%
    slice(-1)
  basis.functions.no.lag <- data.frame(basis.functions = colnames(basis.matrix.no.lag)) %>%
    slice(-1)
  basis.functions.no.drivers <- data.frame(basis.functions = colnames(basis.matrix.no.drivers)) %>%
    slice(-1)
  
  pred <- data.frame(earth.mod$fitted.values) %>%
    add_column(datetime = df$datetime)
  pred_no_lag <- data.frame(earth.mod.no.lag$fitted.values) %>%
    add_column(datetime = df$datetime)
  pred_no_drivers <- data.frame(earth.mod.no.drivers$fitted.values) %>%
    add_column(datetime = df$datetime)
  
  MARS_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = pred_no_lag, aes(x = datetime, y = Chla_ugL_mean, color = "MARS (no lag)"))+
    geom_line(data = pred_no_drivers, aes(x = datetime, y = Chla_ugL_mean, color = "MARS (no drivers)"))+
    geom_line(data = pred, aes(x = datetime, y = Chla_ugL_mean, color = "MARS"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    slice(-1)
  
  #build output df
  df.out <- data.frame(model_id = "MARS",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = pred$Chla_ugL_mean)

  
  #return output + model with best fit + plot
  return(list(out = df.out, MARS = earth.mod, MARS_no_lag = earth.mod.no.lag,
              MARS_no_drivers = earth.mod.no.drivers, plot = MARS_plot, 
              basis.functions = basis.functions, mod.surface.plot = mod.surface.plot,
              basis.functions.no.lag = basis.functions.no.lag,
              basis.functions.no.drivers = basis.functions.no.drivers))
}
