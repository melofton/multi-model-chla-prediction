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

fit_MARS <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)# %>%
    #mutate_at(vars, scale2)
  
  #fit MARS model from earth package
  earth.mod <- earth(Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL, data = df)
  plotmo(earth.mod)
  pred <- data.frame(earth.mod$fitted.values) %>%
    add_column(datetime = df$datetime)
  
  MARS_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = pred, aes(x = datetime, y = Chla_ugL_mean, color = "MARS"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "MARS",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = pred$Chla_ugL_mean)

  
  #return output + model with best fit + plot
  return(list(out = df.out, MARS = earth.mod, plot = MARS_plot))
}
