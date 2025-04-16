#Fit GAM model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit GAM model for chla from 2018-2021

library(mgcv)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period
#'

fit_GAM <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    mutate(doy = yday(datetime)) %>%
    mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
    slice(-1)
  
  #fit GAM following methods in ggplot()
    #fit GAM following methods in ggplot()
    my.gam1 <- mgcv::gam(formula = Chla_ugL_mean ~ s(lag_Chla_ugL_mean, bs = "cs") +
                          s(AirTemp_C_mean, bs = "cs") +
                          s(PAR_umolm2s_mean, bs = "cs") +
                          s(WindSpeed_ms_mean, bs = "cs") +
                          s(Flow_cms_mean, bs = "cs") +
                          s(Temp_C_mean, bs = "cs") +
                          s(SRP_ugL, bs = "cs") +
                          s(DIN_ugL, bs = "cs") +
                          s(LightAttenuation_Kd, bs = "cs"), family = gaussian(),
                        data = df, method = "REML")
    #fit GAM following methods in ggplot()
    my.gam2 <- mgcv::gam(formula = Chla_ugL_mean ~ s(lag_Chla_ugL_mean, bs = "cs"), family = gaussian(),
                        data = df, method = "REML")
    #fit GAM following methods in ggplot()
    my.gam3 <- mgcv::gam(formula = Chla_ugL_mean ~ s(AirTemp_C_mean, bs = "cs") +
                          s(PAR_umolm2s_mean, bs = "cs") +
                          s(WindSpeed_ms_mean, bs = "cs") +
                          s(Flow_cms_mean, bs = "cs") +
                          s(Temp_C_mean, bs = "cs") +
                          s(SRP_ugL, bs = "cs") +
                          s(DIN_ugL, bs = "cs") +
                          s(LightAttenuation_Kd, bs = "cs"), family = gaussian(),
                        data = df, method = "REML")
  
  png("./figures/GAM_diagnostics.png", res = 300,
      width = 8, height = 6, units = "in")
  par(mfrow=c(2,2))
  gam.check(my.gam1)
  dev.off()
  
  png("./figures/GAM_noDrivers_diagnostics.png", res = 300,
      width = 8, height = 6, units = "in")
  par(mfrow=c(2,2))
  gam.check(my.gam2)
  dev.off()
  
  png("./figures/GAM_noLag_diagnostics.png", res = 300,
      width = 8, height = 6, units = "in")
  par(mfrow=c(2,2))
  gam.check(my.gam3)
  dev.off()
  
  GAM_predicted <- mgcv::predict.gam(my.gam1)
  GAM_predicted_noDrivers <- mgcv::predict.gam(my.gam2)
  GAM_predicted_noLag <- mgcv::predict.gam(my.gam3)
  
  fitted_values <- tibble(datetime = df$datetime,
                              `GAM` = GAM_predicted,
                              `GAM (no drivers)` = GAM_predicted_noDrivers,
                              `GAM (no lag)` = GAM_predicted_noLag)
  
  GAM_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = datetime, y = `GAM (no lag)`, color = "GAM (no lag)"))+
    geom_line(data = fitted_values, aes(x = datetime, y = `GAM (no drivers)`, color = "GAM (no drivers)"))+
    geom_line(data = fitted_values, aes(x = datetime, y = `GAM`, color = "GAM"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "GAM",
                       datetime = df$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$GAM)

  #return output + model with best fit + plot
  return(list(out = df.out, plot = GAM_plot))
}
