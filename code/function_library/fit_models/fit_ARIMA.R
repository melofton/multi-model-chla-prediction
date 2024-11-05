#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit ARIMA model for chla from 2018-2021

library(fable)
library(moments)
library(feasts)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_ARIMAs <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal)# %>%
    #mutate_at(vars, scale2)
  
  #fit ARIMAs from fable package
  my.arimas <- df %>%
    model(`Chla ARIMA` = fable::ARIMA(Chla_ugL_mean),
          `Reg. w/ ARIMA errors` = fable::ARIMA(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL)) 
  
  # get model parameters
  params_chla_only <- coefficients(my.arimas %>% select(`Chla ARIMA`))
  params_drivers <- coefficients(my.arimas %>% select(`Reg. w/ ARIMA errors`))
  model_params <- bind_rows(params_chla_only, params_drivers) %>%
    mutate(across(.cols = -c(.model, term),
                  .fns  = ~ round(., 2)))
  
  # plot model diagnostics
  diagnostics_chla_only <- gg_tsresiduals(my.arimas %>% select(`Chla ARIMA`))
  diagnostics_drivers <- gg_tsresiduals(my.arimas %>% select(`Reg. w/ ARIMA errors`))
  
  fitted_values <- fitted(my.arimas)
  
  ARIMA_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = datetime, y = .fitted, group = .model, color = .model))+
    facet_wrap(facets = vars(.model))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = rep(c("ARIMAnoDrivers","ARIMA"), each = length(dates$datetime)),
                       datetime = rep(dates$datetime,2),
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)

  #return output + model with best fit + plot
  return(list(out = df.out, ARIMAs = my.arimas, plot = ARIMA_plot,
              model_params = model_params, diagnostics_chla_only = diagnostics_chla_only,
              diagnostics_drivers = diagnostics_drivers))
}
