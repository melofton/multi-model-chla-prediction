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

fit_NNETAR <- function(data, cal_dates, include_drivers = TRUE){
  
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
  
  #fit NNETARs from fable package
  my.nnetars <- df %>%
    model(`Chla only NNETAR` = fable::NNETAR(Chla_ugL_mean, n_networks = 20),
          `NNETAR w/ drivers` = fable::NNETAR(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL,
                                              n_networks = 20)) 
  
  # get model parameters
  glance(my.nnetars)
  residuals(my.nnetars %>% select(`Chla only NNETAR`))
  params_drivers <- coefficients(my.arimas %>% select(`NNETAR w/ drivers`))
  model_params <- bind_rows(params_chla_only, params_drivers) %>%
    mutate(across(.cols = -c(.model, term),
                  .fns  = ~ round(., 2)))
  
  # plot model diagnostics
  diagnostics_chla_only <- gg_tsresiduals(my.arimas %>% select(`Chla only NNETAR`))
  diagnostics_drivers <- gg_tsresiduals(my.arimas %>% select(`NNETAR w/ drivers`))
  
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
  df.out <- data.frame(model_id = "NNETAR",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)
  
  if(include_drivers == FALSE){
    df.out <- df.out %>%
      mutate(model_id = "NNETARnoDrivers")
  }

  
  #return output + model with best fit + plot
  return(list(out = df.out, NNETAR = my.nnar, plot = NNETAR_plot))
}
