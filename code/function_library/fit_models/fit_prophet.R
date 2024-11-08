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

fit_Prophets <- function(data, cal_dates, include_drivers = TRUE){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    rename(ds = datetime,
           y = Chla_ugL_mean)
  
  #fit prophet models
  
  # CHLA ONLY
  # initialize model
  my.init.prophet <- prophet(df, 
                             growth = "linear",
                             fit = FALSE,
                             yearly.seasonality = TRUE,
                             weekly.seasonality = TRUE,
                             daily.seasonality = FALSE,
                             seasonality.mode = "additive",
                             mcmc.samples = 0,
                             uncertainty.samples = 0) 
  
  # fit model
  my.prophet <- fit.prophet(m = my.init.prophet, df = df)
  
  # initialize prediction dataframe
  future <- make_future_dataframe(my.prophet, periods = 365) 

  # WITH DRIVERS
  # initialize model
  my.init.prophet.w.drivers <- prophet(df, 
                                       growth = "linear",
                                       fit = FALSE,
                                       yearly.seasonality = TRUE,
                                       weekly.seasonality = TRUE,
                                       daily.seasonality = FALSE,
                                       seasonality.mode = "additive",
                                       mcmc.samples = 0,
                                       uncertainty.samples = 0) %>%
    add_regressor(name = "AirTemp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "PAR_umolm2s_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "WindSpeed_ms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "Flow_cms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "Temp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "SRP_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "DIN_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
    add_regressor(name = "LightAttenuation_Kd", prior.scale = NULL, standardize = "auto", mode = "additive") 
    
  # fit model
  my.prophet.w.drivers <- fit.prophet(m = my.init.prophet.w.drivers, df = df)
    
  # initialize prediction dataframe
  future_data_w_drivers <- rename(.data = data, ds = datetime)
  future_w_drivers <- make_future_dataframe(my.prophet.w.drivers, periods = 365) %>%
    left_join(future_data_w_drivers, by = "ds") %>%
    select(ds, AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean,
            Temp_C_mean, SRP_ugL, DIN_ugL, LightAttenuation_Kd)
  
  # cross-validation
  cross_val <- cross_validation(my.prophet, horizon = 30, units = 'days')
  cross_val_w_drivers <- cross_validation(my.prophet.w.drivers, horizon = 30, units = 'days')
  
  # calculate performance metrics
  perf_metrics <- performance_metrics(cross_val, rolling_window = 0) %>%
    add_column(model_id = "Chla only Prophet")
  perf_metrics_w_drivers <- performance_metrics(cross_val_w_drivers, rolling_window = 0) %>%
    add_column(model_id = "Prophet w/ drivers")
  
  all_perf_metrics <- bind_rows(perf_metrics, perf_metrics_w_drivers)
  
  # plot rmse
  rmse_plot <- ggplot(data = all_perf_metrics, aes(x = horizon, y = rmse, 
                                                   group = model_id, color = model_id))+
    geom_line()+
    labs(color = "Model ID")+
    theme_bw()
  
  # generate forecasts
  forecast <- predict(my.prophet, future) %>%
    filter(ds <= "2021-12-31") %>%
    add_column(model_id = "Chla only Prophet")
  forecast_w_drivers <- predict(my.prophet.w.drivers, future_w_drivers) %>%
    filter(ds <= "2021-12-31") %>%
    add_column(model_id = "Prophet w/ drivers")
  
  # plot components
  prophet_components <- prophet_plot_components(my.prophet, forecast)
  prophet_components_w_drivers <- prophet_plot_components(my.prophet.w.drivers, forecast_w_drivers)
  
  # look at regressor coefficients
  reg_coeffs <- regressor_coefficients(my.prophet.w.drivers) %>%
    mutate(across(.cols = -c(regressor, regressor_mode),
                  .fns  = ~ round(., 2)))
  
  all_forecasts <- bind_rows(forecast, forecast_w_drivers)
  
  # plot fits to data
  prophet_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = ds, y = y, fill = "obs"))+
    geom_line(data = all_forecasts, aes(x = as.Date(ds), y = yhat, 
                                        group = model_id, color = model_id))+
    facet_wrap(facets = vars(model_id))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(datetime >= start_cal & datetime <= stop_cal)
  
  #build output df
  df.out <- data.frame(rep(c("ProphetnoDrivers","Prophet"), each = length(dates$datetime)),
                       datetime = rep(dates$datetime,2),
                       variable = "chlorophyll-a",
                       prediction = all_forecasts$yhat)

  #return output + model with best fit + plot
  return(list(out = df.out, plot = prophet_plot, rmse_plot = rmse_plot,
              reg_coeffs = reg_coeffs, prophet_components = prophet_components,
              prophet_components_w_drivers = prophet_components_w_drivers))
}
