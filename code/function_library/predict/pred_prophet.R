#Predict prophet model for chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: make predictions using prophet model for chla

library(prophet)

#'Function to predict chl-a using prophet model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

pred_Prophet <- function(data, pred_dates, forecast_horizon, include_drivers = TRUE){
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build refit dataset
    refit <- data %>%
      filter(datetime <= pred_dates[t]) %>%
      rename(ds = datetime,
             y = Chla_ugL_mean)
    
    #fit prophet model 
    if(include_drivers == TRUE){
      
      # initialize model
      my.init.prophet <- prophet(refit, fit = FALSE) %>%
        add_regressor(name = "AirTemp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "PAR_umolm2s_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "WindSpeed_ms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "Flow_cms_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "Temp_C_mean", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "SRP_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "DIN_ugL", prior.scale = NULL, standardize = "auto", mode = "additive") %>%
        add_regressor(name = "LightAttenuation_Kd", prior.scale = NULL, standardize = "auto", mode = "additive") 
      
      # fit model
      my.prophet <- fit.prophet(m = my.init.prophet, df = refit)
      
      # initialize prediction dataframe
      future_data <- rename(.data = data, ds = datetime)
      future <- make_future_dataframe(my.prophet, periods = forecast_horizon) %>%
        left_join(future_data, by = "ds") %>%
        select(ds, AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean,
               Temp_C_mean, SRP_ugL, DIN_ugL, LightAttenuation_Kd)
      
    } else {
      
      # initialize model
      my.init.prophet <- prophet(refit, fit = FALSE) 
      
      # fit model
      my.prophet <- fit.prophet(m = my.init.prophet, df = refit)
      
      # initialize prediction dataframe
      future <- make_future_dataframe(my.prophet, periods = forecast_horizon) 
    }
    
    #generate predictions
    pred <- predict(my.prophet, future) %>%
      mutate(ds = as.Date(ds)) %>%
      filter(ds %in% forecast_dates)

    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(datetime == pred_dates[t]) %>%
      select(Chla_ugL_mean)
    
    curr_chla <- curr_chla_df$Chla_ugL_mean[1]
    
    temp.df <- data.frame(model_id = "Prophet",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$yhat))
    
    if(include_drivers == FALSE){
      temp.df <- temp.df %>%
        mutate(model_id = "ProphetnoDrivers")
    }
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
    #send update to user
    message(pred_dates[t])
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
