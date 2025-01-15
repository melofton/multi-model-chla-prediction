#Fit TSLM model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: make predictions using TSLM model for chla

library(fable)

#'Function to predict chl-a using TSLM model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_TSLM() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

fableTSLM <- function(data, pred_dates, forecast_horizon, include_drivers,
                      include_lag){
  
  #Fit model
  
  # #define scaling function
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  # 
  # #define vars
  # vars <- c("AirTemp_C","Shortwave_Wm2","Windspeed_ms","Inflow_cms", "WaterTemp_C" ,"LightAttenuation_Kd", "DIN_ugL", "SRP_ugL")
  # 
  #assign target and predictors
  df <- as_tsibble(data, index = datetime) %>%
    mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
    filter(datetime < pred_dates[1]) %>%
    slice(-1)
  
  #fit TSLM from fable package
  if(include_drivers == TRUE & include_lag == TRUE){
  my.tslm <- df %>%
    model(tslm = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL + lag_Chla_ugL_mean))
  } 
  if (include_drivers == FALSE){
  my.tslm <- df %>%
    model(tslm = fable::TSLM(formula = Chla_ugL_mean ~ lag_Chla_ugL_mean))
  }
  if (include_lag == FALSE){
    my.tslm <- df %>%
      model(tslm = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL))
  }
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #message
    message(pred_dates[t])
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build driver dataset
    drivers = as_tsibble(data) %>%
      mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
      filter(datetime %in% forecast_dates) %>%
      slice(-1)
    
    drivers[,"Chla_ugL_mean"] <- NA
    drivers[-1,"lag_Chla_ugL_mean"] <- NA
    
    #refit model
    new.data <- as_tsibble(data) %>%
      mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
      slice(-1) %>%
      filter(datetime <= pred_dates[t])
    ref <- refit(my.tslm, new_data = new.data)
    
    for(h in 1:forecast_horizon){
      #generate predictions
      temp_pred <- forecast(ref, new_data = drivers[h,])
      if(h == 1){
        pred = temp_pred
      } else {
        pred = bind_rows(pred, temp_pred)
      }
      if(h < forecast_horizon){
        drivers$lag_Chla_ugL_mean[h+1] <- temp_pred$.mean
      }
    }

    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(datetime == pred_dates[t]) %>%
      select(Chla_ugL_mean)
    curr_chla <- curr_chla_df$Chla_ugL_mean[1]
    temp.df <- data.frame(model_id = "TSLM",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = forecast_dates,
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$.mean))
    
    if(include_drivers == FALSE){
      temp.df <- temp.df %>%
        mutate(model_id = "TSLMnoDrivers")
    }
    if(include_lag == FALSE){
      temp.df <- temp.df %>%
        mutate(model_id = "TSLMnoLag")
    }
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
