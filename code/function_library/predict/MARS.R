#Predict using ARIMA model for chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: make predictions using DOY model for chla

library(earth)

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

data <- read_csv("./data/data_processed/MARS.csv")
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")
forecast_horizon = 35

MARS <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  
  #assign target and predictors
  df <- data %>%
    filter(datetime < pred_dates[1]) #%>%
    #mutate_at(vars, scale2)
  
  #fit ARIMA from fable package
  earth.mod <- earth(Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL, data = df)
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #message
    message(pred_dates[t])
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build driver dataset
    drivers = data %>%
      filter(datetime %in% forecast_dates) 
    drivers[,"Chla_ugL_mean"] <- NA
    
    #refit model
    new.data <- data %>%
      filter(datetime <= pred_dates[t]) #%>%
      #mutate_at(vars, scale2)
    ref <- earth(Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL, data = df)
    
    #generate predictions
    pred <- predict(object = ref, newdata = drivers)

    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(datetime == pred_dates[t]) %>%
      select(Chla_ugL_mean)
    curr_chla <- curr_chla_df$Chla_ugL_mean[1]
    temp.df <- data.frame(model_id = "MARS",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
