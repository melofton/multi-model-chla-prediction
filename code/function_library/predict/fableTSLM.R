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

fableTSLM <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  
  # #define scaling function
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  # 
  # #define vars
  # vars <- c("AirTemp_C","Shortwave_Wm2","Windspeed_ms","Inflow_cms", "WaterTemp_C" ,"LightAttenuation_Kd", "DIN_ugL", "SRP_ugL")
  # 
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(datetime < pred_dates[1]) #%>%
    #mutate_at(vars, scale2) 
  
  #fit TSLM from fable package
  my.tslm <- df %>%
    model(tslm = fable::TSLM(formula = Chla_ugL_mean ~ AirTemp_C_mean + PAR_umolm2s_mean + WindSpeed_ms_mean + Flow_cms_mean + Temp_C_mean + LightAttenuation_Kd + DIN_ugL + SRP_ugL))
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build driver dataset
    drivers = as_tsibble(data) %>%
      filter(datetime %in% forecast_dates) #%>%
      #mutate_at(vars, scale2) 
    drivers[,"Chla_ugL_mean"] <- NA
    
    #refit model
    new.data <- as_tsibble(data) %>%
      filter(datetime < pred_dates[t])
    ref <- refit(my.tslm, new_data = new.data)
    
    #generate predictions
    pred <- forecast(ref, new_data = drivers)

    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "TSLM",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = forecast_dates,
                          variable = "chlorophyll-a",
                          prediction = pred$.mean)
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
