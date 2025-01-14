#Make predictions with DOY model for chl-a
#Author: Mary Lofton
#Date last updated: 28FEB23

#Purpose: make predictions using DOY model for chla

library(mgcv)

#'Function to predict chl-a using DOY model
#'@param data data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

GAM <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  #assign target and predictors
  df <- data %>%
    filter(!datetime %in% pred_dates) %>%
    mutate(doy = yday(datetime)) %>%
    mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
    slice(-1)
  
  #fit GAM following methods in ggplot()
  my.gam <- mgcv::gam(formula = Chla_ugL_mean ~ s(lag_Chla_ugL_mean, bs = "cs") +
                        s(AirTemp_C_mean, bs = "cs") +
                        s(PAR_umolm2s_mean, bs = "cs") +
                        s(WindSpeed_ms_mean, bs = "cs") +
                        s(Flow_cms_mean, bs = "cs") +
                        s(Temp_C_mean, bs = "cs") +
                        s(SRP_ugL, bs = "cs") +
                        s(DIN_ugL, bs = "cs") +
                        s(LightAttenuation_Kd, bs = "cs"), family = gaussian(),
                      data = df, method = "REML")
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #message
    message(pred_dates[t])
    
    #assign target and predictors
    refit_df <- data %>%
      filter(datetime <= pred_dates[t]) %>%
      mutate(doy = yday(datetime)) %>%
      mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, n = 1)) %>%
      slice(-1)
    
    #fit GAM following methods in ggplot()
    my.gam <- mgcv::gam(formula = Chla_ugL_mean ~ s(lag_Chla_ugL_mean, bs = "cs") +
                          s(AirTemp_C_mean, bs = "cs") +
                          s(PAR_umolm2s_mean, bs = "cs") +
                          s(WindSpeed_ms_mean, bs = "cs") +
                          s(Flow_cms_mean, bs = "cs") +
                          s(Temp_C_mean, bs = "cs") +
                          s(SRP_ugL, bs = "cs") +
                          s(DIN_ugL, bs = "cs") +
                          s(LightAttenuation_Kd, bs = "cs"), family = gaussian(),
                        data = refit_df, method = "REML")
    
    #build driver dataset
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    drivers = data %>%
      mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, 1)) %>%
      mutate(doy = yday(datetime)) %>%
      filter(datetime %in% forecast_dates) %>%
      select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean, lag_Chla_ugL_mean)  
    
    drivers[,"Chla_ugL_mean"] <- NA
    
    for(h in 1:forecast_horizon){
      #generate predictions
      temp_pred <- predict.gam(my.gam, data.frame(drivers[h,]))
      if(h == 1){
        pred = temp_pred
      } else {
        pred = c(pred, temp_pred)
      }
      if(h < 35){
        drivers$lag_Chla_ugL_mean[h+1] <- temp_pred
      }
    }
    
    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(datetime == pred_dates[t]) %>%
      select(Chla_ugL_mean)
    curr_chla <- curr_chla_df$Chla_ugL_mean[1]
    temp.df <- data.frame(model_id = "GAM",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = forecast_dates,
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred))

    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
