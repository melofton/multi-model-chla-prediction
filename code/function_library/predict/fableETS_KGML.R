#Predict using ETS model for chl-a
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: make predictions using DOY model for chla

# THIS SCRIPT COMMTITED AMIDST REVISIONS, NOT RUNNABLE, NEEDS TO BE CHECKED

library(fable)
library(ncdf4)

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

fableETS_KGML <- function(previous_residuals, process_model_predictions, 
                          observations, pred_dates, forecast_horizon,
                          spinup_nc_filepath){
  
  #Fit model
  #assign target and predictors
  df <- as_tsibble(previous_residuals) %>%
    filter(datetime < pred_dates[1]) 
  
  #assign observations
  obs <- KGML_obs
  
  #fit ETS from fable package
  my.ets <- df %>%
    model(ets = fable::ETS(formula = Chla_residuals_ugL))
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #if t==1, pull chla pred from spinup nc
    if(t == 1){
      
      spinup_nc <- ncdf4::nc_open(spinup_nc_filepath)
      
      all_elevs <- ncdf4::ncvar_get(spinup_nc, var = "H")
      current_elevs <- all_elevs[which(!is.na(all_elevs[,ncol(all_elevs)])),ncol(all_elevs)]
      the_elevs <- current_elevs[!is.na(current_elevs)]
      
      ic_col <- ncol(all_elevs)
      
      lake_depth <- max(the_elevs)
      the_depths <- rev(lake_depth - the_elevs)
      
      all_chl <- ncdf4::ncvar_get(spinup_nc, var = "PHY_tchla")
      chl <- rev(all_chl[which(!is.na(all_chl[,ic_col])),ic_col])

      EXO_depth <- which.min(abs(the_depths - 1.6))
      model_chla <- chl[EXO_depth]
      
    } else {
      
      #otherwise, pull chla pred from GLM-AED prediction with 
      #reference datetime == pred_dates[t-1]
      model_chla <- process_model_predictions %>%
        filter(reference_datetime == pred_dates[t-1] & datetime == pred_dates[t]) %>%
        pull(prediction)
    }
    
    #use model_chla and observation to calculate residual for pred_dates[t]
    current_obs <- KGML_obs %>%
      filter(datetime == pred_dates[t]) %>%
      pull(Chla_ugL_mean)
    
    current_residual <- model_chla - current_obs
    
    #append residual to ETS model training data
    new_residual_df <- data.frame(datetime = pred_dates[t],
                                  GLMAED_Chla_ugL = model_chla,
                                  Chla_ugL_mean = current_obs,
                                  Chla_residuals_ugL = current_residual)
    
    #refit model
    new.data <- bind_rows(df, new_residual_df) %>%
      as_tsibble() 
    ref <- refit(my.ets, new_data = new.data)
    
    #make dataframe for predictions
    drivers <- data.frame(datetime = forecast_dates,
                            GLMAED_Chla_ugL = NA,
                            Chla_ugL_mean = NA,
                            Chla_residuals_ugL = NA) %>%
      as_tsibble()

    #generate predictions of residuals for 1-35 days ahead
    residual_pred <- forecast(ref, new_data = drivers)
    
    #pull GLM-AED predictions for reference_datetime == pred_dates[t]
    current_model_pred <- process_model_predictions %>%
      filter(reference_datetime == pred_dates[t]) %>%
      slice(-1) %>%
      pull(prediction)
    
    #correct GLM-AED predictions using new residual predictions
    final_pred <- current_model_pred + residual_pred$.mean

    #format corrected predictions for output
    temp.df <- data.frame(model_id = "ETS_KGML",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(current_obs,final_pred))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
