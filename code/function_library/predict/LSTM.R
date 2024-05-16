#Predict using LSTM model for chl-a
#Author: Mary Lofton
#Date last updated: 14May24

#Purpose: make predictions using DOY model for chla
library(reticulate)

# source custom functions
source_python("./code/model_files/LSTM/utils.py")


#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

LSTM <- function(data, pred_dates, forecast_horizon){
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    input_window = 42
    output_window = forecast_horizon+1
    split_date = as.character(pred_dates[t] - (input_window + output_window))
    df_date = as.character(pred_dates[t] + (output_window-1))
    
    # build df (all for training except for last day which is for testing)
    df <- data %>%
      filter(datetime <= df_date)
    write.csv(df, "./code/model_files/LSTM/LSTM_dataset.csv", row.names = FALSE)
  
    run_all(split_date, input_window, output_window)
    
    pred <- read_csv("./code/model_files/LSTM/LSTM_testing_output.csv") %>%
      pull(as.character(pred_dates[t]))
    
    #set up dataframe for today's prediction
    curr_chla <- data %>%
      filter(datetime == pred_dates[t]) %>%
      pull(Chla_ugL_mean)
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    temp.df <- data.frame(model_id = "LSTM",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred[-1]))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
