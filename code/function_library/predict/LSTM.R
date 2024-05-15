#Predict using LSTM model for chl-a
#Author: Mary Lofton
#Date last updated: 14May24

#Purpose: make predictions using DOY model for chla

library(fable)
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
    
    # build df (all for training except for last day which is for testing)
    df <- data %>%
      filter(datetime <= pred_dates[t])
    write.csv(df, "./code/model_files/LSTM/LSTM_dataset.csv", row.names = FALSE)
    
    # calculate split ratio
    split_ratio = 1- 1/length(unlist(df$datetime))
    
    # fit model
    params = list(epochs = 100,
                  input_window = 14,
                  output_window = 7,
                  weight_decay = 0.05,
                  split_ratio = split_ratio)
    params = {
      'epochs': 100,
      'input_window': 14,
      'output_window': 7,
      'weight_decay': 0.05,
      'split_ratio': split_ratio
    }
    run_all(params = params)
    
    # generate predictions on training dataset

    #set up dataframe for today's prediction
    curr_chla <- data %>%
      filter(datetime == pred_dates[t]) %>%
      pull(Chla_ugL_mean)
    temp.df <- data.frame(model_id = "LSTM",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$.mean))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
