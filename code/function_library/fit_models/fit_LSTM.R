#Predict using LSTM model for chl-a
#Author: Mary Lofton
#Date last updated: 14May24

#Purpose: make predictions using DOY model for chla

# source custom functions
library(reticulate)
source_python("./code/model_files/LSTM/utils.py")


#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions
#'@param input_window integer value of number of days for input window to LSTM
#'@param params_list list of vectors where each vector is a series of possible values for an LSTM
#' hyperparameter; currently tuned parameters are: epochs, dropout, num_layers, hidden_feature_size,
#' weight_decay

fit_LSTM <- function(data, cal_dates, forecast_horizon, input_window, params_list){
  
  #set up empty dataframe
  df.cols = c("model_id","datetime","variable","prediction","param_set","epochs","dropout",
              "num_layers","hidden_feature_size","weight_decay") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  end_cal_date = as.Date(cal_dates[2])
  
  #expand parameters grid
  params <- expand.grid(params_list)
  
  #define static parameters
  input_window = input_window
  output_window = forecast_horizon+1
  split_date = as.character(end_cal_date - (input_window + output_window))
  df_date = as.character(end_cal_date + (output_window-1))
  
  # build df (all for training except for last day which is for testing)
  df <- data %>%
    filter(datetime <= df_date)
  write.csv(df, "./code/model_files/LSTM/LSTM_dataset.csv", row.names = FALSE)
  
  for(p in 1:nrow(params)){
  
    # define parameters
    epochs = params$epochs[p]
    dropout = params$dropout[p]
    num_layers = params$num_layers[p]
    hidden_feature_size = params$hidden_feature_size[p]
    weight_decay = params$weight_decay[p]
    
    run_all(split_date, input_window, output_window, epochs, dropout, num_layers, hidden_feature_size, weight_decay)
    
    pred <- read_csv("./code/model_files/LSTM/LSTM_training_output.csv") %>%
      add_column(horizon = c(0:forecast_horizon)) %>%
      pivot_longer(cols = c(-horizon),names_to = "datetime", values_to = "prediction") %>%
      mutate(datetime = as.Date(datetime))
    
    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "LSTM",
                          datetime = pred$datetime,
                          variable = "chlorophyll-a",
                          horizon = pred$horizon,
                          prediction = pred$prediction,
                          param_set = p,
                          epochs = epochs,
                          dropout = dropout,
                          num_layers = num_layers,
                          hidden_feature_size = hidden_feature_size,
                          weight_decay = weight_decay)
    
  } #end of all hyperparameter tuning loop
  
  write.csv(temp.df, paste0("./model_output/LSTM/param_set_",p,".csv"), row.names = FALSE)
}
