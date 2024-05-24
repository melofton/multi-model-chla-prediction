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

fit_LSTM <- function(data, cal_dates, forecast_horizon){
  
  #set up empty dataframe
  df.cols = c("model_id","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  pred_dates = as.Date(cal_dates[2])
  
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
    
    pred <- read_csv("./code/model_files/LSTM/LSTM_training_output.csv")[1,] %>%
      pivot_longer(cols = everything(),names_to = "datetime", values_to = "prediction") %>%
      mutate(datetime = as.Date(datetime))
    
    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "LSTM",
                          datetime = pred$datetime,
                          variable = "chlorophyll-a",
                          prediction = pred$prediction)
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  LSTM_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = pred.df, aes(x = datetime, y = prediction, color = "LSTM"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()
  
  #return predictions
  return(list(out = pred.df, plot = LSTM_plot))
}
