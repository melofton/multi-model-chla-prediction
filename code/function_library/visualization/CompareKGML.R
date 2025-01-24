#RMSE vs horizon plot
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot a prediction from 1-7 days into future with all models plotted

library(tidyverse)
library(lubridate)

#'Function to fit day of year model for chla
#'@param observations data frame with columns:
#'Date: yyyy-mm-dd
#'Chla_ugL: observed daily median of chlorophyll-a from EXO in ug/L
#'@param model_output data frame with columns:
#'model_id: name of model (e.g., persistence)
#'reference_datetime: date prediction was issued (yyyy-mm-dd)
#'datetime: date of prediction (yyyy-mm-dd)
#'variable: predicted variable (chlorophyll-a)
#'prediction: value of prediction (ug/L)
#'@param reference_datetime date (yyyy-mm-dd) on which prediction you want to 
#'plot starts
#'@param forecast_horizon maximum horizon that you want to plot

CompareKGML <- function(observations, 
                          model_output, 
                          forecast_horizon,
                          model_ids = model_ids,
                          viz_dates = pred_dates,
                          plot_title = "All predictions",
                          viz_metric = "r2",
                          show_legend = FALSE){
  
  #reformat observations
  pred_dates <- data.frame(datetime = viz_dates) %>%
    left_join(., observations, by = "datetime") 
  
  rsq <- function(pred, obs){
    1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  }
  
    #reformat model output
    output <- model_output %>% 
      filter(model_id %in% model_ids & reference_datetime %in% viz_dates) %>%
      group_by(model_type, model_id, reference_datetime) %>%
      mutate(horizon = datetime - reference_datetime) %>%
      ungroup() %>%
      separate(horizon, c("horizon"), sep = " ") %>%
      left_join(., pred_dates, by = "datetime") %>%
      group_by(model_type, model_id, horizon) %>%
      summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE)),
                r2 = rsq(prediction, Chla_ugL_mean),
                bias = mean(prediction - Chla_ugL_mean, na.rm = TRUE)) %>%
      filter(!horizon == 0) %>%
      mutate(horizon = as.numeric(horizon)) %>%
      filter(horizon <= forecast_horizon) %>%
      arrange(model_type, model_id, horizon) %>%
      mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","TSLM (no drivers)","TSLM (no lag)","MARS","MARS (no drivers)","MARS (no lag)","randomForest","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:bias, names_to = "skill_metric", values_to = "skill_value")
  
  plot_data <- output %>%
    filter(skill_metric == viz_metric) 
  
  my.shapes <-             c("NNETAR" = 6,
                             "GLM-AED" = 7,
                             "NNETAR-KGML" = 12)
  my.cols <- c("process-based" = "#B85233",
               "data-driven" = "#6FA19D",
               "KGML" = "navy")
  
  p <- ggplot()+
    geom_point(data = plot_data, aes(x = horizon, y = skill_value, shape = model_id, color = model_type), size = 2)+
    xlab("Prediction horizon (days)")+
    ggtitle(plot_title)+
    scale_shape_manual(name = "Model ID", values = my.shapes)+
    scale_color_manual(name = "Model type", values = my.cols)+
    theme_classic()+
    theme(legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1))
  
  if(viz_metric == "rmse"){
    p <- p +
      ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))
  } else if(viz_metric == "r2"){
    p <- p +
      ylab(expression(paste(R^2)))
  } else {
    p <- p +
      ylab(expression(paste("mean bias (",mu,g,~L^-1,")")))
  }
  
  if(show_legend == FALSE){
    p <- p +
      theme(legend.position = "none")
  }
  
  return(p)
    
}
