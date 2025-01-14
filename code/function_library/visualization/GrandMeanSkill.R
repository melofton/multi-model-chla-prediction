#RMSE vs horizon plot
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot a prediction from 1-7 days into future with all models plotted

library(tidyverse)
library(lubridate)
library(ggpattern)

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

GrandMeanSkill <- function(observations, 
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
    filter(model_id %in% model_ids) %>%
    group_by(model_type, model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    left_join(., pred_dates, by = "datetime") %>%
    filter(horizon <= forecast_horizon & !horizon == 0) %>%
    group_by(model_type, model_id) %>%
    summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE)),
              r2 = rsq(prediction, Chla_ugL_mean),
              bias = mean(prediction - Chla_ugL_mean, na.rm = TRUE)) %>%
    arrange(model_type, model_id) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","GAM","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
    pivot_longer(rmse:bias, names_to = "skill_metric", values_to = "skill_value") 
  
  my.cols <- c("process-based" = "#B85233",
               "data-driven" = "#6FA19D",
               "KGML" = "navy",
               "ensemble" = "darkgray",
               "null" = "#DED50F")
  
  plot_data <- output %>%
    filter(skill_metric == viz_metric) 
  
  if(viz_metric == "rmse"){
    p <- ggplot()+
      geom_bar(data = plot_data, aes(x = skill_value, y = reorder(model_id,-skill_value),
                                     group = model_id, fill = model_type), color = "black",stat = "identity")
  } else if(viz_metric == "r2"){
    p <- ggplot()+
      geom_bar(data = plot_data, aes(x = skill_value, y = reorder(model_id,skill_value),
                                     group = model_id, fill = model_type), color = "black",stat = "identity")
  } else if(viz_metric == "bias"){
    p <- ggplot()+
      geom_bar(data = plot_data, aes(x = skill_value, y = reorder(model_id,-abs(skill_value)),
                                     group = model_id, fill = model_type), color = "black",stat = "identity")
  }
  
 p <- p +
    ylab("")+
    ggtitle(plot_title)+
    scale_fill_manual(name = "Model Type", values = my.cols)+
    scale_pattern_manual(values = c("yes" = "stripe", "no" = "none"),guide = "none") +
    theme_classic()+
    theme(legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1))
 
  if(viz_metric == "rmse"){
    p <- p +
      xlab(expression(paste("RMSE (",mu,g,~L^-1,")")))
  } else if(viz_metric == "r2"){
    p <- p +
      xlab(expression(paste(R^2)))
  } else {
    p <- p +
      xlab(expression(paste("mean bias (",mu,g,~L^-1,")")))
  }
  
  if(show_legend == FALSE){
    p <- p +
      theme(legend.position = "none")
  }
  
  return(p)
    
}
