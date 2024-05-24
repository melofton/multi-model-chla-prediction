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

RMSEVsHorizon <- function(observations, 
                          model_output, 
                          forecast_horizon,
                          best_models_only = TRUE){
  
  #reformat observations
  pred_dates <- data.frame(datetime = unique(model_output$reference_datetime)) %>%
    left_join(., observations, by = "datetime") 
  
  
  #reformat model output
  output <- model_output %>% 
    group_by(model_type, model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    left_join(., pred_dates, by = "datetime") %>%
    group_by(model_type, model_id, horizon) %>%
    summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE))) %>%
    filter(!horizon == 0) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    filter(horizon <= forecast_horizon) %>%
    arrange(model_type, model_id, horizon) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","ARIMA","ETS","TSLM","OneDProcessModel","GLM-AED","prophet","XGBoost","NNETAR","LSTM")))
  
  p <- ggplot()+
    geom_line(data = output, aes(x = horizon, y = rmse,
                                   group = model_id, color = model_type, linetype = model_id),
              linewidth = 1)+
    xlab("Forecast horizon (days)")+
    ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))+
    ggtitle("All predictions (Jan. 1, 2022 - Nov. 26, 2023)")+
    #scale_color_discrete(name = "Model ID")+
    #scale_linetype_discrete(name = "Model type")+
    scale_color_manual(name = "Model type", values = c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9"))+ #"#71BFB9","#B85233","#E69F00","#0072B2"
    scale_linetype_manual(name = "Model ID", values = c("solid", "dashed", "dotted", "solid", "dashed", "solid", "dashed", "dotted", "dotdash","solid", "dashed","dotted","dotdash","longdash","twodash","solid"))+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1)) 
  
  if(best_models_only == TRUE){
  
  bestModByHorizon <- output %>%
    group_by(horizon) %>%
    filter(rmse == min(rmse)) %>%
    arrange(horizon)
  
  pers <- output %>%
    filter(model_id == "persistence")
  
  p <- ggplot()+
    geom_line(data = pers, aes(x = horizon, y = rmse, linetype = "persistence"))+
    geom_point(data = bestModByHorizon, aes(x = horizon, y = rmse, shape = model_id, color = model_type))+
    xlab("Forecast horizon (days)")+
    ylab(expression(paste("Best model RMSE (",mu,g,~L^-1,")")))+
    ggtitle("All predictions (Jan. 1, 2022 - Nov. 26, 2023)")+
    scale_shape_manual(name = "Model ID", values = c(9,16,8))+
    scale_color_manual(name = "Model type", values = c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9"))+ #"#71BFB9","#B85233","#E69F00","#0072B2"
    scale_linetype_discrete(name = "Null model")+
    theme_classic()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1))
  }
  
  return(p)
    
}
