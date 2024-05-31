#7-day prediction plot
#Author: Mary Lofton
#Date: 14MAR23

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
#'@param forecast_horizon horizon that you want to plot

OneHorizonTimeseries <- function(observations, 
                               model_output, 
                               forecast_horizon,
                               model_ids){
  
  #get plotting dates
  plot_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-12-31"), by = "day")  
  
  #limit to relevant observations
  plot_obs <- observations %>%
    filter(datetime %in% plot_dates) %>%
    mutate(variable = "observed")
  
  #limit model output to relevant dates
  plot_mod <- model_output %>%
    filter(datetime %in% plot_dates) %>%
    group_by(model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    filter(horizon == forecast_horizon,
           model_id %in% model_ids) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","ARIMA","ETS","TSLM","OneDProcessModel","GLM-AED","Prophet","XGBoost","NNETAR","LSTM")))
  
  p <- ggplot()+
    geom_point(data = plot_obs, aes(x = datetime, y = Chla_ugL_mean, 
                                    group = variable, fill = variable))+
    geom_line(data = plot_mod, aes(x = datetime, y = prediction,
                                   group = model_id, color = model_type, linetype = model_id), linewidth = 1)+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    scale_color_manual(name = "Model type", values = c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9"))+ #"#71BFB9","#B85233","#E69F00","#0072B2"
    scale_linetype_manual(name = "Model ID", values = c("solid", "dashed", "dotted", "solid", "dashed", "solid", "dashed", "dotted", "dotdash","solid", "dashed","dotted","dotdash","longdash","twodash","solid"))+
    scale_fill_discrete(name = "")+
    theme_classic()+
    ggtitle(paste0("Prediction horizon = ",forecast_horizon," days"))+
    theme(axis.text.x = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2), fill = guide_legend(order = 3))
  
  return(p)
    
}
