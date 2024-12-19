#Example prediction plot
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot a prediction into future with all models plotted

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

ExamplePrediction <- function(observations, 
                              model_output, 
                              reference_datetime, 
                              forecast_horizon,
                              model_ids,
                              show_legend,
                              sub_panel_label,
                              rect_color,
                              ylim_values){
  
  #get plotting dates
  ref_datetime <- as.Date(reference_datetime)
  plot_dates <- seq.Date(from = as.Date(ref_datetime-5), to = as.Date(ref_datetime+forecast_horizon), by = "day")  
  
  #limit to relevant observations
  plot_obs <- observations %>%
    filter(datetime %in% plot_dates) %>%
    mutate(horizon = datetime - ref_datetime) %>%
    mutate(variable = ifelse(datetime <= ref_datetime,"obs. seen by model","obs. not seen by model")) %>%
    mutate(variable = factor(variable, levels = c("obs. seen by model","obs. not seen by model")))
  
  #limit model output to relevant dates
  plot_mod <- model_output %>%
    filter(reference_datetime == ref_datetime & datetime %in% plot_dates & model_id %in% model_ids) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
    mutate(horizon = datetime - ref_datetime) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ETS","TSLM","Prophet","XGBoost","NNETAR","LSTM","MARS","randomForest","NNETAR-KGML","ensemble"))) %>%
    mutate(example_model_names = paste0(model_id," (",model_type,")")) %>%
    mutate(example_model_names = factor(example_model_names, levels = c("persistence (null)",
                                                                        "DOY (null)",
                                                                        "historical mean (null)",
                                                                        "GLM-AED (process-based)",
                                                                        "OneDProcessModel (process-based)",
                                                                        "ARIMA (data-driven)",
                                                                        "ETS (data-driven)",
                                                                        "TSLM (data-driven)",
                                                                        "Prophet (data-driven)",
                                                                        "LSTM (data-driven)",
                                                                        "XGBoost (data-driven)",
                                                                        "NNETAR (data-driven)",
                                                                        "MARS (data-driven)",
                                                                        "randomForest (data-driven)",
                                                                        "NNETAR-KGML (KGML)",
                                                                        "ensemble (ensemble)"
                                                                        
      
    )))
  
  my.cols <-             c("ARIMA (data-driven)" = "#6FA19D",
                             "ETS (data-driven)" = "#6FA19D",
                             "TSLM (data-driven)" = "#6FA19D",
                             "Prophet (data-driven)" = "#6FA19D",
                             "LSTM (data-driven)" = "#6FA19D",
                             "XGBoost (data-driven)" = "#6FA19D",
                             "NNETAR (data-driven)" = "#6FA19D",
                             "GLM-AED (process-based)" = "#B85233",
                             "OneDProcessModel (process-based)" = "#B85233",
                             "MARS (data-driven)" = "#6FA19D",
                             "randomForest (data-driven)" = "#6FA19D",
                             "NNETAR-KGML (KGML)" = "navy",
                             "ensemble (ensemble)" = "darkgray",
                             "persistence (null)" = "#DED50F",
                             "DOY (null)" = "#DED50F",
                             "historical mean (null)" = "#DED50F")
  my.shapes <-             c("ARIMA (data-driven)" = 0,
                             "ETS (data-driven)" = 1,
                             "TSLM (data-driven)" = 2,
                             "Prophet (data-driven)" = 3,
                             "LSTM (data-driven)" = 4,
                             "XGBoost (data-driven)" = 5,
                             "NNETAR (data-driven)" = 6,
                             "GLM-AED (process-based)" = 7,
                             "OneDProcessModel (process-based)" = 8,
                             "MARS (data-driven)" = 9,
                             "randomForest (data-driven)" = 10,
                             "NNETAR-KGML (KGML)" = 11,
                             "ensemble (ensemble)" = 12,
                             "persistence (null)" = 13,
                             "DOY (null)" = 14,
                             "historical mean (null)" = 15)

  p <- ggplot()+
    geom_point(data = plot_obs, aes(x = horizon, y = Chla_ugL_mean, 
                                    group = variable, fill = variable),
               shape = 21)+
    geom_line(data = plot_mod, aes(x = horizon, y = prediction,
                                   group = example_model_names, color = example_model_names))+
    geom_vline(xintercept = 0, linetype = "dashed")+
    annotate("text", x = 2, y = ylim_values[2]-5, 
             label = "future", hjust = 0.25)+
    annotate("text", x = -3.5, y = ylim_values[2]-5, 
             label = "past", hjust = 0.25)+
    xlab("Prediction horizon (days)")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    scale_color_manual(name = "Example Models: \nmodel ID (model type)", values = my.cols)+ #c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9")"#71BFB9","#B85233","#E69F00","#0072B2"
    #scale_shape_manual(name = "Example Models", values = my.shapes)+
    # if want to group models by type, can do that with colors in line below
    #scale_color_manual(name = "Model ID", values = c("#71BFB9","#B85233","#E69F00","#0072B2"))+
    scale_fill_manual(name = "", values = c("obs. seen by model" = "black",
                                            "obs. not seen by model" = "white"))+
    theme_classic()+
    ggtitle(paste0(sub_panel_label,reference_datetime))+
    theme(plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.key=element_rect(colour="white"),
          legend.key.width=unit(2,"cm"),
          panel.background=element_rect(colour=rect_color, linewidth = 2),
          axis.line.x.bottom=element_line(color=rect_color),
          axis.line.y.left=element_line(color=rect_color),
          legend.box = "vertical")+
    guides(color = guide_legend(order = 1),
           linetype = guide_legend(order = 2),
           fill = guide_legend(order = 3))+
    ylim(ylim_values)+
    scale_x_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30, 35))
  
  if(show_legend == FALSE){
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
    
}
