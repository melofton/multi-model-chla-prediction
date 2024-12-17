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
                              legend_option){
  
  #get plotting dates
  ref_datetime <- as.Date(reference_datetime)
  plot_dates <- seq.Date(from = as.Date(ref_datetime-3), to = as.Date(ref_datetime+forecast_horizon), by = "day")  
  
  #limit to relevant observations
  plot_obs <- observations %>%
    filter(datetime %in% plot_dates) %>%
    mutate(variable = ifelse(datetime <= ref_datetime,"observed, seen by model","observed, not seen by model")) %>%
    mutate(variable = factor(variable, levels = c("observed, seen by model","observed, not seen by model")))
  
  #limit model output to relevant dates
  plot_mod <- model_output %>%
    filter(reference_datetime == ref_datetime & datetime %in% plot_dates & model_id %in% model_ids) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ETS","TSLM","Prophet","XGBoost","NNETAR","LSTM","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble")))
  
  my.dd.cols <- scales::seq_gradient_pal(low="#25625E", high="#B9E5E2")(seq(0, 1, length.out = 9))
  my.cols <- c("#948E0A","#DED50F","#F3EC48","#B85233","#E48A71",my.dd.cols,"navy","darkgray")

  p <- ggplot()+
    geom_point(data = plot_obs, aes(x = datetime, y = Chla_ugL_mean, 
                                    group = variable, fill = variable),
               shape = 21)+
    geom_line(data = plot_mod, aes(x = datetime, y = prediction,
                                   group = model_id, color = model_id, linetype = model_type), linewidth = 1)+
    geom_vline(xintercept = ref_datetime, linetype = "dashed")+
    annotate("text", x = ref_datetime + 1.5, y = max(plot_obs$Chla_ugL_mean), 
             label = "future", hjust = 0.25)+
    annotate("text", x = ref_datetime -2.5, y = max(plot_obs$Chla_ugL_mean), 
             label = "past", hjust = 0.25)+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    scale_color_manual(name = "Model ID", values = my.cols)+ #c("null" = "#948E0A", "process-based" = "#B85233","data-driven" = "#71BFB9")"#71BFB9","#B85233","#E69F00","#0072B2"
    scale_linetype_manual(name = "Model Type", values = c("null" = "solid", "process-based" = "dotted", "data-driven" = "dashed","KGML" = "twodash", "ensemble" = "dotdash"))+
    # if want to group models by type, can do that with colors in line below
    #scale_color_manual(name = "Model ID", values = c("#71BFB9","#B85233","#E69F00","#0072B2"))+
    scale_fill_manual(name = "", values = c("observed, seen by model" = "black",
                                            "observed, not seen by model" = "white"))+
    theme_classic()+
    ggtitle(paste0(sub_panel_label,reference_datetime))+
    theme(plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.key=element_rect(colour="white"),
          legend.key.width=unit(2,"cm"),
          panel.background=element_rect(colour=rect_color, linewidth = 2),
          axis.line.x.bottom=element_line(color=rect_color),
          axis.line.y.left=element_line(color=rect_color),
          legend.box = "horizontal")+
    guides(color = guide_legend(order = 1, ncol = 2),
           linetype = guide_legend(order = 2),
           fill = guide_legend(order = 3))
  
  if(legend_option == "model_id_only"){
    p <- p + guides(linetype = "none", fill = "none")
  }
  
  if(legend_option == "no_model_id"){
    p <- p + guides(color = "none")
  }
  
  if(show_legend == FALSE){
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
    
}
