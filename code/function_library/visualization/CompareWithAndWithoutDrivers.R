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

CompareWithAndWithoutDrivers <- function(observations, 
                          model_output, 
                          forecast_horizon,
                          model_ids = model_ids,
                          viz_dates = pred_dates,
                          plot_title = "All predictions",
                          viz_metric = "r2",
                          show_legend = FALSE,
                          make_combined_bestmodel_legend =TRUE,
                          combined_var = "strat"){
  
  #reformat observations
  pred_dates <- data.frame(datetime = viz_dates) %>%
    left_join(., observations, by = "datetime") 
  
  rsq <- function(pred, obs){
    1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  }
  
  if(combined_var == "strat"){
  #reformat model output
  output <- model_output %>% 
    filter(model_id %in% model_ids & reference_datetime %in% viz_dates) %>%
    group_by(model_type, model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    left_join(., pred_dates, by = "datetime") %>%
    group_by(model_type, model_id, horizon, strat_bin) %>%
    summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE)),
              r2 = rsq(prediction, Chla_ugL_mean),
              bias = mean(prediction - Chla_ugL_mean, na.rm = TRUE)) %>%
    filter(!horizon == 0) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    filter(horizon <= forecast_horizon) %>%
    arrange(strat_bin, model_type, model_id, horizon) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
    pivot_longer(rmse:bias, names_to = "skill_metric", values_to = "skill_value")
  } else if(combined_var == "var"){
    output <- model_output %>% 
      filter(model_id %in% model_ids & reference_datetime %in% viz_dates) %>%
      group_by(model_type, model_id, reference_datetime) %>%
      mutate(horizon = datetime - reference_datetime) %>%
      ungroup() %>%
      separate(horizon, c("horizon"), sep = " ") %>%
      left_join(., pred_dates, by = "datetime") %>%
      group_by(model_type, model_id, horizon, var_bin) %>%
      summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE)),
                r2 = rsq(prediction, Chla_ugL_mean),
                bias = mean(prediction - Chla_ugL_mean, na.rm = TRUE)) %>%
      filter(!horizon == 0) %>%
      mutate(horizon = as.numeric(horizon)) %>%
      filter(horizon <= forecast_horizon) %>%
      arrange(var_bin, model_type, model_id, horizon) %>%
      mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:bias, names_to = "skill_metric", values_to = "skill_value")
  } else {
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
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:bias, names_to = "skill_metric", values_to = "skill_value")
  }
  
  my.dd.cols <- scales::seq_gradient_pal(low="#25625E", high="#B9E5E2")(seq(0, 1, length.out = 9))
  my.cols <- c("#948E0A","#DED50F","#F3EC48","#B85233","#E48A71",my.dd.cols,"navy","darkgray")
  
  plot_data <- output %>%
    filter(skill_metric == viz_metric)
  
  p <- ggplot()+
    geom_line(data = plot_data, aes(x = horizon, y = skill_value,
                                   group = model_id, color = model_id, linetype = model_type))+
    xlab("Prediction horizon (days)")+
    ggtitle(plot_title)+
    scale_color_manual(name = "Model ID", values = my.cols)+
    scale_linetype_manual(name = "Model Type", values = c("null" = "solid", "process-based" = "dotted", "data-driven" = "dashed", "ensemble" = "dotdash","KGML" = "F1"))+
    theme_classic()+
    theme(plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1)) 
  
  if(best_models_only == TRUE){
    
    if(viz_metric == "r2"){
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(skill_value == max(skill_value)) %>%
        arrange(horizon)
    } else if(viz_metric == "rmse") {
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(skill_value == min(skill_value)) %>%
        arrange(horizon)
    } else {
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(abs(skill_value) == min(abs(skill_value))) %>%
        arrange(horizon)
    }
  
  pers <- plot_data %>%
    filter(model_id == "persistence")
  
  my.shapes <-             c("ARIMA" = 0,
                             "ETS" = 1,
                             "TSLM" = 2,
                             "Prophet" = 3,
                             "LSTM" = 4,
                             "XGBoost" = 5,
                             "NNETAR" = 6,
                             "GLM-AED" = 7,
                             "OneDProcessModel" = 8,
                              "MARS" = 9,
                             "randomForest" = 10,
                             "NNETAR-KGML" = 11,
                             "ensemble" = 12,
                             "persistence" = 13,
                             "DOY" = 14,
                             "historical mean" = 15)
  my.cols <- c("process-based" = "#B85233",
               "data-driven" = "#6FA19D",
               "KGML" = "navy",
               "ensemble" = "darkgray",
               "null" = "#DED50F")

  p <- ggplot()+
    geom_line(data = pers, aes(x = horizon, y = skill_value, linetype = "persistence"))+
    geom_point(data = bestModByHorizon, aes(x = horizon, y = skill_value, shape = model_id, color = model_type), size = 2)+
    xlab("Prediction horizon (days)")+
    ggtitle(plot_title)+
    scale_shape_manual(name = "Model ID", values = my.shapes)+
    scale_color_manual(name = "Model type", values = my.cols)+ 
    scale_linetype_discrete(name = "Null model")+
    theme_classic()+
    theme(legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(color = guide_legend(order = 1))
  }
  
  if(make_combined_bestmodel_legend == TRUE){
    
    if(combined_var == "strat"){
      bestModByHorizon <- output %>%
        filter(skill_metric == "rmse") %>%
        group_by(strat_bin, horizon) %>%
        filter(skill_value == min(skill_value)) %>%
        arrange(horizon)
    } else if(combined_var == "var"){
      bestModByHorizon <- output %>%
        filter(skill_metric == "rmse") %>%
        group_by(var_bin, horizon) %>%
        filter(skill_value == min(skill_value)) %>%
        arrange(horizon)
    }else {
    bestModByHorizon1 <- output %>%
      filter(skill_metric == "rmse") %>%
      group_by(horizon) %>%
      filter(skill_value == min(skill_value)) %>%
      arrange(horizon)
    bestModByHorizon2 <- output %>%
      filter(skill_metric == "bias") %>%
      group_by(horizon) %>%
      filter(abs(skill_value) == min(abs(skill_value))) %>%
      arrange(horizon)
    bestModByHorizon <- bind_rows(bestModByHorizon1, bestModByHorizon2)
    }
    
    p <- ggplot()+
      geom_line(data = pers, aes(x = horizon, y = skill_value, linetype = "persistence"))+
      geom_point(data = bestModByHorizon, aes(x = horizon, y = skill_value, shape = model_id, color = model_type), size = 2)+
      xlab("Prediction horizon (days)")+
      ggtitle(plot_title)+
      scale_shape_manual(name = "Model ID", values = my.shapes)+
      scale_color_manual(name = "Model Type", values = my.cols)+ 
      scale_linetype_discrete(name = "Null model")+
      theme_classic()+
      theme(legend.title = element_text(face = "bold"),
            panel.background = element_rect(color = "black", linewidth = 1),
            legend.key.width = unit(2,"cm"),
            legend.key=element_rect(colour="white"))+
      guides(color = guide_legend(order = 1))
    
    if(combined_var == "strat"){
      p <- p + facet_wrap(facets = vars(strat_bin))
    } else if(combined_var == "var"){
      p <- p + facet_wrap(facets = vars(var_bin))
    } else {
      p <- p + facet_wrap(facets = vars(skill_metric))
    }
    
  }
  
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
  
  if(add_vline == TRUE){
    p <- p +
      geom_vline(xintercept = vline_intercept, linetype = "dashed")
  }
  
  return(p)
    
}
