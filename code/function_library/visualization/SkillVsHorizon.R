#RMSE vs horizon plot
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot a prediction from 1-7 days into future with all models plotted

library(tidyverse)
library(lubridate)

#'Function to plot skill vs horizon for chl-a
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
#'@param model_ids character vector of model_ids from validation_output.csv to plot
#'@param best_models_only TRUE/FALSE whether to only show best model for each horizon
#'@param viz_dates vector of dates to include when assessing skill (need all dates in vector, not just start/end dates)
#'@param plot_title character vector for desired plot title
#'@param viz_metric choose from "rmse", "r2", "mae" to visualize the model assessment metric you prefer
#'@param show_legend TRUE/FALSE whether to show plot legend
#'@param make_combined_bestmodel_legend TRUE/FALSE make combined legend for a plot with multiple sub-panels, e.g., Figs 3, 4, 5 in main manuscript
#'@param add_vline TRUE/FALSE to add a vertical line, usually used to denote the horizon where performance of all models according to R2 declines to 0
#'@param vline_intercept numeric value of horizon at which to insert vline
#'@param combined_var assigned in combination with make_combined_bestmodel_legend - which variable are you combining across? choose from "strat" for stratficiation period or "var" for high/low variability or "none" for none
#'@param show_null_model TRUE/FALSE show best-performing null model at each horizon for comparison?
#'@param fixed_ylim TRUE/FALSE fix ylims or not
#'@param ylims vector of max/min ylims to be provided if fixed_ylim == TRUE

SkillVsHorizon <- function(observations, 
                          model_output, 
                          forecast_horizon,
                          model_ids = model_ids,
                          best_models_only = TRUE,
                          viz_dates = pred_dates,
                          plot_title = "All predictions",
                          viz_metric = "r2",
                          show_legend = FALSE,
                          make_combined_bestmodel_legend =TRUE,
                          add_vline = TRUE,
                          vline_intercept = 21,
                          combined_var = "strat",
                          show_null_model = TRUE,
                          fixed_ylim,
                          ylims){
  
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
              mae = mean(abs(prediction - Chla_ugL_mean), na.rm = TRUE)) %>%
    filter(!horizon == 0) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    filter(horizon <= forecast_horizon) %>%
    arrange(strat_bin, model_type, model_id, horizon) %>%
    mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","GAM","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
    pivot_longer(rmse:mae, names_to = "skill_metric", values_to = "skill_value")
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
                mae = mean(abs(prediction - Chla_ugL_mean), na.rm = TRUE)) %>%
      filter(!horizon == 0) %>%
      mutate(horizon = as.numeric(horizon)) %>%
      filter(horizon <= forecast_horizon) %>%
      arrange(var_bin, model_type, model_id, horizon) %>%
      mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","GAM","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:mae, names_to = "skill_metric", values_to = "skill_value")
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
                mae = mean(abs(prediction - Chla_ugL_mean), na.rm = TRUE)) %>%
      filter(!horizon == 0) %>%
      mutate(horizon = as.numeric(horizon)) %>%
      filter(horizon <= forecast_horizon) %>%
      arrange(model_type, model_id, horizon) %>%
      mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","GAM","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:mae, names_to = "skill_metric", values_to = "skill_value")
  }
  
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
                             "GAM" = 11,
                             "NNETAR-KGML" = 12,
                             "ensemble" = 13,
                             "persistence" = 14,
                             "DOY" = 15,
                             "historical mean" = 17)
  my.cols <- c("process-based" = "#B85233",
               "data-driven" = "#6FA19D",
               "KGML" = "navy",
               "ensemble" = "black",
               "null" = "#DED50F")
  
  plot_data <- output %>%
    filter(skill_metric == viz_metric)
  
  if(show_null_model == TRUE){
  
  if(viz_metric == "r2"){
    
    best_performing_null <- plot_data %>%
      filter(model_id %in% c("persistence","historical mean","DOY")) %>%
      group_by(horizon) %>%
      filter(skill_value == max(skill_value)) %>%
      arrange(horizon)
    
  } else if(viz_metric == "rmse") {
    
    best_performing_null <- plot_data %>%
      filter(model_id %in% c("persistence","historical mean","DOY")) %>%
      group_by(horizon) %>%
      filter(skill_value == min(skill_value, na.rm = TRUE)) %>%
      arrange(horizon)
  } else {
    
    best_performing_null <- plot_data %>%
      filter(model_id %in% c("persistence","historical mean","DOY")) %>%
      group_by(horizon) %>%
      filter(skill_value == min(skill_value, na.rm = TRUE)) %>%
      arrange(horizon)
  }
  }
  
  p <- ggplot()
  if(show_null_model == TRUE){
    p <- p + geom_line(data = best_performing_null, aes(x = horizon, y = skill_value, linetype = "best null model"))+
      scale_linetype_discrete(name = "")
  }
  p <- p +
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
  
  if(best_models_only == TRUE){
    
    if(viz_metric == "r2"){
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(skill_value == max(skill_value)) %>%
        arrange(horizon)
      
    } else if(viz_metric == "rmse") {
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(skill_value == min(skill_value, na.rm = TRUE)) %>%
        arrange(horizon)
    
    } else {
      bestModByHorizon <- plot_data %>%
        group_by(horizon) %>%
        filter(skill_value == min(skill_value, na.rm = TRUE)) %>%
        arrange(horizon)
      
    }

  p <- ggplot()
  if(show_null_model == TRUE){
    p <- p + geom_line(data = best_performing_null, aes(x = horizon, y = skill_value, linetype = "best null model"))+
      scale_linetype_discrete(name = "")
  }
  p <- p +
    geom_point(data = bestModByHorizon, aes(x = horizon, y = skill_value, shape = model_id, color = model_type), size = 2)+
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
  }
  
  if(make_combined_bestmodel_legend == TRUE){
    
    if(combined_var == "strat"){
      if(viz_metric == "r2"){
        bestModByHorizon <- output %>%
          filter(skill_metric == "r2" & !is.na(strat_bin)) %>%
          group_by(strat_bin, horizon) %>%
          filter(skill_value == max(skill_value)) %>%
          arrange(horizon)
      } else {
      bestModByHorizon <- output %>%
        filter(skill_metric == "rmse" & !is.na(strat_bin)) %>%
        group_by(strat_bin, horizon) %>%
        filter(skill_value == min(skill_value)) %>%
        arrange(horizon)
      }
    } else if(combined_var == "var"){
      bestModByHorizon <- output %>%
        filter(skill_metric == "rmse" & !is.na(var_bin)) %>%
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
      filter(skill_metric == "r2") %>%
      group_by(horizon) %>%
      filter(skill_value == max(skill_value)) %>%
      arrange(horizon)
    bestModByHorizon <- bind_rows(bestModByHorizon1, bestModByHorizon2)
    }
    
    p <- ggplot()
    if(show_null_model == TRUE){
      p <- p + geom_line(data = best_performing_null, aes(x = horizon, y = skill_value, linetype = "best null model"))
    }
      p <- p + geom_point(data = bestModByHorizon, aes(x = horizon, y = skill_value, shape = model_id, color = model_type), size = 2)+
      xlab("Prediction horizon (days)")+
      ggtitle(plot_title)+
      scale_shape_manual(name = "Model ID", values = my.shapes)+
      scale_color_manual(name = "Model Type", values = my.cols)+
      scale_linetype_discrete(name = "")+
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
    if(fixed_ylim == TRUE){
      p <- p + ylim(ylims)
    }
  } else if(viz_metric == "r2"){
    p <- p +
      ylab(expression(paste(R^2)))+
      geom_hline(yintercept = 0, linetype = "dashed")
    if(fixed_ylim == TRUE){
      p <- p + ylim(ylims)
    }
  } else {
    p <- p +
      ylab(expression(paste("MAE (",mu,g,~L^-1,")")))
    if(fixed_ylim == TRUE){
      p <- p + ylim(ylims)
    }
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
