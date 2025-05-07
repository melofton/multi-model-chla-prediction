#Compare models with and without drivers/lags
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot original model performance against alternative versions developed
#with/without drivers and lags

library(tidyverse)
library(lubridate)

#'Function to compare models with and without drivers/lags
#'@param observations data frame with columns:
#'Date: yyyy-mm-dd
#'Chla_ugL: observed daily median of chlorophyll-a from EXO in ug/L
#'@param model_output data frame with columns:
#'model_id: name of model (e.g., persistence)
#'reference_datetime: date prediction was issued (yyyy-mm-dd)
#'datetime: date of prediction (yyyy-mm-dd)
#'variable: predicted variable (chlorophyll-a)
#'prediction: value of prediction (ug/L)
#'@param forecast_horizon maximum horizon that you want to plot
#'@param model_ids character vector of model_ids from validation_output.csv to plot
#'@param viz_dates vector of dates to include when assessing skill (need all dates in vector, not just start/end dates)
#'@param plot_title character vector for desired plot title
#'@param viz_metric choose from "rmse", "r2", "mae" to visualize the model assessment metric you prefer
#'@param show_legend TRUE/FALSE whether to show plot legend
#'@param make_combined_legend TRUE/FALSE make combined legend for a plot with multiple sub-panels, e.g., Figs 3, 4, 5 in main manuscript
#'@param combined_var assigned in combination with make_combined_legend - which variable are you combining across? choose from "strat" for stratification period or "var" for high/low variability or "none" for none
#'@param parent_model character string of model_id for parent model from which drivers and/or lag have been removed for comparison; e.g., "MARS"
#'@param best_performing_horizons data frame with two columns, 'from' and 'to', where 'from' is first horizon where model performs well
#'and 'to' is last horizon; allows for multiple periods of top performance, e.g., from 2-5 days and from 13-16 days into the future

CompareWithAndWithoutDrivers <- function(observations = obs, 
                          model_output = out, 
                          forecast_horizon = forecast_horizon,
                          model_ids = c("MARS","MARS (no drivers)","MARS (no lag)","TSLM","TSLM (no drivers)","TSLM (no lag)"),
                          viz_dates = pred_dates,
                          plot_title = "test",
                          viz_metric = "rmse",
                          show_legend = TRUE,
                          make_combined_legend =FALSE,
                          combined_var = "none",
                          parent_model = c("MARS","TSLM"),
                          best_performing_horizons = data.frame(from = c(1),
                                                                to = c(10))){
  
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
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","TSLM (no drivers)","TSLM (no lag)","GAM","GAM (no drivers)","GAM (no lag)","MARS","MARS (no drivers)","MARS (no lag)","randomForest","Prophet","Prophet (no drivers)","XGBoost","XGBoost (no lag)","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
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
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","TSLM (no drivers)","TSLM (no lag)","GAM","GAM (no drivers)","GAM (no lag)","MARS","MARS (no drivers)","MARS (no lag)","randomForest","Prophet","Prophet (no drivers)","XGBoost","XGBoost (no lag)","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
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
      mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","TSLM (no drivers)","TSLM (no lag)","GAM","GAM (no drivers)","GAM (no lag)","MARS","MARS (no drivers)","MARS (no lag)","randomForest","Prophet","Prophet (no drivers)","XGBoost","XGBoost (no lag)","NNETAR","NNETAR (no drivers)","LSTM","NNETAR-KGML","ensemble"))) %>%
      pivot_longer(rmse:mae, names_to = "skill_metric", values_to = "skill_value")
  }
  
  for(i in 1:length(parent_model)){
  plot_data_temp <- output %>%
    filter(skill_metric == viz_metric & grepl(parent_model[i],model_id)) %>%
    mutate(parent_model = parent_model[i],
           model_version = ifelse(grepl('no drivers',model_id), 'chlorophyll-a data only', 
                                  ifelse(grepl('no lag',model_id), 'environmental variables only','original model'))) %>%
    mutate(model_version = factor(model_version, levels = c("original model","chlorophyll-a data only","environmental variables only")))
  if(i == 1){
    plot_data <- plot_data_temp
  } else {
    plot_data <- bind_rows(plot_data, plot_data_temp)
  }
  }
  
  my.shapes <-             c("ARIMA" = 0,
                             "TSLM" = 2,
                             "Prophet" = 3,
                             "XGBoost" = 5,
                             "NNETAR" = 6,
                             "MARS" = 9,
                             "GAM" = 11)
  
  my.dd.cols <- scales::seq_gradient_pal(low="#25625E", high="#B9E5E2")(seq(0, 1, length.out = 3))
  my.cols <- c("original model" = my.dd.cols[1],
               "chlorophyll-a data only" = my.dd.cols[2],
               "environmental variables only" = my.dd.cols[3])
  
  best_horizons <- data.frame(xmin = best_performing_horizons[1],
                              xmax = best_performing_horizons[2],
                              ymin = 0,
                              ymax = max(plot_data$skill_value + 1))

  p <- ggplot()+
    geom_rect(data = best_performing_horizons, aes(xmin = from - 0.5, xmax = to+0.5, ymin = -Inf, ymax = Inf, fill = "best-performing horizons",), alpha = 0.4, color = NA)+
    geom_point(data = plot_data, aes(x = horizon, y = skill_value, shape = parent_model, color = model_version), size = 2)+
    xlab("Prediction horizon (days)")+
    ggtitle(plot_title)+
    scale_shape_manual(name = "Parent model", values = my.shapes)+
    scale_color_manual(name = "Model version", values = my.cols)+
    scale_fill_manual(name = "", values = c("best-performing horizons" = "lightgray"))+
    theme_classic()+
    theme(legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"),
          legend.key=element_rect(colour="white"))+
    guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2))
  
  if(viz_metric == "rmse"){
    p <- p +
      ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))+
      ylim(c(2,14))
  } else if(viz_metric == "r2"){
    p <- p +
      ylab(expression(paste(R^2)))+
      geom_hline(yintercept = 0, linetype = "dashed")
  } else {
    p <- p +
      ylab(expression(paste("MAE (",mu,g,~L^-1,")")))
  }
  
  if(show_legend == FALSE){
    p <- p +
      theme(legend.position = "none")
  }
  
  return(p)
    
}
