#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit ARIMA model for chla from 2018-2021

pacman::p_load(fable, moments, parsnip, tidymodels, xgboost, DiagrammeR, vip)
tidymodels_prefer()
set.seed(100)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_XGBoost <- function(data, cal_dates, include_lag){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  dates <- as_tibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    select(datetime)
  
  df_lag <- as_tibble(data) %>%
    mutate(lag_Chla_ugL_mean = stats::lag(Chla_ugL_mean, k = 1)) %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean, lag_Chla_ugL_mean)

  df_no_lag <- as_tibble(data) %>%
    filter(datetime >= start_cal & datetime <= stop_cal) %>%
    select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean)

  #set recipe
  xgboost_recipe_lag <- df_lag |> 
    recipe(Chla_ugL_mean ~ . )
  xgboost_recipe_no_lag <- df_no_lag |> 
    recipe(Chla_ugL_mean ~ . )
  
  #assign folds
  folds_lag <- vfold_cv(df_lag, v = 10)
  folds_no_lag <- vfold_cv(df_no_lag, v = 10)
  
  #specify XGBoost from parsnip package
  xgboost_mod <- 
    boost_tree(mtry = 0.33, min_n = tune(), tree_depth = tune(), learn_rate = tune(),
               loss_reduction = tune(), sample_size = tune(), trees = 1000) %>%
    set_engine("xgboost", objective = "reg:squarederror", eval_metric = "rmse",
               counts = FALSE) |> 
    set_mode("regression")
  
  #specify XGBoost workflow 
  xgboost_wflow_lag <- 
    workflow() |> 
    add_model(xgboost_mod) |> 
    add_recipe(xgboost_recipe_lag)
  xgboost_wflow_no_lag <- 
    workflow() |> 
    add_model(xgboost_mod) |> 
    add_recipe(xgboost_recipe_no_lag)
  
  #tune XGBoost hyperparameters
  xgboost_resample_fit_lag <- 
    xgboost_wflow_lag |>  
    tune_grid(resamples = folds_lag,
              grid = 100,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  xgboost_resample_fit_no_lag <- 
    xgboost_wflow_no_lag |>  
    tune_grid(resamples = folds_no_lag,
              grid = 100,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  #select best hyperparameters
  xgboost_resample_fit_lag %>% 
    collect_metrics() |> 
    arrange(mean)
  xgboost_resample_fit_no_lag %>% 
    collect_metrics() |> 
    arrange(mean)
  
  best_hyperparameters_lag <- xgboost_resample_fit_lag %>%
    select_best(metric = "rmse")
  best_hyperparameters_no_lag <- xgboost_resample_fit_no_lag %>%
    select_best(metric = "rmse")
  
  #update workflow
  final_workflow_lag <- 
    xgboost_wflow_lag %>% 
    finalize_workflow(best_hyperparameters_lag)
  final_workflow_no_lag <- 
    xgboost_wflow_no_lag %>% 
    finalize_workflow(best_hyperparameters_no_lag)
  
  #train model
  xgboost_fit_lag <- final_workflow_lag |> 
    fit(data = df_lag)
  xgboost_fit_no_lag <- final_workflow_no_lag |> 
    fit(data = df_no_lag)
  
  #plot model feature importance
  vip_plot_lag <- xgboost_fit_lag %>%
    extract_fit_parsnip() %>%
    vip(geom = "point")
  vip_plot_no_lag <- xgboost_fit_no_lag %>%
    extract_fit_parsnip() %>%
    vip(geom = "point")
  
  #plot model fit
  fitted_values_lag <- predict(xgboost_fit_lag, df_lag) %>%
    bind_cols(dates)
  df_lag <- df_lag %>%
    bind_cols(dates)
  
  fitted_values_no_lag <- predict(xgboost_fit_no_lag, df_no_lag) %>%
    bind_cols(dates)
  df_no_lag <- df_no_lag %>%
    bind_cols(dates)

  XGBoost_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
    geom_line(data = fitted_values_no_lag, aes(x = datetime, y = .pred, color = "XGBoost (no lag)"))+
    geom_line(data = fitted_values_lag, aes(x = datetime, y = .pred, color = "XGBoost"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #build output df
  df.out <- data.frame(model_id = "XGBoost",
                       datetime = dates$datetime,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.pred)

  
  #return output + model with best fit + plot
  return(list(out = df.out, final_workflow_lag = final_workflow_lag, plot = XGBoost_plot,
              best_hyperparameters_lag = best_hyperparameters_lag,
              vip_plot_lag = vip_plot_lag,
              final_workflow_lag = final_workflow_lag,
              best_hyperparameters_lag = best_hyperparameters_lag,
              vip_plot_no_lag = vip_plot_no_lag))
}
