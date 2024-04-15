#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: make predictions using XGBoost model for chla

pacman::p_load(fable, moments, parsnip, tidymodels, xgboost, DiagrammeR)
tidymodels_prefer()
set.seed(100)

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions
#'@param model xgb.booster object fitted and saved from 03_calibrate_models.R

parsnipXGBoost <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  
  # #define scaling function
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  # 
  # #define vars
  # vars <- c("AirTemp_C","Shortwave_Wm2","Windspeed_ms","Inflow_cms", "WaterTemp_C" ,"LightAttenuation_Kd", "DIN_ugL", "SRP_ugL")
  # 
  #assign target and predictors
  df <- as_tibble(data) %>%
    mutate(lag_Chla_ugL_mean = stats::lag(Chla_ugL_mean, k = 1)) %>%
    filter(datetime < pred_dates[1]) %>%
    select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean, lag_Chla_ugL_mean)  
  
  #set recipe
  xgboost_recipe <- df |> 
    recipe(Chla_ugL_mean ~ . )
  
  #assign folds
  folds <- vfold_cv(df, v = 10)
  
  #specify XGBoost from parsnip package
  xgboost_mod <- 
    boost_tree(mtry = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
               loss_reduction = tune(), sample_size = tune(), trees = 1000) %>%
    set_engine("xgboost", num.threads = parallel::detectCores()) |> 
    set_mode("regression")
  
  #specify XGBoost workflow 
  xgboost_wflow <- 
    workflow() |> 
    add_model(xgboost_mod) |> 
    add_recipe(xgboost_recipe)
  
  #tune XGBoost hyperparameters
  xgboost_resample_fit <- 
    xgboost_wflow |>  
    tune_grid(resamples = folds,
              grid = 25,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(mae))
  
  #select best hyperparameters
  xgboost_resample_fit %>% 
    collect_metrics() |> 
    arrange(mean)
  
  best_hyperparameters <- xgboost_resample_fit %>%
    select_best(metric = "mae")
  
  #update workflow
  final_workflow <- 
    xgboost_wflow %>% 
    finalize_workflow(best_hyperparameters)
  
  #train model
  xgboost_fit <- final_workflow |> 
    fit(data = df)
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build driver dataset
    drivers = as_tibble(data) %>%
      mutate(lag_Chla_ugL_mean = dplyr::lag(Chla_ugL_mean, 1)) %>%
      filter(datetime %in% forecast_dates) %>%
      select(AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean, LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL_mean, lag_Chla_ugL_mean)  
    
    drivers[,"Chla_ugL_mean"] <- NA
    
    for(h in 1:forecast_horizon){
    #generate predictions
    temp_pred <- predict(xgboost_fit, new_data = drivers[h,])
    if(h == 1){
      pred = temp_pred
    } else {
      pred = bind_rows(pred, temp_pred)
    }
    if(h < 35){
    drivers$lag_Chla_ugL_mean[h+1] <- temp_pred$.pred
    }
    }

    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(datetime == pred_dates[t]) %>%
      select(Chla_ugL_mean)
    curr_chla <- curr_chla_df$Chla_ugL_mean[1]
    temp.df <- data.frame(model_id = "XGBoost",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$.pred))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
