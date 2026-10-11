#holder for new Fig. 4 supplement figure code

source("./code/function_library/visualization/OneHorizonSkill.R")

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p4b <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p4b <- p4b +
  annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4b

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p4d <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p4d <- p4d +
  annotate("text",x = 8.6, y = "LSTM", label = "*", size = 10, vjust = 0.8)+ # RMSE x = 8.6, r2 x = -0.4, MAE x = 6.5,
  geom_hline(yintercept = 10.5, size = 1)
p4d

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p4f <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p4f <- p4f +
  annotate("text",x = 9, y = "LSTM", label = "*", size = 10, vjust = 0.8)+ # RMSE x = 9, r2 x = -0.45, MAE x = 6.7,
  geom_hline(yintercept = 15.5, size = 1)
p4f

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4h <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p4h <- p4h +
  annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8)+ #RMSE x = 11.8, r2 x = -0.6, MAE x = 7.8,
  geom_hline(yintercept = 7.5, size = 1)
p4h