#Visualize model output
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Visualize model output to assess model performance

#load packages
library(tidyverse)
library(lubridate)
library(ggpubr)
library(viridis)
#library(plotly)

#Read in data
cal <- read_csv("./model_output/calibration_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS","randomForest","GAM"),"data-driven","process-based")))
out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS","randomForest","GAM","TSLMnoDrivers","TSLMnoLag","MARSnoDrivers","MARSnoLag","GAMnoLag","GAMnoDrivers","XGBoostNoLag"),"data-driven",
                                    ifelse(model_id %in% c("NNETAR_KGML_residuals","NNETAR_KGML_observations","NNETAR_KGML_cal_training_resid","NNETAR_KGML_cal_training_obs"),"KGML","process-based"))),
         model_id = ifelse(model_id == "ARIMAnoDrivers","ARIMA (no drivers)",
                           ifelse(model_id == "NNETARnoDrivers","NNETAR (no drivers)",
                                  ifelse(model_id == "ProphetnoDrivers","Prophet (no drivers)",
                                         ifelse(model_id == "NNETAR_KGML_residuals","NNETAR-KGML (trained by horizon)",
                                                ifelse(model_id == "TSLMnoDrivers","TSLM (no drivers)",
                                                       ifelse(model_id == "TSLMnoLag","TSLM (no lag)",
                                                              ifelse(model_id == "MARSnoDrivers","MARS (no drivers)",
                                                                     ifelse(model_id == "MARSnoLag","MARS (no lag)",
                                                                            ifelse(model_id == "NNETAR_KGML_observations","NNETAR-KGML 3",
                                                                                   ifelse(model_id == "NNETAR_KGML_cal_training_resid","NNETAR-KGML",
                                                                                          ifelse(model_id == "NNETAR_KGML_cal_training_obs","NNETAR-KGML 4",
                                                                                                 ifelse(model_id == "GAMnoLag","GAM (no lag)",
                                                                                                        ifelse(model_id == "GAMnoDrivers","GAM (no drivers)",
                                                                                                               ifelse(model_id == "XGBoostNoLag","XGBoost (no lag)",model_id)))))))))))))))
unique(out$model_id)
ens <- out %>%
  filter(model_id %in% c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","OneDProcessModel","MARS","randomForest",
                                     "GAM","NNETAR-KGML")) %>%
  group_by(reference_datetime, datetime) %>%
  summarize(prediction = mean(prediction, na.rm = TRUE)) %>%
  add_column(model_id = "ensemble", model_type = "ensemble", variable = "chlorophyll-a") 

out <- bind_rows(out, ens)
obs <- read_csv("./data/data_processed/chla_obs.csv")
input <- read_csv("./data/data_processed/ARIMA.csv")

ss_data <- read_csv("./data/data_processed/schmidt_stability.csv")


#Set arguments for plotting functions
forecast_horizon = 35
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")

#Plot 

# Figure 1
source("./code/function_library/visualization/PlotObservations.R")
p1a <- PlotObservations(observations = obs, 
                        pred_only = FALSE,
                        focal_dates = c("2022-01-10","2022-04-10","2022-07-10","2022-09-10","2023-01-10","2023-10-15"),
                        forecast_horizon = forecast_horizon,
                       plotly = FALSE,
                       train_test_box = TRUE,
                       training_dates = c("2018-08-06","2021-12-31"),
                       testing_dates = c("2022-01-01","2023-12-31"),
                       focal_dates_geom = "LINE",
                       ss_data = ss_data)
p1a

source("./code/function_library/visualization/PlotSchmidtStability.R")
p1b <- PlotSchmidtStability(ss_data = ss_data,
                            testing_dates = c("2022-01-01","2023-12-31"))
p1b

source("./code/function_library/visualization/PlotChlaVariability.R")
p1c <- PlotChlaVariability(obs)
p1c$p

p1 <- ggarrange(p1a, 
                  ggarrange(p1b, p1c$p, ncol = 2, labels = c("b","c")), 
                  nrow = 2,
                labels = "a"
                )
p1
ggsave(plot = p1, filename = "./figures/final_figures/Figure1.tif",
       device = "tiff", height = 6, width = 10, units = "in")

# Figure 2

source("./code/function_library/visualization/ExamplePrediction.R")
plot_cols <- viridis(6, option = "turbo")
focal_dates = c("2022-01-10","2022-04-10","2022-07-10","2022-09-10","2023-01-10","2023-10-15")

reference_datetime_a = focal_dates[1]
p2a <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_a, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "a. Mixed: ",
                         rect_color = plot_cols[1],
                         ylim_values = c(0,40))
p2a

reference_datetime_b = focal_dates[2]
p2b <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "b. Onset: ",
                         rect_color = plot_cols[2],
                         ylim_values = c(0,40))
p2b

reference_datetime_c = focal_dates[3]
p2c <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_c, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "c. Stratified: ",
                         rect_color = plot_cols[3],
                         ylim_values = c(0,40))
p2c

reference_datetime_d = focal_dates[4]
p2d <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_d, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "d. Decline: ",
                         rect_color = plot_cols[4],
                         ylim_values = c(0,40))
p2d

reference_datetime_e = focal_dates[5]
p2e <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_e, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "e. Low variability: ",
                         rect_color = plot_cols[5],
                         ylim_values = c(0,40))
p2e

reference_datetime_f = focal_dates[6]
p2f <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_f, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "f. High variability: ",
                         rect_color = plot_cols[6],
                         ylim_values = c(0,65))
p2f

leg_plot <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = TRUE,
                         sub_panel_label = "b",
                         rect_color = "black",
                         ylim_values = c(0,40))

# Extract the legend. Returns a gtable
leg <- get_legend(leg_plot)

# Convert to a ggplot and print
p2_leg <- as_ggplot(leg)
p2_leg

p2 <- ggarrange(ggarrange(p2a, p2b, p2c, p2d, p2e, p2f, nrow = 3, ncol = 2),
                p2_leg,
                ncol = 2,
                widths = c(1, 0.4)
) +theme(plot.margin = margin(0.2,0.1,1.5,0.1, "cm"))
  #bgcolor("white")
   

p2

ggsave(plot = p2, filename = "./figures/final_figures/Figure2.tif",
       device = "tiff", height = 11, width = 10, units = "in",bg = "white")

# Figure 2 supplement
p2_supp1a <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_a, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                                   "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                                   "GLM-AED","OneDProcessModel","MARS","randomForest",
                                                   "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "a. Mixed: ",
                         rect_color = plot_cols[1],
                         ylim_values = c(0,40))
p2_supp1a

p2_supp1b <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                       "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                       "GLM-AED","OneDProcessModel","MARS","randomForest",
                                       "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "b. Onset: ",
                         rect_color = plot_cols[2],
                         ylim_values = c(0,40))
p2_supp1b

p2_supp1c <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_c, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                       "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                       "GLM-AED","OneDProcessModel","MARS","randomForest",
                                       "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "c. Stratified: ",
                         rect_color = plot_cols[3],
                         ylim_values = c(0,40))
p2_supp1c

p2_supp1d <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_d, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                       "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                       "GLM-AED","OneDProcessModel","MARS","randomForest",
                                       "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "d. Decline: ",
                         rect_color = plot_cols[4],
                         ylim_values = c(0,40))
p2_supp1d

p2_supp1e <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_e, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                       "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                       "GLM-AED","OneDProcessModel","MARS","randomForest",
                                       "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "e. Low variability: ",
                         rect_color = plot_cols[5],
                         ylim_values = c(0,40))
p2_supp1e

p2_supp1f <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_f, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA",
                                       "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                       "GLM-AED","OneDProcessModel","MARS","randomForest",
                                       "GAM","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "f. High variability: ",
                         rect_color = plot_cols[6],
                         ylim_values = c(0,65))
p2_supp1f

p2_supp_leg_plot <- ExamplePrediction(observations = obs, 
                              model_output = out, 
                              reference_datetime = reference_datetime_b, 
                              forecast_horizon = forecast_horizon,
                              model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                              show_legend = TRUE,
                              sub_panel_label = "b",
                              rect_color = "black",
                              ylim_values = c(0,40))

# Extract the legend. Returns a gtable
p2_supp1_leg1 <- get_legend(p2_supp_leg_plot)

# Convert to a ggplot and print
p2_supp1_leg1 <- as_ggplot(p2_supp1_leg1)
p2_supp1_leg1

p2_supp1 <- ggarrange(ggarrange(p2_supp1a, p2_supp1b, p2_supp1c, p2_supp1d, p2_supp1e, p2_supp1f, nrow = 3, ncol = 2),
                p2_supp1_leg1,
                ncol = 2,
                widths = c(1, 0.4)
) +theme(plot.margin = margin(0.2,0.1,1.5,0.1, "cm"))
#bgcolor("white")


p2_supp1

ggsave(plot = p2_supp1, filename = "./figures/final_figures/Figure2_supp1.tif",
       device = "tiff", height = 11, width = 10, units = "in",bg = "white")

# Figure 3

source("./code/function_library/visualization/SkillVsHorizon.R")
source("./code/function_library/visualization/GrandMeanSkill.R")

p3a <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 26,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = FALSE)
p3a

p3b <- GrandMeanSkill(observations = obs, 
                    model_output = out, 
                    forecast_horizon = 35,
                    model_ids = c("DOY","persistence","historical mean","ARIMA",
                                  "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                  "GLM-AED","OneDProcessModel","MARS","randomForest",
                                  "GAM","NNETAR-KGML","ensemble"),
                    viz_dates = pred_dates,
                    plot_title = "All horizons",
                    viz_metric = "rmse",
                    show_legend = FALSE)
p3b <- p3b +
  annotate("text",x = 10.5, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p3b


p3c <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 21,
                      combined_var = "none",
                      fixed_ylim = FALSE)
p3c

p3d <- GrandMeanSkill(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p3d <- p3d +
  annotate("text",x = -0.35, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p3d

p3e <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 26,
                      combined_var = "none",
                      fixed_ylim = FALSE)
p3e

p3f <- GrandMeanSkill(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p3f <- p3f +
  annotate("text",x = 7.3, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p3f

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "none")

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p3_leg1 <- as_ggplot(leg1)
p3_leg1

leg_plot2 <- GrandMeanSkill(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons up to 21 days",
                      viz_metric = "r2",
                      show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p3_leg2 <- as_ggplot(leg2)
p3_leg2


p3 <- ggarrange(p3_leg1,
                ggarrange(p3a, p3b, p3c, p3d, p3e, p3f,
                nrow = 3,
                ncol = 2,
                labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),
                widths = c(1,0.8)),
                p3_leg2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p3

ggsave(plot = p3, filename = "./figures/final_figures/Figure3.tif",
       device = "tiff", height = 9, width = 12, units = "in",bg = "white")

# Figure 3 supplements

p3_supp1a <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 26,
                      combined_var = "none",
                      show_null_model = FALSE,
                      fixed_ylim = FALSE)
p3_supp1a

p3_supp1b <- GrandMeanSkill(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)

p3_supp1b

p3_supp1c <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 35,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "r2",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            vline_intercept = 26,
                            combined_var = "none",
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p3_supp1c

p3_supp1d <- GrandMeanSkill(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 35,
                            model_ids = c("DOY","persistence","historical mean"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons",
                            viz_metric = "r2",
                            show_legend = FALSE)

p3_supp1d

p3_supp1e <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 35,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "mae",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            vline_intercept = 26,
                            combined_var = "none",
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p3_supp1e

p3_supp1f <- GrandMeanSkill(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 35,
                            model_ids = c("DOY","persistence","historical mean"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons",
                            viz_metric = "mae",
                            show_legend = FALSE)

p3_supp1f

p3_supp1_leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "none",
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)

# Extract the legend. Returns a gtable
p3_supp1_leg1 <- get_legend(p3_supp1_leg_plot1)

# Convert to a ggplot and print
p3_supp1_leg1 <- as_ggplot(p3_supp1_leg1)
p3_supp1_leg1 <- p3_supp1_leg1 #+ theme(plot.background = element_blank()) + bgcolor("white")
p3_supp1_leg1

p3_supp1_leg_plot2 <- GrandMeanSkill(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons up to 21 days",
                            viz_metric = "mae",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
p3_supp1_leg2 <- get_legend(p3_supp1_leg_plot2)

# Convert to a ggplot and print
p3_supp1_leg2 <- as_ggplot(p3_supp1_leg2) 
p3_supp1_leg2 <- p3_supp1_leg2 #+ theme(plot.background = element_blank()) + bgcolor("white")
p3_supp1_leg2

p3_supp1 <- ggarrange(p3_supp1_leg1,
                ggarrange(p3_supp1a, p3_supp1b, p3_supp1c, p3_supp1d, p3_supp1e, p3_supp1f, 
                          nrow = 3,
                          ncol = 2,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),
                          widths = c(1,0.8)),
                p3_supp1_leg2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white") + theme(plot.background = element_blank())

p3_supp1

ggsave(plot = p3_supp1, filename = "./figures/final_figures/Figure3_supp1.tif",
       device = "tiff", height = 9, width = 12, units = "in", bg = "white")


# Figure 4

source("./code/function_library/visualization/SkillVsHorizon.R")
source("./code/function_library/visualization/GrandMeanSkill.R")

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p4a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Mixed period",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(1,12)) #r2 -1.5,1; mae 0,8
p4a

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
  annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
p4b

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p4c <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(1,12))
p4c

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
  annotate("text",x = 8.6, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 8.6, r2 x = -0.4, MAE x = 6.5,
p4d

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p4e <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratified",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(1,12))
p4e

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
  annotate("text",x = 9, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 9, r2 x = -0.45, MAE x = 6.7,
p4f

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4g <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification decline",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(1,12))
p4g

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
  annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.6, MAE x = 7.8,
p4h

mod_out_all_strat <- out %>%
  left_join(., ss_data, by = "datetime")

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_all_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "strat",
                            show_null_model = TRUE,
                            add_vline = FALSE,
                            fixed_ylim = TRUE,
                            ylims = c(1,12))

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p4_leg1 <- as_ggplot(leg1)
p4_leg1

leg_plot2 <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons up to 21 days",
                            viz_metric = "rmse",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p4_leg2 <- as_ggplot(leg2)
p4_leg2


p4 <- ggarrange(p4_leg1,
                ggarrange(p4a, p4b, p4c, p4d, p4e, p4f, p4g, p4h,
                          nrow = 4,
                          ncol = 2,
                          labels = c("a","b","c","d","e","f","g","h"),
                          widths = c(1,0.8)),
                p4_leg2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p4

ggsave(plot = p4, filename = "./figures/final_figures/Figure4.tif",
       device = "tiff", height = 10, width = 12, units = "in", bg = "white")

# Figure 4 supplement

# r2

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p4a_r2 <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Mixed period",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(-0.5,1)) #r2 -1.5,1; mae 0,8
p4a_r2

p4b_r2 <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p4b_r2 <- p4b_r2 +
  annotate("text",x = -0.24, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
p4b_r2

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p4c_r2 <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(-0.5,1))
p4c_r2

p4d_r2 <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p4d_r2 <- p4d_r2 +
  annotate("text",x = -0.4, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 8.6, r2 x = -0.4, MAE x = 6.5,
p4d_r2

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p4e_r2 <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratified",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(-0.5,1))
p4e_r2

p4f_r2 <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p4f_r2 <- p4f_r2 +
  annotate("text",x = -0.45, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 9, r2 x = -0.45, MAE x = 6.7,
p4f_r2

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4g_r2 <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification decline",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(-0.5,1))
p4g_r2

p4h_r2 <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p4h_r2 <- p4h_r2 +
  annotate("text",x = -0.6, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.6, MAE x = 7.8,
p4h_r2

mod_out_all_strat <- out %>%
  left_join(., ss_data, by = "datetime")

leg_plot1_r2 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_all_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "r2",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "strat",
                            show_null_model = TRUE,
                            add_vline = FALSE,
                            fixed_ylim = TRUE,
                            ylims = c(-0.5,1))

# Extract the legend. Returns a gtable
leg1_r2 <- get_legend(leg_plot1_r2)

# Convert to a ggplot and print
p4_leg1_r2 <- as_ggplot(leg1_r2)
p4_leg1_r2

leg_plot2_r2 <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons up to 21 days",
                            viz_metric = "r2",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2_r2 <- get_legend(leg_plot2_r2)

# Convert to a ggplot and print
p4_leg2_r2 <- as_ggplot(leg2_r2)
p4_leg2_r2


p4_r2 <- ggarrange(p4_leg1_r2,
                ggarrange(p4a_r2, p4b_r2, p4c_r2, p4d_r2, p4e_r2, p4f_r2, p4g_r2, p4h_r2,
                          nrow = 4,
                          ncol = 2,
                          labels = c("a","b","c","d","e","f","g","h"),
                          widths = c(1,0.8)),
                p4_leg2_r2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p4_r2

ggsave(plot = p4_r2, filename = "./figures/final_figures/Figure4_r2.tif",
       device = "tiff", height = 10, width = 12, units = "in", bg = "white")

# mae

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p4a_mae <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Mixed period",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(0,8)) #r2 -1.5,1; mae 0,8
p4a_mae

p4b_mae <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p4b_mae <- p4b_mae +
  annotate("text",x = 8.5, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
p4b_mae

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p4c_mae <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(0,8))
p4c_mae

p4d_mae <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p4d_mae <- p4d_mae +
  annotate("text",x = 6.5, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 8.6, r2 x = -0.4, MAE x = 6.5,
p4d_mae

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p4e_mae <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratified",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(0,8))
p4e_mae

p4f_mae <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p4f_mae <- p4f_mae +
  annotate("text",x = 6.7, y = "LSTM", label = "*", size = 10, vjust = 0.8) # RMSE x = 9, r2 x = -0.45, MAE x = 6.7,
p4f_mae

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4g_mae <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification decline",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(0,8))
p4g_mae

p4h_mae <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p4h_mae <- p4h_mae +
  annotate("text",x = 7.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) #RMSE x = 11.8, r2 x = -0.6, MAE x = 7.8,
p4h_mae

mod_out_all_strat <- out %>%
  left_join(., ss_data, by = "datetime")

leg_plot1_mae <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_all_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "mae",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "strat",
                            show_null_model = TRUE,
                            add_vline = FALSE,
                            fixed_ylim = FALSE,
                            ylims = c(0,8))

# Extract the legend. Returns a gtable
leg1_mae <- get_legend(leg_plot1_mae)

# Convert to a ggplot and print
p4_leg1_mae <- as_ggplot(leg1_mae)
p4_leg1_mae

leg_plot2_mae <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons up to 21 days",
                            viz_metric = "mae",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2_mae <- get_legend(leg_plot2_mae)

# Convert to a ggplot and print
p4_leg2_mae <- as_ggplot(leg2_mae)
p4_leg2_mae


p4_mae <- ggarrange(p4_leg1_mae,
                ggarrange(p4a_mae, p4b_mae, p4c_mae, p4d_mae, p4e_mae, p4f_mae, p4g_mae, p4h_mae,
                          nrow = 4,
                          ncol = 2,
                          labels = c("a","b","c","d","e","f","g","h"),
                          widths = c(1,0.8)),
                p4_leg2_mae,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p4_mae

ggsave(plot = p4_mae, filename = "./figures/final_figures/Figure4_mae.tif",
       device = "tiff", height = 10, width = 12, units = "in", bg = "white")

# null models

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p4_supp1a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Mixed period",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = FALSE)
p4_supp1a

p4_supp1b <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Mixed period",
                            viz_metric = "r2",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1b

p4_supp1c <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Mixed period",
                            viz_metric = "mae",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1c

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p4_supp1d <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = FALSE)
p4_supp1d

p4_supp1e <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_onset, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratification onset",
                            viz_metric = "r2",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            vline_intercept = 18,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1e

p4_supp1f <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_onset, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratification onset",
                            viz_metric = "mae",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            vline_intercept = 18,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1f

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p4_supp1g <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratified",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = FALSE)
p4_supp1g

p4_supp1h <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratified",
                            viz_metric = "r2",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1h

p4_supp1i <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratified",
                            viz_metric = "mae",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1i

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4_supp1j <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification decline",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = FALSE)
p4_supp1j

p4_supp1k <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_decline, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratification decline",
                            viz_metric = "r2",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1k

p4_supp1l <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_decline, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "Stratification decline",
                            viz_metric = "mae",
                            show_legend = FALSE,
                            make_combined_bestmodel_legend = FALSE,
                            add_vline = FALSE,
                            combined_var = "none",
                            show_null_model = FALSE)
p4_supp1l

mod_out_all_strat <- out %>%
  left_join(., ss_data, by = "datetime")

p4_supp1_leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_all_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "mae",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "strat",
                            show_null_model = FALSE,
                            add_vline = FALSE)

# Extract the legend. Returns a gtable
p4_supp1_leg1 <- get_legend(p4_supp1_leg_plot1)

# Convert to a ggplot and print
p4_supp1_leg1 <- as_ggplot(p4_supp1_leg1)
p4_supp1_leg1

p4_supp1_leg_plot2 <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_mixed, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons up to 21 days",
                            viz_metric = "mae",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
p4_supp1_leg2 <- get_legend(p4_supp1_leg_plot2)

# Convert to a ggplot and print
p4_supp1_leg2 <- as_ggplot(p4_supp1_leg2)
p4_supp1_leg2


p4_supp1 <- ggarrange(p4_supp1_leg1,
                ggarrange(p4_supp1a, p4_supp1b, p4_supp1c, p4_supp1d, p4_supp1e, p4_supp1f, p4_supp1g, p4_supp1h, p4_supp1i, p4_supp1j, p4_supp1k, p4_supp1l,
                          nrow = 4,
                          ncol = 3,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                          widths = c(1,1,1)),
                p4_supp1_leg2,
                ncol = 3,
                widths = c(0.2, 1, 0.2)
) #+ bgcolor("white")

p4_supp1

ggsave(plot = p4_supp1, filename = "./figures/final_figures/Figure4_supp1.tif",
       device = "tiff", height = 10, width = 15, units = "in", bg = "white")

# Figure 5

source("./code/function_library/visualization/SkillVsHorizon.R")
source("./code/function_library/visualization/GrandMeanSkill.R")

obs_var <- obs %>%
  mutate(delta = c(NA,abs(diff(Chla_ugL_mean, na.rm = TRUE)))) 

dens <- density(obs_var$delta, na.rm = TRUE)
q90 <- quantile(obs_var$delta, 0.90, na.rm = TRUE)
var_df <- obs_var %>%
  mutate(var_bin = ifelse(delta > q90, "high","low"))

high_var <- var_df %>%
  filter(var_bin == "high")

mod_out_high_var <- out %>%
  filter(datetime %in% high_var$datetime)

p5a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 8,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = FALSE)
p5a

p5b <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p5b <- p5b +
  annotate("text",x = 19.6, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p5b

p5c <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      fixed_ylim = FALSE)
p5c

p5d <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p5d <- p5d +
  annotate("text",x = -1.35, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p5d

p5e <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = TRUE,
                      vline_intercept = 8,
                      combined_var = "none",
                      fixed_ylim = FALSE)
p5e

p5f <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p5f <- p5f +
  annotate("text",x = 15.5, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p5f

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "none",
                            fixed_ylim = FALSE)

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p5_leg1 <- as_ggplot(leg1)
p5_leg1

leg_plot2 <- GrandMeanSkill(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","OneDProcessModel","MARS","randomForest",
                                          "NNETAR-KGML","ensemble"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons",
                            viz_metric = "mae",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p5_leg2 <- as_ggplot(leg2)
p5_leg2


p5 <- ggarrange(p5_leg1,
                ggarrange(p5a, p5b, p5c, p5d, p5e, p5f,
                          nrow = 3,
                          ncol = 2,
                          labels = c("a","b","c","d","e","f"),
                          widths = c(1,0.8)),
                p5_leg2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p5

ggsave(plot = p5, filename = "./figures/final_figures/Figure5.tif",
       device = "tiff", height = 9, width = 12, units = "in", bg = "white")

# Figure 5 supplement

p5_supp1a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 8,
                      combined_var = "none",
                      show_null_model = FALSE)
p5_supp1a

p5_supp1b <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p5_supp1b

p5_supp1c <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "r2",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none",
                      show_null_model = FALSE)
p5_supp1c

p5_supp1d <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "r2",
                      show_legend = FALSE)
p5_supp1d

p5_supp1e <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "High chl-a variability",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 8,
                      combined_var = "none",
                      show_null_model = FALSE)
p5_supp1e

p5_supp1f <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "mae",
                      show_legend = FALSE)
p5_supp1f

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "none")

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p5_supp1_leg1 <- as_ggplot(leg1)
p5_supp1_leg1

leg_plot2 <- GrandMeanSkill(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean"),
                            viz_dates = pred_dates,
                            plot_title = "All horizons",
                            viz_metric = "mae",
                            show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p5_supp1_leg2 <- as_ggplot(leg2)
p5_supp1_leg2


p5_supp1 <- ggarrange(p5_supp1_leg1,
                ggarrange(p5_supp1a, p5_supp1b, p5_supp1c, p5_supp1d, p5_supp1e, p5_supp1f,
                          nrow = 3,
                          ncol = 2,
                          labels = c("a","b","c","d","e","f"),
                          widths = c(1,0.8)),
                p5_supp1_leg2,
                ncol = 3,
                widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p5_supp1

ggsave(plot = p5_supp1, filename = "./figures/final_figures/Figure5_supp1.tif",
       device = "tiff", height = 9, width = 12, units = "in", bg = "white")

# additional Fig 5 code (low variability)

mod_out_all_var <- out %>%
  left_join(., var_df, by = "datetime") %>%
  select(-Chla_ugL_mean)

low_var <- var_df %>%
  filter(var_bin == "low")

mod_out_low_var <- out %>%
  filter(datetime %in% low_var$datetime)

p5a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_low_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Low chl-a variability",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      combined_var = "none")
p5a

p5b <- GrandMeanSkill(observations = obs, 
                      model_output = mod_out_low_var, 
                      forecast_horizon = 35,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      viz_dates = pred_dates,
                      plot_title = "All horizons",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p5b <- p5b +
  annotate("text",x = 8.2, y = "LSTM", label = "*", size = 10, vjust = 0.8)
p5b


# Figure 6
source("./code/function_library/visualization/CompareWithAndWithoutDrivers.R")

# TSLM overall
p6a <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = out, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "All predictions",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "TSLM",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(10)))
p6a

# GAM overall
p6b <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = out, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("GAM","GAM (no drivers)","GAM (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "All predictions",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "GAM",
                                    best_performing_horizons = data.frame(from = c(11),
                                                                          to = c(13)))
p6b

# ARIMA mixed
mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p6c <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_mixed, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("ARIMA","ARIMA (no drivers)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Mixed period",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "ARIMA",
                                    best_performing_horizons = data.frame(from = c(20),
                                                                          to = c(23)))
p6c

# MARS onset
onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p6d <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_onset, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("MARS","MARS (no drivers)","MARS (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Stratification onset",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "MARS",
                                    best_performing_horizons = data.frame(from = c(4,14,29),
                                                                          to = c(4,15,35)))
p6d

# Prophet stratified
strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p6e <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_strat, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("Prophet","Prophet (no drivers)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Stratified",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "Prophet",
                                    best_performing_horizons = data.frame(from = c(19),
                                                                          to = c(32)))
p6e

# NNETAR decline

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p6f <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_decline, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("NNETAR","NNETAR (no drivers)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Stratification decline",
                                    viz_metric = "rmse",
                                    show_legend = TRUE,
                                    combined_var = "none",
                                    parent_model = "NNETAR",
                                    best_performing_horizons = data.frame(from = c(15),
                                                                          to = c(35)))
p6f


p6 <- ggarrange(p6a, p6b, p6c, p6d, p6e, p6f,
                nrow = 3, ncol = 2,
                widths = c(1, 1, 1),
                labels = c("a","b","c","d","e","f")
) 

p6

ggsave(plot = p6, filename = "./figures/final_figures/Figure6.tif",
       device = "tiff", height = 8, width = 12, units = "in", bg = "white")

# Figure 6 supplements

# TSLM overall
p6_supp1a <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = out, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "All predictions",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "TSLM",
                                          best_performing_horizons = data.frame(from = c(1),
                                                                                to = c(18)))
p6_supp1a

# XGBoost overall
p6_supp1b <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = out, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("XGBoost","XGBoost (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "All predictions",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "XGBoost",
                                          best_performing_horizons = data.frame(from = c(NA),
                                                                                to = c(NA)))
p6_supp1b

# XGBoost mixed
mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p6_supp1c <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = mod_out_mixed, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("XGBoost","XGBoost (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "Mixed period",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "XGBoost",
                                          best_performing_horizons = data.frame(from = c(9,11),
                                                                                to = c(9,22)))
p6_supp1c

# TSLM onset
onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p6_supp1d <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = mod_out_onset, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "Stratification onset",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "TSLM",
                                          best_performing_horizons = data.frame(from = c(1),
                                                                                to = c(35)))
p6_supp1d

# TSLM stratified
strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

p6_supp1e <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = mod_out_strat, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "Stratified",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "TSLM",
                                          best_performing_horizons = data.frame(from = c(2),
                                                                                to = c(20)))
p6_supp1e

# TSLM decline

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p6_supp1f <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = mod_out_decline, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                          viz_dates = pred_dates,
                                          plot_title = "Stratification decline",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = "TSLM",
                                          best_performing_horizons = data.frame(from = c(1,6,24,30),
                                                                                to = c(1,22,26,32)))
p6_supp1f


p6_supp1 <- ggarrange(p6_supp1a, p6_supp1b, p6_supp1c, p6_supp1d, p6_supp1e, p6_supp1f,
                      nrow = 3, ncol = 2,
                      widths = c(1, 1, 1),
                      labels = c("a","b","c","d","e","f")
) 

p6_supp1

ggsave(plot = p6_supp1, filename = "./figures/final_figures/Figure6_supp1.tif",
       device = "tiff", height = 8, width = 12, units = "in",bg = "white")

# Figure 7
source("./code/function_library/visualization/CompareKGML.R")

# overall
p7a <- CompareKGML(observations = obs, 
                   model_output = out, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "All predictions",
                   viz_metric = "rmse",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(NA),
                                                         to = c(NA)))
p7a

# GLM-AED mixed
mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p7b <- CompareKGML(observations = obs, 
                   model_output = mod_out_mixed, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "Mixed period",
                   viz_metric = "rmse",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(4),
                                                         to = c(13)))
p7b

# high chl-a variability 2022-2023
obs_var <- obs %>%
  mutate(delta = c(NA,abs(diff(Chla_ugL_mean, na.rm = TRUE)))) 

dens <- density(obs_var$delta, na.rm = TRUE)
q90 <- quantile(obs_var$delta, 0.90, na.rm = TRUE)
var_df <- obs_var %>%
  mutate(var_bin = ifelse(delta > q90, "high","low"))

high_var <- var_df %>%
  filter(var_bin == "high")

mod_out_high_var <- out %>%
  filter(datetime %in% high_var$datetime)

p7c <- CompareKGML(observations = obs, 
                   model_output = mod_out_high_var, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "High chl-a variability",
                   viz_metric = "rmse",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(3,5),
                                                         to = c(3,5)),
                   add_vline = TRUE,
                   vline_intercept = 8)
p7c

p7 <- ggarrange(p7a, p7b, p7c,
                nrow = 3, ncol = 1,
                labels = c("a","b","c")
) 

p7

ggsave(plot = p7, filename = "./figures/final_figures/Figure7.tif",
       device = "tiff", height = 12, width = 7, units = "in")

# Figure 7 supplement

# supplement 1

# overall
p7_supp1a <- CompareKGML(observations = obs, 
                   model_output = out, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "All predictions",
                   viz_metric = "r2",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(NA),
                                                         to = c(NA)))
p7_supp1a

# GLM-AED mixed
mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p7_supp1b <- CompareKGML(observations = obs, 
                   model_output = mod_out_mixed, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "Mixed period",
                   viz_metric = "r2",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(4),
                                                         to = c(13)))
p7_supp1b

# high chl-a variability 2022-2023
obs_var <- obs %>%
  mutate(delta = c(NA,abs(diff(Chla_ugL_mean, na.rm = TRUE)))) 

dens <- density(obs_var$delta, na.rm = TRUE)
q90 <- quantile(obs_var$delta, 0.90, na.rm = TRUE)
var_df <- obs_var %>%
  mutate(var_bin = ifelse(delta > q90, "high","low"))

high_var <- var_df %>%
  filter(var_bin == "high")

mod_out_high_var <- out %>%
  filter(datetime %in% high_var$datetime)

p7_supp1c <- CompareKGML(observations = obs, 
                   model_output = mod_out_high_var, 
                   forecast_horizon = forecast_horizon,
                   model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence"),
                   viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                   plot_title = "High chl-a variability",
                   viz_metric = "r2",
                   show_legend = TRUE,
                   best_performing_horizons = data.frame(from = c(3,5),
                                                         to = c(3,5)))
p7_supp1c

p7_supp1 <- ggarrange(p7_supp1a, p7_supp1b, p7_supp1c,
                nrow = 3, ncol = 1,
                labels = c("a","b","c")
) 

p7_supp1

ggsave(plot = p7_supp1, filename = "./figures/final_figures/Figure7_supp1.tif",
       device = "tiff", height = 12, width = 7, units = "in")

# supplement 2

# overall
p7_supp2a <- CompareKGML(observations = obs, 
                         model_output = out, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                         viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                         plot_title = "All predictions",
                         viz_metric = "mae",
                         show_legend = TRUE,
                         best_performing_horizons = data.frame(from = c(NA),
                                                               to = c(NA)))
p7_supp2a

# GLM-AED mixed
mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

p7_supp2b <- CompareKGML(observations = obs, 
                         model_output = mod_out_mixed, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence","DOY","historical mean"),
                         viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                         plot_title = "Mixed period",
                         viz_metric = "mae",
                         show_legend = TRUE,
                         best_performing_horizons = data.frame(from = c(4,10),
                                                               to = c(8,10)))
p7_supp2b

# high chl-a variability 2022-2023
obs_var <- obs %>%
  mutate(delta = c(NA,abs(diff(Chla_ugL_mean, na.rm = TRUE)))) 

dens <- density(obs_var$delta, na.rm = TRUE)
q90 <- quantile(obs_var$delta, 0.90, na.rm = TRUE)
var_df <- obs_var %>%
  mutate(var_bin = ifelse(delta > q90, "high","low"))

high_var <- var_df %>%
  filter(var_bin == "high")

mod_out_high_var <- out %>%
  filter(datetime %in% high_var$datetime)

p7_supp2c <- CompareKGML(observations = obs, 
                         model_output = mod_out_high_var, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("NNETAR","GLM-AED","NNETAR-KGML","persistence"),
                         viz_dates = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day"),
                         plot_title = "High chl-a variability",
                         viz_metric = "mae",
                         show_legend = TRUE,
                         best_performing_horizons = data.frame(from = c(3),
                                                               to = c(8)),
                         add_vline = TRUE,
                         vline_intercept = 8)
p7_supp2c

p7_supp2 <- ggarrange(p7_supp2a, p7_supp2b, p7_supp2c,
                      nrow = 3, ncol = 1,
                      labels = c("a","b","c")
) 

p7_supp2

ggsave(plot = p7_supp2, filename = "./figures/final_figures/Figure7_supp2.tif",
       device = "tiff", height = 12, width = 7, units = "in")

#need to figure out how to detach legend from this and make it a separate
#plot, then add
#focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06","2023-05-16","2023-07-31","2023-10-02","2023-11-11"),
# ADD DROP-ONE-OUT VALIDATION TO THIS FIGURE!
p6 <- PerformanceRelativeToBloom(observations = obs,
                           model_output = out,
                           variable_name = "chlorophyll-a",
                           max_horizon_past = -35,
                           score = "rmse",
                           focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06","2023-05-16","2023-07-31","2023-10-02","2023-11-11"),
                           data_plot = FALSE,
                           best_models_only = TRUE,
                           model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel"))
p6
ggsave(p6, filename = "./figures/BestModelsPerformanceRelativeToBloom.png",
       device = "png", height = 5, width = 7, units = "in")

p7 <- OneHorizonTimeseries(observations = obs, 
                                 model_output = out, 
                                 forecast_horizon = 7,
                           model_ids = c("GLM-AED"))
p7
ggsave(p7, filename = "./figures/predictionHorizon7DaysGLMAED.png",
       device = "png", height = 4, width = 8, units = "in")

# OneDProcessModel

source("./code/function_library/visualization/OneDProcessModelInputData.R")
input_data <- read_csv("./data/data_processed/1DProcessModel.csv")
p_OneDProcessModelInputData <- OneDProcessModelInputData(input_data)
p_OneDProcessModelInputData
ggsave(p_OneDProcessModelInputData, filename = "./figures/OneDProcessModelInputData.png",
       device = "png", height = 4, width = 6.5, units = "in")

# GLM-AED

# functional relationships
PlotMonodLightLimitation(I_K = 250, xlim = c(0,600), save_plot = FALSE)
PlotRespiration(R_resp = 0.17, theta_resp = 1.01, xlim = c(1,30), save_plot = FALSE)
PlotNLimitation(K_N = 2, N_0 = 0.00, xlim = c(0,10), save_plot = FALSE)
PlotPLimitation(K_P = 0.08, P_0 = 0.08, xlim = c(0,0.2), save_plot = FALSE)

# set parameters for temperature limitation
g1 <- list(T_std = 10,
           T_opt = 25,
           T_max = 35,
           Ts = 10,
           To = 25,
           Tm = 35,
           v = 1.02,
           theta = 1.02)
g2 <- list(T_std = 10,
           T_opt = 12,
           T_max = 30,
           Ts = 10,
           To = 12,
           Tm = 30,
           v = 1.02,
           theta = 1.02)
PlotTemperatureLimitation2Groups(g1_parms = g1, g2_parms = g2, save_plot = FALSE)

# comparing phyto sensors
EXOChlaVsFluoroProbe(fp_data = "./data/data_raw/FP_2018_2023_FCR50.csv", 
                     chla_data = "./data/data_processed/chla_obs.csv", 
                     save_plot = TRUE)

# looking at nutrient limitation
PlotNutrientLimitation(chem_data = "./data/data_raw/chemistry_2013_2023.csv", save_plot = TRUE)

# assess calibration model runs


# FluoroProbe heatmap
fp_profiles <- read_csv("./data/data_raw/FP_2018_2023_profiles_FCR50.csv") %>%
  filter(date(DateTime) >= "2018-08-06" & date(DateTime) <= "2021-12-31")

p8 <- flora_heatmap(fp_data = fp_profiles, reservoir = "FCR", years = c(2019:2021), z = "non_cyano")
ggsave(p8, filename = "./figures/FluoroProbeHeatmap.png",
       device = "png", height = 2, width = 7, units = "in")

# illustrate initial conditions updating
p9 <- ExampleInitialConditionsUpdating()
ggsave(p1, filename = "./figures/ExampleInitialConditionsUpdating.png",
       device = "png", height = 3.5, width = 6, units = "in")

## the inevitable special case for GLM-AED ----

## Notes on various GLM-AED prediction runs
#' 1. GLMAED_20240516.csv calibration completed in April 2024 and driver files from CCC in late
#' May 2024; resulted in way-too-high predictions during drawdown in 2022
#' 
#' 2. GLMAED_20240517.csv calibration completed in April 2024 and driver files modified
#' from CCC's version to have lower N in an attempt to rectify too-high predictions
#' THIS METHOD SHOULD BE CONSIDERED A BACK-OF-ENVELOPE HACK, NOT PUBLISHABLE
#' 
#' 3. GLMAED_20240712.csv calibration completed in May 2024 and driver files modified
#' from CCC's version to have higher inflow by taking into account weir overtopping
#' during drawdown in 2022
#' THIS METHOD COULD BE PUBLISHED
#' 
#' 4. GLMAED_20240717.csv calibration completed in May 2024 and driver files modified
#' from CCC's version to have 10x higher inflow during drawdown in 2022

csv_fils <- list.files("model_output", pattern = "GLMAED", full.names = TRUE) 

dat <- map_df(csv_fils, read_csv, .id = "config") %>%
  mutate(scenario = ifelse(config == "1","April 2024 calibration w/ unaltered driver files",
                         ifelse(config == "2","April 2024 calibration w/ 2023 inflow N",
                                ifelse(config == "3","July 2024 calibration w/ V-notch + rectangle discharge calc.",
                                       ifelse(config == "4","July 2024 calibration w/ 10x higher inflow",
                                              ifelse(config == "5","July 2024 calibration w/ 100x higher inflow","July 2024 calibration w/ 100x higher inflow and DOCr")))))) %>%
  filter(!config %in% c(1,2))
unique(dat$config)

h7 <- dat %>%
  mutate(reference_datetime = date(reference_datetime)) %>%
  filter(datetime - reference_datetime == 7)
obs <- read_csv("./data/data_processed/chla_obs.csv") %>%
  filter(datetime %in% h7$datetime)

ggplot()+
  geom_point(data = obs, aes(x = datetime, y = Chla_ugL_mean))+
  geom_line(data = h7, aes(x = datetime, y = prediction, group = scenario, color = scenario))+
  theme_bw()+
  ggtitle("7-day-ahead GLM-AED predictions")+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2))

#reformat model output
h8 <- dat %>% 
  mutate(reference_datetime = date(reference_datetime)) %>%
  group_by(scenario, config, reference_datetime) %>%
  mutate(horizon = datetime - reference_datetime) %>%
  ungroup() %>%
  separate(horizon, c("horizon"), sep = " ") %>%
  left_join(., obs, by = "datetime") %>%
  group_by(scenario, config, horizon) %>%
  summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE))) %>%
  filter(!horizon == 0) %>%
  mutate(horizon = as.numeric(horizon)) %>%
  arrange(config, scenario, horizon)

ggplot(data = h8, aes(x = horizon, y = rmse, group = scenario, color = scenario))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2))

scen_inf_fils <- list.files("code/model_files/GLM-AED/", pattern = "fake_inf", full.names = TRUE,
                       recursive = TRUE)
inf_fil <- list.files("code/model_files/GLM-AED/prediction/inputs/", 
                      pattern = "FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv",
                      full.names = TRUE)
inf_fils <- c(inf_fil, scen_inf_fils)

dat <- map_df(inf_fils, read_csv, .id = "config") %>%
  mutate(scenario = ifelse(config == "1","July 2024 calibration w/ V-notch + rectangle discharge calc.",
                           ifelse(config == "2","July 2024 calibration w/ 100x higher inflow",
                                  ifelse(config == "3","July 2024 calibration w/ 100x higher inflow and DOCr","July 2024 calibration w/ 10x higher inflow")))) %>%
  filter(time >= "2022-01-01") %>%
  pivot_longer(cols = -c(time, config, scenario))
unique(dat$scenario)

ggplot(data = dat, aes(x = time, y = value, group = scenario, color = scenario))+
  geom_line()+
  facet_wrap(facets = vars(name), scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")

focus <- dat %>%
  filter(name %in% c("FLOW","OGM_docr"))

ggplot(data = focus, aes(x = time, y = value, group = scenario, color = scenario))+
  geom_line()+
  facet_wrap(facets = vars(name), scales = "free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2))

## the inevitable additional special case for the LSTM ----

cal_dates = c("2018-08-06","2021-12-31")
start_cal <- date(cal_dates[1])
stop_cal <- date(cal_dates[2])

pred.df <- read_csv("./model_output/LSTM_tuning_09SEP24.csv")

df <- read_csv("./code/model_files/LSTM/LSTM_dataset.csv") %>%
  select(datetime, Chla_ugL_mean) %>%
  filter(datetime >= start_cal & datetime <= stop_cal)

single_horizon_0 <- pred.df %>%
  filter(horizon == 0)

LSTM_ts_0 <- ggplot()+
  xlab("")+
  ylab("Chla (ug/L)")+
  geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
  geom_line(data = single_horizon_0, aes(x = datetime, y = prediction, group = param_set, color = param_set))+
  labs(color = "Parameter set", fill = NULL)+
  theme_classic()+
  ggtitle("Predictions for 0 days ahead")
LSTM_ts_0

single_horizon_20 <- pred.df %>%
  filter(horizon == 20)

LSTM_ts_20 <- ggplot()+
  xlab("")+
  ylab("Chla (ug/L)")+
  geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
  geom_line(data = single_horizon_20, aes(x = datetime, y = prediction, group = param_set, color = param_set))+
  labs(color = "Parameter set", fill = NULL)+
  theme_classic()+
  ggtitle("Predictions for 20 days ahead")
LSTM_ts_20
LSTM_diagnostics <- ggarrange(plotlist = list(LSTM_ts_0, LSTM_ts_20),
                              labels = "auto",
                              nrow = 2)
LSTM_diagnostics
ggsave(LSTM_diagnostics, filename = "./figures/LSTM_diagnostics.png",
       height = 6, width = 8, units = "in")

#reformat model output
lstm_rmse <- pred.df %>% 
  left_join(., df, by = "datetime") %>%
  group_by(param_set, horizon) %>%
  summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE))) %>%
  arrange(param_set, horizon) %>%
  add_column(name = "Parameter scenarios")

horizon_rmse <- ggplot()+
  geom_line(data = lstm_rmse, aes(x = horizon, y = rmse,
                               group = param_set, color = param_set),
            linewidth = 1)+
  xlab("Prediction horizon (days)")+
  ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))+
  labs(color = "Parameter set")+
  ggtitle("RMSE vs. horizon for calibration period")+
  theme_bw()
horizon_rmse
ggsave(plot = horizon_rmse, filename = "./figures/LSTM_tuning_horizon_vs_rmse.png", device = "png",
       height = 4, width = 6, units = "in")

lstm_grand_rmse <- pred.df %>% 
  left_join(., df, by = "datetime") %>%
  group_by(param_set) %>%
  summarize(rmse = sqrt(mean((Chla_ugL_mean - prediction)^2, na.rm = TRUE))) %>%
  arrange(param_set) %>%
  add_column(name = "Parameter scenarios")

LSTM_bp <- ggplot()+
  geom_boxplot(data = lstm_grand_rmse, aes(x = name, y = rmse))+
  geom_jitter(data = lstm_grand_rmse, aes(x = name, y = rmse, color = param_set))+
  theme_classic()+
  xlab("")+
  ylab("RMSE (ug/L)")+
  theme(legend.position = "none")+
  ggtitle("RMSE aggregated \n for all horizons")
LSTM_bp
ggsave(plot = LSTM_bp, filename = "./figures/LSTM_tuning_grand_rmse.png", device = "png",
       height = 6, width = 3, units = "in")

lstm_plot <- plot_grid(LSTM_ts, LSTM_bp, nrow = 1, rel_widths = c(4,1))
lstm_plot

best_model <- lstm_grand_rmse %>%
  slice(which.min(rmse))
best_params <- pred.df %>%
  slice(which(param_set == 64)) %>%
  select(epochs, dropout, num_layers, hidden_feature_size, weight_decay) %>%
  distinct()
