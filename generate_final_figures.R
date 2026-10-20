# Generate final figures
# Author: Mary Lofton
# Date last updated: 27MAR25

# Purpose: Generate final figures for manuscript 
# (and many of the Appendix S2 figures as well)

# Set-up ----

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggpubr, viridis)

# Read in data and tidy up model names
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
# Check model names
unique(out$model_id)

# Calculate ensemble predictions
ens <- out %>%
  filter(model_id %in% c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML")) %>%
  group_by(reference_datetime, datetime) %>%
  summarize(prediction = mean(prediction, na.rm = TRUE)) %>%
  add_column(model_id = "ensemble", model_type = "ensemble", variable = "chlorophyll-a") 

# Final model output dataframe
out <- bind_rows(out, ens)

# Read in lake observational data
obs <- read_csv("./data/data_processed/chla_obs.csv")
input <- read_csv("./data/data_processed/ARIMA.csv")
ss_data <- read_csv("./data/data_processed/schmidt_stability.csv")

#Set arguments across all plotting functions
forecast_horizon = 14
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")

#Plots ----

# Figure 1 ----
source("./code/function_library/visualization/PlotObservations.R")
p1a <- PlotObservations(observations = obs, 
                        pred_only = FALSE,
                        focal_dates = c("2022-01-10","2022-04-10","2022-07-10","2022-09-10","2023-01-10","2023-10-15"),
                        forecast_horizon = forecast_horizon,
                       plotly = FALSE,
                       train_test_box = TRUE,
                       training_dates = c("2018-08-06","2021-12-31"),
                       testing_dates = c("2022-01-01","2023-12-31"),
                       focal_dates_geom = "LINE")
p1a

source("./code/function_library/visualization/PlotSchmidtStability.R")
p1b <- PlotSchmidtStability(ss_data = ss_data,
                            testing_dates = c("2022-01-01","2023-12-31"))
p1b

source("./code/function_library/visualization/PlotChlaVariability.R")
p1c <- PlotChlaVariability(obs)
p1c$p

p1 <- ggarrange(p1a, 
                  ggarrange(p1b, p1c$p, ncol = 2, labels = c("(b)","(c)")), 
                  nrow = 2,
                labels = "(a)"
                )
p1
ggsave(plot = p1, filename = "./figures/final_figures/Figure1.tif",
       device = "tiff", height = 6, width = 10, units = "in")

# Figure 2 ----

source("./code/function_library/visualization/ExamplePrediction.R")
plot_cols <- viridis(6, option = "turbo")
focal_dates = c("2022-01-10","2022-04-10","2022-07-10","2022-09-10","2023-01-10","2023-10-24") # edited date for last panel to show variability with 14-day horizon

reference_datetime_a = focal_dates[1]
p2a <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_a, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(a) Mixed: ",
                         rect_color = plot_cols[1],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2a

reference_datetime_b = focal_dates[2]
p2b <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(b) Onset: ",
                         rect_color = plot_cols[2],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2b

reference_datetime_c = focal_dates[3]
p2c <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_c, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(c) Stratified: ",
                         rect_color = plot_cols[3],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2c

reference_datetime_d = focal_dates[4]
p2d <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_d, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(d) Decline: ",
                         rect_color = plot_cols[4],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2d

reference_datetime_e = focal_dates[5]
p2e <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_e, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(e) Low variability: ",
                         rect_color = plot_cols[5],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2e

reference_datetime_f = focal_dates[6]
p2f <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_f, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "(f) High variability: ",
                         rect_color = plot_cols[6],
                         ylim_values = c(0,40),
                         show_shapes = FALSE)
p2f

leg_plot <- ExamplePrediction(observations = obs, 
                              model_output = out, 
                              reference_datetime = reference_datetime_b, 
                              forecast_horizon = forecast_horizon,
                              model_ids = c("persistence","TSLM","GLM-AED","NNETAR-KGML","ensemble"),
                              show_legend = TRUE,
                              sub_panel_label = "b",
                              rect_color = "black",
                              ylim_values = c(0,40),
                              show_shapes = FALSE)

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
                                             "GLM-AED","MARS","randomForest", # removed 1D process model
                                             "GAM","NNETAR-KGML","ensemble"),
                               show_legend = FALSE,
                               sub_panel_label = "(a) Mixed: ",
                               rect_color = plot_cols[1],
                               ylim_values = c(0,40),
                               show_shapes = TRUE)
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
                               sub_panel_label = "(b) Onset: ",
                               rect_color = plot_cols[2],
                               ylim_values = c(0,40),
                               show_shapes = TRUE)
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
                               sub_panel_label = "(c) Stratified: ",
                               rect_color = plot_cols[3],
                               ylim_values = c(0,40),
                               show_shapes = TRUE)
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
                               sub_panel_label = "(d) Decline: ",
                               rect_color = plot_cols[4],
                               ylim_values = c(0,40),
                               show_shapes = TRUE)
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
                               sub_panel_label = "(e) Low variability: ",
                               rect_color = plot_cols[5],
                               ylim_values = c(0,40),
                               show_shapes = TRUE)
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
                               sub_panel_label = "(f) High variability: ",
                               rect_color = plot_cols[6],
                               ylim_values = c(0,65),
                               show_shapes = TRUE)
p2_supp1f

p2_supp_leg_plot <- ExamplePrediction(observations = obs, 
                                      model_output = out, 
                                      reference_datetime = reference_datetime_b, 
                                      forecast_horizon = forecast_horizon,
                                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                                    "GAM","NNETAR-KGML","ensemble"),
                                      show_legend = TRUE,
                                      sub_panel_label = "b",
                                      rect_color = "black",
                                      ylim_values = c(0,40),
                                      show_shapes = TRUE)

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
       device = "tiff", height = 11, width = 11, units = "in",bg = "white")

# Figure 3 ----

source("./code/function_library/visualization/SkillVsHorizon.R")
source("./code/function_library/visualization/OneHorizonSkill.R")

p3a <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 14, # changing forecast horizon
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest", # eliminating 1D process model
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 26,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = FALSE)
p3a

p3b <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 2, #changing forecast horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,10))
p3b <- p3b +
  #annotate("text",x = 10.5, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 12.5, size = 1)
p3b

p3c <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 5, #changing forecast horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,10))
p3c <- p3c +
  #annotate("text",x = 10.5, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 12.5, size = 1)
p3c

p3d <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 10, #changing forecast horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,10))
p3d <- p3d +
  #annotate("text",x = 10.5, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 10.5, size = 1)
p3d


p3e <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 14, # changing horizon
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest", # eliminating 1D process model
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
p3e

p3f <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 2, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-0.3,1))
p3f <- p3f +
  #annotate("text",x = -0.35, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 12.5, size = 1)
p3f

p3g <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 5, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-0.3,1))
p3g <- p3g +
  #annotate("text",x = -0.35, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 12.5, size = 1)
p3g

p3h <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 10, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-0.3,1))
p3h <- p3h +
  #annotate("text",x = -0.35, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 10.5, size = 1)
p3h

p3i <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = 14, # changing horizon
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest", #removing 1D process model
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 26,
                      combined_var = "none",
                      fixed_ylim = FALSE)
p3i

p3j <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 2, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,8))
p3j <- p3j +
  #annotate("text",x = 7.3, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 12.5, size = 1)
p3j

p3k <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 5, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,8))
p3k <- p3k +
  #annotate("text",x = 7.3, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 13.5, size = 1)
p3k

p3l <- OneHorizonSkill(observations = obs, 
                       model_output = out, 
                       forecast_horizon = 10, # changing horizon
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest", # removing 1D process model
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,8))
p3l <- p3l +
  #annotate("text",x = 7.3, y = "LSTM", label = "*", size = 10, vjust = 0.8) +
  geom_hline(yintercept = 10.5, size = 1)
p3l

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","MARS","randomForest",
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
p3_leg1 <- as_ggplot(leg1)
p3_leg1

leg_plot2 <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = forecast_horizon,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "All horizons up to 14 days",
                             viz_metric = "r2",
                             show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p3_leg2 <- as_ggplot(leg2)
p3_leg2


p3 <- ggarrange(ggarrange(p3_leg1,p3_leg2,
                          nrow = 2,
                          ncol = 1),
                ggarrange(p3a, p3b, p3c, p3d, p3e, p3f, p3g, p3h, p3i, p3j, p3k, p3l,
                          nrow = 3,
                          ncol = 4,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                          widths = c(1,0.9, 0.9, 0.9)),
                ncol = 2,
                widths = c(0.2, 1)
) #+ bgcolor("white")

p3

ggsave(plot = p3, filename = "./figures/final_figures/Figure3.tif",
       device = "tiff", height = 8, width = 12.5, units = "in",bg = "white")

# Figure 3 supplements

p3_supp1a <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 14,
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

p3_supp1b <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "2 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,10))

p3_supp1b

p3_supp1c <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "5 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,10))

p3_supp1c

p3_supp1d <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "10 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,10))

p3_supp1d

p3_supp1e <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 14,
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
p3_supp1e

p3_supp1f <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "2 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-0.3,1))

p3_supp1f

p3_supp1g <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "5 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-0.3,1))

p3_supp1g

p3_supp1h <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "10 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-0.3,1))

p3_supp1h

p3_supp1i <- SkillVsHorizon(observations = obs, 
                            model_output = out, 
                            forecast_horizon = 14,
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
p3_supp1i

p3_supp1j <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "2 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0,7))

p3_supp1j

p3_supp1k <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "5 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0,7))

p3_supp1k

p3_supp1l <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean"),
                             viz_dates = pred_dates,
                             plot_title = "10 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0,7))

p3_supp1l

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

p3_supp1_leg_plot2 <- OneHorizonSkill(observations = obs, 
                                      model_output = out, 
                                      forecast_horizon = forecast_horizon,
                                      model_ids = c("DOY","persistence","historical mean"),
                                      viz_dates = pred_dates,
                                      plot_title = "All horizons up to 14 days",
                                      viz_metric = "mae",
                                      show_legend = TRUE)

# Extract the legend. Returns a gtable
p3_supp1_leg2 <- get_legend(p3_supp1_leg_plot2)

# Convert to a ggplot and print
p3_supp1_leg2 <- as_ggplot(p3_supp1_leg2) 
p3_supp1_leg2 <- p3_supp1_leg2 #+ theme(plot.background = element_blank()) + bgcolor("white")
p3_supp1_leg2

p3_supp1 <- ggarrange(ggarrange(p3_supp1_leg1,p3_supp1_leg2,
                                nrow = 2,
                                ncol = 1),
                      ggarrange(p3_supp1a, p3_supp1b, p3_supp1c, p3_supp1d, p3_supp1e, p3_supp1f, p3_supp1g, p3_supp1h, p3_supp1i, p3_supp1j, p3_supp1k, p3_supp1l,
                                nrow = 3,
                                ncol = 4,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                                widths = c(1,0.9, 0.9, 0.9)),
                      ncol = 2,
                      widths = c(0.2, 1)
) #+ bgcolor("white")

p3_supp1

ggsave(plot = p3_supp1, filename = "./figures/final_figures/Figure3_supp1.tif",
       device = "tiff", height = 8, width = 12.5, units = "in",bg = "white")



# Figure 4 ----

source("./code/function_library/visualization/SkillVsHorizon.R")

mixed <- ss_data %>%
  filter(strat_bin == "mixed")

mod_out_mixed <- out %>%
  filter(datetime %in% mixed$datetime)

onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

strat <- ss_data %>%
  filter(strat_bin == "stratified")

mod_out_strat <- out %>%
  filter(datetime %in% strat$datetime)

decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p4a <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(1,10)) #r2 -1.5,1; mae 0,8
p4a

p4b <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "rmse",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(1,10))
p4b

p4c <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(1,10))
p4c

p4d <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(1,10))
p4d

# r2

p4e <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(-0.5, 1)) #r2 -1.5,1; mae 0,8
p4e

p4f <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(-0.5, 1))
p4f

p4g <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(-0.5, 1))
p4g

p4h <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(-0.5, 1))
p4h

# mae

p4i <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_mixed, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(0,7)) #r2 -1.5,1; mae 0,8
p4i

p4j <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_onset, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
                                    "GAM","NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "Stratification onset",
                      viz_metric = "mae",
                      show_legend = FALSE,
                      make_combined_bestmodel_legend = FALSE,
                      add_vline = FALSE,
                      vline_intercept = 18,
                      combined_var = "none",
                      show_null_model = TRUE,
                      fixed_ylim = TRUE,
                      ylims = c(0,7))
p4j

p4k <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_strat, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(0,7))
p4k

p4l <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_decline, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest",
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
                      ylims = c(0,7))
p4l

mod_out_mixed$strat_bin = "mixed"
mod_out_onset$strat_bin = "onset"
mod_out_strat$strat_bin = "strat"
mod_out_decline$strat_bin = "decline"

mod_out_all_strat <- bind_rows(mod_out_mixed, mod_out_onset, mod_out_strat, mod_out_decline)

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_all_strat, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","MARS","randomForest",
                                          "GAM","NNETAR-KGML","ensemble"),
                            best_models_only = TRUE,
                            viz_dates = pred_dates,
                            plot_title = "",
                            viz_metric = "rmse",
                            show_legend = TRUE,
                            make_combined_bestmodel_legend = TRUE,
                            combined_var = "strat_and_metric",
                            show_null_model = TRUE,
                            add_vline = FALSE,
                            fixed_ylim = TRUE,
                            ylims = c(1,12))

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p4_leg <- as_ggplot(leg1)
p4_leg

p4 <- ggarrange(ggarrange(p4_leg,
                          nrow = 1,
                          ncol = 1),
                ggarrange(p4a, p4b, p4c, p4d, p4e, p4f, p4g, p4h, p4i, p4j, p4k, p4l,
                          nrow = 3,
                          ncol = 4,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                          widths = c(1,1,1)),
                ncol = 2,
                widths = c(0.2, 1)
) #+ bgcolor("white")

p4

ggsave(plot = p4, filename = "./figures/final_figures/Figure4.tif",
       device = "tiff", height = 7, width = 12, units = "in", bg = "white")

# Figure 4 supplements

source("./code/function_library/visualization/OneHorizonSkill.R")

# RMSE at 2, 5, 10 day horizons in different periods

# mixed period
p4_supp1a <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 2 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1a <- p4_supp1a +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 11.5, size = 1)
p4_supp1a

p4_supp1b <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 5 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1b <- p4_supp1b +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp1b

p4_supp1c <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 10 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1c <- p4_supp1c +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 15.5, size = 1)
p4_supp1c

# stratification onset
p4_supp1d <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 2 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1d <- p4_supp1d +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp1d

p4_supp1e <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 5 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1e <- p4_supp1e +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 8.5, size = 1)
p4_supp1e

p4_supp1f <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 10 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1f <- p4_supp1f +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 8.5, size = 1)
p4_supp1f

# stratified period
p4_supp1g <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 2 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1g <- p4_supp1g +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 12.5, size = 1)
p4_supp1g

p4_supp1h <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 5 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1h <- p4_supp1h +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp1h

p4_supp1i <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 10 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1i <- p4_supp1i +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 9.5, size = 1)
p4_supp1i

# stratification decline
p4_supp1j <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 2 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1j <- p4_supp1j +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp1j

p4_supp1k <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 5 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1k <- p4_supp1k +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp1k

p4_supp1l <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 10 days ahead",
                             viz_metric = "rmse",
                             show_legend = FALSE,
                             xlims = c(0,12.5))
p4_supp1l <- p4_supp1l +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp1l

leg_plot2 <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = forecast_horizon,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "All horizons",
                             viz_metric = "mae",
                             show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p4_supp1_leg2 <- as_ggplot(leg2)
p4_supp1_leg2


p4_supp1 <- ggarrange(ggarrange(p4_supp1_leg2,
                                nrow = 1,
                                ncol = 1),
                      ggarrange(p4_supp1a, p4_supp1b, p4_supp1c, p4_supp1d, p4_supp1e, p4_supp1f, p4_supp1g, p4_supp1h, p4_supp1i, p4_supp1j, p4_supp1k, p4_supp1l,
                                nrow = 4,
                                ncol = 3,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                                widths = c(1, 1, 1, 1)),
                      ncol = 2,
                      widths = c(0.15, 1)
) #+ bgcolor("white")

p4_supp1

ggsave(plot = p4_supp1, filename = "./figures/final_figures/Figure4_supp1.tif",
       device = "tiff", height = 10, width = 15, units = "in",bg = "white")

# R2 at 2, 5, 10 day horizons in different periods

# mixed period
p4_supp2a <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 2 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2a <- p4_supp2a +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 11.5, size = 1)
p4_supp2a

p4_supp2b <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 5 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2b <- p4_supp2b +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp2b

p4_supp2c <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 10 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2c <- p4_supp2c +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 15.5, size = 1)
p4_supp2c

# stratification onset
p4_supp2d <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 2 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2d <- p4_supp2d +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp2d

p4_supp2e <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 5 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2e <- p4_supp2e +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 8.5, size = 1)
p4_supp2e

p4_supp2f <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 10 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2f <- p4_supp2f +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 8.5, size = 1)
p4_supp2f

# stratified period
p4_supp2g <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 2 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2, 1))
p4_supp2g <- p4_supp2g +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 12.5, size = 1)
p4_supp2g

p4_supp2h <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 5 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2,1))
p4_supp2h <- p4_supp2h +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp2h

p4_supp2i <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 10 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2, 1))
p4_supp2i <- p4_supp2i +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 9.5, size = 1)
p4_supp2i

# stratification decline
p4_supp2j <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 2 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2, 1))
p4_supp2j <- p4_supp2j +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp2j

p4_supp2k <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 5 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2, 1))
p4_supp2k <- p4_supp2k +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp2k

p4_supp2l <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 10 days ahead",
                             viz_metric = "r2",
                             show_legend = FALSE,
                             xlims = c(-2, 1))
p4_supp2l <- p4_supp2l +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp2l

leg_plot2 <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = forecast_horizon,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "All horizons",
                             viz_metric = "mae",
                             show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p4_supp2_leg2 <- as_ggplot(leg2)
p4_supp2_leg2


p4_supp2 <- ggarrange(ggarrange(p4_supp2_leg2,
                                nrow = 1,
                                ncol = 1),
                      ggarrange(p4_supp2a, p4_supp2b, p4_supp2c, p4_supp2d, p4_supp2e, p4_supp2f, p4_supp2g, p4_supp2h, p4_supp2i, p4_supp2j, p4_supp2k, p4_supp2l,
                                nrow = 4,
                                ncol = 3,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                                widths = c(1, 1, 1, 1)),
                      ncol = 2,
                      widths = c(0.15, 1)
) #+ bgcolor("white")

p4_supp2

ggsave(plot = p4_supp2, filename = "./figures/final_figures/Figure4_supp2.tif",
       device = "tiff", height = 10, width = 15, units = "in",bg = "white")


# MAE at 2, 5, 10 day horizons in different periods

# mixed period
p4_supp3a <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 2 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3a <- p4_supp3a +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 15.5, size = 1)
p4_supp3a

p4_supp3b <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 5 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3b <- p4_supp3b +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 15.5, size = 1)
p4_supp3b

p4_supp3c <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_mixed, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Mixed period: 10 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3c <- p4_supp3c +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp3c

# stratification onset
p4_supp3d <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 2 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3d <- p4_supp3d +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp3d

p4_supp3e <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 5 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3e <- p4_supp3e +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 10.5, size = 1)
p4_supp3e

p4_supp3f <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_onset, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification onset: 10 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3f <- p4_supp3f +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 10.5, size = 1)
p4_supp3f

# stratified period
p4_supp3g <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 2 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3g <- p4_supp3g +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 12.5, size = 1)
p4_supp3g

p4_supp3h <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 5 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3h <- p4_supp3h +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 14.5, size = 1)
p4_supp3h

p4_supp3i <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_strat, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratified period: 10 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3i <- p4_supp3i +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 9.5, size = 1)
p4_supp3i

# stratification decline
p4_supp3j <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 2,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 2 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3j <- p4_supp3j +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp3j

p4_supp3k <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 5,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 5 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3k <- p4_supp3k +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 13.5, size = 1)
p4_supp3k

p4_supp3l <- OneHorizonSkill(observations = obs, 
                             model_output = mod_out_decline, 
                             forecast_horizon = 10,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "GAM","NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "Stratification decline: 10 days ahead",
                             viz_metric = "mae",
                             show_legend = FALSE,
                             xlims = c(0, 9))
p4_supp3l <- p4_supp3l +
  #annotate("text",x = 11.8, y = "LSTM", label = "*", size = 10, vjust = 0.8) + #RMSE x = 11.8, r2 x = -0.24, MAE x = 8.5,
  geom_hline(yintercept = 9.5, size = 1)
p4_supp3l

leg_plot2 <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = forecast_horizon,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
                                           "NNETAR-KGML","ensemble"),
                             viz_dates = pred_dates,
                             plot_title = "All horizons",
                             viz_metric = "mae",
                             show_legend = TRUE)

# Extract the legend. Returns a gtable
leg2 <- get_legend(leg_plot2)

# Convert to a ggplot and print
p4_supp3_leg2 <- as_ggplot(leg2)
p4_supp3_leg2


p4_supp3 <- ggarrange(ggarrange(p4_supp3_leg2,
                                nrow = 1,
                                ncol = 1),
                      ggarrange(p4_supp3a, p4_supp3b, p4_supp3c, p4_supp3d, p4_supp3e, p4_supp3f, p4_supp3g, p4_supp3h, p4_supp3i, p4_supp3j, p4_supp3k, p4_supp3l,
                                nrow = 4,
                                ncol = 3,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                                widths = c(1, 1, 1, 1)),
                      ncol = 2,
                      widths = c(0.15, 1)
) #+ bgcolor("white")

p4_supp3

ggsave(plot = p4_supp3, filename = "./figures/final_figures/Figure4_supp3.tif",
       device = "tiff", height = 10, width = 15, units = "in",bg = "white")

# null models

p4_supp4a <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4a

p4_supp4b <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4b

p4_supp4c <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4c


p4_supp4d <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4d

p4_supp4e <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4e

p4_supp4f <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4f

p4_supp4g <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4g

p4_supp4h <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4h

p4_supp4i <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4i


p4_supp4j <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4j

p4_supp4k <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4k

p4_supp4l <- SkillVsHorizon(observations = obs, 
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p4_supp4l

mod_out_all_strat <- out %>%
  left_join(., ss_data, by = "datetime")

p4_supp4_leg_plot1 <- SkillVsHorizon(observations = obs, 
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
                                     add_vline = FALSE,
                                     fixed_ylim = FALSE)

# Extract the legend. Returns a gtable
p4_supp4_leg1 <- get_legend(p4_supp4_leg_plot1)

# Convert to a ggplot and print
p4_supp4_leg1 <- as_ggplot(p4_supp4_leg1)
p4_supp4_leg1

p4_supp4_leg_plot2 <- OneHorizonSkill(observations = obs, 
                                      model_output = mod_out_mixed, 
                                      forecast_horizon = forecast_horizon,
                                      model_ids = c("DOY","persistence","historical mean"),
                                      viz_dates = pred_dates,
                                      plot_title = "All horizons up to 21 days",
                                      viz_metric = "mae",
                                      show_legend = TRUE)

# Extract the legend. Returns a gtable
p4_supp4_leg2 <- get_legend(p4_supp4_leg_plot2)

# Convert to a ggplot and print
p4_supp4_leg2 <- as_ggplot(p4_supp4_leg2)
p4_supp4_leg2


p4_supp4 <- ggarrange(ggarrange(p4_supp4_leg1,p4_supp4_leg2,
                                nrow = 2,
                                ncol = 1,
                                heights = c(1,0.5)),
                      ggarrange(p4_supp4a, p4_supp4b, p4_supp4c, p4_supp4d, p4_supp4e, p4_supp4f, p4_supp4g, p4_supp4h, p4_supp4i, p4_supp4j, p4_supp4k, p4_supp4l,
                                nrow = 4,
                                ncol = 3,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                                widths = c(1,1,1)),
                      ncol = 2,
                      widths = c(0.2, 1)
) #+ bgcolor("white")

p4_supp4

ggsave(plot = p4_supp4, filename = "./figures/final_figures/Figure4_supp4.tif",
       device = "tiff", height = 10, width = 14, units = "in", bg = "white")

# Figure 5 ----

source("./code/function_library/visualization/SkillVsHorizon.R")
source("./code/function_library/visualization/OneHorizonSkill.R")

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
                                    "GLM-AED","MARS","randomForest", #removing 1D model
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

p5b <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 2,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,20))
p5b <- p5b +
  #annotate("text",x = 19.6, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 11.5, size = 1)
p5b

p5c <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 5,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,20))
p5c <- p5c +
  #annotate("text",x = 19.6, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 13.5, size = 1)
p5c

p5d <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 10,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "rmse",
                       show_legend = FALSE,
                       xlims = c(0,20))
p5d <- p5d +
  #annotate("text",x = 19.6, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 12.5, size = 1)
p5d

p5e <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest", #removing 1D model
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
p5e

p5f <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 2,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-1.5,1))
p5f <- p5f +
  #annotate("text",x = -1.35, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 11.5, size = 1)
p5f

p5g <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 5,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-1.5,1))
p5g <- p5g +
  #annotate("text",x = -1.35, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 13.5, size = 1)
p5g

p5h <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 10,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "r2",
                       show_legend = FALSE,
                       xlims = c(-1.5,1))
p5h <- p5h +
  #annotate("text",x = -1.35, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 12.5, size = 1)
p5h

p5i <- SkillVsHorizon(observations = obs, 
                      model_output = mod_out_high_var, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","MARS","randomForest", # removing 1D model
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
p5i

p5j <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 2,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "2 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,17))
p5j <- p5j +
  #annotate("text",x = 15.5, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 11.5, size = 1)
p5j

p5k <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 5,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "5 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,17))
p5k <- p5k +
  #annotate("text",x = 15.5, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 13.5, size = 1)
p5k

p5l <- OneHorizonSkill(observations = obs, 
                       model_output = mod_out_high_var, 
                       forecast_horizon = 10,
                       model_ids = c("DOY","persistence","historical mean","ARIMA",
                                     "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                     "GLM-AED","MARS","randomForest",
                                     "GAM","NNETAR-KGML","ensemble"),
                       viz_dates = pred_dates,
                       plot_title = "10 days ahead",
                       viz_metric = "mae",
                       show_legend = FALSE,
                       xlims = c(0,17))
p5l <- p5l +
  #annotate("text",x = 15.5, y = "LSTM", label = "*", size = 10, vjust = 0.8)+
  geom_hline(yintercept = 9.5, size = 1)
p5l

leg_plot1 <- SkillVsHorizon(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = forecast_horizon,
                            model_ids = c("DOY","persistence","historical mean","ARIMA",
                                          "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                          "GLM-AED","MARS","randomForest",
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

leg_plot2 <- OneHorizonSkill(observations = obs, 
                             model_output = out, 
                             forecast_horizon = forecast_horizon,
                             model_ids = c("DOY","persistence","historical mean","ARIMA",
                                           "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                           "GLM-AED","MARS","randomForest",
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


p5 <- ggarrange(ggarrange(p5_leg1,p5_leg2,
                          nrow = 2,
                          ncol = 1),
                ggarrange(p5a, p5b, p5c, p5d, p5e, p5f, p5g, p5h, p5i, p5j, p5k, p5l,
                          nrow = 3,
                          ncol = 4,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                          widths = c(1,0.9, 0.9, 0.9)),
                ncol = 2,
                widths = c(0.2, 1)
) #+ bgcolor("white")

p5

ggsave(plot = p5, filename = "./figures/final_figures/Figure5.tif",
       device = "tiff", height = 8, width = 12.5, units = "in",bg = "white")

# Figure 5 supplement

source("./code/function_library/visualization/GrandMeanSkill.R")

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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p5_supp1a

p5_supp1b <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = 14,
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p5_supp1c

p5_supp1d <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = 14,
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
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)
p5_supp1e

p5_supp1f <- GrandMeanSkill(observations = obs, 
                            model_output = mod_out_high_var, 
                            forecast_horizon = 14,
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
                            combined_var = "none",
                            show_null_model = FALSE,
                            fixed_ylim = FALSE)

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


p5_supp1 <- ggarrange(ggarrange(p5_supp1_leg1,p5_supp1_leg2,
                                nrow = 2,
                                ncol = 1,
                                heights = c(1, 0.5)),
                      ggarrange(p5_supp1a, p5_supp1b, p5_supp1c, p5_supp1d, p5_supp1e, p5_supp1f,
                                nrow = 3,
                                ncol = 2,
                                labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),
                                widths = c(1,0.8)),
                      ncol = 3,
                      widths = c(0.3, 1, 0.3)
) #+ bgcolor("white")

p5_supp1

ggsave(plot = p5_supp1, filename = "./figures/final_figures/Figure5_supp1.tif",
       device = "tiff", height = 9, width = 11, units = "in", bg = "white")


# Figure 6 ----
source("./code/function_library/visualization/CompareWithAndWithoutDrivers.R")

# TSLM RMSE
p6a <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = out, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: TSLM",
                                    plot_subtitle = "(full time period)",
                                    viz_metric = "rmse",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "TSLM",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(10)))
p6a

# MARS RMSE
onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p6b <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_onset, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("MARS","MARS (no drivers)","MARS (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: MARS",
                                    plot_subtitle = "(stratification onset)",
                                    viz_metric = "rmse",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "MARS",
                                    best_performing_horizons = data.frame(from = c(4,14),
                                                                          to = c(9,14)))
p6b

# ARIMA RMSE
decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p6c <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_decline, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("ARIMA","ARIMA (no drivers)","ARIMA (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: ARIMA",
                                    plot_subtitle = "(stratification decline)",
                                    viz_metric = "rmse",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "ARIMA",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(14)))
p6c

# TSLM R2
p6d <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = out, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: TSLM",
                                    plot_subtitle = "(full time period)",
                                    viz_metric = "r2",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "TSLM",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(10)))
p6d

# MARS R2
onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p6e <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_onset, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("MARS","MARS (no drivers)","MARS (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: MARS",
                                    plot_subtitle = "(stratification onset)",
                                    viz_metric = "r2",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "MARS",
                                    best_performing_horizons = data.frame(from = c(4,14),
                                                                          to = c(9,14)))
p6e

# ARIMA R2
decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p6f <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_decline, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("ARIMA","ARIMA (no drivers)","ARIMA (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: ARIMA",
                                    plot_subtitle = "(stratification decline)",
                                    viz_metric = "r2",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "ARIMA",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(14)))
p6f

# TSLM MAE
p6g <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = out, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("TSLM","TSLM (no drivers)","TSLM (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: TSLM",
                                    plot_subtitle = "(full time period)",
                                    viz_metric = "mae",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "TSLM",
                                    best_performing_horizons = data.frame(from = c(1),
                                                                          to = c(14)))
p6g

# MARS MAE
onset <- ss_data %>%
  filter(strat_bin == "onset")

mod_out_onset <- out %>%
  filter(datetime %in% onset$datetime)

p6h <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_onset, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("MARS","MARS (no drivers)","MARS (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: MARS",
                                    plot_subtitle = "(stratification onset)",
                                    viz_metric = "mae",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "MARS",
                                    best_performing_horizons = data.frame(from = c(NA),
                                                                          to = c(NA)))
p6h

# ARIMA MAE
decline <- ss_data %>%
  filter(strat_bin == "decline")

mod_out_decline <- out %>%
  filter(datetime %in% decline$datetime)

p6i <- CompareWithAndWithoutDrivers(observations = obs, 
                                    model_output = mod_out_decline, 
                                    forecast_horizon = forecast_horizon,
                                    model_ids = c("ARIMA","ARIMA (no drivers)","ARIMA (no lag)"),
                                    viz_dates = pred_dates,
                                    plot_title = "Data-driven: ARIMA",
                                    plot_subtitle = "(stratification decline)",
                                    viz_metric = "mae",
                                    show_legend = FALSE,
                                    combined_var = "none",
                                    parent_model = "ARIMA",
                                    best_performing_horizons = data.frame(from = c(2),
                                                                          to = c(5)))
p6i

leg_plot1 <- CompareWithAndWithoutDrivers(observations = obs, 
                                          model_output = mod_out_onset, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("MARS","TSLM","ARIMA","MARS (no lag)","MARS (no drivers)"),
                                          viz_dates = pred_dates,
                                          plot_title = "Data-driven: MARS (stratification onset)",
                                          viz_metric = "mae",
                                          show_legend = TRUE,
                                          combined_var = "none",
                                          parent_model = c("MARS","TSLM","ARIMA"),
                                          best_performing_horizons = data.frame(from = c(NA),
                                                                                to = c(NA)))                                    

# Extract the legend. Returns a gtable
leg1 <- get_legend(leg_plot1)

# Convert to a ggplot and print
p6_leg1 <- as_ggplot(leg1)
p6_leg1

p6 <- ggarrange(ggarrange(p6_leg1,
                          nrow = 1,
                          ncol = 1),
                ggarrange(p6a, p6b, p6c, p6d, p6e, p6f, p6g, p6h, p6i,
                          nrow = 3,
                          ncol = 3,
                          labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)"),
                          widths = c(1,1,1)),
                ncol = 2,
                widths = c(0.3, 1)
) #+ bgcolor("white")

p6

ggsave(plot = p6, filename = "./figures/final_figures/Figure6.tif",
       device = "tiff", height = 8, width = 12.5, units = "in",bg = "white")

