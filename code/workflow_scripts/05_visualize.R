#Visualize model output
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Visualize model output to assess model performance

#load packages
library(tidyverse)
library(lubridate)
library(ggpubr)
#library(plotly)

#Read in data
cal <- read_csv("./model_output/calibration_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS","randomForest"),"data-driven","process-based")))
out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS","randomForest"),"data-driven",
                                    ifelse(model_id %in% c("NNETAR_KGML"),"KGML","process-based"))),
         model_id = ifelse(model_id == "ARIMAnoDrivers","ARIMA (no drivers)",
                           ifelse(model_id == "NNETARnoDrivers","NNETAR (no drivers)",
                                  ifelse(model_id == "ProphetnoDrivers","Prophet (no drivers)",
                                         ifelse(model_id == "NNETAR_KGML","NNETAR-KGML",model_id)))))
ens <- out %>%
  filter(!model_id %in% c("ARIMA (no drivers)","NNETAR (no drivers)","Prophet (no drivers)","NNETAR-KGML")) %>%
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
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "a. Mixed: ",
                         rect_color = plot_cols[1],
                         legend_option = "all",
                         ylim_values = c(0,40))
p2a

reference_datetime_b = focal_dates[2]
p2b <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "b. Onset: ",
                         rect_color = plot_cols[2],
                         legend_option = "all",
                         ylim_values = c(0,40))
p2b

reference_datetime_c = focal_dates[3]
p2c <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_c, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "c. Stratified: ",
                         rect_color = plot_cols[3],
                         legend_option = "all",
                         ylim_values = c(0,40))
p2c

reference_datetime_d = focal_dates[4]
p2d <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_d, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "d. Decline: ",
                         rect_color = plot_cols[4],
                         legend_option = "all",
                         ylim_values = c(0,40))
p2d

reference_datetime_e = focal_dates[5]
p2e <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_e, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "e. Low variability: ",
                         rect_color = plot_cols[5],
                         legend_option = "all",
                         ylim_values = c(0,40))
p2e

reference_datetime_f = focal_dates[6]
p2f <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_f, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-corrected GLM-AED","ensemble"),
                         show_legend = FALSE,
                         sub_panel_label = "f. High variability: ",
                         rect_color = plot_cols[6],
                         legend_option = "all",
                         ylim_values = c(0,65))
p2f

leg_plot <- ExamplePrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime_b, 
                         forecast_horizon = forecast_horizon,
                         model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","NNETAR-KGML","ensemble"),
                         show_legend = TRUE,
                         sub_panel_label = "b",
                         rect_color = "black",
                         legend_option = "all",
                         ylim_values = c(0,40))

# Extract the legend. Returns a gtable
leg <- get_legend(leg_plot)

# Convert to a ggplot and print
p2_leg <- as_ggplot(leg)
p2_leg

p2 <- ggarrange(ggarrange(p2a, p2b, p2c, p2d, p2e, p2f, nrow = 3, ncol = 2),
                p2_leg,
                ncol = 2,
                widths = c(1, 0.3)
) +
  bgcolor("white")+
  theme(plot.margin = margin(0.2,0.1,1.5,0.1, "cm")) 

p2

ggsave(plot = p2, filename = "./figures/final_figures/Figure2.tif",
       device = "tiff", height = 11, width = 10, units = "in")

# Figure 3

source("./code/function_library/visualization/SkillVsHorizon.R")
p3a <- SkillVsHorizon(observations = obs, 
                    model_output = out, 
                    forecast_horizon = forecast_horizon,
                    model_ids = c("DOY","persistence","historical mean","ARIMA",
                                  "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                  "GLM-AED","OneDProcessModel","MARS","randomForest",
                                  "NNETAR-KGML","ensemble"),
                    best_models_only = FALSE,
                    viz_dates = pred_dates,
                    plot_title = "All predictions (Jan. 1, 2022 - Nov. 26, 2023)",
                    viz_metric = "rmse",
                    show_legend = FALSE)
p3a

p3b <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      best_models_only = FALSE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "r2",
                      show_legend = FALSE)
p3b

p3c <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      best_models_only = FALSE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "bias",
                      show_legend = FALSE)
p3c

leg_plot <- SkillVsHorizon(observations = obs, 
                           model_output = out, 
                           forecast_horizon = forecast_horizon,
                           model_ids = c("DOY","persistence","historical mean","ARIMA",
                                         "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                         "GLM-AED","OneDProcessModel","MARS","randomForest",
                                         "NNETAR-KGML","ensemble"),
                           best_models_only = FALSE,
                           viz_dates = pred_dates,
                           plot_title = "",
                           viz_metric = "rmse",
                           show_legend = TRUE)

# Extract the legend. Returns a gtable
leg <- get_legend(leg_plot)

# Convert to a ggplot and print
p3_leg <- as_ggplot(leg)
p3_leg


p3 <- ggarrange(ggarrange(p3a, p3b, p3c,
                nrow = 3,
                labels = c("a","b","c")),
                p3_leg,
                ncol = 2,
                widths = c(1,0.5)
) + bgcolor("white")

p3

ggsave(plot = p3, filename = "./figures/final_figures/Figure3.tif",
       device = "tiff", height = 6, width = 7, units = "in")

# Figure 4

source("./code/function_library/visualization/SkillVsHorizon.R")
p4a <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      best_models_only = TRUE,
                      viz_dates = pred_dates,
                      plot_title = "All predictions (Jan. 1, 2022 - Nov. 26, 2023)",
                      viz_metric = "rmse",
                      show_legend = FALSE)
p3a

p3b <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      best_models_only = FALSE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "r2",
                      show_legend = FALSE)
p3b

p3c <- SkillVsHorizon(observations = obs, 
                      model_output = out, 
                      forecast_horizon = forecast_horizon,
                      model_ids = c("DOY","persistence","historical mean","ARIMA",
                                    "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                    "GLM-AED","OneDProcessModel","MARS","randomForest",
                                    "NNETAR-KGML","ensemble"),
                      best_models_only = FALSE,
                      viz_dates = pred_dates,
                      plot_title = "",
                      viz_metric = "bias",
                      show_legend = FALSE)
p3c

leg_plot <- SkillVsHorizon(observations = obs, 
                           model_output = out, 
                           forecast_horizon = forecast_horizon,
                           model_ids = c("DOY","persistence","historical mean","ARIMA",
                                         "ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR",
                                         "GLM-AED","OneDProcessModel","MARS","randomForest",
                                         "NNETAR-KGML","ensemble"),
                           best_models_only = FALSE,
                           viz_dates = pred_dates,
                           plot_title = "",
                           viz_metric = "rmse",
                           show_legend = TRUE)

# Extract the legend. Returns a gtable
leg <- get_legend(leg_plot)

# Convert to a ggplot and print
p3_leg <- as_ggplot(leg)
p3_leg


p3 <- ggarrange(ggarrange(p3a, p3b, p3c,
                          nrow = 3,
                          labels = c("a","b","c")),
                p3_leg,
                ncol = 2,
                widths = c(1,0.5)
) + bgcolor("white")

p3

ggsave(plot = p3, filename = "./figures/final_figures/Figure3.tif",
       device = "tiff", height = 6, width = 7, units = "in")


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
