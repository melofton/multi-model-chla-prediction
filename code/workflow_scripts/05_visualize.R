#Visualize model output
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Visualize model output to assess model performance

#load packages
library(tidyverse)
library(lubridate)
library(cowplot)
#library(plotly)

#Load plotting functions
plot.functions <- list.files("./code/function_library/visualization")
sapply(paste0("./code/function_library/visualization/",plot.functions),source,.GlobalEnv)

# Notes:
##ADD FUNCTION TO DO WHICH DAY IS PREDICTED TO BE PEAK DAY!!!!!!
# also will need to update the plotting function to visualize different methods of
# interpolation once you figure out how you are handling that

#Read in data
cal <- read_csv("./model_output/calibration_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers"),"data-driven","process-based")))
out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS"),"data-driven","process-based")),
         model_id = ifelse(model_id == "ARIMAnoDrivers","ARIMA (no drivers)",
                           ifelse(model_id == "NNETARnoDrivers","NNETAR (no drivers)",
                                  ifelse(model_id == "ProphetnoDrivers","Prophet (no drivers)",model_id))))
obs <- read_csv("./data/data_processed/chla_obs.csv")
input <- read_csv("./data/data_processed/ARIMA.csv")


#Set arguments for plotting functions
forecast_horizon = 35

#Plot 

p1 <- PlotObservations(observations = obs, pred_only = TRUE,
                       focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06","2023-05-16","2023-07-31","2023-10-02","2023-11-11"),
                       forecast_horizon = forecast_horizon,
                       plotly = FALSE)
p1
ggsave(p1, filename = "./figures/predictionBlooms.png",
       device = "png", height = 4, width = 6, units = "in")

p2 <- PlotInputData(input_data = input)
p2
ggsave(p2, filename = "./figures/drivers.png",
       device = "png", height = 3, width = 5, units = "in")

p3 <- PlotModelFits(observations = obs, 
                        predictions = cal, 
                        model_ids = c("GLM-AED"))
p3
ggsave(p3, filename = "./figures/GLMAEDModelFits.png",
       device = "png", height = 3, width = 6, units = "in")

reference_datetime = "2022-07-15"#"2022-05-28" #"2022-10-20" the NNNETAR for this date is incredible
p4 <- ExamplePrediction(observations = obs, 
                        model_output = out, 
                        reference_datetime = reference_datetime, 
                        forecast_horizon = forecast_horizon,
                        model_ids = c("GLM-AED"))
p4
ggsave(p4, filename = "./figures/examplePredictionGLMAED.png",
       device = "png", height = 5, width = 7, units = "in")

p5 <- RMSEVsHorizon(observations = obs, 
                    model_output = out, 
                    forecast_horizon = forecast_horizon,
                    model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                    best_models_only = FALSE)
p5 
ggsave(p5, filename = "./figures/BestModelsRMSEvsHorizonNoDrivers.png",
       device = "png", height = 4, width = 8, units = "in")

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

# GLM-AED

# functional relationships
PlotMonodLightLimitation(I_K = 250, xlim = c(0,600), save_plot = FALSE)
PlotRespiration(R_resp = 0.11, theta_resp = 1.08, xlim = c(1,30), save_plot = FALSE)
PlotNLimitation(K_N = 0.15, N_0 = 0.25, xlim = c(0,10), save_plot = FALSE)
PlotPLimitation(K_P = 0.0001, P_0 = 0.0, xlim = c(0,0.15), save_plot = FALSE)

# set parameters for temperature limitation
g1 <- list(T_std = 10,
           T_opt = 28,
           T_max = 40,
           Ts = 10,
           To = 28,
           Tm = 40,
           v = 1.1,
           theta = 1.1)
g2 <- list(T_std = 10,
           T_opt = 28,
           T_max = 35,
           Ts = 10,
           To = 28,
           Tm = 35,
           v = 1.06,
           theta = 1.06)
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

pred.df <- read_csv("./model_output/LSTM_tuning_09SEP24.csv")
df <- read_csv("./code/model_files/LSTM/LSTM_dataset.csv") %>%
  select(datetime, Chla_ugL_mean)

single_horizon <- pred.df %>%
  filter(horizon == 20)

LSTM_ts <- ggplot()+
  xlab("")+
  ylab("Chla (ug/L)")+
  geom_point(data = df, aes(x = datetime, y = Chla_ugL_mean, fill = "obs"))+
  geom_line(data = single_horizon, aes(x = datetime, y = prediction, group = param_set, color = param_set))+
  labs(color = "parameter scenario", fill = NULL)+
  theme_classic()+
  theme(legend.position = "bottom")+
  ggtitle("Predictions for 20 days ahead")
LSTM_ts
ggsave(plot = LSTM_ts, filename = "./figures/LSTM_tuning_horizon20.png", device = "png",
       height = 4, width = 8, units = "in")

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
  xlab("Forecast horizon (days)")+
  ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))+
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
