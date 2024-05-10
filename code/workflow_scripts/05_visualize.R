#Visualize model output
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Visualize model output to assess model performance

#load packages
library(tidyverse)
library(lubridate)
library(plotly)

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
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","prophet"),"statistical",
                                    ifelse(model_id %in% c("LSTM","XGBoost","NNETAR"),"machine learning","process"))))
out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","prophet"),"statistical",
                                    ifelse(model_id %in% c("LSTM","XGBoost","NNETAR"),"machine learning","process"))))
obs <- read_csv("./data/data_processed/chla_obs.csv")
input <- read_csv("./data/data_processed/ARIMA.csv")


#Set arguments for plotting functions
forecast_horizon = 35

#Plot 

p1 <- PlotObservations(observations = obs, pred_only = TRUE,
                       focal_dates = NULL,
                       #focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06"),
                       forecast_horizon = forecast_horizon,
                       plotly = TRUE)
p1
ggsave(p1, filename = "./figures/observations.png",
       device = "png", height = 3, width = 5, units = "in")

p2 <- PlotInputData(input_data = input)
p2
ggsave(p2, filename = "./figures/drivers.png",
       device = "png", height = 3, width = 5, units = "in")

p3 <- PlotModelFits(observations = obs, 
                        predictions = cal, 
                        model_ids = c("persistence","historical mean","DOY","ETS",
                                      "ARIMA","TSLM","prophet","XGBoost","NNETAR"))
p3
ggsave(p3, filename = "./figures/ModelFits.png",
       device = "png", height = 3, width = 6, units = "in")

reference_datetime = "2022-05-28" #"2022-10-20" the NNNETAR for this date is incredible
p4 <- ExamplePrediction(observations = obs, 
                        model_output = out, 
                        reference_datetime = reference_datetime, 
                        forecast_horizon = forecast_horizon,
                        model_ids = c("persistence","historical mean","DOY","ETS",
                                      "ARIMA","TSLM","prophet","XGBoost","NNETAR",
                                      "OneDProcessModel"))
p4
ggsave(p4, filename = "./figures/examplePrediction.png",
       device = "png", height = 3, width = 7, units = "in")

p5 <- RMSEVsHorizon(observations = obs, 
                          model_output = out, 
                          forecast_horizon = forecast_horizon)
p5
ggsave(p5, filename = "./figures/RMSEvsHorizon.png",
       device = "png", height = 5.5, width = 5.5, units = "in")

#need to figure out how to detach legend from this and make it a separate
#plot, then add
#focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06","2023-05-16","2023-07-31","2023-10-02","2023-11-11"),
# ADD DROP-ONE-OUT VALIDATION TO THIS FIGURE!
p6 <- PerformanceRelativeToBloom(observations = obs,
                           model_output = out,
                           variable_name = "chlorophyll-a",
                           max_horizon_past = -21,
                           score = "rmse",
                           focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06","2023-05-16","2023-07-31","2023-10-02","2023-11-11"),
                           data_plot = FALSE)
p6
ggsave(p6, filename = "./figures/performanceRelativeToBloom.png",
       device = "png", height = 5, width = 7, units = "in")

p7 <- OneHorizonTimeseries(observations = obs, 
                                 model_output = out, 
                                 forecast_horizon = 7)
p7
ggsave(p7, filename = "./figures/predictionHorizon7Days.png",
       device = "png", height = 3, width = 7, units = "in")

# GLM-AED

# functional relationships
PlotMonodLightLimitation(I_K = 250, xlim = c(0,600), save_plot = FALSE)
PlotRespiration(theta_resp = 1.02, xlim = c(1,30), save_plot = TRUE)
PlotNLimitation(K_N = 2.5, N_0 = 0, xlim = c(0,10), save_plot = FALSE)
PlotPLimitation(K_P = 0.15, P_0 = 0, xlim = c(0,0.15), save_plot = TRUE)

# set parameters for temperature limitation
g1 <- list(T_std = 10,
           T_opt = 28,
           T_max = 35,
           Ts = 10,
           To = 28,
           Tm = 35,
           v = 1.1,
           theta = 1.1)
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

