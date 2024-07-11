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
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers"),"data-driven","process-based")))
out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers"),"data-driven","process-based")),
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
                    model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                    best_models_only = TRUE)
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

