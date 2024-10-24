# Analyze model output
# Author: Mary Lofton
# Date: 08OCT24

# Purpose: determine how model performance differs under different thermal 
# stratification conditions and during period of increasing/decreasing chl-a

# Tasks:
#' 1. calculate Schmidt stability
#' 2. determine periods that are mixed, stratified, or exhibit increasing/
#' decreasing stratification
#' 3. calculate daily rate of change in chl-a and plot as histogram
#' 4. determine thresholds for 10th and 90th quantiles for rate-of-change
#' values for chl-a
#' 5. plot model performance during periods of mixing, increasing, decreasing, 
#' and stratified conditions
#' 6. plot model performance during periods of highly variable vs. less variable 
#' chl-a

# Load packages
install.packages("rLakeAnalyzer")
install.packages("vegan")
library(rLakeAnalyzer)
library(tidyverse)
library(lubridate)

# assign important vars and load data
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")

out <- read_csv("./model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","NNETARnoDrivers","ProphetnoDrivers","ARIMAnoDrivers","MARS","randomForest"),"data-driven",
                                    ifelse(model_id %in% c("ETS_KGML"),"KGML","process-based"))),
         model_id = ifelse(model_id == "ARIMAnoDrivers","ARIMA (no drivers)",
                           ifelse(model_id == "NNETARnoDrivers","NNETAR (no drivers)",
                                  ifelse(model_id == "ProphetnoDrivers","Prophet (no drivers)",
                                         ifelse(model_id == "ETS_KGML","ETS-corrected GLM-AED",model_id)))))
ens <- out %>%
  filter(!model_id %in% c("ARIMA (no drivers)","NNETAR (no drivers)","Prophet (no drivers)","ETS-corrected GLM-AED")) %>%
  group_by(reference_datetime, datetime) %>%
  summarize(prediction = mean(prediction, na.rm = TRUE)) %>%
  add_column(model_id = "ensemble", model_type = "ensemble", variable = "chlorophyll-a") 

out <- bind_rows(out, ens)

forecast_horizon = 35

model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble")

obs <- read_csv("./data/data_processed/chla_obs.csv") %>%
  mutate(delta = c(NA,diff(Chla_ugL_mean, na.rm = TRUE)))

pred_dates_df <- data.frame(datetime = pred_dates) %>%
  left_join(., obs, by = "datetime") 

# 1. calculate Schmidt stability

#'@param res_url url to in situ reservoir targets data for VERA
res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
res <- read_csv(res_url) %>%
  filter(site_id == "fcre" & variable == "Temp_C_mean") %>%
  arrange(datetime, depth_m) %>%
  select(datetime, depth_m, observation) %>%
  filter(date(datetime) %in% pred_dates) %>%
  pivot_wider(names_from = depth_m, values_from = observation) 
colnames(res)[2:12] <- paste0("wtr_",colnames(res)[2:12])
res <- res[,c(1:3,12,4:11)]

bth <- read_csv("./data/data_raw/Bathymetry_comb.csv") %>%
  filter(Reservoir == "FCR") %>%
  select(Depth_m, SA_m2)
colnames(bth) <- c("depths","areas")

schmidt <- ts.schmidt.stability(wtr = res, bathy = bth, na.rm = TRUE) %>%
  mutate(datetime = date(datetime)) %>%
  filter(!datetime == "2022-05-24") %>% # bad data day; Schmidt stability of 0
  mutate(strat_bin = ifelse(schmidt.stability <= 1, "mixed",
                                ifelse(schmidt.stability >= 40, "stratified",
                                       ifelse((schmidt.stability > 1 & schmidt.stability < 40 & month(datetime) %in% c(1:7)),"onset","decline"))))

#' 2. determine periods that are mixed, stratified, or exhibit increasing/
#' decreasing stratification

ggplot(data = schmidt, aes(x = datetime, y = schmidt.stability, color = strat_bin))+
  geom_point()+
  theme_classic()
ggplot(data = schmidt, aes(x = schmidt.stability, group = strat_bin, fill = strat_bin))+
  geom_histogram(color = "black", binwidth = 1, boundary = 0)+
  theme_bw()

#' 3. calculate daily rate of change in chl-a and plot as histogram
#' #' 4. determine thresholds for 10th and 90th quantiles for rate-of-change
#' values for chl-a
dens <- density(obs$delta, na.rm = TRUE)
q5 <- quantile(obs$delta, 0.05, na.rm = TRUE)
q95 <- quantile(obs$delta, 0.95, na.rm = TRUE)
dd <- with(dens,data.frame(x,y))
ggplot(data = dd, aes(x = x, y = y))+
  geom_line()+
  geom_ribbon(data=subset(dd,x>q5 & x<q95),aes(ymax=y),ymin=0,
              fill="gray",colour=NA,alpha=0.5)+
  geom_vline(xintercept = q5, linetype = "dashed")+
  geom_vline(xintercept = q95, linetype = "dashed")+
  xlab("Change in chl-a (ug/L)")+
  ylab("density")+
  theme_bw()

obs2 <- obs %>%
  filter(year(datetime) %in% c(2022:2023)) %>%
  mutate(chla_bin = ifelse(delta <= q5,"decreasing",
                           ifelse(delta >= q95, "increasing","stable")))
ggplot(data = obs2, aes(x = datetime, y = Chla_ugL_mean, color = chla_bin))+
  geom_point()+
  theme_classic()

#' 5. plot model performance during periods of mixing, increasing, decreasing, 
#' and stratified conditions
source("./code/function_library/visualization/RMSEvsHorizon.R")

mixed_dates <- schmidt %>%
  filter(strat_bin == "mixed") %>%
  pull(datetime)

performance_plot_mixed <- RMSEVsHorizon(observations = obs, 
                    model_output = out, 
                    forecast_horizon = forecast_horizon,
                    model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                    best_models_only = TRUE,
                    viz_dates = mixed_dates,
                    plot_title = "Predictions during mixed period")
performance_plot_mixed

onset_dates <- schmidt %>%
  filter(strat_bin == "onset") %>%
  pull(datetime)

performance_plot_onset <- RMSEVsHorizon(observations = obs, 
                                        model_output = out, 
                                        forecast_horizon = forecast_horizon,
                                        model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                                        best_models_only = TRUE,
                                        viz_dates = onset_dates,
                                        plot_title = "Predictions during stratification onset")
performance_plot_onset

stratified_dates <- schmidt %>%
  filter(strat_bin == "stratified") %>%
  pull(datetime)

performance_plot_stratified <- RMSEVsHorizon(observations = obs, 
                                        model_output = out, 
                                        forecast_horizon = forecast_horizon,
                                        model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                                        best_models_only = TRUE,
                                        viz_dates = stratified_dates,
                                        plot_title = "Predictions during stratified period")
performance_plot_stratified

decline_dates <- schmidt %>%
  filter(strat_bin == "decline") %>%
  pull(datetime)

performance_plot_decline <- RMSEVsHorizon(observations = obs, 
                                             model_output = out, 
                                             forecast_horizon = forecast_horizon,
                                             model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                                             best_models_only = TRUE,
                                             viz_dates = decline_dates,
                                             plot_title = "Predictions during stratification decline")
performance_plot_decline


#' 6. plot model performance during periods of highly variable vs. less variable 
#' chl-a

low_var_dates <- obs2 %>%
  filter(chla_bin == "stable") %>%
  pull(datetime)

performance_plot_low_var <- RMSEVsHorizon(observations = obs, 
                                        model_output = out, 
                                        forecast_horizon = forecast_horizon,
                                        model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                                        best_models_only = TRUE,
                                        viz_dates = low_var_dates,
                                        plot_title = "Predictions during periods of low chl-a variability")
performance_plot_low_var

high_var_dates <- obs2 %>%
  filter(chla_bin %in% c("increasing","decreasing")) %>%
  pull(datetime)

performance_plot_high_var <- RMSEVsHorizon(observations = obs, 
                                          model_output = out, 
                                          forecast_horizon = forecast_horizon,
                                          model_ids = c("DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","MARS","randomForest","ETS-corrected GLM-AED","ensemble"), # "DOY","persistence","historical mean","ARIMA","ETS","TSLM","Prophet","LSTM","XGBoost","NNETAR","GLM-AED","OneDProcessModel","ARIMA (no drivers)","Prophet (no drivers)","NNETAR (no drivers)"
                                          best_models_only = TRUE,
                                          viz_dates = high_var_dates,
                                          plot_title = "Predictions during periods of high chl-a variability")
performance_plot_high_var


# attempt at 3D plot
chla_sds <- out %>%
  filter(model_id == "persistence") %>%
  select(reference_datetime, datetime) %>%
  left_join(., obs, by = "datetime") %>%
  group_by(reference_datetime) %>%
  summarize(chla_sd = sd(Chla_ugL_mean, na.rm = TRUE))

chla_var_3d_data <- out %>% 
  filter(model_id %in% model_ids) %>%
  left_join(., pred_dates_df, by = "datetime") %>%
  select(-delta, -variable) %>%
  left_join(., chla_sds, by = "reference_datetime") %>%
  group_by(model_type, model_id, reference_datetime) %>%
  mutate(horizon = datetime - reference_datetime) %>%
  ungroup() %>%
  separate(horizon, c("horizon"), sep = " ") %>%
  filter(!is.na(Chla_ugL_mean)) %>%
  mutate(bias = prediction - Chla_ugL_mean) %>%
  filter(!horizon == 0) %>%
  group_by(horizon, chla_sd) %>%
  filter(bias == min(bias, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(horizon = as.numeric(horizon)) %>%
  filter(horizon <= forecast_horizon) %>%
  select(chla_sd, horizon, bias, model_type, model_id) %>%
  arrange(chla_sd, horizon, bias, model_type, model_id) %>%
  mutate(model_type = factor(model_type, levels = c("null","process-based","data-driven","KGML","ensemble"))) %>%
  mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","OneDProcessModel","GLM-AED","ARIMA","ARIMA (no drivers)","ETS","TSLM","MARS","randomForest","Prophet","Prophet (no drivers)","XGBoost","NNETAR","NNETAR (no drivers)","LSTM","ETS-corrected GLM-AED","ensemble"))) %>%
  left_join(.,chla_sds, by = "chla_sd")

bias_vs_sd_by_horizon <- ggplot(data = chla_var_3d_data, aes(x = chla_sd, y = bias, color = model_type))+
  geom_point()+
  facet_wrap(facets = vars(horizon))+
  theme_bw()
bias_vs_sd_by_horizon



