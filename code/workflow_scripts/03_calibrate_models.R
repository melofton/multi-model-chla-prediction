#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)
library(ggpubr)

#Load model fitting functions
fit.model.functions <- list.files("./code/function_library/fit_models")
sapply(paste0("./code/function_library/fit_models/",fit.model.functions[12]),source,.GlobalEnv)

#Read in data
dat_persistence <- read_csv("./data/data_processed/persistence.csv")
dat_historicalMean <- read_csv("./data/data_processed/historicalMean.csv")
dat_DOY <- read_csv("./data/data_processed/DOY.csv")
dat_ETS <- read_csv("./data/data_processed/ETS.csv")
dat_ARIMA <- read_csv("./data/data_processed/ARIMA.csv")
dat_ARIMA_noDrivers <- read_csv("./data/data_processed/ARIMAnoDrivers.csv")
dat_TSLM <- read_csv("./data/data_processed/TSLM.csv")
dat_processModels <- read_csv("./data/data_processed/processModels.csv")
dat_XGBoost <- read_csv("./data/data_processed/XGBoost.csv")
dat_Prophet <- read_csv("./data/data_processed/Prophet.csv")
dat_NNETAR <- read_csv("./data/data_processed/NNETAR.csv")
dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")
dat_LSTM <- read_csv("./data/data_processed/LSTM.csv")
dat_MARS <- read_csv("./data/data_processed/MARS.csv")
dat_randomForest <- read_csv("./data/data_processed/randomForest.csv")
dat_NNETAR_KGML <- read_csv("./data/data_processed/NNETAR_KGML.csv")
dat_GAM <- read_csv("./data/data_processed/GAM.csv")
dat_NNETAR_KGML2 <- read_csv("./data/data_processed/NNETAR_KGML2.csv")

#Set sim folder (for GLM-AED)
sim_folder <- "./code/model_files/GLM-AED/calibration"

#Fit models 
fit_persistence <- fit_persistence(data = dat_persistence, cal_dates = c("2018-08-06","2021-12-31"))
fit_persistence$plot

fit_historicalMean <- fit_historicalMean(data = dat_historicalMean, cal_dates = c("2018-08-06","2021-12-31"))
fit_historicalMean$plot

fit_DOY <- fit_DOY_chla(data = dat_DOY, cal_dates = c("2018-08-06","2021-12-31"))
fit_DOY$plot
ggsave(fit_DOY$plot, filename = "./figures/GAM_fit.png",
       height = 3, width = 5, units = "in")

fit_ETS <- fit_ETS(data = dat_ETS, cal_dates = c("2018-08-06","2021-12-31"))
fit_ETS$plot
ggsave(fit_ETS$plot, filename = "./figures/ETS_fit.png",
       height = 3, width = 5, units = "in")
write.csv(fit_ETS$model_diagnostics, "./model_output/ETS_diagnostics.csv",row.names = FALSE)

fit_ARIMAs <- fit_ARIMAs(data = dat_ARIMA, cal_dates = c("2018-08-06","2021-12-31"))
ggsave(fit_ARIMAs$plot, filename = "./figures/ARIMAs_fit.png",
       height = 3, width = 10, units = "in")
ARIMA_diagnostics <- ggarrange(plotlist = c(fit_ARIMAs$diagnostics_chla_only, fit_ARIMAs$diagnostics_drivers),
                              labels = c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave(ARIMA_diagnostics, filename = "./figures/ARIMA_diagnostics.png",
       height = 6, width = 10, units = "in")
write.csv(fit_ARIMAs$model_params, "./model_output/ARIMA_parameters.csv",row.names = FALSE)

fit_TSLM <- fit_TSLM(data = dat_TSLM, cal_dates = c("2018-08-06","2021-12-31"))
fit_TSLM$plot
ggsave(fit_TSLM$plot, filename = "./figures/TSLM_fit.png",
       height = 3, width = 5, units = "in")
TSLM_diagnostics <- ggarrange(plotlist = c(fit_TSLM$diagnostics_no_lag, fit_TSLM$diagnostics),
                              labels = c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave(TSLM_diagnostics, filename = "./figures/TSLM_diagnostics.png",
       height = 6, width = 10, units = "in")
stats_table <- fit_TSLM$stats %>%
  add_column(model_name = c("no lag or trend","lag","lag and trend")) %>%
  select(model_name, r_squared, adj_r_squared, log_lik, AIC, AICc, BIC, CV, deviance) %>%
  mutate(across(.cols = -c(model_name),
                .fns  = ~ round(., 2)))
write.csv(stats_table, "./model_output/TSLM_diagnostics.csv",row.names = FALSE)

fit_XGBoost <- fit_XGBoost(data = dat_XGBoost, cal_dates = c("2018-08-06","2021-12-31"), include_lag = FALSE)
ggsave(fit_XGBoost$plot, filename = "./figures/XGBoost_fit.png",
       height = 3, width = 5, units = "in")
ggsave(fit_XGBoost$vip_plot, filename = "./figures/XGBoost_feature_importance.png",
       height = 3, width = 5, units = "in")
write.csv(fit_XGBoost$best_hyperparameters, "./model_output/XGBoost_best_hyperparameters.csv",row.names = FALSE)

fit_Prophets <- fit_Prophets(data = dat_Prophet, cal_dates = c("2018-08-06","2021-12-31"))
ggsave(fit_Prophets$plot, filename = "./figures/Prophet_fit.png",
       height = 3, width = 10, units = "in")
ggsave(fit_Prophets$rmse_plot, filename = "./figures/Prophet_fit_rmse.png",
       height = 3, width = 5, units = "in")
Prophet_components_chlaOnly <- ggarrange(plotlist = c(fit_Prophets$prophet_components),
          nrow = 2, ncol = 2, labels = c("(a)","(b)","(c)"))
ggsave(Prophet_components_chlaOnly, filename = "./figures/Prophet_components_chlaOnly.png",
       height = 5.5, width = 10, units = "in")
Prophet_components_drivers <- ggarrange(plotlist = c(fit_Prophets$prophet_components_w_drivers),
                                         nrow = 2, ncol = 2, labels = c("(a)","(b)","(c)","(d)"))
ggsave(Prophet_components_drivers, filename = "./figures/Prophet_components_drivers.png",
       height = 5.5, width = 10, units = "in")
write.csv(fit_Prophets$reg_coeffs, "./model_output/Prophet_regressor_coefficients.csv",row.names = FALSE)

fit_NNETAR <- fit_NNETARs(data = dat_NNETAR, cal_dates = c("2018-08-06","2021-12-31"))
fit_NNETAR$plot
ggsave(fit_NNETAR$plot, filename = "./figures/NNETAR_fit.png",
       height = 3, width = 10, units = "in")

fit_MARS <- fit_MARS(data = dat_MARS, cal_dates = c("2018-08-06","2021-12-31"))
ggsave(fit_MARS$plot, filename = "./figures/MARS_fit.png",
       height = 3, width = 5, units = "in")
png("./figures/MARS_model_surfaces.png", width = 9, height = 6,
    units = "in", res = 300)
plotmo(fit_MARS$MARS, xlab = "predictor value", ylab = "Chla (ug/L)")
dev.off()
write.csv(fit_MARS$basis.functions, "./model_output/MARS_basis_functions.csv",row.names = FALSE)

fit_randomForest <- fit_randomForest(data = dat_randomForest, cal_dates = c("2018-08-06","2021-12-31"))
ggsave(fit_randomForest$importance_plot, filename = "./figures/randomForest_importance.png",
       height = 3, width = 6, units = "in")
ggsave(fit_randomForest$plot, filename = "./figures/randomForest_fit.png",
       height = 3, width = 5, units = "in")

fit_GAM <- fit_GAM(data = dat_GAM, cal_dates = c("2018-08-06","2021-12-31"))

params_list <- list(epochs = c(100,200),
                    dropout = c(0, 0.0001, 0.0005, 0.001, 0.002, 0.01),
                    num_layers = c(1,2,3),
                    hidden_feature_size = c(8,16),
                    weight_decay = c(0, 0.0001, 0.0005, 0.001, 0.002, 0.01))
fit_LSTM <- fit_LSTM(data = dat_LSTM, cal_dates = c("2018-08-06","2021-12-31"), forecast_horizon = 20,
                     input_window = 42, params_list = params_list)
fit_LSTM$plot
LSTM_out <- fit_LSTM$out

#Calibrate process models (this completes one run + diagnostics + assessment
# metrics for GLM-AED) - you must be in a container to run this!
GLMAED_run <- calibrate_GLMAED(sim_folder = sim_folder, save_plot = TRUE)
OneDProcessModel_run <- calibrate_1DProcessModel(
  data = dat_1DProcessModel,
  parms = c(-0.001, #w_p (negative is down, positive is up)
            4.5, #R_growth
            1.02,#1.1, #theta_growth
            1, #light_extinction
            60, #I_K
            0, #N_o
            2.5, #K_N
            0, #P_o
            0.0001, #K_P
            0.1, #f_pr
            0.13, #R_resp
            1.02, #theta_resp
            10, #T_std
            12,#20, #T_opt
            30,#35, #T_max
            0.02, #N_C_ratio
            0.002, #P_C_ratio
            0, #phyto_flux_top
            9.5,# lake_depth
            38,# num_boxes
            0.005,#KePHYTO
            0.01, #D_temp
            0,#phyto_flux_bottom
            10), #Xcc,
  cal_dates = c("2018-08-06","2022-01-01"),
  save_plots = TRUE,
  inputs = NULL
)
write.csv(OneDProcessModel_run$output_df, "./code/model_files/1DProcessModel/output.csv", row.names = FALSE)

OneDProcessModel_run$out <- OneDProcessModel_run$output_df %>%
  filter(depth == 1.5 & variable == "chla") %>%
  add_column(model_id = "OneDProcessModel") %>%
  mutate(depth = 1.6,
         variable = "chlorophyll-a") %>%
  select(model_id, datetime, variable, prediction)

# KGML experiment
fit_NNETAR_KGML <- fit_NNETAR_KGML(data = dat_NNETAR_KGML, cal_dates = c("2018-08-06","2021-12-31"))
fit_NNETAR_KGML$plot

fit_NNETAR_KGML2 <- fit_NNETAR_KGML2(data = dat_NNETAR_KGML2, cal_dates = c("2022-01-01","2022-12-31"), target = "residuals")
fit_NNETAR_KGML2$plot

fit_NNETAR_KGML3 <- fit_NNETAR_KGML2(data = dat_NNETAR_KGML2, cal_dates = c("2022-01-01","2022-12-31"), target = "observations")
fit_NNETAR_KGML3$plot


#Stack model predictions and write to file (not applicable for persistence model
#and currently not supported for models fit in JAGS)
# mod_output <- bind_rows(fit_DOY$out, fit_ARIMA$out, fit_ETS$out, fit_TSLM$out,
#                         fit_XGBoost$out, fit_prophet$out, fit_NNETAR$out)
# 
#OR if you only want to run (or re-run) one or a few models
mod_output <- read_csv("./model_output/calibration_output.csv") %>%
  # filter(!model_id %in% c("prophet")) %>% #names of re-run models if applicable
  bind_rows(.,fit_randomForest$out) # %>% #bind rows with models to add/replace if applicable

unique(mod_output$model_id)
write.csv(mod_output, "./model_output/calibration_output.csv", row.names = FALSE)
