#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)

# #Load targets data for VERA
# res <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
# inf <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"
# met <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"

#Load data formatting functions
data.format.functions <- list.files("./code/function_library/format_data")
sapply(paste0("./code/function_library/format_data/", data.format.functions),source,.GlobalEnv)

#Format data
obs <- format_chla_obs()

dat_persistence <- format_data_persistence()

dat_historicalMean <- format_data_historicalMean()

dat_DOY_chla <- format_data_DOY_chla()

dat_ETS <- format_data_ETS()

dat_ARIMA <- format_data_ARIMA()
dat_ARIMA_noDrivers <- format_data_ARIMA(include_drivers = FALSE)

dat_TSLM <- format_data_TSLM()

dat_XGBoost <- format_data_XGBoost()

dat_LSTM <- format_data_LSTM()

dat_processModels <- format_data_processModels()

dat_Prophet <- format_data_Prophet()
dat_Prophet_noDrivers <- format_data_Prophet(include_drivers = FALSE)

dat_NNETAR <- format_data_NNETAR()
dat_NNETAR_noDrivers <- format_data_NNETAR(include_drivers = FALSE)

dat_1DProcessModel <- format_data_1DProcessModel()

dat_MARS <- format_data_MARS()

dat_randomForest <- format_data_randomForest()

# do not run this unless you are in a containerized environment that includes
# GLM Version 3.3.1a10 and glmtools
# initialConditions_GLMAED <- format_initialConditions_GLMAED()
# code to format GLM-AED data (both driver data and chl-a data for updating
# initial conditions during the prediction period) is still in development
# and can be found in the code/dev folder

#Write processed data to file
write.csv(obs, "./data/data_processed/chla_obs.csv",row.names = FALSE)
write.csv(dat_persistence, "./data/data_processed/persistence.csv",row.names = FALSE)
write.csv(dat_historicalMean, "./data/data_processed/historicalMean.csv",row.names = FALSE)
write.csv(dat_DOY_chla, "./data/data_processed/DOY.csv",row.names = FALSE)
write.csv(dat_ETS, "./data/data_processed/ETS.csv",row.names = FALSE)
write.csv(dat_ARIMA, "./data/data_processed/ARIMA.csv",row.names = FALSE)
write.csv(dat_ARIMA_noDrivers, "./data/data_processed/ARIMAnoDrivers.csv",row.names = FALSE)
write.csv(dat_TSLM, "./data/data_processed/TSLM.csv",row.names = FALSE)
write.csv(dat_XGBoost, "./data/data_processed/XGBoost.csv",row.names = FALSE)
write.csv(dat_processModels, "./data/data_processed/processModels.csv",row.names = FALSE)
write.csv(dat_LSTM$df.out, "./data/data_processed/LSTM.csv",row.names = FALSE)
write.csv(dat_LSTM$metadata, "./data/data_processed/LSTM_metadata.csv",row.names = FALSE)
write.csv(dat_Prophet, "./data/data_processed/Prophet.csv",row.names = FALSE)
write.csv(dat_Prophet_noDrivers, "./data/data_processed/ProphetnoDrivers.csv",row.names = FALSE)
write.csv(dat_NNETAR, "./data/data_processed/NNETAR.csv",row.names = FALSE)
write.csv(dat_NNETAR_noDrivers, "./data/data_processed/NNETARnoDrivers.csv",row.names = FALSE)
write.csv(dat_1DProcessModel, "./data/data_processed/1DProcessModel.csv",row.names = FALSE)
write.csv(dat_MARS, "./data/data_processed/MARS.csv",row.names = FALSE)
write.csv(dat_randomForest, "./data/data_processed/randomForest.csv",row.names = FALSE)

