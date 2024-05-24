#Format data for NNETAR model for chl-a
#Author: Mary Lofton
#Date last updated: 24MAY24

#Purpose: create daily dataframe from Aug 6, 2018 to Dec. 31, 2023 of 
#daily median of the following variables:

#'1. Met: SWR, air temp, wind speed 
#'2. Inflow: discharge 
#'3. Catwalk: EXO chl-a, 1.6 water temp 
#'4. YSI_Secchi: Kd from Secchi
#'5. Chem: SRP @ 1.6, DIN @ 1.6
#'
#'Interpolation Methods
#'1. Linear interpolation for high-frequency variables
#'2. For low-frequency variables (chem, Secchi), create a DOY model (GAM from ggplot)
#'using 2013-2018 data to fill in for 2018-2022
#'3. If data are missing from beginning or end of dataset, curently just filling with 
#'first or most recent value

#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)
# library(glmtools)


#source internal functions
source("./code/function_library/format_data/interpolation.R")

#'Function to format data for ARIMA model for chla from 2018-2022
#'@param filepath_chemistry filepath to chemistry data product from EDI
#'@param res_url url to in situ reservoir targets data for VERA
#'#'@param inf_url url to inflow targets data for VERA
#'@param met_url url to meteorology targets data for VERA



format_data_NNETAR <- function(filepath_chemistry = "./data/data_raw/chemistry_2013_2023.csv",
                              res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                              inf_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz",
                              met_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz",
                              start_date = "2018-08-06",
                              end_date = "2023-12-31",
                              include_drivers = TRUE){
  
  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "datetime"
  
  #set GLM-AED output filepath for vars that use this method of interp
  glm_output_filepath = "./Eco-KGML-transfer-learning/data/data_raw/ModelOutputFCR/output.nc"
  
  
  ##MET----
  #message
  message("interpolating met")
  #read in data
  met_vars <- c("PAR_umolm2s_mean","AirTemp_C_mean","WindSpeed_ms_mean")
  #initial data wrangling
  met <- read_csv(met_url) %>%
    filter(site_id == "fcre" & variable %in% met_vars & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, variable, observation) %>%
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    filter(datetime >= start_date)
  
  #interpolation
  met2 <- interpolate(daily_dates = daily_dates,
                      data = met,
                      variables = met_vars,
                      method = "linear")
  
  
  ##INFLOW----
  
  #message
  message("interpolating inflow")
  
  inf <- read_csv(inf_url) %>%
    filter(variable == "Flow_cms_mean" & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, observation) %>%
    rename(Flow_cms_mean = observation) %>%
    filter(datetime >= start_date)
  
  #interpolation
  inf2 <- interpolate(daily_dates = daily_dates,
                      data = inf,
                      variables = c("Flow_cms_mean"),
                      method = "linear")
  
  ##EXO----
  
  #message
  message("interpolating reservoir data")
  #read in data
  res_vars <- c("Chla_ugL_mean","Temp_C_mean","Secchi_m_sample")
  #initial data wrangling
  res <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable %in% res_vars & year(datetime) %in% c(2018:2023) & depth_m %in% c(1.6, NA)) %>%
    select(datetime, variable, observation) %>%
    mutate(datetime = date(datetime)) %>%
    group_by(datetime, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    mutate(LightAttenuation_Kd = (1.7/Secchi_m_sample)) %>%
    select(-Secchi_m_sample) %>%
    filter(datetime >= start_date)
  
  #interpolation
  res2 <- interpolate(daily_dates = daily_dates,
                      data = res,
                      variables = c("Chla_ugL_mean","Temp_C_mean","LightAttenuation_Kd"),
                      method = "linear")
  
  
  ##CHEM----
  #not removing flags here b/c I don't think any of these will outweigh
  #the uncertainty introduced via interpolation
  
  #message
  message("interpolating chem")
  
  ##start with Site 50
  chem <- read_csv(filepath_chemistry,
                   col_types = cols(
                     .default = col_double(),
                     Reservoir = col_character(),
                     DateTime = col_datetime(format = ""),
                     DIC_mgL = col_double(),
                     DC_mgL = col_double(),
                     DN_mgL = col_double()
                   )) %>%
    filter(Reservoir == "FCR" & Site == 50 & Depth_m == 1.6) %>%
    select(DateTime, SRP_ugL, NH4_ugL, NO3NO2_ugL) %>%
    mutate(datetime = date(DateTime),
           DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
    group_by(datetime) %>%
    summarize(SRP_ugL = mean(SRP_ugL, na.rm = TRUE),
              DIN_ugL = mean(DIN_ugL, na.rm = TRUE)) %>%
    filter(datetime >= start_date)
  
  # this will be needed if you decide to use DOY interpolation
  # chem_cal <- chem %>%
  #   filter_at(vars(DIN_ugL, SRP_ugL),all_vars(!is.na(.))) %>%
  #   filter(datetime <= start_date)
  # 
  # chem_interp <- chem %>%
  #   filter(datetime >= start_date)
  
  #interpolation
  chem2 <- interpolate(daily_dates = daily_dates,
                       data = chem,
                       variables = c("DIN_ugL","SRP_ugL"),
                       method = "linear")
  
  #PREPARE FINAL DF
  message("preparing final df")
  
  df.out <- left_join(met2, inf2) %>%
    left_join(., res2) %>%
    left_join(., chem2) %>%
    select(datetime, AirTemp_C_mean, PAR_umolm2s_mean, WindSpeed_ms_mean, Flow_cms_mean, Temp_C_mean,
           SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL_mean,
           Flag_AirTemp_C_mean, Flag_PAR_umolm2s_mean, Flag_WindSpeed_ms_mean, Flag_Flow_cms_mean, Flag_Temp_C_mean, Flag_SRP_ugL,
           Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL_mean)
  
  if(include_drivers == FALSE){
    df.out <- df.out %>%
      select(datetime, Chla_ugL_mean,
             Flag_Chla_ugL_mean)
  }
  
  return(df.out)
}
