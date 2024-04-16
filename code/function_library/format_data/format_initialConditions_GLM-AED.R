#Format initial conditions for GLM-AED model for chl-a
#Author: Mary Lofton
#Date last updated: 16APR24

#Purpose: develop initial conditions profiles of the following variables for GLM-AED:
#'
#'1. Water temperature
#'2. DO
#'3. NH4
#'4. NO3
#'5. SRP
#'6. DOC
#'
#'this will require accessing the CTD and water chemistry data packages

#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

#'Function to format data for ARIMA model for chla from 2018-2022
#'@param filepath_chemistry filepath to chemistry data product from EDI
#'@param filepath_ctd filepath to modified CTD data product from EDI
#'@param start_date date for which you wish to get initial conditions (first day of model run)
#'@param depths vector of depths for which you need initial conditions values



format_initialConditions_GLMAED <- function(filepath_chemistry = "./data/data_raw/chemistry_2013_2023.csv",
                              filepath_ctd = "./data/data_raw/CTD_2018_2022_FCR50.csv",
                              start_date = "2018-04-20 00:00:00",
                              end_date = "2020-12-31 00:00:00",
                              depths = c(0.1, 0.33, 0.66, 1, 1.33, 1.66, 2, 2.33, 2.66, 3, 3.33, 3.66, 4, 4.33, 4.66, 5, 5.33, 5.66, 6, 6.33, 6.66, 7, 7.33, 7.66, 8, 8.33, 8.66, 9, 9.2),
                              nml_file = "./model_output/GLM-AED/glm3_archived_16APR24.nml"){

  # load packages that must be within container
  library(glmtools)
  
  #get data from start date
  nml_start_date = start_date
  start_date = as.Date(start_date)
  
  #CTD (water temperature, DO)
  ic_ctd <- read_csv(filepath_ctd) %>%
    mutate(Date = date(DateTime)) %>%
    filter(abs(difftime(start_date,Date)) == min(abs(difftime(start_date,Date)))) 
  
  df.final<-data.frame()
  
  for (i in 1:length(depths)){
    
    ctd_layer <- ic_ctd %>% 
      group_by(Date) %>% 
      slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
    
    # Bind each of the data layers together.
    df.final = bind_rows(df.final, ctd_layer)
    
  }
  
  the_temps <- df.final %>%
    pull(Temp_C)
  
  OXY_oxy <- df.final %>%
    mutate(DO_mmolm3 = DO_mgL*(1000/31.999)) %>%
    pull(DO_mmolm3)
  
  # chemistry (NH4, NO3, SRP, DOC)
  ic_chem <- read_csv(filepath_chemistry) %>%
    mutate(Date = date(DateTime)) %>%
    filter(abs(difftime(start_date,Date)) == min(abs(difftime(start_date,Date))),
           Reservoir == "FCR",
           Site == 50) 
  
  #linear interpolation to fill in missing values
  depth.df <- data.frame(Depth_m = depths)
  df <- left_join(depth.df, ic_chem, by = "Depth_m") %>%
    select(Depth_m, NH4_ugL, NO3NO2_ugL, SRP_ugL, DOC_mgL)
  
  variables <- c("NH4_ugL","NO3NO2_ugL","SRP_ugL","DOC_mgL")
  
  for(i in 1:length(variables)){
    
    #replace missing values at top of profile
    df[cumall(is.na(df[,variables[i]])),variables[i]] <- as.double(subset(df, !is.na(df[,variables[i]]))[1,variables[i]])
    
    #create interpolated profile
    interp <- na.approx(df[,variables[i]])
    
    #fill in missing values at bottom of profile
    if(length(interp) < length(depth.df$Depth_m)){
      num_NA = length(depth.df$Depth_m) - length(interp)
      nas <- rep(NA, times = num_NA)
      interp2 = c(interp, nas)
      interp3 <- na.locf(interp2)
      df[,variables[i]] <- interp3
    } else {
      df[,variables[i]] <- interp
    }
    
  }
  
  NIT_amm <- df %>%
    mutate(NH4_mmolm3 = NH4_ugL/18.04) %>%
    pull(NH4_mmolm3)
  
  NIT_nit <- df %>%
    mutate(NO3_mmolm3 = NO3NO2_ugL/62.0049) %>%
    pull(NO3_mmolm3)
  
  PHS_frp <- df %>%
    mutate(SRP_mmolm3 = SRP_ugL/94.9714) %>%
    pull(SRP_mmolm3)
  
  OGM_doc <- df %>%
    mutate(DOC_mmolm3 = DOC_mgL*(1000/12.011)) %>%
    pull(DOC_mmolm3)
  
  start_nml <- glmtools::read_nml(nml_file = nml_file)
  
  start_nml[["time"]][["start"]] <- nml_start_date
  start_nml[["time"]][["stop"]] <- end_date
  start_nml[["init_profiles"]][["the_temps"]] <- the_temps
  start_nml[["init_profiles"]][["wq_init_vals"]] <- round(c(OXY_oxy, NIT_amm, NIT_nit,
                                                      PHS_frp, OGM_doc),3)
  
  glmtools::write_nml(start_nml, file = "./model_output/GLM-AED/glm3.nml")
  
  
}
