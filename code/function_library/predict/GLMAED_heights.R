# Make iterative chl-a predictions with update IC using GLM-AED
# Author: Mary Lofton
# Date: 29APR24

# Purpose: Make iterative chl-a predictions with update IC using GLM-AED

# Notes:

# This script needs to be run in a container with GLM Version 3.3.1a10
#
# Instructions
# 1.	Start Docker
# 2.	Open Terminal and type in the following command:
#   
#   docker run --rm -ti -v /Users/MaryLofton:/home/rstudio -e PASSWORD=yourpassword -p 8787:8787 rqthomas/flare-rocker:4.3.1
# 
# 3.	Open an internet browser and navigate to the following: http://localhost:8787
# 4.	Sign into RStudio
# a.	Username: rstudio
# b.	Password: yourpassword
# 5.  Navigate to correct RProject and script to run GLM-AED

library(tidyverse)
library(lubridate)
library(GLM3r)
library(glmtools)
library(ncdf4)

# Read in data for troubleshooting (commented out when running workflow with other models)
# dat_GLMAED <- read_csv("./data/data_processed/GLMAED.csv")


GLMAED_heights <- function(spinup_folder = "./code/model_files/GLM-AED/spinup",
                   prediction_folder = "./code/model_files/GLM-AED/prediction_heights",
                   rerun_spinup = FALSE,
                   spinup_dates = c('2018-04-20 12:00:00','2021-01-01 12:00:00'),
                   start_from_spinup = FALSE,
                   pred_dates = seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-01-05"), by = "day"),
                   forecast_horizon = 35,
                   wq_vars = c('OXY_oxy','CAR_dic','CAR_pH','CAR_ch4','SIL_rsi','NIT_amm','NIT_nit','PHS_frp','OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop','OGM_docr','OGM_donr','OGM_dopr','OGM_cpom','PHY_hot','PHY_cold','PHY_Nfixer'),
                   data = dat_GLMAED,
                   phyto_nml_file = "/aed/aed2_phyto_pars_24MAY24_MEL.nml",
                   container = "rocker-flare_4.4"){
  
  # run spinup if user specifies it
  if(rerun_spinup == TRUE){
    
    message("setting spinup dates")
    spinup_nml_file <- file.path(paste0(spinup_folder,"/glm3.nml"))
    spinup_nml <- glmtools::read_nml(nml_file = spinup_nml_file)
    spinup_nml <- glmtools::set_nml(spinup_nml, arg_name = "start", arg_val = spinup_dates[1])
    spinup_nml <- glmtools::set_nml(spinup_nml, arg_name = "stop", arg_val = spinup_dates[2])
    glmtools::write_nml(spinup_nml, file = spinup_nml_file)
    
    message("re-running spinup")
    GLM3r::run_glm(sim_folder = spinup_folder,
                   nml_file = "glm3.nml",
                   verbose = TRUE)
    
    message("end spinup model run")
    
  }
  
  # set up prediction data frame and dates
  pred_dates <- as.POSIXct(paste(pred_dates, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")
  
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  # name nc files
  spinup_nc_file <- file.path(paste0(spinup_folder, "/output/output.nc"))
  pred_nc_file <- file.path(paste0(prediction_folder, "/output/output.nc"))
  
  # loop through predictions
  for(t in 1:length(pred_dates)){
    
    message(paste0("reference datetime: ",pred_dates[t]))
    stop_date <- last(seq.Date(from = as.Date(pred_dates[t])+1, to = as.Date(pred_dates[t])+forecast_horizon, by = "day"))
    stop_date <- as.POSIXct(paste(stop_date, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")
    message(paste0("stop datetime: ",stop_date))
    
    # out_vars <- sim_vars(file = spinup_nc_file)
    
    #pull IC from previous sim and set IC for next sim; start with spinup if t == 1
    if(t == 1 & start_from_spinup == TRUE){
      
      current_nc_file <- spinup_nc_file
      
      # pull IC from previous sim
      message("retrieving initial conditions")
      current_nc <- ncdf4::nc_open(current_nc_file)
      
      # the_depths (and then calculate num_depths from this)
      message("the_depths")
      all_elevs <- ncdf4::ncvar_get(current_nc, var = "H")
      the_elevs <- all_elevs[which(!is.na(all_elevs[,ncol(all_elevs)])),ncol(all_elevs)]
      
    } else {
      
      # pull from previous sim's forecast file in this case
      current_nc_file <- pred_nc_file
      
      # pull IC from previous sim
      message("retrieving initial conditions")
      current_nc <- ncdf4::nc_open(current_nc_file)
      
      # the_depths (and then calculate num_depths from this)
      message("the_depths")
      all_elevs <- ncdf4::ncvar_get(current_nc, var = "H")
      current_elevs <- all_elevs[which(!is.na(all_elevs[,ncol(all_elevs)])),4] # this is column for noon on day 2 of sim
      the_elevs <- current_elevs[!is.na(current_elevs)]
      
    }
    
    # num_depths
    message("num_depths")
    num_depths <- length(the_elevs)
    
    # lake_depth
    message("lake_depth")
    lake_depth <- max(the_elevs)
    the_depths <- rev(lake_depth - the_elevs)

    # temperature
    message("temp")
    current_temp <- glmtools::get_var(current_nc_file, var_name = "temp", 
                                      reference ="surface", z_out = the_depths,
                                      t_out = pred_dates[t]) %>%
      filter(hour(DateTime) == 12) 
    the_temps <- c(unlist(unname(current_temp[1,-1])))
    
    # snow_thickness
    message("snow_thickness")
    snow_thickness <- glmtools::get_var(current_nc_file, var_name = "snow_thickness",
                                        t_out=pred_dates[t]) %>%
      filter(hour(DateTime) == 12) %>%
      pull(snow_thickness)
    
    # white_ice_thickness
    message("white_ice_thickness")
    white_ice_thickness <- glmtools::get_var(current_nc_file, var_name = "white_ice_thickness",
                                             t_out=pred_dates[t]) %>%
      filter(hour(DateTime) == 12) %>%
      pull(white_ice_thickness)
    
    # blue_ice_thickness
    message("blue_ice_thickness")
    blue_ice_thickness <- glmtools::get_var(current_nc_file, var_name = "blue_ice_thickness",
                                            t_out=pred_dates[t]) %>%
      filter(hour(DateTime) == 12) %>%
      pull(blue_ice_thickness)
    
    # avg_surf_temp
    message("avg_surf_temp")
    avg_surf_temp <- glmtools::get_var(current_nc_file, var_name = "avg_surf_temp",
                                       t_out=pred_dates[t]) %>%
      filter(hour(DateTime) == 12) %>%
      pull(avg_surf_temp)
    
    # restart_variables
    message("restart_variables")
    restart_variables <- ncdf4::ncvar_get(current_nc, var = "restart_variables")
    
    # restart_mixer_count
    message("restart_mixer_count")
    all_restart_mixer_count <- ncdf4::ncvar_get(current_nc, var = "Mixer_Count")
    restart_mixer_count <- last(all_restart_mixer_count)
    
    # all the water quality
    message("water quality variables")
    all_ic <- NULL
    
    for(i in 1:length(wq_vars)){
      message(wq_vars[i])
      current_var <- glmtools::get_var(current_nc_file, var_name = wq_vars[i], 
                                       reference="surface", z_out = the_depths,
                                       t_out=pred_dates[t]) %>%
        filter(hour(DateTime) == 12)

      if(is_empty(grep("PHY",wq_vars[i]))){
        
        # concatenate other vars
        all_ic <- c(all_ic,unlist(unname(current_var[1,-1])))
        
      } else {
        
        # save phyto biomass to concatenate later after updating
        if(wq_vars[i] == "PHY_hot"){
          PHY_hot <- unlist(unname(current_var[1,-1]))
        } else if(wq_vars[i] == "PHY_cold"){
          PHY_cold <- unlist(unname(current_var[1,-1]))
        } else if(wq_vars[i] == "PHY_Nfixer"){
          PHY_Nfixer <- unlist(unname(current_var[1,-1]))
        }
        
      }
      
    }
    
    # pull chl-a to calculate proportions for updating groups
    message("chlorophyll-a")
    chl <- glmtools::get_var(current_nc_file, var_name = "PHY_tchla", 
                                  reference="surface", z_out= the_depths,
                                  t_out=pred_dates[t]) %>%
      filter(hour(DateTime) == 12)
    chla_prev <- unlist(unname(chl[1,-1]))
    
    ncdf4::nc_close(current_nc)
    
    # update phytoplankton initial conditions using chl-a observations
    message("updating chl-a initial conditions")
    curr_chla <- data %>%
      filter(datetime == as.Date(pred_dates[t])) %>%
      pull(Chla_ugL_mean)
    
    # pull Xcc parameters
    phyto_nml_filepath <- file.path(paste0(prediction_folder, phyto_nml_file))
    phyto_nml <- glmtools::read_nml(nml_file = phyto_nml_filepath)
    Xcc <- phyto_nml$phyto_data[["pd%Xcc"]]
    
    # calculate proportions of groups to update with chl-a
    PHY_list <- list(PHY_hot, PHY_cold, PHY_Nfixer)
    EXO_depth <- which.min(abs(the_depths - 1.6))
    model_chla_EXO_depth <- chla_prev[EXO_depth]
    chla_factor = curr_chla / model_chla_EXO_depth
    
    for(j in 1:length(PHY_list)){
      chla_group = PHY_list[[j]] / Xcc[j] * 12.0
      new_chla_group = chla_group * chla_factor
      new_group_biomass = new_chla_group / 12 * Xcc[j]
      PHY_list[[j]] <- new_group_biomass
    }
    
    all_ic <- c(all_ic, PHY_list[[1]], PHY_list[[2]], PHY_list[[3]])
    
    # set IC for next sim
    message("setting initial conditions in nml file")
    ic_nml_file <- file.path(paste0(prediction_folder,"/glm3.nml"))
    ic_nml <- glmtools::read_nml(nml_file = ic_nml_file)
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "wq_init_vals", arg_val = round(all_ic,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "the_temps", arg_val = round(the_temps,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "start", arg_val = as.character(pred_dates[t]))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "stop", arg_val = as.character(stop_date))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "snow_thickness", arg_val = round(snow_thickness,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "white_ice_thickness", arg_val = round(white_ice_thickness,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "blue_ice_thickness", arg_val = round(blue_ice_thickness,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "avg_surf_temp", arg_val = round(avg_surf_temp,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "restart_variables", arg_val = round(restart_variables,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "num_heights", arg_val = num_depths)
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "lake_depth", arg_val = lake_depth)
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "the_heights", arg_val = round(the_elevs,10))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "the_sals", arg_val = rep(0,num_depths))
    ic_nml <- glmtools::set_nml(ic_nml, arg_name = "restart_mixer_count", arg_val = restart_mixer_count)
    
    message("writing nml file")
    glmtools::write_nml(ic_nml, file = ic_nml_file)
    
    # run model
    message("running GLM-AED")
    GLM3r::run_glm(sim_folder = prediction_folder,
                   nml_file = "glm3.nml",
                   verbose = TRUE)
    
    # pull chl-a prediction
    message("pulling and formatting today's prediction")
    chl <- glmtools::get_var(pred_nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6) %>%
      filter(hour(DateTime) == 12) %>%
      pull(PHY_tchla_1.6)
    if(container == "rocker-flare_4.4"){
    chl <- chl[-length(chl)]
    }
    
    # format today's prediction
    temp.df <- data.frame(model_id = "GLM-AED",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = seq.Date(from = as.Date(pred_dates[t]), to = as.Date(stop_date), by ="day"),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,chl))
    
    # bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df) 
    
    
  } # end prediction loop
  
  #return predictions
  pred.df <- pred.df %>%
    mutate(reference_datetime = date(reference_datetime),
           prediction = as.double(prediction)) 
  return(pred.df)
  
}
