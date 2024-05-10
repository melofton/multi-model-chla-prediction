# Calibrate Quinn's 1D process model to EXO
# Author: Quinn Thomas + Mary Lofton
# Date last updated: 07MAY24

# Purpose: Calibrate Quinn's 1D process model to EXO chl-a data at 1.6 m in FCR from Aug. 2018-2021

# Notes:

# This code is adapted from Quinn's repository:
# https://github.com/rqthomas/simple_phyto_model

# install and load packages ----
library(deSolve)
library(tidyverse)

# source model functions
source("./code/model_files/1DProcessModel/build_output_df.R")
source("./code/model_files/1DProcessModel/depth_phyto_model.R")

# dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")

OneDProcessModel <- function(data = dat_1DProcessModel,
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
                                                10), #Xcc
                                     pred_dates = c("2022-01-01","2023-12-31"),
                             forecast_horizon = 35){

  message("building inputs")
  run_datetimes <- seq(lubridate::as_date(pred_dates[1]), lubridate::as_date(pred_dates[2]), by = "1 day")
  
  #Parameters
  #Currency = mmolC/m3
  #Time scale = day
  
  # INPUTS
  
  obs <- data
  num_boxes <- parms[20]
  
  #create a list of the inputs for each day
  
  morphometry_H <- c(497.683, 497.983, 498.283, 498.683, 498.983, 499.283, 499.583, 499.883, 500.183, 500.483, 500.783, 501.083, 501.383, 501.683, 501.983, 502.283, 502.583, 502.883, 503.183, 503.483, 503.783, 504.083, 504.383, 504.683, 505.083, 505.383, 505.683, 505.983, 506.283, 506.583, 506.983)
  morphometry_A <- c(10, 61.408883, 494.615572, 1201.23579, 2179.597283, 3239.620513, 4358.358439, 5637.911458, 6929.077352, 8228.697419, 9469.324081, 10811.30792, 12399.67051, 14484.22802, 16834.20941, 19631.05422, 22583.1399, 25790.70893, 28442.99667, 31155.95008, 36269.3312, 42851.13714, 51179.89109, 59666.85885, 68146.39437, 76424.14457, 85430.25429, 95068.47603, 103030.4489, 111302.1604, 119880.9164)
  morphometry_depth <- abs(max(morphometry_H) - morphometry_H)
  
  # set up prediction df
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(run_datetimes)){
    
    print(run_datetimes[t])
    
    # set forecast dates
    forecast_dates <- seq.Date(from = as.Date(run_datetimes[t]+1), to = as.Date(run_datetimes[t]+forecast_horizon), by = "day")
    
    # Getting inputs
    inputs <- list()
    for(i in 1:length(forecast_dates)){
      
      day_inflow <- obs |> dplyr::filter(datetime == forecast_dates[i],
                                         variable %in% c("inflow_rate", "n_load", "p_load")) |>
        tidyr::pivot_wider(names_from = variable, values_from = observation)
      
      inflow_n <- rep(0,num_boxes)
      inflow_n[1] <- day_inflow$n_load
      
      inflow_p <- rep(0,num_boxes)
      inflow_p[1] <- day_inflow$p_load
      
      outflow <- rep(0,num_boxes)
      outflow[1] <- day_inflow$inflow_rate
      
      inflow_area <- 5
      outflow_area <- 5
      
      temps <- obs |> dplyr::filter(datetime == forecast_dates[i],
                                    variable %in% c("temperature")) |>
        tidyr::pivot_wider(names_from = variable, values_from = observation)
      
      if(length(which(!is.na(temps$temperature))) < 2){
        fill_dates <- c(run_datetimes[1:t],forecast_dates[1:i])
        k <- length(fill_dates)
        j = 2
        while(length(which(!is.na(temps$temperature))) < 2){
          temps <- obs |> dplyr::filter(datetime == fill_dates[k-j],
                                        variable %in% c("temperature")) |>
            tidyr::pivot_wider(names_from = variable, values_from = observation)
          j <- j + 1
        }
      }
      
      temp_func <- approxfun(x = temps$depth, y = temps$temperature, rule = 2)
      
      light <- obs |> dplyr::filter(datetime == forecast_dates[i],
                                    variable %in% c("par")) |>
        dplyr::pull(observation)
      
      delx <- parms[19] / parms[20]
      depths_mid <- seq(from = delx / 2, by = delx, length.out = parms[20])
      depths_interface <- seq(from = 0, to = parms[19], by = delx)
      
      # use this if you don't know morphometry (assumes 1 meter area per depth)
      areas_mid <- rep(1, length(depths_mid))
      areas_interface <- rep(1, length(depths_interface))
      
      #use this if you have your morphometry defined
      areas_mid <- approx(x = morphometry_depth, y = morphometry_A, xout = depths_mid, rule = 2)$y
      areas_interface <- approx(x = morphometry_depth, y = morphometry_A, xout = depths_interface, rule = 2)$y
      
      inputs[[i]] <- list(datetime = forecast_dates[i],
                          par = light,
                          temp = temp_func(depths_mid),
                          inflow_n = inflow_n,
                          inflow_p = inflow_p,
                          outflow = outflow,
                          areas_mid = areas_mid,
                          areas_interface = areas_interface,
                          inflow_area = inflow_area,
                          outflow_area = outflow_area)
      
    }
    
    names(inputs) <- forecast_dates
    
    # Initial conditions
    message("setting initial conditions")
    
    # Initial conditions
    
    yini <- rep(0, 3*parms[20])
    delx <- parms[19] / parms[20]
    depths <- seq(from = delx / 2, by = delx, length.out = parms[20])
    
    # get calibration model output
    if(t == 1){
    initial_day_model <- read_csv("./code/model_files/1DProcessModel/output.csv") %>%
      filter(datetime == run_datetimes[t])
    } else {
      initial_day_model <- read_csv("./code/model_files/1DProcessModel/iterative_output_files/output.csv") %>%
        filter(datetime == run_datetimes[t])
    }
    initial_day_obs <- obs |> dplyr::filter(datetime == run_datetimes[t])
    
    model_chla_EXO_depth <- initial_day_model %>%
      filter(variable == "chla" & depth == 1.5) %>%
      pull(prediction)
    curr_chla <- initial_day_obs %>%
      filter(variable == "chla" & depth == 1.5) %>%
      pull(observation)
    chla_factor = curr_chla / model_chla_EXO_depth
    
    init_phyto <- initial_day_model |>
      dplyr::filter(variable == "chla") |>
      dplyr::mutate(prediction = (prediction * chla_factor) / (1 / parms[24] * 12))
    
    # Assign a value for each depth
    yini[1:num_boxes] <- approx(x = init_phyto$depth, y = init_phyto$prediction, xout = depths, rule = 2)$y
    
    init_din <- initial_day_model |>
      dplyr::filter(variable == "din")
    yini[(num_boxes+1):(num_boxes*2)] <- approx(x = init_din$depth, y = init_din$prediction, xout = depths, rule = 2)$y
    
    init_frp <- initial_day_model |>
      dplyr::filter(variable == "frp")
    yini[(2*num_boxes+1):(num_boxes*3)] <- approx(x = init_frp$depth, y = init_frp$prediction, xout = depths, rule = 2)$y
    
  simulation_time <- forecast_horizon #DAYS
  dt <- 1
  times <- seq(1, simulation_time, by = dt)

  message("running model")
  start_time <- Sys.time()
  print(start_time)
  output <- ode(y = yini,
                times = times,
                func = phyto_depth_model,
                parms = parms,
                inputs = inputs,
                method = "ode45")
  end_time <- Sys.time()
  runtime <- end_time - start_time
  message("end model run")
  print(runtime)
  
  output_df <- build_output_df(output, obs, parms, run_datetimes = forecast_dates)
  write.csv(output_df, "./code/model_files/1DProcessModel/iterative_output_files/output.csv", row.names = FALSE)
  
  # format today's prediction
  chl <- output_df %>%
    filter(depth == 1.5 & variable == "chla") %>%
    pull(prediction)
  
  temp.df <- data.frame(model_id = "OneDProcessModel",
                        reference_datetime = rep(run_datetimes[t],forecast_horizon+1),
                        datetime = c(run_datetimes[t],forecast_dates),
                        variable = "chlorophyll-a",
                        prediction = c(curr_chla,chl))
  
  # bind today's prediction to larger dataframe
  pred.df <- rbind(pred.df, temp.df)
  
  } # end prediction for-loop

  #return predictions
  message("all done!")
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
  
}
