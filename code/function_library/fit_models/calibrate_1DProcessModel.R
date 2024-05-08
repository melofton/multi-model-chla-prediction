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

dat_1DProcessModel <- read_csv("./data/data_processed/1DProcessModel.csv")

calibrate_1DProcessModel <- function(data = dat_1DProcessModel,
                                     parms = list(w_p = c(-0.001, -0.001, -0.001), #w_p (negative is down, positive is up)
                                                R_growth = c(2,2,2), #R_growth
                                                theta_growth = c(1.08,1.08,1.08), #theta_growth
                                                light_extinction = 1, #light_extinction
                                                I_K = c(10, 10, 10), #I_K
                                                N_o = c(0.0,0.0,0.0), #N_o
                                                K_N = c(2,2,2), #K_N
                                                P_o = c(0.0,0.0,0.0), #P_o
                                                K_P = c(0.0001,0.0001,0.0001), #K_P
                                                f_pr = c(0.1,0.1, 0.1), #f_pr
                                                R_resp = c(0.16,0.16,0.16), #R_resp
                                                theta_resp = c(1.08,1.08,1.08), #theta_resp
                                                T_std = c(10,10,10), #T_std
                                                T_opt = c(28,28,28), #T_opt
                                                T_max = c(35,35,35), #T_max
                                                N_C_ratio = 0.02, #N_C_ratio
                                                P_C_ratio = 0.002, #P_C_ratio
                                                phyto_flux_top = c(0,0,0), #phyto_flux_top
                                                area = 1, #area (not used)
                                                lake_depth = 9.5,# lake_depth
                                                num_boxes = 38,# num_boxes
                                                KePHY = c(0.005,0.005,0.005),#KePHYTO
                                                D_temp = 0.01, #D_temp
                                                Xcc = c(20,20,20),#Xcc
                                                num_phytos = 3,
                                                phyto_flux_bottom = c(0,0,0)), #num_phytos,
                                     cal_dates = c("2018-08-06","2021-12-31"),
                                     save_plots = TRUE,
                                     inputs = NULL){

  message("building inputs")
  run_datetimes <- seq(lubridate::as_date(cal_dates[1]), lubridate::as_date(cal_dates[2]), by = "1 day")
  
  #Parameters
  #Currency = mmolC/m3
  #Time scale = day
  
  # INPUTS
  
  obs <- data
  num_boxes <- parms[["num_boxes"]]
  
  #create a list of the inputs for each day
  
  morphometry_H <- c(497.683, 497.983, 498.283, 498.683, 498.983, 499.283, 499.583, 499.883, 500.183, 500.483, 500.783, 501.083, 501.383, 501.683, 501.983, 502.283, 502.583, 502.883, 503.183, 503.483, 503.783, 504.083, 504.383, 504.683, 505.083, 505.383, 505.683, 505.983, 506.283, 506.583, 506.983)
  morphometry_A <- c(10, 61.408883, 494.615572, 1201.23579, 2179.597283, 3239.620513, 4358.358439, 5637.911458, 6929.077352, 8228.697419, 9469.324081, 10811.30792, 12399.67051, 14484.22802, 16834.20941, 19631.05422, 22583.1399, 25790.70893, 28442.99667, 31155.95008, 36269.3312, 42851.13714, 51179.89109, 59666.85885, 68146.39437, 76424.14457, 85430.25429, 95068.47603, 103030.4489, 111302.1604, 119880.9164)
  morphometry_depth <- abs(max(morphometry_H) - morphometry_H)
  
  if(is.null(inputs)){
  inputs <- list()
  for(i in 1:length(run_datetimes)){
    
    day_inflow <- obs |> dplyr::filter(datetime == run_datetimes[i],
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
    
    temps <- obs |> dplyr::filter(datetime == run_datetimes[i],
                                  variable %in% c("temperature")) |>
      tidyr::pivot_wider(names_from = variable, values_from = observation)
    
    if(length(which(!is.na(temps$temperature))) < 2){
      j = 1
      while(length(which(!is.na(temps$temperature))) < 2){
        temps <- obs |> dplyr::filter(datetime == run_datetimes[i-j],
                                      variable %in% c("temperature")) |>
          tidyr::pivot_wider(names_from = variable, values_from = observation)
        j <- j + 2
      }
    }
    
    temp_func <- approxfun(x = temps$depth, y = temps$temperature, rule = 2)
    
    light <- obs |> dplyr::filter(datetime == run_datetimes[i],
                                  variable %in% c("par")) |>
      dplyr::pull(observation)
    
    delx <- parms[["lake_depth"]] / parms[["num_boxes"]]
    depths_mid <- seq(from = delx / 2, by = delx, length.out = parms[["num_boxes"]])
    depths_interface <- seq(from = 0, to = parms[["lake_depth"]], by = delx)
    
    # use this if you don't know morphometry (assumes 1 meter area per depth)
    areas_mid <- rep(1, length(depths_mid))
    areas_interface <- rep(1, length(depths_interface))
    
    #use this if you have your morphometry defined
    areas_mid <- approx(x = morphometry_depth, y = morphometry_A, xout = depths_mid, rule = 2)$y
    areas_interface <- approx(x = morphometry_depth, y = morphometry_A, xout = depths_interface, rule = 2)$y
    
    inputs[[i]] <- list(datetime = run_datetimes[i],
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
  
  names(inputs) <- run_datetimes
  } 
  
  # Initial conditions
  message("setting initial conditions")
  yini <- rep(0, 5*parms[["num_boxes"]])
  delx <- parms[["lake_depth"]] / parms[["num_boxes"]]
  depths <- seq(from = delx / 2, by = delx, length.out = parms[["num_boxes"]])
  
  initial_day <- obs |> dplyr::filter(datetime == run_datetimes[1])
  
  init_phyto <- initial_day |>
    dplyr::filter(variable == "chla") |>
    dplyr::mutate(hot = (1/3)*observation / (1 / parms[["Xcc"]][1] * 12),
                  cold = (1/3)*observation / (1 / parms[["Xcc"]][2] * 12),
                  Nfixer = (1/3)*observation / (1 / parms[["Xcc"]][3] * 12))
  
  # Assign a value for each depth
  yini[1:num_boxes] <- approx(x = init_phyto$depth, y = init_phyto$hot, xout = depths, rule = 2)$y
  yini[(num_boxes+1):(num_boxes*2)] <- approx(x = init_phyto$depth, y = init_phyto$cold, xout = depths, rule = 2)$y
  yini[(2*num_boxes+1):(num_boxes*3)] <- approx(x = init_phyto$depth, y = init_phyto$Nfixer, xout = depths, rule = 2)$y
  
  init_din <- initial_day |>
    dplyr::filter(variable == "din")
  
  yini[(3*num_boxes+1):(num_boxes*4)] <- approx(x = init_din$depth, y = init_din$observation, xout = depths, rule = 2)$y
  
  init_frp <- initial_day |>
    dplyr::filter(variable == "frp")
  
  yini[(4*num_boxes+1):(num_boxes*5)] <- approx(x = init_frp$depth, y = init_frp$observation, xout = depths, rule = 2)$y
  
  simulation_time <- length(run_datetimes) #DAYS
  dt <- 1
  times <- seq(1, simulation_time, by = dt)
  
  message("running model")
  
  output <- ode(y = yini,
                times = times,
                func = phyto_depth_model,
                parms = parms,
                inputs = inputs,
                method = "ode45")
  
  output_df <- build_output_df(output, obs, parms, run_datetimes)
  
  # Visualize output
  message("building figures")
  # assess model run
  assess_model_run <- output_df |>
    filter(depth == 1.5 | is.na(depth)) |>
    ggplot(aes(x = datetime, y = prediction)) +
    geom_point(aes(y = observation)) +
    geom_line(color = "lightblue3") +
    facet_wrap(~variable, scale = "free") +
    theme_bw()
  if(save_plots == TRUE){
    ggsave(assess_model_run, filename = "./figures/1DProcessModel/assess_1Dmodel_run.png",
           height = 6.4, width = 12, units = "in")
  }
  
  # temporal-spatial plot of the concentrations
  par(oma = c(0, 0, 3, 0))   # set margin size (oma) so that the title is included
  col <- topo.colors
  lake_depth <- parms[20]
  
  mat_phyto <- output_df |>
    filter(variable == "phyto") |>
    select(datetime, depth, prediction) |>
    pivot_wider(names_from = depth, values_from = prediction) |>
    select(-datetime)
  
  if(save_plots == TRUE){
    jpeg("./figures/1DProcessModel/phyto_heatmap.jpeg", res = 300, width = 5, height = 3.5, units = "in")
    filled.contour(x = run_datetimes,
                   y = depths,
                   z = as.matrix(mat_phyto),
                   color = col,
                   ylim = c(lake_depth, 0),
                   zlim = range(c(mat_phyto)),
                   xlab = "time, days",
                   ylab = "Depth, m",
                   main = "Concentration, mmolC/m3")
    dev.off()
  }
  
  mat_din <- output_df |>
    filter(variable == "din") |>
    select(datetime, depth, prediction) |>
    pivot_wider(names_from = depth, values_from = prediction) |>
    select(-datetime)
  
  if(save_plots == TRUE){
    jpeg("./figures/1DProcessModel/din_heatmap.jpeg", res = 300, width = 5, height = 3.5, units = "in")
    filled.contour(x = run_datetimes,
                   y = depths,
                   z = as.matrix(mat_din),
                   color = col,
                   ylim = c(lake_depth, 0),
                   zlim = range(c(mat_din)),
                   xlab = "time, days",
                   ylab = "Depth, m",
                   main = "Concentration, mmolN/m3")
    dev.off()
  }
  
  mat_frp <- output_df |>
    filter(variable == "frp") |>
    select(datetime, depth, prediction) |>
    pivot_wider(names_from = depth, values_from = prediction) |>
    select(-datetime)
  
  if(save_plots == TRUE){
    jpeg("./figures/1DProcessModel/frp_heatmap.jpeg", res = 300, width = 5, height = 3.5, units = "in")
    filled.contour(x = run_datetimes,
                   y = depths,
                   z = as.matrix(mat_frp),
                   color = col,
                   ylim = c(lake_depth, 0),
                   zlim = range(c(mat_frp)),
                   xlab = "time, days",
                   ylab = "Depth, m",
                   main = "Concentration, mmolP/m3")
    dev.off()
  }
  
  mat_par <- output_df |>
    filter(variable == "PAR") |>
    select(datetime, depth, prediction) |>
    pivot_wider(names_from = depth, values_from = prediction) |>
    select(-datetime)
  
  if(save_plots == TRUE){
    jpeg("./figures/1DProcessModel/par_heatmap.jpeg", res = 300, width = 5, height = 3.5, units = "in")
    filled.contour(x = run_datetimes,
                   y = depths,
                   z = as.matrix(mat_par),
                   color = col,
                   ylim = c(lake_depth, 0),
                   zlim = range(c(mat_par)),
                   xlab = "time, days",
                   ylab = "Depth, m",
                   main = "PAR, umol/m2/sec")
    dev.off()
  }
  
  plot_temp <- output_df |>
    filter(variable == "temperature") |>
    ggplot(aes(x = datetime, y = observation)) +
    geom_line() +
    facet_wrap(~depth, scale = "free")
  if(save_plots == TRUE){
    ggsave(plot_temp, filename = "./figures/1DProcessModel/plot_temp.png",
           height = 6.4, width = 12, units = "in")
  }
  
  # Additional plotting code that could be deployed
  
  # output_df |>
  #   filter(variable == "frp") |>
  #   ggplot(aes(x = datetime, y = prediction)) +
  #   geom_line() +
  #   geom_point(aes(y = observation)) +
  #   facet_wrap(~depth, scale = "free")
  # 
  # output_df |>
  #   filter(variable == "din") |>
  #   ggplot(aes(x = datetime, y = prediction)) +
  #   geom_line() +
  #   geom_point(aes(y = observation)) +
  #   facet_wrap(~depth, scale = "free")
  # 
  # output_df |> filter(depth %in% c(1,8),
  #                     variable == "din") |>
  #   ggplot(aes(x = datetime, y = prediction, color = factor(depth))) +
  #   geom_line()
  
  message("all done!")
  return(list(output_df = output_df, inputs = inputs))
}
