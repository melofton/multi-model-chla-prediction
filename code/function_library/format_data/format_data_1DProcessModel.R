#Format data for process models for chl-a
#Author: Mary Lofton
#Date last updated: 07MAY24

#Purpose: create daily dataframe from Aug 6, 2018 to Dec. 31, 2023 of 
#daily median of the following variables:

#'1. Met: PAR 
#'2. Catwalk: EXO chl-a, water temp 
#'3. Chem: SRP @ 1.6, DIN @ 1.6
#'4. Inflow w/ DIN and FRP loads
#'5. Secchi (to help with diagnostics)
#'
#'Interpolation Methods
#'1. Linear interpolation 


#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)
library(rMR)

#'Function to format data for ARIMA model for chla from 2018-2022
#'@param filepath_chemistry filepath to chemistry data product from EDI
#'@param res_url url to in situ reservoir targets data for VERA
#'@param met_url url to meteorology targets data for VERA
#'@param inf_url url to inflow targets data for VERA
#'@param start_date beginning date of dataset
#'@param end_date end date of dataset



format_data_1DProcessModel <- function(filepath_chemistry = "./data/data_raw/chemistry_2013_2023.csv",
                                       filepath_ctd = "./data/data_raw/CTD_2018_2022_FCR50.csv",
                              res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                              met_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz",
                              inf_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz",
                              start_date = "2018-08-06",
                              end_date = "2023-12-31"){
  
  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "datetime"
  
  ##### INFLOW DATA ################################################
  message("interpolating inflow")
  inf_targets <- readr::read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz",
                                  show_col_types = FALSE)
  
  variables <- c("datetime","FLOW","NIT_amm", "NIT_nit", "PHS_frp")
  
  inf <- inf_targets |>
    filter(!variable %in% c("DN_mgL_sample", "DC_mgL_sample")) |>
    select(datetime, variable, observation) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    dplyr::right_join(daily_dates, by = "datetime") |>
    dplyr::arrange(datetime) |>
    dplyr::select(-DRSI_mgL_sample) |>
    mutate(across(Flow_cms_mean:DIC_mgL_sample, imputeTS::na_interpolation)) |>
    tidyr::fill(Flow_cms_mean:DIC_mgL_sample, .direction = "up") |>
    tidyr::fill(Flow_cms_mean:DIC_mgL_sample, .direction = "down") |>
    dplyr::rename(TEMP = Temp_C_mean,
                  FLOW = Flow_cms_mean) |>
    dplyr::mutate(NIT_amm = NH4_ugL_sample*1000*0.001*(1/18.04),
                  NIT_nit = NO3NO2_ugL_sample*1000*0.001*(1/62.00), #as all NO2 is converted to NO3
                  PHS_frp = SRP_ugL_sample*1000*0.001*(1/94.9714),
                  OGM_doc = DOC_mgL_sample*1000*(1/12.01)* 0.10,  #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
                  OGM_docr = 1.5*DOC_mgL_sample*1000*(1/12.01)* 0.90, #assuming 90% of total DOC is in recalcitrant DOC pool
                  TN_ugL = TN_ugL_sample*1000*0.001*(1/14),
                  TP_ugL = TP_ugL_sample*1000*0.001*(1/30.97),
                  OGM_poc = 0.1*(OGM_doc+OGM_docr), #assuming that 10% of DOC is POC (Wetzel page 755
                  OGM_don = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.10, #DON is ~5x greater than PON (Wetzel page 220)
                  OGM_donr = (5/6)*(TN_ugL_sample-(NIT_amm+NIT_nit))*0.90, #to keep mass balance with DOC, DONr is 90% of total DON
                  OGM_pon = (1/6)*(TN_ugL_sample-(NIT_amm+NIT_nit)), #detemined by subtraction
                  OGM_dop = 0.3*(TP_ugL_sample-PHS_frp)*0.10, #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
                  OGM_dopr = 0.3*(TP_ugL_sample-PHS_frp)*0.90,#to keep mass balance with DOC & DON, DOPr is 90% of total DOP
                  OGM_pop = TP_ugL_sample-(OGM_dop+OGM_dopr+PHS_frp), # #In lieu of having the adsorbed P pool activated in the model, need to have higher complexed P
                  CAR_dic = DIC_mgL_sample*1000*(1/52.515),
                  OXY_oxy = rMR::Eq.Ox.conc(TEMP, elevation.m = 506, #creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
                                            bar.press = NULL, bar.units = NULL,
                                            out.DO.meas = "mg/L",
                                            salinity = 0, salinity.units = "pp.thou"),
                  OXY_oxy = OXY_oxy *1000*(1/32),
                  CAR_ch4 = CH4_umolL_sample,
                  PHY_cyano = 0,
                  PHY_green = 0,
                  PHY_diatom = 0,
                  SALT = 0) |>
    dplyr::mutate_if(is.numeric, round, 4) |>
    dplyr::select(dplyr::any_of(variables)) |>
    tidyr::pivot_longer(-c("datetime"), names_to = "variable", values_to = "observation") |>
    tibble::add_column(depth = NA) |>
    dplyr::select(datetime, depth, variable, observation) 
  
  inf2 <- inf |>
    dplyr::filter(variable %in% c("FLOW","NIT_amm", "NIT_nit", "PHS_frp")) |>
    tidyr::pivot_wider(names_from = variable, values_from = observation) |>
    dplyr::mutate(n_load = FLOW * (NIT_amm + NIT_nit),
                  p_load = FLOW * (PHS_frp)) |>
    dplyr::rename(inflow_rate = FLOW) |>
    dplyr::select(datetime, inflow_rate, n_load, p_load) |>
    tidyr::pivot_longer(-datetime, names_to = "variable", values_to = "observation") |>
    dplyr::mutate(depth = NA)  |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::select(datetime, depth, variable, observation)
  #################################################################################
  
  ##MET----
  #message
  message("interpolating light")
  #read in data
  par_vars <- c("PAR_umolm2s_mean")
  #initial data wrangling
  par <- read_csv(met_url) %>%
    filter(site_id == "fcre" & variable %in% par_vars & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, variable, observation) %>%
    filter(datetime >= start_date) %>%
    dplyr::mutate(variable = "par",
                  depth = NA)  |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::select(datetime, depth, variable, observation)
  
  swr_vars <- c("ShortwaveRadiationUp_Wm2_mean")
  #initial data wrangling
  swr <- read_csv(met_url) %>%
    filter(site_id == "fcre" & variable %in% swr_vars & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, variable, observation) %>%
    filter(datetime >= start_date) %>%
    dplyr::mutate(variable = "swr",
                  depth = NA)  |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::select(datetime, depth, variable, observation)
  
  daily_light_data <- dplyr::bind_rows(par, swr)
  
  regression_data <- daily_light_data |>
    dplyr::select(-depth) |>
    tidyr::pivot_wider(names_from = variable, values_from = observation) |>
    na.omit()
  
  fit <- lm(par ~ swr, regression_data)
  
  light_data <- daily_light_data |>
    dplyr::mutate(observation = ifelse(variable == "swr", fit$coefficients[1] + observation * fit$coefficients[2], observation)) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE), .by = c(datetime)) |>
    dplyr::mutate(variable = "par") |>
    tibble::add_column(depth = NA) |>
    dplyr::select(datetime, depth, variable, observation)
  
  ##in situ data----
  
  #message
  message("interpolating temperature data")
  #read in data
  temp_vars <- c("Temp_C_mean")
  #initial data wrangling
  temp <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable %in% temp_vars & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, depth_m, variable, observation) %>%
    mutate(datetime = date(datetime)) %>%
    group_by(datetime, depth_m, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(datetime >= start_date) %>%
    rename(depth = depth_m) %>%
    mutate(variable = "temperature")
  
  #message
  message("interpolating Secchi and chl-a data")
  #read in data
  res_vars <- c("Chla_ugL_mean","Secchi_m_sample")
  #initial data wrangling
  res <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable %in% res_vars & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, depth_m, variable, observation) %>%
    mutate(datetime = date(datetime)) %>%
    group_by(datetime, depth_m, variable) %>%
    summarise(observation = mean(observation, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(datetime >= start_date) %>%
    rename(depth = depth_m) %>%
    mutate(variable = ifelse(variable == "Chla_ugL_mean","chla_exo","secchi"))
  
  secchi <- res %>% filter(variable == "secchi")
  
  exo_chla_data <- res %>% filter(variable == "chla_exo")
  
  ctd <- readr::read_csv(filepath_ctd)
  
  ctd1 <- ctd |> 
    dplyr::rename(depth = Depth_m) |>
    dplyr::select(DateTime, depth, Chla_ugL) |>
    dplyr::mutate(datetime = lubridate::as_date(DateTime)) |>
    dplyr::summarise(observation = mean(Chla_ugL, na.rm = TRUE), .by = c(datetime, depth)) |>
    dplyr::mutate(variable = "chla_ctd") |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::select(datetime, depth, variable, observation)
  
  modeled_depths <- seq(0,20, 0.25)
  
  cuts <- tibble::tibble(cuts = as.integer(factor(seq(0,20, 0.25))),
                         depth = seq(0,20, 0.25))
  
  daily_chla_data <- dplyr::bind_rows(exo_chla_data, ctd1)
  
  binned_chla <- daily_chla_data |>
    dplyr::mutate(cuts = cut(depth, breaks = modeled_depths, include.lowest = TRUE, right = FALSE, labels = FALSE)) |>
    dplyr::group_by(cuts, variable, datetime) |>
    dplyr::summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(cuts, by = "cuts") |>
    dplyr::select(datetime, variable, depth, observation)
  
  regression_data <- binned_chla |>
    dplyr::filter(depth == 1.5) |>
    tidyr::pivot_wider(names_from = variable, values_from = observation) |>
    na.omit()
  
  fit <- lm(chla_exo ~ chla_ctd, regression_data)
  
  daily_chla_data <- binned_chla |>
    dplyr::mutate(observation = ifelse(variable == "chla_ctd", fit$coefficients[1] + observation * fit$coefficients[2], observation)) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE), .by = c(datetime, depth)) |>
    dplyr::mutate(variable = "chla") |>
    dplyr::select(datetime, depth, variable, observation)
  
  
  
  ##CHEM----
  #not removing flags here b/c I don't think any of these will outweigh
  #the uncertainty introduced via interpolation
  
  #message
  message("interpolating chem")
  
  nutrients <- read_csv(filepath_chemistry,
                   col_types = cols(
                     .default = col_double(),
                     Reservoir = col_character(),
                     DateTime = col_datetime(format = ""),
                     DIC_mgL = col_double(),
                     DC_mgL = col_double(),
                     DN_mgL = col_double()
                   )) 
  
  nutrients2 <- nutrients |>
    dplyr::filter(Reservoir == "FCR" & Site == 50) |>
    dplyr::rename(depth = Depth_m) |>
    dplyr::select(DateTime, depth, NO3NO2_ugL, NH4_ugL, SRP_ugL) |>
    dplyr::mutate(datetime = lubridate::as_date(DateTime),
                  NH4 = NH4_ugL * 1000 * 0.001 * (1 / 18.04),
                  NO3NO2 = NO3NO2_ugL * 1000 * 0.001 * (1/62.00),
                  frp = SRP_ugL * 1000 * 0.001 * (1/95),
                  din = NH4 + NO3NO2) |>
    dplyr::select(datetime, depth, frp, din) |>
    tidyr::pivot_longer(-c(datetime, depth), names_to = "variable", values_to = "observation") |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::select(datetime, depth, variable, observation) |>
    dplyr::mutate(cuts = cut(depth, breaks = modeled_depths, include.lowest = TRUE, right = FALSE, labels = FALSE)) |>
    dplyr::group_by(cuts, variable, datetime) |>
    dplyr::summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(cuts, by = "cuts") |>
    dplyr::select(datetime, depth, variable, observation) |>
    dplyr::filter(datetime >= start_date)
  
  #PREPARE FINAL DF
  message("preparing final df")
  
  df.out <- dplyr::bind_rows(inf2, light_data, temp, secchi, daily_chla_data, nutrients2)
  
  return(df.out)
}
