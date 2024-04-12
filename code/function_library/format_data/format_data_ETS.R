#Format data for DOY model for chl-a
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: format data for DOY model for chla from 2018-2021

#'Function to fit day of year model for chla
#'@param res_url url to in situ reservoir targets data for VERA

#load packages
library(tidyverse)
library(lubridate)

format_data_ETS <- function(res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                            start_date = "2018-08-06",
                            end_date = "2023-12-31"){

  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "datetime"
  
  ##EXO----
  
  #message
  message("interpolating exo")
  chla <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable == "Chla_ugL_mean" & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, observation) %>%
    rename(Chla_ugL_mean = observation) %>%
    filter(datetime >= start_date)
  
  #interpolation
  chla2 <- interpolate(daily_dates = daily_dates,
                      data = chla,
                      variables = c("Chla_ugL_mean"),
                      method = "linear")
  
  #format final data frame
  df.out <- chla2 %>%
    select(datetime, Chla_ugL_mean, Flag_Chla_ugL_mean)
    
return(df.out)
}
