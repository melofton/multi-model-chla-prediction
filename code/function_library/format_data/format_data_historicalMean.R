#Format data for DOY model for chl-a
#Author: Mary Lofton
#Date: 12APR24

#Purpose: format data for historical mean model for chla from 2018-2021

#'Function to format data for historical mean model
#'@param res_url url to targets data for daily in situ reservoir variables from VERA

#load packages
library(tidyverse)
library(lubridate)

format_data_historicalMean <- function(res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"){
  
  chla <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable == "Chla_ugL_mean" & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, observation) %>%
    rename(Chla_ugL_mean = observation)
  
  return(chla)
}