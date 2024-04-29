# EXOChlaVsFluoroProbe plot
# Author: Mary Lofton
# Date: 29APR24

# Purpose: compare EXO chl-a data to fluoroprobe data to inform GLM-AED calibraiton

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot)

EXOChlaVsFluoroProbe <- function(fp_data = "./data/data_raw/FP_2018_2023_FCR50.csv", chla_data = "./data/data_processed/chla_obs.csv", save_plot = TRUE){
  
  chla_obs <- read_csv(chla_data) %>%
    rename(DateTime = datetime) %>%
    filter(year(DateTime) %in% c(2018:2021)) %>%
    mutate(DateTime = date(DateTime))
  
  fp <- read_csv(fp_data) %>%
    rename(DateTime = datetime) %>%
    filter(year(DateTime) %in% c(2018:2021),
           DateTime >= "2018-08-06") %>%
    mutate(DateTime = date(DateTime)) %>%
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    rowwise() %>%
    mutate(non_cyano = sum(GreenAlgae_ugL_sample, BrownAlgae_ugL_sample, MixedAlgae_ugL_sample)) %>%
    left_join(., chla_obs, by = "DateTime")
  
  # plot timeseries of FP groups
  p6 <- ggplot()+
    geom_point(data = fp, aes(x = DateTime, y = Bluegreens_ugL_sample, color = "FP cyanobacteria"))+
    geom_line(data = chla_obs, aes(x = DateTime, y = Chla_ugL_mean, color = "EXO chl-a"))+
    geom_point(data = fp, aes(x = DateTime, y = non_cyano, color = "FP biomass w/o cyanos"))+
    scale_color_manual(values = c("EXO chl-a" = "black","FP biomass w/o cyanos" = "gray","FP cyanobacteria" = "cadetblue"))+
    labs(color = NULL, y = "EXO chl-a (ug/L)")+
    scale_y_continuous(sec.axis = sec_axis(~.,name = "FP w/o cyanos (ug/L)"))+
    theme_bw()+
    theme(legend.position = "bottom")
  p6
  #ggplotly(p6)
  
  p7 <- ggplot(data = fp)+
    xlab("EXO chl-a (ug/L)")+
    ylab("FP w/o cyanos (ug/L)")+
    geom_point(aes(x = Chla_ugL_mean, y = non_cyano))+
    ylim(c(0,50))+
    xlim(c(0,50))+
    theme_bw()
  p7
  
  sensors <- plot_grid(p6, p7, rel_widths = c(3,1))
  print(sensors)
  if(save_plot == TRUE){
  ggsave(sensors, filename = "./figures/FPNoCyanosVersusEXOChla.png",
         device = "png", height = 2.5, width = 10, units = "in")
  }
}
