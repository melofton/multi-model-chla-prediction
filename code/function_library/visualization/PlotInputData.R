#Plot input data
#Author: Mary Lofton
#Date last updated: 15APR24

#Purpose: plot model input data, showing interpolated and non-interpolated values

#load packages
library(tidyverse)
library(lubridate)

#'Function to fit day of year model for chla
#'@param data data frame with columns:
#'Date: yyyy-mm-dd
#'variables: environmental variables
#'Flag_: flag columns with 1 indicating interpolated value, 0 indicating not


PlotInputData <- function(input_data){
  
  #reformat data for plotting
  plot_vars <- pivot_longer(input_data[,c(1:10)], -c(datetime), values_to = "value", names_to = "variable")
  flags <- input_data[,c(1,11:19)]
  colnames(flags) <- colnames(input_data)[c(1:10)]
  plot_flags <- pivot_longer(flags, -c(datetime), values_to = "flag", names_to = "variable")
  plot_data <- left_join(plot_vars, plot_flags, by = c("datetime","variable")) %>%
    pivot_wider(id_cols = c("datetime", "variable"), names_from = flag) %>%
    rename(Interpolated = `1`,
           Observed = `0`)
  
  p <- ggplot(data = plot_data)+
    facet_wrap(vars(variable), scales = "free_y", ncol = 2)+
    geom_point(aes(x = datetime, y = Interpolated, color = "Interpolated"))+
    geom_point(aes(x = datetime, y = Observed, color = "Observed"))+
    xlab("")+
    ylab("")+
    labs(color = NULL)+
    theme_bw()
  
  return(p)
}
