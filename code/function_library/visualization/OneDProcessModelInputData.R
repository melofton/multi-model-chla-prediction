# OneDProcess Model Driver Data
# Author: Mary Lofton
# Date: 11DEC24

# Purpose: plot all the driver data for the OneDProcessModel

#load packages
library(tidyverse)
library(lubridate)

OneDProcessModelInputData <- function(input_data){
  
  #data wrangling
  plot_data <- input_data %>%
    filter(variable %in% c("inflow_rate","n_load","p_load","temperature","par")) 
  
  # build facet labels
  fac_labs <- c("(a) Inflow rate (m3/day)","(b) DIN load (mmol N/day)","(c) SRP load (mmol P/day)",
                "(d) Incident PAR (daily mean umol/m2/s)","(e) Water temperature (°C)")
  names(fac_labs) <- unique(plot_data$variable)
  
  #plot
  p <- ggplot(data = plot_data)+
    facet_wrap(vars(variable), scales = "free_y", ncol = 2, labeller = labeller(variable = fac_labs))+
    geom_point(aes(x = datetime, y = observation, group = as.factor(depth), color = as.factor(depth)), size = 0.5)+
    xlab("")+
    ylab("")+
    labs(color = "Depth (m)")+
    theme_bw()
  
  return(p)
}
