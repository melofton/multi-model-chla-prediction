# Calibrate GLM-AED to EXO
# Author: Mary Lofton
# Date last updated: 16APR24

# Purpose: Calibrate GLM-AED to EXO chl-a data at 1.6 m in FCR from Aug. 2018-2021

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

# install and load packages ----
library(tidyverse)
library(lubridate)
library(glmtools)
library(GLM3r)

# run GLM-AED using GLM3r
GLM3r::run_glm(sim_folder = "./model_output/GLM-AED",
               nml_file = "glm3.nml",
               verbose = TRUE)

# set file location of output
nc_file <- file.path('./model_output/GLM-AED/output/output.nc') 

# pull variables of interest from model output
chl <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)

phy <- glmtools::get_var(nc_file, var_name = "PHY_phyto", reference="surface", z_out=1.6)

out_vars <- sim_vars(file = nc_file)

f_factor_names <- c("PHY_phyto_fI","PHY_phyto_fNit","PHY_phyto_fPho","PHY_phyto_fT")

for(i in 1:length(f_factor_names)){
  var <- glmtools::get_var(nc_file, var_name = f_factor_names[i], reference="surface", z_out=1.6)
  
  if(i == 1){
    factors <- left_join(phy, var, by = "DateTime")
  } else {
    factors <- left_join(factors, var, by = "DateTime")
    
  }
  
}

plot_factors <- factors %>%
  select(-PHY_phyto_1.6) %>%
  pivot_longer(PHY_phyto_fI_1.6:PHY_phyto_fT_1.6, names_to = "var_name", values_to = "value") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  select(-c("PHY","group","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06")

# read in observations
chla <- read_csv("./data/data_processed/chla_obs.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2020)) %>%
  left_join(., chl, by = "DateTime") %>%
  mutate(bias = PHY_tchla_1.6 - Chla_ugL_mean)

# read in other key vars that drive phytos
wt <- glmtools::get_var(nc_file, var_name = "temp", reference="surface", z_out=1.6)
par <- glmtools::get_var(nc_file, var_name = "light")
hist(par$TOT_par_1.6)
plot(light$DateTime, light$light)
met <- read_csv("./model_output/GLM-AED/inputs/met_avg_filtered.csv")
hist(met$ShortWave)

# plot timeseries of pred vs obs
p1 <- ggplot(data = chla)+
  geom_point(aes(x = DateTime, y = Chla_ugL_mean, fill = "observed"))+
  geom_line(aes(x = DateTime, y = PHY_tchla_1.6, color = "GLM-AED"))+
  labs(color = "", fill = "", y = "Chlorophyll-a (ug/L)")+
  theme_bw()
p1

# plot scatterplot of pred vs obs
p2 <- ggplot(data = chla, aes(x = Chla_ugL_mean, y = PHY_tchla_1.6))+
  geom_point(alpha = 0.5)+
  labs(x = "observed", y = "predicted")+
  lims(x = c(0,60), y = c(0,60))+
  theme_bw()
p2

# plot bias over time
p3 <- ggplot(data = chla, aes(x = DateTime, y = bias))+
  geom_line(aes(color = "bias"))+
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")+
  labs(color = "", y = "Chlorophyll-a (ug/L)")+
  ggtitle("Predicted - observed chlorophyll-a")+
  theme_bw()
p3

# plot scatterplot of bias vs obs
p4 <- ggplot(data = chla, aes(x = Chla_ugL_mean, y = bias))+
  geom_point(alpha = 0.5)+
  labs(x = "observed", y = "predicted - observed")+
  ggtitle("Model bias vs. observations")+
  theme_bw()
p4

# plot timeseries of groups
p5 <- ggplot(data = phy, aes(x = DateTime, y = PHY_phyto_1.6))+
  geom_line()+
  theme_bw()
p5

# look at f factors
p6 <- ggplot(data = plot_factors, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()
p6

limiting_factor <- plot_factors %>%
  group_by

p7 <- ggplot()
