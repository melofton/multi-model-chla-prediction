# Calibrate GLM-AED to EXO
# Author: Mary Lofton
# Date last updated: 19APR24

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
# install.packages("cowplot")
# install.packages("akima")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("colorRamps")
# install.packages("RColorBrewer")

library(tidyverse)
library(lubridate)
library(glmtools)
library(GLM3r)
library(cowplot)
library(akima)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)

# source custom functions
source("./code/function_library/visualization/FluoroProbeHeatmap.R")

calibrate_GLMAED <- function(sim_folder, save_plot = TRUE){

# run GLM-AED using GLM3r
message("running model")
GLM3r::run_glm(sim_folder = sim_folder,
               nml_file = "glm3.nml",
               verbose = TRUE)
message("end model run")

# set file location of output
nc_file <- file.path(paste0(sim_folder,'/output/output.nc')) 

# pull variables of interest from model output
# out_vars <- sim_vars(file = nc_file)

message("extracting and formatting model output")

chl <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

phy_hot <- glmtools::get_var(nc_file, var_name = "PHY_hot", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

phy_cold <- glmtools::get_var(nc_file, var_name = "PHY_cold", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

phy_Nfixer <- glmtools::get_var(nc_file, var_name = "PHY_Nfixer", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

phy <- left_join(phy_hot, phy_cold, by = "DateTime") %>%
  left_join(.,phy_Nfixer, by = "DateTime") %>%
  pivot_longer(PHY_hot_1.6:PHY_Nfixer_1.6, names_to = "var_name", values_to = "value") %>%
  separate(var_name, c("PHY","group","depth1","depth2")) %>%
  select(-c("PHY","depth1","depth2")) 

f_factor_names_hot <- c("PHY_hot_fI","PHY_hot_fNit","PHY_hot_fPho","PHY_hot_fT")

for(i in 1:length(f_factor_names_hot)){
  var <- glmtools::get_var(nc_file, var_name = f_factor_names_hot[i], reference="surface", z_out=1.6) %>%
    filter(hour(DateTime) == 12) %>%
    mutate(DateTime = date(DateTime))
  
  if(i == 1){
    factors_hot <- left_join(phy_hot, var, by = "DateTime")
  } else {
    factors_hot <- left_join(factors_hot, var, by = "DateTime")
    
  }
  
}

plot_factors_hot <- factors_hot %>%
  mutate(overall = min(PHY_hot_fI_1.6, PHY_hot_fPho_1.6, PHY_hot_fNit_1.6)*PHY_hot_fT_1.6) %>%
  pivot_longer(PHY_hot_fI_1.6:overall, names_to = "var_name", values_to = "value") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  mutate(factor_name = ifelse(PHY == "overall","overall",factor_name)) %>%
  select(-c("PHY","group","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06")

f_factor_names_cold <- c("PHY_cold_fI","PHY_cold_fNit","PHY_cold_fPho","PHY_cold_fT")

for(i in 1:length(f_factor_names_cold)){
  var <- glmtools::get_var(nc_file, var_name = f_factor_names_cold[i], reference="surface", z_out=1.6) %>%
    filter(hour(DateTime) == 12) %>%
    mutate(DateTime = date(DateTime))
  
  if(i == 1){
    factors_cold <- left_join(phy_cold, var, by = "DateTime")
  } else {
    factors_cold <- left_join(factors_cold, var, by = "DateTime")
    
  }
  
}

plot_factors_cold <- factors_cold %>%
  mutate(overall = min(PHY_cold_fI_1.6, PHY_cold_fPho_1.6, PHY_cold_fNit_1.6)*PHY_cold_fT_1.6) %>%
  pivot_longer(PHY_cold_fI_1.6:overall, names_to = "var_name", values_to = "value") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  mutate(factor_name = ifelse(PHY == "overall","overall",factor_name)) %>%
  select(-c("PHY","group","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06")

f_factor_names_Nfixer <- c("PHY_Nfixer_fI","PHY_Nfixer_fNit","PHY_Nfixer_fPho","PHY_Nfixer_fT")

for(i in 1:length(f_factor_names_Nfixer)){
  var <- glmtools::get_var(nc_file, var_name = f_factor_names_Nfixer[i], reference="surface", z_out=1.6) %>%
    filter(hour(DateTime) == 12) %>%
    mutate(DateTime = date(DateTime))
  
  if(i == 1){
    factors_Nfixer <- left_join(phy_Nfixer, var, by = "DateTime")
  } else {
    factors_Nfixer <- left_join(factors_Nfixer, var, by = "DateTime")
    
  }
  
}

plot_factors_Nfixer <- factors_Nfixer %>%
  mutate(overall = min(PHY_Nfixer_fI_1.6, PHY_Nfixer_fPho_1.6, PHY_Nfixer_fNit_1.6)*PHY_Nfixer_fT_1.6) %>%
  pivot_longer(PHY_Nfixer_fI_1.6:overall, names_to = "var_name", values_to = "value") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  mutate(factor_name = ifelse(PHY == "overall","overall",factor_name)) %>%
  select(-c("PHY","group","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06")

# read in other key vars that drive phytos
wt <- glmtools::get_var(nc_file, var_name = "temp", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))
par <- glmtools::get_var(nc_file, var_name = "PHY_par", reference = "surface", z_out = 1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))
nh4 <- glmtools::get_var(nc_file, var_name = "NIT_amm", reference="surface", z_out=1.6)
no3 <- glmtools::get_var(nc_file, var_name = "NIT_nit", reference="surface", z_out=1.6)
din <- left_join(nh4, no3, by = "DateTime") %>%
  mutate(DIN = NIT_amm_1.6 + NIT_nit_1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))
frp <- glmtools::get_var(nc_file, var_name = "PHS_frp", reference = "surface", z_out = 1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

# read in observations

message("reading in observations")

chla <- read_csv("./data/data_processed/chla_obs.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2021)) %>%
  left_join(., chl, by = "DateTime") %>%
  mutate(bias = PHY_tchla_1.6 - Chla_ugL_mean,
         DateTime = date(DateTime))

# fp heatmap
fp_profiles <- read_csv("./data/data_raw/FP_2018_2023_profiles_FCR50.csv") %>%
  filter(date(DateTime) >= "2018-08-06" & date(DateTime) <= "2021-12-31")

# mega-plot for model runs

message("plotting model output")

plot_wt <- ggplot(data = wt, aes(x = DateTime, y = temp_1.6))+
  geom_line(color = "cornflowerblue")+
  xlab("")+
  ylab("Water temperature (ºC)")+
  theme_bw()

plot_par <- ggplot(data = par, aes(x = DateTime, y = PHY_par_1.6))+
  geom_line(color = "goldenrod1")+
  xlab("")+
  ylab("PAR (W/m2)")+
  theme_bw()

plot_din <- ggplot(data = din, aes(x = DateTime, y = DIN))+
  geom_line(color = "maroon")+
  xlab("")+
  ylab("DIN (mmol N m3)")+
  theme_bw()

plot_frp <- ggplot(data = frp, aes(x = DateTime, y = PHS_frp_1.6))+
  geom_line(color = "orange")+
  xlab("")+
  ylab("FRP (mmol P m3)")+
  theme_bw()

# look at f factors
plot_f_factors_hot <- ggplot(data = plot_factors_hot)+
  geom_line(aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  theme_bw()+
  xlab("")+
  ggtitle("Warm group")+
  theme(legend.position = "bottom")+
  labs(color = "Limiting factor")+
  guides(color = guide_legend(nrow = 2))


plot_f_factors_cold <- ggplot(data = plot_factors_cold, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()+
  xlab("")+
  ggtitle("Cold group")+
  theme(legend.position = "bottom")+
  labs(color = "Limiting factor")+
  guides(color = guide_legend(nrow = 2))

plot_f_factors_Nfixer <- ggplot(data = plot_factors_Nfixer, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()+
  xlab("")+
  ggtitle("N-fixing group")+
  theme(legend.position = "bottom")+
  labs(color = "Limiting factor")+
  guides(color = guide_legend(nrow = 2))

# look at timeseries 
plot_ts <- ggplot(data = chla)+
  geom_point(aes(x = DateTime, y = Chla_ugL_mean, fill = "observed"))+
  geom_line(aes(x = DateTime, y = PHY_tchla_1.6,color = "modeled"))+
  labs(color = "", fill = "", y = "Chlorophyll-a (ug/L)")+
  scale_color_manual(values = c("modeled" = "darkolivegreen3"))+
  scale_fill_manual(values = c("observed" = "black"))+
  theme_bw()+
  theme(legend.position = "bottom")


# plot AED phyto groups
plot_AED_groups <- ggplot(data = phy, aes(x = DateTime, y = value, group = group, color = group))+
  geom_line()+
  xlab("")+
  ylab("Phytoplankton biomass (mmol C m3)")+
  theme_bw()+
  labs(color = NULL)+
  theme(legend.position = "bottom")

chl_heatmap <- plot_var_nc(nc_file, var_name = "PHY_tchla", reference = "surface", interval = 0.1, show.legend = TRUE)

fp_heatmap <- flora_heatmap(fp_data = fp_profiles, reservoir = "FCR", years = c(2019:2021), z = "non_cyano")

# plot scatterplot of pred vs obs
predVsObs <- ggplot(data = chla, aes(x = Chla_ugL_mean, y = PHY_tchla_1.6))+
  geom_point(alpha = 0.5)+
  labs(x = "observed", y = "predicted")+
  lims(x = c(0,60), y = c(0,60))+
  geom_abline(slope = 1, linetype = 2)+
  ggtitle("Chlorophyll-a")+
  theme_bw()

first <- plot_grid(plot_wt, plot_par, plot_din, plot_frp,
                              plot_f_factors_hot, plot_f_factors_cold, 
                              plot_f_factors_Nfixer, plot_AED_groups,
                              plot_ts, predVsObs, nrow = 5)
second <- plot_grid(chl_heatmap, fp_heatmap, 
                               nrow = 2, align = "v", axis = "l")
assess_model_run <- plot_grid(first, second,ncol = 1, rel_heights = c(5,2))


if(save_plot == TRUE){
ggsave(assess_model_run, filename = "./figures/assess_model_run.png",
       height = 18, width = 8, units = "in")
}

# assessment metrics
message("calculating assessment metrics")
# RMSE
assess_data <- chla %>%
  filter(year(DateTime) %in% c(2020:2021))

rmse_all = sqrt(mean((chla$Chla_ugL_mean - chla$PHY_tchla_1.6)^2, na.rm = TRUE))
rmse_2020_2021 = sqrt(mean((assess_data$Chla_ugL_mean - assess_data$PHY_tchla_1.6)^2, na.rm = TRUE))

cor_all = cor(chla$Chla_ugL_mean, chla$PHY_tchla_1.6, 
              use = "pairwise.complete.obs", method = "pearson")
cor_2020_2021 = cor(assess_data$Chla_ugL_mean, assess_data$PHY_tchla_1.6, 
                    use = "pairwise.complete.obs", method = "pearson")

chl_out <- chl %>%
  add_column(model_id = "GLM-AED",
             variable = "chlorophyll-a") %>%
  rename(datetime = DateTime, 
         prediction = PHY_tchla_1.6) 

message("all done!")

return(list(chl_out = chl_out, calibration_plot = assess_model_run, 
            rmse_all = rmse_all, rmse_2020_2021 = rmse_2020_2021, 
            cor_all = cor_all, cor_2020_2021 = cor_2020_2021))

}

