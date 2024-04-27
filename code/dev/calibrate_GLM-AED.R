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
library(tidyverse)
library(lubridate)
library(glmtools)
library(GLM3r)
library(lhs)
library(cowplot)
library(plotly)
library(akima)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)
install.packages("lhs")
install.packages("cowplot")
install.packages("plotly")
install.packages("akima")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("grid")
install.packages("colorRamps")
install.packages("RColorBrewer")

# create matrix of parameter values using maximin space-filling design ----

# Notes on choices re: which parameters to include
#'  - not including parameters that specify a choice of method; these are fixed
#'  - temperature method is 1 (so phytos are temperature-limited)
#'  - light method is 1 (Monod; no photoinhibition) and we are only including
#'    light parameters relevant to that method (I_K)
#'  - simDINuptake is 1, but internal N dynamics are not simulated so parameters
#'    related to that are not included here (X_nmin, X_nmax)
#'  - same for DIP
#'  - simNFixation is 0, so this is a group that cannot fix N, so parameters related
#'    to this process are not included (k_nfix, r_nfix); however, I should probably 
#'    do a separate set of runs with a group that does fix N
#'  - parameters related to salinity and silica uptake are not included because these
#'    processes are not relevant for our simulation
#'  - N_o and P_0 are not included as we always want them to be able to take up 
#'    nutrients
#'  - another complication is that T_std, T_opt, and T_max have value dependencies
#'  - eliminating k_fres and k_fdom because this is more about feedbacks between
#'    phytos and OM and we are more interested in focal "knobs" of temperature,
#'    light, and nutrient sensitivity, respiration rate, and sinking/floating
#'  - not tuning Xcc as that is more about the sensor range and less about calibration
#'    

# run GLM-AED using GLM3r
GLM3r::run_glm(sim_folder = "./model_output/GLM-AED",
               nml_file = "glm3.nml",
               verbose = TRUE)

# set file location of output
nc_file <- file.path('./model_output/GLM-AED/output/output.nc') 

# pull variables of interest from model output
out_vars <- sim_vars(file = nc_file)

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

# read in observations
chla <- read_csv("./data/data_processed/chla_obs.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2021)) %>%
  left_join(., chl, by = "DateTime") %>%
  mutate(bias = PHY_tchla_1.6 - Chla_ugL_mean,
         DateTime = date(DateTime))

chla_obs <- read_csv("./data/data_processed/chla_obs.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2021)) %>%
  mutate(DateTime = date(DateTime))

fp <- read_csv("./data/data_raw/FP_2018_2023_FCR50.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2021),
         DateTime >= "2018-08-06") %>%
  mutate(DateTime = date(DateTime))

fp_sum <- fp %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  rowwise() %>%
  mutate(non_cyano = sum(GreenAlgae_ugL_sample, BrownAlgae_ugL_sample, MixedAlgae_ugL_sample)) %>%
  left_join(., chla_obs, by = "DateTime")

fp_groups <- fp %>%
  filter(!variable == "Bluegreens_ugL_sample") %>%
  separate(variable, into = c("group","unit","timing"))

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

# mega-plot for model runs
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
  labs(color = "Limiting factor")
plot_f_factors_hot

plot_f_factors_cold <- ggplot(data = plot_factors_cold, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()+
  xlab("")+
  ggtitle("Cold group")+
  theme(legend.position = "bottom")+
  labs(color = "Limiting factor")
ggplotly(plot_f_factors_cold)

plot_f_factors_Nfixer <- ggplot(data = plot_factors_Nfixer, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()+
  xlab("")+
  ggtitle("N-fixing group")+
  theme(legend.position = "bottom")+
  labs(color = "Limiting factor")

# look at timeseries 
plot_ts <- ggplot(data = chla)+
  geom_point(aes(x = DateTime, y = Chla_ugL_mean, fill = "observed"))+
  geom_line(aes(x = DateTime, y = PHY_tchla_1.6,color = "modeled"))+
  labs(color = "", fill = "", y = "Chlorophyll-a (ug/L)")+
  scale_color_manual(values = c("modeled" = "darkolivegreen3"))+
  scale_fill_manual(values = c("observed" = "black"))+
  theme_bw()+
  theme(legend.position = "bottom")
plot_ts
ggplotly(plot_ts)


# plot AED phyto groups
plot_AED_groups <- ggplot(data = phy, aes(x = DateTime, y = value, group = group, color = group))+
  geom_line()+
  xlab("")+
  ylab("Phytoplankton biomass (mmol C m3)")+
  theme_bw()+
  labs(color = NULL)+
  theme(legend.position = "bottom")
plot_AED_groups

assess_model_run <- plot_grid(plot_wt, plot_par, plot_din, plot_frp,
                              plot_f_factors_hot, plot_f_factors_cold, plot_f_factors_Nfixer, plot_ts, plot_AED_groups,
                              nrow = 3)
ggsave(assess_model_run, filename = "./figures/assess_model_run.png",
       height = 9, width = 12, units = "in")

plot_var_nc(nc_file, var_name = "PHY_tchla", reference = "surface", interval = 0.1, show.legend = TRUE)

# assessment metrics

# RMSE
assess_data <- chla %>%
  filter(year(DateTime) %in% c(2020:2021))

rmse1 = sqrt(mean((chla$Chla_ugL_mean - chla$PHY_tchla_1.6)^2, na.rm = TRUE))
rmse2 = sqrt(mean((assess_data$Chla_ugL_mean - assess_data$PHY_tchla_1.6)^2, na.rm = TRUE))


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

predvobs <- plot_grid(p1, p2)
predvobs
ggsave(predvobs, filename = "./figures/predVersusObsGLM-AED.png",
       device = "png", height = 4, width = 10, units = "in")

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

# plot timeseries of AED groups
p5 <- ggplot(data = phy, aes(x = DateTime, y = PHY_phyto_1.6))+
  geom_line()+
  theme_bw()
p5

# plot timeseries of FP groups
p6 <- ggplot()+
  geom_point(data = fp_sum, aes(x = DateTime, y = Bluegreens_ugL_sample, color = "FP cyanobacteria"))+
  geom_line(data = chla_obs, aes(x = DateTime, y = Chla_ugL_mean, color = "EXO chl-a"))+
  geom_point(data = fp_sum, aes(x = DateTime, y = non_cyano, color = "FP biomass w/o cyanos"))+
  scale_color_manual(values = c("EXO chl-a" = "black","FP biomass w/o cyanos" = "gray","FP cyanobacteria" = "cadetblue"))+
  labs(color = NULL, y = "EXO chl-a (ug/L)")+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "FP w/o cyanos (ug/L)"))+
  theme_bw()+
  theme(legend.position = "bottom")
p6
#ggplotly(p6)

p7 <- ggplot(data = fp_sum)+
  xlab("EXO chl-a (ug/L)")+
  ylab("FP w/o cyanos (ug/L)")+
  geom_point(aes(x = Chla_ugL_mean, y = non_cyano))+
  ylim(c(0,50))+
  xlim(c(0,50))+
  theme_bw()
p7

sensors <- plot_grid(p6, p7, rel_widths = c(3,1))
ggsave(sensors, filename = "./figures/FPNoCyanosVersusEXOChla.png",
       device = "png", height = 2.5, width = 10, units = "in")

# look at f factors
p8 <- ggplot(data = plot_factors, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()
p8

# look at bias + limiting factor
p8 <- ggplot(data = bias_and_factors)+
  geom_line(aes(x = DateTime, y = PHY_tchla_1.6), color = "green")+
  geom_point(aes(x = DateTime, y = Chla_ugL_mean))+
  geom_point(aes(x = DateTime, y = line, group = factor_name, color = factor_name))+
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")+
  ylab("bias (predicted - observed)")+
  labs(color = "Limiting factor")+
  theme_bw()
p8

biasfactor <- plot_grid(p7, p6, ncol = 1)
ggsave(biasfactor, filename = "./figures/biasAndLimitingFactorsGLM-AED.png",
       device = "png", height = 7, width = 7, units = "in")

# temp plot
ggplot(data = wt, aes(x = DateTime, y = temp_1.6))+
  geom_line()+
  theme_bw()

# data wrangling to get factors and bias in same dataset
min_factor <- plot_factors %>%
  group_by(DateTime) %>%
  filter(value == min(value, na.rm = TRUE)) %>%
  select(-value) %>%
  mutate(line = max(chla$Chla_ugL_mean, na.rm = TRUE)+5)
bias_and_factors <- left_join(chla, min_factor, by = "DateTime") %>%
  filter(!is.na(Chla_ugL_mean))

plot_ts_and_factors <- ggplot(data = bias_and_factors)+
  geom_point(aes(x = DateTime, y = Chla_ugL_mean))+
  geom_line(aes(x = DateTime, y = PHY_tchla_1.6), color = "darkolivegreen3")+
  geom_point(aes(x = DateTime, y = line, group = factor_name, color = factor_name))+
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")+
  ylab("Chl-a (ug/L)")+
  xlab("")+
  labs(color = "Limiting factor")+
  theme_bw()+
  theme(legend.position = "bottom")

# plot FP phyto groups
plot_FP_groups <- ggplot(data = fp_groups, aes(x = DateTime, y = observation, group = group, color = group))+
  geom_line()+
  theme_bw()+
  xlab("")+
  theme(legend.position = "bottom")+
  labs(color = NULL)

check <- chla %>%
  filter(year(DateTime) %in% c(2020,2021))

# fp heatmap
fp_profiles <- read_csv("./data/data_raw/FP_2018_2023_profiles_FCR50.csv") %>%
  filter(date(DateTime) >= "2018-08-06" & date(DateTime) <= "2021-12-31")

.interpolate2grid <- function(xyzData, xcol = 1, ycol = 2, zcol = 3) {
  # Interpolate field or modeled data to grid 
  # xcol, ycol, and zcol and column numbers from data.frame
  # The spreads of x and y must be within four orders of magnitude of each other for interp to work
  # Therefore must scale data to be within similar magnitude to numeric dates (1e6)
  gridData <-interp2xyz(interp(
    x = as.numeric(xyzData[,xcol]), y=xyzData[,ycol]*1e6, z=xyzData[,zcol], 
    duplicate="mean", linear = T,
    xo = as.numeric(seq(min(xyzData[,xcol]), max(xyzData[,xcol]), by = 'day')),
    yo = 1e6*seq(min(xyzData[,ycol]), max(xyzData[,ycol]), by = 0.1)), 
    data.frame=TRUE) %>%
    dplyr::mutate(x = as.POSIXct(.data$x, origin = '1970-01-01', 
                                 tz = Sys.timezone())) %>%
    dplyr::mutate(y = .data$y/1e6) %>%
    dplyr::arrange(.data$x, .data$y)
  
  return(gridData)
}

flora_heatmap <- function(fp_data, reservoir, years, z){
  
  #subset to relevant data
  fp <- fp_data %>%
    select(DateTime, Depth_m, {{z}}) 
  fp <- data.frame(fp)


  observed_df <- .interpolate2grid(fp, xcol = 1, ycol = 2, zcol = 3) %>% 
    rename(DateTime = .data$x, Depth = .data$y, var = .data$z)
  
  legend.title = "FP biomass w/o cyanos (ug/L)" 
  text.size = 12
  color.palette = 'RdYlBu' 
  color.direction = -1 
  obs.color = 'white' 
  obs.alpha = 0.6 
  obs.shape = 16 
  obs.size = 1
  shiftPalette = NULL 
  zlim = c(0,45)
  
  h1 = ggplot(data = observed_df, aes(x = .data$DateTime, y = .data$Depth)) +
    geom_raster(aes(fill = .data$var), interpolate = F) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01), limits = c(min(observed_df$DateTime), max(observed_df$DateTime))) +
    scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90", limits = zlim) +
    ylab('Depth (m)') + xlab('Date') +
    labs(fill = legend.title) +
    theme_bw(base_size = text.size)
  
  print(h1)
  
}

flora_heatmap(fp_data = fp_profiles, reservoir = "FCR", years = c(2019:2021), z = "non_cyano")

