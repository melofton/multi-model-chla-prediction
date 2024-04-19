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
install.packages("lhs")
library(lhs)
install.packages("cowplot")
library(cowplot)
install.packages("plotly")
library(plotly)

# create matrix of parameter values using maximin space-filling design ----

# Notes on choices re: which parameters to include
#'  - not including parameters that specify a choice of method; these are fixed
#'  - temperature method is 1 (so phytos are temperature-limited)
#'  - light method is 1 (Monod; no photoinhibition) and we are only including
#'    light parameters relevant to that method (I_K)
#'  - simDINuptake is 1, but internal N dynamics are not simulated so parameters
#'    related to that are not included here (X_nmin, X_nmax)
#'  - same for DIP
#'  - simNFixation is 1, so this is a group that can fix N, so parameters related
#'    to this process are included (k_nfix, r_nfix); however, I should probably 
#'    do a separate set of runs with a group that doesn't fix N
#'  - parameters related to salinity and silica uptake are not included because these
#'    processes are not relevant for our simulation
#'  - N_o and P_0 are not included as we always want them to be able to take up 
#'    nutrients
#'  - another complication is that T_std, T_opt, and T_max have value dependencies
#'  - eliminating k_fres and k_fdom because this is more about feedbacks between
#'    phytos and OM and we are more interested in focal "knobs" of temperature,
#'    light, and nutrient sensitivity, respiration rate, and sinking/floating
#'    

phyto_groups <- c("phyto")
param_names <- c("pd%w_p",          
                 "pd%Xcc",
                 "pd%R_growth",
                 "pd%theta_growth",
                 "pd%T_std",
                 "pd%T_opt", 
                 "pd%T_max",  
                 "pd%I_K", 
                 "pd%f_pr",  
                 "pd%R_resp",  
                 "pd%theta_resp", 
                 "pd%X_ncon",
                 "pd%K_N",   
                 "pd%k_nfix",
                 "pd%R_nfix",
                 "pd%K_P",
                 "pd%X_pcon") 

# Latin hypercube function
# n runs for m factors
# so start with 1000 runs for 6 factors (parameters)

mylhs <- function(n, m)
{
  ## generate the Latin hypercube 
  l <- (-(n - 1)/2):((n - 1)/2)
  L <- matrix(NA, nrow=n, ncol=m)
  for(j in 1:m) L[,j] <- sample(l, n)
  
  ## draw the random uniforms and turn the hypercube into a sample
  U <- matrix(runif(n*m), ncol=m)
  X <- (L + (n - 1)/2 + U)/n
  colnames(X) <- paste0("x", 1:m)
  
  ## return the design and the grid it lives on for visualization
  return(list(X=X, g=c((l + (n - 1)/2)/n,1)))
}

# set number of sims you want to run
n_sims = 1000
Dlist <- mylhs(n = n_sims, m = length(param_names))

# functions to get parameter values in correct range

# function to scale parameters (this works for parameters whose ranges are > 0)
scale_parm <- function(x,min,max, na.rm = FALSE) x*(max - min) + min

# w_p (-1,1)
scale_w_p <- function(x, na.rm = FALSE){
  
  for(i in 1:length(x)){
    if(x[i] == 0.5){x[i] <- 0} else if(x[i] < 0.5){x[i] <- x[i]*-2} else {x[i] <- (x[i]-0.5)*2}
  }
  
  return(x)
  
}

# temperature parameters 
# a,b,c are the standardized values for T_std, T_opt, and T_max respectively
# min = 4ºC
# max_a (maximum value for T_std) = 20
# max_b (maximum value for T_opt) = 35
# max_c (maximum value for T_max) = 40
scale_T_parms <- function(a,b,c,min,max_a,max_b,max_c){
  
  T_std = a*(max_a - min) + min
  T_opt = b*(max_b - T_std) + T_std
  T_max = c*(max_c - T_opt) + T_opt
  
  return(c(T_std,T_opt,T_max))
}



param_values <- tibble(data.frame(Dlist$X)) %>%
  mutate(x1 = scale_w_p(x1),
         x2 = scale_parm(x2, min = 10, max = 1000), #Xcc
         x3 = scale_parm(x3, min = 0.5, max = 3.5), #R_growth
         x4 = scale_parm(x4, min = 1, max = 1.2), #theta_growth; 1.06 to 1.11 and keep same as theta_resp
         x5 = scale_T_parms(x5, min = 4, max_a = 20, max_b = 35, max_c = 40), #T_std
         x6 = scale_T_parms(x5, min = 4, max_a = 20, max_b = 35, max_c = 40), #T_opt
         x7 = scale_T_parms(x5, min = 4, max_a = 20, max_b = 35, max_c = 40), #T_max
         x8 = scale_parm(x8, min = 10, max = 500), #I_K
         x9 = scale_parm(x9, min = 0, max = 1), #f_pr; ASK CCC
         x10 = scale_parm(x10, min = 0.01, max = 0.3), #R_resp
         x11 = scale_parm(x11, min = 1, max = 1.2), #theta_resp
         x14 = scale_parm(x14, min = 0.5, max = 3.5), #K_N; ASK CCC
         x15 = scale_parm(x15, min = 0.5, max = 3.5)) #K_P; ASK CCC
colnames(param_values) <- param_names


# run GLM-AED using GLM3r
GLM3r::run_glm(sim_folder = "./model_output/GLM-AED",
               nml_file = "glm3.nml",
               verbose = TRUE)

# set file location of output
nc_file <- file.path('./model_output/GLM-AED/output/output.nc') 

# pull variables of interest from model output
chl <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

phy <- glmtools::get_var(nc_file, var_name = "PHY_phyto", reference="surface", z_out=1.6) %>%
  filter(hour(DateTime) == 12) %>%
  mutate(DateTime = date(DateTime))

out_vars <- sim_vars(file = nc_file)

f_factor_names <- c("PHY_phyto_fI","PHY_phyto_fNit","PHY_phyto_fPho","PHY_phyto_fT")

for(i in 1:length(f_factor_names)){
  var <- glmtools::get_var(nc_file, var_name = f_factor_names[i], reference="surface", z_out=1.6) %>%
    filter(hour(DateTime) == 12) %>%
    mutate(DateTime = date(DateTime))
  
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
  filter(year(DateTime) %in% c(2018:2021)) %>%
  left_join(., chl, by = "DateTime") %>%
  mutate(bias = PHY_tchla_1.6 - Chla_ugL_mean,
         DateTime = date(DateTime))

fp <- read_csv("./data/data_raw/FP_2018_2023_FCR50.csv") %>%
  rename(DateTime = datetime) %>%
  filter(year(DateTime) %in% c(2018:2021),
         DateTime >= "2018-08-06") %>%
  mutate(DateTime = date(DateTime))

fp_sum <- fp %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  rowwise() %>%
  mutate(non_cyano = sum(GreenAlgae_ugL_sample, BrownAlgae_ugL_sample, MixedAlgae_ugL_sample)) %>%
  left_join(., chla %>% select(DateTime, Chla_ugL_mean))


# data wrangling to get factors and bias in same dataset
min_factor <- plot_factors %>%
  group_by(DateTime) %>%
  filter(value == min(value, na.rm = TRUE)) %>%
  select(-value) %>%
  mutate(line = max(chla$bias, na.rm = TRUE)+5)
bias_and_factors <- left_join(chla, min_factor, by = "DateTime") %>%
  filter(!is.na(Chla_ugL_mean))

# read in other key vars that drive phytos
wt <- glmtools::get_var(nc_file, var_name = "temp", reference="surface", z_out=1.6)
par <- glmtools::get_var(nc_file, var_name = "PHY_par")
hist(par$TOT_par_1.6)
plot(light$DateTime, light$light)
met <- read_csv("./model_output/GLM-AED/inputs/met_avg_filtered.csv")
hist(met$ShortWave)
lake <- read_csv("./model_output/GLM-AED/output/lake.csv")
phyto_pars <- read_csv("./model_output/GLM-AED/aed/aed_phyto_pars.csv")

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
  geom_line(data = fp_sum, aes(x = DateTime, y = Bluegreens_ugL_sample, color = "FP cyanobacteria"))+
  geom_line(data = chla, aes(x = DateTime, y = Chla_ugL_mean, color = "EXO chl-a"))+
  geom_line(data = fp_sum, aes(x = DateTime, y = non_cyano, color = "FP biomass w/o cyanos"), linetype = 2)+
  scale_color_manual(values = c("EXO chl-a" = "black","FP biomass w/o cyanos" = "gray","FP cyanobacteria" = "cadetblue"))+
  labs(color = NULL, y = "ugL")+
  theme_bw()+
  theme(legend.position = "bottom")
p6
#ggplotly(p6)

p7 <- ggplot(data = fp_sum)+
  xlab("EXO chl-a")+
  ylab("FP biomass")+
  geom_point(aes(x = Chla_ugL_mean, y = non_cyano, color = "non-cyanos"))+
  geom_point(aes(x = Chla_ugL_mean, y = Bluegreens_ugL_sample, color = "cyanobacteria"))+
  scale_color_manual(values = c("non-cyanos" = "gray","cyanobacteria" = "cadetblue"))+
  geom_abline(linetype = 2)+
  ylim(c(0,50))+
  xlim(c(0,50))+
  theme_bw()
p7

sensors <- plot_grid(p6, p7, rel_widths = c(3,1))
ggsave(sensors, filename = "./figures/FPNoCyanosVersusEXOChla.png",
       device = "png", height = 2.5, width = 10, units = "in")

# look at f factors
p7 <- ggplot(data = plot_factors, aes(x = DateTime, y = value, group = factor_name, color = factor_name))+
  geom_line()+
  theme_bw()
p7

# look at bias + limiting factor
p8 <- ggplot(data = bias_and_factors)+
  geom_line(aes(x = DateTime, y = bias))+
  geom_point(aes(x = DateTime, y = line, group = factor_name, color = factor_name))+
  geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed")+
  ylab("bias (predicted - observed)")+
  labs(color = "Limiting factor")+
  theme_bw()
p8

biasfactor <- plot_grid(p7, p6, ncol = 1)
ggsave(biasfactor, filename = "./figures/biasAndLimitingFactorsGLM-AED.png",
       device = "png", height = 7, width = 7, units = "in")

