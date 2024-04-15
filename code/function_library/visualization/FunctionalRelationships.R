# Functional Relationship plots
# Author: Mary Lofton
# Date last updated: 15APR24

# Purpose: visualize growth responses to light, temperature, and nutrients for simple
# process models

# load packages
library(tidyverse)
library(lubridate)

# Monod function for light
f1 <- function(par, I_K){
  y = 1 - exp(-(par/I_K))
}
I_K = 200

# check to see what range we should plot over
dat <- read_csv("./data/data_processed/processModels.csv") %>%
  filter(year(datetime) <= 2021) 
range(dat$PAR_umolm2s_mean)
plot(dat$datetime, dat$PAR_umolm2s_mean)
# values above ~800 look like outliers, going to cap plot at 800 umolm2s

# plot
jpeg("./figures/light_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f1(x,I_K=I_K),from=0, to=800,ylab='fI',xlab = expression(paste("Light intensity"," (",mu,"mol ",m^-2," ",s^-1,")")),ylim = c(0,1), yaxt="n")
axis(2, las = 2)
legend("bottomright", lty = c(1), legend = c("Monod: saturating light response"),bty = "n")
dev.off()

# Hellweger function for temperature
# source: 
f2 <- function(x, T_0, q){
  y = exp(-( (x - T_0) / q )^2)
}
T_0 = 22
q = 5

jpeg("./figures/temp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f2(x,T_0 = T_0, q = q),from=1, to=30,ylab='fT',xlab = 'Water temperature (°C)')
dev.off()

f3 <- function(x, theta_resp){
  y = theta_resp^(x-20)
}
theta_resp = 1.08

jpeg("./figures/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f3(x,theta_resp = theta_resp),from=1, to=30,ylab='Respiration scalar',xlab = 'Water temperature (°C)')
dev.off()

f4 <- function(DIN, N_0, K_N){
  y = (DIN - N_0) / (DIN - N_0 + K_N)
}
K_N = 10
N_0 = 0

# check range to plot
range(dat$DIN_ugL)
plot(dat$datetime, dat$DIN_ugL)
# values above ~200 look like outliers, going to cap plot at 200 ugL

jpeg("./figures/DIN_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f4(x, N_0 = N_0, K_N = K_N),from=0, to=200,ylab='fN',xlab = expression(paste("DIN"," (",mu,"mol N ",m^-3,")")),ylim = c(0,1))
dev.off()

f5 <- function(FRP, P_0, K_P){
  y = (FRP - P_0) / (FRP - P_0 + K_P)
}
K_P = 1
P_0 = 0

# check range to plot
range(dat$SRP_ugL)
plot(dat$datetime, dat$SRP_ugL)
# values above ~200 look like outliers, going to cap plot at 200 ugL

jpeg("./figures/FRP_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f5(x, P_0 = P_0, K_P = K_P),from=0, to=20,ylab='fP',xlab = expression(paste("FRP"," (",mu,"mol P ",m^-3,")")),ylim = c(0,1))
dev.off()






