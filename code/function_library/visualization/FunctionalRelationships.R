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
I_K = 20

# plot
jpeg("./figures/light_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f1(x,I_K=I_K),from=0, to=160,ylab='fI',xlab = expression(paste("Light intensity"," (",mu,"mol ",m^-2," ",s^-1,")")),ylim = c(0,1), yaxt="n")
axis(2, las = 2)
legend("bottomright", lty = c(1), legend = c("Monod: saturating light response"),bty = "n")
dev.off()


f3 <- function(x, theta_resp){
  y = theta_resp^(x-20)
}
theta_resp = 1.02

jpeg("./figures/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f3(x,theta_resp = theta_resp),from=1, to=30,ylab='Respiration scalar',xlab = 'Water temperature (°C)')
dev.off()

f4 <- function(DIN, N_0, K_N){
  y = (DIN - N_0) / (DIN - N_0 + K_N)
}
K_N = 10
N_0 = 0

jpeg("./figures/DIN_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f4(x, N_0 = N_0, K_N = K_N),from=0, to=200,ylab='fN',xlab = expression(paste("DIN"," (",mu,"mol N ",m^-3,")")),ylim = c(0,1))
dev.off()

f5 <- function(FRP, P_0, K_P){
  y = (FRP - P_0) / (FRP - P_0 + K_P)
}
K_P = 0.15
P_0 = 0

jpeg("./figures/FRP_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f5(x, P_0 = P_0, K_P = K_P),from=0, to=0.2,ylab='fP',xlab = expression(paste("FRP"," (",mu,"mol P ",m^-3,")")),ylim = c(0,1))
dev.off()

# AED temperature function - why so complex??
g1 <- list(T_std = 10,
           T_opt = 28,
           T_max = 35,
           Ts = 10,
           To = 28,
           Tm = 35,
           v = 1.08,
           theta = 1.08)
g2 <- list(T_std = 10,
           T_opt = 12,
           T_max = 30,
           Ts = 10,
           To = 12,
           Tm = 30,
           v = 1.02,
           theta = 1.02)

get_T_parms <- function(group_parms){
  
  # unpack parms
  T_std = group_parms$T_std
  T_opt = group_parms$T_opt
  T_max = group_parms$T_max
  
  Ts = group_parms$Ts
  To = group_parms$To
  Tm = group_parms$Tm
  
  v = group_parms$v
  theta = group_parms$theta
  
t20 = 20
tol   = 0.05
inn = 1
a0 = v^(Ts-t20)
a1 = v^(To-t20)
a2 = v^(Tm-t20)

# Perform the iteration to find the constants.
# First approximation of k.
k = 6.0
i = 0
G = tol + 1.0
curvef = TRUE
# Do the iterations until -tol < G < tol

repeat{
  
  i=i+1
  
  if(i == 100){ #increases the tolerance if more than 100 iterations performed
    i=0            
    tol=tol+0.01
  }
  
  if(curvef == TRUE){ # Use the condition f(T)=v**(T-20) at T=Tsta
    G = k * v^(k * To) * a2 - a1 * (v^(k * Tm) - v^(k * Ts))
    devG = v^(k * To) * a2 * (inn + k * To * log(v)) - a1 * log(v) * (Tm * v^(k * Tm) - Ts * v^(k * Ts))
  } else { # Use the condition f(T)=1 at T=Tsta
    G = k * v^(k * To) * (a0 - a2 - inn) - a1 * (v^(k * Ts) - v^(k * Tm))
    devG = (a0 - a2 - inn) * v^(k * To) * (inn + k * To * log(v)) - a1 * log(v) * (Ts * v^(k * Ts) - Tm * v^(k * Tm))
  }
  
  # Find the next iteration of k
  k = k - G / devG
  
  if((G <= -tol) | (G >= tol)){
    break
  }
}

# Get the remaining model constants
if(k != 0.0){
  a=-log(a1/(k*v^(k*To)))/(k*log(v))
  if(curvef == TRUE){
    b=v^(k*(Ts-a))
  } else {
    b=inn+v^(k*(Ts-a))-a0
  }
} else {
  a=0.0
  b=0.0
}

# Set the model constants to the calculated values
kTn = k
aTn = a
bTn = b

return(list(kTn = kTn, aTn = aTn, bTn = bTn))

}

g1_Tparms <- get_T_parms(group_parms = g1)
g2_Tparms <- get_T_parms(group_parms = g2)

tp = 20

f6 <- function(temp, theta, tp, kTn, aTn, bTn, T_std, T_opt, T_max){
  y = NULL
  for(i in 1:length(temp)){
  y[i] = 1
  if(temp[i] > T_max){
    y[i] = 0
  } else if(temp[i] < T_std){
      y[i] = theta^(temp[i]-tp)
  } else {
      y[i] = theta^(temp[i]-tp) - theta^(kTn*(temp[i] - aTn)) + bTn
  }
  }
  return(y)

}

jpeg("./figures/AED_temp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f6(x, theta = g1$theta, tp = tp, kTn = g1_Tparms$kTn, aTn = g1_Tparms$aTn, bTn = g1_Tparms$bTn, T_std = g1$T_std, T_opt = g1$T_opt, T_max = g1$T_max),from=0, to=30, n = 300, ylab='fT',xlab = "Water temperature (ºC)")
dev.off()

jpeg("./figures/AED_temp_eq_2groups.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f6(x, theta = g1$theta, tp = tp, kTn = g1_Tparms$kTn, aTn = g1_Tparms$aTn, bTn = g1_Tparms$bTn, T_std = g1$T_std, T_opt = g1$T_opt, T_max = g1$T_max),from=0, to=30, n = 300, ylab='fT',xlab = "Water temperature (ºC)", yaxt = "n", ylim = c(0,1.5))
axis(2, las = 2)
curve(f6(x, theta = g2$theta, tp = tp, kTn = g2_Tparms$kTn, aTn = g2_Tparms$aTn, bTn = g2_Tparms$bTn, T_std = g2$T_std, T_opt = g2$T_opt, T_max = g2$T_max),from=0, to=30, n = 300, add = TRUE, lty = 2)
legend("topleft", lty = c(1,2), legend = c("Warm group","Cold group"),bty = "n")
dev.off()





