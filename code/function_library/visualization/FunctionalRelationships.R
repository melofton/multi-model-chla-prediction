# Functional Relationship plots
# Author: Mary Lofton
# Date last updated: 15APR24

# Purpose: visualize growth responses to light, temperature, and nutrients for simple
# process models

# load packages
library(tidyverse)
library(lubridate)

PlotMonodLightLimitation <- function(I_K = 20, xlim = c(0,160), save_plot = TRUE){

# Monod function for light
f1 <- function(par, I_K){
  y = (par/I_K) / (1 + (par/I_K))
}

par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f1(x,I_K=I_K),from=xlim[1], to=xlim[2],ylab='fI',xlab = expression(paste("Light intensity"," (",mu,"mol ",m^-2," ",s^-1,")")),ylim = c(0,1), yaxt="n")
axis(2, las = 2)
legend("bottomright", lty = c(1), legend = c("Monod: saturating light response"),bty = "n")

# SAVE plot
if(save_plot == TRUE){
jpeg("./figures/light_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f1(x,I_K=I_K),from=xlim[1], to=xlim[2],ylab='fI',xlab = expression(paste("Light intensity"," (",mu,"mol ",m^-2," ",s^-1,")")),ylim = c(0,1), yaxt="n")
axis(2, las = 2)
legend("bottomright", lty = c(1), legend = c("Monod: saturating light response"),bty = "n")
dev.off()
}
}


PlotRespiration <- function(R_resp = 0.08, theta_resp = 1.08, xlim = c(1,30), save_plot = TRUE){

f3 <- function(x, theta_resp, R_resp){
  y = R_resp*theta_resp^(x-20)
}

par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f3(x,theta_resp = theta_resp, R_resp = R_resp),from=xlim[1], to=xlim[2],ylab='Respiration scalar',xlab = 'Water temperature (°C)')

if(save_plot == TRUE){
  jpeg("./figures/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
  par(cex.lab = 1.5, mgp = c(2.7,1,0))
  curve(f3(x,theta_resp = theta_resp, R_resp = R_resp),from=xlim[1], to=xlim[2],ylab='Respiration scalar',xlab = 'Water temperature (°C)')
  dev.off()
}
}

PlotNLimitation <- function(K_N = 0.25, N_0 = 0, xlim = c(0,6), save_plot = TRUE){

  f4 <- function(DIN, N_0, K_N){
  y = (DIN - N_0) / (DIN - N_0 + K_N)
  }
  
  par(cex.lab = 1.5, mgp = c(2.7,1,0))
  curve(f4(x, N_0 = N_0, K_N = K_N),from=xlim[1], to=xlim[2],ylab='fN',xlab = expression(paste("DIN"," (",mu,"mol N ",m^-3,")")),ylim = c(0,1))

  if(save_plot == TRUE){
jpeg("./figures/DIN_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f4(x, N_0 = N_0, K_N = K_N),from=xlim[1], to=xlim[2],ylab='fN',xlab = expression(paste("DIN"," (",mu,"mol N ",m^-3,")")),ylim = c(0,1))
dev.off()
}
}

PlotPLimitation <- function(K_P = 0.15, P_0 = 0, xlim = c(0,0.15), save_plot = TRUE){
f5 <- function(FRP, P_0, K_P){
  y = (FRP - P_0) / (FRP - P_0 + K_P)
}

par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f5(x, P_0 = P_0, K_P = K_P),from=0, to=0.2,ylab='fP',xlab = expression(paste("FRP"," (",mu,"mol P ",m^-3,")")),ylim = c(0,1))

if(save_plot == TRUE){
jpeg("./figures/FRP_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f5(x, P_0 = P_0, K_P = K_P),from=xlim[1], to=xlim[2],ylab='fP',xlab = expression(paste("FRP"," (",mu,"mol P ",m^-3,")")),ylim = c(0,1))
dev.off()
}
}


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

PlotTemperatureLimitation2Groups <- function(g1_parms, g2_parms, save_plot = TRUE){

  # AED temperature function - why so complex??
g1 <- g1_parms
g2 <- g2_parms

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

par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f6(x, theta = g1$theta, tp = tp, kTn = g1_Tparms$kTn, aTn = g1_Tparms$aTn, bTn = g1_Tparms$bTn, T_std = g1$T_std, T_opt = g1$T_opt, T_max = g1$T_max),from=0, to=30, n = 300, ylab='fT',xlab = "Water temperature (ºC)", yaxt = "n")
axis(2, las = 2)
curve(f6(x, theta = g2$theta, tp = tp, kTn = g2_Tparms$kTn, aTn = g2_Tparms$aTn, bTn = g2_Tparms$bTn, T_std = g2$T_std, T_opt = g2$T_opt, T_max = g2$T_max),from=0, to=30, n = 300, add = TRUE, lty = 2)
legend("topleft", lty = c(1,2), legend = c("Warm group","Cold group"),bty = "n")

if(save_plot == TRUE){
jpeg("./figures/AED_temp_eq_2groups.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f6(x, theta = g1$theta, tp = tp, kTn = g1_Tparms$kTn, aTn = g1_Tparms$aTn, bTn = g1_Tparms$bTn, T_std = g1$T_std, T_opt = g1$T_opt, T_max = g1$T_max),from=0, to=30, n = 300, ylab='fT',xlab = "Water temperature (ºC)", yaxt = "n")
axis(2, las = 2)
curve(f6(x, theta = g2$theta, tp = tp, kTn = g2_Tparms$kTn, aTn = g2_Tparms$aTn, bTn = g2_Tparms$bTn, T_std = g2$T_std, T_opt = g2$T_opt, T_max = g2$T_max),from=0, to=30, n = 300, add = TRUE, lty = 2)
legend("topleft", lty = c(1,2), legend = c("Warm group","Cold group"),bty = "n")
dev.off()
}

}





