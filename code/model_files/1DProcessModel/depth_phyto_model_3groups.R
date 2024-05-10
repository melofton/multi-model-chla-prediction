get_T_parms <- function(group_parms){

  # unpack parms
  T_std = group_parms$T_std
  T_opt = group_parms$T_opt
  T_max = group_parms$T_max

  theta = group_parms$theta_growth

  t20 = 20
  tol   = 0.05
  inn = 1
  a0 = theta^(T_std-t20)
  a1 = theta^(T_opt-t20)
  a2 = theta^(T_max-t20)

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
      G = k * theta^(k * T_opt) * a2 - a1 * (theta^(k * T_max) - theta^(k * T_std))
      devG = theta^(k * T_opt) * a2 * (inn + k * T_opt * log(theta)) - a1 * log(theta) * (T_max * theta^(k * T_max) - T_std * theta^(k * T_std))
    } else { # Use the condition f(T)=1 at T=Tsta
      G = k * theta^(k * T_opt) * (a0 - a2 - inn) - a1 * (theta^(k * T_std) - theta^(k * T_max))
      devG = (a0 - a2 - inn) * theta^(k * T_opt) * (inn + k * T_opt * log(theta)) - a1 * log(theta) * (T_std * theta^(k * T_std) - T_max * theta^(k * T_max))
    }

    # Find the next iteration of k
    k = k - G / devG

    if((G <= -tol) | (G >= tol)){
      break
    }
  }

  # Get the remaining model constants
  if(k != 0.0){
    a=-log(a1/(k*theta^(k*T_opt)))/(k*log(theta))
    if(curvef == TRUE){
      b=theta^(k*(T_std-a))
    } else {
      b=inn+theta^(k*(T_std-a))-a0
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

get_fT <- function(layer_temp, T_std, T_max, T_parms, theta_growth, tp){
  kTn = T_parms$kTn
  aTn = T_parms$aTn
  bTn = T_parms$bTn
  fT <- NULL
for(i in 1:length(layer_temp)){
  fT[i] = 1
  if(layer_temp[i] > T_max){
    fT[i] = 0
  } else if(layer_temp[i] < T_std){
    fT[i] = theta_growth^(layer_temp[i]-tp)
  } else {
    fT[i] = theta_growth^(layer_temp[i]-tp) - theta_growth^(kTn*(layer_temp[i] - aTn)) + bTn
  }
}
fT[fT < 0] <- 0.0
return(fT)
}

phyto_depth_model <- function(t, state, parms, inputs) {

  #Calculate the environment (note that this depends on time)
  layer_temp <- inputs[[t]]$temp
  PAR_surface <- inputs[[t]]$par
  inflow_n <-  inputs[[t]]$inflow_n
  inflow_p <-  inputs[[t]]$inflow_p
  outflow <- inputs[[t]]$outflow
  areas_mid <- inputs[[t]]$areas_mid
  areas_interface <- inputs[[t]]$areas_interface
  inflow_area <- inputs[[t]]$inflow_area
  outflow_area<- inputs[[t]]$outflow_area

  #Unpack parameters
  w_p <- parms[["w_p"]]
  R_growth <- parms[["R_growth"]]
  theta_growth <- parms[["theta_growth"]]
  light_extinction <- parms[["light_extinction"]]
  I_K <- parms[["I_K"]]
  N_o <- parms[["N_o"]]
  K_N <- parms[["K_N"]]
  P_o <- parms[["P_o"]]
  K_P <- parms[["K_P"]]
  f_pr <- parms[["f_pr"]]
  R_resp <- parms[["R_resp"]]
  theta_resp <- parms[["theta_resp"]]
  T_std <- parms[["T_std"]]
  T_opt <- parms[["T_opt"]]
  T_max <- parms[["T_max"]]
  N_C_ratio <- parms[["N_C_ratio"]]
  P_C_ratio <- parms[["P_C_ratio"]]
  phyto_flux_top <- parms[["phyto_flux_top"]]
  #area <- parms[19]
  lake_depth <- parms[["lake_depth"]]
  num_boxes <- parms[["num_boxes"]]
  KePHY <- parms[["KePHY"]]
  D_temp <- parms[["D_temp"]]
  num_phytos <- parms[["num_phytos"]]
  phyto_flux_bottom <- parms[["phyto_flux_bottom"]]

  Tparms_hot <- get_T_parms(group_parms = list(T_std = T_std[1], T_opt = T_opt[1], T_max = T_max[1], theta_growth = theta_growth[1]))
  Tparms_cold <- get_T_parms(group_parms = list(T_std = T_std[2], T_opt = T_opt[2], T_max = T_max[2], theta_growth = theta_growth[2]))
  Tparms_Nfixer <- get_T_parms(group_parms = list(T_std = T_std[3], T_opt = T_opt[3], T_max = T_max[3], theta_growth = theta_growth[3]))
  
  #unpack states
  #note that states are vector where each cell is different depth
  PHYTO_hot <- state[1:num_boxes]
  PHYTO_cold <- state[(num_boxes+1):(2 * num_boxes)]
  PHYTO_Nfixer <- state[(2* num_boxes+1):(3 * num_boxes)]
  NIT <- state[(3* num_boxes+1):(4 * num_boxes)]
  PHS <- state[(4* num_boxes+1):(5 * num_boxes)]

  #Calcuate the thickness of each layer
  delx <- lake_depth / num_boxes

  # Reaction
  #calculate the depth of the middle of the box
  layer_mid_depths <- seq(delx, lake_depth, by = delx)
  #calculate the light extinction due to the phytos above
  layer_light_extinction <- light_extinction + cumsum(as.vector(PHYTO_hot))*KePHY[1] +
    cumsum(as.vector(PHYTO_cold))*KePHY[2] + cumsum(as.vector(PHYTO_Nfixer))*KePHY[3]
  #calculate the PAR at each layer
  layer_PAR <- PAR_surface * exp(-layer_light_extinction * layer_mid_depths)
  
  #Diffusion (assume proportional to temperature gradient)
  temp_diff <- diff(layer_temp)
  temp_diff[which(abs(temp_diff) < 0.01)] <- 0.01
  D <- D_temp * c(0, 1/abs(temp_diff), 0)

  # standard temperature
  tp = 20
  
  # Temperature regulation of photosynthesis
  fT_hot <- get_fT(layer_temp = layer_temp, T_std = T_std[1], T_max = T_max[1], 
                   T_parms = Tparms_hot, theta_growth = theta_growth[1], tp = tp)
  fT_cold <- get_fT(layer_temp = layer_temp, T_std = T_std[2], T_max = T_max[2], 
                   T_parms = Tparms_cold, theta_growth = theta_growth[2], tp = tp)
  fT_Nfixer <- get_fT(layer_temp = layer_temp, T_std = T_std[3], T_max = T_max[3], 
                   T_parms = Tparms_Nfixer, theta_growth = theta_growth[3], tp = tp)

  #Nitrogen limitation
  fN_hot <- (NIT - N_o[1]) / (NIT - N_o[1] + K_N[1])
  fN_cold <- (NIT - N_o[2]) / (NIT - N_o[2] + K_N[2])
  fN_Nfixer <- (NIT - N_o[3]) / (NIT - N_o[3] + K_N[3])

  #Phosphorus limitation
  fP_hot <- (PHS - P_o[1]) / (PHS - P_o[1] + K_P[1])
  fP_cold <- (PHS - P_o[2]) / (PHS - P_o[2] + K_P[2])
  fP_Nfixer <- (PHS - P_o[3]) / (PHS - P_o[3] + K_P[3])

  #Light limitation
  fI_hot = (layer_PAR/I_K[1]) / (1 + (layer_PAR/I_K[1]))
  fI_cold = (layer_PAR/I_K[2]) / (1 + (layer_PAR/I_K[2]))
  fI_Nfixer = (layer_PAR/I_K[3]) / (1 + (layer_PAR/I_K[3]))

  #Combined resource limitation
  fResources_hot <- apply(data.frame(fN = fN_hot, fP = fP_hot, fI = fI_hot), 1, FUN = min)
  fResources_cold <- apply(data.frame(fN = fN_cold, fP = fP_cold, fI = fI_cold), 1, FUN = min)
  fResources_Nfixer <- apply(data.frame(fN = fN_Nfixer, fP = fP_Nfixer, fI = fI_Nfixer), 1, FUN = min)

  #primary productivity
  prim_prod_hot <- R_growth[1] * fT_hot * fResources_hot
  prim_prod_cold <- R_growth[2] * fT_cold * fResources_cold
  prim_prod_Nfixer <- R_growth[3] * fT_Nfixer * fResources_Nfixer
  
  #Photoexudation
  exudation_hot <- prim_prod_hot * f_pr[1]
  exudation_cold <- prim_prod_cold * f_pr[2]
  exudation_Nfixer <- prim_prod_Nfixer * f_pr[3]
  
  #Temperature regulation of respiration
  fT_respiration_hot <- theta_resp[1]^(layer_temp - 20.0)
  fT_respiration_cold <- theta_resp[2]^(layer_temp - 20.0)
  fT_respiration_Nfixer <- theta_resp[3]^(layer_temp - 20.0)
  
  #Respiration
  respiration_hot <- PHYTO_hot * R_resp[1] * fT_respiration_hot
  respiration_cold <- PHYTO_cold * R_resp[2] * fT_respiration_cold
  respiration_Nfixer <- PHYTO_Nfixer * R_resp[3] * fT_respiration_Nfixer
  
  #Nitrogen uptake associated with primary productivity
  NIT_uptake <- ((prim_prod_hot + prim_prod_cold + prim_prod_Nfixer) - (exudation_hot + exudation_cold + exudation_Nfixer)) * N_C_ratio
  
  #Phorphorus uptake associated with primary productivity
  PHS_uptake <- ((prim_prod_hot + prim_prod_cold + prim_prod_Nfixer) - (exudation_hot + exudation_cold + exudation_Nfixer)) * P_C_ratio
  
  #nutrient turnover associated with respiration
  NIT_turnover <- (respiration_hot + respiration_cold + respiration_Nfixer) * N_C_ratio
  PHS_turnover <- (respiration_hot + respiration_cold + respiration_Nfixer) * P_C_ratio
  
  #Stream inflow and outflow
  PHYTO_horizontal_hot <- -PHYTO_hot * outflow * (outflow_area / areas_mid)
  PHYTO_horizontal_cold <- -PHYTO_cold * outflow * (outflow_area / areas_mid)
  PHYTO_horizontal_Nfixer <- -PHYTO_Nfixer * outflow * (outflow_area / areas_mid)
  
  #Net reaction of the states
  PHYTO_reaction_hot <- prim_prod_hot - respiration_hot - exudation_hot + PHYTO_horizontal_hot
  PHYTO_reaction_cold <- prim_prod_cold - respiration_cold - exudation_cold + PHYTO_horizontal_cold
  PHYTO_reaction_Nfixer <- prim_prod_Nfixer - respiration_Nfixer - exudation_Nfixer + PHYTO_horizontal_Nfixer
  
  # Advection calculation 
  PHYTO_advection_flux_hot <- c(phyto_flux_top[1], -w_p[1] * PHYTO_hot * c((1/abs(temp_diff)),phyto_flux_bottom[1])) * areas_interface
  PHYTO_advection_flux_cold <- c(phyto_flux_top[2], -w_p[2] * PHYTO_cold * c((1/abs(temp_diff)),phyto_flux_bottom[2])) * areas_interface
  PHYTO_advection_flux_Nfixer <- c(phyto_flux_top[3], -w_p[3] * PHYTO_Nfixer * c((1/abs(temp_diff)),phyto_flux_bottom[3])) * areas_interface
  
  PHYTO_advection_hot <- -(1/areas_mid) * (diff(PHYTO_advection_flux_hot) / delx)
  PHYTO_advection_cold <- -(1/areas_mid) * (diff(PHYTO_advection_flux_cold) / delx)
  PHYTO_advection_Nfixer <- -(1/areas_mid) * (diff(PHYTO_advection_flux_Nfixer) / delx)
  
  # Diffusion calcuation
  #Phyto
  gradient_middle_boxes <- diff(PHYTO_hot)
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  PHYTO_diffusion_hot <- (1/areas_mid) * (diff(diffusion_flux) / delx)
  
  gradient_middle_boxes <- diff(PHYTO_cold)
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  PHYTO_diffusion_cold <- (1/areas_mid) * (diff(diffusion_flux) / delx)
  
  gradient_middle_boxes <- diff(PHYTO_Nfixer)
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  PHYTO_diffusion_Nfixer <- (1/areas_mid) * (diff(diffusion_flux) / delx)
  
  #Net change for each box
  dPHYTO_dt_hot <- PHYTO_advection_hot + PHYTO_reaction_hot + PHYTO_diffusion_hot
  dPHYTO_dt_cold <- PHYTO_advection_cold + PHYTO_reaction_cold + PHYTO_diffusion_cold
  dPHYTO_dt_Nfixer <- PHYTO_advection_Nfixer + PHYTO_reaction_Nfixer + PHYTO_diffusion_Nfixer
  
  # Nutrients

  #Stream inflow and outflow
  NIT_horizontal <- inflow_n * (inflow_area / areas_mid) - NIT * outflow * (outflow_area / areas_mid)
  PHS_horizontal <-  inflow_p * (inflow_area / areas_mid) - PHS * outflow * (outflow_area / areas_mid)

  #Net reaction of the states
  NIT_reaction <- -NIT_uptake + NIT_turnover + NIT_horizontal 
  PHS_reaction <- -PHS_uptake + PHS_turnover + PHS_horizontal

  # Advection calculation 
  w_p_nut <- 0.001
  NIT_advection_flux <- c(0, -w_p_nut * NIT) * areas_interface
  NIT_advection <- -(1/areas_mid) * (diff(NIT_advection_flux) / delx)

  PHS_advection_flux <- c(0, -w_p_nut * PHS) * areas_interface
  PHS_advection <- -(1/areas_mid) * (diff(PHS_advection_flux) / delx)

  #Nitrogen
  gradient_middle_boxes <- diff(NIT)
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  NIT_diffusion <- (1/areas_mid) * (diff(diffusion_flux) / delx)

  #Phorphorus
  gradient_middle_boxes <- diff(PHS)
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  PHS_diffusion <- (1/areas_mid) * (diff(diffusion_flux) / delx)

  #Net change for each box
  dNIT_dt <- NIT_advection + NIT_diffusion + NIT_reaction
  dPHS_dt <- PHS_advection + PHS_diffusion + PHS_reaction

  list(c(dPHYTO_dt_hot, dPHYTO_dt_cold, dPHYTO_dt_Nfixer, dNIT_dt, dPHS_dt),  #This returns the vector of derivatives for each layer box
       c(fN_hot = fN_hot,
         fN_cold = fN_cold,
         fN_Nfixer = fN_Nfixer,
         fP_hot = fP_hot,
         fP_cold = fP_cold,
         fP_Nfixer = fP_Nfixer,
         fI_hot = fI_hot,
         fI_cold = fI_cold,
         fI_Nfixer = fI_Nfixer,
         fResources_hot = fResources_hot,
         fResources_cold = fResources_cold,
         fResources_Nfixer = fResources_Nfixer,
         fT_hot = fT_hot,
         fT_cold = fT_cold,
         fT_Nfixer = fT_Nfixer,
         layer_light_extinction = layer_light_extinction, 
         layer_PAR = layer_PAR))  #This returns the vector of diagnostics
}
