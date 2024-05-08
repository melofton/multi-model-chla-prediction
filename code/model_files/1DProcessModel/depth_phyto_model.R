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

  Tparms <- list()
  for(i in 1:length(T_std)){
  Tparms[[i]] <- get_T_parms(group_parms = list(T_std = T_std[i], T_opt = T_opt[i], T_max = T_max[i], theta_growth = theta_growth[i]))
  }


  #unpack states
  #note that states are vector where each cell is different depth
  PHYTO = vector("list", length = num_phytos)
  PHYTO[[1]] <- state[1:num_boxes]
  PHYTO[[2]] <- state[(num_boxes+1):(2 * num_boxes)]
  PHYTO[[3]] <- state[(2* num_boxes+1):(3 * num_boxes)]
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

  #set placeholders for internal variables
  fT = vector("list", length = num_phytos)
  fI = vector("list", length = num_phytos)
  fN = vector("list", length = num_phytos)
  fP = vector("list", length = num_phytos)
  fResources = vector("list", length = num_phytos)
  prim_prod = vector("list", length = num_phytos)
  exudation = vector("list", length = num_phytos)
  fT_respiration = vector("list", length = num_phytos)
  respiration = vector("list", length = num_phytos)
  NIT_uptake = vector("list", length = num_phytos)
  PHS_uptake = vector("list", length = num_phytos)
  NIT_turnover = vector("list", length = num_phytos)
  PHS_turnover = vector("list", length = num_phytos)
  PHYTO_horizontal = vector("list", length = num_phytos)
  PHYTO_reaction = vector("list", length = num_phytos)
  PHYTO_advection_flux = vector("list", length = num_phytos)
  PHYTO_advection = vector("list", length = num_phytos)
  PHYTO_diffusion = vector("list", length = num_phytos)
  dPHYTO_dt = vector("list", length = num_phytos)
  tp = 20
  
  # loop through phyto groups
  for(j in 1:num_phytos){
    
  # Temperature regulation of photosynthesis
  kTn = Tparms[[j]]$kTn
  aTn = Tparms[[j]]$aTn
  bTn = Tparms[[j]]$bTn
  
  for(i in 1:length(layer_temp)){
    fT[[j]][i] = 1
    if(layer_temp[i] > T_max[j]){
      fT[[j]][i] = 0
    } else if(layer_temp[i] < T_std[j]){
      fT[[j]][i] = theta_growth[j]^(layer_temp[i]-tp)
    } else {
      fT[[j]][i] = theta_growth[j]^(layer_temp[i]-tp) - theta_growth[j]^(kTn*(layer_temp[i] - aTn)) + bTn
    }
  }
  fT[[j]][fT[[j]] < 0] <- 0.0

  #Nitrogen limitation
  fN[[j]] <- (NIT - N_o[j]) / (NIT - N_o[j] + K_N[j])

  #Phosphorus limitation
  fP[[j]] <- (PHS - P_o[j]) / (PHS - P_o[j] + K_P[j])

  #Light limitation
  fI[[j]] = (layer_PAR/I_K[j]) / (1 + (layer_PAR/I_K[j]))

  #Combined resource limitation
  fResources[[j]] <- apply(data.frame(fN = fN[[j]], fP = fP[[j]], fI = fI[[j]]), 1, FUN = min)

  #primary productivity
  prim_prod[[j]] <- R_growth[j] * fT[[j]] * fResources[[j]]
  
  #Photoexudation
  exudation[[j]] <- prim_prod[[j]] * f_pr[j]
  
  #Temperature regulation of respiration
  fT_respiration[[j]] <- theta_resp[j]^(layer_temp - 20.0)
  
  #Respiration
  respiration[[j]] <- PHYTO[[j]] * R_resp[j] * fT_respiration[[j]]
  
  #Nitrogen uptake associated with primary productivity
  NIT_uptake[[j]] <- (prim_prod[[j]] - exudation[[j]]) * N_C_ratio
  
  #Phorphorus uptake associated with primary productivity
  PHS_uptake[[j]] <- (prim_prod[[j]] - exudation[[j]]) * P_C_ratio
  
  #nutrient turnover associated with respiration
  NIT_turnover[[j]] <- respiration[[j]] * N_C_ratio
  PHS_turnover[[j]] <- respiration[[j]] * P_C_ratio
  
  #Stream inflow and outflow
  PHYTO_horizontal[[j]] <- -PHYTO[[j]] * outflow * (outflow_area / areas_mid)
  
  #Net reaction of the states
  PHYTO_reaction[[j]] <- prim_prod[[j]] - respiration[[j]] - exudation[[j]] + PHYTO_horizontal[[j]]
  
  # Advection calculation 
  PHYTO_advection_flux[[j]] <- c(phyto_flux_top[j], -w_p[j] * PHYTO[[j]] * c((1/abs(temp_diff)),phyto_flux_bottom[j])) * areas_interface
  PHYTO_advection[[j]] <- -(1/areas_mid) * (diff(PHYTO_advection_flux[[j]]) / delx)
  
  # Diffusion calcuation
  #Phyto
  gradient_middle_boxes <- diff(PHYTO[[j]])
  gradient <- c(0, gradient_middle_boxes, 0) / delx
  diffusion_flux <- areas_interface * D * gradient
  PHYTO_diffusion[[j]] <- (1/areas_mid) * (diff(diffusion_flux) / delx)
  
  #Net change for each box
  dPHYTO_dt[[j]] <- PHYTO_advection[[j]] + PHYTO_reaction[[j]] + PHYTO_diffusion[[j]]
  
  }
  
  # Nutrients

  #Stream inflow and outflow
  NIT_horizontal <- inflow_n * (inflow_area / areas_mid) - NIT * outflow * (outflow_area / areas_mid)
  PHS_horizontal <-  inflow_p * (inflow_area / areas_mid) - PHS * outflow * (outflow_area / areas_mid)

  #Net reaction of the states
  NIT_reaction <- -Reduce(`+`, NIT_uptake) + Reduce(`+`, NIT_turnover) + NIT_horizontal # sum(unlist(lapply(list, length)))
  PHS_reaction <- -Reduce(`+`, PHS_uptake) + Reduce(`+`, PHS_turnover) + PHS_horizontal

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

  list(c(Reduce(`c`,dPHYTO_dt), dNIT_dt, dPHS_dt),  #This returns the vector of derivatives for each layer box
       c(fN = Reduce(`c`,fN), fP = Reduce(`c`,fP), fI = Reduce(`c`,fI), fResources = Reduce(`c`,fResources), fT = Reduce(`c`,fT), layer_light_extinction = layer_light_extinction, layer_PAR = layer_PAR))  #This returns the vector of diagnostics
}
