

##### Global functions

lossfn_FMDProj <- function(theta,e,ps,pc){
  mu <- theta[1]
  s <- theta[2]
  
  v <- sum((e - ((( mu - ((ps-pc)/phi)))/s)))^2
  
  return(v)
}

optParamFunction_FMDProj <- function(sl, cl, ps, pc, thetas, adj){
  
  s <- sl
  c <- cl
  
  sl_share <- s/((s+c) * adj)
  cl_share <- 1-sl_share
  
  tilde <- log((1-cl_share)/cl_share)
  
  theta0 <- thetas
  
  out <- BBoptim(par= theta0, fn = lossfn_FMDProj, e = tilde ,ps=ps, pc=pc)
  
  muTilde <- out$par[1]
  sTilde <- out$par[2]
  
  return(c(muTilde,sTilde))
  
}

get_k0s_Global_FMD <- function(proj_Q_P, beefINV_FORECAST, calfCrop){
  
  k0s_df <- data.frame(Year = numeric(nrow(proj_Q_P)), k02 = numeric(nrow(proj_Q_P)) , k03 = numeric(nrow(proj_Q_P)), 
                       k04 = numeric(nrow(proj_Q_P)), k05 = numeric(nrow(proj_Q_P)), k06 = numeric(nrow(proj_Q_P)), 
                       k07 = numeric(nrow(proj_Q_P)), k08 = numeric(nrow(proj_Q_P)))
  # calfCrop <- calf_crop_proj_N
  for (i in 1:nrow(proj_Q_P)){
    
    getYear <- beefINV_FORECAST$Year[i]
    
    if(i>1){
      getYear <- beefINV_FORECAST$Year[i-1] + 1
    }
    
    k02[i] <- get_k0s(Yr = getYear, lag = 2, calfCrop)
    k03[i] <- get_k0s(Yr = getYear, lag = 3, calfCrop)
    k04[i] <- get_k0s(Yr = getYear, lag = 4, calfCrop)
    k05[i] <- get_k0s(Yr = getYear, lag = 5, calfCrop)
    k06[i] <- get_k0s(Yr = getYear, lag = 6, calfCrop)
    k07[i] <- get_k0s(Yr = getYear, lag = 7, calfCrop)  
    k08[i] <- get_k0s(Yr = getYear, lag = 8, calfCrop) 
    
    k0s_df[i,] <- c(getYear, k02[i], k03[i], k04[i], k05[i], k06[i], k07[i], k08[i])
    
  }
  
  return(k0s_df)
}

getSlClA_test_FMD <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, 
                              eta_k3 , int_k3, adjF, Dshock, k0s, slAvg, clAvg){
  
  estQ <- BBoptim(par = k, fn = estQFunction_test_FMD, tilde_MU = params[1], 
                  tilde_s = params[2], ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3, 
                  eta_k3 = eta_k3, int = int_k3, k0s = k0s, slAvg = slAvg, clAvg = clAvg)
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  k3_est_Head_OG <- estQ$par
  
  k3_est <- abs(estQ$par)

  slNew <- ((g * K1 - k3_est) * slAvg)/1000000000
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  # clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
  #   (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
  #   (int/(1-gamma)) * ((delta^4)/(gamma^7)) * (delta^2 * (1-gamma^7) + gamma * (1-delta) * (delta * (1-gamma^6) + gamma * (1-gamma^5)) )
  
  clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02)) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07)
  
  
  clNew <- (clNew * clAvg)/1000000000
  
  k3_est <- (k3_est * slAvg)/1000000000
  
  ANew <- (slNew + clNew)
  
  # k3_est_Head <- estQ$par
  
  k3_est_Head <- abs(estQ$par)
  
  # slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  # ANew <- (((g * K1 - k3_est) * slAvg)/1000000000) * (1/slShare)
  # 
  # slNew <- ANew * slShare * adjF
  # clNew <- ANew * (1-slShare) * adjF
  
  return(c(slNew, clNew, ANew, k3_est, k3_est_Head, k3_est_Head_OG))
  
}  


estQFunction_test_FMD <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3, 
                                  eta_k3 , int_k3, k0s, slAvg, clAvg){
  
  k3t2 <- k
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  slHead <- (A * slShare) * (1000000000/slAvg)  
  clHead <- (A * clShare) * (1000000000/clAvg)
  
  F1 <- g * K1 - k3t2 - slHead
  
  F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
    clHead
  
  # F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
  #   (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
  #   (int/(1-gamma)) * ((delta^4)/(gamma^7)) * (delta^2 * (1-gamma^7) + gamma * (1-delta) * (delta * (1-gamma^6) + gamma * (1-gamma^5)) ) -
  #   clHead
  
  F <- F1^2 + F2^2
  
}

getPsPcEpsEpc_FMD <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, 
                              ANew, params){
  
  psNew <- PsM
  pcNew <- PcM
  
  psNew_lo <- psNew  - 0.276
  pcNew_lo <- pcNew - 0.292
  
  # psNew_up <- psNew + 0.28000
  # pcNew_up <- pcNew + 0.25933
  
  psNew_up <- psNew + 0.1
  pcNew_up <- pcNew + 0.1
  
  #### Here we are making sure the lower bound for the prices isn't negative
  if(psNew_lo < 0){
    psNew_lo <- psNew
  }
  
  if(pcNew_lo < 0){
    pcNew_lo <- pcNew
  }
  
  #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
  while( pcNew_lo > psNew_lo ){
    pcNew_lo <- pcNew_lo - 0.01
  }
  
  psNew_expected <- EPsM
  pcNew_expected <- EPcM
  
  hc_new <- HcM
  
  #### Here we make sure that the holding costs are below the cull cow price
  while(hc_new > pcNew){
    hc_new <- hc_new - 0.01
  }
  
  hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  B <- psNew - g * (beta^3) * psNew_expected + hc_discounted
  
  psNew_expected_lo <- psNew_expected - 0.5
  
  psNew_expected_up <- psNew_expected + 0.1
  
  pcNew_expected_lo <- pcNew_expected - 0.5
  
  pcNew_expected_up <- pcNew_expected + 0.1
  
  if(pcNew_expected_lo < 0){
    pcNew_expected_lo <- pcNew_expected
  }
  
  if(ps_expected_lo < 0){
    psNew_expected_lo <- psNew_expected
  }
  
  while(pcNew_expected<0){
    pcNew_expected <- pcNew_expected + 0.01
  }
  
  while(psNew_expected<0){
    psNew_expected <- psNew_expected + 0.01
  }
  
  
  p <- c(psNew, pcNew, psNew_expected, pcNew_expected)
  
  lo <- c(psNew_lo, pcNew_lo, psNew_expected_lo, pcNew_expected_lo)
  up <- c(psNew_up, pcNew_up, psNew_expected_up, pcNew_expected_up)
  
  estPNew <- BBoptim(par = p, fn = estPFunction_FMD, sl = SlNew, cl = ClNew, A = ANew,
                     B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                     tilde_MU = params[1], tilde_s = params[2])
  
  # estPNew <- BBoptim(par = p, fn = estPFunction, sl = SlNew, cl = ClNew, A = ANew, 
  #                    B = B, hc_discounted = hc_discounted,
  #                    tilde_MU = params[1], tilde_s = params[2])
  
  ps1N <- estPNew$par[1]
  pc1N <- estPNew$par[2]
  ps_expected1N <- estPNew$par[3]
  pc_expected1N <- estPNew$par[4]
  
  hc1N <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    (beta * pc_expected1N + g * (beta^3) * ps_expected1N - pc1N)
  
  return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}


estPFunction_FMD <- function(p, sl, cl, A, B, hc_discounted, tilde_MU, tilde_s, hc_new){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  F1 <- sl - A * ((exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))/(1 + (exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))))
  
  F2 <- cl  - A * (1/(1+ exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s)))
  
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  
  return(F)
  
}


estRepHeifersEQ_FMD <- function(slHead, clHead, K1, gamma_k3, 
                                eta_k3 , int_k3, k0s, k){
  k3t2 <- k
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  F1 <- g * K1 - k3t2 - slHead
  
  F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
    clHead
  
  F <- F1^2 + F2^2
  
}


getSlClA_test_FMD_EQ <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, 
                                 eta_k3 , int_k3, adjF, Dshock, k0s, slAvg, clAvg,slDem, clDem){
  
  estQ <- BBoptim(par = k, fn = estQFunction_test_FMD_EQ, tilde_MU = params[1], 
                  tilde_s = params[2], ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3, 
                  eta_k3 = eta_k3, int = int_k3, k0s = k0s, slAvg = slAvg, clAvg = clAvg, 
                  slDem = slDem, clDem= clDem)
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  k3_est_Head_OG <- estQ$par
  
  k3_est <- estQ$par
  
  if(k3_est<0){
    k3_est <- 0.5 * g * K1
  }
  
  slNew <- ((g * K1 - k3_est) * slAvg)/1000000000
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  # clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
  #   (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
  #   (int/(1-gamma)) * ((delta^4)/(gamma^7)) * (delta^2 * (1-gamma^7) + gamma * (1-delta) * (delta * (1-gamma^6) + gamma * (1-gamma^5)) )
  
  clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02)) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07)
  
  
  clNew <- (clNew * clAvg)/1000000000
  
  k3_est_Head <- k3_est
  
  k3_est <- (k3_est * slAvg)/1000000000
  
  ANew <- (slNew + clNew)
  
  # k3_est_Head <- estQ$par
  
  
  
  # slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  # ANew <- (((g * K1 - k3_est) * slAvg)/1000000000) * (1/slShare)
  # 
  # slNew <- ANew * slShare * adjF
  # clNew <- ANew * (1-slShare) * adjF
  
  return(c(slNew, clNew, ANew, k3_est, k3_est_Head, k3_est_Head_OG))
  
}  


estQFunction_test_FMD_EQ <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3, 
                                     eta_k3 , int_k3, k0s, slAvg, clAvg,slDem, clDem){
  
  k3t2 <- k
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  slHead <- (A * slShare) * (1000000000/slAvg)
  clHead <- (A * clShare) * (1000000000/clAvg)
  
  # slHead <- slDem * (1000000000/slAvg)  
  # clHead <- clDem * (1000000000/clAvg)
  
  F1 <- g * K1 - k3t2 - slHead
  
  F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
    clHead
  
  # F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
  #   (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
  #   (int/(1-gamma)) * ((delta^4)/(gamma^7)) * (delta^2 * (1-gamma^7) + gamma * (1-delta) * (delta * (1-gamma^6) + gamma * (1-gamma^5)) ) -
  #   clHead
  
  F <- F1^2 + F2^2
  
}


getPsPcEpsEpc_FMD_EQ <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, 
                                 ANew, params, depops){
  
  psNew <- PsM
  pcNew <- PcM
  
  # psNew <- psM_pre
  # pcNew <- pcM_pre
  # 
  # psNew_expected <- EpsM_pre
  # pcNew_expected <- EpcM_pre
  # 
  # SlNew <- slNewFMD
  # CLNew <- clNewFMD
  # ANew <- capAFMD
  # params = c(MUtilde_pre, Stilde_pre)
  
  # hc_new <- HcM
  # 
  # #### Here we make sure that the holding costs are below the cull cow price
  # while(hc_new > pcNew){
  #   hc_new <- hc_new - 0.01
  # }
  
  
  # if(depops == 20){
    
    # psNew <- psNew + 0.02
    
    psNew_lo <- psNew  - 0.3
    pcNew_lo <- pcNew - 0.3
  
    psNew_up <- psNew + 0.1
    pcNew_up <- pcNew + 0.1
    
    #### Here we are making sure the lower bound for the prices isn't negative
    if(psNew_lo < 0){
      psNew_lo <- psNew
    }
    
    if(pcNew_lo < 0){
      pcNew_lo <- pcNew
    }
    
    #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
    while( pcNew_lo > psNew_lo ){
      pcNew_lo <- pcNew_lo - 0.01
    }
    
    psNew_expected_lo <- psNew_expected - 0.1
    
    psNew_expected_up <- psNew_expected + 0.1
    
    pcNew_expected_lo <- pcNew_expected - 0.1
    
    pcNew_expected_up <- pcNew_expected + 0.1
    
    # while(pcNew_expected_lo < pcNew){
    #   pcNew_expected_lo <- pcNew_expected_lo + 0.05
    # }
    # 
    # while(psNew_expected_lo < psNew){
    #   psNew_expected_lo <- psNew_expected_lo + 0.05
    # }
    # 
    # while(psNew_expected_up < psNew){
    #   psNew_expected_up <- psNew_expected_up + 0.05
    # }
    # 
    # while(pcNew_expected_up < pcNew){
    #   pcNew_expected_up <- pcNew_expected_up + 0.05
    # }
    
    
  # }else if(depops == 50){
  #   
  #   
  #   psNew_lo <- psNew  - 0.2
  #   pcNew_lo <- pcNew - 0.4
  #   
  #   psNew_up <- psNew + 0.1
  #   pcNew_up <- pcNew + 0.08
  #   
  #   # psNew_expected <- psNew_expected + 0.0125
  #   
  #   psNew_expected_lo <- psNew_expected - 0.1
  #   
  #   psNew_expected_up <- psNew_expected + 0.25
  #   
  #   pcNew_expected_lo <- pcNew_expected 
  #   
  #   pcNew_expected_up <- pcNew_expected + 0.15
  #   
  # }else if (depops == 90){
  #   
  #   psNew_lo <- psNew  - 0.2
  #   pcNew_lo <- pcNew - 0.4
  #   
  #   psNew_up <- psNew + 0.1
  #   pcNew_up <- pcNew + 0.08
  #   
  #   # psNew_expected <- psNew_expected + 0.001
  #   
  #   psNew_expected_lo <- psNew_expected - 0.1
  #   
  #   psNew_expected_up <- psNew_expected + 0.25
  #   
  #   pcNew_expected_lo <- pcNew_expected - 0.1
  #   
  #   pcNew_expected_up <- pcNew_expected + 0.15
  # }
  
  
    hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
      (beta * pcNew_expected + g * (beta^3) * psNew_expected - pcNew)
    
    #### Here we make sure that the holding costs are below the cull cow price
    while(hc_new > pcNew){
      hc_new <- hc_new - 0.1
    }
    
    hc_new_lo <- hc_new - 0.1
    hc_new_up <- hc_new + 0.1
      
    while(hc_new_lo<0){
      hc_new_lo <- hc_new_lo + 0.1
    }
      
    while(hc_new_up > pcNew_up){
      hc_new_up <- hc_new_up - 0.1
    }
      
    hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
    B <- psNew - g * (beta^3) * psNew_expected + hc_discounted
      
    p <- c(psNew, pcNew, psNew_expected, pcNew_expected)
      
    lo <- c(psNew_lo, pcNew_lo, psNew_expected_lo, pcNew_expected_lo)
    up <- c(psNew_up, pcNew_up, psNew_expected_up, pcNew_expected_up)
    
    estPNew <- BBoptim(par = p, fn = estPFunction_FMD, sl = SlNew, cl = ClNew, A = ANew,
                       B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                       tilde_MU = params[1], tilde_s = params[2], hc_new = hc_new)
    
    ps1N <- estPNew$par[1]
    pc1N <- estPNew$par[2]
    ps_expected1N <- estPNew$par[3]
    pc_expected1N <- estPNew$par[4]
    
    hc1N <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
      (beta * pc_expected1N + g * (beta^3) * ps_expected1N - pc1N)
    
    return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}


##### Previous work
# Study 1. Inventories and prices of year 2000 (Invasive Species Management: Foot-and-Mouth Disease in the U.S. Beef Industry)

# Study 2. The losses are reported with 2006 baseline (Economic Impacts of Potential Foot and Mouth Disease Agroterrorism in
# the USA: A General Equilibrium Analysis)

# Study 3. The percent changes in the endogenous variables are then applied to a
# baseline defined by the observed data for the first quarter of 2009 through the fourth quarter of
# 2018 of no-disease. (Economic Assessment of FMDv Releases from
# the National Bio and Agro Defense Facility)

#### I think I will use 3 to compare the results. Need to figure out a way to compare the other work as well. 

##### FMD SIMULATIONS


# I get historical maximum and minimum supplies. These are later utilized to determine the imports required to reach the 
# subsistence levels of supply. 

slHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% max()
clHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% max()

slHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% min()
clHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% min()

#### For simplicity I take the 2008 exports as the exports in 2009 and so on
exports_2008 <- cattle_tot %>% select(Year, Exports) %>% filter(Year == 2008) 
rownames(exports_2008) <- seq(1,nrow(exports_2008))
exports_2008$Year <- as.numeric(exports_2008$Year)
exports_2008 <- exports_2008$Exports

Stock_2008L <- Stock %>% filter(Year < 2009)
Stock_2009 <- Stock %>% filter(Year == 2009)

#### Function returning the data with depopulated inventory
dePop <- function(stock, dePopRate){
  stock[,-1] <- stock[,-1] - stock[,-1] * (dePopRate/100)
  return(stock)
}

#### The equilibrium parameters are retrieved and arranged for the simulation
EQ_muTildes <- mu_Tildes_MMNII
EQ_sTildes <- s_Tildes_MMNII
EQ_demandShocks <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock)

EQ_PricesCosts <- Reduce(function(...) merge(...), 
                         list(EQestPSNII,EQestPCNII,EQestHCNII, EQestEPSNII, EQestEPCNII))

EQ_Supplies <- Reduce(function(...) merge(...), 
                      list(EQestObsSLNII %>% select(-errMean, -errmedian),EQestObsCLNII%>% select(-errMean, -errmedian)))

#### Arranging the data
EQ_K_t <- Stock %>% transmute(Year = Year, K = K)
EQ_A <- A_quant

FMD_AllDF_EQ <- Reduce(function(...) merge(...), 
                       list(EQ_K_t, EQ_A, proj_adjFac, EQ_muTildes, EQ_sTildes, EQ_PricesCosts, 
                            EQ_Supplies,dressedWeights_sl_cl, EQ_demandShocks)) %>% round(2) 



################################################################################################################################
######################################################## OPTIMISTIC SCENARIO ###################################################
################################################################################################################################

simOptimisticFMD <- function(calf_cropF, dePopR, modelParamsEQ_PreFMD, exports_preFMD, nn, Stock){
  
  # I depopulate the calf-crop. This will be appended in the simulation. Note: Every year the cattle give birth to young calves
  # so I must keep track of them.
  calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year <= 2009)
  calf_crop_PostFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR) %>% as.data.frame()
  
  ### I get the model parameters from the year of the disease outbreak
  modelParamsEQ_PreFMD <- FMD_AllDF_EQ %>% filter(Year == 2009)
  
  slaughterAvg_pre <- modelParamsEQ_PreFMD$Slaughter_avg
  cullAvg_pre <-  modelParamsEQ_PreFMD$Cull_avg
  
  MUtilde_pre <- modelParamsEQ_PreFMD$muMedian
  Stilde_pre <- modelParamsEQ_PreFMD$sMedian
  
  psM_pre <- modelParamsEQ_PreFMD$psMedian
  pcM_pre <- modelParamsEQ_PreFMD$pcMedian
  hcM_pre <- modelParamsEQ_PreFMD$hcMedian
  
  EpsM_pre <- modelParamsEQ_PreFMD$EpsMedian
  EpcM_pre <- modelParamsEQ_PreFMD$EpcMedian
  
  capA_pre <- modelParamsEQ_PreFMD$A
  capK_pre <- modelParamsEQ_PreFMD$K
  
  adjF_pre <- modelParamsEQ_PreFMD$AdjFactor
  
  ### Here I get the exports form the year 2008 and use them as the future year exports.
  ### Note: We are being conservative with this assumption. 
  ### Basically this will be used as out export market loss
  exports_2008 <- exports %>% filter(Year == 2008) %>% select(Exports) %>% as.numeric()
  exports_2008_meat <- exports_2008 * (slaughterAvg_pre/1000000000)
  exports_2008_meat <- round(exports_2008_meat,1)
  exports_percent <- round((exports_2008_meat/capA_pre) * 100,3)
  
  capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
  exports_percentK <- round((exports_2008_meat/capK_pre_meat) * 100,3)
  ### After careful consideration I am using exports_percentK as the exports in the simulation
  
  beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn+1), K = numeric(nn+1), k3 = numeric(nn+1))
  
  beefINV_FORECAST_PostFMD$Year <- seq(from=2010, to=2010+nn)
  
  
  proj_Q_P_PostFMD <- data.frame(Year = numeric(nn), Ps = numeric(nn), Pc = numeric(nn), 
                                 EPs = numeric(nn), EPc = numeric(nn), Hc = numeric(nn), 
                                 Sl = numeric(nn), Cl = numeric(nn), A = numeric(nn),
                                 repHeif = numeric(nn), repHeif_Head = numeric(nn),
                                 boundCond = numeric(nn), repHeif_HeadOG = numeric(nn),
                                 Sl_Head_OG = numeric(nn), Cl_Head_OG = numeric(nn),
                                 Sl_Head_EQ = numeric(nn), Cl_Head_EQ = numeric(nn))
  
  k0s_PostFMD <- data.frame(Year = numeric(nn), k02 = numeric(nn), k03 = numeric(nn), 
                            k04 = numeric(nn), k05 = numeric(nn), k06 = numeric(nn), 
                            k07 = numeric(nn), k08 = numeric(nn))
  
  k0s_PostFMD[1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[1,], 
                                        beefINV_FORECAST = beefINV_FORECAST_PostFMD[1,], 
                                        calfCrop = calf_crop_PostFMD)
  
  demandFMD <- quantities_prices_capK %>% filter(Year > 2009) %>% select(Year, A)
  demandShocksFMD <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock) %>% 
    filter(Year > 2009)
  
  capK_k3_depopFMD <- dePop(stock = Stock, dePopRate = dePopR) %>% select(Year, K, k3) %>% filter(Year <= 2010)
  
  # capK_k_depopFMD <- dePop(stock = Stock, dePopRate = dePopR) %>% select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10) %>% filter(Year <= 2010)
  
  slHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% max()
  clHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% max()
  
  slHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% min()
  clHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% min()
  
  
  psM_EqMed <- NULL
  pcM_EqMed <- NULL
  psM_EqMN <- NULL
  pcM_EqMN <- NULL
  
  K1 <- NULL
  clHeadDiff <- NULL
  slHeadDiff <- NULL
  headRatio <- NULL
  
  
  
  for(i in 1: nrow(proj_Q_P_PostFMD)){
    
    capA_pre <- modelParamsEQ_PreFMD$A
    
    if(i==1){
      capK_pre <- capK_k3_depopFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-1) %>% select(K) %>% as.numeric()
    }else{
      capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
    }
    
    #### Changes in the demand for beef.
    #### Domestic decline is subtracted from the demand. 
    #### The exports are added to the demand. This is because this meat was supposed to leave the country 
    #### but because of the loss of access to export market, this stays home. 
    
    if(i == 1){
      capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
    }else if(i == 2){
      capA_pre <- capA_pre + capA_pre * (exports_percentK/100)
    }else{
      capA_pre <- capA_pre
    }
    
    ### Here we retrieve the stock two years ago. We use this to get the adult heifers that are born to them.
    if(i <= 3){
      matureYoungs <- capK_k3_depopFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(K) %>% as.numeric()
    } else{
      matureYoungs <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(K) %>% as.numeric()
    }
    
    ### Here we add the adult heifers that are born two years ago to the stock. 
    ### This is where we have to decide the replacemenet heifers.
    capK_pre <- capK_pre + 0.5 * g * matureYoungs
    
    K1[i] <- capK_pre
    
    k <- 0
    k0s <- k0s_PostFMD[i,-1]
    int_k3 <- 0
    
    slDemand <- capA_pre *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    clDemand <- capA_pre * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    ##### Here I have to use the storage model type specification to get 
    ##### the approximation of the supplies. 
    ##### For this I must have the age distribution and also keep track of it. 
    
    Qs <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                               k = k, CapA = capA_pre, gamma_k3 = gamma_k3,
                               eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                               slAvg = slaughterAvg_pre, clAvg = cullAvg_pre, slDem = slDemand, clDem = clDemand)
    slNew <- Qs[1]
    clNew <- Qs[2]
    
    k_old <- Qs[4]
    
    k_old_Head <- Qs[5]
    
    k_old_Head_OG <- Qs[6]
    slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
    clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
    
    ANew1 <- capA_pre
    
    slCounterL <- 0
    clCounterL <- 0
    
    slCounterH <- 0
    clCounterH <- 0
    
    while(clNew < clHistMin){
      clNew <- ((clNew * 1000000000)/cullAvg_pre + 10000) * (cullAvg_pre/1000000000)
      clCounterL <- 1
    }
    
    while(slNew < slHistMin){
      slNew <- ((slNew * 1000000000)/slaughterAvg_pre + 10000) * (slaughterAvg_pre/1000000000)
      slCounterL <- 1
    }
    
    while(clNew > clHistMax){
      clNew <- ((clNew * 1000000000)/cullAvg_pre - 10000) * (cullAvg_pre/1000000000)
      clCounterH <- 1
    }
    
    while(slNew > slHistMax){
      slNew <- ((slNew * 1000000000)/slaughterAvg_pre - 10000) * (slaughterAvg_pre/1000000000)
      slCounterH <- 1
    }
    
    #### Also add condition where the supply is greater than the historical supply. In that case remove 10000 animals from 
    #### each category and then remove the same number of animals from relacement heifers and the stock respectively
    #### Tell them these will be exports. Since I am not including exports or imports, the addition and subtraction of 
    #### animals should be considered as exports and imports. These must be price driven.
    
    if(slCounterL == 1){
      
      slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
      
      if(k_old_Head_OG<=0){
        k_old_Head <- slHeadDiff
      }else{
        k_old_Head <- k_old_Head + slHeadDiff
      }
      
      EpsM_pre <- EpsM_pre + 0.01
    }
    
    
    if(clCounterL == 1){
      
      clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
     
      K1[i] <- K1[i] + clHeadDiff
      
      EpcM_pre <- EpcM_pre + 0.01
    }
    
    
    if(slCounterH == 1){
      
      slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
      
      if(k_old_Head_OG<=0){
        k_old_Head <- slHeadDiff
      }else{
        if((k_old_Head - slHeadDiff)>0){
          k_old_Head <- k_old_Head - slHeadDiff
        }
      }
      
      EpsM_pre <- EpsM_pre - 0.01
    }
    
    if(clCounterH == 1){
      
      clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
      
      if((K1[i] - clHeadDiff)>0){
        K1[i] <- K1[i] - clHeadDiff
      }
      
      EpcM_pre <- EpcM_pre - 0.01
    }
    
    
    if(i>=3){
      psM_pre <- psM_pre + 0.025
      pcM_pre <- pcM_pre - 0.01
    }
    
    Ps <- getPsPcEpsEpc_FMD_EQ(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                               HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                               params = c(MUtilde_pre, Stilde_pre),depops = dePopR)
    
    psM_pre <- Ps[1]
    pcM_pre <- Ps[2]
    hcM_pre <- Ps[3]
    EpsM_pre <- Ps[4]
    EpcM_pre <- Ps[5]
    
    D_sl1 <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    D_cl1 <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    slDiff <- slNew - D_sl1
    clDiff <- clNew - D_cl1
    
    
    if(k_old_Head_OG<=0){
      fedTBA <- k_old_Head
    }else{
      fedTBA <- k_old_Head
      if((K1[i] - fedTBA)>0){
        K1[i] <- K1[i] - fedTBA
      }
    }
    
    k3Median <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
    k3Median <- mean(k3Median$k3)
    
    while(fedTBA > k3Median){
      fedTBA <- fedTBA - 1000
    }
    
    Kmedian <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
    Kmedian <- mean(Kmedian$K)
    
    while(K1[i] > Kmedian){
      K1[i] <- K1[i] - 1000
    }
    
    repNewLbs <- fedTBA * (slaughterAvg_pre/1000000000)
    repNewHead <- fedTBA
    
    beefINV_FORECAST_PostFMD$K[i] <- K1[i]
    
    proj_Q_P_PostFMD$Ps[i] <- psM_pre
    proj_Q_P_PostFMD$Pc[i] <- pcM_pre
    proj_Q_P_PostFMD$Hc[i] <- hcM_pre
    proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
    proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
    
    proj_Q_P_PostFMD$Sl[i] <- slNew
    proj_Q_P_PostFMD$Cl[i] <- clNew
    proj_Q_P_PostFMD$A[i] <- ANew1
    proj_Q_P_PostFMD$repHeif[i] <- repNewLbs
    proj_Q_P_PostFMD$repHeif_Head[i] <- repNewHead
    
    proj_Q_P_PostFMD$boundCond[i] <- abs(repNewHead) <= 0.5 * g * K1[i]
    
    proj_Q_P_PostFMD$repHeif_HeadOG[i] <- k_old_Head_OG
    proj_Q_P_PostFMD$Sl_Head_OG[i] <- slNew_Head_OG
    proj_Q_P_PostFMD$Cl_Head_OG[i] <- clNew_Head_OG
    
    proj_Q_P_PostFMD$Sl_Head_EQ[i] <- ( slNew * 1000000000 )/slaughterAvg_pre
    proj_Q_P_PostFMD$Cl_Head_EQ[i] <- ( clNew * 1000000000 )/cullAvg_pre
    
    proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
    
    beefINV_FORECAST_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i] 
    beefINV_FORECAST_PostFMD$k3[i+1] <-  abs(proj_Q_P_PostFMD$repHeif_Head[i])
    
    calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
                                                       k0 = g * beefINV_FORECAST_PostFMD$K[i])
    
    
  }
  
  return(list(proj_Q_P_PostFMD, beefINV_FORECAST_PostFMD, calf_crop_PostFMD))
  
}


optimisticPostFMD_20 <- simOptimisticFMD(calf_cropF = calf_crop, dePopR = 20, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                        exports_preFMD = exports_2008, nn = 10, Stock = Stock)

optimisticPostFMD_50 <- simOptimisticFMD(calf_cropF = calf_crop, dePopR = 50, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                         exports_preFMD = exports_2008, nn = 10, Stock = Stock)

optimisticPostFMD_90 <- simOptimisticFMD(calf_cropF = calf_crop, dePopR = 90, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                         exports_preFMD = exports_2008, nn = 10, Stock = Stock)

postFMD_P_Q_20_Opt <- optimisticPostFMD_20[[1]]
postFMD_K_20_Opt <- optimisticPostFMD_20[[2]]
postFMD_CC_20_Opt <- optimisticPostFMD_20[[3]]

postFMD_P_Q_50_Opt <- optimisticPostFMD_50[[1]]
postFMD_K_50_Opt <- optimisticPostFMD_50[[2]]
postFMD_CC_50_Opt <- optimisticPostFMD_50[[3]]

postFMD_P_Q_90_Opt <- optimisticPostFMD_90[[1]]
postFMD_K_90_Opt <- optimisticPostFMD_90[[2]]
postFMD_CC_90_Opt <- optimisticPostFMD_90[[3]]

# optBKP <- list(optimisticPostFMD_20, optimisticPostFMD_50, optimisticPostFMD_90)


simPessimisticFMD<- function(calf_cropF, dePopR,modelParamsEQ_PreFMD, exports_preFMD, nn, Stock){
  
  # I depopulate the calf-crop. This will be appended in the simulation. Note: Every year the cattle give birth to young calves
  # so I must keep track of them.
  calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year <= 2009)
  calf_crop_PostFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR) %>% as.data.frame()
  
  ### I get the model parameters from the year of the disease outbreak
  modelParamsEQ_PreFMD <- FMD_AllDF_EQ %>% filter(Year == 2009)
  
  slaughterAvg_pre <- modelParamsEQ_PreFMD$Slaughter_avg
  cullAvg_pre <-  modelParamsEQ_PreFMD$Cull_avg
  
  MUtilde_pre <- modelParamsEQ_PreFMD$muMedian
  Stilde_pre <- modelParamsEQ_PreFMD$sMedian
  
  psM_pre <- modelParamsEQ_PreFMD$psMedian
  pcM_pre <- modelParamsEQ_PreFMD$pcMedian
  hcM_pre <- modelParamsEQ_PreFMD$hcMedian
  
  EpsM_pre <- modelParamsEQ_PreFMD$EpsMedian
  EpcM_pre <- modelParamsEQ_PreFMD$EpcMedian
  
  capA_pre <- modelParamsEQ_PreFMD$A
  capK_pre <- modelParamsEQ_PreFMD$K
  
  adjF_pre <- modelParamsEQ_PreFMD$AdjFactor
  
  ### Here I get the exports form the year 2008 and use them as the future year exports.
  ### Note: We are being conservative with this assumption. 
  ### Basically this will be used as out export market loss
  exports_2008 <- exports %>% filter(Year == 2008) %>% select(Exports) %>% as.numeric()
  exports_2008_meat <- exports_2008 * (slaughterAvg_pre/1000000000)
  exports_2008_meat <- round(exports_2008_meat,1)
  exports_percent <- round((exports_2008_meat/capA_pre) * 100,3)
  
  capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
  exports_percentK <- round((exports_2008_meat/capK_pre_meat) * 100,3)
  ### After careful consideration I am using exports_percentK as the exports in the simulation
  
  beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn+1), K = numeric(nn+1), k3 = numeric(nn+1))
  
  beefINV_FORECAST_PostFMD$Year <- seq(from=2010, to=2010+nn)
  
  
  proj_Q_P_PostFMD <- data.frame(Year = numeric(nn), Ps = numeric(nn), Pc = numeric(nn), 
                                 EPs = numeric(nn), EPc = numeric(nn), Hc = numeric(nn), 
                                 Sl = numeric(nn), Cl = numeric(nn), A = numeric(nn),
                                 repHeif = numeric(nn), repHeif_Head = numeric(nn),
                                 boundCond = numeric(nn), repHeif_HeadOG = numeric(nn),
                                 Sl_Head_OG = numeric(nn), Cl_Head_OG = numeric(nn),
                                 Sl_Head_EQ = numeric(nn), Cl_Head_EQ = numeric(nn))
  
  k0s_PostFMD <- data.frame(Year = numeric(nn), k02 = numeric(nn), k03 = numeric(nn), 
                            k04 = numeric(nn), k05 = numeric(nn), k06 = numeric(nn), 
                            k07 = numeric(nn), k08 = numeric(nn))
  
  k0s_PostFMD[1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[1,], 
                                        beefINV_FORECAST = beefINV_FORECAST_PostFMD[1,], 
                                        calfCrop = calf_crop_PostFMD)
  
  demandFMD <- quantities_prices_capK %>% filter(Year > 2009) %>% select(Year, A)
  demandShocksFMD <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock) %>% 
    filter(Year > 2009)
  
  capK_k3_depopFMD <- dePop(stock = Stock, dePopRate = dePopR) %>% select(Year, K, k3) %>% filter(Year <= 2010)
  
  slHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% max()
  clHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% max()
  
  slHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% min()
  clHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% min()
  
  
  psM_EqMed <- NULL
  pcM_EqMed <- NULL
  psM_EqMN <- NULL
  pcM_EqMN <- NULL
  
  K1 <- NULL
  clHeadDiff <- NULL
  slHeadDiff <- NULL
  headRatio <- NULL
  
  
  
  for(i in 1: nrow(proj_Q_P_PostFMD)){
    
    capA_pre <- modelParamsEQ_PreFMD$A
    
    if(i==1){
      capK_pre <- capK_k3_depopFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-1) %>% select(K) %>% as.numeric()
    }else{
      capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
    }
    
    #### Changes in the demand for beef.
    #### Domestic decline is subtracted from the demand. 
    #### The exports are added to the demand. This is because this meat was supposed to leave the country 
    #### but because of the loss of access to export market, this stays home. 
    
    if( i < 4){
      capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
    }else if( i >= 4 && i <= 5){
      capA_pre <- capA_pre + capA_pre * (exports_percentK/100)
    }else{
      capA_pre <- capA_pre
    }
    
    ### Here we retrieve the stock two years ago. We use this to get the adult heifers that are born to them.
    if(i <= 3){
      matureYoungs <- capK_k3_depopFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(K) %>% as.numeric()
    } else{
      matureYoungs <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(K) %>% as.numeric()
    }
    
    ### Here we add the adult heifers that are born two years ago to the stock. 
    ### This is where we have to decide the replacemenet heifers.
    capK_pre <- capK_pre + 0.5 * g * matureYoungs
    
    K1[i] <- capK_pre
    
    k <- 0
    k0s <- k0s_PostFMD[i,-1]
    int_k3 <- 0
    
    slDemand <- capA_pre *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    clDemand <- capA_pre * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    Qs <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                               k = k, CapA = capA_pre, gamma_k3 = gamma_k3,
                               eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                               slAvg = slaughterAvg_pre, clAvg = cullAvg_pre, slDem = slDemand, clDem = clDemand)
    slNew <- Qs[1]
    clNew <- Qs[2]
    
    k_old <- Qs[4]
    
    k_old_Head <- Qs[5]
    
    k_old_Head_OG <- Qs[6]
    slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
    clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
    
    ANew1 <- capA_pre
    
    slCounterL <- 0
    clCounterL <- 0
    
    slCounterH <- 0
    clCounterH <- 0
    
    while(clNew < clHistMin){
      clNew <- ((clNew * 1000000000)/cullAvg_pre + 10000) * (cullAvg_pre/1000000000)
      clCounterL <- 1
    }
    
    while(slNew < slHistMin){
      slNew <- ((slNew * 1000000000)/slaughterAvg_pre + 10000) * (slaughterAvg_pre/1000000000)
      slCounterL <- 1
    }
    
    while(clNew > clHistMax){
      clNew <- ((clNew * 1000000000)/cullAvg_pre - 10000) * (cullAvg_pre/1000000000)
      clCounterH <- 1
    }
    
    while(slNew > slHistMax){
      slNew <- ((slNew * 1000000000)/slaughterAvg_pre - 10000) * (slaughterAvg_pre/1000000000)
      slCounterH <- 1
    }
    
    #### Also add condition where the supply is greater than the historical supply. In that case remove 10000 animals from 
    #### each category and then remove the same number of animals from relacement heifers and the stock respectively
    #### Tell them these will be exports. Since I am not including exports or imports, the addition and subtraction of 
    #### animals should be considered as exports and imports. These must be price driven.
    
    if(slCounterL == 1){
      
      slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
      
      if(k_old_Head_OG<=0){
        k_old_Head <- slHeadDiff
      }else{
        k_old_Head <- k_old_Head + slHeadDiff
      }
      
      EpsM_pre <- EpsM_pre + 0.01
    }
    
    
    if(clCounterL == 1){
      
      clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
      
      K1[i] <- K1[i] + clHeadDiff
      
      EpcM_pre <- EpcM_pre + 0.01
    }
    
    
    if(slCounterH == 1){
      
      slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
      
      if(k_old_Head_OG<=0){
        k_old_Head <- slHeadDiff
      }else{
        if((k_old_Head - slHeadDiff)>0){
          k_old_Head <- k_old_Head - slHeadDiff
        }
      }
      
      EpsM_pre <- EpsM_pre - 0.01
    }
    
    if(clCounterH == 1){
      
      clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
      
      if((K1[i] - clHeadDiff)>0){
        K1[i] <- K1[i] - clHeadDiff
      }
      
      EpcM_pre <- EpcM_pre - 0.01
    }
    
    
    if(i>5){
      psM_pre <- psM_pre + 0.025
      pcM_pre <- pcM_pre - 0.01
    }
    
    Ps <- getPsPcEpsEpc_FMD_EQ(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                               HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                               params = c(MUtilde_pre, Stilde_pre),depops = dePopR)
    
    psM_pre <- Ps[1]
    pcM_pre <- Ps[2]
    hcM_pre <- Ps[3]
    EpsM_pre <- Ps[4]
    EpcM_pre <- Ps[5]
    
    D_sl1 <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    D_cl1 <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    slDiff <- slNew - D_sl1
    clDiff <- clNew - D_cl1
    
    
    if(k_old_Head_OG<=0){
      fedTBA <- k_old_Head
    }else{
      fedTBA <- k_old_Head
      if((K1[i] - fedTBA)>0){
        K1[i] <- K1[i] - fedTBA
      }
    }
    
    k3Median <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
    k3Median <- mean(k3Median$k3)
    
    while(fedTBA > k3Median){
      fedTBA <- fedTBA - 1000
    }
    
    Kmedian <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
    Kmedian <- mean(Kmedian$K)
    
    while(K1[i] > Kmedian){
      K1[i] <- K1[i] - 1000
    }
    
    repNewLbs <- fedTBA * (slaughterAvg_pre/1000000000)
    repNewHead <- fedTBA
    
    beefINV_FORECAST_PostFMD$K[i] <- K1[i]
    
    proj_Q_P_PostFMD$Ps[i] <- psM_pre
    proj_Q_P_PostFMD$Pc[i] <- pcM_pre
    proj_Q_P_PostFMD$Hc[i] <- hcM_pre
    proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
    proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
    
    proj_Q_P_PostFMD$Sl[i] <- slNew
    proj_Q_P_PostFMD$Cl[i] <- clNew
    proj_Q_P_PostFMD$A[i] <- ANew1
    proj_Q_P_PostFMD$repHeif[i] <- repNewLbs
    proj_Q_P_PostFMD$repHeif_Head[i] <- repNewHead
    
    proj_Q_P_PostFMD$boundCond[i] <- abs(repNewHead) <= 0.5 * g * K1[i]
    
    proj_Q_P_PostFMD$repHeif_HeadOG[i] <- k_old_Head_OG
    proj_Q_P_PostFMD$Sl_Head_OG[i] <- slNew_Head_OG
    proj_Q_P_PostFMD$Cl_Head_OG[i] <- clNew_Head_OG
    
    proj_Q_P_PostFMD$Sl_Head_EQ[i] <- ( slNew * 1000000000 )/slaughterAvg_pre
    proj_Q_P_PostFMD$Cl_Head_EQ[i] <- ( clNew * 1000000000 )/cullAvg_pre
    
    proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
    
    beefINV_FORECAST_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i] 
    beefINV_FORECAST_PostFMD$k3[i+1] <-  abs(proj_Q_P_PostFMD$repHeif_Head[i])
    
    calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
                                                       k0 = g * beefINV_FORECAST_PostFMD$K[i])
    
    
  }
  
  return(list(proj_Q_P_PostFMD, beefINV_FORECAST_PostFMD, calf_crop_PostFMD))
  
}



pessimisticPostFMD_20 <- simPessimisticFMD(calf_cropF = calf_crop, dePopR = 20, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                         exports_preFMD = exports_2008, nn = 10, Stock = Stock)

pessimisticPostFMD_50 <- simPessimisticFMD(calf_cropF = calf_crop, dePopR = 50, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                         exports_preFMD = exports_2008, nn = 10, Stock = Stock)

pessimisticPostFMD_90 <- simPessimisticFMD(calf_cropF = calf_crop, dePopR = 90, modelParamsEQ_PreFMD = proj_AllDF_EQ,
                                         exports_preFMD = exports_2008, nn = 10, Stock = Stock)


postFMD_P_Q_20_Pes <- pessimisticPostFMD_20[[1]]
postFMD_K_20_Pes <- pessimisticPostFMD_20[[2]]
postFMD_CC_20_Pes <- pessimisticPostFMD_20[[3]]

postFMD_P_Q_50_Pes <- pessimisticPostFMD_50[[1]]
postFMD_K_50_Pes <- pessimisticPostFMD_50[[2]]
postFMD_CC_50_Pes <- pessimisticPostFMD_50[[3]]

postFMD_P_Q_90_Pes <- pessimisticPostFMD_90[[1]]
postFMD_K_90_Pes <- pessimisticPostFMD_90[[2]]
postFMD_CC_90_Pes <- pessimisticPostFMD_90[[3]]




############################################################################################################################################################################################################################################################

EQ_muTildes <- mu_Tildes_MMNII
EQ_sTildes <- s_Tildes_MMNII
EQ_demandShocks <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock)

EQ_PricesCosts <- Reduce(function(...) merge(...), 
                           list(EQestPSNII,EQestPCNII,EQestHCNII, EQestEPSNII, EQestEPCNII))

EQ_Supplies <- Reduce(function(...) merge(...), 
                      list(EQestObsSLNII %>% select(-errMean, -errmedian),EQestObsCLNII%>% select(-errMean, -errmedian)))

#### Arranging the data
EQ_K_t <- Stock %>% transmute(Year = Year, K = K)
EQ_A <- A_quant

FMD_AllDF_EQ <- Reduce(function(...) merge(...), 
                        list(EQ_K_t, EQ_A, proj_adjFac, EQ_muTildes, EQ_sTildes, EQ_PricesCosts, 
                             EQ_Supplies,dressedWeights_sl_cl, EQ_demandShocks)) %>% round(2) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

##### Beef exports data
beefExports <- read_excel("Data/Meat-BeefVeal-Exports/Meat-BeefVeal-Exports.xlsx") %>% as.data.frame()
beefExports <- beefExports %>% select(-`Unit Description`, -Commodity, -Country)

exportsBeef <- data.frame(t(beefExports[-1]))
colnames(exportsBeef) <- beefExports[,1]
exportsBeef <- exportsBeef %>% mutate(Year = c(rownames(exportsBeef)))
exportsBeef <- exportsBeef %>% select(Year,everything())
rownames(exportsBeef) <- seq(1, nrow(exportsBeef))
########## Changing the units (the original units are in 1000 MT)
exportsBeef[,-1] <- (exportsBeef[,-1] * 1000 * (2204.68))

########## Now converting meat to billion pounds ############
exportsBeef[,-1] <- exportsBeef[,-1]/1000000000

# 5%, 10%, and 20%.

##### Now I have calf-crop until 2009
##### Now I have calf-crop until 2009
dePopR <- 5
calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year <= 2009)
calf_crop_PostFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR) %>% as.data.frame()

#### Checking the calf crop and K data inconsistencies
stock_PreFMD <- Stock %>% select(Year, K, k3) %>% filter(Year <= 2009)

stock_CC_PreFMD <- merge(stock_PreFMD, merge(calf_crop_PreFMD,modelParamsEQ_PreFMD))

calf_crop_PostFMD <- calf_crop_PreFMD %>% tail(10)

#### Here I get all the equilibrium estimates

modelParamsEQ_PreFMD <- FMD_AllDF_EQ %>% filter(Year <= 2009)

slaughterAvg_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$Slaughter_avg)
cullAvg_pre <-  mean(tail(modelParamsEQ_PreFMD, n=1)$Cull_avg)

MUtilde_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$muMedian)
Stilde_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$sMedian)

slSM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$slMedian)
clSM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$clMedian)

psM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$psMedian)
pcM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$pcMedian)
hcM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$hcMedian)

EpsM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$EpsMedian)
EpcM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$EpcMedian)

capA_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$A)
capK_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$K)

adjF_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$AdjFactor)

slShock2009 <- mean(tail(allStockShocks %>% filter(Year<=2009),n=1)$slShock)
clShock2009 <- mean(tail(allStockShocks %>% filter(Year<=2009),n=1)$clShock)

slShock2009 <- 1
clShock2009 <- 1

#### I convert exported live animals from number of head to pounds in meat
exports_2009_Live <- exports %>% filter(Year == 2009) %>% select(Exports) %>% as.numeric()
exports_2009_LiveMeat <- exports_2009_Live * (slaughterAvg_pre/1000000000)
#### Here I get the exported meat and add this and the above
exportsBeef_2009 <- exportsBeef %>% filter(Year == 2009) %>% select(Exports) %>% as.numeric()
totalBeefExportsMeat_2009 <- round(exports_2009_LiveMeat + exportsBeef_2009, 3)
# beefMeatExports_percent <- round((totalBeefExportsMeat_2009/capA_pre) * 100,3)
capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
exports_percentK <- round((totalBeefExportsMeat_2009/capK_pre_meat) * 100,3)

nn <- 10
beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn), K = NA, k3 = NA,
                                        k4 =  NA, k5 =  NA, k6 =  NA, 
                                        k7 =  NA, k8 =  NA, k9 =  NA)

beefINV_FORECAST_PostFMD$Year <- seq(from=2010, to=2010+nn-1)

# beefINV_FORECAST_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)
# beefINV_FORECAST_PostFMD[2,] <- dePop(stock = Stock %>% filter(Year == 2011), dePopRate = dePopR)

proj_Q_P_PostFMD <- data.frame(Year = numeric(nn), Ps = numeric(nn), Pc = numeric(nn), 
                               EPs = numeric(nn), EPc = numeric(nn), Hc = numeric(nn), 
                               Sl = numeric(nn), Cl = numeric(nn), A = numeric(nn),
                               repHeif = numeric(nn), repHeif_Head = numeric(nn),
                               boundCond = numeric(nn), repHeif_HeadOG = numeric(nn),
                               Sl_Head_OG = numeric(nn), Cl_Head_OG = numeric(nn),
                               Sl_Head_EQ = numeric(nn), Cl_Head_EQ = numeric(nn),
                               muTilde = numeric(nn), sTilde = numeric(nn), sh = numeric(nn))

k0s_PostFMD <- data.frame(Year = numeric(nn), k02 = numeric(nn), k03 = numeric(nn), 
                          k04 = numeric(nn), k05 = numeric(nn), k06 = numeric(nn), 
                          k07 = numeric(nn), k08 = numeric(nn))

k0s_PostFMD[1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[1,], 
                                      beefINV_FORECAST = beefINV_FORECAST_PostFMD[1,], 
                                      calfCrop = calf_crop_PostFMD)


stockForecastFMD <- Stock %>% filter(Year < 2010)
mergedForecastFMD <- merge(stockForecastFMD, beefINV_FORECAST_PostFMD, all=TRUE) %>% filter(Year >= 1995)

# mergedForecastFMD$k3[mergedForecastFMD$Year==2010] <- mergedForecastFMD$k3[mergedForecastFMD$Year==2009]

quantitiesSLCLFMD <- quantities %>% select(Year, sl, cl) %>% filter(Year < 2010 & Year >= 1995)
wtAVGFMD <- allStockShocks %>% select(Year, Slaughter_avg, Cull_avg) %>% filter(Year < 2010 & Year >= 1995)
quantsWeightsFMD <- merge(quantitiesSLCLFMD, wtAVGFMD)

impExpFMD <- stocksImportsExports %>% select(Year, Imports, Exports) %>% filter(Year < 2010 & Year >= 1995)

quantsWeightsFMD <- merge(quantsWeightsFMD, impExpFMD, all=TRUE)

mergedForecastFMD <- merge(mergedForecastFMD, quantsWeightsFMD, all=TRUE)

mergedForecastFMD <- fill(mergedForecastFMD, Cull_avg, .direction = 'down')
mergedForecastFMD <- fill(mergedForecastFMD, Slaughter_avg, .direction = 'down')

# mergedForecastFMD$k3[mergedForecastFMD$Year==2010] <- g * mergedForecastFMD$K[mergedForecastFMD$Year==2008] - 
#   (mergedForecastFMD$sl[mergedForecastFMD$Year==2009] * (1000000000/mergedForecastFMD$Slaughter_avg[mergedForecastFMD$Year==2009]) - 
#      mergedForecastFMD$Imports[mergedForecastFMD$Year==2009] + mergedForecastFMD$Exports[mergedForecastFMD$Year==2009])

mergedForecastFMD <- mergedForecastFMD %>% mutate(k4 = delta * lag(k3), k5 = delta * lag(k4), k6 = delta * lag(k5),
                                                  k7 = delta * lag(k6), k8 = delta * lag(k7), 
                                                  k9 = if_else((K - (k3+k4+k5+k6+k7+k8)) < 0, 0, (K - (k3+k4+k5+k6+k7+k8))),
                                                  k10 = 0) %>% filter(Year > 2000)

#### Here I simply replace the NA's of k9 
# require(imputeTS)
# mergedForecastFMD <- mergedForecastFMD %>% mutate(k9 = 
#                                                     na_ma(k9, k = 8, weighting = "exponential", maxgap = Inf))

# mergedForecastFMD$K[mergedForecastFMD$Year==2010] <- mergedForecastFMD %>% filter(Year == 2010) %>% mutate(K= k3+k4+k5+k6+k7+k8+k9) %>% select(K) %>% as.numeric()

#  I will also have to depopulate the k0s as well
# k0s_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)

# k0s_PostFMD[2,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[2,],
#                                       beefINV_FORECAST = beefINV_FORECAST_PostFMD[2,], 
#                                       calfCrop = calf_crop_PostFMD)


##### Japan lifted it's ban on the importation of US beef nearly 2 years after BSE in the US
##### December 2005, Japan agreed to remove the restriction on importing US beef. However, in January imports stopped again because inspectors found banned cattle parts in a veal shipment from the U.S.
#
####### South Korea resumed U.S. beef imports in July 2008 

### China lifted it's ban in 2016

demandFMD <- quantities_prices_capK %>% filter(Year > 2009) %>% select(Year, A)
demandShocksFMD <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock) %>% 
  filter(Year > 2009)

capK_k3_depopFMD <- dePop(stock = Stock, dePopRate = dePopR) %>% select(Year, K, k3) %>% filter(Year <= 2010)

# I get historical maximum and minimum supplies

slHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% max()
clHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% max()

slHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(slSM) %>% min()
clHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2009) %>% select(clSM) %>% min()


psM_EqMed <- NULL
pcM_EqMed <- NULL
psM_EqMN <- NULL
pcM_EqMN <- NULL

K1 <- NULL
clHeadDiff <- NULL
slHeadDiff <- NULL
headRatio <- NULL

proj_Q_P_PostFMD$Year <- seq(beefINV_FORECAST_PostFMD$Year[1], beefINV_FORECAST_PostFMD$Year[nrow(beefINV_FORECAST_PostFMD)], 
                             by = 1)
mergedForecastFMD_Proj <- mergedForecastFMD %>% filter(Year >= beefINV_FORECAST_PostFMD$Year[1]-8)

#### Since I know the supply of fed cattle meat and cull cow meat in 2009, by using 2008 K I can get the replacement heifers 
#### in 2010. Note : This is done because we must know the approximate supply of both types of meat in 2010 to get the prices.
# mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year==2010] <- 
#   g * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year==2008] - 
#   (mergedForecastFMD_Proj$sl[mergedForecastFMD_Proj$Year==2009] * (1000000000/mergedForecastFMD_Proj$Slaughter_avg[mergedForecastFMD_Proj$Year==2009]) - 
#      mergedForecastFMD_Proj$Imports[mergedForecastFMD_Proj$Year==2009] + mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year==2009])  

# mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year==2010] <- mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year==2009]

slNewPre <- slSM_pre
clNewPre <- clSM_pre

params_mu_s_FMDProj <- optParamFunction_FMDProj(sl = slNewPre, cl = clNewPre, 
                                                 ps = psM_pre, pc = pcM_pre, thetas = c(1,1), adj = 1)

MUtilde_pre <- params_mu_s_FMDProj[1]
Stilde_pre <- params_mu_s_FMDProj[2]

for(i in 1: nrow(proj_Q_P_PostFMD)){
  
  i <- 3
  
  capAFMD <- capA_pre
  
  sh_pre <- ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
           (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  proj_Q_P_PostFMD$muTilde[i] <- MUtilde_pre
  proj_Q_P_PostFMD$sTilde[i] <- MUtilde_pre
  proj_Q_P_PostFMD$sh[i] <- sh_pre
  
  yearIFMD <- proj_Q_P_PostFMD$Year[i]
  
  ##### Here I construct the supply of cull cow meat
  k6nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k6)
  k7nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k7)
  k8nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k8)
  k9nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k9)
  clShnFMD <- 1
  cAvgFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Cull_avg)
  clmFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(cl)
  ## This is because for the past years k9 is zero. So I assume the k9s are zero for next years as well.
  clNewFMD <-  ((k9nFMD + (1-delta) * k8nFMD + (1-delta) * k7nFMD) * clShnFMD +
               (delta * (k8nFMD + k7nFMD + k6nFMD) - (k7nFMD + k8nFMD + k9nFMD)) )* (cAvgFMD/1000000000)
  clNewFMD <- as.numeric(clNewFMD)
  
  ##### Here I construct the supply of fed cattle meat
  slm1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>% select(sl)
  slShmFMD <- 1
  Km2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(K)
  Km3FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-4) %>% select(K)
  k9m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k9)
  k8m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k8)
  k7m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k7)
  fedAvgFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>%select(Slaughter_avg)
  ## This is because for the past years k9 is zero. So I assume the k9s are zero for next years as well.
  ## This might change if the imports are increased. I do not add imports in the analysis for now.
  slNewFMD <- ((g - 0.37 * g) * Km2FMD * slShmFMD +
              ((1 - 0.37 * g) * g * delta * (Km2FMD - (g - 0.37 * g) * Km3FMD -
                                               (k9m2FMD + (1-delta) * k8m2FMD + (1-delta) * k7m2FMD)))) * (fedAvgFMD/1000000000)
  slNewFMD <- as.numeric(slNewFMD)
  
  ANewFMD <- as.numeric((slNewFMD + clNewFMD))
  
  k <- 0
  
  K1[i] <- capK_pre
  
  slaughterAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Slaughter_avg))
  cullAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Cull_avg))
  
  #### These will be halted as soon as the disease outbreaks
  # imprtsFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Imports))
  # exprtsFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Exports))
  
  slDemFMD <- ANewFMD * sh_pre
  slDemHeadFMD <- slDemFMD * (1000000000/slaughterAvgFMD)
  
  k_old_headFMD <- g * K1[i] - slDemHeadFMD
  
  mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD] <- k_old_headFMD
  
  # Chad's suggestion: Use calf crop from 2009 and age distribution from 2009 to get a best estimate of K for 2010
  # So basically use mergedForecastFMD_Proj cap K to get the calf crop and then think creatively to get the K for 2010
  
  ccY1 <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>% 
    mutate(cc = g * K ) %>% select(cc) %>% as.numeric()
  
  clDemFMD <- ANewFMD * (1-sh_pre)
  clDemHeadFMD <- clDemFMD * (1000000000/cullAvgFMD)
  
  approxKFMD <- k_old_headFMD + ccY1 - clDemHeadFMD
  
  mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD] <- approxKFMD
  
  expectedValue_k9FMD <- beta * EpcM_pre + g * (beta^3) * EpsM_pre - (1+g*beta*(gamma0+beta*gamma1)) * hcM_pre
  
  # expectedValue_k9 <- round(expectedValue_k9,2)
  
  #If expectedValue_k9 is > pc then we have 9 year olds in the stock , else we cull all the 9 year olds.
  # This mean no more 10 year olds. See pages 35 and so on in dissertation
  if(round(expectedValue_k9FMD,2) > round(pcM_pre,2)){
    # We should have 9-year olds in the stock. All 10-years are culled.
    k9_OldFMD <- 1
  }else if(round(expectedValue_k9FMD,2) == round(pcM_pre,2)){
    # We should have 8-year olds in the stock. All 10-years and 9-years are culled
    k9_OldFMD <- 0
  } else if(round(expectedValue_k9FMD,2) < round(pcM_pre,2)){
    # We should have 7-year olds in the stock, All the 10,9,8 year old cows are culled
    k9_OldFMD <- 2
  }
  
  if(k9_OldFMD == 1){
    
    mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <- 
      mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>%
      mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9)) %>% select(k9) %>% as.numeric()
    
  }else if(k9_OldFMD == 0) {
    
    mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
    
  } else if(k9_OldFMD == 2){
    
    mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
    mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
    mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD] <- 
      mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% mutate(k7 = delta * k6) %>% select(k7) %>% as.numeric()
    
  }
  
  if(!is.integer(mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+1])){
    
    mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+1] <-
      delta * mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD]
    
    mergedForecastFMD_Proj$k5[mergedForecastFMD_Proj$Year == yearIFMD+2] <-
      delta * mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+1]
    
    mergedForecastFMD_Proj$k6[mergedForecastFMD_Proj$Year == yearIFMD+3] <-
      delta * mergedForecastFMD_Proj$k5[mergedForecastFMD_Proj$Year == yearIFMD+2]
    
    mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD+4] <-
      delta * mergedForecastFMD_Proj$k6[mergedForecastFMD_Proj$Year == yearIFMD+3]
    
    mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD+5] <-
      delta * mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD+4]
  }
  
  mergedForecastFMD_Proj$cl[mergedForecastFMD_Proj$Year == yearIFMD] <- clNewFMD
  mergedForecastFMD_Proj$sl[mergedForecastFMD_Proj$Year == yearIFMD] <- slNewFMD
  
  #### Changes in the beef quantity demanded. 
  #### Here I am considering only the decline in the domestic beef quantity demanded.
  
  #### Export markets
  #### For the export markets inaccessibility I am adding that meat into the supply 
  #### This is because there is more meat left in the country, i.e., more supply.
  
  if(i == 1){
    # i < 4
    # i == 1
    # capAFMD <- capAFMD - capAFMD * (5/100) + capAFMD * (exports_percentK/100)
    capAFMD <- capAFMD - capAFMD * (5/100)
    slNewFMD <- slNewFMD + slNewFMD * (exports_percentK/100)
    
  }else if(i == 2){
    # i >= 4 && i <= 5 
    # i == 2
    # capAFMD <- capAFMD + capAFMD * (exports_percentK/100)
    capAFMD <- capAFMD
    slNewFMD <- slNewFMD + slNewFMD * (exports_percentK/100)
    
  }else{
    
    capAFMD <- capAFMD
    
  }
  
  # k <- 0
  # k0s <- k0s_PostFMD[i,-1]
  # int_k3 <- 0
  # 
  # slaughterAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Slaughter_avg))
  # cullAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Cull_avg))
  # 
  # k_old_headFMD <- g * K1[i] - (slNewFMD * (1000000000/slaughterAvgFMD))
  # 
  # QsFMD <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
  #                            k = k, CapA = capAFMD, gamma_k3 = gamma_k3,
  #                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
  #                            slAvg = slaughterAvgFMD, clAvg = cullAvgFMD, slDem = slNewFMD, clDem = clNewFMD)
  
  ### From the above function, of the returned values I check whether the supplies are greater than the 
  ### constructed supplies. If yes, then I use them as my supply. I also use the replacement heifers determined
  ### from the above to append my age distribution.
  
  # if(QsFMD[1] > slNewFMD){
    # slNewFMD <- QsFMD[1]
  # }
  # if(QsFMD[2]> clNewFMD){
    # clNewFMD <- QsFMD[2]
  # }
  
  # k_oldFMD <-  QsFMD[4]
  # k_old_head1FMD <-  QsFMD[5]
  # 
  # mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD+1] <- k_old_head1FMD
  
  # if(!is.integer(mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD+1])){
  #   mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD+1] <- 
  #     delta * mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD]
  #   mergedForecastFMD_Proj$k10[mergedForecastFMD_Proj$Year == yearIFMD+1] <- 0
  # }
  
  
  # if(!is.integer(mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+2])){
  #   mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+2] <- 
  #     delta * mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD+1]
  #   mergedForecastFMD_Proj$k5[mergedForecastFMD_Proj$Year == yearIFMD+3] <- 
  #     delta * mergedForecastFMD_Proj$k4[mergedForecastFMD_Proj$Year == yearIFMD+2]
  #   mergedForecastFMD_Proj$k6[mergedForecastFMD_Proj$Year == yearIFMD+4] <- 
  #     delta * mergedForecastFMD_Proj$k5[mergedForecastFMD_Proj$Year == yearIFMD+3]
  #   mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD+5] <- 
  #     delta * mergedForecastFMD_Proj$k6[mergedForecastFMD_Proj$Year == yearIFMD+4]
  #   mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD+6] <- 
  #     delta * mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD+5]
  #   # mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD+7] <- 
  #   #   delta * mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD+6]
  # }
  # 
  # mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD+1] <- 
  #   mergedForecastFMD_Proj %>% filter(Year == yearIFMD+1) %>% mutate(K= k3+k4+k5+k6+k7+k8+k9) %>% select(K) %>% as.numeric()
  # 
  # # k9k10 <- mergedForecast_Proj %>% filter(Year == yearI+1) %>%
  # #   mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9), k10 = 0)
  # # mergedForecast_Proj$k9[mergedForecast_Proj$Year == yearI+1] <- k9k10$k9
  # # mergedForecast_Proj$k10[mergedForecast_Proj$Year == yearI+1] <- k9k10$k10
  # 
  # mergedForecastFMD_Proj$cl[mergedForecastFMD_Proj$Year == yearIFMD] <- clNewFMD
  # mergedForecastFMD_Proj$sl[mergedForecastFMD_Proj$Year == yearIFMD] <- slNewFMD
  # 
  # # ANewFMD <- (slNewFMD + clNewFMD) * adjF_pre
  # 
  # ANewFMD <- capAFMD
  # 
  # shFMD <- ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
  #             (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  # 
  # proj_Q_P_PostFMD$muTilde[i] <- MUtilde_pre
  # proj_Q_P_PostFMD$sTilde[i] <- Stilde_pre
  # proj_Q_P_PostFMD$sh[i] <- shFMD
  
  PsFMD <- getPsPcEpsEpc_FMD_EQ(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                             HcM = hcM_pre, SlNew = slNewFMD, ClNew = clNewFMD, ANew = capAFMD, 
                             params = c(MUtilde_pre, Stilde_pre), depops = dePopR)
  
  psM_pre <- PsFMD[1]
  pcM_pre <- PsFMD[2]
  hcM_pre <- PsFMD[3]
  EpsM_pre <- PsFMD[4]
  EpcM_pre <- PsFMD[5]
  
  proj_Q_P_PostFMD$Ps[i] <- psM_pre
  proj_Q_P_PostFMD$Pc[i] <- pcM_pre
  proj_Q_P_PostFMD$Hc[i] <- hcM_pre
  proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
  proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
  
  proj_Q_P_PostFMD$muTilde[i] <- MUtilde_pre
  proj_Q_P_PostFMD$sTilde[i] <- Stilde_pre
  proj_Q_P_PostFMD$sh[i] <- sh_pre
  
  proj_Q_P_PostFMD$Sl[i] <- slNewFMD
  proj_Q_P_PostFMD$Cl[i] <- clNewFMD
  proj_Q_P_PostFMD$A[i] <- ANewFMD
  # proj_Q_P_PostFMD$repHeif[i] <- k_oldFMD
  proj_Q_P_PostFMD$repHeif_Head[i] <- k_old_headFMD
  
  proj_Q_P_PostFMD$boundCond[i] <- abs(k_old_headFMD) <= 0.5 * g * K1[i]
  
  beefINV_FORECAST_PostFMD[i,] <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Year, K, k3, k4, k5, k6, k7, k8, k9)
  
  # calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
  #                                                    k0 = g * beefINV_FORECAST_PostFMD$K[i])
  
  
  # k0s_PostFMD[i+1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i+1,], 
  #                                       beefINV_FORECAST = beefINV_FORECAST_PostFMD[i+1,], 
  #                                       calfCrop = calf_crop_PostFMD)
  
  capK_pre <- beefINV_FORECAST_PostFMD[i,]$K
  
  params_mu_s_FMDProj <- optParamFunction_FMDProj(sl = slNewFMD, cl = clNewFMD, 
                                                  ps = psM_pre, pc = pcM_pre, thetas = c(1,1), adj = 1)
  MUtilde_pre <- params_mu_s_FMDProj[1]
  Stilde_pre <- params_mu_s_FMDProj[2]
  
  
  
  
    
  if(i==1){
    capK_pre <- mergedForecastFMD %>% filter(Year == yearIFMD-2) %>% select(K) %>% as.numeric()
  }else{
    capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
  }
  
  
  
  if(i <= 3){
    matureYoungs <- mergedForecastFMD %>% filter(Year == yearIFMD-2) %>% select(K) %>% as.numeric()
  } else{
    matureYoungs <- beefINV_FORECAST_PostFMD %>% filter(Year == yearIFMD-2) %>% select(K) %>% as.numeric()
  }
  
  # capK_pre <- capK_pre + 0.5 * delta * calfCrop
  capK_pre <- capK_pre + 0.5 * g * matureYoungs
  # capK_pre <- capK_pre * (1+ 0.5 * g)
  
  K1[i] <- capK_pre
  
  k <- 0
  k0s <- k0s_PostFMD[i,-1]
  int_k3 <- 0
  
  ##### Here I must construct the supply with the storage model type equation from the age distribution
  slDemand <- capA_pre *
    ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
       (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  clDemand <- capA_pre * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  Qs <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                       k = k, CapA = capA_pre, gamma_k3 = gamma_k3,
                       eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                       slAvg = slaughterAvg_pre, clAvg = cullAvg_pre, slDem = slDemand, clDem = clDemand)
  slNew <- Qs[1]
  clNew <- Qs[2]

  k_old <- Qs[4]

  k_old_Head <- Qs[5]

  k_old_Head_OG <- Qs[6]
  slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
  clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
  
  ANew1 <- capA_pre
  # slNew <- slNew * adjF_pre
  # clNew <- clNew * adjF_pre
  
  slCounterL <- 0
  clCounterL <- 0

  slCounterH <- 0
  clCounterH <- 0

  while(clNew < clHistMin){
    clNew <- ((clNew * 1000000000)/cullAvg_pre + 10000) * (cullAvg_pre/1000000000)
    # clNew <- clNew + 0.1
    clCounterL <- 1
  }

  while(slNew < slHistMin){
    slNew <- ((slNew * 1000000000)/slaughterAvg_pre + 10000) * (slaughterAvg_pre/1000000000)
    # slNew <- slNew + 0.1
    slCounterL <- 1
  }
  
  while(clNew > clHistMax){
    clNew <- ((clNew * 1000000000)/cullAvg_pre - 10000) * (cullAvg_pre/1000000000)
    # clNew <- clNew - 0.1
    clCounterH <- 1
  }
  
  while(slNew > slHistMax){
    slNew <- ((slNew * 1000000000)/slaughterAvg_pre - 10000) * (slaughterAvg_pre/1000000000)
    # slNew <- slNew - 0.1
    slCounterH <- 1
  }
  
  #### Also add condition where the supply is greater than the historical supply. In that case remove 10000 animals from 
  #### each category and then remove the same number of animals from relacement heifers and the stock respectively
  #### Tell them these will be exports. Since I am not including exports or imports, the addition and subtraction of 
  #### animals should be considered as exports and imports. These must be price driven.
  
  if(slCounterL == 1){
    slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
    slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
    if(k_old_Head_OG<=0){
      k_old_Head <- slHeadDiff
    }else{
      k_old_Head <- k_old_Head + slHeadDiff
    }
    EpsM_pre <- EpsM_pre + 0.01
  }
  
  
  if(clCounterL == 1){
    clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
    clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
    # if(k_old_Head_OG<=0){
    #   K1[i] <- clHeadDiff
    # }else{
      K1[i] <- K1[i] + clHeadDiff
    # }
    EpcM_pre <- EpcM_pre + 0.01
  }
  
  
  if(slCounterH == 1){
    slNew_Head_New <- (slNew * 1000000000)/slaughterAvg_pre
    slHeadDiff <- abs(abs(slNew_Head_New) - abs(slNew_Head_OG))
    if(k_old_Head_OG<=0){
      k_old_Head <- slHeadDiff
    }else{
      if((k_old_Head - slHeadDiff)>0){
        k_old_Head <- k_old_Head - slHeadDiff
      }
      
    }
    EpsM_pre <- EpsM_pre - 0.01
  }
  
  if(clCounterH == 1){
    clNew_Head_New <- (clNew * 1000000000)/cullAvg_pre
    clHeadDiff <- abs(abs(clNew_Head_New) - abs(clNew_Head_OG))
    # if(k_old_Head_OG<=0){
    #   k_old_Head <- clHeadDiff
    # }else{
      # k_old_Head <- k_old_Head - clHeadDiff
    # }
    if((K1[i] - clHeadDiff)>0){
      K1[i] <- K1[i] - clHeadDiff
    }
    # if((slCounterH == 1) && (k_old_Head - clHeadDiff)>0){
    #   k_old_Head <- k_old_Head - clHeadDiff
    # }
    EpcM_pre <- EpcM_pre - 0.01
  }

  
  if(i>5){
    psM_pre <- psM_pre + 0.025
    pcM_pre <- pcM_pre - 0.01
    # EpsM_pre <- EpsM_pre + 0.02
  }
  
  Ps <- getPsPcEpsEpc_FMD_EQ(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                          HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                          params = c(MUtilde_pre, Stilde_pre),depops = dePopR)
  
  psM_pre <- Ps[1]
  pcM_pre <- Ps[2]
  hcM_pre <- Ps[3]
  EpsM_pre <- Ps[4]
  EpcM_pre <- Ps[5]
  
  D_sl1 <- ANew1 *
    ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
       (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  D_cl1 <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  slDiff <- slNew - D_sl1
  clDiff <- clNew - D_cl1
  
  m <- 1
  
  slDiffEq <- NULL
  clDiffEq <- NULL
  psM_Eq <- NULL
  pcM_Eq <- NULL
  
  clHeadDiff1 <- NULL
  headRatioEq <- NULL
  slHeadDiff1 <- NULL
  k_old_Head_EqRatio <- NULL
  K1Ratio <- NULL
  
  
  if(k_old_Head_OG<=0){
    fedTBA <- k_old_Head
  }else{
    fedTBA <- k_old_Head
    if((K1[i] - fedTBA)>0){
      K1[i] <- K1[i] - fedTBA
    }
  }
  
  k3Median <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
  k3Median <- mean(k3Median$k3)
  
  while(fedTBA > k3Median){
    fedTBA <- fedTBA - 1000
  }
  
  Kmedian <- Stock %>% filter(Year <= beefINV_FORECAST_PostFMD$Year[i]) %>% tail(15)
  Kmedian <- mean(Kmedian$K)
  
  while(K1[i] > Kmedian){
    K1[i] <- K1[i] - 1000
  }
  
  repNewLbs <- fedTBA * (slaughterAvg_pre/1000000000)
  repNewHead <- fedTBA
  
  beefINV_FORECAST_PostFMD$K[i] <- K1[i]
  
  proj_Q_P_PostFMD$Ps[i] <- psM_pre
  proj_Q_P_PostFMD$Pc[i] <- pcM_pre
  proj_Q_P_PostFMD$Hc[i] <- hcM_pre
  proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
  proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
  
  proj_Q_P_PostFMD$Sl[i] <- slNew
  proj_Q_P_PostFMD$Cl[i] <- clNew
  proj_Q_P_PostFMD$A[i] <- ANew1
  proj_Q_P_PostFMD$repHeif[i] <- repNewLbs
  proj_Q_P_PostFMD$repHeif_Head[i] <- repNewHead
  
  proj_Q_P_PostFMD$boundCond[i] <- abs(repNewHead) <= 0.5 * g * K1[i]
  
  proj_Q_P_PostFMD$repHeif_HeadOG[i] <- k_old_Head_OG
  proj_Q_P_PostFMD$Sl_Head_OG[i] <- slNew_Head_OG
  proj_Q_P_PostFMD$Cl_Head_OG[i] <- clNew_Head_OG
  
  proj_Q_P_PostFMD$Sl_Head_EQ[i] <- ( slNew * 1000000000 )/slaughterAvg_pre
  proj_Q_P_PostFMD$Cl_Head_EQ[i] <- ( clNew * 1000000000 )/cullAvg_pre
  
  proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
  
  beefINV_FORECAST_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i] 
  beefINV_FORECAST_PostFMD$k3[i+1] <-  abs(proj_Q_P_PostFMD$repHeif_Head[i])
  # beefINV_FORECAST_PostFMD$k4[i+1] <- delta * beefINV_FORECAST_PostFMD$k3[i]
  # beefINV_FORECAST_PostFMD$k5[i+1] <- delta * beefINV_FORECAST_PostFMD$k4[i]
  # beefINV_FORECAST_PostFMD$k6[i+1] <- delta * beefINV_FORECAST_PostFMD$k5[i]
  # beefINV_FORECAST_PostFMD$k7[i+1] <- delta * beefINV_FORECAST_PostFMD$k6[i]
  # beefINV_FORECAST_PostFMD$k8[i+1] <- delta * beefINV_FORECAST_PostFMD$k7[i]
  # beefINV_FORECAST_PostFMD$k9[i+1] <- delta * beefINV_FORECAST_PostFMD$k8[i]
  # beefINV_FORECAST_PostFMD$k10[i+1] <- delta * beefINV_FORECAST_PostFMD$k9[i]
  # beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
  
  calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
                                                     k0 = g * beefINV_FORECAST_PostFMD$K[i])
  
  # capA_pre <- (slNew + clNew) 
    
}



de20P <- proj_Q_P_PostFMD
de20I <- beefINV_FORECAST_PostFMD
de20C <- calf_crop_PostFMD

de50P <- proj_Q_P_PostFMD
de50I <- beefINV_FORECAST_PostFMD
de50C <- calf_crop_PostFMD

de90P <- proj_Q_P_PostFMD
de90I <- beefINV_FORECAST_PostFMD
de90C <- calf_crop_PostFMD

de20P <- de20P %>% transmute(Year = Year, ps20 = Ps, pc20 = Pc) 
de50P <- de50P %>% transmute(Year = Year, ps50 = Ps, pc50 = Pc) 
de90P <- de90P %>% transmute(Year = Year, ps90 = Ps, pc90 = Pc)

de20I <- de20I %>% transmute(Year = Year, K20 = K)
de50I <- de50I %>% transmute(Year = Year, K50 = K)
de90I <- de90I %>% transmute(Year = Year, K90 = K)

PPrices <- merge(de20P,merge(de50P, de90P)) %>% select(Year, ps20, ps50, ps90, pc20, pc50, pc90)
PStocks <- merge(de20I, merge(de50I, de90I))
PStocks[,-1] <- PStocks[,-1]/1000000

OPrices <- merge(de20P,merge(de50P, de90P)) %>% select(Year, ps20, ps50, ps90, pc20, pc50, pc90)
OStocks <- merge(de20I, merge(de50I, de90I))
OStocks[,-1] <- OStocks[,-1]/1000000









for(i in 1:nrow(proj_Q_P_PostFMD)){
  
  # i <- 4
  
  if(i>1){
    #Populating the younglings
    k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,],
                                           beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,],
                                           calfCrop = calf_crop_PostFMD)
    
    #Retrieving the previous years derived demand and total breeding stock
    capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
    
    capA_pre <- proj_Q_P_PostFMD$A[i-1]
  }
  
  if(i==1){
    capK_pre <- capK_depopFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-1) %>% select(K) %>% as.numeric()
    capA_pre <- demandFMD$A[i]
  }
  
  #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
  ### the optimal replacement heifers.
  k <- 0
  K1[i] <- capK_pre
  k0s <- k0s_PostFMD[i,-1]
  int_k3 <- 0

  ### Here I assume a 5% decrease in domestic demand and the exports are banned.
  # I am also assuming decrease in the stocks by the specified depop.
  if(i==1){
    capA_pre <- capA_pre - capA_pre * (5/100) - capA_pre * (exports_percentK/100)
  } else if(i==2){
    capA_pre <- capA_pre - capA_pre * (exports_percentK/100)
  } else{
    capA_pre <- capA_pre
  }
  
  D_sl1 <- capA_pre *
    ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
       (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  D_cl1 <- capA_pre * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  # if(K1[i] < median(Stock$K)){
  #   K1[i] <- K1[i] + calf_crop_PostFMD %>% 
  #     filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
  # }
 
 
  # Here I get the supply by passing the stocks and derived demand. This function will give us
  # the meat supply and also the replacement heifers
  Qs <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = capA_pre, gamma_k3 = gamma_k3, 
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre, slDem = D_sl1, clDem = D_cl1)
    
  slNew <- Qs[1]
  clNew <- Qs[2]
  ANew <- Qs[3]
    
  ### I take the absolute value of the replacement heifers. This is to avoid negative values. It's written inside the 
  ### function. 
  ### Note: by making the negative values positive we can make sure that they are imports.
    
  k_old <- Qs[4]
    
  k_old_Head <- Qs[5]
    
  k_old_Head_OG <- Qs[6]
  slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
  clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
  
  
  # clCounter <- 0
  # slCounter <- 0
  # 
  # while(clNew < 1.01){
  #   clNew <- clNew + 0.01
  #   clCounter <- 1
  # }
  # 
  # while(slNew < 19.01){
  #   slNew <- slNew + 0.01
  #   slCounter <- 1
  # }
  
  slNew <- slNew
  clNew <- clNew 

    # if((slCounter==1) || (clCounter==1)){

    # Here I am saying that if we import live animals to meet the subsistence levels of demand,
    # they must be added to the stocks and replacement heifers.
    # if((slCounter==1)){
    #   
    #   slNew_Head_EQ <- (slNew * 1000000000)/slaughterAvg_pre
    #   slHeadDiff[i] <- slNew_Head_EQ - slNew_Head_OG
    # 
    #   k_old_Head <-  k_old_Head + slHeadDiff[i]
    #   
    #   # headRatio[i] <- slNew_Head_EQ/slNew_Head_OG
    # 
    #   # K1[i] <- K1[i] * headRatio[i]
    #   # k_old_Head <- k_old_Head * headRatio[i]
    # }
  
    # # This cull cow head difference must be added to the next stock.
    # # Rational for this : We are using the current stock to
    # # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
    # if((clCounter==1)){
    #   clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
    #   clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
    #   # K1[i] <- K1[i] + clHeadDiff[i]
    # }
  
  

  # }
  
  # This cull cow head difference must be added to the next stock. 
  # Rational for this : We are using the current stock to 
  # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
  # if((clCounter==1)){
  #   clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
  #   clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
  #   K1[i] <- K1[i] + clHeadDiff[i]
  # }
  
  # if(i == 1){
  #   # i < 4
  #   # i == 1
  #   #### Exports are banned that means the production stays in the country.
  #   ### Here we are changing the total demand for meat. Domestic decline for meat along with the exports are incorporated
  #   # slExports <- slNew * (exports_percentK/100)
  #   # clExports <- clNew * (exports_percentK/100)
  #   # ANew1 <- ANew + slExports +  clExports - (5/100) * ANew
  #   ANew1 <- ANew - (5/100) * ANew - ANew * (exports_percentK/100)
  # } else if(i == 2 ){ 
  #   # i >= 4 && i <= 5 
  #   # i == 2 
  #   ### Here the domestic demand for meat climbs back up
  #   # slExports <- slNew * (exports_percentK/100)
  #   # clExports <- clNew * (exports_percentK/100)
  #   # ANew1 <- ANew + slExports +  clExports + (5/100) * ANew
  #   ANew1 <- ANew - ANew * (exports_percentK/100) + (5/100) * ANew
  #   # ANew1 <- ANew + ANew * (exports_percentK/100)
  # } else{
  #   ### Everything is back to normal
  #   # slExports <- slNew * (exports_percentK/100)
  #   # clExports <- clNew * (exports_percentK/100)
  #   # ANew1 <-  ANew - slExports - clExports + (5/100) * ANew
  #   ANew1 <-  ANew + ANew * (exports_percentK/100) + (5/100) * ANew
  #   # ANew1 <-  ANew 
  # }
  
  # Here I am saying that if we are importing, the expected price must go up. This will 
  # incentivize the farmers to build the stocks.
  # if(slCounter==1){
  #   EpsM_pre <- EpsM_pre + 0.08
  # }
  # 
  # if(clCounter==1){
  #   EpcM_pre <- EpcM_pre + 0.08
  # }
  
  ANew1 <- capA_pre
  
  Ps <- getPsPcEpsEpc_FMD(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                          HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                          params = c(MUtilde_pre, Stilde_pre))
  
  psM_pre <- Ps[1]
  pcM_pre <- Ps[2]
  hcM_pre <- Ps[3]
  EpsM_pre <- Ps[4]
  EpcM_pre <- Ps[5]
  
  # if(k_old_Head == 0){
  #   k_old_Head <- g * K1[i] - slNew_Head_EQ
  # }
  
  ##### Here I check whether the market is cleared with the new demand. 
  #### I simply get the demand for fed cattle meat and cull cow meat under new prices.
  #### Substract them from the supply of the corresponding meat and then give direction 
  #### to prices
  
  D_sl1 <- ANew1 *
    ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
       (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  D_cl1 <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  slDiff <- slNew - D_sl1
  clDiff <- clNew - D_cl1
  
  m <- 1
  
  slDiffEq <- NULL
  clDiffEq <- NULL
  psM_Eq <- NULL
  pcM_Eq <- NULL
  
  clHeadDiff1 <- NULL
  headRatioEq <- NULL
  slHeadDiff1 <- NULL
  k_old_Head_EqRatio <- NULL
  K1Ratio <- NULL
  
  while(abs(slDiff)>0.1 || abs(clDiff)>0.1){

    slDiffEq[m] <- slDiff
    clDiffEq[m] <- clDiff

    if( slDiff < 0){
      psN <- psM_pre + 0.01
    } else if( slDiff > 0){
      psN <- psM_pre - 0.01
    }

    if(psN < 0){
      psN <- psM_pre
    }

    if( clDiff < 0){
      pcN <- pcM_pre + 0.01
    } else if( clDiff > 0){
      pcN <- pcM_pre - 0.01
    }

    if(pcN < 0){
      pcN <- pcM_pre
    }

    hcM_pre <- (((g * (beta^3) * psN) + (beta - 1) * pcN)/(1 + g * beta * (gamma0 + beta * gamma1)))

    while(hcM_pre>pcN){
      hcM_pre <- pcN - 0.01
    }

    Ps <- getPsPcEpsEpc_FMD(PsM = psN, PcM = pcN, EPsM = EpsM_pre, EPcM = EpcM_pre,
                            HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1,
                            params = c(MUtilde_pre, Stilde_pre))

    psM_pre <- Ps[1]
    pcM_pre <- Ps[2]
    hcM_pre <- Ps[3]
    EpsM_pre <- Ps[4]
    EpcM_pre <- Ps[5]

    psM_Eq[m] <- psM_pre
    pcM_Eq[m] <- pcM_pre

    ### Here I make sure the expected price is not going out of bounds

      # while(EpsM_pre < psM_pre){
      #   EpsM_pre <- EpsM_pre  + 0.8
      # }
      #
      # while(EpcM_pre < pcM_pre){
      #   EpcM_pre <- EpcM_pre + 0.8
      # }


    D_sl <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))

    D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))



    Qs <- getSlClA_test_FMD_EQ(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = ANew1, gamma_k3 = gamma_k3,
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre, slDem = D_sl,
                            clDem = D_cl)
    slNew_Eq <- Qs[1]
    clNew_Eq <- Qs[2]
    ANew_Eq <- Qs[3]
    k_old_Eq <- Qs[4]
    k_old_Head_Eq <- Qs[5]

    k_old_Head_OG_Eq <- Qs[6]
    slNew_Head_OG_Eq <- (slNew_Eq * 1000000000)/slaughterAvg_pre
    clNew_Head_OG_Eq <- (clNew_Eq * 1000000000)/cullAvg_pre

    slCounter_Eq <- 0
    clCounter_Eq <- 0

    # while(clNew_Eq < 1.01){
    #   clNew_Eq  <- clNew_Eq  + 0.01
    #   clCounter_Eq <- 1
    # }
    # 
    # while(slNew_Eq < 19.01){
    #   slNew_Eq <- slNew_Eq + 0.01
    #   slCounter_Eq <- 1
    # }

    # ANew_Eq <- (slNew_Eq + clNew_Eq) * (1/adjF_pre)

    # if((slCounter_Eq==1) || (clCounter_Eq==1)){

      # if((slCounter_Eq==1)){
      #   slNew_Head_EQ1 <- (slNew_Eq * 1000000000)/slaughterAvg_pre
      #   slHeadDiff1[m] <- slNew_Head_EQ1 - slNew_Head_OG_Eq
      #   # headRatioEq[m] <- slNew_Head_EQ1/slNew_Head_OG_Eq
      #   # K1[i] <- K1[i] * headRatioEq[m]
      #   # k_old_Head_EqRatio[m] <- k_old_Head_Eq * headRatioEq[m]
      # 
      #   k_old_Head_EqRatio[m] <-  g * K1[i] - slNew_Head_EQ1
      # }

      # if((clCounter_Eq==1)){
      #   clNew_Head_EQ1 <- (clNew_Eq * 1000000000)/cullAvg_pre
      #   clHeadDiff1[m] <- clNew_Head_EQ1 - clNew_Head_OG_Eq
      #   K1[i] <- K1[i] + clHeadDiff1[m]
      # }


    # }

    # if((slCounter_Eq==1)){
    #   slNew_Head_EQ1 <- (slNew_Eq * 1000000000)/slaughterAvg_pre
    #   slHeadDiff1[m] <- slNew_Head_EQ1 - slNew_Head_OG_Eq
    #   headRatioEq[m] <- slNew_Head_EQ1/slNew_Head_OG_Eq
    #   K1[i] <- K1[i] * headRatioEq[m]
    #   k_old_Head_EqRatio[m] <- k_old_Head_Eq * headRatioEq[m]
    # }
    #
    # if((clCounter_Eq==1)){
    #   clNew_Head_EQ1 <- (clNew_Eq * 1000000000)/cullAvg_pre
    #   clHeadDiff1[m] <- clNew_Head_EQ1 - clNew_Head_OG_Eq
    #   K1[i] <- K1[i] + clHeadDiff1[m]
    # }

    # if(i < 4){
    #   # i < 4
    #   # i == 1
    #   # slExp1 <- slNew_Eq  * (exports_percentK/100)
    #   # clExp1 <- clNew_Eq  * (exports_percentK/100)
    #   # ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + slExp1 + clExp1
    #
    #   ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + ANew_Eq * (exports_percentK/100)
    #
    # } else if( i >= 4 && i <= 5 ){
    #   # i >= 4 && i <= 5
    #   # i == 2
    #   # slExp1 <- slNew_Eq * (exports_percentK/100)
    #   # clExp1 <- clNew_Eq * (exports_percentK/100)
    #   # ANew11 <- ANew_Eq + slExp1 + clExp1 + (5/100) * ANew_Eq
    #   ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)  + (5/100) * ANew_Eq
    #   # ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)
    #
    # } else{
    #   # slExp1 <- slNew_Eq * (exports_percentK/100)
    #   # clExp1 <- clNew_Eq * (exports_percentK/100)
    #   # ANew11 <- ANew_Eq  - slExp1 - clExp1 + (5/100) * ANew_Eq
    #   ANew11 <- ANew_Eq - ANew_Eq * (exports_percentK/100) + (5/100) * ANew_Eq
    #   # ANew11 <- ANew_Eq
    # }
    #
    # ANew1 <- ANew11

    slDiff <- slNew_Eq - D_sl
    clDiff <- clNew_Eq - D_cl

    # The reason for this condition is to avoid infinite while loop. Note that the while loop stops
    # if the differences reach below tolerance levels. But sometimes this is never the case and there will be
    # some difference above tolerance level (basically saying that there will be closing stocks). So I exit the loop
    # if the difference stays stagnant.
    if(m >= 15){
      if( (round(slDiffEq[m],1) == round(slDiffEq[m-1],1)) && (round(clDiffEq[m],1) == round(clDiffEq[m-1],1)) ){
        if( (round(slDiffEq[m-1],1) == round(slDiffEq[m-2],1)) && (round(clDiffEq[m-1],1) == round(clDiffEq[m-2],1)) ){
          # if( (round(slDiffEq[m-2],2) == round(slDiffEq[m-3],2)) && (round(clDiffEq[m-2],2) == round(clDiffEq[m-3],2)) ){
               break
          # }
        }
      }
    }

    if((m %% 2 == 0)){
      slNew <- slNew_Eq
      clNew <- clNew_Eq
    }else{
      ANew1 <- (slNew_Eq + clNew_Eq) *  demandShocksFMD$dShock[i]
    }

    m <- m+1

  }
  
  ## Here i gather the replacement heifers from the above simulation
  # if(length(k_old_Head_EqRatio)>0){
  #   fedTBA <- max(k_old_Head,max(na.omit(k_old_Head_EqRatio)))
  # }else{
  #   fedTBA <- max(k_old_Head, k_old_Head_Eq)
  # }
  # 
  fedTBA <- k_old_Head_OG_Eq
  repNewHead <- fedTBA
  repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
  
  # Here I gather the number of cull cow animals need to be added in the stock.
  # Whatever the imports are, I am assuming they are 3 year olds.
  # if(length(clHeadDiff[i])>0){
  #   if(!is.na(clHeadDiff[i])){
  #     cullTBA <- max(clHeadDiff[i], na.omit(clHeadDiff1))
  #   }else{
  #     cullTBA <- 0
  #   }
  # }else{
  #   cullTBA <- 0
  # }

  # beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + cullTBA
  # beefINV_FORECAST_PostFMD$k4[i] <- beefINV_FORECAST_PostFMD$k4[i] + cullTBA
  # 
  # if(k_old_Head_OG<0){
  #   beefINV_FORECAST_PostFMD$K[i]  <- beefINV_FORECAST_PostFMD$K[i] + abs((k_old_Head_OG/g))
  #   beefINV_FORECAST_PostFMD$k4[i] <- beefINV_FORECAST_PostFMD$k4[i] + abs((k_old_Head_OG/g))
  # }

  # Here after knowing the stock, if it is less than the stock in equilibrium simulation, I add the progeny
  # of cull cows added above to the replacement heifers.
  # if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
  #     repNewHead <- repNewHead + g * cullTBA
  #     repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
  #     # beefINV_FORECAST_PostFMD$K[i] <- K1[i]
  # }
  
  # if(dePopR<50){
  # 
  #   if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
  # 
  #     newRatio <- K1[i]/beefINV_FORECAST_PostFMD$K[i]
  #     # beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] * newRatio
  #     repNewHead <- repNewHead * newRatio
  #     # repNewHead <- repNewHead
  #     repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
  #     # beefINV_FORECAST_PostFMD$k3[i] <- beefINV_FORECAST_PostFMD$k3[i] * newRatio
  #     # beefINV_FORECAST_PostFMD$K[i] <- sum(beefINV_FORECAST_PostFMD[i,-1:-2])
  #   }
  # }
  
  
  # beefINV_FORECAST_PostFMD[i,-1] <- beefINV_FORECAST_PostFMD[i,-1] + clHeadDiff1
  
  # multiplierK <- (beefINV_FORECAST_PostFMD$K[i] + clHeadDiff1)/beefINV_FORECAST_PostFMD$K[i] 
  # beefINV_FORECAST_PostFMD[i,-1] <- beefINV_FORECAST_PostFMD[i,-1] * multiplierK
  
  # psM_EqMed[i] <- median(psM_Eq)
  # pcM_EqMed[i] <- median(pcM_Eq)
  # 
  # psM_EqMN[i] <- mean(psM_Eq)
  # pcM_EqMN[i] <- mean(pcM_Eq)
  
  proj_Q_P_PostFMD$Ps[i] <- psM_pre
  proj_Q_P_PostFMD$Pc[i] <- pcM_pre
  proj_Q_P_PostFMD$Hc[i] <- hcM_pre
  proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
  proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
  
  proj_Q_P_PostFMD$Sl[i] <- slNew
  proj_Q_P_PostFMD$Cl[i] <- clNew
  proj_Q_P_PostFMD$A[i] <- ANew1
  proj_Q_P_PostFMD$repHeif[i] <- repNewLbs
  proj_Q_P_PostFMD$repHeif_Head[i] <- repNewHead
  
  proj_Q_P_PostFMD$boundCond[i] <- abs(repNewHead) <= 0.5 * g * K1[i]
  
  proj_Q_P_PostFMD$repHeif_HeadOG[i] <- k_old_Head_OG
  proj_Q_P_PostFMD$Sl_Head_OG[i] <- slNew_Head_OG
  proj_Q_P_PostFMD$Cl_Head_OG[i] <- clNew_Head_OG
  
  proj_Q_P_PostFMD$Sl_Head_EQ[i] <- ( slNew * 1000000000 )/slaughterAvg_pre
  proj_Q_P_PostFMD$Cl_Head_EQ[i] <- ( clNew * 1000000000 )/cullAvg_pre
  
  proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
  
  # cCrop <- calf_crop_PostFMD %>% filter(Year == proj_Q_P_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
  # rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
  # if (proj_Q_P_PostFMD$repHeif_Head[i] < (delta * cCrop - rHeif)){
  #   proj_Q_P_PostFMD$repHeif_Head[i] <- 
  # }
  
  # if( proj_Q_P_PostFMD$Cl_Head_OG[i]  <  proj_Q_P_PostFMD$Cl_Head_EQ[i] ){
    
  # }
  
  proj_Q_P_PostFMD$repHeif_Head[i] <- proj_Q_P_PostFMD$repHeif_Head[i] + 
    abs(proj_Q_P_PostFMD$Sl_Head_OG[i]) - abs(proj_Q_P_PostFMD$Sl_Head_EQ[i])
  
  # beefINV_FORECAST_PostFMD$k6[i] <- beefINV_FORECAST_PostFMD$k6[i] + 
  #   abs(proj_Q_P_PostFMD$Cl_Head_EQ[i]) - abs(proj_Q_P_PostFMD$Cl_Head_OG[i])
  
  beefINV_FORECAST_PostFMD$Year[i+1] <- beefINV_FORECAST_PostFMD$Year[i] + 1
  beefINV_FORECAST_PostFMD$k3[i+1] <-  abs(proj_Q_P_PostFMD$repHeif_Head[i])
  beefINV_FORECAST_PostFMD$k4[i+1] <- delta * beefINV_FORECAST_PostFMD$k3[i]
  beefINV_FORECAST_PostFMD$k5[i+1] <- delta * beefINV_FORECAST_PostFMD$k4[i]
  beefINV_FORECAST_PostFMD$k6[i+1] <- delta * beefINV_FORECAST_PostFMD$k5[i]
  beefINV_FORECAST_PostFMD$k7[i+1] <- delta * beefINV_FORECAST_PostFMD$k6[i]
  beefINV_FORECAST_PostFMD$k8[i+1] <- delta * beefINV_FORECAST_PostFMD$k7[i]
  beefINV_FORECAST_PostFMD$k9[i+1] <- delta * beefINV_FORECAST_PostFMD$k8[i]
  beefINV_FORECAST_PostFMD$k10[i+1] <- delta * beefINV_FORECAST_PostFMD$k9[i]
  beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
  
  # if(abs(proj_Q_P_PostFMD$Cl_Head_OG[i]) < abs(proj_Q_P_PostFMD$Cl_Head_EQ[i]) ){
  #   if(proj_Q_P_PostFMD$Cl_Head_OG[i]<0){
  #     beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + abs(proj_Q_P_PostFMD$Cl_Head_EQ[i])
  #   }else{
  #     beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i]  +
  #       abs(proj_Q_P_PostFMD$Cl_Head_EQ[i]) - abs(proj_Q_P_PostFMD$Cl_Head_OG[i])
  #     # beefINV_FORECAST_PostFMD$k7[i] <- beefINV_FORECAST_PostFMD$k7[i] +
  #     #   abs(proj_Q_P_PostFMD$Cl_Head_EQ[i]) - abs(proj_Q_P_PostFMD$Cl_Head_OG[i])
  #   }
  # }
  
  # if(i>5){
  # 
  #   if(beefINV_FORECAST_PostFMD$K[i+1] < min(Stock$K)){
  #     cCrop <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
  #     rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
  #     beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] +  (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
  #     # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] + (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
  #   }
  # }
  
  # Here if the replacement heifers are greater than the historical replacement heifers, we just export the rest of them.
  # This is an assumption I am willing to make
  
  # if( beefINV_FORECAST_PostFMD$k3[i+1] >  max(Stock$k3)){
  #   beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  #   # beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
  #   beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  # }
  
  # if( beefINV_FORECAST_PostFMD$k3[i+1] >  median(Stock$k3)){
  #   beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  #   beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  # }
  
  # Here once the stock reach the optimal level, the farmers won't keep the 9-year old cows and the rest are exported
  # The following code does that.
  # if( beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
  #   beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k9[i]/delta)
  #   beefINV_FORECAST_PostFMD$k10[i+1] <- 0
  #   # if(beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
  #   #   beefINV_FORECAST_PostFMD$K[i+1] <- median(Stock$K)
  #   #   beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
  #   # }
  #   # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$K[i]/delta)
  #   # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
  #   # beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
  # }
  
  # if((slCounter_Eq==1) || (clCounter_Eq==1)){
  #   if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
  #     beefINV_FORECAST_PostFMD$K[i] <- K1[i]
  #   }
  # }
  
  # if(i>1){
  #   if(beefINV_FORECAST_PostFMD$K[i] <  K1[i]){
  #     # Here we will get the calf crop three years ago and replacement heifers this year. If replacement heifers
  #     # is greater than the calf crop (that means we imported the replacement heifers), we only add the calf-crop
  #     # to the stock otherwise we add calf-crop minus replacement heifers to the stock.
  #     cCrop <- calf_crop_PostFMD %>% filter(Year == proj_Q_P_PostFMD$Year[i-1]-1) %>% select(k0) %>% as.numeric()
  #     rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
  #     # rHeif <- proj_Q_P_PostFMD %>% filter(Year == proj_Q_P_PostFMD$Year[i-2]-1) %>% select(repHeif_Head) %>% as.numeric()
  #     # if(rHeif > cCrop){
  #     #   K1[i] <- K1[i] + cCrop
  #     # } else{
  #     #   K1[i] <- K1[i] + cCrop - rHeif
  #     # }
  #     if(rHeif < cCrop){
  #       differenceTBA <- beefINV_FORECAST_PostFMD$K[i] - (delta * cCrop - rHeif)
  #       beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + differenceTBA
  #     }
  #   }
  # }
  
  # if( beefINV_FORECAST_PostFMD$K[i] >  median(Stock$K)){
  #   beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] - (beefINV_FORECAST_PostFMD$k10[i]/delta)
  #   beefINV_FORECAST_PostFMD$k10[i] <- 0
  # }
  
  calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
                                                     k0 = g * beefINV_FORECAST_PostFMD$K[i])
  
  # calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i],
  #                                                    k0 = g * K1[i])
  
}


proj_Q_P_PostFMD_20_OP <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_OP <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_20_OP_absk3 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_OP_absk3 <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_20_OP_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_OP_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_20_OP_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_20_OP_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_OP_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_20_OP_absk3_11 <- calf_crop_PostFMD

proj_Q_P_PostFMD_50_OP <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_OP <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_50_OP_absk3 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_OP_absk3 <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_50_OP_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_OP_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_50_OP_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_50_OP_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_OP_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_50_OP_absk3_11 <- calf_crop_PostFMD

proj_Q_P_PostFMD_90_OP <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_OP <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_90_OP_absk3 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_OP_absk3 <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_90_OP_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_OP_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_90_OP_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_90_OP_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_OP_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_90_OP_absk3_11 <- calf_crop_PostFMD

proj_Q_P_PostFMD_20_OP_absk3_11 <- proj_Q_P_PostFMD_20_OP_absk3_11 %>% transmute(Year = Year, Ps20 = Ps, Pc20 = Pc)
proj_Q_P_PostFMD_50_OP_absk3_11 <- proj_Q_P_PostFMD_50_OP_absk3_11 %>% transmute(Year = Year, Ps50 = Ps, Pc50 = Pc) 
proj_Q_P_PostFMD_90_OP_absk3_11 <- proj_Q_P_PostFMD_90_OP_absk3_11 %>% transmute(Year = Year, Ps90 = Ps, Pc90 = Pc) 


proj_Q_P_PostFMD_OP_absk3_11 <- merge(merge(proj_Q_P_PostFMD_20_OP_absk3_11, proj_Q_P_PostFMD_50_OP_absk3_11), 
                             proj_Q_P_PostFMD_90_OP_absk3_11)


beefINV_FORECAST_20_OP_absk3_11 <- beefINV_FORECAST_PostFMD_20_OP_absk3_11 %>% transmute(Year = Year, K20 = K) 
beefINV_FORECAST_50_OP_absk3_11 <- beefINV_FORECAST_PostFMD_50_OP_absk3_11 %>% transmute(Year = Year, K50 = K) 
beefINV_FORECAST_90_OP_absk3_11 <- beefINV_FORECAST_PostFMD_90_OP_absk3_11 %>% transmute(Year = Year, K90 = K) 


beefINV_FORECAST_PostFMD_OP_absk3_11 <- merge(merge(beefINV_FORECAST_20_OP_absk3_11, beefINV_FORECAST_50_OP_absk3_11), 
                                     beefINV_FORECAST_90_OP_absk3_11)



proj_Q_P_PostFMD_20_PE <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_PE <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_20_PE_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_PE_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_20_PE_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_20_PE_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_PE_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_20_PE_absk3_11 <- calf_crop_PostFMD

proj_Q_P_PostFMD_50_PE <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_PE <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_50_PE_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_PE_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_50_PE_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_50_PE_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_PE_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_50_PE_absk3_11 <- calf_crop_PostFMD

proj_Q_P_PostFMD_90_PE <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_PE <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_90_PE_absk3_1 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_PE_absk3_1 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_90_PE_absk3_1 <- calf_crop_PostFMD

proj_Q_P_PostFMD_90_PE_absk3_11 <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_PE_absk3_11 <- beefINV_FORECAST_PostFMD
calf_crop_PostFMD_90_PE_absk3_11 <- calf_crop_PostFMD


proj_Q_P_PostFMD_20_PE_absk3_11 <- proj_Q_P_PostFMD_20_PE_absk3_11 %>% transmute(Year = Year, Ps20 = Ps, Pc20 = Pc) 
proj_Q_P_PostFMD_50_PE_absk3_11 <- proj_Q_P_PostFMD_50_PE_absk3_11 %>% transmute(Year = Year, Ps50 = Ps, Pc50 = Pc) 
proj_Q_P_PostFMD_90_PE_absk3_11 <- proj_Q_P_PostFMD_90_PE_absk3_11 %>% transmute(Year = Year, Ps90 = Ps, Pc90 = Pc) 


proj_Q_P_PostFMD_PE_absk3_11 <- merge(merge(proj_Q_P_PostFMD_20_PE_absk3_11, proj_Q_P_PostFMD_50_PE_absk3_11), 
                                     proj_Q_P_PostFMD_90_PE_absk3_11)


beefINV_FORECAST_20_PE_absk3_11 <- beefINV_FORECAST_PostFMD_20_PE_absk3_11 %>% transmute(Year = Year, K20 = K) 
beefINV_FORECAST_50_PE_absk3_11 <- beefINV_FORECAST_PostFMD_50_PE_absk3_11 %>% transmute(Year = Year, K50 = K) 
beefINV_FORECAST_90_PE_absk3_11 <- beefINV_FORECAST_PostFMD_90_PE_absk3_11 %>% transmute(Year = Year, K90 = K) 


beefINV_FORECAST_PostFMD_PE_absk3_11 <- merge(merge(beefINV_FORECAST_20_PE_absk3_11, beefINV_FORECAST_50_PE_absk3_11), 
                                     beefINV_FORECAST_90_PE_absk3_11)

# for(i in 1:nrow(proj_Q_P_PostFMD)){
#   
#   i <- 1
#   
#   if(i>1){
#     
#     beefINV_FORECAST_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i-1] + 1
#     beefINV_FORECAST_PostFMD$k3[i] <-  abs(proj_Q_P_PostFMD$repHeif_Head[i-1])
#     beefINV_FORECAST_PostFMD$k4[i] <- delta * beefINV_FORECAST_PostFMD$k3[i-1]
#     beefINV_FORECAST_PostFMD$k5[i] <- delta * beefINV_FORECAST_PostFMD$k4[i-1]
#     beefINV_FORECAST_PostFMD$k6[i] <- delta * beefINV_FORECAST_PostFMD$k5[i-1]
#     beefINV_FORECAST_PostFMD$k7[i] <- delta * beefINV_FORECAST_PostFMD$k6[i-1]
#     beefINV_FORECAST_PostFMD$k8[i] <- delta * beefINV_FORECAST_PostFMD$k7[i-1]
#     beefINV_FORECAST_PostFMD$k9[i] <- delta * beefINV_FORECAST_PostFMD$k8[i-1]
#     beefINV_FORECAST_PostFMD$k10[i] <- delta * beefINV_FORECAST_PostFMD$k9[i-1]
#     beefINV_FORECAST_PostFMD$K[i] <- sum(beefINV_FORECAST_PostFMD[i,-1:-2])
#     
#     calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i-1],
#                                                        k0 = g * beefINV_FORECAST_PostFMD$K[i-1])
#     
#     k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,],
#                                            beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,],
#                                            calfCrop = calf_crop_PostFMD)
#     
#   }
#   
#   #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
#   ### the optimal replacement heifers. 
#   if(i>1){
#     
#     capA_pre <- proj_Q_P_PostFMD$A[i-1]
#     
#     capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
#     
#     K1 <- capK_pre
#   }
#   
#   k <- 0
#   k0s <- k0s_PostFMD[i,-1]
#   
#   int_k3 <- 0
#   
#   ### Here I assume a 5% decrease in domestic demand and the exports are banned (assume a 10%)
#   if(i==1){
#     
#     capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
#     K1 <- capK_pre * (1 - (dePopR/100))
#     
#     k08 <- k0s$k08
#     k07 <- k0s$k07
#     k06 <- k0s$k06
#     k05 <- k0s$k05
#     k04 <- k0s$k04
#     k03 <- k0s$k03
#     k02 <- k0s$k02
#     
#     k_old_Head <- beefINV_FORECAST_PostFMD[i+1,]$k3
#     
#     slNewHead <- g * K1 - k_old_Head
#     
#     clNewHead <- ((delta^4)/(gamma_k3^7)) * (delta^2 + (1-delta) * gamma_k3 * (delta + gamma_k3)) *
#       (k_old_Head - eta_k3 * ( (gamma_k3^4) * k06 + (gamma^3) * k05 + (gamma_k3^2) * k04 + gamma_k3 * k03 + k02)) -
#       ((delta^5)/(gamma_k3^2)) * eta * (delta * gamma_k3 * k08 + (delta + (1-delta) * gamma_k3) * k07)
#     
#     slNew <- slNewHead * (slaughterAvg_pre/1000000000)
#     clNew <- clNewHead * (cullAvg_pre/1000000000)
#   }
#   
#   if(i > 1){
#     
#     Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1,
#                             k = k, CapA = capA_pre, gamma_k3 = gamma_k3, 
#                             eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
#                             slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
#     
#     slNew <- Qs[1]
#     clNew <- Qs[2]
#     ANew <- Qs[3]
#     
#     ### I take the absolute value of the replacement heifers. This is to avoid negative values. It's written inside the 
#     ### function. 
#     ### Note: by making the negative values positive we can make sure that they are imports?
#     
#     k_old <- Qs[4]
#     
#     k_old_Head <- Qs[5]
#     
#     k_old_Head_OG <- Qs[6]
#     slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
#     clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
#   }
#   
#   
#   clCounter <- 0
#   slCounter <- 0
#   
#   while(clNew < 1.01){
#     clNew <- clNew + 0.01
#     clCounter <- 1
#   }
#   
#   while(slNew < 19.01){
#     slNew <- slNew + 0.01
#     slCounter <- 1
#   }
#   
#   ANew <- (slNew + clNew) * (1/adjF_pre)
#   
#   if((slCounter==1) || (clCounter==1)){
#     
#     slNew_Head_EQ <- (slNew * 1000000000)/slaughterAvg_pre
#     clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
#     
#     K1_new <- K1
#     
#     estK <- BBoptim(par = K1_new, fn = estRepHeifersEQ_FMD, slHead = slNew_Head_EQ, clHead = clNew_Head_EQ, 
#                     k = k_old_Head, gamma_k3 = gamma_k3,
#                     eta_k3 = eta_k3 , int_k3 = int_k3, k0s = k0s)
#     K1 <- estK$par
#   }
#   
#   
#   
#   
#   ##### I am assuming all the export meat is of high quality so the loss of exports means
#   ##### there is excess high quality meat in the country i.e., more supply. 
#   ##### Economic theory simply says high supply means the price is low. 
#   
#   if(i == 1){
#     # i < 4
#     #### Exports are banned that means the production stays in the country. So I assign equal weights to 
#     #### both high quality and low quality meat. This might change but for now this is what I do. 
#     slExports <- slNew * (exports_percentK/100)
#     clExports <- clNew * (exports_percentK/100)
#     ### Here we are changing the total demand for meat. Domestic decline for meat is incorporated
#     # ANew1 <- ANew - (5/100) * ANew + slExports + clExports
#     ANew1 <- ANew - (5/100) * ANew + ANew * (exports_percentK/100)
#   } else if( i == 2  ){ 
#     # i >= 4 && i <= 5 
#     ### Here the domestic demand for meat climbs back up
#     slExports <- slNew * (exports_percentK/100)
#     clExports <- clNew * (exports_percentK/100)
#     # ANew1 <- ANew + slExports + clExports
#     ANew1 <- ANew + ANew * (exports_percentK/100)
#   } else{
#     ### Everything is back to normal
#     ANew1 <-  ANew 
#   }
#   
#   if(slCounter==1){
#     EpsM_pre <- EpsM_pre + 0.08
#   }
#   
#   if(clCounter==1){
#     EpcM_pre <- EpcM_pre + 0.08
#   }
#   
#   # hcM_pre <- (((g * (beta^3) * psM_pre) + (beta - 1) * pcM_pre)/(1 + g * beta * (gamma0 + beta * gamma1)))
#   
#   Ps <- getPsPcEpsEpc_FMD(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
#                           HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
#                           params = c(MUtilde_pre, Stilde_pre))
#   
#   psM_pre <- Ps[1]
#   pcM_pre <- Ps[2]
#   hcM_pre <- Ps[3]
#   EpsM_pre <- Ps[4]
#   EpcM_pre <- Ps[5]
#   
#   ##### Here I check whether the market is cleared with the new demand. 
#   #### I simply get the demand for fed cattle meat and cull cow meat.
#   #### Substract that from the supply of the corresponding meat and then give direction 
#   #### to prices
#   # if(i < 3){
#   
#   D_sl <- ANew1 *
#     ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
#        (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
#   
#   D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
#   
#   slDiff <- slNew - D_sl
#   clDiff <- clNew - D_cl
#   
#   m <- 1
#   
#   slDiffEq <- NULL
#   clDiffEq <- NULL
#   psM_Eq <- NULL
#   pcM_Eq <- NULL
#   
#   while(abs(slDiff)>0.01 || abs(clDiff)>0.01){
#     
#     slDiffEq[m] <- slDiff
#     clDiffEq[m] <- clDiff
#     
#     if( slDiff < 0){
#       psN <- psM_pre + 0.001
#     } else if( slDiff > 0){
#       psN <- psM_pre - 0.001
#     }
#     
#     if(psN < 0){
#       psN <- psM_pre
#     }
#     
#     if( clDiff < 0){
#       pcN <- pcM_pre + 0.001
#     } else if( clDiff > 0){
#       pcN <- pcM_pre - 0.001
#     }
#     
#     if(pcN < 0){
#       pcN <- pcM_pre
#     }
#     
#     hcM_pre <- (((g * (beta^3) * psN) + (beta - 1) * pcN)/(1 + g * beta * (gamma0 + beta * gamma1)))
#     
#     Ps <- getPsPcEpsEpc_FMD(PsM = psN, PcM = pcN, EPsM = EpsM_pre, EPcM = EpcM_pre,
#                             HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1,
#                             params = c(MUtilde_pre, Stilde_pre))
#     
#     psM_pre <- Ps[1]
#     pcM_pre <- Ps[2]
#     hcM_pre <- Ps[3]
#     EpsM_pre <- Ps[4]
#     EpcM_pre <- Ps[5]
#     
#     psM_Eq[m] <- psM_pre
#     pcM_Eq[m] <- pcM_pre
#     
#     ### Here I make sure the expected price is not going out of bounds
#     # if(i>2){
#     #   
#     #   if(EpsM_pre < psM_pre){
#     #     EpsM_pre <- proj_Q_P_PostFMD$EPs[i-2]
#     #   }
#     #   
#     #   if(EpcM_pre < pcM_pre){
#     #     EpcM_pre <- proj_Q_P_PostFMD$EPc[i-2]
#     #   }
#     #   
#     # }
#     
#     Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1,
#                             k = k, CapA = ANew1, gamma_k3 = gamma_k3,
#                             eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
#                             slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
#     slNew_Eq <- Qs[1]
#     clNew_Eq <- Qs[2]
#     ANew_Eq <- Qs[3]
#     k_old_Eq <- Qs[4]
#     k_old_Head_Eq <- Qs[5]
#     
#     k_old_Head_OG_Eq <- Qs[6]
#     slNew_Head_OG_Eq <- (slNew_Eq * 1000000000)/slaughterAvg_pre
#     clNew_Head_OG_Eq <- (clNew_Eq * 1000000000)/cullAvg_pre
#     
#     while(clNew_Eq < 1.01){
#       clNew_Eq  <- clNew_Eq  + 0.01
#     }
#     while(slNew_Eq < 19.01){
#       slNew_Eq <- slNew_Eq + 0.01
#     }
#     
#     ANew_Eq <- (slNew_Eq + clNew_Eq) * (1/adjF_pre)
#     
#     if(i == 1){
#       # i < 4
#       slExp1 <- slNew_Eq  * (exports_percentK/100)
#       clExp1 <- clNew_Eq  * (exports_percentK/100)
#       # ANew11  <- (1 - (5/100)) * ANew_Eq  - slExp1 - clExp1
#       
#       ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + ANew_Eq * (exports_percentK/100)
#       
#     } else if( i == 2 ){
#       # i >= 4 && i <= 5
#       slExp1 <- slNew_Eq * (exports_percentK/100)
#       clExp1 <- clNew_Eq * (exports_percentK/100)
#       # ANew11 <- ANew_Eq - slExp1 - clExp1
#       ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)
#       
#     } else{
#       ANew11 <- ANew_Eq + slExp1 + clExp1
#       
#     }
#     
#     ANew1 <- ANew11
#     
#     
#     D_sl <- ANew1 *
#       ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
#          (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
#     
#     D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
#     
#     slDiff <- slNew - D_sl
#     clDiff <- clNew - D_cl
#     
#     if(m >= 10){
#       if( (round(slDiffEq[m],2) == round(slDiffEq[m-1],2)) && (round(clDiffEq[m],2) == round(clDiffEq[m-1],2)) ){
#         if( (round(slDiffEq[m-1],2) == round(slDiffEq[m-2],2)) && (round(clDiffEq[m-1],2) == round(clDiffEq[m-2],2)) ){
#           break
#         }
#       }
#     }
#     
#     m <- m+1
#     
#   }
#   
#   psM_EqMed[i] <- median(psM_Eq)
#   pcM_EqMed[i] <- median(pcM_Eq)
#   
#   psM_EqMN[i] <- mean(psM_Eq)
#   pcM_EqMN[i] <- mean(pcM_Eq)
#   
#   proj_Q_P_PostFMD$Ps[i] <- psM_pre
#   proj_Q_P_PostFMD$Pc[i] <- pcM_pre
#   proj_Q_P_PostFMD$Hc[i] <- hcM_pre
#   proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
#   proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
#   
#   proj_Q_P_PostFMD$Sl[i] <- slNew
#   proj_Q_P_PostFMD$Cl[i] <- clNew
#   proj_Q_P_PostFMD$A[i] <- ANew1
#   proj_Q_P_PostFMD$repHeif[i] <- k_old_Eq
#   proj_Q_P_PostFMD$repHeif_Head[i] <- k_old_Head_Eq
#   
#   proj_Q_P_PostFMD$boundCond[i] <- abs(k_old_Head_Eq) <= 0.5 * g * K1
#   
#   proj_Q_P_PostFMD$repHeif_HeadOG[i] <- k_old_Head_OG_Eq
#   proj_Q_P_PostFMD$Sl_Head_OG[i] <- slNew_Head_OG
#   proj_Q_P_PostFMD$Cl_Head_OG[i] <- clNew_Head_OG
#   
#   proj_Q_P_PostFMD$Sl_Head_EQ[i] <- ( slNew * 1000000000 )/slaughterAvg_pre
#   proj_Q_P_PostFMD$Cl_Head_EQ[i] <- ( clNew * 1000000000 )/cullAvg_pre
#   
#   proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
#   
# }








