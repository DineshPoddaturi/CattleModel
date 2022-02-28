####################### GLOBAL FUNCTIONS ####################
shareMetric <- function(paramMu, paramS, ps, pc){
  
  share <- ((exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))/(1 + (exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))))
  return(share)
  
}

estQFunction <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3){
  
  k3t2 <- k
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  F1 <- g * K1 - k3t2 - A * slShare
  F2 <-  k3t2 * (delta^4) * (1/(gamma_k3^6)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) ) - A * clShare
  
  F <- F1^2 + F2^2
  
}

estPFunction <- function(p, sl, cl, A, B, hc_discounted, tilde_MU, tilde_s){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  # slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  # clShare <- (1-slShare)
  
  # hc_new <- hc
  # hc_discounted <- hc_dis
  
  ##### Here I am trying to compute the discounted holding costs from the Naive expectations formulation.
  ##### This could be not the correct way of doing (since I promised rational expectations) but this is the best we can do
  # hc_new <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1)))
  # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
  # B <- ps - g * (beta^3) * Eps3 + hc_discounted
  ####### THE ABOVE IS NOT WORKING. CONVERGING VERY QUICKLY #####
  
  # hc_new <- (pc - beta * Epc1 + g * (beta^3) * Eps3) * (1/(1 + beta * (g * gamma0 + beta * g * gamma1)))
  # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
  # B <- ps - g * (beta^3) * Eps3 + hc_discounted
  
  F1 <- sl - A * ((exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))/(1 + (exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))))
  
  F2 <- cl  - A * (1/(1+ exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s)))
  
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  # F <- F1^2 + F2^2 + F3^2
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  # F <- F1^2 + F2^2
  
  return(F)
  
}

getSlClA <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, adjF, Dshock){
  
  estQ <- BBoptim(par = k, fn = estQFunction, tilde_MU = params[1], tilde_s = params[2],
                  ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3)
  
  k3_est <- estQ$par
  
  # if(k3_est < 0){
  #   k3_est <- 0.01
  # }
  
  slNew <- (g * K1 - k3_est)
  
  clNew <- k3_est * (delta^4) * (1/(gamma_k3^5)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) )
  
  ANew <- (slNew + clNew) * (1/adjF)
  
  return(c(slNew, clNew, ANew, k3_est))
  
}

getPsPcEpsEpc <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, ANew, params){
  
  psNew <- PsM
  pcNew <- PcM
  
  psNew_lo <- psNew  - 0.27667
  pcNew_lo <- pcNew - 0.29217
  
  # psNew_up <- psNew + 0.28000
  # pcNew_up <- pcNew + 0.25933
  
  psNew_up <- psNew + 0.10929
  pcNew_up <- pcNew + 0.080153
  
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
  
  psNew_expected_lo <- psNew_expected - 0.1
  
  psNew_expected_up <- psNew_expected + 0.1
  
  pcNew_expected_lo <- pcNew_expected - 0.1
  
  pcNew_expected_up <- pcNew_expected + 0.1
  
  if(pcNew_expected_lo < 0){
    pcNew_expected_lo <- pcNew_expected
  }
  
  if(ps_expected_lo < 0){
    psNew_expected_lo <- psNew_expected
  }
  
  p <- c(psNew, pcNew, psNew_expected, pcNew_expected)
  
  lo <- c(psNew_lo, pcNew_lo, psNew_expected_lo, pcNew_expected_lo)
  up <- c(psNew_up, pcNew_up, psNew_expected_up, pcNew_expected_up)
  
  estPNew <- BBoptim(par = p, fn = estPFunction, sl = SlNew, cl = ClNew, A = ANew, 
                     B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                     tilde_MU = params[1], tilde_s = params[2])
  
  ps1N <- estPNew$par[1]
  pc1N <- estPNew$par[2]
  ps_expected1N <- estPNew$par[3]
  pc_expected1N <- estPNew$par[4]
  
  hc1N <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    (beta * pc_expected1N + g * (beta^3) * ps_expected1N - pc1N)
  
  return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}



##### Projections

proj_adjFac <- adjFactor_New

proj_adjFac <- adjFactor

proj_muTildes <- mu_Tildes_MMN
proj_sTildes <- s_Tildes_MMN
proj_PricesCosts <- Reduce(function(...) merge(...), list(EQestPSN,EQestPCN,EQestHCN, EQestEPSN, EQestEPCN))

# proj_muTildes1 <- mu_Tildes_MM_itr
# proj_sTildes1 <- s_Tildes_MM_itr
# proj_PricesCosts1 <- Reduce(function(...) merge(...), list(ITRestPS,ITRestPC,ITRestHC, ITRestEPS, ITRestEPC))

#### We use the following to get the t+1 supply of the fed cattle
##### See the work in the binder
proj_K_t <- Stock %>% transmute(Year = Year, K = K)
proj_A <- A_quant
proj_Dshocks <- stateVars %>% transmute(Year = Year, dShock = Shock)

proj_AllDF_EQ <- Reduce(function(...) merge(...), 
                   list(proj_K_t,proj_A,proj_Dshocks,proj_adjFac,proj_muTildes,proj_sTildes,proj_PricesCosts, 
                        dressedWeights_sl_cl))

# proj_AllDF_CONV <- Reduce(function(...) merge(...), 
#                         list(proj_K_t,proj_A,proj_Dshocks,proj_adjFac,proj_muTildes1,proj_sTildes1,proj_PricesCosts1, 
#                              dressedWeights_sl_cl))



############ NEED TO FIGURE OUT HOW TO WRITE k7, k8, k9 in terms of k3

replacementHeifers_k3 <- replacementInventory %>% arrange(Year)

replacementHeifers_k3 <- replacementHeifers_k3 %>% mutate(ratio = k3/lag(k3))

summary(replacementHeifers_k3$ratio)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.6766  0.9703  1.0104  1.0062  1.0451  1.1658       1

#### Using the above relationship i.e., relationship between the replacement heifers at t to t-1. 
#### The median is 1.0104. So I will fix this and use this as the relationship. 

# gamma_k3 <- 1.0104
# eta_k3  

modelParamsEQ <- tail(proj_AllDF_EQ, n=1)

modelParamsCONV <- tail(proj_AllDF_CONV, n=1)

################################## CHANGE THE ADJUSTMENT FACTOR #######################################

slaughterAvg <- modelParamsEQ$Slaughter_avg

MUtilde <- modelParamsEQ$muMean
Stilde <- modelParamsEQ$sMean

psM <- modelParamsEQ$psMean
pcM <- modelParamsEQ$pcMean
hcM <- modelParamsEQ$hcMean

EpsM <- modelParamsEQ$EpsMean
EpcM <- modelParamsEQ$EpcMean

capA <- modelParamsEQ$A
capK <- modelParamsEQ$K

adjF <- modelParamsEQ$AdjFactor


# stock_K <- beefInventory %>% arrange(Year) %>% filter(Year >=2017)

# beefINV_FORECAST

nProj <- nrow(beefINV_FORECAST) + 1

proj_Q_P <- data.frame(Year = numeric(nProj), Ps = numeric(nProj), Pc = numeric(nProj), 
                       EPs = numeric(nProj), EPc = numeric(nProj), Hc = numeric(nProj), 
                       Sl = numeric(nProj), Cl = numeric(nProj), A = numeric(nProj))

k_old <- 0

####### Here we are projecting the prices and quantities from the forecasted capK or total stock
for(i in 1:nrow(proj_Q_P)){

  # i <- 1
  
  getYear <- beefINV_FORECAST$Year[i]
  
  k0 <- calf_crop_proj %>% filter(Year <= getYear - 2 & Year >= getYear - 8) %>% select(k0) %>% as.vector()
  
  k0s <- c(k0$k0)
  
  k <- k_old
  K1 <- (capK * slaughterAvg)/1000000000

  Qs <- getSlClA(params = c(MUtilde, Stilde), PsM = psM, PcM = pcM, K1 = K1,
                 k = k, CapA = capA, gamma_k3 = gamma_k3, adjF = adjF)
  slNew <- Qs[1]
  clNew <- Qs[2]
  ANew <- Qs[3]
  
  k_old <- Qs[4]

  Ps <- getPsPcEpsEpc(PsM = psM, PcM = pcM, EPsM = EpsM, EPcM = EpcM,
                      HcM = hcM, SlNew = slNew, ClNew = clNew, ANew = ANew, 
                      params = c(MUtilde, Stilde))
  psM <- Ps[1]
  pcM <- Ps[2]
  hcM <- Ps[3]
  EpsM <- Ps[4]
  EpcM <- Ps[5]

  proj_Q_P$Ps[i] <- psM
  proj_Q_P$Pc[i] <- pcM
  proj_Q_P$Hc[i] <- hcM
  proj_Q_P$EPs[i] <- EpsM
  proj_Q_P$EPc[i] <- EpcM

  proj_Q_P$Sl[i] <- slNew
  proj_Q_P$Cl[i] <- clNew
  proj_Q_P$A[i] <- ANew

  proj_Q_P$Year[i] <- beefINV_FORECAST$Year[i]

  if(i>1){
    proj_Q_P$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }

  capA <- ANew

  capK <- beefINV_FORECAST$K[i]
  
  # EpsM <- sum(as.numeric(psM) * fedMeshCheb)
  # EpcM <- sum(as.numeric(pcM) * cullMeshCheb)
  
  # params_Mu_S <- optParamFunction(sl = slNew, cl = clNew, ps = psM, pc = pcM, thetas = c(1,1))
  # 
  # MUtilde <- params_mu_s[1]
  # Stilde <- params_mu_s[2]

}
  


nProj <- nProj
proj_Q_P_lo <- data.frame(Year = numeric(nProj), Ps_lo = numeric(nProj), Pc_lo = numeric(nProj),
                          EPs_lo = numeric(nProj), EPc_lo = numeric(nProj), Hc_lo = numeric(nProj),
                          Sl_lo = numeric(nProj), Cl_lo = numeric(nProj), A_lo = numeric(nProj))

####### Here we are projecting the prices and quantities from the forecasted capK or total stock lower 95%
psM_lo <- modelParamsEQ$psMean
pcM_lo <- modelParamsEQ$pcMean
hcM_lo <- modelParamsEQ$hcMean

EpsM_lo <- modelParamsEQ$EpsMean
EpcM_lo <- modelParamsEQ$EpcMean

capA_lo <- modelParamsEQ$A
capK_lo <- modelParamsEQ$K

k_old <- 0

for(i in 1:nrow(proj_Q_P_lo)){
  
  # i <- 5
  
  k <- k_old
  K1_lo <- (capK_lo * slaughterAvg)/1000000000
  
  Qs_lo <- getSlClA(params = c(MUtilde, Stilde), PsM = psM_lo, PcM = pcM_lo, K1 = K1_lo,
                    k = k, CapA = capA_lo, gamma_k3 = gamma_k3, adjF = adjF)
  
  slNew_lo <- Qs_lo[1]
  clNew_lo <- Qs_lo[2]
  ANew_lo <- Qs_lo[3]
  
  k_old <-  Qs_lo[4]
  
  Ps_lo <- getPsPcEpsEpc(PsM = psM_lo, PcM = pcM_lo, EPsM = EpsM_lo, EPcM = EpcM_lo,
                         HcM = hcM_lo, SlNew = slNew_lo, ClNew = clNew_lo, ANew = ANew_lo,
                         params = c(MUtilde, Stilde))
  
  psM_lo <- Ps_lo[1]
  pcM_lo <- Ps_lo[2]
  hcM_lo <- Ps_lo[3]
  EpsM_lo <- Ps_lo[4]
  EpcM_lo <- Ps_lo[5]
  
  proj_Q_P_lo$Ps_lo[i] <- psM_lo
  proj_Q_P_lo$Pc_lo[i] <- pcM_lo
  proj_Q_P_lo$Hc_lo[i] <- hcM_lo
  proj_Q_P_lo$EPs_lo[i] <- EpsM_lo
  proj_Q_P_lo$EPc_lo[i] <- EpcM_lo
  
  proj_Q_P_lo$Sl_lo[i] <- slNew_lo
  proj_Q_P_lo$Cl_lo[i] <- clNew_lo
  proj_Q_P_lo$A_lo[i] <- ANew_lo
  
  proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_lo <- ANew_lo
  capK_lo <- beefINV_FORECAST$lo95[i]
  
  EpsM_lo <- sum(as.numeric(psM_lo) * fedMeshCheb)
  EpcM_lo <- sum(as.numeric(pcM_lo) * cullMeshCheb)
  
  # params_Mu_S <- optParamFunction(sl = slNew_lo, cl = clNew_lo,
  #                                 ps = psM_lo, pc = pcM_lo, thetas = c(1,1))
  # 
  # MUtilde <- params_mu_s[1]
  # Stilde <- params_mu_s[2]
  
}

####### Here we are projecting the prices and quantities from the forecasted capK or total stock upper 95%
proj_Q_P_up <- data.frame(Year = numeric(nProj), Ps_up = numeric(nProj), Pc_up = numeric(nProj),
                          EPs_up = numeric(nProj), EPc_up = numeric(nProj), Hc_up = numeric(nProj),
                          Sl_up = numeric(nProj), Cl_up = numeric(nProj), A_up = numeric(nProj)) 

psM_up <- modelParamsEQ$psMean
pcM_up <- modelParamsEQ$pcMean
hcM_up <- modelParamsEQ$hcMean

EpsM_up <- modelParamsEQ$EpsMean
EpcM_up <- modelParamsEQ$EpcMean

capA_up <- modelParamsEQ$A
capK_up <- modelParamsEQ$K

k_old <- 0

for(i in 1:nrow(proj_Q_P_up)){
  
  # i <- 1
  
  k <- k_old
  
  K1_up <- (capK_up * slaughterAvg)/1000000000
  
  Qs_up <- getSlClA(params = c(MUtilde, Stilde), PsM = psM_up, PcM = pcM_up, K1 = K1_up,
                    k = k, CapA = capA_up, gamma_k3 = gamma_k3, adjF = adjF)
  slNew_up <- Qs_up[1]
  clNew_up <- Qs_up[2]
  ANew_up <- Qs_up[3]
  
  k_old <-  Qs_up[4]
  
  Ps_up <- getPsPcEpsEpc(PsM = psM_up, PcM = pcM_up, EPsM = EpsM_up, EPcM = EpcM_up,
                         HcM = hcM_up, SlNew = slNew_up, ClNew = clNew_up, ANew = ANew_up,
                         params = c(MUtilde, Stilde))
  
  psM_up <- Ps_up[1]
  pcM_up <- Ps_up[2]
  hcM_up <- Ps_up[3]
  EpsM_up <- Ps_up[4]
  EpcM_up <- Ps_up[5]
  
  proj_Q_P_up$Ps_up[i] <- psM_up
  proj_Q_P_up$Pc_up[i] <- pcM_up
  proj_Q_P_up$Hc_up[i] <- hcM_up
  proj_Q_P_up$EPs_up[i] <- EpsM_up
  proj_Q_P_up$EPc_up[i] <- EpcM_up
  
  proj_Q_P_up$Sl_up[i] <- slNew_up
  proj_Q_P_up$Cl_up[i] <- clNew_up
  proj_Q_P_up$A_up[i] <- ANew_up
  
  proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_up <- ANew_up
  capK_up <- beefINV_FORECAST$hi95[i]
  
  EpsM_up <- sum(as.numeric(psM_up) * fedMeshCheb)
  EpcM_up <- sum(as.numeric(pcM_up) * cullMeshCheb)
  
}
      
      
      

         
         
##### USING MEDIANS

PQs_MEDIANS <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)

PQs_MEDIANS_PS <- PQs_MEDIANS %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_MEDIANS_PC <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up)
PQs_MEDIANS_SL <- PQs_MEDIANS %>% select(Year, Sl_lo, Sl, Sl_up)
PQs_MEDIANS_CL <- PQs_MEDIANS %>% select(Year, Cl_lo, Cl, Cl_up)
PQs_MEDIANS_A  <- PQs_MEDIANS %>% select(Year, A_lo, A, A_up) 



PQs_MEDIANS_EP  <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)


PQs_MEDIANS_PS_EP <- PQs_MEDIANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_MEDIANS_PC_EP <- PQs_MEDIANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)
PQs_MEDIANS_SL_EP <- PQs_MEDIANS_EP %>% select(Year, Sl_lo, Sl, Sl_up)
PQs_MEDIANS_CL_EP <- PQs_MEDIANS_EP %>% select(Year, Cl_lo, Cl, Cl_up)
PQs_MEDIANS_A_EP  <- PQs_MEDIANS_EP  %>% select(Year, A_lo, A, A_up)      
      
      
##### USING MEANS  

PQs_MEANS <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)

PQs_MEANS_PS  <- PQs_MEANS  %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_MEANS_PC  <- PQs_MEANS  %>% select(Year, Pc_lo, Pc, Pc_up)
PQs_MEANS_SL  <- PQs_MEANS  %>% select(Year, Sl_lo, Sl, Sl_up)
PQs_MEANS_CL  <- PQs_MEANS  %>% select(Year, Cl_lo, Cl, Cl_up)
PQs_MEANS_A  <- PQs_MEANS  %>% select(Year, A_lo, A, A_up)


PQs_MEANS_EP  <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)    
      

PQs_MEANS_PS_EP <- PQs_MEANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_MEANS_PC_EP <- PQs_MEANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)
PQs_MEANS_SL_EP <- PQs_MEANS_EP %>% select(Year, Sl_lo, Sl, Sl_up)
PQs_MEANS_CL_EP <- PQs_MEANS_EP %>% select(Year, Cl_lo, Cl, Cl_up)
PQs_MEANS_A_EP  <- PQs_MEANS_EP  %>% select(Year, A_lo, A, A_up)













