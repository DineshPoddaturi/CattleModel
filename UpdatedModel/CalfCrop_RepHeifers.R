
##### In this work we get the relationship between the calf crop and the replacement heifers
##### So basically I run a simple regression of k_{3,t+1} = gamma k_{3,t} + eta k_{0,t-3}

calf_crop_proj <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year)

replacementInventory_proj <- replacementInventory %>% arrange(Year) 

summary( lead(replacementInventory_proj$k3)-replacementInventory_proj$k3)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -3073100  -194100    64200    14367   264575   849000        1

summary(replacementInventory_proj$k3)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2569000 4859000 5623500 5591931 6363200 9503700  

k3_lo_rep <- min(replacementInventory_proj$k3)

CC_RH <- merge(calf_crop_proj, replacementInventory_proj, by="Year",all=TRUE)


#### Here we run regression without intercept. The rational for this is if both of the explanatory variables () are zero 
#### then the replacement heifers is also zero. 
CC_RH_Fit <- lm(formula = lead(k3,1) ~ k3 + lag(k0,3) - 1 , data = CC_RH)

fitSummary <- summary(CC_RH_Fit)

# plot(CC_RH_Fit, 3)
# 
# plot(CC_RH_Fit$resid ~ CC_RH_Fit$fitted.values)
# #add horizental line from 0
# abline(h = 0, lty = 2)
# require(car)
# avPlots(CC_RH_Fit)
# 
# infIndexPlot(CC_RH_Fit)

# int_k3 <- fitSummary$coefficients[1]
gamma_k3 <- fitSummary$coefficients[1]
eta_k3 <- fitSummary$coefficients[2]


###### Functions to compute the k0s, replacement heifers, fed cattle, cull cows, total demand

get_k0s <- function(Yr, lag, calfCrop){
  
  if(nrow(calfCrop %>% filter( Year == getYear - lag  ) %>% select(k0)) == 0){
    k0 <- 0 
  }else{
    k0 <- calfCrop %>% filter( Year == Yr - lag  ) %>% select(k0) %>% unlist()
  }
  
  return(k0)
  
}


get_k0s_Global <- function(proj_Q_P, beefINV_FORECAST, calfCrop){
  
  k0s_df <- data.frame(Year = numeric(nProj), k02 = numeric(nProj) , k03 = numeric(nProj), 
                       k04 = numeric(nProj), k05 = numeric(nProj), k06 = numeric(nProj), 
                       k07 = numeric(nProj), k08 = numeric(nProj))
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
    
    # if(i>1){
    # 
    #   if( k02[i] == 0 ){
    #     k02[i] <- k02[i-1]
    #   }
    #   if( k03[i] == 0 ){
    #     k03[i] <- k03[i-1]
    #   }
    #   if( k04[i] == 0 ){
    #     k04[i] <- k04[i-1]
    #   }
    #   if( k05[i] == 0 ){
    #     k05[i] <- k05[i-1]
    #   }
    #   if( k06[i] == 0 ){
    #     k06[i] <- k06[i-1]
    #   }
    #   if( k07[i] == 0 ){
    #     k07[i] <- k07[i-1]
    #   }
    #   if( k08[i] == 0 ){
    #     k08[i] <- k08[i-1]
    #   }
    # 
    # }
    
    k0s_df[i,] <- c(getYear, k02[i], k03[i], k04[i], k05[i], k06[i], k07[i], k08[i])
    
  }
  
  return(k0s_df)
}
  
getSlClA_test <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, 
                          eta_k3 , int_k3, adjF, Dshock, k0s, slAvg, clAvg){
  
  estQ <- BBoptim(par = k, fn = estQFunction_test, tilde_MU = params[1], 
                  tilde_s = params[2], ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3, 
                  eta_k3 = eta_k3, int = int_k3, k0s = k0s, slAvg = slAvg, clAvg = clAvg)
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  k3_est <- estQ$par
  
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
  
  ANew <- (slNew + clNew) * (1/adjF)
  
  k3_est_Head <- estQ$par
  
  # slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  # ANew <- (((g * K1 - k3_est) * slAvg)/1000000000) * (1/slShare)
  # 
  # slNew <- ANew * slShare * adjF
  # clNew <- ANew * (1-slShare) * adjF
  
  return(c(slNew, clNew, ANew, k3_est, k3_est_Head))
  
}  

estQFunction_test <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3, 
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



###### Retreiving the model parameters

proj_adjFac <- adjFactor_New

proj_adjFac <- adjFactor

proj_muTildes <- mu_Tildes_MMNIII
proj_sTildes <- s_Tildes_MMNIII
proj_PricesCosts <- Reduce(function(...) merge(...), list(EQestPSNIII,EQestPCNIII,
                                                          EQestHCNIII, EQestEPSNIII, EQestEPCNIII))

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


modelParamsEQ <- tail(proj_AllDF_EQ, n=1)

modelParamsCONV <- tail(proj_AllDF_CONV, n=1)

slaughterAvg <- modelParamsEQ$Slaughter_avg
cullAvg <-  modelParamsEQ$Cull_avg

MUtilde <- modelParamsEQ$muMedian
Stilde <- modelParamsEQ$sMedian

psM <- modelParamsEQ$psMedian
pcM <- modelParamsEQ$pcMedian
hcM <- modelParamsEQ$hcMedian

EpsM <- modelParamsEQ$EpsMedian
EpcM <- modelParamsEQ$EpcMedian

capA <- modelParamsEQ$A
capK <- modelParamsEQ$K

adjF <- modelParamsEQ$AdjFactor

nProj <- nrow(beefINV_FORECAST) + 1

###### Here I define the dataframe for the projections of quantities, prices
proj_Q_P <- data.frame(Year = numeric(nProj), Ps = numeric(nProj), Pc = numeric(nProj), 
                       EPs = numeric(nProj), EPc = numeric(nProj), Hc = numeric(nProj), 
                       Sl = numeric(nProj), Cl = numeric(nProj), A = numeric(nProj),
                       repHeif = numeric(nProj), repHeif_Head = numeric(nProj))

proj_Q_P_up <- data.frame(Year = numeric(nProj), Ps_up = numeric(nProj), Pc_up = numeric(nProj),
                          EPs_up = numeric(nProj), EPc_up = numeric(nProj), Hc_up = numeric(nProj),
                          Sl_up = numeric(nProj), Cl_up = numeric(nProj), A_up = numeric(nProj),
                          repHeif_up = numeric(nProj), repHeif_Head_up = numeric(nProj)) 

proj_Q_P_lo <- data.frame(Year = numeric(nProj), Ps_lo = numeric(nProj), Pc_lo = numeric(nProj),
                          EPs_lo = numeric(nProj), EPc_lo = numeric(nProj), Hc_lo = numeric(nProj),
                          Sl_lo = numeric(nProj), Cl_lo = numeric(nProj), A_lo = numeric(nProj),
                          repHeif_lo = numeric(nProj), repHeif_Head_lo = numeric(nProj))


##### Using the projected stock, I am generating the new born in each year. So basically multiply birth rate with the 
##### stock. 
calf_crop_proj1 <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * Kcast) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

calf_crop_proj1_LO <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * lo95) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

calf_crop_proj1_UP <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * hi95) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

##### Here I join the data and the projected 
calf_crop_proj_N <- rbind(calf_crop_proj, calf_crop_proj1)

calf_crop_proj_N_LO <- rbind(calf_crop_proj, calf_crop_proj1_LO)

calf_crop_proj_N_UP <- rbind(calf_crop_proj, calf_crop_proj1_UP)

k0s_df <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N)
k0s_df_LO <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N_LO)
k0s_df_UP <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N_UP)

####### Here we are projecting the prices and quantities from the forecasted capK or total stock and the calf crop

### k_old is the replacement heifers. We start with zero (almost never true), but we let the program and data to give
### the optimal replacement heifers. This mostly depends on the demand.
k_old <- 0

for(i in 1:nrow(proj_Q_P)){
  
  # i <- 1
  
  #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
  ### the optimal replacement heifers. 
  k <- 0
  
  K1 <- capK
  
  k0s <- k0s_df[i,-1]
  
  # gamma <- gamma_k3
  # eta <- eta_k3
  # 
  # estQ <- BBoptim(par = k,fn = estQFunction_test, tilde_MU = MUtilde,
  #                 tilde_s = Stilde,
  #                 ps = psM, pc = pcM, K1 = K1, A = capA, gamma_k3 = gamma_k3, eta_k3 = eta_k3,
  #                 int = 0, k0s = k0s,
  #                 slAvg = slaughterAvg, clAvg = cullAvg)
  # 
  # k3est <- estQ$par
  # 
  # slNew <- (g * K1 - k3est)
  # 
  # k08 <- k0s$k08
  # k07 <- k0s$k07
  # k06 <- k0s$k06
  # k05 <- k0s$k05
  # k04 <- k0s$k04
  # k03 <- k0s$k03
  # k02 <- k0s$k02
  # 
  # clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
  #   (k3est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) 
  # 
  # slNew <- ((slNew * slaughterAvg)/1000000000)
  # clNew <- ((clNew * cullAvg)/1000000000)
  # 
  # Anew <- (slNew + clNew) * (1/adjF)
  
  int_k3 <- 0
  
  Qs <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM, PcM = pcM, K1 = K1,
                 k = k, CapA = capA, gamma_k3 = gamma_k3, 
                 eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF, k0s = k0s,
                 slAvg = slaughterAvg, clAvg = cullAvg)
  
  slNew <- Qs[1]
  clNew <- Qs[2]
  ANew <- Qs[3]

  k_old <- Qs[4]
  
  k_old_Head <- Qs[5]
  
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
  proj_Q_P$repHeif[i] <- k_old
  proj_Q_P$repHeif_Head[i] <- k_old_Head
  
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


####### Here we are projecting the prices and quantities from the forecasted capK or total stock upper 95%
psM_up <- modelParamsEQ$psMedian
pcM_up <- modelParamsEQ$pcMedian
hcM_up <- modelParamsEQ$hcMedian

EpsM_up <- modelParamsEQ$EpsMedian
EpcM_up <- modelParamsEQ$EpcMedian

capA_up <- modelParamsEQ$A
capK_up <- modelParamsEQ$K

# k_old_up <- 0

for(i in 1:nrow(proj_Q_P_up)){
  
  # i <- 1
  
  k <- 0
  
  K1_up <- capK_up 
  
  k0s <- k0s_df_UP[i,-1]
  
  int_k3 <- 0
  
  Qs_up <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM_up, PcM = pcM_up, K1 = K1_up,
                         k = k, CapA = capA_up, gamma_k3 = gamma_k3, eta_k3 = eta_k3 , int_k3 = int_k3, 
                         adjF = adjF, k0s = k0s,slAvg = slaughterAvg, clAvg = cullAvg)
  slNew_up <- Qs_up[1]
  clNew_up <- Qs_up[2]
  ANew_up <- Qs_up[3]
  
  k_old_up <-  Qs_up[4]
  
  k_old_head_up <- Qs_up[5]
  
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
  proj_Q_P_up$repHeif_up[i] <- k_old_up
  proj_Q_P_up$repHeif_Head_up[i] <- k_old_head_up
  
  proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_up <- ANew_up
  capK_up <- beefINV_FORECAST$hi95[i]
  
}

####### Here we are projecting the prices and quantities from the forecasted capK or total stock lower 95%
psM_lo <- modelParamsEQ$psMedian
pcM_lo <- modelParamsEQ$pcMedian
hcM_lo <- modelParamsEQ$hcMedian

EpsM_lo <- modelParamsEQ$EpsMedian
EpcM_lo <- modelParamsEQ$EpcMedian

capA_lo <- modelParamsEQ$A
capK_lo <- modelParamsEQ$K

k_old_lo <- 0

for(i in 1:nrow(proj_Q_P_lo)){
  
  # i <- 1
  
  k <- 0
  
  K1_lo <- capK_lo
  
  k0s <- k0s_df_LO[i,-1]
  
  int_k3 <- 0
  
  Qs_lo <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM_lo, PcM = pcM_lo, K1 = K1_lo,
                k = k,CapA = capA_lo, gamma_k3 = gamma_k3, eta_k3 = eta_k3 ,
                int_k3 = int_k3, adjF = adjF, k0s = k0s, slAvg = slaughterAvg, clAvg = cullAvg)
  
  slNew_lo <- Qs_lo[1]
  clNew_lo <- Qs_lo[2]
  ANew_lo <- Qs_lo[3]
  
  k_old_lo <-  Qs_lo[4]
  
  k_old_head_lo <-  Qs_lo[5]
  
  
  ###### changing the quantities such that they match the historical quantities. Don't know if this is required.
  ###### If I change this the prices are projected properly i.e., fed cattle price is always greater than cull cow price
  
  while(clNew_lo < 1.01){
    clNew_lo <- clNew_lo + 0.01
  }
  while(slNew_lo < 19.01){
    slNew_lo <- slNew_lo + 0.01
  }
  
  ANew_lo <- (slNew_lo + clNew_lo) * (1/adjF)
  
  Ps_lo <- getPsPcEpsEpc(PsM = psM_lo, PcM = pcM_lo, EPsM = EpsM_lo, EPcM = EpcM_lo,
                         HcM = hcM_lo, SlNew = slNew_lo, ClNew = clNew_lo, ANew = ANew_lo,
                         params = c(MUtilde, Stilde))
  
  psM_lo <- Ps_lo[1]
  pcM_lo <- Ps_lo[2]
  hcM_lo <- Ps_lo[3]
  EpsM_lo <- Ps_lo[4]
  EpcM_lo <- Ps_lo[5]
  
  ####### Need to figure this out
    # counter <- 0
    # while(psM_lo < pcM_lo){
    # 
    #   if(counter == 0){
    #     psM_lo <- proj_Q_P_lo$Ps_lo[i-1] + 0.1
    #     pcM_lo <- proj_Q_P_lo$Pc_lo[i-1] - 0.1
    #   }else{
    #     psM_lo <- psM_lo + 0.1
    #     pcM_lo <- pcM_lo - 0.1
    #   }
    # 
    #   Qs_lo <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM_lo, PcM = pcM_lo, K1 = K1_lo,
    #                          k = k,CapA = capA_lo, gamma_k3 = gamma_k3, eta_k3 = eta_k3 ,
    #                          int_k3 = int_k3, adjF = adjF, k0s = k0s, slAvg = slaughterAvg, clAvg = cullAvg)
    # 
    #   slNew_lo <- Qs_lo[1]
    #   clNew_lo <- Qs_lo[2]
    #   ANew_lo <- Qs_lo[3]
    # 
    #   k_old_lo <-  Qs_lo[4]
    # 
    # 
    #   Ps_lo <- getPsPcEpsEpc(PsM = psM_lo, PcM = pcM_lo, EPsM = EpsM_lo, EPcM = EpcM_lo,
    #                          HcM = hcM_lo, SlNew = slNew_lo, ClNew = clNew_lo, ANew = ANew_lo,
    #                          params = c(MUtilde, Stilde))
    #   psM_lo <- Ps_lo[1]
    #   pcM_lo <- Ps_lo[2]
    #   hcM_lo <- Ps_lo[3]
    #   EpsM_lo <- Ps_lo[4]
    #   EpcM_lo <- Ps_lo[5]
    # 
    #   counter <- counter + 1
    # 
    # }
  
  proj_Q_P_lo$Ps_lo[i] <- psM_lo
  proj_Q_P_lo$Pc_lo[i] <- pcM_lo
  proj_Q_P_lo$Hc_lo[i] <- hcM_lo
  proj_Q_P_lo$EPs_lo[i] <- EpsM_lo
  proj_Q_P_lo$EPc_lo[i] <- EpcM_lo
  
  proj_Q_P_lo$Sl_lo[i] <- slNew_lo
  proj_Q_P_lo$Cl_lo[i] <- clNew_lo
  proj_Q_P_lo$A_lo[i] <- ANew_lo
  proj_Q_P_lo$repHeif_lo[i] <- k_old_lo
  proj_Q_P_lo$repHeif_Head_lo[i] <- k_old_head_lo
  
  proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_lo <- ANew_lo
  capK_lo <- beefINV_FORECAST$lo95[i]
  
}

################### Here we join all the projections 

PQs_MEDIANS <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)

PQs_MEDIANS_PS <- PQs_MEDIANS %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_MEDIANS_PC <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up)

PQs_MEDIANS_SL <- PQs_MEDIANS %>% select(Year, Sl_lo, Sl, Sl_up) %>% round(3)
PQs_MEDIANS_CL <- PQs_MEDIANS %>% select(Year, Cl_lo, Cl, Cl_up) %>% round(3)
PQs_MEDIANS_A  <- PQs_MEDIANS %>% select(Year, A_lo, A, A_up) %>% round(3)

PQs_MEDIANS_PS <- PQs_MEDIANS %>% select(Year, Ps_lo, Ps, Ps_up) %>% transmute(Year = Year, Ps_LO = Ps_lo * 100, 
                                                                               Ps = Ps * 100, Ps_UP = Ps_up * 100)
PQs_MEDIANS_PC <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up) %>% transmute(Year = Year, Pc_LO = Pc_up * 100, 
                                                                               Pc = Pc * 100, Pc_UP = Pc_lo * 100)


PQs_MEDIANS_TS <- merge(PQs_MEDIANS_SL, PQs_MEDIANS_CL) %>%
  transmute(Year = Year, TS_lo = Sl_lo + Cl_lo, TS = Sl + Cl, TS_up = Sl_up + Cl_up)


