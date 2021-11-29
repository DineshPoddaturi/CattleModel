
##### In this work we get the relatiomnship between the calf crop and the replacement heifers
##### Sp basically I run a simple regression of k_{3,t+1} = gamma k_{3,t} + eta k_{0,t-3}

calf_crop_proj <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year)

replacementInventory_proj <- replacementInventory %>% arrange(Year) 

CC_RH <- merge(calf_crop_proj, replacementInventory_proj, by="Year",all=TRUE)

CC_RH_Fit <- lm(formula = lead(k3,1) ~ k3 + lag(k0,3) - 1, data = CC_RH)

fitSummary <- summary(CC_RH_Fit)


gamma_k3 <- fitSummary$coefficients[1]
eta <- fitSummary$coefficients[2]

predict(CC_RH_Fit)



# cowsSlaughtered <- merge(cowsSlaughtered,dressedWeights)  %>% transmute(Year = Year, Cowshead = CowsHead, cull_meat = CowsHead * Cows_avg )
# heifersSlaughtered <- merge(heifersSlaughtered,dressedWeights)  %>% transmute(Year = Year, HeifersHead = HeifersHead, heifer_meat = HeifersHead * Heifers_avg)
# steersSlaughtered <- merge(steersSlaughtered,dressedWeights)  %>% transmute(Year = Year, SteersHead = SteersHead, steer_meat = SteersHead * Steers_avg)





  
getSlClA_test <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, eta_k3 , adjF, Dshock, k0s, slAvg, clAvg){
  
  estQ <- BBoptim(par = k, fn = estQFunction_test, tilde_MU = params[1], tilde_s = params[2],
                  ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3, eta = eta_k3, k0s = k0s,
                  slAvg = slAvg, clAvg = clAvg)
  
  k08 <- k0s[7]
  k07 <- k0s[6]
  k06 <- k0s[5]
  k05 <- k0s[4]
  k04 <- k0s[3]
  k03 <- k0s[2]
  k02 <- k0s[1]
  
  
  k3_est <- estQ$par
  
  slNew <- ((g * K1 - k3_est) * slAvg)/1000000000
  
  gamma <- gamma_k3
  eta <- eta_k3
  
  clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) * 
    (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) - 
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07)
  
  clNew <- (clNew * clAvg)/1000000000
  
  ANew <- (slNew + clNew) * (1/adjF)
  
  k3_est <- (k3_est * slAvg)/1000000000
  
  return(c(slNew, clNew, ANew, k3_est))
  
}  

estQFunction_test <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3, eta_k3 , k0s, slAvg, clAvg){
  
  k3t2 <- k
  
  k08 <- k0s[7]
  k07 <- k0s[6]
  k06 <- k0s[5]
  k05 <- k0s[4]
  k04 <- k0s[3]
  k03 <- k0s[2]
  k02 <- k0s[1]
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  gamma <- gamma_k3
  eta <- eta_k3
  
  slHead <- (A * slShare) * (1000000000/slAvg)  
  clHead <- (A * clShare) * (1000000000/clAvg)
  
  F1 <- g * K1 - k3t2 - slHead
  F2 <-  ( ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) * 
    (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) - 
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) ) - clHead
  
  F <- F1^2 + F2^2
  
}




get_k0s <- function(getYear){
  
  
  k02 <- calf_crop_proj %>% filter( Year == getYear - 2  ) %>% select(k0) %>% as.list()
  k03 <- calf_crop_proj %>% filter( Year == getYear - 3  ) %>% select(k0) %>% as.list()
  k04 <- calf_crop_proj %>% filter( Year == getYear - 4  ) %>% select(k0) %>% as.list()
  k05 <- calf_crop_proj %>% filter( Year == getYear - 5  ) %>% select(k0) %>% as.list()
  k06 <- calf_crop_proj %>% filter( Year == getYear - 6  ) %>% select(k0) %>% as.list()
  k07 <- calf_crop_proj %>% filter( Year == getYear - 7  ) %>% select(k0) %>% as.list()
  k08 <- calf_crop_proj %>% filter( Year == getYear - 8  ) %>% select(k0) %>% as.list()
  
  return(as.data.frame(getYear, k02, k03, k04, k05, k06, k07, k08))
  
  # return(c(getYear, k02, k03, k04, k05, k06, k07, k08))
}



gamma_k3 <- fitSummary$coefficients[1]
eta_k3 <- fitSummary$coefficients[2]

modelParamsEQ <- tail(proj_AllDF_EQ, n=1)

modelParamsCONV <- tail(proj_AllDF_CONV, n=1)

################################## CHANGE THE ADJUSTMENT FACTOR #######################################

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

proj_Q_P <- data.frame(Year = numeric(nProj), Ps = numeric(nProj), Pc = numeric(nProj), 
                       EPs = numeric(nProj), EPc = numeric(nProj), Hc = numeric(nProj), 
                       Sl = numeric(nProj), Cl = numeric(nProj), A = numeric(nProj))

k_old <- 0


k0s_df <- data.frame(Year = numeric(nProj), k02 = numeric(nProj) , k03 = numeric(nProj), 
                     k04 = numeric(nProj), k05 = numeric(nProj), k06 = numeric(nProj), 
                     k07 = numeric(nProj), k08 = numeric(nProj))


for (i in 1:nrow(proj_Q_P)){
  
  i <- 2
  
  getYear <- beefINV_FORECAST$Year[i]
  
  k02[i] <- calf_crop_proj %>% filter( Year == getYear - 2  ) %>% select(k0) 
  k03[i] <- calf_crop_proj %>% filter( Year == getYear - 3  ) %>% select(k0) 
  k04[i] <- calf_crop_proj %>% filter( Year == getYear - 4  ) %>% select(k0) 
  k05[i] <- calf_crop_proj %>% filter( Year == getYear - 5  ) %>% select(k0) 
  k06[i] <- calf_crop_proj %>% filter( Year == getYear - 6  ) %>% select(k0)  
  k07[i] <- calf_crop_proj %>% filter( Year == getYear - 7  ) %>% select(k0)  
  k08[i] <- calf_crop_proj %>% filter( Year == getYear - 8  ) %>% select(k0) 
  
  
  if(i>1){
    
    if( length(k02[i]) == 0 ){
      k02[i]$k0 <- k02[i-1]$k0
    }
    if( length(k03[i]$k0) == 0 ){
      k03[i]$k0 <- k03[i-1]$k0
    }
    if( length(k04[i]$k0) == 0 ){
      k04[i]$k0 <- k04[i-1]$k0
    }
    if( length(k05[i]$k0) == 0 ){
      k05[i]$k0 <- k05[i-1]$k0
    }
    if( length(k06[i]$k0) == 0 ){
      k06[i]$k0 <- k06[i-1]$k0
    }
    if( length(k07[i]$k0) == 0 ){
      k07[i]$k0 <- k07[i-1]$k0
    }
    if( length(k08[i]$k0) == 0 ){
      k08[i]$k0 <- k08[i-1]$k0
    }
    
  }
  
  k0s_df[i,] <- c(getYear, k02[i]$k0, k03[i]$k0, k04[i]$k0, k05[i]$k0, k06[i]$k0, k07[i]$k0, k08[i]$k0)
  
  
}






####### Here we are projecting the prices and quantities from the forecasted capK or total stock
for(i in 1:nrow(proj_Q_P)){
  
  i <- 1
  
  getYear <- beefINV_FORECAST$Year[i]
  
  k02[i] <- calf_crop_proj %>% filter( Year == getYear - 2  ) %>% select(k0) 
  k03[i] <- calf_crop_proj %>% filter( Year == getYear - 3  ) %>% select(k0) 
  k04[i] <- calf_crop_proj %>% filter( Year == getYear - 4  ) %>% select(k0) 
  k05[i] <- calf_crop_proj %>% filter( Year == getYear - 5  ) %>% select(k0) 
  k06[i] <- calf_crop_proj %>% filter( Year == getYear - 6  ) %>% select(k0)  
  k07[i] <- calf_crop_proj %>% filter( Year == getYear - 7  ) %>% select(k0)  
  k08[i] <- calf_crop_proj %>% filter( Year == getYear - 8  ) %>% select(k0) 
  
  
  if(i>1){

    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }
    if( length(k02[i]$k0) == 0 ){
      k02[i] <- k02[i-1]
    }

  }
  
  k0s_df[i,] <- c(getYear, k02[i], k03[i], k04[i], k05[i], k06[i], k07[i], k08[i])
  
  k0s <- c(k0s_df[i,] %>% select(k02, k03, k04, k05, k06, k07, k08))
  
  k <- k_old
  K1 <- capK
  
  gamma <- gamma_k3
  eta <- eta_k3
  
  # estQ <- BBoptim(par = k, fn = estQFunction_test, tilde_MU = MUtilde, tilde_s = Stilde,
  #                 ps = psM, pc = pcM, K1 = K1, A = capA, gamma_k3 = gamma_k3, eta = eta_k3, k0s = k0s,
  #                 slAvg = slaughterAvg, clAvg = cullAvg)
  # 
  # k3est <- estQ$par
  # 
  # slNew <- (g * K1 - k3est)
  # 
  # gamma <- gamma_k3
  # eta <- eta_k3
  # 
  # clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) * 
  #   (k3est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) - 
  #   ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07)
  # 
  # slNew <- ((slNew * slaughterAvg)/1000000000)  
  # clNew <- ((clNew * cullAvg)/1000000000) 
  # 
  # Anew <- (slNew + clNew) * (1/adjF)
  
  Qs <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM, PcM = pcM, K1 = K1,
                 k = k, CapA = capA, gamma_k3 = gamma_k3, eta_k3 =eta_k3 , adjF = adjF, k0s = k0s,
                 slAvg = slaughterAvg, clAvg = cullAvg)
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