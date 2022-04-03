

##### Global functions
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
  
  while(pcNew_expected<0){
    pcNew_expected <- pcNew_expected + 0.01
  }
  
  while(psNew_expected<0){
    psNew_expected <- psNew_expected + 0.01
  }
  
  
  p <- c(psNew, pcNew, psNew_expected, pcNew_expected)
  
  lo <- c(psNew_lo, pcNew_lo, psNew_expected_lo, pcNew_expected_lo)
  up <- c(psNew_up, pcNew_up, psNew_expected_up, pcNew_expected_up)
  
  estPNew <- BBoptim(par = p, fn = estPFunction, sl = SlNew, cl = ClNew, A = ANew,
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

##### Depopulation scenarios

## 20% depopulation
# In this scenario, I remove 20% of the breeding stock from the inventory
# This would change the age distribution. So if I introduce the disease let's say 2009, then in that year I remove a%
# of all the animals. 

# repHeifersMin <- merge(proj_AllDF_EQ, Stock) %>% 
#   mutate(repHeifersLB = (Slaughter_avg * k3)/1000000000) %>% select(repHeifersLB) %>% 
#   summarise(minim = min(repHeifersLB, na.rm = T)) %>% unlist() %>% unname()

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


# ##### depopulation
# dePopR <- 90
# Stock2009_20 <- dePop(stock = Stock_2009, dePopRate = dePopR)
# Stock2009_20 <- rbind(Stock_2008L, Stock2009_20) %>% as.data.frame()


simOptimisticFMD <- function(calf_cropF, dePopR, modelParamsEQ_PreFMD, exports_preFMD, nn, Stock){
  
  ##### Now I have calf-crop until 2009
  calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year < 2009)
  calf_crop_PreFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR) 
  calf_crop_2009 <- calf_cropF %>% filter(Year == 2009) %>% transmute(Year = Year, k0 = (1-dePopR/100) * calfCrop)
  calf_crop_PostFMD <- rbind(calf_crop_PreFMD, calf_crop_2009)
  
  modelParamsEQ_PreFMD <- modelParamsEQ_PreFMD %>% filter(Year == 2009)
  
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
  
  
  exports_2009 <- exports_preFMD
  #### here I am getting export percentage by the meat capA_pre
  exports_2009_meat <- exports_2009 * (slaughterAvg_pre/1000000000)
  exports_percent <- round((exports_2009_meat/capA_pre) * 100,3)
  
  ##### Here I am getting exports percentage by the stocks capK_pre_meat
  capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
  exports_percentK <- round((exports_2009_meat/capK_pre_meat) * 100,3)
  ### After careful consideration I am using exports_percentK as the exports in the simulation
  
  beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn+1), K = numeric(nn+1), k3 = numeric(nn+1), 
                                          k4 = numeric(nn+1), k5 = numeric(nn+1), k6 = numeric(nn+1), 
                                          k7 = numeric(nn+1), k8 = numeric(nn+1), k9 = numeric(nn+1),
                                          k10 = numeric(nn+1))
  
  beefINV_FORECAST_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)
  
  
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
  
  psM_EqMed <- NULL
  pcM_EqMed <- NULL
  psM_EqMN <- NULL
  pcM_EqMN <- NULL
  
  K1 <- NULL
  clHeadDiff <- NULL
  slHeadDiff <- NULL
  headRatio <- NULL
  
  
  for(i in 1:nrow(proj_Q_P_PostFMD)){
    
    if(i>1){
      #Populating the younglings
      k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,],
                                             beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,],
                                             calfCrop = calf_crop_PostFMD)
      #Retrieving the previous years derived demand and total breeding stock
      capA_pre <- proj_Q_P_PostFMD$A[i-1]
      capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
    }
    
    #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
    ### the optimal replacement heifers.
    k <- 0
    K1[i] <- capK_pre
    k0s <- k0s_PostFMD[i,-1]
    int_k3 <- 0
    
    ### Here I assume a 5% decrease in domestic demand and the exports are banned 
    if(i==1){
      capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
      K1[i] <- capK_pre * (1 - (dePopR/100))
    }
    
    # Here I get the supply by passing the stocks and derived demand. This function will give us
    # the meat supply and also the replacement heifers
    Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = capA_pre, gamma_k3 = gamma_k3, 
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
    
    slNew <- Qs[1]
    clNew <- Qs[2]
    ANew <- Qs[3]
    
    ### I take the absolute value of the replacement heifers. This is to avoid negative values. It's written inside the 
    ### function. 
    ### Note: by making the negative values positive we can make sure that they are imports
    
    k_old <- Qs[4]
    
    k_old_Head <- Qs[5]
    
    k_old_Head_OG <- Qs[6]
    slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
    clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
    
    
    clCounter <- 0
    slCounter <- 0
    
    while(clNew < 1.01){
      clNew <- clNew + 0.01
      clCounter <- 1
    }
    
    while(slNew < 19.01){
      slNew <- slNew + 0.01
      slCounter <- 1
    }
    
    ANew <- (slNew + clNew) * (1/adjF_pre)
    
    # Here I am saying that if we import live animals to meet the subsistence levels of demand,
    # they must be added to the stocks and replacement heifers.
    if((slCounter==1)){
      slNew_Head_EQ <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff[i] <- slNew_Head_EQ - slNew_Head_OG
      
      headRatio[i] <- slNew_Head_EQ/slNew_Head_OG
      
      K1[i] <- K1[i] * headRatio[i]
      k_old_Head <- k_old_Head * headRatio[i]
    }
    
    # This cull cow head difference must be added to the next stock.
    # Rational for this : We are using the current stock to
    # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
    if((clCounter==1)){
      clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
      K1[i] <- K1[i] + clHeadDiff[i]
    }
    
    
    ##### I am assuming all the export meat stays home so the loss of exports means
    ##### there is excess high quality meat in the country i.e., more supply. 
    ##### Economic theory simply says high supply means the price is low. 
    
    if(i == 1){
      #### Exports are banned that means the production stays in the country.
      ### Here we are changing the total demand for meat. Domestic decline for meat along with the exports are incorporated
      ANew1 <- ANew - (5/100) * ANew + ANew * (exports_percentK/100)
    } else if(i == 2  ){ 
      ### Here the domestic demand for meat climbs back up
      ANew1 <- ANew + ANew * (exports_percentK/100) + (5/100) * ANew
    } else{
      ### Everything is back to normal
      ANew1 <-  ANew - ANew * (exports_percentK/100) + (5/100) * ANew
    }
    
    # Here I am saying if we are importing, the expected price must go up. This will 
    # incentivize the farmers to build the stocks.
    if(slCounter==1){
      EpsM_pre <- EpsM_pre + 0.08
    }
    
    if(clCounter==1){
      EpcM_pre <- EpcM_pre + 0.08
    }
    
    Ps <- getPsPcEpsEpc_FMD(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                            HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                            params = c(MUtilde_pre, Stilde_pre))
    
    psM_pre <- Ps[1]
    pcM_pre <- Ps[2]
    hcM_pre <- Ps[3]
    EpsM_pre <- Ps[4]
    EpcM_pre <- Ps[5]
    
    ##### Here I check whether the market is cleared with the new demand. 
    #### I simply get the demand for fed cattle meat and cull cow meat under new prices.
    #### Substract them from the supply of the corresponding meat and then give direction 
    #### to prices
    
    D_sl <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    slDiff <- slNew - D_sl
    clDiff <- clNew - D_cl
    
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
    
    while(abs(slDiff)>0.01 || abs(clDiff)>0.01){
      
      slDiffEq[m] <- slDiff
      clDiffEq[m] <- clDiff
      
      if( slDiff < 0){
        psN <- psM_pre + 0.001
      } else if( slDiff > 0){
        psN <- psM_pre - 0.001
      }
      
      if(psN < 0){
        psN <- psM_pre
      }
      
      if( clDiff < 0){
        pcN <- pcM_pre + 0.001
      } else if( clDiff > 0){
        pcN <- pcM_pre - 0.001
      }
      
      if(pcN < 0){
        pcN <- pcM_pre
      }
      
      hcM_pre <- (((g * (beta^3) * psN) + (beta - 1) * pcN)/(1 + g * beta * (gamma0 + beta * gamma1)))
      
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
      if(dePopR==20){
        
        while(EpsM_pre < psM_pre){
          EpsM_pre <- EpsM_pre  + 0.1
        }
        
        while(EpcM_pre < pcM_pre){
          EpcM_pre <- EpcM_pre + 0.8
        }
        
      } else if(dePopR==20) {
        while(EpsM_pre < psM_pre){
          EpsM_pre <- EpsM_pre  + 0.1
        }
        
        while(EpcM_pre < pcM_pre){
          EpcM_pre <- EpcM_pre + 0.3
        }
      }else{
        while(EpsM_pre < psM_pre){
          EpsM_pre <- EpsM_pre  + 0.1
        }
        
        while(EpcM_pre < pcM_pre){
          EpcM_pre <- EpcM_pre + 0.1
        }
      }
     
      
      Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                              k = k, CapA = ANew1, gamma_k3 = gamma_k3,
                              eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                              slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
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
      
      while(clNew_Eq < 1.01){
        clNew_Eq  <- clNew_Eq  + 0.01
        clCounter_Eq <- 1
      }
      
      while(slNew_Eq < 19.01){
        slNew_Eq <- slNew_Eq + 0.01
        slCounter_Eq <- 1
      }
      
      ANew_Eq <- (slNew_Eq + clNew_Eq) * (1/adjF_pre)
      
      if((slCounter_Eq==1)){
        slNew_Head_EQ1 <- (slNew_Eq * 1000000000)/slaughterAvg_pre
        slHeadDiff1[m] <- slNew_Head_EQ1 - slNew_Head_OG_Eq
        headRatioEq[m] <- slNew_Head_EQ1/slNew_Head_OG_Eq
        # K1[i] <- K1[i] * headRatioEq[m]
        k_old_Head_EqRatio[m] <- k_old_Head_Eq * headRatioEq[m]
      }
      
      if((clCounter_Eq==1)){
        clNew_Head_EQ1 <- (clNew_Eq * 1000000000)/cullAvg_pre
        clHeadDiff1[m] <- clNew_Head_EQ1 - clNew_Head_OG_Eq
        K1[i] <- K1[i] + clHeadDiff1[m]
      }
      
      if(i == 1){
        
        ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + ANew_Eq * (exports_percentK/100)
        
      } else if( i == 2 ){
        
        ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)  + (5/100) * ANew_Eq
        
      } else{
        
        ANew11 <- ANew_Eq - ANew_Eq * (exports_percentK/100) + (5/100) * ANew_Eq
        
      }
      
      ANew1 <- ANew11
      
      D_sl <- ANew1 *
        ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
           (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
      
      D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
      
      slDiff <- slNew_Eq - D_sl
      clDiff <- clNew_Eq - D_cl
      
      # The reason for this condition is to avoid infinite while loop. Note that the while loop stops
      # if the differences reach below tolerance levels. But sometimes this is never the case and there will be 
      # some difference above tolerance level (basically saying that there will be closing stocks). So I exit the loop
      # if the difference stays stagnant.
      if(m >= 10){
        if( (round(slDiffEq[m],2) == round(slDiffEq[m-1],2)) && (round(clDiffEq[m],2) == round(clDiffEq[m-1],2)) ){
          if( (round(slDiffEq[m-1],2) == round(slDiffEq[m-2],2)) && (round(clDiffEq[m-1],2) == round(clDiffEq[m-2],2)) ){
            break
          }
        }
      }
      
      m <- m+1
      
    }
    
    ## Here i gather the replacement heifers from the above simulation
    if(length(k_old_Head_EqRatio)>0){
      fedTBA <- max(k_old_Head,max(na.omit(k_old_Head_EqRatio)))
    }else{
      fedTBA <- max(k_old_Head, k_old_Head_Eq)
    }
    
    repNewHead <- fedTBA
    repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
    
    # Here I gather the number of cull cow animals need to be added in the stock.
    # Whatever the imports are, I am assuming they are 3 year olds.
    if(length(clHeadDiff[i])>0){
      if(!is.na(clHeadDiff[i])){
        cullTBA <- max(clHeadDiff[i], na.omit(clHeadDiff1))
      }else{
        cullTBA <- 0
      }
    }else{
      cullTBA <- 0
    }
    
    beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + cullTBA
    beefINV_FORECAST_PostFMD$k3[i] <- beefINV_FORECAST_PostFMD$k3[i] + cullTBA
    
    # Here after knowing the stock, if it is less than the stock in equilibrium simulation, I add the progeny
    # of cull cows added above to the replacement heifers.
    if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
      repNewHead <- repNewHead + g * cullTBA
      repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
    }
    
    psM_EqMed[i] <- median(psM_Eq)
    pcM_EqMed[i] <- median(pcM_Eq)
    
    psM_EqMN[i] <- mean(psM_Eq)
    pcM_EqMN[i] <- mean(pcM_Eq)
    
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
    
    ### The follwowing are the conditions to check whether the stock and replacement heifers are realistic
    k3Counter <- 0
    if(i>3){
      if(k3Counter<4){
        if(beefINV_FORECAST_PostFMD$K[i+1] < min(Stock$K)){
          cCrop <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
          rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
          beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] +  (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
          k3Counter <- k3Counter + 1
        }
      }
    }
    
    # Here if the replacement heifers are greater than the historical replacement heifers, we just export the rest of them.
    # This is an assumption I am willing to make
    
    if( beefINV_FORECAST_PostFMD$k3[i+1] >  max(Stock$k3)){
      beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
      # beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
      beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
    }
    
    # Here once the stock reach the optimal level, the farmers won't keep the 9-year old cows.
    # The following code does that.
    while( beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
      beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k9[i]/delta)
      beefINV_FORECAST_PostFMD$k10[i+1] <- 0
    }
    
    # Here I populate the calf crop
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
  
  ##### Now I have calf-crop until 2009
  calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year < 2009)
  calf_crop_PreFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR) 
  calf_crop_2009 <- calf_cropF %>% filter(Year == 2009) %>% transmute(Year = Year, k0 = (1-dePopR/100) * calfCrop)
  calf_crop_PostFMD <- rbind(calf_crop_PreFMD, calf_crop_2009)
  
  modelParamsEQ_PreFMD <- modelParamsEQ_PreFMD %>% filter(Year == 2009)
  
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
  
  
  exports_2009 <- exports_preFMD
  #### here I am getting export percentage by the meat capA_pre
  exports_2009_meat <- exports_2009 * (slaughterAvg_pre/1000000000)
  exports_percent <- round((exports_2009_meat/capA_pre) * 100,3)
  
  ##### Here I am getting exports percentage by the stocks capK_pre_meat
  capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
  exports_percentK <- round((exports_2009_meat/capK_pre_meat) * 100,3)
  ### After careful consideration I am using exports_percentK as the exports in the simulation
  
  beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn+1), K = numeric(nn+1), k3 = numeric(nn+1), 
                                          k4 = numeric(nn+1), k5 = numeric(nn+1), k6 = numeric(nn+1), 
                                          k7 = numeric(nn+1), k8 = numeric(nn+1), k9 = numeric(nn+1),
                                          k10 = numeric(nn+1))
  
  beefINV_FORECAST_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)
  
  
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
  
  psM_EqMed <- NULL
  pcM_EqMed <- NULL
  psM_EqMN <- NULL
  pcM_EqMN <- NULL
  
  K1 <- NULL
  clHeadDiff <- NULL
  slHeadDiff <- NULL
  headRatio <- NULL
  
  
  for(i in 1:nrow(proj_Q_P_PostFMD)){
    
    if(i>1){
      #Populating the younglings
      k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,],
                                             beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,],
                                             calfCrop = calf_crop_PostFMD)
      #Retrieving the previous years derived demand and total breeding stock
      capA_pre <- proj_Q_P_PostFMD$A[i-1]
      capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
    }
    
    #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
    ### the optimal replacement heifers.
    k <- 0
    K1[i] <- capK_pre
    k0s <- k0s_PostFMD[i,-1]
    int_k3 <- 0
    
    ### Here I assume a 5% decrease in domestic demand and the exports are banned 
    if(i==1){
      capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
      K1[i] <- capK_pre * (1 - (dePopR/100))
    }
    
    # Here I get the supply by passing the stocks and derived demand. This function will give us
    # the meat supply and also the replacement heifers
    Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = capA_pre, gamma_k3 = gamma_k3, 
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
    
    slNew <- Qs[1]
    clNew <- Qs[2]
    ANew <- Qs[3]
    
    ### I take the absolute value of the replacement heifers. This is to avoid negative values. It's written inside the 
    ### function. 
    ### Note: by making the negative values positive we can make sure that they are imports
    
    k_old <- Qs[4]
    
    k_old_Head <- Qs[5]
    
    k_old_Head_OG <- Qs[6]
    slNew_Head_OG <- (slNew * 1000000000)/slaughterAvg_pre
    clNew_Head_OG <- (clNew * 1000000000)/cullAvg_pre
    
    
    clCounter <- 0
    slCounter <- 0
    
    while(clNew < 1.01){
      clNew <- clNew + 0.01
      clCounter <- 1
    }
    
    while(slNew < 19.01){
      slNew <- slNew + 0.01
      slCounter <- 1
    }
    
    ANew <- (slNew + clNew) * (1/adjF_pre)
    
    # Here I am saying that if we import live animals to meet the subsistence levels of demand,
    # they must be added to the stocks and replacement heifers.
    if((slCounter==1)){
      slNew_Head_EQ <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff[i] <- slNew_Head_EQ - slNew_Head_OG
      
      headRatio[i] <- slNew_Head_EQ/slNew_Head_OG
      
      K1[i] <- K1[i] * headRatio[i]
      k_old_Head <- k_old_Head * headRatio[i]
    }
    
    # This cull cow head difference must be added to the next stock.
    # Rational for this : We are using the current stock to
    # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
    if((clCounter==1)){
      clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
      K1[i] <- K1[i] + clHeadDiff[i]
    }
    
    
    ##### I am assuming all the export meat stays home so the loss of exports means
    ##### there is excess high quality meat in the country i.e., more supply. 
    ##### Economic theory simply says high supply means the price is low. 
    
    if(i < 4){
      #### Exports are banned that means the production stays in the country.
      ### Here we are changing the total demand for meat. Domestic decline for meat along with the exports are incorporated
      ANew1 <- ANew - (5/100) * ANew + ANew * (exports_percentK/100)
    } else if(i >= 4 && i <= 5  ){ 
      ### Here the domestic demand for meat climbs back up
      ANew1 <- ANew + ANew * (exports_percentK/100) + (5/100) * ANew
    } else{
      ### Everything is back to normal
      ANew1 <-  ANew - ANew * (exports_percentK/100) + (5/100) * ANew
    }
    
    # Here I am saying if we are importing, the expected price must go up. This will 
    # incentivize the farmers to build the stocks.
    if(slCounter==1){
      EpsM_pre <- EpsM_pre + 0.08
    }
    
    if(clCounter==1){
      EpcM_pre <- EpcM_pre + 0.08
    }
    
    Ps <- getPsPcEpsEpc_FMD(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                            HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                            params = c(MUtilde_pre, Stilde_pre))
    
    psM_pre <- Ps[1]
    pcM_pre <- Ps[2]
    hcM_pre <- Ps[3]
    EpsM_pre <- Ps[4]
    EpcM_pre <- Ps[5]
    
    ##### Here I check whether the market is cleared with the new demand. 
    #### I simply get the demand for fed cattle meat and cull cow meat under new prices.
    #### Substract them from the supply of the corresponding meat and then give direction 
    #### to prices
    
    D_sl <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    slDiff <- slNew - D_sl
    clDiff <- clNew - D_cl
    
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
    
    while(abs(slDiff)>0.01 || abs(clDiff)>0.01){
      
      slDiffEq[m] <- slDiff
      clDiffEq[m] <- clDiff
      
      if( slDiff < 0){
        psN <- psM_pre + 0.001
      } else if( slDiff > 0){
        psN <- psM_pre - 0.001
      }
      
      if(psN < 0){
        psN <- psM_pre
      }
      
      if( clDiff < 0){
        pcN <- pcM_pre + 0.001
      } else if( clDiff > 0){
        pcN <- pcM_pre - 0.001
      }
      
      if(pcN < 0){
        pcN <- pcM_pre
      }
      
      hcM_pre <- (((g * (beta^3) * psN) + (beta - 1) * pcN)/(1 + g * beta * (gamma0 + beta * gamma1)))
      
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
      
      while(EpsM_pre < psM_pre){
        EpsM_pre <- EpsM_pre  + 0.1
      }
      
      while(EpcM_pre < pcM_pre){
        EpcM_pre <- EpcM_pre + 0.8
      }
      
      Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                              k = k, CapA = ANew1, gamma_k3 = gamma_k3,
                              eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                              slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
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
      
      while(clNew_Eq < 1.01){
        clNew_Eq  <- clNew_Eq  + 0.01
        clCounter_Eq <- 1
      }
      
      while(slNew_Eq < 19.01){
        slNew_Eq <- slNew_Eq + 0.01
        slCounter_Eq <- 1
      }
      
      ANew_Eq <- (slNew_Eq + clNew_Eq) * (1/adjF_pre)
      
      if((slCounter_Eq==1)){
        slNew_Head_EQ1 <- (slNew_Eq * 1000000000)/slaughterAvg_pre
        slHeadDiff1[m] <- slNew_Head_EQ1 - slNew_Head_OG_Eq
        headRatioEq[m] <- slNew_Head_EQ1/slNew_Head_OG_Eq
        # K1[i] <- K1[i] * headRatioEq[m]
        k_old_Head_EqRatio[m] <- k_old_Head_Eq * headRatioEq[m]
      }
      
      if((clCounter_Eq==1)){
        clNew_Head_EQ1 <- (clNew_Eq * 1000000000)/cullAvg_pre
        clHeadDiff1[m] <- clNew_Head_EQ1 - clNew_Head_OG_Eq
        K1[i] <- K1[i] + clHeadDiff1[m]
      }
      
      if(i < 4){
        
        ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + ANew_Eq * (exports_percentK/100)
        
      } else if( i >= 4 && i <= 5 ){
        
        ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)  + (5/100) * ANew_Eq
        
      } else{
        
        ANew11 <- ANew_Eq - ANew_Eq * (exports_percentK/100) + (5/100) * ANew_Eq
        
      }
      
      ANew1 <- ANew11
      
      D_sl <- ANew1 *
        ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
           (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
      
      D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
      
      slDiff <- slNew_Eq - D_sl
      clDiff <- clNew_Eq - D_cl
      
      # The reason for this condition is to avoid infinite while loop. Note that the while loop stops
      # if the differences reach below tolerance levels. But sometimes this is never the case and there will be 
      # some difference above tolerance level (basically saying that there will be closing stocks). So I exit the loop
      # if the difference stays stagnant.
      if(m >= 10){
        if( (round(slDiffEq[m],2) == round(slDiffEq[m-1],2)) && (round(clDiffEq[m],2) == round(clDiffEq[m-1],2)) ){
          if( (round(slDiffEq[m-1],2) == round(slDiffEq[m-2],2)) && (round(clDiffEq[m-1],2) == round(clDiffEq[m-2],2)) ){
              break
          }
        }
      }
      
      m <- m+1
      
    }
    
    ## Here i gather the replacement heifers from the above simulation
    if(length(k_old_Head_EqRatio)>0){
      fedTBA <- max(k_old_Head,max(na.omit(k_old_Head_EqRatio)))
    }else{
      fedTBA <- max(k_old_Head, k_old_Head_Eq)
    }
    
    repNewHead <- fedTBA
    repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
    
    # Here I gather the number of cull cow animals need to be added in the stock.
    # Whatever the imports are, I am assuming they are 3 year olds.
    if(length(clHeadDiff[i])>0){
      if(!is.na(clHeadDiff[i])){
        cullTBA <- max(clHeadDiff[i], na.omit(clHeadDiff1))
      }else{
        cullTBA <- 0
      }
    }else{
      cullTBA <- 0
    }
    
    beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + cullTBA
    beefINV_FORECAST_PostFMD$k3[i] <- beefINV_FORECAST_PostFMD$k3[i] + cullTBA
    
    # Here after knowing the stock, if it is less than the stock in equilibrium simulation, I add the progeny
    # of cull cows added above to the replacement heifers.
    if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
      repNewHead <- repNewHead + g * cullTBA
      repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
    }
    
    psM_EqMed[i] <- median(psM_Eq)
    pcM_EqMed[i] <- median(pcM_Eq)
    
    psM_EqMN[i] <- mean(psM_Eq)
    pcM_EqMN[i] <- mean(pcM_Eq)
    
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
    
    ### The follwowing are the conditions to check whether the stock and replacement heifers are realistic
    # if(i>3){
    #   
    #   if(beefINV_FORECAST_PostFMD$K[i+1] < min(Stock$K)){
    #     cCrop <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
    #     rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
    #     beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] +  (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
    #   }
    # }
    
    k3Counter <- 0
    if(i>3){
      if(k3Counter<4){
        if(beefINV_FORECAST_PostFMD$K[i+1] < min(Stock$K)){
          cCrop <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
          rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
          beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] +  (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
          k3Counter <- k3Counter + 1
        }
      }
    }
    
    # Here if the replacement heifers are greater than the historical replacement heifers, we just export the rest of them.
    # This is an assumption I am willing to make
    
    if( beefINV_FORECAST_PostFMD$k3[i+1] >  max(Stock$k3)){
      beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
      # beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
      beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
    }
    
    # Here once the stock reach the optimal level, the farmers won't keep the 9-year old cows.
    # The following code does that.
    while( beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
      beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k9[i]/delta)
      beefINV_FORECAST_PostFMD$k10[i+1] <- 0
    }
    
    # Here I populate the calf crop
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




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


##### Now I have calf-crop until 2009
##### Now I have calf-crop until 2009
dePopR <- 20
calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year < 2009)
calf_crop_PreFMD <- dePop(stock = calf_crop_PreFMD %>% tail(10), dePopRate = dePopR)    
calf_crop_2009 <- calf_crop %>% filter(Year == 2009) %>% transmute(Year = Year, k0 = (1-dePopR/100) * calfCrop)
# calf_crop_2010 <- calf_crop %>% filter(Year == 2010) %>% transmute(Year = Year, k0 = (1-dePopR/100) * calfCrop)
calf_crop_PostFMD <- rbind(calf_crop_PreFMD, calf_crop_2009)

modelParamsEQ_PreFMD <- proj_AllDF_EQ %>% filter(Year == 2009)

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

exports_2009 <- exports_2008
exports_2009_meat <- exports_2009 * (slaughterAvg_pre/1000000000)
exports_percent <- round((exports_2009_meat/capA_pre) * 100,3)

capK_pre_meat <- capK_pre * (cullAvg_pre/1000000000)
exports_percentK <- round((exports_2009_meat/capK_pre_meat) * 100,3)

nn <- 10
beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn+1), K = numeric(nn+1), k3 = numeric(nn+1), 
                                        k4 = numeric(nn+1), k5 = numeric(nn+1), k6 = numeric(nn+1), 
                                        k7 = numeric(nn+1), k8 = numeric(nn+1), k9 = numeric(nn+1),
                                        k10 = numeric(nn+1))

beefINV_FORECAST_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)
# beefINV_FORECAST_PostFMD[2,] <- dePop(stock = Stock %>% filter(Year == 2011), dePopRate = dePopR)

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

#  I will also hav to depopulate the k0s as well
# k0s_PostFMD[1,] <- dePop(stock = Stock %>% filter(Year == 2010), dePopRate = dePopR)

# k0s_PostFMD[2,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[2,],
#                                       beefINV_FORECAST = beefINV_FORECAST_PostFMD[2,], 
#                                       calfCrop = calf_crop_PostFMD)


##### Japan lifted it's ban on the importation of US beef nearly 2 years after BSE in the US
##### December 2005, Japan agreed to remove the restriction on importing US beef. However, in January imports stopped again because inspectors found banned cattle parts in a veal shipment from the U.S.
#
####### South Korea resumed U.S. beef imports in July 2008 

### China lifted it's ban in 2016

psM_EqMed <- NULL
pcM_EqMed <- NULL
psM_EqMN <- NULL
pcM_EqMN <- NULL

K1 <- NULL
clHeadDiff <- NULL
slHeadDiff <- NULL
headRatio <- NULL

for(i in 1:nrow(proj_Q_P_PostFMD)){
  
  i <- 1
  
  if(i>1){
    #Populating the younglings
    k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,],
                                           beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,],
                                           calfCrop = calf_crop_PostFMD)
    #Retrieving the previous years derived demand and total breeding stock
    capA_pre <- proj_Q_P_PostFMD$A[i-1]
    capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
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
    
    
    K1[i] <- capK_pre * (1 - (dePopR/100))
    
    # Here I check whether we can meet the demand with the stock in the country.
    # QsTest <- BBoptim(par = k, fn = estQFunction_test_FMD, tilde_MU = MUtilde_pre, 
    #               tilde_s = Stilde_pre, ps = psM_pre, pc = pcM_pre, K1 = K1[i], A = capA_pre, 
    #               gamma_k3 = gamma_k3, eta_k3 = eta_k3, int = int_k3, k0s = k0s, 
    #               slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
    # # From the above function I get the number of replacement heifers needed for two years from now, to meet the demand.
    # repHeifNeeded <- QsTest$par
    # 
    # # If the number of heifers needed is negative, we import those number of mature cows this year to meet the demand (maybe)
    # # Hence we add these numbers to the stock.
    # if(repHeifNeeded < 0){
    #   matureCowsNeeded <- (abs(repHeifNeeded)/g)
    #   K1[i] <- K1[i] + matureCowsNeeded
    # }
    
  }
  
  # After two years the calf crop will become mature and added to the mature stock.
  # if(i>2){
  #   # Here we will get the calf crop three years ago and replacement heifers this year. If replacement heifers
  #   # is greater than the calf crop (that means we imported the replacement heifers), we only add the calf-crop
  #   # to the stock otherwise we add calf-crop minus replacement heifers to the stock.
  #   cCrop <- calf_crop_PostFMD %>% filter(Year == proj_Q_P_PostFMD$Year[i-2]-1) %>% select(k0) %>% as.numeric()
  #   rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-1) %>% select(k3) %>% as.numeric()
  #   # rHeif <- proj_Q_P_PostFMD %>% filter(Year == proj_Q_P_PostFMD$Year[i-2]-1) %>% select(repHeif_Head) %>% as.numeric()
  #   # if(rHeif > cCrop){
  #   #   K1[i] <- K1[i] + cCrop
  #   # } else{
  #   #   K1[i] <- K1[i] + cCrop - rHeif
  #   # }
  # 
  #   K1[i] <- K1[i] + cCrop - rHeif
  # 
  # 
  # }
  
  # ccTwo <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
  # repHeifNow <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
  # matureStockTBA <- delta * ccTwo - repHeifNow
  # 
  # K1[i] <- K1[i] + matureStockTBA
  
  # Here I get the supply by passing the stocks and derived demand. This function will give us
  # the meat supply and also the replacement heifers
  Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = capA_pre, gamma_k3 = gamma_k3, 
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
    
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
  
  
  clCounter <- 0
  slCounter <- 0
  
  while(clNew < 1.01){
    clNew <- clNew + 0.01
    clCounter <- 1
  }
  
  while(slNew < 19.01){
    slNew <- slNew + 0.01
    slCounter <- 1
  }
  
  ANew <- (slNew + clNew)
  
  slNew <- slNew * adjF_pre
  clNew <- clNew * adjF_pre
  
  # if((slCounter==1) || (clCounter==1)){

    # Here I am saying that if we import live animals to meet the subsistence levels of demand,
    # they must be added to the stocks and replacement heifers.
    if((slCounter==1)){
      slNew_Head_EQ <- (slNew * 1000000000)/slaughterAvg_pre
      slHeadDiff[i] <- slNew_Head_EQ - slNew_Head_OG

      headRatio[i] <- slNew_Head_EQ/slNew_Head_OG

      K1[i] <- K1[i] * headRatio[i]
      k_old_Head <- k_old_Head * headRatio[i]
    }

    # This cull cow head difference must be added to the next stock.
    # Rational for this : We are using the current stock to
    # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
    if((clCounter==1)){
      clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
      clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
      K1[i] <- K1[i] + clHeadDiff[i]
    }

  # }
  
  # This cull cow head difference must be added to the next stock. 
  # Rational for this : We are using the current stock to 
  # project the supply next year. So if I am increasing the cull cow supply that means it is added to the stock next year.
  # if((clCounter==1)){
  #   clNew_Head_EQ <- (clNew * 1000000000)/cullAvg_pre
  #   clHeadDiff[i] <- clNew_Head_EQ - clNew_Head_OG
  #   K1[i] <- K1[i] + clHeadDiff[i]
  # }
  
  if(i < 4){
    # i < 4
    # i == 1
    #### Exports are banned that means the production stays in the country.
    ### Here we are changing the total demand for meat. Domestic decline for meat along with the exports are incorporated
    # slExports <- slNew * (exports_percentK/100)
    # clExports <- clNew * (exports_percentK/100)
    # ANew1 <- ANew + slExports +  clExports - (5/100) * ANew
    ANew1 <- ANew - (5/100) * ANew + ANew * (exports_percentK/100)
  } else if(i >= 4 && i <= 5){ 
    # i >= 4 && i <= 5 
    # i == 2 
    ### Here the domestic demand for meat climbs back up
    # slExports <- slNew * (exports_percentK/100)
    # clExports <- clNew * (exports_percentK/100)
    # ANew1 <- ANew + slExports +  clExports + (5/100) * ANew
    ANew1 <- ANew + ANew * (exports_percentK/100) + (5/100) * ANew
    # ANew1 <- ANew + ANew * (exports_percentK/100)
  } else{
    ### Everything is back to normal
    # slExports <- slNew * (exports_percentK/100)
    # clExports <- clNew * (exports_percentK/100)
    # ANew1 <-  ANew - slExports - clExports + (5/100) * ANew
    ANew1 <-  ANew - ANew * (exports_percentK/100) + (5/100) * ANew
    # ANew1 <-  ANew 
  }
  
  # Here I am saying that if we are importing, the expected price must go up. This will 
  # incentivize the farmers to build the stocks.
  if(slCounter==1){
    EpsM_pre <- EpsM_pre + 0.08
  }
  
  if(clCounter==1){
    EpcM_pre <- EpcM_pre + 0.08
  }
  
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
  
  D_sl <- ANew1 *
    ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
       (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
  
  slDiff <- slNew - D_sl
  clDiff <- clNew - D_cl
  
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
  
  while(abs(slDiff)>0.01 || abs(clDiff)>0.01){
    
    slDiffEq[m] <- slDiff
    clDiffEq[m] <- clDiff
    
    if( slDiff < 0){
      psN <- psM_pre + 0.001
    } else if( slDiff > 0){
      psN <- psM_pre - 0.001
    }
    
    if(psN < 0){
      psN <- psM_pre
    }
    
    if( clDiff < 0){
      pcN <- pcM_pre + 0.001
    } else if( clDiff > 0){
      pcN <- pcM_pre - 0.001
    }
    
    if(pcN < 0){
      pcN <- pcM_pre
    }
    
    hcM_pre <- (((g * (beta^3) * psN) + (beta - 1) * pcN)/(1 + g * beta * (gamma0 + beta * gamma1)))
    
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
    
      while(EpsM_pre < psM_pre){
        EpsM_pre <- EpsM_pre  + 0.8
      }

      while(EpcM_pre < pcM_pre){
        EpcM_pre <- EpcM_pre + 0.8
      }
    
    Qs <- getSlClA_test_FMD(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1[i],
                            k = k, CapA = ANew1, gamma_k3 = gamma_k3,
                            eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                            slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
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
    
    while(clNew_Eq < 1.01){
      clNew_Eq  <- clNew_Eq  + 0.01
      clCounter_Eq <- 1
    }
    
    while(slNew_Eq < 19.01){
      slNew_Eq <- slNew_Eq + 0.01
      slCounter_Eq <- 1
    }
    
    ANew_Eq <- (slNew_Eq + clNew_Eq) * (1/adjF_pre)
    
    # if((slCounter_Eq==1) || (clCounter_Eq==1)){

      if((slCounter_Eq==1)){
        slNew_Head_EQ1 <- (slNew_Eq * 1000000000)/slaughterAvg_pre
        slHeadDiff1[m] <- slNew_Head_EQ1 - slNew_Head_OG_Eq
        headRatioEq[m] <- slNew_Head_EQ1/slNew_Head_OG_Eq
        # K1[i] <- K1[i] * headRatioEq[m]
        k_old_Head_EqRatio[m] <- k_old_Head_Eq * headRatioEq[m]
      }

      if((clCounter_Eq==1)){
        clNew_Head_EQ1 <- (clNew_Eq * 1000000000)/cullAvg_pre
        clHeadDiff1[m] <- clNew_Head_EQ1 - clNew_Head_OG_Eq
        K1[i] <- K1[i] + clHeadDiff1[m]
      }


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
    
    if(i < 4){
      # i < 4
      # i == 1
      # slExp1 <- slNew_Eq  * (exports_percentK/100)
      # clExp1 <- clNew_Eq  * (exports_percentK/100)
      # ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + slExp1 + clExp1
      
      ANew11  <- ANew_Eq - (5/100) * ANew_Eq  + ANew_Eq * (exports_percentK/100)
      
    } else if( i >= 4 && i <= 5 ){
      # i >= 4 && i <= 5
      # i == 2
      # slExp1 <- slNew_Eq * (exports_percentK/100)
      # clExp1 <- clNew_Eq * (exports_percentK/100)
      # ANew11 <- ANew_Eq + slExp1 + clExp1 + (5/100) * ANew_Eq 
      ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)  + (5/100) * ANew_Eq
      # ANew11 <- ANew_Eq + ANew_Eq * (exports_percentK/100)
      
    } else{
      # slExp1 <- slNew_Eq * (exports_percentK/100)
      # clExp1 <- clNew_Eq * (exports_percentK/100)
      # ANew11 <- ANew_Eq  - slExp1 - clExp1 + (5/100) * ANew_Eq 
      ANew11 <- ANew_Eq - ANew_Eq * (exports_percentK/100) + (5/100) * ANew_Eq
      # ANew11 <- ANew_Eq
    }
    
    ANew1 <- ANew11
    
    D_sl <- ANew1 *
      ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
         (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    D_cl <- ANew1 * (1/(1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    slDiff <- slNew_Eq - D_sl
    clDiff <- clNew_Eq - D_cl
    
    # The reason for this condition is to avoid infinite while loop. Note that the while loop stops
    # if the differences reach below tolerance levels. But sometimes this is never the case and there will be 
    # some difference above tolerance level (basically saying that there will be closing stocks). So I exit the loop
    # if the difference stays stagnant.
    if(m >= 15){
      if( (round(slDiffEq[m],2) == round(slDiffEq[m-1],2)) && (round(clDiffEq[m],2) == round(clDiffEq[m-1],2)) ){
        if( (round(slDiffEq[m-1],2) == round(slDiffEq[m-2],2)) && (round(clDiffEq[m-1],2) == round(clDiffEq[m-2],2)) ){
          # if( (round(slDiffEq[m-2],2) == round(slDiffEq[m-3],2)) && (round(clDiffEq[m-2],2) == round(clDiffEq[m-3],2)) ){
               break
          # }
        }
      }
    }
    
    m <- m+1
    
  }
  
  ## Here i gather the replacement heifers from the above simulation
  if(length(k_old_Head_EqRatio)>0){
    fedTBA <- max(k_old_Head,max(na.omit(k_old_Head_EqRatio)))
  }else{
    fedTBA <- max(k_old_Head, k_old_Head_Eq)
  }
  
  repNewHead <- fedTBA
  repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
  
  # Here I gather the number of cull cow animals need to be added in the stock.
  # Whatever the imports are, I am assuming they are 3 year olds.
  if(length(clHeadDiff[i])>0){
    if(!is.na(clHeadDiff[i])){
      cullTBA <- max(clHeadDiff[i], na.omit(clHeadDiff1))
    }else{
      cullTBA <- 0
    }
  }else{
    cullTBA <- 0
  }

  beefINV_FORECAST_PostFMD$K[i] <- beefINV_FORECAST_PostFMD$K[i] + cullTBA
  beefINV_FORECAST_PostFMD$k3[i] <- beefINV_FORECAST_PostFMD$k3[i] + cullTBA

  # Here after knowing the stock, if it is less than the stock in equilibrium simulation, I add the progeny
  # of cull cows added above to the replacement heifers.
  if(beefINV_FORECAST_PostFMD$K[i] < K1[i]){
      repNewHead <- repNewHead + g * cullTBA
      repNewLbs <- repNewHead * (slaughterAvg_pre/1000000000)
      # beefINV_FORECAST_PostFMD$K[i] <- K1[i]
  }
  
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
  
  psM_EqMed[i] <- median(psM_Eq)
  pcM_EqMed[i] <- median(pcM_Eq)
  
  psM_EqMN[i] <- mean(psM_Eq)
  pcM_EqMN[i] <- mean(pcM_Eq)
  
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
  
  if(i>5){

    if(beefINV_FORECAST_PostFMD$K[i+1] < min(Stock$K)){
      cCrop <- calf_crop_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]-2) %>% select(k0) %>% as.numeric()
      rHeif <- beefINV_FORECAST_PostFMD %>% filter(Year == beefINV_FORECAST_PostFMD$Year[i]) %>% select(k3) %>% as.numeric()
      beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] +  (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
      # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] + (beefINV_FORECAST_PostFMD$K[i+1] - (delta * cCrop - rHeif/g))
    }
  }
  
  # Here if the replacement heifers are greater than the historical replacement heifers, we just export the rest of them.
  # This is an assumption I am willing to make
  
  if( beefINV_FORECAST_PostFMD$k3[i+1] >  max(Stock$k3)){
    beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
    # beefINV_FORECAST_PostFMD$K[i+1] <- sum(beefINV_FORECAST_PostFMD[i+1,-1:-2])
    beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  }
  
  # if( beefINV_FORECAST_PostFMD$k3[i+1] >  median(Stock$k3)){
  #   beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  #   beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k3[i+1] - max(Stock$k3))
  # }
  
  # Here once the stock reach the optimal level, the farmers won't keep the 9-year old cows and the rest are exported
  # The following code does that.
  if( beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
    beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$k9[i]/delta)
    beefINV_FORECAST_PostFMD$k10[i+1] <- 0
    # if(beefINV_FORECAST_PostFMD$K[i+1] >  median(Stock$K)){
    #   beefINV_FORECAST_PostFMD$K[i+1] <- median(Stock$K)
    #   beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
    # }
    # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$K[i]/delta)
    # beefINV_FORECAST_PostFMD$K[i+1] <- beefINV_FORECAST_PostFMD$K[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
    # beefINV_FORECAST_PostFMD$k3[i+1] <- beefINV_FORECAST_PostFMD$k3[i+1] - (beefINV_FORECAST_PostFMD$K[i+1] - median(Stock$K))
  }
  
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








