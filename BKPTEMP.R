simOptimisticFMD <- function(dePopR, modelParamsEQ_PreFMD, exports_percentK, 
                             exports_LiveK, exports_percentMeat, nn, Stock, holdingCostsFuturesFMD){
  
  # dePopR <- 5
  
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
  shockD_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$dShock)
  
  # slShock2009 <- mean(tail(allStockShocks %>% filter(Year<=2009),n=1)$slShock)
  # clShock2009 <- mean(tail(allStockShocks %>% filter(Year<=2009),n=1)$clShock)
  
  slShock2021 <- 1
  clShock2021 <- 1
  
  beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn), K = NA, k3 = NA,
                                          k4 =  NA, k5 =  NA, k6 =  NA, 
                                          k7 =  NA, k8 =  NA, k9 =  NA)
  
  beefINV_FORECAST_PostFMD$Year <- seq(from=2022, to=2022+nn-1)
  
  proj_Q_P_PostFMD <- data.frame(Year = numeric(nn), Ps = numeric(nn), Pc = numeric(nn), 
                                 EPs = numeric(nn), EPc = numeric(nn), Hc = numeric(nn), 
                                 Sl = numeric(nn), Cl = numeric(nn), A = numeric(nn),
                                 repHeif_Head = numeric(nn),boundCond = numeric(nn), 
                                 muTilde = numeric(nn), sTilde = numeric(nn), sh = numeric(nn),
                                 demDollarsAfter = numeric(nn), Sl_OG = numeric(nn), Cl_OG = numeric(nn))
  
  # k0s_PostFMD <- data.frame(Year = numeric(nn), k02 = numeric(nn), k03 = numeric(nn), 
  #                           k04 = numeric(nn), k05 = numeric(nn), k06 = numeric(nn), 
  #                           k07 = numeric(nn), k08 = numeric(nn))
  # 
  # k0s_PostFMD[1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[1,], 
  #                                       beefINV_FORECAST = beefINV_FORECAST_PostFMD[1,], 
  #                                       calfCrop = calf_crop_PostFMD)
  
  
  stockForecastFMD <- Stock %>% filter(Year < 2022)
  mergedForecastFMD <- merge(stockForecastFMD, beefINV_FORECAST_PostFMD, all=TRUE) %>% filter(Year >= 1995)
  
  # quantitiesSLCLFMD <- quantities %>% select(Year, sl, cl) %>% filter(Year < 2021 & Year >= 1995)
  
  quantitiesSLCLFMD <- modelParamsEQ_PreFMD %>% 
    select(Year, slMedian, clMedian) %>% filter(Year < 2022 & Year >= 1995)
  
  wtAVGFMD <- allStockShocks %>% 
    select(Year, Slaughter_avg, Cull_avg) %>% filter(Year < 2022 & Year >= 1995)
  
  quantsWeightsFMD <- merge(quantitiesSLCLFMD, wtAVGFMD)
  
  impExpFMD <- stocksImportsExports %>% select(Year, Imports, Exports) %>% 
    filter(Year < 2022 & Year >= 1995)
  
  exportsImportsBeef <- exportsBeef %>% filter(Year < 2022 & Year >= 1995) %>% 
    transmute(Year = Year, ImportsBeef = Imports, ExportsBeef = Exports)
  
  quantsWeightsFMD <- merge(quantsWeightsFMD, impExpFMD, all=TRUE)
  
  mergedForecastFMD <- merge(mergedForecastFMD, quantsWeightsFMD, all=TRUE)
  mergedForecastFMD <- merge(mergedForecastFMD, exportsImportsBeef, all=TRUE)
  
  mergedForecastFMD <- fill(mergedForecastFMD, Cull_avg, .direction = 'down')
  mergedForecastFMD <- fill(mergedForecastFMD, Slaughter_avg, .direction = 'down')
  
  # mergedForecastFMD$k3[mergedForecastFMD$Year==2022] <- 
  #   mergedForecastFMD$k3[mergedForecastFMD$Year==2021]
  
  mergedForecastFMD <- mergedForecastFMD %>% mutate(k4 = delta * lag(k3), k5 = delta * lag(k4), k6 = delta * lag(k5),
                                                    k7 = delta * lag(k6), k8 = delta * lag(k7), 
                                                    k9 = if_else((K - (k3+k4+k5+k6+k7+k8)) < 0, 0, (K - (k3+k4+k5+k6+k7+k8))),
                                                    k10 = 0) %>% filter(Year > 2000)
  
  ##### Japan lifted it's ban on the importation of US beef nearly 2 years after BSE in the US
  ##### December 2005, Japan agreed to remove the restriction on importing US beef. However, in January imports stopped again because inspectors found banned cattle parts in a veal shipment from the U.S.
  ####### South Korea resumed U.S. beef imports in July 2008 
  
  ### China lifted it's ban in 2016
  
  proj_Q_P_PostFMD$Year <- seq(beefINV_FORECAST_PostFMD$Year[1], 
                               beefINV_FORECAST_PostFMD$Year[nrow(beefINV_FORECAST_PostFMD)],  by = 1)
  mergedForecastFMD_Proj <- mergedForecastFMD %>% filter(Year >= beefINV_FORECAST_PostFMD$Year[1]-10)
  
  #### Here I depop the stocks (5%, 10%, 20%)
  mergedForecastFMD_Proj[,1:10] <- 
    dePop(stock = mergedForecastFMD_Proj[,1:10], dePopRate = dePopR) 
  
  slNewPre <- slSM_pre
  clNewPre <- clSM_pre
  
  params_mu_s_FMDProj <- optParamFunction_FMDProj(sl = slNewPre, cl = clNewPre,
                                                  ps = psM_pre, pc = pcM_pre, thetas = c(1,1), adj = 1)
  
  MUtilde_pre <- params_mu_s_FMDProj[1]
  Stilde_pre <- params_mu_s_FMDProj[2]
  
  expUpdatek3 <- NA
  impUpdatek3 <- NA
  
  expUpdateK <- NA
  impUpdateK <- NA
  
  impBeef <- NA
  
  gFMD <- 0.8924
  gFMDCC <- gFMD
  repRatio <- 0.2554
  
  holdingCostsFuturesFMD <- holdingCostsFuturesFMD
  
  
  for(i in 1: nrow(proj_Q_P_PostFMD)){
    
    # i <- 3
    
    yearIFMD <- proj_Q_P_PostFMD$Year[i]
    
    capK_pre <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% 
      select(K) %>% as.numeric()
    
    sh_pre <- ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
                 (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))
    
    proj_Q_P_PostFMD$muTilde[i] <- MUtilde_pre
    proj_Q_P_PostFMD$sTilde[i] <- Stilde_pre
    proj_Q_P_PostFMD$sh[i] <- sh_pre
    
    ##### Here I construct the supply of cull cow meat
    k6nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k6)
    k7nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k7)
    k8nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k8)
    k9nFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(k9)
    clShnFMD <- 1
    
    cAvgFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Cull_avg)
    clmFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(clMedian)
    ## This is because for the past years k9 is zero. So I assume the k9s are zero for next years as well.
    clNewFMD <-  ((k9nFMD + (1-delta) * k8nFMD + (1-delta) * k7nFMD) * clShnFMD +
                    (delta * (k8nFMD + k7nFMD + k6nFMD) - (k7nFMD + k8nFMD + k9nFMD)) )* (cAvgFMD/1000000000)
    clNewFMD <- as.numeric(clNewFMD)
    
    clNewFMDHead_OG <- as.numeric(clNewFMD * (1000000000/cAvgFMD))
    clNewFMD_OG <- clNewFMD
    proj_Q_P_PostFMD$Cl_OG[i] <- clNewFMD_OG
    
    ##### Here I construct the supply of fed cattle meat
    slShmFMD <- 1
    Km2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(K)
    Km3FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-4) %>% select(K)
    k9m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k9)
    k8m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k8)
    k7m2FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-3) %>% select(k7)
    fedAvgFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>%select(Slaughter_avg)
    # sl1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% mutate(sl1 = slMedian * (1000000000/Slaughter_avg))
    # cl1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% mutate(cl1 = clMedian * (1000000000/Cull_avg))
    
    ## This is because for the past years k9 is zero. So I assume the k9s are zero for next years as well.
    ## This might change if the imports are increased. I do not add imports in the analysis for now.
    
    #### Here I am changing 0.37 to 0.26
    # slNewFMD <- ((gFMD - 0.37 * gFMD) * Km2FMD * slShmFMD +
    #                ((1 - 0.37 * gFMD) * gFMD * delta * (Km2FMD - (gFMD - 0.37 * gFMD) * Km3FMD -
    #                                                 (k9m2FMD + (1-delta) * k8m2FMD + (1-delta) * k7m2FMD)))) * (fedAvgFMD/1000000000)
    
    slNewFMD <- ((gFMD - repRatio * gFMD) * Km2FMD * slShmFMD +
                   ((1 - repRatio * gFMD) * gFMD * delta * (Km2FMD - (gFMD - repRatio * gFMD) * Km3FMD -
                                                              (k9m2FMD + (1-delta) * k8m2FMD + (1-delta) * k7m2FMD)))) * (fedAvgFMD/1000000000)
    # ((g - repRatio * g) * Km2FMD * slShmFMD +
    #     ((1 - repRatio * g) * g * delta * (Km2FMD - (g - repRatio * g) * Km3FMD -
    #                                                (k9m2FMD + (1-delta) * k8m2FMD + (1-delta) * k7m2FMD)))) * (fedAvgFMD/1000000000)
    
    slNewFMD <- as.numeric(slNewFMD)
    
    slNewFMDHead_OG <- as.numeric(slNewFMD * (1000000000/fedAvgFMD))
    slNewFMD_OG <- slNewFMD
    proj_Q_P_PostFMD$Sl_OG[i] <- slNewFMD_OG
    
    #### This is the total supply. Now by assuming this is the total demand (i.e., markets clear), we can get the demand for 
    #### fed cattle meat from the share we constructed above. Note that the share metric includes the prices (fed cattle, cull cows),
    #### also the parameters (median willingness to pay for fed cattle meat over cull cow meat). Hence, the replacement
    #### heifers which are added to the stock are price driven.
    ANewFMD <- (slNewFMD + clNewFMD)
    
    ##### Here I use the demand (in quantity) from previous year to get the demand (in dollars)
    ##### Also I compute the live animals that are supposed to be exported using the linear model coefficients
    if(i==1){
      capAFMD <- capA_pre
      capAFMD_DollarsOG <- slSM_pre * psM_pre + clSM_pre * pcM_pre
      # slExprtsLive <- expLM$coefficients[1] * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
      slExprtsLive <- expRatioMax * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
      # slExprtsLive <- (expLM$coefficients[1] * (slNewFMD * (1000000000/fedAvgFMD))) %>% as.numeric()
      # clExprtsLive <- expLM$coefficients[1] * (clNewFMD * (1000000000/cAvgFMD))
      # exports_Live <- (slExprtsLive + clExprtsLive) %>% as.numeric()
      exports_Live <- slExprtsLive 
    }else if(i==2){
      # slExprtsLive <- expLM$coefficients[1] * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
      slExprtsLive <- expRatioMax * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
      # slExprtsLive <- (expLM$coefficients[1] * (slNewFMD * (1000000000/fedAvgFMD) )) %>% as.numeric()
      # clExprtsLive <- expLM$coefficients[1] * (clNewFMD * (1000000000/cAvgFMD))
      # exports_Live <- (slExprtsLive + clExprtsLive) %>% as.numeric()
      exports_Live <- slExprtsLive
    }
    
    #### Export markets
    #### For the export markets inaccessibility I am adding that meat into the supply 
    #### This is because there is more meat left in the country, i.e., more supply.
    if(i == 1){
      
      # slExprtsMeat <- slNewFMD * (exports_percentMeat/100)
      # clExprtsMeat <- clNewFMD * (exports_percentMeat/100)
      
      # slExprtsMeat <- (expBeefLM$coefficients[1] * (slNewFMD + clNewFMD)) %>% as.numeric()
      # slExprtsMeat <- (expRatioBeefMax * (slNewFMD + clNewFMD)) %>% as.numeric()
      slExprtsMeat <- (expRatioBeefMax * slNewFMD ) %>% as.numeric()
      # clExprtsMeat <- (expRatioBeefMax * clNewFMD) %>% as.numeric()
      
      slNewFMD <- slNewFMD + slExprtsMeat
      # clNewFMD <- clNewFMD + clExprtsMeat
      clNewFMD <- clNewFMD
      
      # capAFMD_Dollars <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
      
      exprtsFMD <- -(mergedForecastFMD_Proj %>% filter(Year == 2020) %>% select(Exports) %>% as.numeric())
      
      mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- if_else(exprtsFMD>0,exprtsFMD,0)
      
    }else if(i == 2){
      
      # slExprtsMeat <- slNewFMD * (exports_percentMeat/100)
      # clExprtsMeat <- clNewFMD * (exports_percentMeat/100)
      
      # slExprtsMeat <- (expBeefLM$coefficients[1] *(slNewFMD + clNewFMD)) %>% as.numeric()
      # slExprtsMeat <- (expRatioBeefMax * (slNewFMD + clNewFMD)) %>% as.numeric()
      slExprtsMeat <- (expRatioBeefMax * slNewFMD ) %>% as.numeric()
      # clExprtsMeat <- (expRatioBeefMax * clNewFMD) %>% as.numeric()
      
      slNewFMD <- slNewFMD + slExprtsMeat
      # clNewFMD <- clNewFMD + clExprtsMeat
      clNewFMD <- clNewFMD
      
      exprtsFMD <- -(mergedForecastFMD_Proj %>% filter(Year == 2020) %>% select(Exports) %>% as.numeric())
      mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- if_else(exprtsFMD>0,exprtsFMD,0)
      
    }else{
      
      # slExprtsLive <- slNewFMD * (exports_LiveK/100)
      # clExprtsLive <- clNewFMD * (exports_LiveK/100)
      
      # slExprtsLive <- (expLM$coefficients[1] * (slNewFMD * (1000000000/fedAvgFMD))) %>% as.numeric()
      # clExprtsLive <- ((expLM$coefficients[1] * (clNewFMD * (1000000000/cAvgFMD)))) %>% as.numeric()
      slExprtsLive <- expRatioMax * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
      exports_Live <- slExprtsLive
      # slExprtsMeat <- slNewFMD * (exports_percentMeat/100)
      # clExprtsMeat <- clNewFMD * (exports_percentMeat/100)
      
      # slExprtsMeat <- (expBeefLM$coefficients[1] * (slNewFMD + clNewFMD)) %>% as.numeric()
      totExprtsMeat <- (expRatioBeefMean * (slNewFMD + clNewFMD)) %>% as.numeric()
      slExprtsMeat <- (expRatioBeefMean * slNewFMD ) %>% as.numeric()
      # clExprtsMeat <- (expRatioBeefMax * clNewFMD) %>% as.numeric()
      
      slNewFMD <- slNewFMD - slExprtsMeat
      # clNewFMD <- clNewFMD - clExprtsMeat
      clNewFMD <- clNewFMD
      
    }
    
    ####### We write conditions for importation of meat
    #### Simply put, we import meat if meat in the domestic markets is less than historical minimum meat
    
    impBeef[i] <- 0
    
    if(slNewFMD < slHistMin || clNewFMD < clHistMin){
      
      if(slNewFMD < slHistMin){
        # slBeefImports <- (impBeefLM$coefficients[1] * slNewFMD) %>% as.numeric()
        slBeefImports <- (impRatioBeefMax * (slNewFMD + clNewFMD)) %>% as.numeric()
      }else{
        slBeefImports <- 0
      }
      
      if(clNewFMD < clHistMin){
        # clBeefImports <- (impBeefLM$coefficients[1] * clNewFMD) %>% as.numeric()
        clBeefImports <- (impRatioBeefMax * clNewFMD) %>% as.numeric()
      }else{
        clBeefImports <- 0
      }
      impBeef[i] <- slBeefImports + clBeefImports
      
      slNewFMD <- slNewFMD + slBeefImports
      clNewFMD <- clNewFMD + clBeefImports
    }
    
    mergedForecastFMD_Proj$ImportsBeef[mergedForecastFMD_Proj$Year == yearIFMD] <- round(impBeef[i],2)
    
    k <- 0
    
    K1[i] <- capK_pre
    
    slaughterAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) 
                                  %>% select(Slaughter_avg))
    cullAvgFMD <- as.numeric(mergedForecastFMD_Proj %>% filter(Year == yearIFMD) 
                             %>% select(Cull_avg))
    
    slDemFMD <- ANewFMD  * sh_pre
    
    slDemHeadFMD <- slDemFMD * (1000000000/slaughterAvgFMD)
    
    if(i>=3){
      
      #### Here I convert the live animal exports from meat in pounds to live animals
      slExprtsHead <- slExprtsLive 
      # clExprtsHead <- clExprtsLive 
      
      # expTotOG <- slExprtsHead + clExprtsHead
      expTotOG <- slExprtsHead
      expTot <- expTotOG
      
      ### Here I populate the dataframe with the live animals exported
      mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- round(expTot)
      
      ### Here I populate the dataframe with the beef exported
      mergedForecastFMD_Proj$ExportsBeef[mergedForecastFMD_Proj$Year == yearIFMD] <- round(totExprtsMeat,3)
      
      #### Now I determine the total live animals that are exported. This include the live animals and beef exported
      #### So I convert the beef exported from pounds to live animals and add them to the live animals determined before
      #### Note: We are determining these because we must remove these from the existing stocks. Why? Because these are 
      #### exports
      slExprtsHead <- slExprtsHead + slExprtsMeat * (1000000000/slaughterAvgFMD)
      # clExprtsHead <- clExprtsHead + clExprtsMeat * (1000000000/cullAvgFMD)
      
    }
    
    # Chad's suggestion: Use calf crop from 2009 and age distribution from 2009 to get a best estimate of K for 2010
    # So basically use mergedForecastFMD_Proj cap K to get the calf crop and then think creatively to get the K for 2010
    
    #### Here in order to determine K for this year I get the calf crop from two years ago.
    #### We get that by taking the cull cow supply off of the total inventory K and multiply with g
    #### Now only half of those will be heifers. An assumption made in the model. So two years from now
    #### half of the new born will be up for the decision.
    
    exprtsTwo <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>% select(Exports) %>% as.numeric()
    
    if(is.na(exprtsTwo)){
      exprtsTwo <- 0
    }else{
      exprtsTwo <- exprtsTwo
    }
    
    KTwo1 <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>% select(K) %>% as.numeric()
    
    KTwo2 <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>%
      transmute(K1 = k3+k4+k5+k6+k7+k8+k9+k10) %>% select(K1) %>% as.numeric()
    
    clTwo <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-2) %>% 
      mutate(clH = cl * (1000000000/Cull_avg)) %>% select(clH) %>% as.numeric()
    
    #### I believe we have to deflate the cull cow supply as well. Why? Because we deflated the total inventory.
    #### So to match the process. I will try this and see how it goes
    clTwoNew <- clTwo * (1- (dePopR/100))
    exprtsTwoNew <- exprtsTwo * (1- (dePopR/100))
    
    if(i<3){
      KTwo <- KTwo1
      ccY1 <- gFMDCC * (KTwo - clTwoNew)
    }else{
      # KTwo <- ifelse(KTwo1<KTwo2, KTwo1, KTwo2)
      KTwo <- KTwo1
      ccY1 <- gFMDCC * (KTwo - clTwo)
    }
    
    ### Now we must add the calf crop to the existing stocks and take away the fed cattle supplied for the 
    ### meat. For that, first we must take away the cull cows supplied from the existing stock, then add 
    ### the calf crop, take away the fed cattle supplied, and add the replacement heifers.
    KOne1 <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(K) %>% as.numeric()
    
    KOne2 <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>%
      transmute(K1 = k3+k4+k5+k6) %>% select(K1) %>% as.numeric()
    
    clDemHeadFMDOne <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>%
      mutate(clH = cl * (1000000000/Cull_avg)) %>% select(clH) %>% as.numeric()
    
    exprtsFMDOne <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% select(Exports) %>% as.numeric()
    
    if(is.na(exprtsFMDOne)){
      exprtsFMDOne <- 0
    }else{
      exprtsFMDOne <- exprtsFMDOne
    }
    
    #### Similar to the two year lagged quantities I am also deflating the one year lagged quantities
    clDemHeadFMDOneNew <- clDemHeadFMDOne * (1- (dePopR/100))
    exprtsFMDOneNew <- exprtsFMDOne * (1-(dePopR/100))
    
    # KOne <- ifelse(KOne1<KOne2, KOne1,KOne2)
    
    if(i<2){
      KOne <- KOne1
      KOneNew <- delta * (KOne - clDemHeadFMDOneNew)
    }else{
      # KOne <- ifelse(KOne1<KOne2, KOne1,KOne2)
      KOne <- KOne1
      KOneNew <- delta * (KOne - clDemHeadFMDOne)
    }
    
    if(i<2){
      KOne <- KOne2
      KOneM <- delta * KOne
    }else{
      # KOne <- ifelse(KOne1<KOne2, KOne1,KOne2)
      KOne <- KOne2
      KOneM <- delta * KOne 
    }
    
    #### Now the sum of KOneNew and half of calfCrop are up for decision. 
    #### i.e., sum of those are the mature animals up for decision
    
    ############################
    
    ##### Here I add the feeder cattle to the calf crop which are supposed to be slaughtered
    ##### This is done if the U.S. imports cattle
    slaughterImp <- 0
    if(i>1){
      if(impUpdatek3[i-1]>0){
        slaughterImp <- mergedForecastFMD_Proj$Imports[mergedForecastFMD_Proj$Year == yearIFMD-1] * delta
      }
    }
    
    if(i<3){
      k_old_headFMD <- KOneNew + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
      # k_old_headFMD <- KOneM + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
    }else{
      k_old_headFMD <- KOneNew + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
      # k_old_headFMD <- KOneM + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
    }
    
    k_old_headFMD_OG1 <- k_old_headFMD
    
    #### We add the live animals that are supposed to be exported to the replacement heifers
    if(i<3){
      k_old_headFMD <- (k_old_headFMD + exports_Live) %>% as.numeric()
    }else{
      k_old_headFMD <- (k_old_headFMD - exports_Live) %>% as.numeric()
      # if(expUpdatek3[i-1]==1 && expUpdatek3[i-2]==1){
      #   k_old_headFMD <- (k_old_headFMD - slExprtsHead) %>% as.numeric()
      # }
    }
    
    ##### THINK ABOUT EXPORTING IF THE REPLACEMENT HEIFERS THAT ARE HIGHER THAN THE HISTORICAL MAXIMUM
    #### Here after the export ban is lifted I check whether the replacement heifers are greater than the historical 
    #### maximum. If yes, then remove the animals and add them into the exports.
    expUpdatek3[i] <- 0
    slUpdate <- 0
    clUpdate <- 0
    impUpdatek3[i] <- 0
    # k3HistMed
    
    if(k_old_headFMD < k3HistMed){
      impUpdatek3[i] <- 1
    }
    
    mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD] <- k_old_headFMD
    
    if(dePopR == 20){
      if(i<3){
        approxKFMD <- KOneNew + k_old_headFMD 
      }else if(i>=3){
        approxKFMD <- KOneNew + k_old_headFMD
      }
    }else if(dePopR == 10){
      if(i<3){
        approxKFMD <- KOneNew + k_old_headFMD 
      }else if(i>=3){
        approxKFMD <- KOneNew + k_old_headFMD 
      }
    }else{
      if(i<3){
        approxKFMD <- KOneNew + k_old_headFMD 
      }else if(i>=3){
        approxKFMD <- KOneNew + k_old_headFMD
      }
      
    }
    
    #### Here after the export ban is lifted I check whether the total stocks are greater than the historical 
    #### maximum. If yes, then remove the animals and add them into the exports.
    
    expUpdateK[i] <- 0
    impUpdateK[i] <- 0
    
    #### Here I will get how many animals are imported from the condition 
    #### replacement heifers are approximately 25.54% of the calf crop (historical)
    #### Since we let the imports flow even during the disease outbreak, I must check whether we need imports 
    #### during that time. 
    k3_headFMD_OG <- k_old_headFMD
    
    if(impUpdatek3[i]==1){
      # imprtsFMDK <- (impLM$coefficients[1] * approxKFMD) %>% as.numeric()
      imprtsFMDK <- (impRatioMax * approxKFMD) %>% as.numeric()
      mergedForecastFMD_Proj$Imports[mergedForecastFMD_Proj$Year == yearIFMD] <- imprtsFMDK
    }
    
    if(i >= 3){
      
      if(approxKFMD > KHistMax){
        expUpdateK[i] <- 1
      }
      
      if(k_old_headFMD > k3HistMax){
        expUpdatek3[i] <- 1
      }
      
      expK <- 0
      if(expUpdateK[i]==1 && expUpdatek3[i] == 1){
        # exprtsFMDK <- (expLM$coefficients[1] * approxKFMD) %>% as.numeric()
        exprtsFMDK <- (expRatioMax * approxKFMD) %>% as.numeric()
        expK <- 1
        exprtsFMDK1 <- mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] +  exprtsFMDK
      }else if (expUpdatek3[i] == 1){
        # exprtsFMDK <- (expLM$coefficients[1] * k_old_headFMD) %>% as.numeric()
        exprtsFMDK <- (expRatioMax * k_old_headFMD) %>% as.numeric()
      }else{
        exprtsFMDK <- 0
      }
      
      if(mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] > 0){
        if(expK == 1){
          mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- exprtsFMDK
        }else{
          mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- 
            mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] +  exprtsFMDK
        }
      }else{
        mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <-
          exprtsFMDK
      }
      
      if(expUpdateK[i]==1 && expUpdatek3[i] == 1){
        k_old_headFMD <- k_old_headFMD - exprtsFMDK1
        approxKFMD <- approxKFMD - exprtsFMDK1
      }else if(expUpdatek3[i] == 1){
        k_old_headFMD <- k_old_headFMD - exprtsFMDK
        approxKFMD <- approxKFMD - exprtsFMDK
      }else{
        k_old_headFMD <- k_old_headFMD - exprtsFMDK
        approxKFMD <- approxKFMD - exprtsFMDK
      }
    }
    
    if(dePopR==5){
      if(approxKFMD > KHistMed || k_old_headFMD > k3HistMed){
        gFMDCC <- gFMDCC - 0.05
      }
      # else if( k_old_headFMD < k3HistMed){
      #   gFMDCC <- gFMDCC + 0.05
      # }
    }else if(dePopR==10){
      if(approxKFMD > KHistMed || k_old_headFMD > k3HistMed){
        gFMDCC <- gFMDCC - 0.02
      }else if( k_old_headFMD < k3HistMed){
        gFMDCC <- gFMDCC + 0.02
      }
    } 
    
    # else if(dePopR==20){
    #   if(approxKFMD > KHistMed || k_old_headFMD > k3HistMed){
    #     gFMDCC <- gFMDCC - 0.01
    #   }else if( k_old_headFMD < k3HistMed){
    #     gFMDCC <- gFMDCC + 0.01
    #   }
    # }
    
    mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD] <- k_old_headFMD
    
    mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD] <- approxKFMD
    
    hcM_pre <- holdingCostsFuturesFMD$hc[i]
    
    expectedValue_k9FMD <- 
      beta * EpcM_pre + gFMD * (beta^3) * EpsM_pre - (1+g*beta*(gamma0+beta*gamma1)) * hcM_pre
    
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
    
    k9_OldFMD
    
    if(k9_OldFMD == 1){
      mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <-
        mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>%
        mutate(k9 = delta * k8) %>% select(k9) %>% as.numeric()
      
      clNew1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>%
        mutate(clSupp = (k9 + (1-delta) * k8 + (1-delta) * k7) * (Cull_avg/1000000000)) %>%
        select(clSupp) %>% as.numeric()
      
    }else if(k9_OldFMD == 0) {
      
      k9NextFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>%
        mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9)) %>% select(k9) %>% 
        as.numeric()
      
      clNew1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% 
        mutate(clSupp = (k10 + k9NextFMD + k8 + (1-delta) * k7)* (Cull_avg/1000000000)) %>% 
        select(clSupp) %>%  as.numeric()
      
      mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
      
    } else if(k9_OldFMD == 2){
      
      k9NextFMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>%
        mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9)) %>% select(k9) %>%
        as.numeric()
      
      k8NextFMD <- mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD]
      
      clNew1FMD <- mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% 
        mutate(clSupp = (k10 + k9NextFMD + k8NextFMD + (1-delta) * k7) * (Cull_avg/1000000000)) %>% 
        select(clSupp) %>% as.numeric()
      
      mergedForecastFMD_Proj$k9[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
      mergedForecastFMD_Proj$k8[mergedForecastFMD_Proj$Year == yearIFMD] <- 0
      mergedForecastFMD_Proj$k7[mergedForecastFMD_Proj$Year == yearIFMD] <-
        mergedForecastFMD_Proj %>% filter(Year == yearIFMD-1) %>% mutate(k7 = delta * k6) %>% select(k7) %>% as.numeric()
      
    }
    
    if(clNew1FMD > clNewFMD_OG){
      clNewFMD_OG <- clNew1FMD
      if(i==1){
        clNew <- clNew1FMD 
      }else if(i==2){
        clNew <- clNew1FMD 
      }else{
        clNew <- clNew1FMD 
      }
      clNewFMD <- clNew
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
    
    beefINV_FORECAST_PostFMD[i,] <- 
      mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Year, K, k3, k4, k5, k6, k7, k8, k9)
    
    mergedForecastFMD_Proj$clMedian[mergedForecastFMD_Proj$Year == yearIFMD] <- clNewFMD_OG
    mergedForecastFMD_Proj$slMedian[mergedForecastFMD_Proj$Year == yearIFMD] <- slNewFMD_OG
    
    PsFMD <- getPsPcEpsEpc_FMD_EQ_OPT(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                                      HcM = hcM_pre, SlNew = slNewFMD, ClNew = clNewFMD, ANew = capAFMD, 
                                      params = c(MUtilde_pre, Stilde_pre), depops = dePopR)
    
    psM_pre <- PsFMD[1]
    pcM_pre <- PsFMD[2]
    hcM_pre <- PsFMD[3]
    EpsM_pre <- PsFMD[4]
    EpcM_pre <- PsFMD[5]
    
    ##### Here I am making sure the demand is decreased in the initial years of the disease outbreak
    ##### In order to do that, first I compute the demand under new prices. If the demand is greater than
    ##### 0.95 of the original demand, change the prices and supplies simultaneously and run the model
    ##### until the demand reaches the 0.95 of the original
    if(i == 1){
      
      # capAFMD_DollarsAfter <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
      
      #### Here I compute the demand in dollars. Basically multiply the supplied quantities with the 
      #### price determined from the equilibrium conditions. Note: I don't have to use the markup parameter 
      #### to convert the price to retail price. It's constant and it's 
      capAFMD_DollarsAfter <- slNewFMD * psM_pre + clNewFMD * pcM_pre
      
      while(capAFMD_DollarsAfter > 0.95 * capAFMD_DollarsOG){
        
        if(dePopR == 5){
          
          psM_pre <- psM_pre - 0.01
          pcM_pre <- pcM_pre - 0.01
          
        }else if(dePopR == 10){
          
          psM_pre <- psM_pre - 0.01
          pcM_pre <- pcM_pre - 0.01
          
        }else if(dePopR == 20){
          
          psM_pre <- psM_pre - 0.01
          pcM_pre <- pcM_pre - 0.01
          
        }else{
          psM_pre <- psM_pre - 0.01
          pcM_pre <- pcM_pre - 0.01
        }
        
        capAFMD_DollarsAfter <- slNewFMD * psM_pre + clNewFMD * pcM_pre
        
      }
      
    }
    
    proj_Q_P_PostFMD$Ps[i] <- psM_pre
    proj_Q_P_PostFMD$Pc[i] <- pcM_pre
    proj_Q_P_PostFMD$Hc[i] <- hcM_pre
    proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
    proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
    
    proj_Q_P_PostFMD$Sl[i] <- slNewFMD
    proj_Q_P_PostFMD$Cl[i] <- clNewFMD
    proj_Q_P_PostFMD$A[i] <- capAFMD
    proj_Q_P_PostFMD$repHeif_Head[i] <- k_old_headFMD
    
    proj_Q_P_PostFMD$boundCond[i] <- abs(k_old_headFMD) <= 0.5 * gFMD * K1[i]
    
    beefINV_FORECAST_PostFMD[i,] <- 
      mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Year, K, k3, k4, k5, k6, k7, k8, k9)
    
    capK_pre <- beefINV_FORECAST_PostFMD[i,]$K
    
    capAFMD_DollarsAfter <- slNewFMD * psM_pre + clNewFMD * pcM_pre
    
    proj_Q_P_PostFMD$demDollarsAfter[i] <- capAFMD_DollarsAfter
    
    capAFMD <- (slNewFMD_OG + clNewFMD_OG) * shockD_pre
    
  }
  
  return(list(proj_Q_P_PostFMD, beefINV_FORECAST_PostFMD, mergedForecastFMD_Proj))
  
}