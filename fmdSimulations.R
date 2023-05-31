
############################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################
###################################################### FMD SIMULATIONS WORK ################################################################################################################################################################################
############################################################################################################################################################################################################################################################
############################################################################################################################################################################################################################################################

############################################################################################################################################################################################################################################################
estPFunction_FMD <- function(p, sl, cl, A, B, hc_discounted, tilde_MU, tilde_s, hc_new){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  F1 <- sl - A * ((exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))/(1 + (exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))))
  
  F2 <- cl  - A * (1/(1+ exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s)))
  
  F3 <- B - ps + gFMD * (beta^3) * Eps3 - hc_discounted
  
  F4 <- pc - beta * Epc1 - gFMD * (beta^3) * Eps3 + (1 + gFMD * beta * (gamma0 + beta * gamma1)) * hc_new
  
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  
  return(F)
  
}



getPsPcEpsEpc_FMD_EQ_OPT <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, 
                                 ANew, params, depops){
  
    psNew <- PsM
    pcNew <- PcM
    
    psNew_expected <- EPsM
    pcNew_expected <- EPcM
    
    
    # psNew_expected_lo <- psNew_expected 
    # 
    # psNew_expected_up <- psNew_expected + 0.1
    # 
    # pcNew_expected_lo <- pcNew_expected 
    # 
    # pcNew_expected_up <- pcNew_expected + 0.1
    
    
    if(depops == 5){
      
      # OPT
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.001
      # 
      # psNew_up <- psNew + 0.07
      # pcNew_up <- pcNew + 0.8
      
      psNew_lo <- psNew  - 0.02
      pcNew_lo <- pcNew - 0.03
      
      psNew_up <- psNew + 0.08
      pcNew_up <- pcNew + 0.04
      
      # psNew_up <- psNew + 0.08
      # pcNew_up <- pcNew + 0.05
      
      
      # PES
      # psNew_lo <- psNew  - 0.08
      # pcNew_lo <- pcNew - 0.05
      # 
      # psNew_up <- psNew + 0.5
      # pcNew_up <- pcNew + 0.25
      
      psNew_expected_lo <- psNew_expected - 0.01
      
      psNew_expected_up <- psNew_expected + 0.5
      
      pcNew_expected_lo <- pcNew_expected - 0.01
      
      pcNew_expected_up <- pcNew_expected + 0.5
      
      
    } else if(depops == 10){
      
      #OPT
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.01
      # 
      # psNew_up <- psNew + 0.05
      # pcNew_up <- pcNew + 0.8
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.001
      # 
      # psNew_up <- psNew + 0.07
      # pcNew_up <- pcNew + 0.8
      
      psNew_lo <- psNew  - 0.02
      pcNew_lo <- pcNew - 0.03
      
      psNew_up <- psNew + 0.08
      pcNew_up <- pcNew + 0.04
      
      # PES
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.001
      # 
      # psNew_up <- psNew + 0.04
      # pcNew_up <- pcNew + 1
      psNew_expected_lo <- psNew_expected - 0.01
      
      psNew_expected_up <- psNew_expected + 0.5
      
      pcNew_expected_lo <- pcNew_expected - 0.01
      
      pcNew_expected_up <- pcNew_expected + 0.5
      
    } else if(depops == 20){
      
      # OPT
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.01
      # 
      # psNew_up <- psNew + 0.03
      # pcNew_up <- pcNew + 0.8
      
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.001
      # 
      # psNew_up <- psNew + 0.07
      # pcNew_up <- pcNew + 0.8
      
      psNew_lo <- psNew  - 0.08
      pcNew_lo <- pcNew - 0.05
      
      psNew_up <- psNew + 0.08
      pcNew_up <- pcNew + 0.05
      
      # PES
      # psNew_lo <- psNew  - 0.5
      # pcNew_lo <- pcNew - 0.001
      # 
      # psNew_up <- psNew + 0.05
      # pcNew_up <- pcNew + 1.5
      
      psNew_expected_lo <- psNew_expected - 0.01
      
      psNew_expected_up <- psNew_expected + 0.5
      
      pcNew_expected_lo <- pcNew_expected - 0.01
      
      pcNew_expected_up <- pcNew_expected + 0.5
      
    }else{
      
      psNew_lo <- psNew  - 0.08
      pcNew_lo <- pcNew - 0.05
      
      psNew_up <- psNew + 0.08
      pcNew_up <- pcNew + 0.05
      
      # PES
      # psNew_lo <- psNew  - 0.08
      # pcNew_lo <- pcNew - 0.05
      # 
      # psNew_up <- psNew + 0.5
      # pcNew_up <- pcNew + 0.25
      
      psNew_expected_lo <- psNew_expected - 0.01
      
      psNew_expected_up <- psNew_expected + 0.5
      
      pcNew_expected_lo <- pcNew_expected - 0.01
      
      pcNew_expected_up <- pcNew_expected + 0.5
    }
    
    #### Here we are making sure the lower bound for the prices isn't negative
    if(psNew_lo < 0){
      psNew_lo <- psNew
    }
    
    if(pcNew_lo < 0){
      pcNew_lo <- pcNew
    }
    
    #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
    # while( pcNew_lo > psNew_lo ){
    #   pcNew_lo <- pcNew_lo - 0.01
    # }
    
    while(psNew_expected_lo < 0){
      psNew_expected_lo <- psNew_expected_lo + 0.08
    }
    
    while(pcNew_expected_lo < 0){
      pcNew_expected_lo <- pcNew_expected_lo + 0.08
    }
    
    # hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    #   (beta * pcNew_expected + g * (beta^3) * psNew_expected - pcNew)
    
    hc_new <- HcM
    
    while(hc_new < 0){
      hc_new <- hc_new + 0.1
    }
    
    #### Here we make sure that the holding costs are below the cull cow price
    # while(hc_new > pcNew){
    #   hc_new <- hc_new - 0.1
    # }
    # 
    # hc_new_lo <- hc_new - 0.1
    # hc_new_up <- hc_new + 0.1
    # 
    # while(hc_new_lo<0){
    #   hc_new_lo <- hc_new_lo + 0.1
    # }
    # 
    # while(hc_new_up > pcNew_up){
    #   hc_new_up <- hc_new_up - 0.1
    # }
    
    hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + gFMD * beta * (gamma0 + beta * gamma1)) * hc_new
    B <- psNew - gFMD * (beta^3) * psNew_expected + hc_discounted
    
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
    
    hc1N <- (1/(1+ gFMD * beta * (gamma0 + beta * gamma1))) * 
      (beta * pc_expected1N + gFMD * (beta^3) * ps_expected1N - pc1N)
    
    return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}



getPsPcEpsEpc_FMD_EQ_PES <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, 
                                     ANew, params, depops){
  
      psNew <- PsM
      pcNew <- PcM
      
      psNew_expected <- EPsM
      pcNew_expected <- EPcM
      
      if(depops == 5){
        
        # OPT
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.001
        # 
        # psNew_up <- psNew + 0.07
        # pcNew_up <- pcNew + 0.8
        
        psNew_lo <- psNew  - 0.02
        pcNew_lo <- pcNew - 0.03
        
        psNew_up <- psNew + 0.08
        pcNew_up <- pcNew + 0.04
        
        # psNew_up <- psNew + 0.08
        # pcNew_up <- pcNew + 0.05
        
        
        # PES
        # psNew_lo <- psNew  - 0.08
        # pcNew_lo <- pcNew - 0.05
        # 
        # psNew_up <- psNew + 0.5
        # pcNew_up <- pcNew + 0.25
        
        psNew_expected_lo <- psNew_expected - 0.01
        
        psNew_expected_up <- psNew_expected + 0.5
        
        pcNew_expected_lo <- pcNew_expected - 0.01
        
        pcNew_expected_up <- pcNew_expected + 0.5
        
        
      } else if(depops == 10){
        
        #OPT
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.01
        # 
        # psNew_up <- psNew + 0.05
        # pcNew_up <- pcNew + 0.8
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.001
        # 
        # psNew_up <- psNew + 0.07
        # pcNew_up <- pcNew + 0.8
        
        psNew_lo <- psNew  - 0.02
        pcNew_lo <- pcNew - 0.03
        
        psNew_up <- psNew + 0.08
        pcNew_up <- pcNew + 0.04
        
        # PES
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.001
        # 
        # psNew_up <- psNew + 0.04
        # pcNew_up <- pcNew + 1
        psNew_expected_lo <- psNew_expected - 0.01
        
        psNew_expected_up <- psNew_expected + 0.5
        
        pcNew_expected_lo <- pcNew_expected - 0.01
        
        pcNew_expected_up <- pcNew_expected + 0.5
        
      } else if(depops == 20){
        
        # OPT
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.01
        # 
        # psNew_up <- psNew + 0.03
        # pcNew_up <- pcNew + 0.8
        
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.001
        # 
        # psNew_up <- psNew + 0.07
        # pcNew_up <- pcNew + 0.8
        
        psNew_lo <- psNew  - 0.08
        pcNew_lo <- pcNew - 0.05
        
        psNew_up <- psNew + 0.08
        pcNew_up <- pcNew + 0.05
        
        # PES
        # psNew_lo <- psNew  - 0.5
        # pcNew_lo <- pcNew - 0.001
        # 
        # psNew_up <- psNew + 0.05
        # pcNew_up <- pcNew + 1.5
        
        psNew_expected_lo <- psNew_expected - 0.01
        
        psNew_expected_up <- psNew_expected + 0.5
        
        pcNew_expected_lo <- pcNew_expected - 0.01
        
        pcNew_expected_up <- pcNew_expected + 0.5
        
      }else{
        
        psNew_lo <- psNew  - 0.08
        pcNew_lo <- pcNew - 0.05
        
        psNew_up <- psNew + 0.08
        pcNew_up <- pcNew + 0.05
        
        # PES
        # psNew_lo <- psNew  - 0.08
        # pcNew_lo <- pcNew - 0.05
        # 
        # psNew_up <- psNew + 0.5
        # pcNew_up <- pcNew + 0.25
        
        psNew_expected_lo <- psNew_expected - 0.01
        
        psNew_expected_up <- psNew_expected + 0.5
        
        pcNew_expected_lo <- pcNew_expected - 0.01
        
        pcNew_expected_up <- pcNew_expected + 0.5
      }
      
      #### Here we are making sure the lower bound for the prices isn't negative
      if(psNew_lo < 0){
        psNew_lo <- psNew
      }
      
      if(pcNew_lo < 0){
        pcNew_lo <- pcNew
      }
      
      #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
      # while( pcNew_lo > psNew_lo ){
      #   pcNew_lo <- pcNew_lo - 0.01
      # }
      
      while(psNew_expected_lo < 0){
        psNew_expected_lo <- psNew_expected_lo + 0.08
      }
      
      while(pcNew_expected_lo < 0){
        pcNew_expected_lo <- pcNew_expected_lo + 0.08
      }
      
      
      # hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
      #   (beta * pcNew_expected + g * (beta^3) * psNew_expected - pcNew)
      
      hc_new <- HcM
      
      while(hc_new < 0){
        hc_new <- hc_new + 0.1
      }
      
      #### Here we make sure that the holding costs are below the cull cow price
      # while(hc_new > pcNew){
      #   hc_new <- hc_new - 0.1
      # }
      # 
      # hc_new_lo <- hc_new - 0.1
      # hc_new_up <- hc_new + 0.1
      # 
      # while(hc_new_lo<0){
      #   hc_new_lo <- hc_new_lo + 0.1
      # }
      # 
      # while(hc_new_up > pcNew_up){
      #   hc_new_up <- hc_new_up - 0.1
      # }
      
      hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + gFMD * beta * (gamma0 + beta * gamma1)) * hc_new
      B <- psNew - gFMD * (beta^3) * psNew_expected + hc_discounted
      
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
      
      hc1N <- (1/(1+ gFMD * beta * (gamma0 + beta * gamma1))) * 
        (beta * pc_expected1N + gFMD * (beta^3) * ps_expected1N - pc1N)
      
      return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}



dePop <- function(stock, dePopRate){
  stock[,-1] <- stock[,-1] - stock[,-1] * (dePopRate/100)
  return(stock)
}



############################################################################################################################################################################################################################################################

EQ_muTildes <- mu_Tildes_MMNIII
EQ_sTildes <- s_Tildes_MMNIII
EQ_demandShocks <- demandShockGaussian1 %>% transmute(Year = Year, dShock = Shock)

EQ_PricesCosts <- Reduce(function(...) merge(...), 
                         list(EQestPSNIII,EQestPCNIII,EQestHCNIII, EQestEPSNIII, EQestEPCNIII))

EQ_Supplies <- Reduce(function(...) merge(...), 
                      list(EQestObsSLNIII %>% select(-errMean, -errmedian),
                           EQestObsCLNIII%>% select(-errMean, -errmedian)))

#### Arranging the data
EQ_K_t <- Stock %>% transmute(Year = Year, K = K)
EQ_A <- A_quant

FMD_AllDF_EQ <- Reduce(function(...) merge(...), 
                       list(EQ_K_t, EQ_A, proj_adjFac, EQ_muTildes, EQ_sTildes, EQ_PricesCosts, 
                            EQ_Supplies,dressedWeights_sl_cl, EQ_demandShocks)) %>% round(2) 

modelParamsEQ_PreFMD <- FMD_AllDF_EQ %>% filter(Year <= 2021)

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

#### Here I get all the equilibrium estimates
modelParamsEQ_PreFMD <- FMD_AllDF_EQ %>% filter(Year <= 2021)

slaughterAvg_pre <- modelParamsEQ_PreFMD %>% filter(Year == 2021) %>% select(Slaughter_avg) %>% as.numeric()

#### I convert exported live animals from number of head to pounds in meat
exports_2021_Live <- stocksImportsExports %>% filter(Year == 2021) %>% select(Exports) %>% as.numeric()
exports_2021_LiveMeat <- exports_2021_Live * (slaughterAvg_pre/1000000000)
#### Here I get the exported meat and add this and the above computed meat
exportsBeef_2021 <- exportsBeef %>% filter(Year == 2021) %>% select(Exports) %>% as.numeric()
totalBeefExportsMeat_2021 <- round(exports_2021_LiveMeat + exportsBeef_2021, 3)
##### I get the production (without imports) and determine the export percentage
PR_2021 <- exportsBeef %>% filter(Year == 2021) %>% select(Production) %>% as.numeric()
# capK_pre_meat <- capK_pre * (slaughterAvg_pre/1000000000)
exports_percentK <- round((totalBeefExportsMeat_2021/PR_2021) * 100,3)

### Here I get the export percentage of boxed beef. This is used when the exports are resumed
exports_percentMeat <- round((exportsBeef_2021/PR_2021) * 100,3)

exports_LiveK <- stocksImportsExports %>% filter(Year == 2021) %>% 
  transmute(expK = round((Exports/K)*100,3)) %>% select(expK) %>% as.numeric()


# I get historical maximum, minimum, and median supplies
slHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2021 ) %>% select(slSM) %>% max()
clHistMax <- FMD_AllDF_EQ %>% filter(Year <= 2021 ) %>% select(clSM) %>% max()

slHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2021 ) %>% select(slSM) %>% min()
clHistMin <- FMD_AllDF_EQ %>% filter(Year <= 2021 ) %>% select(clSM) %>% min()

slHistMed <- median(FMD_AllDF_EQ$slSM[FMD_AllDF_EQ$Year<=2021]) 
clHistMed <- median(FMD_AllDF_EQ$clSM[FMD_AllDF_EQ$Year<=2021]) 

slHistMean <- mean(FMD_AllDF_EQ$slSM[FMD_AllDF_EQ$Year<=2021]) 
clHistMean <- mean(FMD_AllDF_EQ$clSM[FMD_AllDF_EQ$Year<=2021]) 

# I get historical maximum, minimum, median of K and k3
k3HistMax <- Stock %>% filter(Year <= 2021 & Year >= 2000) %>% select(k3) %>% max()
KHistMax <- Stock %>% filter(Year <= 2021 & Year >= 2000) %>% select(K) %>% max()

k3HistMin <- Stock %>% filter(Year <= 2021 & Year >= 2000) %>% select(k3) %>% min()
KHistMin <- Stock %>% filter(Year <= 2021 & Year >= 2000) %>% select(K) %>% min()

k3HistMed <- median(Stock$k3[Stock$Year <=2021 & Stock$Year >=2000])
k3HistMean <- mean(Stock$k3[Stock$Year <=2021 & Stock$Year >=2000])

KHistMed <- median(Stock$K[Stock$Year <=2021 & Stock$Year >=2000])
KHistMean <- mean(Stock$K[Stock$Year <=2021 & Stock$Year >=2000])

impHistMax <- stocksImportsExports %>% filter(Year <= 2021 & Year >= 2000) %>% select(Imports) %>% max()
impHistMin <- stocksImportsExports %>% filter(Year <= 2021 & Year >= 2000) %>% select(Imports) %>% min()
impHistMed <- median(stocksImportsExports$Imports[stocksImportsExports$Year <=2021 & stocksImportsExports$Year >=2000])
impHistMean <- mean(stocksImportsExports$Imports[stocksImportsExports$Year <=2021 & stocksImportsExports$Year >=2000])

expHistMax <- stocksImportsExports %>% filter(Year <= 2021 & Year >= 2000) %>% select(Exports) %>% max()
expHistMin <- stocksImportsExports %>% filter(Year <= 2021 & Year >= 2000) %>% select(Exports) %>% min()
expHistMed <- median(stocksImportsExports$Exports[stocksImportsExports$Year <=2021 & stocksImportsExports$Year >=2000])
expHistMean <- mean(stocksImportsExports$Exports[stocksImportsExports$Year <=2021 & stocksImportsExports$Year >=2000])

# Here I get the percentage of imports and exports (live animals) with respect to K
expRatioK <- stocksImportsExports %>% transmute(Year = Year, expRatio = Exports/K) %>% filter(Year >= 1990)
expLM <- stocksImportsExports %>% filter(Year >= 1990) %>% lm(formula=Exports~0+K) %>% summary()
expRatioMax <- max(expRatioK$expRatio)
expRatioMin <- min(expRatioK$expRatio)
expRatioMed <- median(expRatioK$expRatio)
expRatioMean <- mean(expRatioK$expRatio)

impRatioK <- stocksImportsExports %>% transmute(Year = Year, impRatio = Imports/K) %>% filter(Year >= 1990)
impLM <- stocksImportsExports %>% filter(Year >= 1990) %>% lm(formula=Imports~0+K) %>% summary()
impRatioMax <- max(impRatioK$impRatio)
impRatioMin <- min(impRatioK$impRatio)
impRatioMed <- median(impRatioK$impRatio)
impRatioMean <- mean(impRatioK$impRatio)

# Here I get the percentage of imports and exports (beef) with respect to total production
expBeefRatioP <- exportsBeef %>% transmute(Year = Year, expBeefRatio = Exports/Production) %>% filter(Year >= 1990)
expBeefLM <- exportsBeef %>% filter(Year >= 1990) %>% lm(formula=Exports~0+Production) %>% summary()
expRatioBeefMax <- max(expBeefRatioP$expBeefRatio)
expRatioBeefMin <- min(expBeefRatioP$expBeefRatio)
expRatioBeefMed <- median(expBeefRatioP$expBeefRatio)
expRatioBeefMean <- mean(expBeefRatioP$expBeefRatio)

impBeefRatioP <- exportsBeef %>% transmute(Year = Year, impBeefRatio = Imports/Production) %>% filter(Year >= 1990)
impBeefLM <- exportsBeef %>% filter(Year >= 1990) %>% lm(formula=Imports~0+Production) %>% summary()
impRatioBeefMax <- max(impBeefRatioP$impBeefRatio)
impRatioBeefMin <- min(impBeefRatioP$impBeefRatio)
impRatioBeefMed <- median(impBeefRatioP$impBeefRatio)
impRatioBeefMean <- mean(impBeefRatioP$impBeefRatio)

nn <- 10

#### When adding the stocks or evolution of the stocks, keep in mind the calf crop must come from the breeding animals
#### in the stock. Do not use the cap K to get the calf crop and then decide the replacement heifers.
#### Making this change might not blow up the stock levels. This is crucial I think.

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
                                       demDollarsAfter = numeric(nn), Sl_OG = numeric(nn), Cl_OG = numeric(nn),
                                       SlDem = numeric(nn), ClDem = numeric(nn))
        
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
        
        #### Here I depop the stocks (5%, 10%)
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
          
          # i <- 1
          
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
          
          # ANewFMD <- (slDem + clDem)
          
          ##### Here I use the demand (in quantity) from previous year to get the demand (in dollars)
          ##### Also I compute the live animals that are supposed to be exported using the linear model coefficients
          if(i==1){
            # capAFMD <- capA_pre
            capAFMD <- (slSM_pre + clSM_pre)
            # If the depop is 5% then the quantity demanded must be cut off as well
            capAFMD_DollarsOG <- slSM_pre * (psM_pre/phi) + clSM_pre * (pcM_pre/phi)
            psMOG <- psM_pre
            pcMOG <- pcM_pre
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
            slExprtsMeat <- (expRatioBeefMax * slNewFMD) %>% as.numeric()
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
            slExprtsLive <- expRatioMed * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
            exports_Live <- slExprtsLive
            # slExprtsMeat <- slNewFMD * (exports_percentMeat/100)
            # clExprtsMeat <- clNewFMD * (exports_percentMeat/100)
            
            # slExprtsMeat <- (expBeefLM$coefficients[1] * (slNewFMD + clNewFMD)) %>% as.numeric()
            totExprtsMeat <- (expRatioBeefMean * (slNewFMD + clNewFMD)) %>% as.numeric()
            slExprtsMeat <- (expRatioBeefMin * slNewFMD ) %>% as.numeric()
            # clExprtsMeat <- (expRatioBeefMax * clNewFMD) %>% as.numeric()
            
            slNewFMD <- slNewFMD - slExprtsMeat
            # clNewFMD <- clNewFMD - clExprtsMeat
            clNewFMD <- clNewFMD
            
          }
          
          ####### We write conditions for importation of meat
          #### Simply put, we import meat if meat in the domestic markets is less than historical minimum meat
          
          impBeef[i] <- 0
          
          if(i>1){
            proj_Q_P_PostFMD$SlDem[i] <- slDem
            proj_Q_P_PostFMD$ClDem[i] <- clDem
          }
            
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
          
          # Chad's suggestion: Use calf crop from 2009 and age distribution from 2009 to get a best estimate of K for 2023
          # So basically use mergedForecastFMD_Proj cap K to get the calf crop and then think creatively to get the K for 2023
          
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
          k3HistMed
          repRatio * ccY1
          
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
              if(approxKFMD > KHistMin){
                exprtsFMDK <- (expRatioMax * k_old_headFMD) %>% as.numeric()
              }else{
                exprtsFMDK <- 0
              }
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
                gFMDCC <- gFMDCC - 0.048
              } 
              # else if( k_old_headFMD < k3HistMed){
              #   gFMDCC <- gFMDCC + 0.01
              # }
            }else if(dePopR==10){
              if(approxKFMD > KHistMed || k_old_headFMD > k3HistMed){
                gFMDCC <- gFMDCC - 0.0228
              }
              # else if( k_old_headFMD < k3HistMed){
              #   gFMDCC <- gFMDCC + 0.005
              # }
            }
          
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
          
          if(i>1){
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
          }
          
          if(i == 1){
            # capAFMD_DollarsAfter <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
            
            #### Here I compute the demand in dollars. Basically multiply the supplied quantities with the 
            #### price determined from the equilibrium conditions. Note: I don't have to use the markup parameter 
            #### to convert the price to retail price.
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
            
            while(capAFMD_DollarsAfter > 0.95 * capAFMD_DollarsOG){
              
              if(dePopR == 5){
                
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
                
              }else if(dePopR == 10){
                
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
                
              }else{
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
              }
              
              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
              
            }
            
          }
          
          
          ##############################################################################################
          ###################### INCREASE THE DEMAND HERE. ADJUST FOR QUANTITIES AS WELL ###############
          
          if(i==2){
            
            while(capAFMD_DollarsAfter < capAFMD_DollarsOG){
              
              if(dePopR == 5){
                
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
                
              }else if(dePopR == 10){
                
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
                
              }else{
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
              }
              
              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
              
            }
          }
          
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
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
            
            while(capAFMD_DollarsAfter > 0.95 * capAFMD_DollarsOG){
              
              if(dePopR == 5){
                
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
                
              }else if(dePopR == 10){
                
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
                
              }else{
                psM_pre <- psM_pre - 0.01
                pcM_pre <- pcM_pre - 0.01
              }
              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
              
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
          # proj_Q_P_PostFMD$A[i] <- (slNewFMD + clNewFMD)
          proj_Q_P_PostFMD$repHeif_Head[i] <- k_old_headFMD
          
          proj_Q_P_PostFMD$boundCond[i] <- abs(k_old_headFMD) <= 0.5 * gFMD * K1[i]
          
          beefINV_FORECAST_PostFMD[i,] <- 
            mergedForecastFMD_Proj %>% filter(Year == yearIFMD) %>% select(Year, K, k3, k4, k5, k6, k7, k8, k9)
          
          capK_pre <- beefINV_FORECAST_PostFMD[i,]$K
          
          if(i>=3){
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
          }
          
          proj_Q_P_PostFMD$demDollarsAfter[i] <- capAFMD_DollarsAfter
          
          # capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
          # if(i>1){
          #   capAFMD_DollarsAfter <- slDem * (psM_pre/phi) + clDem * (pcM_pre/phi)
          # }
          
          if(i>1){
              if(proj_Q_P_PostFMD$Ps[i]<proj_Q_P_PostFMD$Ps[i-1]){
                ### -1.232 comes from the demand elasticity of fed cattle beef which is at -0.8611.
                ### So if the price declines the demand for the beef goes up by dem^(1/elasticity) so
                ### we have slNewFMD + slNewFMD^(1/-0.8611) = slNewFMD + slNewFMD^(-1.232)
                #### Im Paarlberg they have -1.521 as the own price elasticity. I can use that too.
                # slDem <- slNewFMD + slNewFMD^(-1.232)
                slDem <- slNewFMD * shockD_pre
              }else{
                # slDem <- slNewFMD - slNewFMD^(-1.232)
                slDem <- slNewFMD * adjF_pre
              }
              if(proj_Q_P_PostFMD$Pc[i]<proj_Q_P_PostFMD$Pc[i-1]){
                # clDem <- clNewFMD + clNewFMD^(-2)
                clDem <- clNewFMD * shockD_pre
              }else{
                # clDem <- clNewFMD - clNewFMD^(-2)
                clDem <- clNewFMD * adjF_pre
              }
              capAFMD <- slDem + clDem
          }else{
            if(proj_Q_P_PostFMD$Ps[i] < psMOG){
              # slDem <- slNewFMD + slNewFMD^(-1.232)
              slDem <- slNewFMD * shockD_pre
            }else{
              # slDem <- slNewFMD - slNewFMD^(-1.232)
              slDem <- slNewFMD * adjF_pre
            }
            if(proj_Q_P_PostFMD$Pc[i] < pcMOG){
              # clDem <- clNewFMD + clNewFMD^(-2)
              clDem <- clNewFMD * shockD_pre
            }else{
              # clDem <- clNewFMD - clNewFMD^(-2)
              clDem <- clNewFMD * adjF_pre
            }
            capAFMD <- slDem + clDem
          }
          
          # if(i>1){
          #   slDem <- slDem + (-0.8611) * slDem * ((proj_Q_P_PostFMD$Ps[i] - proj_Q_P_PostFMD$Ps[i-1])/proj_Q_P_PostFMD$Ps[i-1])
          #   clDem <- clDem + (-0.5) * clDem * ((proj_Q_P_PostFMD$Pc[i] - proj_Q_P_PostFMD$Pc[i-1])/proj_Q_P_PostFMD$Pc[i-1])
          #   capAFMD <- slDem + clDem
          # }else{
          #   slDem <- slDem + (-0.8611) * slDem * ((proj_Q_P_PostFMD$Ps[i] - psMOG)/psMOG)
          #   clDem <- clDem + (-0.5) * clDem * ((proj_Q_P_PostFMD$Pc[i] - pcMOG)/pcMOG)
          #   capAFMD <- slDem + clDem
          # }
          
        }
        
        return(list(proj_Q_P_PostFMD, beefINV_FORECAST_PostFMD, mergedForecastFMD_Proj))
        
}


# optimisticPostFMD_5 <- simOptimisticFMD(dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                          exports_percentK = exports_percentK, nn = 11, Stock = Stock)

# optimisticPostFMD_5I <- simOptimisticFMD(dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                         exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# 
# optimisticPostFMD_10 <- simOptimisticFMD(dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                          exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# optimisticPostFMD_20 <- simOptimisticFMD(dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                          exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# 
# 
# 
# 
# optimisticPostFMD_0 <- simOptimisticFMD(dePopR = 0, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                          exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# 
# postFMD_P_Q_5_Opt <- optimisticPostFMD_5[[1]]
# postFMD_INV_5_Opt <- optimisticPostFMD_5[[2]]
# postFMD_MER_INV_5_Opt <- optimisticPostFMD_5[[3]]
# 
# 
# postFMD_P_Q_5_OptI <- optimisticPostFMD_5I[[1]]
# postFMD_INV_5_OptI <- optimisticPostFMD_5I[[2]]
# postFMD_MER_INV_5_OptI <- optimisticPostFMD_5I[[3]]
# 
# 
# 
# postFMD_P_Q_10_Opt <- optimisticPostFMD_10[[1]]
# postFMD_INV_10_Opt <- optimisticPostFMD_10[[2]]
# postFMD_MER_INV_10_Opt <- optimisticPostFMD_10[[3]]
# 
# postFMD_P_Q_20_Opt <- optimisticPostFMD_20[[1]]
# postFMD_INV_20_Opt <- optimisticPostFMD_20[[2]]
# postFMD_MER_INV_20_Opt <- optimisticPostFMD_20[[3]]
# 
# postFMD_P_Q_0_Opt <- optimisticPostFMD_0[[1]]
# postFMD_INV_0_Opt <- optimisticPostFMD_0[[2]]
# postFMD_MER_INV_0_Opt <- optimisticPostFMD_0[[3]]


################### SIMLUATIONS AFTER CHANGING CODE AND OTHER THINGS ###############

# optimisticPostFMD_5_08 <- simOptimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)
# 
# optimisticPostFMD_10_08 <- simOptimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)
# 
# optimisticPostFMD_20_08 <- simOptimisticFMD(
#   dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD20)

# optimisticPostFMD_5_0822 <- simOptimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)
# 
# optimisticPostFMD_10_0822 <- simOptimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)
# 
# optimisticPostFMD_20_0822 <- simOptimisticFMD(
#   dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD20)


# optimisticPostFMD_5_0831 <- simOptimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

optimisticPostFMD_5_0902 <- simOptimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

optimisticPostFMD_5_0904 <- simOptimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)


optimisticPostFMD_5_0909 <- simOptimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

optimisticPostFMD_5_0909[[1]]$Ps

# optimisticPostFMD_10_0831 <- simOptimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)

optimisticPostFMD_10_0902 <- simOptimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)

optimisticPostFMD_10_0904 <- simOptimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)

optimisticPostFMD_10_0909 <- simOptimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)



simPessimisticFMD <- function(dePopR, modelParamsEQ_PreFMD, exports_percentK, 
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
                                       demDollarsAfter = numeric(nn), Sl_OG = numeric(nn), Cl_OG = numeric(nn),
                                       SlDem = numeric(nn), ClDem = numeric(nn))
        
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
          
          # i <- 10
          
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
          
          if(i==1){
            # capAFMD <- capA_pre
            # capAFMD_DollarsOG <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
            capAFMD <- (slSM_pre + clSM_pre)
            capAFMD_DollarsOG <- slSM_pre * (psM_pre/phi) + clSM_pre * (pcM_pre/phi)
            psMOG <- psM_pre
            pcMOG <- pcM_pre
            slExprtsLive <- expRatioMax * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
            exports_Live <- slExprtsLive 
          }else if(i>=2 && i <=5){
            slExprtsLive <- expRatioMax * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
            exports_Live <- slExprtsLive
          }
          
          #### Export markets
          #### For the export markets inaccessibility I am adding that meat into the supply 
          #### This is because there is more meat left in the country, i.e., more supply.
          if(i < 4){
            
            slExprtsMeat <- (expRatioBeefMax * slNewFMD ) %>% as.numeric()
            
            slNewFMD <- slNewFMD + slExprtsMeat
            clNewFMD <- clNewFMD
            # slNewFMD <- slNewFMD + slNewFMD * (exports_percentMeat/100)
            # clNewFMD <- clNewFMD + clNewFMD * (exports_percentMeat/100)
            
            # capAFMD_Dollars <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
            
            exprtsFMD <- -(mergedForecastFMD_Proj %>% filter(Year == 2020) %>% select(Exports) %>% as.numeric())
            
            mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- if_else(exprtsFMD>0,exprtsFMD,0)
            
          }else if(i >= 4 && i <= 5){
            
            slExprtsMeat <- (expRatioBeefMax * slNewFMD ) %>% as.numeric()
            slNewFMD <- slNewFMD + slExprtsMeat
            clNewFMD <- clNewFMD
            # slNewFMD <- slNewFMD + slNewFMD * (exports_percentMeat/100)
            # clNewFMD <- clNewFMD + clNewFMD * (exports_percentMeat/100)
            exprtsFMD <- -(mergedForecastFMD_Proj %>% filter(Year == 2020) %>% select(Exports) %>% as.numeric())
            mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- if_else(exprtsFMD>0,exprtsFMD,0)
            
          }else{
            
            # slExprtsLive <- slNewFMD * (exports_LiveK/100)
            # clExprtsLive <- clNewFMD * (exports_LiveK/100)
            slExprtsLive <- expRatioMed * mergedForecastFMD_Proj$K[mergedForecastFMD_Proj$Year == yearIFMD-1]
            exports_Live <- slExprtsLive
            
            # slExprtsMeat <- slNewFMD * (exports_percentMeat/100)
            # clExprtsMeat <- clNewFMD * (exports_percentMeat/100)
            totExprtsMeat <- (expRatioBeefMean * (slNewFMD + clNewFMD)) %>% as.numeric()
            slExprtsMeat <- (expRatioBeefMin * slNewFMD ) %>% as.numeric()
            
            slNewFMD <- slNewFMD - slExprtsMeat
            # clNewFMD <- clNewFMD - clExprtsMeat
            clNewFMD <- clNewFMD
          }
          
          
          ####### We write conditions for importation of meat
          #### Simply put, we import meat if meat in the domestic markets is less than historical minimum meat
          impBeef[i] <- 0
          
          if(i>1){
            
            proj_Q_P_PostFMD$SlDem[i] <- slDem
            proj_Q_P_PostFMD$ClDem[i] <- clDem
            
          }
          
          if(slNewFMD < slHistMin || clNewFMD < clHistMin){
            
            # slNewFMD_OG_New <- slNewFMD
            # clNewFMD_OG_New <- clNewFMD
            
            if(slNewFMD < slHistMin){
              slBeefImports <- (impRatioBeefMax * (slNewFMD + clNewFMD)) %>% as.numeric()
              # slNewFMD <- slNewFMD + 0.01
            }else{
              slBeefImports <- 0
            }
            
            if(clNewFMD < clHistMin){
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
          
          if(i>5){
            
            #### Here I convert the live animal exports from meat in pounds to live animals
            # slExprtsHead <- slExprtsLive * (1000000000/slaughterAvgFMD)
            # clExprtsHead <- clExprtsLive * (1000000000/cullAvgFMD)
            slExprtsHead <- slExprtsLive
            
            # expTotOG <- slExprtsHead + clExprtsHead
            expTotOG <- slExprtsHead
            expTot <- expTotOG
            
            ### Here I populate the dataframe with the live animals exported
            mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- round(expTot)
            
            ### Here I populate the dataframe with the beef exported
            mergedForecastFMD_Proj$ExportsBeef[mergedForecastFMD_Proj$Year == yearIFMD] <- round(totExprtsMeat,3)
            
            # slExprtsHead <- slExprtsHead * (expTot/expTotOG)
            # clExprtsHead <- clExprtsHead * (expTot/expTotOG)
            
            ### Here I populate the dataframe with the live animals exported
            mergedForecastFMD_Proj$Exports[mergedForecastFMD_Proj$Year == yearIFMD] <- round(expTot)
            
            ### Here I populate the dataframe with the beef exported
            mergedForecastFMD_Proj$ExportsBeef[mergedForecastFMD_Proj$Year == yearIFMD] <- round(totExprtsMeat,3)
            
            #### Now I determine the total live animals that are exported. This include the live animals and beef exported
            #### So I convert the beef exported from pounds to live animals and add them to the live animals determined before
            #### Note: We are determining these because we must remove these from the existing stocks. Why? Because these are 
            #### exports
            slExprtsHead <- slExprtsHead + slExprtsMeat * (1000000000/slaughterAvgFMD)
            
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
          
          ### Here we use the depopulated numbers for two iterations. 
          ### From then we use actuals because those will be realized after the outbreak
          if(i<3){
            KTwo <- KTwo1
            ccY1 <- gFMDCC * (KTwo - clTwoNew)
          }else{
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
          
          ##### Here I add the feeder cattle to the calf crop which are supposed to be slaughtered
          ##### This is done if the U.S. imports cattle
          slaughterImp <- 0
          if(i>1){
            if(impUpdatek3[i-1]>0){
              slaughterImp <- mergedForecastFMD_Proj$Imports[mergedForecastFMD_Proj$Year == yearIFMD-1] * delta
            }
          }
          
          if(i<=5){
            k_old_headFMD <- KOneNew + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
            # k_old_headFMD <- KOneM + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
          }else{
            k_old_headFMD <- KOneNew + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
            # k_old_headFMD <- KOneM + ((delta^2) * 0.5 * (ccY1)) + slaughterImp - slDemHeadFMD
          }
          
          k_old_headFMD_OG1 <- k_old_headFMD
          
          #### We add the live animals that are supposed to be exported to the replacement heifers
          if(i<=5){
            k_old_headFMD <- (k_old_headFMD + exports_Live) %>% as.numeric()
          }else{
            k_old_headFMD <- (k_old_headFMD - exports_Live) %>% as.numeric()
            # if(expUpdatek3[i-1]==1 && expUpdatek3[i-2]==1){
            #   k_old_headFMD <- (k_old_headFMD - slExprtsHead) %>% as.numeric()
            # }
          }
          
          # #### Now the sum of KOneNew and half of calfCrop are up for decision. 
          # #### i.e., sum of those are the mature animals up for decision
          # ############################
          # 
          # k_old_headFMD <- KOneNew + (0.5 * (ccY1)) - slDemHeadFMD
          # k_old_headFMD <- KOneNew + ((delta^2) * 0.5 * (ccY1)) - slDemHeadFMD
          # 
          # k_old_headFMD_OG1 <- k_old_headFMD
          
          #### We add the live animals that are supposed to be exported to the replacement heifers
          # if(i<=5){
          #   k_old_headFMD <- k_old_headFMD + exports_Live
          # }
          
          ##### THINK ABOUT EXPORTING IF THE REPLACEMENT HEIFERS THAT ARE HIGHER THAN THE HISTORICAL MAXIMUM
          #### Here after the export ban is lifted I check whether the replacement heifers are greater than the historical 
          #### maximum. If yes, then remove the animals and add them into the exports.
          expUpdatek3[i] <- 0
          slUpdate <- 0
          clUpdate <- 0
          impUpdatek3[i] <- 0
          
          # if(i>=3 && i <=5){
          if(k_old_headFMD < k3HistMed){
              impUpdatek3[i] <- 1
          }
          
          mergedForecastFMD_Proj$k3[mergedForecastFMD_Proj$Year == yearIFMD] <- k_old_headFMD
          
          if(dePopR == 20){
            if(i < 6){
              approxKFMD <- KOneNew + k_old_headFMD 
            }else if(i >= 6){
              approxKFMD <- KOneNew + k_old_headFMD 
            }
          }else if(dePopR == 10){
            if(i < 6){
              approxKFMD <- KOneNew + k_old_headFMD 
            }else if(i >= 6){
              approxKFMD <- KOneNew + k_old_headFMD 
            }
          }else{
            if(i < 6){
              approxKFMD <- KOneNew + k_old_headFMD 
            }else if(i >= 6){
              approxKFMD <- KOneNew + k_old_headFMD
            }
            
          }
          
          #### Here after the export ban is lifted I check whether the replacement heifers are greater than the historical 
          #### maximum. If yes, then remove the animals and add them into the exports.
          expUpdateK[i] <- 0
          impUpdateK[i] <- 0
          
          #### Here I will get how many animals are imported from the condition 
          #### replacement heifers are approximately 25.54% of the calf crop (historical)
          #### Since we let the imports flow even during thr disease outbreak, I must check whether we need imports 
          #### during that time. 
          k3_headFMD_OG <- k_old_headFMD
          
          if(impUpdatek3[i]==1){
            imprtsFMDK <- (impRatioMax * approxKFMD) %>% as.numeric()
            mergedForecastFMD_Proj$Imports[mergedForecastFMD_Proj$Year == yearIFMD] <- imprtsFMDK
          }
          
          if(i >= 6){
            
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
              if(approxKFMD > KHistMin){
                exprtsFMDK <- (expRatioMax * k_old_headFMD) %>% as.numeric()
              }else{
                exprtsFMDK <- 0
              }
              
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
            }else if( k_old_headFMD < k3HistMed){
              gFMDCC <- gFMDCC + 0.025
            }
          }else if(dePopR==10){
            if(approxKFMD > KHistMed || k_old_headFMD > k3HistMed){
              gFMDCC <- gFMDCC - 0.026
            }
            # else if( k_old_headFMD < k3HistMed){
            #   gFMDCC <- gFMDCC + 0.02
            # }
          } 
          
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
            if(i < 4){
              clNew <- clNew1FMD
            }else if(i >= 4 && i <= 5){
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
          
          if(i>1){
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
          }
          
          if(i < 4){
            
            # capAFMD_DollarsAfter <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre
            
            #### Here I compute the demand in dollars. Basically multiply the supplied quantities with the 
            #### price determined from the equilibrium conditions. Note: I don't have to use the markup parameter 
            #### to convert the price to retail price. It's constant and it's 
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
            
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
              
              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
              
            }
            
          }
          
          ##############################################################################################
          ###################### INCREASE THE DEMAND HERE. ADJUST FOR QUANTITIES AS WELL ###############
          
          if(i==4){
            
            while(capAFMD_DollarsAfter < capAFMD_DollarsOG){
              
              if(dePopR == 5){
                
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
                
              }else if(dePopR == 10){
                
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
                
              }else{
                psM_pre <- psM_pre + 0.01
                pcM_pre <- pcM_pre + 0.01
              }
              
              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
              
            }
          }
          
          PsFMD <- getPsPcEpsEpc_FMD_EQ_PES(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
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
          if(i < 4){

            # capAFMD_DollarsAfter <- capAFMD * sh_pre * psM_pre + capAFMD * (1-sh_pre) * pcM_pre

            #### Here I compute the demand in dollars. Basically multiply the supplied quantities with the
            #### price determined from the equilibrium conditions. Note: I don't have to use the markup parameter
            #### to convert the price to retail price. It's constant and it's
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)

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

              capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)

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
          
          if(i>=5){
            capAFMD_DollarsAfter <- slNewFMD * (psM_pre/phi) + clNewFMD * (pcM_pre/phi)
          }
          
          proj_Q_P_PostFMD$demDollarsAfter[i] <- capAFMD_DollarsAfter
          
          if(i>1){
            if(proj_Q_P_PostFMD$Ps[i]<proj_Q_P_PostFMD$Ps[i-1]){
              slDem <- slNewFMD * shockD_pre
            }else{
              slDem <- slNewFMD * adjF_pre
            }
            if(proj_Q_P_PostFMD$Pc[i]<proj_Q_P_PostFMD$Pc[i-1]){
              clDem <- clNewFMD * shockD_pre
            }else{
              clDem <- clNewFMD * adjF_pre
            }
            capAFMD <- slDem + clDem
          }else{
            if(proj_Q_P_PostFMD$Ps[i] < psMOG){
              slDem <- slNewFMD * shockD_pre
            }else{
              slDem <- slNewFMD * adjF_pre
            }
            if(proj_Q_P_PostFMD$Pc[i] < pcMOG){
              clDem <- clNewFMD * shockD_pre
            }else{
              clDem <- clNewFMD * adjF_pre
            }
            capAFMD <- slDem + clDem
          }
          
          # capAFMD <- (slNewFMD_OG + clNewFMD_OG) * shockD_pre
          # capAFMD <- (slNewFMD + clNewFMD) * shockD_pre
          
        }
        
        return(list(proj_Q_P_PostFMD, beefINV_FORECAST_PostFMD, mergedForecastFMD_Proj))
  
}


pessimisticPostFMD_5_0902 <- simPessimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

pessimisticPostFMD_5_0904 <- simPessimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

pessimisticPostFMD_5_0909 <- simPessimisticFMD(
  dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD5)

pessimisticPostFMD_5_0909[[1]]$Hc

pessimisticPostFMD_10_0902 <- simPessimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)

pessimisticPostFMD_10_0904 <- simPessimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)


pessimisticPostFMD_10_0909 <- simPessimisticFMD(
  dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
  exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
  holdingCostsFuturesFMD = holdingCostsFuturesFMD10)

pessimisticPostFMD_10_0909[[2]]


# pessimisticPostFMD_5 <- simPessimisticFMD(dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                            exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# pessimisticPostFMD_10 <- simPessimisticFMD(dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                            exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# pessimisticPostFMD_20 <- simPessimisticFMD(dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                            exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# pessimisticPostFMD_0 <- simPessimisticFMD(dePopR = 0, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD,
#                                         exports_percentK = exports_percentK, nn = 11, Stock = Stock)
# 
# 
# 
# postFMD_P_Q_5_Pes <- pessimisticPostFMD_5[[1]]
# postFMD_INV_5_Pes <- pessimisticPostFMD_5[[2]]
# postFMD_MER_INV_5_Pes <- pessimisticPostFMD_5[[3]]
# 
# postFMD_P_Q_10_Pes <- pessimisticPostFMD_10[[1]]
# postFMD_INV_10_Pes <- pessimisticPostFMD_10[[2]]
# postFMD_MER_INV_10_Pes <- pessimisticPostFMD_10[[3]]
# 
# postFMD_P_Q_20_Pes <- pessimisticPostFMD_20[[1]]
# postFMD_INV_20_Pes <- pessimisticPostFMD_20[[2]]
# postFMD_MER_INV_20_Pes <- pessimisticPostFMD_20[[3]]
# 
# postFMD_P_Q_0_Pes <- pessimisticPostFMD_0[[1]]
# postFMD_INV_0_Pes <- pessimisticPostFMD_0[[2]]
# postFMD_MER_INV_0_Pes <- pessimisticPostFMD_0[[3]]


################### SIMLUATIONS AFTER CHANGING CODE AND OTHER THINGS ###############

# pessimisticPostFMD_5_08 <- simPessimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)
# 
# pessimisticPostFMD_10_08 <- simPessimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)
# 
# pessimisticPostFMD_20_08 <- simPessimisticFMD(
#   dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD20)

# pessimisticPostFMD_5_0822 <- simPessimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)
# 
# pessimisticPostFMD_10_0822 <- simPessimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)
# 
# pessimisticPostFMD_20_0822 <- simPessimisticFMD(
#   dePopR = 20, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD20)


# pessimisticPostFMD_5_0831 <- simPessimisticFMD(
#   dePopR = 5, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD5)
# 
# pessimisticPostFMD_10_0831 <- simPessimisticFMD(
#   dePopR = 10, modelParamsEQ_PreFMD = modelParamsEQ_PreFMD, exports_percentK = exports_percentK, 
#   exports_LiveK = exports_LiveK, exports_percentMeat = exports_percentMeat, nn = nn, Stock = Stock,
#   holdingCostsFuturesFMD = holdingCostsFuturesFMD10)





###################################################################################################################################################
###################################################################### PLOTS ######################################################################
###################################################################################################################################################

####### Optimistic Scenario
proj_Q_P_PostFMD_OPT_5 <- postFMD_P_Q_5_Opt
proj_Q_P_PostFMD_OPT_10 <- postFMD_P_Q_10_Opt
proj_Q_P_PostFMD_OPT_20 <- postFMD_P_Q_20_Opt

beefINV_FORECAST_PostFMD_OPT_5 <- postFMD_INV_5_Opt
beefINV_FORECAST_PostFMD_OPT_10 <- postFMD_INV_10_Opt
beefINV_FORECAST_PostFMD_OPT_20 <- postFMD_INV_20_Opt


### Prices, Supplies and Quantity demanded
proj_Q_P_PostFMD_OPT_5I <- proj_Q_P_PostFMD_OPT_5 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl,Cl5 = Cl,
                                                                Hc5 = Hc, Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                        mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_OPT_10I <- proj_Q_P_PostFMD_OPT_10 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                  Hc10 = Hc, Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                          mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter) 

proj_Q_P_PostFMD_OPT_20I <- proj_Q_P_PostFMD_OPT_20 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
                                                                  Hc20 = Hc, Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
                                                                          mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)

##### Merging all data frames 
proj_Q_P_PostFMD_OPTI <- Reduce(function(...) merge(...), 
                                    list(proj_Q_P_PostFMD_OPT_5I, proj_Q_P_PostFMD_OPT_10I, proj_Q_P_PostFMD_OPT_20I))

##### Stocks
beefINV_FORECAST_PostFMD_OPT_5I <- beefINV_FORECAST_PostFMD_OPT_5 %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_OPT_10I <- beefINV_FORECAST_PostFMD_OPT_10 %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_OPT_20I <- beefINV_FORECAST_PostFMD_OPT_20 %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_OPTI <- Reduce(function(...) merge(...), 
                                            list(beefINV_FORECAST_PostFMD_OPT_5I, beefINV_FORECAST_PostFMD_OPT_10I, 
                                                 beefINV_FORECAST_PostFMD_OPT_20I))


####### Pessimistic Scenario

proj_Q_P_PostFMD_PES_5 <- postFMD_P_Q_5_Pes
proj_Q_P_PostFMD_PES_10 <- postFMD_P_Q_10_Pes
proj_Q_P_PostFMD_PES_20 <- postFMD_P_Q_20_Pes

beefINV_FORECAST_PostFMD_PES_5 <- postFMD_INV_5_Pes
beefINV_FORECAST_PostFMD_PES_10 <- postFMD_INV_10_Pes
beefINV_FORECAST_PostFMD_PES_20 <- postFMD_INV_20_Pes

### Prices, Supplies and Quantity demanded
proj_Q_P_PostFMD_PES_5I <- proj_Q_P_PostFMD_PES_5 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl, Cl5 = Cl,
                                                                        Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                        mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_PES_10I <- proj_Q_P_PostFMD_PES_10 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                          Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                          mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter) 

proj_Q_P_PostFMD_PES_20I <- proj_Q_P_PostFMD_PES_20 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
                                                                          Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
                                                                          mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)

##### Merging all data frames 
proj_Q_P_PostFMD_PESI <- Reduce(function(...) merge(...), 
                                    list(proj_Q_P_PostFMD_PES_5I, proj_Q_P_PostFMD_PES_10I, proj_Q_P_PostFMD_PES_20I))

##### Stocks
beefINV_FORECAST_PostFMD_PES_5I <- beefINV_FORECAST_PostFMD_PES_5 %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_PES_10I <- beefINV_FORECAST_PostFMD_PES_10 %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_PES_20I <- beefINV_FORECAST_PostFMD_PES_20 %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_PESI <- Reduce(function(...) merge(...), 
                                            list(beefINV_FORECAST_PostFMD_PES_5I, beefINV_FORECAST_PostFMD_PES_10I, 
                                                 beefINV_FORECAST_PostFMD_PES_20I))

#####################################################################################################################################
######################################################### OPTIMISTIC PLOTS ##########################################################
#####################################################################################################################################

proj_Q_P_PostFMD_OPTI_PS_PC <- proj_Q_P_PostFMD_OPTI %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20, Hc5, Hc10, Hc20)

proj_Q_P_PostFMD_OPTI_PS_PC[,-1] <- proj_Q_P_PostFMD_OPTI_PS_PC[,-1] * 100

# EQ_PricesCosts_OPT <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian * 100, PcB = pcMedian * 100)

EQ_PricesCosts_OPT <- proj_Q_P %>% transmute(Year = Year, PsB = Ps * 100, PcB = Pc * 100, HcB = Hc * 100) %>% filter(PsB>0)

proj_Q_P_PostFMD_OPTI_PS_PC_B <- merge(proj_Q_P_PostFMD_OPTI_PS_PC, EQ_PricesCosts_OPT)

proj_Q_P_PostFMD_OPTI_PS_PC_B <- merge(proj_Q_P_PostFMD_OPTI_PS_PC, EQ_PricesCosts_OPT)


proj_Q_P_PostFMD_OPTI_PS_B <- proj_Q_P_PostFMD_OPTI_PS_PC_B %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_OPTI_PC_B <- proj_Q_P_PostFMD_OPTI_PS_PC_B %>% select(Year, PcB, Pc5, Pc10, Pc20)

proj_Q_P_PostFMD_OPTI_HC_B <- proj_Q_P_PostFMD_OPTI_PS_PC_B %>% select(Year, HcB, Hc5, Hc10, Hc20)

#### The following has the percent change
proj_Q_P_PostFMD_OPTI_PS_B_PercentChange <- proj_Q_P_PostFMD_OPTI_PS_B %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
            Ps20Percent =(((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_PC_B_PercentChange <- proj_Q_P_PostFMD_OPTI_PC_B %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
            Pc20Percent =(((Pc20-PcB)/PcB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_HC_B_PercentChange <- proj_Q_P_PostFMD_OPTI_HC_B %>%
  transmute(Year, Hc5Percent = (((Hc5-HcB)/HcB) * 100), Hc10Percent = (((Hc10-HcB)/HcB) * 100),
            Hc20Percent =(((Hc20-HcB)/HcB) * 100)) %>% round(3)

PostFMD_OPTI_HC_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_HC_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Hc5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Hc5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Hc10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Hc10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Hc20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Hc20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_HC_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_HC_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_HC_B_PercentChange)])))+
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  geom_hline(yintercept=0, linetype="dashed", size=0.25)


PostFMD_OPTI_PS_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B)])))+ 
  scale_y_continuous(name="Fed cattle price ($/CWT)")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

# PostFMD_OPTI_PS_PlotPPT <- round(proj_Q_P_PostFMD_OPTI_PS_B,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = PsB, color="Baseline"),size=2) +
#   geom_point(aes(y = PsB, color = "Baseline"),size=2.5) +
#   geom_line(aes(y = Ps10, color="10% Depop"),size=2) +
#   geom_point(aes(y = Ps10, color="10% Depop"),size=2.5) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_PS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B)])))+
#    scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,140,by=5) ) +
#   theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 25,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=25, face = "bold"), 
#         axis.text.y = element_text(size=25, face = "bold"), 
#         axis.title = element_text(size= 25, face = "bold")) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('10% Depop', 'Baseline'),
#                      values=c('10% Depop'='#00BA38', 'Baseline'='#F8766D'))+theme(axis.ticks.length=unit(.35, "cm"))
# 
# 
# PostFMD_OPTI_Sl_OG_PlotPPT <- round(proj_Q_P_PostFMD_OPTI_Sl_B,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = SlB, color="Baseline"),size=2) +
#   geom_point(aes(y = SlB, color = "Baseline"),size=2.5) +
#   geom_line(aes(y = Sl10_OG, color="10% Depop"),size=2) +
#   geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2.5)+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
#   scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(20,28,by=1))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 25,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 25, face = "bold"), 
#         axis.text.y = element_text(size = 25, face = "bold"), 
#         axis.title=element_text(size = 25, face = "bold")) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('10% Depop', 'Baseline'),
#                      values=c('10% Depop'='#F8766D', 'Baseline' = '#C77CFF')) + theme(axis.ticks.length=unit(.35, "cm"))
# 
# 
# PostFMD_PESI_PS_PlotPPT <- round(proj_Q_P_PostFMD_PESI_PS_B,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = PsB, color="Baseline"),size=2) +
#   geom_point(aes(y = PsB, color = "Baseline"),size=2.5) +
#   geom_line(aes(y = Ps10, color="10% Depop"),size=2) +
#   geom_point(aes(y = Ps10, color="10% Depop"),size=2.5) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_PS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B)])))+
#   scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,140,by=5) ) +
#   theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 25,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=25, face = "bold"), 
#         axis.text.y = element_text(size=25, face = "bold"), 
#         axis.title = element_text(size= 25, face = "bold")) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('10% Depop', 'Baseline'),
#                      values=c('10% Depop'='#00BA38', 'Baseline'='#F8766D'))+theme(axis.ticks.length=unit(.35, "cm"))
# 
# 
# PostFMD_OPTI_Sl_OG_PlotPPT <- round(proj_Q_P_PostFMD_OPTI_Sl_B,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = SlB, color="Baseline"),size=2) +
#   geom_point(aes(y = SlB, color = "Baseline"),size=2.5) +
#   geom_line(aes(y = Sl10_OG, color="10% Depop"),size=2) +
#   geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2.5)+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
#   scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(20,28,by=1))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 25,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 25, face = "bold"), 
#         axis.text.y = element_text(size = 25, face = "bold"), 
#         axis.title=element_text(size = 25, face = "bold")) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('10% Depop', 'Baseline'),
#                      values=c('10% Depop'='#F8766D', 'Baseline' = '#C77CFF')) + theme(axis.ticks.length=unit(.35, "cm"))


PostFMD_OPTI_PS_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_PS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Ps5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_PercentChange)])))+
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  geom_hline(yintercept=c(-20, -10, 0), linetype="dashed", size=0.25) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


PostFMD_OPTI_PC_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_PC_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PC_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PC_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PC_B)])))+ 
  scale_y_continuous(name="Change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))+
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))


PostFMD_OPTI_PC_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_PC_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Pc5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PC_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PC_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_PC_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


####### Now I plot the stocks as well #####

# stocks_Baseline <- Stock %>% filter(Year >= 2010) %>% transmute(Year, KB = K)

stocks_Baseline <- mergedForecast_Proj %>% filter(Year >= 2020) %>% transmute(Year, KB = K)

stocks_OPT_Baseline <- merge(beefINV_FORECAST_PostFMD_OPTI, stocks_Baseline)

stocks_OPT_B_PercentChange <- stocks_OPT_Baseline %>% 
  transmute(Year, K5Percent = (((K5-KB)/KB) * 100), K10Percent = (((K10-KB)/KB) * 100),
            K20Percent = (((K20-KB)/KB) * 100)) %>% round(3)

stocks_OPT_Baseline[,-1] <- stocks_OPT_Baseline[,-1]/1000000

PostFMD_stocks_OPT_ChangePlot <- round(stocks_OPT_Baseline,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = KB, color="Baseline"),size=1.1) +
  geom_point(aes(y = KB, color = "Baseline"),size=2) +
  geom_line(aes(y = K5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = K5, color = "5% Depop"),size=2) +
  geom_line(aes(y = K10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = K10, color="10% Depop"),size=2) +
  geom_line(aes(y = K20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = K20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_OPT_Baseline$Year[1],
                                  stocks_OPT_Baseline$Year[nrow(stocks_OPT_Baseline)])))+ 
  scale_y_continuous(name="Change in the stocks")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

PostFMD_stocks_OPT_PercentChangePlot <- stocks_OPT_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = K5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = K5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = K10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = K10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = K20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = K20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_OPT_B_PercentChange$Year[1],
                                  stocks_OPT_B_PercentChange$Year[nrow(stocks_OPT_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  geom_hline(yintercept=0, linetype="dashed", size=0.25) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


######## Supplies

proj_Q_P_PostFMD_OPTI_Sl_Cl <- proj_Q_P_PostFMD_OPTI %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20,
                                                                        Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)



# EQ_Supplies_OPT <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

EQ_Supplies_OPT <- proj_Q_PIIV  %>% transmute(Year = Year, SlB = Sl, ClB = Cl)

proj_Q_P_PostFMD_OPTI_Sl_Cl_B <- merge(proj_Q_P_PostFMD_OPTI_Sl_Cl, EQ_Supplies_OPT)

proj_Q_P_PostFMD_OPTI_Sl_B <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B %>% select(Year, SlB, Sl5, Sl10, Sl20,
                                                                               Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_OPTI_Cl_B <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B %>% select(Year, ClB, Cl5, Cl10, Cl20,
                                                                               Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_OPTI_TS_B <- merge(proj_Q_P_PostFMD_OPTI_Sl_B, proj_Q_P_PostFMD_OPTI_Cl_B) %>%
  transmute(Year = Year, TSB = SlB + ClB, TS5_OG = Sl5_OG + Cl5_OG, TS10_OG = Sl10_OG + Cl10_OG,
            TS20_OG = Sl20_OG + Cl20_OG)


#### The following has the percent change of supplies
proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange <- proj_Q_P_PostFMD_OPTI_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5-SlB)/SlB) * 100), Sl10Percent = (((Sl10-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange <- proj_Q_P_PostFMD_OPTI_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5-ClB)/ClB) * 100), Cl10Percent = (((Cl10-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20-ClB)/ClB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_Sl_OG_B_PercentChange <- proj_Q_P_PostFMD_OPTI_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5_OG-SlB)/SlB) * 100), Sl10Percent = (((Sl10_OG-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20_OG-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_Cl_OG_B_PercentChange <- proj_Q_P_PostFMD_OPTI_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5_OG-ClB)/ClB) * 100), Cl10Percent = (((Cl10_OG-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20_OG-ClB)/ClB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_TS_B_PercentChange <- proj_Q_P_PostFMD_OPTI_TS_B %>%
  transmute(Year, TS5Percent = (((TS5_OG-TSB)/TSB) * 100), TS10Percent = (((TS10_OG-TSB)/TSB) * 100),
            TS20Percent =(((TS20_OG-TSB)/TSB) * 100)) %>% round(3)



PostFMD_OPTI_Sl_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5, color="5% Depop")) +
  geom_point(aes(y = Sl5, color = "5% Depop")) +
  geom_line(aes(y = Sl10, color="10% Depop")) +
  geom_point(aes(y = Sl10, color="10% Depop")) +
  geom_line(aes(y = Sl20, color="20% Depop")) +
  geom_point(aes(y = Sl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPTI_Sl_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))

PostFMD_OPTI_TS_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_TS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TSB, color="Baseline")) +
  geom_point(aes(y = TSB, color = "Baseline")) +
  geom_line(aes(y = TS5_OG, color="5% Depop")) +
  geom_point(aes(y = TS5_OG, color = "5% Depop")) +
  geom_line(aes(y = TS10_OG, color="10% Depop")) +
  geom_point(aes(y = TS10_OG, color="10% Depop")) +
  geom_line(aes(y = TS20_OG, color="20% Depop")) +
  geom_point(aes(y = TS20_OG, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_TS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B)])))+ 
  scale_y_continuous(name="Change in the total supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38', 
                              'Baseline' = '#C77CFF'))

PostFMD_OPTI_TS_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_TS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TS5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TS5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = TS10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TS10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = TS20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = TS20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_TS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the total supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


PostFMD_OPTI_Cl_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5, color="5% Depop")) +
  geom_point(aes(y = Cl5, color = "5% Depop")) +
  geom_line(aes(y = Cl10, color="10% Depop")) +
  geom_point(aes(y = Cl10, color="10% Depop")) +
  geom_line(aes(y = Cl20, color="20% Depop")) +
  geom_point(aes(y = Cl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPTI_Cl_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


PostFMD_OPTI_Sl_OG_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))




PostFMD_OPTI_Sl_OG_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_Sl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38'))


PostFMD_OPTI_Cl_OG_ChangePlot <- round(proj_Q_P_PostFMD_OPTI_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"),size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"),size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

PostFMD_OPTI_Cl_OG_PercentChangePlot <- proj_Q_P_PostFMD_OPTI_Cl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")



###### MuTildes and Shares
proj_Q_P_PostFMD_OPT_MU <- proj_Q_P_PostFMD_OPTI %>% select(Year, mu5, mu10, mu20)
Eq_OPT_Mu <- mu_Tildes_MMNII %>% transmute(Year = Year, muB = muMedian)

proj_Q_P_PostFMD_OPT_MU_B <- merge(proj_Q_P_PostFMD_OPT_MU, Eq_OPT_Mu)

proj_Q_P_PostFMD_OPT_MU_PercentChange <- proj_Q_P_PostFMD_OPT_MU_B %>%
  transmute(Year, mu5Percent = (((mu5-muB)/muB) * 100), mu10Percent = (((mu10-muB)/muB) * 100),
            mu20Percent =(((mu20-muB)/muB) * 100)) %>% round(3)


proj_Q_P_PostFMD_OPT_SHR <- proj_Q_P_PostFMD_OPTI %>% select(Year, sh5, sh10, sh20)
Eq_OPT_Sh <- sharesEq_Median %>% transmute(Year = Year, shB = shareMedian)

Eq_OPT_Sh <- proj_Q_P %>% transmute(Year = Year, shB = sh)

proj_Q_P_PostFMD_OPT_SHR_B <- merge(proj_Q_P_PostFMD_OPT_SHR, Eq_OPT_Sh) %>% filter(shB>0)

proj_Q_P_PostFMD_OPT_SHR_PercentChange <- proj_Q_P_PostFMD_OPT_SHR_B %>%
  transmute(Year, sh5Percent = (((sh5-shB)/shB) * 100), sh10Percent = (((sh10-shB)/shB) * 100),
            sh20Percent =(((sh20-shB)/shB) * 100)) %>% round(3)


PostFMD_OPTI_MU_ChangePlot <- round(proj_Q_P_PostFMD_OPT_MU_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = muB, color="Baseline")) +
  geom_point(aes(y = muB, color = "Baseline")) +
  geom_line(aes(y = mu5, color="5% Depop")) +
  geom_point(aes(y = mu5, color = "5% Depop")) +
  geom_line(aes(y = mu10, color="10% Depop")) +
  geom_point(aes(y = mu10, color="10% Depop")) +
  geom_line(aes(y = mu20, color="20% Depop")) +
  geom_point(aes(y = mu20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_MU_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_MU_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_MU_PercentChange)])))+ 
  scale_y_continuous(name="Change in the Median willingness to pay from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPTI_MU_PercentChangePlot <- proj_Q_P_PostFMD_OPT_MU_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = mu5Percent, color="5% Depop")) +
  geom_point(aes(y = mu5Percent, color = "5% Depop")) +
  geom_line(aes(y = mu10Percent, color="10% Depop")) +
  geom_point(aes(y = mu10Percent, color="10% Depop")) +
  geom_line(aes(y = mu20Percent, color="20% Depop")) +
  geom_point(aes(y = mu20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_MU_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_MU_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_MU_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the Median willingness to pay from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_OPTI_SHR_ChangePlot <- round(proj_Q_P_PostFMD_OPT_SHR_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = shB, color="Baseline")) +
  geom_point(aes(y = shB, color = "Baseline")) +
  geom_line(aes(y = sh5, color="5% Depop")) +
  geom_point(aes(y = sh5, color = "5% Depop")) +
  geom_line(aes(y = sh10, color="10% Depop")) +
  geom_point(aes(y = sh10, color="10% Depop")) +
  geom_line(aes(y = sh20, color="20% Depop")) +
  geom_point(aes(y = sh20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_SHR_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_SHR_B$Year[nrow(proj_Q_P_PostFMD_OPT_SHR_B)])))+ 
  scale_y_continuous(name="Change in the share from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

PostFMD_OPTI_SHR_PercentChangePlot <- proj_Q_P_PostFMD_OPT_SHR_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5Percent, color="5% Depop")) +
  geom_point(aes(y = sh5Percent, color = "5% Depop")) +
  geom_line(aes(y = sh10Percent, color="10% Depop")) +
  geom_point(aes(y = sh10Percent, color="10% Depop")) +
  geom_line(aes(y = sh20Percent, color="20% Depop")) +
  geom_point(aes(y = sh20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_SHR_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_SHR_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_SHR_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the share from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


#####################################################################################################################################
######################################################### PESSIMISTIC PLOTS ##########################################################
#####################################################################################################################################

proj_Q_P_PostFMD_PESI_PS_PC <- proj_Q_P_PostFMD_PESI %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20)

proj_Q_P_PostFMD_PESI_PS_PC[,-1] <- proj_Q_P_PostFMD_PESI_PS_PC[,-1] * 100

# EQ_PricesCosts_PES <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian * 100, PcB = pcMedian * 100)

EQ_PricesCosts_PES <- proj_Q_P %>% transmute(Year = Year, PsB = Ps * 100, PcB = Pc * 100, HcB = Hc * 100) %>% filter(PsB>0)

proj_Q_P_PostFMD_PESI_PS_PC_B <- merge(proj_Q_P_PostFMD_PESI_PS_PC, EQ_PricesCosts_PES)

proj_Q_P_PostFMD_PESI_PS_B <- proj_Q_P_PostFMD_PESI_PS_PC_B %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_PESI_PC_B <- proj_Q_P_PostFMD_PESI_PS_PC_B %>% select(Year, PcB, Pc5, Pc10, Pc20)

#### The following has the percent change
proj_Q_P_PostFMD_PESI_PS_B_PercentChange <- proj_Q_P_PostFMD_PESI_PS_B %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
            Ps20Percent =(((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_PC_B_PercentChange <- proj_Q_P_PostFMD_PESI_PC_B %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
            Pc20Percent =(((Pc20-PcB)/PcB) * 100)) %>% round(3)

PostFMD_PESI_PS_ChangePlot <- round(proj_Q_P_PostFMD_PESI_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PS_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_PS_B$Year[nrow(proj_Q_P_PostFMD_PESI_PS_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle prices")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

PostFMD_PESI_PS_PercentChangePlot <- proj_Q_P_PostFMD_PESI_PS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Ps5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_PS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_PS_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  geom_hline(yintercept=c(0,-20,-40), linetype="dashed", size=0.25)



PostFMD_PESI_PC_ChangePlot <- round(proj_Q_P_PostFMD_PESI_PC_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PC_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_PC_B$Year[nrow(proj_Q_P_PostFMD_PESI_PC_B)])))+ 
  scale_y_continuous(name="Change in the cull cow prices")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))

PostFMD_PESI_PC_PercentChangePlot <- proj_Q_P_PostFMD_PESI_PC_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Pc5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PC_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_PC_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_PC_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


####### Now I plot the stocks as well #####

# stocks_Baseline <- Stock %>% filter(Year >= 2010) %>% transmute(Year, KB = K)

stocks_Baseline <- mergedForecast_Proj %>% filter(Year >= 2020) %>% transmute(Year, KB = K)

stocks_PES_Baseline <- merge(beefINV_FORECAST_PostFMD_PESI, stocks_Baseline)

stocks_PES_B_PercentChange <- stocks_PES_Baseline %>% 
  transmute(Year, K5Percent = (((K5-KB)/KB) * 100), K10Percent = (((K10-KB)/KB) * 100),
            K20Percent = (((K20-KB)/KB) * 100)) %>% round(3)

stocks_PES_Baseline[,-1] <- stocks_PES_Baseline[,-1]/1000000

PostFMD_stocks_PES_ChangePlot <- round(stocks_PES_Baseline,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = KB, color="Baseline"),size=1.1) +
  geom_point(aes(y = KB, color = "Baseline"),size=2) +
  geom_line(aes(y = K5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = K5, color = "5% Depop"),size=2) +
  geom_line(aes(y = K10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = K10, color="10% Depop"),size=2) +
  geom_line(aes(y = K20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = K20, color="20% Depop"),size=1.1) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_PES_Baseline$Year[1],
                                  stocks_PES_Baseline$Year[nrow(stocks_PES_Baseline)])))+ 
  scale_y_continuous(name="Change in the stocks from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))



PostFMD_stocks_PES_PercentChangePlot <- stocks_PES_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = K5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = K5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = K10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = K10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = K20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = K20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_PES_B_PercentChange$Year[1],
                                  stocks_PES_B_PercentChange$Year[nrow(stocks_PES_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  geom_hline(yintercept=0, linetype="dashed", size=0.25)


######## Supplies

proj_Q_P_PostFMD_PESI_Sl_Cl <- proj_Q_P_PostFMD_PESI %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20,
                                                                        Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

# EQ_Supplies_PES <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

EQ_Supplies_PES <- proj_Q_P  %>% transmute(Year = Year, SlB = Sl, ClB = Cl) %>% filter(SlB>0)

proj_Q_P_PostFMD_PESI_Sl_Cl_B <- merge(proj_Q_P_PostFMD_PESI_Sl_Cl, EQ_Supplies_PES)

proj_Q_P_PostFMD_PESI_Sl_B <- proj_Q_P_PostFMD_PESI_Sl_Cl_B %>% select(Year, SlB, Sl5, Sl10, Sl20,
                                                                               Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_PESI_Cl_B <- proj_Q_P_PostFMD_PESI_Sl_Cl_B %>% select(Year, ClB, Cl5, Cl10, Cl20,
                                                                               Cl5_OG, Cl10_OG, Cl20_OG)

#### The following has the percent change of supplies
proj_Q_P_PostFMD_PESI_Sl_B_PercentChange <- proj_Q_P_PostFMD_PESI_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5-SlB)/SlB) * 100), Sl10Percent = (((Sl10-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_Cl_B_PercentChange <- proj_Q_P_PostFMD_PESI_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5-ClB)/ClB) * 100), Cl10Percent = (((Cl10-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20-ClB)/ClB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_Sl_OG_B_PercentChange <- proj_Q_P_PostFMD_PESI_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5_OG-SlB)/SlB) * 100), Sl10Percent = (((Sl10_OG-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20_OG-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_Cl_OG_B_PercentChange <- proj_Q_P_PostFMD_PESI_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5_OG-ClB)/ClB) * 100), Cl10Percent = (((Cl10_OG-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20_OG-ClB)/ClB) * 100)) %>% round(3)


PostFMD_PESI_Sl_ChangePlot <- round(proj_Q_P_PostFMD_PESI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5, color="5% Depop")) +
  geom_point(aes(y = Sl5, color = "5% Depop")) +
  geom_line(aes(y = Sl10, color="10% Depop")) +
  geom_point(aes(y = Sl10, color="10% Depop")) +
  geom_line(aes(y = Sl20, color="20% Depop")) +
  geom_point(aes(y = Sl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PESI_Sl_PercentChangePlot <- proj_Q_P_PostFMD_PESI_Sl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_PESI_Cl_ChangePlot <- round(proj_Q_P_PostFMD_PESI_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5, color="5% Depop")) +
  geom_point(aes(y = Cl5, color = "5% Depop")) +
  geom_line(aes(y = Cl10, color="10% Depop")) +
  geom_point(aes(y = Cl10, color="10% Depop")) +
  geom_line(aes(y = Cl20, color="20% Depop")) +
  geom_point(aes(y = Cl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PESI_Cl_PercentChangePlot <- proj_Q_P_PostFMD_PESI_Cl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_PESI_Sl_OG_ChangePlot <- round(proj_Q_P_PostFMD_PESI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))


PostFMD_PESI_Sl_OG_PercentChangePlot <- proj_Q_P_PostFMD_PESI_Sl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_OG_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_OG_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_OG_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_PESI_Cl_OG_ChangePlot <- round(proj_Q_P_PostFMD_PESI_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"), size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"), size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"), size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"), size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"), size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"), size=2) +
  geom_line(aes(y = Cl20_OG, color="20% Depop"), size=1.1) +
  geom_point(aes(y = Cl20_OG, color="20% Depop"), size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))


PostFMD_PESI_Cl_OG_PercentChangePlot <- proj_Q_P_PostFMD_PESI_Cl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_OG_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_OG_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_OG_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


###### MuTildes and Shares
proj_Q_P_PostFMD_PES_MU <- proj_Q_P_PostFMD_PESI %>% select(Year, mu5, mu10, mu20)
Eq_PES_Mu <- mu_Tildes_MMNII %>% transmute(Year = Year, muB = muMedian)

proj_Q_P_PostFMD_PES_MU_B <- merge(proj_Q_P_PostFMD_PES_MU, Eq_PES_Mu)

proj_Q_P_PostFMD_PES_MU_PercentChange <- proj_Q_P_PostFMD_PES_MU_B %>%
  transmute(Year, mu5Percent = (((mu5-muB)/muB) * 100), mu10Percent = (((mu10-muB)/muB) * 100),
            mu20Percent =(((mu20-muB)/muB) * 100)) %>% round(3)


PostFMD_PESI_MU_ChangePlot <- proj_Q_P_PostFMD_PES_MU_B %>% ggplot(aes(x = Year))+
  geom_line(aes(y = muB, color="Baseline")) +
  geom_point(aes(y = muB, color = "Baseline")) +
  geom_line(aes(y = mu5, color="5% Depop")) +
  geom_point(aes(y = mu5, color = "5% Depop")) +
  geom_line(aes(y = mu10, color="10% Depop")) +
  geom_point(aes(y = mu10, color="10% Depop")) +
  geom_line(aes(y = mu20, color="20% Depop")) +
  geom_point(aes(y = mu20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_MU_B$Year[1],
                                  proj_Q_P_PostFMD_PES_MU_B$Year[nrow(proj_Q_P_PostFMD_PES_MU_B)])))+ 
  scale_y_continuous(name="Change in the Median willingness to pay from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PESI_MU_PercentChangePlot <- proj_Q_P_PostFMD_PES_MU_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = mu5Percent, color="5% Depop")) +
  geom_point(aes(y = mu5Percent, color = "5% Depop")) +
  geom_line(aes(y = mu10Percent, color="10% Depop")) +
  geom_point(aes(y = mu10Percent, color="10% Depop")) +
  geom_line(aes(y = mu20Percent, color="20% Depop")) +
  geom_point(aes(y = mu20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_MU_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_MU_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_MU_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the Median willingness to pay from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


proj_Q_P_PostFMD_PES_SHR <- proj_Q_P_PostFMD_PESI %>% select(Year, sh5, sh10, sh20)
Eq_PES_Sh <- sharesEq_Median %>% transmute(Year = Year, shB = shareMedian)

Eq_PES_Sh <- proj_Q_P %>% transmute(Year = Year, shB = sh)

proj_Q_P_PostFMD_PES_SHR_B <- merge(proj_Q_P_PostFMD_PES_SHR, Eq_PES_Sh) %>% round(3) %>% filter(shB>0)

proj_Q_P_PostFMD_PES_SHR_PercentChange <- proj_Q_P_PostFMD_PES_SHR_B %>%
  transmute(Year, sh5Percent = (((sh5-shB)/shB) * 100), sh10Percent = (((sh10-shB)/shB) * 100),
            sh20Percent =(((sh20-shB)/shB) * 100)) %>% round(3)


PostFMD_PESI_SHR_ChangePlot <- proj_Q_P_PostFMD_PES_SHR_B %>% ggplot(aes(x = Year))+
  geom_line(aes(y = shB, color="Baseline"),size=1.1) +
  geom_point(aes(y = shB, color = "Baseline"),size=2) +
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  geom_line(aes(y = sh20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = sh20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_SHR_B$Year[1],
                                  proj_Q_P_PostFMD_PES_SHR_B$Year[nrow(proj_Q_P_PostFMD_PES_SHR_B)])))+ 
  scale_y_continuous(name="Change in the share") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', '20% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', '20% Depop'='#00BA38',
                              'Baseline' = '#C77CFF'))


PostFMD_PESI_SHR_PercentChangePlot <- proj_Q_P_PostFMD_PES_SHR_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5Percent, color="5% Depop")) +
  geom_point(aes(y = sh5Percent, color = "5% Depop")) +
  geom_line(aes(y = sh10Percent, color="10% Depop")) +
  geom_point(aes(y = sh10Percent, color="10% Depop")) +
  geom_line(aes(y = sh20Percent, color="20% Depop")) +
  geom_point(aes(y = sh20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_SHR_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_SHR_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_SHR_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the share from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")




estProj_PSIII_plots <- estProj_PSIII_FAPRI %>% filter(Year >=2018 & Year < 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="Baseline"),size=1.1) +
  geom_point(aes(y = psMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Ps, color="Model Projection"),size=1.1) +
  geom_point(aes(y=Ps, color="Model Projection"),size=2) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI Projection"),size=1.1) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI Projection"),size=2) +
  geom_line(aes(y=USDA_Ps, color="USDA Projection"),size=1.1) +
  geom_point(aes(y=USDA_Ps, color="USDA Projection"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price ")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())

CARD_USDA_FAPRI_TS_Proj_plotIII <- CARD_USDA_FAPRI_TS_ProjIII  %>% filter(Year >=2018 & Year < 2031) %>% ggplot(aes(x=Year))  + 
  geom_line(aes(y=tsMedian, color="Baseline"),size=1.1) + 
  geom_point(aes(y=tsMedian, color="Baseline"),size=2) + 
  geom_line(aes(y=TS, color="Model Projection"),size=1.1) + 
  geom_point(aes(y=TS, color="Model Projection"),size=2) + 
  geom_line(aes(y=FAPRI_TS, color="FAPRI Projection"),size=1.1)  + 
  geom_point(aes(y=FAPRI_TS, color="FAPRI Projection"),size=2) +
  geom_line(aes(y=USDA_TS, color="USDA Projection"),size=1.1)  + 
  geom_point(aes(y=USDA_TS, color="USDA Projection"),size=2)  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_ProjIII$Year[1],
                                  CARD_USDA_FAPRI_TS_ProjIII$Year[nrow(CARD_USDA_FAPRI_TS_ProjIII)])))+ 
  scale_y_continuous(name="Total Production Projections ") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())









################################################################################################################################################################################################################################################################################################################################

mergedForecastFMD_Proj_OPT_5_New <- mergedForecastFMD_Proj_OPT_5_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)

mergedForecastFMD_Proj_OPT_10_New <- mergedForecastFMD_Proj_OPT_10_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)

mergedForecastFMD_Proj_OPT_20_New <- mergedForecastFMD_Proj_OPT_20_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)

mergedForecastFMD_Proj_PES_5_New <- mergedForecastFMD_Proj_PES_5_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)

mergedForecastFMD_Proj_PES_10_New <- mergedForecastFMD_Proj_PES_10_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)

mergedForecastFMD_Proj_PES_20_New <- mergedForecastFMD_Proj_PES_20_I_I %>% 
  select(-Slaughter_avg, -Cull_avg, -Imports, -Exports)




##### Here I get the changes in the animal numbers from baseline

Stock_Baseline <- Stock %>% filter(Year > 2009 & Year <= 2020) %>% select(-k10)

OPT_5_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_OPT_5_I_I

OPT_10_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_OPT_10_I_I

OPT_20_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_OPT_20_I_I

PES_5_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_PES_5_I_I

PES_10_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_PES_10_I_I

PES_20_Baseline <- Stock_Baseline - beefINV_FORECAST_PostFMD_PES_20_I_I





library(latexpdf)
as.pdf(beefINV_FORECAST_PostFMD_OPT_5_I_I)
as.pdf(beefINV_FORECAST_PostFMD_OPT_10_I_I)
as.pdf(beefINV_FORECAST_PostFMD_OPT_20_I_I)

as.pdf(beefINV_FORECAST_PostFMD_PES_5_I_I)
as.pdf(beefINV_FORECAST_PostFMD_PES_10_I_I)
as.pdf(beefINV_FORECAST_PostFMD_PES_20_I_I)

















EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"),size=1.1) +
  geom_point(aes(y=K,color="Observed Inventory"),size=2) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"),size=1.1) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),size=2) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  scale_y_continuous(name="Million Head") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15)) +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                             axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



PostFMD_BaselineInventory <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"),size=1.1) +
  geom_point(aes(y=K,color="Observed Inventory"),size=2) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"),size=1.1) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),size=2) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  scale_y_continuous(name="Million Head") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) 


PostFMD_BaselineFedCattlePrices <- EQestObsPSNI_plots %>% ggplot(aes(x=Year))+ geom_line(aes(y=psMean, color="Mean fitted price"),size=1.1) +
  geom_point(aes(y = psMean, color = "Mean fitted price"),size=2) + geom_line(aes(y=ps, color = "Observed price"),size=1.1) + 
  geom_point(aes(y=ps, color = "Observed price"),size=2) + geom_line(aes(y=psMedian, color="Median Fitted price"),size=1.1) +
  geom_point(aes(y = psMedian, color = "Median Fitted price"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)], by = 2))) +
  scale_y_continuous(name="Fed Cattle Price")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())


PostFMD_OPTI_PS_PercentChangePlot_Bar <- proj_Q_P_PostFMD_OPTI_PS_B_PercentChange %>% transmute(Year = Year, `5` = Ps5Percent,
                                                                                                `10` = Ps10Percent, `20` = Ps20Percent)

PostFMD_OPTI_PS_aggW <- 
  gather(PostFMD_OPTI_PS_PercentChangePlot_Bar, depop, value, `5`:`20`, factor_key=TRUE)

PostFMD_OPTI_PS_aggW_BarPlot <- PostFMD_OPTI_PS_aggW %>% ggplot(aes(fill=depop, y=value, x=Year)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_x_continuous(name = "Year", breaks=c(seq(PostFMD_OPTI_PS_aggW$Year[1],
                                                 PostFMD_OPTI_PS_aggW$Year[nrow(PostFMD_OPTI_PS_aggW)]))) + 
  scale_y_continuous(name="Percent Change") +
  theme_test() + 
  theme(legend.position="bottom", legend.box = "horizontal")




