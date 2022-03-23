dePopR <- 90
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
  
  # i <- 4
  
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
    
    capA_pre <- capA_pre - capA_pre * (5/100) + capA_pre * (exports_percentK/100)
    
    
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
  
  ANew <- (slNew + clNew) * (1/adjF_pre)
  
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
  } else if(i >= 4 && i <= 5  ){ 
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
      EpsM_pre <- EpsM_pre  + 0.1
    }
    
    while(EpcM_pre < pcM_pre){
      EpcM_pre <- EpcM_pre + 0.1
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
  
  if(i>3){
    
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