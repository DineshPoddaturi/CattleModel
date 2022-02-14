
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

Stock_2009L <- Stock %>% filter(Year <= 2009)
Stock_2010 <- Stock %>% filter(Year == 2010)

#### Function returning the data with depopulated inventory
dePop <- function(stock, dePopRate){
  stock[,-1] <- stock[,-1] - stock[,-1] * (dePopRate/100)
  return(stock)
}


##### 20% depopulation
dePopR <- 20
Stock2010_20 <- dePop(stock = Stock_2010, dePopRate = dePopR)
Stock2010_20 <- rbind(Stock_2009L, Stock2010_20) %>% as.data.frame()

##### Now I have calf-crop until 2009
calf_crop_PreFMD <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year) %>% filter(Year <= 2009)
calf_crop_2010 <- calf_crop %>% filter(Year == 2010) %>% transmute(Year = Year, k0 = (1-20/100) * calfCrop)
calf_crop_PostFMD <- rbind(calf_crop_PreFMD, calf_crop_2010)

modelParamsEQ_PreFMD <- proj_AllDF_EQ %>% filter(Year == 2008)

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



nn <- 5
beefINV_FORECAST_PostFMD <-  data.frame(Year = numeric(nn), K = numeric(nn), k3 = numeric(nn), 
                                        k4 = numeric(nn), k5 = numeric(nn), k6 = numeric(nn), 
                                        k7 = numeric(nn), k8 = numeric(nn), k9 = numeric(nn),
                                        k10 = numeric(nn))

beefINV_FORECAST_PostFMD[1,] <- Stock2010_20 %>% filter(Year == 2010)


proj_Q_P_PostFMD <- data.frame(Year = numeric(nn), Ps = numeric(nn), Pc = numeric(nn), 
                               EPs = numeric(nn), EPc = numeric(nn), Hc = numeric(nn), 
                               Sl = numeric(nn), Cl = numeric(nn), A = numeric(nn),
                               repHeif = numeric(nn), repHeif_Head = numeric(nn))

k0s_PostFMD <- data.frame(Year = numeric(nn), k02 = numeric(nn), k03 = numeric(nn), 
                          k04 = numeric(nn), k05 = numeric(nn), k06 = numeric(nn), 
                          k07 = numeric(nn), k08 = numeric(nn))

k0s_PostFMD[1,] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[1,], 
                               beefINV_FORECAST = beefINV_FORECAST_PostFMD[1,], 
                               calfCrop = calf_crop_PostFMD)


for(i in 1:nrow(proj_Q_P_PostFMD)){
  
  # i <- 1
  
  if(i>1){
    
    beefINV_FORECAST_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i-1] + 1
    beefINV_FORECAST_PostFMD$k3[i] <-  proj_Q_P_PostFMD$repHeif_Head[i-1]
    beefINV_FORECAST_PostFMD$k4[i] <- delta * beefINV_FORECAST_PostFMD$k3[i-1]
    beefINV_FORECAST_PostFMD$k5[i] <- delta * beefINV_FORECAST_PostFMD$k4[i-1]
    beefINV_FORECAST_PostFMD$k6[i] <- delta * beefINV_FORECAST_PostFMD$k5[i-1]
    beefINV_FORECAST_PostFMD$k7[i] <- delta * beefINV_FORECAST_PostFMD$k6[i-1]
    beefINV_FORECAST_PostFMD$k8[i] <- delta * beefINV_FORECAST_PostFMD$k7[i-1]
    beefINV_FORECAST_PostFMD$k9[i] <- delta * beefINV_FORECAST_PostFMD$k8[i-1]
    beefINV_FORECAST_PostFMD$K[i] <- sum(beefINV_FORECAST_PostFMD[i,-1:-2])
    
    calf_crop_PostFMD <- calf_crop_PostFMD %>% add_row(Year = beefINV_FORECAST_PostFMD$Year[i], 
                                                       k0 = g * beefINV_FORECAST_PostFMD$K[i])
    
    k0s_PostFMD[i, ] <- get_k0s_Global_FMD(proj_Q_P = proj_Q_P_PostFMD[i,], 
                                            beefINV_FORECAST = beefINV_FORECAST_PostFMD[i,], 
                                            calfCrop = calf_crop_PostFMD)
    
    capA_pre <- proj_Q_P_PostFMD$A[i-1]

    capK_pre <- beefINV_FORECAST_PostFMD$K[i-1]
  }

  #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
  ### the optimal replacement heifers. 
  k <- 0
  
  K1 <- capK_pre
  
  k0s <- k0s_PostFMD[i,-1]
  
  
  int_k3 <- 0
  
  ### Here I assume a 5% decrease in domestic demand and the exports are banned (assume a 10%)
  capA_pre1 <- (1 - (5/100) + (10/100)) * capA_pre
  
  Qs <- getSlClA_test(params = c(MUtilde_pre, Stilde_pre), PsM = psM_pre, PcM = pcM_pre, K1 = K1,
                      k = k, CapA = capA_pre1, gamma_k3 = gamma_k3, 
                      eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF_pre, k0s = k0s,
                      slAvg = slaughterAvg_pre, clAvg = cullAvg_pre)
  
  slNew <- Qs[1]
  clNew <- Qs[2]
  ANew <- Qs[3]
  
  k_old <- Qs[4]
  
  k_old_Head <- Qs[5]
  
  if(i<3){
    ANew1 <- (1 - (5/100) + (10/100)) * ANew
  }
  
  if(i==3){
    ANew1 <- (1 + (10/100)) * ANew
  }
  
  if(i>3){
    
    ANew1 <-  ANew
  }
  
  # ANew <- ANew1
  
  # while(clNew < 1.01){
  #   clNew <- clNew + 0.01
  # }
  # while(slNew < 19.01){
  #   slNew <- slNew + 0.01
  # }
  
  Ps <- getPsPcEpsEpc(PsM = psM_pre, PcM = pcM_pre, EPsM = EpsM_pre, EPcM = EpcM_pre,
                      HcM = hcM_pre, SlNew = slNew, ClNew = clNew, ANew = ANew1, 
                      params = c(Stilde_pre, Stilde_pre))
  psM_pre <- Ps[1]
  pcM_pre <- Ps[2]
  hcM_pre <- Ps[3]
  EpsM_pre <- Ps[4]
  EpcM_pre <- Ps[5]
  
  proj_Q_P_PostFMD$Ps[i] <- psM_pre
  proj_Q_P_PostFMD$Pc[i] <- pcM_pre
  proj_Q_P_PostFMD$Hc[i] <- hcM_pre
  proj_Q_P_PostFMD$EPs[i] <- EpsM_pre
  proj_Q_P_PostFMD$EPc[i] <- EpcM_pre
  
  proj_Q_P_PostFMD$Sl[i] <- slNew
  proj_Q_P_PostFMD$Cl[i] <- clNew
  proj_Q_P_PostFMD$A[i] <- ANew1
  proj_Q_P_PostFMD$repHeif[i] <- k_old
  proj_Q_P_PostFMD$repHeif_Head[i] <- k_old_Head
  
  proj_Q_P_PostFMD$Year[i] <- beefINV_FORECAST_PostFMD$Year[i]
  
  
}






##### 30% depopulation
Stock2009_30 <- dePop(stock = Stock2009, dePopRate = 30)
Stock2009_30 <- rbind(Stock_2008L, Stock2009_30)

##### 40% depopulation
Stock2009_40 <- dePop(stock = Stock2009, dePopRate = 40)
Stock2009_40 <- rbind(Stock_2008L, Stock2009_40)

##### 50% depopulation
Stock2009_50 <- dePop(stock = Stock2009, dePopRate = 50)
Stock2009_50 <- rbind(Stock_2008L, Stock2009_50)

##### 60% depopulation
Stock2009_60 <- dePop(stock = Stock2009, dePopRate = 60)
Stock2009_60 <- rbind(Stock_2008L, Stock2009_60)

##### 70% depopulation
Stock2009_70 <- dePop(stock = Stock2009, dePopRate = 70)
Stock2009_70 <- rbind(Stock_2008L, Stock2009_70)

##### 80% depopulation
Stock2009_80 <- dePop(stock = Stock2009, dePopRate = 80)
Stock2009_80 <- rbind(Stock_2008L, Stock2009_80)

##### 90% depopulation
Stock2009_90 <- dePop(stock = Stock2009, dePopRate = 90)
Stock2009_90 <- rbind(Stock_2008L, Stock2009_90)

#### I have to study the exports and determine the share of production going towards exports



























