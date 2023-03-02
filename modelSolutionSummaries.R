# Estimated Equilibrium Parameters

# mu tildes
mu_Tildes_MeansNIII <- apply(unique(mu_Tildes_eq[1:25,]), 2, mean)
mu_Tildes_MeansNIII <- mu_Tildes_MeansNIII %>% as.data.frame()
names(mu_Tildes_MeansNIII) <- "muMean"
mu_Tildes_MeansNIII <- mu_Tildes_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

mu_Tildes_MediansNIII <- apply(unique(mu_Tildes_eq[1:25,]), 2, median)
mu_Tildes_MediansNIII <- mu_Tildes_MediansNIII %>% as.data.frame()
names(mu_Tildes_MediansNIII) <- "muMedian"
mu_Tildes_MediansNIII <- mu_Tildes_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

mu_Tildes_MMNIII <- merge(mu_Tildes_MeansNIII, mu_Tildes_MediansNIII)

# s tildes
s_Tildes_MeansNIII <- apply(unique(s_Tildes_eq[1:25,]), 2, mean)
s_Tildes_MeansNIII <- s_Tildes_MeansNIII %>% as.data.frame()
names(s_Tildes_MeansNIII) <- "sMean"
s_Tildes_MeansNIII <- s_Tildes_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

s_Tildes_MediansNIII <- apply(unique(s_Tildes_eq[1:25,]), 2, median)
s_Tildes_MediansNIII <- s_Tildes_MediansNIII %>% as.data.frame()
names(s_Tildes_MediansNIII) <- "sMedian"
s_Tildes_MediansNIII <- s_Tildes_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

s_Tildes_MMNIII <- merge(s_Tildes_MeansNIII, s_Tildes_MediansNIII)


merge(mu_Tildes_MMNIII,s_Tildes_MMNIII) %>% select(Year, muMedian, sMedian) %>% filter(Year >2009) %>% round(3)


###### Fitted Fed Cattle Equilibrium prices

EQprices_ps_MeansNIII <- apply(unique(prices_ps_eq[1:25,]), 2, mean)
EQprices_ps_MeansNIII <- EQprices_ps_MeansNIII %>% as.data.frame()
names(EQprices_ps_MeansNIII) <- "psMean"
EQprices_ps_MeansNIII <- EQprices_ps_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_ps_MediansNIII <- apply(unique(prices_ps_eq[1:25,]), 2, median)
EQprices_ps_MediansNIII <- EQprices_ps_MediansNIII %>% as.data.frame()
names(EQprices_ps_MediansNIII) <- "psMedian"
EQprices_ps_MediansNIII <- EQprices_ps_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestPSNIII <- merge(EQprices_ps_MeansNIII, EQprices_ps_MediansNIII)

EQestObsPSNIII <- left_join(EQestPSNIII,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

EQestObsPSNIII

EQestObsPSNIII_Err <- EQestObsPSNIII %>% select(Year, psMedian, ps) %>% 
  mutate(eHat = ((ps-psMedian)/ps)* 100)

EQestObsPSNIII_Err_Median <- median(abs(EQestObsPSNIII_Err$eHat)) %>% round(2)
EQestObsPSNIII_Err_Max <- max(abs(EQestObsPSNIII_Err$eHat)) %>% round(2)

###### Fitted Cull Cow Equilibrium prices
EQprices_pc_MeansNIII <- apply(unique(prices_pc_eq[1:25,]), 2, mean)
EQprices_pc_MeansNIII <- EQprices_pc_MeansNIII %>% as.data.frame()
names(EQprices_pc_MeansNIII) <- "pcMean"
EQprices_pc_MeansNIII <- EQprices_pc_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_pc_MediansNIII <- apply(unique(prices_pc_eq[1:25,]), 2, median)
EQprices_pc_MediansNIII <- EQprices_pc_MediansNIII %>% as.data.frame()
names(EQprices_pc_MediansNIII) <- "pcMedian"
EQprices_pc_MediansNIII <- EQprices_pc_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestPCNIII <- merge(EQprices_pc_MeansNIII, EQprices_pc_MediansNIII)

EQestObsPCNIII <- left_join(EQestPCNIII,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

EQestObsPCNIII

EQestObsPCNIII_Err <- EQestObsPCNIII %>% select(Year, pcMedian, pc) %>% 
  mutate(eHat = ((pc-pcMedian)/pc)*100)

EQestObsPCNIII_Err_Median <- median(abs(EQestObsPCNIII_Err$eHat)) %>% round(2)
EQestObsPCNIII_Err_Max <- max(abs(EQestObsPCNIII_Err$eHat)) %>% round(2)


EQestObsPCNIII_R2 <- EQestObsPCNIII_Err %>% select(Year, pcMedian, pc) %>% 
  mutate(resSquared = (pcMedian-pc)^2, totSquared = (pc - mean(pc))^2)

EQestObsPCNIII_sumSquaredRes <- sum(EQestObsPCNIII_R2$resSquared)
EQestObsPCNIII_sumSquaredTotal <- sum(EQestObsPCNIII_R2$totSquared)

EQestObsPCNIII_RSquared <- 1 -  (EQestObsPCNIII_sumSquaredRes/EQestObsPCNIII_sumSquaredTotal)



mergedPrices <- merge(EQestObsPSNIII %>% select(-errMean, -errmedian), 
                      EQestObsPCNIII %>% select(-errMean, -errmedian)) %>% select(Year, ps, psMedian, 
                                                                                 pc, pcMedian) %>%
  filter(Year >= 2010)
mergedPrices[,-1] <- mergedPrices[,-1] * 100


###### Fitted Holding costs
EQcosts_hc_MeansNIII <- apply(unique(prices_hc_eq[1:25,]), 2, mean)
EQcosts_hc_MeansNIII <- EQcosts_hc_MeansNIII %>% as.data.frame()
names(EQcosts_hc_MeansNIII) <- "hcMean"
EQcosts_hc_MeansNIII <- EQcosts_hc_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQcosts_hc_MediansNIII <- apply(unique(prices_hc_eq[1:25,]), 2, median)
EQcosts_hc_MediansNIII <- EQcosts_hc_MediansNIII %>% as.data.frame()
names(EQcosts_hc_MediansNIII) <- "hcMedian"
EQcosts_hc_MediansNIII <- EQcosts_hc_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestHCNIII <- merge(EQcosts_hc_MeansNIII, EQcosts_hc_MediansNIII)

###### Fitted Expected Prices
EQprices_Eps_MeansNIII <- apply(unique(expected_PS_eq[1:25,]), 2, mean)
EQprices_Eps_MeansNIII <- EQprices_Eps_MeansNIII %>% as.data.frame()
names(EQprices_Eps_MeansNIII) <- "EpsMean"
EQprices_Eps_MeansNIII <- EQprices_Eps_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_Eps_MediansNIII <- apply(unique(expected_PS_eq[1:25,]), 2, median)
EQprices_Eps_MediansNIII <- EQprices_Eps_MediansNIII %>% as.data.frame()
names(EQprices_Eps_MediansNIII) <- "EpsMedian"
EQprices_Eps_MediansNIII <- EQprices_Eps_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPSNIII <- merge(EQprices_Eps_MeansNIII, EQprices_Eps_MediansNIII)


EQprices_Epc_MeansNIII <- apply(unique(expected_PC_eq[1:25,]), 2, mean)
EQprices_Epc_MeansNIII <- EQprices_Epc_MeansNIII %>% as.data.frame()
names(EQprices_Epc_MeansNIII) <- "EpcMean"
EQprices_Epc_MeansNIII <- EQprices_Epc_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Epc_MediansNIII <- apply(unique(expected_PC_eq[1:25,]), 2, median)
EQprices_Epc_MediansNIII <- EQprices_Epc_MediansNIII %>% as.data.frame()
names(EQprices_Epc_MediansNIII) <- "EpcMedian"
EQprices_Epc_MediansNIII <- EQprices_Epc_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPCNIII <- merge(EQprices_Epc_MeansNIII, EQprices_Epc_MediansNIII)


# Fitted Fed Cattle Equilibrium Supply
EQquantities_sl_MeansNIII <- apply(unique(slNodes_eq[1:25,]), 2, mean)
EQquantities_sl_MeansNIII <- EQquantities_sl_MeansNIII %>% as.data.frame()
names(EQquantities_sl_MeansNIII) <- "slMean"
EQquantities_sl_MeansNIII <- EQquantities_sl_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQquantities_sl_MediansNIII <- apply(unique(slNodes_eq[1:25,]), 2, median)
EQquantities_sl_MediansNIII <- EQquantities_sl_MediansNIII %>% as.data.frame()
names(EQquantities_sl_MediansNIII) <- "slMedian"
EQquantities_sl_MediansNIII <- EQquantities_sl_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestSLNIII <- merge(EQquantities_sl_MeansNIII, EQquantities_sl_MediansNIII)

supp_sl_MODEL <- supp_diss_adj %>% select(Year, slSM = fedSlaughter_BillLb)

# supp_sl_MODEL <- obsEst_sl_Supply %>% select(Year, slSM = sl_obs)
# supp_sl_MODEL <- quantities_prices_capK %>% select(Year, slSM)

EQestObsSLNIII <- left_join(EQestSLNIII, supp_sl_MODEL) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)

EQestObsSLNIII 


EQestObsSLNIII_Err <- EQestObsSLNIII %>% select(Year, slMedian, slSM) %>% 
  mutate(eHat = ((slSM-slMedian)/slSM)*100)

EQestObsSLNIII_Err_Median <- median(abs(EQestObsSLNIII_Err$eHat)) %>% round(2)
EQestObsSLNIII_Err_Max <- max(abs(EQestObsSLNIII_Err$eHat)) %>% round(2)


EQestObsSLNIII_R2 <- EQestObsSLNIII_Err %>% select(Year, slMedian, slSM) %>% 
  mutate(resSquared = (slMedian-slSM)^2, totSquared = (slSM - mean(slSM))^2)

EQestObsSLNIII_sumSquaredRes <- sum(EQestObsSLNIII_R2$resSquared)
EQestObsSLNIII_sumSquaredTotal <- sum(EQestObsSLNIII_R2$totSquared)

EQestObsSLNIII_RSquared <- 1 -  (EQestObsSLNIII_sumSquaredRes/EQestObsSLNIII_sumSquaredTotal)

EQestObsSLNIII_DiagResPlot <- plot(EQestObsSLNIII$slMedian, EQestObsSLNIII$errmedian)


# Fitted Cull Cow Equilibrium Supply 
EQquantities_cl_MeansNIII <- apply(unique(clNodes_eq[1:25,]), 2, mean)
EQquantities_cl_MeansNIII <- EQquantities_cl_MeansNIII %>% as.data.frame()
names(EQquantities_cl_MeansNIII) <- "clMean"
EQquantities_cl_MeansNIII <- EQquantities_cl_MeansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQquantities_cl_MediansNIII <- apply(unique(clNodes_eq[1:25,]), 2, median)
EQquantities_cl_MediansNIII <- EQquantities_cl_MediansNIII %>% as.data.frame()
names(EQquantities_cl_MediansNIII) <- "clMedian"
EQquantities_cl_MediansNIII <- EQquantities_cl_MediansNIII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestCLNIII <- merge(EQquantities_cl_MeansNIII, EQquantities_cl_MediansNIII)

supp_cl_MODEL <- supp_diss_adj %>% select(Year, clSM = cowsCulled_BillLb)

# supp_cl_MODEL <- obsEst_cl_Supply %>% select(Year, clSM = cl_obs)
# supp_cl_MODEL <- quantities_prices_capK %>% select(Year, clSM)

EQestObsCLNIII <- left_join(EQestCLNIII, supp_cl_MODEL) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)
EQestObsCLNIII

EQestObsCLNIII_Err <- EQestObsCLNIII %>% select(Year, clMedian, clSM) %>% 
  mutate(eHat = ((clSM-clMedian)/clSM)*100)

EQestObsCLNIII_Err_Median <- median(abs(EQestObsCLNIII_Err$eHat)) %>% round(2)
EQestObsCLNIII_Err_Max <- max(abs(EQestObsCLNII_Err$eHat)) %>% round(2)


EQestObsCLNIII_R2 <- EQestObsCLNIII_Err %>% select(Year, clMedian, clSM) %>% filter(Year >= 1990) %>%
  mutate(resSquared = (clMedian-clSM)^2, totSquared = (clSM - mean(clSM))^2)

EQestObsCLNIII_sumSquaredRes <- sum(EQestObsCLNIII_R2$resSquared)
EQestObsCLNIII_sumSquaredTotal <- sum(EQestObsCLNIII_R2$totSquared)

EQestObsCLNIII_RSquared <- 1 -  (EQestObsCLNIII_sumSquaredRes/EQestObsCLNIII_sumSquaredTotal)

EQestObsCLNIII_DiagResPlot <- plot(EQestObsCLNIII$clMedian, EQestObsCLNIII$errmedian)


merge(EQestObsSLNIII %>% select(-errMean, -errmedian), 
      EQestObsCLNIII %>% select(-errMean, -errmedian)) %>% 
  select(Year, slSM, slMedian, clSM, clMedian) %>% round(2) %>% filter(Year >= 2010)


#### Fitted Total Equilibrium Supply 

EQestTSNIII <- merge(EQestCLNIII, EQestSLNIII) %>% transmute(Year = Year, TSmean = slMean + clMean, 
                                                          TSmedian = slMedian + clMedian)

totalSupply <- supp_diss_adj %>% transmute(Year = Year, TS = TotalSupply)

EQestObsTSNIII <- left_join(EQestTSNIII,totalSupply) %>% select(Year, TSmean, TSmedian, TS) %>% 
  mutate(errMean = (TS - TSmean), errmedian = (TS- TSmedian)) %>% round(4)

EQestObsTSNIII %>% select(Year, TSmedian, TS) %>% filter(Year >= 2010) %>%
  select(Year, TS, TSmedian) %>% round(2)








# #### Final iterations quantities
# # Fed Cattle Supply
# ITRquantities_sl_MeansNII <- apply(slNodes_itr[1:25,], 2, mean)
# ITRquantities_sl_MeansNII <- ITRquantities_sl_MeansNII %>% as.data.frame()
# names(ITRquantities_sl_MeansNII) <- "slMean"
# ITRquantities_sl_MeansNII <- ITRquantities_sl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRquantities_sl_MediansNII <- apply(slNodes_itr[1:25,], 2, median)
# ITRquantities_sl_MediansNII <- ITRquantities_sl_MediansNII %>% as.data.frame()
# names(ITRquantities_sl_MediansNII) <- "slMedian"
# ITRquantities_sl_MediansNII <- ITRquantities_sl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRestSLNII <- merge(ITRquantities_sl_MeansNII, ITRquantities_sl_MediansNII)
# 
# supp_sl_MODEL <- supp_diss_adj %>% select(Year, slSM = fedSlaughter_BillLb)
# 
# ITRestObsSLNII <- left_join(ITRestSLNII, supp_sl_MODEL) %>% select(Year, slMean, slMedian, slSM) %>% 
#   mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
# ITRestObsSLNII
# 
# # Cull Cow Supply
# ITRquantities_cl_MeansNII <- apply(clNodes_itr[1:25,], 2, mean)
# ITRquantities_cl_MeansNII <- ITRquantities_cl_MeansNII %>% as.data.frame()
# names(ITRquantities_cl_MeansNII) <- "clMean"
# ITRquantities_cl_MeansNII <- ITRquantities_cl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRquantities_cl_MediansNII <- apply(clNodes_itr[1:25,], 2, median)
# ITRquantities_cl_MediansNII <- ITRquantities_cl_MediansNII %>% as.data.frame()
# names(ITRquantities_cl_MediansNII) <- "clMedian"
# ITRquantities_cl_MediansNII <- ITRquantities_cl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRestCLNII <- merge(ITRquantities_cl_MeansNII, ITRquantities_cl_MediansNII)
# 
# supp_cl_MODEL <- supp_diss_adj %>% select(Year, clSM = cowsCulled_BillLb)
# 
# ITRestObsCLNII <- left_join(ITRestCLNII, supp_cl_MODEL) %>% select(Year, clMean, clMedian, clSM) %>% 
#   mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)
# ITRestObsCLNII
# 
# merge(ITRestObsSLNII %>% select(-errMean, -errmedian), 
#       ITRestObsCLNII %>% select(-errMean, -errmedian)) %>% 
#   select(Year, slSM, slMedian, clSM, clMedian) %>% round(2) %>% filter(Year >= 2010)
# 
# 
# ### Fitted fed cattle final iterated prices
# 
# ITRprices_ps_MeansNII <- apply(prices_ps_itr[1:25,], 2, mean)
# ITRprices_ps_MeansNII <- ITRprices_ps_MeansNII %>% as.data.frame()
# names(ITRprices_ps_MeansNII) <- "psMean"
# ITRprices_ps_MeansNII <- ITRprices_ps_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRprices_ps_MediansNII <- apply(prices_ps_itr[1:25,], 2, median)
# ITRprices_ps_MediansNII <- ITRprices_ps_MediansNII %>% as.data.frame()
# names(ITRprices_ps_MediansNII) <- "psMedian"
# ITRprices_ps_MediansNII <- ITRprices_ps_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRestPSNII <- merge(ITRprices_ps_MeansNII, ITRprices_ps_MediansNII)
# 
# ITRestObsPSNII <- left_join(ITRestPSNII,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
#   mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)
# 
# ITRestObsPSNII
# 
# ###### Fitted Cull Cow Equilibrium prices
# ITRprices_pc_MeansNII <- apply(prices_pc_itr[1:25,], 2, mean)
# ITRprices_pc_MeansNII <- ITRprices_pc_MeansNII %>% as.data.frame()
# names(ITRprices_pc_MeansNII) <- "pcMean"
# ITRprices_pc_MeansNII <- ITRprices_pc_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRprices_pc_MediansNII <- apply(prices_pc_itr[1:25,], 2, median)
# ITRprices_pc_MediansNII <- ITRprices_pc_MediansNII %>% as.data.frame()
# names(ITRprices_pc_MediansNII) <- "pcMedian"
# ITRprices_pc_MediansNII <- ITRprices_pc_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# ITRestPCNII <- merge(ITRprices_pc_MeansNII, ITRprices_pc_MediansNII)
# 
# ITRestObsPCNII <- left_join(ITRestPCNII,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
#   mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)
# 
# ITRestObsPCNII
# 
# mergedPricesITR <- merge(ITRestObsPSNII %>% select(-errMean, -errmedian), 
#                          ITRestObsPCNII %>% select(-errMean, -errmedian)) %>% select(Year, ps, psMedian, psMean, 
#                                                                                      pc, pcMedian, pcMean) %>%
#   filter(Year >= 2010)
# mergedPricesITR[,-1] <- mergedPricesITR[,-1] * 100

