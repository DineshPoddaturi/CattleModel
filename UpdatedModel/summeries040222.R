
# Here I get the means and median of the first 25 nodes of the equilibrium prices and quantities

EQprices_ps_MeansNI <- apply(prices_ps_eq[1:25,], 2, mean)
EQestPSNI <- EQprices_ps_MeansNI %>% as.data.frame()
names(EQestPSNI) <- "psMean"
EQestPSNI <- EQestPSNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_ps_MediansNI <- apply(prices_ps_eq[1:25,], 2, median)
EQestPS_MediansNI <- EQprices_ps_MediansNI %>% as.data.frame()
names(EQestPS_MediansNI) <- "psMedian"
EQprices_ps_MediansNI <- EQestPS_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestPSNI <- merge(EQestPSNI, EQprices_ps_MediansNI)

## @knitr EQestObsPS

EQestObsPSNI <- left_join(EQestPSNI,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

EQestObsPSNI

## @knitr EQestObsPS_plot

EQestObsPS_plotNI <- EQestObsPSNI %>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsPS$Year[1],EQestObsPS$Year[nrow(EQestObsPS)]))) +
  expand_limits(y = 0.5)

EQestObsPS_plotNI

## @knitr EQestObsPC
EQprices_pc_MeansNI <- apply(prices_pc_eq[1:25,], 2, mean)
EQestPCNI <- EQprices_pc_MeansNI %>% as.data.frame()
names(EQestPCNI) <- "pcMean"
EQestPCNI <- EQestPCNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_pc_MediansNI <- apply(prices_pc_eq[1:25,], 2, median)
EQestPC_MediansNI <- EQprices_pc_MediansNI %>% as.data.frame()
names(EQestPC_MediansNI) <- "pcMedian"
EQprices_pc_MediansNI <- EQestPC_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestPCNI <- merge(EQestPCNI, EQprices_pc_MediansNI)

EQestObsPCNI <- left_join(EQestPCNI,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

EQestObsPCNI

## @knitr EQestObsPC_plot
EQestObsPC_plotNI <- EQestObsPCNI %>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsPC$Year[1],EQestObsPC$Year[nrow(EQestObsPC)]))) +
  expand_limits(y = 0.5)

EQestObsPC_plotNI

## @knitr EQestObsSL
EQquantities_sl_MeansNI <- apply(slNodes_eq[1:25,], 2, mean)
EQestSLNI <- EQquantities_sl_MeansNI %>% as.data.frame()
names(EQestSLNI) <- "slMean"
EQestSLNI <- EQestSLNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQquantities_sl_MediansNI <- apply(slNodes_eq[1:25,], 2, median)
EQestSL_MediansNI <- EQquantities_sl_MediansNI %>% as.data.frame()
names(EQestSL_MediansNI) <- "slMedian"
EQestSL_MediansNI <- EQestSL_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestSLNI <- merge(EQestSLNI, EQestSL_MediansNI)

# EQestObsSLNI <- left_join(EQestSLNI,quantities_prices_capK) %>% select(Year, slMean, slMedian, slSM) %>%
#   mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
# EQestObsSLNI

supp_sl_OBS <- supp_sl_adj %>% select(Year, slSM = Bill_meatLb_sl)

EQestObsSLNI <- left_join(EQestSLNI,supp_sl_OBS) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
EQestObsSLNI


## @knitr EQestObsSL_plot
EQestObsSL_plotNI <- EQestObsSLNI %>% ggplot(aes(x=Year))+geom_line(aes(y=slMean, color="FED PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = slMean, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
  geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + geom_line(aes(y=slMedian, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = slMedian, color = "FED PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsSL$Year[1],EQestObsSL$Year[nrow(EQestObsSL)]))) 
EQestObsSL_plotNI

## @knitr EQestObsCL
EQquantities_cl_MeansNI <- apply(clNodes_eq[1:25,], 2, mean)
EQestCLNI <- EQquantities_cl_MeansNI %>% as.data.frame()
names(EQestCLNI) <- "clMean"
EQestCLNI <- EQestCLNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQquantities_cl_MediansNI <- apply(clNodes_eq[1:25,], 2, median)
EQestCL_MediansNI <- EQquantities_cl_MediansNI %>% as.data.frame()
names(EQestCL_MediansNI) <- "clMedian"
EQestCL_MediansNI <- EQestCL_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestCLNI <- merge(EQestCLNI, EQestCL_MediansNI)

# EQestObsCL <- left_join(EQestCL,quantities_prices_capK) %>% select(Year, clMean, clMedian, clSM) %>% 
#   mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)


supp_cl_OBS <- supp_cl_adj %>% select(Year, clSM = Bill_meatLb_cl)

EQestObsCLNI <- left_join(EQestCLNI,supp_cl_OBS) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)


EQestObsCLNI

## @knitr EQestObsCL_plot
EQestObsCL_plotNI <- EQestObsCLNI %>% ggplot(aes(x=Year))+geom_line(aes(y=clMean, color="CULL PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = clMean, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
  geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + geom_line(aes(y=clMedian, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = clMedian, color = "CULL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsCL$Year[1],EQestObsCL$Year[nrow(EQestObsCL)])))

EQestObsCL_plotNI


#### Plotting the observed and total derived demand

EQestTSNI <- merge(EQestCLNI, EQestSLNI) %>% transmute(Year = Year, TSmean = slMean + clMean, 
                                                    TSmedian = slMedian + clMedian)

EQquantities_A_MeansNI <- apply(A_nodes_eq[1:25,], 2, mean)
EQestANI <- EQquantities_A_MeansNI %>% as.data.frame()
names(EQestANI) <- "AMean"
EQestANI <- EQestANI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQquantities_A_MediansNI <- apply(A_nodes_eq[1:25,], 2, median)
EQestA_MediansNI <- EQquantities_A_MediansNI %>% as.data.frame()
names(EQestA_MediansNI) <- "AMedian"
EQestA_MediansNI <- EQestA_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestANI <- merge(EQestANI, EQestA_MediansNI) %>% mutate(TSmean = AMean, TSmedian = AMedian)

# EQestObsA <- left_join(EQestA,A_quant) %>% select(Year, Amean, Amedian, A) %>% 
#   mutate(errMean = (A - Amean), errmedian = (A- Amedian)) %>% round(4)

totalSupply <- totalSupply_adj %>% transmute(Year = Year, TS = TotalSupply)

totalSupply <- totalDisappearedNew %>% transmute(Year = Year, TS = total_meat_bill)

EQestObsTSNI <- left_join(EQestANI,totalSupply) %>% select(Year, TSmean, TSmedian, TS) %>% 
  mutate(errMean = (TS - TSmean), errmedian = (TS- TSmedian)) %>% round(4)

EQestObsTS_plotNI <- EQestObsTSNI %>% ggplot(aes(x=Year))+ geom_line(aes(y=TS, color = "TOTAL PRODUCTION OBS")) + 
  geom_point(aes(y= TS, color = "TOTAL PRODUCTION OBS")) + geom_line(aes(y=TSmedian, color="TOTAL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = TSmedian, color = "TOTAL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsA$Year[1],EQestObsA$Year[nrow(EQestObsA)])))

EQestObsTS_plotNI



EQcosts_hc_MeansNI <- apply(prices_hc_eq[1:25,], 2, mean)
EQestHCNI <- EQcosts_hc_MeansNI %>% as.data.frame()
names(EQestHCNI) <- "hcMean"
EQestHCNI <- EQestHCNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQcosts_hc_MediansNI <- apply(prices_hc_eq[1:25,], 2, median)
EQestHC_MediansNI <- EQcosts_hc_MediansNI %>% as.data.frame()
names(EQestHC_MediansNI) <- "hcMedian"
EQestHC_MediansNI <- EQestHC_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestHCN <- merge(EQestHCNI, EQestHC_MediansNI)



###### Expected prices Equilibrium
EQprices_Eps_MeansNI <- apply(expected_PS_eq[1:25,], 2, mean)
EQestEPSNI <- EQprices_Eps_MeansNI %>% as.data.frame()
names(EQestEPSNI) <- "EpsMean"
EQestEPSNI <- EQestEPSNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Eps_MediansNI <- apply(expected_PS_eq[1:25,], 2, median)
EQestEPS_MediansNI <- EQprices_Eps_MediansNI %>% as.data.frame()
names(EQestEPS_MediansNI) <- "EpsMedian"
EQprices_Eps_MediansNI <- EQestEPS_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPSNI <- merge(EQestEPSNI, EQprices_Eps_MediansNI)


EQprices_Epc_MeansNI <- apply(expected_PC_eq[1:25,], 2, mean)
EQestEPCNI <- EQprices_Epc_MeansNI %>% as.data.frame()
names(EQestEPCNI) <- "EpcMean"
EQestEPCNI <- EQestEPCNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Epc_MediansNI <- apply(expected_PC_eq[1:25,], 2, median)
EQestEPC_MediansNI <- EQprices_Epc_MediansNI %>% as.data.frame()
names(EQestEPC_MediansNI) <- "EpcMedian"
EQprices_Epc_MediansNI <- EQestEPC_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPCN <- merge(EQestEPCNI, EQprices_Epc_MediansNI)



## @knitr ITRestObsPS

ITRprices_ps_MeansNI <- apply(prices_ps_itr[1:25,], 2, mean)
ITRestPSNI <- ITRprices_ps_MeansNI %>% as.data.frame()
names(ITRestPSNI) <- "psMean"
ITRestPSNI <- ITRestPSNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_ps_MediansNI <- apply(prices_ps_itr[1:25,], 2, median)
ITRestPS_MediansNI <- ITRprices_ps_MediansNI %>% as.data.frame()
names(ITRestPS_MediansNI) <- "psMedian"
ITRprices_ps_MediansNI <- ITRestPS_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestPSNI <- merge(ITRestPSNI, ITRprices_ps_MediansNI)

ITRestObsPSNI <- left_join(ITRestPSNI,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

ITRestObsPSNI


## @knitr ITRestObsPS_plot
ITRestObsPS_plotNI <- ITRestObsPSNI %>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPS$Year[1],ITRestObsPS$Year[nrow(ITRestObsPS)]))) +
  expand_limits(y = 0.5)

ITRestObsPS_plotNI


## @knitr ITRestObsPC

ITRprices_pc_MeansNI <- apply(prices_pc_itr[1:25,], 2, mean)
ITRestPCNI <- ITRprices_pc_MeansNI %>% as.data.frame()
names(ITRestPCNI) <- "pcMean"
ITRestPCNI <- ITRestPCNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_pc_MediansNI <- apply(prices_pc_itr[1:25,], 2, median)
ITRestPC_MediansNI <- ITRprices_pc_MediansNI %>% as.data.frame()
names(ITRestPC_MediansNI) <- "pcMedian"
ITRprices_pc_MediansNI <- ITRestPC_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestPCNI <- merge(ITRestPCNI, ITRprices_pc_Medians)

ITRestObsPCNI <- left_join(ITRestPCNI,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

ITRestObsPCNI


## @knitr ITRestObsPC_plot
ITRestObsPC_plotNI <- ITRestObsPCNI %>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPC$Year[1],ITRestObsPC$Year[nrow(ITRestObsPC)]))) +
  expand_limits(y = 0.5)
ITRestObsPC_plotNI


## @knitr ITRestObsSL
ITRquantities_sl_MeansNI <- apply(slNodes_itr[1:25,], 2, mean)
ITRestSLNI <- ITRquantities_sl_MeansNI %>% as.data.frame()
names(ITRestSLNI) <- "slMean"
ITRestSLNI <- ITRestSLNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRquantities_sl_MediansNI <- apply(slNodes_itr[1:25,], 2, median)
ITRestSL_MediansNI <- ITRquantities_sl_MediansNI %>% as.data.frame()
names(ITRestSL_MediansNI) <- "slMedian"
ITRestSL_MediansNI <- ITRestSL_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestSLNI <- merge(ITRestSLNI, ITRestSL_MediansN)

supp_sl_OBS <- supp_sl_adj %>% select(Year, slSM = Bill_meatLb_sl)

ITRestObsSLNI <- left_join(ITRestSLNI,supp_sl_OBS) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)

ITRestObsSLNI

## @knitr ITRestObsSL_plot
ITRestObsSL_plotNI <- ITRestObsSLNI %>% ggplot(aes(x=Year))+geom_line(aes(y=slMean, color="FED PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = slMean, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
  geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + geom_line(aes(y=slMedian, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = slMedian, color = "FED PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsSL$Year[1],ITRestObsSL$Year[nrow(ITRestObsSL)]))) 
ITRestObsSL_plotNI

## @knitr ITRestObsCL
ITRquantities_cl_MeansNI <- apply(clNodes_itr[1:25,], 2, mean)
ITRestCLNI <- ITRquantities_cl_MeansNI %>% as.data.frame()
names(ITRestCLNI) <- "clMean"
ITRestCLNI <- ITRestCLNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRquantities_cl_MediansNI <- apply(clNodes_itr[1:25,], 2, median)
ITRestCL_MediansNI <- ITRquantities_cl_MediansNI %>% as.data.frame()
names(ITRestCL_MediansNI) <- "clMedian"
ITRestCL_MediansNI <- ITRestCL_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestCLNI <- merge(ITRestCLNI, ITRestCL_MediansNI)

supp_cl_OBS <- supp_cl_adj %>% select(Year, clSM = Bill_meatLb_cl)

ITRestObsCLNI <- left_join(ITRestCLNI,supp_cl_OBS) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)

ITRestObsCLNI

## @knitr ITRestObsCL_plot
ITRestObsCL_plotNI <- ITRestObsCLNI %>% ggplot(aes(x=Year))+geom_line(aes(y=clMean, color="CULL PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = clMean, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
  geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + geom_line(aes(y=clMedian, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = clMedian, color = "CULL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsCL$Year[1],ITRestObsCL$Year[nrow(ITRestObsCL)]))) 
ITRestObsCL_plotNI


ITRcosts_hc_Means <- apply(prices_hc_itr[1:25,], 2, mean)
ITRestHC <- ITRcosts_hc_Means %>% as.data.frame()
names(ITRestHC) <- "hcMean"
ITRestHC <- ITRestHC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRcosts_hc_Medians <- apply(prices_hc_itr[1:25,], 2, median)
ITRestHC_Medians <- ITRcosts_hc_Medians %>% as.data.frame()
names(ITRestHC_Medians) <- "hcMedian"
ITRcosts_hc_Medians <- ITRestHC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestHC <- merge(ITRestHC, ITRcosts_hc_Medians)

###### Expected prices Converged
ITRprices_Eps_Means <- apply(expected_PS_itr[1:25,], 2, mean)
ITRestEPS <- ITRprices_Eps_Means %>% as.data.frame()
names(ITRestEPS) <- "EpsMean"
ITRestEPS <- ITRestEPS %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_Eps_Medians <- apply(expected_PS_itr[1:25,], 2, median)
ITRestEPS_Medians <- ITRprices_Eps_Medians %>% as.data.frame()
names(ITRestEPS_Medians) <- "EpsMedian"
ITRprices_Eps_Medians <- ITRestEPS_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestEPS <- merge(ITRestEPS, ITRprices_Eps_Medians)


ITRprices_Epc_Means <- apply(expected_PC_itr[1:25,], 2, mean)
ITRestEPC <- ITRprices_Epc_Means %>% as.data.frame()
names(ITRestEPC) <- "EpcMean"
ITRestEPC <- ITRestEPC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_Epc_Medians <- apply(expected_PC_itr[1:25,], 2, median)
ITRestEPC_Medians <- ITRprices_Epc_Medians %>% as.data.frame()
names(ITRestEPC_Medians) <- "EpcMedian"
ITRprices_Epc_Medians <- ITRestEPC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestEPC <- merge(ITRestEPC, ITRprices_Epc_Medians)






mu_Tildes_MeansNI <- apply(mu_Tildes_eq[1:25,], 2, mean)
mu_Tildes_MeansNI <- mu_Tildes_MeansNI %>% as.data.frame()
names(mu_Tildes_MeansNI) <- "muMean"
mu_Tildes_MeansNI <- mu_Tildes_MeansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_MediansNI <- apply(mu_Tildes_eq[1:25,], 2, median)
mu_Tildes_MediansNI <- mu_Tildes_MediansNI %>% as.data.frame()
names(mu_Tildes_MediansNI) <- "muMedian"
mu_Tildes_MediansNI <- mu_Tildes_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_MMNI <- merge(mu_Tildes_MeansNI, mu_Tildes_MediansNI)


s_Tildes_MeansNI <- apply(s_Tildes_eq[1:25,], 2, mean)
s_Tildes_MeansNI <- s_Tildes_MeansNI %>% as.data.frame()
names(s_Tildes_MeansNI) <- "sMean"
s_Tildes_MeansNI <- s_Tildes_MeansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_MediansNI <- apply(s_Tildes_eq[1:25,], 2, median)
s_Tildes_MediansNI <- s_Tildes_MediansNI %>% as.data.frame()
names(s_Tildes_MediansNI) <- "sMedian"
s_Tildes_MediansNI <- s_Tildes_MediansNI %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_MMNI <- merge(s_Tildes_MeansNI, s_Tildes_MediansNI)




mu_Tildes_Means_itr <- apply(mu_Tildes_itr[1:25,], 2, mean)
mu_Tildes_Means_itr <- mu_Tildes_Means_itr %>% as.data.frame()
names(mu_Tildes_Means_itr) <- "muMean"
mu_Tildes_Means_itr <- mu_Tildes_Means_itr %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_Medians_itr <- apply(mu_Tildes_itr[1:25,], 2, median)
mu_Tildes_Medians_itr <- mu_Tildes_Medians_itr %>% as.data.frame()
names(mu_Tildes_Medians_itr) <- "muMedian"
mu_Tildes_Medians_itr <- mu_Tildes_Medians_itr %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_MM_itr <- merge(mu_Tildes_Means_itr, mu_Tildes_Medians_itr)


s_Tildes_Means_itr <- apply(s_Tildes_itr[1:25,], 2, mean)
s_Tildes_Means_itr <- s_Tildes_Means_itr %>% as.data.frame()
names(s_Tildes_Means_itr) <- "sMean"
s_Tildes_Means_itr <- s_Tildes_Means_itr %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_Medians_itr <- apply(s_Tildes_itr[1:25,], 2, median)
s_Tildes_Medians_itr <- s_Tildes_Medians_itr %>% as.data.frame()
names(s_Tildes_Medians_itr) <- "sMedian"
s_Tildes_Medians_itr <- s_Tildes_Medians_itr %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_MM_itr <- merge(s_Tildes_Means_itr, s_Tildes_Medians_itr)








# EQestObsPS_Medians <- left_join(EQprices_ps_Medians,quantities_prices_capK) %>% select(Year, fedPrice, ps) %>% mutate(err = (fedPrice - ps))
# EQestObsPS_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=fedPrice, color="PS RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = fedPrice, color = "PS RATIONAL (MEDIAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsPS_Medians$Year[1],EQestObsPS_Medians$Year[nrow(EQestObsPS_Medians)]))) +
#   expand_limits(y = 0.5)
# 
# 
# EQprices_pc_Means <- apply(prices_pc_eq[1:25,], 2, mean)
# EQestPC <- EQprices_pc_Means %>% as.data.frame()
# names(EQestPC) <- "cullPrice"
# EQestPC <- EQestPC %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# EQestObsPC <- left_join(EQestPC,quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(err = (cullPrice - pc))
# EQestObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL (MEAN)")) +
#   geom_point(aes(y = cullPrice, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsPC$Year[1],EQestObsPC$Year[nrow(EQestObsPC)]))) +
#   expand_limits(y = 0.5)
# 
# EQprices_pc_Medians <- apply(prices_pc_eq[1:25,], 2, median)
# EQestPC_Medians <- EQprices_pc_Medians %>% as.data.frame()
# names(EQestPC_Medians) <- "cullPrice"
# EQprices_PC_Medians <- EQestPC_Medians %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# EQestObsPC_Medians <- left_join(EQprices_PC_Medians,quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(err = (cullPrice - pc))
# EQestObsPC_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = cullPrice, color = "PC RATIONAL (MEDIAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsPC$Year[1],EQestObsPC$Year[nrow(EQestObsPC)]))) +
#   expand_limits(y = 0.5)
# 
# 
# EQestObsSL <- left_join(EQestSL,quantities_prices_capK) %>% select(Year, fedProduction, slSM) %>% mutate(err = (fedProduction - slSM))
# EQestObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=fedProduction, color="FED PRODUCTION RATIONAL (MEAN)")) +
#   geom_point(aes(y = fedProduction, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
#   geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsSL$Year[1],EQestObsSL$Year[nrow(EQestObsSL)]))) 
# 
# 
# EQestObsSL_Medians <- left_join(EQestSL,quantities_prices_capK) %>% select(Year, fedProduction, slSM) %>% mutate(err = (fedProduction - slSM))
# EQestObsSL_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=fedProduction, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = fedProduction, color = "FED PRODUCTION RATIONAL (MEDIAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
#   geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsSL$Year[1],EQestObsSL$Year[nrow(EQestObsSL)]))) 
# 
# 
# EQquantities_cl_Means <- apply(clNodes_eq[1:25,], 2, mean)
# EQestCL <- EQquantities_cl_Means %>% as.data.frame()
# names(EQestCL) <- "cullProduction"
# EQestCL <- EQestCL %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# EQestObsCL <- left_join(EQestCL,quantities_prices_capK) %>% select(Year, cullProduction, clSM) %>% mutate(err = (cullProduction - clSM))
# EQestObsCL %>% ggplot(aes(x=Year))+geom_line(aes(y=cullProduction, color="CULL PRODUCTION RATIONAL (MEAN)")) +
#   geom_point(aes(y = cullProduction, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
#   geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsCL$Year[1],EQestObsCL$Year[nrow(EQestObsCL)])))
# 
# EQquantities_cl_Medians <- apply(clNodes_eq[1:25,], 2, median)
# EQestCL_Medians  <- EQquantities_cl_Medians %>% as.data.frame()
# names(EQestCL_Medians ) <- "cullProduction"
# EQestCL_Medians  <- EQestCL_Medians  %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# EQestObsCL_Medians  <- left_join(EQestCL_Medians ,quantities_prices_capK) %>% select(Year, cullProduction, clSM) %>% mutate(err = (cullProduction - clSM))
# EQestObsCL_Medians  %>% ggplot(aes(x=Year))+geom_line(aes(y=cullProduction, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = cullProduction, color = "CULL PRODUCTION RATIONAL (MEDIAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
#   geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(EQestObsCL_Medians$Year[1],EQestObsCL_Medians$Year[nrow(EQestObsCL_Medians)]))) 


# ITRprices_ps_Means <- apply(prices_ps_itr[1:25,], 2, mean)
# ITRestPS <- ITRprices_ps_Means %>% as.data.frame()
# names(ITRestPS) <- "slPrice"
# ITRestPS <- ITRestPS %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# 
# 
# 
# 
# ITRestObsPS <- left_join(ITRestPS,quantities_prices_capK) %>% select(Year, fedPrice, ps) %>% mutate(err = (fedPrice - ps))
# ITRestObsPS %>% ggplot(aes(x=Year))+geom_line(aes(y=fedPrice, color="PS RATIONAL (MEAN)")) +
#   geom_point(aes(y = fedPrice, color = "PS RATIONAL (MEAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPS$Year[1],ITRestObsPS$Year[nrow(ITRestObsPS)]))) +
#   expand_limits(y = 0.5)
# 
# ITRprices_ps_Medians <- apply(prices_ps_itr[1:25,], 2, median)
# ITRestPS_Medians <- ITRprices_ps_Medians %>% as.data.frame()
# names(ITRestPS_Medians) <- "fedPrice"
# ITRestPS_Medians <- ITRestPS_Medians %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestObsPS_Medians <- left_join(ITRestPS_Medians,quantities_prices_capK) %>% select(Year, fedPrice, ps) %>% mutate(err = (fedPrice - ps))
# ITRestObsPS_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=fedPrice, color="PS RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = fedPrice, color = "PS RATIONAL (MEDIAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPS_Medians$Year[1],ITRestObsPS_Medians$Year[nrow(ITRestObsPS_Medians)]))) +
#   expand_limits(y = 0.5)
# 
# ITRprices_pc_Means <- apply(prices_pc_itr[1:25,], 2, mean)
# ITRestPC <- ITRprices_pc_Means %>% as.data.frame()
# names(ITRestPC) <- "cullPrice"
# ITRestPC <- ITRestPC %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestObsPC <- left_join(ITRestPC,quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(err = (cullPrice - pc))
# ITRestObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL (MEAN)")) +
#   geom_point(aes(y = cullPrice, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPC$Year[1],ITRestObsPC$Year[nrow(ITRestObsPC)]))) +
#   expand_limits(y = 0.5)
# 
# ITRprices_pc_Medians <- apply(prices_pc_itr[1:25,], 2, median)
# ITRestPC_Medians <- ITRprices_pc_Medians %>% as.data.frame()
# names(ITRestPC_Medians) <- "cullPrice"
# ITRestPC_Medians <- ITRestPC_Medians %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestObsPC_Medians <- left_join(ITRestPC_Medians,quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(err = (cullPrice - pc))
# ITRestObsPC_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = cullPrice, color = "PC RATIONAL (MEDIAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPC$Year[1],ITRestObsPC$Year[nrow(ITRestObsPC)]))) +
#   expand_limits(y = 0.5)
# 
# 
# ITRquantities_sl_Means <- apply(slNodes_itr[1:25,], 2, mean)
# ITRestSL <- ITRquantities_sl_Means %>% as.data.frame()
# names(ITRestSL) <- "fedProduction"
# ITRestSL <- ITRestSL %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestObsSL <- left_join(ITRestSL,quantities_prices_capK) %>% select(Year, fedProduction, slSM) %>% mutate(err = (fedProduction - sl))
# ITRestObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=fedProduction, color="FED PRODUCTION RATIONAL (MEAN)")) +
#   geom_point(aes(y = fedProduction, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION  OBS")) + 
#   geom_point(aes(y=slSM, color = "FED PRODUCTION  OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsSL$Year[1],ITRestObsSL$Year[nrow(ITRestObsSL)])))
# 
# ITRquantities_sl_Medians <- apply(slNodes_itr[1:25,], 2, median)
# ITRestSL_Medians <- ITRquantities_sl_Medians %>% as.data.frame()
# names(ITRestSL_Medians) <- "fedProduction"
# ITRestSL_Medians <- ITRestSL_Medians %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestSL_Medians <- left_join(ITRestSL_Medians,quantities_prices_capK) %>% select(Year, fedProduction, slSM) %>% mutate(err = (fedProduction - sl))
# ITRestSL_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=fedProduction, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = fedProduction, color = "FED PRODUCTION RATIONAL (MEDIAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION  OBS")) + 
#   geom_point(aes(y=slSM, color = "FED PRODUCTION  OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestSL_Medians$Year[1],ITRestSL_Medians$Year[nrow(ITRestSL_Medians)]))) 
# 
# 
# ITRquantities_cl_Means <- apply(clNodes_itr[1:25,], 2, mean)
# ITRestCL <- ITRquantities_cl_Means %>% as.data.frame()
# names(ITRestCL) <- "cullProduction"
# ITRestCL <- ITRestCL %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestObsCL <- left_join(ITRestCL,quantities_prices_capK) %>% select(Year, cullProduction, clSM) %>% mutate(err = (cullProduction - cl))
# ITRestObsCL %>% ggplot(aes(x=Year))+geom_line(aes(y=cullProduction, color="CULL PRODUCTION RATIONAL (MEAN)")) +
#   geom_point(aes(y = cullProduction, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION  OBS")) + 
#   geom_point(aes(y=clSM, color = "CULL PRODUCTION  OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestObsCL$Year[1],ITRestObsCL$Year[nrow(ITRestObsCL)])))
# 
# ITRquantities_cl_Medians <- apply(clNodes_itr[1:25,], 2, median)
# ITRestCL_Medians <- ITRquantities_cl_Medians %>% as.data.frame()
# names(ITRestCL_Medians) <- "cullProduction"
# ITRestCL_Medians <- ITRestCL_Medians %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# ITRestCL_Medians <- left_join(ITRestCL_Medians,quantities_prices_capK) %>% select(Year, cullProduction, clSM) %>% mutate(err = (cullProduction - cl))
# ITRestCL_Medians %>% ggplot(aes(x=Year))+geom_line(aes(y=cullProduction, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = cullProduction, color = "CULL PRODUCTION RATIONAL (MEDIAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION  OBS")) + 
#   geom_point(aes(y=clSM, color = "CULL PRODUCTION  OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(ITRestCL_Medians$Year[1],ITRestCL_Medians$Year[nrow(ITRestCL_Medians)])))
# 
# 

shrT <- shareMetric(paramMu = MUtilde, paramS = Stilde, ps = proj2016$psMean, pc = proj2016$pcMean)

ANew <- (g * K1 - k3_est) * (1/shrT)

slNew <- ANew *  shrT * proj2016$AdjFactor

merge(proj_Q_P_lo,merge(proj_Q_P,proj_Q_P_up)) %>% select(Year, repHeif_Head_lo, repHeif_Head, repHeif_Head_up)




