
### Here I am getting the summary of the price differences. This can be used to set the boundaries for the price.
### Note: Without this the program can find any number that equates the supply and demand.
### Also note that this is weak form of rational expectations so we use the limited information available for us. 
### That means the decision maker knows how the prices are fluctuating and makes predictions of the future prices.

summary( lead(quantities_prices_capK$ps)-quantities_prices_capK$ps )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.27667 -0.02492  0.03296  0.02760  0.05510  0.28000        1 

summary( lead(quantities_prices_capK$ps,3)-quantities_prices_capK$ps )
# std((lead(quantities_prices_capK$ps,3)-quantities_prices_capK$ps) %>% na.omit())
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.32417  0.02262  0.09821  0.10929  0.23644  0.37750        3 

summary( lead(quantities_prices_capK$ps,2)-quantities_prices_capK$ps )
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -0.326667  0.006083  0.083417  0.060004  0.145708  0.318333         2


summary( lead(quantities_prices_capK$pc)-quantities_prices_capK$pc )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.29217 -0.03717  0.01442  0.01723  0.06054  0.25933        1 

summary( lead(quantities_prices_capK$pc,3)-quantities_prices_capK$pc )
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -0.386667 -0.003938  0.059292  0.080153  0.192417  0.371250         3 

summary( lead(quantities_prices_capK$pc,2)-quantities_prices_capK$pc )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.34550 -0.01883  0.04058  0.03968  0.11925  0.27042        2 

fedPS <- 0
for(i in 1:length(fedPrice)){
  fedPS[i] <- mean(colMeans(fedPrice[[i]][,apply(fedPrice[[i]],2,function(x) !all(x==0))]))
}


fedPrice[[1]][,apply(fedPrice[[1]],2,function(x) !all(x==0))]

round(c_cull_itr[[1]][, apply(c_cull_itr[[1]],2,function(x) !all(x==0))][,136],5)

round(c_fed_itr[[1]][, apply(c_fed_itr[[1]],2,function(x) !all(x==0))][,136],5)

colMeans(prices_ps)

####### NEW NOTE: take the unique values by rounding the matrix. This way we are not deflating/overflating the real
####### price estimate. 

###### Write the code to get the unique values of the matrix and plot the densities and also the price series. 
prices_pstemp <- NULL
estPS <- NULL
for(i in 1:ncol(prices_ps)){
  prices_pstemp <- unique((prices_ps[,i]))
  estPS[i] <- mean(prices_pstemp)
}
# prices_pstemp <- unique(round(prices_ps,5))
estPS <- colMeans(prices_ps) %>% as.data.frame()
# estPS <- apply(prices_ps,2,max) %>% as.data.frame()
estPS <- estPS %>% as.data.frame()
names(estPS) <- "fedPrice"
estPS <- estPS %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# estObsPS <- left_join(estPS, quantities_prices_capK) %>% select(Year, fedPrice, ps)
estObsPS <- merge(estPS, quantities_prices_capK) %>% select(Year, fedPrice, ps) %>% mutate(err = (fedPrice - ps))
estObsPS %>% ggplot(aes(x=Year))+geom_line(aes(y=fedPrice, color="PS RATIONAL")) +
  geom_point(aes(y = fedPrice, color = "PS RATIONAL")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(estObsPS$Year[1],estObsPS$Year[nrow(estObsPS)]))) +
  expand_limits(y = 0.5)

prices_pctemp <- NULL
estPC <- NULL
for(i in 1:ncol(prices_pc)){
  prices_pctemp <- unique(prices_pc[,i])
  estPC[i] <- mean(prices_pctemp)
}
# prices_pctemp <- unique(round(prices_pc,5))
estPC <- colMeans(prices_pc) %>% as.data.frame()
estPC <- estPC %>% as.data.frame()
names(estPC) <- "cullPrice"
estPC <- estPC %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
estObsPC <- merge(estPC, quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(err = (cullPrice - pc))
estObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL")) +
  geom_point(aes(y = cullPrice, color = "PC RATIONAL")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(estObsPC$Year[1],estObsPC$Year[nrow(estObsPC)]))) +
  expand_limits(y = 0)


# allPrices <- Reduce(function(...) merge(...), list(pcs,pss,estPC,estPS))
# 
# allPrices %>% transmute(Year, ps, fedPrice, fedDiff = ps - fedPrice, pc, cullPrice, cullDiff = pc - cullPrice)
# 
# 
# estSL <- colMeans(slNew) %>% as.data.frame()
# names(estSL) <- "SL"
# estSL <- estSL %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())
# 
# estObsSL <- merge(estSL, quantities_prices_capK) %>% select(Year, SL, sl) %>% mutate(D = (SL - sl))
# 
# 
# estObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=SL, color="SL RATIONAL")) +
#   geom_point(aes(y = SL, color = "SL RATIONAL")) + geom_line(aes(y=sl, color = "SL OBS")) + 
#   geom_point(aes(y=sl, color = "SL OBS")) + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(estObsSL$Year[1],estObsSL$Year[nrow(estObsSL)]))) 


mutildes_agg <- colMeans(mu_Tildes) %>% as.data.frame()
names(mutildes_agg) <- "muTilde"
mutildes_agg <- mutildes_agg %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())

stildes_agg <- colMeans(s_Tildes) %>% as.data.frame()
names(stildes_agg) <- "sTilde"
stildes_agg <- stildes_agg %>% mutate(Year = quantities_prices_capK$Year+3) %>% select(Year, everything())

params <- merge(mutildes_agg, stildes_agg)

# Stock_temp <- Stock %>% filter(Year>=1994 & Year<=2017)
# imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
# exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)
# 
# test_df <- merge(merge(merge(Stock_temp, imports_temp), exports_temp), merge(supp_sl_adj, supp_cl_adj)) %>% 
#   select(Year, K, k3, k9, k8, k7, k6, Slaughter, Cull, Exports, Imports)
# 
# K1 <- 0
# K1 <- test_df %>% transmute(Year = Year + 1, K1 = g * (1+delta) * K - (delta * lag(K) - k3) - 
#                               (k9 + (1-delta) * k8 + (1-delta) * k7), K2 = 0.9941 * K)
# 
# merge(K1, test_df) %>% select(Year, K, K1, K2) %>% mutate(DIFF1 = K1 - K, DIFF2 = K2 - K)
# 
# 
# lm(K ~ -1 + lag(K), data = test_df)


# equilibriumCheck[[j]]
#             [,1]          [,2]          [,3]      [,4]      [,5]     [,6]
# [1,] 5.891271e-01  5.891314e-01  1.178258e+00 0.7286352 0.5181443 1.628173
# [2,] 1.550339e-01 -1.550339e-01 -1.596708e-08 0.7379252 0.4982973 1.651095
# [3,] 2.977878e-06 -2.509227e-06  4.686512e-07 0.7472152 0.5075868 1.674017


# equilibriumCheck[[j]]
#             [,1]          [,2]          [,3]      [,4]      [,5]     [,6]
# [1,] 5.891271e-01  5.891314e-01  1.178258e+00 0.7286352 0.5181443 1.628173
# [2,] 1.550339e-01 -1.550339e-01 -1.596708e-08 0.7379252 0.4982973 1.651095
# [3,] 2.337763e-06 -2.822381e-06 -4.846183e-07 0.7472152 0.5367238 1.674017


# equilibriumCheck[[j]]
#           [,1]          [,2]          [,3]      [,4]      [,5]     [,6]
# [1,] 5.891326e-01  5.891258e-01  1.178258e+00 0.7286352 0.5575041 1.628173
# [2,] 1.385547e-01 -1.385541e-01  5.836811e-07 0.7379252 0.5376571 1.651095
# [3,] 3.391118e-06 -3.655470e-06 -2.643519e-07 0.7472152 0.5469464 1.674017

# equilibriumCheck[[j]]
# [,1]          [,2]         [,3]      [,4]      [,5]     [,6]
# [1,] 5.891326e-01  5.891258e-01 1.178258e+00 0.7286352 0.5575041 1.628173
# [2,] 1.385547e-01 -1.385541e-01 5.836811e-07 0.7379252 0.5376571 1.651095
# [3,] 4.375354e-06 -3.527321e-06 8.480330e-07 0.7472152 0.5469464 1.674017


# mu_Tildes[,i] 
# [1] 2.170549 2.170549 2.170549 2.170549 2.170549
# [6] 2.170142 2.170142 2.170142 2.170142 2.170142
# [11] 2.160700 2.160700 2.160700 2.160700 2.160700
# [16] 2.309768 2.309768 2.309768 2.309768 2.309768
# [21] 1.644836 1.644836 1.644836 1.644836 1.644836
# [26] 2.170549 2.170549 2.170549 2.170549 2.170549
# [31] 2.170142 2.170142 2.170142 2.170142 2.170142
# [36] 2.160700 2.160700 2.160700 2.160700 2.160700
# [41] 2.309768 2.309768 2.309768 2.309768 2.309768
# [46] 1.644836 1.644836 1.644836 1.644836 1.644836
# [51] 2.170549 2.170549 2.170549 2.170549 2.170549
# [56] 2.170142 2.170142 2.170142 2.170142 2.170142
# [61] 2.160700 2.160700 2.160700 2.160700 2.160700
# [66] 2.309768 2.309768 2.309768 2.309768 2.309768
# [71] 1.644836 1.644836 1.644836 1.644836 1.644836
# [76] 2.170549 2.170549 2.170549 2.170549 2.170549
# [81] 2.170142 2.170142 2.170142 2.170142 2.170142
# [86] 2.160700 2.160700 2.160700 2.160700 2.160700
# [91] 2.309768 2.309768 2.309768 2.309768 2.309768
# [96] 1.644836 1.644836 1.644836 1.644836 1.644836
# [101] 2.170549 2.170549 2.170549 2.170549 2.170549
# [106] 2.170142 2.170142 2.170142 2.170142 2.170142
# [111] 2.160700 2.160700 2.160700 2.160700 2.160700
# [116] 2.309768 2.309768 2.309768 2.309768 2.309768
# [121] 1.644836 1.644836 1.644836 1.644836 1.644836

# s_Tildes[,i]
# [1] 0.7986686 0.7986686 0.7986686 0.7986686 0.7986686
# [6] 0.8159213 0.8159213 0.8159213 0.8159213 0.8159213
# [11] 0.8369502 0.8369502 0.8369502 0.8369502 0.8369502
# [16] 0.9351183 0.9351183 0.9351183 0.9351183 0.9351183
# [21] 0.6082816 0.6082816 0.6082816 0.6082816 0.6082816
# [26] 0.7986686 0.7986686 0.7986686 0.7986686 0.7986686
# [31] 0.8159213 0.8159213 0.8159213 0.8159213 0.8159213
# [36] 0.8369502 0.8369502 0.8369502 0.8369502 0.8369502
# [41] 0.9351183 0.9351183 0.9351183 0.9351183 0.9351183
# [46] 0.6082816 0.6082816 0.6082816 0.6082816 0.6082816
# [51] 0.7986686 0.7986686 0.7986686 0.7986686 0.7986686
# [56] 0.8159213 0.8159213 0.8159213 0.8159213 0.8159213
# [61] 0.8369502 0.8369502 0.8369502 0.8369502 0.8369502
# [66] 0.9351183 0.9351183 0.9351183 0.9351183 0.9351183
# [71] 0.6082816 0.6082816 0.6082816 0.6082816 0.6082816
# [76] 0.7986686 0.7986686 0.7986686 0.7986686 0.7986686
# [81] 0.8159213 0.8159213 0.8159213 0.8159213 0.8159213
# [86] 0.8369502 0.8369502 0.8369502 0.8369502 0.8369502
# [91] 0.9351183 0.9351183 0.9351183 0.9351183 0.9351183
# [96] 0.6082816 0.6082816 0.6082816 0.6082816 0.6082816
# [101] 0.7986686 0.7986686 0.7986686 0.7986686 0.7986686
# [106] 0.8159213 0.8159213 0.8159213 0.8159213 0.8159213
# [111] 0.8369502 0.8369502 0.8369502 0.8369502 0.8369502
# [116] 0.9351183 0.9351183 0.9351183 0.9351183 0.9351183
# [121] 0.6082816 0.6082816 0.6082816 0.6082816 0.6082816



########################################################################################

# Here I get the means and median of the first 25 nodes of the equilibrium prices and quantities

EQprices_ps_Means <- apply(prices_ps_eq[1:25,], 2, mean)
EQestPS <- EQprices_ps_Means %>% as.data.frame()
names(EQestPS) <- "psMean"
EQestPS <- EQestPS %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_ps_Medians <- apply(prices_ps_eq[1:25,], 2, median)
EQestPS_Medians <- EQprices_ps_Medians %>% as.data.frame()
names(EQestPS_Medians) <- "psMedian"
EQprices_ps_Medians <- EQestPS_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestPS <- merge(EQestPS, EQprices_ps_Medians)

## @knitr EQestObsPS

EQestObsPS <- left_join(EQestPS,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

EQestObsPS

## @knitr EQestObsPS_plot

EQestObsPS_plot <- EQestObsPS %>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsPS$Year[1],EQestObsPS$Year[nrow(EQestObsPS)]))) +
  expand_limits(y = 0.5)

EQestObsPS_plot

## @knitr EQestObsPC
EQprices_pc_Means <- apply(prices_pc_eq[1:25,], 2, mean)
EQestPC <- EQprices_pc_Means %>% as.data.frame()
names(EQestPC) <- "pcMean"
EQestPC <- EQestPC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_pc_Medians <- apply(prices_pc_eq[1:25,], 2, median)
EQestPC_Medians <- EQprices_pc_Medians %>% as.data.frame()
names(EQestPC_Medians) <- "pcMedian"
EQprices_pc_Medians <- EQestPC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestPC <- merge(EQestPC, EQprices_pc_Medians)

EQestObsPC <- left_join(EQestPC,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

EQestObsPC

## @knitr EQestObsPC_plot
EQestObsPC_plot <- EQestObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsPC$Year[1],EQestObsPC$Year[nrow(EQestObsPC)]))) +
  expand_limits(y = 0.5)

EQestObsPC_plot

## @knitr EQestObsSL
EQquantities_sl_Means <- apply(slNodes_eq[1:25,], 2, mean)
EQestSL <- EQquantities_sl_Means %>% as.data.frame()
names(EQestSL) <- "slMean"
EQestSL <- EQestSL %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQquantities_sl_Medians <- apply(slNodes_eq[1:25,], 2, median)
EQestSL_Medians <- EQquantities_sl_Medians %>% as.data.frame()
names(EQestSL_Medians) <- "slMedian"
EQestSL_Medians <- EQestSL_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestSL <- merge(EQestSL, EQestSL_Medians)

# EQestObsSL <- left_join(EQestSL,quantities_prices_capK) %>% select(Year, slMean, slMedian, slSM) %>% 
#   mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
# EQestObsSL


supp_sl_OBS <- supp_sl_adj %>% select(Year, slSM = Bill_meatLb_sl)

EQestObsSL <- left_join(EQestSL,supp_sl_OBS) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
EQestObsSL


## @knitr EQestObsSL_plot
EQestObsSL_plot <- EQestObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=slMean, color="FED PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = slMean, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
  geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + geom_line(aes(y=slMedian, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = slMedian, color = "FED PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsSL$Year[1],EQestObsSL$Year[nrow(EQestObsSL)]))) 
EQestObsSL_plot

## @knitr EQestObsCL
EQquantities_cl_Means <- apply(clNodes_eq[1:25,], 2, mean)
EQestCL <- EQquantities_cl_Means %>% as.data.frame()
names(EQestCL) <- "clMean"
EQestCL <- EQestCL %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQquantities_cl_Medians <- apply(clNodes_eq[1:25,], 2, median)
EQestCL_Medians <- EQquantities_cl_Medians %>% as.data.frame()
names(EQestCL_Medians) <- "clMedian"
EQestCL_Medians <- EQestCL_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestCL <- merge(EQestCL, EQestCL_Medians)

# EQestObsCL <- left_join(EQestCL,quantities_prices_capK) %>% select(Year, clMean, clMedian, clSM) %>% 
#   mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)


supp_cl_OBS <- supp_cl_adj %>% select(Year, clSM = Bill_meatLb_cl)
EQestObsCL <- left_join(EQestCL,supp_cl_OBS) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)


EQestObsCL

## @knitr EQestObsCL_plot
EQestObsCL_plot <- EQestObsCL %>% ggplot(aes(x=Year))+geom_line(aes(y=clMean, color="CULL PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = clMean, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
  geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + geom_line(aes(y=clMedian, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = clMedian, color = "CULL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsCL$Year[1],EQestObsCL$Year[nrow(EQestObsCL)])))

EQestObsCL_plot


#### Plotting the observed and total derived demand


EQestTS <- merge(EQestCL, EQestSL) %>% transmute(Year = Year, TSmean = slMean + clMean, 
                                                TSmedian = slMedian + clMedian)

# EQestObsA <- left_join(EQestA,A_quant) %>% select(Year, Amean, Amedian, A) %>% 
#   mutate(errMean = (A - Amean), errmedian = (A- Amedian)) %>% round(4)


totalSupply <- totalSupply_adj %>% transmute(Year = Year, TS = TotalSupply)

EQestObsTS <- left_join(EQestTS,totalSupply) %>% select(Year, TSmean, TSmedian, TS) %>% 
  mutate(errMean = (TS - TSmean), errmedian = (TS- TSmedian)) %>% round(4)

EQestObsTS_plot <- EQestObsTS %>% ggplot(aes(x=Year))+geom_line(aes(y=TSmean, color="TOTAL PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = TSmean, color = "TOTAL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=TS, color = "TOTAL PRODUCTION OBS")) + 
  geom_point(aes(y= TS, color = "TOTAL PRODUCTION OBS")) + geom_line(aes(y=TSmedian, color="TOTAL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = TSmedian, color = "TOTAL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(EQestObsA$Year[1],EQestObsA$Year[nrow(EQestObsA)])))

EQestObsTS_plot



EQcosts_hc_Means <- apply(prices_hc_eq[1:25,], 2, mean)
EQestHC <- EQcosts_hc_Means %>% as.data.frame()
names(EQestHC) <- "hcMean"
EQestHC <- EQestHC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQcosts_hc_Medians <- apply(prices_hc_eq[1:25,], 2, median)
EQestHC_Medians <- EQcosts_hc_Medians %>% as.data.frame()
names(EQestHC_Medians) <- "hcMedian"
EQestHC_Medians <- EQestHC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestHC <- merge(EQestHC, EQestHC_Medians)



###### Expected prices Equilibrium
EQprices_Eps_Means <- apply(expected_PS_eq[1:25,], 2, mean)
EQestEPS <- EQprices_Eps_Means %>% as.data.frame()
names(EQestEPS) <- "EpsMean"
EQestEPS <- EQestEPS %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Eps_Medians <- apply(expected_PS_eq[1:25,], 2, median)
EQestEPS_Medians <- EQprices_Eps_Medians %>% as.data.frame()
names(EQestEPS_Medians) <- "EpsMedian"
EQprices_Eps_Medians <- EQestEPS_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPS <- merge(EQestEPS, EQprices_Eps_Medians)


EQprices_Epc_Means <- apply(expected_PC_eq[1:25,], 2, mean)
EQestEPC <- EQprices_Epc_Means %>% as.data.frame()
names(EQestEPC) <- "EpcMean"
EQestEPC <- EQestEPC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Epc_Medians <- apply(expected_PC_eq[1:25,], 2, median)
EQestEPC_Medians <- EQprices_Epc_Medians %>% as.data.frame()
names(EQestEPC_Medians) <- "EpcMedian"
EQprices_Epc_Medians <- EQestEPC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPC <- merge(EQestEPC, EQprices_Epc_Medians)






## @knitr ITRestObsPS

ITRprices_ps_Means <- apply(prices_ps_itr[1:25,], 2, mean)
ITRestPS <- ITRprices_ps_Means %>% as.data.frame()
names(ITRestPS) <- "psMean"
ITRestPS <- ITRestPS %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_ps_Medians <- apply(prices_ps_itr[1:25,], 2, median)
ITRestPS_Medians <- ITRprices_ps_Medians %>% as.data.frame()
names(ITRestPS_Medians) <- "psMedian"
ITRprices_ps_Medians <- ITRestPS_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestPS <- merge(ITRestPS, ITRprices_ps_Medians)

ITRestObsPS <- left_join(ITRestPS,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

ITRestObsPS


## @knitr ITRestObsPS_plot
ITRestObsPS_plot <- ITRestObsPS %>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPS$Year[1],ITRestObsPS$Year[nrow(ITRestObsPS)]))) +
  expand_limits(y = 0.5)

ITRestObsPS_plot


## @knitr ITRestObsPC

ITRprices_pc_Means <- apply(prices_pc_itr[1:25,], 2, mean)
ITRestPC <- ITRprices_pc_Means %>% as.data.frame()
names(ITRestPC) <- "pcMean"
ITRestPC <- ITRestPC %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRprices_pc_Medians <- apply(prices_pc_itr[1:25,], 2, median)
ITRestPC_Medians <- ITRprices_pc_Medians %>% as.data.frame()
names(ITRestPC_Medians) <- "pcMedian"
ITRprices_pc_Medians <- ITRestPC_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestPC <- merge(ITRestPC, ITRprices_pc_Medians)

ITRestObsPC <- left_join(ITRestPC,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

ITRestObsPC


## @knitr ITRestObsPC_plot
ITRestObsPC_plot <- ITRestObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsPC$Year[1],ITRestObsPC$Year[nrow(ITRestObsPC)]))) +
  expand_limits(y = 0.5)
ITRestObsPC_plot


## @knitr ITRestObsSL
ITRquantities_sl_Means <- apply(slNodes_itr[1:25,], 2, mean)
ITRestSL <- ITRquantities_sl_Means %>% as.data.frame()
names(ITRestSL) <- "slMean"
ITRestSL <- ITRestSL %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRquantities_sl_Medians <- apply(slNodes_itr[1:25,], 2, median)
ITRestSL_Medians <- ITRquantities_sl_Medians %>% as.data.frame()
names(ITRestSL_Medians) <- "slMedian"
ITRestSL_Medians <- ITRestSL_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestSL <- merge(ITRestSL, ITRestSL_Medians)

ITRestObsSL <- left_join(ITRestSL,quantities_prices_capK) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)

ITRestObsSL

## @knitr ITRestObsSL_plot
ITRestObsSL_plot <- ITRestObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=slMean, color="FED PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = slMean, color = "FED PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=slSM, color = "FED PRODUCTION OBS")) + 
  geom_point(aes(y=slSM, color = "FED PRODUCTION OBS")) + geom_line(aes(y=slMedian, color="FED PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = slMedian, color = "FED PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsSL$Year[1],ITRestObsSL$Year[nrow(ITRestObsSL)]))) 
ITRestObsSL_plot

## @knitr ITRestObsCL
ITRquantities_cl_Means <- apply(clNodes_itr[1:25,], 2, mean)
ITRestCL <- ITRquantities_cl_Means %>% as.data.frame()
names(ITRestCL) <- "clMean"
ITRestCL <- ITRestCL %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRquantities_cl_Medians <- apply(clNodes_itr[1:25,], 2, median)
ITRestCL_Medians <- ITRquantities_cl_Medians %>% as.data.frame()
names(ITRestCL_Medians) <- "clMedian"
ITRestCL_Medians <- ITRestCL_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

ITRestCL <- merge(ITRestCL, ITRestCL_Medians)

ITRestObsCL <- left_join(ITRestCL,quantities_prices_capK) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)

ITRestObsCL

## @knitr ITRestObsCL_plot
ITRestObsCL_plot <- ITRestObsCL %>% ggplot(aes(x=Year))+geom_line(aes(y=clMean, color="CULL PRODUCTION RATIONAL (MEAN)")) +
  geom_point(aes(y = clMean, color = "CULL PRODUCTION RATIONAL (MEAN)")) + geom_line(aes(y=clSM, color = "CULL PRODUCTION OBS")) + 
  geom_point(aes(y=clSM, color = "CULL PRODUCTION OBS")) + geom_line(aes(y=clMedian, color="CULL PRODUCTION RATIONAL (MEDIAN)")) +
  geom_point(aes(y = clMedian, color = "CULL PRODUCTION RATIONAL (MEDIAN)"))  + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(ITRestObsCL$Year[1],ITRestObsCL$Year[nrow(ITRestObsCL)]))) 
ITRestObsCL_plot


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






mu_Tildes_Means <- apply(mu_Tildes_eq[1:25,], 2, mean)
mu_Tildes_Means <- mu_Tildes_Means %>% as.data.frame()
names(mu_Tildes_Means) <- "muMean"
mu_Tildes_Means <- mu_Tildes_Means %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_Medians <- apply(mu_Tildes_eq[1:25,], 2, median)
mu_Tildes_Medians <- mu_Tildes_Medians %>% as.data.frame()
names(mu_Tildes_Medians) <- "muMedian"
mu_Tildes_Medians <- mu_Tildes_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

mu_Tildes_MM <- merge(mu_Tildes_Means, mu_Tildes_Medians)


s_Tildes_Means <- apply(s_Tildes_eq[1:25,], 2, mean)
s_Tildes_Means <- s_Tildes_Means %>% as.data.frame()
names(s_Tildes_Means) <- "sMean"
s_Tildes_Means <- s_Tildes_Means %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_Medians <- apply(s_Tildes_eq[1:25,], 2, median)
s_Tildes_Medians <- s_Tildes_Medians %>% as.data.frame()
names(s_Tildes_Medians) <- "sMedian"
s_Tildes_Medians <- s_Tildes_Medians %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

s_Tildes_MM <- merge(s_Tildes_Means, s_Tildes_Medians)




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




