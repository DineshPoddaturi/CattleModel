#### The changes I made from the past code are
#### I keep the replacement heifers as it is i.e., do not change them 
#### to positive, instead they are whatever the function returns. 
#### I argue that negative sign indicates that the country imports those number of animals 
#### just to meet the demand.
#### Now I am making another assumption: If the supply of the fed cattle and cull cow doesn't meet
#### the subsistence levels the c ountry imports the exact number of animals just to meet the 
##### levels. This can be argued but in this simulation I make this needed assumption.

###### I do not make any assertion about how many more animals need to be imported to 
###### build back the stocks such that it meets the pre disease outbreak. That completely depends
###### on the nations interest on how agressively they want to build up the stocks

###### Optimistic scenario
# 1. 5% Decline in domestic demand for 1 year
# 2. In year 2 domestic demand goes back up
# 3. Exports banned for 2 years
# 4. In year 3 exports ban is lifted and everything is back to normal
#### All under equillibrium simulation continues

#### 20% depopulation
proj_Q_P_PostFMD_20_N <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_N <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_20_NI <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_20_NI  <- beefINV_FORECAST_PostFMD

### 50% depopulation
proj_Q_P_PostFMD_50_N <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_N <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_50_NI <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_50_NI <- beefINV_FORECAST_PostFMD

#### 90% depopulation
proj_Q_P_PostFMD_90_N <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_N <- beefINV_FORECAST_PostFMD

proj_Q_P_PostFMD_90_NI <- proj_Q_P_PostFMD
beefINV_FORECAST_PostFMD_90_NI <- beefINV_FORECAST_PostFMD

###################### Here I merge the optimistic case prices and stocks under all depopulation levels

proj_Q_P_PostFMD_20_OPN <- proj_Q_P_PostFMD_20_NI
proj_Q_P_PostFMD_20_OPN <- proj_Q_P_PostFMD_20_OPN %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                        Ps20 = Ps,
                                                                                        Pc20 = Pc)
proj_Q_P_PostFMD_50_OPN <- proj_Q_P_PostFMD_50_NI
proj_Q_P_PostFMD_50_OPN <- proj_Q_P_PostFMD_50_OPN %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                        Ps50 = Ps,
                                                                                        Pc50 = Pc)

proj_Q_P_PostFMD_90_OPN <- proj_Q_P_PostFMD_90_NI
proj_Q_P_PostFMD_90_OPN <- proj_Q_P_PostFMD_90_OPN %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                        Ps90 = Ps,
                                                                                        Pc90 = Pc)

optPriceListN <- list(proj_Q_P_PostFMD_20_OPN, proj_Q_P_PostFMD_50_OPN, proj_Q_P_PostFMD_90_OPN)
optPricePostFMDN <- Reduce(function(...) merge(...), optPriceListN)

optPricePostFMDNI <- Reduce(function(...) merge(...), optPriceListN)


beefINV_FORECAST_PostFMD_20_OPN <- beefINV_FORECAST_PostFMD_20_NI
beefINV_FORECAST_PostFMD_20_OPN <- beefINV_FORECAST_PostFMD_20_OPN %>% select(Year, K) %>% transmute(Year = Year,
                                                                                                   K20 = K)
beefINV_FORECAST_PostFMD_50_OPN <- beefINV_FORECAST_PostFMD_50_NI
beefINV_FORECAST_PostFMD_50_OPN <- beefINV_FORECAST_PostFMD_50_OPN %>% select(Year, K) %>% transmute(Year = Year,
                                                                                                   K50 = K)

beefINV_FORECAST_PostFMD_90_OPN <- beefINV_FORECAST_PostFMD_90_NI
beefINV_FORECAST_PostFMD_90_OPN <- beefINV_FORECAST_PostFMD_90_OPN %>% select(Year, K) %>% transmute(Year = Year,
                                                                                                   K90 = K)

optStockListN <- list(beefINV_FORECAST_PostFMD_20_OPN, beefINV_FORECAST_PostFMD_50_OPN, 
                      beefINV_FORECAST_PostFMD_90_OPN)
optStockPostFMDN <- Reduce(function(...) merge(...), optStockListN)

optStockPostFMDNI <- Reduce(function(...) merge(...), optStockListN)


#####  Baseline fitted prices ##########
### Here I use the fitted prices until 2017 and then projections until 2019
pricesBaseFit <- proj_AllDF_EQ %>% select(Year, psMedian, pcMedian) %>% 
  filter(Year > (optPricePostFMDN$Year[1] - 3)) %>% transmute(Year = Year, psB = psMedian, pcB = pcMedian)

pricesBaseProj <- PQs_MEDIANS %>% select(Year, Ps, Pc) %>% 
  filter(Year <= optPricePostFMDN$Year[nrow(optPricePostFMDN)]) %>% transmute(Year = Year, psB = Ps, pcB = Pc)

pricesBaseline <- rbind(pricesBaseFit, pricesBaseProj)

prices_Opt <- left_join(pricesBaseline,optPricePostFMDNI)

optPricesPlot_PS <- prices_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=psB, color="Baseline")) + 
  geom_point(aes(y=psB, color="Baseline")) + geom_line(aes(y=Ps20, color="20% depop")) + 
  geom_point(aes(y=Ps20, color="20% depop")) + geom_line(aes(y=Ps50, color="50% depop")) + 
  geom_point(aes(y=Ps50, color="50% depop")) + geom_line(aes(y=Ps90, color="90% depop")) + 
  geom_point(aes(y=Ps90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Opt$Year[1],
                                  prices_Opt$Year[nrow(prices_Opt)]))) 


optPricesPlot_PC <- prices_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=pcB, color="Baseline")) + 
  geom_point(aes(y=pcB, color="Baseline")) + geom_line(aes(y=Pc20, color="20% depop")) + 
  geom_point(aes(y=Pc20, color="20% depop")) + geom_line(aes(y=Pc50, color="50% depop")) + 
  geom_point(aes(y=Pc50, color="50% depop")) + geom_line(aes(y=Pc90, color="90% depop")) + 
  geom_point(aes(y=Pc90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Opt$Year[1],
                                  prices_Opt$Year[nrow(prices_Opt)]))) 


##### 
## Baseline stocks
obsKN <- Stock %>% select(Year, K) %>% 
  filter(Year > (optStockPostFMDN$Year[1]-3) & Year < (optStockPostFMDN$Year[nrow(optStockPostFMDN)]-1))

## Projected stocks
projKN <- beefINV_FORECAST %>% select(Year, Kcast) %>% filter(Year <= optStockPostFMDN$Year[nrow(optStockPostFMDN)]) %>%
  transmute(Year = Year, K = Kcast)

### Merged Baseline Stocks
KBaseline <- rbind(obsKN, projKN)

stocks_Opt <- left_join(KBaseline,optStockPostFMDNI)
stocks_Opt[,-1] <- stocks_Opt[,-1]/1000

optStockPlotN <- stocks_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=K, color="Baseline"))+ 
  geom_point(aes(y=K, color="Baseline")) + geom_line(aes(y=K20, color="20% DEPOP"))+ 
  geom_point(aes(y=K20, color="20% DEPOP")) + geom_line(aes(y=K50, color="50% DEPOP"))+ 
  geom_point(aes(y=K50, color="50% DEPOP")) + geom_line(aes(y=K90, color="90% DEPOP"))+ 
  geom_point(aes(y=K90, color="90% DEPOP")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_Opt$Year[1],
                                  stocks_Opt$Year[nrow(stocks_Opt)]))) 











