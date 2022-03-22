#####  Baseline fitted prices ##########
### Here I use the fitted prices until 2017 and then projections until 2019
pricesBaseFit <- proj_AllDF_EQ %>% select(Year, psMedian, pcMedian) %>% 
  filter(Year > (optPricePostFMDN$Year[1] - 3)) %>% transmute(Year = Year, 
                                                              psB = psMedian, pcB = pcMedian)

pricesBaseProj <- PQs_MEDIANS %>% select(Year, Ps, Pc) %>% 
  filter(Year <= optPricePostFMDN$Year[nrow(optPricePostFMDN)]) %>% transmute(Year = Year, 
                                                                              psB = Ps, pcB = Pc)

pricesBaseline <- rbind(pricesBaseFit, pricesBaseProj)

## Baseline stocks
obsKN <- Stock %>% select(Year, K) %>% 
  filter(Year > (optStockPostFMDN$Year[1]-3) & Year < (optStockPostFMDN$Year[nrow(optStockPostFMDN)]-1))

## Projected stocks
projKN <- beefINV_FORECAST %>% select(Year, Kcast) %>% filter(Year <= optStockPostFMDN$Year[nrow(optStockPostFMDN)]) %>%
  transmute(Year = Year, K = Kcast)

### Merged Baseline Stocks
KBaseline <- rbind(obsKN, projKN)


# Optimstic scenario

###################### Here I merge the optimistic case prices and stocks under all depopulation levels

postFMD_P_Q_20_OP <- postFMD_P_Q_20_Opt
postFMD_P_Q_20_OP <- postFMD_P_Q_20_OP %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                          Ps20 = Ps,
                                                                                          Pc20 = Pc)
postFMD_P_Q_50_OP <- postFMD_P_Q_50_Opt
postFMD_P_Q_50_OP <- postFMD_P_Q_50_OP %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                          Ps50 = Ps,
                                                                                          Pc50 = Pc)

postFMD_P_Q_90_OP <- postFMD_P_Q_90_Opt
postFMD_P_Q_90_OP <- postFMD_P_Q_90_OP %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                                          Ps90 = Ps,
                                                                                          Pc90 = Pc)


optPriceList <- list(postFMD_P_Q_20_OP, postFMD_P_Q_50_OP, postFMD_P_Q_90_OP)
optPricePostFMD <- Reduce(function(...) merge(...), optPriceList)

optPricePostFMD <- proj_Q_P_PostFMD_OP_absk3_1

postFMD_K_20_OP <- postFMD_K_20_Opt
postFMD_K_20_OP <- postFMD_K_20_OP %>% select(Year, K) %>% transmute(Year = Year, K20 = K)

postFMD_K_50_OP <- postFMD_K_50_Opt
postFMD_K_50_OP <- postFMD_K_50_OP %>% select(Year, K) %>% transmute(Year = Year, K50 = K)

postFMD_K_90_OP <- postFMD_K_90_Opt
postFMD_K_90_OP <- postFMD_K_90_OP %>% select(Year, K) %>% transmute(Year = Year, K90 = K)


optStockList <- list(postFMD_K_20_OP, postFMD_K_50_OP, 
                      postFMD_K_90_OP)
optStockPostFMD <- Reduce(function(...) merge(...), optStockList)

optStockPostFMD <- beefINV_FORECAST_PostFMD_OP_absk3_1

prices_Opt <- left_join(pricesBaseline, optPricePostFMD) %>% filter(Year <= optPricePostFMD$Year[nrow(optPricePostFMD)])

prices_Opt[,-1] <- prices_Opt[,-1] * 100

prices_Opt <- prices_Opt %>% round(3)

optPricesPlot_PS <- prices_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=psB, color="Baseline")) + 
  geom_point(aes(y=psB, color="Baseline")) + geom_line(aes(y=Ps20, color="20% depop")) + 
  geom_point(aes(y=Ps20, color="20% depop")) + geom_line(aes(y=Ps50, color="50% depop")) + 
  geom_point(aes(y=Ps50, color="50% depop")) + geom_line(aes(y=Ps90, color="90% depop")) + 
  geom_point(aes(y=Ps90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Opt$Year[1],
                                  prices_Opt$Year[nrow(prices_Opt)]))) + theme_classic() + 
  scale_y_continuous(name="Fed Cattle Price") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


optPricesPlot_PC <- prices_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=pcB, color="Baseline")) + 
  geom_point(aes(y=pcB, color="Baseline")) + geom_line(aes(y=Pc20, color="20% depop")) + 
  geom_point(aes(y=Pc20, color="20% depop")) + geom_line(aes(y=Pc50, color="50% depop")) + 
  geom_point(aes(y=Pc50, color="50% depop")) + geom_line(aes(y=Pc90, color="90% depop")) + 
  geom_point(aes(y=Pc90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Opt$Year[1],
                                  prices_Opt$Year[nrow(prices_Opt)])))+ theme_classic() +
  scale_y_continuous(name="Cull Cow Price") + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


##### 
stocks_Opt <- left_join(KBaseline, optStockPostFMD) %>% filter(Year < optStockPostFMD$Year[nrow(optStockPostFMD)])
stocks_Opt[,-1] <- stocks_Opt[,-1]/1000

optStockPlotN <- stocks_Opt %>% ggplot(aes(x=Year)) + geom_line(aes(y=K, color="Baseline"))+ 
  geom_point(aes(y=K, color="Baseline")) + geom_line(aes(y=K20, color="20% DEPOP"))+ 
  geom_point(aes(y=K20, color="20% DEPOP")) + geom_line(aes(y=K50, color="50% DEPOP"))+ 
  geom_point(aes(y=K50, color="50% DEPOP")) + geom_line(aes(y=K90, color="90% DEPOP"))+ 
  geom_point(aes(y=K90, color="90% DEPOP")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_Opt$Year[1],
                                  stocks_Opt$Year[nrow(stocks_Opt)]))) + theme_classic() +
  scale_y_continuous(name="Stocks") + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Pessimistic scenario

###################### Here I merge the pessimistic case prices and stocks under all depopulation levels

postFMD_P_Q_20_PE <- postFMD_P_Q_20_Pes
postFMD_P_Q_20_PE <- postFMD_P_Q_20_PE %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                              Ps20 = Ps,
                                                                              Pc20 = Pc)
postFMD_P_Q_50_PE <- postFMD_P_Q_50_Pes
postFMD_P_Q_50_PE <- postFMD_P_Q_50_PE %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                              Ps50 = Ps,
                                                                              Pc50 = Pc)

postFMD_P_Q_90_PE <- postFMD_P_Q_90_Pes
postFMD_P_Q_90_PE <- postFMD_P_Q_90_PE %>% select(Year, Ps, Pc) %>% transmute(Year = Year,
                                                                              Ps90 = Ps,
                                                                              Pc90 = Pc)

pesPriceList <- list(postFMD_P_Q_20_PE, postFMD_P_Q_50_PE, postFMD_P_Q_90_PE)
pesPricePostFMD <- Reduce(function(...) merge(...), pesPriceList)

pesPricePostFMD <- proj_Q_P_PostFMD_PE_absk3_1


postFMD_K_20_PE <- postFMD_K_20_Pes
postFMD_K_20_PE <- postFMD_K_20_PE %>% select(Year, K) %>% transmute(Year = Year, K20 = K)

postFMD_K_50_PE <- postFMD_K_50_Pes
postFMD_K_50_PE <- postFMD_K_50_PE %>% select(Year, K) %>% transmute(Year = Year, K50 = K)

postFMD_K_90_PE <- postFMD_K_90_Pes
postFMD_K_90_PE <- postFMD_K_90_PE %>% select(Year, K) %>% transmute(Year = Year, K90 = K)

pesStockList <- list(postFMD_K_20_PE, postFMD_K_50_PE, 
                     postFMD_K_90_PE)
pesStockPostFMD <- Reduce(function(...) merge(...), pesStockList)

pesStockPostFMD <- beefINV_FORECAST_PostFMD_PE_absk3_1


prices_Pes <- left_join(pricesBaseline, pesPricePostFMD) %>% filter(Year <= pesPricePostFMD$Year[nrow(pesPricePostFMD)])

prices_Pes[,-1] <- prices_Pes[,-1] * 100
prices_Pes <- prices_Pes %>% round(3)

pesPricesPlot_PS <- prices_Pes %>% ggplot(aes(x=Year)) + geom_line(aes(y=psB, color="Baseline")) + 
  geom_point(aes(y=psB, color="Baseline")) + geom_line(aes(y=Ps20, color="20% depop")) + 
  geom_point(aes(y=Ps20, color="20% depop")) + geom_line(aes(y=Ps50, color="50% depop")) + 
  geom_point(aes(y=Ps50, color="50% depop")) + geom_line(aes(y=Ps90, color="90% depop")) + 
  geom_point(aes(y=Ps90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Pes$Year[1],
                                  prices_Pes$Year[nrow(prices_Pes)]))) + theme_classic() + 
  scale_y_continuous(name="Fed Cattle Price") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


pesPricesPlot_PC <- prices_Pes %>% ggplot(aes(x=Year)) + geom_line(aes(y=pcB, color="Baseline")) + 
  geom_point(aes(y=pcB, color="Baseline")) + geom_line(aes(y=Pc20, color="20% depop")) + 
  geom_point(aes(y=Pc20, color="20% depop")) + geom_line(aes(y=Pc50, color="50% depop")) + 
  geom_point(aes(y=Pc50, color="50% depop")) + geom_line(aes(y=Pc90, color="90% depop")) + 
  geom_point(aes(y=Pc90, color="90% depop")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(prices_Pes$Year[1],
                                  prices_Pes$Year[nrow(prices_Pes)]))) + theme_classic() +
  scale_y_continuous(name="Cull Cow Price") + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



stocks_Pes <- left_join(KBaseline, pesStockPostFMD) %>% filter(Year < pesStockPostFMD$Year[nrow(pesStockPostFMD)])
stocks_Pes[,-1] <- stocks_Pes[,-1]/1000

pesStockPlotN <- stocks_Pes %>% ggplot(aes(x=Year)) + geom_line(aes(y=K, color="Baseline"))+ 
  geom_point(aes(y=K, color="Baseline")) + geom_line(aes(y=K20, color="20% DEPOP"))+ 
  geom_point(aes(y=K20, color="20% DEPOP")) + geom_line(aes(y=K50, color="50% DEPOP"))+ 
  geom_point(aes(y=K50, color="50% DEPOP")) + geom_line(aes(y=K90, color="90% DEPOP"))+ 
  geom_point(aes(y=K90, color="90% DEPOP")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_Pes$Year[1],
                                  stocks_Pes$Year[nrow(stocks_Pes)]))) + theme_classic() +
  scale_y_continuous(name="Stocks") + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))







