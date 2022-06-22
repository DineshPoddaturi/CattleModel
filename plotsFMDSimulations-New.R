

####### Optimistic Scenario

### Prices, Supplies and Quantity demanded
proj_Q_P_PostFMD_OPT_5_II_I <- proj_Q_P_PostFMD_OPT_5_I_I %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl,Cl5 = Cl,
                                                                      Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                      mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_OPT_10_II_I <- proj_Q_P_PostFMD_OPT_10_I_I %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                        Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                        mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter) 

proj_Q_P_PostFMD_OPT_20_II_I <- proj_Q_P_PostFMD_OPT_20_I_I %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
                                                                        Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
                                                                        mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)

##### Merging all data frames 
proj_Q_P_PostFMD_OPT_II_I <- Reduce(function(...) merge(...), 
                                  list(proj_Q_P_PostFMD_OPT_5_II_I, proj_Q_P_PostFMD_OPT_10_II_I, proj_Q_P_PostFMD_OPT_20_II_I))

##### Stocks
beefINV_FORECAST_PostFMD_OPT_5_II_I <- beefINV_FORECAST_PostFMD_OPT_5_I_I %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_OPT_10_II_I <- beefINV_FORECAST_PostFMD_OPT_10_I_I %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_OPT_20_II_I <- beefINV_FORECAST_PostFMD_OPT_20_I_I %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_OPT_II_I <- Reduce(function(...) merge(...), 
                                          list(beefINV_FORECAST_PostFMD_OPT_5_II_I, beefINV_FORECAST_PostFMD_OPT_10_II_I, 
                                               beefINV_FORECAST_PostFMD_OPT_20_II_I))


####### Pessimistic Scenario

### Prices, Supplies and Quantity demanded
proj_Q_P_PostFMD_PES_5_II_I <- proj_Q_P_PostFMD_PES_5_I_I %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl, Cl5 = Cl,
                                                                      Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                      mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_PES_10_II_I <- proj_Q_P_PostFMD_PES_10_I_I %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                        Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                        mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter) 

proj_Q_P_PostFMD_PES_20_II_I <- proj_Q_P_PostFMD_PES_20_I_I %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
                                                                        Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
                                                                        mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)

##### Merging all data frames 
proj_Q_P_PostFMD_PES_II_I <- Reduce(function(...) merge(...), 
                                  list(proj_Q_P_PostFMD_PES_5_II_I, proj_Q_P_PostFMD_PES_10_II_I, proj_Q_P_PostFMD_PES_20_II_I))

##### Stocks
beefINV_FORECAST_PostFMD_PES_5_II_I <- beefINV_FORECAST_PostFMD_PES_5_I_I %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_PES_10_II_I <- beefINV_FORECAST_PostFMD_PES_10_I_I %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_PES_20_II_I <- beefINV_FORECAST_PostFMD_PES_20_I_I %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_PES_II_I <- Reduce(function(...) merge(...), 
                                          list(beefINV_FORECAST_PostFMD_PES_5_II_I, beefINV_FORECAST_PostFMD_PES_10_II_I, 
                                               beefINV_FORECAST_PostFMD_PES_20_II_I))

#####################################################################################################################################
######################################################### OPTIMISTIC PLOTS ##########################################################
#####################################################################################################################################

proj_Q_P_PostFMD_OPT_II_I_PS_PC <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20)

proj_Q_P_PostFMD_OPT_II_I_PS_PC[,-1] <- proj_Q_P_PostFMD_OPT_II_I_PS_PC[,-1] * 100

EQ_PricesCosts_OPT <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian * 100, PcB = pcMedian * 100)

proj_Q_P_PostFMD_OPT_II_I_PS_PC_B <- merge(proj_Q_P_PostFMD_OPT_II_I_PS_PC, EQ_PricesCosts_OPT)

proj_Q_P_PostFMD_OPT_II_I_PS_B <- proj_Q_P_PostFMD_OPT_II_I_PS_PC_B %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_OPT_II_I_PC_B <- proj_Q_P_PostFMD_OPT_II_I_PS_PC_B %>% select(Year, PcB, Pc5, Pc10, Pc20)

#### The following has the percent change
proj_Q_P_PostFMD_OPT_II_I_PS_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_PS_B %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
            Ps20Percent =(((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPT_II_I_PC_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_PC_B %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
            Pc20Percent =(((Pc20-PcB)/PcB) * 100)) %>% round(3)

PostFMD_OPT_II_I_PS_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline")) +
  geom_point(aes(y = PsB, color = "Baseline")) +
  geom_line(aes(y = Ps5, color="5% Depop")) +
  geom_point(aes(y = Ps5, color = "5% Depop")) +
  geom_line(aes(y = Ps10, color="10% Depop")) +
  geom_point(aes(y = Ps10, color="10% Depop")) +
  geom_line(aes(y = Ps20, color="20% Depop")) +
  geom_point(aes(y = Ps20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_PS_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_PS_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_PS_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_PS_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_PS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Ps5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_PS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_PS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_PS_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")

PostFMD_OPT_II_I_PC_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_PC_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline")) +
  geom_point(aes(y = PcB, color = "Baseline")) +
  geom_line(aes(y = Pc5, color="5% Depop")) +
  geom_point(aes(y = Pc5, color = "5% Depop")) +
  geom_line(aes(y = Pc10, color="10% Depop")) +
  geom_point(aes(y = Pc10, color="10% Depop")) +
  geom_line(aes(y = Pc20, color="20% Depop")) +
  geom_point(aes(y = Pc20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_PC_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_PC_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_PC_B)])))+ 
  scale_y_continuous(name="Change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_PC_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_PC_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Pc5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_PC_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_PC_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_PC_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


####### Now I plot the stocks as well #####

stocks_Baseline <- Stock %>% filter(Year >= 2010) %>% transmute(Year, KB = K)
stocks_OPT_Baseline <- merge(beefINV_FORECAST_PostFMD_OPT_II_I, stocks_Baseline)

stocks_OPT_B_PercentChange <- stocks_OPT_Baseline %>% 
  transmute(Year, K5Percent = (((K5-KB)/KB) * 100), K10Percent = (((K10-KB)/KB) * 100),
            K20Percent = (((K20-KB)/KB) * 100)) %>% round(3)


stocks_OPT_Baseline[,-1] <- stocks_OPT_Baseline[,-1]/1000000
PostFMD_stocks_OPT_ChangePlot <- round(stocks_OPT_Baseline,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = KB, color="Baseline")) +
  geom_point(aes(y = KB, color = "Baseline")) +
  geom_line(aes(y = K5, color="5% Depop")) +
  geom_point(aes(y = K5, color = "5% Depop")) +
  geom_line(aes(y = K10, color="10% Depop")) +
  geom_point(aes(y = K10, color="10% Depop")) +
  geom_line(aes(y = K20, color="20% Depop")) +
  geom_point(aes(y = K20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_OPT_Baseline$Year[1],
                                  stocks_OPT_Baseline$Year[nrow(stocks_OPT_Baseline)])))+ 
  scale_y_continuous(name="Change in the stocks from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

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
  scale_y_continuous(name="Percent change in the stocks from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


######## Supplies

proj_Q_P_PostFMD_OPT_II_I_Sl_Cl <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20,
                                                                        Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

EQ_Supplies_OPT <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

proj_Q_P_PostFMD_OPT_II_I_Sl_Cl_B <- merge(proj_Q_P_PostFMD_OPT_II_I_Sl_Cl, EQ_Supplies_OPT)

proj_Q_P_PostFMD_OPT_II_I_Sl_B <- proj_Q_P_PostFMD_OPT_II_I_Sl_Cl_B %>% select(Year, SlB, Sl5, Sl10, Sl20,
                                                                               Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_OPT_II_I_Cl_B <- proj_Q_P_PostFMD_OPT_II_I_Sl_Cl_B %>% select(Year, ClB, Cl5, Cl10, Cl20,
                                                                               Cl5_OG, Cl10_OG, Cl20_OG)

#### The following has the percent change of supplies
proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5-SlB)/SlB) * 100), Sl10Percent = (((Sl10-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5-ClB)/ClB) * 100), Cl10Percent = (((Cl10-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20-ClB)/ClB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPT_II_I_Sl_OG_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5_OG-SlB)/SlB) * 100), Sl10Percent = (((Sl10_OG-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20_OG-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPT_II_I_Cl_OG_B_PercentChange <- proj_Q_P_PostFMD_OPT_II_I_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5_OG-ClB)/ClB) * 100), Cl10Percent = (((Cl10_OG-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20_OG-ClB)/ClB) * 100)) %>% round(3)


PostFMD_OPT_II_I_Sl_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5, color="5% Depop")) +
  geom_point(aes(y = Sl5, color = "5% Depop")) +
  geom_line(aes(y = Sl10, color="10% Depop")) +
  geom_point(aes(y = Sl10, color="10% Depop")) +
  geom_line(aes(y = Sl20, color="20% Depop")) +
  geom_point(aes(y = Sl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_Sl_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")

PostFMD_OPT_II_I_Cl_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5, color="5% Depop")) +
  geom_point(aes(y = Cl5, color = "5% Depop")) +
  geom_line(aes(y = Cl10, color="10% Depop")) +
  geom_point(aes(y = Cl10, color="10% Depop")) +
  geom_line(aes(y = Cl20, color="20% Depop")) +
  geom_point(aes(y = Cl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Cl_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_Cl_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop")) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop")) +
  geom_line(aes(y = Cl10Percent, color="10% Depop")) +
  geom_point(aes(y = Cl10Percent, color="10% Depop")) +
  geom_line(aes(y = Cl20Percent, color="20% Depop")) +
  geom_point(aes(y = Cl20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_OPT_II_I_Sl_OG_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5_OG, color="5% Depop")) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop")) +
  geom_line(aes(y = Sl10_OG, color="10% Depop")) +
  geom_point(aes(y = Sl10_OG, color="10% Depop")) +
  geom_line(aes(y = Sl20_OG, color="20% Depop")) +
  geom_point(aes(y = Sl20_OG, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_Sl_OG_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_Sl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")

PostFMD_OPT_II_I_Cl_OG_ChangePlot <- round(proj_Q_P_PostFMD_OPT_II_I_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5_OG, color="5% Depop")) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop")) +
  geom_line(aes(y = Cl10_OG, color="10% Depop")) +
  geom_point(aes(y = Cl10_OG, color="10% Depop")) +
  geom_line(aes(y = Cl20_OG, color="20% Depop")) +
  geom_point(aes(y = Cl20_OG, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Cl_B$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_OPT_II_I_Cl_OG_PercentChangePlot <- proj_Q_P_PostFMD_OPT_II_I_Cl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPT_II_I_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")



###### MuTildes and Shares
proj_Q_P_PostFMD_OPT_MU <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, mu5, mu10, mu20)
Eq_OPT_Mu <- mu_Tildes_MMNII %>% transmute(Year = Year, muB = muMedian)

proj_Q_P_PostFMD_OPT_MU_B <- merge(proj_Q_P_PostFMD_OPT_MU, Eq_OPT_Mu)

proj_Q_P_PostFMD_OPT_MU_PercentChange <- proj_Q_P_PostFMD_OPT_MU_B %>%
  transmute(Year, mu5Percent = (((mu5-muB)/muB) * 100), mu10Percent = (((mu10-muB)/muB) * 100),
            mu20Percent =(((mu20-muB)/muB) * 100)) %>% round(3)


proj_Q_P_PostFMD_OPT_SHR <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, sh5, sh10, sh20)
Eq_OPT_Sh <- sharesEq_Median %>% transmute(Year = Year, shB = shareMedian)

proj_Q_P_PostFMD_OPT_SHR_B <- merge(proj_Q_P_PostFMD_OPT_SHR, Eq_OPT_Sh)

proj_Q_P_PostFMD_OPT_SHR_PercentChange <- proj_Q_P_PostFMD_OPT_SHR_B %>%
  transmute(Year, sh5Percent = (((sh5-shB)/shB) * 100), sh10Percent = (((sh10-shB)/shB) * 100),
            sh20Percent =(((sh20-shB)/shB) * 100)) %>% round(3)


PostFMD_OPT_II_I_MU_ChangePlot <- round(proj_Q_P_PostFMD_OPT_MU_B,3) %>% ggplot(aes(x = Year))+
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

PostFMD_OPT_II_I_MU_PercentChangePlot <- proj_Q_P_PostFMD_OPT_MU_PercentChange %>% ggplot(aes(x = Year))+
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


PostFMD_OPT_II_I_SHR_ChangePlot <- round(proj_Q_P_PostFMD_OPT_SHR_B,3) %>% ggplot(aes(x = Year))+
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
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) 

PostFMD_OPT_II_I_SHR_PercentChangePlot <- proj_Q_P_PostFMD_OPT_SHR_PercentChange %>% ggplot(aes(x = Year))+
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

proj_Q_P_PostFMD_PES_II_I_PS_PC <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20)

proj_Q_P_PostFMD_PES_II_I_PS_PC[,-1] <- proj_Q_P_PostFMD_PES_II_I_PS_PC[,-1] * 100

EQ_PricesCosts_PES <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian * 100, PcB = pcMedian * 100)

proj_Q_P_PostFMD_PES_II_I_PS_PC_B <- merge(proj_Q_P_PostFMD_PES_II_I_PS_PC, EQ_PricesCosts_PES)

proj_Q_P_PostFMD_PES_II_I_PS_B <- proj_Q_P_PostFMD_PES_II_I_PS_PC_B %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_PES_II_I_PC_B <- proj_Q_P_PostFMD_PES_II_I_PS_PC_B %>% select(Year, PcB, Pc5, Pc10, Pc20)

#### The following has the percent change
proj_Q_P_PostFMD_PES_II_I_PS_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_PS_B %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
            Ps20Percent =(((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PES_II_I_PC_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_PC_B %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
            Pc20Percent =(((Pc20-PcB)/PcB) * 100)) %>% round(3)

PostFMD_PES_II_I_PS_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline")) +
  geom_point(aes(y = PsB, color = "Baseline")) +
  geom_line(aes(y = Ps5, color="5% Depop")) +
  geom_point(aes(y = Ps5, color = "5% Depop")) +
  geom_line(aes(y = Ps10, color="10% Depop")) +
  geom_point(aes(y = Ps10, color="10% Depop")) +
  geom_line(aes(y = Ps20, color="20% Depop")) +
  geom_point(aes(y = Ps20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_PS_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_PS_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_PS_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_PES_II_I_PS_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_PS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Ps5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_PS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_PS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_PS_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")

PostFMD_PES_II_I_PC_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_PC_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline")) +
  geom_point(aes(y = PcB, color = "Baseline")) +
  geom_line(aes(y = Pc5, color="5% Depop")) +
  geom_point(aes(y = Pc5, color = "5% Depop")) +
  geom_line(aes(y = Pc10, color="10% Depop")) +
  geom_point(aes(y = Pc10, color="10% Depop")) +
  geom_line(aes(y = Pc20, color="20% Depop")) +
  geom_point(aes(y = Pc20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_PC_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_PC_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_PC_B)])))+ 
  scale_y_continuous(name="Change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

PostFMD_PES_II_I_PC_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_PC_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Pc5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_PC_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_PC_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_PC_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow prices from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


####### Now I plot the stocks as well #####

stocks_Baseline <- Stock %>% filter(Year >= 2010) %>% transmute(Year, KB = K)
stocks_PES_Baseline <- merge(beefINV_FORECAST_PostFMD_PES_II_I, stocks_Baseline)

stocks_PES_B_PercentChange <- stocks_PES_Baseline %>% 
  transmute(Year, K5Percent = (((K5-KB)/KB) * 100), K10Percent = (((K10-KB)/KB) * 100),
            K20Percent = (((K20-KB)/KB) * 100)) %>% round(3)

stocks_PES_Baseline[,-1] <- stocks_PES_Baseline[,-1]/1000000
PostFMD_stocks_PES_ChangePlot <- round(stocks_PES_Baseline,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = KB, color="Baseline")) +
  geom_point(aes(y = KB, color = "Baseline")) +
  geom_line(aes(y = K5, color="5% Depop")) +
  geom_point(aes(y = K5, color = "5% Depop")) +
  geom_line(aes(y = K10, color="10% Depop")) +
  geom_point(aes(y = K10, color="10% Depop")) +
  geom_line(aes(y = K20, color="20% Depop")) +
  geom_point(aes(y = K20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(stocks_PES_Baseline$Year[1],
                                  stocks_PES_Baseline$Year[nrow(stocks_PES_Baseline)])))+ 
  scale_y_continuous(name="Change in the stocks from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

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
  scale_y_continuous(name="Percent change in the stocks from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


######## Supplies

proj_Q_P_PostFMD_PES_II_I_Sl_Cl <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20,
                                                                        Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

EQ_Supplies_PES <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

proj_Q_P_PostFMD_PES_II_I_Sl_Cl_B <- merge(proj_Q_P_PostFMD_PES_II_I_Sl_Cl, EQ_Supplies_PES)

proj_Q_P_PostFMD_PES_II_I_Sl_B <- proj_Q_P_PostFMD_PES_II_I_Sl_Cl_B %>% select(Year, SlB, Sl5, Sl10, Sl20,
                                                                               Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_PES_II_I_Cl_B <- proj_Q_P_PostFMD_PES_II_I_Sl_Cl_B %>% select(Year, ClB, Cl5, Cl10, Cl20,
                                                                               Cl5_OG, Cl10_OG, Cl20_OG)

#### The following has the percent change of supplies
proj_Q_P_PostFMD_PES_II_I_Sl_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5-SlB)/SlB) * 100), Sl10Percent = (((Sl10-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PES_II_I_Cl_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5-ClB)/ClB) * 100), Cl10Percent = (((Cl10-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20-ClB)/ClB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PES_II_I_Sl_OG_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_Sl_B %>%
  transmute(Year, Sl5Percent = (((Sl5_OG-SlB)/SlB) * 100), Sl10Percent = (((Sl10_OG-SlB)/SlB) * 100),
            Sl20Percent =(((Sl20_OG-SlB)/SlB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PES_II_I_Cl_OG_B_PercentChange <- proj_Q_P_PostFMD_PES_II_I_Cl_B %>%
  transmute(Year, Cl5Percent = (((Cl5_OG-ClB)/ClB) * 100), Cl10Percent = (((Cl10_OG-ClB)/ClB) * 100),
            Cl20Percent =(((Cl20_OG-ClB)/ClB) * 100)) %>% round(3)


PostFMD_PES_II_I_Sl_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5, color="5% Depop")) +
  geom_point(aes(y = Sl5, color = "5% Depop")) +
  geom_line(aes(y = Sl10, color="10% Depop")) +
  geom_point(aes(y = Sl10, color="10% Depop")) +
  geom_line(aes(y = Sl20, color="20% Depop")) +
  geom_point(aes(y = Sl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Sl_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PES_II_I_Sl_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_Sl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop")) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop")) +
  geom_line(aes(y = Sl10Percent, color="10% Depop")) +
  geom_point(aes(y = Sl10Percent, color="10% Depop")) +
  geom_line(aes(y = Sl20Percent, color="20% Depop")) +
  geom_point(aes(y = Sl20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Sl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Sl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Sl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_PES_II_I_Cl_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5, color="5% Depop")) +
  geom_point(aes(y = Cl5, color = "5% Depop")) +
  geom_line(aes(y = Cl10, color="10% Depop")) +
  geom_point(aes(y = Cl10, color="10% Depop")) +
  geom_line(aes(y = Cl20, color="20% Depop")) +
  geom_point(aes(y = Cl20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Cl_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PES_II_I_Cl_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_Cl_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop")) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop")) +
  geom_line(aes(y = Cl10Percent, color="10% Depop")) +
  geom_point(aes(y = Cl10Percent, color="10% Depop")) +
  geom_line(aes(y = Cl20Percent, color="20% Depop")) +
  geom_point(aes(y = Cl20Percent, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Cl_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Cl_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Cl_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")



PostFMD_PES_II_I_Sl_OG_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline")) +
  geom_point(aes(y = SlB, color = "Baseline")) +
  geom_line(aes(y = Sl5_OG, color="5% Depop")) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop")) +
  geom_line(aes(y = Sl10_OG, color="10% Depop")) +
  geom_point(aes(y = Sl10_OG, color="10% Depop")) +
  geom_line(aes(y = Sl20_OG, color="20% Depop")) +
  geom_point(aes(y = Sl20_OG, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Sl_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Sl_B)])))+ 
  scale_y_continuous(name="Change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PES_II_I_Sl_OG_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_Sl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Sl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Sl_OG_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Sl_OG_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Sl_OG_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the fed cattle supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


PostFMD_PES_II_I_Cl_OG_ChangePlot <- round(proj_Q_P_PostFMD_PES_II_I_Cl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline")) +
  geom_point(aes(y = ClB, color = "Baseline")) +
  geom_line(aes(y = Cl5_OG, color="5% Depop")) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop")) +
  geom_line(aes(y = Cl10_OG, color="10% Depop")) +
  geom_point(aes(y = Cl10_OG, color="10% Depop")) +
  geom_line(aes(y = Cl20_OG, color="20% Depop")) +
  geom_point(aes(y = Cl20_OG, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Cl_B$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Cl_B$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Cl_B)])))+ 
  scale_y_continuous(name="Change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PES_II_I_Cl_OG_PercentChangePlot <- proj_Q_P_PostFMD_PES_II_I_Cl_OG_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Cl5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_II_I_Cl_OG_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_PES_II_I_Cl_OG_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_PES_II_I_Cl_OG_B_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in the cull cow supply from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")


###### MuTildes and Shares
proj_Q_P_PostFMD_PES_MU <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, mu5, mu10, mu20)
Eq_PES_Mu <- mu_Tildes_MMNII %>% transmute(Year = Year, muB = muMedian)

proj_Q_P_PostFMD_PES_MU_B <- merge(proj_Q_P_PostFMD_PES_MU, Eq_PES_Mu)

proj_Q_P_PostFMD_PES_MU_PercentChange <- proj_Q_P_PostFMD_PES_MU_B %>%
  transmute(Year, mu5Percent = (((mu5-muB)/muB) * 100), mu10Percent = (((mu10-muB)/muB) * 100),
            mu20Percent =(((mu20-muB)/muB) * 100)) %>% round(3)


PostFMD_PES_II_I_MU_ChangePlot <- proj_Q_P_PostFMD_PES_MU_B %>% ggplot(aes(x = Year))+
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


PostFMD_PES_II_I_MU_PercentChangePlot <- proj_Q_P_PostFMD_PES_MU_PercentChange %>% ggplot(aes(x = Year))+
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


proj_Q_P_PostFMD_PES_SHR <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, sh5, sh10, sh20)
Eq_PES_Sh <- sharesEq_Median %>% transmute(Year = Year, shB = shareMedian)

proj_Q_P_PostFMD_PES_SHR_B <- merge(proj_Q_P_PostFMD_PES_SHR, Eq_PES_Sh) %>% round(3) 

proj_Q_P_PostFMD_PES_SHR_PercentChange <- proj_Q_P_PostFMD_PES_SHR_B %>%
  transmute(Year, sh5Percent = (((sh5-shB)/shB) * 100), sh10Percent = (((sh10-shB)/shB) * 100),
            sh20Percent =(((sh20-shB)/shB) * 100)) %>% round(3)


PostFMD_PES_II_I_SHR_ChangePlot <- proj_Q_P_PostFMD_PES_SHR_B %>% ggplot(aes(x = Year))+
  geom_line(aes(y = shB, color="Baseline")) +
  geom_point(aes(y = shB, color = "Baseline")) +
  geom_line(aes(y = sh5, color="5% Depop")) +
  geom_point(aes(y = sh5, color = "5% Depop")) +
  geom_line(aes(y = sh10, color="10% Depop")) +
  geom_point(aes(y = sh10, color="10% Depop")) +
  geom_line(aes(y = sh20, color="20% Depop")) +
  geom_point(aes(y = sh20, color="20% Depop")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PES_SHR_B$Year[1],
                                  proj_Q_P_PostFMD_PES_SHR_B$Year[nrow(proj_Q_P_PostFMD_PES_SHR_B)])))+ 
  scale_y_continuous(name="Change in the share from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


PostFMD_PES_II_I_SHR_PercentChangePlot <- proj_Q_P_PostFMD_PES_SHR_PercentChange %>% ggplot(aes(x = Year))+
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










