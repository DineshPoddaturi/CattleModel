

#### Computing producer surplus
#### The producer surplus is computed by taking the differences in revenue 
#### So basically, the revenue under the counterfactuals minus revenue under baseline

#### Revenue under baseline

pricesCosts_Baseline <- proj_Q_PIIVII %>% transmute(Year = Year, PsB = Ps, 
                                                    PcB = Pc , HcB = Hc * 100) %>% filter(PsB>0) %>% round(3)

quantities_Baseline <- proj_Q_PIIVII %>% transmute(Year, SlB = Sl, ClB = Cl)

#### Note: the quantities are in beef. Now I convert them to live animals. 
#### I will use the same denominator I used in my projections to convert into number of head

baseline_Weights <- mergedForecast_Proj %>% select(Year, Slaughter_avg, Cull_avg) %>% filter(Year >= 2022)

baseline_Weights <- baseline_Weights %>% mutate(Slaughter_live = Slaughter_avg/phi, Cull_live = Cull_avg/phi)

#### Converting the beef into the live animals

quantities_BaselineHead <- merge(quantities_Baseline, baseline_Weights) %>% transmute(Year = Year, 
                                                                                      slHead = (SlB * 1000000000/Slaughter_avg),
                                                                                      clHead = (ClB * 1000000000/Cull_avg),
                                                                                      SlB = SlB, ClB = ClB,
                                                                                      slLiveB = (slHead * Slaughter_live)/1000000000,
                                                                                      clLiveB = (clHead * Cull_live)/1000000000)

#### Computing the revenue. Note: This revenue is on the beef determined from the live cattle. 
#### But Do I need to convert it from beef to live animal to get the price?

revenue_BaselineLive <- merge(pricesCosts_Baseline, quantities_BaselineHead) %>% 
  transmute(Year = Year, slRevenueBeef = slLiveB * (PsB/100),
            clRevenueBeef = clLiveB * (PcB/100), totRevenue = slRevenueBeef + clRevenueBeef) %>% round(3)

revenue_BaselineLive %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totRevenue, color="Baseline"),size=1.1) +
  geom_point(aes(y = totRevenue, color = "Baseline"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Baseline$Year[1],
                                  revenue_Baseline$Year[nrow(revenue_Baseline)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline'),
                     values=c('Baseline' = '#619CFF'))

######## Revenue Optimistic Scenario

#### 5% Depopulation levels

suppliesSl_OPT5 <- merge(proj_Q_P_PostFMD_OPTI_Sl_B_08,baseline_Weights) %>% 
  select(Year, Sl5_OG, Sl5_DOM, Slaughter_avg, Slaughter_live) %>% round(3)

suppliesSl_OPT5 <- suppliesSl_OPT5 %>% mutate(slHead5_OG = (Sl5_OG * Slaughter_live/Slaughter_avg),
                                              slHead5_DOM = (Sl5_DOM * Slaughter_live/Slaughter_avg))

suppliesCl_OPT5 <- merge(proj_Q_P_PostFMD_OPTI_Cl_B_08,baseline_Weights)  %>% 
  select(Year, Cl5_OG, Cl5_DOM, Cull_avg, Cull_live) %>% round(3)

suppliesCl_OPT5 <- suppliesCl_OPT5 %>% mutate(clHead5_OG = (Cl5_OG * Cull_live/Cull_avg),
                                              clHead5_DOM = (Cl5_DOM * Cull_live/Cull_avg))

pricesPs_OPT5 <- proj_Q_P_PostFMD_OPTI_PS_B_08 %>% select(Year, Ps5) %>% round(3)

pricesPc_OPT5 <- proj_Q_P_PostFMD_OPTI_PC_B_08 %>% select(Year, Pc5) %>% round(3)

pricesHc_OPT5 <- proj_Q_P_PostFMD_OPT_5I_08 %>% transmute(Year = Year, Hc5 = (Hc5*100)) %>% round(3)

revenueSl_OPT5Live <- merge(suppliesSl_OPT5, pricesPs_OPT5) %>% transmute(Year = Year, Sl5_OG_Rev = slHead5_OG * (Ps5/100),
                                                                          Sl5_DOM_Rev = slHead5_DOM * (Ps5/100)) %>% round(3)

revenueCl_OPT5Live <- merge(suppliesCl_OPT5, pricesPc_OPT5) %>% transmute(Year = Year, 
                                                                          Cl5_OG_Rev = clHead5_OG * (Pc5/100),
                                                                          Cl5_DOM_Rev = clHead5_DOM * (Pc5/100)) %>% round(3)

##### Here I use a condition to add the total revenue according to the live weight
#### I add the domestic revenue (or quantities adjusting with exports) until the export ban is lifted
#### After that I add the original revenue. Because that's the original production.
exprtBan <- 2
revenue_Optimistic_Depop5LiveI <- merge(revenueSl_OPT5Live, revenueCl_OPT5Live) %>% 
  filter(Year <= Year[exprtBan]) %>% transmute(Year = Year, totRevenue = Sl5_DOM_Rev + Cl5_DOM_Rev)
revenue_Optimistic_Depop5LiveII <- merge(revenueSl_OPT5Live, revenueCl_OPT5Live) %>% 
  filter(Year > Year[exprtBan]) %>% transmute(Year = Year, totRevenue = Sl5_OG_Rev + Cl5_OG_Rev)

revenue_Optimistic_Depop5Live <- rbind(revenue_Optimistic_Depop5LiveI, revenue_Optimistic_Depop5LiveII)

revenue_Optimistic_Depop5Live %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totRevenue, color="5% depop"),size=1.1) +
  geom_point(aes(y = totRevenue, color = "5% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Optimistic_Depop5$Year[1],
                                  revenue_Optimistic_Depop5$Year[nrow(revenue_Optimistic_Depop5)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% depop'),
                     values=c('5% depop'='#F8766D'))

#### 10% Depopulation levels

suppliesSl_OPT10 <- merge(proj_Q_P_PostFMD_OPTI_Sl_B_08,baseline_Weights) %>% 
  select(Year, Sl10_OG, Sl10_DOM, Slaughter_avg, Slaughter_live) %>% round(3)

suppliesSl_OPT10 <- suppliesSl_OPT10 %>% mutate(slHead10_OG = (Sl10_OG * Slaughter_live/Slaughter_avg),
                                              slHead10_DOM = (Sl10_DOM * Slaughter_live/Slaughter_avg))

suppliesCl_OPT10 <- merge(proj_Q_P_PostFMD_OPTI_Cl_B_08,baseline_Weights)  %>% 
  select(Year, Cl10_OG, Cl10_DOM, Cull_avg, Cull_live) %>% round(3)

suppliesCl_OPT10 <- suppliesCl_OPT10 %>% mutate(clHead10_OG = (Cl10_OG * Cull_live/Cull_avg),
                                              clHead10_DOM = (Cl10_DOM * Cull_live/Cull_avg))

pricesPs_OPT10 <- proj_Q_P_PostFMD_OPTI_PS_B_08 %>% select(Year, Ps10) %>% round(3)

pricesPc_OPT10 <- proj_Q_P_PostFMD_OPTI_PC_B_08 %>% select(Year, Pc10) %>% round(3)

pricesHc_OPT10 <- proj_Q_P_PostFMD_OPT_10I_08 %>% transmute(Year = Year, Hc10 = (Hc10*100)) %>% round(3)

revenueSl_OPT10Live <- merge(suppliesSl_OPT10, pricesPs_OPT10) %>% transmute(Year = Year, Sl10_OG_Rev = slHead10_OG * (Ps10/100),
                                                                          Sl10_DOM_Rev = slHead10_DOM * (Ps10/100)) %>% round(3)

revenueCl_OPT10Live <- merge(suppliesCl_OPT10, pricesPc_OPT10) %>% transmute(Year = Year, 
                                                                          Cl10_OG_Rev = clHead10_OG * (Pc10/100),
                                                                          Cl10_DOM_Rev = clHead10_DOM * (Pc10/100)) %>% round(3)

##### Here I use a condition to add the total revenue according to the live weight
#### I add the domestic revenue (or quantities adjusting with exports) until the export ban is lifted
#### After that I add the original revenue. Because that's the original production.
exprtBan <- 2
revenue_Optimistic_Depop10LiveI <- merge(revenueSl_OPT10Live, revenueCl_OPT10Live) %>% 
  filter(Year <= Year[exprtBan]) %>% transmute(Year = Year, totRevenue = Sl10_DOM_Rev + Cl10_DOM_Rev)

revenue_Optimistic_Depop10LiveII <- merge(revenueSl_OPT10Live, revenueCl_OPT10Live) %>% 
  filter(Year > Year[exprtBan]) %>% transmute(Year = Year, totRevenue = Sl10_OG_Rev + Cl10_OG_Rev)

revenue_Optimistic_Depop10Live <- rbind(revenue_Optimistic_Depop10LiveI, revenue_Optimistic_Depop10LiveII)

revenue_Optimistic_Depop10Live %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totRevenue, color="10% depop"),size=1.1) +
  geom_point(aes(y = totRevenue, color = "10% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Optimistic_Depop5$Year[1],
                                  revenue_Optimistic_Depop5$Year[nrow(revenue_Optimistic_Depop5)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('10% depop'),
                     values=c('10% depop'='#C77CFF'))


####### Merging all the reveneue dataframes
revenue_BaselineMergeLive <- revenue_BaselineLive %>% transmute(Year = Year, totRevB = totRevenue)
revenue_Optimistic_Depop5MergeLive <- revenue_Optimistic_Depop5Live %>% transmute(Year = Year, totRev5 = totRevenue)
revenue_Optimistic_Depop10MergeLive <- revenue_Optimistic_Depop10Live %>% transmute(Year = Year, totRev10 = totRevenue)

revenue_Optimistic_MergeLive <- Reduce(function(...) merge(...), 
                                   list(revenue_BaselineMergeLive, revenue_Optimistic_Depop5MergeLive, 
                                        revenue_Optimistic_Depop10MergeLive))

revenue_Optimistic_MergeLive %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y = totRevB, color="Baseline"),size=1.1) +
  geom_point(aes(y = totRevB, color = "Baseline"),size=2) +
  geom_line(aes(y = totRev5, color="5% depop"),size=1.1) +
  geom_point(aes(y = totRev5, color = "5% depop"),size=2) +
  geom_line(aes(y = totRev10, color="10% depop"),size=1.1) +
  geom_point(aes(y = totRev10, color = "10% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Optimistic_MergeLive$Year[1],
                                  revenue_Optimistic_MergeLive$Year[nrow(revenue_Optimistic_MergeLive)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', '5% depop', '10% depop'),
                     values=c('Baseline' ='#619CFF' , '5% depop' = '#F8766D',
                              '10% depop'='#C77CFF'))


revenueDifference_OptimisticLive <- revenue_Optimistic_MergeLive %>% 
  transmute(Year = Year, revDiff5 = totRev5 - totRevB, revDiff10 = totRev10 - totRevB)

revenueLost_Optimistic_Cumulative5Live <- sum(revenueDifference_OptimisticLive$revDiff5)

revenueLost_Optimistic_Cumulative10Live <- sum(revenueDifference_OptimisticLive$revDiff10)


######## Revenue Pessimistic Scenario

#### 5% Depopulation levels

suppliesSl_PES5 <- merge(proj_Q_P_PostFMD_PESI_Sl_B_08, baseline_Weights) %>% 
  select(Year, Sl5_OG, Sl5_DOM, Slaughter_avg, Slaughter_live) %>% round(3)

suppliesSl_PES5 <- suppliesSl_PES5 %>% mutate(slHead5_OG = (Sl5_OG * Slaughter_live/Slaughter_avg),
                                              slHead5_DOM = (Sl5_DOM * Slaughter_live/Slaughter_avg))

suppliesCl_PES5 <- merge(proj_Q_P_PostFMD_PESI_Cl_B_08,baseline_Weights) %>% 
  select(Year, Cl5_OG, Cl5_DOM, Cull_avg, Cull_live) %>% round(3)

suppliesCl_PES5 <- suppliesCl_PES5 %>% mutate(clHead5_OG = (Cl5_OG * Cull_live/Cull_avg),
                                              clHead5_DOM = (Cl5_DOM * Cull_live/Cull_avg))

pricesPs_PES5 <- proj_Q_P_PostFMD_PESI_PS_B_08 %>% select(Year, Ps5) %>% round(3)

pricesPc_PES5 <- proj_Q_P_PostFMD_PESI_PC_B_08 %>% select(Year, Pc5) %>% round(3)

pricesHc_PES5 <- proj_Q_P_PostFMD_PES_5I_08 %>% transmute(Year = Year, Hc5 = (Hc5*100)) %>% round(3)

revenueSl_PES5Live <- merge(suppliesSl_PES5, pricesPs_PES5) %>% transmute(Year = Year, Sl5_OG_Rev = slHead5_OG * (Ps5/100),
                                                                             Sl5_DOM_Rev = slHead5_DOM * (Ps5/100)) %>% round(3)

revenueCl_PES5Live <- merge(suppliesCl_PES5, pricesPc_PES5) %>% transmute(Year = Year, 
                                                                             Cl5_OG_Rev = clHead5_OG * (Pc5/100),
                                                                             Cl5_DOM_Rev = clHead5_DOM * (Pc5/100)) %>% round(3)

##### Here I use a condition to add the total revenue according to the live weight
#### I add the domestic revenue (or quantities adjusting with exports) until the export ban is lifted
#### After that I add the original revenue. Because that's the original production.
exprtBanPES <- 5

revenue_Pessimistic_Depop5LiveI <- merge(revenueSl_PES5Live, revenueCl_PES5Live) %>% 
  filter(Year <= Year[exprtBanPES]) %>% transmute(Year = Year, totRevenue = Sl5_DOM_Rev + Cl5_DOM_Rev)

revenue_Pessimistic_Depop5LiveII <- merge(revenueSl_PES5Live, revenueCl_PES5Live) %>% 
  filter(Year > Year[exprtBanPES]) %>% transmute(Year = Year, totRevenue = Sl5_DOM_Rev + Cl5_DOM_Rev)

revenue_Pessimistic_Depop5Live <- rbind(revenue_Pessimistic_Depop5LiveI, revenue_Pessimistic_Depop5LiveII)

revenue_Pessimistic_Depop5Live %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totRevenue, color="5% depop"),size=1.1) +
  geom_point(aes(y = totRevenue, color = "5% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Pessimistic_Depop5Live$Year[1],
                                  revenue_Pessimistic_Depop5Live$Year[nrow(revenue_Pessimistic_Depop5Live)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% depop'),
                     values=c('5% depop'='#F8766D'))



#### 10% Depopulation levels

suppliesSl_PES10 <- merge(proj_Q_P_PostFMD_PESI_Sl_B_08, baseline_Weights) %>% 
  select(Year, Sl10_OG, Sl10_DOM, Slaughter_avg, Slaughter_live) %>% round(3)

suppliesSl_PES10 <- suppliesSl_PES10 %>% mutate(slHead10_OG = (Sl10_OG * Slaughter_live/Slaughter_avg),
                                              slHead10_DOM = (Sl10_DOM * Slaughter_live/Slaughter_avg))

suppliesCl_PES10 <- merge(proj_Q_P_PostFMD_PESI_Cl_B_08,baseline_Weights) %>% 
  select(Year, Cl10_OG, Cl10_DOM, Cull_avg, Cull_live) %>% round(3)

suppliesCl_PES10 <- suppliesCl_PES10 %>% mutate(clHead10_OG = (Cl10_OG * Cull_live/Cull_avg),
                                              clHead10_DOM = (Cl10_DOM * Cull_live/Cull_avg))

pricesPs_PES10 <- proj_Q_P_PostFMD_PESI_PS_B_08 %>% select(Year, Ps10) %>% round(3)

pricesPc_PES10 <- proj_Q_P_PostFMD_PESI_PC_B_08 %>% select(Year, Pc10) %>% round(3)

pricesHc_PES10 <- proj_Q_P_PostFMD_PES_10I_08 %>% transmute(Year = Year, Hc10 = (Hc10*100)) %>% round(3)

revenueSl_PES10Live <- merge(suppliesSl_PES10, pricesPs_PES10) %>% transmute(Year = Year, Sl10_OG_Rev = slHead10_OG * (Ps10/100),
                                                                          Sl10_DOM_Rev = slHead10_DOM * (Ps10/100)) %>% round(3)

revenueCl_PES10Live <- merge(suppliesCl_PES10, pricesPc_PES10) %>% transmute(Year = Year, 
                                                                          Cl10_OG_Rev = clHead10_OG * (Pc10/100),
                                                                          Cl10_DOM_Rev = clHead10_DOM * (Pc10/100)) %>% round(3)

##### Here I use a condition to add the total revenue according to the live weight
#### I add the domestic revenue (or quantities adjusting with exports) until the export ban is lifted
#### After that I add the original revenue. Because that's the original production.
exprtBanPES <- 5

revenue_Pessimistic_Depop10LiveI <- merge(revenueSl_PES10Live, revenueCl_PES10Live) %>% 
  filter(Year <= Year[exprtBanPES]) %>% transmute(Year = Year, totRevenue = Sl10_DOM_Rev + Cl10_DOM_Rev)

revenue_Pessimistic_Depop10LiveII <- merge(revenueSl_PES10Live, revenueCl_PES10Live) %>% 
  filter(Year > Year[exprtBanPES]) %>% transmute(Year = Year, totRevenue = Sl10_DOM_Rev + Cl10_DOM_Rev)

revenue_Pessimistic_Depop10Live <- rbind(revenue_Pessimistic_Depop10LiveI, revenue_Pessimistic_Depop10LiveII)

revenue_Pessimistic_Depop10Live %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totRevenue, color="10% depop"),size=1.1) +
  geom_point(aes(y = totRevenue, color = "10% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Pessimistic_Depop10Live$Year[1],
                                  revenue_Pessimistic_Depop10Live$Year[nrow(revenue_Pessimistic_Depop10Live)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('10% depop'),
                     values=c('10% depop'='#C77CFF'))

####### Merging all the reveneue dataframes
revenue_BaselineMergeLive <- revenue_BaselineLive %>% transmute(Year = Year, totRevB = totRevenue)
revenue_Pessimistic_Depop5MergeLive <- revenue_Pessimistic_Depop5Live %>% transmute(Year = Year, totRev5 = totRevenue)
revenue_Pessimistic_Depop10MergeLive <- revenue_Pessimistic_Depop10Live %>% transmute(Year = Year, totRev10 = totRevenue)

revenue_Pessimistic_MergeLive <- Reduce(function(...) merge(...), 
                                       list(revenue_BaselineMergeLive, revenue_Pessimistic_Depop5MergeLive, 
                                            revenue_Pessimistic_Depop10MergeLive))

revenue_Pessimistic_MergeLive %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y = totRevB, color="Baseline"),size=1.1) +
  geom_point(aes(y = totRevB, color = "Baseline"),size=2) +
  geom_line(aes(y = totRev5, color="5% depop"),size=1.1) +
  geom_point(aes(y = totRev5, color = "5% depop"),size=2) +
  geom_line(aes(y = totRev10, color="10% depop"),size=1.1) +
  geom_point(aes(y = totRev10, color = "10% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Pessimistic_MergeLive$Year[1],
                                  revenue_Pessimistic_MergeLive$Year[nrow(revenue_Pessimistic_MergeLive)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', '5% depop', '10% depop'),
                     values=c('Baseline' ='#619CFF' , '5% depop' = '#F8766D',
                              '10% depop'='#C77CFF'))


revenueDifference_PessimisticLive <- revenue_Pessimistic_MergeLive %>% 
  transmute(Year = Year, revDiff5 = totRev5 - totRevB, revDiff10 = totRev10 - totRevB)

revenueLost_Pessimistic_Cumulative5Live <- sum(revenueDifference_PessimisticLive$revDiff5)

revenueLost_Pessimistic_Cumulative10Live <- sum(revenueDifference_PessimisticLive$revDiff10)


##################### CONSUMER SURPLUS #######################

#### The consumer surplus is computed by taking difference between the baseline expenditure and the counterfactual
#### expenditure for both fed cattle and cull cow market separately

#### The total consumer surplus is simply sum of the consumer surplus for both fed cattle and cull cow markets

csBaseline_FedBeef <- proj_Q_PIIVII %>% transmute(Year = Year, fedBeef = A * sh, 
                                                  psR = (Ps/100) * (1/phi),
                                                  fedBeefExp = fedBeef * psR,
                                                  slExp = Sl * psR) %>% round(3)

csBaseline_CullBeef <- proj_Q_PIIVII %>% transmute(Year = Year, cullBeef = A * (1-sh), 
                                                   pcR = (Pc/100) * (1/phi),
                                                   cullBeefExp = cullBeef * pcR,
                                                   clExp = Cl * pcR) %>% round(3)

csBaseline_TotBeef <- merge(csBaseline_FedBeef, csBaseline_CullBeef) %>% 
  transmute(Year = Year, totBeefExp = fedBeefExp + cullBeefExp, totBeefExpDom = slExp + clExp)


csBaseline_TotBeef %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totBeefExp, color="Baseline"),size=1.1) +
  geom_point(aes(y = totBeefExp, color = "Baseline"),size=2) +
  geom_line(aes(y = totBeefExpDom, color="BaselineDom"),size=1.1) +
  geom_point(aes(y = totBeefExpDom, color = "BaselineDom"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(csBaseline_TotBeef$Year[1],
                                  csBaseline_TotBeef$Year[nrow(csBaseline_TotBeef)])))+ 
  scale_y_continuous(name="Expenditure", limits = c(50,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', 'BaselineDom'),
                     values=c('Baseline' = '#C77CFF', 'BaselineDom' = '#00BA38'))


###### Optimistic Scenario #####

### 5% depopulation

csOPT5_FedBeef <- proj_Q_P_PostFMD_OPT_5_09 %>% transmute(Year = Year, fedBeef5 = A * sh, 
                                                          psR5 = Ps * (1/phi),
                                                          fedBeefExp5 = fedBeef5 * psR5,
                                                          slExp5 = Sl * psR5) %>% round(3)

csOPT5_CullBeef <- proj_Q_P_PostFMD_OPT_5_09 %>% transmute(Year = Year, cullBeef5 = A * (1-sh), 
                                                           pcR5 = Pc * (1/phi),
                                                           cullBeefExp5 = cullBeef5 * pcR5,
                                                           clExp5 = Cl * pcR5) %>% round(3)

csOPT5_TotBeef <- merge(csOPT5_FedBeef, csOPT5_CullBeef) %>% 
  transmute(Year = Year, totBeefExp5 = fedBeefExp5 + cullBeefExp5, totBeefExpDom5 = slExp5 + clExp5)

csOPT5_TotBeef %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totBeefExp5, color="5% depop - With share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExp5, color = "5% depop - With share adjustment"),size=2) +
  geom_line(aes(y = totBeefExpDom5, color="5% depop - Without share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExpDom5, color = "5% depop - Without share adjustment"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(csOPT5_TotBeef$Year[1],
                                  csOPT5_TotBeef$Year[nrow(csOPT5_TotBeef)])))+ 
  scale_y_continuous(name="Expenditure (billion $)", limits = c(40,65,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% depop - With share adjustment', '5% depop - Without share adjustment'),
                     values=c('5% depop - With share adjustment'='#619CFF', 
                              '5% depop - Without share adjustment'='#F8766D'))

### 10% depopulation

csOPT10_FedBeef <- proj_Q_P_PostFMD_OPT_10_09 %>% transmute(Year = Year, fedBeef10 = A * sh, 
                                                            psR10 = Ps * (1/phi),
                                                            fedBeefExp10 = fedBeef10 * psR10,
                                                            slExp10 = Sl * psR10) %>% round(3)

csOPT10_CullBeef <- proj_Q_P_PostFMD_OPT_10_09 %>% transmute(Year = Year, cullBeef10 = A * (1-sh), 
                                                             pcR10 = Pc * (1/phi),
                                                             cullBeefExp10 = cullBeef10 * pcR10,
                                                             clExp10 = Cl * pcR10) %>% round(3)

csOPT10_TotBeef <- merge(csOPT10_FedBeef, csOPT10_CullBeef) %>% 
  transmute(Year = Year, totBeefExp10 = fedBeefExp10 + cullBeefExp10, totBeefExpDom10 = slExp10 + clExp10)

csOPT10_TotBeef %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totBeefExp10, color="10% depop - With share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExp10, color = "10% depop - With share adjustment"),size=2) +
  geom_line(aes(y = totBeefExpDom10, color="10% depop - Without share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExpDom10, color = "10% depop - Without share adjustment"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(csOPT10_TotBeef$Year[1],
                                  csOPT10_TotBeef$Year[nrow(csOPT10_TotBeef)])))+ 
  scale_y_continuous(name="Expenditure (billion $)", limits = c(40,65,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('10% depop - With share adjustment', '10% depop - Without share adjustment'),
                     values=c('10% depop - With share adjustment'='#619CFF', 
                              '10% depop - Without share adjustment'='#F8766D'))

####### Merging all the expenditure dataframes
csBaseline_TotBeefMerge <- csBaseline_TotBeef %>% transmute(Year = Year, totExpB = totBeefExpDom)
csOPT5_TotBeefMerge <- csOPT5_TotBeef %>% transmute(Year = Year, totExpDOM5 = totBeefExpDom5)
csOPT10_TotBeefMerge <- csOPT10_TotBeef %>% transmute(Year = Year, totExpDOM10 = totBeefExpDom10)

cs_Optimistic_Merge <- Reduce(function(...) merge(...), 
                              list(csBaseline_TotBeefMerge, csOPT5_TotBeefMerge, csOPT10_TotBeefMerge))


cs_Optimistic_Merge %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y = totExpB, color="Baseline"),size=1.1) +
  geom_point(aes(y = totExpB, color = "Baseline"),size=2) +
  geom_line(aes(y = totExpDOM5, color="5% depop "),size=1.1) +
  geom_point(aes(y = totExpDOM5, color = "5% depop"),size=2) +
  geom_line(aes(y = totExpDOM10, color="5% depop - With adjustment of trade"),size=1.1) +
  geom_point(aes(y = totExpDOM10, color = "5% depop - With adjustment of trade"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Optimistic_Merge$Year[1],
                                  revenue_Optimistic_Merge$Year[nrow(revenue_Optimistic_Merge)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline',
                              '5% depop - Without adjustment of trade', '5% depop - With adjustment of trade',
                              '10% depop - Without adjustment of trade', '10% depop - With adjustment of trade'),
                     values=c('Baseline' ='#C77CFF' , '5% depop - Without adjustment of trade' = '#CC3366',
                              '5% depop - With adjustment of trade' = '#CC6600',
                              '10% depop - Without adjustment of trade'='#619CFF', 
                              '10% depop - With adjustment of trade'='#F8766D'))

cs_Optimistic_Merge

expenditureDifference_Optimistic <- cs_Optimistic_Merge %>% 
  transmute(Year = Year, expDiff5 = totExpB - totExpDOM5, expDiff10 = totExpB - totExpDOM10)

cs_Optimistic_Cumulative5 <- sum(expenditureDifference_Optimistic$expDiff5)

cs_Optimistic_Cumulative10 <- sum(expenditureDifference_Optimistic$expDiff10)


###### Pessimistic Scenario #####

### 5% depopulation

csPES5_FedBeef <- proj_Q_P_PostFMD_PES_5_08 %>% transmute(Year = Year, fedBeef5 = A * sh, 
                                                          psR5 = Ps * (1/phi),
                                                          fedBeefExp5 = fedBeef5 * psR5,
                                                          slExp5 = Sl * psR5) %>% round(3)

csPES5_CullBeef <- proj_Q_P_PostFMD_PES_5_08 %>% transmute(Year = Year, cullBeef5 = A * (1-sh), 
                                                           pcR5 = Pc * (1/phi),
                                                           cullBeefExp5 = cullBeef5 * pcR5,
                                                           clExp5 = Cl * pcR5) %>% round(3)

csPES5_TotBeef <- merge(csPES5_FedBeef, csPES5_CullBeef) %>% 
  transmute(Year = Year, totBeefExp5 = fedBeefExp5 + cullBeefExp5, totBeefExpDom5 = slExp5 + clExp5)

csPES5_TotBeef %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totBeefExp5, color="5% depop - With share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExp5, color = "5% depop - With share adjustment"),size=2) +
  geom_line(aes(y = totBeefExpDom5, color="5% depop - Without share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExpDom5, color = "5% depop - Without share adjustment"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(csOPT5_TotBeef$Year[1],
                                  csOPT5_TotBeef$Year[nrow(csOPT5_TotBeef)])))+ 
  scale_y_continuous(name="Expenditure (billion $)", limits = c(40,65,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% depop - With share adjustment', '5% depop - Without share adjustment'),
                     values=c('5% depop - With share adjustment'='#619CFF', 
                              '5% depop - Without share adjustment'='#F8766D'))

### 10% depopulation

csPES10_FedBeef <- proj_Q_P_PostFMD_PES_10_08 %>% transmute(Year = Year, fedBeef10 = A * sh, 
                                                            psR10 = Ps * (1/phi),
                                                            fedBeefExp10 = fedBeef10 * psR10,
                                                            slExp10 = Sl * psR10) %>% round(3)

csPES10_CullBeef <- proj_Q_P_PostFMD_PES_10_08 %>% transmute(Year = Year, cullBeef10 = A * (1-sh), 
                                                             pcR10 = Pc * (1/phi),
                                                             cullBeefExp10 = cullBeef10 * pcR10,
                                                             clExp10 = Cl * pcR10) %>% round(3)

csPES10_TotBeef <- merge(csPES10_FedBeef, csPES10_CullBeef) %>% 
  transmute(Year = Year, totBeefExp10 = fedBeefExp10 + cullBeefExp10, totBeefExpDom10 = slExp10 + clExp10)

csPES10_TotBeef %>% ggplot(aes(x = Year))+
  geom_line(aes(y = totBeefExp10, color="10% depop - With share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExp10, color = "10% depop - With share adjustment"),size=2) +
  geom_line(aes(y = totBeefExpDom10, color="10% depop - Without share adjustment"),size=1.1) +
  geom_point(aes(y = totBeefExpDom10, color = "10% depop - Without share adjustment"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(csOPT5_TotBeef$Year[1],
                                  csOPT5_TotBeef$Year[nrow(csOPT5_TotBeef)])))+ 
  scale_y_continuous(name="Expenditure (billion $)", limits = c(40,65,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('10% depop - With share adjustment', '5% depop - Without share adjustment'),
                     values=c('10% depop - With share adjustment'='#619CFF', 
                              '10% depop - Without share adjustment'='#F8766D'))


####### Merging all the expenditure dataframes
csBaseline_TotBeefMerge <- csBaseline_TotBeef %>% transmute(Year = Year, totExpB = totBeefExpDom)
csPES5_TotBeefMerge <- csPES5_TotBeef %>% transmute(Year = Year, totExpDOM5 = totBeefExpDom5)
csPES10_TotBeefMerge <- csPES10_TotBeef %>% transmute(Year = Year, totExpDOM10 = totBeefExpDom10)

cs_Pessimistic_Merge <- Reduce(function(...) merge(...), 
                               list(csBaseline_TotBeefMerge, csPES5_TotBeefMerge, csPES10_TotBeefMerge))


cs_Pessimistic_Merge %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y = totExpB, color="Baseline"),size=1.1) +
  geom_point(aes(y = totExpB, color = "Baseline"),size=2) +
  geom_line(aes(y = totExpDOM5, color="5% depop"),size=1.1) +
  geom_point(aes(y = totExpDOM5, color = "5% depop"),size=2) +
  geom_line(aes(y = totExpDOM10, color="10% depop"),size=1.1) +
  geom_point(aes(y = totExpDOM10, color = "10% depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(revenue_Optimistic_Merge$Year[1],
                                  revenue_Optimistic_Merge$Year[nrow(revenue_Optimistic_Merge)])))+ 
  scale_y_continuous(name="Revenue (billion $)", limits = c(40,60,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', '5% depop', '10% depop'),
                     values=c('Baseline' ='#C77CFF' , '5% depop' = '#F8766D',
                              '10% depop'='#619CFF'))
cs_Pessimistic_Merge

expenditureDifference_Pessimistic <- cs_Pessimistic_Merge %>% 
  transmute(Year = Year, expDiff5 = totExpB - totExpDOM5, expDiff10 = totExpB - totExpDOM10)

cs_Pessimistic_Cumulative5 <- sum(expenditureDifference_Pessimistic$expDiff5)

cs_Pessimistic_Cumulative10 <- sum(expenditureDifference_Pessimistic$expDiff10)



