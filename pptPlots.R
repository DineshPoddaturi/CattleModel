

PostFMD_OPTI_PS_PercentChangePlotPPT <- proj_Q_P_PostFMD_OPTI_PS_B_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = Ps10Percent, color="Percent change in the price of fed cattle"),size=3) +
  geom_point(aes(y = Ps10Percent, color="Percent change in the price of fed cattle"),size=3.5) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B_PercentChange$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B_PercentChange$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_PercentChange)])))+
  scale_y_continuous(name="Percent change from baseline")  + theme_classic() + 
  theme(legend.position='bottom', legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=40, face = "bold"), 
        axis.text.y = element_text(size=40, face = "bold"), 
        axis.title = element_text(size= 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  geom_hline(yintercept=c(-20, -10, 0), linetype="dashed", size=0.25) +
  scale_color_manual(breaks=c('Percent change in the price of fed cattle'),
                     values=c('Percent change in the price of fed cattle'='#F8766D'))+
  theme(axis.ticks.length=unit(.35, "cm"))


PostFMD_OPTI_PS_PlotPPT <- round(proj_Q_P_PostFMD_OPTI_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=3) +
  geom_point(aes(y = PsB, color = "Baseline"),size=3.5) +
  geom_line(aes(y = Ps10, color="Depop"),size=3) +
  geom_point(aes(y = Ps10, color="Depop"),size=3.5) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B)])))+
  scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,140,by=5) ) +
  theme_classic() + 
  theme(legend.position=c(.85, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=40, face = "bold"), 
        axis.text.y = element_text(size=40, face = "bold"), 
        axis.title = element_text(size= 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Depop', 'Baseline'),
                     values=c('Depop'='#00BA38', 'Baseline'='#F8766D'))+theme(axis.ticks.length=unit(.35, "cm"))


PostFMD_OPTI_Sl_OG_PlotPPT <- round(proj_Q_P_PostFMD_OPTI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=3) +
  geom_point(aes(y = SlB, color = "Baseline"),size=3.5) +
  geom_line(aes(y = Sl10_OG, color="Depop"),size=3) +
  geom_point(aes(y = Sl10_OG, color="Depop"),size=3.5)+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Fed cattle supply (billion pounds)", limits = c(20,28,by=0.7))  + theme_classic() + 
  theme(legend.position=c(.85, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Depop', 'Baseline'),
                     values=c('Depop'='#F8766D', 'Baseline' = '#C77CFF')) + theme(axis.ticks.length=unit(.35, "cm"))


PostFMD_PESI_PS_PlotPPT <- round(proj_Q_P_PostFMD_PESI_PS_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=3) +
  geom_point(aes(y = PsB, color = "Baseline"),size=3.5) +
  geom_line(aes(y = Ps10, color="Depop"),size=3) +
  geom_point(aes(y = Ps10, color="Depop"),size=3.5) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B)])))+
  scale_y_continuous(name="Fed Cattle Price ($/CWT)", limits = c(95,140,by=5) ) +
  theme_classic() + 
  theme(legend.position=c(.85, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=40, face = "bold"), 
        axis.text.y = element_text(size=40, face = "bold"), 
        axis.title = element_text(size= 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Depop', 'Baseline'),
                     values=c('Depop'='#00BA38', 'Baseline'='#F8766D'))+theme(axis.ticks.length=unit(.35, "cm"))



PostFMD_PESI_Sl_OG_PlotPPT <- round(proj_Q_P_PostFMD_PESI_Sl_B,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=3) +
  geom_point(aes(y = SlB, color = "Baseline"),size=3.5) +
  geom_line(aes(y = Sl10_OG, color="Depop"),size=3) +
  geom_point(aes(y = Sl10_OG, color="Depop"),size=3.5)+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Fed Cattle Supply (billion pounds)", limits = c(20,28,by=1))  + theme_classic() + 
  theme(legend.position=c(.28, .85), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Depop', 'Baseline'),
                     values=c('Depop'='#F8766D', 'Baseline' = '#C77CFF')) + theme(axis.ticks.length=unit(.35, "cm"))



estProj_PSIII_plotsPPT <- estProj_PSIII_FAPRI %>% filter(Year >=2018 & Year < 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="Baseline"),size=3) +
  geom_point(aes(y = psMedian, color = "Baseline"),size=3.5) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI Projection"),size=3) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI Projection"),size=3.5) +
  geom_line(aes(y=USDA_Ps, color="USDA Projection"),size=3) +
  geom_point(aes(y=USDA_Ps, color="USDA Projection"),size=3.5) +
  geom_line(aes(y=Ps, color="Model Projection"),size=3) +
  geom_point(aes(y=Ps, color="Model Projection"),size=3.5) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price ($/CWT)", limits = c(105,150,by=10))+ theme_classic() + 
  theme(legend.position=c(.85, .3), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', 'Model Projection', 'FAPRI Projection', 'USDA Projection'),
                     values=c('Baseline'='#619CFF', 'Model Projection'='#F8766D', 'FAPRI Projection'='#00BA38',
                              'USDA Projection' = '#C77CFF'))+ theme(axis.ticks.length=unit(.35, "cm"))


CARD_USDA_FAPRI_TS_Proj_plotPPT <- CARD_USDA_FAPRI_TS_ProjIII %>% filter(Year >=2018 & Year < 2031)%>% ggplot(aes(x=Year))  + 
  geom_line(aes(y=tsMedian, color="Baseline"),size=3) + 
  geom_point(aes(y=tsMedian, color="Baseline"),size=3.5) + 
  geom_line(aes(y=FAPRI_TS, color="FAPRI Projection"),size=3)  + 
  geom_point(aes(y=FAPRI_TS, color="FAPRI Projection"),size=3.5) +
  geom_line(aes(y=USDA_TS, color="USDA Projection"),size=3)  + 
  geom_point(aes(y=USDA_TS, color="USDA Projection"),size=3.5)  + 
  geom_line(aes(y=TS, color="Model Projection"),size=3) + 
  geom_point(aes(y=TS, color="Model Projection"),size=3.5) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_ProjIII$Year[1],
                                  CARD_USDA_FAPRI_TS_ProjIII$Year[nrow(CARD_USDA_FAPRI_TS_ProjIII)])))+ 
  scale_y_continuous(name="Total Production (billion pounds)", limits = c(23,30,by=1)) + theme_classic() + 
  theme(legend.position=c(.85, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('Baseline', 'Model Projection', 'FAPRI Projection', 'USDA Projection'),
                     values=c('Baseline'='#619CFF', 'Model Projection'='#F8766D', 'FAPRI Projection'='#00BA38',
                              'USDA Projection' = '#C77CFF'))+ theme(axis.ticks.length=unit(.35, "cm"))



slaughter_plotPPT <- EQestObsPSNI_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=psMean, color="Mean fitted price"),size=3) +
  geom_point(aes(y = psMean, color = "Mean fitted price"),size=3.5) + 
  geom_line(aes(y=ps, color = "Observed price"),size=3) + 
  geom_point(aes(y=ps, color = "Observed price"),size=3.5) + 
  geom_line(aes(y=psMedian, color="Median fitted price"),size=3) +
  geom_point(aes(y = psMedian, color = "Median fitted price"),size=3.5) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)]))) +
  scale_y_continuous(name="Fed Cattle Price ($/CWT)")+  theme_classic() + 
  theme(legend.position=c(.85, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.ticks.length=unit(.35, "cm"))



invReplication_plotPPT <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"),size=3) +
  geom_point(aes(y=K,color="Observed Inventory"),size=3.5) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"),size=3) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),size=3.5) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  scale_y_continuous(name="Million Head" , limits = c(28,36,by=5)) + theme_classic() + 
  theme(legend.position=c(.3, .28), legend.box = "horizontal",text = element_text(size = 35,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank(),legend.key.size = unit(2, 'cm')) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 40, face = "bold"), 
        axis.text.y = element_text(size = 40, face = "bold"), 
        axis.title=element_text(size = 35, face = "bold")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) + 
  theme(axis.ticks.length=unit(.35, "cm"))




