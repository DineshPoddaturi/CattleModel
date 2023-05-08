
slaughter_plotMedianJMP <- EQestObsPSNI_plots %>% ggplot(aes(x=Year))+ 
  geom_line(aes(y=ps, color = "Observed price"), size = 1) + 
  geom_point(aes(y=ps, color = "Observed price"), shape=15, size=2.25) + 
  geom_line(aes(y=psMedian, color="Median fitted price"), size = 1) +
  geom_point(aes(y = psMedian, color = "Median fitted price"), size=2.25) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)], by = 2))) +
  scale_y_continuous(name="Fed cattle price ($/cwt)")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


cull_plotMedianJMP <- EQestObsPCNI_plots%>% ggplot(aes(x=Year))+ 
  geom_line(aes(y=pc, color = "Observed price"), size = 1) + 
  geom_point(aes(y=pc, color = "Observed price"), shape=15 ,size=2.25) + 
  geom_line(aes(y=pcMedian, color="Median fitted price"), size = 1) +
  geom_point(aes(y = pcMedian, color = "Median fitted price"),size=2.25) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNI_plots$Year[1],EQestObsPCNI_plots$Year[nrow(EQestObsPCNI_plots)], by = 2))) +
  scale_y_continuous(name="Cull cow price ($/cwt)") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


invReplication_plotJMP <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"), size = 1) +
  geom_point(aes(y=K,color="Observed Inventory"), shape=15, size=2.25) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"), size = 1) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),size=2.25) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  scale_y_continuous(name="Million head") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))

# deTrendedInvReplication_plot <- ddlObsInventory_plot %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=K,color="Observed Inventory")) +
#   geom_point(aes(y=K,color="Observed Inventory"),size=0.75) +
#   geom_line(aes(y=fitK,color="Fitted Inventory")) +
#   geom_point(aes(y=fitK,color="Fitted Inventory"),size=0.75) +
#   scale_x_continuous(name="Year",
#                      breaks=c(seq(ddlInventory_plot$Year[1],
#                                   ddlInventory_plot$Year[nrow(ddlInventory_plot)], by = 2))) +
#   geom_hline(yintercept=0, linetype="dashed", color = "black") +
#   theme_classic() +
#   theme(legend.position="bottom", legend.box = "horizontal",
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12),
#         axis.text.y = element_text(size=12))+
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
#   theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))



estProj_PSIII_plotsJMP <- estProj_PSIII_FAPRI %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="Baseline"), size=1) +
  geom_point(aes(y = psMedian, color = "Baseline"), shape=16, size=2.25) +
  geom_line(aes(y=Ps, color="Projected"), size=1) +
  geom_point(aes(y=Ps, color="Projected"), shape=16, size=2.25) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI"), size=1) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI"), shape=15, size=2.25) +
  geom_line(aes(y=USDA_Ps, color="USDA"), size=1) +
  geom_point(aes(y=USDA_Ps, color="USDA"), shape=17,size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed cattle price ($/cwt)") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15,16,17))))


CARD_USDA_FAPRI_TS_Proj_plotIIIJMP <- CARD_USDA_FAPRI_TS_ProjIII %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year))  + 
  geom_line(aes(y=tsMedian, color="Baseline"), size=1) + 
  geom_point(aes(y=tsMedian, color="Baseline"), shape=16, size=2.25) + 
  geom_line(aes(y=TS, color="Projected"), size=1) + 
  geom_point(aes(y=TS, color="Projected"), shape=16, size=2.25) + 
  geom_line(aes(y=FAPRI_TS, color="FAPRI"), size=1)  + 
  geom_point(aes(y=FAPRI_TS, color="FAPRI"), shape=15, size=2.25) +
  geom_line(aes(y=USDA_TS, color="USDA"), size=1)  + 
  geom_point(aes(y=USDA_TS, color="USDA"), shape=17, size=2.25)  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_ProjIII$Year[1],
                                  CARD_USDA_FAPRI_TS_ProjIII$Year[nrow(CARD_USDA_FAPRI_TS_ProjIII)])))+ 
  scale_y_continuous(name="Total supply (billion pounds)", limits = c(25,29,by=2)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15,16,17))))


fedPriceOPT_plotJMP <- fedPricesOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"), size=1) +
  geom_point(aes(y = PsB, color = "Baseline"), size = 2.25) +
  geom_line(aes(y = Ps5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = Ps5, color = "5% Depopulation"), size = 2.25) +
  geom_line(aes(y = Ps10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = Ps10, color="10% Depopulation"), size = 2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedPricesOPT$Year[1],
                                  fedPricesOPT$Year[nrow(fedPricesOPT)])))+ 
  scale_y_continuous(name="Fed cattle price ($/cwt)", limits = c(95,150,by=5) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))


cullPriceOPT_plotJMP <- cullPricesOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"), size=1) +
  geom_point(aes(y = PcB, color = "Baseline"), size = 2.25) +
  geom_line(aes(y = Pc5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = Pc5, color = "5% Depopulation"), size = 2.25) +
  geom_line(aes(y = Pc10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = Pc10, color="10% Depopulation"), size = 2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullPricesOPT$Year[1],
                                  cullPricesOPT$Year[nrow(cullPricesOPT)])))+ 
  scale_y_continuous(name="Cull cow price ($/cwt)", limits = c(50,85,by=5) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))



fedSupplyOPT_plotJMP <- fedCattleSupplyOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlCattleB, color="Baseline"), size=1) +
  geom_point(aes(y = SlCattleB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = SlCattle5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = SlCattle5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = SlCattle10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = SlCattle10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedCattleSupplyOPT$Year[1],
                                  fedCattleSupplyOPT$Year[nrow(fedCattleSupplyOPT)])))+ 
  scale_y_continuous(name="Fed cattle supply (million head)", limits = c(20,32,by=2) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))



cullSupplyOPT_plotJMP <- cullCattleSupplyOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClCattleB, color="Baseline"), size=1) +
  geom_point(aes(y = ClCattleB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = ClCattle5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = ClCattle5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = ClCattle10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = ClCattle10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullCattleSupplyOPT$Year[1],
                                  cullCattleSupplyOPT$Year[nrow(cullCattleSupplyOPT)])))+ 
  scale_y_continuous(name="Cull cow supply (million head)", limits = c(4,7,by=2) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))


fedPricePES_plotJMP <- fedPricesPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"), size=1) +
  geom_point(aes(y = PsB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = Ps5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = Ps5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = Ps10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = Ps10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedPricesPES$Year[1],
                                  fedPricesPES$Year[nrow(fedPricesPES)])))+ 
  scale_y_continuous(name="Fed cattle price ($/cwt)", limits = c(95,150,by=5) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))



cullPricePES_plotJMP <- cullPricesPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"), size=1) +
  geom_point(aes(y = PcB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = Pc5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = Pc5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = Pc10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = Pc10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullPricesPES$Year[1],
                                  cullPricesPES$Year[nrow(cullPricesPES)])))+ 
  scale_y_continuous(name="Cull cow price ($/cwt)", limits = c(50,85,by=5) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))


fedSupplyPES_plotJMP <- fedCattleSupplyPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlCattleB, color="Baseline"), size=1) +
  geom_point(aes(y = SlCattleB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = SlCattle5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = SlCattle5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = SlCattle10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = SlCattle10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedCattleSupplyPES$Year[1],
                                  fedCattleSupplyPES$Year[nrow(fedCattleSupplyPES)])))+ 
  scale_y_continuous(name="Fed cattle supply (million head)", limits = c(20,32,by=2) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))



cullSupplyPES_plotJMP <- cullCattleSupplyPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClCattleB, color="Baseline"), size=1) +
  geom_point(aes(y = ClCattleB, color = "Baseline"), size=2.25) +
  geom_line(aes(y = ClCattle5, color="5% Depopulation"), size=1) +
  geom_point(aes(y = ClCattle5, color = "5% Depopulation"), size=2.25) +
  geom_line(aes(y = ClCattle10, color="10% Depopulation"), size=1) +
  geom_point(aes(y = ClCattle10, color="10% Depopulation"), size=2.25) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullCattleSupplyPES$Year[1],
                                  cullCattleSupplyPES$Year[nrow(cullCattleSupplyPES)])))+ 
  scale_y_continuous(name="Cull cow supply (million head)", limits = c(4,7,by=2) )  + 
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))




##### Regional Plots

slaughter_plotMedianRegionalJMP <- EQestObsPSNR_plots %>% ggplot(aes(x=Year))+ 
  geom_line(aes(y=ps, color = "Observed price"), size = 1) + 
  geom_point(aes(y=ps, color = "Observed price"), shape=15, size=2.25) + 
  geom_line(aes(y=psMedian, color="Median fitted price"), size = 1) +
  geom_point(aes(y = psMedian, color = "Median fitted price"), size=2.25) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)], by = 2))) +
  scale_y_continuous(name="Fed cattle price ($/cwt)")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


cull_plotMedianRegionalJMP <- EQestObsPCNR_plots %>% ggplot(aes(x=Year))+ 
  geom_line(aes(y=pc, color = "Observed price"), size = 1) + 
  geom_point(aes(y=pc, color = "Observed price"), shape=15 ,size=2.25) + 
  geom_line(aes(y=pcMedian, color="Median fitted price"), size = 1) +
  geom_point(aes(y = pcMedian, color = "Median fitted price"),size=2.25) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNI_plots$Year[1],EQestObsPCNI_plots$Year[nrow(EQestObsPCNI_plots)], by = 2))) +
  scale_y_continuous(name="Cull cow price ($/cwt)") + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


slSupply_plotMedianRegionalJMP <- EQestObsSLNR_Head %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=slSMHeadMill, color = "Observed supply"), size = 1) + 
  geom_point(aes(y=slSMHeadMill, color = "Observed supply"),shape=15,size=2.25) + 
  geom_line(aes(y=slMedianHeadMill, color="Median fitted supply"), size = 1) +
  geom_point(aes(y = slMedianHeadMill, color = "Median fitted supply"),shape=16,size=2.25) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsSLNR_Head$Year[1],EQestObsSLNR_Head$Year[nrow(EQestObsSLNR_Head)], by = 2))) +
  scale_y_continuous(name="Fed cattle supply (million head)")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 18, face="bold")) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 15, face="bold"),
                                              axis.text.y = element_text(size = 15, face="bold")) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5)) +
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))








