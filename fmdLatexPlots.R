require(tikzDevice)

## Getting the data frames for latex plots

#### Optimistic Scenario
fedPricesOPT <- proj_Q_P_PostFMD_OPTI_PS_B_08 %>% round(3)
cullPricesOPT <- proj_Q_P_PostFMD_OPTI_PC_B_08 %>% round(3)

weightsCattle <- mergedForecast_Proj %>% select(Year, Slaughter_avg, Cull_avg) %>% filter(Year >= 2022)

fedBeefSupplyOPT <- proj_Q_P_PostFMD_OPTI_Sl_B_08 %>% 
  transmute(Year = Year, SlB = SlB, Sl5 = Sl5_DOM, Sl10 = Sl10_DOM) %>% round(3)

fedCattleSupplyOPT <- merge(fedBeefSupplyOPT, weightsCattle %>% select(Year, Slaughter_avg)) %>% 
  mutate(SlCattleB = (SlB * 1000)/Slaughter_avg, SlCattle5 = (Sl5 * 1000)/Slaughter_avg, 
         SlCattle10 = (Sl10 * 1000)/Slaughter_avg) %>% select(Year, SlCattleB, SlCattle5, SlCattle10) %>% round(3)

cullBeefSupplyOPT <- proj_Q_P_PostFMD_OPTI_Cl_B_08 %>% 
  transmute(Year = Year, ClB = ClB, Cl5 = Cl5_DOM, Cl10 = Cl10_DOM) %>% round(3)

cullCattleSupplyOPT <- merge(cullBeefSupplyOPT, weightsCattle %>% select(Year, Cull_avg)) %>%
  mutate(ClCattleB = (ClB * 1000)/Cull_avg, ClCattle5 = (Cl5 * 1000)/Cull_avg,
         ClCattle10 = (Cl10 * 1000)/Cull_avg) %>% select(Year, ClCattleB, ClCattle5, ClCattle10) %>% round(3)

demandOPT <- proj_Q_P_PostFMD_OPTI_Dem_08 %>% select(Year, D, D5, D10)  %>% round(3)


##### Pessimistic Scenario
fedPricesPES <- proj_Q_P_PostFMD_PESI_PS_B_08 %>% round(3)
cullPricesPES <- proj_Q_P_PostFMD_PESI_PC_B_08 %>% round(3)

weightsCattle <- mergedForecast_Proj %>% select(Year, Slaughter_avg, Cull_avg) %>% filter(Year >= 2022)

fedBeefSupplyPES <- proj_Q_P_PostFMD_PESI_Sl_B_08 %>% 
  transmute(Year = Year, SlB = SlB, Sl5 = Sl5_DOM, Sl10 = Sl10_DOM) %>% round(3)

fedCattleSupplyPES <- merge(fedBeefSupplyPES, weightsCattle %>% select(Year, Slaughter_avg)) %>% 
  mutate(SlCattleB = (SlB * 1000)/Slaughter_avg, SlCattle5 = (Sl5 * 1000)/Slaughter_avg, 
         SlCattle10 = (Sl10 * 1000)/Slaughter_avg) %>% select(Year, SlCattleB, SlCattle5, SlCattle10) %>% round(3)

cullBeefSupplyPES <- proj_Q_P_PostFMD_PESI_Cl_B_08 %>% 
  transmute(Year = Year, ClB = ClB, Cl5 = Cl5_DOM, Cl10 = Cl10_DOM) %>% round(3)

cullCattleSupplyPES <- merge(cullBeefSupplyPES, weightsCattle %>% select(Year, Cull_avg)) %>%
  mutate(ClCattleB = (ClB * 1000)/Cull_avg, ClCattle5 = (Cl5 * 1000)/Cull_avg,
         ClCattle10 = (Cl10 * 1000)/Cull_avg) %>% select(Year, ClCattleB, ClCattle5, ClCattle10) %>% round(3)

demandPES <- proj_Q_P_PostFMD_PESI_Dem_08 %>% select(Year, D, D5, D10)  %>% round(3)



################################## Tikz Plots ###############################

#### Optimistic Scenario

### Fed Cattle price changes

tikz(file="fmdLatexPlots/FedCattlePricePlotOPT.tex", width=6.8, height=4, sanitize = TRUE)

fedPriceOPT_plot <- fedPricesOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = PsB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = Ps5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = Ps5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = Ps10, color="10% Depopulation")) +
  geom_point(aes(y = Ps10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedPricesOPT$Year[1],
                                  fedPricesOPT$Year[nrow(fedPricesOPT)])))+ 
  scale_y_continuous(name="Fed Cattle Price", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(fedPriceOPT_plot)

dev.off()

### Cull cow price changes

tikz(file="fmdLatexPlots/CullCowPricePlotOPT.tex", width=6.8, height=4, sanitize = TRUE)

cullPriceOPT_plot <- cullPricesOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = PcB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = Pc5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = Pc5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = Pc10, color="10% Depopulation")) +
  geom_point(aes(y = Pc10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullPricesOPT$Year[1],
                                  cullPricesOPT$Year[nrow(cullPricesOPT)])))+ 
  scale_y_continuous(name="Cull Cow Price", limits = c(50,85,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(cullPriceOPT_plot)

dev.off()

### Changes in fed cattle supplies
tikz(file="fmdLatexPlots/FedCattleSupplyPlotOPT.tex", width=6.8, height=4, sanitize = TRUE)

fedSupplyOPT_plot <- fedCattleSupplyOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlCattleB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = SlCattleB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = SlCattle5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = SlCattle5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = SlCattle10, color="10% Depopulation")) +
  geom_point(aes(y = SlCattle10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedCattleSupplyOPT$Year[1],
                                  fedCattleSupplyOPT$Year[nrow(fedCattleSupplyOPT)])))+ 
  scale_y_continuous(name="Fed Cattle Supply", limits = c(20,32,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(fedSupplyOPT_plot)

dev.off()

### Changes in cull cow supplies
tikz(file="fmdLatexPlots/CullCowSupplyPlotOPT.tex", width=6.8, height=4, sanitize = TRUE)

cullSupplyOPT_plot <- cullCattleSupplyOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClCattleB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = ClCattleB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = ClCattle5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = ClCattle5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = ClCattle10, color="10% Depopulation")) +
  geom_point(aes(y = ClCattle10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullCattleSupplyOPT$Year[1],
                                  cullCattleSupplyOPT$Year[nrow(cullCattleSupplyOPT)])))+ 
  scale_y_continuous(name="Cull Cow Supply", limits = c(4,7,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(cullSupplyOPT_plot)

dev.off()

### Changes in demand
tikz(file="fmdLatexPlots/DemandPlotOPT.tex", width=6.8, height=4, sanitize = TRUE)

demandOPT_plot <- demandOPT %>% ggplot(aes(x = Year))+
  geom_line(aes(y = D, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = D, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = D5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = D5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = D10, color="10% Depopulation")) +
  geom_point(aes(y = D10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(demandOPT$Year[1],
                                  demandOPT$Year[nrow(demandOPT)])))+ 
  scale_y_continuous(name="Demand", limits = c(42,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(demandOPT_plot)

dev.off()



#### Pessimistic Scenario

### Fed Cattle price changes

tikz(file="fmdLatexPlots/FedCattlePricePlotPES.tex", width=6.8, height=4, sanitize = TRUE)

fedPricePES_plot <- fedPricesPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = PsB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = Ps5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = Ps5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = Ps10, color="10% Depopulation")) +
  geom_point(aes(y = Ps10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedPricesPES$Year[1],
                                  fedPricesPES$Year[nrow(fedPricesPES)])))+ 
  scale_y_continuous(name="Fed Cattle Price", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(fedPricePES_plot)

dev.off()

### Cull cow price changes

tikz(file="fmdLatexPlots/CullCowPricePlotPES.tex", width=6.8, height=4, sanitize = TRUE)

cullPricePES_plot <- cullPricesPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = PcB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = Pc5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = Pc5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = Pc10, color="10% Depopulation")) +
  geom_point(aes(y = Pc10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullPricesPES$Year[1],
                                  cullPricesPES$Year[nrow(cullPricesPES)])))+ 
  scale_y_continuous(name="Cull Cow Price", limits = c(50,85,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(cullPricePES_plot)

dev.off()

### Changes in fed cattle supplies
tikz(file="fmdLatexPlots/FedCattleSupplyPlotPES.tex", width=6.8, height=4, sanitize = TRUE)

fedSupplyPES_plot <- fedCattleSupplyPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlCattleB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = SlCattleB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = SlCattle5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = SlCattle5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = SlCattle10, color="10% Depopulation")) +
  geom_point(aes(y = SlCattle10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(fedCattleSupplyPES$Year[1],
                                  fedCattleSupplyPES$Year[nrow(fedCattleSupplyPES)])))+ 
  scale_y_continuous(name="Fed Cattle Supply", limits = c(20,32,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(fedSupplyPES_plot)

dev.off()

### Changes in cull cow supplies
tikz(file="fmdLatexPlots/CullCowSupplyPlotPES.tex", width=6.8, height=4, sanitize = TRUE)

cullSupplyPES_plot <- cullCattleSupplyPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClCattleB, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = ClCattleB, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = ClCattle5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = ClCattle5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = ClCattle10, color="10% Depopulation")) +
  geom_point(aes(y = ClCattle10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(cullCattleSupplyPES$Year[1],
                                  cullCattleSupplyPES$Year[nrow(cullCattleSupplyPES)])))+ 
  scale_y_continuous(name="Cull Cow Supply", limits = c(4,7,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(cullSupplyPES_plot)

dev.off()

### Changes in demand
tikz(file="fmdLatexPlots/DemandPlotPES.tex", width=6.8, height=4, sanitize = TRUE)

demandPES_plot <- demandPES %>% ggplot(aes(x = Year))+
  geom_line(aes(y = D, color="Baseline")) +
  # stat_smooth(aes(y=PsB, color="Baseline"), method=lm, formula = y ~ poly(x,8.5), level=0.1) +
  geom_point(aes(y = D, color = "Baseline"), size = 0.75) +
  geom_line(aes(y = D5, color="5% Depopulation")) +
  # stat_smooth(aes(y=Ps5, color="5% Depopulation"), method=lm, formula = y ~ poly(x,9), level=0.1) +
  geom_point(aes(y = D5, color = "5% Depopulation"), size = 0.75) +
  geom_line(aes(y = D10, color="10% Depopulation")) +
  geom_point(aes(y = D10, color="10% Depopulation"), size = 0.75) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(demandPES$Year[1],
                                  demandPES$Year[nrow(demandPES)])))+ 
  scale_y_continuous(name="Demand", limits = c(42,60,by=2) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depopulation', '10% Depopulation','Baseline'),
                     values=c('5% Depopulation'='#619CFF', '10% Depopulation'='#F8766D', 'Baseline' = '#C77CFF')) + 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))

print(demandPES_plot)

dev.off()




