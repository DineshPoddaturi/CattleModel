######## Code to generate plots of fmd analysis


# proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_08[[1]]
# proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_08[[1]]


# proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0822[[1]]
# proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0822[[1]]
# proj_Q_P_PostFMD_OPT_20_08 <- optimisticPostFMD_20_0822[[1]]

proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0831[[1]]
proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0831[[1]]


proj_Q_P_PostFMD_OPT_5I_08 <- proj_Q_P_PostFMD_OPT_5_08 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl,Cl5 = Cl,
                                                                Hc5 = Hc, Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_OPT_10I_08 <- proj_Q_P_PostFMD_OPT_10_08 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                  Hc10 = Hc, Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                  mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter) 

# proj_Q_P_PostFMD_OPT_20I_08 <- proj_Q_P_PostFMD_OPT_20_08 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
#                                                                         Hc20 = Hc, Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
#                                                                         mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter) 

proj_Q_P_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                list(proj_Q_P_PostFMD_OPT_5I_08, proj_Q_P_PostFMD_OPT_10I_08))

# proj_Q_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0822[[3]]
# proj_Q_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0822[[3]]
# proj_Q_PostFMD_OPT_20_08 <- optimisticPostFMD_20_0822[[3]]

proj_Q_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0831[[3]]
proj_Q_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0831[[3]]

proj_Q_PostFMD_OPT_5I_08 <- proj_Q_PostFMD_OPT_5_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_OG = slMedian, Cl5_OG = clMedian)

proj_Q_PostFMD_OPT_10I_08 <- proj_Q_PostFMD_OPT_10_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl10_OG = slMedian, Cl10_OG = clMedian)

proj_Q_PostFMD_OPT_5I_08D <- proj_Q_P_PostFMD_OPT_5_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_DOM = Sl, Cl5_DOM = Cl)

proj_Q_PostFMD_OPT_10I_08D <- proj_Q_P_PostFMD_OPT_10_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl10_DOM = Sl, Cl10_DOM = Cl)

proj_Q_PostFMD_OPT_5I_08 <- merge(proj_Q_PostFMD_OPT_5I_08, proj_Q_PostFMD_OPT_5I_08D)
proj_Q_PostFMD_OPT_10I_08 <- merge(proj_Q_PostFMD_OPT_10I_08, proj_Q_PostFMD_OPT_10I_08D)

# proj_Q_PostFMD_OPT_20I_08 <- proj_Q_PostFMD_OPT_20_08 %>% filter(Year >= 2022) %>% 
#   transmute(Year = Year, Sl20_OG = slMedian, Cl20_OG = clMedian)

proj_Q_PostFMD_OPTI_08 <- Reduce(function(...) merge(...),
                                   list(proj_Q_PostFMD_OPT_5I_08, proj_Q_PostFMD_OPT_10I_08))

# beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0822[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0822[[2]] %>% transmute(Year = Year, K10 = K)
# 
# beefINV_FORECAST_PostFMD_OPT_20I_08 <- optimisticPostFMD_20_0822[[2]] %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0831[[2]] %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0831[[2]] %>% transmute(Year = Year, K10 = K)


beefINV_FORECAST_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                        list(beefINV_FORECAST_PostFMD_OPT_5I_08, beefINV_FORECAST_PostFMD_OPT_10I_08))


trade_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0831[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports5 = Imports, Exports5 = Exports, ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0831[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports10 = Imports, Exports10 = Exports, ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

# trade_FORECAST_PostFMD_OPT_20I_08 <- optimisticPostFMD_20_0831[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports20 = Imports, Exports20 = Exports, ImportsBeef20 = ImportsBeef, ExportsBeef20 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))


trade_FORECAST_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                           list(trade_FORECAST_PostFMD_OPT_5I_08, trade_FORECAST_PostFMD_OPT_10I_08))

# proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0822[[1]]
# proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0822[[1]]
# proj_Q_P_PostFMD_PES_20_08 <- pessimisticPostFMD_20_0822[[1]]

proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0831[[1]]
proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0831[[1]]

proj_Q_P_PostFMD_PES_5I_08 <- proj_Q_P_PostFMD_PES_5_08 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl, Cl5 = Cl,
                                                                      Hc5 = Hc, Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                      mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter)

proj_Q_P_PostFMD_PES_10I_08 <- proj_Q_P_PostFMD_PES_10_08 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                        Hc10 = Hc, Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                        mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter)

# proj_Q_P_PostFMD_PES_20I_08 <- proj_Q_P_PostFMD_PES_20_08 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
#                                                                         Hc20 = Hc, Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
#                                                                         mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)


proj_Q_P_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                list(proj_Q_P_PostFMD_PES_5I_08, proj_Q_P_PostFMD_PES_10I_08))


# proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0822[[3]]
# proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0822[[3]]
# proj_Q_PostFMD_PES_20_08 <- pessimisticPostFMD_20_0822[[3]]


proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0831[[3]]
proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0831[[3]]


proj_Q_PostFMD_PES_5I_08 <- proj_Q_PostFMD_PES_5_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_OG = slMedian, Cl5_OG = clMedian)

proj_Q_PostFMD_PES_10I_08 <- proj_Q_PostFMD_PES_10_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl10_OG = slMedian, Cl10_OG = clMedian)

# proj_Q_PostFMD_PES_20I_08 <- proj_Q_PostFMD_PES_20_08 %>% filter(Year >= 2022) %>% 
#   transmute(Year = Year, Sl20_OG = slMedian, Cl20_OG = clMedian)

proj_Q_PostFMD_PES_5I_08D <- proj_Q_P_PostFMD_PES_5_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_DOM = Sl, Cl5_DOM = Cl)

proj_Q_PostFMD_PES_10I_08D <- proj_Q_P_PostFMD_PES_10_08 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl10_DOM = Sl, Cl10_DOM = Cl)

proj_Q_PostFMD_PES_5I_08 <- merge(proj_Q_PostFMD_PES_5I_08, proj_Q_PostFMD_PES_5I_08D)
proj_Q_PostFMD_PES_10I_08 <- merge(proj_Q_PostFMD_PES_10I_08, proj_Q_PostFMD_PES_10I_08D)

proj_Q_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                 list(proj_Q_PostFMD_PES_5I_08, proj_Q_PostFMD_PES_10I_08))


# beefINV_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0822[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0822[[2]] %>% transmute(Year = Year, K10 = K)
# 
# beefINV_FORECAST_PostFMD_PES_20I_08 <- pessimisticPostFMD_20_0822[[2]] %>% transmute(Year = Year, K20 = K)

beefINV_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0831[[2]] %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0831[[2]] %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                        list(beefINV_FORECAST_PostFMD_PES_5I_08, beefINV_FORECAST_PostFMD_PES_10I_08))

trade_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0831[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports5 = Imports, Exports5 = Exports, ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0831[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports10 = Imports, Exports10 = Exports, ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

# trade_FORECAST_PostFMD_PES_20I_08 <- pessimisticPostFMD_20_0822[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports20 = Imports, Exports20 = Exports, ImportsBeef20 = ImportsBeef, ExportsBeef20 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))


trade_FORECAST_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                         list(trade_FORECAST_PostFMD_PES_5I_08, trade_FORECAST_PostFMD_PES_10I_08))



# proj_Q_P_PostFMD_OPTI_PS_PC_08 <- proj_Q_P_PostFMD_OPTI_08 %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20, Hc5, Hc10, Hc20)

proj_Q_P_PostFMD_OPTI_PS_PC_08 <- proj_Q_P_PostFMD_OPTI_08 %>% select(Year, Ps5, Pc5, Ps10, Pc10, Hc5, Hc10)

proj_Q_P_PostFMD_OPTI_PS_PC_08[,-1] <- proj_Q_P_PostFMD_OPTI_PS_PC_08[,-1] * 100

EQ_PricesCosts_OPT_08 <- proj_Q_PIIVII %>% transmute(Year = Year, PsB = Ps, PcB = Pc , HcB = Hc * 100) %>% filter(PsB>0)

proj_Q_P_PostFMD_OPTI_PS_PC_B_08 <- merge(proj_Q_P_PostFMD_OPTI_PS_PC_08, EQ_PricesCosts_OPT_08)

# proj_Q_P_PostFMD_OPTI_PS_B_08 <- proj_Q_P_PostFMD_OPTI_PS_PC_B_08 %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_OPTI_PS_B_08 <- proj_Q_P_PostFMD_OPTI_PS_PC_B_08 %>% select(Year, PsB, Ps5, Ps10)

# proj_Q_P_PostFMD_OPTI_PC_B_08 <- proj_Q_P_PostFMD_OPTI_PS_PC_B_08 %>% select(Year, PcB, Pc5, Pc10, Pc20)
proj_Q_P_PostFMD_OPTI_PC_B_08 <- proj_Q_P_PostFMD_OPTI_PS_PC_B_08 %>% select(Year, PcB, Pc5, Pc10)

# proj_Q_P_PostFMD_OPTI_PS_B_PercentChange_08 <- proj_Q_P_PostFMD_OPTI_PS_B_08 %>%
#   transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
#             Ps20Percent = (((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_PS_B_PercentChange_08 <- proj_Q_P_PostFMD_OPTI_PS_B_08 %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100)) %>% round(3)


# proj_Q_P_PostFMD_OPTI_PC_B_PercentChange_08 <- proj_Q_P_PostFMD_OPTI_PC_B_08 %>%
#   transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
#             Pc20Percent = (((Pc20-PcB)/PcB) * 100)) %>% round(3)

proj_Q_P_PostFMD_OPTI_PC_B_PercentChange_08 <- proj_Q_P_PostFMD_OPTI_PC_B_08 %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100)) %>% round(3)


PostFMD_OPTI_PS_Plot_08 <- round(proj_Q_P_PostFMD_OPTI_PS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_08)])))+ 
  scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_OPTI_PS_Plot_0820N <- round(proj_Q_P_PostFMD_OPTI_PS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PS_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_08)])))+ 
  scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', 'Baseline' = '#C77CFF'))


PostFMD_OPTI_PC_Plot_08 <- round(proj_Q_P_PostFMD_OPTI_PC_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PC_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PC_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PC_B_08)])))+ 
  scale_y_continuous(name="Cull Cow Price ($/CWT)", limits = c(50,100,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_OPTI_PC_Plot_0820N <- round(proj_Q_P_PostFMD_OPTI_PC_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_PC_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_PC_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PC_B_08)])))+ 
  scale_y_continuous(name="Cull Cow Price ($/CWT)", limits = c(50,100,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))



# proj_Q_P_PostFMD_OPTI_Sl_Cl_08 <- proj_Q_P_PostFMD_OPTI_08 %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20, 
#                                                                       Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

proj_Q_P_PostFMD_OPTI_Sl_Cl_08 <- proj_Q_PostFMD_OPTI_08 %>% select(Year, Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

EQ_Supplies_OPT_08 <- proj_Q_PIIVII  %>% transmute(Year = Year, SlB = Sl, ClB = Cl)

proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 <- merge(proj_Q_P_PostFMD_OPTI_Sl_Cl_08, EQ_Supplies_OPT_08)

# proj_Q_P_PostFMD_OPTI_Sl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5, Sl10, Sl20, Sl5_OG, Sl10_OG, Sl20_OG)
# 
# proj_Q_P_PostFMD_OPTI_Cl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5, Cl10, Cl20, Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_OPTI_Sl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_OPTI_Cl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_OPTI_TS_B_08 <- merge(proj_Q_P_PostFMD_OPTI_Sl_B_08, proj_Q_P_PostFMD_OPTI_Cl_B_08) %>%
  transmute(Year = Year, TSB = SlB + ClB, TS5_OG = Sl5_OG + Cl5_OG, TS10_OG = Sl10_OG + Cl10_OG, TS20_OG = Sl20_OG + Cl20_OG)

PostFMD_OPTI_Sl_ChangePlot_08 <- round(proj_Q_P_PostFMD_OPTI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(14,30,by=2))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_OPTI_Sl_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B)])))+ 
  scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(14,30,by=2))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', 'Baseline' = '#C77CFF'))




PostFMD_OPTI_Cl_ChangePlot_08 <- round(proj_Q_P_PostFMD_OPTI_Cl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"),size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"),size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B_08)])))+ 
  scale_y_continuous(name="Cull cow supply (in billion pounds)", limits = c(1,5,by=1))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_OPTI_Cl_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_Cl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"),size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"),size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Cl_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Cl_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Cl_B_08)])))+ 
  scale_y_continuous(name="Cull cow supply (in billion pounds)", limits = c(1,5,by=1))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


# PostFMD_OPTI_TS_ChangePlot_08 <- round(proj_Q_P_PostFMD_OPTI_TS_B_08,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
#   geom_point(aes(y = TSB, color = "Baseline"),size=2) +
#   geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
#   geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
#   geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
#   geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
#   geom_line(aes(y = TS20_OG, color="20% Depop"),size=1.1) +
#   geom_point(aes(y = TS20_OG, color="20% Depop"),size=2) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B_08$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B_08)])))+ 
#   scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
#         axis.text.y = element_text(size=15, face = "bold"), 
#         axis.title=element_text(size=14)) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
#                      values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))
# 
# PostFMD_OPTI_TS_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_TS_B_08,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
#   geom_point(aes(y = TSB, color = "Baseline"),size=2) +
#   geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
#   geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
#   geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
#   geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B_08$Year[1],
#                                   proj_Q_P_PostFMD_OPTI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B_08)])))+ 
#   scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
#         axis.text.y = element_text(size=15, face = "bold"), 
#         axis.title=element_text(size=14)) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
#                      values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


proj_Q_P_PostFMD_OPTI_SH_08 <- proj_Q_P_PostFMD_OPTI_08 %>% select(Year, sh5, sh10, sh20)

PostFMD_OPTI_SH_ChangePlot_08 <- round(proj_Q_P_PostFMD_OPTI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  geom_line(aes(y = sh20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = sh20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_SH_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_SH_08$Year[nrow(proj_Q_P_PostFMD_OPTI_SH_08)])))+ 
  scale_y_continuous(name="Share", limits = c(0.5,1.0,by=0.05))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38'))

PostFMD_OPTI_SH_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_SH_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_SH_08$Year[nrow(proj_Q_P_PostFMD_OPTI_SH_08)])))+ 
  scale_y_continuous(name="Share", limits = c(0.5,1.0,by=0.05))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D'))




proj_Q_P_PostFMD_PESI_PS_PC_08 <- proj_Q_P_PostFMD_PESI_08 %>% select(Year, Ps5, Pc5, Ps10, Pc10, Ps20, Pc20, Hc5, Hc10, Hc20)

proj_Q_P_PostFMD_PESI_PS_PC_08[,-1] <- proj_Q_P_PostFMD_PESI_PS_PC_08[,-1] * 100

EQ_PricesCosts_PES_08 <- proj_Q_PIIVII %>% transmute(Year = Year, PsB = Ps, PcB = Pc , HcB = Hc * 100) %>% filter(PsB>0)

proj_Q_P_PostFMD_PESI_PS_PC_B_08 <- merge(proj_Q_P_PostFMD_PESI_PS_PC_08, EQ_PricesCosts_PES_08)

proj_Q_P_PostFMD_PESI_PS_B_08 <- proj_Q_P_PostFMD_PESI_PS_PC_B_08 %>% select(Year, PsB, Ps5, Ps10, Ps20)

proj_Q_P_PostFMD_PESI_PC_B_08 <- proj_Q_P_PostFMD_PESI_PS_PC_B_08 %>% select(Year, PcB, Pc5, Pc10, Pc20)

proj_Q_P_PostFMD_PESI_PS_B_PercentChange_08 <- proj_Q_P_PostFMD_PESI_PS_B_08 %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100),
            Ps20Percent = (((Ps20-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_PC_B_PercentChange_08 <- proj_Q_P_PostFMD_PESI_PC_B_08 %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100),
            Pc20Percent = (((Pc20-PcB)/PcB) * 100)) %>% round(3)


PostFMD_PESI_PS_Plot_08 <- round(proj_Q_P_PostFMD_PESI_PS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  geom_line(aes(y = Ps20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Ps20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PS_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_PS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_08)])))+ 
  scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_PESI_PS_Plot_0820N <- round(proj_Q_P_PostFMD_PESI_PS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PsB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PsB, color = "Baseline"),size=2) +
  geom_line(aes(y = Ps5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Ps5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Ps10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Ps10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PS_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_PS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_PS_B_08)])))+ 
  scale_y_continuous(name="Fed cattle price ($/CWT)", limits = c(95,150,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


PostFMD_PESI_PC_Plot_08 <- round(proj_Q_P_PostFMD_PESI_PC_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  geom_line(aes(y = Pc20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Pc20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PC_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_PC_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_PC_B_08)])))+ 
  scale_y_continuous(name="Cull Cow Price ($/CWT)", limits = c(50,100,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_PESI_PC_Plot_0820N <- round(proj_Q_P_PostFMD_PESI_PC_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = PcB, color="Baseline"),size=1.1) +
  geom_point(aes(y = PcB, color = "Baseline"),size=2) +
  geom_line(aes(y = Pc5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Pc5, color = "5% Depop"),size=2) +
  geom_line(aes(y = Pc10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Pc10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_PC_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_PC_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_PC_B_08)])))+ 
  scale_y_continuous(name="Cull Cow Price ($/CWT)", limits = c(50,100,by=5) )  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


# proj_Q_P_PostFMD_PESI_Sl_Cl_08 <- proj_Q_P_PostFMD_PESI_08 %>% select(Year, Sl5, Cl5, Sl10, Cl10, Sl20, Cl20,
#                                                                       Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

proj_Q_P_PostFMD_PESI_Sl_Cl_08 <- proj_Q_PostFMD_PESI_08 %>% select(Year, Sl5_OG, Cl5_OG, Sl10_OG, Cl10_OG, Sl20_OG, Cl20_OG)

EQ_Supplies_PES_08 <- proj_Q_PIIVII  %>% transmute(Year = Year, SlB = Sl, ClB = Cl)

proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 <- merge(proj_Q_P_PostFMD_PESI_Sl_Cl_08, EQ_Supplies_PES_08)

# proj_Q_P_PostFMD_PESI_Sl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5, Sl10, Sl20, Sl5_OG, Sl10_OG, Sl20_OG)
# 
# proj_Q_P_PostFMD_PESI_Cl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5, Cl10, Cl20, Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_PESI_Sl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5_OG, Sl10_OG, Sl20_OG)

proj_Q_P_PostFMD_PESI_Cl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5_OG, Cl10_OG, Cl20_OG)


proj_Q_P_PostFMD_PESI_TS_B_08 <- merge(proj_Q_P_PostFMD_PESI_Sl_B_08, proj_Q_P_PostFMD_PESI_Cl_B_08) %>%
  transmute(Year = Year, TSB = SlB + ClB, TS5_OG = Sl5_OG + Cl5_OG, TS10_OG = Sl10_OG + Cl10_OG, TS20_OG = Sl20_OG + Cl20_OG)

PostFMD_PESI_Sl_ChangePlot_08 <- round(proj_Q_P_PostFMD_PESI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Sl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Sl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_B)])))+ 
  scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_PESI_Sl_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Sl_B$Year[1],
                                  proj_Q_P_PostFMD_PESI_Sl_B$Year[nrow(proj_Q_P_PostFMD_PESI_Sl_B)])))+ 
  scale_y_continuous(name="Fed cattle supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


PostFMD_PESI_Cl_ChangePlot_08 <- round(proj_Q_P_PostFMD_PESI_Cl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"),size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"),size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"),size=2) +
  geom_line(aes(y = Cl20_OG, color="20% Depop"),size=1.1) +
  geom_point(aes(y = Cl20_OG, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_B_08)])))+ 
  scale_y_continuous(name="Cull cow supply (in billion pounds)", limits = c(0.5,8,by=1))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))

PostFMD_PESI_Cl_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_Cl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = ClB, color="Baseline"),size=1.1) +
  geom_point(aes(y = ClB, color = "Baseline"),size=2) +
  geom_line(aes(y = Cl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Cl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Cl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Cl10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Cl_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_Cl_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_Cl_B_08)])))+ 
  scale_y_continuous(name="Cull cow supply (in billion pounds)", limits = c(0.5,8,by=1))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop', 'Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D', 'Baseline' = '#C77CFF'))




# PostFMD_PESI_TS_ChangePlot_08 <- round(proj_Q_P_PostFMD_PESI_TS_B_08,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
#   geom_point(aes(y = TSB, color = "Baseline"),size=2) +
#   geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
#   geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
#   geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
#   geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
#   geom_line(aes(y = TS20_OG, color="20% Depop"),size=1.1) +
#   geom_point(aes(y = TS20_OG, color="20% Depop"),size=2) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_PESI_TS_B_08$Year[1],
#                                   proj_Q_P_PostFMD_PESI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_TS_B_08)])))+ 
#   scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
#         axis.text.y = element_text(size=15, face = "bold"), 
#         axis.title=element_text(size=14)) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop','Baseline'),
#                      values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38', 'Baseline' = '#C77CFF'))
# 
# PostFMD_PESI_TS_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_TS_B_08,3) %>% ggplot(aes(x = Year))+
#   geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
#   geom_point(aes(y = TSB, color = "Baseline"),size=2) +
#   geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
#   geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
#   geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
#   geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P_PostFMD_PESI_TS_B_08$Year[1],
#                                   proj_Q_P_PostFMD_PESI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_TS_B_08)])))+ 
#   scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
#         axis.text.y = element_text(size=15, face = "bold"), 
#         axis.title=element_text(size=14)) +
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
#   scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
#                      values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


proj_Q_P_PostFMD_PESI_SH_08 <- proj_Q_P_PostFMD_PESI_08 %>% select(Year, sh5, sh10, sh20)

PostFMD_PESI_SH_ChangePlot_08 <- round(proj_Q_P_PostFMD_PESI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  geom_line(aes(y = sh20, color="20% Depop"),size=1.1) +
  geom_point(aes(y = sh20, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_SH_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_SH_08$Year[nrow(proj_Q_P_PostFMD_OPTI_SH_08)])))+ 
  scale_y_continuous(name="Share", limits = c(0.5,1.0,by=0.05))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','20% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','20% Depop'='#00BA38'))

PostFMD_PESI_SH_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_SH_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_SH_08$Year[nrow(proj_Q_P_PostFMD_OPTI_SH_08)])))+ 
  scale_y_continuous(name="Share", limits = c(0.5,1.0,by=0.05))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D'))


optimisticPostFMD_5_08_INV <- optimisticPostFMD_5_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

optimisticPostFMD_10_08_INV <- optimisticPostFMD_10_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

optimisticPostFMD_20_08_INV <- optimisticPostFMD_20_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)


library(latexpdf)
as.pdf(optimisticPostFMD_5_08_INV)
as.pdf(optimisticPostFMD_10_08_INV)
as.pdf(optimisticPostFMD_20_08_INV)

optimisticPostFMD_5_08_TRADE <- trade_FORECAST_PostFMD_OPTI_08 %>%
  transmute(Year = Year, imports = Imports5, exports = Exports5, importsBeef = ImportsBeef5, exportsBeef = ExportsBeef5)

optimisticPostFMD_10_08_TRADE <- trade_FORECAST_PostFMD_OPTI_08 %>%
  transmute(Year = Year, imports = Imports10, exports = Exports10, importsBeef = ImportsBeef10, exportsBeef = ExportsBeef10)

optimisticPostFMD_20_08_TRADE <- trade_FORECAST_PostFMD_OPTI_08 %>%
  transmute(Year = Year, imports = Imports20, exports = Exports20, importsBeef = ImportsBeef20, exportsBeef = ExportsBeef20)

as.pdf(optimisticPostFMD_5_08_TRADE)
as.pdf(optimisticPostFMD_10_08_TRADE)
as.pdf(optimisticPostFMD_20_08_TRADE)



pessimisticPostFMD_5_08_INV <- pessimisticPostFMD_5_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

pessimisticPostFMD_10_08_INV <- pessimisticPostFMD_10_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

pessimisticPostFMD_20_08_INV <- pessimisticPostFMD_20_0822[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

as.pdf(pessimisticPostFMD_5_08_INV)
as.pdf(pessimisticPostFMD_10_08_INV)
as.pdf(pessimisticPostFMD_20_08_INV)


pessimisticPostFMD_5_08_TRADE <- trade_FORECAST_PostFMD_PESI_08 %>%
  transmute(Year = Year, imports = Imports5, exports = Exports5, importsBeef = ImportsBeef5, exportsBeef = ExportsBeef5)

pessimisticPostFMD_10_08_TRADE <- trade_FORECAST_PostFMD_PESI_08 %>%
  transmute(Year = Year, imports = Imports10, exports = Exports10, importsBeef = ImportsBeef10, exportsBeef = ExportsBeef10)

pessimisticPostFMD_20_08_TRADE <- trade_FORECAST_PostFMD_PESI_08 %>%
  transmute(Year = Year, imports = Imports20, exports = Exports20, importsBeef = ImportsBeef20, exportsBeef = ExportsBeef20)

as.pdf(pessimisticPostFMD_5_08_TRADE)
as.pdf(pessimisticPostFMD_10_08_TRADE)
as.pdf(pessimisticPostFMD_20_08_TRADE)










