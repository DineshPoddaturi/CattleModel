######## Code to generate plots of fmd analysis


# proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_08[[1]]
# proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_08[[1]]


# proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0822[[1]]
# proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0822[[1]]
# proj_Q_P_PostFMD_OPT_20_08 <- optimisticPostFMD_20_0822[[1]]

# proj_Q_P_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0831[[1]]
# proj_Q_P_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0831[[1]]

# proj_Q_P_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0902[[1]]
# proj_Q_P_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0902[[1]]


# proj_Q_P_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0904[[1]]
# proj_Q_P_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0904[[1]]


proj_Q_P_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0909[[1]]
proj_Q_P_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0909[[1]]

proj_Q_P_PostFMD_OPT_5I_08 <- proj_Q_P_PostFMD_OPT_5_09 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl,Cl5 = Cl,
                                                                      Hc5 = Hc, Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                      mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter,
                                                                      A5 = A)

proj_Q_P_PostFMD_OPT_10I_08 <- proj_Q_P_PostFMD_OPT_10_09 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                        Hc10 = Hc, Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                        mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter,
                                                                        A10 = A) 

# proj_Q_P_PostFMD_OPT_20I_08 <- proj_Q_P_PostFMD_OPT_20_08 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
#                                                                         Hc20 = Hc, Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
#                                                                         mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter) 

proj_Q_P_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                   list(proj_Q_P_PostFMD_OPT_5I_08, proj_Q_P_PostFMD_OPT_10I_08))

# proj_Q_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0822[[3]]
# proj_Q_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0822[[3]]
# proj_Q_PostFMD_OPT_20_08 <- optimisticPostFMD_20_0822[[3]]

# proj_Q_PostFMD_OPT_5_08 <- optimisticPostFMD_5_0831[[3]]
# proj_Q_PostFMD_OPT_10_08 <- optimisticPostFMD_10_0831[[3]]

# proj_Q_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0902[[3]]
# proj_Q_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0902[[3]]

# proj_Q_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0904[[3]]
# proj_Q_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0904[[3]]

proj_Q_PostFMD_OPT_5_09 <- optimisticPostFMD_5_0909[[3]]
proj_Q_PostFMD_OPT_10_09 <- optimisticPostFMD_10_0909[[3]]


proj_Q_PostFMD_OPT_5I_08 <- proj_Q_PostFMD_OPT_5_09 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_OG = slMedian, Cl5_OG = clMedian)

proj_Q_PostFMD_OPT_10I_08 <- proj_Q_PostFMD_OPT_10_09 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl10_OG = slMedian, Cl10_OG = clMedian)

proj_Q_PostFMD_OPT_5I_08D <- proj_Q_P_PostFMD_OPT_5_09 %>% filter(Year >= 2022) %>% 
  transmute(Year = Year, Sl5_DOM = Sl, Cl5_DOM = Cl)

proj_Q_PostFMD_OPT_10I_08D <- proj_Q_P_PostFMD_OPT_10_09 %>% filter(Year >= 2022) %>% 
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

# beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0831[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0831[[2]] %>% transmute(Year = Year, K10 = K)

# beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0902[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0902[[2]] %>% transmute(Year = Year, K10 = K)

# beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0904[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0904[[2]] %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0909[[2]] %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0909[[2]] %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                           list(beefINV_FORECAST_PostFMD_OPT_5I_08, beefINV_FORECAST_PostFMD_OPT_10I_08))


# trade_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0902[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports5 = round(Imports), Exports5 = round(Exports), ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))
# 
# trade_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0902[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports10 = round(Imports), Exports10 = round(Exports), ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_OPT_5I_08 <- optimisticPostFMD_5_0909[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports5 = round(Imports), Exports5 = round(Exports), ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_OPT_10I_08 <- optimisticPostFMD_10_0909[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports10 = round(Imports), Exports10 = round(Exports), ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))



# trade_FORECAST_PostFMD_OPT_20I_08 <- optimisticPostFMD_20_0831[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports20 = Imports, Exports20 = Exports, ImportsBeef20 = ImportsBeef, ExportsBeef20 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))


trade_FORECAST_PostFMD_OPTI_08 <- Reduce(function(...) merge(...), 
                                         list(trade_FORECAST_PostFMD_OPT_5I_08, trade_FORECAST_PostFMD_OPT_10I_08))

# proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0822[[1]]
# proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0822[[1]]
# proj_Q_P_PostFMD_PES_20_08 <- pessimisticPostFMD_20_0822[[1]]

# proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0902[[1]]
# proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0902[[1]]

# proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0904[[1]]
# proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0904[[1]]

proj_Q_P_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0909[[1]]
proj_Q_P_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0909[[1]]


proj_Q_P_PostFMD_PES_5I_08 <- proj_Q_P_PostFMD_PES_5_08 %>% transmute(Year = Year, Ps5 = Ps, Pc5 = Pc, Sl5 = Sl, Cl5 = Cl,
                                                                      Hc5 = Hc, Sl5_OG = Sl_OG, Cl5_OG = Cl_OG,
                                                                      mu5 = muTilde, sh5 = sh, D5 = demDollarsAfter,
                                                                      A5 = A)

proj_Q_P_PostFMD_PES_10I_08 <- proj_Q_P_PostFMD_PES_10_08 %>% transmute(Year = Year,Ps10 = Ps, Pc10 = Pc, Sl10 = Sl, Cl10 = Cl,
                                                                        Hc10 = Hc, Sl10_OG = Sl_OG, Cl10_OG = Cl_OG,
                                                                        mu10 = muTilde, sh10 = sh, D10 = demDollarsAfter,
                                                                        A10 = A)

# proj_Q_P_PostFMD_PES_20I_08 <- proj_Q_P_PostFMD_PES_20_08 %>% transmute(Year = Year,Ps20 = Ps, Pc20 = Pc, Sl20 = Sl, Cl20 = Cl,
#                                                                         Hc20 = Hc, Sl20_OG = Sl_OG, Cl20_OG = Cl_OG,
#                                                                         mu20 = muTilde, sh20 = sh, D20 = demDollarsAfter)


proj_Q_P_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                   list(proj_Q_P_PostFMD_PES_5I_08, proj_Q_P_PostFMD_PES_10I_08))


# proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0822[[3]]
# proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0822[[3]]
# proj_Q_PostFMD_PES_20_08 <- pessimisticPostFMD_20_0822[[3]]


# proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0902[[3]]
# proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0902[[3]]

# proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0904[[3]]
# proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0904[[3]]

proj_Q_PostFMD_PES_5_08 <- pessimisticPostFMD_5_0909[[3]]
proj_Q_PostFMD_PES_10_08 <- pessimisticPostFMD_10_0909[[3]]


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

# beefINV_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0902[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0902[[2]] %>% transmute(Year = Year, K10 = K)

# beefINV_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0904[[2]] %>% transmute(Year = Year, K5 = K)
# 
# beefINV_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0904[[2]] %>% transmute(Year = Year, K10 = K)

beefINV_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0909[[2]] %>% transmute(Year = Year, K5 = K)

beefINV_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0909[[2]] %>% transmute(Year = Year, K10 = K)



beefINV_FORECAST_PostFMD_PESI_08 <- Reduce(function(...) merge(...), 
                                           list(beefINV_FORECAST_PostFMD_PES_5I_08, beefINV_FORECAST_PostFMD_PES_10I_08))

# trade_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0902[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports5 = round(Imports), Exports5 = round(Exports), ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))
# 
# trade_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0902[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports10 = round(Imports), Exports10 = round(Exports), ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))

# trade_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0904[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports5 = round(Imports), Exports5 = round(Exports), ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))
# 
# trade_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0904[[3]] %>% filter(Year>=2022) %>%
#   transmute(Year = Year,Imports10 = round(Imports), Exports10 = round(Exports), ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
#   mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_PES_5I_08 <- pessimisticPostFMD_5_0909[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports5 = round(Imports), Exports5 = round(Exports), ImportsBeef5 = ImportsBeef, ExportsBeef5 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))

trade_FORECAST_PostFMD_PES_10I_08 <- pessimisticPostFMD_10_0909[[3]] %>% filter(Year>=2022) %>%
  transmute(Year = Year,Imports10 = round(Imports), Exports10 = round(Exports), ImportsBeef10 = ImportsBeef, ExportsBeef10 = ExportsBeef) %>% 
  mutate_all(~replace_na(.,0))



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

proj_Q_P_PostFMD_OPTI_Sl_Cl_08 <- proj_Q_PostFMD_OPTI_08 %>% select(Year, Sl5_OG, Cl5_OG, Sl5_DOM, 
                                                                    Cl5_DOM, Sl10_OG, Cl10_OG, Sl10_DOM, Cl10_DOM)

EQ_Supplies_OPT_08 <- proj_Q_PIIVII  %>% transmute(Year = Year, SlB = Sl, ClB = Cl)

proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 <- merge(proj_Q_P_PostFMD_OPTI_Sl_Cl_08, EQ_Supplies_OPT_08)

# proj_Q_P_PostFMD_OPTI_Sl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5, Sl10, Sl20, Sl5_OG, Sl10_OG, Sl20_OG)
# 
# proj_Q_P_PostFMD_OPTI_Cl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5, Cl10, Cl20, Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_OPTI_Sl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5_OG, Sl10_OG, Sl5_DOM, Sl10_DOM)

proj_Q_P_PostFMD_OPTI_Cl_B_08 <- proj_Q_P_PostFMD_OPTI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5_OG, Cl10_OG, Cl5_DOM, Cl10_DOM)

proj_Q_P_PostFMD_OPTI_TS_B_08 <- merge(proj_Q_P_PostFMD_OPTI_Sl_B_08, proj_Q_P_PostFMD_OPTI_Cl_B_08) %>%
  transmute(Year = Year, TSB = SlB + ClB, TS5_OG = Sl5_OG + Cl5_OG, TS10_OG = Sl10_OG + Cl10_OG,
            TS5_DOM = Sl5_DOM + Cl5_DOM, TS10_DOM = Sl10_DOM + Cl10_DOM)

PostFMD_OPTI_Sl_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B_08)])))+ 
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

PostFMD_OPTI_Sl_ChangePlot_0820ND <- round(proj_Q_P_PostFMD_OPTI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_DOM, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_DOM, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_DOM, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_DOM, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Sl_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Sl_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Sl_B_08)])))+ 
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

PostFMD_OPTI_TS_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_TS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
  geom_point(aes(y = TSB, color = "Baseline"),size=2) +
  geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year",
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B_08)])))+
  scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"),
        axis.text.y = element_text(size=15, face = "bold"),
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

PostFMD_OPTI_TS_ChangePlot_0820ND <- round(proj_Q_P_PostFMD_OPTI_TS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
  geom_point(aes(y = TSB, color = "Baseline"),size=2) +
  geom_line(aes(y = TS5_DOM, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TS5_DOM, color = "5% Depop"),size=2) +
  geom_line(aes(y = TS10_DOM, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TS10_DOM, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year",
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_TS_B_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_OPTI_TS_B_08)])))+
  scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"),
        axis.text.y = element_text(size=15, face = "bold"),
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


Eq_proj_Q_P_Dem <- proj_Q_PIIVII %>% transmute(Year = Year, A = A/shockD, D = Sl * ((Ps/100)/phi) + Cl * ((Pc/100)/phi))

proj_Q_P_PostFMD_OPTI_Dem_08 <- merge(proj_Q_P_PostFMD_OPTI_08, Eq_proj_Q_P_Dem) %>% select(Year, A, D, A5, D5, A10, D10)

PostFMD_OPTI_A_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_Dem_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = A5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = A5, color = "5% Depop"),size=2) +
  geom_line(aes(y = A10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = A10, color="10% Depop"),size=2) +
  geom_line(aes(y = A, color="Baseline"),size=1.1) +
  geom_point(aes(y = A, color = "Baseline"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Dem_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Dem_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Dem_08)])))+ 
  scale_y_continuous(name="Demand(billion pounds)", limits = c(20,40,by=5))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

PostFMD_OPTI_D_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_Dem_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = D5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = D5, color = "5% Depop"),size=2) +
  geom_line(aes(y = D10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = D10, color="10% Depop"),size=2) +
  geom_line(aes(y = D, color="Baseline"),size=1.1) +
  geom_point(aes(y = D, color = "Baseline"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_OPTI_Dem_08$Year[1],
                                  proj_Q_P_PostFMD_OPTI_Dem_08$Year[nrow(proj_Q_P_PostFMD_OPTI_Dem_08)])))+ 
  scale_y_continuous(name="Demand(billion dollars)", limits = c(40,60,by=5))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

proj_Q_P_PostFMD_OPTI_SH_08 <- merge(proj_Q_PIIVII,proj_Q_P_PostFMD_OPTI_08) %>% select(Year, sh, sh5, sh10)

PostFMD_OPTI_SH_ChangePlot_0820N <- round(proj_Q_P_PostFMD_OPTI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  geom_line(aes(y = sh, color="Baseline"),size=1.1) +
  geom_point(aes(y = sh, color = "Baseline"),size=2) +
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
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))



proj_Q_P_PostFMD_PESI_PS_PC_08 <- proj_Q_P_PostFMD_PESI_08 %>% select(Year, Ps5, Pc5, Ps10, Pc10, Hc5, Hc10)

proj_Q_P_PostFMD_PESI_PS_PC_08[,-1] <- proj_Q_P_PostFMD_PESI_PS_PC_08[,-1] * 100

EQ_PricesCosts_PES_08 <- proj_Q_PIIVII %>% transmute(Year = Year, PsB = Ps, PcB = Pc , HcB = Hc * 100) %>% filter(PsB>0)

proj_Q_P_PostFMD_PESI_PS_PC_B_08 <- merge(proj_Q_P_PostFMD_PESI_PS_PC_08, EQ_PricesCosts_PES_08)

proj_Q_P_PostFMD_PESI_PS_B_08 <- proj_Q_P_PostFMD_PESI_PS_PC_B_08 %>% select(Year, PsB, Ps5, Ps10)

proj_Q_P_PostFMD_PESI_PC_B_08 <- proj_Q_P_PostFMD_PESI_PS_PC_B_08 %>% select(Year, PcB, Pc5, Pc10)

proj_Q_P_PostFMD_PESI_PS_B_PercentChange_08 <- proj_Q_P_PostFMD_PESI_PS_B_08 %>%
  transmute(Year, Ps5Percent = (((Ps5-PsB)/PsB) * 100), Ps10Percent = (((Ps10-PsB)/PsB) * 100)) %>% round(3)

proj_Q_P_PostFMD_PESI_PC_B_PercentChange_08 <- proj_Q_P_PostFMD_PESI_PC_B_08 %>%
  transmute(Year, Pc5Percent = (((Pc5-PcB)/PcB) * 100), Pc10Percent = (((Pc10-PcB)/PcB) * 100)) %>% round(3)

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

proj_Q_P_PostFMD_PESI_Sl_Cl_08 <- proj_Q_PostFMD_PESI_08 %>% select(Year, Sl5_OG, Cl5_OG, Sl5_DOM, Cl5_DOM, 
                                                                    Sl10_OG, Cl10_OG, Sl10_DOM, Cl10_DOM)

EQ_Supplies_PES_08 <- proj_Q_PIIVII  %>% transmute(Year = Year, SlB = Sl, ClB = Cl)

proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 <- merge(proj_Q_P_PostFMD_PESI_Sl_Cl_08, EQ_Supplies_PES_08)

# proj_Q_P_PostFMD_PESI_Sl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5, Sl10, Sl20, Sl5_OG, Sl10_OG, Sl20_OG)
# 
# proj_Q_P_PostFMD_PESI_Cl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5, Cl10, Cl20, Cl5_OG, Cl10_OG, Cl20_OG)

proj_Q_P_PostFMD_PESI_Sl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, SlB, Sl5_OG, Sl10_OG, Sl5_DOM, Sl10_DOM)

proj_Q_P_PostFMD_PESI_Cl_B_08 <- proj_Q_P_PostFMD_PESI_Sl_Cl_B_08 %>% select(Year, ClB, Cl5_OG, Cl10_OG, Cl5_DOM, Cl10_DOM)


proj_Q_P_PostFMD_PESI_TS_B_08 <- merge(proj_Q_P_PostFMD_PESI_Sl_B_08, proj_Q_P_PostFMD_PESI_Cl_B_08) %>%
  transmute(Year = Year, TSB = SlB + ClB, TS5_OG = Sl5_OG + Cl5_OG, TS10_OG = Sl10_OG + Cl10_OG, 
            TS5_DOM = Sl5_DOM + Cl5_DOM, TS10_DOM = Sl10_DOM + Cl10_DOM)

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

PostFMD_PESI_Sl_ChangePlot_0820ND <- round(proj_Q_P_PostFMD_PESI_Sl_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = SlB, color="Baseline"),size=1.1) +
  geom_point(aes(y = SlB, color = "Baseline"),size=2) +
  geom_line(aes(y = Sl5_DOM, color="5% Depop"),size=1.1) +
  geom_point(aes(y = Sl5_DOM, color = "5% Depop"),size=2) +
  geom_line(aes(y = Sl10_DOM, color="10% Depop"),size=1.1) +
  geom_point(aes(y = Sl10_DOM, color="10% Depop"),size=2) +
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

PostFMD_PESI_TS_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_TS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
  geom_point(aes(y = TSB, color = "Baseline"),size=2) +
  geom_line(aes(y = TS5_OG, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TS5_OG, color = "5% Depop"),size=2) +
  geom_line(aes(y = TS10_OG, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TS10_OG, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year",
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_TS_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_TS_B_08)])))+
  scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"),
        axis.text.y = element_text(size=15, face = "bold"),
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

PostFMD_PESI_TS_ChangePlot_0820ND <- round(proj_Q_P_PostFMD_PESI_TS_B_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TSB, color="Baseline"),size=1.1) +
  geom_point(aes(y = TSB, color = "Baseline"),size=2) +
  geom_line(aes(y = TS5_DOM, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TS5_DOM, color = "5% Depop"),size=2) +
  geom_line(aes(y = TS10_DOM, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TS10_DOM, color="10% Depop"),size=2) +
  scale_x_continuous(name="Year",
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_TS_B_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_TS_B_08$Year[nrow(proj_Q_P_PostFMD_PESI_TS_B_08)])))+
  scale_y_continuous(name="Total supply (in billion pounds)", limits = c(15,35,by=2))  + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"),
        axis.text.y = element_text(size=15, face = "bold"),
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

Eq_proj_Q_P_Dem <- proj_Q_PIIVII %>% transmute(Year = Year, A = A, D = Sl * ((Ps/100)/phi) + Cl * ((Pc/100)/phi))

proj_Q_P_PostFMD_PESI_Dem_08 <- merge(proj_Q_P_PostFMD_PESI_08, Eq_proj_Q_P_Dem) %>% select(Year, A, D, A5, D5, A10, D10)

PostFMD_PESI_A_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_Dem_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = A5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = A5, color = "5% Depop"),size=2) +
  geom_line(aes(y = A10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = A10, color="10% Depop"),size=2) +
  geom_line(aes(y = A, color="Baseline"),size=1.1) +
  geom_point(aes(y = A, color = "Baseline"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Dem_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_Dem_08$Year[nrow(proj_Q_P_PostFMD_PESI_Dem_08)])))+ 
  scale_y_continuous(name="Demand(billion pounds)", limits = c(20,40,by=2))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

PostFMD_PESI_D_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_Dem_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = D5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = D5, color = "5% Depop"),size=2) +
  geom_line(aes(y = D10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = D10, color="10% Depop"),size=2) +
  geom_line(aes(y = D, color="Baseline"),size=1.1) +
  geom_point(aes(y = D, color = "Baseline"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P_PostFMD_PESI_Dem_08$Year[1],
                                  proj_Q_P_PostFMD_PESI_Dem_08$Year[nrow(proj_Q_P_PostFMD_PESI_Dem_08)])))+ 
  scale_y_continuous(name="Demand(billion dollars)", limits = c(40,60,by=0.05))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 15,face = "bold"),
        legend.background = element_rect(color = NA)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15, face = "bold"), 
        axis.text.y = element_text(size=15, face = "bold"), 
        axis.title=element_text(size=14)) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank()) +
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))

proj_Q_P_PostFMD_PESI_SH_08 <- merge(proj_Q_PIIVII, proj_Q_P_PostFMD_PESI_08) %>% select(Year, sh, sh5, sh10)

PostFMD_PESI_SH_ChangePlot_0820N <- round(proj_Q_P_PostFMD_PESI_SH_08,3) %>% ggplot(aes(x = Year))+
  geom_line(aes(y = sh5, color="5% Depop"),size=1.1) +
  geom_point(aes(y = sh5, color = "5% Depop"),size=2) +
  geom_line(aes(y = sh10, color="10% Depop"),size=1.1) +
  geom_point(aes(y = sh10, color="10% Depop"),size=2) +
  geom_line(aes(y = sh, color="Baseline"),size=1.1) +
  geom_point(aes(y = sh, color = "Baseline"),size=2) +
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
  scale_color_manual(breaks=c('5% Depop', '10% Depop','Baseline'),
                     values=c('5% Depop'='#619CFF', '10% Depop'='#F8766D','Baseline' = '#C77CFF'))


optimisticPostFMD_5_08_INV <- optimisticPostFMD_5_0909[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

optimisticPostFMD_10_08_INV <- optimisticPostFMD_10_0909[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

library(latexpdf)
as.pdf(x=optimisticPostFMD_5_08_INV, stem="OPT_INV_5", dir = "FMD-SimulationPlots/09-12")
as.pdf(x=optimisticPostFMD_10_08_INV, stem="OPT_INV_10", dir = "FMD-SimulationPlots/09-12")

optimisticPostFMD_5_08_TRADE <- trade_FORECAST_PostFMD_OPTI_08 %>%
  transmute(Year = Year, imports = round(Imports5), exports = round(Exports5), importsBeef = ImportsBeef5, exportsBeef = ExportsBeef5)

optimisticPostFMD_10_08_TRADE <- trade_FORECAST_PostFMD_OPTI_08 %>%
  transmute(Year = Year, imports = round(Imports10), exports = round(Exports10), importsBeef = ImportsBeef10, exportsBeef = ExportsBeef10)

as.pdf(x=optimisticPostFMD_5_08_TRADE, stem="OPT_TRADE_5", dir = "FMD-SimulationPlots/09-12")
as.pdf(x=optimisticPostFMD_10_08_TRADE, stem="OPT_TRADE_10", dir = "FMD-SimulationPlots/09-12")


pessimisticPostFMD_5_08_INV <- pessimisticPostFMD_5_0909[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

pessimisticPostFMD_10_08_INV <- pessimisticPostFMD_10_0909[[3]] %>% filter(Year >= 2022) %>%
  select(Year, K, k3, k4, k5, k6, k7, k8, k9, k10)

as.pdf(x=pessimisticPostFMD_5_08_INV, stem="PES_INV_5", dir = "FMD-SimulationPlots/09-12")
as.pdf(x=pessimisticPostFMD_10_08_INV, stem="PES_INV_10", dir = "FMD-SimulationPlots/09-12")


pessimisticPostFMD_5_08_TRADE <- trade_FORECAST_PostFMD_PESI_08 %>%
  transmute(Year = Year, imports = round(Imports5), exports = round(Exports5), importsBeef = ImportsBeef5, exportsBeef = ExportsBeef5)

pessimisticPostFMD_10_08_TRADE <- trade_FORECAST_PostFMD_PESI_08 %>%
  transmute(Year = Year, imports = round(Imports10), exports = round(Exports10), importsBeef = ImportsBeef10, exportsBeef = ExportsBeef10)

as.pdf(x=pessimisticPostFMD_5_08_TRADE, stem="PES_TRADE_5", dir = "FMD-SimulationPlots/09-12")
as.pdf(x=pessimisticPostFMD_10_08_TRADE, stem="PES_TRADE_10", dir = "FMD-SimulationPlots/09-12")




