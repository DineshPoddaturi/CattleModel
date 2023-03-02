# Plotting the shares

# mu_Tildes_eq[1:25,]
# s_Tildes_eq[1:25,]
# 
# prices_ps_eq[1:25,]
# prices_pc_eq[1:25,]
# 
# shares_eq <- ((exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,]))/
#     (1 + (exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,])))) 
# 
# shares_eqMean <- apply(shares_eq, 2, mean) %>% as.data.frame()
# names(shares_eqMean) <- "shareMean"
# shares_eqMean <- shares_eqMean %>% mutate(Year = quantities_prices_capK$Year) %>%
#   select(Year, everything())
# 
# shares_eqMeanPlot <- shares_eqMean %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMean, color="Share Mean"),size=1.1) +
#   geom_point(aes(y = shareMean, color = "Share Mean"),size=2) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(shares_eqMean$Year[1],shares_eqMean$Year[nrow(shares_eqMean)]))) +
#   scale_y_continuous(name="Share Mean")+ theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
#                                               axis.text.y = element_text(size = 12)) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# 
# shares_eqMed <- apply(shares_eq, 2, median) %>% as.data.frame() %>% round(3)
# names(shares_eqMed) <- "shareMedian"
# shares_eqMed <- shares_eqMed %>% mutate(Year = quantities_prices_capK$Year) %>% 
#   select(Year, everything())
# 
# shares_eqMedPlot <- shares_eqMed %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMedian, color="Share Median"),size=1.1) +
#   geom_point(aes(y = shareMedian, color = "Share Median"),size=2) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(shares_eqMed$Year[1],shares_eqMed$Year[nrow(shares_eqMed)]))) +
#   scale_y_continuous(name="Share Median")+ theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
#                                               axis.text.y = element_text(size = 12)) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



mu_Tildes_MMNII
s_Tildes_MMNII

EQestPSNIII
EQestPCNIII


sharesEq_Mean <- ((exp((mu_Tildes_MMNIII$muMean - ((EQestPSNIII$psMean/phi) - (EQestPCNIII$pcMean/phi)))/s_Tildes_MMNIII$sMean))/
                    (1 + (exp((mu_Tildes_MMNIII$muMean - ((EQestPSNIII$psMean/phi) - (EQestPCNIII$pcMean/phi)))/s_Tildes_MMNIII$sMean))))

sharesEq_Mean <- sharesEq_Mean %>% as.data.frame()
names(sharesEq_Mean) <- "shareMean"
sharesEq_Mean <- sharesEq_Mean %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

sharesEq_MeanPlot <- sharesEq_Mean %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMean, color="Share Mean"),size=1.1) +
  geom_point(aes(y = shareMean, color = "Share Mean"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(sharesEq_Mean$Year[1],sharesEq_Mean$Year[nrow(sharesEq_Mean)]))) +
  scale_y_continuous(name="Share Mean")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



muMeanFittedPlot <- mu_Tildes_MMNII %>% ggplot(aes(x=Year))+geom_line(aes(y=muMean, color="MU Mean"),size=1.1) +
  geom_point(aes(y = muMean, color = "MU Mean"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(mu_Tildes_MMNII$Year[1],mu_Tildes_MMNII$Year[nrow(mu_Tildes_MMNII)]))) +
  scale_y_continuous(name="MU Mean")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

muMedianFittedPlot <- mu_Tildes_MMNIII %>% ggplot(aes(x=Year))+geom_line(aes(y=muMedian, color="MU Median"),size=1.1) +
  geom_point(aes(y = muMedian, color = "MU Median"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(mu_Tildes_MMNII$Year[1],mu_Tildes_MMNII$Year[nrow(mu_Tildes_MMNII)]))) +
  scale_y_continuous(name="MU Median")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



sharesEq_Median <- ((exp((mu_Tildes_MMNIII$muMedian - ((EQestPSNIII$psMedian/phi) - (EQestPCNIII$pcMedian/phi)))/s_Tildes_MMNIII$sMedian))/
                    (1 + (exp((mu_Tildes_MMNIII$muMedian - ((EQestPSNIII$psMedian/phi) - (EQestPCNIII$pcMedian/phi)))/s_Tildes_MMNIII$sMedian))))

sharesEq_Median <- sharesEq_Median %>% as.data.frame()
names(sharesEq_Median) <- "shareMedian"
sharesEq_Median <- sharesEq_Median %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

sharesEq_MedianPlot <- sharesEq_Median %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMedian, color="Share Median"),size=1.1) +
  geom_point(aes(y = shareMedian, color = "Share Median"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(sharesEq_Median$Year[1],sharesEq_Median$Year[nrow(sharesEq_Median)]))) +
  scale_y_continuous(name="Share Median")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



# shares_eq <- ((exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,]))/
#                 (1 + (exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,])))) %>% as.data.frame() 
# 
# names(shares_eq) <- seq(from=1990, to=2020)
# 
# 
# shares_eq <- shares_eq %>% mutate(Id = rownames(shares_eq)) %>% select(Id, everything())
# 
# dfShares <- melt(shares_eq, id.vars = 'Id', variable.name = 'year')
# 
# ggplot(dfShares, aes (value)) +
#   geom_density() +
#   facet_wrap(~year)

proj_Q_P1 <- proj_Q_P %>% select(Year, Ps) %>% mutate(Ps = Ps * 100) %>% filter(Ps>0)
FAPRI_Proj1 <- FAPRI_Proj %>% select(FAPRI_Years, FAPRI_Ps) %>% mutate(Year = FAPRI_Years) %>% select(Year, FAPRI_Ps)

proj_Q_P2 <- proj_Q_P %>% select(Year, Ps) %>% mutate(Ps = Ps * 100) %>% filter(Ps>0)



USDA_Proj1 <- USDA_Proj %>% select(USDA_Years, USDA_Ps) %>% mutate(Year = USDA_Years) %>% select(Year, USDA_Ps)

mergedPS <- merge(proj_Q_P2, FAPRI_Proj1)

mergedPSPlot <- mergedPS %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(mergedPS$Year[1],
                                  mergedPS$Year[nrow(mergedPS)])))+ 
  scale_y_continuous(name="Fed Cattle Price Projections ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



proj_Q_PII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PIII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PII_FAPRI <- merge(proj_Q_PIII, merge(FAPRI_Proj1, USDA_Proj1)) %>% select(Year, Ps, FAPRI_Ps, USDA_Ps)

proj_Q_PII_FAPRI_PS <- proj_Q_PII_FAPRI %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(mergedPS$Year[1],
                                  mergedPS$Year[nrow(mergedPS)])))+ 
  scale_y_continuous(name="Fed Cattle Price Projections ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


shPlot_III <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=sh, color="Proj Share")) +
  geom_point(aes(y = sh, color = "Proj Share")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
  scale_y_continuous(name="Proj Share")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank())  + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

muPlot_III <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=muTilde, color="MU")) +
  geom_point(aes(y = muTilde, color = "MU")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
  scale_y_continuous(name="Mu Tilde")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


proj_Q_PIII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PIIV <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

estProj_PSIII <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),proj_Q_PIII %>% select(Year, Ps), all=TRUE)

estProj_PSIIV <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),proj_Q_PIIV %>% select(Year, Ps), all=TRUE)

estProj_PSIII_FAPRI <- merge(estProj_PSIIV,merge(FAPRI_Proj1, USDA_Proj1), all=TRUE) %>% round(2)

estProj_PSIII_plots <- estProj_PSIII_FAPRI %>% filter(Year >=2016 & Year < 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="PS Fitted")) +
  geom_point(aes(y = psMedian, color = "PS Fitted")) +
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_line(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  geom_point(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


estProj_PCIII <- merge(EQestObsPC_Medians %>% mutate(pcMedian = pcMedian*100),proj_Q_PIII %>% select(Year, Pc), all=TRUE) %>% round(2)

estProj_PCIII <- merge(EQestObsPC_Medians %>% mutate(pcMedian = pcMedian*100),proj_Q_PIIV %>% select(Year, Pc), all=TRUE) %>% round(2)

estProj_PCIII_plots <- estProj_PCIII %>% filter(Year >=2016 & Year < 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=pcMedian, color="PC Fitted")) +
  geom_point(aes(y = pcMedian, color = "PC Fitted")) +
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PCIII$Year[1],
                                  estProj_PCIII$Year[nrow(estProj_PCIII)])))+ 
  scale_y_continuous(name="Cull Cow Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

estProj_SLIII <- merge(EQestSl_Medians, proj_Q_PIII %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_SLIIV <- merge(EQestSl_Medians, proj_Q_PIIV %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_SLIII_plots <- estProj_SLIII %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=slMedian, color="SL Fitted")) +
  geom_point(aes(y = slMedian, color = "SL Fitted")) +
  geom_line(aes(y=Sl, color="SL PROJECTION")) +
  geom_point(aes(y=Sl, color="SL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_SLIII$Year[1],
                                  estProj_SLIII$Year[nrow(estProj_SLIII)])))+ 
  scale_y_continuous(name="Fed Cattle Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

estProj_CLIII <- merge(EQestCl_Medians, proj_Q_PIII %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_CLIIV <- merge(EQestCl_Medians, proj_Q_PIIV %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)


estProj_CLIII_plots <- estProj_CLIIV %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=clMedian, color="CL Fitted")) +
  geom_point(aes(y = clMedian, color = "CL Fitted")) +
  geom_line(aes(y=Cl, color="CL PROJECTION")) +
  geom_point(aes(y=Cl, color="CL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_CLIII$Year[1],
                                  estProj_CLIII$Year[nrow(estProj_CLIII)])))+ 
  scale_y_continuous(name="Cull Cow Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



# proj_Q_PIIV <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

# proj_Q_PIIVI <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PIIVII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

EQestObsPS_Medians <- EQestObsPSNIII %>% select(Year, psMedian, ps) %>% filter(Year > 2015)

estProj_PSIIV <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),proj_Q_PIIVII %>% select(Year, Ps), all=TRUE)

estProj_PSIII_FAPRI <- merge(estProj_PSIIV,
                             merge(FAPRI_Proj1, USDA_Proj1) %>% filter(Year > 2021),
                             all=TRUE) %>% round(3)

estProj_PSIII_plots <- estProj_PSIII_FAPRI %>% filter(Year >=2016 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="PS Fitted")) +
  geom_point(aes(y = psMedian, color = "PS Fitted")) +
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_line(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  geom_point(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

FAPRI_Proj_TSIII <- FAPRI_Proj %>% select(-FAPRI_Ps) %>% 
  transmute(Year = FAPRI_Years, FAPRI_TS = FAPRI_TS)  %>% filter(Year >2021)

USDA_Proj_TSIII <- USDA_Proj %>% select(-USDA_Ps) %>% 
  transmute(Year = USDA_Years, USDA_TS = USDA_TS) %>% filter(Year >2020)

# estProj_SLIIV <- merge(EQestSl_Medians, proj_Q_PIIV %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
#   filter(Year > 2015)
# 
# estProj_CLIIV <- merge(EQestCl_Medians, proj_Q_PIIV %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
#   filter(Year > 2015)

EQestSl_Medians <- EQestObsSLNIII %>% filter(Year > 2015)

EQestCl_Medians <- EQestObsCLNIII %>% filter(Year > 2015)

estProj_SLIIVI <- merge(EQestSl_Medians, proj_Q_PIIVII %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_CLIIVI <- merge(EQestCl_Medians, proj_Q_PIIVII %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

EQestObsTS_MediansIIV <- merge(estProj_SLIIVI %>% select(-errMean, -errmedian), 
                                  estProj_CLIIVI %>% select(-errMean, -errmedian)) %>%
  transmute(Year = Year, tsMedian = slMedian + clMedian, 
            TS = Sl + Cl) %>% round(3)

CARD_USDA_FAPRI_TS_ProjIII <- left_join(left_join(EQestObsTS_MediansIIV, FAPRI_Proj_TSIII, by="Year"), 
                                      USDA_Proj_TSIII %>% filter(Year > 2021), by="Year") %>% round(2)
CARD_USDA_FAPRI_TS_ProjIII

CARD_USDA_FAPRI_TS_Proj_plotIII <- CARD_USDA_FAPRI_TS_ProjIII %>% ggplot(aes(x=Year))  + 
  geom_line(aes(y=tsMedian, color="Baseline")) + 
  geom_point(aes(y=tsMedian, color="Baseline")) + 
  geom_line(aes(y=TS, color="Projected")) + 
  geom_point(aes(y=TS, color="Projected")) + 
  geom_line(aes(y=FAPRI_TS, color="FAPRI Projection"))  + 
  geom_point(aes(y=FAPRI_TS, color="FAPRI Projection")) +
  geom_line(aes(y=USDA_TS, color="USDA Projection"))  + 
  geom_point(aes(y=USDA_TS, color="USDA Projection"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_ProjIII$Year[1],
                                  CARD_USDA_FAPRI_TS_ProjIII$Year[nrow(CARD_USDA_FAPRI_TS_ProjIII)])))+ 
  scale_y_continuous(name="Total Production Projections ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))






estProj_AIII <- merge(proj_AllDF_EQ %>% select(Year, A), proj_Q_PIII %>% mutate(A_proj = A) %>% select(Year, A_proj)
                       , all=TRUE) %>% round(2) %>% filter(Year > 2015)

estProj_AIII_plots <- estProj_AIII %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=A, color="A Fitted")) +
  geom_point(aes(y = A, color = "A Fitted")) +
  geom_line(aes(y=A_proj, color="A PROJECTION")) +
  geom_point(aes(y=A_proj, color="A PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_AIII$Year[1],
                                  estProj_AIII$Year[nrow(estProj_AIII)])))+ 
  scale_y_continuous(name="Demand for Meat") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


proj_Q_PV <- proj_Q_P
proj_Q_PVI <- proj_Q_P

# k9=0, shocks = hist, Qs, Qs replacement heifers, psNew_lo <- psNew  - 0.05
#pcNew_lo <- pcNew - 0.05, psNew_up <- psNew + 0.1, pcNew_up <- pcNew + 0.09
proj_Q_PVII <- proj_Q_P



# shPlot_IV <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=sh, color="Proj Share")) +
#   geom_point(aes(y = sh, color = "Proj Share")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
#   scale_y_continuous(name="Proj Share")+ theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank())  + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# muPlot_IV <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=muTilde, color="MU")) +
#   geom_point(aes(y = muTilde, color = "MU")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
#   scale_y_continuous(name="Mu Tilde")+ theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# 
# proj_Q_PIV <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)
# 
# estProj_PSIV <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),proj_Q_PIV %>% select(Year, Ps), all=TRUE)
# 
# estProj_PSIV_FAPRI <- merge(estProj_PSIV,merge(FAPRI_Proj1, USDA_Proj1), all=TRUE) %>% round(2)
# 
# estProj_PSIV_plots <- estProj_PSIV_FAPRI %>% filter(Year >=2016 & Year < 2031) %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=psMedian, color="PS Fitted")) +
#   geom_point(aes(y = psMedian, color = "PS Fitted")) +
#   geom_line(aes(y=Ps, color="PS PROJECTION")) +
#   geom_point(aes(y=Ps, color="PS PROJECTION")) +
#   geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
#   geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
#   geom_line(aes(y=USDA_Ps, color="USDA PROJECTION")) +
#   geom_point(aes(y=USDA_Ps, color="USDA PROJECTION")) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(estProj_PSIV_FAPRI$Year[1],
#                                   estProj_PSIV_FAPRI$Year[nrow(estProj_PSIV_FAPRI)])))+ 
#   scale_y_continuous(name="Fed Cattle Price ") +  theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# estProj_PCIV <- merge(EQestObsPC_Medians %>% mutate(pcMedian = pcMedian*100),proj_Q_PIV %>% select(Year, Pc), all=TRUE) %>% round(2)
# 
# estProj_PCIV_plots <- estProj_PCIV %>% filter(Year >=2016 & Year < 2031) %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=pcMedian, color="PC Fitted")) +
#   geom_point(aes(y = pcMedian, color = "PC Fitted")) +
#   geom_line(aes(y=Pc, color="PC PROJECTION")) +
#   geom_point(aes(y=Pc, color="PC PROJECTION"))+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(estProj_PCIV$Year[1],
#                                   estProj_PCIV$Year[nrow(estProj_PCIV)])))+ 
#   scale_y_continuous(name="Cull Cow Price ") +  theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# estProj_SLIV <- merge(EQestSl_Medians, proj_Q_PIV %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
#   filter(Year > 2015)
# 
# estProj_SLIV_plots <- estProj_SLIV %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=slMedian, color="SL Fitted")) +
#   geom_point(aes(y = slMedian, color = "SL Fitted")) +
#   geom_line(aes(y=Sl, color="SL PROJECTION")) +
#   geom_point(aes(y=Sl, color="SL PROJECTION"))+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(estProj_SLIV$Year[1],
#                                   estProj_SLIV$Year[nrow(estProj_SLIV)])))+ 
#   scale_y_continuous(name="Fed Cattle Production") +  theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# estProj_CLIV <- merge(EQestCl_Medians, proj_Q_PIV %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
#   filter(Year > 2015)
# 
# estProj_CLIV_plots <- estProj_CLIV %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=clMedian, color="CL Fitted")) +
#   geom_point(aes(y = clMedian, color = "CL Fitted")) +
#   geom_line(aes(y=Cl, color="CL PROJECTION")) +
#   geom_point(aes(y=Cl, color="CL PROJECTION"))+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(estProj_CLIV$Year[1],
#                                   estProj_CLIV$Year[nrow(estProj_CLIV)])))+ 
#   scale_y_continuous(name="Cull Cow Production") +  theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# 
# estProj_AIV <- merge(proj_AllDF_EQ %>% select(Year, A), proj_Q_PIV %>% mutate(A_proj = A) %>% select(Year, A_proj)
#                       , all=TRUE) %>% round(2) %>% filter(Year > 2015)
# 
# estProj_AIV_plots <- estProj_AIV %>% ggplot(aes(x=Year)) +
#   geom_line(aes(y=A, color="A Fitted")) +
#   geom_point(aes(y = A, color = "A Fitted")) +
#   geom_line(aes(y=A_proj, color="A PROJECTION")) +
#   geom_point(aes(y=A_proj, color="A PROJECTION"))+
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(estProj_AIV$Year[1],
#                                   estProj_AIV$Year[nrow(estProj_AIV)])))+ 
#   scale_y_continuous(name="Demand for Meat") +  theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
#   theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


mergedForecast_ProjV <- mergedForecast_Proj

shPlot_V <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=sh, color="Proj Share")) +
  geom_point(aes(y = sh, color = "Proj Share")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
  scale_y_continuous(name="Proj Share")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank())  + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

muPlot_V <- proj_Q_P %>% filter(Ps > 0) %>% ggplot(aes(x=Year))+geom_line(aes(y=muTilde, color="MU")) +
  geom_point(aes(y = muTilde, color = "MU")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(proj_Q_P$Year[1],proj_Q_P$Year[nrow(proj_Q_P)]))) +
  scale_y_continuous(name="Mu Tilde")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


proj_Q_PV <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100, Hc = Hc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PIIVI

estProj_PSV <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),
                     proj_Q_PIIVI %>% select(Year, Ps), all=TRUE)

estProj_PSV_FAPRI <- merge(estProj_PSV,merge(FAPRI_Proj1, USDA_Proj1), all=TRUE) %>% round(2)

estProj_PSV_plots <- estProj_PSV_FAPRI %>% filter(Year >=2016 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="PS Fitted")) +
  geom_point(aes(y = psMedian, color = "PS Fitted")) +
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION")) +
  geom_line(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  geom_point(aes(y=USDA_Ps, color="USDA PROJECTION")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSV_FAPRI$Year[1],
                                  estProj_PSV_FAPRI$Year[nrow(estProj_PSV_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

estProj_PCV <- merge(EQestObsPC_Medians %>% mutate(pcMedian = pcMedian*100),
                     proj_Q_PIIVI %>% select(Year, Pc), all=TRUE) %>% round(2)

estProj_PCV_plots <- estProj_PCV %>% filter(Year >=2016 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=pcMedian, color="PC Fitted")) +
  geom_point(aes(y = pcMedian, color = "PC Fitted")) +
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PCV$Year[1],
                                  estProj_PCV$Year[nrow(estProj_PCV)])))+ 
  scale_y_continuous(name="Cull Cow Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


EQestHC_Medians <- EQestHCNII  %>% select(Year, hcMedian)

estProj_HCV <- merge(EQestHC_Medians %>% mutate(hcMedian = hcMedian*100),proj_Q_PV %>% select(Year, Hc), all=TRUE) %>% round(2)

estProj_HCV_plots <- estProj_HCV %>% filter(Year >=2016 & Year < 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=hcMedian, color="HC Fitted")) +
  geom_point(aes(y = hcMedian, color = "HC Fitted")) +
  geom_line(aes(y=Hc, color="HC PROJECTION")) +
  geom_point(aes(y=Hc, color="HC PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_HCV$Year[1],
                                  estProj_HCV$Year[nrow(estProj_HCV)])))+ 
  scale_y_continuous(name="Holding Costs") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

estProj_SLV <- merge(EQestSl_Medians, proj_Q_PIIVI %>% select(Year, Sl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_SLV_plots <- estProj_SLV %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=slMedian, color="SL Fitted")) +
  geom_point(aes(y = slMedian, color = "SL Fitted")) +
  geom_line(aes(y=Sl, color="SL PROJECTION")) +
  geom_point(aes(y=Sl, color="SL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_SLV$Year[1],
                                  estProj_SLV$Year[nrow(estProj_SLV)])))+ 
  scale_y_continuous(name="Fed Cattle Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

estProj_CLV <- merge(EQestCl_Medians, proj_Q_PIIVI %>% select(Year, Cl), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_CLV_plots <- estProj_CLV %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=clMedian, color="CL Fitted")) +
  geom_point(aes(y = clMedian, color = "CL Fitted")) +
  geom_line(aes(y=Cl, color="CL PROJECTION")) +
  geom_point(aes(y=Cl, color="CL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_CLV$Year[1],
                                  estProj_CLV$Year[nrow(estProj_CLV)])))+ 
  scale_y_continuous(name="Cull Cow Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


estProj_AV <- merge(proj_AllDF_EQ %>% select(Year, A), proj_Q_PV %>% mutate(A_proj = A) %>% select(Year, A_proj)
                     , all=TRUE) %>% round(2) %>% filter(Year > 2015)

estProj_AV_plots <- estProj_AV %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=A, color="A Fitted")) +
  geom_point(aes(y = A, color = "A Fitted")) +
  geom_line(aes(y=A_proj, color="A PROJECTION")) +
  geom_point(aes(y=A_proj, color="A PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_AV$Year[1],
                                  estProj_AV$Year[nrow(estProj_AV)])))+ 
  scale_y_continuous(name="Demand for Meat") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))















