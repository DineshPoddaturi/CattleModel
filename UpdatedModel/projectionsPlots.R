
####### Projection Plots


# proj_Q_PIIVII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)

proj_Q_PIIVIII <- proj_Q_P %>% mutate(Ps = Ps * 100, Pc = Pc * 100) %>% round(3) %>% filter(Ps>0)


EQestObsPS_Medians <- EQestObsPSNIII %>% select(Year, psMedian, ps) %>% filter(Year > 2015)

estProj_PSIIV <- merge(EQestObsPS_Medians %>% mutate(psMedian = psMedian*100),proj_Q_PIIVIII 
                       %>% select(Year, Ps), all=TRUE)


EQestObsPC_Medians <- EQestObsPCNIII %>% select(Year, pcMedian, pc) %>% filter(Year > 2015)

estProj_PCIIV <- merge(EQestObsPC_Medians %>% mutate(pcMedian = pcMedian*100),proj_Q_PIIVIII 
                       %>% select(Year, Pc), all=TRUE)


EQestObsSL_Medians <- EQestObsSLNIII %>% select(Year, slMedian, slSM) %>% filter(Year > 2015)

estProj_SLIIV <- merge(EQestObsSL_Medians ,proj_Q_PIIVIII %>% select(Year, Sl), all=TRUE)


EQestObsCL_Medians <- EQestObsCLNIII %>% select(Year, clMedian, clSM) %>% filter(Year > 2015)

estProj_CLIIV <- merge(EQestObsCL_Medians ,proj_Q_PIIVIII %>% select(Year, Cl), all=TRUE)



estProj_PS_plots <- estProj_PSIIV %>% filter(Year >=2016 & Year <= 2033) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="Baseline")) +
  geom_point(aes(y = psMedian, color = "Baseline")) +
  geom_line(aes(y=Ps, color="Projection")) +
  geom_point(aes(y=Ps, color="Projection")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIIV$Year[1],
                                  estProj_PSIIV$Year[nrow(estProj_PSIIV)])))+ 
  scale_y_continuous(name="Fed Cattle Price", limits = c(100,175,by=5)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



estProj_PC_plots <- estProj_PCIIV %>% filter(Year >=2016 & Year <= 2033) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=pcMedian, color="Baseline")) +
  geom_point(aes(y = pcMedian, color = "Baseline")) +
  geom_line(aes(y=Pc, color="Projection")) +
  geom_point(aes(y=Pc, color="Projection")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PCIIV$Year[1],
                                  estProj_PCIIV$Year[nrow(estProj_PCIIV)])))+ 
  scale_y_continuous(name="Cull Cow Price", limits = c(65,112,by=5)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


estProj_SL_plots <- estProj_SLIIV %>% filter(Year >=2016 & Year <= 2033) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y= slMedian, color="Baseline")) +
  geom_point(aes(y = slMedian, color = "Baseline")) +
  geom_line(aes(y=Sl, color="Projection")) +
  geom_point(aes(y=Sl, color="Projection")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_SLIIV$Year[1],
                                  estProj_SLIIV$Year[nrow(estProj_SLIIV)])))+ 
  scale_y_continuous(name="Fed Cattle Production", limits = c(19,26,by=5)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


estProj_CL_plots <- estProj_CLIIV %>% filter(Year >=2018 & Year <= 2033) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y= clMedian, color="Baseline")) +
  geom_point(aes(y = clMedian, color = "Baseline")) +
  geom_line(aes(y=Cl, color="Projection")) +
  geom_point(aes(y=Cl, color="Projection")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_CLIIV$Year[1],
                                  estProj_CLIIV$Year[nrow(estProj_CLIIV)])))+ 
  scale_y_continuous(name="Cull Cow Production", limits = c(1.0,5,by=1)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


#### USDA long term projections
USDA_Years <- c(seq(from = 2023, to = 2032, by = 1))


## I am taking Steers, 5-area projections from the document. These are in $/CWT
# USDA_Ps <- c(108.51, 121.06, 128.75, 134.94, 135.48, 137.24, 137.73, 138.08, 138.66, 139.63, 140.86, 142.55)
USDA_Ps <- c(153.50, 145.81, 139.23, 137.82, 138.95, 139.61, 140.68, 142.85, 145.50, 148.73)

#### I am taking the total production from USDA for our total supply numbers. This is in million pounds. 
#### I will convert them into billion pounds 
# USDA_TS <- c(27224, 27902, 27065, 26742, 26847, 27034, 27263, 27480, 27715, 27944, 28167, 28384)

USDA_TS <- c(26433, 26096, 27084, 27305, 27410, 27600, 27797, 27942, 28080, 28167)

USDA_TS <- USDA_TS/1000

USDA_Proj <- cbind(USDA_Years, USDA_Ps, USDA_TS) %>% as.data.frame()


#### FAPRI long term projections
FAPRI_Years <- c(seq(from = 2023, to = 2032, by = 1))

## I am taking Steers, 5-area projections from the document page 61. These are in $/CWT
# FAPRI_Ps <- c(122.40, 135.54, 141.05, 142.90, 144.81, 147.40, 148.39, 146.19, 143.46, 140.71, 138.39)

FAPRI_Ps <- c(155.51, 160.21, 163.37, 163.42, 158.88, 155.96, 153.77, 150.43, 147.69, 144.92)

#### I am taking the total production from FAPRI for our total supply numbers. This is in million pounds. 
#### I will convert them into billion pounds 
# FAPRI_TS <- c(28008, 27320, 26855, 26715, 26720, 26703, 26825, 27130, 27513, 27892, 28250)

FAPRI_TS <- c(26845, 25921, 25555, 25682, 25984, 26369, 26734, 27136, 27513, 27842)

FAPRI_TS <- FAPRI_TS/1000

FAPRI_Proj <- cbind(FAPRI_Years, FAPRI_Ps, FAPRI_TS) %>% as.data.frame()

FAPRI_Proj_Ps <- FAPRI_Proj %>% select(-FAPRI_TS) %>% transmute(Year = FAPRI_Years, FAPRI_Ps = FAPRI_Ps)
USDA_Proj_Ps <- USDA_Proj %>% select(-USDA_TS) %>% transmute(Year = USDA_Years, USDA_Ps = USDA_Ps)

estProj_PSIII_FAPRI <- merge(estProj_PSIIV,
                             merge(FAPRI_Proj_Ps, USDA_Proj_Ps) %>% filter(Year > 2021),
                             all=TRUE) %>% round(3)

estProj_PSIII_plots <- estProj_PSIII_FAPRI %>% filter(Year >=2016 & Year <= 2033) %>% ggplot(aes(x=Year)) +
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
  scale_y_continuous(name="Fed Cattle Price", limits = c(100,172,by=5)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


FAPRI_Proj_TSIII <- FAPRI_Proj %>% select(-FAPRI_Ps) %>% 
  transmute(Year = FAPRI_Years, FAPRI_TS = FAPRI_TS)  %>% filter(Year >2021)

USDA_Proj_TSIII <- USDA_Proj %>% select(-USDA_Ps) %>% 
  transmute(Year = USDA_Years, USDA_TS = USDA_TS) %>% filter(Year >2020)

EQestSl_Medians <- EQestObsSLNIII %>% filter(Year > 2015)

EQestCl_Medians <- EQestObsCLNIII %>% filter(Year > 2015)

estProj_SLIIVI <- merge(EQestSl_Medians, proj_Q_PIIVIII %>% select(Year, Sl, TP), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

estProj_CLIIVI <- merge(EQestCl_Medians, proj_Q_PIIVIII %>% select(Year, Cl, TP), all=TRUE) %>% round(2) %>% 
  filter(Year > 2015)

EQestObsTS_MediansIIV <- merge(estProj_SLIIVI %>% select(-errMean, -errmedian), 
                               estProj_CLIIVI %>% select(-errMean, -errmedian)) %>%
  transmute(Year = Year, tsMedian = slMedian + clMedian, 
            TS = Sl + Cl) %>% round(3)

# EQestObsTS_MediansIIV <- merge(estProj_SLIIVI %>% select(-errMean, -errmedian),
#                                estProj_CLIIVI %>% select(-errMean, -errmedian)) %>%
#   transmute(Year = Year, tsMedian = slMedian + clMedian,
#             TS = TP) %>% round(3)

CARD_USDA_FAPRI_TS_ProjIII <- left_join(left_join(EQestObsTS_MediansIIV, FAPRI_Proj_TSIII, by="Year"), 
                                        USDA_Proj_TSIII %>% filter(Year > 2021), by="Year") %>% round(3)

CARD_USDA_FAPRI_TS_ProjIII %>% mutate(diffFAPRI = TS - FAPRI_TS, diffUSDA = TS - USDA_TS)


CARD_USDA_FAPRI_TS_Proj_plotIII <- CARD_USDA_FAPRI_TS_ProjIII %>% filter(Year >=2016 & Year <= 2033) %>% ggplot(aes(x=Year))  + 
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
  scale_y_continuous(name="Total Production Projections", limits = c(20,30,by=5)) +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))





