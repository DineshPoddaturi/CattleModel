# Here I am plotting the projections with USDA long term and FAPRI lon g term projections


#### USDA long term projections
USDA_Years <- c(seq(from = 2019, to = 2030, by = 1))

## I am taking Steers, 5-area projections from the document page 44. These are in $/CWT
USDA_Ps <- c(116.78, 108.71, 114.00, 120.99, 117.39, 116.52, 114.61, 112.44, 110.22, 108.14, 106.25, 104.61)

#### I am taking the total production from USDA for our total supply numbers. This is in million pounds. 
#### I will convert them into billion pounds 
USDA_TS <- c(27224, 27207, 27434, 26985, 27315, 27523, 27835, 28143, 28453, 28749, 29026, 29281)

USDA_TS <- USDA_TS/1000

USDA_Proj <- cbind(USDA_Years, USDA_Ps, USDA_TS) %>% as.data.frame()



#### FAPRI long term projections
FAPRI_Years <- c(seq(from = 2019, to = 2030, by = 1))

## I am taking Steers, 5-area projections from the document page 44. These are in $/CWT
FAPRI_Ps <- c(116.78, 108.51, 121.55, 126.70, 129.25, 132.90, 135.76, 137.14, 136.55, 134.61, 133.34, 132.44)

#### I am taking the total production from USDA for our total supply numbers. This is in million pounds. 
#### I will convert them into billion pounds 
FAPRI_TS <- c(27224, 27244, 27549, 27334, 27100, 26904, 26757, 26756, 27067, 27329, 27638, 27942)

FAPRI_TS <- FAPRI_TS/1000

FAPRI_Proj <- cbind(FAPRI_Years, FAPRI_Ps, FAPRI_TS) %>% as.data.frame()



FAPRI_Proj_Ps <- FAPRI_Proj %>% select(-FAPRI_TS) %>% transmute(Year = FAPRI_Years, FAPRI_Ps = FAPRI_Ps)
USDA_Proj_Ps <- USDA_Proj %>% select(-USDA_TS) %>% transmute(Year = USDA_Years, USDA_Ps = USDA_Ps)

CARD_USDA_FAPRI_PS_Proj <- merge(merge(EQestObsPS_Medians_proj, FAPRI_Proj_Ps, by="Year", all=TRUE), 
      USDA_Proj_Ps, by="Year", all=TRUE) %>% mutate(psMedian = psMedian * 100, ps = ps * 100,
                                                    Ps_lo = Ps_lo * 100, Ps = Ps * 100, Ps_up = Ps_up * 100)

CARD_USDA_FAPRI_PS_Proj_Plot <- CARD_USDA_FAPRI_PS_Proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) +  
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) + geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) + geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + geom_line(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI PROJECTION"))  + geom_line(aes(y=USDA_Ps, color="USDA PROJECTION"))  +
  geom_point(aes(y=USDA_Ps, color="USDA PROJECTION"))  + geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", breaks=c(seq(CARD_USDA_FAPRI_PS_Proj$Year[1],
                                  CARD_USDA_FAPRI_PS_Proj$Year[nrow(CARD_USDA_FAPRI_PS_Proj)])))



FAPRI_Proj_TS <- FAPRI_Proj %>% select(-FAPRI_Ps) %>% transmute(Year = FAPRI_Years, FAPRI_TS = FAPRI_TS)
USDA_Proj_TS <- USDA_Proj %>% select(-USDA_Ps) %>% transmute(Year = USDA_Years, USDA_TS = USDA_TS)

CARD_USDA_FAPRI_TS_Proj <- merge(merge(EQestObsA_Medians_proj, FAPRI_Proj_TS, by="Year", all=TRUE), 
                                 USDA_Proj_TS, by="Year", all=TRUE)

CARD_USDA_FAPRI_TS_Proj_plot <- CARD_USDA_FAPRI_TS_Proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=A, color = "A OBS")) + geom_point(aes(y=A, color = "A OBS")) + 
  geom_line(aes(y=AMedian, color="A RATIONAL (MEDIAN)")) + geom_point(aes(y = AMedian, color = "A RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=A_lo, color="A_LO PROJECTION")) + geom_point(aes(y=A_lo, color="A_LO PROJECTION")) + geom_line(aes(y=A_proj, color="A PROJECTION")) + 
  geom_point(aes(y=A_proj, color="A PROJECTION")) + geom_line(aes(y=FAPRI_TS, color="FAPRI PROJECTION"))  +
  geom_point(aes(y=FAPRI_TS, color="FAPRI PROJECTION"))  +geom_line(aes(y=USDA_TS, color="USDA PROJECTION"))  +
  geom_point(aes(y=USDA_TS, color="USDA PROJECTION"))  + geom_line(aes(y=A_up, color="A_UP PROJECTION"))  +
  geom_point(aes(y=A_up, color="A_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_Proj$Year[1],
                                  CARD_USDA_FAPRI_TS_Proj$Year[nrow(CARD_USDA_FAPRI_TS_Proj)])))





EQestObsSL1 <- EQestObsSL %>% select(Year, slMedian)
EQestObsCL1 <- EQestObsCL %>% select(Year, clMedian)

EQestA_Medians_temp <- merge(EQestObsSL1,EQestObsCL1)  %>% mutate(AMedian = (slMedian + clMedian)) 

EQestObsA_Medians_temp <- merge(EQestA_Medians_temp, A_quant) %>% select(Year, AMedian, A) %>% filter(Year > 2009)

EQestObsA_Medians_proj_temp <- merge(EQestObsCL_Medians_proj, EQestObsSL_Medians_proj) %>% transmute(Year = Year, A_lo = Cl_lo + Sl_lo,
                                                                                                     A_proj = Cl + Sl,
                                                                                                     A_up = Cl_up + Sl_up)

EQestObsA_Medians_proj_temp <- merge(EQestObsA_Medians_temp, EQestObsA_Medians_proj_temp, by = "Year", all=TRUE)

CARD_USDA_FAPRI_TS_Proj_temp <- merge(merge(EQestObsA_Medians_proj_temp, FAPRI_Proj_TS, by="Year", all=TRUE), 
                                 USDA_Proj_TS, by="Year", all=TRUE)

CARD_USDA_FAPRI_TS_Proj_plot_temp <- CARD_USDA_FAPRI_TS_Proj_temp %>% ggplot(aes(x=Year)) +geom_line(aes(y=A, color = "A OBS")) + geom_point(aes(y=A, color = "A OBS")) + 
  geom_line(aes(y=AMedian, color="A RATIONAL (MEDIAN)")) + geom_point(aes(y = AMedian, color = "A RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=A_lo, color="A_LO PROJECTION")) + geom_point(aes(y=A_lo, color="A_LO PROJECTION")) + geom_line(aes(y=A_proj, color="A PROJECTION")) + 
  geom_point(aes(y=A_proj, color="A PROJECTION")) + geom_line(aes(y=FAPRI_TS, color="FAPRI PROJECTION"))  +
  geom_point(aes(y=FAPRI_TS, color="FAPRI PROJECTION"))  +geom_line(aes(y=USDA_TS, color="USDA PROJECTION"))  +
  geom_point(aes(y=USDA_TS, color="USDA PROJECTION"))  + geom_line(aes(y=A_up, color="A_UP PROJECTION"))  +
  geom_point(aes(y=A_up, color="A_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_Proj_temp$Year[1],
                                  CARD_USDA_FAPRI_TS_Proj_temp$Year[nrow(CARD_USDA_FAPRI_TS_Proj_temp)])))


