
EQestObsPS_Medians <- EQestObsPS %>% select(Year, psMedian, ps) %>% filter(Year >2009)

PQs_MEDIANS_proj <- PQs_MEDIANS %>% select(Year, Ps_lo, Ps, Ps_up)

EQestObsPS_Medians_proj <- merge(EQestObsPS_Medians,PQs_MEDIANS_proj,by="Year",all=TRUE)


EQestObsPS_Medians_proj_plot <- EQestObsPS_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
  geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Medians_proj$Year[1],
                                  EQestObsPS_Medians_proj$Year[nrow(EQestObsPS_Medians_proj)]))) +
  expand_limits(y = 0.5)


PQs_MEDIANS_proj_EP <- PQs_MEDIANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)

EQestObsPS_Medians_proj_EP <- merge(EQestObsPS_Medians,PQs_MEDIANS_proj_EP,by="Year",all=TRUE)


EQestObsPS_Medians_projEP_plot <- EQestObsPS_Medians_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
  geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Medians_proj$Year[1],
                                  EQestObsPS_Medians_proj$Year[nrow(EQestObsPS_Medians_proj)]))) +
  expand_limits(y = 0.5)



EQestObsPC_Medians <- EQestObsPC %>% select(Year, pcMedian, pc) %>% filter(Year >2009)

PQs_MEDIANS_proj <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Medians_proj <- merge(EQestObsPC_Medians,PQs_MEDIANS_proj,by="Year",all=TRUE)


EQestObsPC_Medians_proj_plot <- EQestObsPC_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Medians_proj$Year[1],
                                  EQestObsPC_Medians_proj$Year[nrow(EQestObsPC_Medians_proj)]))) +
  expand_limits(y = 0.5)


PQs_MEDIANS_proj_EP <- PQs_MEDIANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Medians_proj_EP <- merge(EQestObsPC_Medians,PQs_MEDIANS_proj_EP,by="Year",all=TRUE)


EQestObsPC_Medians_projEP_plot <- EQestObsPC_Medians_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Medians_proj_EP$Year[1],
                                  EQestObsPC_Medians_proj_EP$Year[nrow(EQestObsPC_Medians_proj_EP)]))) +
  expand_limits(y = 0.5)











EQestObsPS_Means <- EQestObsPS %>% select(Year, psMean, ps) %>% filter(Year >2009)

PQs_MEANS_proj <- PQs_MEANS %>% select(Year, Ps_lo, Ps, Ps_up)

EQestObsPS_Means_proj <- merge(EQestObsPS_Means,PQs_MEANS_proj,by="Year",all=TRUE)


EQestObsPS_Means_proj_plot <- EQestObsPS_Means_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
  geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Means_proj$Year[1],
                                  EQestObsPS_Means_proj$Year[nrow(EQestObsPS_Means_proj)]))) +
  expand_limits(y = 0.5)


PQs_MEANS_proj_EP <- PQs_MEANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)

EQestObsPS_Means_proj_EP <- merge(EQestObsPS_Means,PQs_MEANS_proj_EP,by="Year",all=TRUE)


EQestObsPS_Means_projEP_plot <- EQestObsPS_Means_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
  geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
  geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Means_proj_EP$Year[1],
                                  EQestObsPS_Means_proj_EP$Year[nrow(EQestObsPS_Means_proj_EP)]))) +
  expand_limits(y = 0.5)



EQestObsPC_Means <- EQestObsPC %>% select(Year, pcMean, pc) %>% filter(Year >2009)

PQs_MEANS_proj <- PQs_MEANS %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Means_proj <- merge(EQestObsPC_Means,PQs_MEANS_proj,by="Year",all=TRUE)


EQestObsPC_Means_proj_plot <- EQestObsPC_Means_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Means_proj$Year[1],
                                  EQestObsPC_Means_proj$Year[nrow(EQestObsPC_Means_proj)]))) +
  expand_limits(y = 0.5)


PQs_MEANS_proj_EP <- PQs_MEANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Means_proj_EP <- merge(EQestObsPC_Means,PQs_MEANS_proj_EP,by="Year",all=TRUE)


EQestObsPC_Means_projEP_plot <- EQestObsPC_Means_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
  geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + 
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Means_proj_EP$Year[1],
                                  EQestObsPC_Means_proj_EP$Year[nrow(EQestObsPC_Means_proj_EP)]))) +
  expand_limits(y = 0.5)




