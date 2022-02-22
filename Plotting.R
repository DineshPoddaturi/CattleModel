
EQestObsPS_Medians <- EQestObsPS %>% select(Year, psMedian, ps) %>% filter(Year > 2009)

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
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Medians_proj$Year[1],
                                  EQestObsPS_Medians_proj$Year[nrow(EQestObsPS_Medians_proj)])))


EQestObsPC_Medians <- EQestObsPC %>% select(Year, pcMedian, pc) %>% filter(Year > 2009)

PQs_MEDIANS_proj <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Medians_proj <- merge(EQestObsPC_Medians,PQs_MEDIANS_proj,by="Year",all=TRUE)


EQestObsPC_Medians_proj_plot <- EQestObsPC_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) +
  geom_line(aes(y=Pc_up, color="PC_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PC_LO PROJECTION")) +
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Medians_proj$Year[1],
                                  EQestObsPC_Medians_proj$Year[nrow(EQestObsPC_Medians_proj)])))


EQestObsSL1 <- EQestObsSL %>% select(Year, slMedian)
EQestObsCL1 <- EQestObsCL %>% select(Year, clMedian)

EQestA_Medians <- merge(EQestObsSL1,EQestObsCL1)  %>% mutate(AMedian = (slMedian + clMedian) * (1/adjFac)) 

EQestObsA_Medians <- merge(EQestA_Medians, A_quant) %>% select(Year, AMedian, A) %>% filter(Year > 2009)

PQs_MEDIANS_A_proj <- PQs_MEDIANS %>% transmute(Year = Year, A_lo, A_proj = A, A_up)

EQestObsA_Medians_proj <- merge(EQestObsA_Medians,PQs_MEDIANS_A_proj,by="Year",all=TRUE)

EQestObsA_Medians_proj_plot <- EQestObsA_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=A, color = "A OBS")) + 
  geom_point(aes(y=A, color = "A OBS")) + geom_line(aes(y=AMedian, color="A RATIONAL (MEDIAN)")) +
  geom_point(aes(y = AMedian, color = "A RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=A_lo, color="A_LO PROJECTION")) +
  geom_point(aes(y=A_lo, color="A_LO PROJECTION")) + 
  geom_line(aes(y=A_proj, color="A PROJECTION")) +
  geom_point(aes(y=A_proj, color="A PROJECTION")) + 
  geom_line(aes(y=A_up, color="A_UP PROJECTION"))  +
  geom_point(aes(y=A_up, color="A_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsA_Medians_proj$Year[1],
                                  EQestObsA_Medians_proj$Year[nrow(EQestObsA_Medians_proj)])))

EQestSl_Medians <- EQestObsSL1
supp_sl_OBS <- supp_sl_adj %>% select(Year, SlObs = Bill_meatLb_sl)
EQestObsSL_Medians <- merge(EQestSl_Medians, supp_sl_OBS) %>% filter(Year > 2009)

PQs_MEDIANS_SL_proj <- PQs_MEDIANS %>% transmute(Year = Year, Sl_lo, Sl, Sl_up)

EQestObsSL_Medians_proj <- merge(EQestObsSL_Medians,PQs_MEDIANS_SL_proj,by="Year",all=TRUE)

EQestObsSL_Medians_proj_plot <- EQestObsSL_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=SlObs, color = "SL OBS")) + 
  geom_point(aes(y=SlObs, color = "SL OBS")) + geom_line(aes(y=slMedian, color="SL RATIONAL (MEDIAN)")) +
  geom_point(aes(y = slMedian, color = "SL RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Sl_lo, color="SL_LO PROJECTION")) +
  geom_point(aes(y=Sl_lo, color="SL_LO PROJECTION")) + 
  geom_line(aes(y=Sl, color="SL PROJECTION")) +
  geom_point(aes(y=Sl, color="SL PROJECTION")) + 
  geom_line(aes(y=Sl_up, color="SL_UP PROJECTION"))  +
  geom_point(aes(y=Sl_up, color="SL_UP PROJECTION")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsSL_Medians_proj$Year[1],
                                  EQestObsSL_Medians_proj$Year[nrow(EQestObsSL_Medians_proj)])))

EQestCl_Medians <- EQestObsCL1
supp_cl_OBS <- supp_cl_adj %>% select(Year, ClObs = Bill_meatLb_cl)
EQestObsCL_Medians <- merge(EQestCl_Medians, supp_cl_OBS) %>% filter(Year > 2009)

PQs_MEDIANS_CL_proj <- PQs_MEDIANS %>% transmute(Year = Year, Cl_lo, Cl, Cl_up)

EQestObsCL_Medians_proj <- merge(EQestObsCL_Medians,PQs_MEDIANS_CL_proj,by="Year",all=TRUE)

EQestObsCL_Medians_proj_plot <- EQestObsCL_Medians_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=ClObs, color = "CL OBS")) + 
  geom_point(aes(y=ClObs, color = "CL OBS")) + geom_line(aes(y=clMedian, color="CL RATIONAL (MEDIAN)")) +
  geom_point(aes(y = clMedian, color = "CL RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Cl_lo, color="CL_LO PROJECTION")) +
  geom_point(aes(y=Cl_lo, color="CL_LO PROJECTION")) + 
  geom_line(aes(y=Cl, color="CL PROJECTION")) +
  geom_point(aes(y=Cl, color="CL PROJECTION")) + 
  geom_line(aes(y=Cl_up, color="CL_UP PROJECTION"))  +
  geom_point(aes(y=Cl_up, color="CL_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsCL_Medians_proj$Year[1],
                                  EQestObsCL_Medians_proj$Year[nrow(EQestObsCL_Medians_proj)])))



EQestObsPS_Medians_proj_plot

EQestObsPC_Medians_proj_plot

EQestObsA_Medians_proj_plot

EQestObsSL_Medians_proj_plot

EQestObsCL_Medians_proj_plot





projections_BKP <- cbind(EQestObsPS_Medians_proj, EQestObsPC_Medians_proj, EQestObsA_Medians_proj, EQestObsSL_Medians_proj,
                         EQestObsCL_Medians_proj)

projectionsPlots_BKP <- c(EQestObsPS_Medians_proj_plot, EQestObsPC_Medians_proj_plot, EQestObsA_Medians_proj_plot, 
                          EQestObsSL_Medians_proj_plot, EQestObsCL_Medians_proj_plot)


# PQs_MEDIANS_proj_EP <- PQs_MEDIANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)
# 
# EQestObsPS_Medians_proj_EP <- merge(EQestObsPS_Medians,PQs_MEDIANS_proj_EP,by="Year",all=TRUE)
# 
# 
# EQestObsPS_Medians_projEP_plot <- EQestObsPS_Medians_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + 
#   geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
#   geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
#   geom_line(aes(y=Ps, color="PS PROJECTION")) +
#   geom_point(aes(y=Ps, color="PS PROJECTION")) + 
#   geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
#   geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPS_Medians_proj$Year[1],
#                                   EQestObsPS_Medians_proj$Year[nrow(EQestObsPS_Medians_proj)]))) +
#   expand_limits(y = 0.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# PQs_MEDIANS_proj_EP <- PQs_MEDIANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)
# 
# EQestObsPC_Medians_proj_EP <- merge(EQestObsPC_Medians,PQs_MEDIANS_proj_EP,by="Year",all=TRUE)
# 
# 
# EQestObsPC_Medians_projEP_plot <- EQestObsPC_Medians_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
#   geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) + 
#   geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
#   geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
#   geom_line(aes(y=Pc, color="PC PROJECTION")) +
#   geom_point(aes(y=Pc, color="PC PROJECTION")) + 
#   geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
#   geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPC_Medians_proj_EP$Year[1],
#                                   EQestObsPC_Medians_proj_EP$Year[nrow(EQestObsPC_Medians_proj_EP)]))) +
#   expand_limits(y = 0.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# EQestObsPS_Means <- EQestObsPS %>% select(Year, psMean, ps) %>% filter(Year >2009)
# 
# PQs_MEANS_proj <- PQs_MEANS %>% select(Year, Ps_lo, Ps, Ps_up)
# 
# EQestObsPS_Means_proj <- merge(EQestObsPS_Means,PQs_MEANS_proj,by="Year",all=TRUE)
# 
# 
# EQestObsPS_Means_proj_plot <- EQestObsPS_Means_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
#   geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + 
#   geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
#   geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
#   geom_line(aes(y=Ps, color="PS PROJECTION")) +
#   geom_point(aes(y=Ps, color="PS PROJECTION")) + 
#   geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
#   geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPS_Means_proj$Year[1],
#                                   EQestObsPS_Means_proj$Year[nrow(EQestObsPS_Means_proj)]))) +
#   expand_limits(y = 0.5)
# 
# 
# PQs_MEANS_proj_EP <- PQs_MEANS_EP %>% select(Year, Ps_lo, Ps, Ps_up)
# 
# EQestObsPS_Means_proj_EP <- merge(EQestObsPS_Means,PQs_MEANS_proj_EP,by="Year",all=TRUE)
# 
# 
# EQestObsPS_Means_projEP_plot <- EQestObsPS_Means_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
#   geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMean, color="PS RATIONAL (MEAN)")) +
#   geom_point(aes(y = psMean, color = "PS RATIONAL (MEAN)")) + 
#   geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
#   geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
#   geom_line(aes(y=Ps, color="PS PROJECTION")) +
#   geom_point(aes(y=Ps, color="PS PROJECTION")) + 
#   geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
#   geom_point(aes(y=Ps_up, color="PS_UP PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPS_Means_proj_EP$Year[1],
#                                   EQestObsPS_Means_proj_EP$Year[nrow(EQestObsPS_Means_proj_EP)]))) +
#   expand_limits(y = 0.5)
# 
# 
# 
# EQestObsPC_Means <- EQestObsPC %>% select(Year, pcMean, pc) %>% filter(Year >2009)
# 
# PQs_MEANS_proj <- PQs_MEANS %>% select(Year, Pc_lo, Pc, Pc_up)
# 
# EQestObsPC_Means_proj <- merge(EQestObsPC_Means,PQs_MEANS_proj,by="Year",all=TRUE)
# 
# 
# EQestObsPC_Means_proj_plot <- EQestObsPC_Means_proj %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
#   geom_point(aes(y = pcMean, color = "PC RATIONAL (MEDIAN)")) + 
#   geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
#   geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
#   geom_line(aes(y=Pc, color="PC PROJECTION")) +
#   geom_point(aes(y=Pc, color="PC PROJECTION")) + 
#   geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
#   geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPC_Means_proj$Year[1],
#                                   EQestObsPC_Means_proj$Year[nrow(EQestObsPC_Means_proj)]))) +
#   expand_limits(y = 0.5)
# 
# 
# PQs_MEANS_proj_EP <- PQs_MEANS_EP %>% select(Year, Pc_lo, Pc, Pc_up)
# 
# EQestObsPC_Means_proj_EP <- merge(EQestObsPC_Means,PQs_MEANS_proj_EP,by="Year",all=TRUE)
# 
# 
# EQestObsPC_Means_projEP_plot <- EQestObsPC_Means_proj_EP %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
#   geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMean, color="PC RATIONAL (MEAN)")) +
#   geom_point(aes(y = pcMean, color = "PC RATIONAL (MEAN)")) + 
#   geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
#   geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
#   geom_line(aes(y=Pc, color="PC PROJECTION")) +
#   geom_point(aes(y=Pc, color="PC PROJECTION")) + 
#   geom_line(aes(y=Pc_up, color="PS_LO PROJECTION"))  +
#   geom_point(aes(y=Pc_up, color="PS_LO PROJECTION")) + theme_classic() + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(EQestObsPC_Means_proj_EP$Year[1],
#                                   EQestObsPC_Means_proj_EP$Year[nrow(EQestObsPC_Means_proj_EP)]))) +
#   expand_limits(y = 0.5)




