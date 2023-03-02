# require(tikzDevice)
# # 
# # # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# # 
# # 
# # 
# # 

tikz(file="projectionsLatexPlots/UpdatedDissertation/FedCattlePriceProjectionsPlot.tex", width=6.2, height=3.5)

slaughter_Projection_Plot <- estProj_PSIIV %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year))+
  geom_line(aes(y=psMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = psMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Ps, color="Projection"),size=0.75) +
  geom_point(aes(y=Ps, color="Projection"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIIV$Year[1],
                                  estProj_PSIIV$Year[nrow(estProj_PSIIV)])))+ 
  scale_y_continuous(name="Fed Cattle Price", limits = c(110,145,by=5)) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(slaughter_Projection_Plot)

dev.off()


tikz(file="projectionsLatexPlots/UpdatedDissertation/CullCowPriceProjectionsPlot.tex", width=6.2, height=3.5)

cull_Projection_Plot <- estProj_PCIIV %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year))+
  geom_line(aes(y=pcMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = pcMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Pc, color="Projection"),size=0.75) +
  geom_point(aes(y=Pc, color="Projection"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PCIIV$Year[1],
                                  estProj_PCIIV$Year[nrow(estProj_PCIIV)])))+ 
  scale_y_continuous(name="Cull Cow Price", limits = c(65,80,by=5)) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(cull_Projection_Plot)

dev.off()



tikz(file="projectionsLatexPlots/UpdatedDissertation/FedCattleProductionProjectionsPlot.tex", width=6.2, height=3.5)

fedCattleProduction_Projection_Plot <- estProj_SLIIV %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year))+
  geom_line(aes(y=slMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = slMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Sl, color="Projection"),size=0.75) +
  geom_point(aes(y=Sl, color="Projection"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_SLIIV$Year[1],
                                  estProj_SLIIV$Year[nrow(estProj_SLIIV)])))+ 
  scale_y_continuous(name="Fed Cattle Production", limits = c(22,25,by=5)) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(fedCattleProduction_Projection_Plot)

dev.off()


tikz(file="projectionsLatexPlots/UpdatedDissertation/CullCowProductionProjectionsPlot.tex", width=6.2, height=3.5)

cullCowProduction_Projection_Plot <- estProj_CLIIV %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year))+
  geom_line(aes(y=clMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = clMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Cl, color="Projection"),size=0.75) +
  geom_point(aes(y=Cl, color="Projection"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_CLIIV$Year[1],
                                  estProj_CLIIV$Year[nrow(estProj_CLIIV)])))+ 
  scale_y_continuous(name="Cull Cow Production", limits = c(2.0,4.0,by=1)) + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(cullCowProduction_Projection_Plot)

dev.off()



tikz(file="projectionsLatexPlots/UpdatedDissertation/CARD_USDA_FAPRI_PS.tex", width=6.2, height=3.5)

CARD_USDA_FAPRI_PS_Plot <- estProj_PSIII_FAPRI %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=psMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = psMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=Ps, color="Projected"),size=0.75) +
  geom_point(aes(y=Ps, color="Projected"),size=2) +
  geom_line(aes(y=FAPRI_Ps, color="FAPRI"),size=0.75) +
  geom_point(aes(y=FAPRI_Ps, color="FAPRI"),size=2) +
  geom_line(aes(y=USDA_Ps, color="USDA"),size=0.75) +
  geom_point(aes(y=USDA_Ps, color="USDA"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_PSIII_FAPRI$Year[1],
                                  estProj_PSIII_FAPRI$Year[nrow(estProj_PSIII_FAPRI)])))+ 
  scale_y_continuous(name="Fed Cattle Price") +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(CARD_USDA_FAPRI_PS_Plot)

dev.off()


tikz(file="projectionsLatexPlots/UpdatedDissertation/CARD_USDA_FAPRI_TS.tex", width=6.2, height=3.5)

CARD_USDA_FAPRI_TS_Plot <- CARD_USDA_FAPRI_TS_ProjIII %>% filter(Year >=2018 & Year <= 2031) %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=tsMedian, color="Baseline"),size=0.75) +
  geom_point(aes(y = tsMedian, color = "Baseline"),size=2) +
  geom_line(aes(y=TS, color="Projected"),size=0.75) +
  geom_point(aes(y=TS, color="Projected"),size=2) +
  geom_line(aes(y=FAPRI_TS, color="FAPRI"),size=0.75) +
  geom_point(aes(y=FAPRI_TS, color="FAPRI"),size=2) +
  geom_line(aes(y=USDA_TS, color="USDA"),size=0.75) +
  geom_point(aes(y=USDA_TS, color="USDA"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_ProjIII$Year[1],
                                  CARD_USDA_FAPRI_TS_ProjIII$Year[nrow(CARD_USDA_FAPRI_TS_ProjIII)])))+ 
  scale_y_continuous(name="Total Supply") +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal", text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(CARD_USDA_FAPRI_TS_Plot)

dev.off()










# projPS_plots <- EQestObsPS_Medians_projs
# 
# tikz(file="projectionsLatexPlots/Updated/ProjectedFedCattlePricePlot.tex", width=6.2, height=3.5)
# 
# projectedFedPrice_plot <- projPS_plots %>% ggplot(aes(x=Year))+
#   geom_line(aes(y=psMedian, color="Fitted")) + geom_point(aes(y = psMedian, color = "Fitted")) +
#   geom_line(aes(y=Ps_lo, color="Lower bound")) + geom_point(aes(y=Ps_lo, color="Lower bound")) + 
#   geom_line(aes(y=Ps, color="Projected")) + geom_point(aes(y=Ps, color="Projected")) + 
#   geom_line(aes(y=Ps_up, color="Upper bound"))  + geom_point(aes(y=Ps_up, color="Upper bound")) +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(projPS_plots$Year[1],
#                                   projPS_plots$Year[nrow(projPS_plots)]))) + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal") +
#   theme(legend.title=element_blank()) +
#   scale_y_continuous(name="Fed Cattle Price") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# print(projectedFedPrice_plot)
# 
# dev.off()
# 
# 
# 
# projPC_plots <- EQestObsPC_Medians_projs
# 
# tikz(file="projectionsLatexPlots/Updated/ProjectedCullCowPricePlot.tex",  width=6.2, height=3.5)
# 
# projectedCullPrice_plot <- projPC_plots %>% ggplot(aes(x=Year))+
#   geom_line(aes(y=pcMedian, color="Fitted")) + geom_point(aes(y = pcMedian, color = "Fitted")) +  
#   geom_line(aes(y=Pc_lo, color="Upper bound")) + geom_point(aes(y=Pc_lo, color="Upper bound")) + 
#   geom_line(aes(y=Pc, color="Projected")) + geom_point(aes(y=Pc, color="Projected")) + 
#   geom_line(aes(y=Pc_up, color="Lower bound"))  + geom_point(aes(y=Pc_up, color="Lower bound"))  +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(projPC_plots$Year[1],
#                                   projPC_plots$Year[nrow(projPC_plots)])))  + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal") +
#   theme(legend.title=element_blank()) +
#   scale_y_continuous(name="Cull Cow Price")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# print(projectedCullPrice_plot)
# 
# dev.off()
# 
# # +theme(legend.position = c(0.1, 0.75))
# 
# adjFactorProj <- adjFactor %>% filter(Year==2020)
# 
# projSL_plots <- EQestObsSL_Medians_projs %>% select(Year, slMedian, Sl_lo, Sl, Sl_up)
# 
# projSL_plots[,-1:-2] <- projSL_plots[,-1:-2] * adjFactorProj$AdjFactor
# 
# projSL_plots <- projSL_plots %>% round(2)
# 
# tikz(file="projectionsLatexPlots/Updated/ProjectedFedCattleProduction.tex",  width=6.2, height=3.5)
# 
# projectedFedProduction_plot <- projSL_plots %>% ggplot(aes(x=Year))+
#   geom_line(aes(y=slMedian, color="Fitted")) + geom_point(aes(y = slMedian, color = "Fitted")) +  
#   geom_line(aes(y=Sl_lo, color="Lower bound")) + geom_point(aes(y=Sl_lo, color="Lower bound")) + 
#   geom_line(aes(y=Sl, color="Projected")) + geom_point(aes(y=Sl, color="Projected")) + 
#   geom_line(aes(y=Sl_up, color="Upper bound"))  + geom_point(aes(y=Sl_up, color="Upper bound"))  +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(projSL_plots$Year[1],
#                                   projSL_plots$Year[nrow(projSL_plots)]))) + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal") +
#   theme(legend.title=element_blank()) + 
#   scale_y_continuous(name="Fed Cattle Production") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
# 
# print(projectedFedProduction_plot)
# 
# dev.off()
# 
# 
# 
# projCL_plots <- EQestObsCL_Medians_projs %>% select(Year, clMedian, Cl_lo, Cl, Cl_up)
# projCL_plots <- projCL_plots %>% round(2)
# 
# tikz(file="projectionsLatexPlots/Updated/ProjectedCullCowProduction.tex",  width=6.2, height=3.5)
# 
# projectedCowsProduction_plot <- projCL_plots %>% ggplot(aes(x=Year))+
#   geom_line(aes(y=clMedian, color="Fitted")) + geom_point(aes(y = clMedian, color = "Fitted")) + 
#   geom_line(aes(y=Cl_lo, color="Lower bound")) + geom_point(aes(y=Cl_lo, color="Lower bound")) + 
#   geom_line(aes(y=Cl, color="Projected")) + geom_point(aes(y=Cl, color="Projected")) + 
#   geom_line(aes(y=Cl_up, color="Upper bound"))  + geom_point(aes(y=Cl_up, color="Upper bound"))  +
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(projCL_plots$Year[1],
#                                   projCL_plots$Year[nrow(projCL_plots)]))) + theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal") +
#   theme(legend.title=element_blank()) + 
#   scale_y_continuous(name="Cull Cow Production") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) 
# 
# print(projectedCowsProduction_plot)
# 
# dev.off()
# 
# CARD_USDA_FAPRI_PS_plots <- CARD_USDA_FAPRI_PS_Proj
# 
# 
# 
# 
# 
# #### Updated plots from 2022 to 2031
# CARD_USDA_FAPRI_PS_plots <- estProj_PSIII_FAPRI
# 
# tikz(file="projectionsLatexPlots/UpdatedNew/CARD_USDA_FAPRI_PS.tex",  width=6.2, height=4)
# 
# CARD_USDA_FAPRI_PS_plot <- CARD_USDA_FAPRI_PS_plots %>% ggplot(aes(x=Year)) + 
#   geom_line(aes(y=psMedian, color="Baseline")) + geom_point(aes(y = psMedian, color = "Baseline"),size=0.75) + 
#   geom_line(aes(y=Ps, color="Projected")) + geom_point(aes(y=Ps, color="Projected"),size=0.75) + 
#   geom_line(aes(y=FAPRI_Ps, color="FAPRI"))  + geom_point(aes(y=FAPRI_Ps, color="FAPRI"),size=0.75) + 
#   geom_line(aes(y=USDA_Ps, color="USDA"))  + geom_point(aes(y=USDA_Ps, color="USDA"),size=0.75)  + 
#   scale_x_continuous(name="Year", breaks=c(seq(CARD_USDA_FAPRI_PS_plots$Year[1],
#                                                CARD_USDA_FAPRI_PS_plots$Year[nrow(CARD_USDA_FAPRI_PS_plots)])))  +
#   theme_classic() +  
#   theme(legend.position="bottom", legend.box = "horizontal",
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   scale_y_continuous(name="Fed Cattle Price") + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
#         axis.text.y = element_text(size=12))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
#   theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))
# 
# print(CARD_USDA_FAPRI_PS_plot)
# 
# dev.off()
# 
# 
# CARD_USDA_FAPRI_TS_plots <- CARD_USDA_FAPRI_TS_Proj
# 
# CARD_USDA_FAPRI_TS_ProjIII
# 
# CARD_USDA_FAPRI_TS_plots[,3:5] <- CARD_USDA_FAPRI_TS_plots[,3:5] * adjFactorProj$AdjFactor
# CARD_USDA_FAPRI_TS_plots <- CARD_USDA_FAPRI_TS_plots %>% round(2)
# 
# 
# #### Updated plots from 2022 to 2031
# 
# CARD_USDA_FAPRI_TS_plots <- CARD_USDA_FAPRI_TS_ProjIII %>% as.data.frame()
# 
# tikz(file="projectionsLatexPlots/UpdatedNew/CARD_USDA_FAPRI_TS.tex",  width=6.2, height=4)
# 
# CARD_USDA_FAPRI_TS_plot <- CARD_USDA_FAPRI_TS_plots %>% ggplot(aes(x=Year)) + 
#   geom_line(aes(y=tsMedian, color="Baseline")) + geom_point(aes(y=tsMedian, color="Baseline"),size=0.75) + 
#   geom_line(aes(y=TS, color="Projected")) + geom_point(aes(y=TS, color="Projected"),size=0.75) + 
#   geom_line(aes(y=FAPRI_TS, color="FAPRI"))  + geom_point(aes(y=FAPRI_TS, color="FAPRI"),size=0.75) +
#   geom_line(aes(y=USDA_TS, color="USDA"))  + geom_point(aes(y=USDA_TS, color="USDA"),size=0.75)  + 
#   scale_x_continuous(name="Year", 
#                      breaks=c(seq(CARD_USDA_FAPRI_TS_plots$Year[1],
#                                   CARD_USDA_FAPRI_TS_plots$Year[nrow(CARD_USDA_FAPRI_TS_plots)]))) +
#   theme_classic() + 
#   theme(legend.position="bottom", legend.box = "horizontal",
#         legend.background = element_rect(color = NA)) +
#   theme(legend.title=element_blank()) + 
#   scale_y_continuous(name="Total Supply") + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
#         axis.text.y = element_text(size=12))+ 
#   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
#   theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))
# 
# print(CARD_USDA_FAPRI_TS_plot)
# 
# dev.off()
# 
# 
# 
# 
# 
