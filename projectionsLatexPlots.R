require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

projPS_plots <- EQestObsPS_Medians_proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/ProjectedFedCattlePricePlot.tex", width=6.2, height=3.5)

projectedFedPrice_plot <- projPS_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=psMedian, color="Fitted")) + geom_point(aes(y = psMedian, color = "Fitted")) +
  geom_line(aes(y=Ps_lo, color="Lower bound")) + geom_point(aes(y=Ps_lo, color="Lower bound")) + 
  geom_line(aes(y=Ps, color="Projected")) + geom_point(aes(y=Ps, color="Projected")) + 
  geom_line(aes(y=Ps_up, color="Upper bound"))  + geom_point(aes(y=Ps_up, color="Upper bound")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projPS_plots$Year[1],
                                  projPS_plots$Year[nrow(projPS_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Fed Cattle Price") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(projectedFedPrice_plot)

dev.off()



projPC_plots <- EQestObsPC_Medians_proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/ProjectedCullCowPricePlot.tex",  width=6.2, height=3.5)

projectedCullPrice_plot <- projPC_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=pcMedian, color="Fitted")) + geom_point(aes(y = pcMedian, color = "Fitted")) +  
  geom_line(aes(y=Pc_lo, color="Upper bound")) + geom_point(aes(y=Pc_lo, color="Upper bound")) + 
  geom_line(aes(y=Pc, color="Projected")) + geom_point(aes(y=Pc, color="Projected")) + 
  geom_line(aes(y=Pc_up, color="Lower bound"))  + geom_point(aes(y=Pc_up, color="Lower bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projPC_plots$Year[1],
                                  projPC_plots$Year[nrow(projPC_plots)])))  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Cull Cow Price")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedCullPrice_plot)

dev.off()

# +theme(legend.position = c(0.1, 0.75))

projSL_plots <- EQestObsSL_Medians_proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/ProjectedFedCattleProduction.tex",  width=6.2, height=3.5)

projectedFedProduction_plot <- projSL_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=slMedian, color="Fitted")) + geom_point(aes(y = slMedian, color = "Fitted")) +  
  geom_line(aes(y=Sl_lo, color="Lower bound")) + geom_point(aes(y=Sl_lo, color="Lower bound")) + 
  geom_line(aes(y=Sl, color="Projected")) + geom_point(aes(y=Sl, color="Projected")) + 
  geom_line(aes(y=Sl_up, color="Upper bound"))  + geom_point(aes(y=Sl_up, color="Upper bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projSL_plots$Year[1],
                                  projSL_plots$Year[nrow(projSL_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name="Fed Cattle Production") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedFedProduction_plot)

dev.off()



projCL_plots <- EQestObsCL_Medians_proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/ProjectedCullCowProduction.tex",  width=6.2, height=3.5)

projectedCowsProduction_plot <- projCL_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=clMedian, color="Fitted")) + geom_point(aes(y = clMedian, color = "Fitted")) + 
  geom_line(aes(y=Cl_lo, color="Lower bound")) + geom_point(aes(y=Cl_lo, color="Lower bound")) + 
  geom_line(aes(y=Cl, color="Projected")) + geom_point(aes(y=Cl, color="Projected")) + 
  geom_line(aes(y=Cl_up, color="Upper bound"))  + geom_point(aes(y=Cl_up, color="Upper bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projCL_plots$Year[1],
                                  projCL_plots$Year[nrow(projCL_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name="Cull Cow Production") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedCowsProduction_plot)

dev.off()





CARD_USDA_FAPRI_PS_plots <- CARD_USDA_FAPRI_PS_Proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/CARD_USDA_FAPRI_PS.tex",  width=6.2, height=4)

CARD_USDA_FAPRI_PS_plot <- CARD_USDA_FAPRI_PS_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=psMedian, color="Fitted")) + geom_point(aes(y = psMedian, color = "Fitted")) + 
  geom_line(aes(y=Ps_lo, color="Lower bound")) + geom_point(aes(y=Ps_lo, color="Lower bound")) + 
  geom_line(aes(y=Ps, color="Projected")) + geom_point(aes(y=Ps, color="Projected")) + 
  geom_line(aes(y=FAPRI_Ps, color="FAPRI"))  + geom_point(aes(y=FAPRI_Ps, color="FAPRI")) + 
  geom_line(aes(y=USDA_Ps, color="USDA"))  + geom_point(aes(y=USDA_Ps, color="USDA"))  + 
  geom_line(aes(y=Ps_up, color="Upper bound"))  + geom_point(aes(y=Ps_up, color="Upper bound"))  + 
  scale_x_continuous(name="Year", breaks=c(seq(CARD_USDA_FAPRI_PS_plots$Year[1],
                                               CARD_USDA_FAPRI_PS_plots$Year[nrow(CARD_USDA_FAPRI_PS_plots)])))  +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name="Fed Cattle Price") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(CARD_USDA_FAPRI_PS_plot)

dev.off()


CARD_USDA_FAPRI_TS_plots <- CARD_USDA_FAPRI_TS_Proj %>% filter(Year > 2013)

tikz(file="projectionsLatexPlots/CARD_USDA_FAPRI_TS.tex",  width=6.2, height=4)

CARD_USDA_FAPRI_TS_plot <- CARD_USDA_FAPRI_TS_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=tsMedian, color="Fitted")) + geom_point(aes(y=tsMedian, color="Fitted")) + 
  geom_line(aes(y=TS_lo, color="Lower Bound")) + geom_point(aes(y=TS_lo, color="Lower Bound")) + 
  geom_line(aes(y=TS, color="Projected")) + geom_point(aes(y=TS, color="Projected")) + 
  geom_line(aes(y=FAPRI_TS, color="FAPRI"))  + geom_point(aes(y=FAPRI_TS, color="FAPRI")) +
  geom_line(aes(y=USDA_TS, color="USDA"))  + geom_point(aes(y=USDA_TS, color="USDA"))  + 
  geom_line(aes(y=TS_up, color="Upper Bound"))  + geom_point(aes(y=TS_up, color="Upper Bound"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(CARD_USDA_FAPRI_TS_plots$Year[1],
                                  CARD_USDA_FAPRI_TS_plots$Year[nrow(CARD_USDA_FAPRI_TS_plots)]))) +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name="Total Supply") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(CARD_USDA_FAPRI_TS_plot)

dev.off()





