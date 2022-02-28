require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

projPS_plots <- EQestObsPS_Medians_proj

tikz(file="projectionsLatexPlots/ProjectedFedCattlePricePlot.tex", width=6, height=3)

projectedFedPrice_plot <- projPS_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=psMedian, color="Fitted")) + geom_point(aes(y = psMedian, color = "Fitted")) + 
  geom_line(aes(y=ps, color = "Observed")) + geom_point(aes(y=ps, color = "Observed")) + 
  geom_line(aes(y=Ps_lo, color="Lower bound")) + geom_point(aes(y=Ps_lo, color="Lower bound")) + 
  geom_line(aes(y=Ps, color="Projected")) + geom_point(aes(y=Ps, color="Projected")) + 
  geom_line(aes(y=Ps_up, color="Upper bound"))  + geom_point(aes(y=Ps_up, color="Upper bound")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projPS_plots$Year[1],
                                  projPS_plots$Year[nrow(projPS_plots)]))) +
  scale_y_continuous(name="Fed cattle Price") + 
  theme(legend.position = c(0.1, 0.75)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(projectedFedPrice_plot)

dev.off()



projPC_plots <- EQestObsPC_Medians_proj

tikz(file="projectionsLatexPlots/ProjectedCullCowPricePlot.tex", width=6, height=3)

projectedCullPrice_plot <- projPC_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=pcMedian, color="Fitted")) + geom_point(aes(y = pcMedian, color = "Fitted")) + 
  geom_line(aes(y=pc, color = "Observed")) + geom_point(aes(y=pc, color = "Observed")) + 
  geom_line(aes(y=Pc_lo, color="Lower bound")) + geom_point(aes(y=Pc_lo, color="Lower bound")) + 
  geom_line(aes(y=Pc, color="Projected")) + geom_point(aes(y=Pc, color="Projected")) + 
  geom_line(aes(y=Pc_up, color="Upper bound"))  + geom_point(aes(y=Pc_up, color="Upper bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projPC_plots$Year[1],
                                  projPC_plots$Year[nrow(projPC_plots)]))) + 
  scale_y_continuous(name="Cull cow price") +
  theme(legend.position = c(0.1, 0.75)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedCullPrice_plot)

dev.off()



projSL_plots <- EQestObsSL_Medians_proj

tikz(file="projectionsLatexPlots/ProjectedFedCattleProduction.tex", width=6, height=3)

projectedFedProduction_plot <- projSL_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=slMedian, color="Fitted")) + geom_point(aes(y = slMedian, color = "Fitted")) + 
  geom_line(aes(y=SlObs, color = "Observed")) + geom_point(aes(y=SlObs, color = "Observed")) + 
  geom_line(aes(y=Sl_lo, color="Lower bound")) + geom_point(aes(y=Sl_lo, color="Lower bound")) + 
  geom_line(aes(y=Sl, color="Projected")) + geom_point(aes(y=Sl, color="Projected")) + 
  geom_line(aes(y=Sl_up, color="Upper bound"))  + geom_point(aes(y=Sl_up, color="Upper bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projSL_plots$Year[1],
                                  projSL_plots$Year[nrow(projSL_plots)]))) + 
  scale_y_continuous(name="Fed cattle production") +
  theme(legend.position = c(0.1, 0.75)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedFedProduction_plot)

dev.off()



projCL_plots <- EQestObsCL_Medians_proj

tikz(file="projectionsLatexPlots/ProjectedCullCowProduction.tex", width=6, height=3)

projectedCowsProduction_plot <- projCL_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=clMedian, color="Fitted")) + geom_point(aes(y = clMedian, color = "Fitted")) + 
  geom_line(aes(y=ClObs, color = "Observed")) + geom_point(aes(y=ClObs, color = "Observed")) + 
  geom_line(aes(y=Cl_lo, color="Lower bound")) + geom_point(aes(y=Cl_lo, color="Lower bound")) + 
  geom_line(aes(y=Cl, color="Projected")) + geom_point(aes(y=Cl, color="Projected")) + 
  geom_line(aes(y=Cl_up, color="Upper bound"))  + geom_point(aes(y=Cl_up, color="Upper bound"))  +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(projCL_plots$Year[1],
                                  projCL_plots$Year[nrow(projCL_plots)]))) + 
  scale_y_continuous(name="Cull cow production") +
  theme(legend.position = c(0.1, 0.75)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(projectedCowsProduction_plot)

dev.off()



