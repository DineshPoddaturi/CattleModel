require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

EQestObsPSNI_plots <- EQestObsPSNI %>% filter(Year >=2006) %>% select(Year, psMean, psMedian, ps) %>% 
  transmute(Year = Year, psMean = psMean * 100, psMedian = psMedian * 100, ps = ps * 100)

tikz(file="rationalExpectationsLatexPlots/FedCattlePricePlot.tex", width=6, height=3)

slaughter_plot <- EQestObsPSNI_plots%>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="Mean fitted price")) +
  geom_point(aes(y = psMean, color = "Mean fitted price")) + geom_line(aes(y=ps, color = "Observed price")) + 
  geom_point(aes(y=ps, color = "Observed price")) + geom_line(aes(y=psMedian, color="Median fitted price")) +
  geom_point(aes(y = psMedian, color = "Median fitted price")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)]))) +
  scale_y_continuous(name="Fed Cattle Price")+
  theme(legend.position = c(0.2, 0.7))

print(slaughter_plot)

dev.off()


EQestObsPCNI_plots <- EQestObsPCNI %>% filter(Year >=2006) %>% select(Year, pcMean, pcMedian, pc) %>% 
  transmute(Year = Year, pcMean = pcMean * 100, pcMedian = pcMedian * 100, pc = pc * 100)

tikz(file="rationalExpectationsLatexPlots/CullCowPricePlot.tex", width=6, height=3)

cull_plot <- EQestObsPCNI_plots%>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="Mean fitted price")) +
  geom_point(aes(y = pcMean, color = "Mean fitted price")) + geom_line(aes(y=pc, color = "Observed price")) + 
  geom_point(aes(y=pc, color = "Observed price")) + geom_line(aes(y=pcMedian, color="Median fitted price")) +
  geom_point(aes(y = pcMedian, color = "Median fitted price")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNI_plots$Year[1],EQestObsPCNI_plots$Year[nrow(EQestObsPCNI_plots)]))) +
  scale_y_continuous(name="Cull Cow Price") +
  theme(legend.position = c(0.2, 0.7))

print(cull_plot)

dev.off()







