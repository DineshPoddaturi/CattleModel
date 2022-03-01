require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

EQestObsPSNI_plots <- EQestObsPSNI  %>% select(Year, psMean, psMedian, ps) %>% 
  transmute(Year = Year, psMean = psMean * 100, psMedian = psMedian * 100, ps = ps * 100)

tikz(file="rationalExpectationsLatexPlots/FedCattlePricePlot.tex", width=6.2, height=3.5)

slaughter_plot <- EQestObsPSNI_plots%>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="Mean fitted price")) +
  geom_point(aes(y = psMean, color = "Mean fitted price")) + geom_line(aes(y=ps, color = "Observed price")) + 
  geom_point(aes(y=ps, color = "Observed price")) + geom_line(aes(y=psMedian, color="Median fitted price")) +
  geom_point(aes(y = psMedian, color = "Median fitted price")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)]))) +
  scale_y_continuous(name="Fed Cattle Price")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(slaughter_plot)

dev.off()


EQestObsPCNI_plots <- EQestObsPCNI %>% select(Year, pcMean, pcMedian, pc) %>% 
  transmute(Year = Year, pcMean = pcMean * 100, pcMedian = pcMedian * 100, pc = pc * 100)

tikz(file="rationalExpectationsLatexPlots/CullCowPricePlot.tex", width=6.2, height=3.5)

cull_plot <- EQestObsPCNI_plots%>% ggplot(aes(x=Year))+geom_line(aes(y=pcMean, color="Mean fitted price")) +
  geom_point(aes(y = pcMean, color = "Mean fitted price")) + geom_line(aes(y=pc, color = "Observed price")) + 
  geom_point(aes(y=pc, color = "Observed price")) + geom_line(aes(y=pcMedian, color="Median fitted price")) +
  geom_point(aes(y = pcMedian, color = "Median fitted price")) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNI_plots$Year[1],EQestObsPCNI_plots$Year[nrow(EQestObsPCNI_plots)]))) +
  scale_y_continuous(name="Cull Cow Price") +theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(cull_plot)

dev.off()







