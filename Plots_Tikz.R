########## The following are similar to above plots but to make them compatible to insert in  Latex document
# require(ggpubr)
require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

pricesMerge_new_plots <- pricesMerge_new %>% filter(Year >=2000)

tikz(file="TexPlots/SlaughterPlot.tex", width=6, height=3)
slaughter_plot <- pricesMerge_new_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) +
  labs(x="Year", y="Fed Cattle Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(slaughter_plot)
dev.off()

tikz(file="TexPlots/CullPlot.tex", width=6, height=3)
cull_plot <- pricesMerge_new_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) +
  labs(x="Year", y="Cull Cow Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(cull_plot)
dev.off()

tikz(file="TexPlots/HoldingPlot.tex", width=6, height=3)
holding_plot <- pricesMerge_new_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) +
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(holding_plot)
dev.off()











