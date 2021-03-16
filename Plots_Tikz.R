########## The following are similar to above plots but to make them compatible to insert in  Latex document
# require(ggpubr)
require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html
# 
# 
# 
# 

pricesMerge_new_plots <- pricesMerge_new %>% filter(Year >=2005)

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

Master_sl_cl_plots <- Master_sl_cl %>% filter(Year >=2005)

tikz(file="TexPlots/FedCattleSupply.tex", width=6, height=4)
fedCattleSupply_plot <- Master_sl_cl_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +
  geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_plots$Year[1],Master_sl_cl_plots$Year[nrow(Master_sl_cl_plots)]))) + 
  theme(legend.position = c(0.9, 0.9))
print(fedCattleSupply_plot)
dev.off()

tikz(file="TexPlots/CullCowSupply.tex", width=6, height=3)
cullCowSupply_plot <- Master_sl_cl_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +
  geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_plots$Year[1],Master_sl_cl_plots$Year[nrow(Master_sl_cl_plots)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(cullCowSupply_plot)
dev.off()


parameters_paper <- parameters %>% filter(Year>=2006) %>% mutate(mu_tilde = round(mu_tilde,3), s_tilde = round(s_tilde,3))

pricesMerge_new_paper <- pricesMerge_new
pricesMerge_new_paper[,-1] <- round(pricesMerge_new_paper[,-1],3)

dressedWeights_sl_cl_paper <- dressedWeights_sl_cl %>% filter(Year>=2006) %>% mutate(Slaughter_avg = round(Slaughter_avg,3),
                                                                                    Cull_avg = round(Cull_avg,3))
supp_cl_adj_paper <- supp_cl_adj %>% filter(Year>=2006) %>% mutate(Bill_meatLb_cl = round(Bill_meatLb_cl,3))
supp_sl_adj_paper <- supp_sl_adj %>% filter(Year>=2006) %>% mutate(Bill_meatLb_sl = round(Bill_meatLb_sl,3))

pc_ps_cwt_paper <- pc_ps_cwt %>% filter(Year >=2006) %>% mutate(pss_cwt = round(pss_cwt,3), pcs_cwt = round(pcs_cwt,3))











