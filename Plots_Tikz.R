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







prices_predict_co4_merge111_paper <- prices_predict_co4_merge111 %>% filter(Year>=2004)

tikz(file="TexPlots/FedCattlePrices.tex", width=6, height=3)
fedCattlePrices_plot <- prices_predict_co4_merge111_paper %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est,color="Model Estimate"))+
  geom_point(aes(y=ps_est,color="Model Estimate"))+ geom_line(aes(y=ps_hat, color="Estimate with added costs"))+
  geom_point(aes(y=ps_hat,color="Estimate with added costs")) + geom_line(aes(y=ps, color="Observed"))+
  geom_point(aes(y=ps,color="Observed")) + labs(x="Year", y="Fed Cattle Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge111_paper$Year[1], 
                                               prices_predict_co4_merge111_paper$Year[nrow(prices_predict_co4_merge111_paper)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(fedCattlePrices_plot)
dev.off()

tikz(file="TexPlots/CullCowPrices.tex", width=6, height=3)
cullCowPrices_plot <- prices_predict_co4_merge111_paper %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est,color="Model estimate"))+
  geom_point(aes(y=pc_est,color="Model estimate")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=pc_hat,color="Estimate with added costs")) + geom_line(aes(y=pc, color="Observed")) + 
  geom_point(aes(y=pc,color="Observed")) + labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge111_paper$Year[1], 
                                               prices_predict_co4_merge111_paper$Year[nrow(prices_predict_co4_merge111_paper)]))) + 
  theme(legend.position = c(0.2, 0.7))
print(cullCowPrices_plot)
dev.off()



demand_predict_co4_merge111_paper <- demand_predict_co4_merge111 %>% filter(Year>=2004)

tikz(file="TexPlots/FedCattleMeat.tex", width=6, height=4)
fedCattleMeat_plot <- demand_predict_co4_merge111_paper %>% ggplot(aes(x=Year))+geom_line(aes(y=sl_est,color="Model estimate"))+
  geom_point(aes(y=sl_est,color="Model estimate")) + geom_line(aes(y=sl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=sl_hat,color="Estimate with added costs")) + geom_line(aes(y=sl, color="Observed")) + 
  geom_point(aes(y=sl,color="Observed")) + labs(x="Year", y="Fed Cattle meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge111_paper$Year[1],
                                               demand_predict_co4_merge111_paper$Year[nrow(demand_predict_co4_merge111_paper)]))) + 
  theme(legend.position = c(0.3, 0.4))
print(fedCattleMeat_plot)
dev.off()

tikz(file="TexPlots/CullCowMeat.tex", width=6, height=3)
cullCowMeat_plot <- demand_predict_co4_merge111_paper %>% ggplot(aes(x=Year))+geom_line(aes(y=cl_est,color="Model estimate"))+
  geom_point(aes(y=cl_est,color="Model estimate")) + geom_line(aes(y=cl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=cl_hat,color="Estimate with added costs")) + geom_line(aes(y=cl, color="Observed")) + 
  geom_point(aes(y=cl,color="Observed")) + labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge111_paper$Year[1],
                                               demand_predict_co4_merge111_paper$Year[nrow(demand_predict_co4_merge111_paper)]))) + 
  theme(legend.position = c(0.2, 0.8))
print(cullCowMeat_plot)
dev.off()


##### Different adoption rates no cost sharing
pSurplus_paper <- round(merge(pSurp_30, merge(pSurp_50, merge(pSurp_70, merge(pSurp_90, pSurp_100)))),3)
names(pSurplus_paper) <- c("Year", "30 \\%", "50 \\%", "70 \\%", "90 \\%", "100 \\%")

pSurplus_merge_paper <- pSurplus

pSurplus_long_paper <- pivot_longer(pSurplus_paper, -c(Year), values_to = "Surplus", names_to = "Adoption") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010.tex", width=6, height=3)
pSurplus_2010_plot <- pSurplus_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="darkseagreen4", width=0.5)+ 
  labs(x=  "Animal ID and Traceability Adoption Rate" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) + 
  theme_test()
print(pSurplus_2010_plot)
dev.off()


### 100% adoption different cost sharing schemes
pSurplus_adopt100_paper <- surplusLoss_costShare
names(pSurplus_adopt100_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt100_long_paper <- pivot_longer(pSurplus_adopt100_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010_adopt100_sub.tex", width=6, height=3)
pSurplus_adopt100_2010_plot <- pSurplus_adopt100_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue4", width=0.45)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) + 
  theme_test()
print(pSurplus_adopt100_2010_plot)
dev.off()


##### 30% adoption
pSurplus_adopt30_paper <- pSurp_30_CostShare
names(pSurplus_adopt30_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt30_long_paper <- pivot_longer(pSurplus_adopt30_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010_adopt30_sub.tex", width=6, height=3)
pSurplus_adopt30_2010_plot <- pSurplus_adopt30_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue4", width=0.45)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) + 
  theme_test()
print(pSurplus_adopt30_2010_plot)
dev.off()

##### 50% adoption
pSurplus_adopt50_paper <- pSurp_50_CostShare
names(pSurplus_adopt50_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt50_long_paper <- pivot_longer(pSurplus_adopt50_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010_adopt50_sub.tex", width=6, height=3)
pSurplus_adopt50_2010_plot <- pSurplus_adopt50_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="turquoise4", width=0.45)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) + theme_test() + 
  theme(plot.background = element_rect(fill = 'grey', colour = 'red'))

ggsave(pSurplus_adopt50_2010_plot, filename = "ProducerSurplus2010_adopt50_sub.png",  bg = "transparent")

print(pSurplus_adopt50_2010_plot)
dev.off()

##### 70% adoption
pSurplus_adopt70_paper <- pSurp_70_CostShare
names(pSurplus_adopt70_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt70_long_paper <- pivot_longer(pSurplus_adopt70_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010_adopt70_sub.tex", width=6, height=3)
pSurplus_adopt70_2010_plot <- pSurplus_adopt70_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="turquoise4", width=0.45)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) + 
  theme_test()
print(pSurplus_adopt70_2010_plot)
dev.off()

##### 90% adoption
pSurplus_adopt90_paper <- pSurp_90_CostShare
names(pSurplus_adopt90_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt90_long_paper <- pivot_longer(pSurplus_adopt90_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

tikz(file="TexPlots/ProducerSurplus2010_adopt90_sub.tex", width=6, height=3)
pSurplus_adopt90_2010_plot <- pSurplus_adopt90_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="thistle4", width=0.45)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=-1) +
  theme_test()
print(pSurplus_adopt90_2010_plot)
dev.off()


