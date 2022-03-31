require(tikzDevice)
# 
# # http://iltabiai.github.io/tips/latex/2015/09/15/latex-tikzdevice-r.html


EQestObsPSNI_plots <- EQestObsPSNII  %>% select(Year, psMean, psMedian, ps) %>% 
  transmute(Year = Year, psMean = psMean * 100, psMedian = psMedian * 100, ps = ps * 100) %>% filter(Year>=1998)

tikz(file="rationalExpectationsLatexPlots/Updated/FedCattlePricePlot.tex", width=6.2, height=3.5)

slaughter_plot <- EQestObsPSNI_plots %>% ggplot(aes(x=Year))+geom_line(aes(y=psMean, color="Mean fitted price")) +
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


EQestObsPCNI_plots <- EQestObsPCNII %>% select(Year, pcMean, pcMedian, pc) %>% 
  transmute(Year = Year, pcMean = pcMean * 100, pcMedian = pcMedian * 100, pc = pc * 100) %>% filter(Year >= 1998)

tikz(file="rationalExpectationsLatexPlots/Updated/CullCowPricePlot.tex", width=6.2, height=3.5)

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




dressedWeights_cl <- dressedWeights_sl_cl %>% select(Year, Cull_avg)
EQestObsCLNI_Plots <- EQestObsCLNII %>% select(Year, clMedian)
EQestObsCLNI_Head <- merge(EQestObsCLNI_Plots, dressedWeights_cl) %>% mutate(clMedianHead = 
                                                                               clMedian * (1000000000/Cull_avg))

Stock_lessK <- Stock %>% select(-K)
EQestObsCLNI_Head_Inventory <- left_join(Stock_lessK, EQestObsCLNI_Head) %>% select(-clMedian, 
                                                                                    -Cull_avg, -k10)

EQestObsCLNI_Head_Inventory1 <- EQestObsCLNI_Head_Inventory %>% mutate(CLk_987 = clMedianHead + lead(k9,1) + lead(k8,1))
EQestObsCLNI_Head_Inventory1 <- EQestObsCLNI_Head_Inventory1 %>% na.exclude()
EQestObsCLNI_Head_Inventory11 <- EQestObsCLNI_Head_Inventory1 %>% select(-clMedianHead)
EQestObsCLNI_Head_Inventory11 <- EQestObsCLNI_Head_Inventory11 %>% 
  mutate(fitK = k3 + k4 + k5 + k6 + CLk_987) %>% select(Year, fitK)
totalInventory <- Stock %>% select(Year, K)
EQestObstotalInventory <- merge(totalInventory, EQestObsCLNI_Head_Inventory11) %>% mutate(K = K/1000000, 
                                                                                          fitK = fitK/1000000) %>% 
  filter(Year >= 1990)

tikz(file="rationalExpectationsLatexPlots/Updated/CattleCyclesReplicationPlot.tex", width=6.2, height=3.5)

invReplication_plot <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory")) +
  geom_point(aes(y=K,color="Observed Inventory")) + 
  geom_line(aes(y=fitK,color="Fitted Inventory")) +
  geom_point(aes(y=fitK,color="Fitted Inventory")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  # geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                             axis.title.y = element_blank())

print(invReplication_plot)

dev.off()


ddlObsInventory_plot <- detrend(as.matrix(EQestObstotalInventory%>%select(-Year)),tt='linear') %>% as.data.frame() %>% 
  mutate(Year = c(seq(EQestObstotalInventory$Year[1],
                      EQestObstotalInventory$Year[nrow(EQestObstotalInventory)]))) %>% select(Year, everything())

tikz(file="rationalExpectationsLatexPlots/Updated/CattleCyclesReplicationPlotDetrended.tex", width=6.2, height=3.5)

deTrendedInvReplication_plot <- ddlObsInventory_plot %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory")) +
  geom_point(aes(y=K,color="Observed Inventory")) + 
  geom_line(aes(y=fitK,color="Fitted Inventory")) +
  geom_point(aes(y=fitK,color="Fitted Inventory")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(ddlInventory_plot$Year[1],
                                  ddlInventory_plot$Year[nrow(ddlInventory_plot)], by = 2))) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                             axis.title.y = element_blank())

print(deTrendedInvReplication_plot)

dev.off()






dressedWeights_sl <- dressedWeights_sl_cl %>% select(Year, Slaughter_avg)
EQestObsSLNI_Plots <- EQestObsSLNII %>% select(Year, slMedian)

EQestObsSLNI_Plots <- 
  left_join(EQestObsSLNI_Plots,supp_diss_adj) %>% select(Year, slMedian, AdjFactor) %>% 
  mutate(slMedian = slMedian*AdjFactor)

replacementHeifers <- k3
EQestObsSLNI_Head <- merge(EQestObsSLNI_Plots, dressedWeights_sl) %>% mutate(slMedianHead = slMedian * (1000000000/Slaughter_avg))
EQestObsSlHead_repHeifers <- left_join(EQestObsSLNI_Head,stocksImportsExports) %>% 
  transmute(Year = Year, slHeadEst = slMedianHead, repH = k3, Imports = Imports, Exports = Exports)
EQesttotalInventory <- EQestObsSlHead_repHeifers %>% select(Year,slHeadEst, repH, Imports, Exports) %>%
  transmute(Year = Year, fitK = ((slHeadEst + lead(repH,1))/g)) %>% na.exclude()

totalInventory <- Stock %>% select(Year, K)
EQestObstotalInventorySL <- merge(totalInventory, EQesttotalInventory) %>% mutate(K = K/1000000,
                                                                                  fitK = fitK/1000000) %>% filter(Year >=1990)

inventoryPlotSL <- EQestObstotalInventorySL %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory")) +
  geom_line(aes(y=fitK,color="Fitted Inventory")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)])))+ 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                             axis.title.y = element_blank())








# Try either adding imports on fitK or remove exports from K
EQestObstotalInventory <- merge(EQesttotalInventory, totalInventory) %>% transmute(Year = Year,
                                                                                   fitK = fitK/1000000,
                                                                                   K = K/1000000)

ddlInventory_plot <- detrend(as.matrix(EQestObstotalInventory%>%select(-Year)),tt='constant') %>% as.data.frame() %>% 
  mutate(Year = c(seq(EQestObstotalInventory$Year[1],
                      EQestObstotalInventory$Year[nrow(EQestObstotalInventory)]))) %>% select(Year, everything())

deTrendedInventory_plot <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + geom_line(aes(y=K,color="Observed Inventory")) +
  geom_line(aes(y=fitK,color="Fitted Inventory")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)])))+ 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                             axis.title.y = element_blank())















