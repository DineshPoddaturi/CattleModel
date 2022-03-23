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


EQestObsSLNI_Plots <- EQestObsSLNI %>% select(Year, slMedian)
replacementHeifers <- k3
totalInventory <- Stock %>% select(Year, K)

dressedWeights_sl <- dressedWeights_sl_cl %>% select(Year, Slaughter_avg)
EQestObsSLNI_Head <- merge(EQestObsSLNI_Plots, dressedWeights_sl) %>% mutate(slMedianHead = slMedian * (1000000000/Slaughter_avg))
EQestObsSlHead_totalINV <- merge(EQestObsSLNI_Head, totalInventory) %>% select(Year, slMedianHead, K)
EQestObsSlHead_totalINV_repHeifers <- left_join(replacementHeifers, EQestObsSlHead_totalINV) %>% 
  transmute(Year = Year, K = K, slHeadEst = slMedianHead, repH = k3)

EQesttotalInventory <- EQestObsSlHead_totalINV_repHeifers %>% select(Year,slHeadEst, repH) %>%
  transmute(Year = Year-1, fitK = (slHeadEst + lead(repH,1)/g)) %>% na.exclude()


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















