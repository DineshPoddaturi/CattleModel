
### Fed cattle price plot
EQestObsPSNI_plots <- EQestObsPSNIII  %>% select(Year, psMean, psMedian, ps) %>% 
  transmute(Year = Year, psMean = psMean * 100, psMedian = psMedian * 100, ps = ps * 100) %>% filter(Year>=1998)

slaughter_plotMedian <- EQestObsPSNI_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=ps, color = "Observed price"),size=0.75) + 
  geom_point(aes(y=ps, color = "Observed price"),shape=15,size=2) + 
  geom_line(aes(y=psMedian, color="Median fitted price"),size=0.75) +
  geom_point(aes(y = psMedian, color = "Median fitted price"),shape=16,size=2) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNI_plots$Year[1],EQestObsPSNI_plots$Year[nrow(EQestObsPSNI_plots)], by = 2))) +
  scale_y_continuous(name="Fed Cattle Price")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


#### Cull cow price plot
EQestObsPCNI_plots <- EQestObsPCNIII %>% select(Year, pcMean, pcMedian, pc) %>% 
  transmute(Year = Year, pcMean = pcMean * 100, pcMedian = pcMedian * 100, pc = pc * 100) %>% filter(Year >= 1998)

cull_plotMedian <- EQestObsPCNI_plots%>% ggplot(aes(x=Year))+ 
  geom_line(aes(y=pc, color = "Observed price"),size=0.75) + 
  geom_point(aes(y=pc, color = "Observed price"),shape=15,size=2) + 
  geom_line(aes(y=pcMedian, color="Median fitted price"),size=0.75) +
  geom_point(aes(y = pcMedian, color = "Median fitted price"),shape=16,size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNI_plots$Year[1],EQestObsPCNI_plots$Year[nrow(EQestObsPCNI_plots)], by = 2))) +
  scale_y_continuous(name="Cull Cow Price") +theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))





###### Replicating the inventory and plotting
dressedWeights_cl <- dressedWeights_sl_cl %>% select(Year, Cull_avg)
EQestObsCLNI_Plots <- EQestObsCLNIII %>% select(Year, clMedian)

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

invReplication_plot <- EQestObstotalInventory %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"),size=0.75) +
  geom_point(aes(y=K,color="Observed Inventory"),shape=15,size=2) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"),size=0.75) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),shape=16,size=2) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  # geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_y_continuous(name="Million Head") + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA),text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))


invReplication_plot







