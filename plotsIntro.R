#### Plots for introduction
require(tikzDevice)


pc_ps_cwt_plots <- merge(pcs_cwt, pss_cwt) %>% select(Year,pss_cwt, pcs_cwt) %>% 
  transmute(Year = Year, PS = pss_cwt, PC = pcs_cwt) %>% round(2) %>% filter(Year > 1969)

tikz(file="introPlots/Updated2023/PricesReceived.tex", width = 6.2, height = 3.5)

price_plot <- pc_ps_cwt_plots %>% ggplot(aes(x=Year)) + geom_line(aes(y=PS, color = "Steers and Heifers")) + 
  geom_line(aes(y=PC, color="Cows")) + 
  scale_x_continuous(name = "Year", 
                     breaks = c(seq(pc_ps_cwt_plots$Year[1],pc_ps_cwt_plots$Year[nrow(pc_ps_cwt_plots)], 
                                    by = 3))) + theme_classic() + 
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank()) +
  scale_y_continuous(name="Price") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))
  

print(price_plot)

dev.off()



beefInventory_plots <- beefInventory %>% transmute(Year = Year, K = K/1000000) %>% filter(Year > 1969) %>% arrange(Year)

# require(pracma)
# ddlBeefINV <- detrend(as.matrix(beefInventory_plots%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1970,2020))) %>% select(Year, everything())
# 
# ddl_plotBeefINV <- ddlBeefINV %>% ggplot(aes(x=Year))+geom_line(aes(y=K,color="Stock"))+geom_point(aes(y=K,color="Stock")) + 
#   labs(x="Year", y="", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(1970,2020, by =2)))  + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tikz(file="introPlots/Updated2023/CattleCycle.tex", width = 6.2, height = 3.5)

inventoryBeef_plot <- beefInventory_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=K, color="Beef cows")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefInventory_plots$Year[1],
                                  beefInventory_plots$Year[nrow(beefInventory_plots)], by=3))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Million head") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(inventoryBeef_plot)

dev.off()


yearsReceipts <- seq(from = 2002, to = 2021, by = 1) %>% as.numeric() %>% as.data.frame() 
names(yearsReceipts) <- "Year"

domesticReceipts <- c(38.1, 45.3, 47.4, 49.3, 49.1, 49.8,
                      48.4, 43.7, 51.2, 62.3, 66.1, 67.8, 81.1,
                      78.3, 63.7, 66.9, 67.0, 66.3, 63.1, 72.8) %>% as.data.frame()
names(domesticReceipts) <- "domesticReceipts"

exportValue <- c(2.6, 3.2, 0.6, 1.0, 1.6, 2.2, 3.0, 2.9,
                 3.8, 5.0, 5.1, 5.7, 6.5, 5.4, 5.5, 6.4,
                 7.5, 7.1, 6.8, 9.9) %>% as.data.frame()
names(exportValue) <- "exportValue"

beefReceipts <- cbind(yearsReceipts, domesticReceipts, exportValue) %>% as.data.frame()


beefReceiptsDomestic_plots <- beefReceipts %>% select(Year, 
                                                      domesticReceipts)

tikz(file="introPlots/Updated2023/domesticReceipts.tex", width = 6.2, height = 3.5)

domesticReceipts_plot <- beefReceiptsDomestic_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=domesticReceipts, color="Cash receipts")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefReceiptsDomestic_plots$Year[1],
                                  beefReceiptsDomestic_plots$Year[nrow(beefReceiptsDomestic_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Billion dollars") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(domesticReceipts_plot)

dev.off()


beefValueExport_plots <- beefReceipts %>% select(Year, exportValue)

tikz(file="introPlots/Updated2023/exportValue.tex", width = 6.2, height = 3.5)

exportValue_plot <- beefValueExport_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=exportValue, color="Export value")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefValueExport_plots$Year[1],
                                  beefValueExport_plots$Year[nrow(beefValueExport_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Billion dollars") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(exportValue_plot)

dev.off()



###### Combinded plot
domesticExportValue_Plots <- merge(beefReceiptsDomestic_plots, beefValueExport_plots)

tikz(file="introPlots/Updated2023/domesticExportValue.tex", width = 6.2, height = 3.5)

domesticExportValue_plot <- domesticExportValue_Plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=exportValue, color="Export Value")) +
  geom_line(aes(y=domesticReceipts, color="Domestic Cash Receipts")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(domesticExportValue_Plots$Year[1],
                                  domesticExportValue_Plots$Year[nrow(domesticExportValue_Plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Billion dollars") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(domesticExportValue_plot)

dev.off()







###### Detrended plots


require(pracma)

ddlPrice_plot <- detrend(as.matrix(pc_ps_cwt_plots%>%select(-Year)),tt='linear') %>% as.data.frame() %>% 
  mutate(Year = c(seq(pc_ps_cwt_plots$Year[1],
                      pc_ps_cwt_plots$Year[nrow(pc_ps_cwt_plots)]))) %>% select(Year, everything())

tikz(file="introPlots/Updated2023/PricesReceivedDeTrended.tex", width = 6.2, height = 3)

deTrendedPrice_plot <- ddlPrice_plot %>% ggplot(aes(x=Year)) + geom_line(aes(y=PS,color="Steers and Heifers")) +
  geom_line(aes(y=PC,color="Cows")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(ddlPrice_plot$Year[1],
                                  ddlPrice_plot$Year[nrow(ddlPrice_plot)], by = 3))) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank())+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                             axis.title.y = element_blank())+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(deTrendedPrice_plot)

dev.off()



ddlInventory_plot <- detrend(as.matrix(beefInventory_plots%>%select(-Year)),tt='linear') %>% as.data.frame() %>% 
  mutate(Year = c(seq(beefInventory_plots$Year[1],
                      beefInventory_plots$Year[nrow(beefInventory_plots)]))) %>% select(Year, everything())

tikz(file="introPlots/Updated2023/CattleCycleDeTrended.tex", width = 6.2, height = 3.5)

deTrendedInv_plot <- ddlInventory_plot %>% ggplot(aes(x=Year)) + geom_line(aes(y=K, color="Beef cows")) +
  scale_x_continuous(name="Year",
                     breaks=c(seq(ddlInventory_plot$Year[1],
                                  ddlInventory_plot$Year[nrow(ddlInventory_plot)], by = 3))) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") + 
  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                                              axis.title.y = element_blank())+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

print(deTrendedInv_plot)

dev.off()








