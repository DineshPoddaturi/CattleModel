#### Plots for introduction
require(tikzDevice)


pc_ps_cwt_plots <- merge(pcs_cwt, pss_cwt) %>% select(Year,pss_cwt, pcs_cwt) %>% 
  transmute(Year = Year, PS = pss_cwt, PC = pcs_cwt) %>% round(2) %>% filter(Year > 1969)

tikz(file="introPlots/PricesReceived.tex", width = 6.2, height = 3)

price_plot <- pc_ps_cwt_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=PS, color="Fed Cattle")) +
  geom_line(aes(y=PC, color="Cull Cow")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(pc_ps_cwt_plots$Year[1],
                                  pc_ps_cwt_plots$Year[nrow(pc_ps_cwt_plots)], by = 2))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Price") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(price_plot)

dev.off()



beefInventory_plots <- beefInventory %>% transmute(Year = Year, K = K/1000000) %>% filter(Year > 1969) %>% arrange(Year)

tikz(file="introPlots/CattleCycle.tex", width = 6.2, height = 3.5)

inventoryBeef_plot <- beefInventory_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=K, color="Beef Cows")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefInventory_plots$Year[1],
                                  beefInventory_plots$Year[nrow(beefInventory_plots)],by=2))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Million head") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(inventoryBeef_plot)

dev.off()


yearsReceipts <- seq(from = 2001, to = 2020, by = 1) %>% as.numeric() %>% as.data.frame() 
names(yearsReceipts) <- "Year"

domesticReceipts <- c(40.5, 38.1, 45.3, 47.4, 49.3, 49.1, 49.8,
                      48.4, 43.7, 51.2, 62.3, 66.1, 67.8, 81.1,
                      78.3, 63.7, 66.9, 67.0, 66.3, 63.1) %>% as.data.frame()
names(domesticReceipts) <- "domesticReceipts"

exportValue <- c(2.7, 2.6, 3.2, 0.6, 1.0, 1.6, 2.2, 3.0, 2.9,
                 3.8, 5.0, 5.1, 5.7, 6.5, 5.4, 5.5, 6.4,
                 7.5, 7.1, 6.8) %>% as.data.frame()
names(exportValue) <- "exportValue"

beefReceipts <- cbind(yearsReceipts, domesticReceipts, exportValue) %>% as.data.frame()


beefReceiptsDomestic_plots <- beefReceipts %>% select(Year, 
                                                      domesticReceipts)

tikz(file="introPlots/domesticReceipts.tex", width = 6.2, height = 3.5)

domesticReceipts_plot <- beefReceiptsDomestic_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=domesticReceipts, color="Cash Receipts")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefReceiptsDomestic_plots$Year[1],
                                  beefReceiptsDomestic_plots$Year[nrow(beefReceiptsDomestic_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Billion dollars") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(domesticReceipts_plot)

dev.off()




beefValueExport_plots <- beefReceipts %>% select(Year, exportValue)

tikz(file="introPlots/exportValue.tex", width = 6.2, height = 3.5)

exportValue_plot <- beefValueExport_plots %>% ggplot(aes(x=Year))+
  geom_line(aes(y=exportValue, color="Export Value")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(beefValueExport_plots$Year[1],
                                  beefValueExport_plots$Year[nrow(beefValueExport_plots)]))) + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) +
  scale_y_continuous(name="Billion dollars") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(exportValue_plot)

dev.off()





