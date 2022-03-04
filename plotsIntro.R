#### Plots for introduction
require(tikzDevice)


pc_ps_cwt_plots <- merge(pcs_cwt, pss_cwt) %>% select(Year,pss_cwt, pcs_cwt) %>% 
  transmute(Year = Year, PS = pss_cwt, PC = pcs_cwt) %>% round(2) %>% filter(Year > 1969)

tikz(file="introPlots/PricesReceived.tex", width=7, height=3.5)

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

tikz(file="introPlots/CattleCycle.tex", width=7, height=3.5)

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














