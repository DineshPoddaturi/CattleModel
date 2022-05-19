# Plotting the shares

mu_Tildes_eq[1:25,]
s_Tildes_eq[1:25,]

prices_ps_eq[1:25,]
prices_pc_eq[1:25,]

shares_eq <- ((exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,]))/
    (1 + (exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,])))) 

shares_eqMean <- apply(shares_eq, 2, mean) %>% as.data.frame()
names(shares_eqMean) <- "shareMean"
shares_eqMean <- shares_eqMean %>% mutate(Year = quantities_prices_capK$Year) %>%
  select(Year, everything())

shares_eqMeanPlot <- shares_eqMean %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMean, color="Share Mean"),size=1.1) +
  geom_point(aes(y = shareMean, color = "Share Mean"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(shares_eqMean$Year[1],shares_eqMean$Year[nrow(shares_eqMean)]))) +
  scale_y_continuous(name="Share Mean")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


shares_eqMed <- apply(shares_eq, 2, median) %>% as.data.frame() %>% round(3)
names(shares_eqMed) <- "shareMedian"
shares_eqMed <- shares_eqMed %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

shares_eqMedPlot <- shares_eqMed %>% ggplot(aes(x=Year))+geom_line(aes(y=shareMedian, color="Share Median"),size=1.1) +
  geom_point(aes(y = shareMedian, color = "Share Median"),size=2) + theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(shares_eqMed$Year[1],shares_eqMed$Year[nrow(shares_eqMed)]))) +
  scale_y_continuous(name="Share Median")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 12),
                                              axis.text.y = element_text(size = 12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))



# shares_eq <- ((exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,]))/
#                 (1 + (exp((mu_Tildes_eq[1:25,] - ((prices_ps_eq[1:25,]/phi) - (prices_pc_eq[1:25,]/phi)))/s_Tildes_eq[1:25,])))) %>% as.data.frame() 
# 
# names(shares_eq) <- seq(from=1990, to=2020)
# 
# 
# shares_eq <- shares_eq %>% mutate(Id = rownames(shares_eq)) %>% select(Id, everything())
# 
# dfShares <- melt(shares_eq, id.vars = 'Id', variable.name = 'year')
# 
# ggplot(dfShares, aes (value)) +
#   geom_density() +
#   facet_wrap(~year)






