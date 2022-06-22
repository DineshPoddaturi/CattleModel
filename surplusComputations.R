
#### Optimistic Prices, Quantities
proj_Q_P_PostFMD_OPTI

#### Pessimistic Prices, Quantities
proj_Q_P_PostFMD_PESI


#### Baseline Prices
EQ_Prices_B <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian, PcB = pcMedian)

#### Baseline Quantities
EQ_Supplies_B <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

EQ_Prices_Supplies_B <- merge(EQ_Prices_B, EQ_Supplies_B)


################# Surplus - OPTIMISTIC ############ 
# transmute(Year = Year, Ps5 = Ps5 * 100, 
#           Ps10 = Ps10 * 100, Ps20 = Ps20 * 100,
#           Sl5 = Sl5, Sl5_OG = Sl5_OG,
#           Sl10 = Sl10, Sl10_OG = Sl10_OG,
#           Sl20 = Sl20, Sl20_OG = Sl20_OG)

postFMD_PS_SL_OPT <- proj_Q_P_PostFMD_OPTI %>% select(Year, Ps5, Ps10, Ps20,Sl5, Sl5_OG,
                                                          Sl10, Sl10_OG, Sl20, Sl20_OG)

postFMD_B_PS_SL_OPT <- merge(postFMD_PS_SL_OPT, EQ_Prices_Supplies_B %>% select(Year, PsB, SlB))

postFMD_B_PS_SL_OPT_Revenue <- postFMD_B_PS_SL_OPT %>% transmute(Year = Year, 
                                                                 revSl5 = Ps5 * Sl5, revSl5_OG = Ps5 * Sl5_OG,
                                                                 revSl10 = Ps10 * Sl10, revSl10_OG = Ps10 * Sl10_OG,
                                                                 revSl20 = Ps20 * Sl20, revSl20_OG = Ps20 * Sl20_OG,
                                                                 revSlB = PsB * SlB)


postFMD_PC_CL_OPT <- proj_Q_P_PostFMD_OPTI %>% select(Year, Pc5, Pc10, Pc20,Cl5, Cl5_OG,
                                                          Cl10, Cl10_OG, Cl20, Cl20_OG)

postFMD_B_PC_CL_OPT <- merge(postFMD_PC_CL_OPT, EQ_Prices_Supplies_B %>% select(Year, PcB, ClB))

postFMD_B_PC_CL_OPT_Revenue <- postFMD_B_PC_CL_OPT %>% transmute(Year = Year, 
                                                                 revCl5 = Pc5 * Cl5, revCl5_OG = Pc5 * Cl5_OG,
                                                                 revCl10 = Pc10 * Cl10, revCl10_OG = Pc10 * Cl10_OG,
                                                                 revCl20 = Pc20 * Cl20, revCl20_OG = Pc20 * Cl20_OG,
                                                                 revClB = PcB * ClB)


totalRevenue_OPT <- merge(postFMD_B_PS_SL_OPT_Revenue, postFMD_B_PC_CL_OPT_Revenue)

totalRevenue_OPT_TR <- totalRevenue_OPT %>% transmute(Year = Year, 
                                                      TR5 = revSl5 + revCl5, TR5_OG = revSl5_OG + revCl5_OG,
                                                      TR10 = revSl10 + revCl10, TR10_OG = revSl10_OG + revCl10_OG,
                                                      TR20 = revSl20 + revCl20, TR20_OG = revSl20_OG + revCl20_OG,
                                                      TRB = revSlB + revClB) %>% round(3)



################# Surplus - PESSIMISTIC ############ 
# transmute(Year = Year, Ps5 = Ps5 * 100, 
#           Ps10 = Ps10 * 100, Ps20 = Ps20 * 100,
#           Sl5 = Sl5, Sl5_OG = Sl5_OG,
#           Sl10 = Sl10, Sl10_OG = Sl10_OG,
#           Sl20 = Sl20, Sl20_OG = Sl20_OG)

postFMD_PS_SL_PES <- proj_Q_P_PostFMD_PESI %>% select(Year, Ps5, Ps10, Ps20,Sl5, Sl5_OG,
                                                          Sl10, Sl10_OG, Sl20, Sl20_OG)

postFMD_B_PS_SL_PES <- merge(postFMD_PS_SL_PES, EQ_Prices_Supplies_B %>% select(Year, PsB, SlB))

postFMD_B_PS_SL_PES_Revenue <- postFMD_B_PS_SL_PES %>% transmute(Year = Year, 
                                                                 revSl5 = Ps5 * Sl5, revSl5_OG = Ps5 * Sl5_OG,
                                                                 revSl10 = Ps10 * Sl10, revSl10_OG = Ps10 * Sl10_OG,
                                                                 revSl20 = Ps20 * Sl20, revSl20_OG = Ps20 * Sl20_OG,
                                                                 revSlB = PsB * SlB)


postFMD_PC_CL_PES <- proj_Q_P_PostFMD_PESI %>% select(Year, Pc5, Pc10, Pc20,Cl5, Cl5_OG,
                                                          Cl10, Cl10_OG, Cl20, Cl20_OG)

postFMD_B_PC_CL_PES <- merge(postFMD_PC_CL_PES, EQ_Prices_Supplies_B %>% select(Year, PcB, ClB))

postFMD_B_PC_CL_PES_Revenue <- postFMD_B_PC_CL_PES %>% transmute(Year = Year, 
                                                                 revCl5 = Pc5 * Cl5, revCl5_OG = Pc5 * Cl5_OG,
                                                                 revCl10 = Pc10 * Cl10, revCl10_OG = Pc10 * Cl10_OG,
                                                                 revCl20 = Pc20 * Cl20, revCl20_OG = Pc20 * Cl20_OG,
                                                                 revClB = PcB * ClB)

totalRevenue_PES <- merge(postFMD_B_PS_SL_PES_Revenue, postFMD_B_PC_CL_PES_Revenue)

totalRevenue_PES_TR <- totalRevenue_PES %>% transmute(Year = Year, 
                                                      TR5 = revSl5 + revCl5, TR5_OG = revSl5_OG + revCl5_OG,
                                                      TR10 = revSl10 + revCl10, TR10_OG = revSl10_OG + revCl10_OG,
                                                      TR20 = revSl20 + revCl20, TR20_OG = revSl20_OG + revCl20_OG,
                                                      TRB = revSlB + revClB) %>% round(3)



###### Percentage change in revenues OPTIMISTIC

totalRevenue_OPT_TR_PercentChange <- totalRevenue_OPT_TR %>%
  transmute(Year, 
            TR5Percent = (((TR5-TRB)/TRB) * 100), TR10Percent = (((TR10-TRB)/TRB) * 100),
            TR20Percent =(((TR20-TRB)/TRB) * 100), TROG5Percent = (((TR5_OG-TRB)/TRB) * 100), 
            TROG10Percent = (((TR10_OG-TRB)/TRB) * 100),TROG20Percent =(((TR20_OG-TRB)/TRB) * 100)) %>% round(3)


totalRevenue_OPT_PercentChangePlot <- totalRevenue_OPT_TR_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TROG5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TROG5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = TROG10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TROG10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = TROG20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = TROG20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(totalRevenue_OPT_TR_PercentChange$Year[1],
                                  totalRevenue_OPT_TR_PercentChange$Year[nrow(totalRevenue_OPT_TR_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in total revenue from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")



totalRevenue_PES_TR_PercentChange <- totalRevenue_PES_TR %>%
  transmute(Year, 
            TR5Percent = (((TR5-TRB)/TRB) * 100), TR10Percent = (((TR10-TRB)/TRB) * 100),
            TR20Percent =(((TR20-TRB)/TRB) * 100), TROG5Percent = (((TR5_OG-TRB)/TRB) * 100), 
            TROG10Percent = (((TR10_OG-TRB)/TRB) * 100),TROG20Percent =(((TR20_OG-TRB)/TRB) * 100)) %>% round(3)

totalRevenue_PES_PercentChangePlot <- totalRevenue_PES_TR_PercentChange %>% ggplot(aes(x = Year))+
  geom_line(aes(y = TROG5Percent, color="5% Depop"),size=1.1) +
  geom_point(aes(y = TROG5Percent, color = "5% Depop"),size=2) +
  geom_line(aes(y = TROG10Percent, color="10% Depop"),size=1.1) +
  geom_point(aes(y = TROG10Percent, color="10% Depop"),size=2) +
  geom_line(aes(y = TROG20Percent, color="20% Depop"),size=1.1) +
  geom_point(aes(y = TROG20Percent, color="20% Depop"),size=2) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(totalRevenue_PES_TR_PercentChange$Year[1],
                                  totalRevenue_PES_TR_PercentChange$Year[nrow(totalRevenue_PES_TR_PercentChange)])))+ 
  scale_y_continuous(name="Percent change in total revenue from baseline")  + theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) + geom_hline(yintercept=0, linetype="dashed")




