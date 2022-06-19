
#### Optimistic Prices, Quantities
proj_Q_P_PostFMD_OPT_II_I


#### Pessimistic Prices, Quantities
proj_Q_P_PostFMD_PES_II_I


#### Baseline Prices
EQ_Prices_B <- EQ_PricesCosts %>% transmute(Year = Year, PsB = psMedian * 100, PcB = pcMedian * 100)

#### Baseline Quantities
EQ_Supplies_B <- EQ_Supplies %>% transmute(Year = Year, SlB = slMedian, ClB = clMedian)

EQ_Prices_Supplies_B <- merge(EQ_Prices_B, EQ_Supplies_B)


################# Surplus - OPTIMISTIC ############ 
# transmute(Year = Year, Ps5 = Ps5 * 100, 
#           Ps10 = Ps10 * 100, Ps20 = Ps20 * 100,
#           Sl5 = Sl5, Sl5_OG = Sl5_OG,
#           Sl10 = Sl10, Sl10_OG = Sl10_OG,
#           Sl20 = Sl20, Sl20_OG = Sl20_OG)

postFMD_PS_SL_OPT <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, Ps5, Ps10, Ps20,Sl5, Sl5_OG,
                                                          Sl10, Sl10_OG, Sl20, Sl20_OG)

postFMD_B_PS_SL_OPT <- merge(postFMD_PS_SL_OPT, EQ_Prices_Supplies_B %>% select(Year, PsB, SlB))

postFMD_B_PS_SL_OPT_Revenue <- postFMD_B_PS_SL_OPT %>% transmute(Year = Year, 
                                                                 rev5 = Ps5 * Sl5, rev5_OG = Ps5 * Sl5_OG,
                                                                 rev10 = Ps10 * Sl10, rev10_OG = Ps10 * Sl10_OG,
                                                                 rev20 = Ps20 * Sl20, rev20_OG = Ps20 * Sl20_OG,
                                                                 revB = PsB * SlB)


postFMD_PC_CL_OPT <- proj_Q_P_PostFMD_OPT_II_I %>% select(Year, Pc5, Pc10, Pc20,Cl5, Cl5_OG,
                                                          Cl10, Cl10_OG, Cl20, Cl20_OG)

postFMD_B_PC_CL_OPT <- merge(postFMD_PC_CL_OPT, EQ_Prices_Supplies_B %>% select(Year, PcB, ClB))

postFMD_B_PC_CL_OPT_Revenue <- postFMD_B_PC_CL_OPT %>% transmute(Year = Year, 
                                                                 rev5 = Pc5 * Cl5, rev5_OG = Pc5 * Cl5_OG,
                                                                 rev10 = Pc10 * Cl10, rev10_OG = Pc10 * Cl10_OG,
                                                                 rev20 = Pc20 * Cl20, rev20_OG = Pc20 * Cl20_OG,
                                                                 revB = PcB * ClB)



################# Surplus - PESSIMISTIC ############ 
# transmute(Year = Year, Ps5 = Ps5 * 100, 
#           Ps10 = Ps10 * 100, Ps20 = Ps20 * 100,
#           Sl5 = Sl5, Sl5_OG = Sl5_OG,
#           Sl10 = Sl10, Sl10_OG = Sl10_OG,
#           Sl20 = Sl20, Sl20_OG = Sl20_OG)

postFMD_PS_SL_PES <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, Ps5, Ps10, Ps20,Sl5, Sl5_OG,
                                                          Sl10, Sl10_OG, Sl20, Sl20_OG)

postFMD_B_PS_SL_PES <- merge(postFMD_PS_SL_PES, EQ_Prices_Supplies_B %>% select(Year, PsB, SlB))

postFMD_B_PS_SL_PES_Revenue <- postFMD_B_PS_SL_PES %>% transmute(Year = Year, 
                                                                 rev5 = Ps5 * Sl5, rev5_OG = Ps5 * Sl5_OG,
                                                                 rev10 = Ps10 * Sl10, rev10_OG = Ps10 * Sl10_OG,
                                                                 rev20 = Ps20 * Sl20, rev20_OG = Ps20 * Sl20_OG,
                                                                 revB = PsB * SlB)


postFMD_PC_CL_PES <- proj_Q_P_PostFMD_PES_II_I %>% select(Year, Pc5, Pc10, Pc20,Cl5, Cl5_OG,
                                                          Cl10, Cl10_OG, Cl20, Cl20_OG)

postFMD_B_PC_CL_PES <- merge(postFMD_PC_CL_PES, EQ_Prices_Supplies_B %>% select(Year, PcB, ClB))

postFMD_B_PC_CL_PES_Revenue <- postFMD_B_PC_CL_PES %>% transmute(Year = Year, 
                                                                 rev5 = Pc5 * Cl5, rev5_OG = Pc5 * Cl5_OG,
                                                                 rev10 = Pc10 * Cl10, rev10_OG = Pc10 * Cl10_OG,
                                                                 rev20 = Pc20 * Cl20, rev20_OG = Pc20 * Cl20_OG,
                                                                 revB = PcB * ClB)







