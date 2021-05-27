
####### I read calf crop data. These are in number of head
calf_crop <- read_excel("Data/New/CalfCrop.xlsx") %>% as.data.frame()

calf_crop <- calf_crop %>% select(Year, Value) %>% transmute(Year = Year, calfCrop = Value)

calfCrop_replacementHeifers <- merge(calf_crop, replacementInventory) %>% transmute(Year = Year, calfCrop = calfCrop,
                                                                                    repHeifers = k3)

##### Here I am computing the ratio of the replacement heifers to calf crop of prv year
calfCrop_replacementHeifers <- calfCrop_replacementHeifers %>% mutate(calfCrop_repHeifers_Ratio = repHeifers/lag(calfCrop), 
                                       calfCrop_repHeifers_Percent = repHeifers/lag(calfCrop) * 100)

summary(calfCrop_replacementHeifers$calfCrop_repHeifers_Percent)





#### Here I read corn price data. These are in $/bushel
corn_price <- read_excel("Data/New/CornPrices_Monthly.xlsx") %>% as.data.frame()
names(corn_price)
corn_price <- corn_price %>% select(Year, Period, Value)
pcorn <- corn_price %>% group_by(Year) %>% mutate(pcorn = round(mean(Value),3)) %>% 
  select(Year,pcorn) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

allPrices <- merge(pcorn, prices_costs)

meat_bill <- merge(supp_sl, merge(supp_cl, merge(totalSupply, totalDisappearedNew))) %>% mutate(
  sl = Bill_meatLb_sl, cl = Bill_meatLb_cl, TS = TotalSupply, TD = total_meat_bill) %>% select(
    Year, sl, cl, TS, TD)

prices_quant <- merge(allPrices, meat_bill)

# In order to construct the shocks I need to estimate the quantities and see the observed ones. 