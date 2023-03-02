require(librarian)

librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes, pracma)

############################################################################################
############################## REGIONAL MODEL FIT ##########################################
############################################################################################

# Fixed Parameters
beta <- 0.98
delta <- 0.95

gamma0 <- 0.90
gamma1 <- 0.95

g <- 0.97
phi <- 0.63


################### We will start off with Texas, Oklahoma, and New Mexico ################

### Prices are in $/CWT
cows_pricesR <- read_excel("./RegionalData/PricesReceived_Cattle.xlsx") %>% as.data.frame()
cows_pricesR <- cows_pricesR %>% select(Year, Period, Value)

steersHeifers_PricesR <- read_excel("./RegionalData/PricesReceived_Steers_Heifers.xlsx") %>% as.data.frame()
steersHeifers_PricesR <- steersHeifers_PricesR %>% select(Year, Period, Value)

############################ converting the prices into yearly by taking the mean of the observed prices #########
pcsR <- cows_pricesR %>% group_by(Year) %>% mutate(pc = mean(Value)/100) %>% select(Year,pc) %>% 
  group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()
pssR <- steersHeifers_PricesR %>% group_by(Year) %>% mutate(ps = mean(Value)/100) %>% select(Year,ps) %>% 
  group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcs_cwtR <- pcsR %>% mutate(pcs_cwt = pc*100) %>% arrange(Year)
pss_cwtR <- pssR %>% mutate(pss_cwt = ps*100) %>% arrange(Year)
pc_ps_cwtR <- merge(pcs_cwtR, pss_cwtR) %>% select(Year,pss_cwt, pcs_cwt)

pc_psR <- merge(pcsR,pssR)





######################### Here we read the number of animals slaughtered steers, heifers, and cows ##################
cowsSlaughteredR <- read_excel("./RegionalData/CowsSlaughtered-Region6.xlsx") %>% as.data.frame()

cowsSlaughteredR <- cowsSlaughteredR %>% select(Year, Value) %>% mutate(CowsHead=Value) %>% 
  select(Year, CowsHead) %>% arrange(Year)

heifersSlaughteredR <- read_excel("./RegionalData/HeifersSlaughtered-Region6.xlsx") %>% as.data.frame()

heifersSlaughteredR <- heifersSlaughteredR %>% select(Year, Value) %>% mutate(HeifersHead=Value) %>% 
  select(Year, HeifersHead) %>% arrange(Year)

steersSlaughteredR <- read_excel("./RegionalData/SteersSlaughtered-Region6.xlsx") %>% as.data.frame()

steersSlaughteredR <- steersSlaughteredR %>% select(Year, Value) %>% mutate(SteersHead=Value) %>% 
  select(Year, SteersHead) %>% arrange(Year)



################ dressed weights 
dressedWeightsR <- read_excel("./RegionalData/DressedWeights.xlsx") %>% as.data.frame()
dressedWeightsR <- dressedWeightsR[-c(1:2),]
row.names(dressedWeightsR) <- 1:nrow(dressedWeightsR)
dressedWeightsR <- dressedWeightsR %>% separate(col = Date, into = c("Period", "Year")) %>% 
  select(Year,Period,everything())
dressedWeightsR$Period <- toupper(dressedWeightsR$Period)
dressedWeightsR$Year <- as.numeric(dressedWeightsR$Year)
dressedWeightsR <- dressedWeightsR %>% select(Year, Period, Cattle, Steers, Heifers, Cows)
######## computing the yearly dressed weights by taking the average of the observed #########
dressedWeightsR <- dressedWeightsR %>% group_by(Year) %>% 
  mutate(Cattle_avg = mean(Cattle), Steers_avg = mean(Steers), 
         Heifers_avg = mean(Heifers), Cows_avg = mean(Cows)) %>% 
  select(Year, Cattle_avg, Steers_avg, Heifers_avg, Cows_avg) %>% group_by(Year) %>%
  distinct() %>% ungroup() %>% as.data.frame()

dressedWeightsR <- dressedWeightsR %>% arrange(Year)

# We are missing data for some years. So I replace all the NA's with previous years data.
# If there is no data of previous year I replace them with next years data. 
# This is a naive approach and I am okay with this.
dressedWeightsR <- dressedWeightsR %>% 
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg) %>%
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg, .direction="up") %>% 
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg, .direction="down")


############ Here we convert the number of head to pounds in weight from the dressed weights data ###############

cowsSlaughteredR <- merge(cowsSlaughteredR,dressedWeightsR)  %>% transmute(Year = Year, Cowshead = CowsHead, 
                                                                        cull_meat = CowsHead * Cows_avg )

heifersSlaughteredR <- merge(heifersSlaughteredR,dressedWeightsR)  %>% transmute(Year = Year, HeifersHead = HeifersHead, 
                                                                              heifer_meat = HeifersHead * Heifers_avg)

steersSlaughteredR <- merge(steersSlaughteredR,dressedWeightsR)  %>% transmute(Year = Year, SteersHead = SteersHead, 
                                                                            steer_meat = SteersHead * Steers_avg)

totalDisappearedR <- merge(cowsSlaughteredR, merge(heifersSlaughteredR,steersSlaughteredR)) %>% 
  mutate(total_meat = cull_meat + heifer_meat + steer_meat)

totalDisappearedR <- totalDisappearedR %>% mutate(total_meat_bill = total_meat/1000000000, 
                                                cull_meat_bill = cull_meat/1000000000, 
                                                fed_meat_bill = (heifer_meat + steer_meat)/1000000000 )

dressedWeights_sl_clR <- dressedWeightsR %>% mutate(Slaughter_avg = Steers_avg, Cull_avg = Cows_avg)

dressedWeights_sl_clR <- dressedWeights_sl_clR %>% select(Year, Slaughter_avg, Cull_avg)


################## reading beef inventory (This is K in our model)  This is beef cows inventory data (so basically sum(k3 to k10))##################
beefInventoryR <- read_excel("./RegionalData/BeefCowInventory-TX-OK-NM.xlsx") %>% as.data.frame()

#### Since I have data of three states for every year, I group the data by year, sum the total inventory and arrange in a data frame
beefInventoryR <- beefInventoryR %>% select(Year, Period, State, Value) %>% group_by(Year) %>% 
  mutate(K = sum(Value)) %>% select(Year, K) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

KR <- beefInventoryR
KR <- KR %>% arrange(Year)

#################### reading replacement heifers (this is k3 in our model) ##############
replacementInventoryR <- read_excel("./RegionalData/ReplacementHeifers-TX-OK-NM.xlsx") %>% as.data.frame()

#### Since I have data of three states for every year, I group the data by year, sum the total inventory and arrange in a data frame
replacementInventoryR <- replacementInventoryR %>% select(Year, Period, State, Value) %>% group_by(Year) %>% 
  mutate(k3 = sum(Value)) %>% select(Year, k3) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

####### note that these replacement heifers are counted for next year in the population mechanics #####
k3R <- replacementInventoryR %>% select(Year,k3) %>% mutate(Year = Year + 1) %>% arrange(Year)

k4R <- delta * k3R$k3 %>% as.data.frame()
names(k4R) <- "k4"
k4R <- k4R %>% mutate(Year=k3R$Year+1)%>% select(Year,k4) %>% arrange(Year)

k5R <- delta * k4R$k4 %>% as.data.frame()
names(k5R) <- "k5"
k5R <- k5R %>% mutate(Year=k4R$Year+1) %>% select(Year,k5) %>% arrange(Year) 

k6R <- delta * k5R$k5 %>% as.data.frame()
names(k6R) <- "k6"
k6R <- k6R %>% mutate(Year = k5R$Year+1) %>% select(Year,k6) %>% arrange(Year) 

k7R <- delta * k6R$k6 %>% as.data.frame()
names(k7R) <- "k7"
k7R <- k7R %>% mutate(Year = k6R$Year+1) %>% select(Year,k7) %>% arrange(Year)

k8R <- delta * k7R$k7 %>% as.data.frame()
names(k8R) <- "k8"
k8R <- k8R %>% mutate(Year = k7R$Year+1) %>% select(Year,k8) %>% arrange(Year) 

stockListR <- list(KR, k3R, k4R, k5R, k6R, k7R, k8R)
StockR <- Reduce(function(...) merge(...), stockListR) %>% round()

StockR <- StockR %>% mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% 
  mutate(k9 = if_else(k9 < 0, 0, k9), k10 = 0)

# Determining the supply of fed cattle (in head)
supp_slR <- StockR %>% select(Year, K, k3) %>% 
  mutate(fedSlaughter = g * lag(K,1) - k3) %>% select(Year, fedSlaughter)

# Determining the supply of cull cows (in head)
supp_clR <-  StockR %>% select(Year, k7, k8, k9, k10) %>% 
  mutate(cowsCulled = k10 + (k9 - lead(k10,1)) + (k8 - lead(k9,1)) + (k7 - lead(k8,1)) ) %>% 
  select(Year, cowsCulled)

#putting dressed weights and supply together
supplyDressedWeightsListR <- list(dressedWeights_sl_clR, supp_slR, supp_clR)
supplyDressedWeightsR <- Reduce(function(...) merge(...), supplyDressedWeightsListR)

#Converting from number of head to pounds in meat
supp_slR <- supplyDressedWeightsR %>% select(Year, Slaughter_avg, fedSlaughter) %>% 
  mutate(fedSlaughter_BillLb = (fedSlaughter*(Slaughter_avg))/1000000000 ) %>% 
  select(Year, fedSlaughter, fedSlaughter_BillLb)

supp_clR <- supplyDressedWeightsR %>% select(Year, Cull_avg, cowsCulled) %>% 
  mutate(cowsCulled_BillLb = (cowsCulled*(Cull_avg))/1000000000 ) %>% 
  select(Year, cowsCulled, cowsCulled_BillLb)


############## here we simply add the fed cattle meat and cull meat for each year to find the total supply ######

totalSupplyR <- merge(supp_slR, supp_clR) %>% mutate(TotalSupply = fedSlaughter_BillLb + cowsCulled_BillLb)

totalDisappearedNewR <- totalDisappearedR  %>% select(Year, total_meat_bill)

supp_dissR <- merge(totalDisappearedNewR,totalSupplyR) %>% mutate(TotalDiss = total_meat_bill) %>% 
  select(Year, TotalSupply, TotalDiss)

supp_dissR %>% ggplot(aes(x=Year))+ geom_line(aes(y=TotalDiss,color="Dissapeared")) + 
  geom_point(aes(y=TotalDiss,color="Dissapeared")) +
  geom_line(aes(y=TotalSupply, color="Supply")) + 
  geom_point(aes(y=TotalSupply, color="Supply")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss$Year[1],
                                  supp_diss$Year[nrow(supp_diss)]))) + theme_classic() + 
  scale_y_continuous(name="Meat (in billion pounds)") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


supp_dissR <- supp_dissR %>% mutate(AdjFactor = TotalDiss/TotalSupply)

adjFactor_PlotR <- supp_dissR %>% ggplot(aes(x=Year))+geom_line(aes(y=AdjFactor,color="Adjustment Factor"))+
  geom_point(aes(y=AdjFactor,color="Adjustment Factor")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss$Year[1],
                                  supp_diss$Year[nrow(supp_diss)],2))) + theme_classic() + 
  scale_y_continuous(name="Adjustment Factor") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

adjFactor_NewR <- supp_dissR %>% mutate(Year = Year + 1) %>% select(Year, AdjFactor)

supp_dissR <- merge(adjFactor_NewR, supp_dissR %>% select(-AdjFactor))

supp_dissR <- merge(merge(supp_dissR, supp_slR), supp_clR)

supp_diss_adjR <- supp_dissR %>% select(Year, AdjFactor, TotalDiss, TotalSupply, 
                                      fedSlaughter_BillLb, cowsCulled_BillLb) %>% mutate( 
                                        TotalSupply = TotalSupply * AdjFactor, 
                                        fedSlaughter_BillLb = fedSlaughter_BillLb * AdjFactor,
                                        cowsCulled_BillLb = cowsCulled_BillLb * AdjFactor)

supp_diss_adjR %>% ggplot(aes(x=Year))+ geom_line(aes(y=TotalDiss,color="Dissapearence")) + 
  geom_point(aes(y=TotalDiss,color="Dissapearence")) +
  geom_line(aes(y=TotalSupply, color="Supply")) + 
  geom_point(aes(y=TotalSupply, color="Supply")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss_adj$Year[1],
                                  supp_diss_adj$Year[nrow(supp_diss_adj)],2))) + theme_classic() + 
  scale_y_continuous(name="Meat (in billion pounds)") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



#### We are in the case where the farmers cull the 9 year old cows
#### The holding costs will become. 
pc_ps_hcR <- pc_psR %>% mutate( hc = (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1))))
prices_costsR <- pc_ps_hcR %>% round(4)

####### I read calf crop data. These are in number of head
calf_cropR <- read_excel("./RegionalData/CalfCrop-TX-OK-NM.xlsx") %>% as.data.frame()

calf_cropR <- calf_cropR %>% select(Year, Period, State, Value) %>% group_by(Year) %>% 
  mutate(calfCrop = sum(Value)) %>% select(Year, calfCrop) %>% group_by(Year) %>% distinct() %>% ungroup() %>% 
  as.data.frame() %>% arrange(Year)

calfCrop_replacementHeifersR <- merge(calf_cropR, replacementInventoryR) %>% transmute(Year = Year, calfCrop = calfCrop,
                                                                                    repHeifers = k3)

##### Here I am computing the ratio of the replacement heifers to calf crop of prev year
calfCrop_replacementHeifersR <- calfCrop_replacementHeifersR %>% mutate(
  calfCrop_repHeifers_Ratio = repHeifers/lag(calfCrop),
  calfCrop_repHeifers_Percent = repHeifers/lag(calfCrop) * 100)

summary(calfCrop_replacementHeifersR$calfCrop_repHeifers_Percent)

#### Here I am getting an approximate percentage of the progeny of the total stock that are added into 
#### breeding stock as replacement heifers. Note that here I am taking replacement heifers two periods ahead
#### Because if the cow gives birth this period the heifers are added into the breeding stock two periods from now
summary(StockR %>% select(Year, K, k3) %>% mutate(ratios = lead(k3,2)/(g*K)) %>% select(ratios))

# ratios      
# Min.   :0.1457  
# 1st Qu.:0.1759  
# Median :0.2063  
# Mean   :0.2350  
# 3rd Qu.:0.2908 
# Max.   :0.3727  
# NA's   :2 

#### From the above on average approximately 23% of the heifers are added into the breeding stock 

#### Here I read corn price data. These are in $/bushel. 
#### I am converting the price from $/bushel to $/pound
corn_priceR <- read_excel("./RegionalData/CornPriceReceived-TX-OK-NM.xlsx") %>% as.data.frame()
names(corn_priceR)
corn_priceR <- corn_priceR %>% select(Year, Period, Value)
pcornR <- corn_priceR %>% group_by(Year) %>% mutate(pcorn = round(mean(Value),3)) %>% 
  select(Year,pcorn) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcornR <- pcornR %>% mutate(pcornLb = pcorn/56)



allPricesR <- merge(pcornR, prices_costsR) 

meat_billR <- supp_dissR %>% 
  mutate(TS = TotalSupply, TD = TotalDiss, sl = fedSlaughter_BillLb, cl = cowsCulled_BillLb) %>%
  select(Year, sl, cl, TS, TD)

prices_quantR <- merge(allPricesR, meat_billR) %>% round(3)




