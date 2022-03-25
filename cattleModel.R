require(librarian)

librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes)

# Parameters
beta <- 0.98
delta <- 0.95

gamma0 <- 0.90
gamma1 <- 0.95

g <- 0.97
phi <- 0.63

############# Cattle totals (these are numbers of total cattle in any year not age dist.) in 1000 head. Downloaded from PSD ##########
cattle_totals <- read_excel("Data/Latest-03-24/CattleTotals.xlsx") %>% as.data.frame()

cattle_tot <- cattle_totals %>% select(-Attribute, -Commodity, -Country, -`Unit Description`)
cattle_tot <- data.frame(t(cattle_tot))
colnames(cattle_tot) <- cattle_totals[,2]
cattle_tot <- cattle_tot %>% mutate(Year = c(rownames(cattle_tot))) %>% select(Year, everything())
cattle_tot[,-1] <- cattle_tot[,-1] * 1000

### Prices are in $/CWT
cows_prices <- read_excel("Data/Latest-03-24/PricesReceived_Cattle.xlsx") %>% as.data.frame()
cows_prices <- cows_prices %>% select(Year, Period, Value)

steersHeifers_Prices <- read_excel("Data/Latest-03-24/PricesReceived_Steers_Heifers.xlsx") %>% as.data.frame()
steersHeifers_Prices <- steersHeifers_Prices %>% select(Year, Period, Value)

############################ converting the prices into yearly by taking the mean of the observed prices #########
pcs <- cows_prices %>% group_by(Year) %>% mutate(pc = mean(Value)/100) %>% select(Year,pc) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()
pss <- steersHeifers_Prices %>% group_by(Year) %>% mutate(ps = mean(Value)/100) %>% select(Year,ps) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcs_cwt <- pcs %>% mutate(pcs_cwt = pc*100)
pss_cwt <- pss %>% mutate(pss_cwt = ps*100)
pc_ps_cwt <- merge(pcs_cwt, pss_cwt) %>% filter(Year>=1993) %>% select(Year,pss_cwt, pcs_cwt)

pc_ps <- merge(pcs,pss) %>% filter(Year>=1993)

######################### Here we read the number of animals slaughtered steers, heifers, and cows ##################
cowsSlaughtered <- read_excel("Data/Latest-03-24/Slaughtered_Cows.xlsx") %>% as.data.frame()

cowsSlaughtered <- cowsSlaughtered %>% select(Year, Value) %>% mutate(CowsHead=Value) %>% select(Year, CowsHead) %>% arrange(Year)

heifersSlaughtered <- read_excel("Data/Latest-03-24/Slaughtered_Heifers.xlsx") %>% as.data.frame()

heifersSlaughtered <- heifersSlaughtered %>% select(Year, Value) %>% mutate(HeifersHead=Value) %>% select(Year, HeifersHead) %>% arrange(Year)

steersSlaughtered <- read_excel("Data/Latest-03-24/Slaughtered_Steers.xlsx") %>% as.data.frame()

steersSlaughtered <- steersSlaughtered %>% select(Year, Value) %>% mutate(SteersHead=Value) %>% select(Year, SteersHead) %>% arrange(Year)



################ dressed weights 
dressedWeights <- read_excel("Data/Latest-03-24/DressedWeights.xlsx") %>% as.data.frame()
dressedWeights <- dressedWeights[-c(1:2),]
row.names(dressedWeights) <- 1:nrow(dressedWeights)
dressedWeights <- dressedWeights %>% separate(col = Date, into = c("Period", "Year")) %>% select(Year,Period,everything())
dressedWeights$Period <- toupper(dressedWeights$Period)
dressedWeights$Year <- as.numeric(dressedWeights$Year)
dressedWeights <- dressedWeights %>% select(Year, Period, Cattle, Steers, Heifers, Cows)
######## computing the yearly dressed weights by taking the average of the observed #########
dressedWeights <- dressedWeights %>% group_by(Year) %>% 
  mutate(Cattle_avg = mean(Cattle), Steers_avg = mean(Steers), 
         Heifers_avg = mean(Heifers), Cows_avg = mean(Cows)) %>% 
  select(Year, Cattle_avg, Steers_avg, Heifers_avg, Cows_avg) %>% group_by(Year) %>%
  distinct() %>% ungroup() %>% as.data.frame()

dressedWeights <- dressedWeights %>% arrange(Year)

# We are missing data for some years. So I replace all the NA's with previous years data.
# If there is no data of previous year I replace them with next years data. 
# This is a naive approach and I am okay with this.
dressedWeights <- dressedWeights %>% 
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg) %>%
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg, .direction="up") %>% 
  fill(Cattle_avg, Steers_avg, Heifers_avg, Cows_avg, .direction="down")


############ Here we convert the number of head to pounds in weight from the dressed weights data ###############

cowsSlaughtered <- merge(cowsSlaughtered,dressedWeights)  %>% transmute(Year = Year, Cowshead = CowsHead, 
                                                                        cull_meat = CowsHead * Cows_avg )

heifersSlaughtered <- merge(heifersSlaughtered,dressedWeights)  %>% transmute(Year = Year, HeifersHead = HeifersHead, 
                                                                              heifer_meat = HeifersHead * Heifers_avg)

steersSlaughtered <- merge(steersSlaughtered,dressedWeights)  %>% transmute(Year = Year, SteersHead = SteersHead, 
                                                                            steer_meat = SteersHead * Steers_avg)

totalDisappeared <- merge(cowsSlaughtered, merge(heifersSlaughtered,steersSlaughtered)) %>% 
  mutate(total_meat = cull_meat + heifer_meat + steer_meat)

totalDisappeared <- totalDisappeared %>% mutate(total_meat_bill = total_meat/1000000000, cull_meat_bill = cull_meat/1000000000, 
                                                fed_meat_bill = (heifer_meat + steer_meat)/1000000000 )

dressedWeights_sl_cl <- dressedWeights %>% mutate(Slaughter_avg = Steers_avg, Cull_avg = Cows_avg)

dressedWeights_sl_cl <- dressedWeights_sl_cl %>% select(Year, Slaughter_avg, Cull_avg)


################## reading beef inventory (This is K in our model)  This is beef cows inventory data (so basically sum(k3 to k10))##################
beefInventory <- read_excel("Data/Latest-03-24/BeefCowsInventory.xlsx") %>% as.data.frame()

beefInventory <- beefInventory %>% select(Year, Period, Value) %>% mutate(K = Value) %>% select(Year,K)

K <- beefInventory
K <- K %>% arrange(Year)

#################### reading replacement heifers (this is k3 in our model) ##############
replacementInventory <- read_excel("Data/Latest-03-24/ReplacementHeifersInventory.xlsx") %>% as.data.frame()

replacementInventory <- replacementInventory %>% select(Year, Period, Value) %>% mutate (k3 = Value) %>% select(Year, k3)

####### note that these replacement heifers are counted for next year in the population mechanics #####
k3 <- replacementInventory %>% select(Year,k3) %>% mutate(Year = Year + 1) %>% arrange(Year)

k4 <- delta * k3$k3 %>% as.data.frame()
names(k4) <- "k4"
k4 <- k4 %>% mutate(Year=k3$Year+1)%>% select(Year,k4) %>% arrange(Year) 

k5 <- delta * k4$k4 %>% as.data.frame()
names(k5) <- "k5"
k5 <- k5 %>% mutate(Year=k4$Year+1) %>% select(Year,k5) %>% arrange(Year) 

k6 <- delta * k5$k5 %>% as.data.frame()
names(k6) <- "k6"
k6 <- k6 %>% mutate(Year = k5$Year+1) %>% select(Year,k6) %>% arrange(Year) 

k7 <- delta * k6$k6 %>% as.data.frame()
names(k7) <- "k7"
k7 <- k7 %>% mutate(Year = k6$Year+1) %>% select(Year,k7) %>% arrange(Year) 

k8 <- delta * k7$k7 %>% as.data.frame()
names(k8) <- "k8"
k8 <- k8 %>% mutate(Year = k7$Year+1) %>% select(Year,k8) %>% arrange(Year) 


stockList <- list(K, k3, k4, k5, k6, k7, k8)
Stock <- Reduce(function(...) merge(...), stockList)

Stock <- Stock %>% mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9), k10 = 0)

exports <- cattle_tot %>% select(Year, Exports)
imports <- cattle_tot %>% select(Year, Imports)

# putting stocks, imports and exports together
stocksImportsExportsList <- list(imports, exports, Stock)
stocksImportsExports <- Reduce(function(...) merge(...), stocksImportsExportsList)

# Determining the supply of fed cattle (in head)
supp_sl <- stocksImportsExports %>% select(Year, Imports, Exports, K, k3) %>% 
  mutate(fedSlaughter = g * lag(K,1) - lead(k3,1) + Imports - Exports) %>% select(Year, fedSlaughter)
  
# Determining the supply of cull cows (in head)
supp_cl <-  stocksImportsExports %>% select(Year, k7, k8, k9, k10) %>% 
  mutate(cowsCulled = k10 + (k9 - lead(k10,1)) + (k8 - lead(k9,1)) + (k7 - lead(k8)) ) %>% select(Year, cowsCulled)

#putting dressed weights and supply together
supplyDressedWeightsList <- list(dressedWeights_sl_cl, supp_sl, supp_cl)
supplyDressedWeights <- Reduce(function(...) merge(...), supplyDressedWeightsList)

#Converting from number of head to pounds in meat
supp_sl <- supplyDressedWeights %>% select(Year, Slaughter_avg, fedSlaughter) %>% 
  mutate(fedSlaughter_BillLb = (fedSlaughter*(Slaughter_avg))/1000000000 ) %>% 
  select(Year, fedSlaughter, fedSlaughter_BillLb)

supp_cl <- supplyDressedWeights %>% select(Year, Cull_avg, cowsCulled) %>% 
  mutate(cowsCulled_BillLb = (cowsCulled*(Cull_avg))/1000000000 ) %>% 
  select(Year, cowsCulled, cowsCulled_BillLb)

############## here we simply add the fed cattle meat and cull meat for each year to find the total supply ######

totalSupply <- merge(supp_sl, supp_cl) %>% mutate(TotalSupply = fedSlaughter_BillLb + cowsCulled_BillLb)

totalDisappearedNew <- totalDisappeared  %>% select(Year, total_meat_bill)

supp_diss <- merge(totalDisappearedNew,totalSupply) %>% mutate(TotalDiss = total_meat_bill) %>% 
  select(Year, TotalSupply, TotalDiss)

supp_diss %>% ggplot(aes(x=Year))+ geom_line(aes(y=TotalDiss,color="Demand")) + 
  geom_point(aes(y=TotalDiss,color="Demand")) +
  geom_line(aes(y=TotalSupply, color="Supply")) + 
  geom_point(aes(y=TotalSupply, color="Supply")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss$Year[1],
                                  supp_diss$Year[nrow(supp_diss)],2))) + theme_classic() + 
  scale_y_continuous(name="Meat (in billion pounds)") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


supp_diss <- supp_diss %>% mutate(AdjFactor = TotalDiss/TotalSupply)

adjFactor_Plot <- supp_diss %>% ggplot(aes(x=Year))+geom_line(aes(y=AdjFactor,color="Adjustment Factor"))+
  geom_point(aes(y=AdjFactor,color="Adjustment Factor")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss$Year[1],
                                  supp_diss$Year[nrow(supp_diss)],2))) + theme_classic() + 
  scale_y_continuous(name="Adjustment Factor") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

adjFactor_New <- supp_diss %>% mutate(Year = Year + 1) %>% select(Year, AdjFactor)

supp_diss <- merge(adjFactor_New, supp_diss %>% select(-AdjFactor))

supp_diss <- merge(merge(supp_diss, supp_sl), supp_cl)

supp_diss_adj <- supp_diss %>% select(Year, AdjFactor, TotalDiss, TotalSupply, 
                                      fedSlaughter_BillLb, cowsCulled_BillLb) %>% mutate( 
                                        TotalSupply = TotalSupply * AdjFactor, 
                                        fedSlaughter_BillLb = fedSlaughter_BillLb * AdjFactor,
                                        cowsCulled_BillLb = cowsCulled_BillLb * AdjFactor)

supp_diss_adj %>% ggplot(aes(x=Year))+ geom_line(aes(y=TotalDiss,color="Demand")) + 
  geom_point(aes(y=TotalDiss,color="Demand")) +
  geom_line(aes(y=TotalSupply, color="Supply")) + 
  geom_point(aes(y=TotalSupply, color="Supply")) +
  scale_x_continuous(name="Year", 
                     breaks=c(seq(supp_diss_adj$Year[1],
                                  supp_diss_adj$Year[nrow(supp_diss_adj)],2))) + theme_classic() + 
  scale_y_continuous(name="Meat (in billion pounds)") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



  
  





