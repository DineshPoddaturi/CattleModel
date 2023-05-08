require(librarian)

librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes, pracma)

# Fixed Parameters
beta <- 0.98
delta <- 0.95

gamma0 <- 0.90
gamma1 <- 0.95

g <- 0.97
phi <- 0.63

############# Cattle totals (these are numbers of total cattle in any year not age dist.) in 1000 head. Downloaded from PSD ##########
cattle_totals <- read_excel("./Data/Latest-04-2023/CattleTotals.xlsx") %>% as.data.frame()

cattle_tot <- cattle_totals %>% select(-Attribute, -Commodity, -Country, -`Unit Description`)
cattle_tot <- data.frame(t(cattle_tot))
colnames(cattle_tot) <- cattle_totals[,2]
cattle_tot <- cattle_tot %>% mutate(Year = c(rownames(cattle_tot))) %>% select(Year, everything())
cattle_tot[,-1] <- cattle_tot[,-1] * 1000

### Prices are in $/CWT
cows_prices <- read_excel("./Data/Latest-04-2023/PricesReceived_Cattle.xlsx") %>% as.data.frame()
cows_prices <- cows_prices %>% select(Year, Period, Value)

steersHeifers_Prices <- read_excel("./Data/Latest-04-2023/PricesReceived_Steers_Heifers.xlsx") %>% as.data.frame()
steersHeifers_Prices <- steersHeifers_Prices %>% select(Year, Period, Value)

############################ converting the prices into yearly by taking the mean of the observed prices #########
pcs <- cows_prices %>% group_by(Year) %>% mutate(pc = mean(Value)/100) %>% select(Year,pc) %>% 
  group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()
pss <- steersHeifers_Prices %>% group_by(Year) %>% mutate(ps = mean(Value)/100) %>% select(Year,ps) %>% 
  group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcs_cwt <- pcs %>% mutate(pcs_cwt = pc*100) %>% arrange(Year)
pss_cwt <- pss %>% mutate(pss_cwt = ps*100) %>% arrange(Year)
pc_ps_cwt <- merge(pcs_cwt, pss_cwt) %>% select(Year,pss_cwt, pcs_cwt)

pc_ps <- merge(pcs,pss)

######################### Here we read the number of animals slaughtered steers, heifers, and cows ##################
cowsSlaughtered <- read_excel("./Data/Latest-04-2023/Slaughtered_Cows.xlsx") %>% as.data.frame()

cowsSlaughtered <- cowsSlaughtered %>% select(Year, Value) %>% mutate(CowsHead=Value) %>% 
  select(Year, CowsHead) %>% arrange(Year)

heifersSlaughtered <- read_excel("./Data/Latest-04-2023/Slaughtered_Heifers.xlsx") %>% as.data.frame()

heifersSlaughtered <- heifersSlaughtered %>% select(Year, Value) %>% mutate(HeifersHead=Value) %>% 
  select(Year, HeifersHead) %>% arrange(Year)

steersSlaughtered <- read_excel("./Data/Latest-04-2023/Slaughtered_Steers.xlsx") %>% as.data.frame()

steersSlaughtered <- steersSlaughtered %>% select(Year, Value) %>% mutate(SteersHead=Value) %>% 
  select(Year, SteersHead) %>% arrange(Year)



################ dressed weights 
dressedWeights <- read_excel("./Data/Latest-04-2023/DressedWeights.xlsx") %>% as.data.frame()
dressedWeights <- dressedWeights[-c(1:2),]
row.names(dressedWeights) <- 1:nrow(dressedWeights)
dressedWeights <- dressedWeights %>% separate(col = Date, into = c("Period", "Year")) %>% 
  select(Year,Period,everything())
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

totalDisappeared <- totalDisappeared %>% mutate(total_meat_bill = total_meat/1000000000, 
                                                cull_meat_bill = cull_meat/1000000000, 
                                                fed_meat_bill = (heifer_meat + steer_meat)/1000000000 )

dressedWeights_sl_cl <- dressedWeights %>% mutate(Slaughter_avg = Steers_avg, Cull_avg = Cows_avg)

dressedWeights_sl_cl <- dressedWeights_sl_cl %>% select(Year, Slaughter_avg, Cull_avg)


################## reading beef inventory (This is K in our model)  This is beef cows inventory data (so basically sum(k3 to k10))##################
beefInventory <- read_excel("./Data/Latest-04-2023/BeefCowsInventory.xlsx") %>% as.data.frame()

beefInventory <- beefInventory %>% select(Year, Period, Value) %>% mutate(K = Value) %>% select(Year,K)

K <- beefInventory
K <- K %>% arrange(Year)

#################### reading replacement heifers (this is k3 in our model) ##############
replacementInventory <- read_excel("./Data/Latest-04-2023/ReplacementHeifersInventory.xlsx") %>% as.data.frame()

replacementInventory <- replacementInventory %>% select(Year, Period, Value) %>% mutate (k3 = Value) %>% 
  select(Year, k3)

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
stocksImportsExports$Year <- as.numeric(stocksImportsExports$Year)

# Determining the supply of fed cattle (in head)
supp_sl <- stocksImportsExports %>% select(Year, Imports, Exports, K, k3) %>% 
  mutate(fedSlaughter = g * lag(K,1) - k3 + Imports - Exports) %>% select(Year, fedSlaughter)
  
# Determining the supply of cull cows (in head)
supp_cl <-  stocksImportsExports %>% select(Year, k7, k8, k9, k10) %>% 
  mutate(cowsCulled = k10 + (k9 - lead(k10,1)) + (k8 - lead(k9,1)) + (k7 - lead(k8,1)) ) %>% select(Year, cowsCulled)

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
                                  supp_diss$Year[nrow(supp_diss)]))) + theme_classic() + 
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

adjFactor_New <- supp_diss %>% mutate(Year = Year+1) %>% select(Year, AdjFactor)

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

#### We are in the case where the farmers cull the 9 year old cows
#### The holding costs will become. 
pc_ps_hc <- pc_ps %>% mutate( hc = (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1))))
prices_costs <- pc_ps_hc %>% round(4)


##################################################################################################

####### I read calf crop data. These are in number of head
calf_crop <- read_excel("./Data/Latest-04-2023/CalfCrop.xlsx") %>% as.data.frame()

calf_crop <- calf_crop %>% select(Year, Value) %>% transmute(Year = Year, calfCrop = Value)

calfCrop_replacementHeifers <- merge(calf_crop, replacementInventory) %>% transmute(Year = Year, calfCrop = calfCrop,
                                                                                    repHeifers = k3)

##### Here I am computing the ratio of the replacement heifers to calf crop of previous year
calfCrop_replacementHeifers <- calfCrop_replacementHeifers %>% mutate(
  calfCrop_repHeifers_Ratio = repHeifers/lag(calfCrop),
  calfCrop_repHeifers_Percent = repHeifers/lag(calfCrop) * 100)

summary(calfCrop_replacementHeifers$calfCrop_repHeifers_Percent)

#### Here I am getting an approximate percentage of the progeny of the total stock that are added into 
#### breeding stock as replacement heifers. Note that here I am taking replacement heifers two periods ahead
#### Because if the cow gives birth this period the heifers are added into the breeding stock two periods from now
summary(Stock %>% select(Year, K, k3) %>% mutate(ratios = lead(k3,2)/(g*K)) %>% select(ratios))

# ratios      
# Min.   :0.1457  
# 1st Qu.:0.1757  
# Median :0.1975  
# Mean   :0.2328 
# 3rd Qu.:0.2903 
# Max.   :0.3727  
# NA's   :2 

#### From the above on average approximately 23% of the heifers are added into the breeding stock 

#### Here I read corn price data. These are in $/bushel. 
#### I am converting the price from $/bushel to $/pound
corn_price <- read_excel("./Data/Latest-04-2023/CornPriceReceived.xlsx") %>% as.data.frame()
names(corn_price)
corn_price <- corn_price %>% select(Year, Period, Value)
pcorn <- corn_price %>% group_by(Year) %>% mutate(pcorn = round(mean(Value),3)) %>% 
  select(Year,pcorn) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcorn <- pcorn %>% mutate(pcornLb = pcorn/56)

allPrices <- merge(pcorn, prices_costs) 

meat_bill <- supp_diss %>% 
  mutate(TS = TotalSupply, TD = TotalDiss, sl = fedSlaughter_BillLb, cl = cowsCulled_BillLb) %>%
  select(Year, sl, cl, TS, TD)

prices_quant <- merge(allPrices, meat_bill) %>% round(5)



# In order to construct the shocks I need to estimate the quantities and see the observed ones. 

### But first I will construct the nodes for corn price, fed cattle supply, and cull cow supply.

#### I need to construct production shock. supp_sl and supp_cl are the estimated fed cattle and cull cow supply. 
#### I need to get the observed supply as well. Isn't it the animals slaughtered? 
#### If yes, then we are assuming that the supply equals demand.
#### For now that is what I am doing i.e., assuming that the animal slaughtered as the observed supply 
#### and the constructed supply is the estimated supply. 
#### (After talking with Lee and Chad and by their advice, I have decided the slaughtered data as the observed supply)

################### Here I am retracing the steps by writing the code again

# State variables: corn price, demand shock, cull cows production with Gaussian shocks, and fed cattle production 
# with Gaussian shock.
## NOTE: the demand shock is also gaussian.

#### Here I am constructing the sl and cl quantities that includes shock (which is a gaussian random variable). 
#### Note: these gaussian shocks has mean one and standard deviation according to the historical shocks 
#### (see above for details)
cowsSlaughtered_obs <- cowsSlaughtered %>% transmute(Year = Year, cullMeat = cull_meat/1000000000)
heifersSlaughtered_obs <- heifersSlaughtered %>% transmute(Year = Year, heiferMeat = heifer_meat/1000000000)
steersSlaughtered_obs <- steersSlaughtered %>% transmute(Year = Year, steerMeat = steer_meat/1000000000)

fedCattleSupply_obs <- merge(heifersSlaughtered_obs, steersSlaughtered_obs) %>% 
  transmute(Year = Year, sl_obs = heiferMeat + steerMeat)

cullCowSupply_obs <- cowsSlaughtered_obs %>% transmute(Year = Year, cl_obs = cullMeat)

#### Here we get the production shocks
obsEst_sl_Supply <- merge(fedCattleSupply_obs, supp_sl) %>% 
  transmute(Year = Year, sl_obs = sl_obs, sl_est = fedSlaughter_BillLb, slShock = sl_obs/sl_est)

slSupplyShockGaussian <- merge(prices_quant, obsEst_sl_Supply) %>% transmute(Year = Year, slShock = 0)

set.seed(3)
slSupply_Shock <- rnorm(n = nrow(slSupplyShockGaussian), mean = 1, sd = std(obsEst_sl_Supply$slShock))
slSupplyShockGaussian$slShock <- slSupply_Shock

#### Here we construct the cull cows production shock
obsEst_cl_Supply <- merge(cullCowSupply_obs, supp_cl) %>% 
  transmute(Year = Year, cl_obs = cl_obs, cl_est = cowsCulled_BillLb,
            clShock = cl_obs/cl_est)

clSupplyShockgaussian <- merge(prices_quant, obsEst_cl_Supply) %>% transmute(Year = Year, clShock = 0)

set.seed(4)
clSupply_Shock <- rnorm(n = nrow(clSupplyShockgaussian), mean = 1, sd = std(obsEst_cl_Supply$clShock))
clSupplyShockgaussian$clShock <- clSupply_Shock

##### I will generate the demand shocks. For this I need the observed demand and the constructed demand. 
##### The observed demand is from derived demand from data. 
##### Constructed demand would be animals slaughtered for consumption.

### Observed derived demand would be the sum of Exports and Domestic Consumption from demandBeef dataframe
### Constructed demand would be the total animals slaughtered for consumption purposes. 
### I get this from slaughtered data. The dataframe
### totalDisappeared has the data.

estD <- merge(supp_sl, supp_cl) %>% 
  transmute(Year = Year, demandEst = fedSlaughter_BillLb + cowsCulled_BillLb) %>% na.omit()

obsDemand <- supp_diss %>% transmute(Year = Year, demandObs = TotalDiss)
demandShock <- merge(obsDemand, estD) %>% mutate(dShock = demandObs/demandEst)

demandShockGaussian <- merge(prices_quant, demandShock) %>% transmute(Year = Year, Shock = 0)

## Now i generate Gaussian shock which is consistent with historical data.
## I use the standard deviation of historical data to construct the Gaussian random variables. Here the mean is 1
set.seed(1)
demandShockG <- rnorm(n = nrow(demandShockGaussian), mean = 1, sd = std(demandShock$dShock))
demandShockGaussian$Shock <- demandShockG

#### I am merging all the supply and demand shocks
allShocks <- merge(merge(demandShockGaussian, slSupplyShockGaussian), clSupplyShockgaussian)

sl_stock <- supp_sl %>% transmute(Year = Year, sl_est = fedSlaughter_BillLb, slHead = fedSlaughter)
cl_stock <- supp_cl %>% transmute(Year = Year, cl_est = cowsCulled_BillLb, clHead = cowsCulled)

dataList <- list(sl_stock, cl_stock, slSupplyShockGaussian, 
                 clSupplyShockgaussian, dressedWeights_sl_cl, calf_crop, stocksImportsExports)

allStockShocks <- Reduce(function(...) merge(...), dataList) %>% as.data.frame()


#### Here I am constructing the supply of fed cattle ahead. Note that this is approximation. 
#### When I compare these numbers with the observed ones, these are a bit High. This comes from:
#### 1. We incorporated a Gaussian shock, 2. The storage approximation comes into play as well.
#### 0.37 comes from the fact that approximately a maximum of 37% of the progeny is added to the breeding stock
newSL_1 <- allStockShocks %>% 
  transmute(Year = Year+1, slt = ((g - 0.37 * g) * lag(K,2) * lag(slShock,1) + 
              (1 - 0.37 * g) * g * delta * (lag(K,2) - (g - 0.37 * g) * lag(K,3) - 
                                              (lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2)))) + Imports - Exports,
            slLbs = slt * Slaughter_avg/1000000000)


# merge(allStockShocks, newSL_1) %>% select(Year, sl_est, slLbs) %>% mutate(diffSL = slLbs - sl_est)

# newSL_1 <- allStockShocks %>% 
#   transmute(Year = Year+1, slt = ((g - 0.37 * g) * lag(K,2) * lag(slShock,1) + 
#                                     (1 - 0.37 * g) * g * delta * (lag(K,2) - (g - 0.37 * g) * lag(K,3) - 
#                                                                     (lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2)))) + 
#               Imports - Exports,
#             slLbs = slt * Slaughter_avg/1000000000)

# newSL_3 <- allStockShocks %>%
#   transmute(Year = Year+3, slt = (g - 0.37 * g) * K * slShock +
#               (1 - 0.37 * g) * g * delta * (K - (g - 0.37 * g) * lag(K) - (k9 + (1-delta) * k8 + (1-delta) * k7)),
#             slLbs = slt * Slaughter_avg/1000000000)

#### Since the production of fed cattle is computed for one period ahead, we construct the production of cull 
#### cows in the similar fashion

newCL_1 <- allStockShocks %>%
  transmute(Year = Year + 1, clt = (k9 + (1-delta) * k8 + (1-delta) * k7) * clShock +
              (delta * (k8 + k7 + k6) - (k7 + k8 + k9)),
            clLbs = clt * Cull_avg/1000000000)

# merge(allStockShocks, newCL_1) %>% select(Year, cl_est, clLbs) %>% mutate(diffCL = clLbs - cl_est)

# newCL_3 <- allStockShocks %>%
#   transmute(Year = Year + 3, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock +
#               (delta^2) * (delta * (k6 + k5 + k4) - (k5 + k6 + k7)),
#             clLbs = clt * Cull_avg/1000000000)


cornPrice <- pcorn
# cullCowsProd_3 <- newCL_3 %>% transmute(Year = Year, cullCows = clLbs)
# fedCattleProd_3 <- newSL_3 %>% transmute(Year = Year, fedCattle = slLbs)

cullCowsProd_1 <- newCL_1 %>% transmute(Year = Year, cullCows = clLbs) %>% round(5)
fedCattleProd_1 <- newSL_1 %>% transmute(Year = Year, fedCattle = slLbs) %>% round(5)


prod_CornP <- merge(merge(fedCattleProd_1, cullCowsProd_1),cornPrice) %>% drop_na() %>% round(5)

### Here I am generating the shocks again so that when we merge all the data frames we have enough data.
### Note: Since these are independent random shocks we are okay by increasing the n.
demandShockGaussian1 <- prod_CornP %>% transmute(Year = Year, Shock = 0)
slSupplyShockGaussian1 <- prod_CornP %>% transmute(Year = Year, slShock = 0)
clSupplyShockgaussian1 <- prod_CornP %>% transmute(Year = Year, clShock = 0)

set.seed(1)
demandShockG <- rnorm(n = nrow(prod_CornP), mean = 1, sd = std(demandShock$dShock))
demandShockGaussian1$Shock <- demandShockG

set.seed(3)
slSupply_Shock <- rnorm(n = nrow(prod_CornP), mean = 1, sd = std(obsEst_sl_Supply$slShock))
slSupplyShockGaussian1$slShock <- slSupply_Shock

set.seed(4)
clSupply_Shock <- rnorm(n = nrow(prod_CornP), mean = 1, sd = std(obsEst_cl_Supply$clShock))
clSupplyShockgaussian1$clShock <- clSupply_Shock

fedCattleProd <- fedCattleProd_1
cullCowsProd <-  cullCowsProd_1

#### NOTE: We constructed fed cattle supply and cull cow supply for existing years ahead which 
#### includes Gaussian shocks as well. 
#### Although we are using the data of existing years ahead, since we are using all the nodes of both fed cattle, 
#### and cull cows supply the price is right. DO NOT GET CONFUSED!

##############################################################################################
###### Functions that returns the chebychev nodes and Chebychev polynomial matrix.############
##############################################################################################

##### Function to create chebyshev polynomial matrix
chebyshevMatrix <- function(x,d,n){
  # x contains chebyshev nodes, d contains the original data, and n contains the number of polynomials
  xmin <- min(d)
  xmax <- max(d)
  z <- (2 * (x - xmin )/(xmax - xmin)) - 1
  mat <- matrix(data=0, nrow = length(z), ncol = n)
  for(i in 1:n){
    mat[,1] <- 1
    mat[,2] <- z
    if(i >=3){
      mat[,i] <- 2 * z * mat[,i-1] - mat[,i-2]
    }
  }
  return(mat)
}

#### Function that creates chebyshev node vector
chebyshevNodes <- function(d, n){
  # d contains the data and n contains the number of chebyshev nodes to be created
  xmin <- min(d)
  xmax <- max(d)
  x <- NA
  for (i in 1:n){
    x[i] <- ((xmin + xmax)/2) + ((xmax - xmin)/2) * cos( ((n-i+0.5)/n) * pi )
  }
  return(x)
}

#### For simplicity I use n = 5. 
chebNodesN <- 5

stateVarDemand <- supp_diss %>% transmute(Year = Year, demand = TotalDiss)

stateVariablesList <- list(cornPrice, cullCowsProd, fedCattleProd, demandShockGaussian1,
                           slSupplyShockGaussian1, clSupplyShockgaussian1, stateVarDemand)

stateVars <- Reduce(function(...) merge(...), stateVariablesList) %>% drop_na() %>% round(5)


cornNodes <- chebyshevNodes(d = stateVars$pcorn, n = chebNodesN)
cullCowNodes <- chebyshevNodes(d = stateVars$cullCows, n = chebNodesN)
fedCattleNodes <- chebyshevNodes(d = stateVars$fedCattle, n = chebNodesN)
demandNodes <- chebyshevNodes(d = stateVars$demand, n = chebNodesN)
dShockNodes <- chebyshevNodes(d = stateVars$Shock, n = chebNodesN)
slShockNodes <- chebyshevNodes(d = stateVars$slShock, n = chebNodesN)
clShockNodes <- chebyshevNodes(d = stateVars$clShock, n = chebNodesN)

corn_nodes <- cornNodes %>% as.data.frame()
cull_nodes <- cullCowNodes %>% as.data.frame()
fed_nodes <- fedCattleNodes  %>% as.data.frame()
d_nodes <- demandNodes %>% as.data.frame()
dshock_nodes <- dShockNodes %>% as.data.frame()
slShock_nodes <- slShockNodes %>% as.data.frame()
clShock_nodes <- clShockNodes %>% as.data.frame()

names(corn_nodes) <- "cornNodes"
names(cull_nodes) <- "cullNodes"
names(fed_nodes) <- "fedNodes"
names(d_nodes) <- "demandNodes"
names(dshock_nodes) <- "dShockNodes"
names(slShock_nodes) <- "slShockNodes"
names(clShock_nodes) <- "clShockNodes"

##### Cartesian product of the nodes
cull_cartesian <- crossing(corn_nodes, cull_nodes,dshock_nodes) %>% as.data.frame()
fed_cartesian <- crossing(corn_nodes, fed_nodes,dshock_nodes) %>% as.data.frame()

#### The following created chebyshev matrix containing chebyshev polynomials
cornChebyshevMatrix <- chebyshevMatrix(x = cornNodes, d = stateVars$pcorn, n = chebNodesN)
cullCowsChebyshevMatrix <- chebyshevMatrix(x = cullCowNodes, d = stateVars$cullCows, n = chebNodesN)
fedCattleChebyshevMatrix <- chebyshevMatrix(x = fedCattleNodes, d = stateVars$fedCattle, n = chebNodesN)
dShockChebyshevMatrix <- chebyshevMatrix(x = dShockNodes, d = stateVars$Shock, n = chebNodesN)
demandChebyshevMatrix <- chebyshevMatrix(x = demandNodes, d = stateVars$demand, n = chebNodesN)

###### Here I am taking the tensor product to create interpolation matrix of grids. 
###### kron takes the kronecker tensor product of two matrices
##### For cull cows we use corn, cull cows production, and demand shock chebyshev matrices
##### For fed cattle we use corn, fed cattle production, and demand shock chebyshev matrices

cullInterpolationMatrix <-  kron(kron(cornChebyshevMatrix, cullCowsChebyshevMatrix), dShockChebyshevMatrix)
fedCattleInterpolationMatrix <-  kron(kron(cornChebyshevMatrix, fedCattleChebyshevMatrix), dShockChebyshevMatrix)

#### Now I have to write code to generate price series using the interpolation matrix, coefficient vector, 
#### and actual prices. This needs a lot of work. 

##### First I write the functions that I use to get the parameters, prices, and stocks. These functions are based off 
##### of the model solution. For more information regarding the system of equations please refer the dissertation
##### document

########### We use the following loss function to estimate mu_tilde and s_tilde
lossfn <- function(theta,e,ps,pc){
  mu <- theta[1]
  s <- theta[2]
  
  v <- sum((e - ((( mu - ((ps-pc)/phi)))/s)))^2
  
  return(v)
}

#### optParamFunction returns the parameters mu_tilde and s_tilde
optParamFunction <- function(sl, cl, ps, pc, thetas){
  
  s <- sl
  c <- cl
  
  sl_share <- s/(s+c)
  cl_share <- 1-sl_share
  
  tilde <- log((1-cl_share)/cl_share)
  
  theta0 <- thetas
  
  out <- BBoptim(par= theta0, fn = lossfn, e=tilde ,ps=ps, pc=pc)
  
  muTilde <- out$par[1]
  sTilde <- out$par[2]
  
  return(c(muTilde,sTilde))
  
}





##### Price function. This contains the solution system
optPriceFunction<- function(p, sl, cl, A, B, hc_discounted){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  ### Equilibrium condition of the fed cattle production
  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  
  ### equilibrium condition of the cull cow production
  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  ### These are the price conditions
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  
  #### above four equations muct be solved simultanously to get the price and expected price.
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  return(F)
  
}


###### optKFunction returns the optimal k_{3,t+1} and sum of k_{j,t+1} where j E [7,8,9]
optKFunction <- function(K, ps, pc, A, B){
  
  K1 <- K[1]
  K2 <- K[2]
  
  fed <- g * Stock_1t - K1 - A * 
    ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  
  cull <- k_9t + k_8t + k_7t - K2 - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F = fed^2 + cull^2
  
  return(F)
  
}


##### Here setting up the data frame for the quantities. Note: It contains K_{t-1} and K_{j,t} for j = {10,9,8,7}

### The following dataframe contains the cows of age 7, 8, 9, 10 at time t.
K_jt <- Stock %>% select(Year, k7, k8, k9, k10)

#### The followng dataframe has K_{t-1} i.e., the previous period stock
K_1t <- Stock %>% transmute(Year = Year+1, K = K)

capK <- merge(K_1t, K_jt)

#### Production from the storage model specification
sl_quant_SM <- fedCattleProd_1 %>% transmute(Year = Year, slSM = fedCattle)
cl_quant_SM <- cullCowsProd_1 %>% transmute(Year = Year, clSM = cullCows)

sl_quant <- supp_sl %>% transmute(Year = Year, sl = fedSlaughter_BillLb)
cl_quant <- supp_cl %>% transmute(Year = Year, cl = cowsCulled_BillLb)

A_quant <-  supp_diss  %>% transmute(Year = Year, A = TotalDiss)

quantList <- list(A_quant, sl_quant_SM, cl_quant_SM, sl_quant, cl_quant)

quantities <-  Reduce(function(...) merge(...), quantList) %>% drop_na()

price_sl_cl_hc <- prices_quant %>% select(Year, ps, pc, hc)

imports_exports <- merge(imports, exports)

adjFactor <- supp_diss %>% select(Year,AdjFactor)

variablesList <- list(price_sl_cl_hc, capK, dressedWeights_sl_cl, imports_exports, quantities, adjFactor)

quantities_prices_capK <- Reduce(function(...) merge(...), variablesList) %>% drop_na() %>% round(6)

################################### IMPORTANT ##################################
####### We have the constructed quantities and shocks from 1982 to 2020 ########
####### So the prices we use are from 1982 to 2020 ############################
################################################################################

quantities_prices_capK <- quantities_prices_capK %>% filter(Year >= 1990)

prices_ps <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_pc <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_hc <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
expected_PS <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
expected_PC <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

mu_Tildes <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
s_Tildes <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

A_nodes <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
slNodes <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
clNodes <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

##### The following saves the equilibrium prices and quantities
prices_ps_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_pc_eq <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_hc_eq <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

expected_PS_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
expected_PC_eq <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

mu_Tildes_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
s_Tildes_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

A_nodes_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
slNodes_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
clNodes_eq <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

##### The following saves the iterations after the equilibrium 
prices_ps_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_pc_itr <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
prices_hc_itr <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

expected_PS_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
expected_PC_itr <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

mu_Tildes_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
s_Tildes_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

A_nodes_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
slNodes_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
clNodes_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

D_slPsPc_itr <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
D_clPsPc_itr <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

#### The following saves overall prices and quantities
D_slPsPc <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
D_clPsPc <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

D_PsPc <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
S_psPC <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

fedDiff <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
cullDiff <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

mu_Tildes_Prior <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
s_Tildes_Prior <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))

k3t1 <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
kjt1 <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

slNew <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
clNew <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

slD <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
clD <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

c_cull1 <- matrix(data=0, nrow = nrow(cullInterpolationMatrix), ncol = 1)
c_fed1 <- matrix(data=0, nrow = nrow(fedCattleInterpolationMatrix), ncol = 1)

c_cull_opt <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=nrow(cullInterpolationMatrix), ncol=1)
c_fed_opt <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=nrow(fedCattleInterpolationMatrix), ncol=1)

maxIter <- 750

fedPrice <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)
cullPrice <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)
fedProd <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)
cullProd <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)

c_cull_itr <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)
c_fed_itr <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)

checkTol <- matrix(data = 0, nrow = maxIter, ncol = 4)

############################### THE ITERATION ALGORITHM ########################

for(i in 1:nrow(quantities_prices_capK)){
  
  # i <- 1
  ### Here we get the observed quantities. For fed production and cull production these are estimated production 3 years ahead
  A <- quantities_prices_capK$A[i] ## Note: Although I am assigning the total demand to variable here, I am using the
  #                                  ## fed cattle production node and cull cow production node with demand shock to get 
  #                                  ## the total demand for that particular node. 
  
  sl <- quantities_prices_capK$slSM[i]
  cl <- quantities_prices_capK$clSM[i]
  
  adjFac <- A/(sl+cl)
  
  ps <-   quantities_prices_capK$ps[i]
  pc <-   quantities_prices_capK$pc[i]
  hc <- quantities_prices_capK$hc[i]
  
  # if(i>1){
  #   ps <-   median(quantities_prices_capK$ps[1:i])
  #   pc <-   median(quantities_prices_capK$pc[1:i])
  #   hc <-   median(quantities_prices_capK$hc[1:i])
  #   A <- median(quantities_prices_capK$A[1:i])
  #   sl <- median(quantities_prices_capK$sl[1:i])
  #   cl <- median(quantities_prices_capK$cl[1:i])
  #   adjFac <- A/(sl+cl)
  # }
  
  params_mu_s <- optParamFunction(sl = sl, cl = cl, ps = ps, pc = pc, thetas = c(1,1))
  
  mu_Tilde <- params_mu_s[1]
  s_Tilde <- params_mu_s[2]
  mu_Tildes_Prior[,i] <- mu_Tilde
  s_Tildes_Prior[,i] <- s_Tilde
  
  K1t  <- quantities_prices_capK$K[i]
  k9 <- quantities_prices_capK$k9[i]
  k8 <- quantities_prices_capK$k8[i]
  k7 <- quantities_prices_capK$k7[i]
  
  slDressed <- quantities_prices_capK$Slaughter_avg[i]
  clDressed <- quantities_prices_capK$Cull_avg[i]
  
  # adjFac <- quantities_prices_capK$AdjFactor[i]
  
  #### For imports and exports I will take the mean/median of the past data.
  # importsObs <- median(quantities_prices_capK$Imports[1:i])
  # exportsObs <- median(quantities_prices_capK$Exports[1:i])
  
  importsObs <- quantities_prices_capK$Imports[i]
  exportsObs <- quantities_prices_capK$Exports[i]
  
  hc_new <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1)))
  hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
  
  Stock_1t <- (K1t*slDressed)/1000000000
  imports <- (importsObs*slDressed)/1000000000
  exports <- (exportsObs*slDressed)/1000000000
  k_9t <- (k9*clDressed)/1000000000
  k_8t <- (k8*clDressed)/1000000000
  k_7t <- (k7*clDressed)/1000000000
  
  ps_new <- as.matrix(rep(ps,nrow(fed_cartesian)), ncol = 1)
  pc_new <- as.matrix(rep(pc,nrow(cull_cartesian)), ncol = 1)
  
  c_cull <- solve(cullInterpolationMatrix) %*% pc_new
  c_fed <- solve(fedCattleInterpolationMatrix) %*% ps_new
  
  ps_m <- mean(ps_new[,1])
  pc_m <- mean(pc_new[,1])
  
  count <- 0
  c_old_cull <- matrix(data = 0, nrow = nrow(cullInterpolationMatrix), ncol = 1)
  c_old_fed <- matrix(data = 0, nrow = nrow(fedCattleInterpolationMatrix), ncol = 1)
  
  ps_old <- 0
  pc_old <- 0
  
  count <- 0
  innerCount <- 0
  
  eqIter <- 100
  eqCols <- 7
  
  equilibriumCheck <- lapply(1:nrow(cull_cartesian), matrix, data= 0, nrow = eqIter, ncol = eqCols)
  
  checkTol <- matrix(data = 0, nrow = maxIter, ncol = 4)
  
  for(k in 1:maxIter){
    
    # k <- 2
    
    # if( norm(x = (c_cull - c_old_cull), type = "f") < 0.01 && norm(x = (c_fed - c_old_fed) , type = "f") < 0.01){
    #   if( (ps_m - ps_old)^2 < 0.001 && (pc_m - pc_old)^2 < 0.001){
    #     break
    #   }
    # }
    
    if( k > 1 ){
      if( round(checkTol[k-1,1],3) < 0.01 && round(checkTol[k-1,2],3) < 0.01){
        if( round(checkTol[k-1,3],3) < 0.01 && round(checkTol[k-1,4],3) < 0.01){
          break
        }
      }
    }
    
    
    # if( k > 50 ){
    #   break
    # }
    
    count <- count + 1
    
    c_old_cull <- c_cull
    c_old_fed <- c_fed
    
    ps_old <- ps_m
    pc_old <- pc_m
    
    for(j in 1:nrow(cull_cartesian)){
      
      # j <- 1
      
      if(k == 1){
        
        cullCowNode <- cull_cartesian$cullNodes[j]
        dShockNode <- cull_cartesian$dShockNodes[j]
        fedCattleNode <- fed_cartesian$fedNodes[j]
        cornNode <- fed_cartesian$cornNodes[j]
        # aNode <- fed_cartesian$demandNodes[j]
        
        # slShare_t <- (exp((mu_Tilde - ((ps_old - pc_old))/phi)/s_Tilde))
        
        # sl_node <- (fedCattleNode) * adjFac
        # cl_node <- (cullCowNode) * adjFac
        
        # A_node <- (fedCattleNode + cullCowNode) * (dShockNode)
        adjFac <- A/(fedCattleNode+ cullCowNode)
        
        sl_node <- (fedCattleNode) * adjFac
        cl_node <- (cullCowNode) * adjFac
        
        A_node <- A  * dShockNode
        
        # A_node <- (fedCattleNode + cullCowNode) * (dShockNode)
        # sl_node <- (A_node * ((slShare_t)/(1 + slShare_t))) * adjFac
        # cl_node <- (A_node * (1/(1+slShare_t))) * adjFac
        
        
        # slD <- A_node * ((slShare_t)/(1 + slShare_t))
        # clD <- A_node * (1/(1 + slShare_t))
        
        A_nodes[j,i] <- A_node
        slNodes[j,i] <- sl_node
        clNodes[j,i] <- cl_node
        
        #### getting the parameters from the optParamFunction
        params_mu_s <- optParamFunction(sl = sl_node, cl = cl_node, ps = ps_old, pc = pc_old, thetas = c(1,1))
        
        mu_Tilde <- params_mu_s[1]
        s_Tilde <- params_mu_s[2]
        
        mu_Tildes[j,i] <- mu_Tilde
        s_Tildes[j,i] <- s_Tilde
        
        if( ps_old < ps){
          ps_o <- ps
        }else{
          ps_o <- ps_old
        }
        
        if( pc_old < pc){
          pc_o <- pc
        }else{
          pc_o <- pc_old
        }
        
        # I am giving upper and lower bounds by looking at the historical prices (I look at the summary of difference between prices and the lagged price and use the min of those differences). 
        # Specifically for the lower bounds.
        # This is what my focus for setting the bounds for the prices. I don't want the program to choose some 
        # random arbitrary number which satisfies the system of equations. Note we can always find a number that
        # satisfies the system of equations. Our goal is to find the realistic solution and as realistic as possible.
        ps_lo <- ps_o  - 0.276
        pc_lo <- pc_o - 0.292
        
        ps_up <- ps_o + 0.1
        pc_up <- pc_o + 0.1
        
        #### Here we are making sure the lower bound for the prices isn't negative
        if(ps_lo < 0){
          ps_lo <- ps_o
        }
        
        if(pc_lo < 0){
          pc_lo <- pc_o
        }
        
        #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
        while( pc_lo > ps_lo ){
          pc_lo <- pc_lo - 0.01
        }
        
        ##### Gaussian quadrature for integration to get the expected price. The weights are pre determined.
        ps_expected <- sum(as.numeric(ps_o) * fedMeshCheb)
        pc_expected <- sum(as.numeric(pc_o) * cullMeshCheb)
        
        ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
        ### we use the euqlity of that to get the holding costs. From my first observation this is greater than the naive 
        ### expectations holding costs. Because we have the expected price in the equality.
        hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expected + g * (beta^3) * ps_expected - pc_o)
        # hc_new <- hc_new + cornNode/56
        #### Here we make sure that the holding costs are below the cull cow price
        while(hc_new > pc_o){
          hc_new <- hc_new - 0.01
        }
        
        hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
        B <- ps_o - g * (beta^3) * ps_expected + hc_discounted ## Comes from the model
        
        ps_expected_lo <- ps_expected - 0.5
        
        ps_expected_up <- ps_expected + 0.1
        
        pc_expected_lo <- pc_expected - 0.5
        
        pc_expected_up <- pc_expected + 0.1
        
        if(pc_expected_lo < 0){
          pc_expected_lo <- pc_expected
        }
        
        if(ps_expected_lo < 0){
          ps_expected_lo <- ps_expected
        }
        
        p <- c(ps_o, pc_o, ps_expected, pc_expected)
        
        lo <- c(ps_lo, pc_lo, ps_expected_lo, pc_expected_lo)
        up <- c(ps_up, pc_up, ps_expected_up, pc_expected_up)
        
        estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node, 
                        B = B, hc_discounted = hc_discounted, lower = lo, upper = up)
        
        ps1 <- estP$par[1]
        pc1 <- estP$par[2]
        ps_expected1 <- estP$par[3]
        pc_expected1 <- estP$par[4]
        
        prices_ps[j,i] <- ps1
        prices_pc[j,i] <- pc1
        expected_PS[j,i] <- ps_expected1
        expected_PC[j,i] <- pc_expected1
        prices_hc[j,i] <- hc_new
        
      }
      
      if( k == 2 ){
        
        m <- 1
        
        cullCowNode <- cull_cartesian$cullNodes[j]
        dShockNode <- cull_cartesian$dShockNodes[j]
        fedCattleNode <- fed_cartesian$fedNodes[j]
        cornNode <- fed_cartesian$cornNodes[j]
        aNode <- fed_cartesian$demandNodes[j]
        
        sl_node <- slNodes[j,i]
        cl_node <- clNodes[j,i]
        # A_node <- A_nodes[j,i]
        # A_node <- (sl_node + cl_node) * dShockNode
        A_node <- A_nodes[j,i]
        
        # A_nodes[j,i] <- A_node
        
        while(abs(fedDiff[j,i])>0.001 || abs(cullDiff[j,i])>0.001){
          
          if( fedDiff[j,i] < 0){
            ps_n <- prices_ps[j,i] + 0.001
          } else if( fedDiff[j,i] > 0){
            ps_n <- prices_ps[j,i] - 0.001
          }
          
          if(ps_n < 0){
            ps_n <- prices_ps[j,i]
          }
          
          if( cullDiff[j,i] < 0){
            pc_n <- prices_pc[j,i] + 0.001
          } else if( cullDiff[j,i] > 0){
            pc_n <- prices_pc[j,i] - 0.001
          }
          
          if(pc_n < 0){
            pc_n <- prices_pc[j,i]
          }
          
          ps_lo <- ps_n  - 0.276
          pc_lo <- pc_n - 0.292
          
          ps_up <- ps_n + 0.1
          pc_up <- pc_n + 0.1
          
          if(ps_lo < 0){
            ps_lo <- ps_n
          }
          
          if(pc_lo < 0){
            pc_lo <- pc_n
          }
          
          while(pc_lo>ps_lo){
            pc_lo <- pc_lo - 0.01
          }
          
          ps_expected <- expected_PS[j,i]
          pc_expected <- expected_PC[j,i]
          
          # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
          # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
          
          ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
          ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
          ### expectations holding costs. Because we have the expected price in the equality.
          hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expected + g * (beta^3) * ps_expected - pc_n)
          # hc_new <- hc_new + (cornNode/56)
          
          while(hc_new>pc_n){
            hc_new <- hc_new - 0.01
          }
          
          hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
          B <- ps_n - g * (beta^3) * ps_expected + hc_discounted
          
          ps_expected_lo <- ps_expected - 0.5
          
          ps_expected_up <- ps_expected + 0.1
          
          pc_expected_lo <- pc_expected - 0.5
          
          pc_expected_up <- pc_expected + 0.1
          
          if(pc_expected_lo < 0){
            pc_expected_lo <- pc_expected
          }
          
          if(ps_expected_lo < 0){
            ps_expected_lo <- ps_expected
          }
          
          p <- c(ps_n, pc_n, ps_expected, pc_expected)
          
          lo <- c(ps_lo, pc_lo, ps_expected_lo, pc_expected_lo)
          up <- c(ps_up, pc_up, ps_expected_up, pc_expected_up)
          
          params_mu_s <- optParamFunction(sl = sl_node, cl = cl_node, 
                                          ps = ps_n, pc = pc_n, thetas = c(1,1))
          
          mu_Tilde <- params_mu_s[1]
          s_Tilde <- params_mu_s[2]
          
          mu_Tildes_eq[j,i] <- mu_Tilde
          s_Tildes_eq[j,i] <- s_Tilde
          
          estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node, B = B, 
                          hc_discounted = hc_discounted, lower = lo, upper = up)
          
          ps1 <- estP$par[1]
          pc1 <- estP$par[2]
          ps_expected1 <- estP$par[3]
          pc_expected1 <- estP$par[4]
          
          prices_ps[j,i] <- ps1
          prices_pc[j,i] <- pc1
          expected_PS[j,i] <- ps_expected1
          expected_PC[j,i] <- pc_expected1
          prices_hc[j,i] <- hc_new
          
          prices_ps_eq[j,i] <- ps1
          prices_pc_eq[j,i] <- pc1
          expected_PS_eq[j,i] <- ps_expected1
          expected_PC_eq[j,i] <- pc_expected1
          prices_hc_eq[j,i] <- hc_new
          
          ### Demand of the fed cattle meat under the new prices
          D_slPsPc[j,i] <- A_node *
            ((exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))/
               (1 + (exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))))
          
          ### Demand of the cull cow meat under the new prices
          D_clPsPc[j,i] <- A_node * (1/(1+ exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde)))
          
          ### Here we get the "optimal" supply of the meat by solving for k_{3,t+1} and k_{j,t+1} where j = [7,8,9]. Note we get
          ### these quantities seperately i.e., k_{3,t+1} and sum k_{j,t+1} where j = [7,8,9]
          K <- c(0,0)
          
          #### Here we use sl+cl for A_node. Why? because we use the current quantities i.e., Stock_1t and k_9t + k_8t + k_7t
          #### That means the total derived demand should be sl+cl? Dunno Have to think more.....
          estK <- BBoptim(par = K, fn = optKFunction, ps = ps1, pc = pc1, A = A_node)
          
          k_3t1 <- estK$par[1]
          k_7_10t1 <- estK$par[2]
          
          sl1 <- (g * Stock_1t - k_3t1) 
          cl1 <- (k_9t + k_8t + k_7t - k_7_10t1)
          
          slNodes[j,i] <- sl1
          clNodes[j,i] <- cl1
          
          A_nodes[j,i] <- A_node
          
          #### Total demand for the meat under new prices
          D_PsPc <- as.matrix(D_slPsPc[j,i] + D_clPsPc[j,i])
          
          #### Total supply of meat (this is by adding the results)
          S_psPC <- as.matrix(sl1 + cl1)
          
          fedDiff[j,i] <- sl_node - D_slPsPc[j,i]
          cullDiff[j,i] <- cl_node - D_clPsPc[j,i]
          
          equilibriumCheck[[j]][m,1] <-  fedDiff[j,i]
          equilibriumCheck[[j]][m,2] <-  cullDiff[j,i] 
          equilibriumCheck[[j]][m,3] <-  fedDiff[j,i] + cullDiff[j,i] 
          equilibriumCheck[[j]][m,4] <- prices_ps[j,i]
          equilibriumCheck[[j]][m,5] <- prices_pc[j,i]
          equilibriumCheck[[j]][m,6] <- expected_PS[j,i]
          equilibriumCheck[[j]][m,7] <- expected_PC[j,i]
          
          slNodes_eq[j,i] <- sl1 
          clNodes_eq[j,i] <- cl1
          A_nodes_eq[j,i] <- A_node
          
          ### Here we use the share of the cattle meat under new price as the supply of the corresponding meat in the next iteration
          if((m %% 2 == 0)){
            A_node <- (sl1 + cl1) * dShockNode
          }else{
            sl_node <- sl1
            cl_node <- cl1 
          }
          
          m <- m+1
          
        }
        
        # if(abs(fedDiff[j,i])<0.001 && abs(cullDiff[j,i])<0.001){
        # slNodes_eq[j,i] <- slNodes_eq[j,i] * adjFac
        # clNodes_eq[j,i] <- clNodes_eq[j,i] * adjFac
        # A_nodes_eq[j,i] <- A_nodes_eq[j,i] * (1/dShockNode)
        # }
        
      }
      
      if( k > 2){
        
        # cullCowNode <- cull_cartesian$cullNodes[j]
        # dShockNode <- cull_cartesian$dShockNodes[j]
        # fedCattleNode <- fed_cartesian$fedNodes[j]
        # 
        # sl_node <- fedCattleNode
        # cl_node <- cullCowNode
        # A_node <- (sl_node + cl_node) * dShockNode
        
        ps_n <- prices_ps[j,i]
        pc_n <- prices_pc[j,i]
        
        ps_lo <- ps_n  - 0.276
        pc_lo <- pc_n - 0.292
        
        ps_up <- ps_n + 0.1
        pc_up <- pc_n + 0.1
        
        if(ps_lo < 0){
          ps_lo <- ps_n
        }
        
        if(pc_lo < 0){
          pc_lo <- pc_n
        }
        
        while(pc_lo>ps_lo){
          pc_lo <- pc_lo - 0.01
        }
        
        ps_expected <- expected_PS[j,i]
        pc_expected <- expected_PC[j,i]
        
        # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
        # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
        
        ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
        ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
        ### expectations holding costs. Because we have the expected price in the equality.
        hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expected + g * (beta^3) * ps_expected - pc_n)
        # hc_new <- hc_new + (cornNode/56)
        
        while( hc_new > pc_n ){
          hc_new <- hc_new - 0.01
        }
        
        hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
        B <- ps_n - g * (beta^3) * ps_expected + hc_discounted
        
        ps_expected_lo <- ps_expected - 0.5
        
        ps_expected_up <- ps_expected + 0.1
        
        pc_expected_lo <- pc_expected - 0.5
        
        pc_expected_up <- pc_expected + 0.1
        
        if(pc_expected_lo < 0){
          pc_expected_lo <- pc_expected
        }
        
        if(ps_expected_lo < 0){
          ps_expected_lo <- ps_expected
        }
        
        
        p <- c(ps_n, pc_n, ps_expected, pc_expected)
        
        lo <- c(ps_lo, pc_lo, ps_expected_lo, pc_expected_lo)
        up <- c(ps_up, pc_up, ps_expected_up, pc_expected_up)
        
        dShockNode <- cull_cartesian$dShockNodes[j]
        
        sl_node <- slNodes[j,i]
        cl_node <- clNodes[j,i]
        A_node <- A_nodes[j,i]  * dShockNode
        
        params_mu_s <- optParamFunction(sl = sl_node, cl = cl_node, 
                                        ps = ps_n, pc = pc_n, thetas = c(1,1))
        
        mu_Tilde <- params_mu_s[1]
        s_Tilde <- params_mu_s[2]
        
        mu_Tildes_itr[j,i] <- mu_Tilde
        s_Tildes_itr[j,i] <- s_Tilde
        
        estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node, B = B, 
                        hc_discounted = hc_discounted, lower = lo, upper = up)
        
        ps1 <- estP$par[1]
        pc1 <- estP$par[2]
        ps_expected1 <- estP$par[3]
        pc_expected1 <- estP$par[4]
        
        prices_ps[j,i] <- ps1
        prices_pc[j,i] <- pc1
        expected_PS[j,i] <- ps_expected1
        expected_PC[j,i] <- pc_expected1
        prices_hc[j,i] <- hc_new
        
        prices_ps_itr[j,i] <- ps1
        prices_pc_itr[j,i] <- pc1
        expected_PS_itr[j,i] <- ps_expected1
        expected_PC_itr[j,i] <- pc_expected1
        prices_hc_itr[j,i] <- hc_new
        
        ### Demand of the fed cattle meat under the new prices
        D_slPsPc_itr[j,i] <- A_node *
          ((exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))/
             (1 + (exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))))
        
        ### Demand of the cull cow meat under the new prices
        D_clPsPc_itr[j,i] <- A_node * (1/(1+ exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde)))
        
        slNodes_itr[j,i] <- D_slPsPc_itr[j,i]
        clNodes_itr[j,i] <- D_clPsPc_itr[j,i]
        
      }
      
      fedPrice[[i]][j,k] <- ps1
      cullPrice[[i]][j,k] <- pc1
      
      fedProd[[i]][j,k] <- slNodes[j,i]
      cullProd[[i]][j,k] <- clNodes[j,i]
      
    }
    
    if(k==1){
      ### Demand of the fed cattle meat under the new prices
      D_slPsPc[,i] <- A_nodes[,i] *
        ((exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i]))/
           (1 + (exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i]))))
      
      ### Demand of the cull cow meat under the new prices
      D_clPsPc[,i] <- A_nodes[,i] * (1/(1+ exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i])))
      
      #### Total demand for the meat under new prices
      D_PsPc <- as.matrix(D_slPsPc[,i] + D_clPsPc[,i])
      
      #### Total supply of meat (this is by adding the nodes)
      S_psPC <- as.matrix(slNodes[,i] + clNodes[,i])
      
      fedDiff[,i] <- slNodes[,i] - D_slPsPc[,i]
      cullDiff[,i] <- clNodes[,i] - D_clPsPc[,i]
      
      # slNodes[,i] <- D_slPsPc[,i]
      # clNodes[,i] <- D_clPsPc[,i]
      
      sdiff <- fedDiff[,i]
      cdiff <- cullDiff[,i]
    }
    
    c_fed  <- solve(fedCattleInterpolationMatrix) %*% prices_ps[,i]
    c_cull <- solve(cullInterpolationMatrix) %*% prices_pc[,i]
    
    ps_m <- mean(prices_ps[,i])
    pc_m <- mean(prices_pc[,i])
    
    ps_exp_m <- mean(expected_PS[,i])
    pc_exp_m <- mean(expected_PC[,i])
    
    fedDiff_m <- mean(fedDiff[,i])
    cullDiff_m <- mean(cullDiff[,i])
    
    c_cull_itr[[i]][,k] <- c_cull
    c_fed_itr[[i]][,k] <- c_fed
    
    
    cat("\n norm of old and new fed coefficients: ", norm(x = (c_fed - c_old_fed) , type = "f"))
    
    cat("\n norm of old and new cull coefficients: ", norm(x = (c_cull - c_old_cull) , type = "f"))
    
    cat("\n Squared difference between old and new mean fed cattle prices: ", (ps_m - ps_old)^2)
    
    cat("\n Squared difference between old and new mean cull cattle prices: ", (pc_m - pc_old)^2)
    
    checkTol[k,1] <- norm(x = (c_fed - c_old_fed) , type = "f")
    checkTol[k,2] <- norm(x = (c_cull - c_old_cull) , type = "f")
    checkTol[k,3] <- (ps_m - ps_old)^2
    checkTol[k,4] <- (pc_m - pc_old)^2
    
    if( k > 3 ){
      if( all(round(checkTol[k-1,],1) == round(checkTol[k,],1)) ){
        if( all(round(checkTol[k-2,],1) == round(checkTol[k-1,],1)) ){
          if( all(round(checkTol[k-3,],1) == round(checkTol[k-2,],1)) ){
            break
          }
        }
      }
      
    }
    
  }
  
}


######## Gathering the summaries from the above iterative algorithm


# Estimated Equilibrium Parameters

# mu tildes
mu_Tildes_MeansNII <- apply(unique(mu_Tildes_eq[1:25,]), 2, mean)
mu_Tildes_MeansNII <- mu_Tildes_MeansNII %>% as.data.frame()
names(mu_Tildes_MeansNII) <- "muMean"
mu_Tildes_MeansNII <- mu_Tildes_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

mu_Tildes_MediansNII <- apply(unique(mu_Tildes_eq[1:25,]), 2, median)
mu_Tildes_MediansNII <- mu_Tildes_MediansNII %>% as.data.frame()
names(mu_Tildes_MediansNII) <- "muMedian"
mu_Tildes_MediansNII <- mu_Tildes_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

mu_Tildes_MMNII <- merge(mu_Tildes_MeansNII, mu_Tildes_MediansNII)

# s tildes
s_Tildes_MeansNII <- apply(unique(s_Tildes_eq[1:25,]), 2, mean)
s_Tildes_MeansNII <- s_Tildes_MeansNII %>% as.data.frame()
names(s_Tildes_MeansNII) <- "sMean"
s_Tildes_MeansNII <- s_Tildes_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

s_Tildes_MediansNII <- apply(unique(s_Tildes_eq[1:25,]), 2, median)
s_Tildes_MediansNII <- s_Tildes_MediansNII %>% as.data.frame()
names(s_Tildes_MediansNII) <- "sMedian"
s_Tildes_MediansNII <- s_Tildes_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

s_Tildes_MMNII <- merge(s_Tildes_MeansNII, s_Tildes_MediansNII)


merge(mu_Tildes_MMNII,s_Tildes_MMNII) %>% select(Year, muMedian, sMedian) %>% filter(Year >2009) %>% round(3)


###### Fitted Fed Cattle Equilibrium prices

EQprices_ps_MeansNII <- apply(unique(prices_ps_eq[1:25,]), 2, mean)
EQprices_ps_MeansNII <- EQprices_ps_MeansNII %>% as.data.frame()
names(EQprices_ps_MeansNII) <- "psMean"
EQprices_ps_MeansNII <- EQprices_ps_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_ps_MediansNII <- apply(unique(prices_ps_eq[1:25,]), 2, median)
EQprices_ps_MediansNII <- EQprices_ps_MediansNII %>% as.data.frame()
names(EQprices_ps_MediansNII) <- "psMedian"
EQprices_ps_MediansNII <- EQprices_ps_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestPSNII <- merge(EQprices_ps_MeansNII, EQprices_ps_MediansNII)

EQestObsPSNII <- left_join(EQestPSNII,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

EQestObsPSNII

EQestObsPSNII_Err <- EQestObsPSNII %>% select(Year, psMedian, ps) %>% 
  mutate(eHat = ((ps-psMedian)/ps)* 100)

EQestObsPSNII_Err_Median <- median(EQestObsPSNII_Err$eHat) %>% round(2)
EQestObsPSNII_Err_Max <- max(EQestObsPSNII_Err$eHat) %>% round(2)

###### Fitted Cull Cow Equilibrium prices
EQprices_pc_MeansNII <- apply(unique(prices_pc_eq[1:25,]), 2, mean)
EQprices_pc_MeansNII <- EQprices_pc_MeansNII %>% as.data.frame()
names(EQprices_pc_MeansNII) <- "pcMean"
EQprices_pc_MeansNII <- EQprices_pc_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_pc_MediansNII <- apply(unique(prices_pc_eq[1:25,]), 2, median)
EQprices_pc_MediansNII <- EQprices_pc_MediansNII %>% as.data.frame()
names(EQprices_pc_MediansNII) <- "pcMedian"
EQprices_pc_MediansNII <- EQprices_pc_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestPCNII <- merge(EQprices_pc_MeansNII, EQprices_pc_MediansNII)

EQestObsPCNII <- left_join(EQestPCNII,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

EQestObsPCNII

EQestObsPCNII_Err <- EQestObsPCNII %>% select(Year, pcMedian, pc) %>% 
  mutate(eHat = ((pc-pcMedian)/pc)*100)

EQestObsPCNII_Err_Median <- median(EQestObsPCNII_Err$eHat) %>% round(2)
EQestObsPCNII_Err_Max <- max(EQestObsPCNII_Err$eHat) %>% round(2)


EQestObsPCNII_R2 <- EQestObsPCNII_Err %>% select(Year, pcMedian, pc) %>% 
  mutate(resSquared = (pcMedian-pc)^2, totSquared = (pc - mean(pc))^2)

EQestObsPCNII_sumSquaredRes <- sum(EQestObsPCNII_R2$resSquared)
EQestObsPCNII_sumSquaredTotal <- sum(EQestObsPCNII_R2$totSquared)

EQestObsPCNII_RSquared <- 1 -  (EQestObsPCNII_sumSquaredRes/EQestObsPCNII_sumSquaredTotal)



mergedPrices <- merge(EQestObsPSNII %>% select(-errMean, -errmedian), 
      EQestObsPCNII %>% select(-errMean, -errmedian)) %>% select(Year, ps, psMedian, 
                                                                 pc, pcMedian) %>%
  filter(Year >= 2010)
mergedPrices[,-1] <- mergedPrices[,-1] * 100




###### Fitted Holding costs
EQcosts_hc_MeansNII <- apply(unique(prices_hc_eq[1:25,]), 2, mean)
EQcosts_hc_MeansNII <- EQcosts_hc_MeansNII %>% as.data.frame()
names(EQcosts_hc_MeansNII) <- "hcMean"
EQcosts_hc_MeansNII <- EQcosts_hc_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQcosts_hc_MediansNII <- apply(unique(prices_hc_eq[1:25,]), 2, median)
EQcosts_hc_MediansNII <- EQcosts_hc_MediansNII %>% as.data.frame()
names(EQcosts_hc_MediansNII) <- "hcMedian"
EQcosts_hc_MediansNII <- EQcosts_hc_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestHCNII <- merge(EQcosts_hc_MeansNII, EQcosts_hc_MediansNII)

###### Fitted Expected Prices
EQprices_Eps_MeansNII <- apply(unique(expected_PS_eq[1:25,]), 2, mean)
EQprices_Eps_MeansNII <- EQprices_Eps_MeansNII %>% as.data.frame()
names(EQprices_Eps_MeansNII) <- "EpsMean"
EQprices_Eps_MeansNII <- EQprices_Eps_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQprices_Eps_MediansNII <- apply(unique(expected_PS_eq[1:25,]), 2, median)
EQprices_Eps_MediansNII <- EQprices_Eps_MediansNII %>% as.data.frame()
names(EQprices_Eps_MediansNII) <- "EpsMedian"
EQprices_Eps_MediansNII <- EQprices_Eps_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPSNII <- merge(EQprices_Eps_MeansNII, EQprices_Eps_MediansNII)


EQprices_Epc_MeansNII <- apply(unique(expected_PC_eq[1:25,]), 2, mean)
EQprices_Epc_MeansNII <- EQprices_Epc_MeansNII %>% as.data.frame()
names(EQprices_Epc_MeansNII) <- "EpcMean"
EQprices_Epc_MeansNII <- EQprices_Epc_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQprices_Epc_MediansNII <- apply(unique(expected_PC_eq[1:25,]), 2, median)
EQprices_Epc_MediansNII <- EQprices_Epc_MediansNII %>% as.data.frame()
names(EQprices_Epc_MediansNII) <- "EpcMedian"
EQprices_Epc_MediansNII <- EQprices_Epc_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% select(Year, everything())

EQestEPCNII <- merge(EQprices_Epc_MeansNII, EQprices_Epc_MediansNII)


# Fitted Fed Cattle Equilibrium Supply
EQquantities_sl_MeansNII <- apply(unique(slNodes_eq[1:25,]), 2, mean)
EQquantities_sl_MeansNII <- EQquantities_sl_MeansNII %>% as.data.frame()
names(EQquantities_sl_MeansNII) <- "slMean"
EQquantities_sl_MeansNII <- EQquantities_sl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQquantities_sl_MediansNII <- apply(unique(slNodes_eq[1:25,]), 2, median)
EQquantities_sl_MediansNII <- EQquantities_sl_MediansNII %>% as.data.frame()
names(EQquantities_sl_MediansNII) <- "slMedian"
EQquantities_sl_MediansNII <- EQquantities_sl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestSLNII <- merge(EQquantities_sl_MeansNII, EQquantities_sl_MediansNII)

supp_sl_MODEL <- supp_diss_adj %>% select(Year, slSM = fedSlaughter_BillLb)

# supp_sl_MODEL <- obsEst_sl_Supply %>% select(Year, slSM = sl_obs)
# supp_sl_MODEL <- quantities_prices_capK %>% select(Year, slSM)

EQestObsSLNII <- left_join(EQestSLNII, supp_sl_MODEL) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)

EQestObsSLNII 


EQestObsSLNII_Err <- EQestObsSLNII %>% select(Year, slMedian, slSM) %>% 
  mutate(eHat = ((slSM-slMedian)/slSM)*100)

EQestObsSLNII_Err_Median <- median(EQestObsSLNII_Err$eHat) %>% round(2)
EQestObsSLNII_Err_Max <- max(EQestObsSLNII_Err$eHat) %>% round(2)


EQestObsSLNII_R2 <- EQestObsSLNII_Err %>% select(Year, slMedian, slSM) %>% 
  mutate(resSquared = (slMedian-slSM)^2, totSquared = (slSM - mean(slSM))^2)

EQestObsSLNII_sumSquaredRes <- sum(EQestObsSLNII_R2$resSquared)
EQestObsSLNII_sumSquaredTotal <- sum(EQestObsSLNII_R2$totSquared)

EQestObsSLNII_RSquared <- 1 -  (EQestObsSLNII_sumSquaredRes/EQestObsSLNII_sumSquaredTotal)

EQestObsSLNII_DiagResPlot <- plot(EQestObsSLNII$slMedian, EQestObsSLNII$errmedian)


# Fitted Cull Cow Equilibrium Supply 
EQquantities_cl_MeansNII <- apply(unique(clNodes_eq[1:25,]), 2, mean)
EQquantities_cl_MeansNII <- EQquantities_cl_MeansNII %>% as.data.frame()
names(EQquantities_cl_MeansNII) <- "clMean"
EQquantities_cl_MeansNII <- EQquantities_cl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQquantities_cl_MediansNII <- apply(unique(clNodes_eq[1:25,]), 2, median)
EQquantities_cl_MediansNII <- EQquantities_cl_MediansNII %>% as.data.frame()
names(EQquantities_cl_MediansNII) <- "clMedian"
EQquantities_cl_MediansNII <- EQquantities_cl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

EQestCLNII <- merge(EQquantities_cl_MeansNII, EQquantities_cl_MediansNII)

supp_cl_MODEL <- supp_diss_adj %>% select(Year, clSM = cowsCulled_BillLb)

# supp_cl_MODEL <- obsEst_cl_Supply %>% select(Year, clSM = cl_obs)
# supp_cl_MODEL <- quantities_prices_capK %>% select(Year, clSM)

EQestObsCLNII <- left_join(EQestCLNII, supp_cl_MODEL) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)
EQestObsCLNII

EQestObsCLNII_Err <- EQestObsCLNII %>% select(Year, clMedian, clSM) %>% 
  mutate(eHat = ((clSM-clMedian)/clSM)*100)

EQestObsCLNII_Err_Median <- median(EQestObsCLNII_Err$eHat) %>% round(2)
EQestObsCLNII_Err_Max <- max(EQestObsCLNII_Err$eHat) %>% round(2)


EQestObsCLNII_R2 <- EQestObsCLNII_Err %>% select(Year, clMedian, clSM) %>% filter(Year >= 1990) %>%
  mutate(resSquared = (clMedian-clSM)^2, totSquared = (clSM - mean(clSM))^2)

EQestObsCLNII_sumSquaredRes <- sum(EQestObsCLNII_R2$resSquared)
EQestObsCLNII_sumSquaredTotal <- sum(EQestObsCLNII_R2$totSquared)

EQestObsCLNII_RSquared <- 1 -  (EQestObsCLNII_sumSquaredRes/EQestObsCLNII_sumSquaredTotal)

EQestObsCLNII_DiagResPlot <- plot(EQestObsCLNII$clMedian, EQestObsCLNII$errmedian)


merge(EQestObsSLNII %>% select(-errMean, -errmedian), 
      EQestObsCLNII %>% select(-errMean, -errmedian)) %>% 
  select(Year, slSM, slMedian, clSM, clMedian) %>% round(2) %>% filter(Year >= 2010)


#### Fitted Total Equilibrium Supply 

EQestTSNII <- merge(EQestCLNII, EQestSLNII) %>% transmute(Year = Year, TSmean = slMean + clMean, 
                                                          TSmedian = slMedian + clMedian)

totalSupply <- supp_diss_adj %>% transmute(Year = Year, TS = TotalSupply)

EQestObsTSNII <- left_join(EQestTSNII,totalSupply) %>% select(Year, TSmean, TSmedian, TS) %>% 
  mutate(errMean = (TS - TSmean), errmedian = (TS- TSmedian)) %>% round(4)

EQestObsTSNII %>% select(Year, TSmedian, TS) %>% filter(Year >= 2010) %>%
  select(Year, TS, TSmedian) %>% round(2)



#### Final iterations quantities
# Fed Cattle Supply
ITRquantities_sl_MeansNII <- apply(slNodes_itr[1:25,], 2, mean)
ITRquantities_sl_MeansNII <- ITRquantities_sl_MeansNII %>% as.data.frame()
names(ITRquantities_sl_MeansNII) <- "slMean"
ITRquantities_sl_MeansNII <- ITRquantities_sl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRquantities_sl_MediansNII <- apply(slNodes_itr[1:25,], 2, median)
ITRquantities_sl_MediansNII <- ITRquantities_sl_MediansNII %>% as.data.frame()
names(ITRquantities_sl_MediansNII) <- "slMedian"
ITRquantities_sl_MediansNII <- ITRquantities_sl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRestSLNII <- merge(ITRquantities_sl_MeansNII, ITRquantities_sl_MediansNII)

supp_sl_MODEL <- supp_diss_adj %>% select(Year, slSM = fedSlaughter_BillLb)

ITRestObsSLNII <- left_join(ITRestSLNII, supp_sl_MODEL) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)
ITRestObsSLNII

# Cull Cow Supply
ITRquantities_cl_MeansNII <- apply(clNodes_itr[1:25,], 2, mean)
ITRquantities_cl_MeansNII <- ITRquantities_cl_MeansNII %>% as.data.frame()
names(ITRquantities_cl_MeansNII) <- "clMean"
ITRquantities_cl_MeansNII <- ITRquantities_cl_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRquantities_cl_MediansNII <- apply(clNodes_itr[1:25,], 2, median)
ITRquantities_cl_MediansNII <- ITRquantities_cl_MediansNII %>% as.data.frame()
names(ITRquantities_cl_MediansNII) <- "clMedian"
ITRquantities_cl_MediansNII <- ITRquantities_cl_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRestCLNII <- merge(ITRquantities_cl_MeansNII, ITRquantities_cl_MediansNII)

supp_cl_MODEL <- supp_diss_adj %>% select(Year, clSM = cowsCulled_BillLb)

ITRestObsCLNII <- left_join(ITRestCLNII, supp_cl_MODEL) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)
ITRestObsCLNII

merge(ITRestObsSLNII %>% select(-errMean, -errmedian), 
      ITRestObsCLNII %>% select(-errMean, -errmedian)) %>% 
  select(Year, slSM, slMedian, clSM, clMedian) %>% round(2) %>% filter(Year >= 2010)


### Fitted fed cattle final iterated prices

ITRprices_ps_MeansNII <- apply(prices_ps_itr[1:25,], 2, mean)
ITRprices_ps_MeansNII <- ITRprices_ps_MeansNII %>% as.data.frame()
names(ITRprices_ps_MeansNII) <- "psMean"
ITRprices_ps_MeansNII <- ITRprices_ps_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRprices_ps_MediansNII <- apply(prices_ps_itr[1:25,], 2, median)
ITRprices_ps_MediansNII <- ITRprices_ps_MediansNII %>% as.data.frame()
names(ITRprices_ps_MediansNII) <- "psMedian"
ITRprices_ps_MediansNII <- ITRprices_ps_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRestPSNII <- merge(ITRprices_ps_MeansNII, ITRprices_ps_MediansNII)

ITRestObsPSNII <- left_join(ITRestPSNII,quantities_prices_capK) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(4)

ITRestObsPSNII

###### Fitted Cull Cow Equilibrium prices
ITRprices_pc_MeansNII <- apply(prices_pc_itr[1:25,], 2, mean)
ITRprices_pc_MeansNII <- ITRprices_pc_MeansNII %>% as.data.frame()
names(ITRprices_pc_MeansNII) <- "pcMean"
ITRprices_pc_MeansNII <- ITRprices_pc_MeansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRprices_pc_MediansNII <- apply(prices_pc_itr[1:25,], 2, median)
ITRprices_pc_MediansNII <- ITRprices_pc_MediansNII %>% as.data.frame()
names(ITRprices_pc_MediansNII) <- "pcMedian"
ITRprices_pc_MediansNII <- ITRprices_pc_MediansNII %>% mutate(Year = quantities_prices_capK$Year) %>% 
  select(Year, everything())

ITRestPCNII <- merge(ITRprices_pc_MeansNII, ITRprices_pc_MediansNII)

ITRestObsPCNII <- left_join(ITRestPCNII,quantities_prices_capK) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(4)

ITRestObsPCNII

mergedPricesITR <- merge(ITRestObsPSNII %>% select(-errMean, -errmedian), 
                      ITRestObsPCNII %>% select(-errMean, -errmedian)) %>% select(Year, ps, psMedian, psMean, 
                                                                                 pc, pcMedian, pcMean) %>%
  filter(Year >= 2010)
mergedPricesITR[,-1] <- mergedPricesITR[,-1] * 100





















































































