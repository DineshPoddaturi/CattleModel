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

#### Cow prices I have two different datasets. I used these datasets to convert and compile the data.

cows_pricesR1 <- read_excel("./RegionalData/CowBullPrices.xlsx", sheet='SouthCentralDressedDelivered') %>% as.data.frame()

cows_pricesR1 <- cows_pricesR1 %>% select(DATE, `PREMIUM WHITE-WTD-AVERAGE-PRICE`, 
                                          `BREAKER75-500Up-WTD-AVERAGE-PRICE`, `BREAKER75-500L-WTD-AVERAGE-PRICE`, 
                                          `BONER85-500Up-WTD-AVERAGE-PRICE`, `BONER85-500L-WTD-AVERAGE-PRICE`, 
                                          `CUTTER90-500Up-WTD-AVERAGE-PRICE`,
                                          `CUTTER90-4-500-WTD-AVERAGE-PRICE`, 
                                          `CUTTER90-400L-WTD-AVERAGE-PRICE`)

cows_pricesR1_Long <- gather(cows_pricesR1, WeightRange_Measurement, PriceDressed, 
                             `PREMIUM WHITE-WTD-AVERAGE-PRICE`:`CUTTER90-400L-WTD-AVERAGE-PRICE`) %>% arrange(DATE)

cows_pricesR1 <- cows_pricesR1_Long %>% separate(col=DATE, into=c("Year","Month","Day"))

cows_pricesR1$Year <- as.numeric(cows_pricesR1$Year)
cows_pricesR1$Month <- as.numeric(cows_pricesR1$Month)
cows_pricesR1$Day <- as.numeric(cows_pricesR1$Day)
cows_pricesR1$PriceDressed <- as.numeric(cows_pricesR1$PriceDressed)

cows_pricesR1 <- cows_pricesR1 %>% select(Year, Month, Day, PriceDressed) %>% filter(Year>2005)

cows_pricesR1 <- cows_pricesR1 %>% group_by(Year, Month, Day) %>% 
  mutate(PriceDressedDay = mean(PriceDressed, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR1 <- cows_pricesR1 %>% group_by(Year, Month) %>% 
  mutate(PriceDressedMonth = mean(PriceDressedDay, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR1 <- cows_pricesR1 %>% group_by(Year) %>% 
  mutate(PriceDressedYear = mean(PriceDressedMonth, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR1 <- cows_pricesR1 %>% select(Year, PriceDressedYear) %>% group_by(Year) %>% 
  distinct() %>% ungroup() %>% as.data.frame() %>% round(5) %>% transmute(Year = Year, DressedPrice = PriceDressedYear)


cows_pricesR2 <- read_excel("./RegionalData/CowBullPrices.xlsx", sheet='SouthCentralLiveDelivered') %>% as.data.frame()

cows_pricesR2 <- cows_pricesR2 %>% select(DATE, `PREMIUM WHITE-WTD-AVERAGE-PRICE`,
                                          `BREAKER75-WTD-AVERAGE-PRICE`, `BREAKER85-WTD-AVERAGE-PRICE`,
                                          `BREAKER90-WTD-AVERAGE-PRICE`)

cows_pricesR2_Long <- gather(cows_pricesR2, WeightRange_Measurement, PriceLive, 
                             `PREMIUM WHITE-WTD-AVERAGE-PRICE`:`BREAKER90-WTD-AVERAGE-PRICE`) %>% arrange(DATE)

cows_pricesR2 <- cows_pricesR2_Long %>% separate(col=DATE, into=c("Year","Month","Day"))

cows_pricesR2$Year <- as.numeric(cows_pricesR2$Year)
cows_pricesR2$Month <- as.numeric(cows_pricesR2$Month)
cows_pricesR2$Day <- as.numeric(cows_pricesR2$Day)
cows_pricesR2$PriceLive <- as.numeric(cows_pricesR2$PriceLive)

cows_pricesR2 <- cows_pricesR2 %>% select(Year, Month, Day, PriceLive) %>% filter(Year>2005)

cows_pricesR2 <- cows_pricesR2 %>% group_by(Year, Month, Day) %>% 
  mutate(PriceLiveDay = mean(PriceLive, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR2 <- cows_pricesR2 %>% group_by(Year, Month) %>% 
  mutate(PriceLiveMonth = mean(PriceLiveDay, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR2 <- cows_pricesR2 %>% group_by(Year) %>% 
  mutate(PriceLiveYear = mean(PriceLiveMonth, na.rm=TRUE)) %>% ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR2 <- cows_pricesR2 %>% select(Year, PriceLiveYear) %>% group_by(Year) %>% 
  distinct() %>% ungroup() %>% as.data.frame() %>% round(5) %>% transmute(Year = Year, LivePrice = PriceLiveYear)

cows_pricesR <- read_excel("./RegionalData/RegionalCowBullPrices.xlsx") %>% as.data.frame()

cows_pricesR <- cows_pricesR %>% separate(col = `Report Date`, into = c("Year", "Month", "Day"))

cows_pricesR <- cows_pricesR %>% filter(`Region Name`=='SOUTHWEST' | `Region Name`=='SOUTH CENTRAL')

cows_pricesR <- cows_pricesR %>% group_by(Year,Month,Day) %>% 
  mutate(Ratio=mean(`Live Wtd Avg Price`/`Dressed Wtd Avg Price`,na.rm=TRUE)) %>% 
  ungroup() %>% as.data.frame() %>% mutate_all(~ifelse(is.nan(.), NA, .))

cows_pricesR <- cows_pricesR %>%  fill(Ratio) %>% fill(Ratio, .direction="up") %>% 
  fill(Ratio, .direction="down")

cows_pricesR <- cows_pricesR %>% mutate(LivePrice = round(`Dressed Wtd Avg Price` * Ratio,5))

cows_pricesR <- cows_pricesR %>% select(Year, Month, Day, Grade, `Weight Range`, LivePrice) %>%
  filter(Grade!='Bull (92% lean)')

cows_pricesR <- cows_pricesR %>% group_by(Year,Month,Day,Grade) %>% mutate(LivePriceM = round(mean(LivePrice),5)) %>% 
  ungroup() %>% as.data.frame()

cows_pricesR <- cows_pricesR %>% select(Year, Month, Day, LivePriceM)

cows_pricesR <- cows_pricesR %>% group_by(Year,Month,Day) %>% mutate(LivePriceDay = round(mean(LivePriceM),5)) %>% 
  ungroup() %>% as.data.frame()

cows_pricesR <- cows_pricesR %>% group_by(Year,Month) %>% mutate(LivePriceMon = round(mean(LivePriceDay, na.rm=TRUE),5)) %>% 
  ungroup() %>% as.data.frame()

cows_pricesR <- cows_pricesR %>% group_by(Year) %>% 
  mutate(LivePriceYr = round(mean(LivePriceMon, na.rm=TRUE),5)) %>% 
  ungroup() %>% as.data.frame()

cows_pricesR <- cows_pricesR %>% select(Year, LivePriceYr) %>% group_by(Year) %>% 
  distinct() %>% ungroup() %>% as.data.frame()

cows_pricesR$Year <- as.numeric(cows_pricesR$Year)

cows_pricesR <- cows_pricesR %>% add_row(Year = c(2006,2007,2008,2009))

cows_pricesR <- cows_pricesR %>% arrange(Year)

cows_pricesR$LivePriceYr[cows_pricesR$Year>2007 & cows_pricesR$Year<2018] <- 
  cows_pricesR2$LivePrice[cows_pricesR2$Year>2007 & cows_pricesR2$Year<2018]

#### The final regional cow prices 
cows_pricesR <- cows_pricesR %>% mutate(pc=LivePriceYr)



#### Here sheet 2 contains the regional steer prices (Note: I edited the excel file to have readable column names)
steers_PricesR <- read_excel("./RegionalData/SlaughteredPrices_Steers_Heifers_TX-OK-Monthly.xlsx", sheet = 2) %>% 
  as.data.frame()

steers_PricesR <- steers_PricesR %>% select(DATE, `Total all grades1-AVERAGE`)
steers_PricesR <- steers_PricesR %>% separate(col = DATE, into = c("Year", "Month", "Day")) %>% 
  transmute(Year = Year, SteerPrice = `Total all grades1-AVERAGE`)
steers_PricesR$Year <- as.numeric(steers_PricesR$Year)

steers_PricesR <- steers_PricesR %>% group_by(Year) %>% mutate(SteerPrice = mean(SteerPrice, na.rm=TRUE)) %>% 
  select(Year, SteerPrice) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame() %>% round(5) %>% filter(Year<2024)

#### Here sheet 3 contains the regional heifer prices (Note: I edited the excel file to have readable column names)
heifers_PricesR <- read_excel("./RegionalData/SlaughteredPrices_Steers_Heifers_TX-OK-Monthly.xlsx", sheet = 3) %>% 
  as.data.frame()
heifers_PricesR <- heifers_PricesR %>% select(DATE, `Total all grades1-AVERAGE`)
heifers_PricesR <- heifers_PricesR %>% separate(col = DATE, into = c("Year", "Month", "Day")) %>% 
  transmute(Year = Year, HeiferPrice = `Total all grades1-AVERAGE`)
heifers_PricesR$Year <- as.numeric(heifers_PricesR$Year)
heifers_PricesR <- heifers_PricesR %>% group_by(Year) %>% mutate(HeiferPrice = mean(HeiferPrice, na.rm=TRUE)) %>% 
  select(Year, HeiferPrice) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame() %>% round(5) %>% filter(Year<2024)

steersHeifers_PricesR <- merge(steers_PricesR, heifers_PricesR) 

steersHeifers_PricesR <- steersHeifers_PricesR %>% 
  mutate(ps = rowMeans(steersHeifers_PricesR %>% select(-Year))) %>% round(5)



############################ converting the prices into yearly by taking the mean of the observed prices #########
pcsR <- cows_pricesR %>% select(Year,pc) %>% mutate(pc = pc/100)
pssR <- steersHeifers_PricesR %>% select(Year, ps) %>% mutate(ps=ps/100)

pcs_cwtR <- pcsR %>% mutate(pcs_cwt = pc*100) %>% arrange(Year)
pss_cwtR <- pssR %>% mutate(pss_cwt = ps*100) %>% arrange(Year)
pc_ps_cwtR <- merge(pcs_cwtR, pss_cwtR) %>% select(Year,pss_cwt, pcs_cwt)
pc_psR <- merge(pcsR,pssR)

##### Since I do not have data for 2006 and  2007. I used the national data for these two years. 
##### The following code snippet does that.
pc_ps_cwt_I <- pc_ps_cwt %>% filter(Year>=2002&Year<=2023)
pc_ps_cwtR <- pc_ps_cwtR %>% mutate(pcs_cwt= coalesce(pcs_cwt, pc_ps_cwt_I$pcs_cwt)) %>% round(5)

pc_ps_I <- pc_ps %>% filter(Year>=2002&Year<=2023)
pc_psR <- pc_psR %>% mutate(pc= coalesce(pc, pc_ps_I$pc)) %>% round(5)

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
  mutate(k9 = if_else(k9 < delta*lag(k8), if_else(k9<0,0,k9), delta*lag(k8)), k10 = 0)

# Determining the supply of fed cattle (in head)
supp_slR <- StockR %>% select(Year, K, k3) %>% 
  mutate(fedSlaughter = g * lag(K,1) - k3) %>% select(Year, fedSlaughter)

# Determining the supply of cull cows (in head)
# Before determining he supply of the cull cows, first I check the cull cow price and holding costs
# To compute holding costs, first I assume the cows are culled when they are 9 years old. This is an approximation for now
# Why? : Because I need to decide the age distribution of the older animals.
#### We are in the case where the farmers cull the 9 year old cows
#### The holding costs will become.
pc_ps_hcR <- pc_psR %>% mutate( hc = (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1))))
prices_costsR <- pc_ps_hcR %>% round(5)

StockRc <- StockR %>% filter(Year >= prices_costsR$Year[1]-5 & Year <= prices_costsR$Year[nrow(prices_costsR)])
clNew1R <- NULL

for(i in 1:nrow(prices_costsR)){
  
  # i <- 9
  
  yearIR <- prices_costsR$Year[i]
  
  psRc <- prices_costsR$ps[i]
  pcRc <- prices_costsR$pc[i]
  hcRc <- prices_costsR$hc[i]
  
  ##### Here I compute the expected price using Gaussian Quadrature for Integration
  EpsRc <- sum(as.numeric(psRc) * fedMeshCheb)
  EpcRc <- sum(as.numeric(pcRc) * cullMeshCheb)
  
  expectedValue_k9R <- beta * EpcRc + g * (beta^3) * EpsRc - (1+g*beta*(gamma0+beta*gamma1)) * hcRc
  
  #If expectedValue_k9 is > pc then we have 9 year olds in the stock , else we cull all the 9 year olds.
  # This mean no more 10 year olds. See pages 35 and so on in dissertation
  if(round(expectedValue_k9R,3) > round(pcRc,3)){
    # We should have 9-year olds in the stock. All 10-years are culled.
    k9_OldR <- 1
  }else if(round(expectedValue_k9R,3) == round(pcRc,3)){
    # We should have 8-year olds in the stock. All 10-years and 9-years are culled
    k9_OldR <- 0
  } else if(round(expectedValue_k9R,3) < round(pcRc,3)){
    # We should have 7-year olds in the stock, All the 10,9,8 year old cows are culled
    k9_OldR <- 2
  }
  
  k9_OldR
  
  if(k9_OldR == 1){
    
    # mergedForecast_Proj$k9[mergedForecast_Proj$Year == yearI] <- mergedForecast_Proj %>% filter(Year == yearI) %>%
    #   mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% 
    #   mutate(k9 = if_else(k9 < 0, 0, k9)) %>% select(k9) %>% as.numeric()
    
    StockRc$k9[StockRc$Year == yearIR] <- StockRc %>% filter(Year == yearIR) %>%
        mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% 
        mutate(k9 = if_else(k9 < 0, 0, delta * StockRc$k8[StockRc$Year == yearIR-1])) %>% select(k9) %>% as.numeric()
    
    # clNew1R[i] <- StockRc %>% filter(Year == yearIR) %>%
    #   mutate(clSupp = (k10 + (1-delta) * k9 + (1-delta) * k8 + (1-delta) * k7)) %>%
    #   select(clSupp) %>% as.numeric()
    
    clNew1R[i] <- StockRc %>% filter(Year == yearIR) %>%
      mutate(clSupp = ((k9 + (1-delta) * k8 + (1-delta) * k7) * 1 +
                         (delta * (k8 + k7 + k6) - (k7 + k8 + k9)))) %>%
      select(clSupp) %>% as.numeric()
    
  }else if(k9_OldR == 0) {
    
    k9NextR <- StockRc %>% filter(Year == yearIR) %>%
      mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% 
      mutate(k9 = if_else(k9 < 0, 0, delta * StockRc$k8[StockRc$Year == yearIR-1])) %>% select(k9) %>% 
      as.numeric()
    
    # clNew1R[i] <- StockRc %>% filter(Year == yearIR) %>%
    #   mutate(clSupp = (k10 + k9NextR + k8 + (1-delta) * k7)) %>%
    #   select(clSupp) %>%  as.numeric()
    
    clNew1R <- StockRc %>% filter(Year == yearIR) %>%
      mutate(clSupp = ((k9NextR + (1-delta) * k8 + (1-delta) * k7) * 1 +
                         (delta * (k8 + k7 + k6) - (k7 + k8 + k9NextR)))) %>%
      select(clSupp) %>% as.numeric()
    
    StockRc$k9[StockRc$Year == yearIR] <- 0
    
  } else if(k9_OldR == 2){
    
    # k9Next <- mergedForecast_Proj %>% filter(Year == yearI) %>%
    #   mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9)) %>% select(k9) %>% 
    #   as.numeric()
    
    k9NextR <- StockRc %>% filter(Year == yearIR) %>%
      mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% 
      mutate(k9 = if_else(k9 < 0, 0, delta * StockRc$k8[StockRc$Year == yearIR-1])) %>% select(k9) %>% 
      as.numeric()
    
    k8NextR <- StockRc$k8[StockRc$Year == yearIR]
    
    # clNew1R[i] <- StockRc %>% filter(Year == yearIR) %>%
    #   mutate(clSupp = (k10 + k9NextR + k8NextR + (1-delta) * k7) * (Cull_avg/1000000000)) %>%
    #   select(clSupp) %>% as.numeric()
    
    clNew1R[i] <- StockRc %>% filter(Year == yearIR) %>%
      mutate(clSupp = ((k9NextR + (1-delta) * k8NextR + (1-delta) * k7) * 1 +
                         (delta * (k8NextR + k7 + k6) - (k7 + k8 + k9NextR)))) %>%
      select(clSupp) %>% as.numeric()
    
    StockRc$k9[StockRc$Year == yearIR] <- 0
    StockRc$k8[StockRc$Year == yearIR] <- 0
    StockRc$k7[StockRc$Year == yearIR] <- StockRc %>% filter(Year == yearIR-1) %>%
      mutate(k7 = delta * k6) %>% select(k7) %>% as.numeric()
  }
  
}

clSuppR <- as.data.frame(clNew1R) %>% mutate(Year = prices_costsR$Year, 
                                             cowsCulled = clNew1R) %>% select(Year, cowsCulled)

supp_clR <- clSuppR

# supp_clR <-  StockRc %>% select(Year, k7, k8, k9, k10) %>%
#   mutate(cowsCulled = k10 + (k9 - lead(k10,1)) + (k8 - lead(k9,1)) + (k7 - lead(k8,1)) ) %>%
#   select(Year, cowsCulled)



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
# Min.   :0.1287  
# 1st Qu.:0.1614  
# Median :0.1676  
# Mean   :0.1706  
# 3rd Qu.:0.1776  
# Max.   :0.2177  
# NA's   :2 

#### From the above on average approximately 17% of the heifers are added into the breeding stock 

#### Here I read corn price data. These are in $/bushel. 
#### I am converting the price from $/bushel to $/pound
corn_priceR <- read_excel("./RegionalData/CornPriceReceived-TX-OK-NM.xlsx") %>% as.data.frame()
names(corn_priceR)
corn_priceR <- corn_priceR %>% select(Year, Period, Value)
pcornR <- corn_priceR %>% group_by(Year) %>% mutate(pcorn = round(mean(Value),5)) %>% 
  select(Year,pcorn) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcornR <- pcornR %>% mutate(pcornLb = pcorn/56)


allPricesR <- merge(pcornR, prices_costsR) 

meat_billR <- supp_dissR %>% 
  mutate(TS = TotalSupply, TD = TotalDiss, sl = fedSlaughter_BillLb, cl = cowsCulled_BillLb) %>%
  select(Year, sl, cl, TS, TD)

prices_quantR <- merge(allPricesR, meat_billR) %>% round(5)



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
cowsSlaughtered_obsR <- cowsSlaughteredR %>% transmute(Year = Year, cullMeat = cull_meat/1000000000)
heifersSlaughtered_obsR <- heifersSlaughteredR %>% transmute(Year = Year, heiferMeat = heifer_meat/1000000000)
steersSlaughtered_obsR <- steersSlaughteredR %>% transmute(Year = Year, steerMeat = steer_meat/1000000000)

fedCattleSupply_obsR <- merge(heifersSlaughtered_obsR, steersSlaughtered_obsR) %>% 
  transmute(Year = Year, sl_obs = heiferMeat + steerMeat)

cullCowSupply_obsR <- cowsSlaughtered_obsR %>% transmute(Year = Year, cl_obs = cullMeat)

#### Here we get the production shocks
obsEst_sl_SupplyR <- merge(fedCattleSupply_obsR, supp_slR) %>% 
  transmute(Year = Year, sl_obs = sl_obs, sl_est = fedSlaughter_BillLb, slShock = sl_obs/sl_est)

slSupplyShockGaussianR <- obsEst_sl_SupplyR %>% transmute(Year = Year, slShock = 0)

set.seed(3)
slSupply_ShockR <- rnorm(n = nrow(slSupplyShockGaussianR), mean = 1, sd = std(obsEst_sl_SupplyR$slShock))
slSupplyShockGaussianR$slShock <- slSupply_ShockR

#### Here we construct the cull cows production shock
obsEst_cl_SupplyR <- merge(cullCowSupply_obsR, supp_clR) %>% 
  transmute(Year = Year, cl_obs = cl_obs, cl_est = cowsCulled_BillLb,
            clShock = cl_obs/cl_est)

clSupplyShockgaussianR <- obsEst_cl_SupplyR %>% transmute(Year = Year, clShock = 0)

set.seed(4)
clSupply_ShockR <- rnorm(n = nrow(clSupplyShockgaussianR), mean = 1, sd = std(obsEst_cl_SupplyR$clShock))
clSupplyShockgaussianR$clShock <- clSupply_ShockR

##### I will generate the demand shocks. For this I need the observed demand and the constructed demand. 
##### The observed demand is the derived demand from data. 
##### Constructed demand would be animals slaughtered for consumption.

### Observed derived demand would be the sum of Exports and Domestic Consumption from demandBeef dataframe
### Constructed demand would be the total animals slaughtered for consumption purposes. 
### I get this from slaughtered data. The dataframe
### totalDisappeared has the data.

estDR <- merge(supp_slR, supp_clR) %>% 
  transmute(Year = Year, demandEst = fedSlaughter_BillLb + cowsCulled_BillLb) %>% na.omit()

obsDemandR <- supp_dissR %>% transmute(Year = Year, demandObs = TotalDiss)
demandShockR <- merge(obsDemandR, estDR) %>% mutate(dShock = demandObs/demandEst)

demandShockGaussianR <- demandShockR %>% transmute(Year = Year, Shock = 0)

## Now i generate gaussian shock which is consistent with historical data.
## I use the standard deviation of historical data to construct the gaussian random variables. Here the mean is 1
set.seed(1)
demandShockGR <- rnorm(n = nrow(demandShockGaussianR), mean = 1, sd = std(demandShockR$dShock))
demandShockGaussianR$Shock <- demandShockGR

#### I am merging all the supply and demand shocks
allShocksR <- merge(merge(demandShockGaussianR, slSupplyShockGaussianR), clSupplyShockgaussianR)

sl_stockR <- supp_slR %>% transmute(Year = Year, sl_est = fedSlaughter_BillLb, slHead = fedSlaughter)
cl_stockR <- supp_clR %>% transmute(Year = Year, cl_est = cowsCulled_BillLb, clHead = cowsCulled)

dataListR <- list(sl_stockR, cl_stockR, slSupplyShockGaussianR, 
                 clSupplyShockgaussianR, dressedWeights_sl_clR, calf_cropR, StockRc)

allStockShocksR <- Reduce(function(...) merge(...), dataListR) %>% as.data.frame()


#### Here I am constructing the supply of fed cattle ahead. Note that this is approximation. 
#### When I compare these numbers with the observed ones, these are a bit high. This comes from:
#### 1. We incorporated a Gaussian shock, 2. The storage approximation comes into play as well.
#### 0.21 comes from the fact that approximately a maximum of 21% of the progeny is added to the breeding stock
#### See above summary for more details
newSL_1R <- allStockShocksR %>% 
  transmute(Year = Year+1, slt = ((g - 0.21 * g) * lag(K,2) * lag(slShock,1) + 
                                    (1 - 0.21 * g) * g * delta * (lag(K,2) - (g - 0.21 * g) * lag(K,3) - 
                                                                    (lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2)))),
            slLbs = slt * Slaughter_avg/1000000000)

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

newCL_1R <- allStockShocksR %>%
  transmute(Year = Year + 1, clt = (k9 + (1-delta) * k8 + (1-delta) * k7) * clShock +
              (delta * (k8 + k7 + k6) - (k7 + k8 + k9)),
            clLbs = clt * Cull_avg/1000000000)

# newCL_1R <- supp_clR %.% mutate(clLbs = cowsCulled_BillLb)


# newCL_3 <- allStockShocks %>%
#   transmute(Year = Year + 3, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock +
#               (delta^2) * (delta * (k6 + k5 + k4) - (k5 + k6 + k7)),
#             clLbs = clt * Cull_avg/1000000000)


cornPriceR <- pcornR

cullCowsProd_1R <- newCL_1R %>% transmute(Year = Year, cullCows = clLbs) %>% round(5)
fedCattleProd_1R <- newSL_1R %>% transmute(Year = Year, fedCattle = slLbs) %>% round(5)


prod_CornPR <- merge(merge(fedCattleProd_1R, cullCowsProd_1R),cornPriceR) %>% drop_na() %>% round(5)

### Here I am generating the shocks again so that when we merge all the data frames we have enough data.
### Note: Since these are independent random shocks we are okay by increasing the n.
demandShockGaussian1R <- prod_CornPR %>% transmute(Year = Year, Shock = 0)
slSupplyShockGaussian1R <- prod_CornPR %>% transmute(Year = Year, slShock = 0)
clSupplyShockgaussian1R <- prod_CornPR %>% transmute(Year = Year, clShock = 0)

set.seed(1)
demandShockGR <- rnorm(n = nrow(prod_CornPR), mean = 1, sd = std(demandShockR$dShock))
demandShockGaussian1R$Shock <- demandShockGR

set.seed(3)
slSupply_ShockR <- rnorm(n = nrow(prod_CornPR), mean = 1, sd = std(obsEst_sl_SupplyR$slShock))
slSupplyShockGaussian1R$slShock <- slSupply_ShockR

set.seed(4)
clSupply_ShockR <- rnorm(n = nrow(prod_CornPR), mean = 1, sd = std(obsEst_cl_SupplyR$clShock))
clSupplyShockgaussian1R$clShock <- clSupply_ShockR

fedCattleProdR <- fedCattleProd_1R
cullCowsProdR <-  cullCowsProd_1R



#### NOTE: We constructed fed cattle supply and cull cow supply for existing years ahead which 
#### includes Gaussian shocks as well. 
#### Although we are using the data of existing years ahead, since we are using all the nodes of both fed cattle, 
#### and cull cows supply the price is right. DO NOT GET CONFUSED!

##############################################################################################
###### Functions that returns the chebychev nodes and Chebychev polynomial matrix.############
##############################################################################################

##### Function to create chebyshev polynomial matrix
chebyshevMatrixR <- function(x,d,n){
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
chebyshevNodesR <- function(d, n){
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
chebNodesNR <- 5

stateVarDemandR <- supp_dissR %>% transmute(Year = Year, demand = TotalDiss)

stateVariablesListR <- list(cornPriceR, cullCowsProdR, fedCattleProdR, demandShockGaussian1R,
                           slSupplyShockGaussian1R, clSupplyShockgaussian1R, stateVarDemandR)

stateVarsR <- Reduce(function(...) merge(...), stateVariablesListR) %>% drop_na() %>% round(5)


cornNodesR <- chebyshevNodes(d = stateVarsR$pcorn, n = chebNodesNR)
cullCowNodesR <- chebyshevNodes(d = stateVarsR$cullCows, n = chebNodesNR)
fedCattleNodesR <- chebyshevNodes(d = stateVarsR$fedCattle, n = chebNodesNR)
demandNodesR <- chebyshevNodes(d = stateVarsR$demand, n = chebNodesNR)
dShockNodesR <- chebyshevNodes(d = stateVarsR$Shock, n = chebNodesNR)
slShockNodesR <- chebyshevNodes(d = stateVarsR$slShock, n = chebNodesNR)
clShockNodesR <- chebyshevNodes(d = stateVarsR$clShock, n = chebNodesNR)

corn_nodesR <- cornNodesR %>% as.data.frame()
cull_nodesR <- cullCowNodesR %>% as.data.frame()
fed_nodesR <- fedCattleNodesR  %>% as.data.frame()
d_nodesR <- demandNodesR %>% as.data.frame()
dshock_nodesR <- dShockNodesR %>% as.data.frame()
slShock_nodesR <- slShockNodesR %>% as.data.frame()
clShock_nodesR <- clShockNodesR %>% as.data.frame()

names(corn_nodesR) <- "cornNodes"
names(cull_nodesR) <- "cullNodes"
names(fed_nodesR) <- "fedNodes"
names(d_nodesR) <- "demandNodes"
names(dshock_nodesR) <- "dShockNodes"
names(slShock_nodesR) <- "slShockNodes"
names(clShock_nodesR) <- "clShockNodes"

##### Cartesian product of the nodes
cull_cartesianR <- crossing(corn_nodesR, cull_nodesR, dshock_nodesR) %>% as.data.frame()
fed_cartesianR <- crossing(corn_nodesR, fed_nodesR, dshock_nodesR) %>% as.data.frame()

#### The following created chebyshev matrix containing chebyshev polynomials
cornChebyshevMatrixR <- chebyshevMatrix(x = cornNodesR, d = stateVarsR$pcorn, n = chebNodesNR)
cullCowsChebyshevMatrixR <- chebyshevMatrix(x = cullCowNodesR, d = stateVarsR$cullCows, n = chebNodesNR)
fedCattleChebyshevMatrixR <- chebyshevMatrix(x = fedCattleNodesR, d = stateVarsR$fedCattle, n = chebNodesNR)
dShockChebyshevMatrixR <- chebyshevMatrix(x = dShockNodesR, d = stateVarsR$Shock, n = chebNodesNR)
demandChebyshevMatrixR <- chebyshevMatrix(x = demandNodesR, d = stateVarsR$demand, n = chebNodesNR)

###### Here I am taking the tensor product to create interpolation matrix of grids. 
###### kron takes the kronecker tensor product of two matrices
##### For cull cows we use corn, cull cows production, and demand shock chebyshev matrices
##### For fed cattle we use corn, fed cattle production, and demand shock chebyshev matrices

cullInterpolationMatrixR <-  kron(kron(cornChebyshevMatrixR, cullCowsChebyshevMatrixR), dShockChebyshevMatrixR)
fedCattleInterpolationMatrixR <-  kron(kron(cornChebyshevMatrixR, fedCattleChebyshevMatrixR), dShockChebyshevMatrixR)

#### Now I have to write code to generate price series using the interpolation matrix, coefficient vector, 
#### and actual prices. This needs a lot of work. 

##### First I write the functions that I use to get the parameters, prices, and stocks. These functions are based off 
##### of the model solution. For more information regarding the system of equations please refer the dissertation
##### document

########### We use the following loss function to estimate mu_tilde and s_tilde
lossfnR <- function(theta,e,ps,pc){
  mu <- theta[1]
  s <- theta[2]
  
  v <- sum((e - ((( mu - ((ps-pc)/phi)))/s)))^2
  
  return(v)
}

#### optParamFunction returns the parameters mu_tilde and s_tilde
optParamFunctionR <- function(sl, cl, ps, pc, thetas){
  
  s <- sl
  c <- cl
  
  sl_share <- s/(s+c)
  cl_share <- 1-sl_share
  
  tilde <- log((1-cl_share)/cl_share)
  
  theta0 <- thetas
  
  out <- BBoptim(par= theta0, fn = lossfnR, e=tilde ,ps=ps, pc=pc)
  
  muTilde <- out$par[1]
  sTilde <- out$par[2]
  
  return(c(muTilde,sTilde))
  
}

##### Price function. This contains the solution system
optPriceFunctionR <- function(p, sl, cl, A, B, hc_discounted){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  ### Equilibrium condition of the fed cattle production
  F1 <- sl - A * ((exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR))/(1 + (exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR))))
  
  ### equilibrium condition of the cull cow production
  F2 <- cl  - A * (1/(1+ exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR)))
  
  ### These are the price conditions
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_newR
  
  #### above four equations muct be solved simultanously to get the price and expected price.
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  return(F)
  
}


###### optKFunctionR returns the optimal k_{3,t+1} and sum of k_{j,t+1} where j E [7,8,9]
optKFunctionR <- function(K, ps, pc, A, B){
  
  K1 <- K[1]
  K2 <- K[2]
  
  fed <- g * Stock_1tR - K1 - A * 
    ((exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR))/(1 + (exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR))))
  
  cull <- k_9tR + k_8tR + k_7tR - K2 - A * (1/(1+ exp((mu_TildeR - ((ps/phi) - (pc/phi)))/s_TildeR)))
  
  F <- fed^2 + cull^2
  
  return(F)
  
}


##### Here setting up the data frame for the quantities. Note: It contains K_{t-1} and K_{j,t} for j = {10,9,8,7}

### The following data frame contains the cows of age 7, 8, 9, 10 at time t.
K_jtR <- StockRc %>% select(Year, k7, k8, k9, k10)

#### The following data frame has K_{t-1} i.e., the previous period stock
K_1tR <- StockRc %>% transmute(Year = Year+1, K = K)

capKR <- merge(K_1tR, K_jtR)

#### Production from the storage model specification
sl_quant_SMR <- fedCattleProd_1R %>% transmute(Year = Year, slSM = fedCattle)
cl_quant_SMR <- cullCowsProd_1R %>% transmute(Year = Year, clSM = cullCows)

sl_quantR <- supp_slR %>% transmute(Year = Year, sl = fedSlaughter_BillLb)
cl_quantR <- supp_clR %>% transmute(Year = Year, cl = cowsCulled_BillLb)

A_quantR <-  supp_dissR  %>% transmute(Year = Year, A = TotalDiss)

quantListR <- list(A_quantR, sl_quant_SMR, cl_quant_SMR, sl_quantR, cl_quantR)

quantitiesR <-  Reduce(function(...) merge(...), quantListR) %>% drop_na()

price_sl_cl_hcR <- prices_quantR %>% select(Year, ps, pc, hc)

# imports_exports <- merge(imports, exports)

adjFactorR <- supp_dissR %>% select(Year,AdjFactor)

variablesListR <- list(price_sl_cl_hcR, capKR, dressedWeights_sl_clR, quantitiesR, adjFactorR)

quantities_prices_capKR <- Reduce(function(...) merge(...), variablesListR) %>% drop_na() %>% round(5)

################################### IMPORTANT ##################################
####### We have the constructed quantities and shocks from 1982 to 2020 ########
####### So the prices we use are from 1982 to 2020 ############################
################################################################################

quantities_prices_capKR <- quantities_prices_capKR %>% filter(Year >= 1990)

prices_psR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_pcR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_hcR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
expected_PSR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
expected_PCR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

mu_TildesR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
s_TildesR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

A_nodesR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
slNodesR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
clNodesR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

##### The following saves the equilibrium prices and quantities
prices_ps_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_pc_eqR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_hc_eqR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

expected_PS_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
expected_PC_eqR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

mu_Tildes_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
s_Tildes_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

A_nodes_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
slNodes_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
clNodes_eqR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

##### The following saves the iterations after the equilibrium 
prices_ps_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_pc_itrR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
prices_hc_itrR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

expected_PS_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
expected_PC_itrR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

mu_Tildes_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
s_Tildes_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

A_nodes_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
slNodes_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
clNodes_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

D_slPsPc_itrR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
D_clPsPc_itrR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

#### The following saves overall prices and quantities
D_slPsPcR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
D_clPsPcR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

D_PsPcR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
S_psPCR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

fedDiffR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
cullDiffR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

mu_Tildes_PriorR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
s_Tildes_PriorR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

k3t1R <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
kjt1R <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

slNewR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
clNewR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

slDR <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))
clDR <- matrix(data = 0,nrow=nrow(cullInterpolationMatrixR),ncol = nrow(quantities_prices_capKR))

c_cull1R <- matrix(data=0, nrow = nrow(cullInterpolationMatrixR), ncol = 1)
c_fed1R <- matrix(data=0, nrow = nrow(fedCattleInterpolationMatrixR), ncol = 1)

c_cull_optR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow=nrow(cullInterpolationMatrixR), ncol=1)
c_fed_optR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow=nrow(fedCattleInterpolationMatrixR), ncol=1)

maxIterR <- 750

fedPriceR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(fed_cartesianR), ncol=maxIterR)
cullPriceR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(cull_cartesianR), ncol=maxIterR)
fedProdR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(fed_cartesianR), ncol=maxIterR)
cullProdR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(cull_cartesianR), ncol=maxIterR)

c_cull_itrR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(cull_cartesianR), ncol=maxIterR)
c_fed_itrR <- lapply(1:nrow(quantities_prices_capKR), matrix, data= 0, nrow = nrow(fed_cartesianR), ncol=maxIterR)

checkTolR <- matrix(data = 0, nrow = maxIterR, ncol = 4)



############################### THE ITERATION ALGORITHM ########################

for(i in 1:nrow(quantities_prices_capKR)){
  
  # i <- 20
  ### Here we get the observed quantities. For fed production and cull production these are estimated production 3 years ahead
  AR <- quantities_prices_capKR$A[i] ## Note: Although I am assigning the total demand to variable here, I am using the
  #                                  ## fed cattle production node and cull cow production node with demand shock to get 
  #                                  ## the total demand for that particular node. 
  
  slR <- quantities_prices_capKR$slSM[i]
  clR <- quantities_prices_capKR$clSM[i]
  
  adjFacR <- A/(sl+cl)
  
  psR <-   quantities_prices_capKR$ps[i]
  pcR <-   quantities_prices_capKR$pc[i]
  hcR <- quantities_prices_capKR$hc[i]
  
  # if(i>1){
  #   ps <-   median(quantities_prices_capK$ps[1:i])
  #   pc <-   median(quantities_prices_capK$pc[1:i])
  #   hc <-   median(quantities_prices_capK$hc[1:i])
  #   A <- median(quantities_prices_capK$A[1:i])
  #   sl <- median(quantities_prices_capK$sl[1:i])
  #   cl <- median(quantities_prices_capK$cl[1:i])
  #   adjFac <- A/(sl+cl)
  # }
  
  params_mu_sR <- optParamFunctionR(sl = slR, cl = clR, ps = psR, pc = pcR, thetas = c(1,1))
  
  mu_TildeR <- params_mu_sR[1]
  s_TildeR <- params_mu_sR[2]
  mu_Tildes_PriorR[,i] <- mu_TildeR
  s_Tildes_PriorR[,i] <- s_TildeR
  
  K1tR  <- quantities_prices_capKR$K[i]
  k9R <- quantities_prices_capKR$k9[i]
  k8R <- quantities_prices_capKR$k8[i]
  k7R <- quantities_prices_capKR$k7[i]
  
  slDressedR <- quantities_prices_capKR$Slaughter_avg[i]
  clDressedR <- quantities_prices_capKR$Cull_avg[i]
  
  # adjFac <- quantities_prices_capK$AdjFactor[i]
  
  #### For imports and exports I will take the mean/median of the past data.
  # importsObs <- median(quantities_prices_capK$Imports[1:i])
  # exportsObs <- median(quantities_prices_capK$Exports[1:i])
  
  # importsObsR <- quantities_prices_capKR$Imports[i]
  # exportsObsR <- quantities_prices_capKR$Exports[i]
  
  hc_newR <- (((g * (beta^3) * psR) + (beta - 1) * pcR)/(1 + g * beta * (gamma0 + beta * gamma1)))
  hc_discountedR <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_newR
  
  Stock_1tR <- (K1tR*slDressed)/1000000000
  # imports <- (importsObs*slDressed)/1000000000
  # exports <- (exportsObs*slDressed)/1000000000
  k_9tR <- (k9R*clDressed)/1000000000
  k_8tR <- (k8R*clDressed)/1000000000
  k_7tR <- (k7R*clDressed)/1000000000
  
  ps_newR <- as.matrix(rep(psR,nrow(fed_cartesianR)), ncol = 1)
  pc_newR <- as.matrix(rep(pcR,nrow(cull_cartesianR)), ncol = 1)
  
  c_cullR <- solve(cullInterpolationMatrixR) %*% pc_newR
  c_fedR <- solve(fedCattleInterpolationMatrixR) %*% ps_newR
  
  ps_mR <- mean(ps_newR[,1])
  pc_mR <- mean(pc_newR[,1])
  
  countR <- 0
  c_old_cullR <- matrix(data = 0, nrow = nrow(cullInterpolationMatrixR), ncol = 1)
  c_old_fedR <- matrix(data = 0, nrow = nrow(fedCattleInterpolationMatrixR), ncol = 1)
  
  ps_oldR <- 0
  pc_oldR <- 0
  
  countR <- 0
  innerCountR <- 0
  
  eqIterR <- 100
  eqColsR <- 7
  
  equilibriumCheckR <- lapply(1:nrow(cull_cartesianR), matrix, data= 0, nrow = eqIterR, ncol = eqColsR)
  
  checkTolR <- matrix(data = 0, nrow = maxIterR, ncol = 4)
  
  for(k in 1:maxIterR){
    
    # k <- 1
    
    # if( norm(x = (c_cull - c_old_cull), type = "f") < 0.01 && norm(x = (c_fed - c_old_fed) , type = "f") < 0.01){
    #   if( (ps_m - ps_old)^2 < 0.001 && (pc_m - pc_old)^2 < 0.001){
    #     break
    #   }
    # }
    
    if( k > 1 ){
      if( round(checkTolR[k-1,1],3) < 0.01 && round(checkTolR[k-1,2],3) < 0.01){
        if( round(checkTolR[k-1,3],3) < 0.01 && round(checkTolR[k-1,4],3) < 0.01){
          break
        }
      }
    }
    
    
    # if( k > 50 ){
    #   break
    # }
    
    countR <- countR + 1
    
    c_old_cullR <- c_cullR
    c_old_fedR <- c_fedR
    
    ps_oldR <- ps_mR
    pc_oldR <- pc_mR
    
    for(j in 1:nrow(cull_cartesianR)){
      
      # j <- 5
      
      if(k == 1){
        
        cullCowNodeR <- cull_cartesianR$cullNodes[j]
        dShockNodeR <- cull_cartesianR$dShockNodes[j]
        fedCattleNodeR <- fed_cartesianR$fedNodes[j]
        cornNodeR <- fed_cartesianR$cornNodes[j]
        # aNode <- fed_cartesian$demandNodes[j]
        
        # slShare_t <- (exp((mu_Tilde - ((ps_old - pc_old))/phi)/s_Tilde))
        
        # sl_node <- (fedCattleNode) * adjFac
        # cl_node <- (cullCowNode) * adjFac
        
        # A_node <- (fedCattleNode + cullCowNode) * (dShockNode)
        adjFacR <- AR/(fedCattleNodeR+ cullCowNodeR)
        
        sl_nodeR <- (fedCattleNodeR) * adjFacR
        cl_nodeR <- (cullCowNodeR) * adjFacR
        
        A_nodeR <- AR  * dShockNodeR
        
        # A_node <- (fedCattleNode + cullCowNode) * (dShockNode)
        # sl_node <- (A_node * ((slShare_t)/(1 + slShare_t))) * adjFac
        # cl_node <- (A_node * (1/(1+slShare_t))) * adjFac
        
        
        # slD <- A_node * ((slShare_t)/(1 + slShare_t))
        # clD <- A_node * (1/(1 + slShare_t))
        
        A_nodesR[j,i] <- A_nodeR
        slNodesR[j,i] <- sl_nodeR
        clNodesR[j,i] <- cl_nodeR
        
        #### getting the parameters from the optParamFunction
        params_mu_sR <- optParamFunctionR(sl = sl_nodeR, cl = cl_nodeR, ps = ps_oldR, pc = pc_oldR, thetas = c(1,1))
        
        mu_TildeR <- params_mu_sR[1]
        s_TildeR <- params_mu_sR[2]
        
        mu_TildesR[j,i] <- mu_TildeR
        s_TildesR[j,i] <- s_TildeR
        
        if( ps_oldR < psR){
          ps_oR <- psR
        }else{
          ps_oR <- ps_oldR
        }
        
        if( pc_oldR < pcR){
          pc_oR <- pcR
        }else{
          pc_oR <- pc_oldR
        }
        
        # I am giving upper and lower bounds by looking at the historical prices (I look at the summary of difference between prices and the lagged price and use the min of those differences). 
        # Specifically for the lower bounds.
        # This is what my focus for setting the bounds for the prices. I don't want the program to choose some 
        # random arbitrary number which satisfies the system of equations. Note we can always find a number that
        # satisfies the system of equations. Our goal is to find the realistic solution and as realistic as possible.
        ps_loR <- ps_oR  - 0.269
        pc_loR <- pc_oR - 0.220
        
        # ps_upR <- ps_oR + 0.1
        # pc_upR <- pc_oR + 0.1
        
        ps_upR <- ps_oR + 0.2
        pc_upR <- pc_oR + 0.1
        
        #### Here we are making sure the lower bound for the prices isn't negative
        if(ps_loR < 0){
          ps_loR <- ps_oR
        }
        
        if(pc_loR < 0){
          pc_loR <- pc_oR
        }
        
        #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
        while( pc_loR > ps_loR ){
          pc_loR <- pc_loR - 0.01
        }
        
        ##### Gaussian quadrature for integration to get the expected price. The weights are pre determined.
        ps_expectedR <- sum(as.numeric(ps_oR) * fedMeshCheb)
        pc_expectedR <- sum(as.numeric(pc_oR) * cullMeshCheb)
        
        ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
        ### we use the euqlity of that to get the holding costs. From my first observation this is greater than the naive 
        ### expectations holding costs. Because we have the expected price in the equality.
        hc_newR <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expectedR + g * (beta^3) * ps_expectedR - pc_oR)
        # hc_newR <- hc_newR + cornNode/56
        
        #### Here we make sure that the holding costs are below the cull cow price
        while(hc_newR > pc_oR){
          hc_newR <- hc_newR - 0.01
        }
        
        hc_discountedR <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_newR
        BR <- ps_oR - g * (beta^3) * ps_expectedR + hc_discountedR ## Comes from the model
        
        ps_expected_loR <- ps_expectedR - 0.5
        
        ps_expected_upR <- ps_expectedR + 0.1
        
        pc_expected_loR <- pc_expectedR - 0.5
        
        pc_expected_upR <- pc_expectedR + 0.1
        
        if(pc_expected_loR < 0){
          pc_expected_loR <- pc_expectedR
        }
        
        if(ps_expected_loR < 0){
          ps_expected_loR <- ps_expectedR
        }
        
        pR <- c(ps_oR, pc_oR, ps_expectedR, pc_expectedR)
        
        loR <- c(ps_loR, pc_loR, ps_expected_loR, pc_expected_loR)
        upR <- c(ps_upR, pc_upR, ps_expected_upR, pc_expected_upR)
        
        estPR <- BBoptim(par = pR, fn = optPriceFunctionR, sl = sl_nodeR, cl = cl_nodeR, A = A_nodeR, 
                        B = BR, hc_discounted = hc_discountedR, lower = loR, upper = upR)
        
        ps1R <- estPR$par[1]
        pc1R <- estPR$par[2]
        ps_expected1R <- estPR$par[3]
        pc_expected1R <- estPR$par[4]
        
        prices_psR[j,i] <- ps1R
        prices_pcR[j,i] <- pc1R
        expected_PSR[j,i] <- ps_expected1R
        expected_PCR[j,i] <- pc_expected1R
        prices_hcR[j,i] <- hc_newR
        
      }
      
      if( k == 2 ){
        
        m <- 1
        
        cullCowNodeR <- cull_cartesianR$cullNodes[j]
        dShockNodeR <- cull_cartesianR$dShockNodes[j]
        fedCattleNodeR <- fed_cartesianR$fedNodes[j]
        cornNodeR <- fed_cartesianR$cornNodes[j]
        aNodeR <- fed_cartesianR$demandNodes[j]
        
        sl_nodeR <- slNodesR[j,i]
        cl_nodeR <- clNodesR[j,i]
        # A_node <- A_nodes[j,i]
        # A_node <- (sl_node + cl_node) * dShockNode
        A_nodeR <- A_nodesR[j,i]
        
        # A_nodes[j,i] <- A_node
        
        while(abs(round(fedDiffR[j,i],2))>0.01 || abs(round(cullDiffR[j,i],2))>0.01){
          
          if( fedDiffR[j,i] < 0){
            ps_nR <- prices_psR[j,i] + 0.1
          } else if( fedDiffR[j,i] > 0){
            ps_nR <- prices_psR[j,i] - 0.01
          }
          
          if(ps_nR < 0){
            ps_nR <- prices_psR[j,i]
          }
          
          if( cullDiffR[j,i] < 0){
            pc_nR <- prices_pcR[j,i] + 0.05
          } else if( cullDiff[j,i] > 0){
            pc_nR <- prices_pcR[j,i] - 0.01
          }
          
          if(pc_nR < 0){
            pc_nR <- prices_pcR[j,i]
          }
          
          ps_loR <- ps_nR  - 0.269
          pc_loR <- pc_nR - 0.220
          
          ps_upR <- ps_nR + 0.1
          pc_upR <- pc_nR + 0.1
          
          if(ps_loR < 0){
            ps_loR <- ps_nR
          }
          
          if(pc_loR < 0){
            pc_loR <- pc_nR
          }
          
          while(pc_loR > ps_loR){
            pc_loR <- pc_loR - 0.01
          }
          
          ps_expectedR <- expected_PSR[j,i]
          pc_expectedR <- expected_PCR[j,i]
          
          # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
          # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
          
          ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
          ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
          ### expectations holding costs. Because we have the expected price in the equality.
          hc_newR <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expectedR + g * (beta^3) * ps_expectedR - pc_nR)
          # hc_new <- hc_new + (cornNode/56)
          
          while(hc_newR > pc_nR){
            hc_newR <- hc_newR - 0.01
          }
          
          hc_discountedR <- ((1-beta^7)/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_newR
          BR <- ps_nR - g * (beta^3) * ps_expectedR + hc_discountedR
          
          ps_expected_loR <- ps_expectedR - 0.5
          
          ps_expected_upR <- ps_expectedR + 0.1
          
          pc_expected_loR <- pc_expectedR - 0.5
          
          pc_expected_upR <- pc_expectedR + 0.1
          
          if(pc_expected_loR < 0){
            pc_expected_loR <- pc_expectedR
          }
          
          if(ps_expected_loR < 0){
            ps_expected_loR <- ps_expectedR
          }
          
          pR <- c(ps_nR, pc_nR, ps_expectedR, pc_expectedR)
          
          loR <- c(ps_loR, pc_loR, ps_expected_loR, pc_expected_loR)
          upR <- c(ps_upR, pc_upR, ps_expected_upR, pc_expected_upR)
          
          params_mu_sR <- optParamFunctionR(sl = sl_nodeR, cl = cl_nodeR, 
                                          ps = ps_nR, pc = pc_nR, thetas = c(1,1))
          
          mu_TildeR <- params_mu_sR[1]
          s_TildeR <- params_mu_sR[2]
          
          mu_Tildes_eqR[j,i] <- mu_TildeR
          s_Tildes_eqR[j,i] <- s_TildeR
          
          estPR <- BBoptim(par = pR, fn = optPriceFunctionR, sl = sl_nodeR, cl = cl_nodeR, A = A_nodeR, B = BR, 
                          hc_discounted = hc_discountedR, lower = loR, upper = upR)
          
          ps1R <- estPR$par[1]
          pc1R <- estPR$par[2]
          ps_expected1R <- estPR$par[3]
          pc_expected1R <- estPR$par[4]
          
          prices_psR[j,i] <- ps1R
          prices_pcR[j,i] <- pc1R
          expected_PSR[j,i] <- ps_expected1R
          expected_PCR[j,i] <- pc_expected1R
          prices_hcR[j,i] <- hc_newR
          
          prices_ps_eqR[j,i] <- ps1R
          prices_pc_eqR[j,i] <- pc1R
          expected_PS_eqR[j,i] <- ps_expected1R
          expected_PC_eqR[j,i] <- pc_expected1R
          prices_hc_eqR[j,i] <- hc_newR
          
          ### Demand of the fed cattle meat under the new prices
          D_slPsPcR[j,i] <- A_nodeR *
            ((exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR))/
               (1 + (exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR))))
          
          ### Demand of the cull cow meat under the new prices
          D_clPsPcR[j,i] <- A_nodeR * (1/(1+ exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR)))
          
          ### Here we get the "optimal" supply of the meat by solving for k_{3,t+1} and k_{j,t+1} where j = [7,8,9]. Note we get
          ### these quantities separately i.e., k_{3,t+1} and sum k_{j,t+1} where j = [7,8,9]
          KR <- c(0,0)
          
          K_loR <- c(0,0)
          
          #### Here we use sl+cl for A_node. Why? because we use the current quantities i.e., Stock_1t and k_9t + k_8t + k_7t
          #### That means the total derived demand should be sl+cl? Dunno Have to think more.....
          estKR <- BBoptim(par = KR, fn = optKFunctionR, ps = ps1R, pc = pc1R, A = A_nodeR,
                           lower = K_loR)
          
          k_3t1R <- estKR$par[1]
          k_7_10t1R <- estKR$par[2]
          
          sl1R <- (g * Stock_1tR - k_3t1R) 
          cl1R <- (k_9tR + k_8tR + k_7tR - k_7_10t1R)
          
          slNodesR[j,i] <- sl1R
          clNodesR[j,i] <- cl1R
          
          A_nodesR[j,i] <- A_nodeR
          
          #### Total demand for the meat under new prices
          D_PsPcR <- as.matrix(D_slPsPcR[j,i] + D_clPsPcR[j,i])
          
          #### Total supply of meat (this is by adding the results)
          S_psPCR <- as.matrix(sl1R + cl1R)
          
          fedDiffR[j,i] <- (sl_nodeR - D_slPsPcR[j,i])
          cullDiffR[j,i] <- (cl_nodeR - D_clPsPcR[j,i])
          
          equilibriumCheckR[[j]][m,1] <-  fedDiffR[j,i]
          equilibriumCheckR[[j]][m,2] <-  cullDiffR[j,i] 
          equilibriumCheckR[[j]][m,3] <-  fedDiffR[j,i] + cullDiffR[j,i] 
          equilibriumCheckR[[j]][m,4] <- prices_psR[j,i]
          equilibriumCheckR[[j]][m,5] <- prices_pcR[j,i]
          equilibriumCheckR[[j]][m,6] <- expected_PSR[j,i]
          equilibriumCheckR[[j]][m,7] <- expected_PCR[j,i]
          
          slNodes_eqR[j,i] <- sl1R
          clNodes_eqR[j,i] <- cl1R
          A_nodes_eqR[j,i] <- A_nodeR
          
          ### Here we use the share of the cattle meat under new price as the supply of the corresponding meat in the next iteration
          if((m %% 2 == 0)){
            A_nodeR <- (sl1R + cl1R) * dShockNodeR
          }else{
            sl_nodeR <- sl1R
            cl_nodeR <- cl1R
          }
          
          m <- m+1
          
        }
        
      }
      
      if( k > 2){
        
        # cullCowNode <- cull_cartesian$cullNodes[j]
        # dShockNode <- cull_cartesian$dShockNodes[j]
        # fedCattleNode <- fed_cartesian$fedNodes[j]
        # 
        # sl_node <- fedCattleNode
        # cl_node <- cullCowNode
        # A_node <- (sl_node + cl_node) * dShockNode
        
        ps_nR <- prices_psR[j,i]
        pc_nR <- prices_pcR[j,i]
        
        ps_loR <- ps_nR  - 0.269
        pc_loR <- pc_nR - 0.220
        
        # ps_upR <- ps_nR + 0.1
        # pc_upR <- pc_nR + 0.1
        
        ps_upR <- ps_nR + 0.2
        pc_upR <- pc_nR + 0.1
        
        if(ps_loR < 0){
          ps_loR <- ps_nR
        }
        
        if(pc_loR < 0){
          pc_loR <- pc_nR
        }
        
        while(pc_loR > ps_loR){
          pc_loR <- pc_loR - 0.01
        }
        
        ps_expectedR <- expected_PSR[j,i]
        pc_expectedR <- expected_PCR[j,i]
        
        # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
        # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
        
        ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
        ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
        ### expectations holding costs. Because we have the expected price in the equality.
        hc_newR <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expectedR + g * (beta^3) * ps_expectedR - pc_nR)
        # hc_new <- hc_new + (cornNode/56)
        
        while( hc_newR > pc_nR ){
          hc_newR <- hc_newR - 0.01
        }
        
        hc_discountedR <- ((1-beta^7)/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_newR
        BR <- ps_nR - g * (beta^3) * ps_expectedR + hc_discountedR
        
        ps_expected_loR <- ps_expectedR - 0.5
        
        ps_expected_upR <- ps_expectedR + 0.1
        
        pc_expected_loR <- pc_expectedR - 0.5
        
        pc_expected_upR <- pc_expectedR + 0.1
        
        if(pc_expected_loR < 0){
          pc_expected_loR <- pc_expectedR
        }
        
        if(ps_expected_loR < 0){
          ps_expected_loR <- ps_expectedR
        }
        
        
        pR <- c(ps_nR, pc_nR, ps_expectedR, pc_expectedR)
        
        loR <- c(ps_loR, pc_loR, ps_expected_loR, pc_expected_loR)
        upR <- c(ps_upR, pc_upR, ps_expected_upR, pc_expected_upR)
        
        dShockNodeR <- cull_cartesianR$dShockNodes[j]
        
        sl_nodeR <- slNodesR[j,i]
        cl_nodeR <- clNodesR[j,i]
        A_nodeR <- A_nodesR[j,i]  * dShockNodeR
        
        params_mu_sR <- optParamFunctionR(sl = sl_nodeR, cl = cl_nodeR, 
                                        ps = ps_nR, pc = pc_nR, thetas = c(1,1))
        
        mu_TildeR <- params_mu_sR[1]
        s_TildeR <- params_mu_sR[2]
        
        mu_Tildes_itrR[j,i] <- mu_TildeR
        s_Tildes_itrR[j,i] <- s_TildeR
        
        estPR <- BBoptim(par = pR, fn = optPriceFunctionR, sl = sl_nodeR, cl = cl_nodeR, A = A_nodeR, B = BR, 
                        hc_discounted = hc_discountedR, lower = loR, upper = upR)
        
        ps1R <- estPR$par[1]
        pc1R <- estPR$par[2]
        ps_expected1R <- estPR$par[3]
        pc_expected1R <- estPR$par[4]
        
        prices_psR[j,i] <- ps1R
        prices_pcR[j,i] <- pc1R
        expected_PSR[j,i] <- ps_expected1R
        expected_PCR[j,i] <- pc_expected1R
        prices_hcR[j,i] <- hc_newR
        
        prices_ps_itrR[j,i] <- ps1R
        prices_pc_itrR[j,i] <- pc1R
        expected_PS_itrR[j,i] <- ps_expected1R
        expected_PC_itrR[j,i] <- pc_expected1R
        prices_hc_itrR[j,i] <- hc_newR
        
        ### Demand of the fed cattle meat under the new prices
        D_slPsPc_itrR[j,i] <- A_nodeR *
          ((exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR))/
             (1 + (exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR))))
        
        ### Demand of the cull cow meat under the new prices
        D_clPsPc_itrR[j,i] <- A_nodeR * (1/(1+ exp((mu_TildeR - ((ps1R/phi) - (pc1R/phi)))/s_TildeR)))
        
        slNodes_itrR[j,i] <- D_slPsPc_itrR[j,i]
        clNodes_itrR[j,i] <- D_clPsPc_itrR[j,i]
        
      }
      
      fedPriceR[[i]][j,k] <- ps1R
      cullPriceR[[i]][j,k] <- pc1R
      
      fedProdR[[i]][j,k] <- slNodesR[j,i]
      cullProdR[[i]][j,k] <- clNodesR[j,i]
      
    }
    
    if(k==1){
      ### Demand of the fed cattle meat under the new prices
      D_slPsPcR[,i] <- A_nodesR[,i] *
        ((exp((mu_TildesR[,i] - ((prices_psR[,i]/phi) - (prices_pcR[,i]/phi)))/s_TildesR[,i]))/
           (1 + (exp((mu_TildesR[,i] - ((prices_psR[,i]/phi) - (prices_pcR[,i]/phi)))/s_TildesR[,i]))))
      
      ### Demand of the cull cow meat under the new prices
      D_clPsPcR[,i] <- A_nodesR[,i] * (1/(1+ exp((mu_TildesR[,i] - ((prices_psR[,i]/phi) - (prices_pcR[,i]/phi)))/s_TildesR[,i])))
      
      #### Total demand for the meat under new prices
      D_PsPcR <- as.matrix(D_slPsPcR[,i] + D_clPsPcR[,i])
      
      #### Total supply of meat (this is by adding the nodes)
      S_psPCR <- as.matrix(slNodesR[,i] + clNodesR[,i])
      
      fedDiffR[,i] <- (slNodesR[,i] - D_slPsPcR[,i])
      cullDiffR[,i] <- (clNodesR[,i] - D_clPsPcR[,i])
      
      # slNodes[,i] <- D_slPsPc[,i]
      # clNodes[,i] <- D_clPsPc[,i]
      
      sdiffR <- fedDiffR[,i]
      cdiffR <- cullDiffR[,i]
    }
    
    c_fedR  <- solve(fedCattleInterpolationMatrixR) %*% prices_psR[,i]
    c_cullR <- solve(cullInterpolationMatrixR) %*% prices_pcR[,i]
    
    ps_mR <- mean(prices_psR[,i])
    pc_mR <- mean(prices_pcR[,i])
    
    ps_exp_mR <- mean(expected_PSR[,i])
    pc_exp_mR <- mean(expected_PCR[,i])
    
    fedDiff_mR <- mean(fedDiffR[,i])
    cullDiff_mR <- mean(cullDiffR[,i])
    
    c_cull_itrR[[i]][,k] <- c_cullR
    c_fed_itrR[[i]][,k] <- c_fedR
    
    
    cat("\n norm of old and new fed coefficients: ", norm(x = (c_fedR - c_old_fedR) , type = "f"))
    
    cat("\n norm of old and new cull coefficients: ", norm(x = (c_cullR - c_old_cullR) , type = "f"))
    
    cat("\n Squared difference between old and new mean fed cattle prices: ", (ps_mR - ps_oldR)^2)
    
    cat("\n Squared difference between old and new mean cull cattle prices: ", (pc_mR - pc_oldR)^2)
    
    checkTolR[k,1] <- norm(x = (c_fedR - c_old_fedR) , type = "f")
    checkTolR[k,2] <- norm(x = (c_cullR - c_old_cullR) , type = "f")
    checkTolR[k,3] <- (ps_mR - ps_oldR)^2
    checkTolR[k,4] <- (pc_mR - pc_oldR)^2
    
    if( k > 3 ){
      if( all(round(checkTolR[k-1,],1) == round(checkTolR[k,],1)) ){
        if( all(round(checkTolR[k-2,],1) == round(checkTolR[k-1,],1)) ){
          if( all(round(checkTolR[k-3,],1) == round(checkTolR[k-2,],1)) ){
            break
          }
        }
      }
      
    }
    
  }
  
}



# Estimated Equilibrium Parameters

# mu tildes
mu_Tildes_MeansR <- apply(unique(mu_Tildes_eqR[1:25,]), 2, mean)
mu_Tildes_MeansR <- mu_Tildes_MeansR %>% as.data.frame()
names(mu_Tildes_MeansR) <- "muMean"
mu_Tildes_MeansR <- mu_Tildes_MeansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

mu_Tildes_MediansR <- apply(unique(mu_Tildes_eqR[1:25,]), 2, median)
mu_Tildes_MediansR <- mu_Tildes_MediansR %>% as.data.frame()
names(mu_Tildes_MediansR) <- "muMedian"
mu_Tildes_MediansR <- mu_Tildes_MediansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

mu_Tildes_MMNR <- merge(mu_Tildes_MeansR, mu_Tildes_MediansR)

# s tildes
s_Tildes_MeansR <- apply(unique(s_Tildes_eqR[1:25,]), 2, mean)
s_Tildes_MeansR <- s_Tildes_MeansR %>% as.data.frame()
names(s_Tildes_MeansR) <- "sMean"
s_Tildes_MeansR <- s_Tildes_MeansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

s_Tildes_MediansR <- apply(unique(s_Tildes_eqR[1:25,]), 2, median)
s_Tildes_MediansR <- s_Tildes_MediansR %>% as.data.frame()
names(s_Tildes_MediansR) <- "sMedian"
s_Tildes_MediansR <- s_Tildes_MediansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

s_Tildes_MMNR <- merge(s_Tildes_MeansR, s_Tildes_MediansR)


merge(mu_Tildes_MMNR,s_Tildes_MMNR) %>% select(Year, muMedian, sMedian) %>% round(3)


###### Fitted Fed Cattle Equilibrium prices

EQprices_ps_MeansR <- apply(unique(prices_ps_eqR[1:25,]), 2, mean)
EQprices_ps_MeansR <- EQprices_ps_MeansR %>% as.data.frame()
names(EQprices_ps_MeansR) <- "psMean"
EQprices_ps_MeansR <- EQprices_ps_MeansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQprices_ps_MediansR <- apply(unique(prices_ps_eqR[1:25,]), 2, median)
EQprices_ps_MediansR <- EQprices_ps_MediansR %>% as.data.frame()
names(EQprices_ps_MediansR) <- "psMedian"
EQprices_ps_MediansR <- EQprices_ps_MediansR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQestPSNR <- merge(EQprices_ps_MeansR, EQprices_ps_MediansR)

EQestObsPSNR <- left_join(EQestPSNR,quantities_prices_capKR) %>% select(Year,psMean, psMedian, ps) %>% 
  mutate(errMean = (ps - psMean), errmedian = (ps - psMedian)) %>% round(5)

EQestObsPSNR

EQestObsPSNR_Err <- EQestObsPSNR %>% select(Year, psMedian, ps) %>% 
  mutate(eHat = ((ps-psMedian)/ps)* 100)

EQestObsPSNR_Err_Median <- median(abs(EQestObsPSNR_Err$eHat)) %>% round(2)
EQestObsPSNR_Err_Max <- max(abs(EQestObsPSNR_Err$eHat)) %>% round(2)


###### Fitted Cull Cow Equilibrium prices
EQprices_pc_MeansNR <- apply(unique(prices_pc_eqR[1:25,]), 2, mean)
EQprices_pc_MeansNR <- EQprices_pc_MeansNR %>% as.data.frame()
names(EQprices_pc_MeansNR) <- "pcMean"
EQprices_pc_MeansNR <- EQprices_pc_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQprices_pc_MediansNR <- apply(unique(prices_pc_eqR[1:25,]), 2, median)
EQprices_pc_MediansNR <- EQprices_pc_MediansNR %>% as.data.frame()
names(EQprices_pc_MediansNR) <- "pcMedian"
EQprices_pc_MediansNR <- EQprices_pc_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQestPCNR <- merge(EQprices_pc_MeansNR, EQprices_pc_MediansNR)

EQestObsPCNR <- left_join(EQestPCNR,quantities_prices_capKR) %>% select(Year,pcMean, pcMedian, pc) %>% 
  mutate(errMean = (pc - pcMean), errmedian = (pc - pcMedian)) %>% round(5)

EQestObsPCNR

EQestObsPCNR_Err <- EQestObsPCNR %>% select(Year, pcMedian, pc) %>% 
  mutate(eHat = ((pc-pcMedian)/pc)*100)

EQestObsPCNR_Err_Median <- median(abs(EQestObsPCNR_Err$eHat)) %>% round(2)
EQestObsPCNR_Err_Max <- max(abs(EQestObsPCNR_Err$eHat)) %>% round(2)


EQestObsPCNR_R2 <- EQestObsPCNR_Err %>% select(Year, pcMedian, pc) %>% 
  mutate(resSquared = (pcMedian-pc)^2, totSquared = (pc - mean(pc))^2)

EQestObsPCNR_sumSquaredRes <- sum(EQestObsPCNR_R2$resSquared)
EQestObsPCNR_sumSquaredTotal <- sum(EQestObsPCNR_R2$totSquared)

EQestObsPCNR_RSquared <- 1 -  (EQestObsPCNR_sumSquaredRes/EQestObsPCNR_sumSquaredTotal)



mergedPricesR <- merge(EQestObsPSNR %>% select(-errMean, -errmedian), 
                      EQestObsPCNR %>% select(-errMean, -errmedian)) %>% select(Year, ps, psMedian, 
                                                                                  pc, pcMedian)
mergedPricesR[,-1] <- mergedPricesR[,-1] * 100




###### Fitted Holding costs
EQcosts_hc_MeansNR <- apply(unique(prices_hc_eqR[1:25,]), 2, mean)
EQcosts_hc_MeansNR <- EQcosts_hc_MeansNR %>% as.data.frame()
names(EQcosts_hc_MeansNR) <- "hcMean"
EQcosts_hc_MeansNR <- EQcosts_hc_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQcosts_hc_MediansNR <- apply(unique(prices_hc_eqR[1:25,]), 2, median)
EQcosts_hc_MediansNR <- EQcosts_hc_MediansNR %>% as.data.frame()
names(EQcosts_hc_MediansNR) <- "hcMedian"
EQcosts_hc_MediansNR <- EQcosts_hc_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQestHCNR <- merge(EQcosts_hc_MeansNR, EQcosts_hc_MediansNR)

###### Fitted Expected Prices
EQprices_Eps_MeansNR <- apply(unique(expected_PS_eqR[1:25,]), 2, mean)
EQprices_Eps_MeansNR <- EQprices_Eps_MeansNR %>% as.data.frame()
names(EQprices_Eps_MeansNR) <- "EpsMean"
EQprices_Eps_MeansNR <- EQprices_Eps_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQprices_Eps_MediansNR <- apply(unique(expected_PS_eqR[1:25,]), 2, median)
EQprices_Eps_MediansNR <- EQprices_Eps_MediansNR %>% as.data.frame()
names(EQprices_Eps_MediansNR) <- "EpsMedian"
EQprices_Eps_MediansNR <- EQprices_Eps_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% select(Year, everything())

EQestEPSNR <- merge(EQprices_Eps_MeansNR, EQprices_Eps_MediansNR)


EQprices_Epc_MeansNR <- apply(unique(expected_PC_eqR[1:25,]), 2, mean)
EQprices_Epc_MeansNR <- EQprices_Epc_MeansNR %>% as.data.frame()
names(EQprices_Epc_MeansNR) <- "EpcMean"
EQprices_Epc_MeansNR <- EQprices_Epc_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% select(Year, everything())

EQprices_Epc_MediansNR <- apply(unique(expected_PC_eqR[1:25,]), 2, median)
EQprices_Epc_MediansNR <- EQprices_Epc_MediansNR %>% as.data.frame()
names(EQprices_Epc_MediansNR) <- "EpcMedian"
EQprices_Epc_MediansNR <- EQprices_Epc_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% select(Year, everything())

EQestEPCNR <- merge(EQprices_Epc_MeansNR, EQprices_Epc_MediansNR)


# Fitted Fed Cattle Equilibrium Supply
EQquantities_sl_MeansNR <- apply(unique(slNodes_eqR[1:25,]), 2, mean)
EQquantities_sl_MeansNR <- EQquantities_sl_MeansNR %>% as.data.frame()
names(EQquantities_sl_MeansNR) <- "slMean"
EQquantities_sl_MeansNR <- EQquantities_sl_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQquantities_sl_MediansNR <- apply(unique(slNodes_eqR[1:25,]), 2, median)
EQquantities_sl_MediansNR <- EQquantities_sl_MediansNR %>% as.data.frame()
names(EQquantities_sl_MediansNR) <- "slMedian"
EQquantities_sl_MediansNR <- EQquantities_sl_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQestSLNR <- merge(EQquantities_sl_MeansNR, EQquantities_sl_MediansNR)

supp_sl_MODELR <- supp_diss_adjR %>% select(Year, slSM = fedSlaughter_BillLb)



# supp_sl_MODEL <- obsEst_sl_Supply %>% select(Year, slSM = sl_obs)
# supp_sl_MODEL <- quantities_prices_capK %>% select(Year, slSM)

EQestObsSLNR <- left_join(EQestSLNR, supp_sl_MODELR) %>% select(Year, slMean, slMedian, slSM) %>% 
  mutate(errMean = (slSM - slMean), errmedian = (slSM - slMedian)) %>% round(4)

EQestObsSLNR 


EQestObsSLNR_Err <- EQestObsSLNR %>% select(Year, slMedian, slSM) %>% 
  mutate(eHat = ((slSM-slMedian)/slSM)*100)

EQestObsSLNR_Err_Median <- median(abs(EQestObsSLNR_Err$eHat)) %>% round(2)
EQestObsSLNR_Err_Max <- max(abs(EQestObsSLNR_Err$eHat)) %>% round(2)


EQestObsSLNR_R2 <- EQestObsSLNR_Err %>% select(Year, slMedian, slSM) %>% 
  mutate(resSquared = (slMedian-slSM)^2, totSquared = (slSM - mean(slSM))^2)

EQestObsSLNR_sumSquaredRes <- sum(EQestObsSLNR_R2$resSquared)
EQestObsSLNR_sumSquaredTotal <- sum(EQestObsSLNR_R2$totSquared)

EQestObsSLNR_RSquared <- 1 -  (EQestObsSLNR_sumSquaredRes/EQestObsSLNR_sumSquaredTotal)

EQestObsSLNR_DiagResPlot <- plot(EQestObsSLNR$slMedian, EQestObsSLNR$errmedian)


# Fitted Cull Cow Equilibrium Supply 
EQquantities_cl_MeansNR <- apply(unique(clNodes_eqR[1:25,]), 2, mean)
EQquantities_cl_MeansNR <- EQquantities_cl_MeansNR %>% as.data.frame()
names(EQquantities_cl_MeansNR) <- "clMean"
EQquantities_cl_MeansNR <- EQquantities_cl_MeansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQquantities_cl_MediansNR <- apply(unique(clNodes_eqR[1:25,]), 2, median)
EQquantities_cl_MediansNR <- EQquantities_cl_MediansNR %>% as.data.frame()
names(EQquantities_cl_MediansNR) <- "clMedian"
EQquantities_cl_MediansNR <- EQquantities_cl_MediansNR %>% mutate(Year = quantities_prices_capKR$Year) %>% 
  select(Year, everything())

EQestCLNR <- merge(EQquantities_cl_MeansNR, EQquantities_cl_MediansNR)

supp_cl_MODELR <- supp_diss_adjR %>% select(Year, clSM = cowsCulled_BillLb)

# supp_cl_MODEL <- obsEst_cl_Supply %>% select(Year, clSM = cl_obs)
# supp_cl_MODEL <- quantities_prices_capK %>% select(Year, clSM)

EQestObsCLNR <- left_join(EQestCLNR, supp_cl_MODELR) %>% select(Year, clMean, clMedian, clSM) %>% 
  mutate(errMean = (clSM - clMean), errmedian = (clSM - clMedian)) %>% round(4)
EQestObsCLNR

EQestObsCLNR_Err <- EQestObsCLNR %>% select(Year, clMedian, clSM) %>% 
  mutate(eHat = ((clSM-clMedian)/clSM)*100)

EQestObsCLNR_Err_Median <- median(abs(EQestObsCLNR_Err$eHat)) %>% round(2)
EQestObsCLNR_Err_Max <- max(abs(EQestObsCLNR_Err$eHat)) %>% round(2)


EQestObsCLNR_R2 <- EQestObsCLNR_Err %>% select(Year, clMedian, clSM) %>% filter(Year >= 1990) %>%
  mutate(resSquared = (clMedian-clSM)^2, totSquared = (clSM - mean(clSM))^2)

EQestObsCLNR_sumSquaredRes <- sum(EQestObsCLNR_R2$resSquared)
EQestObsCLNR_sumSquaredTotal <- sum(EQestObsCLNR_R2$totSquared)

EQestObsCLNR_RSquared <- 1 -  (EQestObsCLNR_sumSquaredRes/EQestObsCLNR_sumSquaredTotal)

EQestObsCLNR_DiagResPlot <- plot(EQestObsCLNR$clMedian, EQestObsCLNR$errmedian)


merge(EQestObsSLNR %>% select(-errMean, -errmedian), 
      EQestObsCLNR %>% select(-errMean, -errmedian)) %>% 
  select(Year, slSM, slMedian, clSM, clMedian) %>% round(4)


#### Fitted Total Equilibrium Supply 

EQestTSNR <- merge(EQestCLNR, EQestSLNR) %>% transmute(Year = Year, TSmean = slMean + clMean, 
                                                             TSmedian = slMedian + clMedian)

totalSupplyR <- supp_diss_adjR %>% transmute(Year = Year, TS = TotalSupply)

EQestObsTSNR <- left_join(EQestTSNR,totalSupplyR) %>% select(Year, TSmean, TSmedian, TS) %>% 
  mutate(errMean = (TS - TSmean), errmedian = (TS- TSmedian)) %>% round(4)

EQestObsTSNR %>% select(Year, TSmedian, TS) %>%
  select(Year, TS, TSmedian) %>% round(4)



####### Plotting the equilibrium solutions


EQestObsPSNR_plots <- EQestObsPSNR %>% select(Year, psMean, psMedian, ps) %>% 
  transmute(Year = Year, psMean = psMean * 100, psMedian = psMedian * 100, ps = ps * 100) %>% filter(Year>=1998) %>% round(2)


slaughter_plotMedianR <- EQestObsPSNR_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=ps, color = "Observed price"),size=0.75) + 
  geom_point(aes(y=ps, color = "Observed price"),shape=15,size=2) + 
  geom_line(aes(y=psMedian, color="Median fitted price"),size=0.75) +
  geom_point(aes(y = psMedian, color = "Median fitted price"),shape=16,size=2) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNR_plots$Year[1],EQestObsPSNR_plots$Year[nrow(EQestObsPSNR_plots)], by = 2))) +
  scale_y_continuous(name="Fed cattle price")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))



EQestObsPCNR_plots <- EQestObsPCNR %>% select(Year, pcMean, pcMedian, pc) %>% 
  transmute(Year = Year, pcMean = pcMean * 100, pcMedian = pcMedian * 100, pc = pc * 100) %>% filter(Year>=1998) %>% round(2)

cull_plotMedianR <- EQestObsPCNR_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=pc, color = "Observed price"),size=0.75) + 
  geom_point(aes(y=pc, color = "Observed price"),shape=15,size=2) + 
  geom_line(aes(y=pcMedian, color="Median fitted price"),size=0.75) +
  geom_point(aes(y = pcMedian, color = "Median fitted price"),shape=16,size=2) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPCNR_plots$Year[1],EQestObsPCNR_plots$Year[nrow(EQestObsPCNR_plots)], by = 2))) +
  scale_y_continuous(name="Cull Cow Price")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))



EQestObsSLNR_plots <- EQestObsSLNR %>% select(Year, slMean, slMedian, slSM) %>% filter(Year>=1998) %>% round(2)

dressedWeights_slR <- dressedWeights_sl_clR %>% select(Year, Slaughter_avg)

EQestObsSLNR_Head <- merge(EQestObsSLNR_plots, dressedWeights_slR) %>% mutate(slMedianHead = 
                                                           slMedian * (1000000000/Slaughter_avg), 
                                                           slSMHead = slSM * (1000000000/Slaughter_avg)) %>% select(Year, slMedianHead, slSMHead)

EQestObsSLNR_Head <- EQestObsSLNR_Head %>% mutate(slMedianHeadMill = slMedianHead/1000000, 
                                                  slSMHeadMill = slSMHead/1000000) %>% round(3) 

slSupply_plotMedianR <- EQestObsSLNR_Head %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=slSMHeadMill, color = "Observed supply"),size=0.75) + 
  geom_point(aes(y=slSMHeadMill, color = "Observed supply"),shape=15,size=2) + 
  geom_line(aes(y=slMedianHeadMill, color="Median fitted supply"),size=0.75) +
  geom_point(aes(y = slMedianHeadMill, color = "Median fitted supply"),shape=16,size=2) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsSLNR_Head$Year[1],EQestObsSLNR_Head$Year[nrow(EQestObsSLNR_Head)], by = 2))) +
  scale_y_continuous(name="Fed cattle supply (million head)")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))




EQestObsCLNR_plots <- EQestObsCLNR %>% select(Year, clMean, clMedian, clSM) %>% filter(Year>=1998) %>% round(2)

clSupply_plotMedianR <- EQestObsCLNR_plots %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y= clSM, color = "Observed supply"),size=0.75) + 
  geom_point(aes(y= clSM, color = "Observed supply"),shape=15,size=2) + 
  geom_line(aes(y= clMedian, color="Median fitted supply"),size=0.75) +
  geom_point(aes(y = clMedian, color = "Median fitted supply"),shape=16,size=2) + 
  theme_classic() + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPSNR_plots$Year[1],EQestObsPSNR_plots$Year[nrow(EQestObsPSNR_plots)], by = 2))) +
  scale_y_continuous(name="Cull cow supply (billion pounds)")+ theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA), text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12)) + 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))






dressedWeights_clR <- dressedWeights_sl_clR %>% select(Year, Cull_avg)
# EQestObsCLNI_Plots <- EQestObsCLNII %>% select(Year, clMedian)
EQestObsCLNR_Plots <- EQestObsCLNR %>% select(Year, clMedian)

EQestObsCLNR_Head <- merge(EQestObsCLNR_Plots, dressedWeights_clR) %>% mutate(clMedianHead = 
                                                                               clMedian * (1000000000/Cull_avg))

Stock_lessKR <- StockR %>% select(-K)

EQestObsCLNR_Head_Inventory <- left_join(Stock_lessKR, EQestObsCLNR_Head) %>% select(-clMedian, 
                                                                                    -Cull_avg, -k10)

EQestObsCLNR_Head_Inventory1 <- EQestObsCLNR_Head_Inventory %>% mutate(CLk_987 = clMedianHead + lead(k9,1) + lead(k8,1))
EQestObsCLNR_Head_Inventory1 <- EQestObsCLNR_Head_Inventory1 %>% na.exclude()
EQestObsCLNR_Head_Inventory11 <- EQestObsCLNR_Head_Inventory1 %>% select(-clMedianHead)
EQestObsCLNR_Head_Inventory11 <- EQestObsCLNR_Head_Inventory11 %>% 
  mutate(fitK = k3 + k4 + k5 + k6 + CLk_987) %>% select(Year, fitK)
totalInventoryR <- StockR %>% select(Year, K)
EQestObstotalInventoryR <- merge(totalInventoryR, EQestObsCLNR_Head_Inventory11) %>% 
  mutate(K = K/1000000, fitK = fitK/1000000) %>% filter(Year >= 1990)

invReplication_plotR <- EQestObstotalInventoryR %>% ggplot(aes(x=Year)) + 
  geom_line(aes(y=K,color="Observed Inventory"),size=0.75) +
  geom_point(aes(y=K,color="Observed Inventory"),shape=15,size=2) + 
  geom_line(aes(y=fitK,color="Fitted Inventory"),size=0.75) +
  geom_point(aes(y=fitK,color="Fitted Inventory"),shape=16,size=2) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObstotalInventory$Year[1],
                                  EQestObstotalInventory$Year[nrow(EQestObstotalInventory)], by = 2))) + 
  # geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_y_continuous(name="Million Head") + theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal",
        legend.background = element_rect(color = NA),text = element_text(size = 15)) +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), 
        axis.text.y = element_text(size=12))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")), panel.border = element_blank())+ 
  theme(axis.title.x = element_text(vjust=-0.5)) + theme(axis.title.y = element_text(vjust=1.5))+
  guides(color = guide_legend(override.aes=list(shape = c(16,15))))














