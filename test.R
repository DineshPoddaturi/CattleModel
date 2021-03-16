require(tidyverse)
require(reshape2)
require(readxl)
library(data.table)
require(nleqslv)
require(BB)
require(Metrics)
require(pracma)

# Parameters
beta <- 0.98
delta <- 0.95

gamma0 <- 0.90
gamma1 <- 0.95

g <- 0.97
phi <- 0.63


############# Cattle totals (these are numbers of total cattle in any year not age dist.) in 1000 head##########
cattle_totals <- read_excel("Data/New/CattleTotals_PSD.xlsx") %>% as.data.frame()

cattle_tot <- cattle_totals %>% select(-Attribute, -Commodity, -Country, -`Unit Description`)
cattle_tot <- data.frame(t(cattle_tot))
colnames(cattle_tot) <- cattle_totals[,2]
cattle_tot <- cattle_tot %>% mutate(Year = c(rownames(cattle_tot))) %>% select(Year, everything())
cattle_tot[,-1] <- cattle_tot[,-1] * 1000


### Prices are in $/CWT
cows_prices <- read_excel("Data/New/PricesReceived_Cattle.xlsx") %>% as.data.frame()
cows_prices <- cows_prices %>% select(Year, Period, Value)

steersHeifers_Prices <- read_excel("Data/New/PricesReceived_Steers_Heifers.xlsx") %>% as.data.frame()
steersHeifers_Prices <- steersHeifers_Prices %>% select(Year, Period, Value)

# ##### animals on feed######
# steers_onFeed <- read_excel("Data/Steers_onFeedInventory.xlsx") %>% as.data.frame()
# steers_onFeed <- steers_onFeed %>% select(Year, Period, State, Value)
# 
# heifers_onFeed <- read_excel("Data/Heifers_onFeedInventory.xlsx") %>% as.data.frame()
# heifers_onFeed <- heifers_onFeed %>% select(Year, Period, State, Value)
# 
# cattle_onFeed <- read_excel("Data/Cattle_onFeedInventory.xlsx") %>% as.data.frame()
# cattle_onFeed <- cattle_onFeed %>% select(Year, Period, State, Value)



# beef_demand <- read_excel("Data/BeefDemand.xlsx") %>% as.data.frame()



# ##### dressed weights ####
# dressed_weights <- read_excel("Data/DressedWeights.xlsx") %>% as.data.frame()
# dressed_weights <- dressed_weights[-c(1:2),]
# row.names(dressed_weights) <- 1:nrow(dressed_weights)
# dressed_weights <- dressed_weights %>% separate(col = Date, into = c("Period", "Year")) %>% select(Year,Period,everything())
# dressed_weights$Period <- toupper(dressed_weights$Period)
# dressed_weights$Year <- as.numeric(dressed_weights$Year)


############## converting all the dataframes above to quarterly data ##############

# cows_prices <- cows_prices %>% filter(Period %in% c("JAN","APR", "JUL","OCT"))

# steersHeifers_Prices <- steersHeifers_Prices %>% filter(Period %in% c("JAN", "APR", "JUL", "OCT"))

############################ converting the prices into yearly by taking the mean of the observed prices #########
pcs <- cows_prices %>% group_by(Year) %>% mutate(pc = mean(Value)/100) %>% select(Year,pc) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()
pss <- steersHeifers_Prices %>% group_by(Year) %>% mutate(ps = mean(Value)/100) %>% select(Year,ps) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()

pcs_cwt <- pcs %>% mutate(pcs_cwt = pc*100)
pss_cwt <- pss %>% mutate(pss_cwt = ps*100)
pc_ps_cwt <- merge(pcs_cwt, pss_cwt) %>% filter(Year>=1993) %>% select(Year,pss_cwt, pcs_cwt)

pc_ps <- merge(pcs,pss) %>% filter(Year>=1993)



################################## from ERS #############################################

### demand is in million pounds in carcass weight ##########
beefDemand <- read_excel("Data/New/BeefDemand.xlsx") %>% as.data.frame()
beefDemand <- beefDemand %>% separate(col= Quarter, into=c("Quarter","Period", "PeriodEnd")) %>% filter(Quarter == "Yr")
######### here we get the yearly demand for beef ##########
beefDemand <- beefDemand %>% select(Year, Commercial, Farm, `Beginning stocks`, Imports, Exports, `Ending stocks`, `Total supply`, `Total disappearance`) %>% filter(Year>=1994 & Year<2018)
######### Converting into billion pounds ##########
beefDemand[,-1] <- beefDemand[,-1]/1000

############################################################################################


####################################### from PSD ############################################

beefDemand <- read_excel("Data/New/BeefDemand_bkp.xlsx") %>% as.data.frame()
beefDemand <- beefDemand %>% select(-`Unit Description`, -Commodity, -Country)

demandBeef <- data.frame(t(beefDemand[-1]))
colnames(demandBeef) <- beefDemand[,1]
demandBeef <- demandBeef %>% mutate(Year = c(rownames(demandBeef)))
demandBeef <- demandBeef %>% select(Year,everything())

########## Changing the units (the original units are in 1000 MT)

demandBeef[,-1] <- (demandBeef[,-1] * 1000 * (2204.68))

########## Now converting meat to billion pounds ############
demandBeef[,-1] <- demandBeef[,-1]/1000000000



beefDemand <- demandBeef %>% mutate(Demand = `Beginning Stocks` + Production + Imports - Exports - `Ending Stocks`) %>% select(Year, Demand) %>% filter(Year>=1994 & Year<2018)


############################################################################################

######################### Here we read the number of animals slaughtered steers, heifers, and cows ##################
cowsSlaughtered <- read_excel("Data/New/CowsSlaughtered.xlsx") %>% as.data.frame()

cowsSlaughtered <- cowsSlaughtered %>% select(Year, Value) %>% mutate(CowsHead=Value) %>% select(Year, CowsHead) %>% arrange(Year) %>% filter(Year >= 1994)

heifersSlaughtered <- read_excel("Data/New/HeifersSlaughtered.xlsx") %>% as.data.frame()

heifersSlaughtered <- heifersSlaughtered %>% select(Year, Value) %>% mutate(HeifersHead=Value) %>% select(Year, HeifersHead) %>% arrange(Year)%>% filter(Year >= 1994)

steersSlaughtered <- read_excel("Data/New/SteersSlaughtered.xlsx") %>% as.data.frame()

steersSlaughtered <- steersSlaughtered %>% select(Year, Value) %>% mutate(SteersHead=Value) %>% select(Year, SteersHead) %>% arrange(Year) %>% filter(Year >= 1994)

################ dressed weights 
dressedWeights <- read_excel("Data/New/DressedWeights.xlsx") %>% as.data.frame()
dressedWeights <- dressedWeights[-c(1:2),]
row.names(dressedWeights) <- 1:nrow(dressedWeights)
dressedWeights <- dressedWeights %>% separate(col = Date, into = c("Period", "Year")) %>% select(Year,Period,everything())
dressedWeights$Period <- toupper(dressedWeights$Period)
dressedWeights$Year <- as.numeric(dressedWeights$Year)
dressedWeights <- dressedWeights %>% filter(Year >= 1994) %>% select(Year, Period, Cattle, Steers, Heifers, Cows)
######## computing the yearly dressed weights by taking the average of the observed #########
dressedWeights <- dressedWeights %>% group_by(Year) %>% mutate(Cattle_avg = mean(Cattle), Steers_avg = mean(Steers), Heifers_avg = mean(Heifers), Cows_avg = mean(Cows)) %>% select(Year, Cattle_avg, Steers_avg, Heifers_avg, Cows_avg) %>% group_by(Year) %>% distinct() %>% ungroup() %>% as.data.frame()
dressedWeights <- dressedWeights %>% arrange(Year)

dressedWeights <- dressedWeights %>% filter(Year<2020)

############ Here we convert the number of head to pounds in weight from the dressed weights data ###############

cowsSlaughtered <- cowsSlaughtered %>% mutate(cull_meat = (dressedWeights$Cows_avg)* CowsHead)
heifersSlaughtered <- heifersSlaughtered %>% mutate(heifer_meat = dressedWeights$Heifers_avg * HeifersHead)
steersSlaughtered <- steersSlaughtered %>% mutate(steer_meat = dressedWeights$Steers_avg * SteersHead)

totalDisappeared <- merge(cowsSlaughtered, merge(heifersSlaughtered,steersSlaughtered)) %>% mutate(total_meat = cull_meat + heifer_meat + steer_meat)

totalDisappeared <- totalDisappeared %>% mutate(total_meat_bill = total_meat/1000000000, cull_meat_bill = cull_meat/1000000000, 
                                                fed_meat_bill = (heifer_meat + steer_meat)/1000000000 )


####### for heifers and steers we take average of both for each year so we have one dressed weight for slaughtered animal###
dressedWeights_sl_cl <- dressedWeights %>% mutate(Slaughter_avg = Steers_avg, Cull_avg = Cows_avg)

dressedWeights_sl_cl <- dressedWeights_sl_cl %>% select(Year, Slaughter_avg, Cull_avg)




################## reading beef inventory (This is K in our model) ##################
beefInventory <- read_excel("Data/New/CowsBeefInventory.xlsx") %>% as.data.frame()
beefInventory <- beefInventory %>% select(Year, Period, Value) %>% mutate(K = Value) %>% select(Year,K)

K <- beefInventory
K <- K %>% arrange(Year) %>% filter(Year<2020)


#################### reading replacement heifers (this is k3 in our model) ##############
replacementInventory <- read_excel("Data/New/HeifersReplacementInventory.xlsx") %>% as.data.frame()
replacementInventory <- replacementInventory %>% select(Year, Period, Value) %>% mutate (k3 = Value) %>% select(Year, k3)

####### note that these replacement heifers are counted for next year in the population mechanics #####
k3 <- replacementInventory %>% select(Year,k3) %>% mutate(Year = Year + 1) %>% arrange(Year) %>% filter(Year<2020)

k4 <- delta * k3$k3 %>% as.data.frame()
names(k4) <- "k4"
k4 <- k4 %>% mutate(Year=k3$Year+1)%>% select(Year,k4) %>% arrange(Year) %>% filter(Year<2020)


k5 <- delta * k4$k4 %>% as.data.frame()
names(k5) <- "k5"
k5 <- k5 %>% mutate(Year=k4$Year+1) %>% select(Year,k5) %>% arrange(Year) %>% filter(Year<2020) 

k6 <- delta * k5$k5 %>% as.data.frame()
names(k6) <- "k6"
k6 <- k6 %>% mutate(Year = k5$Year+1) %>% select(Year,k6) %>% arrange(Year) %>% filter(Year<2020)

k7 <- delta * k6$k6 %>% as.data.frame()
names(k7) <- "k7"
k7 <- k7 %>% mutate(Year = k6$Year+1) %>% select(Year,k7) %>% arrange(Year) %>% filter(Year<2020)

k8 <- delta * k7$k7 %>% as.data.frame()
names(k8) <- "k8"
k8 <- k8 %>% mutate(Year = k7$Year+1) %>% select(Year,k8) %>% arrange(Year) %>% filter(Year<2020)

Stock <- merge(merge(merge(merge(merge(merge(K,k3,all=TRUE),k4, all=TRUE),k5,all=TRUE),k6,all=TRUE),k7,all=TRUE),k8,all=TRUE) %>% as.data.frame()

k9 <- Stock %>% filter(Year>1926) %>% select(K,k3,k4,k5,k6,k7,k8) %>% mutate(k9 = K -(k3+k4+k5+k6+k7+k8) ) %>% select(k9) %>% as.data.frame()
k9 <- k9 %>% mutate(Year = seq(from=1927, to=2019)) %>% select(Year,k9)
k9$Year <- as.numeric(k9$Year)

Stock <- merge(Stock,k9, all=TRUE)

k10 <- Stock %>% filter(Year>1927) %>% select(K,k3,k4,k5,k6,k7,k8,k9) %>% mutate(k10 = K -(k3+k4+k5+k6+k7+k8+k9) ) %>% select(k10) %>% round() %>% as.data.frame()
k10 <- k10 %>% mutate(Year = seq(from=1928, to=2019)) %>% select(Year,k10)

Stock <- merge(merge(Stock,k9,all=TRUE),k10,all=TRUE) %>% select(Year,K,k3,k4,k5,k6,k7,k8,k9,k10) %>% mutate(k9=round(k9))

Stock <- Stock %>% filter(Year>=1980 & Year<2019)

exports <- cattle_tot %>% select(Year, Exports) %>% filter(Year>=1980 & Year<2019)
imports <- cattle_tot %>% select(Year, Imports) %>% filter(Year>=1980 & Year<2019)



supp_sl <- NULL

for(i in 1:nrow(Stock)){
  if(Stock$Year[i]>=1981){
    supp_sl[i] <- g * Stock$K[i-1] - Stock$k3[i+1]  + imports$Imports[i] - exports$Exports[i]
  }
}

supp_sl <- supp_sl %>% na.omit() %>% as.data.frame() 
names(supp_sl) <- "Slaughter"
supp_sl <- supp_sl %>% mutate(Year = seq(from=1981, to = 2017)) %>% select(Year,everything()) %>% filter(Year>=1994)




# supply of the cull animals

supp_cl <- NULL

for(i in 1:nrow(Stock)){
  if(Stock$Year[i]>=1981){
    supp_cl[i] <-  Stock$k10[i] + (Stock$k9[i] - Stock$k10[i+1]) + (Stock$k8[i]-Stock$k9[i+1]) + (Stock$k7[i] - Stock$k8[i+1])
  }
  
}

supp_cl <- supp_cl %>% na.omit() %>% as.data.frame() 
names(supp_cl) <- "Cull"
supp_cl <- supp_cl %>% mutate(Year = seq(from=1981, to = 2017)) %>% select(Year,everything()) %>% filter(Year>=1994)


dressedWeights_sl_cl <- dressedWeights_sl_cl %>% filter(Year<=2017)

supp_sl <- supp_sl %>% mutate(Bill_meatLb_sl = (Slaughter*(dressedWeights_sl_cl$Slaughter_avg))/1000000000)
supp_cl <- supp_cl %>% mutate(Bill_meatLb_cl = (Cull*(dressedWeights_sl_cl$Cull_avg))/1000000000)



############## here we simply add the slaughtered and cull meat for each year to find the total supply ######

totalSupply <- (supp_sl %>% select(Bill_meatLb_sl)) + (supp_cl %>% select(Bill_meatLb_cl))
names(totalSupply) <- "TotalSupply"
totalSupply <- totalSupply %>% mutate(Year = supp_cl$Year) %>% select(Year,everything())

totalDisappearedNew <- totalDisappeared %>% filter(Year<2018) %>% select(Year, total_meat_bill)

supp_diss <- merge(totalDisappearedNew,totalSupply)

supp_diss$Year <- as.numeric(supp_diss$Year)


supp_diss %>% ggplot(aes(x=Year))+ geom_line(aes(y=total_meat_bill,color="Demand Slaughter")) + geom_point(aes(y=total_meat_bill,color="Demand Slaughter")) +
  geom_line(aes(y=TotalSupply, color="Supply")) + geom_point(aes(y=TotalSupply, color="Supply")) +
  labs(x="Year", y="Meat (in billion pounds)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))







################## Here we find the adjustment factor to adjust for demand and supply meat ###############
# Taking ratio of demand and supply

adjFactor <- (supp_diss%>%select(total_meat_bill)/supp_diss%>%select(TotalSupply))
names(adjFactor) <- "AdjFactor"
adjFactor <- adjFactor %>% mutate(Year = beefDemand$Year) %>% select(Year, everything())
adjFactor$Year <- as.numeric(adjFactor$Year)

adjFactor_Plot <- adjFactor %>% ggplot(aes(x=Year))+geom_line(aes(y=AdjFactor,color="Adjustment Factor"))+geom_point(aes(y=AdjFactor,color="Adjustment Factor")) +
  labs(x="Year", y="Adjustment Factor", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))


adjFactor_New <- adjFactor %>% mutate(Year = Year + 1) %>% select(Year, AdjFactor) %>% filter(Year<2018)

totalSupply_adj <- totalSupply %>% filter(Year>1994) %>% mutate(TotalSupply = TotalSupply * adjFactor_New$AdjFactor)
supp_sl_adj  <- supp_sl %>% filter(Year>1994) %>% mutate(Bill_meatLb_sl = Bill_meatLb_sl * adjFactor_New$AdjFactor)
supp_cl_adj <- supp_cl %>% filter(Year>1994) %>% mutate(Bill_meatLb_cl = Bill_meatLb_cl * adjFactor_New$AdjFactor)

demand_new <- supp_diss %>% select(Year,total_meat_bill)

names(demand_new) <- c("Year", "Demand")


### Ratio of slaughter supply  to total supply, in the model this is exp()/(1+exp())

sl_ratio <- (supp_sl_adj %>% select(Bill_meatLb_sl)) / (totalSupply_adj %>% select(TotalSupply))
names(sl_ratio) <- "SlaughterShare" 
sl_ratio <- sl_ratio %>% mutate(Year = totalSupply_adj$Year) %>% select(Year,everything())

### Ratio of cull supply to total supply or simpley 1-slaughtersupply, in the model this is 1/(1+exp())
cl_ratio <- sl_ratio
cl_ratio <- cl_ratio %>% mutate(CullShare = 1-SlaughterShare) %>% select(Year,CullShare)

## solving for the expression in inside the exponential

tildes <- log((1- cl_ratio$CullShare)/cl_ratio$CullShare) %>% as.data.frame()
names(tildes) <- "Tildes"

pc_ps <- pc_ps %>% filter(Year<2018)


########### We use the following loss function to estimate mu_tilde and s_tilde
lossfn <- function(theta,e,ps,pc){
  mu <- theta[1]
  s <- theta[2]
  
  v <- sum((e - ((( mu - ((ps-pc)/phi)))/s)))^2
  
  return(v)
}

theta0 <- c(1,1)

e <- tildes$Tildes %>% as.vector()
ps <- pc_ps %>% filter(Year>=1995) %>% select(ps)
pc <- pc_ps %>% filter(Year>=1995) %>% select(pc)


out <- BBoptim(par= theta0, fn = lossfn, e=e,ps=ps,pc=pc)

muTilde <- out$par[1]
sTilde <- out$par[2]

mu <- muTilde/phi
pStd <- (sTilde/phi) * (pi/sqrt(3))


#### We are in the case where the farmers cull the 9 year old cows

ps <- pc_ps$ps
pc <- pc_ps$pc

hc <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1))) %>% as.data.frame()
names(hc) <- "hc"

prices_costs <- cbind(ps,pc,hc) %>% mutate(Year=pc_ps$Year) %>% select(Year,everything())

########################## Plotting the supply of meat and the observed prices ###############################
sl_cl <- merge(supp_sl_adj, supp_cl_adj) %>% select(Year, Bill_meatLb_sl, Bill_meatLb_cl)
sl_cl_supp <- sl_cl %>% ggplot(aes(x=Year)) + geom_line(aes(y=Bill_meatLb_sl, color="Fed cattle meat")) + geom_point(aes(y=Bill_meatLb_sl, color="Fed cattle meat")) + geom_line(aes(y=Bill_meatLb_cl, color="Cull cattle meat")) + 
  geom_point(aes(y=Bill_meatLb_cl, color="Cull cattle meat")) + labs(x="Year", y="Meat in billion pounds", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks = c(seq(1995,2017)))

supp_demand <- merge(totalSupply_adj, demand_new) %>% select(Year, TotalSupply, Demand)
supp_demand_plot <- supp_demand %>% ggplot(aes(x=Year)) + geom_line(aes(y=TotalSupply, color="Total Supply"))  +geom_point(aes(y=TotalSupply, color="Total Supply")) + geom_line(aes(y=Demand, color="Total disapperance")) + 
  geom_point(aes(y=Demand, color="Total disapperance"))+ labs(x="Year", y="Meat in billion pounds", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks = c(seq(1995,2017)))

prices_costs_plot <- prices_costs %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Fed cattle price"))+geom_point(aes(y=ps,color="Fed cattle price"))+
  geom_line(aes(y=pc,color="Cull cattle price")) + geom_point(aes(y=pc,color="Cull cattle price"))+geom_line(aes(y=hc, color="Holding Costs")) +geom_point(aes(y=hc, color="Holding Costs"))+ 
  labs(x="Year", y="Prices and costs(\\$/pound)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1993,2017))) 
#################################



Stock_new <- Stock %>% filter(Year>=1994 & Year<2018)
supp_sl_new <- supp_sl_adj %>% select(Year, Bill_meatLb_sl)
supp_cl_new <- supp_cl_adj %>% select(Year, Bill_meatLb_cl)

# demand_new <- beefDemand %>% select(Year, `Total disappearance`) %>% mutate(Demand = `Total disappearance`) %>% select(Year, Demand)


# demand_new <- totalSupply %>% mutate(Demand = TotalSupply) %>% select(Year, Demand)


##### with the estimated mu_tilde and s_tilde we solve for optimal prices. Here we use Stock_new,prices_costs,demand_new to solve

sysEqs_9 <- function(p){
  ps <- p[1]
  pc <- p[2]
  h <- p[3]
  
  F1 <- sl - A * ((exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))/(1 + (exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))))
  
  F2 <- cl  - A * (1/(1+ exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde)))
  
  F3 <- ps * ( 1- ((g*(beta^3)) * ((1-beta^7)/(1-beta)) ) ) - (beta^7)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^7)/(1-beta))*h
  
  F = F1^2 + F2^2 + F3^2
  
  # F = F1^2 + F2^2 
  
  return(F)
}


masterData <- left_join(prices_costs,merge(supp_sl_new,merge(supp_cl_new,demand_new))) %>% filter(Year>1993)

len <- nrow(masterData)

out_optim <- data.frame(Year=masterData$Year+1,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))
out_bb <- data.frame(Year=masterData$Year+1,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))
out_nl <- data.frame(Year=masterData$Year+1,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))

for(i in 1:len){
  
  A <- masterData$Demand[i+1]
  
  
  sl <- masterData$Bill_meatLb_sl[i+1]
  cl <- masterData$Bill_meatLb_cl[i+1]
  
  ps <- masterData$ps[i]
  pc <- masterData$pc[i]
  hc <- masterData$hc[i]
  
  p <- c(ps,pc,hc)
  
  # p <- c(ps,pc)
  
  ## Here we use three different functions to solve the equations. Eventually we use BBoptim results
  
  # est_optim <- optim(par=p, fn = sysEqs_9)$par
  
  
  if(!is.na(A)){
    est_bb <- BBoptim(par=p, fn = sysEqs_9)$par
  }
  
  # est_nl <- nlm(p = p, f = sysEqs_9)$estimate
  
  
  
  # out_optim$ps_hat[i] <- est_optim[1]
  # out_optim$pc_hat[i] <- est_optim[2]
  # out_optim$hc_hat[i] <- est_optim[3]
  
  out_bb$ps_hat[i] <- est_bb[1]
  out_bb$pc_hat[i] <- est_bb[2]
  out_bb$hc_hat[i] <- est_bb[3]
  
  # out_nl$ps_hat[i] <- est_nl[1]
  # out_nl$pc_hat[i] <- est_nl[2]
  # out_nl$hc_hat[i] <- est_nl[3]
  
}



#### now we calculate the estimated prices, costs, slaughtered meat, culled meat from the model 

out_bb <- out_bb %>% filter(Year<2018)
A <- masterData %>% filter(Year>1994) %>% select(Demand)
est_sl_cl <- cbind(out_bb,A)

est_sl_cl <- est_sl_cl %>% mutate(sl_hat = Demand * ((exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde))/(1 + (exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde)))), 
                                  cl_hat = Demand * (1/(1 + (exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde)))) )



MasterPricesCosts <- merge(prices_costs,est_sl_cl) %>% as.data.frame() %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

MasterPricesCosts[,-1] <- round(MasterPricesCosts[,-1],2)


slaughter_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 

cull_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 

holding_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 



Master_sl_cl <- merge(est_sl_cl,merge(supp_sl_new,supp_cl_new)) %>% as.data.frame()  %>% select(Year,Bill_meatLb_sl, sl_hat, Bill_meatLb_cl, cl_hat)
names(Master_sl_cl) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl[,-1] <- round(Master_sl_cl[,-1],2) 
Master_sl_cl$Year <- as.numeric(Master_sl_cl$Year)

stock_slaughter <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))

stock_cull <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))






ssl <- detrend(as.matrix(MasterPricesCosts%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1995,2017))) %>% select(Year, everything())

slaughter_plot <- ssl %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 

cull_plot <- ssl %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 

holding_plot <- ssl %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017))) 










################## Here I am predicting the demand for meat using the estimated prices and costs and also the estimated parameters mu_tilde and s_tilde #####
Stock_temp <- Stock%>% filter(Year>1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)


predict_df <- cbind(Stock_temp$Year, Stock_temp$k9  ,Stock_temp$k8, Stock_temp$k7, Stock_temp$k6, MasterPricesCosts$ps_hat, 
                    MasterPricesCosts$pc_hat, dressedWeights_sl_cl %>% filter(Year>1994)%>% select(Cull_avg)) %>% as.data.frame()
names(predict_df) <- c("Year", "k9", "k8", "k7", "k6", "ps_hat", "pc_hat", "dressedWeight")

demand_predict <- NULL





for(i in 1:nrow(predict_df)){
  # K_t <- predict_df$K[i]
  # k_3_t2 <- predict_df$k3[i+2]
  # imports_t1 <- predict_df$imports[i+1]
  
  k6_t <- predict_df$k6[i]
  k7_t <- predict_df$k7[i]
  k8_t <- predict_df$k8[i]
  k9_t <- predict_df$k9[i]
  ps_hat_t1 <- predict_df$ps_hat[i]/100
  pc_hat_t1 <- predict_df$pc_hat[i]/100
  dressed_t <- predict_df$dressedWeight[i]
  # clShare <- predict_df$cullShare[i]
  share <- (exp((muTilde - ((ps_hat_t1 - pc_hat_t1))/phi)/sTilde))
  # if(share > max(cl_ratio$CullShare)){
  #   share <- 0
  # }
  
  demand_predict[i+1] <- delta * (k8_t + (1-delta) * (k7_t + k6_t) )* (dressed_t/1000000000) * (1 + share)
}

demand_predict <- demand_predict %>% as.data.frame() %>% drop_na()
names(demand_predict) <- "Demand_hat"
demand_predict <- demand_predict %>% mutate(Year = predict_df$Year+1) %>% select(Year, Demand_hat)

demandMerge <- merge(round(demand_new%>%filter(Year>1995),4), round(demand_predict,4))
demandMerge$Year <- as.numeric(demandMerge$Year)




demand_plot <- demandMerge %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=Demand_hat, color="Estimated"))+geom_point(aes(y=Demand_hat,color="Estimated")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1996,2017))) 



ddl <- detrend(as.matrix(demandMerge%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1996,2017))) %>% select(Year, everything())

ddl_plot <- ddl %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=Demand_hat, color="Estimate"))+geom_point(aes(y=Demand_hat,color="Estimate")) + 
  labs(x="Year", y="", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1995,2017))) 











Stock_temp <- Stock%>% filter(Year>1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, MasterPricesCosts$ps_hat, 
                    MasterPricesCosts$pc_hat, dressedWeights_sl_cl %>% filter(Year>1994)%>% select(Slaughter_avg)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "ps_hat", "pc_hat", "dressedWeight")


demand_predict <- NULL

for(i in 1:nrow(predict_df)){
  # K_t <- predict_df$K[i]
  # k_3_t2 <- predict_df$k3[i+2]
  # imports_t1 <- predict_df$imports[i+1]
  
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  imports_t1 <- predict_df$imports[i]
  ps_hat_t1 <- predict_df$ps_hat[i]/100
  pc_hat_t1 <- predict_df$pc_hat[i]/100
  dressed_t <- predict_df$dressedWeight[i]
  slShare_t <- (exp((muTilde - ((ps_hat_t1 - pc_hat_t1))/phi)/sTilde))
  
  demand_predict[i+1] <- (g * K_t - k3_t2 + imports_t1) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
}

demand_predict <- demand_predict %>% as.data.frame() %>% drop_na()
names(demand_predict) <- "Demand_hat"
demand_predict <- demand_predict %>% mutate(Year =seq(from=1996, to = 2016)) %>% select(Year, Demand_hat)
demandMerge <- merge(demand_new, demand_predict)
demandMerge$Year <- as.numeric(demandMerge$Year)

demand_plot <- demandMerge %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=Demand_hat, color="Estimated"))+geom_point(aes(y=Demand_hat,color="Estimated")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1995,2016))) 

ddl <- detrend(as.matrix(demandMerge%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1996,2016))) %>% select(Year, everything())

ddl_plot <- ddl %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=Demand_hat, color="Estimated"))+geom_point(aes(y=Demand_hat,color="Estimated")) + 
  labs(x="Year", y="", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1995,2016))) 





Master_sl_cl <- merge(est_sl_cl,merge(supp_sl_new,supp_cl_new)) %>% as.data.frame()  %>% select(Year,Bill_meatLb_sl, sl_hat, Bill_meatLb_cl, cl_hat)
names(Master_sl_cl) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl[,-1] <- round(Master_sl_cl[,-1],2) 
Master_sl_cl$Year <- as.numeric(Master_sl_cl$Year)



stock_slaughter <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))

stock_cull <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(1994,2017)))

















############################################################################################################

#### Compute mu_tilde and s_tilde
mu_s_tildes <- function(sl, cl, ps, pc, thetas){
  
  s <- sl
  c <- cl
  sl_share <- s/(s+c)
  cl_share <- 1-sl_share
  
  # print(c(sl_share, cl_share))
  
  
  tilde <- log((1-cl_share)/cl_share)
  
  theta0 <- thetas
  
  out <- BBoptim(par= theta0, fn = lossfn, e=tilde ,ps=ps, pc=pc)
  
  muTilde <- out$par[1]
  sTilde <- out$par[2]
  return(c(muTilde,sTilde))
}


sysEqs_solve <- function(p){
  ps <- p[1]
  pc <- p[2]
  h <- p[3]
  
  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  
  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F3 <- ps * ( 1- ((g*(beta^3)) * ((1-beta^7)/(1-beta)) ) ) - (beta^7)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^7)/(1-beta))*h
  
  F = F1^2 + F2^2 + F3^2
  
  # F = F1^2 + F2^2 
  
  return(F)
}





#### This is using cull animals
Stock_temp <- Stock %>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$k9  ,Stock_temp$k8, Stock_temp$k7, Stock_temp$k6, Stock_temp$k5,
                    Stock_temp$k4, Stock_temp$k3,
                    prices_costs %>% filter(Year>=1994) %>% select(ps), 
                    prices_costs %>% filter(Year>=1994) %>% select(pc), 
                    prices_costs %>% filter(Year>=1994) %>% select(hc), 
                    dressedWeights_sl_cl %>% filter(Year>1993)%>% select(Cull_avg),
                    dressedWeights_sl_cl %>% filter(Year>1993)%>% select(Slaughter_avg),
                    supp_sl %>% filter(Year>=1994) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994) %>% select(total_meat_bill),
                    imports_temp %>% select(Imports), exports_temp %>% select(Exports)) %>% as.data.frame()
names(predict_df) <- c("Year", "k9", "k8", "k7", "k6", "k5", "k4", "k3" ,"ps", "pc", "hc", 
                       "dressedWeightCL","dressedWeightSL", "sl", "cl", "Dissappear", "Imports", "Exports")

# k6_t <- predict_df %>% select(Year, k6)
# k7_t <- predict_df %>% select(Year, k7)
# k8_t <- predict_df %>% select(Year, k8)
# dressed_t <- predict_df %>% select(Year, dressedWeight)
# pricesCosts_t <- prices_costs %>% filter(Year>=1995)

demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))
prices_predict <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

for(i in 1:nrow(predict_df)-1){
    # K_t <- predict_df$K[i]
    # k_3_t2 <- predict_df$k3[i+2]
    # imports_t1 <- predict_df$imports[i+1]
    
    # i <- 24
    
    #### We use the current data to estimate the future demand first and use that demand to estimate 
    #### the future prices
    k3_t <- predict_df$k3[i]
    k4_t <- predict_df$k4[i]
    k5_t <- predict_df$k5[i]
    k6_t <- predict_df$k6[i]
    k7_t <- predict_df$k7[i]
    
    k8_t <- predict_df$k8[i]
    k8_t1 <- predict_df$k8[i+1]
    
    k9_t <- predict_df$k9[i]
    k9_t1 <- predict_df$k9[i+1]
    
    imports_t <- predict_df$Imports[i]
    exports_t <- predict_df$Exports[i]
    
    # sl_t <- predict_df$sl[i]
    # cl_t <- predict_df$cl[i]
    
    
    
    # if(i<=1){
    #   ps_t <- predict_df$ps[i]
    #   pc_t <- predict_df$pc[i]
    #   hc_t <- predict_df$hc[i]
    #   sl <- predict_df$sl[i]
    #   cl <- predict_df$cl[i]
    #   demand <- predict_df$Dissappear[i]
    #   adj <- demand/(sl+cl)
    #   params <- mu_s_tildes(sl=sl, cl=cl, ps = ps_t, pc = pc_t, thetas = c(1,1))
    #   
    #   # muTilde <- params[1]
    #   # sTilde <- params[2]
    #   tildes <- c(params[1],params[2])
    #   
    #   # share_t1 <- (exp((muTilde - ((ps_t - pc_t)/phi))/ (sTilde)))
    # }
    
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    dressed_tCL <- predict_df$dressedWeightCL[i]
    dressed_tSL <- predict_df$dressedWeightSL[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
    adj <- demand/(sl+cl)
    
    ### One year ahead
    
    share_t1 <- (exp((muTilde - ((ps_t - pc_t)/phi))/ (sTilde)))
    
    demand_t1_hat <- (k9_t + k7_t - k8_t1 + k8_t - k9_t1) * (dressed_tCL/1000000000) * (1 + share_t1)
    
    sl_t1_hat <-   (demand_t1_hat * ((share_t1)/(1 + share_t1)))* adj
    cl_t1_hat <- (demand_t1_hat * (1/(1+share_t1))) * adj
    
    
    # demand_t1_hat - (sl_t1_hat + cl_t1_hat)
    # predict_df$Dissappear[i+1] - (predict_df$sl[i+1] * adj +  predict_df$cl[i+1] * adj)
    
    
    
    
    params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
    parameters$mu_tilde[i] <- params_t1[1]
    parameters$s_tilde[i] <- params_t1[2]
    
    
    p <- c(ps_t, pc_t, hc_t)
    A <- demand_t1_hat
    sl <- sl_t1_hat
    cl <- cl_t1_hat
    mu_Tilde <- params_t1[1]
    s_Tilde <- params_t1[2]
    
    est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
    ps_hat_t1 <- est_bb[1]
    pc_hat_t1 <- est_bb[2]
    hc_hat_t1 <- est_bb[3]
    
    
    
    # share_t1 <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1)/phi))/ (params_t1[2])))
    # 
    # # demand_t1_hat <- delta * (k8_t + (1-delta) * (k7_t + k6_t) ) * (dressed_t/1000000000) * (1 + share_t1)
    # 
    # sl_t1_hat <- (demand_t1_hat * ((share_t1)/(1 + share_t1))) * adj
    # cl_t1_hat <- (demand_t1_hat * 1/(1+share_t1)) * adj
    # demand_t1_hat <- sl_t1_hat + cl_t1_hat
    
    
    
    prices_predict$ps_hat[i] <- ps_hat_t1
    prices_predict$pc_hat[i] <- pc_hat_t1
    prices_predict$hc_hat[i] <- hc_hat_t1
    demand_predict$demand_est[i] <- A
    demand_predict$sl_est[i] <- sl_t1_hat
    demand_predict$cl_est[i] <- cl_t1_hat
    
    
    # ps_t <- ps_hat_t1
    # pc_t <- pc_hat_t1
    # hc_t <- hc_hat_t1
    
    # demand <- A
    # adj <- demand/(sl+cl)
    # tildes <- c(params_t1[1], params_t1[2])
    
}

demandMerge_new <- merge(demand_new, demand_predict)  %>% filter(demand_est>0)%>% select(Year, Demand, demand_est)
demandMerge_new$Year <- as.numeric(demandMerge_new$Year)
demand_plot_new <- demandMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated"))+geom_point(aes(y=demand_est,color="Estimated")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_new$Year[1], demandMerge_new$Year[nrow(demandMerge_new)]))) 


pricesMerge_new <- merge(prices_predict,prices_costs) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot <- pricesMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)]))) 

cullPrices_plot <- pricesMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)]))) 

holdingCosts_plot <- pricesMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))



Master_sl_cl <- merge(demand_predict,merge(supp_sl_new,supp_cl_new)) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl[,-1] <- round(Master_sl_cl[,-1],2) 
Master_sl_cl$Year <- as.numeric(Master_sl_cl$Year)



stock_slaughter <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))

stock_cull <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))


parameters_new <- parameters %>% filter(mu_tilde>0)
parameter_mu <- parameters_new %>% ggplot(aes(x=Year)) + geom_line(aes(y=mu_tilde, color="mu_tilde")) + geom_point(aes(y=mu_tilde, color="mu_tilde"))+
  labs(x="Year", y="mu_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new$Year[1],parameters_new$Year[nrow(parameters_new)])))

parameter_s <- parameters_new %>% ggplot(aes(x=Year)) + geom_line(aes(y=s_tilde)) + geom_point(aes(y=s_tilde))+
  labs(x="Year", y="s_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new$Year[1],parameters_new$Year[nrow(parameters_new)])))




######################################################
## Here we use the slaughter animals numbers
Stock_temp <- Stock%>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=1994)%>% select(Slaughter_avg),
                    prices_costs%>%filter(Year>=1994)%>% select(ps), prices_costs%>%filter(Year>=1994)%>% select(pc),
                    prices_costs%>%filter(Year>=1994)%>% select(hc), supp_sl %>% filter(Year>=1994) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

# predict_df$pc <- predict_df$pc + 1
# predict_df$hc <- (((g * (beta^3) * predict_df$ps) + (beta - 1) * predict_df$pc)/(1 + g * beta * (gamma0 + beta * gamma1)))

demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))
adj_factor <- data.frame(Year = predict_df$Year+1, adj = numeric(nrow(predict_df)))
shares_slcl <- data.frame(Year = predict_df$Year+1, share_pre = numeric(nrow(predict_df)), 
                          share_post = numeric(nrow(predict_df)))


for(i in 1:(nrow(predict_df)-2)){
    
    # i <- 2
    K_t <- predict_df$K[i]
    k3_t2 <- predict_df$k3[i+2]
    # imports_t <- predict_df$imports[i]
    
    if(i<=1){
      ps_t <- predict_df$ps[i]
      pc_t <- predict_df$pc[i]
      hc_t <- predict_df$hc[i]
      sl <- predict_df$sl[i]
      cl <- predict_df$cl[i]
      demand <- predict_df$Dissappear[i]
    }
    
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    dressed_t <- predict_df$dressedWeight[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
    adj <- demand/(sl+cl)
    
    # if(adj>1){
    #   adj <- 1/adj
    # }
    
    
    
    
    adj_factor$adj[i] <- adj
    
    imports_t <- predict_df$imports[i]
    exports_t <- predict_df$exports[i]
    
    
    slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
    shares_slcl$share_pre[i] <- slShare_t
    
    demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t) 
    sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
    cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
    
    
    
    params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
    parameters$mu_tilde[i] <- params_t1[1]
    parameters$s_tilde[i] <- params_t1[2]
    
    
    # slShare_t <- (exp((params_t1[1] - ((ps_t - pc_t))/phi)/params_t1[2]))
    # 
    # demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
    # 
    # sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
    # cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
    
    
    
    p <- c(ps_t, pc_t, hc_t)
    sl <- sl_t1_hat
    cl <- cl_t1_hat
    A <- demand_t1_hat
    mu_Tilde <- params_t1[1]
    s_Tilde <- params_t1[2]
    
    
    est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
    ps_hat_t1 <- est_bb[1]
    pc_hat_t1 <- est_bb[2]
    hc_hat_t1 <- est_bb[3]
    
    slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
    shares_slcl$share_post[i] <- slShare_t
    
    # demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
    # sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
    # cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
    # demand_t1_hat <- sl_t1_hat + cl_t1_hat
    
    
    prices_predict$ps_hat[i] <- ps_hat_t1
    prices_predict$pc_hat[i] <- pc_hat_t1
    prices_predict$hc_hat[i] <- hc_hat_t1
    demand_predict$demand_est[i] <- demand_t1_hat
    demand_predict$sl_est[i] <- sl_t1_hat
    demand_predict$cl_est[i] <- cl_t1_hat
    
    # ps_t <- ps_hat_t1
    # pc_t <- pc_hat_t1
    # hc_t <- hc_hat_t1
    # demand <- A
    
}

demandMerge_new <- merge(demand_new, demand_predict) %>% select(Year, Demand, demand_est) %>% filter(demand_est>0)
demandMerge_new$Year <- as.numeric(demandMerge_new$Year)
demand_plot_new <- demandMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated"))+geom_point(aes(y=demand_est,color="Estimated")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_new$Year[1], demandMerge_new$Year[nrow(demandMerge_new)])))+
  theme(legend.position = c(0.7, 0.2))

# ddl <- detrend(as.matrix(demandMerge_new%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1995,2016))) %>% select(Year, everything())

# ddl_plot <- ddl %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated"))+geom_point(aes(y=demand_est,color="Estimated")) + 
#   labs(x="Year", y="", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1995,2016))) 



# prices_costs_tmp <- prices_costs
# 
# prices_costs_tmp$pc <- prices_costs_tmp$pc + 1
# prices_costs_tmp$hc <- (((g * (beta^3) * prices_costs_tmp$ps) + (beta - 1) * prices_costs_tmp$pc)/(1 + g * beta * (gamma0 + beta * gamma1)))

pricesMerge_new <- merge(prices_predict,prices_costs) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot <- pricesMerge_new   %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

cullPrices_plot <- pricesMerge_new %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7)) 

holdingCosts_plot <- pricesMerge_new   %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

Master_sl_cl <- merge(demand_predict,merge(supp_sl_new,supp_cl_new)) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl[,-1] <- round(Master_sl_cl[,-1],3) 
Master_sl_cl$Year <- as.numeric(Master_sl_cl$Year)

stock_slaughter <- Master_sl_cl   %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.9, 0.9)) 

stock_cull <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.8, 0.3))

# ddl <- detrend(as.matrix(Master_sl_cl%>%select(-Year)),tt='linear') %>% as.data.frame() %>% mutate(Year = c(seq(1995,2016))) %>% select(Year, everything())
# 
# ddl_plot <- ddl %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed"))+geom_line(aes(y=sl_hat, color="Estimated"))+geom_point(aes(y=sl_hat,color="Estimated")) + 
#   labs(x="Year", y="", colour = "") + geom_hline(yintercept=0, linetype="dashed", color = "black") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1995,2016)))



parameters_new <- parameters %>% filter(mu_tilde>0)
parameter_mu <- parameters_new %>% ggplot(aes(x=Year)) + geom_line(aes(y=mu_tilde, color="mu_tilde")) + geom_point(aes(y=mu_tilde, color="mu_tilde"))+
  labs(x="Year", y="mu_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new$Year[1],parameters_new$Year[nrow(parameters_new)])))

parameter_s <- parameters_new %>% ggplot(aes(x=Year)) + geom_line(aes(y=s_tilde)) + geom_point(aes(y=s_tilde))+
  labs(x="Year", y="s_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new$Year[1],parameters_new$Year[nrow(parameters_new)])))

###### For our projections we are using the Fed Cattle stock and their corresponding estimates

# First we need to come up with the cost increase when traceability is introduced.
# I am using the estimates from Balsi et. al. 2010. 

# Are we assuming the cow-calf producers bear all the costs? Because the animal ownership changes throughout it's life. If we assume
# the ID installed by the cow-calf producer stays until it reaches the market we can confidently say cow-calf producer incurs all the cost.
# Balsi et. al. 2010 estimated costs for different segments of the industry. Cow-calf, backgrounding, feedlot, auction, and packer.
# Shall we do that here in this paper too. If that were the case we need to get data of those operations as well. With the existing things in the model
# it would make the model very complex. 

# We have prices per head. Need to convert them to price per pound. So basically this is same for steers, heifers, and cows.
prices_costs_new <- prices_costs

# We use tagging costs from CostEstimates_SAV.Rmd. These costs are sum of all the costs (which are weighted average of costs for each capacity).
# See CostEstimates_SAV.Rmd for details.




##### Work on these again (I think the problem is with the cost per pound) ###########

taggingCosts <- round(total_Costs,3)

aggCosts <- (Stock_temp[,2] * taggingCosts) %>% as.data.frame()
names(aggCosts) <- "AggregateCosts"
aggCosts <- aggCosts %>% mutate(Year = Stock_temp$Year, AggregateCosts_mill = AggregateCosts/1000000) %>% select(Year, everything())

#Costs for each age in our data in million $
Stock_temp_costs <-  (Stock_temp[,-1] * taggingCosts)  %>% as.data.frame()

Stock_temp_costs_mill <- (Stock_temp_costs/1000000) %>% mutate(Year = Stock_temp$Year) %>% select(Year, everything())

costs_cl <- supp_cl %>% mutate(costs = Cull * taggingCosts, cost_perLb = costs/(Cull * dressedWeights_sl_cl$Cull_avg))
costs_sl <- supp_sl %>% mutate(costs = Slaughter * taggingCosts, cost_perLb = costs/(Slaughter * dressedWeights_sl_cl$Slaughter_avg))


### Remember the culled cattle stayed longer (In our case 9 years). So we have to account for that. Basically costs for 9 years.
### First work on the culled cattle. This might take some time.

# Here I am multiplying the costs by 9. This is because the cow stayed in the stock for 9 years increasing the costs.
costs_cl_9years <- costs_cl %>% mutate(costs_9years = Cull * taggingCosts * 9, cost_Lb_9years = costs_9years/(Cull * dressedWeights_sl_cl$Cull_avg)) %>% 
  select(Year, Cull, costs_9years, cost_Lb_9years)

### The same applies for the fed cattle. They are alive for two years. So costs for two years need to be included.
costs_sl_2years <- costs_sl %>% mutate(costs_2years = Slaughter * taggingCosts * 2, cost_Lb_2years = costs_2years/(Slaughter * dressedWeights_sl_cl$Slaughter_avg)) %>%
  select(Year, Slaughter, costs_2years, cost_Lb_2years)


# Now think about how to include these costs in the model. 
costs_sl_cl <- merge(costs_cl_9years, costs_sl_2years) %>% select(Year, cost_Lb_9years, cost_Lb_2years) %>% 
  mutate(cost_both = cost_Lb_9years + cost_Lb_2years)

# A simple increase in cost is increasing the fed cattle and cull cow prices. 
# I kept the supply and demand same as observed in that year. No change in it at all.

# Think about how you are going to change the quantities? 
# Are we going to keep the observed supply and demand as it is?
# Keep the same demand as this year but change the supply?
# We know the share depends on ps and pc, so if it changes the sl & cl supply changes as well. Think more about these scenarios


# I am building the added costs dataframe
# Change the costs from 2009. I take the year from the paper
cost_price_hat <- prices_predict %>% filter(ps_hat>0)


cost_price_addedCosts <- merge(cost_price_hat,costs_sl_cl) %>% select(Year, ps_hat, pc_hat, hc_hat, cost_both)

ccc <- cost_price_addedCosts %>% filter(Year>=2009) %>% mutate(hc_hat = hc_hat + cost_both) %>% select (-cost_both)

cost_price_addedCosts <- left_join(cost_price_addedCosts, ccc, by = "Year") %>% 
  mutate(ps_hat = ps_hat.x, pc_hat = pc_hat.x, hc_hat = ifelse(is.na(hc_hat.y), hc_hat.x, hc_hat.y)) %>% 
  select(Year, ps_hat, pc_hat, hc_hat)


# I will also add the costs to the observed prices and costs. I will try the predictions with the observed first and see how things turn out.
cost_price_obs <- merge(prices_costs,costs_sl_cl) %>% select(Year, ps, pc, hc, cost_both)

costs_TBAdded <- cost_price_obs %>% filter(Year>=2009) %>% mutate(hc = hc + cost_both) %>% select (-cost_both)

cost_price_addedCosts_obs_r <- left_join(cost_price_obs, costs_TBAdded, by = "Year") %>% 
  mutate(ps = ps.x, pc = pc.x, hc = ifelse(is.na(hc.y), hc.x, hc.y)) %>% 
  select(Year, ps, pc, hc)


########### Here I have estimated prices, demand, sl, and cl from the observed data. I also have the observed counterparts as well. 
prices_predict_est <- prices_predict %>% mutate(ps_est = ps_hat, pc_est = pc_hat, hc_est = hc_hat) %>% filter(ps_hat>0) %>% select(Year, ps_est, pc_est, hc_est)
demand_predict_est <- demand_predict %>% mutate(Demand_est = demand_est, sl_est = sl_est, cl_est = cl_est) %>% filter(Demand_est>0) %>% select(Year, Demand_est, sl_est, cl_est)

prices_costs_obs <- prices_costs
demand_obs <- merge(demand_new, merge(supp_sl_new,supp_cl_new)) %>% mutate(sl = Bill_meatLb_sl, cl = Bill_meatLb_cl) %>% select(Year, Demand, sl, cl)


# Now I have to include these new price and cost estimates to predict the prices.

##### Do the above again. Collect your thoughts. Think carefully. You can do it
# 1. Use observed prices with added costs and estimated sl, cl, demand
# 2. Use estimated prices with added costs and estimated sl, sl, demand
# 3. Use estimated prices with added costs and original sl, cl, demand
# 4. Use observed prices with added costs and original sl, cl, demand

# Case 4: Observed prices with added costs and original sl, cl, demand

cost_price_addedCosts_obs <- cost_price_addedCosts_obs_r

Stock_temp <- Stock%>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=1994  & Year<=2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts_obs %>% select(ps), cost_price_addedCosts_obs %>% select(pc),
                    cost_price_addedCosts_obs %>% select(hc), supp_sl %>% filter(Year>=1994 & Year<=2017) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994 & Year<=2017) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994 & Year<=2017) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

predict_df <- predict_df %>% filter(Year > 2008)
demand_predict_co4<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co4 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co4 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))
adj_co4 <- data.frame(Year = predict_df$Year + 1, adj = numeric(nrow(predict_df)), adj_2009 = numeric(nrow(predict_df)))
shares_co4 <- data.frame(Year = predict_df$Year + 1, shares = numeric(nrow(predict_df)), shares_2009 = numeric(nrow(predict_df)))


for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
    adj <- demand/(sl+cl)
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  adj_co4$adj[i] <- adj
  
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  shares_co4$shares[i] <- slShare_t
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co4$mu_tilde[i] <- params_t1[1]
  parameters_co4$s_tilde[i] <- params_t1[2]
  
  
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  # adj1 <- demand_t1_hat / (sl_t1_hat + cl_t1_hat)
  # adj_co4$adj_2009[i] <- adj1
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  shares_co4$shares_2009[i] <- slShare_t

  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  demand_t1_hat <- (sl_t1_hat + cl_t1_hat)
  
  
  
  prices_predict_co4$ps_hat[i] <- ps_hat_t1
  prices_predict_co4$pc_hat[i] <- pc_hat_t1
  prices_predict_co4$hc_hat[i] <- hc_hat_t1
  demand_predict_co4$demand_est[i] <- demand_t1_hat
  demand_predict_co4$sl_est[i] <- sl_t1_hat
  demand_predict_co4$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
  # adj <- demand_t1_hat / (sl_t1_hat + cl_t1_hat)
}

prices_predict_co4 <- prices_predict_co4 %>% filter(ps_hat>0)
demand_predict_co4 <- demand_predict_co4 %>% filter(demand_est>0) %>% mutate(Demand_hat = demand_est, sl_hat = sl_est, 
                                                                             cl_hat = cl_est) %>% select(Year, Demand_hat,
                                                                                                         sl_hat, cl_hat)


# prices_predict_co4_merge <- merge(prices_predict_co4, merge(prices_predict_est, prices_costs_obs))%>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
#          ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
#          ps = ps * 100, pc = pc * 100, hc = hc * 100) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat ,hc, hc_est, hc_hat)
# 
# demand_predict_co4_merge <- merge(demand_predict_co4, merge(demand_predict_est, demand_obs)) %>% 
#   select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)

prices_predict_co4_merge <- left_join(merge(prices_predict_est,prices_costs_obs), prices_predict_co4) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
         ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
         ps = ps * 100, pc = pc * 100, hc = hc * 100,) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])
demand_predict_co4_merge <- left_join(merge(demand_predict_est,demand_obs), demand_predict_co4) %>%
  select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])

# prices_predict_co4_merge1 <- merge(prices_predict_co4, prices_costs_obs) %>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100) %>%
#   select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat)
# demand_predict_co4_merge1 <- merge(demand_predict_co4, demand_obs)

prices_predict_co4_merge111 <- prices_predict_co4_merge %>% select(Year, ps_est, ps_hat, pc_est, pc_hat, hc_est, hc_hat) %>% filter(Year>2009)

prices_predict_co4_merge111 <- left_join(prices_predict_co4_merge %>% select(Year, ps, pc, hc), prices_predict_co4_merge111
                                         ) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat)

# prices_predict_co4_merge111$ps_est[-16:-22] <- NA
# prices_predict_co4_merge111$ps_hat[-16:-22] <- NA
# prices_predict_co4_merge111$pc_est[-16:-22] <- NA
# prices_predict_co4_merge111$pc_hat[-16:-22] <- NA
# prices_predict_co4_merge111$hc_est[-16:-22] <- NA
# prices_predict_co4_merge111$hc_hat[-16:-22] <- NA
# 
# demand_predict_co4_merge111 <- demand_predict_co4_merge
# demand_predict_co4_merge111$Demand_est[-16:-22] <- NA
# demand_predict_co4_merge111$Demand_hat[-16:-22] <- NA
# demand_predict_co4_merge111$sl_est[-16:-22] <- NA
# demand_predict_co4_merge111$sl_hat[-16:-22] <- NA
# demand_predict_co4_merge111$cl_est[-16:-22] <- NA
# demand_predict_co4_merge111$cl_hat[-16:-22] <- NA

slaughterPrices_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est,color="Model Estimate"))+
  geom_point(aes(y=ps_est,color="Model Estimate"))+ geom_line(aes(y=ps_hat, color="Estimate with added costs"))+
  geom_point(aes(y=ps_hat,color="Estimate with added costs")) + geom_line(aes(y=ps, color="Observed"))+
  geom_point(aes(y=ps,color="Observed")) + labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge$Year[1], prices_predict_co4_merge$Year[nrow(prices_predict_co4_merge)]))) 

cullPrices_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est,color="Model estimate"))+
  geom_point(aes(y=pc_est,color="Model estimate")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=pc_hat,color="Estimate with added costs")) + geom_line(aes(y=pc, color="Observed")) + 
  geom_point(aes(y=pc,color="Observed")) + labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge$Year[1], prices_predict_co4_merge$Year[nrow(prices_predict_co4_merge)]))) 

holdingCosts_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc_est,color="Model estimate"))+
  geom_point(aes(y=hc_est,color="Model estimate")) + geom_line(aes(y=hc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=hc_hat,color="Estimate with added costs")) + geom_line(aes(y=hc, color="Observed")) + 
  geom_point(aes(y=hc,color="Observed")) + labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge$Year[1], prices_predict_co4_merge$Year[nrow(prices_predict_co4_merge)])))


demand_predict_co4_merge111 <- demand_predict_co4_merge %>% select(Year, Demand_est, Demand_hat, sl_est, sl_hat, cl_est, cl_hat) %>% filter(Year>2009)

demand_predict_co4_merge111 <- left_join(demand_predict_co4_merge %>% select(Year, Demand , sl, cl), demand_predict_co4_merge111
                                         ) %>% select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)

stock_slaughter_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl_est,color="Model estimate"))+
  geom_point(aes(y=sl_est,color="Model estimate")) + geom_line(aes(y=sl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=sl_hat,color="Estimate with added costs")) + geom_line(aes(y=sl, color="Observed")) + 
  geom_point(aes(y=sl,color="Observed")) + labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))

stock_cull_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl_est,color="Model estimate"))+
  geom_point(aes(y=cl_est,color="Model estimate")) + geom_line(aes(y=cl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=cl_hat,color="Estimate with added costs")) + geom_line(aes(y=cl, color="Observed")) + 
  geom_point(aes(y=cl,color="Observed")) + labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))

demand_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand_est,color="Model estimate"))+
  geom_point(aes(y=Demand_est,color="Model estimate")) + geom_line(aes(y=Demand_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=Demand_hat,color="Estimate with added costs")) + geom_line(aes(y=Demand, color="Observed")) + 
  geom_point(aes(y=Demand,color="Observed")) + labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))



# slaughterPrices_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)]))) 
# 
# cullPrices_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)]))) 
# 
# holdingCosts_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)])))
# 
# 
# stock_slaughter_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))
# 
# stock_cull_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))
# 
# demand_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))


rev_sl <-  prices_predict_co4_merge %>% mutate(slRev_post = (ps_hat/100) * demand_predict_co4_merge$sl_hat,
                                               slRev_model = (ps_est/100) * demand_predict_co4_merge$sl_est,
                                               slRev_obs = (ps/100) * demand_predict_co4_merge$sl,
                                               slRev_diff_obs = slRev_post - slRev_obs,
                                               slRev_diff_model = slRev_post - slRev_model) %>% select(Year, slRev_post, slRev_model, 
                                                                                                       slRev_obs, slRev_diff_obs, slRev_diff_model)

rev_cl <- prices_predict_co4_merge %>% mutate(clRev_post = (pc_hat/100) * demand_predict_co4_merge$cl_hat,
                                              clRev_model = (pc_est/100) * demand_predict_co4_merge$cl_est,
                                              clRev_obs = (pc/100) * demand_predict_co4_merge$cl,
                                              clRev_diff_obs = clRev_post - clRev_obs,
                                              clRev_diff_model = clRev_post - clRev_model) %>% select(Year, clRev_post, clRev_model, 
                                                                                                      clRev_obs, clRev_diff_obs, clRev_diff_model)

rev_total <- merge(rev_sl, rev_cl) %>% mutate(totalRev_post = slRev_post + clRev_post, 
                                              totalRev_model = slRev_model + clRev_model,
                                              totalRev_obs = slRev_obs + clRev_obs,
                                              totalRev_diff_obs = totalRev_post - totalRev_obs,
                                              totalRev_diff_model = totalRev_post - totalRev_model) %>% select(Year, totalRev_post,
                                                                                                       totalRev_model, totalRev_obs,
                                                                                                       totalRev_diff_obs, totalRev_diff_model)
rev_total_2009 <- rev_total %>% filter(Year>2009)

# costs_cl_2009 <- costs_cl %>% mutate(costs_9years = Cull * taggingCosts * 9, 
#                                      cost_Lb_9years = costs_9years/(Cull * dressedWeights_sl_cl$Cull_avg), 
#                                      cost_cl_meat = Bill_meatLb_cl * cost_Lb_9years) %>% filter(Year>2009) %>% select(Year, cost_cl_meat)
# 
# costs_sl_2009 <- costs_sl %>% mutate(costs_2years = Slaughter * taggingCosts * 2, 
#                                      cost_Lb_2years = costs_2years/ (Slaughter * dressedWeights_sl_cl$Slaughter_avg), 
#                                      cost_sl_meat = Bill_meatLb_sl * cost_Lb_2years) %>% filter(Year>2009) %>% select(Year, cost_sl_meat)
# 
# 
# totalCosts_2009 <- merge(costs_cl_2009, costs_sl_2009) %>% mutate(costsBill_total = cost_cl_meat + cost_sl_meat)
# 
# costsRev_2009 <- merge(rev_total_2009, totalCosts_2009) %>% mutate(netRev = totalRev_post - costsBill_total, netRevDiff = totalRev_pre - netRev)
# 

rev_total_Plot <- rev_total %>% ggplot(aes(x=Year))+geom_line(aes(y=totalRev_post,color="Revenue estimate after adding costs (in Billion $)"))+
  geom_point(aes(y=totalRev_post,color="Revenue estimate after adding costs (in Billion $)")) +
  geom_line(aes(y=totalRev_model, color="Model revenue estimate (in Billion $)")) + 
  geom_point(aes(y=totalRev_model,color="Model revenue estimate (in Billion $)")) + 
  geom_line(aes(y=totalRev_obs, color="Observed revenue (in Billion $)")) + 
  geom_point(aes(y=totalRev_obs,color="Observed revenue (in Billion $)")) + 
  labs(x="Year", y="Revenue (in Billion $)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(rev_total$Year[1],rev_total$Year[nrow(rev_total)])))


percentChange_price <- prices_predict_co4_merge %>% mutate(percentChange_ps_obs = round(((ps_hat - ps)/ps)*100,3),
                                                           percentChange_ps_model = round(((ps_hat - ps_est)/ps_est)*100,3),
                                                           percentChange_pc_obs = round(((pc_hat - pc)/pc)*100,3), 
                                                           percentChange_pc_model = round(((pc_hat - pc_est)/pc_est)*100,3),
                                                           percentChange_hc_obs = round(((hc_hat - hc)/hc)*100,3),
                                                           percentChange_hc_model = round(((hc_hat - hc_est)/hc_est)*100,3)) %>% select(Year, percentChange_ps_obs, 
                                                                                                                     percentChange_ps_model, percentChange_pc_obs,
                                                                                                                     percentChange_pc_model, percentChange_hc_obs,
                                                                                                                    percentChange_hc_model) %>% filter(Year>2009)
percentChange_price_ps <- percentChange_price %>% select(Year, percentChange_ps_obs, percentChange_ps_model)
percentChange_price_pc <- percentChange_price %>% select(Year, percentChange_pc_obs, percentChange_pc_model)
percentChange_cost_hc <- percentChange_price %>% select(Year, percentChange_hc_obs, percentChange_hc_model)

percentChange_demand <- demand_predict_co4_merge %>% mutate(percentChange_demand_obs = round(((Demand_hat - Demand)/Demand)*100,3),
                                                            percentChange_demand_model = round(((Demand_hat - Demand_est)/Demand_est)*100,3),
                                                            percentChange_sl_obs = round(((sl_hat - sl)/sl)*100,3),
                                                            percentChange_sl_model = round(((sl_hat - sl_est)/sl_est)*100,3),
                                                            percentChange_cl_obs = round(((cl_hat - cl)/cl)*100,3),
                                                            percentChange_cl_model = round(((cl_hat - cl_est)/cl_est)*100,3)) %>% select(Year, percentChange_demand_obs,
                                                                                                                              percentChange_demand_model, percentChange_sl_obs, 
                                                                                                                              percentChange_sl_model, percentChange_cl_obs,
                                                                                                                              percentChange_cl_model) %>% filter(Year>2009)

percentChange_sl <- percentChange_demand %>% select(Year, percentChange_sl_obs, percentChange_sl_model)
percentChange_cl <- percentChange_demand %>% select(Year, percentChange_cl_obs, percentChange_cl_model)

percentChange_slRev <- rev_sl %>% mutate(percentChange_slRev_model = ((slRev_post - slRev_model)/slRev_model)*100,
                                         percentChange_slRev_obs = ((slRev_post - slRev_obs)/slRev_obs)*100
                                         ) %>% select(Year, percentChange_slRev_obs, percentChange_slRev_model) %>% filter(Year>2009)

percentChange_clRev <- rev_cl %>% mutate(percentChange_clRev_model = ((clRev_post - clRev_model)/clRev_model)*100,
                                         percentChange_clRev_obs = ((clRev_post - clRev_obs)/clRev_obs)*100
                                         ) %>% select(Year, percentChange_clRev_obs, percentChange_clRev_model) %>% filter(Year>2009)

percentChange_tRev <- rev_total %>% mutate(percentChange_tRev_model = ((totalRev_post - totalRev_model)/totalRev_model)*100,
                                           percentChange_tRev_obs = ((totalRev_post - totalRev_obs)/totalRev_obs)*100
                                           ) %>% select(Year, percentChange_tRev_obs, percentChange_tRev_model) %>% filter(Year>2009)


## Here we compute costs (only tagging costs not holding costs) for the supplied meat
costs_cl <- costs_cl_9years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_9years)
costs_sl <- costs_sl_2years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_2years)

costs_hc_cl <- prices_predict_co4_merge %>% mutate(hc_9years = (hc_hat/100)) %>% select(Year, hc_9years)


costsSupply_sl <- demand_predict_co4_merge %>% mutate(costSupply_sl_obs = sl * costs_sl$cost_Lb_2years,
                                                      costSupply_sl_model = sl_est * costs_sl$cost_Lb_2years
                                                      ) %>% select(Year, costSupply_sl_obs,  costSupply_sl_model)
costsSupply_cl <- demand_predict_co4_merge %>% mutate(costSupply_cl_obs = cl * costs_cl$cost_Lb_9years,
                                                      costSupply_cl_model = cl_est * costs_cl$cost_Lb_9years
                                                      ) %>% select(Year, costSupply_cl_obs, costSupply_cl_model)
costsSupply_t <- merge(costsSupply_sl, costsSupply_cl) %>% mutate(costSupply_t_obs = costSupply_sl_obs + costSupply_cl_obs,
                                                                  costSupply_t_model = costSupply_sl_model + costSupply_cl_model)

costsRevenues <- merge(rev_cl, merge(rev_sl, merge(rev_total,costsSupply_t))) %>% select(Year, clRev_diff_obs, clRev_diff_model,
                                                                                         costSupply_cl_obs, costSupply_cl_model, 
                                                                                         slRev_diff_obs, slRev_diff_model,
                                                                                         costSupply_sl_obs, costSupply_sl_model,
                                                                                         totalRev_diff_obs, totalRev_diff_model,
                                                                                         costSupply_t_obs, costSupply_t_model) %>% filter(Year>2009)

revDiff_costs_sl <- costsRevenues %>% select(Year, slRev_diff_obs, costSupply_sl_obs, slRev_diff_model, costSupply_sl_model)
revDiff_costs_cl <- costsRevenues %>% select(Year, clRev_diff_obs, costSupply_cl_obs, clRev_diff_model, costSupply_cl_model)
revDiff_costs_t <- costsRevenues %>% select(Year, totalRev_diff_obs, costSupply_t_obs, totalRev_diff_model, costSupply_t_model)

revDiff_costs_sl_pSurp <- revDiff_costs_sl %>% mutate(diffRevCost_sl_obs = slRev_diff_obs - costSupply_sl_obs, 
                                                diffRevCost_sl_model = slRev_diff_model - costSupply_sl_model) %>% select(
                                                  Year, diffRevCost_sl_obs, diffRevCost_sl_model)
revDiff_costs_cl_pSurp <- revDiff_costs_cl %>% mutate(diffRevCost_cl_obs = clRev_diff_obs - costSupply_cl_obs,
                                                diffRevCost_cl_model = clRev_diff_model - costSupply_cl_model) %>% select(
                                                  Year, diffRevCost_cl_obs, diffRevCost_cl_model)
revDiff_costs_t_pSurp <- revDiff_costs_t %>% mutate(diffRevCost_t_obs = round(totalRev_diff_obs - costSupply_t_obs,4),
                                              diffRevCost_t_model = round(totalRev_diff_model - costSupply_t_model,4)) %>% select(
                                                Year, diffRevCost_t_obs, diffRevCost_t_model)

######## Compute the above again. Note sl, cl, demand are not changing at all. This is because of small changes in the prices and costs.
####### If the changes in prices are very small the supply and demand wouldn't change much. But the costs are relatively high to the revenues.
###### Hence in aggregate the costs are greater than the change in revenue. This could be one way of explaining.

# Here we are using the observed data to estimate until the shock and then using the model estimates to project the impacts of the shock. I am changing prices and costs only.




#### Case 1:
# cost_price_addedCosts_obs_1_ps_pc <- cost_price_addedCosts_obs_r %>% select(Year, ps, pc)
# cost_price_addedCosts_obs_1_hc <- cost_price_addedCosts_obs_r %>% select(Year, hc) %>% filter(Year<=2009)
# 
# ccc_1 <- ccc %>% mutate(Year = Year,hc = hc_hat) %>% select(Year, hc) %>% filter(Year>2009)
# cost_price_addedCosts_obs_1_hc <- rbind(cost_price_addedCosts_obs_1_hc, ccc_1)
# 
# cost_price_addedCosts_obs_1 <- merge(cost_price_addedCosts_obs_1_ps_pc, cost_price_addedCosts_obs_1_hc)

##### Case 2:
# cost_price_addedCosts_obs_1_ps_pc_hc <- cost_price_addedCosts_obs_r %>% select(Year, ps, pc, hc) %>% filter(Year<=2009)
# ccc_1 <- ccc %>% mutate(Year = Year - 1, ps = ps_hat, pc = pc_hat, hc = hc_hat) %>% select(Year, ps, pc, hc) %>% filter(Year>2009)
# cost_price_addedCosts_obs_1 <- rbind(cost_price_addedCosts_obs_1_ps_pc_hc, ccc_1)
# # # # 
# Stock_temp <- Stock%>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])
# imports_temp <- imports %>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])
# exports_temp <- exports %>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])
# 
# predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
#                     dressedWeights_sl_cl %>% filter(Year>=1994  & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])%>% select(Slaughter_avg),
#                     cost_price_addedCosts_obs_1 %>% select(ps), cost_price_addedCosts_obs_1 %>% select(pc),
#                     cost_price_addedCosts_obs_1 %>% select(hc), supp_sl %>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)]) %>% select(Bill_meatLb_sl),
#                     supp_cl %>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)]) %>% select(Bill_meatLb_cl),
#                     totalDisappearedNew %>% filter(Year>=1994 & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)]) %>% select(total_meat_bill)) %>% as.data.frame()
# names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

#### Case 3:
cost_price_addedCosts_obs_1_ps_pc_hc <- cost_price_addedCosts_obs_r %>% select(Year, ps, pc, hc) %>% filter(Year<=2009)
ccc_1 <- ccc %>% mutate(Year = Year , ps = ps_hat, pc = pc_hat, hc = hc_hat) %>% select(Year, ps, pc, hc) %>% filter(Year>2009)
cost_price_addedCosts_obs_1 <- rbind(cost_price_addedCosts_obs_1_ps_pc_hc, ccc_1)

supp_sl_1 <- supp_sl %>% filter(Year<=2009) %>% mutate(sl = Bill_meatLb_sl) %>% select(Year, sl)
sl_1 <- demand_predict_est %>% mutate(Year = Year , sl = sl_est) %>% select(Year, sl) %>% filter(Year>2009)
supp_sl_1 <- rbind(supp_sl_1, sl_1)

supp_cl_1 <- supp_cl %>% filter(Year<=2009) %>% mutate(cl = Bill_meatLb_cl) %>% select(Year, cl)
cl_1 <- demand_predict_est %>% mutate(Year = Year , cl = cl_est) %>% select(Year, cl) %>% filter(Year>2009)
supp_cl_1 <- rbind(supp_cl_1, cl_1)

totalDisappearedNew_1 <- totalDisappearedNew %>% filter(Year >= min(supp_sl_1$Year) & Year<=2009) %>% mutate(Demand = total_meat_bill) %>% select(Year, Demand)
disappear_1 <- demand_predict_est %>% mutate(Year = Year, Demand = Demand_est) %>% select(Year, Demand) %>% filter(Year>2009)
totalDisappearedNew_1 <- rbind(totalDisappearedNew_1,disappear_1)

cost_price_addedCosts_obs_1 <- cost_price_addedCosts_obs_1 %>% filter(Year >= min(supp_sl_1$Year))

Stock_temp <- Stock %>% filter(Year>=min(supp_sl_1$Year) & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])
imports_temp <- imports %>% filter(Year>=min(supp_sl_1$Year) & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])
exports_temp <- exports %>% filter(Year>=min(supp_sl_1$Year) & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=min(supp_sl_1$Year)  & Year<=cost_price_addedCosts_obs_1$Year[nrow(cost_price_addedCosts_obs_1)])%>% select(Slaughter_avg),
                    cost_price_addedCosts_obs_1 %>% select(ps), cost_price_addedCosts_obs_1 %>% select(pc),
                    cost_price_addedCosts_obs_1 %>% select(hc), supp_sl_1 %>% select(sl), supp_cl_1 %>% select(cl),
                    totalDisappearedNew_1 %>% select(Demand)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

predict_df <- predict_df %>% filter(Year > 2008)

demand_predict_co4_1<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co4_1 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co4_1 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))
adj_co4_1 <- data.frame(Year = predict_df$Year+1, adj = numeric(nrow(predict_df)))
shares_co4_1 <- data.frame(Year = predict_df$Year+1, shares = numeric(nrow(predict_df)), shares_2009 = numeric(nrow(predict_df)))

# for(i in 1:(nrow(predict_df) - 2)){
for(i in 1:(nrow(predict_df))){
  # i <- 8
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  
  # if(is.na(k3_t2)){
  #   k3_t2 <- predict_df$k3[i]
  # }
  # k3_t2
  
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
    # adj <- demand/(sl+cl)
  }
  
  year_t <- predict_df$Year[i]
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  if(adj>1){
    adj <- 1/adj
  }
  
  
  adj_co4_1$adj[i] <- adj
  
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  shares_co4_1$shares[i] <- slShare_t
  
  if(year_t<=2009){
    shares_co4_1$shares[i] <- slShare_t
    demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
    sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
    cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  }else{
    demand_t1_hat <- demand
    sl_t1_hat <- sl
    cl_t1_hat <- cl
  }
  
  
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co4_1$mu_tilde[i] <- params_t1[1]
  parameters_co4_1$s_tilde[i] <- params_t1[2]
  
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  shares_co4_1$shares_2009[i] <- slShare_t
  
  if(year_t <= 2009){

    shares_co4_1$shares_2009[i] <- slShare_t
    sl_t1_hat <- demand_t1_hat * ((slShare_t)/(1 + slShare_t)) * adj
    cl_t1_hat <- demand_t1_hat * 1/(1+slShare_t)  * adj
    demand_t1_hat <- (sl_t1_hat + cl_t1_hat)

  }else{
    shares_co4_1$shares_2009[i] <- slShare_t

    sl_t1_hat <- demand_t1_hat * ((slShare_t)/(1 + slShare_t)) * adj
    cl_t1_hat <- demand_t1_hat * 1/(1+slShare_t) * adj
    demand_t1_hat <- sl_t1_hat + cl_t1_hat
  }
    
  
  
  
  prices_predict_co4_1$ps_hat[i] <- ps_hat_t1
  prices_predict_co4_1$pc_hat[i] <- pc_hat_t1
  prices_predict_co4_1$hc_hat[i] <- hc_hat_t1
  demand_predict_co4_1$demand_est[i] <- demand_t1_hat
  demand_predict_co4_1$sl_est[i] <- sl_t1_hat
  demand_predict_co4_1$cl_est[i] <- cl_t1_hat
  
  
  # adj <- demand_t1_hat/ (sl_t1_hat + cl_t1_hat)
  # print(adj)
  
}

prices_predict_co4_1 <- prices_predict_co4_1 %>% filter(ps_hat>0)
demand_predict_co4_1 <- demand_predict_co4_1 %>% filter(demand_est>0) %>% mutate(Demand_hat = demand_est,
                                                                                 sl_hat = sl_est,
                                                                                 cl_hat = cl_est) %>% select(Year, Demand_hat, sl_hat, cl_hat)


prices_predict_co4_merge_1 <- left_join(merge(prices_predict_est,prices_costs_obs), prices_predict_co4_1) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
         ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
         ps = ps * 100, pc = pc * 100, hc = hc * 100,) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])
demand_predict_co4_merge_1 <- left_join(merge(demand_predict_est,demand_obs), demand_predict_co4_1) %>%
  select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])


# prices_predict_co4_merge1_1 <- merge(prices_predict_co4_1, prices_costs_obs) %>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100) %>%
#   select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat)
# demand_predict_co4_merge1_1 <- merge(demand_predict_co4_1, demand_obs)

prices_predict_co4_merge_1_111 <- prices_predict_co4_merge_1 %>% select(Year, ps_est, ps_hat, pc_est, pc_hat, hc_est, hc_hat) %>% filter(Year>2009)

prices_predict_co4_merge_1_111 <- left_join(prices_predict_co4_merge_1 %>% select(Year, ps, pc, hc), prices_predict_co4_merge_1_111
                                            ) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat)

slaughterPrices_plot_co4_1 <- prices_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est,color="Model estimate"))+
  geom_point(aes(y=ps_est,color="Model estimate"))+ geom_line(aes(y=ps_hat, color="Estimate with added costs"))+
  geom_point(aes(y=ps_hat,color="Estimate with added costs")) + geom_line(aes(y=ps, color="Observed"))+
  geom_point(aes(y=ps,color="Observed")) + labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge_1$Year[1], prices_predict_co4_merge_1$Year[nrow(prices_predict_co4_merge_1)])))

cullPrices_plot_co4_1 <- prices_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est,color="Model estimate"))+
  geom_point(aes(y=pc_est,color="Model estimate")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=pc_hat,color="Estimate with added costs")) + geom_line(aes(y=pc, color="Observed")) + 
  geom_point(aes(y=pc,color="Observed")) + labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge_1$Year[1], prices_predict_co4_merge_1$Year[nrow(prices_predict_co4_merge_1)]))) 

holdingCosts_plot_co4_1 <- prices_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc_est,color="Model estimate"))+
  geom_point(aes(y=hc_est,color="Model estimate")) + geom_line(aes(y=hc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=hc_hat,color="Estimate with added costs")) + geom_line(aes(y=hc, color="Observed")) + 
  geom_point(aes(y=hc,color="Observed")) + labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge_1$Year[1], prices_predict_co4_merge_1$Year[nrow(prices_predict_co4_merge_1)])))


demand_predict_co4_merge_1_111 <- demand_predict_co4_merge_1 %>% select(Year, Demand_est, Demand_hat, sl_est, sl_hat, cl_est, cl_hat) %>% filter(Year>2009)

demand_predict_co4_merge_1_111 <- left_join(demand_predict_co4_merge_1 %>% select(Year, Demand, sl, cl), demand_predict_co4_merge_1_111
                                            ) %>% select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)



stock_slaughter_co4_1 <- demand_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+ geom_line(aes(y=sl_est,color="Model estimate"))+
  geom_point(aes(y=sl_est,color="Model estimate")) + geom_line(aes(y=sl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=sl_hat,color="Estimate with added costs")) + geom_line(aes(y=sl, color="Observed")) + 
  geom_point(aes(y=sl,color="Observed")) + labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge_1$Year[1],demand_predict_co4_merge_1$Year[nrow(demand_predict_co4_merge_1)])))

stock_cull_co4_1 <- demand_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+ geom_line(aes(y=cl_est,color="Model estimate"))+
  geom_point(aes(y=cl_est,color="Model estimate")) + geom_line(aes(y=cl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=cl_hat,color="Estimate with added costs")) + geom_line(aes(y=cl, color="Observed")) + 
  geom_point(aes(y=cl,color="Observed")) + labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge_1$Year[1],demand_predict_co4_merge_1$Year[nrow(demand_predict_co4_merge_1)])))

demand_co4_1 <- demand_predict_co4_merge_1_111 %>% ggplot(aes(x=Year))+ geom_line(aes(y=Demand_est,color="Model estimate"))+
  geom_point(aes(y=Demand_est,color="Model estimate")) + geom_line(aes(y=Demand_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=Demand_hat,color="Estimate with added costs")) + geom_line(aes(y=Demand, color="Observed")) + 
  geom_point(aes(y=Demand,color="Observed")) + labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge_1$Year[1],demand_predict_co4_merge_1$Year[nrow(demand_predict_co4_merge_1)])))




rev_sl_1 <-  prices_predict_co4_merge_1 %>% mutate(slRev_post = (ps_hat/100) * demand_predict_co4_merge_1$sl_hat,
                                               slRev_model = (ps_est/100) * demand_predict_co4_merge_1$sl_est,
                                               slRev_obs = (ps/100) * demand_predict_co4_merge_1$sl,
                                               slRev_diff_model = slRev_post - slRev_model,
                                               slRev_diff_obs = slRev_post - slRev_obs) %>% select(Year, slRev_post, slRev_model, 
                                                                                                   slRev_obs, slRev_diff_model,
                                                                                                   slRev_diff_obs)


rev_cl_1 <- prices_predict_co4_merge_1 %>% mutate(clRev_post = (pc_hat/100) * demand_predict_co4_merge_1$cl_hat,
                                              clRev_model = (pc_est/100) * demand_predict_co4_merge_1$cl_est,
                                              clRev_obs = (pc/100) * demand_predict_co4_merge_1$cl,
                                              clRev_diff_model = clRev_post - clRev_model,
                                              clRev_diff_obs = clRev_post - clRev_obs) %>% select(Year, clRev_post, 
                                                                                                  clRev_model, clRev_obs,
                                                                                                  clRev_diff_model, clRev_diff_obs)


rev_total_1 <- merge(rev_sl_1, rev_cl_1) %>% mutate(totalRev_post = slRev_post + clRev_post, 
                                              totalRev_model = slRev_model + clRev_model,
                                              totalRev_obs = slRev_obs + clRev_obs,
                                              totalRev_diff_model = totalRev_post - totalRev_model,
                                              totalRev_diff_obs = totalRev_post - totalRev_obs) %>% select(Year, totalRev_post,
                                                                                                       totalRev_model, totalRev_obs, 
                                                                                                       totalRev_diff_model, totalRev_diff_obs)

rev_total_Plot_1 <- rev_total_1 %>% ggplot(aes(x=Year))+geom_line(aes(y=totalRev_obs,color="Observed revenue (in Billion $)"))+
  geom_point(aes(y=totalRev_obs,color="Observed revenue (in Billion $)")) + geom_line(aes(y=totalRev_model, color="Model revenue estimate (in Billion $)")) + 
  geom_point(aes(y=totalRev_model,color="Model revenue estimate (in Billion $)")) + geom_line(aes(y=totalRev_post, color="Revenue estimate after adding costs (in Billion $)")) + 
  geom_point(aes(y=totalRev_post,color="Revenue estimate after adding costs (in Billion $)")) + 
  labs(x="Year", y="Revenue (in Billion $)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(rev_total$Year[1],rev_total$Year[nrow(rev_total)])))

### Here I compute the percent changes in prices and quantities
percentChange_price_1 <- prices_predict_co4_merge_1 %>% mutate(percentChange_ps_model = round(((ps_hat - ps_est)/ps_est)*100,3),
                                                               percentChange_ps_obs = round(((ps_hat - ps)/ps)*100,3),
                                                               percentChange_pc_model = round(((pc_hat - pc_est)/pc_est)*100,3),
                                                               percentChange_pc_obs = round(((pc_hat - pc)/pc)*100,3),
                                                               percentChange_hc_model = round(((hc_hat - hc_est)/hc_est)*100,3),
                                                               percentChange_hc_obs = round(((hc_hat - hc)/hc)*100,3)) %>% select(
                                                                 Year, percentChange_ps_model, percentChange_ps_obs,
                                                                 percentChange_pc_model, percentChange_pc_obs, 
                                                                 percentChange_hc_model, percentChange_hc_obs) %>% filter(Year>2009)

percentChange_price_1_ps <- percentChange_price_1 %>% select(Year, percentChange_ps_obs, percentChange_ps_model)
percentChange_price_1_pc <- percentChange_price_1 %>% select(Year, percentChange_pc_obs, percentChange_pc_model)
percentChange_cost_1_hc <- percentChange_price_1 %>% select(Year, percentChange_hc_obs, percentChange_hc_model)


percentChange_demand_1 <- demand_predict_co4_merge_1 %>% mutate(percentChange_demand_model = round(((Demand_hat - Demand_est)/Demand_est)*100,3),
                                                                percentChange_demand_obs = round(((Demand_hat - Demand)/Demand)*100,3),
                                                                percentChange_sl_model = round(((sl_hat - sl_est)/sl_est)*100,3),
                                                                percentChange_sl_obs = round(((sl_hat - sl)/sl)*100,3),
                                                                percentChange_cl_model = round(((cl_hat - cl_est)/cl_est)*100,3),
                                                                percentChange_cl_obs = round(((cl_hat - cl)/cl)*100,3)) %>% select(
                                                                  Year, percentChange_demand_model, percentChange_demand_obs,
                                                                  percentChange_sl_model, percentChange_sl_obs,
                                                                  percentChange_cl_model, percentChange_cl_obs) %>% filter(Year>2009)

percentChange_sl_1 <- percentChange_demand_1 %>% select(Year, percentChange_sl_obs, percentChange_sl_model)
percentChange_cl_1 <- percentChange_demand_1 %>% select(Year, percentChange_cl_obs, percentChange_cl_model)


percentChange_slRev_1 <- rev_sl_1 %>% mutate(percentChange_slRev_model = ((slRev_post - slRev_model)/slRev_model)*100,
                                           percentChange_slRev_obs = ((slRev_post - slRev_obs)/slRev_obs)*100) %>% select(
                                             Year, percentChange_slRev_obs, percentChange_slRev_model) %>% filter(Year>2009)

percentChange_clRev_1 <- rev_cl_1 %>% mutate(percentChange_clRev_model = ((clRev_post - clRev_model)/clRev_model)*100,
                                           percentChange_clRev_obs = ((clRev_post - clRev_obs)/clRev_obs)*100
                                           ) %>% select(Year, percentChange_clRev_obs, percentChange_clRev_model) %>% filter(Year>2009)

percentChange_tRev_1 <- rev_total_1 %>% mutate(percentChange_tRev_model = ((totalRev_post - totalRev_model)/totalRev_model)*100,
                                             percentChange_tRev_obs = ((totalRev_post - totalRev_obs)/totalRev_obs)*100
                                             ) %>% select(Year, percentChange_tRev_obs, percentChange_tRev_model) %>% filter(Year>2009)


## Here we compute costs (only tagging costs not holding costs) for the supplied meat
costs_cl_1 <- costs_cl_9years %>% filter(Year>1994 & Year<=demand_predict_co4_merge_1$Year[nrow(demand_predict_co4_merge_1)]) %>% select(Year, cost_Lb_9years)
costs_sl_1 <- costs_sl_2years %>% filter(Year>1994 & Year<=demand_predict_co4_merge_1$Year[nrow(demand_predict_co4_merge_1)]) %>% select(Year, cost_Lb_2years)

# costs_hc_cl_pre_1 <- prices_predict_co4_merge_1 %>% mutate(hc_9years = hc/100) %>% select(Year, hc_9years)
# costs_hc_cl_post_1 <- prices_predict_co4_merge_1 %>% mutate(hc_9years = (hc_hat/100)) %>% select(Year, hc_9years)


costsSupply_sl_1 <- demand_predict_co4_merge_1 %>% mutate(costSupply_sl_obs = sl * costs_sl_1$cost_Lb_2years,
                                                          costSupply_sl_model = sl_est * costs_sl_1$cost_Lb_2years) %>% select(
                                                            Year, costSupply_sl_obs, costSupply_sl_model)
costsSupply_cl_1 <- demand_predict_co4_merge_1 %>% mutate(costSupply_cl_obs = cl * costs_cl_1$cost_Lb_9years,
                                                          costSupply_cl_model = cl_est * costs_cl_1$cost_Lb_9years) %>% select(
                                                            Year, costSupply_cl_obs, costSupply_cl_model)

# costsSupply_hc_cl_pre_1 <- demand_predict_co4_merge_1 %>% mutate(costSupply_cl_pre = cl * costs_hc_cl_pre_1$hc_9years) %>% select(Year, costSupply_cl_pre)
# costsSupply_hc_cl_post_1 <- demand_predict_co4_merge_1 %>% mutate(costSupply_cl_post = cl * costs_hc_cl_post_1$hc_9years) %>% select(Year, costSupply_cl_post)

costsSupply_t_1 <- merge(costsSupply_sl_1, costsSupply_cl_1) %>% mutate(costSupply_t_obs = costSupply_sl_obs + costSupply_cl_obs,
                                                                        costSupply_t_model = costSupply_sl_model + costSupply_cl_model)


# costsSupply_t_1 <- merge(costsSupply_sl_1, merge(costsSupply_hc_cl_pre_1, costsSupply_hc_cl_post_1)) %>% mutate(costSupply_t = costSupply_sl + costSupply_cl_post)


# costsRevenues_1 %>% ggplot(aes(x=Year)) + geom_point(aes(y=clRev_diff, color="Revenue increase")) + geom_line(aes(y=clRev_diff, color="Revenue increase"))+ geom_line(aes(y=costSupply_cl, color="Additional costs")) + geom_point(aes(y=costSupply_cl, color="Additional costs"))+
#   labs(title = "Revenue increase and additional costs for cull cows with tagging", x="Year", y="Revenue and costs (in Billion $)", colour="")  + theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+ scale_x_continuous(name="Year", breaks=c(seq(costsRevenues_1$Year[1],costsRevenues_1$Year[nrow(costsRevenues_1)])))
# 
# costsRevenues_1 %>% ggplot(aes(x=Year)) + geom_point(aes(y=slRev_diff, color="Revenue increase")) + geom_line(aes(y=slRev_diff, color="Revenue increase"))+ geom_line(aes(y=costSupply_sl, color="Additional costs")) + geom_point(aes(y=costSupply_sl, color="Additional costs"))+
#   labs(title = "Revenue increase and additional costs for fed cattle with tagging", x="Year", y="Revenue and costs (in Billion $)", colour="")  + theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+ scale_x_continuous(name="Year", breaks=c(seq(costsRevenues_1$Year[1],costsRevenues_1$Year[nrow(costsRevenues_1)])))
# 
# costsRevenues_1 %>% ggplot(aes(x=Year)) + geom_point(aes(y=totalRev_diff, color="Revenue increase")) + geom_line(aes(y=totalRev_diff, color="Revenue increase"))+ geom_line(aes(y=costSupply_t, color="Additional costs")) + geom_point(aes(y=costSupply_t, color="Additional costs"))+
#   labs(title = "Total Revenue increase and additional costs with tagging", x="Year", y="Revenue and costs (in Billion $)", colour="")  + theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))+ scale_x_continuous(name="Year", breaks=c(seq(costsRevenues_1$Year[1],costsRevenues_1$Year[nrow(costsRevenues_1)])))


# The above are additional costs, excluding holding costs. So basically we compare revenue difference and the additional costs. 
# We do that below

costsRevenues_1 <- merge(rev_cl_1, merge(rev_sl_1, merge(rev_total_1,costsSupply_t_1))) %>% 
  select(Year, clRev_diff_model, costSupply_cl_model, clRev_diff_obs, costSupply_cl_obs, 
         slRev_diff_model, costSupply_sl_model, slRev_diff_obs, costSupply_sl_obs, 
         totalRev_diff_model,costSupply_t_model, totalRev_diff_obs,costSupply_t_obs) %>% filter(Year>2009)

# costsRevenues_1 <- merge(rev_cl_1, merge(rev_sl_1, merge(rev_total_1,costsSupply_t_1))) %>% select(Year, clRev_diff, costSupply_cl_pre, costSupply_cl_post, 
#                                                                                                    slRev_diff,costSupply_sl,
#                                                                                                    totalRev_diff,costSupply_t) %>% filter(Year>2009)

revDiff_costs_sl_1 <- costsRevenues_1 %>% select(Year, slRev_diff_model, costSupply_sl_model, slRev_diff_obs, costSupply_sl_obs)
revDiff_costs_cl_1 <- costsRevenues_1 %>% select(Year, clRev_diff_model, costSupply_cl_model, clRev_diff_obs, costSupply_cl_obs)
revDiff_costs_t_1 <- costsRevenues_1 %>% select(Year, totalRev_diff_model, costSupply_t_model, totalRev_diff_obs, costSupply_t_obs)

revDiff_costs_sl_1_pSurp <- revDiff_costs_sl_1 %>% mutate(diffRevCost_sl_model = slRev_diff_model - costSupply_sl_model,
                                                    diffRevCost_sl_obs = slRev_diff_obs - costSupply_sl_obs) %>% select(Year, diffRevCost_sl_obs,
                                                                                                                        diffRevCost_sl_model)
revDiff_costs_cl_1_pSurp <- revDiff_costs_cl_1 %>% mutate(diffRevCost_cl_model = clRev_diff_model - costSupply_cl_model,
                                                    diffRevCost_cl_obs = clRev_diff_obs - costSupply_cl_obs) %>% select(Year, diffRevCost_cl_obs,
                                                                                                                        diffRevCost_cl_model)
revDiff_costs_t_1_pSurp <- revDiff_costs_t_1 %>% mutate(diffRevCost_t_model = round(totalRev_diff_model - costSupply_t_model,4),
                                                  diffRevCost_t_obs = round(totalRev_diff_obs - costSupply_t_obs,4)) %>% select(Year, diffRevCost_t_obs,
                                                                                                                       diffRevCost_t_model)

 

# revDiff_costs_sl_1[,-1]*1000
# revDiff_costs_cl_1[,-1]*1000
# revDiff_costs_t_1[,-1]*1000

############################ These are with observed sl, cl, disappearance ###########################
######## The above computations show that the price of the sl increases and the cl decreases. The sl quantity remains same.
####### However, the cl increases inducing total increase in demand. Since cl increase, the price of cl falls (one way of explaining). 
####### But with the system in place, the price for sl increases increasing total revenue for fed cattle. 
####### Although the price of cull cow decrease, the revenue for cull cows increase. This is due to the fact that the supply of cl increased.
####### The difference in revenue (before and after the system) for fed cattle is greater than costs. The costs for cull cows are greater than the difference in revenue.
####### In aggregate, the change in revenues are greater than the costs implying the system results in greater producer surplus.
###### We need to keep in mind that the cull cow producers are at a big loss. This is important? I dunno.

########################## These are with estimated sl, cl, disappearance after the system is implemented ##########







### Note that the holding costs are for both sl and cl combined. Try to include them to compute the costs. 
### Compute the costs again. Think carefully.






# Case 1: Observed prices with added costs and estimated sl, cl, demand

cost_price_addedCosts_obs <- cost_price_addedCosts_obs_r %>% filter(Year>=1995 & Year<=2016)
demand_predict <- demand_predict %>% filter(demand_est>0)

Stock_temp <- Stock%>% filter(Year>1994 & Year<2017)
imports_temp <- imports %>% filter(Year>1994 & Year<2017)
exports_temp <- exports %>% filter(Year>1994 & Year<2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>1994  & Year<2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts_obs %>% select(ps), cost_price_addedCosts_obs %>% select(pc),
                    cost_price_addedCosts_obs %>% select(hc), demand_predict %>% filter(Year>1994  & Year<2017) %>% select(sl_est), 
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(cl_est),
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(demand_est)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")


demand_predict_co1<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co1 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co1 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co1$mu_tilde[i] <- params_t1[1]
  parameters_co1$s_tilde[i] <- params_t1[2]
  
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  prices_predict_co1$ps_hat[i] <- ps_hat_t1
  prices_predict_co1$pc_hat[i] <- pc_hat_t1
  prices_predict_co1$hc_hat[i] <- hc_hat_t1
  demand_predict_co1$demand_est[i] <- demand_t1_hat
  demand_predict_co1$sl_est[i] <- sl_t1_hat
  demand_predict_co1$cl_est[i] <- cl_t1_hat
}

prices_predict_co1 <- prices_predict_co1 %>% filter(ps_hat>0)
demand_predict_co1 <- demand_predict_co1 %>% filter(demand_est>0)

prices_predict_co1_merge <- merge(prices_predict_co1, prices_predict_est) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co1_merge <- merge(demand_predict_co1, demand_predict_est)

prices_predict_co1_merge1 <- merge(prices_predict_co1, prices_costs_obs) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co1_merge1 <- merge(demand_predict_co1, demand_obs)


slaughterPrices_plot_co1 <- prices_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Estimate without added costs"))+geom_point(aes(y=ps,color="Estimate without added costs"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge$Year[1], prices_predict_co1_merge$Year[nrow(prices_predict_co1_merge)]))) 

cullPrices_plot_co1 <- prices_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Estimate without added costs"))+geom_point(aes(y=pc,color="Estimate without added costs")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge$Year[1], prices_predict_co1_merge$Year[nrow(prices_predict_co1_merge)]))) 

holdingCosts_plot_co1 <- prices_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Estimate without added costs"))+geom_point(aes(y=hc,color="Estimate without added costs")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge$Year[1], prices_predict_co1_merge$Year[nrow(prices_predict_co1_merge)])))


stock_slaughter_co1 <- demand_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Estimate without added costs"))+geom_point(aes(y=sl,color="Estimate without added costs")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge$Year[1],demand_predict_co1_merge$Year[nrow(demand_predict_co1_merge)])))

stock_cull_co1 <- demand_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Estimate without added costs"))+geom_point(aes(y=cl,color="Estimate without added costs")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge$Year[1],demand_predict_co1_merge$Year[nrow(demand_predict_co1_merge)])))

demand_co1 <- demand_predict_co1_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Estimate without added costs"))+geom_point(aes(y=Demand,color="Estimate without added costs")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge$Year[1],demand_predict_co1_merge$Year[nrow(demand_predict_co1_merge)])))



slaughterPrices_plot_co11 <- prices_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge1$Year[1], prices_predict_co1_merge1$Year[nrow(prices_predict_co1_merge1)]))) 

cullPrices_plot_co11 <- prices_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge1$Year[1], prices_predict_co1_merge1$Year[nrow(prices_predict_co1_merge1)]))) 

holdingCosts_plot_co11 <- prices_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co1_merge1$Year[1], prices_predict_co1_merge1$Year[nrow(prices_predict_co1_merge1)])))


stock_slaughter_co11 <- demand_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge1$Year[1],demand_predict_co1_merge1$Year[nrow(demand_predict_co1_merge1)])))

stock_cull_co11 <- demand_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge1$Year[1],demand_predict_co1_merge1$Year[nrow(demand_predict_co1_merge1)])))

demand_co11 <- demand_predict_co1_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co1_merge1$Year[1],demand_predict_co1_merge1$Year[nrow(demand_predict_co1_merge1)])))









# Case 2: Estimated prices with added costs and estimated sl, cl, demand

cost_price_addedCosts <- cost_price_addedCosts %>% filter(Year>=1995 & Year<=2016)
demand_predict <- demand_predict %>% filter(demand_est>0)

Stock_temp <- Stock%>% filter(Year>1994 & Year<2017)
imports_temp <- imports %>% filter(Year>1994 & Year<2017)
exports_temp <- exports %>% filter(Year>1994 & Year<2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>1994  & Year<2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts %>% select(ps_hat), cost_price_addedCosts %>% select(pc_hat),
                    cost_price_addedCosts %>% select(hc_hat), demand_predict %>% filter(Year>1994  & Year<2017) %>% select(sl_est), 
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(cl_est),
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(demand_est)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")


demand_predict_co2<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co2 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co2 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co2$mu_tilde[i] <- params_t1[1]
  parameters_co2$s_tilde[i] <- params_t1[2]
  
  
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  prices_predict_co2$ps_hat[i] <- ps_hat_t1
  prices_predict_co2$pc_hat[i] <- pc_hat_t1
  prices_predict_co2$hc_hat[i] <- hc_hat_t1
  demand_predict_co2$demand_est[i] <- demand_t1_hat
  demand_predict_co2$sl_est[i] <- sl_t1_hat
  demand_predict_co2$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
}

prices_predict_co2 <- prices_predict_co2 %>% filter(ps_hat>0)
demand_predict_co2 <- demand_predict_co2 %>% filter(demand_est>0)


prices_predict_co2_merge <- merge(prices_predict_co2, prices_predict_est) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co2_merge <- merge(demand_predict_co2, demand_predict_est)

prices_predict_co2_merge1 <- merge(prices_predict_co2, prices_costs_obs) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co2_merge1 <- merge(demand_predict_co2, demand_obs)



slaughterPrices_plot_co2 <- prices_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Estimate without added costs"))+geom_point(aes(y=ps,color="Estimate without added costs"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge$Year[1], prices_predict_co2_merge$Year[nrow(prices_predict_co2_merge)]))) 

cullPrices_plot_co2 <- prices_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Estimate without added costs"))+geom_point(aes(y=pc,color="Estimate without added costs")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge$Year[1], prices_predict_co2_merge$Year[nrow(prices_predict_co2_merge)]))) 

holdingCosts_plot_co2 <- prices_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Estimate without added costs"))+geom_point(aes(y=hc,color="Estimate without added costs")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge$Year[1], prices_predict_co2_merge$Year[nrow(prices_predict_co2_merge)])))


stock_slaughter_co2 <- demand_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Estimate without added costs"))+geom_point(aes(y=sl,color="Estimate without added costs")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge$Year[1],demand_predict_co2_merge$Year[nrow(demand_predict_co2_merge)])))

stock_cull_co2 <- demand_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Estimate without added costs"))+geom_point(aes(y=cl,color="Estimate without added costs")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge$Year[1],demand_predict_co2_merge$Year[nrow(demand_predict_co2_merge)])))

demand_co2 <- demand_predict_co2_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Estimate without added costs"))+geom_point(aes(y=Demand,color="Estimate without added costs")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge$Year[1],demand_predict_co2_merge$Year[nrow(demand_predict_co2_merge)])))



slaughterPrices_plot_co21 <- prices_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge1$Year[1], prices_predict_co2_merge1$Year[nrow(prices_predict_co2_merge1)]))) 

cullPrices_plot_co21 <- prices_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge1$Year[1], prices_predict_co2_merge1$Year[nrow(prices_predict_co2_merge1)]))) 

holdingCosts_plot_co21 <- prices_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co2_merge1$Year[1], prices_predict_co2_merge1$Year[nrow(prices_predict_co2_merge1)])))


stock_slaughter_co21 <- demand_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge1$Year[1],demand_predict_co2_merge1$Year[nrow(demand_predict_co2_merge1)])))

stock_cull_co21 <- demand_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge1$Year[1],demand_predict_co2_merge1$Year[nrow(demand_predict_co2_merge1)])))

demand_co21 <- demand_predict_co2_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co2_merge1$Year[1],demand_predict_co2_merge1$Year[nrow(demand_predict_co2_merge1)])))



# Case 3: Estimated prices with added costs and original sl, cl, demand

cost_price_addedCosts <- cost_price_addedCosts %>% filter(Year>=1995 & Year<=2016)
demand_predict <- demand_predict %>% filter(demand_est>0)

Stock_temp <- Stock%>% filter(Year>1994 & Year<2017)
imports_temp <- imports %>% filter(Year>1994 & Year<2017)
exports_temp <- exports %>% filter(Year>1994 & Year<2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>1994  & Year<2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts %>% select(ps_hat), cost_price_addedCosts %>% select(pc_hat),
                    cost_price_addedCosts %>% select(hc_hat), supp_sl %>% filter(Year>1994 & Year<2017) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>1994 & Year<2017) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>1994 & Year<2017) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")


demand_predict_co3<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co3 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co3 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co3$mu_tilde[i] <- params_t1[1]
  parameters_co3$s_tilde[i] <- params_t1[2]
  
  
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  prices_predict_co3$ps_hat[i] <- ps_hat_t1
  prices_predict_co3$pc_hat[i] <- pc_hat_t1
  prices_predict_co3$hc_hat[i] <- hc_hat_t1
  demand_predict_co3$demand_est[i] <- demand_t1_hat
  demand_predict_co3$sl_est[i] <- sl_t1_hat
  demand_predict_co3$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
}

prices_predict_co3 <- prices_predict_co3 %>% filter(ps_hat>0)
demand_predict_co3 <- demand_predict_co3 %>% filter(demand_est>0)

prices_predict_co3_merge <- merge(prices_predict_co3, prices_predict_est) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co3_merge <- merge(demand_predict_co3, demand_predict_est)

prices_predict_co3_merge1 <- merge(prices_predict_co3, prices_costs_obs) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100)
demand_predict_co3_merge1 <- merge(demand_predict_co3, demand_obs)


slaughterPrices_plot_co3 <- prices_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Estimate without added costs"))+geom_point(aes(y=ps,color="Estimate without added costs"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge$Year[1], prices_predict_co3_merge$Year[nrow(prices_predict_co3_merge)]))) 

cullPrices_plot_co3 <- prices_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Estimate without added costs"))+geom_point(aes(y=pc,color="Estimate without added costs")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge$Year[1], prices_predict_co3_merge$Year[nrow(prices_predict_co3_merge)]))) 

holdingCosts_plot_co3 <- prices_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Estimate without added costs"))+geom_point(aes(y=hc,color="Estimate without added costs")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge$Year[1], prices_predict_co3_merge$Year[nrow(prices_predict_co3_merge)])))


stock_slaughter_co3 <- demand_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Estimate without added costs"))+geom_point(aes(y=sl,color="Estimate without added costs")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge$Year[1],demand_predict_co3_merge$Year[nrow(demand_predict_co3_merge)])))

stock_cull_co3 <- demand_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Estimate without added costs"))+geom_point(aes(y=cl,color="Estimate without added costs")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge$Year[1],demand_predict_co3_merge$Year[nrow(demand_predict_co3_merge)])))

demand_co3 <- demand_predict_co3_merge %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Estimate without added costs"))+geom_point(aes(y=Demand,color="Estimate without added costs")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge$Year[1],demand_predict_co3_merge$Year[nrow(demand_predict_co3_merge)])))


slaughterPrices_plot_co31 <- prices_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge1$Year[1], prices_predict_co3_merge1$Year[nrow(prices_predict_co3_merge1)]))) 

cullPrices_plot_co31 <- prices_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge1$Year[1], prices_predict_co3_merge1$Year[nrow(prices_predict_co3_merge1)]))) 

holdingCosts_plot_co31 <- prices_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co3_merge1$Year[1], prices_predict_co3_merge1$Year[nrow(prices_predict_co3_merge1)])))


stock_slaughter_co31 <- demand_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge1$Year[1],demand_predict_co3_merge1$Year[nrow(demand_predict_co3_merge1)])))

stock_cull_co31 <- demand_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge1$Year[1],demand_predict_co3_merge1$Year[nrow(demand_predict_co3_merge1)])))

demand_co31 <- demand_predict_co3_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
  labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co3_merge1$Year[1],demand_predict_co3_merge1$Year[nrow(demand_predict_co3_merge1)])))




