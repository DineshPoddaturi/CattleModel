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


# - exports$Exports[i]

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

totalSupply <- totalSupply %>% filter(Year>1994) %>% mutate(TotalSupply = TotalSupply * adjFactor_New$AdjFactor)
supp_sl  <- supp_sl %>% filter(Year>1994) %>% mutate(Bill_meatLb_sl = Bill_meatLb_sl * adjFactor_New$AdjFactor)
supp_cl <- supp_cl %>% filter(Year>1994) %>% mutate(Bill_meatLb_cl = Bill_meatLb_cl * adjFactor_New$AdjFactor)

demand_new <- supp_diss %>% select(Year,total_meat_bill)

names(demand_new) <- c("Year", "Demand")


### Ratio of slaughter supply  to total supply, in the model this is exp()/(1+exp())

sl_ratio <- (supp_sl %>% select(Bill_meatLb_sl)) / (totalSupply %>% select(TotalSupply))
names(sl_ratio) <- "SlaughterShare" 
sl_ratio <- sl_ratio %>% mutate(Year = totalSupply$Year) %>% select(Year,everything())

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
  
  v <- sum((e - (( mu - ((ps-pc)/phi)))/s))^2
  
  return(v)
}

theta0 <- c(3,2)

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
sl_cl <- merge(supp_sl, supp_cl) %>% select(Year, Bill_meatLb_sl, Bill_meatLb_cl)
sl_cl_supp <- sl_cl %>% ggplot(aes(x=Year)) + geom_line(aes(y=Bill_meatLb_sl, color="Fed cattle meat")) + geom_point(aes(y=Bill_meatLb_sl, color="Fed cattle meat")) + geom_line(aes(y=Bill_meatLb_cl, color="Cull cattle meat")) + 
  geom_point(aes(y=Bill_meatLb_cl, color="Cull cattle meat")) + labs(x="Year", y="Meat in billion pounds", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks = c(seq(1995,2017)))

supp_demand <- merge(totalSupply, demand_new) %>% select(Year, TotalSupply, Demand)
supp_demand_plot <- supp_demand %>% ggplot(aes(x=Year)) + geom_line(aes(y=TotalSupply, color="Total Supply"))  +geom_point(aes(y=TotalSupply, color="Total Supply")) + geom_line(aes(y=Demand, color="Total disapperance")) + 
  geom_point(aes(y=Demand, color="Total disapperance"))+ labs(x="Year", y="Meat in billion pounds", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks = c(seq(1995,2017)))

prices_costs_plot <- prices_costs %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Fed cattle price"))+geom_point(aes(y=ps,color="Fed cattle price"))+
  geom_line(aes(y=pc,color="Cull cattle price")) + geom_point(aes(y=pc,color="Cull cattle price"))+geom_line(aes(y=hc, color="Holding Costs")) +geom_point(aes(y=hc, color="Holding Costs"))+ 
  labs(x="Year", y="Prices and costs(\\$/cwt)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(1993,2017))) 
#################################



Stock_new <- Stock %>% filter(Year>=1994 & Year<2018)
supp_sl_new <- supp_sl %>% select(Year, Bill_meatLb_sl)
supp_cl_new <- supp_cl %>% select(Year, Bill_meatLb_cl)

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



# err <- 2
# while (err>0.1) {
#   
#   
#   p <- c(ps,hc)
#   
#   # lo <- c(0,0)
#   hi <- c(1.5,0.5)
#   
#   out_b <- BBoptim(par=p, fn = sysEqs_9, upper = hi)$par
#   
#   ps <- out_b[1]+0.11
#   # pc <- out_b[2] + 0.01
#   hc <- out_b[2]+0.11
#   
#   
#   sl_est <- A * ((exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))/(1 + (exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))))
#   # cl_est <- A * (1/(1+ exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde)))
#   
#   err <- abs( (sl   - sl_est ) )
# }






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

ddl_plot <- ddl %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=Demand_hat, color="Predicted"))+geom_point(aes(y=Demand_hat,color="Predicted")) + 
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


#### This is using cull animals
predict_df <- cbind(Stock_temp$Year, Stock_temp$k9  ,Stock_temp$k8, Stock_temp$k7, Stock_temp$k6, prices_costs %>% filter(Year>=1995) %>% select(ps), 
                    prices_costs %>% filter(Year>=1995) %>% select(pc), prices_costs %>% filter(Year>=1995) %>% select(hc), dressedWeights_sl_cl %>% filter(Year>1994)%>% select(Cull_avg),
                    masterData %>% filter(Year>=1995) %>% select(Bill_meatLb_sl), masterData %>% filter(Year>=1995) %>% select(Bill_meatLb_cl)) %>% as.data.frame()
names(predict_df) <- c("Year", "k9", "k8", "k7", "k6", "ps", "pc", "hc", "dressedWeight", "sl", "cl")

k6_t <- predict_df %>% select(Year, k6)
k7_t <- predict_df %>% select(Year, k7)
k8_t <- predict_df %>% select(Year, k8)
dressed_t <- predict_df %>% select(Year, dressedWeight)
pricesCosts_t <- prices_costs %>% filter(Year>=1995)

demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))
prices_predict <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))


for(i in 1:nrow(predict_df)){
# K_t <- predict_df$K[i]
# k_3_t2 <- predict_df$k3[i+2]
# imports_t1 <- predict_df$imports[i+1]

# i=4
    k6_t <- predict_df$k6[i]
    k7_t <- predict_df$k7[i]
    k8_t <- predict_df$k8[i]
    k9_t <- predict_df$k9[i]
    
    if(i<=1){
      ps_t <- predict_df$ps[i]
      pc_t <- predict_df$pc[i]
      hc_t <- predict_df$hc[i]
    }
    
    dressed_t <- predict_df$dressedWeight[i]
    # sl_t <- predict_df$sl[i+1]
    # cl_t <- predict_df$cl[i+1]
    
    share <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
    
    demand_t1 <- delta * (k8_t + (1-delta) * (k7_t + k6_t) ) * (dressed_t/1000000000) * (1 + share)
    
    sl_t1 <- demand_t1 * ((share)/(1 + share))
    cl_t1 <- demand_t1 * 1/(1+share)
    
    
    
    p <- c(ps_t, pc_t, hc_t)
    sl <- sl_t1
    cl <- cl_t1
    A <- demand_t1
    
    est_bb <- BBoptim(par=p, fn = sysEqs_9)$par
    ps_hat_t1 <- est_bb[1]
    pc_hat_t1 <- est_bb[2]
    hc_hat_t1 <- est_bb[3]
    
    ps_t <- ps_hat_t1
    pc_t <- pc_hat_t1
    hc_t <- hc_hat_t1
    
    prices_predict$ps_hat[i] <- ps_t
    prices_predict$pc_hat[i] <- pc_t
    prices_predict$hc_hat[i] <- hc_t
    demand_predict$demand_est[i] <- demand_t1
    demand_predict$sl_est[i] <- sl_t1
    demand_predict$cl_est[i] <- cl_t1

}



######################################################
## Here we use the slaughter animals numbers












