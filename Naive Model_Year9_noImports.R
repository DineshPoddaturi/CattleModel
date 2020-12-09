require(tidyverse)
require(reshape2)
require(readxl)
library(data.table)
require(nleqslv)
require(BB)

#Reading totals data
tot <- read_excel("Data/Cattle-Totals.xlsx") %>% as.data.frame()

#some cleaning
totals <- data.frame(t(tot[-1]))
colnames(totals) <- tot[,1]
totals <- totals %>% mutate(Year = c(rownames(totals)))
totals <- totals %>% select(Year,everything())
totals$Year <- as.numeric(totals$Year)

Inv_check <- totals$`INCL CALVES - INVENTORY` - totals$`BULLS, GE 500 LBS - INVENTORY` - totals$`CALVES - INVENTORY` - 
  totals$`COWS - INVENTORY` - totals$`HEIFERS, GE 500 LBS, (EXCL REPLACEMENT) - INVENTORY` - totals$`HEIFERS, GE 500 LBS, BEEF REPLACEMENT - INVENTORY` - 
  totals$`HEIFERS, GE 500 LBS, MILK REPLACEMENT - INVENTORY` - totals$`STEERS, GE 500 LBS - INVENTORY`

Inv_check <- as.data.frame(Inv_check)
Inv_check <- Inv_check %>% mutate(Year = totals$Year) %>% select(Year,everything())

export <- NULL
for(i in 1:nrow(totals)){
  export[i] <- totals$`INCL CALVES - INVENTORY`[i] + totals$`CALF CROP, MEASURED IN HEAD`[i] - totals$`(EXCL CALVES) - LOSS, DEATH, MEASURED IN HEAD`[i] - 
    totals$`CALVES - LOSS, DEATH, MEASURED IN HEAD`[i] - totals$`CALVES - SLAUGHTERED, MEASURED IN HEAD`[i] - totals$`GE 500 LBS - SLAUGHTERED, MEASURED IN HEAD`[i] - 
    totals$`INCL CALVES - INVENTORY`[i+1]
}

Net_export <- as.data.frame(export) %>% mutate(Year = totals$Year) %>% select(Year,everything())


### these contain percentages
ratios <- totals$Year %>% as.data.frame()
names(ratios) <- "Year"

ratios <- ratios %>% mutate(CalfCrop_CowInv = totals$`CALF CROP, MEASURED IN HEAD`/totals$`COWS - INVENTORY`, 
                            BeefCowInv_CowInv = totals$`COWS, BEEF - INVENTORY`/totals$`COWS - INVENTORY`,
                            CalfDeathLoss = totals$`CALVES - LOSS, DEATH, MEASURED IN HEAD`/totals$`CALF CROP, MEASURED IN HEAD`,
                            Cattle_notCalf_DeathLoss= totals$`(EXCL CALVES) - LOSS, DEATH, MEASURED IN HEAD`/(totals$`INCL CALVES - INVENTORY`-totals$`CALVES - INVENTORY`),
                            Import = -Net_export$export/totals$`INCL CALVES - INVENTORY`, 
                            Bull = totals$`BULLS, GE 500 LBS - INVENTORY`/totals$`INCL CALVES - INVENTORY`)


Heifer_Steer_Inv <- totals$Year %>% as.data.frame()
names(Heifer_Steer_Inv) <- "Year"
Heifer_Steer_Inv <- Heifer_Steer_Inv %>% 
  mutate(Slaughter_Inventory = totals$`GE 500 LBS - SLAUGHTERED, MEASURED IN HEAD` - totals$`HEIFERS, GE 500 LBS, (EXCL REPLACEMENT) - INVENTORY`-  totals$`STEERS, GE 500 LBS - INVENTORY`)
Heifer_Steer_Inv <- Heifer_Steer_Inv %>% 
  mutate(Percent_Slaughter_Cull = Slaughter_Inventory/totals$`GE 500 LBS - SLAUGHTERED, MEASURED IN HEAD`)

Beef_Dairy_Cows <- totals$Year %>% as.data.frame()
names(Beef_Dairy_Cows) <- "Year"
Beef_Dairy_Cows <- Beef_Dairy_Cows %>% mutate(Inv = totals$`COWS, BEEF - INVENTORY` + 0.5 * totals$`COWS, MILK - INVENTORY`)


# Reading production data
production <- read_excel("Data/BeefProduction.xlsx") %>% as.data.frame()
production <- production %>% mutate(Prod_bill = Production/1000000000, Cattle_500lbs_slaug = Production/totals$`GE 500 LBS - SLAUGHTERED, MEASURED IN HEAD`,
                                    Avg_Weight = Cattle_500lbs_slaug/0.63)

#Reading Meat, Beef and Veal data

meatBeefVeal <- read_excel("Data/MeatBeefVeal-Data.xlsx") %>% as.data.frame()

meat <- data.frame(t(meatBeefVeal[-1]))
colnames(meat) <- meatBeefVeal[,1]
meat <- meat %>% mutate(Year = c(rownames(meat)))
meat <- meat %>% select(Year,everything())
meat_billion <- meat %>% select(Year,everything())
meat_billion[,2:ncol(meat_billion)] <- (meat_billion[,2:ncol(meat_billion)] * 2204)/1000000



demand <- totals$Year %>% as.data.frame()
names(demand) <- "Year"

demand <- demand %>% 
  mutate(Demand = meat_billion$`Beginning Stocks` + meat_billion$Production + meat_billion$Imports - meat_billion$Exports - meat_billion$`Ending Stocks`)

########################

## This is different data from the above


beefDemand <- read_excel("Data/BeefDemand.xlsx") %>% as.data.frame()
beefDemand1 <- beefDemand %>% separate(col= Quarter, into=c("Quarter","Period", "PeriodEnd")) %>% filter(Quarter == "Yr")
beefDemand1 <- beefDemand1 %>% select(Year, Commercial, Farm, `Beginning stocks`, Imports, Exports, `Ending stocks`, `Total supply`, `Total disappearance`) %>% filter(Year>=1994)

## converting from million to billion pounds in carcass weight
beefDemand1[,-1] <- (beefDemand1[,-1])/1000

beefDemand1 <- beefDemand1 %>% mutate(Demand = Commercial + Farm + Imports - Exports +`Beginning stocks` - `Ending stocks`)
beefDemand1 <- beefDemand1 %>% filter(Year >= 2009 & Year < 2018)


##########################

dressedWeights <- dressedWeights %>% filter(Year >= 2009 & Year <2018)

##############################


#reading prices data

price <- read_excel("Data/PricesReceived.xlsx") %>% as.data.frame()


prices_received <- data.frame(t(price[-1]))
colnames(prices_received) <- price[,1]
prices_received <- prices_received %>% mutate(Year = c(rownames(prices_received)))
prices_received <- prices_received %>% select(Year,everything())
prices_received$Year <- as.numeric(prices_received$Year)





# Parameters
beta <- 0.98
delta <- 0.95

gamma0 <- 0.90
gamma1 <- 0.95

g <- 0.97
phi <- 0.63

avg_weight <- 1250

#### working with stock 

K <- totals %>% filter(Year>=2001 & Year<=2018) %>% select(Year, `COWS, BEEF - INVENTORY`) %>% as.data.frame()
names(K) <- c("Year", "K")

k3 <- totals %>% filter(Year>=2001 & Year<=2017) %>% select(Year,`HEIFERS, GE 500 LBS, BEEF REPLACEMENT - INVENTORY`) %>% mutate(Year= Year+1) %>% as.data.frame()
names(k3) <- c("Year", "k3")

k4 <- delta * k3$k3 %>% as.data.frame()
names(k4) <- "k4"
k4 <- k4 %>% mutate(Year=k3$Year+1)%>% select(Year,k4) %>% filter(Year<2019)


k5 <- delta * k4$k4 %>% as.data.frame()
names(k5) <- "k5"
k5 <- k5 %>% mutate(Year=k4$Year+1) %>% select(Year,k5) %>% filter(Year<2019)

k6 <- delta * k5$k5 %>% as.data.frame()
names(k6) <- "k6"
k6 <- k6 %>% mutate(Year = k5$Year+1) %>% select(Year,k6) %>% filter(Year<2019)

k7 <- delta * k6$k6 %>% as.data.frame()
names(k7) <- "k7"
k7 <- k7 %>% mutate(Year = k6$Year+1) %>% select(Year,k7) %>% filter(Year<2019)

k8 <- delta * k7$k7 %>% as.data.frame()
names(k8) <- "k8"
k8 <- k8 %>% mutate(Year = k7$Year+1) %>% select(Year,k8) %>% filter(Year<2019)


Stock <- merge(merge(merge(merge(merge(merge(K,k3,all=TRUE),k4, all=TRUE),k5,all=TRUE),k6,all=TRUE),k7,all=TRUE),k8,all=TRUE) %>% as.data.frame()

k9 <- Stock %>% filter(Year>2007) %>% select(K,k3,k4,k5,k6,k7,k8) %>% mutate(k9 = K -(k3+k4+k5+k6+k7+k8) ) %>% select(k9) %>% round() %>% as.data.frame()
k9 <- k9 %>% mutate(Year = seq(from=2008, to=2018)) %>% select(Year,k9)
k9$Year <- as.numeric(k9$Year)

Stock <- merge(Stock,k9, all=TRUE)

k10 <- Stock %>% filter(Year>2008) %>% select(K,k3,k4,k5,k6,k7,k8,k9) %>% mutate(k10 = K -(k3+k4+k5+k6+k7+k8+k9) ) %>% select(k10) %>% round() %>% as.data.frame()
k10 <- k10 %>% mutate(Year = seq(from=2009, to=2018)) %>% select(Year,k10)

Stock <- merge(merge(Stock,k9,all=TRUE),k10,all=TRUE) %>% select(Year,K,k3,k4,k5,k6,k7,k8,k9,k10)


Stock <- Stock %>% filter(Year>=2008 & Year<=2018)
Stock <- as.data.frame(Stock)

###### Calculations

# Supply of slaughter animals

exports_2002 <- Net_export %>% filter(Year>=2008 & Year<=2018) %>% as.data.frame()
exportsNew <- beefDemand1 %>% filter(Year>=2008 & Year<=2018) %>% select(Year, Exports) 
importsNew <- beefDemand1  %>% filter(Year>=2008 & Year<=2018) %>% select(Year, Imports)


supp_sl <- NULL

for(i in 1:nrow(Stock)){
  if(Stock$Year[i]>=2009){
    supp_sl[i] <- g * Stock$K[i-1] - Stock$k3[i+1] + importsNew$Imports[i]
  }
}


supp_sl <- supp_sl %>% as.data.frame() %>% na.omit()
names(supp_sl) <- "Slaughter"
supp_sl <- supp_sl %>% mutate(Year = seq(from=2009, to = 2017)) %>% select(Year,everything())
supp_sl <- supp_sl %>% mutate(Bill_meatLb_sl = (Slaughter * dressedWeights$Steers_avg)/1000000000)


# supply of the cull animals

supp_cl <- NULL

for(i in 1:nrow(Stock)){
  if(Stock$Year[i]>=2009){
    supp_cl[i] <-  Stock$k10[i] + (Stock$k9[i] - Stock$k10[i+1]) + (Stock$k8[i]-Stock$k9[i+1]) + (Stock$k7[i] - Stock$k8[i+1])
  }
}


supp_cl <- supp_cl %>% as.data.frame() %>% na.omit()
names(supp_cl) <- "Cull"
supp_cl <- supp_cl %>% mutate(Year = seq(from=2009, to = 2017)) %>% select(Year,everything())
supp_cl <- supp_cl %>% mutate(Bill_meatLb_cl = (Cull * dressedWeights$Cattle_avg)/1000000000)





totalSupply <- (supp_sl %>% filter(Year>=2009) %>% select(Bill_meatLb_sl)) + (supp_cl %>% select(Bill_meatLb_cl)) %>% na.omit()
names(totalSupply) <- "TotalSupply"
totalSupply <- totalSupply %>% mutate(Year = supp_cl$Year) %>% select(Year,everything())



slaughter <- totals %>% select(Year,`GE 500 LBS - SLAUGHTERED, MEASURED IN HEAD`)
names(slaughter) <- c("Year","Slaughter")
slaughter <- slaughter %>% filter(Year>=2001) %>% na.omit()

dairy_Supp <- (totalSupply %>% filter(Year>=2009) %>% select(TotalSupply)) - (slaughter %>% filter(Year>=2009) %>% select(Slaughter))
names(dairy_Supp) <- "dairyBeef"
dairy_Supp <- dairy_Supp %>% mutate(Year = seq(2009,2018), Bill_meatLb = (dairyBeef * avg_weight * phi)/1000000000) %>% select(Year, everything())


### Ratio of slaughter supply  to total supply, in the model this is exp()/(1+exp())

sl_ratio <- (supp_sl %>% filter(Year>=2009) %>% select(Bill_meatLb_sl)) / (totalSupply %>% select(Bill_meatLb))
names(sl_ratio) <- "SlaughterShare" 
sl_ratio <- sl_ratio %>% mutate(Year = totalSupply$Year) %>% select(Year,everything())

### Ratio of cull supply to total supply or simpley 1-slaughtersupply, in the model this is 1/(1+exp())
cl_ratio <- sl_ratio
cl_ratio <- cl_ratio %>% mutate(CullShare = 1-SlaughterShare) %>% select(Year,CullShare)

## solving for the expression in inside the exponential

tildes <- log((1- cl_ratio$CullShare)/cl_ratio$CullShare) %>% as.data.frame()
names(tildes) <- "Tildes"




prices_received_new <- prices_received %>% filter(Year>=2009 & Year<=2018) %>% mutate(pc = `COWS($/CWT)`/100 , ps = `STEERS & HEIFERS, GE 500 LBS($/CWT)`/100) %>% select(Year,ps,pc)


#############################

pc_ps <- merge(pcs,pss) %>% filter(Year>=1994)
receivedPrices <- pc_ps %>% filter(Year>=2009 & Year<2018) %>% select(Year, ps, pc)
#######################





lossfn <- function(theta,e,ps,pc){
  mu <- theta[1]
  s <- theta[2]
  
  v <- sum((e - (( mu - ((ps/phi) - (pc/phi)))/s))^2)
  
  return(v)
}

theta0 <- c(1,0.5)

e <- tildes$Tildes %>% as.vector()
ps <- receivedPrices$ps
pc <- receivedPrices$pc

out <- BBoptim(par= theta0, fn = lossfn, e=e,ps=ps,pc=pc)


muTilde <- out$par[1]
sTilde <- out$par[2]


mu <- muTilde/phi
pStd <- (sTilde/phi) * (pi/sqrt(3))


#### We are in the case where the farmers cull the 9 year old cows

hc <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1))) %>% as.data.frame()
names(hc) <- "hc"

prices_costs <- cbind(ps,pc,hc) %>% mutate(Year=receivedPrices$Year) %>% select(Year,everything())

Stock_new <- Stock %>% filter(Year>=2008)
supp_sl_new <- supp_sl %>% filter(Year>=2009) %>% select(Year, Bill_meatLb_sl)
supp_cl_new <- supp_cl %>% select(Year, Bill_meatLb_cl)
demand_new1 <- beefDemand1 %>% filter(Year >= 2009 & Year < 2018) %>% select(Year, Demand)

demand_new1 <-  totalSupply %>% mutate(Demand = TotalSupply) %>% select(Year, Demand)





##### with the estimated mu_tilde and s_tilde we solve for optimal prices. Here we use Stock_new,prices_costs,demand_new to solve


sysEqs_9 <- function(p){
  ps <- p[1]
  pc <- p[2]
  h <- p[3]
  
  # F <- numeric(length(p))
  
  F1 <- sl - A * ((exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))/(1 + (exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde))))
  
  F2 <- cl  - A * (1/(1+ exp((muTilde - ((ps/phi) - (pc/phi)))/sTilde)))
  
  F3 <- ps * ( 1- ((g*(beta^3)) * ((1-beta^7)/(1-beta)) ) ) - (beta^7)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^7)/(1-beta))*h
  
  F = F1^2 + F2^2 + F3^2
  
  return(F)
}

masterData <- merge(demand_new1,merge(supp_sl_new,merge(supp_cl_new,prices_costs)))

len <- nrow(masterData)

out_optim <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))
out_bb <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))
out_nl <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len),hc_hat=numeric(len))

# out_optim <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len))
# out_bb <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len))
# out_nl <- data.frame(Year=masterData$Year,ps_hat=numeric(len),pc_hat=numeric(len))

for(i in 1:len){
  
  A <- masterData$Demand[i]
  sl <- masterData$Bill_meatLb_sl[i]
  cl <- masterData$Bill_meatLb_cl[i]
  
  ps <- masterData$ps[i]
  pc <- masterData$pc[i]
  hc <- masterData$hc[i]
  
  p <- c(ps,pc,hc)
  
  # p <- c(ps,pc)
  
  est_optim <- optim(par=p, fn = sysEqs_9)$par
  est_bb <- BBoptim(par=p, fn = sysEqs_9)$par
  est_nl <- nlm(p = p, f = sysEqs_9)$estimate
  
  out_optim$ps_hat[i] <- est_optim[1]
  out_optim$pc_hat[i] <- est_optim[2]
  out_optim$hc_hat[i] <- est_optim[3]
  
  out_bb$ps_hat[i] <- est_bb[1]
  out_bb$pc_hat[i] <- est_bb[2]
  out_bb$hc_hat[i] <- est_bb[3]
  
  out_nl$ps_hat[i] <- est_nl[1]
  out_nl$pc_hat[i] <- est_nl[2]
  out_nl$hc_hat[i] <- est_nl[3]
  
}


#### now we calculate the supposedly slaughtered and culled meat in billion pounds


A <- masterData %>% select(Demand)
est_sl_cl <- cbind(out_optim,A)

est_sl_cl <- est_sl_cl %>% mutate(sl_hat = Demand * ((exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde))/(1 + (exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde)))), 
                                  cl_hat = Demand * (1/(1 + (exp((muTilde - ((ps_hat/phi) - (pc_hat/phi)))/sTilde)))) )



MasterPricesCosts <- merge(prices_costs,est_sl_cl) %>% as.data.frame() %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)
# MasterPricesCosts <- merge(prices_costs,est_sl_cl) %>% as.data.frame() %>% select(Year, ps, ps_hat, pc, pc_hat, hc) %>% mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100)



slaughter_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(2009,2017))) 

cull_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + scale_x_continuous(name="Year", breaks=c(seq(2009,2017))) 

holding_plot <- MasterPricesCosts %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ scale_x_continuous(name="Year", breaks=c(seq(2009,2017))) 


Master_sl_cl <- merge(est_sl_cl,merge(supp_sl_new,supp_cl_new)) %>% select(Year,Bill_meatLb_sl, sl_hat, Bill_meatLb_cl, cl_hat)
names(Master_sl_cl) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")



stock_slaughter <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Predicted")) + geom_point(aes(y=sl_hat,color="Predicted")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="Stock") + theme_classic()

stock_cull <- Master_sl_cl %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Predicted")) + geom_point(aes(y=cl_hat,color="Predicted")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="Stock") + theme_classic()



Master_sl_cl_head <- Master_sl_cl %>% mutate(sl = ((sl*1000000000)/(avg_weight * phi))/1000000, sl_hat = ((sl_hat * 1000000000)/(avg_weight * phi))/1000000, cl = ((cl * 1000000000)/(avg_weight * phi))/1000000, cl_hat = ((cl_hat * 1000000000)/(avg_weight * phi))/1000000)


stock_slaughter_head <- Master_sl_cl_head %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Predicted")) + geom_point(aes(y=sl_hat,color="Predicted")) + 
  labs(x="Year", y="Slaughtered animals (in million)", colour="Stock") + theme_classic()

stock_cull_head <- Master_sl_cl_head %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Predicted")) + geom_point(aes(y=cl_hat,color="Predicted")) +
  labs(x="Year", y="Culled animals (in million)", colour="Stock") + theme_classic()








