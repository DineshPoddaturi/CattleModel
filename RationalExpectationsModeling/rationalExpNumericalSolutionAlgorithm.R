####### I read calf crop data. These are in number of head
calf_crop <- read_excel("Data/New/CalfCrop.xlsx") %>% as.data.frame()

calf_crop <- calf_crop %>% select(Year, Value) %>% transmute(Year = Year, calfCrop = Value)

calfCrop_replacementHeifers <- merge(calf_crop, replacementInventory) %>% transmute(Year = Year, calfCrop = calfCrop,
                                                                                    repHeifers = k3)

##### Here I am computing the ratio of the replacement heifers to calf crop of prv year
calfCrop_replacementHeifers <- calfCrop_replacementHeifers %>% mutate(calfCrop_repHeifers_Ratio = repHeifers/lag(calfCrop), 
                                                                      calfCrop_repHeifers_Percent = repHeifers/lag(calfCrop) * 100)

summary(calfCrop_replacementHeifers$calfCrop_repHeifers_Percent)

#### Here I am getting an approximate percentage of the progeny of the total stock that are added into 
#### breeding stock as replacement heifers. Note that here I am taking replacement heifers two periods ahead
#### Because if the cow gives birth this period the heifers are added into the breeding stock two periods from now
summary(Stock %>% select(Year, K, k3) %>% mutate(ratios = lead(k3,2)/(g*K)) %>% select(ratios))

# ratios      
# Min.   :0.1504  
# 1st Qu.:0.1684  
# Median :0.1757  
# Mean   :0.1779  
# 3rd Qu.:0.1840  
# Max.   :0.2227  
# NA's   :2 

#### From the above on average approximately 18% of the heifers are added into the breeding stock



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

### But first I will construct the nodes for corn price, fed cattle supply, and cull cow supply.

#### I need to construct production shock. supp_sl and supp_cl are the estimated fed cattle and cull cow supply. I need to get the 
#### observed supply as well. Isn't it the animals slaughtered? If yes, then we are assuming that the supply equals demand.
#### For now that is what I am doing i.e., assuming that the animal slaughtered as the observed supply and the constructed supply is 
#### the estimated supply. (After talking with Lee and Chad, I have decided the slaughtered data as the observed supply)

################### Here I am retracing the steps by writing the code again

# State variables: corn price, demand shock, cull cows production with gaussian shocks, and fed cattle production with gaussian shock.
## NOTE: the demand shock is also gaussian.

#### Here I am constructing the sl and cl quantities that includes shock (which is a gaussian random variable). 
#### Note: these gaussian shocks has mean one and standard deviation according to the historical shocks (see above for details)

cowsSlaughtered_obs <- cowsSlaughtered %>% transmute(Year = Year, cullMeat = cull_meat/1000000000)
heifersSlaughtered_obs <- heifersSlaughtered %>% transmute(Year = Year, heiferMeat = heifer_meat/1000000000)
steersSlaughtered_obs <- steersSlaughtered %>% transmute(Year = Year, steerMeat = steer_meat/1000000000)

fedCattleSupply_obs <- merge(heifersSlaughtered_obs, steersSlaughtered_obs) %>% transmute(Year = Year, sl_obs = heiferMeat + steerMeat)
cullCowSupply_obs <- cowsSlaughtered_obs %>% transmute(Year = Year, cl_obs = cullMeat)

#### Here we get the fed cattle production shock
obsEst_sl_Supply <- merge(fedCattleSupply_obs, supp_sl) %>% transmute(Year = Year, sl_obs = sl_obs, sl_est = Bill_meatLb_sl,
                                                                      slShock = sl_obs/sl_est)
slSupplyShockGaussian <- obsEst_sl_Supply %>% transmute(Year = Year, slShock = 0)

set.seed(3)
slSupply_Shock <- rnorm(n = nrow(prices_quant), mean = 1, sd = std(obsEst_sl_Supply$slShock))
slSupplyShockGaussian$slShock <- slSupply_Shock

# ggplot(data = slSupplyShockGaussian, aes(x=slShock)) + geom_density()

#### Here we construct the cull cows production shock
obsEst_cl_Supply <- merge(cullCowSupply_obs, supp_cl) %>% transmute(Year = Year, cl_obs = cl_obs, cl_est = Bill_meatLb_cl,
                                                                    clShock = cl_obs/cl_est)
clSupplyShockgaussian <- obsEst_cl_Supply %>% transmute(Year = Year, clShock = 0)

set.seed(4)
clSupply_Shock <- rnorm(n = nrow(prices_quant), mean = 1, sd = std(obsEst_cl_Supply$clShock))
clSupplyShockgaussian$clShock <- clSupply_Shock

##### I will generate the demand shocks. For this I need the observed demand and the constructed demand. 
##### The observed demand is from derived demand from data. 
##### Constructed demand would be animals slaughtered for consumption.

### Observed derived demand would be the sum of Exports and Domestic Consumption from demandBeef dataframe
### Constructed demand would be the total animals slaughtered for consumption purposes. I get this from slaughtered data. The dataframe
### totalDisappeared has the data.

estD <- merge(supp_sl, supp_cl) %>% transmute(Year = Year, demandEst = Bill_meatLb_sl + Bill_meatLb_cl)

# obsDemand <- demandBeef %>% transmute(Year = Year, demandObs = Exports + `Domestic Consumption`)
# rownames(obsDemand) <- 1:nrow(obsDemand)
obsDemand <- totalDisappeared %>% transmute(Year = Year, demandObs = total_meat_bill)
demandShock <- merge(obsDemand, estD) %>% mutate(dShock = demandObs/demandEst)

demandShockGaussian <- demandShock %>% transmute(Year = Year, Shock = 0)

# demandConstructed <- demandShockGaussian %>% transmute(Year = Year + 2, )

## Now i generate gaussian shock which is consistent with historical data.
## I use the standard deviation of historical data to construct the gaussian random variables. Here the mean is 1
set.seed(1)
demandShockG <- rnorm(n = nrow(demandShock), mean = 1, sd = std(demandShock$dShock))
demandShockGaussian$Shock <- demandShockG
demandShockGaussian$Year <- as.double(demandShockGaussian$Year)

# ggplot(data = clSupplyShockgaussian, aes(x=clShock)) + geom_density()

#### I am merging all the supply and demand shocks
allShocks <- merge(merge(demandShockGaussian, slSupplyShockGaussian), clSupplyShockgaussian)


sl_stock <- supp_sl %>% transmute(Year = Year, sl_est = Bill_meatLb_sl, slHead = Slaughter)
cl_stock <- supp_cl %>% transmute(Year = Year, cl_est = Bill_meatLb_cl, clHead = Cull)

dataList <- list(Stock, sl_stock, cl_stock, slSupplyShockGaussian, 
                 clSupplyShockgaussian, dressedWeights_sl_cl, imports_exports, calf_crop)

allStockShocks <- Reduce(function(...) merge(...), dataList)


#### Here I am writing functions that return the fed cattle and cull cow production.
#### Note: this includes the gaussian shocks we generated which has mean 1 and standard deviation according to the historical data

fedProduction <- function(stockShocks){
  
  newSL_3 <- stockShocks %>%
    transmute(Year = Year+3, slt = (delta - 0.19) * K * slShock + 
                (1-0.19) * g * delta * (K - (delta - 0.19) * lag(K) - (k9 + (1-delta) * k8 + (1-delta) * k7)),
              slLbs = slt * Slaughter_avg/1000000000) 
  
  newSL_1 <- stockShocks %>%
    transmute(Year = Year+1, slt = (g - 0.22 * g) * lag(K,2) * lag(slShock,1) + 
                (1 - 0.22 * g) * g * delta * (lag(K,2) - (g - 0.22 * g) * lag(K,3) - 
                                                (lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2))),
              slLbs = slt * Slaughter_avg/1000000000) 
  
  return(c(newSL_3,newSL_1))
}

cullProduction <- function(stockShocks){
  
  newCL_3 <- stockShocks %>%
    transmute(Year = Year + 3, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock +
                (delta^2) * (delta * (k6 + k5 + k4) - (k5 + k6 + k7)),
              clLbs = clt * Cull_avg/1000000000)
  
  newCL_1 <- allStockShocks %>%
    transmute(Year = Year + 1, clt = (k9 + (1-delta) * k8 + (1-delta) * k7) * clShock +
                (delta * (k8 + k7 + k6) - (k7 + k8 + k9)),
              clLbs = clt * Cull_avg/1000000000)
  
  return(c(newCL_3, newCL_1))
}

newSL <- allStockShocks %>% transmute(Year = Year + 3, slStock = 0, slLbs = 0)

#### Here I am constructing the supply of fed cattle three periods ahead. Note that this is approximation. 
#### When I compare these numbers with the observed ones, these are a bit high. This comes from:
#### 1. We incorporated a gaussian shock, 2. The storage approximation comes into play as well.
#### Maybe I need to fit a model to get some coefficient for the supply of fed cattle three periods ahead.

#### 0.22 comes from the fact that approximately a maximum of 22% of the progeny is added to the breeding stock

newSL_1 <- allStockShocks %>%
  transmute(Year = Year+1, slt = (g - 0.22 * g) * lag(K,2) * lag(slShock,1) + 
              (1 - 0.22 * g) * g * delta * (lag(K,2) - (g - 0.22 * g) * lag(K,3) - 
                                              (lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2))),
            slLbs = slt * Slaughter_avg/1000000000)

newSL_3 <- allStockShocks %>%
  transmute(Year = Year+3, slt = (g - 0.22 * g) * K * slShock +
              (1 - 0.22 * g) * g * delta * (K - (g - 0.22 * g) * lag(K) - (k9 + (1-delta) * k8 + (1-delta) * k7)),
            slLbs = slt * Slaughter_avg/1000000000)



### NOTE: we did not add any imports or exports in constructing the fed cattle production.
#### CHECK THE ABOVE AGAIN!!!!! 
#### NEW NOTE: I reconstructed the total supply of the fed cattle and cull cows. This includes 
#### production this period with shock and storage.


# newCL <- allStockShocks %>% transmute(Year = Year + 3, clStock = 0, clLbs = 0)


#### Since the production of fed cattle is computed for three periods ahead, we construct the production of cull 
#### cows in the similar fashion

newCL_1 <- allStockShocks %>%
  transmute(Year = Year + 1, clt = (k9 + (1-delta) * k8 + (1-delta) * k7) * clShock +
              (delta * (k8 + k7 + k6) - (k7 + k8 + k9)),
            clLbs = clt * Cull_avg/1000000000)

newCL_3 <- allStockShocks %>%
  transmute(Year = Year + 3, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock +
              (delta^2) * (delta * (k6 + k5 + k4) - (k5 + k6 + k7)),
            clLbs = clt * Cull_avg/1000000000)

# newCL <- allStockShocks %>%
#   transmute(Year = Year + 3, 
#             clt = (k9 + (1-delta) * k8 + (1-delta) * k7) * clShock * lead(clShock) * lead(clShock,2) + 
#               (delta * k8 + delta * (1-delta) * (k7 + k6)) * lead(clShock) * lead(clShock,2) + 
#               (delta^2 * k7 + delta^2 * (1-delta) * (k6 + k5)) * lead(clShock,2) + 
#               (delta^3 * k6 + delta^3 * (1-delta) * (k5 + k4)),
#             clLbs = clt * clDressed/1000000000)

# newCL <- allStockShocks %>%
#   transmute(Year = Year + 2, 
#             clt = (lead(k9) + (1-delta) * (lead(k8) + lead(k7))) * lead(clShock) + 
#               delta * (lead(k8) + (1-delta) * (lead(k7) + lead(k6))),
#             clLbs = clt * clDressed/1000000000)

# clEstAge <- allStockShocks %>% filter(Year > 1995) %>% transmute(
#   Year = Year, clHead = clHead, clLbsEst = clHead * clDressed/1000000000)

cornPrice <- pcorn
cullCowsProd_3 <- newCL_3 %>% transmute(Year = Year, cullCows = clLbs)
fedCattleProd_3 <- newSL_3 %>% transmute(Year = Year, fedCattle = slLbs)

cullCowsProd_1 <- newCL_1 %>% transmute(Year = Year, cullCows = clLbs)
fedCattleProd_1 <- newSL_1 %>% transmute(Year = Year, fedCattle = slLbs)

# adjFactor_R <- adjFactor %>% mutate(Year = Year + 3)
# 
# cullCowsProd_adj <- merge(cullCowsProd,adjFactor_R) %>% mutate(cullCows = 
#                                                                  cullCows * AdjFactor) %>% select(Year, cullCows)
# 
# fedCattleProd_adj <- merge(fedCattleProd,adjFactor_R) %>% mutate(fedCattle = 
#                                                                    fedCattle * AdjFactor) %>% select(Year, fedCattle)
# 
# prod_CornP <- merge(merge(fedCattleProd_adj, cullCowsProd_adj),cornPrice) %>% drop_na()

prod_CornP <- merge(merge(fedCattleProd_1, cullCowsProd_1),cornPrice) %>% drop_na()


### Here I am generating the shocks again such that when we merge all the dataframes we have enough data.
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

# fedCattleProd <- supp_sl %>% transmute(Year = Year, fedCattle = Bill_meatLb_sl)
# cullCowsProd <- supp_cl %>% transmute(Year = Year, cullCows = Bill_meatLb_cl)


#### NOTE: We constructed fed cattle supply and cull cow supply for three years ahead which includes gaussian shocks as well. 
#### Although we are using the data of three years ahead, since we are using all the nodes of both fed cattle, and cull cows
#### supply the price is right. DO NOT GET CONFUSED!

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

###### The following function returns the normalized nodes
normalizedNodes <- function(d){
  z <- (2 * d - max(d) - min(d))/(max(d) - min(d))
  return(z)
}

#### For testing purposes I use n = 5 for now. 
chebNodesN <- 5

# supp_sl_adj_N <- supp_sl_adj %>% select(Year, fedCattle = Bill_meatLb_sl)
# supp_cl_adj_N <- supp_cl_adj %>% select(Year, cullCows = Bill_meatLb_cl)

# stateVarDemand <- merge(supp_sl_adj_N, supp_cl_adj_N) %>% transmute(Year = Year, demand = fedCattle + cullCows)

# stateVarDemand <- merge(fedCattleProd, cullCowsProd) %>% transmute(Year = Year, demand = fedCattle + cullCows)

stateVarDemand <- totalDisappeared %>% transmute(Year = Year, demand = total_meat_bill)

stateVariablesList <- list(cornPrice, cullCowsProd, fedCattleProd, demandShockGaussian1,
                           slSupplyShockGaussian1, clSupplyShockgaussian1, stateVarDemand)
# stateVariablesList <- list(cornPrice, supp_sl_adj_N, supp_cl_adj_N, demandShockGaussian1, 
#                            slSupplyShockGaussian1, clSupplyShockgaussian1, stateVarDemand)

stateVars <- Reduce(function(...) merge(...), stateVariablesList) %>% drop_na()

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

# cull_cartesian <- crossing(corn_nodes, cull_nodes, dshock_nodes, clShock_nodes) %>% as.data.frame()
# fed_cartesian <- crossing(corn_nodes, fed_nodes, dshock_nodes, slShock_nodes) %>% as.data.frame()

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

#### I also create normalized nodes i.e., all the nodes are in [-1,1]
cornNormalizedNodes <- normalizedNodes(d = corn_nodes)
cullNormalizedNodes <- normalizedNodes(d = cull_nodes)
fedNormalizedNodes <- normalizedNodes(d = fed_nodes)
dshockNormalizedNodes <- normalizedNodes(d = dshock_nodes)

names(cornNormalizedNodes) <- "cornNormNodes"
names(cullNormalizedNodes) <- "cullNormNodes"
names(fedNormalizedNodes) <- "fedNormNodes"
names(dshockNormalizedNodes) <- "dShockNormNodes"

cull_cartesianNormalized <- crossing(cornNormalizedNodes, cullNormalizedNodes, dshockNormalizedNodes) %>% as.data.frame()
fed_cartesianNormalized <- crossing(cornNormalizedNodes, fedNormalizedNodes, dshockNormalizedNodes) %>% as.data.frame()




#### Now I have to write code to generate price series using the interpolation matrix, coefficient vector, 
#### and actual prices. This needs a lot of work. 

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

###### optPriceFunction returns the price for the passed supply and demand numerics.

# optPriceFunction<- function(p, sl, cl, A, Eps, B, hc_discounted){
# 
#   ps <- p[1]
#   pc <- p[2]
#   
#   Eps3 <- p[3]
#   
#   # Eps3 <- Eps
#   
#   ##### Here I am trying to compute the discounted holding costs from the Naive expectations formulation.
#   ##### This could be not the correct way of doing (since I promised rational expectations) but this is the best we can do
#   # hc_new <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1)))
#   # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
#   # B <- ps - g * (beta^3) * Eps3 + hc_discounted
#   ####### THE ABOVE IS NOT WORKING. CONVERGING VERY QUICKLY #####
#   
#   
#   F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
# 
#   F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
# 
#   F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
# 
#   F <- F1^2 + F2^2 + F3^2
#   
#   # F <- F1^2 + F2^2
# 
#   return(F)
# 
# }

optPriceFunction<- function(p, sl, cl, A, B, hc_discounted){
  
  ps <- p[1]
  pc <- p[2]
  
  Eps3 <- p[3]
  Epc1 <- p[4]
  
  # hc_new <- hc
  # hc_discounted <- hc_dis
  
  ##### Here I am trying to compute the discounted holding costs from the Naive expectations formulation.
  ##### This could be not the correct way of doing (since I promised rational expectations) but this is the best we can do
  # hc_new <- (((g * (beta^3) * ps) + (beta - 1) * pc)/(1 + g * beta * (gamma0 + beta * gamma1)))
  # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
  # B <- ps - g * (beta^3) * Eps3 + hc_discounted
  ####### THE ABOVE IS NOT WORKING. CONVERGING VERY QUICKLY #####
  
  # hc_new <- (pc - beta * Epc1 + g * (beta^3) * Eps3) * (1/(1 + beta * (g * gamma0 + beta * g * gamma1)))
  # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
  # B <- ps - g * (beta^3) * Eps3 + hc_discounted
  
  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  
  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  # F <- F1^2 + F2^2 + F3^2
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  # F <- F1^2 + F2^2
  
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

# sl_quant_SM <- fedCattleProd_adj %>% transmute(Year = Year, slSM = fedCattle)
# cl_quant_SM <- cullCowsProd_adj %>% transmute(Year = Year, clSM = cullCows)

sl_quant_SM <- fedCattleProd_1 %>% transmute(Year = Year, slSM = fedCattle)
cl_quant_SM <- cullCowsProd_1 %>% transmute(Year = Year, clSM = cullCows)

# sl_quant <- supp_sl_adj_N %>% transmute(Year = Year, sl = fedCattle)
# cl_quant <- supp_cl_adj_N %>% transmute(Year = Year, cl = cullCows)

sl_quant <- supp_sl %>% transmute(Year = Year, sl = Bill_meatLb_sl)
cl_quant <- supp_cl %>% transmute(Year = Year, cl = Bill_meatLb_cl)

# dShocks <- stateVars %>% select(Year, Shock)

# sl_quantObs <- supp_sl_adj %>% transmute(Year = Year, slO = Bill_meatLb_sl)
# 
# 
# NetIMPORTS <- quantities_prices_capK %>% transmute(Year = Year, NET = (Imports - Exports) * Slaughter_avg/ 1000000000)
# 
# merge(sl_quant, sl_quantObs) %>% na.omit() %>% ggplot(aes(x=Year))+ geom_line(aes(y=sl,color="sl CSM")) + geom_point(aes(y=sl,color="sl CSM")) +
#   geom_line(aes(y=slO, color="sl OBS")) + geom_point(aes(y=slO, color="sl OBS")) +
#   labs(x="Year", y="Meat (in billion pounds)", colour = "") + theme_classic() 


# cl_quantObs <- supp_cl_adj %>% transmute(Year = Year, clO = Bill_meatLb_cl)


# slQuantitiesMerge <- merge(sl_quant, sl_quantObs) %>% na.omit() %>% mutate(diff = slO-sl)
# clQuantitiesMerge <- merge(cl_quant_adj, cl_quantObs) %>% na.omit() %>% mutate(diff = clO-cl)

A_quant <-  totalDisappearedNew  %>% transmute(Year = Year, A = total_meat_bill)

# A_quant <- merge(A_quant, dShocks) %>% mutate(A = A * lag(Shock)) %>% select(Year, A)

quantList <- list(A_quant, sl_quant_SM, cl_quant_SM, sl_quant, cl_quant)

quantities <-  Reduce(function(...) merge(...), quantList) %>% drop_na()

hcosts <- prices_costs %>% select(Year, hc)

price_sl_cl <- prices_quant %>% select(Year, ps , pc)
price_sl_cl_hc <- merge(price_sl_cl, hcosts)

imports_exports <- merge(imports_temp, exports_temp)

variablesList <- list(price_sl_cl_hc, capK, dressedWeights_sl_cl, imports_exports, quantities, adjFactor)

quantities_prices_capK <- Reduce(function(...) merge(...), variablesList) %>% drop_na() %>% filter(Year>1994)

################################### IMPORTANT ##################################
####### We have the constructed quantities and shocks from 1998 to 2017 ########
####### So the prices we use are from 1998 to 2017. ############################
################################################################################


# valueFunction <- function(cornNode, cullCowNode, dShockNode, fedCattleNode, pCorn, TSCull, dShock, TSFed){

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











############################### ALGORITHM ########################

for(i in 1:nrow(quantities_prices_capK)){
  
  # i <- 1
  ### Here we get the observed quantities. For fed production and cull production these are estimated production 3 years ahead
  A <- quantities_prices_capK$A[i] ## Note: Although I am assigning the total demand to variable here, I am using the
  #                                  ## fed cattle production node and cull cow production node with demand shock to get 
  #                                  ## the total demand for that particular node. 
  # sl <- quantities_prices_capK$slSM[i]
  # cl <- quantities_prices_capK$clSM[i]
  
  sl <- quantities_prices_capK$slSM[i]
  cl <- quantities_prices_capK$clSM[i]
  
  adjFac <- A/(sl+cl)
  
  #### Here I am trying another route. Take mean/median of the past prices and use it as the starting price for optimization
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
        if( round(checkTol[k-1,3],3) < 0.001 && round(checkTol[k-1,4],3) < 0.001){
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
        
        # slShare_t <- (exp((mu_Tilde - ((ps_old - pc_old))/phi)/s_Tilde))
        
        sl_node <- (fedCattleNode) * adjFac
        cl_node <- (cullCowNode) * adjFac
        
        
        ################################################ CHANGE TO THIS AND SEE ####################     
        A_node <- (sl_node + cl_node) * (dShockNode)
        
        
        
        # A_node <- (fedCattleNode + cullCowNode) * (dShockNode)
        
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
        
        ps_lo <- ps_o  - 0.35
        pc_lo <- pc_o - 0.4
        
        ps_up <- ps_o + 0.10929
        pc_up <- pc_o + 0.080153
        
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
        # if(j==1){
        ps_expected <- sum(as.numeric(ps_o) * fedMeshCheb)
        pc_expected <- sum(as.numeric(pc_o) * cullMeshCheb)
        # }
        
        
        ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
        ### we use the euqlity of that to get the holding costs. From my first observation this is greater than the naive 
        ### expectations holding costs. Because we have the expected price in the equality.
        hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expected + g * (beta^3) * ps_expected - pc_o)
        
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
        
        sl_node <- slNodes[j,i]
        cl_node <- clNodes[j,i]
        A_node <- (sl_node + cl_node) * dShockNode
        # A_node <- A_nodes[j,i]
        
        A_nodes[j,i] <- A_node
        
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
          
          ps_lo <- ps_n  - 0.35
          pc_lo <- pc_n - 0.4
          
          ps_up <- ps_n + 0.10929
          pc_up <- pc_n + 0.080153
          
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
        slNodes_eq[j,i] <- slNodes_eq[j,i] * adjFac
        clNodes_eq[j,i] <- clNodes_eq[j,i] * adjFac
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
        
        ps_lo <- ps_n  - 0.35
        pc_lo <- pc_n - 0.4
        
        ps_up <- ps_n + 0.10929
        pc_up <- pc_n + 0.080153
        
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
        A_node <- A_nodes[j,i] * dShockNode
        
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
      
      slNodes[,i] <- D_slPsPc[,i]
      clNodes[,i] <- D_clPsPc[,i]
      
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
      if( all(round(checkTol[k-1,],2) == round(checkTol[k,],2)) ){
        if( all(round(checkTol[k-2,],2) == round(checkTol[k-1,],2)) ){
          if( all(round(checkTol[k-3,],2) == round(checkTol[k-2,],2)) ){
            break
          }
        }
      }
      
    }
    
  }
  
}






