
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

### But first I will construct the nodes for corn price, fed cattle supply, and cull cow supply.

#### I need to construct production shock. supp_sl and supp_cl are the estimated fed cattle and cull cow supply. I need to get the 
#### observed supply as well. Isn't it the animals slaughtered? If yes, then we are assuming that the supply equals demand.
#### For now that is what I am doing i.e., assuming that the animal slaughtered as the observed supply and the constructed supply is 
#### the estimated supply. (After talking with Lee and Chad, I have decided the slaughtered data as the observed supply)

################### Here I am retracing the steps by writing the code again

# State variables: corn price, demand shock, cull cows production with gaussian shocks, and fed cattle production with gaussian shock.
## NOTE: the demand shock is also gaussian.

##### I will generate the demand shocks. For this I need the observed demand and the constructed demand. 
##### The observed demand is from derived demand from data. 
##### Constructed demand would be animals slaughtered for consumption.

### Observed derived demand would be the sum of Exports and Domestic Consumption from demandBeef dataframe
### Constructed demand would be the total animals slaughtered for consumption purposes. I get this from slaughtered data. The dataframe
### totalDisappeared has the data.

obsDemand <- demandBeef %>% transmute(Year = Year, demandObs = Exports + `Domestic Consumption`)
rownames(obsDemand) <- 1:nrow(obsDemand)
estDemand <- totalDisappeared %>% transmute(Year = Year, demandEst = total_meat_bill)
demandShock <- merge(obsDemand, estDemand) %>% mutate(dShock = demandObs/demandEst)

demandShockGaussian <- demandShock %>% transmute(Year = Year, Shock = 0)

# demandConstructed <- demandShockGaussian %>% transmute(Year = Year + 2, )

## Now i generate gaussian shock which is consistent with historical data.
## I use the standard deviation of historical data to construct the gaussian random variables. Here the mean is 1
set.seed(1)
demandShockG <- rnorm(n = nrow(demandShock), mean = 1, sd = std(demandShock$dShock))
demandShockGaussian$Shock <- demandShockG
demandShockGaussian$Year <- as.double(demandShockGaussian$Year)


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

#### Here we construct the cull cows production shock
obsEst_cl_Supply <- merge(cullCowSupply_obs, supp_cl) %>% transmute(Year = Year, cl_obs = cl_obs, cl_est = Bill_meatLb_cl,
                                                                    clShock = cl_obs/cl_est)
clSupplyShockgaussian <- obsEst_cl_Supply %>% transmute(Year = Year, clShock = 0)

set.seed(4)
clSupply_Shock <- rnorm(n = nrow(prices_quant), mean = 1, sd = std(obsEst_cl_Supply$clShock))
clSupplyShockgaussian$clShock <- clSupply_Shock

#### I am merging all the supply and demand shocks
allShocks <- merge(merge(demandShockGaussian, slSupplyShockGaussian), clSupplyShockgaussian)


sl_stock <- supp_sl %>% transmute(Year = Year, sl_est = Bill_meatLb_sl, slHead = Slaughter)
cl_stock <- supp_cl %>% transmute(Year = Year, cl_est = Bill_meatLb_cl, clHead = Cull)

dataList <- list(Stock, sl_stock, cl_stock, slSupplyShockGaussian, clSupplyShockgaussian, dressedWeights_sl_cl, imports_exports)

allStockShocks <- Reduce(function(...) merge(...), dataList)


#### Here I am writing functions that return the fed cattle and cull cow production.
#### Note: this includes the gaussian shocks we generated which has mean 1 and standard deviation according to the historical data

fedProduction <- function(stockShocks){
  newSL <- stockShocks %>%
    transmute(Year = Year+3, slt = (delta - 0.19) * K * slShock + 
                (1-0.19) * g * delta * (K - (delta - 0.19) * lag(K) - (k9 + (1-delta) * k8 + (1-delta) * k7)),
              slLbs = slt * Slaughter_avg/1000000000) 
  
  return(newSL)
}

cullProduction <- function(stockShocks){
  
  newCL <- stockShocks %>%
    transmute(Year = Year + 3, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock +
                (delta^2) * (delta * (k6 + k5 + k4) - (k5 + k6 + k7)),
              clLbs = clt * Cull_avg/1000000000)
  
  return(newCL)
}

newSL <- allStockShocks %>% transmute(Year = Year + 3, slStock = 0, slLbs = 0)

#### Here I am constructing the supply of fed cattle three periods ahead. Note that this is approximation. 
#### When I compare these numbers with the observed ones, these are a bit high. This comes from:
#### 1. We incorporated a gaussian shock, 2. The storage approximation comes into play as well.
#### Maybe I need to fit a model to get some coefficient for the supply of fed cattle three periods ahead.

newSL <- allStockShocks %>%
  transmute(Year = Year+3, slt = (delta - 0.19) * K * slShock + 
              (1-0.19) * g * delta * (K - (delta - 0.19) * lag(K) - (k9 + (1-delta) * k8 + (1-delta) * k7)),
            slLbs = slt * Slaughter_avg/1000000000) 
### NOTE: we did not add any imports or exports in constructing the fed cattle production.
#### CHECK THE ABOVE AGAIN!!!!! 
#### NEW NOTE: I reconstructed the total supply of the fed cattle and cull cows. This includes 
#### production this period with shock and storage.


newCL <- allStockShocks %>% transmute(Year = Year + 3, clStock = 0, clLbs = 0)


#### Since the production of fed cattle is computed for three periods ahead, we construct the production of cull 
#### cows in the similar fashion

newCL <- allStockShocks %>%
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

cornPrice <- prices_quant %>% select(Year, pcorn)
cullCowsProd <- newCL %>% transmute(Year = Year, cullCows = clLbs)
fedCattleProd <- newSL %>% transmute(Year = Year, fedcattle = slLbs)


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
chebNodesN <- 7

stateVariablesList <- list(cornPrice, cullCowsProd, fedCattleProd, demandShockGaussian, slSupplyShockGaussian, clSupplyShockgaussian)

stateVars <- Reduce(function(...) merge(...), stateVariablesList) %>% drop_na()

cornNodes <- chebyshevNodes(d = stateVars$pcorn, n = chebNodesN)
cullCowNodes <- chebyshevNodes(d = stateVars$cullCows, n = chebNodesN)
fedCattleNodes <- chebyshevNodes(d = stateVars$fedcattle, n = chebNodesN)
dShockNodes <- chebyshevNodes(d = stateVars$Shock, n = chebNodesN)
slShockNodes <- chebyshevNodes(d = stateVars$slShock, n = chebNodesN)
clShockNodes <- chebyshevNodes(d = stateVars$clShock, n = chebNodesN)

corn_nodes <- cornNodes %>% as.data.frame()
cull_nodes <- cullCowNodes %>% as.data.frame()
fed_nodes <- fedCattleNodes  %>% as.data.frame()
dshock_nodes <- dShockNodes %>% as.data.frame()
slShock_nodes <- slShockNodes %>% as.data.frame()
clShock_nodes <- clShockNodes %>% as.data.frame()

names(corn_nodes) <- "cornNodes"
names(cull_nodes) <- "cullNodes"
names(fed_nodes) <- "fedNodes"
names(dshock_nodes) <- "dShockNodes"
names(slShock_nodes) <- "slShockNodes"
names(clShock_nodes) <- "clShockNodes"

##### Cartesian product of the nodes
cull_cartesian <- crossing(corn_nodes, cull_nodes, dshock_nodes) %>% as.data.frame()
fed_cartesian <- crossing(corn_nodes, fed_nodes, dshock_nodes) %>% as.data.frame()

# cull_cartesian <- crossing(corn_nodes, cull_nodes, dshock_nodes, clShock_nodes) %>% as.data.frame()
# fed_cartesian <- crossing(corn_nodes, fed_nodes, dshock_nodes, slShock_nodes) %>% as.data.frame()

#### The following created chebyshev matrix containing chebyshev polynomials
cornChebyshevMatrix <- chebyshevMatrix(x = cornNodes, d = stateVars$pcorn, n = chebNodesN)
cullCowsChebyshevMatrix <- chebyshevMatrix(x = cullCowNodes, d = stateVars$cullCows, n = chebNodesN)
fedCattleChebyshevMatrix <- chebyshevMatrix(x = fedCattleNodes, d = stateVars$fedcattle, n = chebNodesN)
dShockChebyshevMatrix <- chebyshevMatrix(x = dShockNodes, d = stateVars$Shock, n = chebNodesN)

###### Here I am taking the tensor product to create interpolation matrix of grids. 
###### kron takes the kronecker tensor product of two matrices
##### For cull cows we use corn, cull cows production, and demand shock chebyshev matrices
##### For fed cattle we use corn, fed cattle production, and demand shock chebyshev matrices

cullInterpolationMatrix <- kron(kron(cornChebyshevMatrix, cullCowsChebyshevMatrix), dShockChebyshevMatrix)
fedCattleInterpolationMatrix <- kron(kron(cornChebyshevMatrix, fedCattleChebyshevMatrix), dShockChebyshevMatrix)


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
# optPriceFunction<- function(p, sl, cl, A, hc, Eps3)
# optPriceFunction<- function(p, sl, cl, A, B, Eps){
# 
#   ps <- p[1]
#   pc <- p[2]
#   hc <- p[3]
# 
#   # Eps3 <- Eps
#   
#   # hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc
# 
#   F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
# 
#   F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
# 
#   F3 <- B - ps - g * (beta^3) * Eps + 
#     ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc
# 
#   F <- F1^2 + F2^2 + F3^2
# 
#   return(F)
# 
# }

optPriceFunction<- function(p, sl, cl, A, Eps, B, hc_discounted){

  ps <- p[1]
  pc <- p[2]

  # Eps3 <- Eps

  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))

  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))

  # F3 <- B - ps - g * (beta^3) * Eps3 + hc_discounted

  # F <- F1^2 + F2^2 + F3^2
  
  F <- F1^2 + F2^2

  return(F)

}

###### optKFunction returns the optimal k_{3,t+1} and sum of k_{j,t+1} where j E [7,8,9]
optKFunction <- function(K, ps, pc, A){
  
  K1 <- K[1]
  K2 <- K[2]
  
  fed <- g * Stock_1t - K1 + imports - exports - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
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

sl_quant <- fedCattleProd %>% transmute(Year = Year, sl = fedcattle)
cl_quant <- cullCowsProd %>% transmute(Year = Year, cl = cullCows)

# sl_quantObs <- supp_sl_adj %>% transmute(Year = Year, slO = Bill_meatLb_sl)
# cl_quantObs <- supp_cl_adj %>% transmute(Year = Year, clO = Bill_meatLb_cl)


# slQuantitiesMerge <- merge(sl_quant, sl_quantObs) %>% na.omit() %>% mutate(diff = slO-sl)
# clQuantitiesMerge <- merge(cl_quant_adj, cl_quantObs) %>% na.omit() %>% mutate(diff = clO-cl)

A_quant <-  totalDisappearedNew  %>% transmute(Year = Year, A = total_meat_bill)

quantities <- merge(merge(A_quant,sl_quant), cl_quant)

hcosts <- prices_costs %>% select(Year, hc)

price_sl_cl <- prices_quant %>% select(Year, ps , pc)
price_sl_cl_hc <- merge(price_sl_cl, hcosts)

imports_exports <- merge(imports_temp, exports_temp)

variablesList <- list(quantities, price_sl_cl_hc, capK, dressedWeights_sl_cl, imports_exports)

quantities_prices_capK <- Reduce(function(...) merge(...), variablesList) %>% drop_na()

# quantities_prices_capK <- merge(merge(merge(merge(quantities, price_sl_cl_hc), capK),dressedWeights_sl_cl),imports_exports) %>% 
#   drop_na() 


valueFunction <- function(cornNode, cullCowNode, dShockNode, fedCattleNode, pCorn, TSCull, dShock, TSFed){
  
  prices_ps <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
  prices_pc <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
  
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
  
  maxIter <- 500
  
  fedPrice <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)
  cullPrice <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)
  fedProd <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)
  cullProd <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)
  
  c_cull_itr <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian), ncol=maxIter)
  c_fed_itr <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian), ncol=maxIter)
  
 ###### THINK ABOUT THE INDEXING PROPERLY. ARE YOU PREDICTING THE NEXT YEAR OR JUST USING THE SAME YEARS DATA TO 
 ###### ESTIMATE THE SAME NUMBERS? WE SHOULD BE ESTIMATING THE NEXT YEARS PRICES AND QUANTITIES.
 ###### When I assume the prices and quantities are for the subsequent year and compare them with naive and observed
 ###### Theres not much difference between naive and rational. However, this is with the normalized nodes. I think 
 ###### if I use the coefficients to get the price we might see some improvement.
  
  for(i in 1:nrow(quantities_prices_capK)){
  
    # i <- 1
    ### Here we get the observed quantities. For fed production and cull production these are estimated production 3 years ahead
    A <- quantities_prices_capK$A[i]
    sl <- quantities_prices_capK$sl[i]
    cl <- quantities_prices_capK$cl[i]
    
    #### Here I am trying another route. Take mean/median of the past prices and use it as the starting price for optimization
    ps <-   quantities_prices_capK$ps[i]
    pc <-   quantities_prices_capK$pc[i]
    hc <- quantities_prices_capK$hc[i]
    
    if(i > 1){
      if(quantities_prices_capK$ps[i] < quantities_prices_capK$ps[i-1]){
        ps <- (quantities_prices_capK$ps[i] + quantities_prices_capK$ps[i-1] + quantities_prices_capK$ps[i-2])/3
      }
      if(quantities_prices_capK$pc[i] < quantities_prices_capK$pc[i-1]){
        pc <- (quantities_prices_capK$pc[i] + quantities_prices_capK$pc[i-1] + quantities_prices_capK$pc[i-2])/3
      }
    }
    
    hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc
    
    K1t  <- quantities_prices_capK$K[i]
    k9 <- quantities_prices_capK$k9[i]
    k8 <- quantities_prices_capK$k8[i]
    k7 <- quantities_prices_capK$k7[i]
    
    slDressed <- quantities_prices_capK$Slaughter_avg[i]
    clDressed <- quantities_prices_capK$Cull_avg[i]
    
    
    #### For imports and exports I will take the mean/median of the past data.
    # importsObs <- median(quantities_prices_capK$Imports[1:i])
    # exportsObs <- median(quantities_prices_capK$Exports[1:i])
    
    importsObs <- quantities_prices_capK$Imports[i]
    exportsObs <- quantities_prices_capK$Exports[i]
    
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
    
    #### getting the parameters from the optParamFunction
    params_mu_s <- optParamFunction(sl = sl, cl = cl, ps = ps, pc = pc, thetas = c(1,1))
    
    mu_Tilde <- params_mu_s[1]
    s_Tilde <- params_mu_s[2]
    
    count <- 0
    
    c_old_cull <- matrix(data = 0, nrow = nrow(cullInterpolationMatrix), ncol = 1)
    c_old_fed <- matrix(data = 0, nrow = nrow(fedCattleInterpolationMatrix), ncol = 1)
    
    sl_obs <- sl
    cl_obs <- cl
    
    slD_obs <- A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
    clD_obs <- A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
    
    sl_itr <- 0
    cl_itr <- 0
    
    for(k in 1:maxIter){
      
        if(norm(c_cull - c_old_cull) < 0.001  && norm(c_fed - c_old_fed) < 0.001){
          break
        }
      
        #### NOTE: For i = 12 the following condition is true in the first iteration. So I change it from 0.01 to 0.001
        # if((slD_obs + clD_obs - sl_itr - cl_itr)^2 < 0.001){
        #   break
        # }
      
        count <- count + 1
        
        c_old_cull <- c_cull
        c_old_fed <- c_fed
        
        #### Here we are going through each node
        for (j in 1:nrow(cull_cartesian)) {
          
          # j <- 1
          #### Note: We don't have to normalize/need normalized nodes here. Because we are normalizing them when we are getting the 
          #### chebyshev matrix. See the function written to generate chebyshev matrix containing the chebyshev polynomials
          cornNode <- cull_cartesian$cornNodes[j]
          cullCowNode <- cull_cartesian$cullNodes[j]
          dShockNode <- cull_cartesian$dShockNodes[j]
          fedCattleNode <- fed_cartesian$fedNodes[j]
          
          pCorn <- stateVars$pcorn
          TSCull <- stateVars$cullCows
          dShock <- stateVars$Shock
          TSFed <- stateVars$fedcattle
          
          corn_ChebyshevMatrix <- chebyshevMatrix(x = cornNode, d = pCorn, n = chebNodesN)
          cullCows_ChebyshevMatrix <- chebyshevMatrix(x = cullCowNode, d = TSCull, n = chebNodesN)
          dShock_ChebyshevMatrix <- chebyshevMatrix(x = dShockNode, d = dShock, n = chebNodesN)
          fedCattle_ChebyshevMatrix <- chebyshevMatrix(x = fedCattleNode, d = TSFed, n = chebNodesN)
          
          cull_InterpolationMatrix <- kron(kron(corn_ChebyshevMatrix, cullCows_ChebyshevMatrix), dShock_ChebyshevMatrix)
          fedCattle_InterpolationMatrix <- kron(kron(corn_ChebyshevMatrix, fedCattle_ChebyshevMatrix), dShock_ChebyshevMatrix)
          
          pc_new <- cull_InterpolationMatrix %*% c_old_cull
          ps_new <- fedCattle_InterpolationMatrix %*% c_old_fed
          
          #### Here we apply the chebyshev node to solve the system of equations
          # sl_node <- fedCattleNode + imports - exports
          sl_node <- fedCattleNode  
          cl_node <- cullCowNode  
          A_node <- (sl_node + cl_node) * dShockNode
          
          #### getting the parameters from the optParamFunction
          params_mu_s <- optParamFunction(sl = sl_node, cl = cl_node, ps = ps_new, pc = pc_new, thetas = c(1,1))

          mu_Tilde <- params_mu_s[1]
          s_Tilde <- params_mu_s[2]
          
          ### Here we get the price for the observed supply and demand of fed and cull cows
          p <- c(ps_new, pc_new)
          
          #### I am setting the lower and upper boundaries for fed cattle and cull cows price. 
          #### My rational for this is: we would like to achieve global maximum/minumum. Sometimes the point estimate 
          #### jumps to some local maxima/minima and do not move from there. We are making sure that the prices are within
          #### the boundaries. 
          #### NEED MORE EXPLANATION? 
          
          ps_lo <- ps - 0.4
          pc_lo <- pc - 0.4
          
          ps_up <- ps  + 0.5
          pc_up <- pc  + 0.5
          
          #### Here we are making sure the lower bound for the prices isn't negative
          if(ps_lo < 0){
             ps_lo <- ps
          }

          if(pc_lo < 0){
            pc_lo <- pc
          }
          
          lo <- c(ps_lo, pc_lo) ## Here we set the lower limit for the price
          up <- c(ps_up, pc_up) # Here we set the upper limit for the price. I am assuming the price per pound of meat won't go larger than a dollar
          
          # ps_expected <- 
          ps_expected <- c(ps_new)  * slWeights
          
          B <- ps_new - g * (beta^3) * ps_expected + hc_discounted
          
          estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node,B = B, hc_discounted = hc_discounted, 
                          Eps = ps_new, lower = lo, upper = up)
          # B = B, hc_discounted = hc_discounted, Eps = ps_new, lower = lo, upper = up
          ps1 <- estP$par[1]
          pc1 <- estP$par[2]
          
          ### From the following we get the quantities of k_{3,t+1} and sum(k_{j,t+1}) where j in {7,8,9} which are storage 
          K <- c(0,0)

          estK <- BBoptim(par = K, fn = optKFunction, ps = ps1, pc = pc1, A = A_node)

          k_3t1 <- estK$par[1]
          k_7_10t1 <- estK$par[2]

          sl1 <- (g * Stock_1t - k_3t1 + imports - exports)
          cl1 <- (k_9t + k_8t + k_7t - k_7_10t1)
          
          #### getting the parameters from the optParamFunction
          # params_mu_s <- optParamFunction(sl = sl1, cl = cl1, ps = ps1, pc = pc1, thetas = c(1,1))
          # 
          # mu_Tilde <- params_mu_s[1]
          # s_Tilde <- params_mu_s[2]
          
          
          ## Backing the price for the optimal quantities determined above
          # p <- c(ps1, pc1)
          # estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl1, cl = cl1, A = A_node)
          # ps1 <- estP$par[1]
          # pc1 <- estP$par[2]
          # ps <- ps1
          # pc <- pc1
          
          prices_ps[j,i] <- ps1
          prices_pc[j,i] <- pc1
          
          k3t1[j,i] <- k_3t1
          kjt1[j,i] <- k_7_10t1
          slNew[j,i] <- sl1
          clNew[j,i] <- cl1
          
          fedPrice[[i]][j,k] <- ps1
          cullPrice[[i]][j,k] <- pc1
          
          fedProd[[i]][j,k] <- sl1
          cullProd[[i]][j,k] <- cl1
          
          # slD[j,i] <- A_node * ((exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))))
          # clD[j,i] <- A_node * (1/(1+ exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde)))
            
          # # ps_new1 <- as.matrix(x = rep(ps, 125), ncol = 1)
          # # pc_new1 <- as.matrix(x = rep(pc, 125), ncol = 1)
          # # 
          # # c_cull  <- solve(cullInterpolationMatrix) %*% pc_new1
          # # c_fed  <- solve(fedCattleInterpolationMatrix) %*% ps_new1
          # 
          # # c_cull1[,j] <- c_cull
          # # c_fed1[,j] <- c_fed
          # 
          # # c_old_cull <- c_cull
          # # c_old_fed <- c_fed
          
        }
        
        c_fed  <- solve(fedCattleInterpolationMatrix) %*% prices_ps[,i]
        c_cull <- solve(cullInterpolationMatrix) %*% prices_pc[,i]
        
        c_cull_itr[[i]][,k] <- c_cull
        c_fed_itr[[i]][,k] <- c_fed
        
        
        cat("\n norm of old and new fed coefficients: ", norm(c_fed - c_old_fed))

        cat("\n norm of old and new cull coefficients: ", norm(c_cull - c_old_cull))
        
        # cat("\n diff: ", (sl_obs + cl_obs - slD_obs - clD_obs)^2)
        
        
        sl_itr <- mean(slD[,i])
        cl_itr <- mean(clD[,i])
        
        # sl_obs <- mean(slNew[,i])
        # cl_obs <- mean(clNew[,i])
        # 
        # sl_itr <- mean(slNew[,i])
        # cl_itr <- mean(clNew[,i])
        
    }
    
    c_cull_opt[[i]] <- c_cull
    c_fed_opt[[i]] <- c_fed
    
  }
  
  
  
  
  cornNode <- cull_cartesian$cornNodes[j]
  cullCowNode <- cull_cartesian$cullNodes[j]
  dShockNode <- cull_cartesian$dShockNodes[j]
  fedCattleNode <- fed_cartesian$fedNodes[j]
  
  pCorn <- stateVars$pcorn
  TSCull <- stateVars$cullCows
  dShock <- stateVars$Shock
  TSFed <- stateVars$fedcattle

  
  corn_ChebyshevMatrix <- chebyshevMatrix(x = cornNode, d = pCorn, n = chebNodesN)
  cullCows_ChebyshevMatrix <- chebyshevMatrix(x = cullCowNode, d = TSCull, n = chebNodesN)
  dShock_ChebyshevMatrix <- chebyshevMatrix(x = dShockNode, d = dShock, n = chebNodesN)
  fedCattle_ChebyshevMatrix <- chebyshevMatrix(x = fedCattleNode, d = TSFed, n = chebNodesN)
  
  cull_InterpolationMatrix <- kron(kron(corn_ChebyshevMatrix, cullCows_ChebyshevMatrix), dShock_ChebyshevMatrix)
  fedCattle_InterpolationMatrix <- kron(kron(corn_ChebyshevMatrix, fedCattle_ChebyshevMatrix), dShock_ChebyshevMatrix)
  
  pc_new <- cull_InterpolationMatrix %*% c_old_cull
  ps_new <- fedCattle_InterpolationMatrix %*% c_old_fed
  
  p <- c(ps_new, pc_new)
  
  estP <- BBoptim(par = p, fn = optPriceFunction)
  
  ps_new <- estP$par[1]
  pc_new <- estP$par[2]
  
  Stock_1t <- (Kt1*slDressed)/1000000000
  k_9t <- (k9*clDressed)/1000000000
  k_8t <- (k8*clDressed)/1000000000
  k_7t <- (k7*clDressed)/1000000000
  
  K <- c(0,0)
  ps <- ps_new
  pc <- pc_new

  estK <- BBoptim(par = K, fn = optKFunction)

  k_3t1 <- estK$par[1]
  k_7_10t1 <- estK$par[2]
  
  sl <- g * Stock_1t - k_3t1
  cl <- k_9t + k_8t + k_7t - k_7_10t1
  
  p <- c(ps, pc)
  estP <- BBoptim(par = p, fn = optPriceFunction)
  ps <- estP$par[1]
  pc <- estP$par[2]

  prices_ps[j,i] <- ps
  prices_pc[j,i] <- pc
  k3t1[j,i] <- k_3t1
  kjt1[j,i] <- k_7_10t1
  
  ps_new1 <- as.matrix(x = rep(ps, 125), ncol = 1)
  pc_new1 <- as.matrix(x = rep(pc, 125), ncol = 1)
  
  c_cull  <- solve(cullInterpolationMatrix) %*% pc_new1
  c_fed  <- solve(fedCattleInterpolationMatrix) %*% ps_new1
  
  c_cull1[,j] <- c_cull
  c_fed1[,j] <- c_fed
  
  norm(c_old_cull - c_cull)
  norm(c_old_fed - c_fed)
  
  c_old_cull <- c_cull
  c_old_fed <- c_fed 
  
  
  
  
  
  return(c(prices, Ks))
  
  # return(c(ps_new, pc_new))
  
}
 







 






