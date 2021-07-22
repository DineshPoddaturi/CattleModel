
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

sl_stock <- supp_sl %>% transmute(Year = Year, sl_est = Bill_meatLb_sl, slHead = Slaughter)
cl_stock <- supp_cl %>% transmute(Year = Year, cl_est = Bill_meatLb_cl, clHead = Cull)

dataList <- list(Stock, sl_stock, cl_stock, slSupplyShockGaussian, clSupplyShockgaussian, dressedWeights_sl_cl, imports_exports)

allStockShocks <- Reduce(function(...) merge(...), dataList)

newSL <- allStockShocks %>% transmute(Year = Year + 2, slStock = 0, slLbs = 0)

newSL <- allStockShocks %>%
  transmute(Year = Year + 2, slt = (g - 0.19 ) * lag(K) * slShock ,
            slLbs = slt * slDressed/1000000000) 
### NOTE: we did not add any imports or exports in constructing the fed cattle production. 

# slEstAge <- allStockShocks %>% filter(Year > 1995) %>% transmute(
#   Year = Year, slHead = slHead - Imports + Exports , slLbsEst = slHead * slDressed/1000000000)

newCL <- allStockShocks %>% transmute(Year = Year + 2, clStock = 0, clLbs = 0)

newCL <- allStockShocks %>%
  transmute(Year = Year + 2, clt = (delta^2) * (k7 + (1-delta) * k6 + (1-delta) * k5) * clShock * lead(clShock),
            clLbs = clt * clDressed/1000000000)

# clEstAge <- allStockShocks %>% filter(Year > 1995) %>% transmute(
#   Year = Year, clHead = clHead, clLbsEst = clHead * clDressed/1000000000)

cornPrice <- prices_quant %>% select(Year, pcorn)
cullCowsProd <- newCL %>% transmute(Year = Year, cullCows = clLbs)
fedCattleProd <- newSL %>% transmute(Year = Year, fedcattle = slLbs)


#### NOTE: We constructed fed cattle supply and cull cow supply for two years ahead which includes gaussian shocks as well. 
#### Although we are using the data of two years ahead, since we are using all the nodes of both fed cattle, and cull cows
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

stateVars <- merge(merge(merge(cornPrice, cullCowsProd),fedCattleProd),demandShockGaussian) %>% drop_na()

cornNodes <- chebyshevNodes(d = stateVars$pcorn, n = chebNodesN)
cullCowNodes <- chebyshevNodes(d = stateVars$cullCows, n = chebNodesN)
fedCattleNodes <- chebyshevNodes(d = stateVars$fedcattle, n = chebNodesN)
dShockNodes <- chebyshevNodes(d = stateVars$Shock, n = chebNodesN)

corn_nodes <- cornNodes %>% as.data.frame()
cull_nodes <- cullCowNodes %>% as.data.frame()
fed_nodes <- fedCattleNodes  %>% as.data.frame()
dshock_nodes <- dShockNodes %>% as.data.frame()

names(corn_nodes) <- "cornNodes"
names(cull_nodes) <- "cullNodes"
names(fed_nodes) <- "fedNodes"
names(dshock_nodes) <- "dShockNodes"

##### Cartesian product of the nodes
cull_cartesian <- crossing(corn_nodes, cull_nodes, dshock_nodes) %>% as.data.frame()
fed_cartesian <- crossing(corn_nodes, fed_nodes, dshock_nodes) %>% as.data.frame()

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

#### Now I have to write code to generate price series using the interpolation matrix, coefficient vector, 
#### and actual prices. This needs a lot of work. 

#### optParamFunction returns the parameters mu_tilde and s_tilde
optParamFunction <- function(sl, cl, ps, pc, thetas){
  
  s <- sl
  c <- cl
  
  # if(sl ==0){
  #   sl <- 1
  # }
  # if(cl == 0){
  #   cl <- 1
  # }
  
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

###### optPriceFunction returns the price for the passed supply and demand numerics.
# optPriceFunction<- function(p, sl, cl, A, hc, Eps3)
optPriceFunction<- function(p, sl, cl, A){
  
  ps <- p[1]
  pc <- p[2]
  
  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))

  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  # F3 <- ps - g * (beta^3) * Eps3 + ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc 
  
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

K_jt <- Stock %>% select(Year, k7, k8, k9, k10)

K_1t <- Stock %>% transmute(Year = Year+1, K)

capK <- merge(K_1t, K_jt)

sl_quant <- fedCattleProd %>% transmute(Year = Year, sl = fedcattle)
cl_quant <- cullCowsProd %>% transmute(Year = Year, cl = cullCows)
A_quant <-  totalDisappearedNew  %>% transmute(Year = Year, A = total_meat_bill)

quantities <- merge(merge(A_quant,sl_quant), cl_quant)

hcosts <- prices_costs %>% select(Year, hc)

price_sl_cl <- prices_quant %>% select(Year, ps , pc)
price_sl_cl_hc <- merge(price_sl_cl, hcosts)

imports_exports <- merge(imports_temp, exports_temp)

quantities_prices_capK <- merge(merge(merge(merge(quantities, price_sl_cl_hc), capK),dressedWeights_sl_cl),imports_exports) %>% drop_na()


valueFunction <- function(cornNode, cullCowNode, dShockNode, fedCattleNode, pCorn, TSCull, dShock, TSFed){
  
  prices_ps <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  prices_pc <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  
  k3t1 <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  kjt1 <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  
  slNew <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  clNew <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  
  slD <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  clD <- matrix(data = 0,nrow=125,ncol = nrow(quantities_prices_capK))
  
  c_cull1 <- matrix(data=0, nrow = 125, ncol = 1)
  c_fed1 <- matrix(data=0, nrow = 125, ncol = 1)
  
  c_cull_opt <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=125, ncol=1)
  c_fed_opt <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=125, ncol=1)
  
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
  
    i <- 1
    ### Here we get the observed quantities
    A <- quantities_prices_capK$A[i]
    sl <- quantities_prices_capK$sl[i]
    cl <- quantities_prices_capK$cl[i]
    
    ps <- quantities_prices_capK$ps[i]
    pc <- quantities_prices_capK$pc[i]
    hc <- quantities_prices_capK$hc[i]
    
    K1t  <- quantities_prices_capK$K[i]
    k9 <- quantities_prices_capK$k9[i]
    k8 <- quantities_prices_capK$k8[i]
    k7 <- quantities_prices_capK$k7[i]
    
    slDressed <- quantities_prices_capK$Slaughter_avg[i]
    clDressed <- quantities_prices_capK$Cull_avg[i]
    
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
    # meanPricePS <- 0
    # meanPricePC <- 0
    # slOBS <- 0
    # clOBS <- 0
    # slDOBS <- 0
    # clDOBS <- 0
    # coefSLDIFF <- 0
    # coefCLDIFF <- 0
    # suppDemandDIFF <- 0
    
    c_old_cull <- matrix(data = 0, nrow = 125, ncol = 1)
    c_old_fed <- matrix(data = 0, nrow = 125, ncol = 1)
    
    sl_obs <- sl
    cl_obs <- cl
    
    slD_obs <- A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
    clD_obs <- A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
    
    # while( norm(c_cull - c_old_cull) > 0.001  && norm(c_fed - c_old_fed) > 0.001 ){
    # while((sl_obs + cl_obs - slD_obs - clD_obs)^2 > 0.01){
    
    # maxIter <- 500
    
    for(k in 1:maxIter){
      
        if(norm(c_cull - c_old_cull) < 0.001  && norm(c_fed - c_old_fed) < 0.001){
          break
        }
        count <- count + 1
        c_old_cull <- c_cull
        c_old_fed <- c_fed
        
        #### Here we are going through each node
        for (j in 1:dim(cullInterpolationMatrix)[1]) {
          
          # j <- 1
          # cornNode <- cull_cartesianNormalized$cornNormNodes[j]
          # cullCowNode <- cull_cartesianNormalized$cullNormNodes[j]
          # dShockNode <- cull_cartesianNormalized$dShockNormNodes[j]
          # fedCattleNode <- fed_cartesianNormalized$fedNormNodes[j]
          
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
          sl_node <- fedCattleNode + imports - exports
          cl_node <- cullCowNode
          A_node <- A * dShockNode
          
          # * dShockNode
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
          
          ps_lo <- ps + 0.02262 
          pc_lo <- pc - 0.003938
          
          ps_up <- ps + 0.23644
          pc_up <- pc + 0.192417 
          
          #### Here we are making sure the lower bound for the prices isn't negative
          if(ps_lo < 0){
             ps_lo <- 0
          }
          
          if(pc_lo < 0){
            pc_lo <- 0
          }
          
          lo <- c(ps_lo, pc_lo) ## Here we set the lower limit for the price
          up <- c(ps_up, pc_up) # Here we set the upper limit for the price. I am assuming the price per pound of meat won't go larger than a dollar
          
          estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node,
                          lower = lo, upper = up)
          
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
          
          slD[j,i] <- A_node * ((exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))))
          clD[j,i] <- A_node * (1/(1+ exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde)))
            
          # ps_new1 <- as.matrix(x = rep(ps, 125), ncol = 1)
          # pc_new1 <- as.matrix(x = rep(pc, 125), ncol = 1)
          # 
          # c_cull  <- solve(cullInterpolationMatrix) %*% pc_new1
          # c_fed  <- solve(fedCattleInterpolationMatrix) %*% ps_new1
          
          # c_cull1[,j] <- c_cull
          # c_fed1[,j] <- c_fed
          
          # c_old_cull <- c_cull
          # c_old_fed <- c_fed
          
        }
        
        c_fed  <- solve(fedCattleInterpolationMatrix) %*% prices_ps[,i]
        c_cull <- solve(cullInterpolationMatrix) %*% prices_pc[,i]
        
        c_cull_itr[[i]][,k] <- c_cull
        c_fed_itr[[i]][,k] <- c_fed
        
        
        cat("\n norm of old and new fed coefficients: ", norm(c_fed - c_old_fed))

        cat("\n norm of old and new cull coefficients: ", norm(c_cull - c_old_cull))
        
        cat("\n diff: ", (sl_obs + cl_obs - slD_obs - clD_obs)^2)
        
        
        sl_obs <- mean(slNew[,i])
        cl_obs <- mean(clNew[,i])
        
        # meanPricePS[count] <- mean(prices_ps[,i])
        # meanPricePC[count] <- mean(prices_pc[,i])
        # slOBS[count] <- mean(slNew[,i])
        # clOBS[count] <- mean(clNew[,i])
        # slDOBS[count] <- mean(slD[,i])
        # clDOBS[count] <- mean(clD[,i])
        # 
        # coefSLDIFF[count] <- norm(c_fed - c_old_fed)
        # coefCLDIFF[count] <- norm(c_cull - c_old_cull)
        # suppDemandDIFF[count] <- (sl_obs + cl_obs - slD_obs - clD_obs)^2
        
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
 







 






