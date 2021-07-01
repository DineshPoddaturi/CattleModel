
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

#### Functions that returns the chebychev nodes and Chebychev interpolation matrix.


########################## NOTE: ALL THESE ARE EMPLOYED TO GET OPTIMAL COEFFICIENTS #################

###### Chebychev Nodes

chebychevNodes <- function(d, n){
  
  a <- min(d)
  b <- max(d)
  
  x <- NA
  
  #### Here we create chebychev collocation nodes
  for (i  in 1:n) {
    x[i] <- ((a+b)/2) + ((b-a)/2) * cos( ((n-i+0.5)/n) * pi )
  }

  #### We normalize the chevychev collocation nodes such that they are in between [-1,1]
  nodes <- (2 * (x-a)/(b-a)) - 1
  # nodes <- x
  
  return(nodes)
}

###### Here we construct interpolation matrix

chebychevInterpolationMatrix <- function(d, n){
  
  normalizedNodes <- chebychevNodes(d,n)
  interpolationMatrix <- matrix(data=0, nrow = length(normalizedNodes), ncol = length(normalizedNodes))
  
  for(i in 1:length(normalizedNodes)){
    interpolationMatrix[,1] <- 1
    interpolationMatrix[,2] <- normalizedNodes
    if(i >=3){
      interpolationMatrix[,i] <- 2 * normalizedNodes * interpolationMatrix[,i-1] - interpolationMatrix[,i-2]
    }
  }
  return(interpolationMatrix)
}

### I will have to select n. I will start with three
chebNodesN <- 3

corn_interpolationMatrix <- chebychevInterpolationMatrix(d = prices_quant$pcorn, n = chebNodesN)

# The interpolation matrix is orthogonal. To see that we just get the diagonal elements of t(matrix) * matrix

#### Interpolationmatrix for fed cattle supply (which is in billion pounds)

fedCattleSupply_interpolationMatrix <- chebychevInterpolationMatrix(d = prices_quant$sl, n = chebNodesN)


#### Interpolationmatrix for cull cow supply (which is in billion pounds)
cullCowSupply_interpolationMatrix <- chebychevInterpolationMatrix(d = prices_quant$cl, n = chebNodesN)


### First shot at constructing demand shocks.
### We assume these shocks follow Gaussian distribution with mean 1 and standard deviation consistent with historical observations.


### I will use Exports + Domestic Consumption as the observed derived demand. 

################################################### THINK ABOUT THIS AGAIN #############################################################
##################################################################################################################################
obsEst_Demand <- merge(beefDemand, totalDisappearedNew) %>% transmute(Year = Year, obsDemand = Demand, 
                                                                      estDemand = total_meat_bill, shock = obsDemand/estDemand)

#### I will generate 24 shocks (length of the other state variables in the analysis) with mean 1 and standard deviation 
#### equal to the standard deviation of the constructed shocks. I am also setting seed so that we get the same random numbers.
set.seed(1)
demand_Shock <- rnorm(n=nrow(prices_quant), mean = 1, sd = std(obsEst_Demand$shock))

demandShock_interpolationMatrix <- chebychevInterpolationMatrix(d= demand_Shock, n = chebNodesN)


#### I need to construct production shock. supp_sl and supp_cl are the estimated fed cattle and cull cow supply. I need to get the 
#### observed supply as well. Isn't it the animals slaughtered? If yes, then we are assuming that the supply equals demand.
#### For now that is what I am doing i.e., assuming that the animal slaughtered as the observed supply and the constructed supply is 
#### the estimated supply. (After talking with Lee and Chad, I have decided the slaughtered data as the observed supply)
obsEst_Supply <- merge(totalSupply, totalDisappearedNew) %>% transmute(Year = Year, obsSupply = total_meat_bill,
                                                      estSupply = TotalSupply, shock = obsSupply/estSupply)

set.seed(2)
supply_Shock <- rnorm(n=nrow(prices_quant), mean = 1, sd = std(obsEst_Supply$shock))
supplyShock_interpolationMatrix <- chebychevInterpolationMatrix(d= supply_Shock, n = chebNodesN)

##### Now I get production shocks for fed cattle and cull cows seperately

cowsSlaughtered_obs <- cowsSlaughtered %>% transmute(Year = Year, cullMeat = cull_meat/1000000000)
heifersSlaughtered_obs <- heifersSlaughtered %>% transmute(Year = Year, heiferMeat = heifer_meat/1000000000)
steersSlaughtered_obs <- steersSlaughtered %>% transmute(Year = Year, steerMeat = steer_meat/1000000000)

fedCattleSupply_obs <- merge(heifersSlaughtered_obs, steersSlaughtered_obs) %>% transmute(Year = Year, sl_obs = heiferMeat + steerMeat)
cullCowSupply_obs <- cowsSlaughtered_obs %>% transmute(Year = Year, cl_obs = cullMeat)

obsEst_sl_Supply <- merge(fedCattleSupply_obs, supp_sl) %>% transmute(Year = Year, sl_obs = sl_obs, sl_est = Bill_meatLb_sl,
                                                                      slShock = sl_obs/sl_est)
set.seed(3)
slSupply_Shock <- rnorm(n = nrow(prices_quant), mean = 1, sd = std(obsEst_sl_Supply$slShock))

sl_supplyShock_interporlationMatrix <- chebychevInterpolationMatrix(d = slSupply_Shock, n = chebNodesN)


obsEst_cl_Supply <- merge(cullCowSupply_obs, supp_cl) %>% transmute(Year = Year, cl_obs = cl_obs, cl_est = Bill_meatLb_cl,
                                                                    clShock = cl_obs/cl_est)
set.seed(4)
clSupply_Shock <- rnorm(n = nrow(prices_quant), mean = 1, sd = std(obsEst_cl_Supply$clShock))

cl_supplyShock_interporlationMatrix <- chebychevInterpolationMatrix(d = slSupply_Shock, n = chebNodesN)

######### Here I am generating the all the grids of the cull cows ###########

corn_interpolationMatrix

cl_supplyShock_interporlationMatrix



#### We generate the grids with kronecker product of interpolation matrices.
gridsCullCows <- kronecker(kronecker(corn_interpolationMatrix, cl_supplyShock_interporlationMatrix),
          demandShock_interpolationMatrix)




################### Here I am retracing the steps by writing the code again

# State variables: corn price, demand shock, cull cows production, fed cattle production
cornPrice <- prices_quant %>% select(Year, pcorn)
cullCowsProd <- supp_cl %>% transmute(Year = Year, cullCows = Bill_meatLb_cl)
fedCattleProd <- supp_sl %>% transmute(Year = Year, fedcattle = Bill_meatLb_sl)

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


## Now i generate gaussian shock which is consistent with historical data.
## I use the standard deviation of historical data to construct the gaussian random variables. Here the mean is 1
set.seed(1)
demandShockG <- rnorm(n = nrow(demandShock), mean = 1, sd = std(demandShock$dShock))
demandShockGaussian$Shock <- demandShockG

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

#### Functoin that creates chebyshev node vector
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

#### For testing purposes I use n = 5 for now. 

chebNodesN <- 5

stateVars <- merge(merge(merge(cornPrice, cullCowsProd),fedCattleProd),demandShockGaussian)

cornNodes <- chebyshevNodes(d = stateVars$pcorn, n = chebNodesN)
cullCowNodes <- chebyshevNodes(d = stateVars$cullCows, n = chebNodesN)
fedCattleNodes <- chebyshevNodes(d = stateVars$fedcattle, n = chebNodesN)
dShockNodes <- chebyshevNodes(d = stateVars$Shock, n = chebNodesN)



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

c_cull <- matrix(data = numeric(nrow(cullInterpolationMatrix)), ncol = 1)
c_fed <- matrix(data = numeric(nrow(fedCattleInterpolationMatrix)), ncol = 1)


p_cull <- matrix(data = numeric(nrow(cullInterpolationMatrix)), ncol = 1)
p_fed <- matrix(data = numeric(nrow(fedCattleInterpolationMatrix)), ncol = 1)


pc_obs <- prices_quant %>% select(Year, pc)
ps_obs <- prices_quant %>% select(Year, ps)
p_corn <- prices_quant %>% select(Year, pcorn)

p_cull[,1] <- pc_obs$pc[1]


c_cull <- solve(cullInterpolationMatrix) %*% p_cull



##### Here I am writing the system of equations that need to be solved for optimal k_{3,t+1} and k_{j,t+1} for j in {8,9,10}
##### let K[1] is k_{3,t+1} and K[2] is sum of k_{8,t+1}, k_{9,t+1}, k_{10,t+1}

g * Stock_1t - K[1] - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))

# k_10t + k_9t + k_8t + k_7t - K[2] - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
# We know k_10t is zero. So the above expression becomes
k_9t + k_8t + k_7t - K[2] - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))

#### I can back out the value of three year old animal as follows

V_3t1 <- ps - g * beta^3 * p_st3 + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^7)/(1-beta))*h


####### I construct the supply in addition to the storage

Stock_1t_rat <- Stock_temp %>% transmute(Year = Year, K = K)
k_9t_rat <- Stock_temp %>% select(Year, k9)
k_8t_rat <- Stock_temp %>% select(Year, k8)
k_7t_rat <- Stock_temp %>% select(Year, k7)


###### To get the expected price I need to integrate out the demand shock and supply shock from the price. 
###### For that we employ guassian quadrature integration.


#### Function that computes the optimal K[1] and K[2]
Stock_1t <- (Stock_1t_rat$K[23]*1500)/1000000000
k_9t <- (k_9t_rat$k9[23]*1500)/1000000000
k_8t <- (k_8t_rat$k8[23]*1500)/1000000000
k_7t <- (k_7t_rat$k7[23]*1500)/1000000000
# 
ps <- ps_obs$ps[23]
pc <- pc_obs$pc[23]
A <-  demandShock$demandEst[23]

fnnnnn <- function(K){
  K1 <- K[1]
  K2 <- K[2]
  
  fed <- g * Stock_1t - K1 - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  cull <- k_9t + k_8t + k_7t - K2 - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F = fed^2 + cull^2

  return(F)
  # return(c(fed,cull))

}

op <- BBoptim(par = c(1,1), fn = fnnnnn)


### Here K[1] is the number of replacement heifers in next period i.e., the heifers that are not sent to slaughter
### house for consumption purposes.

#### Now I have to write code to generate price series using the interpolation matrix, coefficient vector, 
#### and actual prices. This needs a lot of work. 


### I am going to guess the 

optPriceFunction<- function(p){
  
  ps <- p[1]
  pc <- p[2]
  
  F1 <- sl - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))

  F2 <- cl  - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F <- F1^2 + F2^2
  
  return(F)

}

optKFunction <- function(K){
  
  K1 <- K[1]
  K2 <- K[2]
  
  fed <- g * Stock_1t - K1 - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))
  cull <- k_9t + k_8t + k_7t - K2 - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))
  
  F = fed^2 + cull^2
  
  return(F)
  
}

valueFunction <- function(cornNode, cullCowNode, dShockNode, fedCattleNode, pCorn, TSCull, dShock, TSFed){
  
  prices_ps <- matrix(data = 0,nrow=125,ncol = 23)
  prices_pc <- matrix(data = 0,nrow=125,ncol = 23)
  k3t1 <- matrix(data = 0,nrow=125,ncol = 23)
  kjt1 <- matrix(data = 0,nrow=125,ncol = 23)
  c_cull1 <- matrix(data=0, nrow = 125, ncol = 125)
  c_fed1 <- matrix(data=0, nrow = 125, ncol = 125)
  
  c_cull_opt <- list(matrix(data=0, nrow = 125, ncol = 125))
  c_fed_opt <- rep(0,23)
  
  i <- 1
  
  A <- quantities_prices_capK$A[i]
  sl <- quantities_prices_capK$sl[i]
  cl <- quantities_prices_capK$cl[i]
  
  ps <- quantities_prices_capK$ps[i]
  pc <- quantities_prices_capK$pc[i]
  
  Kt1  <- quantities_prices_capK$K[i]
  k9 <- quantities_prices_capK$k9[i]
  k8 <- quantities_prices_capK$k8[i]
  k7 <- quantities_prices_capK$k7[i]
  
  slDressed <- quantities_prices_capK$Slaughter_avg[i]
  clDressed <- quantities_prices_capK$Cull_avg[i]
  
  
  
  ps_new <- as.matrix(rep(ps,nrow(fed_cartesian)), ncol = 1)
  pc_new <- as.matrix(rep(pc,nrow(fed_cartesian)), ncol = 1)
  
  # ps_new <- prices_ps[,i-1]
  # pc_new <- prices_pc[,i-1]
  
  c_cull <- solve(cullInterpolationMatrix) %*% pc_new
  c_fed <- solve(fedCattleInterpolationMatrix) %*% ps_new
  
  c_old_cull <- c_cull
  c_old_fed <- c_fed
  
  
  #### Here we are going through each node
  for (j in 1:125) {
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
  }
  
  c_cull_opt[[i]] <- c_cull1
  c_fed_opt[[i]] <- c_fed1
  
  
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

collocationMethod <- function(chebNodesN, cornNodes, cullCowNodes, fedCattleNodes, dShockNodes,
                              stateVars, A, sl, cl, capK, ps, pc, k9, k8, k7, slWeight, clWeight){
  
  cornPrice <- stateVars$pcorn
  cullSupp <- stateVars$cullCows
  dShock <- stateVars$Shock
  fedSupp <- stateVars$fedcattle
  
  A <- quantities_prices_capK$A[1]
  sl <- quantities_prices_capK$sl[1]
  cl <- quantities_prices_capK$cl[1]
  
  ps <- quantities_prices_capK$ps[1]
  pc <- quantities_prices_capK$pc[1]
  
  Kt1  <- quantities_prices_capK$K[1]
  k9 <- quantities_prices_capK$k9[1]
  k8 <- quantities_prices_capK$k8[1]
  k7 <- quantities_prices_capK$k7[1]
  
  slDressed <- quantities_prices_capK$Slaughter_avg[1]
  clDressed <- quantities_prices_capK$Cull_avg[1]
  
  corn_nodes <- cornNodes %>% as.data.frame()
  cull_nodes <- cullCowNodes %>% as.data.frame()
  fed_nodes <- fedCattleNodes  %>% as.data.frame()
  dshock_nodes <- dShockNodes %>% as.data.frame()
  
  names(corn_nodes) <- "cornNodes"
  names(cull_nodes) <- "cullNodes"
  names(fed_nodes) <- "fedNodes"
  names(dshock_nodes) <- "dShockNodes"
  
  
  ### Cartesian product of the nodes
  cull_cartesian <- crossing(corn_nodes, cull_nodes, dshock_nodes) %>% as.data.frame()
  
  fed_cartesian <- crossing(corn_nodes, fed_nodes, dshock_nodes) %>% as.data.frame()
  
  c_old_cull <- as.matrix(numeric(chebNodesN*chebNodesN*chebNodesN), ncol = 1)
  c_old_fed <- as.matrix(numeric(chebNodesN*chebNodesN*chebNodesN), ncol = 1)
  
  c_cull <- as.matrix(numeric(chebNodesN*chebNodesN*chebNodesN), ncol = 1)
  c_fed <- as.matrix(numeric(chebNodesN*chebNodesN*chebNodesN), ncol = 1)
  
  ps_new <- as.matrix(rep(ps,nrow(fed_cartesian)), ncol = 1)
  pc_new <- as.matrix(rep(pc,nrow(fed_cartesian)), ncol = 1)
  
  c_cull <- solve(cullInterpolationMatrix) %*% pc_new
  c_fed <- solve(fedCattleInterpolationMatrix) %*% ps_new
  
  
  maxit <- 10
  
  for(l in 1:maxit){
    
    c_old_cull <- c_cull
    c_old_fed <- c_fed
    
    
    for(i in 1:nrow(fed_cartesian)){
    
      # pc_new <- cull_InterpolationMatrix %*% c_old_cull
      # ps_new <- fedCattle_InterpolationMatrix %*% c_old_fed
      
      P_Q <- valueFunction(cornNode = cull_cartesian$cornNodes[i], cullCowNode = cull_cartesian$cullNodes[i],
                           dShockNode = cull_cartesian$dShockNodes[i], fedCattleNode = fed_cartesian$fedNodes[i],
                           pCorn = cornPrice, TSCull = cullSupp, dShock = dShock, TSFed = fedSupp)
      
      ps_new[i,] <- P_Q[1]
      pc_new[i,] <- P_Q[2]
      
    }
    
    ps_new1 <- as.matrix(x = rep(ps_new[1], 125), ncol = 1)
    pc_new1 <- as.matrix(x = rep(pc_new[1], 125), ncol = 1)
    
    c_cull <- solve(cullInterpolationMatrix) %*% pc_new1
    c_fed <- solve(fedCattleInterpolationMatrix) %*% ps_new1
    
    if((norm(c_cull - c_old_cull) ) < 0.0001){
      if(norm(c_fed - c_old_fed) < 0.0001){
        break
      }
    }
    
  }
  
}


##### Here setting up the data frame for the quantities. Note: It contains K_{t-1} and K_{j,t} for j = {10,9,8,7}

K_jt <- Stock %>% select(Year, k7, k8, k9, k10)

K_1t <- Stock %>% transmute(Year = Year+1, K)

capK <- merge(K_1t, K_jt)

sl_quant <- supp_sl_adj %>% transmute(Year = Year, sl = Bill_meatLb_sl)
cl_quant <- supp_cl_adj %>% transmute(Year = Year, cl = Bill_meatLb_cl)
A_quant <- totalDisappearedNew %>% transmute(Year = Year, A = total_meat_bill)

quantities <- merge(merge(A_quant,sl_quant), cl_quant)

price_sl_cl <- prices_quant %>% select(Year, ps , pc)

dressedWeights_sl_cl

quantities_prices_capK <- merge(merge(merge(quantities, price_sl_cl), capK),dressedWeights_sl_cl)

collocationMethod(chebNodesN = chebNodesN, cornNodes = cornNodes, cullCowNodes = cullCowNodes, fedCattleNodes = fedCattleNodes,
                  dShockNodes = dShockNodes, stateVars = stateVars, A = quantities_prices_capK$A[1], sl = quantities_prices_capK$sl[1],
                  cl = quantities_prices_capK$cl[1], capK = quantities_prices_capK$K[1], ps = quantities_prices_capK$ps[1], 
                  pc = quantities_prices_capK$pc[1], k9 = quantities_prices_capK$k9[1], k8 = quantities_prices_capK$k8[1],
                  k7 = quantities_prices_capK$k7[1], slWeight = quantities_prices_capK$Slaughter_avg[1],
                  clWeight = quantities_prices_capK$Cull_avg[1])




