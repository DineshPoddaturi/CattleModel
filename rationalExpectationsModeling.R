
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



##### Here I am writing the system of equations that need to be solved for optimal k_{3,t+1} and k_{j,t+1} for j in {8,9,10}
##### let K[1] is k_{3,t+1} and K[2] is sum of k_{8,t+1}, k_{9,t+1}, k_{10,t+1}

g * Stock_1t - K[1] - A * ((exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))/(1 + (exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde))))

k_10t + k_9t + k_8t + k_7t - K[2] - A * (1/(1+ exp((mu_Tilde - ((ps/phi) - (pc/phi)))/s_Tilde)))











