
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

cornPrices <- allPrices %>% select(Year, pcorn)

cornPrices_nodes <- cornPrices %>% mutate(cNode = (2*pcorn - max(pcorn) - min(pcorn))/(max(pcorn) - min(pcorn)))


#### Functions that returns the chebychev nodes and Chebychev interpolation matrix

###### Chebychev Nodes

chebychevNodes <- function(d, n){
  
  a <- min(d)
  b <- max(d)
  
  x <- NA
  
  #### Here we create chebychev collocation nodes
  for (i  in 1:n) {
    x[i] <- 0.5 * (a+b) + 0.5 * (b-a) * cos(((n-i+0.5)/n)*pi)
  }

  #### We normalize the chevychev collocation nodes such that they are in between [-1,1]
  nodes <- (2*x - b - a)/(b - a)
  return(nodes)
}

###### Here we construct interpolation matrix

chebychevInterpolationMatrix <- function(d, n){
  
  normalizedNodes <- chebychevNodes(d,n)
  
  interpolationMatrix <- matrix(data=0, nrow = length(normalizedNodes), ncol = n)
  
  for(i in 1:n){
    interpolationMatrix[,1] <- 1
    interpolationMatrix[,2] <- normalizedNodes
    if(i >=3){
      interpolationMatrix[,i] <- 2 * normalizedNodes * interpolationMatrix[,i-1] - interpolationMatrix[,i-2]
    }
  }
  return(interpolationMatrix)
}

### I will have to select n. I will start with three
chebNodes <- 5

corn_interpolationMatrix <- chebychevInterpolationMatrix(d = prices_quant$pcorn, n = chebNodes)

# The interpolation matrix is orthogonal. To see that we just get the diagonal elements of t(matrix) * matrix

#### Interpolationmatrix for fed cattle supply (which is in billion pounds)

fedCattleSupply_interpolationMatrix <- chebychevInterpolationMatrix(d=prices_quant$sl, n = chebNodes)


#### Interpolationmatrix for cull cow supply (which is in billion pounds)

cullCowSupply_interpolationMatrix <- chebychevInterpolationMatrix(d=prices_quant$cl, n = chebNodes)


### First shot at constructing demand shocks
obsEst_Demand <- merge(beefDemand, totalDisappearedNew) %>% transmute(Year = Year, obsDemand = Demand, 
                                                                      estDemand = total_meat_bill, shock = obsDemand/estDemand)


plot(sort(obsEst_Demand$shock), 
     pnorm(sort(obsEst_Demand$shock), mean = mean(sort(obsEst_Demand$shock)), sd = std(sort(obsEst_Demand$shock))))



