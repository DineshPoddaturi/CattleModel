#### One period ahead quantities


fedProduction1Period <- function(stockShocks){
  
  newSL <- stockShocks %>%
    transmute(Year = Year+1, slt = (delta - 0.19) * lag(K,2) * slShock + 
                (1-0.19)  * delta * g * (lag(K,2) - (delta-0.19)*lag(K,3) - 
                                           ( lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2)) ),
              slLbs = slt * Slaughter_avg/1000000000) 
  return(newSL)
}

cullProduction1Period <- function(stockShocks){
  
  newCL <- stockShocks %>%
    transmute(Year = Year + 1, 
              clt = (delta^2) * (lag(k7,2) + (1-delta) * lag(k6,2) + (1-delta) * lag(k5,2)) * clShock +
                (delta^2) * (delta * (lag(k6,2) + lag(k5,2) + lag(k4,2)) - (lag(k5,2) + lag(k6,2) + lag(k7,2))),
              clLbs = clt * Cull_avg/1000000000)
  return(newCL)
}


sl1Period <- fedProduction1Period(allStockShocks)
cl1Period <- cullProduction1Period(allStockShocks)


cornPrice <- prices_quant %>% select(Year, pcorn)
cullCowsProd1 <- cl1Period %>% transmute(Year = Year, cullCows = clLbs)
fedCattleProd1 <- sl1Period %>% transmute(Year = Year, fedcattle = slLbs)


chebNodesN <- 7

stateVariablesList <- list(cornPrice, cullCowsProd1, fedCattleProd1, demandShockGaussian, slSupplyShockGaussian, clSupplyShockgaussian)

stateVars <- Reduce(function(...) merge(...), stateVariablesList) %>% drop_na()

cornNodes1 <- chebyshevNodes(d = stateVars$pcorn, n = chebNodesN)
cullCowNodes1 <- chebyshevNodes(d = stateVars$cullCows, n = chebNodesN)
fedCattleNodes1 <- chebyshevNodes(d = stateVars$fedcattle, n = chebNodesN)
dShockNodes1 <- chebyshevNodes(d = stateVars$Shock, n = chebNodesN)
slShockNodes1 <- chebyshevNodes(d = stateVars$slShock, n = chebNodesN)
clShockNodes1 <- chebyshevNodes(d = stateVars$clShock, n = chebNodesN)

corn_nodes1 <- cornNodes1 %>% as.data.frame()
cull_nodes1 <- cullCowNodes1 %>% as.data.frame()
fed_nodes1 <- fedCattleNodes1  %>% as.data.frame()
dshock_nodes1 <- dShockNodes1 %>% as.data.frame()
slShock_nodes1 <- slShockNodes1 %>% as.data.frame()
clShock_nodes1 <- clShockNodes1 %>% as.data.frame()

names(corn_nodes1) <- "cornNodes"
names(cull_nodes1) <- "cullNodes"
names(fed_nodes1) <- "fedNodes"
names(dshock_nodes1) <- "dShockNodes"
names(slShock_nodes1) <- "slShockNodes"
names(clShock_nodes1) <- "clShockNodes"

##### Cartesian product of the nodes
cull_cartesian1 <- crossing(corn_nodes1, cull_nodes1, dshock_nodes1) %>% as.data.frame()
fed_cartesian1 <- crossing(corn_nodes1, fed_nodes1, dshock_nodes1) %>% as.data.frame()



#### The following created chebyshev matrix containing chebyshev polynomials
cornChebyshevMatrix1 <- chebyshevMatrix(x = cornNodes1, d = stateVars$pcorn, n = chebNodesN)
cullCowsChebyshevMatrix1 <- chebyshevMatrix(x = cullCowNodes1, d = stateVars$cullCows, n = chebNodesN)
fedCattleChebyshevMatrix1 <- chebyshevMatrix(x = fedCattleNodes1, d = stateVars$fedcattle, n = chebNodesN)
dShockChebyshevMatrix1 <- chebyshevMatrix(x = dShockNodes1, d = stateVars$Shock, n = chebNodesN)

cullInterpolationMatrix1 <- kron(kron(cornChebyshevMatrix1, cullCowsChebyshevMatrix1), dShockChebyshevMatrix1)
fedCattleInterpolationMatrix1 <- kron(kron(cornChebyshevMatrix1, fedCattleChebyshevMatrix1), dShockChebyshevMatrix1)


### The following dataframe contains the cows of age 7, 8, 9, 10 at time t.
K_jt <- Stock %>% select(Year, k7, k8, k9, k10)

#### The followng dataframe has K_{t-1} i.e., the previous period stock
K_1t <- Stock %>% transmute(Year = Year+1, K = K)

capK <- merge(K_1t, K_jt)

sl_quant1 <- fedCattleProd1 %>% transmute(Year = Year, sl = fedcattle)
cl_quant1 <- cullCowsProd1 %>% transmute(Year = Year, cl = cullCows)

A_quant <-  totalDisappearedNew  %>% transmute(Year = Year, A = total_meat_bill)

quantities1 <- merge(merge(A_quant,sl_quant1), cl_quant1)

hcosts <- prices_costs %>% select(Year, hc)

price_sl_cl <- prices_quant %>% select(Year, ps , pc)
price_sl_cl_hc <- merge(price_sl_cl, hcosts)

imports_exports <- merge(imports_temp, exports_temp)

variablesList <- list(quantities1, price_sl_cl_hc, capK, dressedWeights_sl_cl, imports_exports)

quantities_prices_capK <- Reduce(function(...) merge(...), variablesList) %>% drop_na()


prices_ps1 <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix1),ncol = nrow(quantities_prices_capK))
prices_pc1 <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix1),ncol = nrow(quantities_prices_capK))

k3t1 <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix1),ncol = nrow(quantities_prices_capK))
kjt1 <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix1),ncol = nrow(quantities_prices_capK))

slNew1 <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix1),ncol = nrow(quantities_prices_capK))
clNew1 <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix1),ncol = nrow(quantities_prices_capK))

slD1 <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix1),ncol = nrow(quantities_prices_capK))
clD1 <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix1),ncol = nrow(quantities_prices_capK))

c_cull1 <- matrix(data=0, nrow = nrow(cullInterpolationMatrix1), ncol = 1)
c_fed1 <- matrix(data=0, nrow = nrow(fedCattleInterpolationMatrix1), ncol = 1)

c_cull_opt1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=nrow(cullInterpolationMatrix1), ncol=1)
c_fed_opt1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow=nrow(fedCattleInterpolationMatrix1), ncol=1)

maxIter <- 500

fedPrice1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian1), ncol=maxIter)
cullPrice1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian1), ncol=maxIter)
fedProd1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian1), ncol=maxIter)
cullProd1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian1), ncol=maxIter)

c_cull_itr1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(cull_cartesian1), ncol=maxIter)
c_fed_itr1 <- lapply(1:nrow(quantities_prices_capK), matrix, data= 0, nrow = nrow(fed_cartesian1), ncol=maxIter)

###### THINK ABOUT THE INDEXING PROPERLY. ARE YOU PREDICTING THE NEXT YEAR OR JUST USING THE SAME YEARS DATA TO 
###### ESTIMATE THE SAME NUMBERS? WE SHOULD BE ESTIMATING THE NEXT YEARS PRICES AND QUANTITIES.
###### When I assume the prices and quantities are for the subsequent year and compare them with naive and observed
###### Theres not much difference between naive and rational. However, this is with the normalized nodes. I think 
###### if I use the coefficients to get the price we might see some improvement.

for(i in 1:nrow(quantities_prices_capK)){
  
  i <- 1
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
  
  ps_new <- as.matrix(rep(ps,nrow(fed_cartesian1)), ncol = 1)
  pc_new <- as.matrix(rep(pc,nrow(cull_cartesian1)), ncol = 1)
  
  c_cull <- solve(cullInterpolationMatrix1) %*% pc_new
  c_fed <- solve(fedCattleInterpolationMatrix1) %*% ps_new
  
  #### getting the parameters from the optParamFunction
  params_mu_s <- optParamFunction(sl = sl, cl = cl, ps = ps, pc = pc, thetas = c(1,1))
  
  mu_Tilde <- params_mu_s[1]
  s_Tilde <- params_mu_s[2]
  
  count <- 0
  
  c_old_cull <- matrix(data = 0, nrow = nrow(cullInterpolationMatrix1), ncol = 1)
  c_old_fed <- matrix(data = 0, nrow = nrow(fedCattleInterpolationMatrix1), ncol = 1)
  
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
    for (j in 1:nrow(cull_cartesian1)) {
      
      # j <- 1
      #### Note: We don't have to normalize/need normalized nodes here. Because we are normalizing them when we are getting the 
      #### chebyshev matrix. See the function written to generate chebyshev matrix containing the chebyshev polynomials
      cornNode <- cull_cartesian1$cornNodes[j]
      cullCowNode <- cull_cartesian1$cullNodes[j]
      dShockNode <- cull_cartesian1$dShockNodes[j]
      fedCattleNode <- fed_cartesian1$fedNodes[j]
      
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
      
      ps_lo <- ps - 0.5
      pc_lo <- pc - 0.5
      
      ps_up <- ps + 1
      pc_up <- pc + 1
      
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
      ps_expected <- ps_new 

      B <- ps_new - g * (beta^3) * ps_expected + hc_discounted
      
      estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node,B = B, hc_discounted = hc_discounted, 
                      Eps = ps_new, lower = lo, upper = up)
      # B = B, hc_discounted = hc_discounted, Eps = ps_new, lower = lo, upper = up
      ps1 <- estP$par[1]
      pc1 <- estP$par[2]
      
      ### From the following we get the quantities of k_{3,t+1} and sum(k_{j,t+1}) where j in {7,8,9} which are storage 
      # K <- c(0,0)
      # 
      # estK <- BBoptim(par = K, fn = optKFunction, ps = ps1, pc = pc1, A = A_node)
      # 
      # k_3t1 <- estK$par[1]
      # k_7_10t1 <- estK$par[2]
      # 
      # sl1 <- (g * Stock_1t - k_3t1 + imports - exports)
      # cl1 <- (k_9t + k_8t + k_7t - k_7_10t1)
      
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
      
      prices_ps1[j,i] <- ps1
      prices_pc1[j,i] <- pc1
      
      # k3t1[j,i] <- k_3t1
      # kjt1[j,i] <- k_7_10t1
      # slNew[j,i] <- sl1
      # clNew[j,i] <- cl1
      
      fedPrice1[[i]][j,k] <- ps1
      cullPrice1[[i]][j,k] <- pc1
      
      # fedProd[[i]][j,k] <- sl1
      # cullProd[[i]][j,k] <- cl1
      
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
    
    c_fed  <- solve(fedCattleInterpolationMatrix) %*% prices_ps1[,i]
    c_cull <- solve(cullInterpolationMatrix) %*% prices_pc1[,i]
    
    c_cull_itr1[[i]][,k] <- c_cull
    c_fed_itr1[[i]][,k] <- c_fed
    
    
    cat("\n norm of old and new fed coefficients: ", norm(c_fed - c_old_fed))
    
    cat("\n norm of old and new cull coefficients: ", norm(c_cull - c_old_cull))
    
    sl_itr1 <- mean(slD[,i])
    cl_itr1 <- mean(clD[,i])
    
  }
  
  c_cull_opt1[[i]] <- c_cull
  c_fed_opt1[[i]] <- c_fed
  
}
























