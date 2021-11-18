##### Projections

proj_adjFac <- adjFactor_New

proj_adjFac <- adjFactor

proj_muTildes <- mu_Tildes_MM
proj_sTildes <- s_Tildes_MM
proj_PricesCosts <- Reduce(function(...) merge(...), list(EQestPS,EQestPC,EQestHC, EQestEPS, EQestEPC))



proj_muTildes1 <- mu_Tildes_MM_itr
proj_sTildes1 <- s_Tildes_MM_itr
proj_PricesCosts1 <- Reduce(function(...) merge(...), list(ITRestPS,ITRestPC,ITRestHC, ITRestEPS, ITRestEPC))

#### We use the following to get the t+1 supply of the fed cattle
##### See the work in the binder
proj_K_t <- Stock %>% transmute(Year = Year, K = K)
proj_A <- A_quant
proj_Dshocks <- stateVars %>% transmute(Year = Year, dShock = Shock)

proj_AllDF_EQ <- Reduce(function(...) merge(...), 
                   list(proj_K_t,proj_A,proj_Dshocks,proj_adjFac,proj_muTildes,proj_sTildes,proj_PricesCosts, 
                        dressedWeights_sl_cl))

proj_AllDF_CONV <- Reduce(function(...) merge(...), 
                        list(proj_K_t,proj_A,proj_Dshocks,proj_adjFac,proj_muTildes1,proj_sTildes1,proj_PricesCosts1, 
                             dressedWeights_sl_cl))

shareMetric <- function(paramMu, paramS, ps, pc){
  
  share <- ((exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))/(1 + (exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))))
  return(share)
  
}

estQFunction <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3){
  
  k3t2 <- k
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  F1 <- g * K1 - k3t2 - A * slShare
  F2 <-  k3t2 * (delta^4) * (1/(gamma_k3^6)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) ) - A * clShare
  
  F <- F1^2 + F2^2
  
}

estPFunction <- function(p, sl, cl, A, B, hc_discounted, tilde_MU, tilde_s){
  
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
  
  F1 <- sl - A * ((exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))/(1 + (exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))))
  
  F2 <- cl  - A * (1/(1+ exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s)))
  
  F3 <- B - ps + g * (beta^3) * Eps3 - hc_discounted
  
  # F <- F1^2 + F2^2 + F3^2
  
  F4 <- pc - beta * Epc1 - g * (beta^3) * Eps3 + (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  
  F <- F1^2 + F2^2 + F3^2 + F4^2
  
  # F <- F1^2 + F2^2
  
  return(F)
  
}


#### Here I create chebyshev nodes for total stock
stockNodes <- chebyshevNodes(d = proj_AllDF$K, n = chebNodesN)

proj_Q_P <- data.frame(Year = numeric(5), Ps = numeric(5), Pc = numeric(5), Sl = numeric(5), Cl = numeric(5),
                       A = numeric(5))

############ NEED TO FIGURE OUT HOW TO WRITE k7, k8, k9 in terms of k3

replacementHeifers_k3 <- replacementInventory %>% arrange(Year)

replacementHeifers_k3 <- replacementHeifers_k3 %>% mutate(ratio = k3/lag(k3))

summary(replacementHeifers_k3$ratio)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.6766  0.9703  1.0104  1.0062  1.0451  1.1658       1

#### Using the above relationship i.e., relationship between the replacement heifers at t to t-1. 
#### The median is 1.0104. So I will fix this and use this as the relationship. 

gamma_k3 <- 1.0104

modelParamsEQ <- tail(proj_AllDF_EQ, n=1)

modelParamsCONV <- tail(proj_AllDF_CONV, n=1)


beefINV_FORECAST

################################## CHANGE THE ADJUSTMENT FACTOR #######################################

slaughterAvg <- modelParamsEQ$Slaughter_avg

MUtilde <- modelParamsEQ$muMedian
Stilde <- modelParamsEQ$sMedian

psM <- modelParamsEQ$psMedian
pcM <- modelParamsEQ$pcMedian
hcM <- modelParamsEQ$hcMedian

EpsM <- modelParamsEQ$EpsMedian
EpcM <- modelParamsEQ$EpcMedian

capA <- modelParamsEQ$A
capK <- modelParamsEQ$K

adjF <- modelParamsEQ$AdjFactor


for(i in 1:nrow(beefINV_FORECAST)){
  
  k <- 0
  K1 <- (capK * slaughterAvg)/1000000000
  
  estQ <- BBoptim(par = k, fn = estQFunction, tilde_MU = MUtilde, tilde_s = Stilde,
                  ps = psM, pc = pcM, K1 = K1, A = capA, gamma_k3 = gamma_k3)
  
  k3_est <- estQ$par
  
  shrT <- shareMetric(paramMu = MUtilde, paramS = Stilde, ps = psM, pc = pcM)
  
  ANew <- (g * K1 - k3_est) * (1/shrT)
  
  slNew <- ANew *  shrT * adjF
  clNew <- ANew * (1-shrT) * adjF
  
  psNew <- psM
  pcNew <- pcM
  
  psNew_lo <- psNew  - 0.35
  pcNew_lo <- pcNew - 0.4
  
  psNew_up <- psNew + 0.10929
  pcNew_up <- pcNew + 0.080153
  
  #### Here we are making sure the lower bound for the prices isn't negative
  if(psNew_lo < 0){
    psNew_lo <- psNew
  }
  
  if(pcNew_lo < 0){
    pcNew_lo <- pcNew
  }
  
  #### Note: The price of the fed cattle is always higher than the cull cows. So we are making sure it holds.
  while( pcNew_lo > psNew_lo ){
    pcNew_lo <- pcNew_lo - 0.01
  }
  
  psNew_expected <- EpsM
  pcNew_expected <- EpcM
  
  hc_new <- hcM
  
  #### Here we make sure that the holding costs are below the cull cow price
  while(hc_new > pcNew){
    hc_new <- hc_new - 0.01
  }
  
  hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  B <- psNew - g * (beta^3) * psNew_expected + hc_discounted
  
  psNew_expected_lo <- psNew_expected - 0.5
  
  psNew_expected_up <- psNew_expected + 0.1
  
  pcNew_expected_lo <- pcNew_expected - 0.5
  
  pcNew_expected_up <- pcNew_expected + 0.1
  
  if(pcNew_expected_lo < 0){
    pcNew_expected_lo <- pcNew_expected
  }
  
  if(ps_expected_lo < 0){
    psNew_expected_lo <- psNew_expected
  }
  
  p <- c(psNew, pcNew, psNew_expected, pcNew_expected)
  
  lo <- c(psNew_lo, pcNew_lo, psNew_expected_lo, pcNew_expected_lo)
  up <- c(psNew_up, pcNew_up, psNew_expected_up, pcNew_expected_up)
  
  estPNew <- BBoptim(par = p, fn = estPFunction, sl = slNew, cl = clNew, A = ANew, 
                     B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                     tilde_MU = MUtilde, tilde_s = Stilde)
  
  ps1N <- estPNew$par[1]
  pc1N <- estPNew$par[2]
  ps_expected1N <- estPNew$par[3]
  pc_expected1N <- estPNew$par[4]
  
  proj_Q_P$Year[i] <- beefINV_FORECAST$Year[i]
  proj_Q_P$Ps[i] <- ps1N
  proj_Q_P$Pc[i] <- pc1N
  proj_Q_P$Sl[i] <- slNew
  proj_Q_P$Cl[i] <- clNew
  proj_Q_P$A[i] <- ANew
  
  psM <- ps1N
  pcM <- pc1N
  
  EpsM <- ps_expected1N
  EpcM <- pc_expected1N
  
  hcM <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    (beta * EpcM + g * (beta^3) * EpsM - pcM)
  
  capA <- ANew
  capK <- beefINV_FORECAST$Kcast[i]
  
  
}
      
     
      
      
      
      
      
      
      
      
      
      
      
      
      # D_slPsPcN <- ANew *
      #   ((exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))/
      #      (1 + (exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))))
      # 
      # ### Demand of the cull cow meat under the new prices
      # D_clPsPcN <- ANew * (1/(1+ exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde)))
      # 
      # #### Total demand for the meat under new prices
      # D_PsPcN <- D_slPsPcN + D_clPsPcN
      # 
      # #### Total supply of meat (this is by adding the nodes)
      # S_psPCN <- slNew + clNew
      # 
      # fedDiffN <- slNew - D_slPsPcN
      # cullDiffN <- clNew - D_clPsPcN
      # 
      # slNew <- D_slPsPcN
      # clNew <- D_clPsPcN
      # 
      # # m <- 1
      # 
      # ANew <- (slNew + clNew) * (1/proj2016$AdjFactor)
      
      # while(abs(fedDiffN)>0.001 || abs(cullDiffN)>0.001){
      #   
      #   k3_estOld <- k3_est
      #   
      #   if( fedDiffN < 0){
      #     ps_n <- ps1N + 0.001
      #   } else if( fedDiffN > 0){
      #     ps_n <- ps1N - 0.001
      #   }
      #   
      #   if(ps_n < 0){
      #     ps_n <- ps1N
      #   }
      #   
      #   if( cullDiffN < 0){
      #     pc_n <- pc1N + 0.001
      #   } else if( cullDiffN > 0){
      #     pc_n <- pc1N - 0.001
      #   }
      #   
      #   if(pc_n < 0){
      #     pc_n <- pc1N
      #   }
      #   
      #   ps_lo <- ps_n  - 0.35
      #   pc_lo <- pc_n - 0.4
      #   
      #   ps_up <- ps_n + 0.10929
      #   pc_up <- pc_n + 0.080153
      #   
      #   if(ps_lo < 0){
      #     ps_lo <- ps_n
      #   }
      #   
      #   if(pc_lo < 0){
      #     pc_lo <- pc_n
      #   }
      #   
      #   while(pc_lo>ps_lo){
      #     pc_lo <- pc_lo - 0.01
      #   }
      #   
      #   ps_expected <- ps_expected1N
      #   pc_expected <- pc_expected1N
      #   
      #   # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
      #   # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
      #   
      #   ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
      #   ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
      #   ### expectations holding costs. Because we have the expected price in the equality.
      #   hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
      #     (beta * pc_expected + g * (beta^3) * ps_expected - pc_n)
      #   
      #   while(hc_new>pc_n){
      #     hc_new <- hc_new - 0.01
      #   }
      #   
      #   hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
      #   B <- ps_n - g * (beta^3) * ps_expected + hc_discounted
      #   
      #   ps_expected_lo <- ps_expected - 0.5
      #   
      #   ps_expected_up <- ps_expected + 0.1
      #   
      #   pc_expected_lo <- pc_expected - 0.5
      #   
      #   pc_expected_up <- pc_expected + 0.1
      #   
      #   if(pc_expected_lo < 0){
      #     pc_expected_lo <- pc_expected
      #   }
      #   
      #   if(ps_expected_lo < 0){
      #     ps_expected_lo <- ps_expected
      #   }
      #   
      #   p <- c(ps_n, pc_n, ps_expected, pc_expected)
      #   
      #   lo <- c(ps_lo, pc_lo, ps_expected_lo, pc_expected_lo)
      #   up <- c(ps_up, pc_up, ps_expected_up, pc_expected_up)
      #   
      #   estPNew_EQ <- BBoptim(par = p, fn = estPFunction, sl = slNew, cl = clNew, A = ANew, 
      #                      B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
      #                      tilde_MU = MUtilde, tilde_s = Stilde)
      #   
      #   ps1N <- estPNew_EQ$par[1]
      #   pc1N <- estPNew_EQ$par[2]
      #   ps_expected1N <- estPNew_EQ$par[3]
      #   pc_expected1N <- estPNew_EQ$par[4]
      #   
      #   ### Demand of the fed cattle meat under the new prices
      #   D_slPsPcN <- ANew *
      #     ((exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))/
      #        (1 + (exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))))
      #   
      #   ### Demand of the cull cow meat under the new prices
      #   D_clPsPcN <- ANew * (1/(1+ exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde)))
      #   
      #   ### Here we get the "optimal" supply of the meat by solving for k_{3,t+1} and k_{j,t+1} where j = [7,8,9]. Note we get
      #   ### these quantities seperately i.e., k_{3,t+1} and sum k_{j,t+1} where j = [7,8,9]
      #   k <- k3_estOld
      # 
      #   #### Here we use sl+cl for A_node. Why? because we use the current quantities i.e., Stock_1t and k_9t + k_8t + k_7t
      #   #### That means the total derived demand should be sl+cl? Dunno Have to think more.....
      #   estQNew <- BBoptim(par = k, fn = estQFunction, tilde_MU = MUtilde, tilde_s = Stilde,
      #                   ps = ps1N, pc = pc1N, K1 = K1, A = ANew, gamma_k3 = gamma_k3)
      # 
      #   k3_est <- estQNew$par
      # 
      #   slNew <- g * K1 - k3_est
      # 
      #   clNew <- k3_est * (delta^4) * (1/(gamma_k3^6)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) )
      # 
      # 
      #   # slN <- slNew
      #   # clN <- clNew
      #   
      #   # slNew <- D_slPsPcN
      #   # clNew <- D_clPsPcN
      #   # 
      #   # slN <- slNew
      #   # clN <- clNew
      #   
      #   #### Total demand for the meat under new prices
      #   D_PsPcN <-  D_slPsPcN + D_clPsPcN
      #   
      #   #### Total supply of meat (this is by adding the results)
      #   S_psPCN <- slNew + clNew
      #   
      #   fedDiffN <- slNew - D_slPsPcN
      #   cullDiffN <- clNew - D_clPsPcN
      #   
      #   # slNodes_eq[j,i] <- sl1
      #   # clNodes_eq[j,i] <- cl1
      #   # A_nodes_eq[j,i] <- A_node
      #   
      #   ### Here we use the share of the cattle meat under new price as the supply of the corresponding meat in the next iteration
      #   if((m %% 2 == 0)){
      #     ANew <- (slNew + clNew)  * (1/proj2016$AdjFactor) 
      #   }else{
      #     slNew <- slNew
      #     clNew <- clNew
      #   }
      #   
      #   m <- m+1
      #   
      # }
      


















