##### Projections

proj_adjFac <- adjFactor_New

proj_muTildes <- mu_Tildes_MM
proj_sTildes <- s_Tildes_MM
proj_PricesCosts <- Reduce(function(...) merge(...), list(EQestPS,EQestPC,EQestHC))

proj_muTildes1 <- mu_Tildes_MM_itr
proj_sTildes1 <- s_Tildes_MM_itr
proj_PricesCosts1 <- Reduce(function(...) merge(...), list(ITRestPS,ITRestPC,ITRestHC))

#### We use the following to get the t+1 supply of the fed cattle
##### See the work in the binder
proj_K_t <- Stock %>% transmute(Year = Year, K = K)
proj_A <- A_quant

proj_AllDF <- Reduce(function(...) merge(...), 
                   list(proj_K_t,proj_A,proj_adjFac,proj_muTildes,proj_sTildes,proj_PricesCosts, 
                        dressedWeights_sl_cl))

shareMetric <- function(paramMu, paramS, ps, pc){
  
  share <- ((exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))/(1 + (exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))))
  return(share)
  
}

optQFunction <- function(tilde_MU, tilde_s, ps, pc, K1, k, A){
  
  k3t2 <- k
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  F1 <- g * K1 - k3t2 - A * slShare
  F2 <-  (1-delta) * (delta^4 + delta^5 + delta^6) * k3t2 - A * clShare
  
  F <- F1^2 + F2^2
  
}


#### Here I create chebyshev nodes for total stock
stockNodes <- chebyshevNodes(d = proj_AllDF$K, n = chebNodesN)

proj_Q_P <- data.frame(Year = numeric(6), Ps = numeric(6), Pc = numeric(6), Sl = numeric(6), Cl = numeric(6),
                       A = numeric(6))

### Let me use 2016 data to project for 2017

proj2016 <- proj_AllDF %>% filter(Year == 2015)

proj_Q_P$Year[1] <- proj2016$Year+1


k <- 0
K1 <- (proj2016$K * proj2016$Slaughter_avg)/1000000000
estQ <- BBoptim(par = k, fn = optQFunction, tilde_MU = proj2016$muMean, tilde_s = proj2016$sMean,
        ps = proj2016$psMean, pc = proj2016$pcMean, K1 = K1, A = proj2016$A)

(delta^4) * (1-delta^3) * estQ$par

K1 - estQ$par

proj_AllDF %>% filter(Year == 2016)


############ NEED TO FIGURE OUT HOW TO WRITE k7, k8, k9 in terms of k3







