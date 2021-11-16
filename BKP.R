
D_slPsPcN <- ANew *
  ((exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))/
     (1 + (exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))))

### Demand of the cull cow meat under the new prices
D_clPsPcN <- ANew * (1/(1+ exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde)))

#### Total demand for the meat under new prices
D_PsPcN <- D_slPsPcN + D_clPsPcN

#### Total supply of meat (this is by adding the nodes)
S_psPCN <- slNew + clNew

fedDiffN <- slNew - D_slPsPcN
cullDiffN <- clNew - D_clPsPcN

slN <- D_slPsPcN
clN <- D_clPsPcN

m <- 1

ANew <- (slN + clN) * proj2016$AdjFactor

while(abs(fedDiffN)>0.001 || abs(cullDiffN)>0.001){
  
  k3_estOld <- k3_est
  
  if( fedDiffN < 0){
    ps_n <- ps1N + 0.001
  } else if( fedDiffN > 0){
    ps_n <- ps1N - 0.001
  }
  
  if(ps_n < 0){
    ps_n <- ps1N
  }
  
  if( cullDiffN < 0){
    pc_n <- pc1N + 0.001
  } else if( cullDiffN > 0){
    pc_n <- pc1N - 0.001
  }
  
  if(pc_n < 0){
    pc_n <- pc1N
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
  
  ps_expected <- ps_expected1N
  pc_expected <- pc_expected1N
  
  # ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
  # pc_expected <- sum(as.numeric(pc_n) * cullMeshCheb)
  
  ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
  ### we use the equality of that to get the holding costs. From my first observation this is greater than the naive 
  ### expectations holding costs. Because we have the expected price in the equality.
  hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    (beta * pc_expected + g * (beta^3) * ps_expected - pc_n)
  
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
  
  estPNew_EQ <- BBoptim(par = p, fn = estPFunction, sl = slN, cl = clN, A = ANew, 
                        B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                        tilde_MU = MUtilde, tilde_s = Stilde)
  
  ps1N <- estPNew_EQ$par[1]
  pc1N <- estPNew_EQ$par[2]
  ps_expected1N <- estPNew_EQ$par[3]
  pc_expected1N <- estPNew_EQ$par[4]
  
  ### Demand of the fed cattle meat under the new prices
  D_slPsPcN <- ANew *
    ((exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))/
       (1 + (exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde))))
  
  ### Demand of the cull cow meat under the new prices
  D_clPsPcN <- ANew * (1/(1+ exp((MUtilde - ((ps1N/phi) - (pc1N/phi)))/Stilde)))
  
  ### Here we get the "optimal" supply of the meat by solving for k_{3,t+1} and k_{j,t+1} where j = [7,8,9]. Note we get
  ### these quantities seperately i.e., k_{3,t+1} and sum k_{j,t+1} where j = [7,8,9]
  k <- k3_estOld
  
  #### Here we use sl+cl for A_node. Why? because we use the current quantities i.e., Stock_1t and k_9t + k_8t + k_7t
  #### That means the total derived demand should be sl+cl? Dunno Have to think more.....
  estQNew <- BBoptim(par = k, fn = estQFunction, tilde_MU = MUtilde, tilde_s = Stilde,
                     ps = ps1N, pc = pc1N, K1 = K1, A = ANew, gamma_k3 = gamma_k3)
  
  k3_est <- estQNew$par
  
  slNew <- g * K1 - k3_est 
  
  clNew <- k3_est * (delta^4) * (1/(gamma_k3^6)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) )
  
  
  slN <- slNew
  clN <- clNew
  
  # A_nodes[j,i] <- A_node
  
  #### Total demand for the meat under new prices
  D_PsPcN <-  D_slPsPcN + D_clPsPcN
  
  #### Total supply of meat (this is by adding the results)
  S_psPCN <- slN + clN
  
  fedDiffN <- slN - D_slPsPcN
  cullDiffN <- clN - D_clPsPcN
  
  # slNodes_eq[j,i] <- sl1
  # clNodes_eq[j,i] <- cl1
  # A_nodes_eq[j,i] <- A_node
  
  ### Here we use the share of the cattle meat under new price as the supply of the corresponding meat in the next iteration
  if((m %% 2 == 0)){
    ANew <- (slN + clN) * proj2016$AdjFactor
  }else{
    slN <- slNew
    clN <- clNew
  }
  
  m <- m+1
  
}