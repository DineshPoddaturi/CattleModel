count <- 0
innerCount <- 0

eqIter <- 100
eqCols <- 7

equilibriumCheck <- lapply(1:nrow(cull_cartesian), matrix, data= 0, nrow = eqIter, ncol = eqCols)

for(k in 1:maxIter){
  
  # k <- 2
  
  # if( norm(x = (c_cull - c_old_cull), type = "f") < 0.01 && norm(x = (c_fed - c_old_fed) , type = "f") < 0.01){
  #   if( (ps_m - ps_old)^2 < 0.001 && (pc_m - pc_old)^2 < 0.001){
  #     break
  #   }
  # }
  if(k>1){
    if( round(checkTol[k-1,1],5) < 0.01 && round(checkTol[k-1,2],5) < 0.01){
      if( round(checkTol[k-1,3],5) < 0.001 && round(checkTol[k-1,4],5) < 0.001){
        break
      }
    }
  }
  
  
  if( k > 50 ){
    break
  }
  
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
      
      sl_node <- fedCattleNode 
      cl_node <- cullCowNode
      A_node <- (sl_node + cl_node) * dShockNode
      
      Anodes[j,i] <- A_node
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
      ps_expected <- sum(as.numeric(ps_o) * fedMeshCheb)
      
      pc_expected <- sum(as.numeric(pc_o) * cullMeshCheb)
      
      ### This holding costs are derived from the fact that the farmers cull cows when they reach 9 yeards old. So, 
      ### we use the euqlity of that to get the holding costs. From my first observation this is greater than the naive 
      ### expectations holding costs. Because we have the expected price in the equality.
      hc_new <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * (beta * pc_expected + g * (beta^3) * ps_expected - pc_o)
      
      #### Here we make sure that the holding costs are below the cull cow price
      while(hc_new > pc_o){
        hc_new <- hc_new - 0.01
      }
      
      hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
      B <- ps_o - g * (beta^3) * ps_expected + hc_discounted
      
      ps_expected_lo <- ps_expected - 0.1
      
      ps_expected_up <- ps_expected + 0.1
      
      pc_expected_lo <- pc_expected - 0.1
      
      pc_expected_up <- pc_expected + 0.1
      
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
      A_node <- (cullCowNode + fedCattleNode) * dShockNode
      
      sl_node <- slNodes[j,i]
      cl_node <- clNodes[j,i]
      A_node <- (sl_node + cl_node) * dShockNode
      
      Anodes[j,i] <- A_node
      
      while(abs(fedDiff[j,i])>0.001 || abs(cullDiff[j,i])>0.001){
        
        if( fedDiff[j,i] < 0){
          ps_n <- prices_ps[j,i] + 0.01
        } else if( fedDiff[j,i] > 0){
          ps_n <- prices_ps[j,i] - 0.01
        }
        
        if(ps_n < 0){
          ps_n <- prices_ps[j,i]
        }
        
        if( cullDiff[j,i] < 0){
          pc_n <- prices_pc[j,i] + 0.01
        } else if( cullDiff[j,i] > 0){
          pc_n <- prices_pc[j,i] - 0.01
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
        
        ps_expected_lo <- ps_expected - 0.1
        
        ps_expected_up <- ps_expected + 0.1
        
        pc_expected_lo <- pc_expected - 0.1
        
        pc_expected_up <- pc_expected + 0.1
        
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
      
      ps_expected_lo <- ps_expected - 0.1
      
      ps_expected_up <- ps_expected + 0.1
      
      pc_expected_lo <- pc_expected - 0.1
      
      pc_expected_up <- pc_expected + 0.1
      
      p <- c(ps_n, pc_n, ps_expected, pc_expected)
      
      lo <- c(ps_lo, pc_lo, ps_expected_lo, pc_expected_lo)
      up <- c(ps_up, pc_up, ps_expected_up, pc_expected_up)
      
      dShockNode <- cull_cartesian$dShockNodes[j]
      
      sl_node <- slNodes[j,i]
      cl_node <- clNodes[j,i]
      A_node <- Anodes[j,i]
      
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
      
      # slNodes[j,i] <- D_slPsPc_itr[j,i]
      # clNodes[j,i] <- D_clPsPc_itr[j,i]
      
    }
    
    
    if(k>3){
      
      if( round(checkTol[k-2,],5) == round(checkTol[k-1,],5)){
        if(round(checkTol[k-3,],5) == round(checkTol[k-2,],5)){
          break
        }
      }
      
    }
    
    fedPrice[[i]][j,k] <- ps1
    cullPrice[[i]][j,k] <- pc1
    
    fedProd[[i]][j,k] <- slNodes[j,i]
    cullProd[[i]][j,k] <- clNodes[j,i]
    
  }
  
  if(k==1){
    ### Demand of the fed cattle meat under the new prices
    D_slPsPc[,i] <- Anodes[,i] *
      ((exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i]))/
         (1 + (exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i]))))
    
    ### Demand of the cull cow meat under the new prices
    D_clPsPc[,i] <- Anodes[,i] * (1/(1+ exp((mu_Tildes[,i] - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tildes[,i])))
    
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
  
}
