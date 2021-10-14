
count <- 0
innerCount <- 0

eqIter <- 100
eqCols <- 6

equilibriumCheck <- lapply(1:nrow(cull_cartesian), matrix, data= 0, nrow = eqIter, ncol = eqCols)

for(k in 1:maxIter){
  
    # k <- 3
    
    if(norm(x = (c_cull - c_old_cull), type = "f") < 0.001 && norm(x = (c_fed - c_old_fed) , type = "f") < 0.001){
      if( (ps_m - ps_old)^2 < 0.001 && (pc_m - pc_old)^2 < 0.001){
        break
      }
    }
  
    count <- count + 1
    
    c_old_cull <- c_cull
    c_old_fed <- c_fed
    
    ps_old <- ps_m
    pc_old <- pc_m
    
    for(j in 1:nrow(cull_cartesian)){
      
        # j <- 1
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
        params_mu_s <- optParamFunction(sl = sl_node, cl = cl_node, ps = ps_new, pc = pc_new, thetas = c(1,1))
        
        mu_Tilde <- params_mu_s[1]
        s_Tilde <- params_mu_s[2]
        
        mu_Tildes[j,i] <- mu_Tilde
        s_Tildes[j,i] <- s_Tilde
        
        if(k == 1){
          
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
          pc_up <- pc_o  + 0.080153
          
          #### Here we are making sure the lower bound for the prices isn't negative
          if(ps_lo < 0){
            ps_lo <- ps_o
          }
          
          if(pc_lo < 0){
            pc_lo <- pc_o
          }
          
          hc_new <- (((g * (beta^3) * ps_o) + (beta - 1) * pc_o)/(1 + g * beta * (gamma0 + beta * gamma1)))
          hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
          
          ps_expected <- sum(as.numeric(ps_o) * fedMeshCheb)
          
          B <- ps_o - g * (beta^3) * ps_expected + hc_discounted
          
          ps_expected_lo <- ps_expected - 0.1
          
          ps_expected_up <- ps_expected + 0.1
          
          p <- c(ps_o, pc_o, ps_expected)
          
          lo <- c(ps_lo, pc_lo, ps_expected_lo)
          up <- c(ps_up, pc_up, ps_expected_up)
          
          estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node, B = B, 
                          hc_discounted = hc_discounted, Eps = ps_expected, lower = lo, upper = up)
          
          ps1 <- estP$par[1]
          pc1 <- estP$par[2]
          ps_expected1 <- estP$par[3]
          
          prices_ps[j,i] <- ps1
          prices_pc[j,i] <- pc1
          expected_PS[j,i] <- ps_expected1
          prices_hc[j,i] <- hc_new
          
        }
        
        if( k > 1 ){
          
            m <- 1
            
            while(abs(fedDiff[j,i])>0.001 || abs(cullDiff[j,i])>0.001){
              
              if( fedDiff[j,i] < 0){
                ps_n <- prices_ps[j,i] + 0.1
              } else if( fedDiff[j,i] > 0){
                ps_n <- prices_ps[j,i] - 0.1
              }
              
              if(ps_n < 0){
                ps_n <- prices_ps[j,i]
              }
              
              if( cullDiff[j,i] < 0){
                pc_n <- prices_pc[j,i] + 0.1
              } else if( cullDiff[j,i] > 0){
                pc_n <- prices_pc[j,i] - 0.1
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
              
              hc_new <- (((g * (beta^3) * ps_n) + (beta - 1) * pc_n)/(1 + g * beta * (gamma0 + beta * gamma1)))
              hc_discounted <- ((1-beta^7)/(1-beta)) * (1 + beta * (g * gamma0 + beta * g * gamma1)) * hc_new
              
              ps_expected <- sum(as.numeric(ps_n) * fedMeshCheb)
              
              B <- ps_n - g * (beta^3) * ps_expected + hc_discounted
              
              ps_expected_lo <- ps_expected - 0.1
              
              ps_expected_up <- ps_expected + 0.1
              
              p <- c(ps_n, pc_n, ps_expected)
              
              lo <- c(ps_lo, pc_lo, ps_expected_lo)
              up <- c(ps_up, pc_up, ps_expected_up)
              
              estP <- BBoptim(par = p, fn = optPriceFunction, sl = sl_node, cl = cl_node, A = A_node, B = B, 
                              hc_discounted = hc_discounted, Eps = ps_expected, lower = lo, upper = up )
              
              ps1 <- estP$par[1]
              pc1 <- estP$par[2]
              ps_expected1 <- estP$par[3]
              
              prices_ps[j,i] <- ps1
              prices_pc[j,i] <- pc1
              expected_PS[j,i] <- ps_expected1
              prices_hc[j,i] <- hc_new
              
              ### Demand of the fed cattle meat under the new prices
              D_slPsPc[j,i] <- A_node *
                ((exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))/
                   (1 + (exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde))))
              
              ### Demand of the cull cow meat under the new prices
              D_clPsPc[j,i] <- A_node * (1/(1+ exp((mu_Tilde - ((ps1/phi) - (pc1/phi)))/s_Tilde)))
              
              #### Total demand for the meat under new prices
              D_PsPc <- as.matrix(D_slPsPc[j,i] + D_clPsPc[j,i])
              
              #### Total supply of meat (this is by adding the nodes)
              S_psPC <- as.matrix(slNodes[j,i] + clNodes[j,i])
              
              fedDiff[j,i] <- sl_node - D_slPsPc[j,i]
              cullDiff[j,i] <- cl_node - D_clPsPc[j,i]
              
              equilibriumCheck[[j]][m,1] <-  fedDiff[j,i]
              equilibriumCheck[[j]][m,2] <-  cullDiff[j,i] 
              equilibriumCheck[[j]][m,3] <-  fedDiff[j,i] + cullDiff[j,i] 
              equilibriumCheck[[j]][m,4] <- prices_ps[j,i]
              equilibriumCheck[[j]][m,5] <- prices_pc[j,i]
              equilibriumCheck[[j]][m,6] <- expected_PS[j,i]
              
              sl_node <- D_slPsPc[j,i]
              cl_node <- D_clPsPc[j,i]
              # slNodes[j,i] <- sl_node
              # clNodes[j,i] <- cl_node
              
              m <- m+1
            }
            
            # prices_ps[j,i] <- equilibriumCheck[[j]][m,4]
            # prices_pc[j,i] <- equilibriumCheck[[j]][m,5]
            # expected_PS[j,i] <- equilibriumCheck[[j]][m,6]
            
        }
        
        # fedPrice[[i]][j,k] <- ps1
        # cullPrice[[i]][j,k] <- pc1
      
    }
    
    if(k==1){
      ### Demand of the fed cattle meat under the new prices
      D_slPsPc[,i] <- Anodes[,i] *
        ((exp((mu_Tilde - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tilde))/
           (1 + (exp((mu_Tilde - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tilde))))
      
      ### Demand of the cull cow meat under the new prices
      D_clPsPc[,i] <- Anodes[,i] * (1/(1+ exp((mu_Tilde - ((prices_ps[,i]/phi) - (prices_pc[,i]/phi)))/s_Tilde)))
      
      #### Total demand for the meat under new prices
      D_PsPc <- as.matrix(D_slPsPc[,i] + D_clPsPc[,i])
      
      #### Total supply of meat (this is by adding the nodes)
      S_psPC <- as.matrix(slNodes[,i] + clNodes[,i])
      
      fedDiff[,i] <- slNodes[,i] - D_slPsPc[,i]
      cullDiff[,i] <- clNodes[,i] - D_clPsPc[,i]
      
      # TS_TD_diff <- norm(x = (S_psPC- D_PsPc) , type = "f")
      TS_D_sl_diff <- norm(x = as.matrix(slNodes[,i] - D_slPsPc[,i]) , type = "f")
      TS_D_cl_diff <- norm(x =  as.matrix(clNodes[,i] - D_clPsPc[,i]) , type = "f")
      # cat("\n norm of supply and demand: ", fedDiff[,i])
      
      # cat("\n difference of fed supply and demand: ", as.matrix(fedDiff[,i]))
      
      # cat("\n difference of cull supply and demand: ", as.matrix(cullDiff[,i] ))
      
      sdiff <- fedDiff[,i]
      cdiff <- cullDiff[,i]
    }
    
    c_fed  <- solve(fedCattleInterpolationMatrix) %*% prices_ps[,i]
    c_cull <- solve(cullInterpolationMatrix) %*% prices_pc[,i]
    
    ps_m <- mean(prices_ps[,i])
    pc_m <- mean(prices_pc[,i])
    
    fedDiff_m <- mean(fedDiff[,i])
    cullDiff_m <- mean(cullDiff[,i])
    
    c_cull_itr[[i]][,k] <- c_cull
    c_fed_itr[[i]][,k] <- c_fed
    
    
    cat("\n norm of old and new fed coefficients: ", norm(x = (c_fed - c_old_fed) , type = "f"))
    
    cat("\n norm of old and new cull coefficients: ", norm(x = (c_cull - c_old_cull) , type = "f"))
    
    cat("\n Squared difference between old and new mean fed cattle prices: ", (ps_m - ps_old)^2)
    
    cat("\n Squared difference between old and new mean cull cattle prices: ", (pc_m - pc_old)^2)
    
    
    
}


