
fedNodes <- fed_cartesian$fedNodes
cullNodes <- cull_cartesian$cullNodes
# aNodes <- (fedNodes + cullNodes) * (cull_cartesian$dShockNodes)^3 


fedShare <- matrix(data = 0,nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
cullShare <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
totalA <- matrix(data = 0,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
# AShock <- matrix(data = aNodes, ncol = 1)

fedNodes_Vec <- matrix(data = fedNodes, ncol = 1)
cullNodes_Vec <-  matrix(data = cullNodes, ncol = 1)
A_Vec <- matrix(data = aNodes, ncol = 1)

fedShare_fedNodes <- matrix(data = NA, nrow=nrow(fedCattleInterpolationMatrix),ncol = nrow(quantities_prices_capK))
cullShare_cullNodes <- matrix(data = NA,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))
A_ANodes <- matrix(data = NA,nrow=nrow(cullInterpolationMatrix),ncol = nrow(quantities_prices_capK))

##### Getting fedShare from the prices and the parameters
for (i in 1:ncol(fedShare)){
  
  pss <- prices_ps[,i]
  pcc <- prices_pc[,i]
  AShocks <- Anodes[,i]
  tildeMU <- mu_Tildes[,i]
  tildeS <- s_Tildes[,i]
  
  fedShare[,i] <- 
    AShocks * ((exp((tildeMU - ((pss/phi) - (pcc/phi)))/tildeS))/(1 + (exp((tildeMU - ((pss/phi) - (pcc/phi)))/tildeS))))
  cullShare[,i] <- 
    AShocks * (1/(1+ exp((tildeMU - ((pss/phi) - (pcc/phi)))/tildeS)))
  
  fedShare_fedNodes[,i] <- fedShare[,i] - fedNodes_Vec
  cullShare_cullNodes[,i] <- cullShare[,i] - cullNodes_Vec
  # A_ANodes[,i] <- fedShare[,i] + cullShare[,i] - A_Vec
  
}

# apply(apply(round(fedShare_fedNodes,1),2,function(x) x==0),2, function(x) x==TRUE)



##### Getting cullShare from the prices and the parameters
for (i in 1:ncol(cullShare)){
  pss <- prices_ps[,i]
  pcc <- prices_pc[,i]
  tildeMU <- mu_Tildes[,i]
  tildeS <- s_Tildes[,i]
  cullShare[,i] <- 
    AShock * (1/(1+ exp((tildeMU - ((pss/phi) - (pcc/phi)))/tildeS)))
  
  cullShare_cullNodes[,i] <- cullShare[,i] - cullNodes_Vec
}



# mu_Tildes_Prior - mu_Tildes
# 
# s_Tildes_Prior - s_Tildes


# > lo
# [1] 0.3779567 0.0099030 1.6961611
# > up
# [1] 0.8372467 0.4900560 1.8961611


