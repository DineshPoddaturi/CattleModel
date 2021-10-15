
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



#############################################################################################################################




(unique(sort(prices_ps[,i])))


(unique(sort(prices_pc[,i])))



pricesCheck <- quantities_prices_capK %>% select(Year, ps, pc) %>% filter(Year>=2001)

pricesCheckPS <- pricesCheck$ps
pricesCheckPC <- pricesCheck$pc

psCheckRes <- prices_ps
pcCheckRes <- prices_pc

for(l in 1:length(pricesCheckPS)){
  psCheck <- pricesCheckPS[l]
  pcCheck <- pricesCheckPC[l]
  
  for(m in 1:nrow(prices_ps)){
    
    if(prices_ps[m,l]>0 && prices_pc[m,l]>0){
      psCheckRes[m,l] <- prices_ps[m,l] - psCheck
      pcCheckRes[m,l] <- prices_pc[m,l] - pcCheck
    }else{
      psCheckRes[m,l] <- 0
      pcCheckRes[m,l] <- 0
    }
    
    
  }
  
}

















