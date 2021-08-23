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





