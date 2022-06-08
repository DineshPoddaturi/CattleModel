
invForecast <- beefINV_FORECAST %>% select(Year, K)
# k3_Proj <- k3 %>% filter(Year >= invForecast$Year[1] & Year <= invForecast$Year[1]+1) 
slShock <- rnorm(n = nrow(invForecast), mean = 1, sd = std(obsEst_sl_Supply$slShock))
clShock <- rnorm(n = nrow(invForecast), mean = 1, sd = std(obsEst_cl_Supply$clShock))

invForecast <- cbind(invForecast, cbind(slShock, clShock))

stockForecast <- Stock %>% filter(Year < 2021 & Year > 2000)
mergedForecast <- merge(stockForecast, invForecast, all=TRUE)

# suppShocks <- cbind(slShock, clShock) %>% as.data.frame()
# 
# mergedForecast <- mergedForecast %>% mutate(slShock = suppShocks$slShock, clShock = suppShocks$clShock)

quantitiesSLCL <- quantities %>% select(Year, sl, cl) %>% filter(Year >= mergedForecast$Year[1])
wtAVG <- allStockShocks %>% select(Year, Slaughter_avg, Cull_avg)%>% filter(Year >= mergedForecast$Year[1])

quantsWeights <- merge(quantitiesSLCL, wtAVG)

impExp <- stocksImportsExports %>% select(Year, Imports, Exports) %>% filter(Year >= mergedForecast$Year[1])

quantsWeights <- merge(quantsWeights, impExp, all=TRUE)

mergedForecast <- merge(mergedForecast, quantsWeights, all=TRUE)

### Here I populate the data frame with known k3's 
# mergedForecast$k3[is.na(mergedForecast$k3)] <- 
#   k3_Proj$k3[match(mergedForecast$Year,k3_Proj$Year)][which(is.na(mergedForecast$k3))]

# mergedForecast$k3[mergedForecast$Year==2021] <- 
#   g * mergedForecast$K[mergedForecast$Year==2018] -  
#   mergedForecast %>% filter(Year == 2019) %>% mutate(slH = sl * (1000000000/Slaughter_avg))  %>% select(slH) %>% as.numeric() +
#   mergedForecast$Imports[mergedForecast$Year==2019] - mergedForecast$Exports[mergedForecast$Year==2019]
#   

mergedForecast <- mergedForecast %>% mutate(k4 = delta * lag(k3), k5 = delta * lag(k4), k6 = delta * lag(k5),
                          k7 = delta * lag(k6), k8 = delta * lag(k7)) %>% filter(Year > 2010)

mergedForecast <- mergedForecast %>% 
  mutate(k9 = K - (k3+k4+k5+k6+k7+k8)) %>% mutate(k9 = if_else(k9 < 0, 0, k9), k10=0)

mergedForecast <- fill(mergedForecast, Cull_avg, .direction = 'down')
mergedForecast <- fill(mergedForecast, Slaughter_avg, .direction = 'down')
mergedForecast <- fill(mergedForecast, Imports, .direction = 'down')
mergedForecast <- fill(mergedForecast, Exports, .direction = 'down')

slFun <- function(stocksALL){
  stocks1 <- stocksALL %>%
  transmute(slt1 = lag(sl,1)* (1000000000/Slaughter_avg) * lag(slShock,1) +  (1 - 0.37 * g) * g * delta * (lag(K,2) 
                                              - (g - 0.37 * g) * lag(K,3) -(lag(k9,2) + (1-delta) * lag(k8,2) + (1-delta) * lag(k7,2))))
  return(stocks1$slt1)
}

clFun <-function(stocksALL){
  stocks1 <- stocksALL %>%
    transmute(clt1 = cl * (1000000000/Cull_avg) * clShock +  (delta * (k8 + k7 + k6) - (k7 + k8 + k9)))
  return(stocks1$clt1)
}

# 
# 
# mergedForecast <- mergedForecast %>% 
#   mutate(sl = (g * lag(K,1) - k3 + Imports - Exports) * 
#            (Slaughter_avg/1000000000))
# 
# 
# mergedForecast <- mergedForecast %>% 
#   mutate(cl = (k10 + (k9 - lead(k10,1)) + (k8 - lead(k9,1)) + (k7 - lead(k8,1)))* 
#            (Cull_avg/1000000000))


















