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

proj_Q_P <- data.frame(Year = numeric(6), Ps = numeric(6), Pc = numeric(6), Sl = numeric(6), Cl = numeric(6),
                       A = numeric(6))

############ NEED TO FIGURE OUT HOW TO WRITE k7, k8, k9 in terms of k3

replacementHeifers_k3 <- replacementInventory %>% arrange(Year)

replacementHeifers_k3 <- replacementHeifers_k3 %>% mutate(ratio = k3/lag(k3))

summary(replacementHeifers_k3$ratio)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.6766  0.9703  1.0104  1.0062  1.0451  1.1658       1

#### Using the above relationship i.e., relationship between the replacement heifers at t to t-1. 
#### The median is 1.0104. So I will fix this and use this as the relationship. 

gamma_k3 <- 1.0104

### Let me use 2016 data to project for 2017

proj2016 <- proj_AllDF %>% filter(Year == 2016)

proj_Q_P$Year[1] <- proj2016$Year+1


k <- 0
K1 <- (proj2016$K * proj2016$Slaughter_avg)/1000000000

estQ <- BBoptim(par = k, fn = estQFunction, tilde_MU = proj2016$muMedian, tilde_s = proj2016$sMedian,
                ps = proj2016$psMedian, pc = proj2016$pcMedian, K1 = K1, A = proj2016$A, gamma_k3 = gamma_k3)

slNew <- g * K1 - estQ$par 
  
clNew <- estQ$par * (delta^4) * (1/(gamma_k3^6)) * ( (delta/gamma_k3)^2 + (1-delta) * ((delta/gamma_k3) + 1) )

estP <- 




quantities_prices_capK %>% filter(Year == 2017)









################## Here I fit a time series model for replacement heifers
require(tseries)
require(forecast)
require(artfima)
require(arfima)

replacementHeifers_k3 <- replacementInventory %>% arrange(Year)

replacementHeifers_k3 <- replacementHeifers_k3 %>% mutate(ratio = k3/lag(k3))

summary(replacementHeifers_k3$ratio)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.6766  0.9703  1.0104  1.0062  1.0451  1.1658       1

replacementHeifers_k3_ts <- ts(replacementHeifers_k3$k3, start = replacementHeifers_k3$Year[1], 
                               end = replacementHeifers_k3$Year[nrow(replacementHeifers_k3)], frequency = 1)

plot_time_series(replacementHeifers_k3_ts, "Replacement Heifers")

adf.test(replacementHeifers_k3_ts)

# Augmented Dickey-Fuller Test
# 
# data:  replacementHeifers_k3_ts
# Dickey-Fuller = -1.3771, Lag order = 4, p-value =
#   0.8349
# alternative hypothesis: stationary

auto.arima(replacementHeifers_k3_ts)

#### The series in not stationary. So I am differencing 
tsDiff <- diff(x = replacementHeifers_k3$k3, lag = 1) %>% na.omit()

zz <- ts(tsDiff, start = replacementHeifers_k3$Year[1], 
         end = replacementHeifers_k3$Year[nrow(replacementHeifers_k3)], frequency = 1)

plot_time_series(zz,"Differenced time")

adf.test(zz)

# Augmented Dickey-Fuller Test
# 
# data:  zz
# Dickey-Fuller = -6.5601, Lag order = 4, p-value =
#   0.01
# alternative hypothesis: stationary

auto.arima(zz)

Box.test(replacementHeifers_k3_ts, lag=4, type = "Ljung-Box")






