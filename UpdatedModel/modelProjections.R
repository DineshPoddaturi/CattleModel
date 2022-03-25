########## This contains the code for the mode projections

#### First we fit a linear model with the total inventory in the United States
librarian::shelf(tseries, arfima, forecast, lmtest)

stock_K <- beefInventory %>% arrange(Year) %>% filter(Year <= 2020)
stock_K_ts <- ts(stock_K$K, start = stock_K$Year[1], 
                 end = stock_K$Year[nrow(stock_K)], frequency = 1)

plot_time_series(stock_K_ts, "Total Stock")

adf.test(stock_K_ts)

# Augmented Dickey-Fuller Test
# 
# data:  stock_K_ts
# Dickey-Fuller = -0.50599, Lag order = 4, p-value
# = 0.9799
# alternative hypothesis: stationary

##### I fit a linear model here. Note that I am using very extensive data from 1924 to 2020.

seriesK <- auto.arima(stock_K_ts, trace=TRUE)

Kfit <- arima(stock_K_ts, order = c(2,1,3))

Kfit_Residuals <- ts(Kfit$res, 
                     start = stock_K$Year[1], 
                     end = stock_K$Year[nrow(stock_K)], frequency = 1)

Box.test(Kfit$residuals, type = "Ljung-Box")

# Box-Ljung test
# H0:  The residuals are random.
# Ha:  The residuals are not random.
# data:  Kfit$residuals
# X-squared = 0.00026887, df = 1, p-value = 0.9869

# The follwing commented code is to check the diagnostics and make sure the linear model is fitted properly
# qqnorm(Kfit_Residuals)
# ggtsdiag_custom(Kfit, "Stock K") +
#   theme(panel.background = element_rect(fill = "gray98"),
#         panel.grid.minor = element_blank(),
#         axis.line.y = element_line(colour="gray"),
#         axis.line.x = element_line(colour="gray"))
# 
# ggplot2::autoplot(Kfit, na.action = stats::na.pass,
#                   colour = 'turquoise4', size = 1.05) +
#   ggplot2::geom_hline(yintercept = 0,
#                       linetype = 'dashed', size = 0.1,
#                       colour = 'turquoise4') +
#   labs(subtitle = '') +
#   ggplot2::ggtitle("Non-Standardized Residuals")


beefINV_FORECAST <- forecast(object = Kfit, h = 12, level = 95) %>% as.data.frame()

beefINV_FORECAST <- beefINV_FORECAST %>% transmute(Year =  as.double(row.names(beefINV_FORECAST)), 
                                                   Kcast = `Point Forecast`, lo95 = `Lo 95`, hi95 = `Hi 95`)

row.names(beefINV_FORECAST) <- NULL

beefInventory_test <- beefInventory %>% arrange(Year)

combinedK <- left_join(beefINV_FORECAST, beefInventory_test) %>% mutate(err = K - Kcast)
#   Year    Kcast     lo95     hi95        K       err
# 1  2021 31051044 29609589 32492498 30843600 -207443.7
# 2  2022 31063066 28157618 33968514 30125100 -937966.2
# 3  2023 31157824 26693220 35622427       NA        NA
# 4  2024 31209654 25298713 37120596       NA        NA
# 5  2025 31194386 24199626 38189146       NA        NA
# 6  2026 31159438 23373813 38945062       NA        NA
# 7  2027 31145681 22693681 39597680       NA        NA
# 8  2028 31154580 22055784 40253376       NA        NA
# 9  2029 31166811 21424517 40909104       NA        NA
# 10 2030 31169867 20814139 41525595       NA        NA
# 11 2031 31165680 20242995 42088365       NA        NA
# 12 2032 31161615 19711322 42611909       NA        NA

#### Here we fit a linear model between the calf crop and the replacement heifers to get the relationship
#### The relationship is shown in the model framework in dissertation document

calf_crop_proj <- calf_crop %>% transmute(Year = Year, k0 = calfCrop) %>% arrange(Year)

replacementInventory_proj <- replacementInventory %>% arrange(Year) 

CC_RH <- merge(calf_crop_proj, replacementInventory_proj, by="Year",all=TRUE)

#### Here we run regression without intercept. 
#### The rational for this is if both of the explanatory variables () are zero 
#### then the replacement heifers is also zero. 
CC_RH_Fit <- lm(formula = lead(k3,1) ~ k3 + lag(k0,3) - 1 , data = CC_RH)

fitSummary <- summary(CC_RH_Fit)

gamma_k3 <- fitSummary$coefficients[1]
eta_k3 <- fitSummary$coefficients[2]
int_k3 <- 0

###### Functions to compute the k0s, replacement heifers, fed cattle, cull cows, total demand
###### The expressions inside the function are derived from the model
###### Refer to the dissertation document for details. Specifically the model projection framework section 

get_k0s <- function(Yr, lag, calfCrop){
  
  if(nrow(calfCrop %>% filter( Year == getYear - lag  ) %>% select(k0)) == 0){
    k0 <- 0 
  }else{
    k0 <- calfCrop %>% filter( Year == Yr - lag  ) %>% select(k0) %>% unlist()
  }
  
  return(k0)
  
}


get_k0s_Global <- function(proj_Q_P, beefINV_FORECAST, calfCrop){
  
  k0s_df <- data.frame(Year = numeric(nProj), k02 = numeric(nProj) , k03 = numeric(nProj), 
                       k04 = numeric(nProj), k05 = numeric(nProj), k06 = numeric(nProj), 
                       k07 = numeric(nProj), k08 = numeric(nProj))
  for (i in 1:nrow(proj_Q_P)){
    
    getYear <- beefINV_FORECAST$Year[i]
    
    if(i>1){
      getYear <- beefINV_FORECAST$Year[i-1] + 1
    }
    
    k02[i] <- get_k0s(Yr = getYear, lag = 2, calfCrop)
    k03[i] <- get_k0s(Yr = getYear, lag = 3, calfCrop)
    k04[i] <- get_k0s(Yr = getYear, lag = 4, calfCrop)
    k05[i] <- get_k0s(Yr = getYear, lag = 5, calfCrop)
    k06[i] <- get_k0s(Yr = getYear, lag = 6, calfCrop)
    k07[i] <- get_k0s(Yr = getYear, lag = 7, calfCrop)  
    k08[i] <- get_k0s(Yr = getYear, lag = 8, calfCrop) 
    
    k0s_df[i,] <- c(getYear, k02[i], k03[i], k04[i], k05[i], k06[i], k07[i], k08[i])
    
  }
  
  return(k0s_df)
}

getSlClA_Proj <- function(params, PsM, PcM, K1, k, CapA, gamma_k3, 
                          eta_k3 , int_k3, adjF, Dshock, k0s, slAvg, clAvg){
  
  estQ <- BBoptim(par = k, fn = estQFunction_Proj, tilde_MU = params[1], 
                  tilde_s = params[2], ps = PsM, pc = PcM, K1 = K1, A = CapA, gamma_k3 = gamma_k3, 
                  eta_k3 = eta_k3, int = int_k3, k0s = k0s, slAvg = slAvg, clAvg = clAvg)
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  k3_est <- estQ$par
  
  slNew <- ((g * K1 - k3_est) * slAvg)/1000000000
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  
  clNew <- ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3_est - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02)) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07)
  
  clNew <- (clNew * clAvg)/1000000000
  
  k3_est <- (k3_est * slAvg)/1000000000
  
  ANew <- (slNew + clNew) * (1/adjF)
  
  k3_est_Head <- estQ$par
  
  return(c(slNew, clNew, ANew, k3_est, k3_est_Head))
  
}  

estQFunction_Proj <- function(tilde_MU, tilde_s, ps, pc, K1, k, A, gamma_k3, 
                              eta_k3 , int_k3, k0s, slAvg, clAvg){
  
  k3t2 <- k
  
  k08 <- k0s$k08
  k07 <- k0s$k07
  k06 <- k0s$k06
  k05 <- k0s$k05
  k04 <- k0s$k04
  k03 <- k0s$k03
  k02 <- k0s$k02
  
  slShare <- shareMetric(paramMu = tilde_MU, paramS = tilde_s, ps = ps, pc = pc)
  clShare <- (1-slShare)
  
  gamma <- gamma_k3
  eta <- eta_k3
  int <- int_k3
  
  slHead <- (A * slShare) * (1000000000/slAvg)  
  clHead <- (A * clShare) * (1000000000/clAvg)
  
  F1 <- g * K1 - k3t2 - slHead
  
  F2 <-  ((delta^4)/(gamma^7)) * (delta^2 + (1-delta) * gamma * (delta + gamma)) *
    (k3t2 - eta * ( (gamma^4) * k06 + (gamma^3) * k05 + (gamma^2) * k04 + gamma * k03 + k02  ) ) -
    ((delta^5)/(gamma^2)) * eta * (delta * gamma * k08 + (delta + (1-delta) * gamma) * k07) -
    clHead
  
  F <- F1^2 + F2^2
  
}

shareMetric <- function(paramMu, paramS, ps, pc){
  
  share <- ((exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))/(1 + (exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))))
  return(share)
  
}

getPsPcEpsEpc_Proj <- function(PsM, PcM, EPsM, EPcM, HcM, SlNew, ClNew, ANew, params){
  
  psNew <- PsM
  pcNew <- PcM
  
  psNew_lo <- psNew  - 0.27667
  pcNew_lo <- pcNew - 0.29217
  
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
  
  psNew_expected <- EPsM
  pcNew_expected <- EPcM
  
  hc_new <- HcM
  
  #### Here we make sure that the holding costs are below the cull cow price
  while(hc_new > pcNew){
    hc_new <- hc_new - 0.01
  }
  
  hc_discounted <- ((1-(beta^7))/(1-beta)) * (1 + g * beta * (gamma0 + beta * gamma1)) * hc_new
  B <- psNew - g * (beta^3) * psNew_expected + hc_discounted
  
  psNew_expected_lo <- psNew_expected - 0.1
  
  psNew_expected_up <- psNew_expected + 0.1
  
  pcNew_expected_lo <- pcNew_expected - 0.1
  
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
  
  estPNew <- BBoptim(par = p, fn = estPFunction, sl = SlNew, cl = ClNew, A = ANew, 
                     B = B, hc_discounted = hc_discounted, lower = lo, upper = up,
                     tilde_MU = params[1], tilde_s = params[2])
  
  ps1N <- estPNew$par[1]
  pc1N <- estPNew$par[2]
  ps_expected1N <- estPNew$par[3]
  pc_expected1N <- estPNew$par[4]
  
  hc1N <- (1/(1+ g * beta * (gamma0 + beta * gamma1))) * 
    (beta * pc_expected1N + g * (beta^3) * ps_expected1N - pc1N)
  
  return(c(ps1N, pc1N, hc1N, ps_expected1N, pc_expected1N))
  
}


###### Retreiving the model parameters, fitted prices

proj_adjFac <- adjFactor

proj_muTildes <- mu_Tildes_MMNII
proj_sTildes <- s_Tildes_MMNII

proj_PricesCosts <- Reduce(function(...) merge(...), 
                           list(EQestPSNII,EQestPCNII,EQestHCNII, EQestEPSNII, EQestEPCNII))

#### Arranging the data
proj_K_t <- Stock %>% transmute(Year = Year, K = K)
proj_A <- A_quant

proj_AllDF_EQ <- Reduce(function(...) merge(...), 
                        list(proj_K_t, proj_A, proj_adjFac, proj_muTildes, proj_sTildes, proj_PricesCosts, 
                             dressedWeights_sl_cl))

modelParamsEQ <- tail(proj_AllDF_EQ, n=1)

slaughterAvg <- modelParamsEQ$Slaughter_avg
cullAvg <-  modelParamsEQ$Cull_avg

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

nProj <- nrow(beefINV_FORECAST) + 1

###### Here I define the dataframe for the projections of quantities, prices
proj_Q_P <- data.frame(Year = numeric(nProj), Ps = numeric(nProj), Pc = numeric(nProj), 
                       EPs = numeric(nProj), EPc = numeric(nProj), Hc = numeric(nProj), 
                       Sl = numeric(nProj), Cl = numeric(nProj), A = numeric(nProj),
                       repHeif = numeric(nProj), repHeif_Head = numeric(nProj))

proj_Q_P_up <- data.frame(Year = numeric(nProj), Ps_up = numeric(nProj), Pc_up = numeric(nProj),
                          EPs_up = numeric(nProj), EPc_up = numeric(nProj), Hc_up = numeric(nProj),
                          Sl_up = numeric(nProj), Cl_up = numeric(nProj), A_up = numeric(nProj),
                          repHeif_up = numeric(nProj), repHeif_Head_up = numeric(nProj)) 

proj_Q_P_lo <- data.frame(Year = numeric(nProj), Ps_lo = numeric(nProj), Pc_lo = numeric(nProj),
                          EPs_lo = numeric(nProj), EPc_lo = numeric(nProj), Hc_lo = numeric(nProj),
                          Sl_lo = numeric(nProj), Cl_lo = numeric(nProj), A_lo = numeric(nProj),
                          repHeif_lo = numeric(nProj), repHeif_Head_lo = numeric(nProj))



##### Using the projected stock, I am generating the new born in each year. So basically multiply birth rate with the 
##### stock.

calf_crop_proj1 <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * Kcast) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

calf_crop_proj1_LO <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * lo95) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

calf_crop_proj1_UP <- left_join(beefINV_FORECAST, calf_crop_proj) %>% mutate(k0 = g * hi95) %>% 
  filter(Year > calf_crop_proj$Year[nrow(calf_crop_proj)]) %>% select(Year, k0)

##### Here I join the data and the projected 
calf_crop_proj_N <- rbind(calf_crop_proj, calf_crop_proj1)

calf_crop_proj_N_LO <- rbind(calf_crop_proj, calf_crop_proj1_LO)

calf_crop_proj_N_UP <- rbind(calf_crop_proj, calf_crop_proj1_UP)

k0s_df <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N)
k0s_df_LO <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N_LO)
k0s_df_UP <- get_k0s_Global(proj_Q_P = proj_Q_P, beefINV_FORECAST = beefINV_FORECAST, calfCrop = calf_crop_proj_N_UP)

####### Here we are projecting the prices and quantities from the forecasted capK or total stock and the calf crop

### k_old is the replacement heifers. We start with zero (almost never true), but we let the program and data to give
### the optimal replacement heifers. This mostly depends on the demand.
k_old <- 0

for(i in 1:nrow(proj_Q_P)){
  
  #### k is replacement heifers starting value.We start with zero (almost never true), but we let the program and data to give
  ### the optimal replacement heifers. 
  k <- 0
  
  K1 <- capK
  
  k0s <- k0s_df[i,-1]
  
  int_k3 <- 0
  
  
  Qs <- getSlClA_Proj(params = c(MUtilde, Stilde), PsM = psM, PcM = pcM, K1 = K1,
                      k = k, CapA = capA, gamma_k3 = gamma_k3, 
                      eta_k3 = eta_k3 , int_k3 = int_k3, adjF = adjF, k0s = k0s,
                      slAvg = slaughterAvg, clAvg = cullAvg)
  
  slNew <- Qs[1]
  clNew <- Qs[2]
  ANew <- Qs[3]
  
  k_old <- Qs[4]
  
  k_old_Head <- Qs[5]
  
  if(EpcM < pcM){
    EpcM <- EpcM + 0.08
  }
  
  Ps <- getPsPcEpsEpc_Proj(PsM = psM, PcM = pcM, EPsM = EpsM, EPcM = EpcM,
                      HcM = hcM, SlNew = slNew, ClNew = clNew, ANew = ANew, 
                      params = c(MUtilde, Stilde))
  psM <- Ps[1]
  pcM <- Ps[2]
  hcM <- Ps[3]
  EpsM <- Ps[4]
  EpcM <- Ps[5]
  
  proj_Q_P$Ps[i] <- psM
  proj_Q_P$Pc[i] <- pcM
  proj_Q_P$Hc[i] <- hcM
  proj_Q_P$EPs[i] <- EpsM
  proj_Q_P$EPc[i] <- EpcM
  
  proj_Q_P$Sl[i] <- slNew
  proj_Q_P$Cl[i] <- clNew
  proj_Q_P$A[i] <- ANew
  proj_Q_P$repHeif[i] <- k_old
  proj_Q_P$repHeif_Head[i] <- k_old_Head
  
  proj_Q_P$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA <- ANew
  
  capK <- beefINV_FORECAST$Kcast[i]
  
}


####### Here we are projecting the prices and quantities from the forecasted capK or total stock upper 95%
psM_up <- modelParamsEQ$psMedian
pcM_up <- modelParamsEQ$pcMedian
hcM_up <- modelParamsEQ$hcMedian

EpsM_up <- modelParamsEQ$EpsMedian
EpcM_up <- modelParamsEQ$EpcMedian

capA_up <- modelParamsEQ$A
capK_up <- modelParamsEQ$K


for(i in 1:nrow(proj_Q_P_up)){
  
  # i <- 1
  
  k <- 0
  
  K1_up <- capK_up 
  
  k0s <- k0s_df_UP[i,-1]
  
  int_k3 <- 0
  
  Qs_up <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM_up, PcM = pcM_up, K1 = K1_up,
                         k = k, CapA = capA_up, gamma_k3 = gamma_k3, eta_k3 = eta_k3 , int_k3 = int_k3, 
                         adjF = adjF, k0s = k0s,slAvg = slaughterAvg, clAvg = cullAvg)
  slNew_up <- Qs_up[1]
  clNew_up <- Qs_up[2]
  ANew_up <- Qs_up[3]
  
  k_old_up <-  Qs_up[4]
  
  k_old_head_up <- Qs_up[5]
  
  Ps_up <- getPsPcEpsEpc(PsM = psM_up, PcM = pcM_up, EPsM = EpsM_up, EPcM = EpcM_up,
                         HcM = hcM_up, SlNew = slNew_up, ClNew = clNew_up, ANew = ANew_up,
                         params = c(MUtilde, Stilde))
  
  psM_up <- Ps_up[1]
  pcM_up <- Ps_up[2]
  hcM_up <- Ps_up[3]
  EpsM_up <- Ps_up[4]
  EpcM_up <- Ps_up[5]
  
  
  proj_Q_P_up$Ps_up[i] <- psM_up
  proj_Q_P_up$Pc_up[i] <- pcM_up
  proj_Q_P_up$Hc_up[i] <- hcM_up
  proj_Q_P_up$EPs_up[i] <- EpsM_up
  proj_Q_P_up$EPc_up[i] <- EpcM_up
  
  proj_Q_P_up$Sl_up[i] <- slNew_up
  proj_Q_P_up$Cl_up[i] <- clNew_up
  proj_Q_P_up$A_up[i] <- ANew_up
  proj_Q_P_up$repHeif_up[i] <- k_old_up
  proj_Q_P_up$repHeif_Head_up[i] <- k_old_head_up
  
  proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_up$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_up <- ANew_up
  capK_up <- beefINV_FORECAST$hi95[i]
  
}

####### Here we are projecting the prices and quantities from the forecasted capK or total stock lower 95%
psM_lo <- modelParamsEQ$psMedian
pcM_lo <- modelParamsEQ$pcMedian
hcM_lo <- modelParamsEQ$hcMedian

EpsM_lo <- modelParamsEQ$EpsMedian
EpcM_lo <- modelParamsEQ$EpcMedian

capA_lo <- modelParamsEQ$A
capK_lo <- modelParamsEQ$K

k_old_lo <- 0

for(i in 1:nrow(proj_Q_P_lo)){
  
  # i <- 1
  
  k <- 0
  
  K1_lo <- capK_lo
  
  k0s <- k0s_df_LO[i,-1]
  
  int_k3 <- 0
  
  Qs_lo <- getSlClA_test(params = c(MUtilde, Stilde), PsM = psM_lo, PcM = pcM_lo, K1 = K1_lo,
                         k = k,CapA = capA_lo, gamma_k3 = gamma_k3, eta_k3 = eta_k3 ,
                         int_k3 = int_k3, adjF = adjF, k0s = k0s, slAvg = slaughterAvg, clAvg = cullAvg)
  
  slNew_lo <- Qs_lo[1]
  clNew_lo <- Qs_lo[2]
  ANew_lo <- Qs_lo[3]
  
  k_old_lo <-  Qs_lo[4]
  
  k_old_head_lo <-  Qs_lo[5]
  
  
  ###### changing the quantities such that they match the historical quantities. Don't know if this is required.
  ###### If I change this the prices are projected properly i.e., fed cattle price is always greater than cull cow price
  
  while(clNew_lo < 1.01){
    clNew_lo <- clNew_lo + 0.01
  }
  while(slNew_lo < 19.01){
    slNew_lo <- slNew_lo + 0.01
  }
  
  ANew_lo <- (slNew_lo + clNew_lo) * (1/adjF)
  
  if(EpcM < pcM){
    EpcM <- EpcM + 0.08
  }
  
  Ps_lo <- getPsPcEpsEpc(PsM = psM_lo, PcM = pcM_lo, EPsM = EpsM_lo, EPcM = EpcM_lo,
                         HcM = hcM_lo, SlNew = slNew_lo, ClNew = clNew_lo, ANew = ANew_lo,
                         params = c(MUtilde, Stilde))
  
  psM_lo <- Ps_lo[1]
  pcM_lo <- Ps_lo[2]
  hcM_lo <- Ps_lo[3]
  EpsM_lo <- Ps_lo[4]
  EpcM_lo <- Ps_lo[5]
  
  proj_Q_P_lo$Ps_lo[i] <- psM_lo
  proj_Q_P_lo$Pc_lo[i] <- pcM_lo
  proj_Q_P_lo$Hc_lo[i] <- hcM_lo
  proj_Q_P_lo$EPs_lo[i] <- EpsM_lo
  proj_Q_P_lo$EPc_lo[i] <- EpcM_lo
  
  proj_Q_P_lo$Sl_lo[i] <- slNew_lo
  proj_Q_P_lo$Cl_lo[i] <- clNew_lo
  proj_Q_P_lo$A_lo[i] <- ANew_lo
  proj_Q_P_lo$repHeif_lo[i] <- k_old_lo
  proj_Q_P_lo$repHeif_Head_lo[i] <- k_old_head_lo
  
  proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i]
  
  if(i>1){
    proj_Q_P_lo$Year[i] <- beefINV_FORECAST$Year[i-1] + 1
  }
  
  capA_lo <- ANew_lo
  capK_lo <- beefINV_FORECAST$lo95[i]
  
}

################### Here we join all the projections 

PQs_PROJs <- round(merge(merge(proj_Q_P_lo, proj_Q_P),proj_Q_P_up),5) %>% select(
  Year, Ps_lo, Ps, Ps_up, Pc_lo, Pc, Pc_up, Sl_lo, Sl, Sl_up, Cl_lo, Cl, Cl_up, 
  A_lo, A, A_up)

PQs_PROJs_PS <- PQs_MEDIANS %>% select(Year, Ps_lo, Ps, Ps_up)
PQs_PROJs_PC <- PQs_MEDIANS %>% select(Year, Pc_lo, Pc, Pc_up)

PQs_PROJs_SL <- PQs_PROJs %>% select(Year, Sl_lo, Sl, Sl_up) %>% round(3)
PQs_PROJs_CL <- PQs_PROJs %>% select(Year, Cl_lo, Cl, Cl_up) %>% round(3)
PQs_PROJs_A  <- PQs_PROJs %>% select(Year, A_lo, A, A_up) %>% round(3)

PQs_PROJs_PS <- PQs_PROJs %>% select(Year, Ps_lo, Ps, Ps_up) %>% transmute(Year = Year, Ps_LO = Ps_lo * 100, 
                                                                               Ps = Ps * 100, Ps_UP = Ps_up * 100)
PQs_PROJs_PC <- PQs_PROJs %>% select(Year, Pc_lo, Pc, Pc_up) %>% transmute(Year = Year, Pc_LO = Pc_up * 100, 
                                                                               Pc = Pc * 100, Pc_UP = Pc_lo * 100)


PQs_PROJs_TS <- merge(PQs_PROJs_SL, PQs_PROJs_CL) %>%
  transmute(Year = Year, TS_lo = Sl_lo + Cl_lo, TS = Sl + Cl, TS_up = Sl_up + Cl_up)



EQestObsPS_Medians <- EQestObsPSNII %>% select(Year, psMedian, ps) %>% filter(Year > 2015)

PQs_MEDIANS_projs <- PQs_PROJs %>% select(Year, Ps_lo, Ps, Ps_up)

EQestObsPS_Medians_projs <- merge(EQestObsPS_Medians,PQs_MEDIANS_projs,by="Year",all=TRUE)

EQestObsPS_Medians_projs[,-1] <- EQestObsPS_Medians_projs[,-1] * 100

EQestObsPS_Medians_proj_plots <- EQestObsPS_Medians_projs %>% ggplot(aes(x=Year)) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + geom_line(aes(y=psMedian, color="PS RATIONAL (MEDIAN)")) +
  geom_point(aes(y = psMedian, color = "PS RATIONAL (MEDIAN)")) + 
  geom_line(aes(y=Ps_lo, color="PS_LO PROJECTION")) +
  geom_point(aes(y=Ps_lo, color="PS_LO PROJECTION")) + 
  geom_line(aes(y=Ps, color="PS PROJECTION")) +
  geom_point(aes(y=Ps, color="PS PROJECTION")) + 
  geom_line(aes(y=Ps_up, color="PS_UP PROJECTION"))  +
  geom_point(aes(y=Ps_up, color="PS_UP PROJECTION"))  + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPS_Medians_projs$Year[1],
                                  EQestObsPS_Medians_projs$Year[nrow(EQestObsPS_Medians_projs)])))+ 
  scale_y_continuous(name="Fed Cattle Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


EQestObsPC_Medians <- EQestObsPCNII %>% select(Year, pcMedian, pc) %>% filter(Year > 2015)

PQs_MEDIANS_projs <- PQs_PROJs %>% select(Year, Pc_lo, Pc, Pc_up)

EQestObsPC_Medians_projs <- merge(EQestObsPC_Medians,PQs_MEDIANS_projs,by="Year",all=TRUE)

EQestObsPC_Medians_projs[,-1] <- EQestObsPC_Medians_projs[,-1] * 100

EQestObsPC_Medians_proj_plots <- EQestObsPC_Medians_projs %>% ggplot(aes(x=Year)) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + geom_line(aes(y=pcMedian, color="PC RATIONAL (MEDIAN)")) +
  geom_point(aes(y = pcMedian, color = "PC RATIONAL (MEDIAN)")) +
  geom_line(aes(y=Pc_up, color="PC_LO PROJECTION"))  +
  geom_point(aes(y=Pc_up, color="PC_LO PROJECTION")) +
  geom_line(aes(y=Pc_lo, color="PC_UP PROJECTION")) +
  geom_point(aes(y=Pc_lo, color="PC_UP PROJECTION")) + 
  geom_line(aes(y=Pc, color="PC PROJECTION")) +
  geom_point(aes(y=Pc, color="PC PROJECTION")) + 
  scale_x_continuous(name="Year", 
                     breaks=c(seq(EQestObsPC_Medians_projs$Year[1],
                                  EQestObsPC_Medians_projs$Year[nrow(EQestObsPC_Medians_projs)])))+ 
  scale_y_continuous(name="Cull Cattle Price ") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

EQestObsPC_Medians_proj_plots













