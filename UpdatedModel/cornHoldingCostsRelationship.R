
######## Here I determine the corn and holding costs relationship

cornPrice_Bushel <- allPrices %>% select(Year, pcorn)


EQestHCMedians <- EQestHCNIII %>% select(Year, hcMedian)

cornHC <- merge(EQestHCMedians, cornPrice_Bushel) %>% round(3)

### A simple linear model to get the relationship between the fitted holding costs and the corn price ($/bushel)

Corn_HC_Fit <- lm(formula = hcMedian ~ pcorn , data = cornHC)
fitCHCSummary <- summary(Corn_HC_Fit)

# Call:
#   lm(formula = hcMedian ~ pcorn, data = cornHC)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.32144 -0.09406 -0.01773  0.06465  0.46548 
# 
# Coefficients:
#   Estimate     Std. Error t value Pr(>|t|)    
# (Intercept)    0.21854    0.07948   2.750     0.01 *  
#   pcorn        0.11090    0.02258   4.911    3e-05 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1662 on 30 degrees of freedom
# Multiple R-squared:  0.4457,	Adjusted R-squared:  0.4272 
# F-statistic: 24.12 on 1 and 30 DF,  p-value: 2.995e-05



##### Getting corn futures data
#### First I get the futures from CME and then for the rest of the years I project the corn prices using a simple 
#### linear model

### CME DATA : TRADED on THURSDAY 14 JULY 2022. I get the December contract month data
# 2023 to 2026
# cornFuturesCME <- c(601, 568, 530, 509) 2022 to 2025

### CME DATA : TRADED on THURSDAY 27 APRIL 2023. I get the December contract month data
# 2023 to 2026
cornFuturesCME <- c(543, 528, 486, 473)
cornFuturesCME <- (cornFuturesCME/100) %>% as.data.frame()
names(cornFuturesCME) <- "pcorn"
cornFuturesCME$Year <- seq(from = 2023, to=2026, by =1)
cornFuturesCME <- cornFuturesCME %>% select(Year, pcorn)

### Here I get the past 10 years data of corn prices. 
pCornHC <- pcorn %>% filter(Year >= 2011 & Year <= 2022) %>% 
  select(Year, pcorn) %>% arrange(Year)

### Now I join both the futures from CME and the existing prices
cornFuturesTS <- rbind(pCornHC, cornFuturesCME)
cornFutures_TS <- ts(cornFuturesTS$pcorn, start = cornFuturesTS$Year[1], 
                 end = cornFuturesTS$Year[nrow(cornFuturesTS)], frequency = 1)

plot_time_series(cornFutures_TS, "Corn Prices")

adf.test(cornFutures_TS)
# Augmented Dickey-Fuller Test
# 
# data:  cornFutures_TS
# Dickey-Fuller = -2.7143, Lag order = 2, p-value = 0.3003
# alternative hypothesis: stationary

# Fit a simple linear time series model
seriesCorn <- auto.arima(cornFutures_TS, trace=TRUE)

cornFit <- arima(cornFutures_TS, order = c(1,0,0))

cornFit_Residuals <- ts(cornFit$res, 
                     start = cornFuturesTS$Year[1], 
                     end = cornFuturesTS$Year[nrow(cornFuturesTS)], frequency = 1)

Box.test(cornFit$residuals, type = "Ljung-Box")
# Box-Ljung test
# 
# data:  cornFit$residuals
# X-squared = 1.5264, df = 1, p-value = 0.2166

# The following commented code is to check the diagnostics and make sure the linear model is fitted properly
# qqnorm(cornFit_Residuals)
# ggtsdiag_custom(cornFit, "Stock K") +
#   theme(panel.background = element_rect(fill = "gray98"),
#         panel.grid.minor = element_blank(),
#         axis.line.y = element_line(colour="gray"),
#         axis.line.x = element_line(colour="gray"))
# 
# ggplot2::autoplot(cornFit, na.action = stats::na.pass,
#                   colour = 'turquoise4', size = 1.05) +
#   ggplot2::geom_hline(yintercept = 0,
#                       linetype = 'dashed', size = 0.1,
#                       colour = 'turquoise4') +
#   labs(subtitle = '') +
#   ggplot2::ggtitle("Non-Standardized Residuals")

corn_FORECAST <- forecast(object = cornFit, h = 7, level = 95) %>% as.data.frame()

corn_FORECAST <- corn_FORECAST %>% transmute(Year =  as.double(row.names(corn_FORECAST)), 
                               pcorn = `Point Forecast`)
row.names(corn_FORECAST) <- NULL

cornFutures <- rbind(cornFuturesCME, corn_FORECAST) %>% round(5)

# cornProjUSDA <- c(4.20,	4.20,	4.20,	4.20,	4.20,	4.20) %>% as.data.frame()
# cornFutures <- rbind(cornFuturesCME, cornProjUSDA)
# names(cornFutures) <- "pcorn"
# cornFutures$Year <- seq(from = 2022, to=2031, by =1)
# cornFutures <- cornFutures %>% select(Year, pcorn)

holdingCostsFutures <- cornFutures %>% 
  mutate(hc = fitCHCSummary$coefficients[1] + fitCHCSummary$coefficients[2] * pcorn) %>% round(3) %>% 
  select(Year, hc)


# holdingCostsFuturesBKP <- holdingCostsFutures




####### CALF CROP OBSERVED AND CALF CROP FROM BEEF STOCKS

calf_cropOBS <- calf_crop %>% arrange(by_group = Year)

calf_cropBeefK <- Stock %>% transmute(Year = Year, K = K, ccK = g * K)

ccOBs_K <- merge(calf_cropOBS, calf_cropBeefK) %>% mutate(ratioKcc = ccK/calfCrop)
summary(ccOBs_K$ratioKcc)

gg <- ccOBs_K %>% mutate(g = 0.75 * (calfCrop/K)) %>% select(g) %>% as.vector()

summary(gg$g)




### Reading milk cows inventory
milkCowsInv <- read_excel("./Data/Latest-07-11-SlaughteredData/CowsMilkInventory.xlsx") %>% as.data.frame()
milkCowsInv <- milkCowsInv %>% select(Year, Value) %>% mutate(Km=Value) %>% 
  select(Year, Km) %>% arrange(Year)


milkBeefK <- left_join(milkCowsInv, Stock) %>% select(Year, K, Km)

milkBeefK <- milkBeefK %>% mutate(ccMB = g * (K+Km))

milkBeefK <- merge(milkBeefK, calf_cropOBS)

milkBeefK <- milkBeefK %>% mutate(ggg = calfCrop / (K+Km))

summary(milkBeefK$ggg)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8656  0.8806  0.8902  0.8924  0.9041  0.9388

#### From the above the birth rate is approximately 89% on average. I have been using 97% all this time.
#### I will change this in the model later on but for FMD analysis I will use 89% and see if that changes the
#### stocks

#### I will also look at replacement rates. This is crucial because this enters in our storage model type construction for
#### supplies. 
summary(Stock %>% select(Year, K, k3) %>% mutate(ratios = lead(k3,2)/(gFMD*K)) %>% select(ratios))

# ratios      
# Min.   :0.1584  
# 1st Qu.:0.1912  
# Median :0.2243  
# Mean   :0.2554  
# 3rd Qu.:0.3161  
# Max.   :0.4051  
# NA's   :2 

### From the above it is clear that approximately 25.54% of the calf crop is added as replacement heifers. This is used in 
### in determining the fed cattle supply







##### From Hayes et al. we saw the corn prices fall by 20 cents for the entire period of simulation. 
##### So I make the same assumption to change the corn prices during FMD. Now once I get these corn futures
##### I again use the linear model estimates to get new holding costs during FMD

#### I have to make sure these prices changes in corn futures are relative to the depop.
#### For 20% depop I assume a 20 cent decline, for 10% depop I assume a 10 cent decline, 
#### and finally for 5% depop I assume a 5 cent decline.

cornFuturesFMD20 <- cornFutures %>% mutate(pcorn = pcorn - 0.20)

holdingCostsFuturesFMD20 <- cornFuturesFMD20 %>% 
  mutate(hc = fitCHCSummary$coefficients[1] + fitCHCSummary$coefficients[2] * pcorn) %>% round(3) %>% 
  select(Year, hc)

cornFuturesFMD10 <- cornFutures %>% mutate(pcorn = pcorn - 0.10)

holdingCostsFuturesFMD10 <- cornFuturesFMD10 %>% 
  mutate(hc = fitCHCSummary$coefficients[1] + fitCHCSummary$coefficients[2] * pcorn) %>% round(3) %>% 
  select(Year, hc)


cornFuturesFMD5 <- cornFutures %>% mutate(pcorn = pcorn - 0.05)

holdingCostsFuturesFMD5 <- cornFuturesFMD5 %>% 
  mutate(hc = fitCHCSummary$coefficients[1] + fitCHCSummary$coefficients[2] * pcorn) %>% round(3) %>% 
  select(Year, hc)















