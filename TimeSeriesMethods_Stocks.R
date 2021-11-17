################## Here I fit a time series model for replacement heifers
require(tseries)
require(forecast)
require(arfima)


stock_K <- beefInventory %>% arrange(Year) %>% filter(Year <= 2010)
stock_K_ts <- ts(stock_K$K, start = stock_K$Year[1], 
                               end = stock_K$Year[nrow(stock_K)], frequency = 1)

plot_time_series(stock_K_ts, "Total Stock")

adf.test(stock_K_ts)

# Augmented Dickey-Fuller Test
# 
# data:  stock_K_ts
# Dickey-Fuller = -0.45877, Lag order = 4,
# p-value = 0.9821
# alternative hypothesis: stationary

#### The series in not stationary. So I am differencing 
tsDiffK <- diff(x = stock_K$K, lag = 1) %>% na.omit()

zzK <- ts(tsDiffK, start = stock_K$Year[1], 
         end = stock_K$Year[nrow(stock_K)], frequency = 1)

plot_time_series(zzK,"Differenced time series")

adf.test(zzK)

# Augmented Dickey-Fuller Test
# 
# data:  zzK
# Dickey-Fuller = -4.1723, Lag order = 4,
# p-value = 0.01
# alternative hypothesis: stationary


#### The differenced time series is used to fit a model

# seriesK <- auto.arima(zzK)
# 
# Kfit <- arima(zzK, order = c(2,0,3))

seriesK <- auto.arima(stock_K_ts)

Kfit <- arima(stock_K_ts, order = c(2,1,3))

Kfit_Residuals <- ts(Kfit$res, 
                     start = stock_K$Year[1], 
                     end = stock_K$Year[nrow(stock_K)], frequency = 1)

Box.test(Kfit$residuals, type = "Ljung-Box")
# Box-Ljung test
# 
# data:  Kfit$residuals
# X-squared = 0.0023968, df = 1, p-value =
#   0.961

# qqnorm(Kfit_Residuals)
# 
# ggtsdiag_custom(Kfit, "Maximum Temperature") +
#   theme(panel.background = element_rect(fill = "gray98"),
#         panel.grid.minor = element_blank(),
#         axis.line.y = element_line(colour="gray"),
#         axis.line.x = element_line(colour="gray"))
# 
# 
# ggplot2::autoplot(Kfit, na.action = stats::na.pass,
#                   colour = 'turquoise4', size = 1.05) +
#   ggplot2::geom_hline(yintercept = 0,
#                       linetype = 'dashed', size = 0.1,
#                       colour = 'turquoise4') +
#   labs(subtitle = '') +
#   ggplot2::ggtitle("Non-Standardized Residuals")


# KfitPredict <- predict(Kfit , n.ahead = 5)

beefINV_FORECAST <- forecast(object = Kfit, h = 15, level = 95) %>% as.data.frame()

beefINV_FORECAST <- beefINV_FORECAST %>% transmute(Year = as.numeric(row.names(beefINV_FORECAST)), Kcast = `Point Forecast`, lo95 = `Lo 95`, hi95 = `Hi 95`)

row.names(beefINV_FORECAST) <- NULL

beefInventory_test <- beefInventory %>% arrange(Year) %>% filter(Year > 2010)

left_join(beefINV_FORECAST, beefInventory_test) %>% mutate(err = K - Kcast)





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