require(tidyverse)
require(tseries)
require(artfima)
require(arfima)
require(FitARMA)
require(gdata)
require(lubridate)
require(varhandle)
require(forecast)
require(xts)
require(plotly)
require(ggfortify)
require(gridExtra)



##### Functions
plot_time_series <- function(ts_object, ts_object_name){
  #' Plot Time Series Object
  #'
  #' Creates time series plot utilizing \code{ggplot2} utlizing
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_time_series(air_pass_ts, 'Air Passengers')
  
  
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot is not unavailable')
    } else {
      startYear <- start(ts_object) # Grabs start date
      endYear <- end(ts_object) # Grabs end date
      tsPlot <- autoplot(ts_object,
                         ts.colour = 'turquoise4',
                         size = 1,
                         main = sprintf("Plot of %s",
                                        ts_object_name, startYear[1], endYear[1])) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1),
              panel.background = element_rect(fill = "gray98"),
              axis.line.x = element_line(colour="gray"),
              axis.line.y = element_line(colour="gray")) +
        labs(x = "Year", y = "Average Wind Speed") 
      return(tsPlot)
    }
  }
  else {
    warning('Make sure object entered is time-series object!')
  }
}

plot_acf_pacf <- function(ts_object, ts_object_name){
  #' Plot ACF and PACF for Time Series Object
  #'
  #' Creates \emph{Autocorrelation} and \emph{Partial Autocorrelation} plot
  #' utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_acf_pacf(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      a <- autoplot(acf(ts_object, plot = FALSE, lag.max = length(ts_object)),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) +
        ggtitle(sprintf("ACF plot of %s", ts_object_name))
      
      b <- autoplot(pacf(ts_object, plot = FALSE, lag.max = length(ts_object)),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) + labs(y="PACF") +
        ggtitle(sprintf("PACF plot of %s", ts_object_name))
      
      grid.arrange(a, b)
    }
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

# Decomposed Plot
plot_decomp <- function(ts_object, ts_object_name){
  #' Plots Seasonal Decomposition for Time Series Object
  #'
  #' Decomposes time series object to \emph{Seasonal},
  #' \emph{Remainder}, and \emph{Trend}.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_decomp(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    autoplot(stl(ts_object, s.window = "periodic"),
             main = sprintf("Decomposition Plot of %s", ts_object_name),
             ts.colour = "turquoise4") +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y   = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

# Seasonal Plot
plot_seasonal <- function(ts_object, ts_object_name){
  
  #' Plots Seasonal Component for Time Series Object
  #'
  #' Plots \emph{Seasonal} aspect of time series object.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_seasonal(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    ggseasonplot(ts_object, xlab="Year",
                 main=sprintf("Seasonal Plot of %s", ts_object_name),
                 year.labels=TRUE, year.labels.left=TRUE,
                 col=1:20, pch=19) +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

ggtsdiag_custom <- function(object, ts_object_name, gof.lag = 10,
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                            conf.int.fill = NULL, conf.int.alpha = 0.3,
                            ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                            nrow = NULL, ncol = 1, ...) {
  rs <- stats::residuals(object)
  # rs <- object
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }
  
  p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
                             ts.colour = 'turquoise4', size = 1.05) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    labs(subtitle = '') +
    ggplot2::ggtitle(sprintf("Residual Diagnostics for %s \nNon-Standardized Residuals",
                             ts_object_name))
  
  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot(acfobj, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill,
                    conf.int.alpha = conf.int.alpha,
                    colour = 'turquoise4', size = 1.25)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')
  
  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`'), na.rm = TRUE,
                        colour = 'turquoise4') +
    ggplot2::scale_y_continuous(limits=c(-0.1, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')
  
  p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                                   conf.int.colour = conf.int.colour,
                                   conf.int.linetype = conf.int.linetype,
                                   conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  
  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}



crudeOil <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=2)
conventionalGasoline <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=3)
regularGasoline <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=4)
heatingOil <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=5)
dieselFuel <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=6)
keroseneTypeJetFuel <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=7)
propane <- read.xls("Data/Oil Prices/Prices_Day.xls", sheet=8)


sNp <- read.csv("Data/S&P/GSPC.csv")

#Before doing any kind of analysis we clean the data.
crudeOil <- crudeOil[, colMeans(is.na(crudeOil)) <= .90]
crudeOil <- as.data.frame(crudeOil)[-c(1,2),]
names(crudeOil) <- c("Date", "Cushing, OK WTI (Dollars per Barrel)", "Europe Brent (Dollars per Barrel)")
rownames(crudeOil) <- NULL
crudeOil$`Cushing, OK WTI (Dollars per Barrel)` <- 
  unfactor(crudeOil$`Cushing, OK WTI (Dollars per Barrel)`)
crudeOil$`Europe Brent (Dollars per Barrel)` <- 
  unfactor(crudeOil$`Europe Brent (Dollars per Barrel)`)
crudeOil$Date <- mdy(crudeOil$Date)
# crudeOil%>%glimpse()

conventionalGasoline<- conventionalGasoline[, colMeans(is.na(conventionalGasoline)) <= .90]
conventionalGasoline <- as.data.frame(conventionalGasoline)[-c(1,2),]
names(conventionalGasoline) <- c("Date", 
                                 "New York Harbor (Dollars per Gallon)",
                                 "U.S. Gulf Coast (Dollars per Gallon)")
rownames(conventionalGasoline) <- NULL
conventionalGasoline$`New York Harbor (Dollars per Gallon)`<- 
  unfactor(conventionalGasoline$`New York Harbor (Dollars per Gallon)`)
conventionalGasoline$`U.S. Gulf Coast (Dollars per Gallon)` <- 
  unfactor(conventionalGasoline$`U.S. Gulf Coast (Dollars per Gallon)`)
conventionalGasoline$Date <- mdy(conventionalGasoline$Date)
# conventionalGasoline%>%glimpse()



regularGasoline<- regularGasoline[,colMeans(is.na(regularGasoline))<=.90]
regularGasoline <- as.data.frame(regularGasoline)[-c(1,2),]
names(regularGasoline) <- c("Date", 
                            "Los Angeles (Dollars per Gallon)")
rownames(regularGasoline) <- NULL
regularGasoline$`Los Angeles (Dollars per Gallon)` <- 
  unfactor(regularGasoline$`Los Angeles (Dollars per Gallon)`)
regularGasoline$Date <- mdy(regularGasoline$Date)
regularGasoline[10,]$Date <- mdy("03/24/2003")
# regularGasoline %>% glimpse()


heatingOil <- heatingOil[,colMeans(is.na(heatingOil))<=.90]
heatingOil <- as.data.frame(heatingOil)[-c(1,2),]
names(heatingOil) <- c("Date",
                       "New York Harbor (Dollars per Gallon)")
rownames(heatingOil) <- NULL
heatingOil$`New York Harbor (Dollars per Gallon)` <- 
  unfactor(heatingOil$`New York Harbor (Dollars per Gallon)`)
heatingOil$Date <- mdy(heatingOil$Date)
# heatingOil %>% glimpse()


dieselFuel <- dieselFuel[,colMeans(is.na(dieselFuel))<=.90]
dieselFuel <- as.data.frame(dieselFuel)[-c(1,2),]
names(dieselFuel) <- c("Date",
                       "New York Harbor (Dollars per Gallon)",
                       "U.S. Gulf Coast (Dollars per Gallon)",
                       "Los Angeles, CA (Dollars per Gallon)")
rownames(dieselFuel) <- NULL

dieselFuel$`New York Harbor (Dollars per Gallon)` <-
  unfactor(dieselFuel$`New York Harbor (Dollars per Gallon)`)
dieselFuel$`U.S. Gulf Coast (Dollars per Gallon)` <-
  unfactor(dieselFuel$`U.S. Gulf Coast (Dollars per Gallon)`)
dieselFuel$`Los Angeles, CA (Dollars per Gallon)` <- 
  unfactor(dieselFuel$`Los Angeles, CA (Dollars per Gallon)`)
dieselFuel$Date <- mdy(dieselFuel$Date)
# dieselFuel %>% glimpse()


keroseneTypeJetFuel <- keroseneTypeJetFuel[,colMeans(is.na(keroseneTypeJetFuel))<=.90]
keroseneTypeJetFuel <- as.data.frame(keroseneTypeJetFuel)[-c(1,2),]
names(keroseneTypeJetFuel) <- c("Date",
                                "U.S. Gulf Coast (Dollars per Gallon)")
rownames(keroseneTypeJetFuel) <- NULL
keroseneTypeJetFuel$`U.S. Gulf Coast (Dollars per Gallon)` <- 
  unfactor(keroseneTypeJetFuel$`U.S. Gulf Coast (Dollars per Gallon)`)
keroseneTypeJetFuel$Date <- mdy(keroseneTypeJetFuel$Date)
# keroseneTypeJetFuel %>% glimpse()

propane <- propane[,colMeans(is.na(propane))<=.90]
propane <- as.data.frame(propane)[-c(1,2),]
names(propane) <- c("Date",
                    "Mont Belvieu, TX (Dollars per Gallon)")
rownames(propane) <- NULL
propane$`Mont Belvieu, TX (Dollars per Gallon)` <- 
  unfactor(propane$`Mont Belvieu, TX (Dollars per Gallon)`)
propane$Date <- mdy(propane$Date)
# propane %>% glimpse()

ggplot(propane, aes(Date, `Mont Belvieu, TX (Dollars per Gallon)`)) + geom_line()



sNp<- as.data.frame(sNp)
sNp$Date <- ymd(sNp$Date)
# sNp %>% glimpse()

closeP<- sNp[,c(1,5)]
#the following function makes the time series continuous: means this inserts new row for the missing data with NA value
require(padr)
closeP <- pad(closeP)
closeP$Close <- na.approx(closeP$Close)
tt_day <- ts(closeP$Close, start = c(1950,01), end = c(2018,05), frequency = 365 )
# plot.ts(tt_day)

plot_acf_pacf(tt_day,"Close")


#the data needs to be time series data. So we convert the data we have to that format. 
#Using tseries package we can change the data to tsd

#We do not have continuous time series so make it continuous by using padr

################ crude oil

# crudeOil%>%glimpse()

crudeOil[,1:2]

cushingOK_Price <- crudeOil %>% select(Date,`Cushing, OK WTI (Dollars per Barrel)`)
cushingOK_Price <- pad(cushingOK_Price)
summary(cushingOK_Price$Date)

cushingOK_Price$`Cushing, OK WTI (Dollars per Barrel)` <- 
  na.approx(cushingOK_Price$`Cushing, OK WTI (Dollars per Barrel)`)


range(cushingOK_Price$Date)

cushing_training <- subset(cushingOK_Price, year(Date)<=2015)
cushing_test <- subset(cushingOK_Price, year(Date)>=2016)

startDay_train <- as.numeric(format(cushing_training$Date[1], "%j"))

endDay_train <- as.numeric(format(cushing_training$Date[nrow(cushing_training)], "%j"))

startDay_test <- as.numeric(format(cushing_test$Date[1], "%j"))

endDay_test <- as.numeric(format(cushing_test$Date[nrow(cushing_test)], "%j"))

cushing_train_TS <- ts(cushing_training$`Cushing, OK WTI (Dollars per Barrel)`, 
                       start = c(1986,startDay_train), end = c(2015,endDay_train), frequency = 365 )
cushing_test_TS <- ts(cushing_test$`Cushing, OK WTI (Dollars per Barrel)`, 
                      start = c(2016,startDay_test), end = c(2018,endDay_test), frequency = 365 )


seriesPlot <- plot_time_series(cushing_train_TS,"Cushing OK")
ggplotly(seriesPlot)

Box.test(cushing_train_TS, lag = 20, type = 'Ljung-Box')
adf.test(cushing_train_TS)

decompositionPlot <- plot_decomp(cushing_train_TS,"Cushing OK")
ggplotly(decompositionPlot)

plot_acf_pacf(cushing_train_TS,"Cushing OK")

#Looking at the ACF and PACF it is clear that the time series ia AR process. So we try to fit ARTFIMA with different 
#p values and pick the p with less AIC/BIC value


TS_diff <- diff(cushing_train_TS)
firstDifference <- plot_time_series(TS_diff,"First Difference")
ggplotly(firstDifference)
Box.test(TS_diff, lag = 20, type = 'Ljung-Box')
adf.test(TS_diff)
plot_acf_pacf(TS_diff,"Cushing Ok")


# fit ARTFIMA to wind data
X=cushing_train_TS
#
require("artfima")
#
out <- artfima(X, likAlg="Whittle")
out
#
# dtheory=5/6
# out <- artfima(X, fixd=dtheory,likAlg="Whittle")
# out
#
Y=spectrum(X)
freq=Y$freq
summary(log(freq))
spec=Y$spec
plot(log(freq),log(spec),type="p",xlim=c(-5,8),cex=.2)

#
n=length(X)
d=out$d
lambda=out$lambda
theoryspec=artfimaSDF(n, d, lambda, phi = numeric(0), theta = numeric(0))

length(theoryspec)

L=floor(n/2)
theoryfreq=1:L/L
summary(log(theoryfreq))

lines(log(freq),log(2*3.14159*theoryspec),type="l")
#
# standard errors (approx for v11)
#
# Case d fit
#
N=length(X[,1])
v22=d*d*(exp(-2*lambda))/(1-exp(-2*lambda))
sqrt(v22/N)
v11=4*3.14159*(exp(-2*lambda)+exp(-4*lambda)/2)
sqrt(v11/N)
#case d=5/6
lambda=0.02653006
d=5/6 
v22=d*d*(exp(-2*lambda))/(1-exp(-2*lambda))
sqrt(v22/N)
#
# now try MLE
out <- artfima(X[,1])
out
#
out <- artfima(X[,1],fixd=dtheory)
out







#### Checking with ARTFIMA 
ps <- 5
qs <- 5
AICs <- matrix(numeric(0), ncol=qs+1,nrow=ps+1)
BICs <- matrix(numeric(0), ncol=qs+1,nrow=ps+1)
Ds <- matrix(numeric(0), ncol=qs+1,nrow=ps+1)
Lambdas <- matrix(numeric(0), ncol=qs+1,nrow=ps+1)


for(i in 0:ps){
  for(j in 0:qs){
    #if(i==0&j==0){
    out_pq <- artfima(cushing_train_TS, arimaOrder = c(i,1,j), glp="ARTFIMA", likAlg = "exact")
    b1_pq <- c(out_pq$b0,0)
    AICs[i+1,j+1] <- out_pq$aic
    BICs[i+1,j+1] <- out_pq$bic
    Ds[i+1,j+1] <- out_pq$b0[1]
    Lambdas[i+1,j+1] <- out_pq$b0[2]
  }
}

indexAIC <- which.min(AICs)
indexBIC <- which.min(BICs)
AICs

#from the above with AIC's we have p=5, q=5, d-1
#With BIC's we have p=3,q=3, d=1


o <- artfima(cushing_train_TS, arimaOrder = c(0,0,0), glp="ARTFIMA", likAlg = "exact")
resd <- o$res


ggtsdiag_custom(resd , "ARTFIMA FIT") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=o, aes(resd)) +
  geom_histogram(aes(y =..density..),
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot ARIMA Model Residuals")


plot.artfima(o, which = "res", mainQ = TRUE, subQ = TRUE)



graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))



b1 <- c(o$b0,0)
#with p=5,q=5,d=1
o1 <-  artfima(cushing_train_TS, arimaOrder = c(5,1,5),glp="ARTFIMA", likAlg = "exact")

#### Using bestModels to find the appropriate p and q 
bestModels(cushing_train_TS)

#                   best     2nd best     3rd best     4th best
# AIC models ARFIMA(2,0,1) ARIMA(2,0,2) ARIMA(1,0,2) ARIMA(4,0,0)
# AIC             28053.12     28053.18     28053.34     28054.82
# p(AIC)             1.000        0.973        0.895        0.427
# BIC models ARFIMA(1,0,0) ARIMA(1,0,0) ARIMA(2,0,0) ARIMA(1,0,2)
# BIC             28086.44     28088.07     28089.69     28089.85
# p(BIC)             1.000        0.443        0.197        0.182

best_glpModels <- best_glp_models(cushing_train_TS, glp = "ARTFIMA")
# $LL
#         MA(0)     MA(1)     MA(2)
# AR(0) -14029.71 -14028.21 -14198.00
# AR(1) -17393.04 -14122.06 -14114.64
# AR(2) -14546.46 -14568.33 -14024.38
# 
# $artfima_time
# elapsed 
# 4410.3 
# 
# $aic
# $aic$bestaic
# [1] 28064.76
# 
# $aic$bestaicModel
# [1] "ARTFIMA(2,2)"
# 
# $aic$aic
# MA(0)    MA(1)    MA(2)
# AR(0) 28067.43 28066.43 28408.01
# AR(1) 34796.08 28256.13 28243.28
# AR(2) 29104.92 29150.65 28064.76
# 
# 
# $bic
# $bic$bestbic
# [1] 28096.63
# 
# $bic$bestbicModel
# [1] "ARTFIMA(0,0)"
# 
# $bic$bic
#         MA(0)    MA(1)    MA(2)
# AR(0) 28096.63 28102.93 28451.82
# AR(1) 34832.59 28299.94 28294.39
# AR(2) 29148.73 29201.76 28123.17


best_glpModels_PQ <- best_glp_models(cushing_train_TS, glp = "ARTFIMA", p=10, q=10)

# $LL
#         MA(0)     MA(1)     MA(2)     MA(3)     MA(4)
# AR(0) -14029.71 -14028.21 -14198.00 -14026.26 -14056.92
# AR(1) -17393.04 -14122.06 -14114.64 -14402.17 -14049.41
# AR(2) -14546.46 -14568.33 -14024.38 -14876.75 -14986.26
# AR(3) -14172.35 -14352.88 -14315.59 -14269.27 -14416.90
# AR(4) -14285.47 -14433.55 -14155.39 -14035.83 -14144.78
# 
# $artfima_time
# elapsed 
# 12000.83 
# 
# $aic
# $aic$bestaic
# [1] 28064.76
# 
# $aic$bestaicModel
# [1] "ARTFIMA(2,2)"
# 
# $aic$aic
#         MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 28067.43 28066.43 28408.01 28066.53 28129.83
# AR(1) 34796.08 28256.13 28243.28 28820.35 28116.82
# AR(2) 29104.92 29150.65 28064.76 29771.51 29992.52
# AR(3) 28358.70 28721.76 28649.18 28558.53 28855.81
# AR(4) 28586.94 28885.11 28330.78 28093.66 28313.56
# 
# 
# $bic
# $bic$bestbic
# [1] 28096.63
# 
# $bic$bestbicModel
# [1] "ARTFIMA(0,0)"
# 
# $bic$bic
# MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 28096.63 28102.93 28451.82 28117.63 28188.24
# AR(1) 34832.59 28299.94 28294.39 28878.75 28182.53
# AR(2) 29148.73 29201.76 28123.17 29837.22 30065.53
# AR(3) 28409.80 28780.17 28714.89 28631.54 28936.12
# AR(4) 28645.35 28950.82 28403.79 28173.97 28401.18




best_glpModels_ARFIMA <- best_glp_models(cushing_train_TS, glp="ARFIMA", p=4, q=4)

# $LL
#         MA(0)     MA(1)     MA(2)     MA(3)     MA(4)
# AR(0) -21044.83 -17897.88 -16686.71 -15780.79 -15538.83
# AR(1) -14024.62 -14198.81 -14221.05 -14074.66 -14042.81
# AR(2) -14023.79 -14020.56 -14023.53 -14018.95 -14018.95
# AR(3) -14022.37 -14023.48 -14019.27 -14019.54 -14004.54
# AR(4) -14023.75 -14230.81 -14004.54 -14011.98 -14002.05
# 
# $artfima_time
# elapsed 
# 2897.933 
# 
# $aic
# $aic$bestaic
# [1] 28026.1
# 
# $aic$bestaicModel
# [1] "ARFIMA(4,4)"
# 
# $aic$aic
#         MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 42095.65 35803.75 33383.42 31573.58 31091.65
# AR(1) 28057.24 28407.62 28454.10 28163.32 28101.61
# AR(2) 28057.58 28053.12 28061.07 28053.90 28055.89
# AR(3) 28056.73 28060.95 28054.54 28057.08 28029.07
# AR(4) 28061.50 28477.62 28027.08 28043.95 28026.10
# 
# 
# $bic
# $bic$bestbic
# [1] 28086.44
# 
# $bic$bestbicModel
# [1] "ARFIMA(1,0)"
# 
# $bic$bic
#         MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 42117.56 35832.96 33419.93 31617.38 31142.76
# AR(1) 28086.44 28444.12 28497.91 28214.43 28160.02
# AR(2) 28094.08 28096.93 28112.17 28112.30 28121.60
# AR(3) 28100.54 28112.06 28112.95 28122.79 28102.08
# AR(4) 28112.61 28536.03 28092.79 28116.96 28106.41





best_glpModels_ARIMA <- best_glp_models(cushing_train_TS, glp = "ARIMA", p=4, q=4)


# $LL
#         MA(0)     MA(1)     MA(2)     MA(3)     MA(4)
# AR(0) -53012.55 -45649.35 -39293.80 -34508.10 -30739.19
# AR(1) -14030.08 -14026.60 -14021.67 -14051.36 -14021.17
# AR(2) -14026.24 -14029.16 -14020.59 -14020.85 -14018.99
# AR(3) -18797.06 -14023.46 -14025.34 -15172.52 -14011.10
# AR(4) -14021.41 -14019.11 -14016.44 -14017.44 -14002.33
# 
# $artfima_time
# elapsed 
# 1866.441 
# 
# $aic
# $aic$bestaic
# [1] 28024.65
# 
# $aic$bestaicModel
# [1] "ARIMA(4,4)"
# 
# $aic$aic
#         MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 106029.10 91304.70 78595.59 69026.19 61490.39
# AR(1)  28066.17 28061.20 28053.34 28114.73 28056.33
# AR(2)  28060.49 28068.31 28053.18 28055.70 28053.99
# AR(3)  37604.12 28058.93 28064.69 30361.04 28040.21
# AR(4)  28054.82 28052.21 28048.88 28052.87 28024.65
# 
# 
# $bic
# $bic$bestbic
# [1] 28088.07
# 
# $bic$bestbicModel
# [1] "ARIMA(1,0)"
# 
# $bic$bic
#         MA(0)    MA(1)    MA(2)    MA(3)    MA(4)
# AR(0) 106043.70 91326.60 78624.80 69062.70 61534.19
# AR(1)  28088.07 28090.41 28089.85 28158.53 28107.44
# AR(2)  28089.69 28104.82 28096.98 28106.81 28112.39
# AR(3)  37640.62 28102.73 28115.79 30419.45 28105.92
# AR(4)  28098.63 28103.32 28107.29 28118.58 28097.66




### Fitting arfima using forecast package
out_arfima <- arfima(cushing_train_TS, drange = c(0,0.5), estim="mle")
AIC(out_arfima)

# Coefficients:
#   d     ma.ma1     ma.ma2     ma.ma3     ma.ma4     ma.ma5 
# 0.4999542 -0.6652382 -0.5778002 -0.4591324 -0.3233099 -0.1868725 
# sigma[eps] = 1.04509 
ggtsdiag_custom(out_arfima, "ARFIMA FIT") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

residFit <- ggplot(data=out_arfima, aes(residuals(out_arfima))) +
  geom_histogram(aes(y =..density..),
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot ARIMA Model Residuals")

data("eaglecol")
plot_time_series(eaglecol,"eaglecol")
is.ts(eaglecol)
plot_decomp(eaglecol,"eaglecol")


##fitting arfim using arfima package
p <- 5
q <- 5
ICs <- matrix(numeric(0), ncol=q+1,nrow=p+1)
for(i in 0:p){
  for(j in 0:q){
    arfima_new <- arfima::arfima(cushing_train_TS, order = c(i,0,j), numeach = c(1,1))
    ICs[i+1,j+1] <- AIC(arfima_new)
  }
}

##### Fitting arima

autoARIMAFIT <- auto.arima(cushing_train_TS)
# Series: cushing_train_TS 
# ARIMA(3,1,2) 
# 
# Coefficients:
#         ar1      ar2     ar3    ma1     ma2
#       -0.2029  -0.8930  0.0207  0.230  0.9249
# s.e.   0.0258   0.0341  0.0105  0.024  0.0303
# 
# sigma^2 estimated as 0.7561:  log likelihood=-14001.93
# AIC=28015.87   AICc=28015.87   BIC=28059.67

# RESIDUAL DIAGNOSTICS
ggtsdiag_custom(autoARIMAFIT, "ARIMA FIT") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

tsdiag(autoARIMAFIT)
class(residuals(autoARIMAFIT))

residFit <- ggplot(data=autoARIMAFIT, aes(residuals(autoARIMAFIT))) +
  geom_histogram(aes(y =..density..),
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot ARIMA Model Residuals")


out_arima <- artfima(cushing_train_TS, order = c(3,1,2))









# log( (1 + exp(-2*wind_ARTFIMA0$lambda) - 2 * exp(-wind_ARTFIMA0$lambda)*cos(fr_amesWind))^(-5/6) )
# log( (2-2*cos(fr_amesWind))^(-5/6)    )
d <- 0.3
lambda <- 0.005
freqs <- seq(from=0, to=0.5, length.out = 1000) %>% as.data.frame()
ylim <- seq(from=0, to=8, length.out = 1000)%>% as.data.frame()
ARTFIMA_SDF <- (1 + exp(-2*lambda) - 2 * exp(-lambda)*cos(freqs))^(-d) 
ARFIMA_SDF <- (2-2*cos(freqs))^(-d)
combs <- cbind(log(freqs), log(ylim), log(ARTFIMA_SDF), log(ARFIMA_SDF))
names(combs) <- c("Log-Frequency", "ylim", "Log-ARTFIMA", "Log-ARFIMA")

combs %>% ggplot(aes(x=`Log-Frequency`, y=ylim))+
  geom_line(aes(x=`Log-Frequency`,y=`Log-ARTFIMA`, color ="ARTFIMA SDF"))+ 
  geom_line(aes(x=`Log-Frequency`,y=`Log-ARFIMA`, color = "ARFIMA SDF"))+labs(color="")+
  theme_minimal()+ylab("")+ggtitle("ARTFIMA SDF & ARFIMA SDF - d=0.3, lambda=0.005")
















