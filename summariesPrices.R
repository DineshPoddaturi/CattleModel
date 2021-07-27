
### Here I am getting the summary of the price differences. This can be used to set the boundaries for the price.
### Note: Without this the program can find any number that equates the supply and demand.
### Also note that this is weak form of rational expectations so we use the limited information available for us. 
### That means the decision maker knows how the prices are fluctuating and makes predictions of the future prices.

summary( lead(quantities_prices_capK$ps)-quantities_prices_capK$ps )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.27667 -0.02492  0.03296  0.02760  0.05510  0.28000        1 

summary( lead(quantities_prices_capK$ps,3)-quantities_prices_capK$ps )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.32417  0.02262  0.09821  0.10929  0.23644  0.37750        3 

summary( lead(quantities_prices_capK$ps,2)-quantities_prices_capK$ps )
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -0.326667  0.006083  0.083417  0.060004  0.145708  0.318333         2


summary( lead(quantities_prices_capK$pc)-quantities_prices_capK$pc )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.29217 -0.03717  0.01442  0.01723  0.06054  0.25933        1 

summary( lead(quantities_prices_capK$pc,3)-quantities_prices_capK$pc )
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -0.386667 -0.003938  0.059292  0.080153  0.192417  0.371250         3 

summary( lead(quantities_prices_capK$pc,2)-quantities_prices_capK$pc )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.34550 -0.01883  0.04058  0.03968  0.11925  0.27042        2 

fedPS <- 0
for(i in 1:length(fedPrice)){
  fedPS[i] <- mean(colMeans(fedPrice[[i]][,apply(fedPrice[[i]],2,function(x) !all(x==0))]))
}


fedPrice[[1]][,apply(fedPrice[[1]],2,function(x) !all(x==0))]

round(c_cull_itr[[1]][, apply(c_cull_itr[[1]],2,function(x) !all(x==0))][,136],5)

round(c_fed_itr[[1]][, apply(c_fed_itr[[1]],2,function(x) !all(x==0))][,136],5)

colMeans(prices_ps)


estPS <- colMeans(prices_ps) %>% as.data.frame()
# estPS <- fedPS %>% as.data.frame()
names(estPS) <- "fedPrice"
estPS <- estPS %>% mutate(Year = quantities_prices_capK$Year+2) %>% select(Year, everything())

estObsPS <- merge(estPS, quantities_prices_capK) %>% select(Year, fedPrice, ps) %>% mutate(D = (fedPrice - ps))


estObsPS %>% ggplot(aes(x=Year))+geom_line(aes(y=fedPrice, color="PS RATIONAL")) +
  geom_point(aes(y = fedPrice, color = "PS RATIONAL")) + geom_line(aes(y=ps, color = "PS OBS")) + 
  geom_point(aes(y=ps, color = "PS OBS")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(estObs$Year[1],estObs$Year[nrow(estObs)]))) +
  expand_limits(y = 0.25)

estPC <- colMeans(prices_pc) %>% as.data.frame()
names(estPC) <- "cullPrice"
estPC <- estPC %>% mutate(Year = quantities_prices_capK$Year+2) %>% select(Year, everything())

estObsPC <- merge(estPC, quantities_prices_capK) %>% select(Year, cullPrice, pc) %>% mutate(D = (cullPrice - pc)*100)


estObsPC %>% ggplot(aes(x=Year))+geom_line(aes(y=cullPrice, color="PC RATIONAL")) +
  geom_point(aes(y = cullPrice, color = "PC RATIONAL")) + geom_line(aes(y=pc, color = "PC OBS")) + 
  geom_point(aes(y=pc, color = "PC OBS")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(estObs$Year[1],estObs$Year[nrow(estObs)]))) +
  expand_limits(y = 0)


allPrices <- Reduce(function(...) merge(...), list(pcs,pss,estPC,estPS))

allPrices %>% transmute(Year, ps, fedPrice, fedDiff = ps - fedPrice, pc, cullPrice, cullDiff = pc - cullPrice)


estSL <- colMeans(slNew) %>% as.data.frame()
names(estSL) <- "SL"
estSL <- estSL %>% mutate(Year = quantities_prices_capK$Year+2) %>% select(Year, everything())

estObsSL <- merge(estSL, quantities_prices_capK) %>% select(Year, SL, sl) %>% mutate(D = (SL - sl))


estObsSL %>% ggplot(aes(x=Year))+geom_line(aes(y=SL, color="SL RATIONAL")) +
  geom_point(aes(y = SL, color = "SL RATIONAL")) + geom_line(aes(y=sl, color = "SL OBS")) + 
  geom_point(aes(y=sl, color = "SL OBS")) + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(estObsSL$Year[1],estObsSL$Year[nrow(estObsSL)]))) 




psMean <- 0
for (i in 1: length(quantities_prices_capK$ps)){
  psMean[i] <- mean(quantities_prices_capK$ps[1:i])
}

