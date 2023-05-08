########## PROJECTIONS AGE DISTRIBUTION ARRANGEMENT

invForecast <- beefINV_FORECAST %>% select(Year, K) %>% filter(Year > 2022)
# k3_Proj <- k3 %>% filter(Year >= invForecast$Year[1] & Year <= invForecast$Year[1]+1) 
slShock <- rnorm(n = nrow(invForecast), mean = 1, sd = std(obsEst_sl_Supply$slShock))
clShock <- rnorm(n = nrow(invForecast), mean = 1, sd = std(obsEst_cl_Supply$clShock))

invForecast <- cbind(invForecast, cbind(slShock, clShock))

stockForecast <- Stock %>% filter(Year <= 2022 & Year > 2000)
mergedForecast <- merge(stockForecast, invForecast, all=TRUE)


# invForecast1 <-  data.frame(Year = numeric(10), K = NA, k3 = NA,
#                                         k4 =  NA, k5 =  NA, k6 =  NA, 
#                                         k7 =  NA, k8 =  NA, k9 =  NA)
# 
# invForecast1$Year <- seq(from=2022, to=2031)
# 
# mergedForecast <- merge(stockForecast, invForecast1, all=TRUE)

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




### Here I am estimating the production back and check whether my projected projection makes a 
### connection with the existing data. For this I am using the projected data frame.

#### First I will add some rows above to the data frame

mergedForecast_backCast <- mergedForecast %>% filter(Year < mergedForecast_Proj$Year[1])

mergedForecast_Proj_BackCast <- rbind(mergedForecast_backCast, mergedForecast_Proj)

# mergedForecast_Proj_BackCastBKP <- mergedForecast_Proj_BackCast


##### In backcasting, change the shock from 1 to the shock in that specific year from the fitted model

for (i in 1:nrow(mergedForecast_Proj_BackCast)) {
  
  # i <- 1
  
  yearBackCast <- mergedForecast_Proj_BackCast$Year[i]
  
  if(yearBackCast == 2022){
    break
  }else{
    
    #### Getting cl Production
    k6nB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-1) %>% select(k6) %>% as.numeric()
    k7nB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-1) %>% select(k7) %>% as.numeric()
    k8nB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-1) %>% select(k8) %>% as.numeric()
    k9nB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-1) %>% select(k9) %>% as.numeric()
    clShnB <- 1
    cAvgB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast) %>% select(Cull_avg)
    clNewB <-  ((k9nB + (1-delta) * k8nB + (1-delta) * k7nB) * clShnB +
                 (delta * (k8nB + k7nB + k6nB) - (k7nB + k8nB + k9nB)) )* (cAvgB/1000000000)
    
    clNewB <- round(as.numeric(clNewB),2)
    
    #### Getting sl Production
    slm1B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-2) %>% select(sl) %>% as.numeric()
    slShm1B <- 1
    Km2B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-3) %>% select(K) %>% as.numeric()
    Km3B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-4) %>% select(K) %>% as.numeric()
    k9m2B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-3) %>% select(k9) %>% as.numeric()
    k8m2B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-3) %>% select(k8) %>% as.numeric()
    k7m2B <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-3) %>% select(k7) %>% as.numeric()
    fedAvgB <- mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast-1) %>%select(Slaughter_avg) %>% as.numeric()
    
    fedImprts <- as.numeric(mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast - 2) %>% select(Imports)) %>% as.numeric()
    fedExprts <- as.numeric(mergedForecast_Proj_BackCast %>% filter(Year == yearBackCast - 2) %>% select(Exports)) %>% as.numeric()
    
    fedNetImpExp <- fedImprts - fedExprts
    
    # slNewB <- ((g - 0.37 * g) * Km2B * slShm1B +
    #             ((1 - 0.37 * g) * g * delta * (Km2B - (g - 0.37 * g) * Km3B -
    #                                              (k9m2B + (1-delta) * k8m2B + (1-delta) * k7m2B))) ) * (fedAvgB/1000000000)
    
    slNewB <- ((g - 0.37 * g) * Km2B * slShm1B +
                 ((1 - 0.37 * g) * g * delta * (Km2B - (g - 0.37 * g) * Km3B -
                                                  (k9m2B + (1-delta) * k8m2B + (1-delta) * k7m2B))) + fedNetImpExp) * (fedAvgB/1000000000)
    
    
    
    slNewB <- round(as.numeric(slNewB), 2)
  }
  
  mergedForecast_Proj_BackCast$sl[i] <- slNewB
  mergedForecast_Proj_BackCast$cl[i] <- clNewB
  
}


mergedForecast_Proj_BC <- mergedForecast_Proj_BackCast %>% filter(!is.na(sl)) %>% 
  mutate(slBC = sl, clBC = cl) %>% select(Year, slBC, clBC) %>% filter(Year < 2022 & Year >=2018)

mergedForecast_Proj_BC_SL <- merge(estProj_SLV, mergedForecast_Proj_BC, all=TRUE) %>%
  select(-errMean, -errmedian, -slMean, -clBC)

mergedForecast_Proj_BC_CL <- merge(estProj_CLV, mergedForecast_Proj_BC, all=TRUE) %>%
  select(-errMean, -errmedian, -clMean, -slBC)

mergedForecast_Proj_BC_SL <- mergedForecast_Proj_BC_SL %>% 
  mutate(Sl = coalesce(Sl,slBC)) %>% select(Year, slMedian, Sl) %>% filter(Year > 2015)

mergedForecast_Proj_BC_CL <- mergedForecast_Proj_BC_CL %>%
  mutate(Cl = coalesce(Cl,clBC)) %>% select(Year, clMedian, Cl) %>% filter(Year > 2015)

estProj_SLV_plotsBC <- mergedForecast_Proj_BC_SL %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=slMedian, color="SL Fitted")) +
  geom_point(aes(y = slMedian, color = "SL Fitted")) +
  geom_line(aes(y=Sl, color="SL PROJECTION")) +
  geom_point(aes(y=Sl, color="SL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_SLV$Year[1],
                                  estProj_SLV$Year[nrow(estProj_SLV)])))+ 
  scale_y_continuous(name="Fed Cattle Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))

estProj_CLV_plotsBC <- mergedForecast_Proj_BC_CL %>% ggplot(aes(x=Year)) +
  geom_line(aes(y=clMedian, color="CL Fitted")) +
  geom_point(aes(y = clMedian, color = "CL Fitted")) +
  geom_line(aes(y=Cl, color="CL PROJECTION")) +
  geom_point(aes(y=Cl, color="CL PROJECTION"))+
  scale_x_continuous(name="Year", 
                     breaks=c(seq(estProj_CLV$Year[1],
                                  estProj_CLV$Year[nrow(estProj_CLV)])))+ 
  scale_y_continuous(name="Cull Cow Production") +  theme_classic() + 
  theme(legend.position="bottom", legend.box = "horizontal",text = element_text(size = 12)) +
  theme(legend.title=element_blank()) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ 
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt")))


##### The below plots of fed cattle (Imports and exports are included. Specifically, I used lagged - 2 years imports and exports) 
##### and cull cow production
estProj_SLV_plotsBC_IE <- estProj_SLV_plotsBC

estProj_CLV_plotsBC

#### From the above plots it is apparent from back casting of the production, both fed cattle and cull cow 
#### meat production is over estimated in the year 2020. This back casting is purely related to age distribution.
#### Note: Since I already have the age distribution determined. I may not have to use the prices in that specific
#### years to get the production. However, I can and should be able to back cast with prices too. 
#### In an instance of using the prices to back cast, the age distribution might change. As the data is already observed
#### I may not have to do that.
#### When back casting the production, I am assuming the market is functioning properly. But as we know we did had 
#### a huge exogenous shock in 2019-2020. So this might be the case we see a bug jump in the supply of the meat.



##### The below plots of fed cattle and cull cow production
estProj_SLV_plotsBC_NIE <- estProj_SLV_plotsBC

estProj_CLV_plotsBC

## In the case where I do not include the exports and imports, purely from the age distribution, the model underestimated 
## fed cattle production back casting. When I closely observe the production of fed cattle, the supply is increasing 
## overtime from 2018 and so on.

















