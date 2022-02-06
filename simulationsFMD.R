


##### FMD SIMULATIONS

##### Depopulation scenarios

## 20% depopulation
# In this scenario, I remove 20% of the breeding stock from the inventory
# This would change the age distribution. So if I introduce the disease let's say 2009, then in that year I remove a%
# of all the animals. 

Stock_2008L <- Stock %>% filter(Year < 2009)
Stock_2009 <- Stock %>% filter(Year == 2009)

#### Function returning the data with depopulated inventory
dePop <- function(stock, dePopRate){
  stock[,-1] <- stock[,-1] - stock[,-1] * (dePopRate/100)
  return(stock)
}


##### 20% depopulation
Stock2009_20 <- dePop(stock = Stock2009, dePopRate = 20)
Stock2009_20 <- rbind(Stock_2008L, Stock2009_20)

##### 30% depopulation
Stock2009_30 <- dePop(stock = Stock2009, dePopRate = 30)
Stock2009_30 <- rbind(Stock_2008L, Stock2009_30)

##### 40% depopulation
Stock2009_40 <- dePop(stock = Stock2009, dePopRate = 40)
Stock2009_40 <- rbind(Stock_2008L, Stock2009_40)

##### 50% depopulation
Stock2009_50 <- dePop(stock = Stock2009, dePopRate = 50)
Stock2009_50 <- rbind(Stock_2008L, Stock2009_50)

##### 60% depopulation
Stock2009_60 <- dePop(stock = Stock2009, dePopRate = 60)
Stock2009_60 <- rbind(Stock_2008L, Stock2009_60)

##### 70% depopulation
Stock2009_70 <- dePop(stock = Stock2009, dePopRate = 70)
Stock2009_70 <- rbind(Stock_2008L, Stock2009_70)

##### 80% depopulation
Stock2009_80 <- dePop(stock = Stock2009, dePopRate = 80)
Stock2009_80 <- rbind(Stock_2008L, Stock2009_80)

##### 90% depopulation
Stock2009_90 <- dePop(stock = Stock2009, dePopRate = 90)
Stock2009_90 <- rbind(Stock_2008L, Stock2009_90)

#### I have to study the exports and determine the share of production going towards exports



























