
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

summary( lead(quantities_prices_capK$pc)-quantities_prices_capK$pc )
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.29217 -0.03717  0.01442  0.01723  0.06054  0.25933        1 

summary( lead(quantities_prices_capK$pc,3)-quantities_prices_capK$pc )
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -0.386667 -0.003938  0.059292  0.080153  0.192417  0.371250         3 


mean(colMeans(fedPrice[[4]][,apply(fedPrice[[4]],2,function(x) !all(is.na(x)))]))
