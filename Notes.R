#### Notes:

#### This contains the output we get for different iterations of the code

# if(i > 1){
#   if(quantities_prices_capK$ps[i] < quantities_prices_capK$ps[i-1]){
#     ps <- (quantities_prices_capK$ps[i] + quantities_prices_capK$ps[i-1] + quantities_prices_capK$ps[i-2] + quantities_prices_capK$ps[i-3])/4
#   }
#   if(quantities_prices_capK$pc[i] < quantities_prices_capK$pc[i-1]){
#     pc <- (quantities_prices_capK$pc[i] + quantities_prices_capK$pc[i-1] + quantities_prices_capK$pc[i-2] + quantities_prices_capK$pc[i-3])/4
#   }
# }



pricePS <- prices_ps[,i] %>% as.data.frame()
names(pricePS) <- "ps"
ggplot(data = pricePS, aes(x=ps)) + geom_density()

pricePC <- prices_pc[,i] %>% as.data.frame()
names(pricePC) <- "pc"
ggplot(data = pricePC, aes(x=pc)) + geom_density()








