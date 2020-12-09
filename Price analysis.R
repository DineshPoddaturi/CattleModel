require(tidyverse)
require(readxl)

#https://www.nass.usda.gov/Charts_and_Maps/graphics/data/priceca.txt

prices <- read_table("priceca.txt") %>% as.data.frame()

names(prices) <- c('Year', 'Month', 'All_Beef_Cattle', 'Calves', 'Cows', 'Heifers_Steers')

prices <- prices[-1,]

rownames(prices) <- NULL

write.csv(prices,"prices.csv", row.names = FALSE)

prices <- read_csv('prices.csv') %>% as.data.frame()


prices <- prices %>% select(Year, Month, Cows, Heifers_Steers)

prices <- prices %>% mutate(Cows=Cows, Heifers_Steers = Heifers_Steers)
prices <- prices %>% mutate(premium =  (Heifers_Steers/(100))- (Cows/(100) )  )
prices <- prices %>% mutate(Cows_Pound = Cows/(100), 
                            Heifers_Steers_Pound = Heifers_Steers/(100))

prices_std <- sd(prices$premium) 
prices_mean <- mean(prices$premium)

cow_summary <- summary(prices$Cows_Pound)
heifer_summary <- summary(prices$Heifers_Steers_Pound)









#################################################################
prices_retail <- read_excel('cuts-edited.xls') %>% select(`All uncooked ground beef`, `All uncooked beef steaks`)

prices_retail <- prices_retail[-1,]

prices_retail <- prices_retail %>% as.data.frame() %>% drop_na()

prices_retail$`All uncooked ground beef` <- as.numeric(prices_retail$`All uncooked ground beef`)
prices_retail$`All uncooked beef steaks` <- as.numeric(prices_retail$`All uncooked beef steaks`)

prices_retail <- prices_retail %>% mutate(premium = `All uncooked beef steaks` - `All uncooked ground beef`)

prices_retail_mean <- mean(prices_retail$premium)
prices_retail_std <- sd(prices_retail$premium)

prices_retail_cow <- mean(prices_retail$`All uncooked ground beef`)
prices_retail_heifer <- mean(prices_retail$`All uncooked beef steaks`)

#######################################################################



#https://quickstats.nass.usda.gov/


cattle_inv <- read_csv('Total Cattle - Inventory.csv') %>% as.data.frame() %>% select(Year,Value) %>% 
  mutate(cattle=Value) %>% select(Year,cattle)

calves_inv <- read_csv('Total Calves - Inventory.csv') %>% as.data.frame() %>% select(Year, Value) %>% 
  mutate(calves=Value) %>% select(Year,calves)

cowBeef_inv <- read_csv('Cows, Beef - Inventory.csv') %>% as.data.frame() %>% select(Year, Value) %>% 
  mutate(cowBeef=Value) %>% select(Year,cowBeef)

bull_inv <- read_csv('Bull - Inventory.csv') %>% as.data.frame() %>% select(Year, Value) %>% 
  mutate(bull=Value) %>% select(Year,bull)

all_inv <- merge(merge(cattle_inv, calves_inv,by="Year"), merge(cowBeef_inv,bull_inv, by="Year"), 
                 by="Year")

all_inv <- all_inv %>% mutate(K = cattle - calves - bull)

all_inv_avg <- mean(all_inv$K)






# https://data.ers.usda.gov/reports.aspx?ID=17832&AspxAutoDetectCookieSupport=1#Pd6e38190f9a74fc68f89645303288396_2_17iT0R0x0







# prices_mean <- prices %>% group_by(Year) %>% summarize(All_Beef_Cattle = mean(All_Beef_Cattle, na.rm = TRUE), 
#                                         Calves = mean(Calves, na.rm = TRUE),
#                                         Cows = mean(Cows, na.rm = TRUE),
#                                         Heifers_Steers = mean(Heifers_Steers, na.rm = TRUE)) %>% as.data.frame()
# 
# 
# 
# prices_cows_heifers <- prices_mean %>% select(Year, Cows, Heifers_Steers)
# 
# prices_cows_heifers <- prices_cows_heifers %>% mutate(Cows=Cows/0.63, Heifers_Steers = Heifers_Steers/0.63)









