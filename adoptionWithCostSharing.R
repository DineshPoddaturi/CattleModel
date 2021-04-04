costShare <- 0

taggingCosts <- round(total_Costs * (1-costShare),3)

aggCosts <- (Stock_temp[,2] * taggingCosts) %>% as.data.frame()
names(aggCosts) <- "AggregateCosts"
aggCosts <- aggCosts %>% mutate(Year = Stock_temp$Year, AggregateCosts_mill = AggregateCosts/1000000) %>% select(Year, everything())

#Costs for each age in our data in million $
Stock_temp_costs <-  (Stock_temp[,-1] * taggingCosts)  %>% as.data.frame()

Stock_temp_costs_mill <- (Stock_temp_costs/1000000) %>% mutate(Year = Stock_temp$Year) %>% select(Year, everything())

costs_cl <- supp_cl %>% mutate(costs = Cull * taggingCosts, cost_perLb = costs/(Cull * dressedWeights_sl_cl$Cull_avg))
costs_sl <- supp_sl %>% mutate(costs = Slaughter * taggingCosts, cost_perLb = costs/(Slaughter * dressedWeights_sl_cl$Slaughter_avg))


### Remember the culled cattle stayed longer (In our case 9 years). So we have to account for that. Basically costs for 9 years.
### First work on the culled cattle. This might take some time.

# Here I am multiplying the costs by 9. This is because the cow stayed in the stock for 9 years increasing the costs.
costs_cl_9years <- costs_cl %>% mutate(costs_9years = Cull * taggingCosts * 9, cost_Lb_9years = costs_9years/(Cull * dressedWeights_sl_cl$Cull_avg)) %>% 
  select(Year, Cull, costs_9years, cost_Lb_9years)

### The same applies for the fed cattle. They are alive for two years. So costs for two years need to be included.
costs_sl_2years <- costs_sl %>% mutate(costs_2years = Slaughter * taggingCosts * 2, cost_Lb_2years = costs_2years/(Slaughter * dressedWeights_sl_cl$Slaughter_avg)) %>%
  select(Year, Slaughter, costs_2years, cost_Lb_2years)


# Now think about how to include these costs in the model. 
costs_sl_cl <- merge(costs_cl_9years, costs_sl_2years) %>% select(Year, cost_Lb_9years, cost_Lb_2years) %>% 
  mutate(cost_both = cost_Lb_9years + cost_Lb_2years)

# A simple increase in cost is increasing the fed cattle and cull cow prices. 
# I kept the supply and demand same as observed in that year. No change in it at all.

# Think about how you are going to change the quantities? 
# Are we going to keep the observed supply and demand as it is?
# Keep the same demand as this year but change the supply?
# We know the share depends on ps and pc, so if it changes the sl & cl supply changes as well. Think more about these scenarios


# I am building the added costs dataframe
# Change the costs from 2009. I take the year from the paper
cost_price_hat <- prices_predict %>% filter(ps_hat>0)

cost_price_addedCosts <- merge(cost_price_hat,costs_sl_cl) %>% select(Year, ps_hat, pc_hat, hc_hat, cost_both)

ccc <- cost_price_addedCosts %>% filter(Year>=2009) %>% mutate(hc_hat = hc_hat + cost_both) %>% select (-cost_both)

cost_price_addedCosts <- left_join(cost_price_addedCosts, ccc, by = "Year") %>% 
  mutate(ps_hat = ps_hat.x, pc_hat = pc_hat.x, hc_hat = ifelse(is.na(hc_hat.y), hc_hat.x, hc_hat.y)) %>% 
  select(Year, ps_hat, pc_hat, hc_hat)


# I will also add the costs to the observed prices and costs. I will try the predictions with the observed first and see how things turn out.
cost_price_obs <- merge(prices_costs,costs_sl_cl) %>% select(Year, ps, pc, hc, cost_both)

costs_TBAdded <- cost_price_obs %>% filter(Year>=2009) %>% mutate(hc = hc + cost_both) %>% select (-cost_both)

cost_price_addedCosts_obs_r <- left_join(cost_price_obs, costs_TBAdded, by = "Year") %>% 
  mutate(ps = ps.x, pc = pc.x, hc = ifelse(is.na(hc.y), hc.x, hc.y)) %>% 
  select(Year, ps, pc, hc)