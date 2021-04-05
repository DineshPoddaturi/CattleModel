
#############################################################################################################
### Assume a policy where the policy offers subsidies for adopting to the system. For instance ##############
### offers a cost sharing with the producers. Here I simulate the model with different cost sharing #########
#############################################################################################################

###### We will perform this with 30%, 50%, 70% cost sharing by the policy makers #############

costShare <- 0.5

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


########### Here I have estimated prices, demand, sl, and cl from the observed data. I also have the observed counterparts as well. 
prices_predict_est <- prices_predict %>% mutate(ps_est = ps_hat, pc_est = pc_hat, hc_est = hc_hat) %>% filter(ps_hat>0) %>% select(Year, ps_est, pc_est, hc_est)
demand_predict_est <- demand_predict %>% mutate(Demand_est = demand_est, sl_est = sl_est, cl_est = cl_est) %>% filter(Demand_est>0) %>% select(Year, Demand_est, sl_est, cl_est)

prices_costs_obs <- prices_costs
demand_obs <- merge(demand_new, merge(supp_sl_new,supp_cl_new)) %>% mutate(sl = Bill_meatLb_sl, cl = Bill_meatLb_cl) %>% select(Year, Demand, sl, cl)


# Now I have to include these new price and cost estimates to predict the prices.

##### Do the above again. Collect your thoughts. Think carefully. You can do it
# 1. Use observed prices with added costs and estimated sl, cl, demand
# 2. Use estimated prices with added costs and estimated sl, sl, demand
# 3. Use estimated prices with added costs and original sl, cl, demand
# 4. Use observed prices with added costs and original sl, cl, demand

# Case 4: Observed prices with added costs and original sl, cl, demand

cost_price_addedCosts_obs <- cost_price_addedCosts_obs_r

Stock_temp <- Stock%>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=1994  & Year<=2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts_obs %>% select(ps), cost_price_addedCosts_obs %>% select(pc),
                    cost_price_addedCosts_obs %>% select(hc), supp_sl %>% filter(Year>=1994 & Year<=2017) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994 & Year<=2017) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994 & Year<=2017) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

predict_df <- predict_df %>% filter(Year > 2008)
demand_predict_co4<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co4 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co4 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))
adj_co4 <- data.frame(Year = predict_df$Year + 1, adj = numeric(nrow(predict_df)), adj_2009 = numeric(nrow(predict_df)))
shares_co4 <- data.frame(Year = predict_df$Year + 1, shares = numeric(nrow(predict_df)), shares_2009 = numeric(nrow(predict_df)))


for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df$ps[i]
    pc_t <- predict_df$pc[i]
    hc_t <- predict_df$hc[i]
    sl <- predict_df$sl[i]
    cl <- predict_df$cl[i]
    demand <- predict_df$Dissappear[i]
    adj <- demand/(sl+cl)
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  adj_co4$adj[i] <- adj
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  shares_co4$shares[i] <- slShare_t
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co4$mu_tilde[i] <- params_t1[1]
  parameters_co4$s_tilde[i] <- params_t1[2]
  
  p <- c(ps_t, pc_t, hc_t)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1]
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  # adj1 <- demand_t1_hat / (sl_t1_hat + cl_t1_hat)
  # adj_co4$adj_2009[i] <- adj1
  
  ################################################################################################################################
  ##################################################### IMPORTANT ################################################################
  ################################################################################################################################
  
  #### Here we get the shares after the prices are realized. This is because once the new prices are realized the share of 
  #### fed cattle and cull cows might change. This share metric will give us new supply of fed cattle and cull cows. 
  #### I guess this process is mainly giving us the counterfactuals i.e., what would the supply looks like with increased 
  #### costs which change the realized price.
  #### Note: We did not do that in the model estimation because that is representing the real world realization not the 
  #### changes with the policy.
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  shares_co4$shares_2009[i] <- slShare_t
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
  demand_t1_hat <- demand_t1_hat
  
  prices_predict_co4$ps_hat[i] <- ps_hat_t1
  prices_predict_co4$pc_hat[i] <- pc_hat_t1
  prices_predict_co4$hc_hat[i] <- hc_hat_t1
  demand_predict_co4$demand_est[i] <- demand_t1_hat
  demand_predict_co4$sl_est[i] <- sl_t1_hat
  demand_predict_co4$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
  # adj <- demand_t1_hat / (sl_t1_hat + cl_t1_hat)
}

prices_predict_co4 <- prices_predict_co4 %>% filter(ps_hat>0)
demand_predict_co4 <- demand_predict_co4 %>% filter(demand_est>0) %>% mutate(Demand_hat = demand_est, sl_hat = sl_est, 
                                                                             cl_hat = cl_est) %>% select(Year, Demand_hat,
                                                                                                         sl_hat, cl_hat)


# prices_predict_co4_merge <- merge(prices_predict_co4, merge(prices_predict_est, prices_costs_obs))%>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
#          ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
#          ps = ps * 100, pc = pc * 100, hc = hc * 100) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat ,hc, hc_est, hc_hat)
# 
# demand_predict_co4_merge <- merge(demand_predict_co4, merge(demand_predict_est, demand_obs)) %>% 
#   select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)

prices_predict_co4_merge <- left_join(merge(prices_predict_est,prices_costs_obs), prices_predict_co4) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
         ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
         ps = ps * 100, pc = pc * 100, hc = hc * 100,) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])
demand_predict_co4_merge <- left_join(merge(demand_predict_est,demand_obs), demand_predict_co4) %>%
  select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat) %>% filter(Year<=prices_predict_co4_1$Year[nrow(prices_predict_co4_1)])

# prices_predict_co4_merge1 <- merge(prices_predict_co4, prices_costs_obs) %>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, ps = ps * 100, pc = pc * 100, hc = hc * 100) %>%
#   select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat)
# demand_predict_co4_merge1 <- merge(demand_predict_co4, demand_obs)

prices_predict_co4_merge111 <- prices_predict_co4_merge %>% select(Year, ps_est, ps_hat, pc_est, pc_hat, hc_est, hc_hat) %>% filter(Year>2009)

prices_predict_co4_merge111 <- left_join(prices_predict_co4_merge %>% select(Year, ps, pc, hc), prices_predict_co4_merge111
) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat, hc, hc_est, hc_hat)

# prices_predict_co4_merge111$ps_est[-16:-22] <- NA
# prices_predict_co4_merge111$ps_hat[-16:-22] <- NA
# prices_predict_co4_merge111$pc_est[-16:-22] <- NA
# prices_predict_co4_merge111$pc_hat[-16:-22] <- NA
# prices_predict_co4_merge111$hc_est[-16:-22] <- NA
# prices_predict_co4_merge111$hc_hat[-16:-22] <- NA
# 
# demand_predict_co4_merge111 <- demand_predict_co4_merge
# demand_predict_co4_merge111$Demand_est[-16:-22] <- NA
# demand_predict_co4_merge111$Demand_hat[-16:-22] <- NA
# demand_predict_co4_merge111$sl_est[-16:-22] <- NA
# demand_predict_co4_merge111$sl_hat[-16:-22] <- NA
# demand_predict_co4_merge111$cl_est[-16:-22] <- NA
# demand_predict_co4_merge111$cl_hat[-16:-22] <- NA

slaughterPrices_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est,color="Model Estimate"))+
  geom_point(aes(y=ps_est,color="Model Estimate"))+ geom_line(aes(y=ps_hat, color="Estimate with added costs"))+
  geom_point(aes(y=ps_hat,color="Estimate with added costs")) + geom_line(aes(y=ps, color="Observed"))+
  geom_point(aes(y=ps,color="Observed")) + labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge111$Year[1], prices_predict_co4_merge111$Year[nrow(prices_predict_co4_merge111)]))) 

cullPrices_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est,color="Model estimate"))+
  geom_point(aes(y=pc_est,color="Model estimate")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=pc_hat,color="Estimate with added costs")) + geom_line(aes(y=pc, color="Observed")) + 
  geom_point(aes(y=pc,color="Observed")) + labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge111$Year[1], prices_predict_co4_merge111$Year[nrow(prices_predict_co4_merge111)]))) 

holdingCosts_plot_co4 <- prices_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc_est,color="Model estimate"))+
  geom_point(aes(y=hc_est,color="Model estimate")) + geom_line(aes(y=hc_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=hc_hat,color="Estimate with added costs")) + geom_line(aes(y=hc, color="Observed")) + 
  geom_point(aes(y=hc,color="Observed")) + labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge111$Year[1], prices_predict_co4_merge111$Year[nrow(prices_predict_co4_merge111)])))


demand_predict_co4_merge111 <- demand_predict_co4_merge %>% select(Year, Demand_est, Demand_hat, sl_est, sl_hat, cl_est, cl_hat) %>% filter(Year>2009)

demand_predict_co4_merge111 <- left_join(demand_predict_co4_merge %>% select(Year, Demand , sl, cl), demand_predict_co4_merge111
) %>% select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)

stock_slaughter_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl_est,color="Model estimate"))+
  geom_point(aes(y=sl_est,color="Model estimate")) + geom_line(aes(y=sl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=sl_hat,color="Estimate with added costs")) + geom_line(aes(y=sl, color="Observed")) + 
  geom_point(aes(y=sl,color="Observed")) + labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))

stock_cull_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl_est,color="Model estimate"))+
  geom_point(aes(y=cl_est,color="Model estimate")) + geom_line(aes(y=cl_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=cl_hat,color="Estimate with added costs")) + geom_line(aes(y=cl, color="Observed")) + 
  geom_point(aes(y=cl,color="Observed")) + labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))

demand_co4 <- demand_predict_co4_merge111 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand_est,color="Model estimate"))+
  geom_point(aes(y=Demand_est,color="Model estimate")) + geom_line(aes(y=Demand_hat, color="Estimate with added costs")) + 
  geom_point(aes(y=Demand_hat,color="Estimate with added costs")) + geom_line(aes(y=Demand, color="Observed")) + 
  geom_point(aes(y=Demand,color="Observed")) + labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge$Year[1],demand_predict_co4_merge$Year[nrow(demand_predict_co4_merge)])))



# slaughterPrices_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)]))) 
# 
# cullPrices_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)]))) 
# 
# holdingCosts_plot_co41 <- prices_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
#   labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(prices_predict_co4_merge1$Year[1], prices_predict_co4_merge1$Year[nrow(prices_predict_co4_merge1)])))
# 
# 
# stock_slaughter_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_est, color="Estimate with added costs")) + geom_point(aes(y=sl_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))
# 
# stock_cull_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_est, color="Estimate with added costs")) + geom_point(aes(y=cl_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))
# 
# demand_co41 <- demand_predict_co4_merge1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed")) +geom_line(aes(y=demand_est, color="Estimate with added costs")) + geom_point(aes(y=demand_est,color="Estimate with added costs")) + 
#   labs(x="Year", y="Demand meat (in Billion pounds)", colour="") + theme_classic()+ 
#   scale_x_continuous(name="Year", breaks=c(seq(demand_predict_co4_merge1$Year[1],demand_predict_co4_merge1$Year[nrow(demand_predict_co4_merge1)])))


rev_sl <-  prices_predict_co4_merge %>% mutate(slRev_post = (ps_hat/100) * demand_predict_co4_merge$sl_hat,
                                               slRev_model = (ps_est/100) * demand_predict_co4_merge$sl_est,
                                               slRev_obs = (ps/100) * demand_predict_co4_merge$sl,
                                               slRev_diff_obs = slRev_post - slRev_obs,
                                               slRev_diff_model = slRev_post - slRev_model) %>% select(Year, slRev_post, slRev_model, 
                                                                                                       slRev_obs, slRev_diff_obs, slRev_diff_model)

rev_cl <- prices_predict_co4_merge %>% mutate(clRev_post = (pc_hat/100) * demand_predict_co4_merge$cl_hat,
                                              clRev_model = (pc_est/100) * demand_predict_co4_merge$cl_est,
                                              clRev_obs = (pc/100) * demand_predict_co4_merge$cl,
                                              clRev_diff_obs = clRev_post - clRev_obs,
                                              clRev_diff_model = clRev_post - clRev_model) %>% select(Year, clRev_post, clRev_model, 
                                                                                                      clRev_obs, clRev_diff_obs, clRev_diff_model)

rev_total <- merge(rev_sl, rev_cl) %>% mutate(totalRev_post = slRev_post + clRev_post, 
                                              totalRev_model = slRev_model + clRev_model,
                                              totalRev_obs = slRev_obs + clRev_obs,
                                              totalRev_diff_obs = totalRev_post - totalRev_obs,
                                              totalRev_diff_model = totalRev_post - totalRev_model) %>% select(Year, totalRev_post,
                                                                                                               totalRev_model, totalRev_obs,
                                                                                                               totalRev_diff_obs, totalRev_diff_model)
rev_total_2009 <- rev_total %>% filter(Year>2009)

# costs_cl_2009 <- costs_cl %>% mutate(costs_9years = Cull * taggingCosts * 9, 
#                                      cost_Lb_9years = costs_9years/(Cull * dressedWeights_sl_cl$Cull_avg), 
#                                      cost_cl_meat = Bill_meatLb_cl * cost_Lb_9years) %>% filter(Year>2009) %>% select(Year, cost_cl_meat)
# 
# costs_sl_2009 <- costs_sl %>% mutate(costs_2years = Slaughter * taggingCosts * 2, 
#                                      cost_Lb_2years = costs_2years/ (Slaughter * dressedWeights_sl_cl$Slaughter_avg), 
#                                      cost_sl_meat = Bill_meatLb_sl * cost_Lb_2years) %>% filter(Year>2009) %>% select(Year, cost_sl_meat)
# 
# 
# totalCosts_2009 <- merge(costs_cl_2009, costs_sl_2009) %>% mutate(costsBill_total = cost_cl_meat + cost_sl_meat)
# 
# costsRev_2009 <- merge(rev_total_2009, totalCosts_2009) %>% mutate(netRev = totalRev_post - costsBill_total, netRevDiff = totalRev_pre - netRev)
# 

rev_total_Plot <- rev_total %>% ggplot(aes(x=Year))+geom_line(aes(y=totalRev_post,color="Revenue estimate after adding costs (in Billion $)"))+
  geom_point(aes(y=totalRev_post,color="Revenue estimate after adding costs (in Billion $)")) +
  geom_line(aes(y=totalRev_model, color="Model revenue estimate (in Billion $)")) + 
  geom_point(aes(y=totalRev_model,color="Model revenue estimate (in Billion $)")) + 
  geom_line(aes(y=totalRev_obs, color="Observed revenue (in Billion $)")) + 
  geom_point(aes(y=totalRev_obs,color="Observed revenue (in Billion $)")) + 
  labs(x="Year", y="Revenue (in Billion $)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(rev_total$Year[1],rev_total$Year[nrow(rev_total)])))


percentChange_price <- prices_predict_co4_merge %>% mutate(percentChange_ps_obs = round(((ps_hat - ps)/ps)*100,3),
                                                           percentChange_ps_model = round(((ps_hat - ps_est)/ps_est)*100,3),
                                                           percentChange_pc_obs = round(((pc_hat - pc)/pc)*100,3), 
                                                           percentChange_pc_model = round(((pc_hat - pc_est)/pc_est)*100,3),
                                                           percentChange_hc_obs = round(((hc_hat - hc)/hc)*100,3),
                                                           percentChange_hc_model = round(((hc_hat - hc_est)/hc_est)*100,3)) %>% select(Year, percentChange_ps_obs, 
                                                                                                                                        percentChange_ps_model, percentChange_pc_obs,
                                                                                                                                        percentChange_pc_model, percentChange_hc_obs,
                                                                                                                                        percentChange_hc_model) %>% filter(Year>2009)
percentChange_price_ps <- percentChange_price %>% select(Year, percentChange_ps_obs, percentChange_ps_model)
percentChange_price_pc <- percentChange_price %>% select(Year, percentChange_pc_obs, percentChange_pc_model)
percentChange_cost_hc <- percentChange_price %>% select(Year, percentChange_hc_obs, percentChange_hc_model)

percentChange_demand <- demand_predict_co4_merge %>% mutate(percentChange_demand_obs = round(((Demand_hat - Demand)/Demand)*100,3),
                                                            percentChange_demand_model = round(((Demand_hat - Demand_est)/Demand_est)*100,3),
                                                            percentChange_sl_obs = round(((sl_hat - sl)/sl)*100,3),
                                                            percentChange_sl_model = round(((sl_hat - sl_est)/sl_est)*100,3),
                                                            percentChange_cl_obs = round(((cl_hat - cl)/cl)*100,3),
                                                            percentChange_cl_model = round(((cl_hat - cl_est)/cl_est)*100,3)) %>% select(Year, percentChange_demand_obs,
                                                                                                                                         percentChange_demand_model, percentChange_sl_obs, 
                                                                                                                                         percentChange_sl_model, percentChange_cl_obs,
                                                                                                                                         percentChange_cl_model) %>% filter(Year>2009)

percentChange_sl <- percentChange_demand %>% select(Year, percentChange_sl_obs, percentChange_sl_model)
percentChange_cl <- percentChange_demand %>% select(Year, percentChange_cl_obs, percentChange_cl_model)

percentChange_slRev <- rev_sl %>% mutate(percentChange_slRev_model = ((slRev_post - slRev_model)/slRev_model)*100,
                                         percentChange_slRev_obs = ((slRev_post - slRev_obs)/slRev_obs)*100
) %>% select(Year, percentChange_slRev_obs, percentChange_slRev_model) %>% filter(Year>2009)

percentChange_clRev <- rev_cl %>% mutate(percentChange_clRev_model = ((clRev_post - clRev_model)/clRev_model)*100,
                                         percentChange_clRev_obs = ((clRev_post - clRev_obs)/clRev_obs)*100
) %>% select(Year, percentChange_clRev_obs, percentChange_clRev_model) %>% filter(Year>2009)

percentChange_tRev <- rev_total %>% mutate(percentChange_tRev_model = ((totalRev_post - totalRev_model)/totalRev_model)*100,
                                           percentChange_tRev_obs = ((totalRev_post - totalRev_obs)/totalRev_obs)*100
) %>% select(Year, percentChange_tRev_obs, percentChange_tRev_model) %>% filter(Year>2009)


## Here we compute costs (only tagging costs not holding costs) for the supplied meat
costs_cl <- costs_cl_9years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_9years)
costs_sl <- costs_sl_2years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_2years)

costs_hc_cl <- prices_predict_co4_merge %>% mutate(hc_9years = (hc_hat/100)) %>% select(Year, hc_9years)


costsSupply_sl <- demand_predict_co4_merge %>% mutate(costSupply_sl_obs = sl * costs_sl$cost_Lb_2years,
                                                      costSupply_sl_model = sl_est * costs_sl$cost_Lb_2years
) %>% select(Year, costSupply_sl_obs,  costSupply_sl_model)
costsSupply_cl <- demand_predict_co4_merge %>% mutate(costSupply_cl_obs = cl * costs_cl$cost_Lb_9years,
                                                      costSupply_cl_model = cl_est * costs_cl$cost_Lb_9years
) %>% select(Year, costSupply_cl_obs, costSupply_cl_model)
costsSupply_t <- merge(costsSupply_sl, costsSupply_cl) %>% mutate(costSupply_t_obs = costSupply_sl_obs + costSupply_cl_obs,
                                                                  costSupply_t_model = costSupply_sl_model + costSupply_cl_model)

costsRevenues <- merge(rev_cl, merge(rev_sl, merge(rev_total,costsSupply_t))) %>% select(Year, clRev_diff_obs, clRev_diff_model,
                                                                                         costSupply_cl_obs, costSupply_cl_model, 
                                                                                         slRev_diff_obs, slRev_diff_model,
                                                                                         costSupply_sl_obs, costSupply_sl_model,
                                                                                         totalRev_diff_obs, totalRev_diff_model,
                                                                                         costSupply_t_obs, costSupply_t_model) %>% filter(Year>2009)

revDiff_costs_sl <- costsRevenues %>% select(Year, slRev_diff_obs, costSupply_sl_obs, slRev_diff_model, costSupply_sl_model)
revDiff_costs_cl <- costsRevenues %>% select(Year, clRev_diff_obs, costSupply_cl_obs, clRev_diff_model, costSupply_cl_model)
revDiff_costs_t <- costsRevenues %>% select(Year, totalRev_diff_obs, costSupply_t_obs, totalRev_diff_model, costSupply_t_model)

revDiff_costs_sl_pSurp <- revDiff_costs_sl %>% mutate(diffRevCost_sl_obs = slRev_diff_obs - costSupply_sl_obs, 
                                                      diffRevCost_sl_model = slRev_diff_model - costSupply_sl_model) %>% select(
                                                        Year, diffRevCost_sl_obs, diffRevCost_sl_model)
revDiff_costs_cl_pSurp <- revDiff_costs_cl %>% mutate(diffRevCost_cl_obs = clRev_diff_obs - costSupply_cl_obs,
                                                      diffRevCost_cl_model = clRev_diff_model - costSupply_cl_model) %>% select(
                                                        Year, diffRevCost_cl_obs, diffRevCost_cl_model)
revDiff_costs_t_pSurp <- revDiff_costs_t %>% mutate(diffRevCost_t_obs = round(totalRev_diff_obs - costSupply_t_obs,4),
                                                    diffRevCost_t_model = round(totalRev_diff_model - costSupply_t_model,4)) %>% select(
                                                      Year, diffRevCost_t_obs, diffRevCost_t_model)

######## No cost share
# > revDiff_costs_t_pSurp
# > revDiff_costs_t_pSurp
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.4758             -0.4655
# 2 2011           -4.6500             -0.3373
# 3 2012           -2.2253             -0.2975
# 4 2013           -0.6869             -0.3515
# 5 2014           -7.0593             -0.3099
# 6 2015            1.5133             -0.4581
# 7 2016            7.2943             -0.4254

surplus_costShare_0 <- data.frame(Year = revDiff_costs_t_pSurp$Year ,
                                  surplusLoss0= c(0.4655,0.3373,0.2975,0.3515,0.3099,0.4581,0.4254))


######### 20% cost share
# > revDiff_costs_t_pSurp
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.4304             -0.4200
# 2 2011           -4.6071             -0.2881
# 3 2012           -2.1803             -0.2492
# 4 2013           -0.6398             -0.3100
# 5 2014           -7.0213             -0.2653
# 6 2015            1.5489             -0.4156
# 7 2016            7.3318             -0.3804

surplus_costShare_20 <- data.frame(Year = revDiff_costs_t_pSurp$Year ,
                                   surplusLoss20= c(0.4200,0.2881,0.2492,0.3100,0.2653,0.4156,0.3804))

###### 30% cost share
# > revDiff_costs_t_pSurp
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.4076             -0.3972
# 2 2011           -4.5856             -0.2634
# 3 2012           -2.1578             -0.2250
# 4 2013           -0.6162             -0.2892
# 5 2014           -7.0023             -0.2430
# 6 2015            1.5668             -0.3943
# 7 2016            7.3505             -0.3578

surplus_costShare_30 <- data.frame(Year = revDiff_costs_t_pSurp$Year ,
                                   surplusLoss30= c(0.3972,0.2634,0.2250,0.2892,0.2430,0.3943,0.3578))

###### 50% cost share
# > revDiff_costs_t_pSurp
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.3621             -0.3517
# 2 2011           -4.5427             -0.2142
# 3 2012           -2.1128             -0.1766
# 4 2013           -0.5691             -0.2478
# 5 2014           -6.9644             -0.1984
# 6 2015            1.6024             -0.3518
# 7 2016            7.3879             -0.3128

surplus_costShare_50 <- data.frame(Year = revDiff_costs_t_pSurp$Year ,
                                   surplusLoss50= c(0.3517,0.2142,0.1766,0.2478,0.1984,0.3518,0.3128))

###### 70% cost share
# > revDiff_costs_t_pSurp
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.3167             -0.3062
# 2 2011           -4.4998             -0.1649
# 3 2012           -2.0678             -0.1283
# 4 2013           -0.5220             -0.2063
# 5 2014           -6.9264             -0.1538
# 6 2015            1.6381             -0.3093
# 7 2016            7.4253             -0.2677

surplus_costShare_70 <- data.frame(Year = revDiff_costs_t_pSurp$Year ,
                                   surplusLoss70= c(0.3062,0.1649,0.1283,0.2063,0.1538,0.3093,0.2677))

surplusLoss_costShare <- round(merge(surplus_costShare_0,merge(surplus_costShare_20,merge(surplus_costShare_30,
                                                                                     merge(surplus_costShare_50,surplus_costShare_70)))),3)
surplusLoss_costShare[,-1] <- surplusLoss_costShare[,-1] * (-1)

surplusChange_0ToAll <- surplusLoss_costShare
surplusChange_0ToAll <- surplusChange_0ToAll%>% filter(Year==2010) %>% mutate(change_0To20 = ((surplusLoss20 - surplusLoss0)/surplusLoss0)*100,
                                                        change_0To30 = ((surplusLoss30 - surplusLoss0)/surplusLoss0)*100,
                                                        change_0To50 = ((surplusLoss50 - surplusLoss0)/surplusLoss0)*100,
                                                        change_0To70 = ((surplusLoss70 - surplusLoss0)/surplusLoss0)*100) %>% select(
                                                          Year, change_0To20, change_0To30, change_0To50, change_0To70)
surplusChange_20ToAll <- surplusLoss_costShare
surplusChange_20ToAll <- surplusChange_20ToAll%>% filter(Year==2010) %>% mutate(change_20To30 = ((surplusLoss30 - surplusLoss20)/surplusLoss20)*100,
                                                                              change_20To50 = ((surplusLoss50 - surplusLoss20)/surplusLoss20)*100,
                                                                              change_20To70 = ((surplusLoss70 - surplusLoss20)/surplusLoss20)*100) %>% select(
                                                                                Year, change_20To30, change_20To50, change_20To70)

surplusChange_30ToAll <- surplusLoss_costShare
surplusChange_30ToAll <- surplusChange_30ToAll%>% filter(Year==2010) %>% mutate(change_30To50 = ((surplusLoss50 - surplusLoss30)/surplusLoss30)*100,
                                                                                change_30To70 = ((surplusLoss70 - surplusLoss30)/surplusLoss30)*100) %>% select(
                                                                                  Year, change_30To50, change_30To70)


surplusChange_50ToAll <- surplusLoss_costShare
surplusChange_50ToAll <- surplusChange_50ToAll%>% filter(Year==2010) %>% mutate(change_50To70 = ((surplusLoss70 - surplusLoss50)/surplusLoss50)*100) %>% select(
                                                                                  Year, change_50To70)



# surplusLoss_costShare %>% mutate(zero_20 = surplusLoss0 - surplusLoss20,
#                                  zero_30 = surplusLoss0 - surplusLoss30,
#                                  zero_50 = surplusLoss0 - surplusLoss50,
#                                  zero_70 = surplusLoss0 - surplusLoss70)



