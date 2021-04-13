################ different adoption rate ##########
adoption <- 0.9

Stock_temp <- Stock%>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=1994)%>% select(Slaughter_avg),
                    prices_costs%>%filter(Year>=1994)%>% select(ps), prices_costs%>%filter(Year>=1994)%>% select(pc),
                    prices_costs%>%filter(Year>=1994)%>% select(hc), supp_sl %>% filter(Year>=1994) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")

# predict_df$pc <- predict_df$pc + 1
# predict_df$hc <- (((g * (beta^3) * predict_df$ps) + (beta - 1) * predict_df$pc)/(1 + g * beta * (gamma0 + beta * gamma1)))

predict_df_adopt_Y <- predict_df %>% mutate(K = K * adoption, k3 = k3 * adoption, sl = sl * adoption, 
                                            cl = cl* adoption, Dissappear = Dissappear * adoption)

predict_df_adopt_N <- predict_df %>% mutate(K = K * (1-adoption), k3 = k3 * (1-adoption), sl = sl * (1-adoption), 
                                            cl = cl* (1-adoption), Dissappear = Dissappear * (1-adoption))


demand_predict_adopt <- data.frame(Year = predict_df_adopt_Y$Year+1, demand_est = numeric(nrow(predict_df_adopt_Y)), 
                                   sl_est = numeric(nrow(predict_df_adopt_Y)), cl_est = numeric(nrow(predict_df_adopt_Y)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_adopt <- data.frame(Year = predict_df_adopt_Y$Year+1, ps_hat = numeric(nrow(predict_df_adopt_Y)), 
                                   pc_hat = numeric(nrow(predict_df_adopt_Y)), hc_hat = numeric(nrow(predict_df_adopt_Y)))

parameters_adopt <- data.frame(Year = predict_df_adopt_Y$Year+1, mu_tilde = numeric(nrow(predict_df_adopt_Y)), 
                               s_tilde = numeric(nrow(predict_df_adopt_Y)))
adj_factor_adopt <- data.frame(Year = predict_df_adopt_Y$Year+1, adj = numeric(nrow(predict_df_adopt_Y)))
shares_slcl_adopt <- data.frame(Year = predict_df_adopt_Y$Year+1, share_pre = numeric(nrow(predict_df_adopt_Y)), 
                                share_post = numeric(nrow(predict_df_adopt_Y)))

predict_df_adopt <- predict_df_adopt_N

for(i in 1:(nrow(predict_df_adopt)-2)){
  
  # i <- 1
  K_t <- predict_df_adopt$K[i]
  k3_t2 <- predict_df_adopt$k3[i+2]
  ps_t <- predict_df_adopt$ps[i]
  pc_t <- predict_df_adopt$pc[i]
  hc_t <- predict_df_adopt$hc[i]
  dressed_t <- predict_df_adopt$dressedWeight[i]
  sl <- predict_df_adopt$sl[i]
  cl <- predict_df_adopt$cl[i]
  demand <- predict_df_adopt$Dissappear[i]
  adj <- demand/(sl+cl)
  
  # if(adj>1){
  #   adj <- 1/adj
  # }
  
  adj_factor_adopt$adj[i] <- adj
  
  imports_t <- predict_df_adopt$imports[i]
  exports_t <- predict_df_adopt$exports[i]
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
  shares_slcl_adopt$share_pre[i] <- slShare_t
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_adopt$mu_tilde[i] <- params_t1[1]
  parameters_adopt$s_tilde[i] <- params_t1[2]
  
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
  
  # slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  # sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  # cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
  
  prices_predict_adopt$ps_hat[i] <- ps_hat_t1
  prices_predict_adopt$pc_hat[i] <- pc_hat_t1
  prices_predict_adopt$hc_hat[i] <- hc_hat_t1
  demand_predict_adopt$demand_est[i] <- demand_t1_hat
  demand_predict_adopt$sl_est[i] <- sl_t1_hat
  demand_predict_adopt$cl_est[i] <- cl_t1_hat
}

pricesMerge_new_adopt <- merge(prices_predict_adopt,prices_costs) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot_adopt <- pricesMerge_new_adopt   %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

cullPrices_plot_adopt <- pricesMerge_new_adopt %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7)) 

holdingCosts_plot_adopt <- pricesMerge_new_adopt   %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

Master_sl_cl_adopt <- merge(demand_predict_adopt,merge(supp_sl_new,supp_cl_new)) %>% filter(
  sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, 
                        Bill_meatLb_cl, cl_est)  %>% mutate(
                          Bill_meatLb_sl = Bill_meatLb_sl * (1-adoption),
                          Bill_meatLb_cl = Bill_meatLb_cl * (1-adoption))

names(Master_sl_cl_adopt) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl_adopt[,-1] <- round(Master_sl_cl_adopt[,-1],3)
Master_sl_cl_adopt$Year <- as.numeric(Master_sl_cl_adopt$Year)

stock_slaughter_adopt <- Master_sl_cl_adopt   %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.9, 0.9)) 

stock_cull_adopt <- Master_sl_cl_adopt %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.8, 0.3))



prices_predict_est_adopt_Y <- prices_predict_adopt %>% mutate(ps_estY = ps_hat, pc_estY = pc_hat, hc_estY = hc_hat) %>% filter(ps_estY>0) %>% select(Year, ps_estY, pc_estY, hc_estY)
demand_predict_est_adopt_Y <- demand_predict_adopt %>% mutate(Demand_estY = demand_est, 
                                                              sl_estY = sl_est, cl_estY = cl_est) %>% filter(Demand_estY>0) %>% select(Year, Demand_estY, sl_estY, cl_estY)

prices_predict_est_adopt_N <- prices_predict_adopt %>% mutate(ps_estN = ps_hat, pc_estN = pc_hat, hc_estN = hc_hat) %>% filter(ps_estN>0) %>% select(Year, ps_estN, pc_estN, hc_estN)
demand_predict_est_adopt_N <- demand_predict_adopt %>% mutate(Demand_estN = demand_est, 
                                                              sl_estN = sl_est, cl_estN = cl_est) %>% filter(Demand_estN>0) %>% select(Year, Demand_estN, sl_estN, cl_estN)


########################################################################################################################################################################

costShare <- 0.7

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
# prices_predict_est <- prices_predict %>% mutate(ps_est = ps_hat, pc_est = pc_hat, hc_est = hc_hat) %>% filter(ps_hat>0) %>% select(Year, ps_est, pc_est, hc_est)
# demand_predict_est <- demand_predict %>% mutate(Demand_est = demand_est, sl_est = sl_est, cl_est = cl_est) %>% filter(Demand_est>0) %>% select(Year, Demand_est, sl_est, cl_est)

prices_costs_obs <- prices_costs
demand_obs <- merge(demand_new, merge(supp_sl_new,supp_cl_new)) %>% mutate(sl = Bill_meatLb_sl, cl = Bill_meatLb_cl) %>% select(Year, Demand, sl, cl)

########################################################################################################################################################################
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

predict_df_adopt <- predict_df %>% mutate(K = K * adoption, k3 = k3 * adoption, sl = sl * adoption, 
                                          cl = cl * adoption, Dissappear = Dissappear * adoption)


demand_predict_co4_adopt <- data.frame(Year = predict_df_adopt$Year+1, 
                                      demand_est = numeric(nrow(predict_df_adopt)), 
                                      sl_est = numeric(nrow(predict_df_adopt)), 
                                      cl_est = numeric(nrow(predict_df_adopt)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co4_adopt <- data.frame(Year = predict_df_adopt$Year+1, ps_hat = numeric(nrow(predict_df_adopt)), 
                                       pc_hat = numeric(nrow(predict_df_adopt)), 
                                       hc_hat = numeric(nrow(predict_df_adopt)))

parameters_co4_adopt <- data.frame(Year = predict_df_adopt$Year+1, mu_tilde = numeric(nrow(predict_df_adopt)), 
                                   s_tilde = numeric(nrow(predict_df_adopt)))
adj_co4_adopt <- data.frame(Year = predict_df_adopt$Year + 1, adj = numeric(nrow(predict_df_adopt)), 
                            adj_2009 = numeric(nrow(predict_df_adopt)))
shares_co4_adopt <- data.frame(Year = predict_df_adopt$Year + 1, shares = numeric(nrow(predict_df_adopt)), 
                               shares_2009 = numeric(nrow(predict_df_adopt)))


for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 2
  K_t <- predict_df_adopt$K[i]
  k3_t2 <- predict_df_adopt$k3[i+2]
  # imports_t <- predict_df$imports[i]
  
  if(i<=1){
    ps_t <- predict_df_adopt$ps[i]
    pc_t <- predict_df_adopt$pc[i]
    hc_t <- predict_df_adopt$hc[i]
    sl <- predict_df_adopt$sl[i]
    cl <- predict_df_adopt$cl[i]
    demand <- predict_df_adopt$Dissappear[i]
    adj <- demand/(sl+cl)
  }
  
  ps_t <- predict_df_adopt$ps[i]
  pc_t <- predict_df_adopt$pc[i]
  hc_t <- predict_df_adopt$hc[i]
  dressed_t <- predict_df_adopt$dressedWeight[i]
  sl <- predict_df_adopt$sl[i]
  cl <- predict_df_adopt$cl[i]
  demand <- predict_df_adopt$Dissappear[i]
  adj <- demand/(sl+cl)
  adj_co4_adopt$adj[i] <- adj
  
  
  imports_t <- predict_df_adopt$imports[i]
  exports_t <- predict_df_adopt$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  shares_co4_adopt$shares[i] <- slShare_t
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co4_adopt$mu_tilde[i] <- params_t1[1]
  parameters_co4_adopt$s_tilde[i] <- params_t1[2]
  
  
  
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
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  shares_co4$shares_2009[i] <- slShare_t
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  demand_t1_hat <- (sl_t1_hat + cl_t1_hat)
  
  prices_predict_co4_adopt$ps_hat[i] <- ps_hat_t1
  prices_predict_co4_adopt$pc_hat[i] <- pc_hat_t1
  prices_predict_co4_adopt$hc_hat[i] <- hc_hat_t1
  demand_predict_co4_adopt$demand_est[i] <- demand_t1_hat
  demand_predict_co4_adopt$sl_est[i] <- sl_t1_hat
  demand_predict_co4_adopt$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
  # adj <- demand_t1_hat / (sl_t1_hat + cl_t1_hat)
}

prices_predict_co4_adopt <- prices_predict_co4_adopt %>% filter(ps_hat>0)
demand_predict_co4_adopt <- demand_predict_co4_adopt %>% filter(demand_est>0) %>% mutate(Demand_hat = demand_est, sl_hat = sl_est, 
                                                                                         cl_hat = cl_est) %>% select(Year, Demand_hat,
                                                                                                                     sl_hat, cl_hat)


# prices_predict_co4_merge <- merge(prices_predict_co4, merge(prices_predict_est, prices_costs_obs))%>% 
#   mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
#          ps_est = ps_est * 100, pc_est = pc_est * 100, hc_est = hc_est * 100,
#          ps = ps * 100, pc = pc * 100, hc = hc * 100) %>% select(Year, ps, ps_est, ps_hat, pc, pc_est, pc_hat ,hc, hc_est, hc_hat)
# 
# demand_predict_co4_merge <- merge(demand_predict_co4, merge(demand_predict_est, demand_obs)) %>% 
#   select(Year, Demand, Demand_est, Demand_hat, sl, sl_est, sl_hat, cl, cl_est, cl_hat)


prices_predict_co4_merge_adopt <- left_join(merge(prices_predict_est_adopt_Y, merge(prices_predict_est_adopt_N,prices_costs_obs)), prices_predict_co4_adopt) %>% 
  mutate(ps_hat = ps_hat * 100, pc_hat = pc_hat * 100, hc_hat = hc_hat * 100, 
         ps_estY = ps_estY * 100, pc_estY = pc_estY * 100, hc_estY = hc_estY * 100,
         ps_estN = ps_estN * 100, pc_estN = pc_estN * 100, hc_estN = hc_estN * 100,
         ps = ps * 100, pc = pc * 100, hc = hc * 100,) %>% select(
           Year, ps, ps_estY,ps_estN, ps_hat, pc, pc_estY,pc_estN,pc_hat, hc, hc_estY, hc_estN, hc_hat) %>% filter(
             Year<=prices_predict_co4_adopt$Year[nrow(prices_predict_co4_adopt)])
demand_predict_co4_merge_adopt <- left_join(merge(demand_predict_est_adopt_Y,merge(demand_predict_est_adopt_N, demand_obs)), demand_predict_co4_adopt) %>%
  select(Year, Demand, Demand_estY, Demand_estN, Demand_hat, sl, sl_estY, sl_estN, sl_hat, cl, cl_estY,cl_estN,cl_hat) %>% filter(Year<=prices_predict_co4_adopt$Year[nrow(prices_predict_co4_adopt)])

rev_sl_adopt <-  prices_predict_co4_merge_adopt %>% mutate(slRev_post_adopt = (ps_hat/100) * demand_predict_co4_merge_adopt$sl_hat,
                                                           slRev_model_adopt = (ps_estY/100) * demand_predict_co4_merge_adopt$sl_estY,
                                                           slRev_obs_adopt = (ps/100) * demand_predict_co4_merge_adopt$sl * adoption,
                                                           slRev_diff_obs = slRev_post_adopt - slRev_obs_adopt,
                                                           slRev_diff_model = slRev_post_adopt  - slRev_model_adopt) %>% select(
                                                             Year, slRev_post_adopt, slRev_model_adopt, slRev_obs_adopt, 
                                                             slRev_diff_obs, slRev_diff_model)

rev_cl_adopt <- prices_predict_co4_merge_adopt %>% mutate(clRev_post_adopt = (pc_hat/100) * demand_predict_co4_merge_adopt$cl_hat,
                                                          clRev_model_adopt = (pc_estY/100) * demand_predict_co4_merge_adopt$cl_estY,
                                                          clRev_obs_adopt = (pc/100) * demand_predict_co4_merge_adopt$cl * adoption,
                                                          clRev_diff_obs = clRev_post_adopt - clRev_obs_adopt,
                                                          clRev_diff_model = clRev_post_adopt - clRev_model_adopt) %>% select(
                                                            Year, clRev_post_adopt, clRev_model_adopt, clRev_obs_adopt, 
                                                            clRev_diff_obs, clRev_diff_model)

rev_total_adopt <- merge(rev_sl_adopt, rev_cl_adopt) %>% mutate(
  totalRev_post = slRev_post_adopt + clRev_post_adopt , totalRev_model = slRev_model_adopt + clRev_model_adopt,
  totalRev_obs = slRev_obs_adopt + clRev_obs_adopt, totalRev_diff_obs = totalRev_post - totalRev_obs,
  totalRev_diff_model = totalRev_post - totalRev_model) %>% select(Year, totalRev_post, totalRev_model, 
                                                                   totalRev_obs, totalRev_diff_obs, totalRev_diff_model)



costs_cl <- costs_cl_9years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_9years)
costs_sl <- costs_sl_2years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_2years)

costs_hc_cl <- prices_predict_co4_merge %>% mutate(hc_9years = (hc_hat/100)) %>% select(Year, hc_9years)


costsSupply_sl_adopt <- demand_predict_co4_merge_adopt %>% mutate(costSupply_sl_obs = sl * costs_sl$cost_Lb_2years * adoption,
                                                                  costSupply_sl_model = sl_estY * costs_sl$cost_Lb_2years ) %>% select(
                                                                    Year, costSupply_sl_obs,  costSupply_sl_model)

costsSupply_cl_adopt <- demand_predict_co4_merge_adopt %>% mutate(costSupply_cl_obs = cl * costs_cl$cost_Lb_9years * adoption,
                                                                  costSupply_cl_model = cl_estY * costs_cl$cost_Lb_9years) %>% select(
                                                                    Year, costSupply_cl_obs, costSupply_cl_model)

costsSupply_t_adopt <- merge(costsSupply_sl_adopt, costsSupply_cl_adopt) %>% mutate(costSupply_t_obs = costSupply_sl_obs + costSupply_cl_obs,
                                                                                    costSupply_t_model = costSupply_sl_model + costSupply_cl_model)

costsRevenues_adopt <- merge(rev_cl_adopt, merge(rev_sl_adopt, merge(rev_total_adopt, costsSupply_t_adopt))) %>% select(
  Year, clRev_diff_obs, clRev_diff_model, costSupply_cl_obs, costSupply_cl_model, slRev_diff_obs, slRev_diff_model,costSupply_sl_obs, 
  costSupply_sl_model, totalRev_diff_obs, totalRev_diff_model, costSupply_t_obs, costSupply_t_model) %>% filter(Year>2009)

revDiff_costs_sl_adopt <- costsRevenues_adopt %>% select(Year, slRev_diff_obs, costSupply_sl_obs, slRev_diff_model, costSupply_sl_model)
revDiff_costs_cl_adopt <- costsRevenues_adopt %>% select(Year, clRev_diff_obs, costSupply_cl_obs, clRev_diff_model, costSupply_cl_model)
revDiff_costs_t_adopt <- costsRevenues_adopt %>% select(Year, totalRev_diff_obs, costSupply_t_obs, totalRev_diff_model, costSupply_t_model)

revDiff_costs_sl_pSurp_adopt <- revDiff_costs_sl_adopt %>% mutate(diffRevCost_sl_obs = slRev_diff_obs - costSupply_sl_obs, 
                                                                  diffRevCost_sl_model = slRev_diff_model - costSupply_sl_model) %>% select(
                                                                    Year, diffRevCost_sl_obs, diffRevCost_sl_model)
revDiff_costs_cl_pSurp_adopt <- revDiff_costs_cl_adopt %>% mutate(diffRevCost_cl_obs = clRev_diff_obs - costSupply_cl_obs,
                                                                  diffRevCost_cl_model = clRev_diff_model - costSupply_cl_model) %>% select(
                                                                    Year, diffRevCost_cl_obs, diffRevCost_cl_model)
revDiff_costs_t_pSurp_adopt <- revDiff_costs_t_adopt %>% mutate(diffRevCost_t_obs = round(totalRev_diff_obs - costSupply_t_obs,4),
                                                                diffRevCost_t_model = round(totalRev_diff_model - costSupply_t_model,4)) %>% select(
                                                                  Year, diffRevCost_t_obs, diffRevCost_t_model)




####################################################################################
############################    30% adoption   #####################################
####################################################################################


###### 20% Cost Share
pSurplus_30_20 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus30_20 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus30_20)

##### 30% Cost Share
pSurplus_30_30 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus30_30 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus30_30)

##### 50% Cost Share
pSurplus_30_50 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus30_50 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus30_50)

##### 70% Cost Share
pSurplus_30_70 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus30_70 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus30_70)

####################################################################################
############################    50% adoption   #####################################
####################################################################################


###### 20% Cost Share
pSurplus_50_20 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus50_20 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus50_20)

##### 30% Cost Share
pSurplus_50_30 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus50_30 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus50_30)

##### 50% Cost Share
pSurplus_50_50 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus50_50 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus50_50)

##### 70% Cost Share
pSurplus_50_70 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus50_70 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus50_70)


####################################################################################
############################    70% adoption   #####################################
####################################################################################


###### 20% Cost Share
pSurplus_70_20 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus70_20 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus70_20)

##### 30% Cost Share
pSurplus_70_30 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus70_30 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus70_30)

##### 50% Cost Share
pSurplus_70_50 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus70_50 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus70_50)

##### 70% Cost Share
pSurplus_70_70 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus70_70 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus70_70)


####################################################################################
############################    90% adoption   #####################################
####################################################################################

###### 20% Cost Share
pSurplus_90_20 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus90_20 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus90_20)

##### 30% Cost Share
pSurplus_90_30 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus90_30 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus90_30)

##### 50% Cost Share
pSurplus_90_50 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus90_50 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus90_50)

##### 70% Cost Share
pSurplus_90_70 <- revDiff_costs_t_pSurp_adopt %>% mutate(pSurplus90_70 = diffRevCost_t_model) %>% select(Year, 
                                                                                                         pSurplus90_70)






############# Here I compute the percent changes from no cost sharing to cost sharing for different adoption rates ########

##### Under 30% adoption rate
pSurp_30_CostShare<- round(merge(pSurp_30,merge(pSurplus_30_20,merge(pSurplus_30_30, merge(pSurplus_30_50,pSurplus_30_70)))),3)
# pSurp_30_CostShare_Loss <- pSurp_30_CostShare[,-1] * (-1)
# pSurp_30_CostShare_Loss <- pSurp_30_CostShare_Loss %>% mutate(Year = pSurp_30_CostShare$Year) %>% select(Year, everything())

pSurp_30_CostShare_Change <- pSurp_30_CostShare %>% mutate(change30_0To20 =  round(((pSurplus30_20 - surplus_30)/surplus_30)*100,3),
                                                                change30_0To30 =  round(((pSurplus30_30 - surplus_30)/surplus_30)*100,3),
                                                                change30_0To50 =  round(((pSurplus30_50 - surplus_30)/surplus_30)*100,3),
                                                                change30_0To70 =  round(((pSurplus30_70 - surplus_30)/surplus_30)*100,3)) %>% select(
                                                                  Year, change30_0To20, change30_0To30, change30_0To50, change30_0To70)

##### Under 50% adoption rate
pSurp_50_CostShare <- round(merge(pSurp_50,merge(pSurplus_50_20,merge(pSurplus_50_30, merge(pSurplus_50_50,pSurplus_50_70)))),3)
# pSurp_50_CostShare_Loss <- pSurp_50_CostShare[,-1] * (-1)
# pSurp_50_CostShare_Loss <- pSurp_50_CostShare_Loss %>% mutate(Year = pSurp_50_CostShare$Year) %>% select(Year, everything())

pSurp_50_CostShare_Change <- pSurp_50_CostShare %>% mutate(change50_0To20 =  round(((pSurplus50_20 - surplus_50)/surplus_50)*100,3),
                                                                change50_0To30 =  round(((pSurplus50_30 - surplus_50)/surplus_50)*100,3),
                                                                change50_0To50 =  round(((pSurplus50_50 - surplus_50)/surplus_50)*100,3),
                                                                change50_0To70 =  round(((pSurplus50_70 - surplus_50)/surplus_50)*100,3)) %>% select(
                                                                  Year, change50_0To20, change50_0To30, change50_0To50, change50_0To70)

##### Under 70% adoption rate
pSurp_70_CostShare <- round(merge(pSurp_70,merge(pSurplus_70_20,merge(pSurplus_70_30, merge(pSurplus_70_50,pSurplus_70_70)))),3)
# pSurp_70_CostShare_Loss <- pSurp_70_CostShare[,-1]
# pSurp_70_CostShare_Loss <- pSurp_70_CostShare_Loss %>% mutate(Year = pSurp_70_CostShare$Year) %>% select(Year, everything())

pSurp_70_CostShare_Change <- pSurp_70_CostShare %>% mutate(change70_0To20 =  round(((pSurplus70_20 - surplus_70)/surplus_70)*100,3),
                                                                change70_0To30 =  round(((pSurplus70_30 - surplus_70)/surplus_70)*100,3),
                                                                change70_0To50 =  round(((pSurplus70_50 - surplus_70)/surplus_70)*100,3),
                                                                change70_0To70 =  round(((pSurplus70_70 - surplus_70)/surplus_70)*100,3)) %>% select(
                                                                  Year, change70_0To20, change70_0To30, change70_0To50, change70_0To70)

##### Under 90% adoption rate
pSurp_90_CostShare <- round(merge(pSurp_90,merge(pSurplus_90_20,merge(pSurplus_90_30, merge(pSurplus_90_50,pSurplus_90_70)))),3)
# pSurp_90_CostShare_Loss <- pSurp_90_CostShare[,-1] * (-1)
# pSurp_90_CostShare_Loss <- pSurp_90_CostShare_Loss %>% mutate(Year = pSurp_90_CostShare$Year) %>% select(Year, everything())

pSurp_90_CostShare_Change <- pSurp_90_CostShare %>% mutate(change90_0To20 =  round(((pSurplus90_20 - surplus_90)/surplus_90)*100,3),
                                                                change90_0To30 =  round(((pSurplus90_30 - surplus_90)/surplus_90)*100,3),
                                                                change90_0To50 =  round(((pSurplus90_50 - surplus_90)/surplus_90)*100,3),
                                                                change90_0To70 =  round(((pSurplus90_70 - surplus_90)/surplus_90)*100,3)) %>% select(
                                                                  Year, change90_0To20, change90_0To30, change90_0To50, change90_0To70)






###########PLOTS


### 100% adoption different cost sharing schemes
pSurplus_adopt100_paper <- surplusLoss_costShare
names(pSurplus_adopt100_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt100_long_paper <- pivot_longer(pSurplus_adopt100_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

pSurplus_adopt100_2010_plot <- pSurplus_adopt100_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue4", width=0.3)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=1.5) + 
  theme_test()


##### 30% adoption
pSurplus_adopt30_paper <- pSurp_30_CostShare
names(pSurplus_adopt30_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt30_long_paper <- pivot_longer(pSurplus_adopt30_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

pSurplus_adopt30_2010_plot <- pSurplus_adopt30_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="turquoise4", width=0.3)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=1.5) + 
  theme_test()

##### 50% adoption
pSurplus_adopt50_paper <- pSurp_50_CostShare
names(pSurplus_adopt50_paper) <- c("Year", "0 \\%", "20 \\%", "30 \\%", "50 \\%", "70 \\%")

pSurplus_adopt50_long_paper <- pivot_longer(pSurplus_adopt50_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

pSurplus_adopt50_2010_plot <- pSurplus_adopt50_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="thistle4", width=0.3)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=1.5) + 
  theme_test()

##### 70% adoption
pSurplus_adopt70_paper <- pSurp_70_CostShare
names(pSurplus_adopt70_paper) <- c("Year", "0\\%", "20\\%", "30\\%", "50\\%", "70\\%")

pSurplus_adopt70_long_paper <- pivot_longer(pSurplus_adopt70_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

pSurplus_adopt70_2010_plot <- pSurplus_adopt70_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="wheat4", width=0.3)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=1.5) + 
  theme_test()

##### 90% adoption
pSurplus_adopt90_paper <- pSurp_90_CostShare
names(pSurplus_adopt90_paper) <- c("Year", "0\\%", "20\\%", "30\\%", "50\\%", "70\\%")

pSurplus_adopt90_long_paper <- pivot_longer(pSurplus_adopt90_paper, -c(Year), values_to = "Surplus", names_to = "CostShare") %>% as.data.frame()

pSurplus_adopt90_2010_plot <- pSurplus_adopt90_long_paper %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(CostShare, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="seashell4", width=0.3)+ 
  labs(x=  "Animal ID and Traceability Cost Share" , y= "Surplus (in Billion \\$)") +
  geom_text(aes(label=Surplus),vjust=1.5) + 
  theme_test()







