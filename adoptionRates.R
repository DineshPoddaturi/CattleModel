
####### In this case we know how the prices and quantities change once adoption. ############ 
####### So use them for whomever adopts and for rest old price and quantities. ##############

adoption_20 <- 30
adoption_50 <- 50
adoption_70 <- 70






################ different adoption rate ##########
adoption <- 0.5

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
  # shares_slcl$share_post[i] <- slShare_t
  
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


demand_predict_co4_adopt<- data.frame(Year = predict_df_adopt$Year+1, 
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



# revDiff_costs_t_pSurp_adopt_90 <- revDiff_costs_t_pSurp_adopt

# revDiff_costs_t_pSurp_adopt_70 <- revDiff_costs_t_pSurp_adopt

# revDiff_costs_t_pSurp_adopt_50 <- revDiff_costs_t_pSurp_adopt

# revDiff_costs_t_pSurp_adopt_20 <- revDiff_costs_t_pSurp_adopt

pSurp_100 <- revDiff_costs_t_pSurp %>% mutate(surplus_100 = diffRevCost_t_model) %>% select(Year, surplus_100)
pSurp_90 <- revDiff_costs_t_pSurp_adopt_90 %>% mutate(surplus_90 = diffRevCost_t_model) %>% select(Year, surplus_90)
pSurp_70 <- revDiff_costs_t_pSurp_adopt_70 %>% mutate(surplus_70 = diffRevCost_t_model) %>% select(Year, surplus_70)
pSurp_50 <- revDiff_costs_t_pSurp_adopt_50 %>% mutate(surplus_50 = diffRevCost_t_model) %>% select(Year, surplus_50)
pSurp_20 <- revDiff_costs_t_pSurp_adopt_20 %>% mutate(surplus_20 = diffRevCost_t_model) %>% select(Year, surplus_20)


pSurplus <- merge(pSurp_20, merge(pSurp_50, merge(pSurp_70, merge(pSurp_90, pSurp_100))))
names(pSurplus) <- c("Year", "20%", "50%", "70%", "90%", "100%")

pSurplus_merge <- pSurplus

pSurplus_long <- pivot_longer(pSurplus, -c(Year), values_to = "Surplus", names_to = "Adoption") %>% as.data.frame()

pSurplus_2010 <- pSurplus_long %>% filter(Year <= 2010)

pSurplus_2010 <- pSurplus_long %>% filter(Year == 2010) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue4", width=0.3)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+
  theme_test()

pSurplus_2011 <- pSurplus_long %>% filter(Year == 2011) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()

pSurplus_2012 <- pSurplus_long %>% filter(Year == 2012) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()

pSurplus_2013 <- pSurplus_long %>% filter(Year == 2013) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()

pSurplus_2014 <- pSurplus_long %>% filter(Year == 2014) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()

pSurplus_2015 <- pSurplus_long %>% filter(Year == 2015) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()

pSurplus_2016 <- pSurplus_long %>% filter(Year == 2016) %>% ggplot(aes(fct_rev(fct_reorder(Adoption, Surplus)),Surplus))+
  geom_bar(stat="identity", fill="steelblue", width=0.4)+ labs(x="Adoption Rate", y=" Surplus (in billion $)")+ 
  scale_x_discrete(position = "top") + theme_linedraw()




# 100% adoption
# > revDiff_costs_t_pSurp_adopt
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.4758             -0.4655
# 2 2011           -4.6500             -0.3373
# 3 2012           -2.2253             -0.2975
# 4 2013           -0.6869             -0.3515
# 5 2014           -7.0593             -0.3099
# 6 2015            1.5133             -0.4581
# 7 2016            7.2943             -0.4254

# 90% adoption
# > revDiff_costs_t_pSurp_adopt
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -2.0784             -0.4223
# 2 2011           -3.9908             -0.3063
# 3 2012           -1.7948             -0.2699
# 4 2013           -0.3787             -0.3191
# 5 2014           -6.1294             -0.2812
# 6 2015            1.6868             -0.4166
# 7 2016            6.8396             -0.3863


# 70% adoption
# > revDiff_costs_t_pSurp_adopt
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -1.2835             -0.3358
# 2 2011           -2.6724             -0.2442
# 3 2012           -0.9338             -0.2147
# 4 2013            0.2377             -0.2545
# 5 2014           -4.2697             -0.2238
# 6 2015            2.0338             -0.3335
# 7 2016            5.9300             -0.3079

#50% adoption
# > revDiff_costs_t_pSurp_adopt
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010           -0.4887             -0.2493
# 2 2011           -1.3541             -0.1821
# 3 2012           -0.0728             -0.1594
# 4 2013            0.8542             -0.1899
# 5 2014           -2.4099             -0.1665
# 6 2015            2.3809             -0.2503
# 7 2016            5.0205             -0.2296

#30% adoption
# > revDiff_costs_t_pSurp_adopt
# Year diffRevCost_t_obs diffRevCost_t_model
# 1 2010            0.3062             -0.1628
# 2 2011           -0.0357             -0.1200
# 3 2012            0.7882             -0.1042
# 4 2013            1.4706             -0.1252
# 5 2014           -0.5502             -0.1092
# 6 2015            2.7280             -0.1672
# 7 2016            4.1109             -0.1513



