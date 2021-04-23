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


demand_predict_els <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_els <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_els <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))
adj_factor_els <- data.frame(Year = predict_df$Year+1, adj = numeric(nrow(predict_df)))
shares_slcl_els <- data.frame(Year = predict_df$Year+1, share_pre = numeric(nrow(predict_df)), 
                          share_post = numeric(nrow(predict_df)))


for(i in 1:(nrow(predict_df)-2)){
  
  # i <- 1
  K_t <- predict_df$K[i]
  k3_t2 <- predict_df$k3[i+2]
  
  ps_t <- predict_df$ps[i] + predict_df$ps[i]  * 0.01
  pc_t <- predict_df$pc[i] 
  hc_t <- predict_df$hc[i]
  
  dressed_t <- predict_df$dressedWeight[i]
  
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  adj_factor_els$adj[i] <- adj
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
  shares_slcl_els$share_pre[i] <- slShare_t
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t) 
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_els$mu_tilde[i] <- params_t1[1]
  parameters_els$s_tilde[i] <- params_t1[2]
  
  p <- c(ps_t, pc_t, hc_t)
  # p <- c(0.5,0.3,0.2)
  sl <- sl_t1_hat
  cl <- cl_t1_hat
  A <- demand_t1_hat
  mu_Tilde <- params_t1[1]
  s_Tilde <- params_t1[2]
  
  est_bb <- BBoptim(par=p, fn = sysEqs_solve)$par
  ps_hat_t1 <- est_bb[1] 
  pc_hat_t1 <- est_bb[2]
  hc_hat_t1 <- est_bb[3]
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  shares_slcl_els$share_post[i] <- slShare_t

  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
  demand_t1_hat <- demand_t1_hat
  
  prices_predict_els$ps_hat[i] <- ps_hat_t1
  prices_predict_els$pc_hat[i] <- pc_hat_t1
  prices_predict_els$hc_hat[i] <- hc_hat_t1
  demand_predict_els$demand_est[i] <- demand_t1_hat
  demand_predict_els$sl_est[i] <- sl_t1_hat
  demand_predict_els$cl_est[i] <- cl_t1_hat
}


demandMerge_new_els <- merge(demand_new, demand_predict_els) %>% select(Year, Demand, demand_est) %>% filter(demand_est>0)
demandMerge_new_els$Year <- as.numeric(demandMerge_new_els$Year)
demand_plot_new_els <- demandMerge_new_els %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated"))+geom_point(aes(y=demand_est,color="Estimated")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_new$Year[1], demandMerge_new$Year[nrow(demandMerge_new)])))+
  theme(legend.position = c(0.7, 0.2))


pricesMerge_new_els <- merge(prices_predict_els,prices_costs) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot_els <- pricesMerge_new_els   %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Observed"))+geom_point(aes(y=ps,color="Observed"))+geom_line(aes(y=ps_hat, color="Estimate"))+geom_point(aes(y=ps_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

cullPrices_plot_els <- pricesMerge_new_els %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Observed"))+geom_point(aes(y=pc,color="Observed")) + geom_line(aes(y=pc_hat, color="Estimate")) + geom_point(aes(y=pc_hat,color="Estimate")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7)) 

holdingCosts_plot_els <- pricesMerge_new_els   %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Observed"))+geom_point(aes(y=hc,color="Observed")) +geom_line(aes(y=hc_hat, color="Estimate")) + geom_point(aes(y=hc_hat,color="Estimate")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_new$Year[1], pricesMerge_new$Year[nrow(pricesMerge_new)])))+ 
  theme(legend.position = c(0.2, 0.7))

Master_sl_cl_els <- merge(demand_predict_els,merge(supp_sl_new,supp_cl_new)) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl_els) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl_els[,-1] <- round(Master_sl_cl_els[,-1],3) 
Master_sl_cl_els$Year <- as.numeric(Master_sl_cl_els$Year)

stock_slaughter_els <- Master_sl_cl_els   %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimate")) + geom_point(aes(y=sl_hat,color="Estimate")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.9, 0.9)) 

stock_cull_els <- Master_sl_cl_els %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimate")) + geom_point(aes(y=cl_hat,color="Estimate")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl$Year[1],Master_sl_cl$Year[nrow(Master_sl_cl)])))+ 
  theme(legend.position = c(0.8, 0.3))


Master_sl_cl_model <- Master_sl_cl %>% mutate(sl_model = sl_hat, cl_model = cl_hat) %>% select(Year, sl_model, cl_model)
pricesMerge_new_model <- pricesMerge_new %>% mutate(ps_model = ps_hat, pc_model = pc_hat) %>% select(Year, ps_model, pc_model)


changeFedQuant <- merge(Master_sl_cl_els,Master_sl_cl_model)  %>% 
  mutate(sl_percentChange = abs(((sl_hat - sl_model)/sl_model)*100)) %>% select(Year, sl_percentChange)

changeFedPrice <- merge(pricesMerge_new_els,pricesMerge_new_model) %>% 
  mutate(ps_percentChange = abs( ((ps_hat-ps_model)/ps_model)*100)) %>% select(Year, ps_percentChange)

mean(changeFedQuant$sl_percentChange/changeFedPrice$ps_percentChange)


# 0.05321271

# 2.908246

# https://core.ac.uk/download/pdf/7043787.pdf 2.63





