## Here we use the slaughter animals numbers
Stock_temp <- Stock%>% filter(Year>=1994 & Year<=2017)
imports_temp <- imports %>% filter(Year>=1994 & Year<=2017)
exports_temp <- exports %>% filter(Year>=1994 & Year<=2017)

predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>=1994)%>% select(Slaughter_avg),
                    cost_price_addedCosts_obs%>% select(ps), cost_price_addedCosts_obs%>% select(pc),
                    cost_price_addedCosts_obs%>% select(hc), supp_sl %>% filter(Year>=1994) %>% select(Bill_meatLb_sl), 
                    supp_cl %>% filter(Year>=1994) %>% select(Bill_meatLb_cl),
                    totalDisappearedNew %>% filter(Year>=1994) %>% select(total_meat_bill)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")


demand_predict_co <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

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
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters_co$mu_tilde[i] <- params_t1[1]
  parameters_co$s_tilde[i] <- params_t1[2]
  
  
  # slShare_t <- (exp((params_t1[1] - ((ps_t - pc_t))/phi)/params_t1[2]))
  # 
  # demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  # 
  # sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  # cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  
  
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
  
  prices_predict_co$ps_hat[i] <- ps_hat_t1
  prices_predict_co$pc_hat[i] <- pc_hat_t1
  prices_predict_co$hc_hat[i] <- hc_hat_t1
  demand_predict_co$demand_est[i] <- demand_t1_hat
  demand_predict_co$sl_est[i] <- sl_t1_hat
  demand_predict_co$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
}


### Look at these numbers again. These look promising
(prices_predict_co %>% filter(ps_hat>0) * 100 - cost_price_addedCosts %>% select(Year, ps_hat, pc_hat, hc_hat) * 100) %>% mutate(net = (pc_hat + pc_hat + hc_hat))



prices_predict_old <- prices_predict %>% mutate(ps = ps_hat, pc = pc_hat, hc = hc_hat) %>% select(Year, ps, pc, hc) %>% filter(ps>0)


### Here I am plotting these with the estimated ones

pricesMerge_co <- merge(prices_predict_co,prices_predict_old) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot_co <- pricesMerge_co %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Estimate without added costs"))+geom_point(aes(y=ps,color="Estimate without added costs"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)]))) 

cullPrices_plot_co <- pricesMerge_co %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Estimate without added costs"))+geom_point(aes(y=pc,color="Estimate without added costs")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)]))) 

holdingCosts_plot_co <- pricesMerge_co %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Estimate without added costs"))+geom_point(aes(y=hc,color="Estimate without added costs")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)])))


### Repeat this for the estimated ones as well. You could see some change
demandMerge_co <- merge(demand_new, demand_predict_co) %>% select(Year, Demand, demand_est) %>% filter(demand_est>0)
demandMerge_co$Year <- as.numeric(demandMerge_co$Year)
demand_plot_co <- demandMerge_co %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated with added costs"))+geom_point(aes(y=demand_est,color="Estimated with added costs")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_co$Year[1], demandMerge_co$Year[nrow(demandMerge_co)])))

Master_sl_cl_co <- merge(demand_predict_co,merge(supp_sl_new,supp_cl_new)) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl_co) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl_co[,-1] <- round(Master_sl_cl_co[,-1],2) 
Master_sl_cl_co$Year <- as.numeric(Master_sl_cl_co$Year)



stock_slaughter_co <- Master_sl_cl_co %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimated with added costs")) + geom_point(aes(y=sl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co)])))

stock_cull_co <- Master_sl_cl_co %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimated with added costs")) + geom_point(aes(y=cl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co)])))

parameters_new_co <- parameters_co %>% filter(mu_tilde>0)
parameter_mu_co <- parameters_new_co %>% ggplot(aes(x=Year)) + geom_line(aes(y=mu_tilde, color="mu_tilde")) + geom_point(aes(y=mu_tilde, color="mu_tilde"))+
  labs(x="Year", y="mu_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new_co$Year[1],parameters_new_co$Year[nrow(parameters_new_co)])))

parameter_s_co <- parameters_new_co %>% ggplot(aes(x=Year)) + geom_line(aes(y=s_tilde)) + geom_point(aes(y=s_tilde))+
  labs(x="Year", y="s_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new_co$Year[1],parameters_new_co$Year[nrow(parameters_new_co)])))

#### Here we are comparing the above with the original estimated numbers (without added costs)
demand_predict_old <- demand_predict %>% mutate(Demand = demand_est) %>% select(Year, Demand) %>% filter(Demand>0)
demandMerge_co <- merge(demand_predict_old, demand_predict_co) %>% select(Year, Demand, demand_est) %>% filter(demand_est>0)
demandMerge_co$Year <- as.numeric(demandMerge_co$Year)
demand_plot_co <- demandMerge_co %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Observed"))+geom_point(aes(y=Demand,color="Observed"))+geom_line(aes(y=demand_est, color="Estimated with added costs"))+geom_point(aes(y=demand_est,color="Estimated with added costs")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_co$Year[1], demandMerge_co$Year[nrow(demandMerge_co)])))

sl_cl_old <- demand_predict %>% mutate(Bill_meatLb_sl = sl_est, Bill_meatLb_cl = cl_est) %>% select(Year, Bill_meatLb_sl, Bill_meatLb_cl) %>% filter(Bill_meatLb_cl>0)
Master_sl_cl_co <- merge(demand_predict_co,sl_cl_old) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl_co) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl_co[,-1] <- round(Master_sl_cl_co[,-1],2) 
Master_sl_cl_co$Year <- as.numeric(Master_sl_cl_co$Year)



stock_slaughter_co <- Master_sl_cl_co %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Observed"))+geom_point(aes(y=sl,color="Observed")) +geom_line(aes(y=sl_hat, color="Estimated with added costs")) + geom_point(aes(y=sl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co)])))

stock_cull_co <- Master_sl_cl_co %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Observed"))+geom_point(aes(y=cl,color="Observed")) +geom_line(aes(y=cl_hat, color="Estimated with added costs")) + geom_point(aes(y=cl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co)])))

parameters_new_co <- parameters_co %>% filter(mu_tilde>0)
parameter_mu_co <- parameters_new_co %>% ggplot(aes(x=Year)) + geom_line(aes(y=mu_tilde, color="mu_tilde")) + geom_point(aes(y=mu_tilde, color="mu_tilde"))+
  labs(x="Year", y="mu_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new_co$Year[1],parameters_new_co$Year[nrow(parameters_new_co)])))

parameter_s_co <- parameters_new_co %>% ggplot(aes(x=Year)) + geom_line(aes(y=s_tilde)) + geom_point(aes(y=s_tilde))+
  labs(x="Year", y="s_tilde")+theme_classic()+
  scale_x_continuous(name="Year", breaks=c(seq(parameters_new_co$Year[1],parameters_new_co$Year[nrow(parameters_new_co)])))













##### Here I am using the estimated prices to predict.
cost_price_addedCosts <- cost_price_addedCosts %>% mutate(ps = ps_hat, pc = pc_hat, hc = hc_hat)

Stock_temp <- Stock%>% filter(Year>1994 & Year<2017)
imports_temp <- imports %>% filter(Year>1994 & Year<2017)
exports_temp <- exports %>% filter(Year>1994 & Year<2017)

demand_predict <- demand_predict %>% filter(demand_est>0)
predict_df <- cbind(Stock_temp$Year, Stock_temp$K, Stock_temp$k3 , imports_temp$Imports, exports_temp$Exports,
                    dressedWeights_sl_cl %>% filter(Year>1994  & Year<2017)%>% select(Slaughter_avg),
                    cost_price_addedCosts %>% select(ps), cost_price_addedCosts%>% select(pc),
                    cost_price_addedCosts%>% select(hc), demand_predict %>% filter(Year>1994  & Year<2017) %>% select(sl_est), 
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(cl_est),
                    demand_predict %>% filter(Year>1994  & Year<2017) %>% select(demand_est)) %>% as.data.frame()
names(predict_df) <- c("Year", "K", "k3", "imports", "exports", "dressedWeight", "ps", "pc", "hc", "sl", "cl", "Dissappear")


demand_predict_co1<- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)), sl_est = numeric(nrow(predict_df)), cl_est = numeric(nrow(predict_df)))

# demand_predict <- data.frame(Year = predict_df$Year+1, demand_est = numeric(nrow(predict_df)))

prices_predict_co1 <- data.frame(Year = predict_df$Year+1, ps_hat = numeric(nrow(predict_df)), pc_hat = numeric(nrow(predict_df)), hc_hat = numeric(nrow(predict_df)))

parameters_co1 <- data.frame(Year = predict_df$Year+1, mu_tilde = numeric(nrow(predict_df)), s_tilde = numeric(nrow(predict_df)))

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
  }
  
  ps_t <- predict_df$ps[i]
  pc_t <- predict_df$pc[i]
  hc_t <- predict_df$hc[i]
  dressed_t <- predict_df$dressedWeight[i]
  sl <- predict_df$sl[i]
  cl <- predict_df$cl[i]
  demand <- predict_df$Dissappear[i]
  adj <- demand/(sl+cl)
  
  imports_t <- predict_df$imports[i]
  exports_t <- predict_df$exports[i]
  
  
  slShare_t <- (exp((muTilde - ((ps_t - pc_t))/phi)/sTilde))
  
  demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  
  sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
  parameters$mu_tilde[i] <- params_t1[1]
  parameters$s_tilde[i] <- params_t1[2]
  
  
  # slShare_t <- (exp((params_t1[1] - ((ps_t - pc_t))/phi)/params_t1[2]))
  # 
  # demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
  # 
  # sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
  # cl_t1_hat <- (demand_t1_hat * 1/(1+slShare_t)) * adj
  
  
  
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
  
  prices_predict_co1$ps_hat[i] <- ps_hat_t1
  prices_predict_co1$pc_hat[i] <- pc_hat_t1
  prices_predict_co1$hc_hat[i] <- hc_hat_t1
  demand_predict_co1$demand_est[i] <- demand_t1_hat
  demand_predict_co1$sl_est[i] <- sl_t1_hat
  demand_predict_co1$cl_est[i] <- cl_t1_hat
  
  # ps_t <- ps_hat_t1
  # pc_t <- pc_hat_t1
  # hc_t <- hc_hat_t1
  # demand <- A
}

pricesMerge_co1 <- merge(prices_predict_co1,prices_predict_old) %>% filter(ps_hat>0) %>% select(Year, ps, ps_hat, pc, pc_hat, hc, hc_hat) %>% 
  mutate(ps = ps*100, ps_hat = ps_hat*100, pc = pc*100, pc_hat = pc_hat*100, hc = hc*100, hc_hat = hc_hat*100)

slaughterPrices_plot_co1 <- pricesMerge_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=ps,color="Estimate without added costs"))+geom_point(aes(y=ps,color="Estimate without added costs"))+geom_line(aes(y=ps_hat, color="Estimate with added costs"))+geom_point(aes(y=ps_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)]))) 

cullPrices_plot_co1 <- pricesMerge_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=pc,color="Estimate without added costs"))+geom_point(aes(y=pc,color="Estimate without added costs")) + geom_line(aes(y=pc_hat, color="Estimate with added costs")) + geom_point(aes(y=pc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Culled Prices (\\$/cwt)", colour="") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)]))) 

holdingCosts_plot_co1 <- pricesMerge_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=hc,color="Estimate without added costs"))+geom_point(aes(y=hc,color="Estimate without added costs")) +geom_line(aes(y=hc_hat, color="Estimate with added costs")) + geom_point(aes(y=hc_hat,color="Estimate with added costs")) + 
  labs(x="Year", y="Holding Costs (\\$/cwt)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(pricesMerge_co$Year[1], pricesMerge_co$Year[nrow(pricesMerge_co)])))



demandMerge_co1 <- merge(demand_predict_old, demand_predict_co1) %>% select(Year, Demand, demand_est) %>% filter(demand_est>0)
demandMerge_co1$Year <- as.numeric(demandMerge_co1$Year)
demand_plot_co1 <- demandMerge_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=Demand,color="Estimated w/o added costs"))+geom_point(aes(y=Demand,color="Estimated w/o added costs"))+geom_line(aes(y=demand_est, color="Estimated with added costs"))+geom_point(aes(y=demand_est,color="Estimated with added costs")) + 
  labs(x="Year", y="Demand (in bill pounds)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(demandMerge_co1$Year[1], demandMerge_co$Year[nrow(demandMerge_co1)])))

sl_cl_old <- demand_predict %>% mutate(Bill_meatLb_sl = sl_est, Bill_meatLb_cl = cl_est) %>% select(Year, Bill_meatLb_sl, Bill_meatLb_cl) %>% filter(Bill_meatLb_cl>0)
Master_sl_cl_co1 <- merge(demand_predict_co1,sl_cl_old) %>% filter(sl_est>0)  %>% select(Year,Bill_meatLb_sl, sl_est, Bill_meatLb_cl, cl_est)
names(Master_sl_cl_co1) <- c("Year", "sl", "sl_hat", "cl", "cl_hat")
Master_sl_cl_co1[,-1] <- round(Master_sl_cl_co1[,-1],2) 
Master_sl_cl_co1$Year <- as.numeric(Master_sl_cl_co1$Year)



stock_slaughter_co1 <- Master_sl_cl_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=sl,color="Estimate without added costs"))+geom_point(aes(y=sl,color="Estimate without added costs")) +geom_line(aes(y=sl_hat, color="Estimated with added costs")) + geom_point(aes(y=sl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co1$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co1)])))

stock_cull_co1 <- Master_sl_cl_co1 %>% ggplot(aes(x=Year))+geom_line(aes(y=cl,color="Estimate without added costs"))+geom_point(aes(y=cl,color="Estimate without added costs")) +geom_line(aes(y=cl_hat, color="Estimated with added costs")) + geom_point(aes(y=cl_hat,color="Estimated with added costs")) + 
  labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(Master_sl_cl_co1$Year[1],Master_sl_cl_co$Year[nrow(Master_sl_cl_co1)])))