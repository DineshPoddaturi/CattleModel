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

predict_df_tail <- tail(predict_df, 3)

# demand_predict_project <- demand_predict %>% filter(demand_est>0)
# prices_predict_project <- prices_predict %>% filter(ps_hat>0)
# 
# demand_prices_project <- merge(demand_predict_project, prices_predict_project)
# 
# demand_prices_project_last <- last(demand_prices_project)

years_project <- c(seq(predict_df_tail$Year[nrow(predict_df_tail)]-1,
                       predict_df_tail$Year[nrow(predict_df_tail)]+8))

demand_project <- data.frame(Year = years_project, Demand_est = numeric(length(years_project)), 
                             sl_est = numeric(length(years_project)), cl_est = numeric(length(years_project)))

prices_project <- data.frame(Year = years_project, ps_est = numeric(length(years_project)), 
                             pc_est = numeric(length(years_project)), hc_est = numeric(length(years_project)))

parameters_project <- data.frame(Year = years_project, mu_tilde = numeric(length(years_project)), 
                                 s_tilde = numeric(length(years_project)))

adj_project <- data.frame(Year = years_project, adj_fac = numeric(length(years_project)))
share_project <- data.frame(Year = years_project, shares_project = numeric(length(years_project)))

for(i in 1:nrow(prices_project)){
  
    # i <- 3
    if(i<=1){
      
      K_t <- predict_df_tail$K[i]
      k3_t2 <- predict_df_tail$k3[i+2]
      
      ps_t <- predict_df_tail$ps[i]
      pc_t <- predict_df_tail$pc[i]
      hc_t <- predict_df_tail$hc[i]
      sl <- predict_df_tail$sl[i]
      cl <- predict_df_tail$cl[i]
      demand <- predict_df_tail$Dissappear[i]
      imports_t <- predict_df_tail$imports[i]
      exports_t <- predict_df_tail$exports[i]
      # slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
      adj <- demand/(sl+cl)
    }
    year_t <- demand_project$Year[i]
    
    adj <- demand/(sl+cl)

    # adj1 <- adj

    if(adj>1){
      adj <- 1/adj
    }
    
    adj_project$adj_fac[i] <- adj
    
    slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
    
    if(year_t<=2016){
      demand_t1_hat <- (g * K_t - k3_t2 + imports_t - exports_t) * (dressed_t/1000000000) * ((1+slShare_t)/slShare_t)
      sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
      cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
    }else{
      demand_t1_hat <- demand
      sl_t1_hat <- sl * adj
      cl_t1_hat <- cl * adj
    }      
  
    params_t1 <- mu_s_tildes(sl=sl_t1_hat, cl=cl_t1_hat, ps = ps_t, pc = pc_t, thetas = c(1,1))
    parameters_project$mu_tilde[i] <- params_t1[1]
    parameters_project$s_tilde[i] <- params_t1[2]
    
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
    
    slShare_t <- (exp((muTilde - ((ps_hat_t1 - pc_hat_t1))/phi)/sTilde))
    share_project$shares_project[i] <- slShare_t
    
    if(year_t>2016){
      sl_t1_hat <- (demand_t1_hat * ((slShare_t)/(1 + slShare_t))) * adj
      cl_t1_hat <- (demand_t1_hat * (1/(1+slShare_t))) * adj
      demand_t1_hat <- demand_t1_hat
    }
  
    prices_project$ps_est[i] <- ps_hat_t1
    prices_project$pc_est[i] <- pc_hat_t1
    prices_project$hc_est[i] <- hc_hat_t1
    demand_project$Demand_est[i] <- demand_t1_hat
    demand_project$sl_est[i] <- sl_t1_hat
    demand_project$cl_est[i] <- cl_t1_hat
  
    sl <- sl_t1_hat
    cl <- cl_t1_hat
    demand <- demand_t1_hat
  
    # ps_t <- ps_hat_t1
    # pc_t <- pc_hat_t1
    # hc_t <- hc_hat_t1
    
}

demand_proj <- left_join(rbind(demand_predict_est,demand_project%>%filter(Year>2016)), demand_obs%>%filter(Year<=2016))

prices_proj <- left_join(rbind(prices_predict_est, prices_project%>% filter(Year>2016)),prices_costs_obs%>%filter(Year<=2016)) %>%
  mutate(ps = ps* 100, pc = pc*100, hc=hc*100, ps_est = ps_est*100, pc_est = pc_est*100, hc_est = hc_est*100)




prices_proj %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est,color="Model Estimate"))+
  geom_point(aes(y=ps_est,color="Model Estimate"))+geom_line(aes(y=ps, color="Observed"))+
  geom_point(aes(y=ps,color="Observed")) + labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_proj$Year[1], prices_proj$Year[nrow(prices_proj)])))

prices_proj %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est,color="Model Estimate"))+
  geom_point(aes(y=pc_est,color="Model Estimate"))+geom_line(aes(y=pc, color="Observed"))+
  geom_point(aes(y=pc,color="Observed")) + labs(x="Year", y="Culled Prices (\\$/cwt)", colour = "") + theme_classic() + 
  scale_x_continuous(name="Year", breaks=c(seq(prices_proj$Year[1], prices_proj$Year[nrow(prices_proj)]))) 


demand_proj %>% ggplot(aes(x=Year))+geom_line(aes(y=sl_est,color="Model estimate"))+
  geom_point(aes(y=sl_est,color="Model estimate")) + geom_line(aes(y=sl, color="Observed")) + 
  geom_point(aes(y=sl,color="Observed")) + labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_proj$Year[1],demand_proj$Year[nrow(demand_proj)])))

demand_proj %>% ggplot(aes(x=Year))+geom_line(aes(y=cl_est,color="Model estimate"))+
  geom_point(aes(y=cl_est,color="Model estimate")) + geom_line(aes(y=cl, color="Observed")) + 
  geom_point(aes(y=cl,color="Observed")) + labs(x="Year", y="Culled meat (in Billion pounds)", colour="") + theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_proj$Year[1],demand_proj$Year[nrow(demand_proj)])))


round(prices_project,3) %>% ggplot(aes(x=Year))+geom_line(aes(y=ps_est*100,color="Model Projection"))+
  geom_point(aes(y=ps_est*100,color="Model Projection"))+ labs(x="Year", y="Slaughter Prices (\\$/cwt)", colour = "") + 
  theme_classic() + 
  scale_x_continuous(breaks=c(seq(prices_project$Year[1], prices_project$Year[nrow(prices_project)])))

round(prices_project,3) %>% ggplot(aes(x=Year))+geom_line(aes(y=pc_est*100,color="Model Projection"))+
  geom_point(aes(y=pc_est*100,color="Model Projection"))+ labs(x="Year", y="Cull Cow Prices (\\$/cwt)", colour = "") + 
  theme_classic() + 
  scale_x_continuous(breaks=c(seq(prices_project$Year[1], prices_project$Year[nrow(prices_project)])))

round(prices_project,3) %>% ggplot(aes(x=Year))+geom_line(aes(y=hc_est*100,color="Model Projection"))+
  geom_point(aes(y=hc_est*100,color="Model Projection"))+ labs(x="Year", y="Holding Costs(\\$/cwt)", colour = "") + 
  theme_classic() + 
  scale_x_continuous(breaks=c(seq(prices_project$Year[1], prices_project$Year[nrow(prices_project)])))


round(demand_project,4) %>% ggplot(aes(x=Year))+geom_line(aes(y=sl_est,color="Model Projection"))+
  geom_point(aes(y=sl_est,color="Model Projection")) + labs(x="Year", y="Slaughter meat (in Billion pounds)", colour="") + 
  theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_project$Year[1],demand_project$Year[nrow(demand_project)])))

round(demand_project,4) %>% ggplot(aes(x=Year))+geom_line(aes(y=cl_est,color="Model Projection"))+
  geom_point(aes(y=cl_est,color="Model Projection")) + labs(x="Year", y="Cull cow meat (in Billion pounds)", colour="") + 
  theme_classic()+ 
  scale_x_continuous(name="Year", breaks=c(seq(demand_project$Year[1],demand_project$Year[nrow(demand_project)])))



# prices_project
# Year   ps_est    pc_est    hc_est
# 1  2016 1.527698 1.0122139 0.4730073
# 2  2017 1.554179 0.9837815 0.4789974
# 3  2018 1.545118 0.9935105 0.4769477
# 4  2019 1.545866 0.9927073 0.4771170
# 5  2020 1.545799 0.9927787 0.4771019
# 6  2021 1.545805 0.9927724 0.4771033
# 7  2022 1.545805 0.9927729 0.4771031
# 8  2023 1.545805 0.9927729 0.4771031
# 9  2024 1.545805 0.9927729 0.4771031
# 10 2025 1.545805 0.9927729 0.4771031

# demand_project
# Year demand_est   sl_est   cl_est
# 1  2016   24.50651 20.48467 2.937606
# 2  2017   24.50651 19.67647 3.745800
# 3  2018   24.50651 19.81608 3.606187
# 4  2019   24.50651 19.80472 3.617550
# 5  2020   24.50651 19.80573 3.616539
# 6  2021   24.50651 19.80564 3.616628
# 7  2022   24.50651 19.80565 3.616620
# 8  2023   24.50651 19.80565 3.616621
# 9  2024   24.50651 19.80565 3.616621
# 10 2025   24.50651 19.80565 3.616621


