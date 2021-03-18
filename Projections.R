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

demand_project <- data.frame(Year = years_project, demand_est = numeric(length(years_project)), 
                             sl_est = numeric(length(years_project)), cl_est = numeric(length(years_project)))

prices_project <- data.frame(Year = years_project, ps_est = numeric(length(years_project)), 
                             pc_est = numeric(length(years_project)), hc_est = numeric(length(years_project)))

parameters_project <- data.frame(Year = years_project, mu_tilde = numeric(length(years_project)), 
                                 s_tilde = numeric(length(years_project)))

adj_project <- data.frame(Year = years_project, adj_fac = numeric(length(years_project)))

for(i in 1:nrow(prices_project)){
  
  
  i <- 2
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
    slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
  }
  year_t <- predict_df_tail$Year[i]
  
  adj <- demand/(sl+cl)
  adj_project$adj_fac[i] <- adj
  
  if(adj>1){
    adj <- 1/adj
  }
  
  
  
  # slShare_t <- (exp((muTilde - ((ps_t - pc_t)/phi))/sTilde))
  
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
  
  slShare_t <- (exp((params_t1[1] - ((ps_hat_t1 - pc_hat_t1))/phi)/params_t1[2]))
  
  if(year_t<=2016){
    sl_t1_hat <- demand_t1_hat * ((slShare_t)/(1 + slShare_t)) * adj
    cl_t1_hat <- demand_t1_hat * (1/(1+slShare_t)) * adj
    # demand_t1_hat <- demand_t1_hat
  }else{
    # if(adj > 1){
    #   adj1 <- 1/adj
    #   sl_t1_hat <- demand_t1_hat * ((slShare_t)/(1 + slShare_t)) * adj1
    #   cl_t1_hat <- demand_t1_hat * (1/(1+slShare_t)) * adj1
    # }else{
      sl_t1_hat <- demand_t1_hat * ((slShare_t)/(1 + slShare_t)) * adj
      cl_t1_hat <- demand_t1_hat * (1/(1+slShare_t)) * adj
      # demand_t1_hat <- demand_t1_hat 
    # }
    # demand_t1_hat <- (sl_t1_hat + cl_t1_hat)
  }
  
    prices_project$ps_est[i] <- ps_hat_t1
    prices_project$pc_est[i] <- pc_hat_t1
    prices_project$hc_est[i] <- hc_hat_t1
    demand_project$demand_est[i] <- demand_t1_hat
    demand_project$sl_est[i] <- sl_t1_hat
    demand_project$cl_est[i] <- cl_t1_hat
  
    sl <- sl_t1_hat
    cl <- cl_t1_hat
    demand <- demand_t1_hat
  
    ps_t <- ps_hat_t1
    pc_t <- pc_hat_t1
    hc_t <- hc_hat_t1
  
  
}
