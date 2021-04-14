##### In this simulation I use the change in the prices and quantities immediately after implementing 
##### the traceability systems. This percentage change is used to construct the new prices and quantities
##### just by adding the percentage change to the observed data. Now with this new counterfactual, we
##### compute the long-run impacts and how they vary.





ps_change <- percentChange_price_ps %>% filter(Year==2010) %>% select(percentChange_ps_model)

pc_change <- percentChange_price_pc %>% filter(Year==2010) %>% select(percentChange_pc_model) 

sl_change <- percentChange_sl %>% filter(Year==2010) %>% select(percentChange_sl_model) 

cl_change <- percentChange_cl %>% filter(Year==2010) %>% select(percentChange_cl_model)

psTBA <- prices_predict_co4_merge %>% filter(Year>2009) %>% select(ps_est)
pcTBA <- prices_predict_co4_merge %>% filter(Year>2009) %>% select(pc_est)
slTBA <- demand_predict_co4_merge %>% filter(Year>2009) %>% select(sl_est)
clTBA <- demand_predict_co4_merge %>% filter(Year>2009) %>% select(cl_est)

temp_ps <- psTBA + psTBA*(ps_change$percentChange_ps_model/100)
temp_pc <- pcTBA + pcTBA*(pc_change$percentChange_pc_model/100)

temp_sl <- slTBA + slTBA * (sl_change$percentChange_sl_model/100)
temp_cl <- clTBA + clTBA * (cl_change$percentChange_cl_model/100)

temp_p <- cbind(temp_ps, temp_pc) %>% mutate(Year = c(seq(2010,2016)), ps_hat = ps_est, pc_hat = pc_est) %>% select(Year, ps_hat, pc_hat)
temp_q <-  cbind(temp_sl, temp_cl) %>% mutate(Year = c(seq(2010,2016)), sl_hat = sl_est, cl_hat = cl_est) %>% select(Year, sl_hat, cl_hat)

prices_predict_co4_merge_LR <- prices_predict_co4_merge %>% select(-ps_hat, -pc_hat)
demand_predict_co4_merge_LR <- demand_predict_co4_merge %>% select(-sl_hat, -cl_hat)

prices_predict_co4_merge_LR <- left_join(prices_predict_co4_merge_LR, temp_p, by = "Year") %>% select(Year, ps, ps_est, ps_hat,
                                                                                                      pc, pc_est, pc_hat)

demand_predict_co4_merge_LR <- left_join(demand_predict_co4_merge_LR, temp_q, by = "Year") %>% select(Year, sl, sl_est, sl_hat,
                                                                                                      cl, cl_est, cl_hat)


rev_sl_LR <-  prices_predict_co4_merge_LR %>% mutate(slRev_post = (ps_hat/100) * demand_predict_co4_merge_LR$sl_hat,
                                               slRev_model = (ps_est/100) * demand_predict_co4_merge_LR$sl_est,
                                               slRev_obs = (ps/100) * demand_predict_co4_merge_LR$sl,
                                               slRev_diff_obs = slRev_post - slRev_obs,
                                               slRev_diff_model = slRev_post - slRev_model) %>% select(Year, slRev_post, slRev_model, 
                                                                                                       slRev_obs, slRev_diff_obs, slRev_diff_model)

rev_cl_LR <- prices_predict_co4_merge_LR %>% mutate(clRev_post = (pc_hat/100) * demand_predict_co4_merge_LR$cl_hat,
                                              clRev_model = (pc_est/100) * demand_predict_co4_merge_LR$cl_est,
                                              clRev_obs = (pc/100) * demand_predict_co4_merge_LR$cl,
                                              clRev_diff_obs = clRev_post - clRev_obs,
                                              clRev_diff_model = clRev_post - clRev_model) %>% select(Year, clRev_post, clRev_model, 
                                                                                                      clRev_obs, clRev_diff_obs, clRev_diff_model)

rev_total_LR <- merge(rev_sl_LR, rev_cl_LR) %>% mutate(totalRev_post = slRev_post + clRev_post, 
                                              totalRev_model = slRev_model + clRev_model,
                                              totalRev_obs = slRev_obs + clRev_obs,
                                              totalRev_diff_obs = totalRev_post - totalRev_obs,
                                              totalRev_diff_model = totalRev_post - totalRev_model) %>% select(Year, totalRev_post,
                                                                                                               totalRev_model, totalRev_obs,
                                                                                                               totalRev_diff_obs, totalRev_diff_model)
rev_total_2009_LR <- rev_total_LR %>% filter(Year>2009)



costs_cl_LR <- costs_cl_9years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_9years)
costs_sl_LR <- costs_sl_2years %>% filter(Year>1994 & Year<2017) %>% select(Year, cost_Lb_2years)

# costs_hc_cl_LR <- prices_predict_co4_merge %>% mutate(hc_9years = (hc_hat/100)) %>% select(Year, hc_9years)


costsSupply_sl_LR <- demand_predict_co4_merge_LR %>% mutate(costSupply_sl_obs = sl * costs_sl$cost_Lb_2years,
                                                      costSupply_sl_model = sl_est * costs_sl$cost_Lb_2years) %>% select(
                                                        Year, costSupply_sl_obs,  costSupply_sl_model)

costsSupply_cl_LR <- demand_predict_co4_merge_LR %>% mutate(costSupply_cl_obs = cl * costs_cl$cost_Lb_9years,
                                                      costSupply_cl_model = cl_est * costs_cl$cost_Lb_9years) %>% select(
                                                        Year, costSupply_cl_obs, costSupply_cl_model)

costsSupply_t_LR <- merge(costsSupply_sl_LR, costsSupply_cl_LR) %>% mutate(costSupply_t_obs = costSupply_sl_obs + costSupply_cl_obs,
                                                                  costSupply_t_model = costSupply_sl_model + costSupply_cl_model)

costsRevenues_LR <- merge(rev_cl_LR, merge(rev_sl_LR, merge(rev_total_LR,costsSupply_t_LR))) %>% select(Year, clRev_diff_obs, clRev_diff_model,
                                                                                         costSupply_cl_obs, costSupply_cl_model, 
                                                                                         slRev_diff_obs, slRev_diff_model,
                                                                                         costSupply_sl_obs, costSupply_sl_model,
                                                                                         totalRev_diff_obs, totalRev_diff_model,
                                                                                         costSupply_t_obs, costSupply_t_model) %>% filter(Year>2009)

revDiff_costs_sl_LR <- costsRevenues_LR %>% select(Year, slRev_diff_obs, costSupply_sl_obs, slRev_diff_model, costSupply_sl_model)
revDiff_costs_cl_LR <- costsRevenues_LR %>% select(Year, clRev_diff_obs, costSupply_cl_obs, clRev_diff_model, costSupply_cl_model)
revDiff_costs_t_LR <- costsRevenues_LR %>% select(Year, totalRev_diff_obs, costSupply_t_obs, totalRev_diff_model, costSupply_t_model)

revDiff_costs_sl_pSurp_LR <- revDiff_costs_sl_LR %>% mutate(diffRevCost_sl_obs = slRev_diff_obs - costSupply_sl_obs, 
                                                      diffRevCost_sl_model = slRev_diff_model - costSupply_sl_model) %>% select(
                                                        Year, diffRevCost_sl_obs, diffRevCost_sl_model)
revDiff_costs_cl_pSurp_LR <- revDiff_costs_cl_LR %>% mutate(diffRevCost_cl_obs = clRev_diff_obs - costSupply_cl_obs,
                                                      diffRevCost_cl_model = clRev_diff_model - costSupply_cl_model) %>% select(
                                                        Year, diffRevCost_cl_obs, diffRevCost_cl_model)
revDiff_costs_t_pSurp_LR <- revDiff_costs_t_LR %>% mutate(diffRevCost_t_obs = round(totalRev_diff_obs - costSupply_t_obs,4),
                                                    diffRevCost_t_model = round(totalRev_diff_model - costSupply_t_model,4)) %>% select(
                                                      Year, diffRevCost_t_obs, diffRevCost_t_model)









