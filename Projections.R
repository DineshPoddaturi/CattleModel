##### Projections

proj_adjFac <- adjFactor_New

proj_muTildes <- mu_Tildes_MM
proj_sTildes <- s_Tildes_MM
proj_PricesCosts <- Reduce(function(...) merge(...), list(EQestPS,EQestPC,EQestHC))

proj_muTildes1 <- mu_Tildes_MM_itr
proj_sTildes1 <- s_Tildes_MM_itr
proj_PricesCosts1 <- Reduce(function(...) merge(...), list(ITRestPS,ITRestPC,ITRestHC))

#### We use the following to get the t+1 supply of the fed cattle
##### See the work in the binder
proj_K_t <- Stock %>% transmute(Year = Year, K = K)
proj_A <- A_quant

proj_All <- Reduce(function(...) merge(...), 
                   list(proj_K_t,proj_A,proj_adjFac,proj_muTildes,proj_sTildes,proj_PricesCosts))

shareMetric <- function(paramMu, paramS, ps, pc){
  
  share <- ((exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))/(1 + (exp((paramMu - ((ps/phi) - (pc/phi)))/paramS))))
  return(share)
  
}











