### Changes in the demand
psM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$psMedian)
pcM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$pcMedian)
hcM_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$hcMedian)

MUtilde_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$muMedian)
Stilde_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$sMedian)

capA_pre <- mean(tail(modelParamsEQ_PreFMD, n=1)$A)

sh_pre <- ((exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))/
    (1 + (exp((MUtilde_pre - ((psM_pre/phi) - (pcM_pre/phi)))/Stilde_pre))))



# Total demand = ps * As + pc * Ac

TD_pre <- psM_pre * capA_pre * sh_pre + pcM_pre * capA_pre * (1-sh_pre)

TD_2010 <- 0.95 * TD_pre


F1 <- TD_2010 - 
  capA_pre * ((exp((MUtilde_pre - ((ps/phi) - (pc/phi)))/Stilde_pre))/(1 + (exp((tilde_MU - ((ps/phi) - (pc/phi)))/tilde_s))))














