install.packages("BB")
require(BB)
require(nleqslv)



####################################################
### All 6-year old cows are culled ######
####################################################



## From WASDE we get the supply and ending stocks of beef 
# production <- 27202
# beg_stocks <- 662
# imports <- 3046
# exports <- 3074
# end_stocks <- 625
# demand <- production + beg_stocks + imports - exports - end_stocks

#https://www.usda.gov/oce/commodity/wasde/wasde0520.pdf



beta <- 0.98
delta <- 0.95
gamma0 <- 0.90
gamma1 <- 0.95
g <- 0.97
phi <- 0.63

production <- 27221
beg_stocks <- 662
imports <- 3057
exports <- 3022
end_stocks <- 642

# demand <- production + beg_stocks + imports - exports - end_stocks
demand <- production + beg_stocks - exports - end_stocks


#Converting the demand of meat into live animal weight
A <- ((demand *1000000)/phi)

#mu is the williness to pay for slaughtered meat over culled meat (simply a premium)
mu <- 0.4823063
p_std <-  0.08653615

#multiplying them by 0.63 to convert it into per pound of live cattle
mu_tilde <- mu * phi
s_tilde <- (p_std * (sqrt(3)/pi)) * phi



#Assuming a 1300 pound animal the supply of live animal in meat is 
K <- 23411950 * 1300

k_6 <- (delta^4) * ((1-delta)/(1-delta^5)) * K
k_3 <- (1/delta^3) * k_6 


sysEqs_6_1 <- function(p){
  ps <- p[1]
  pc <- p[2]
  # h <- p[3]
  
  F <- numeric(length(p))
  
  F[1] <- g * K - (1/delta^3)  * k_6 - A * (exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde))) 
  
  # F[2] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  F[2] <- ps * ( 1- ((g*(beta^3)) * ((1-beta^4)/(1-beta)) ) ) - (beta^4)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^4)/(1-beta))*h
  
  # F[3] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  F
}

sysEqs_6_2 <- function(p){
  ps <- p[1]
  pc <- p[2]
  # h <- p[3]
  
  F <- numeric(length(p))
  
  # F[1] <- g * K - (1/delta^3)  * k_6 - A * (exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde))) 
  
  F[1] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  F[2] <- ps * ( 1- ((g*(beta^3)) * ((1-beta^4)/(1-beta)) ) ) - (beta^4)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^4)/(1-beta))*h
  
  # F[3] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  F
}


hol <- rep(seq(from = 0.1, to = 1, by = 0.1), each=10)

ps <- seq(from = 1.243333, to = 1.392222, length.out = 10)
pc <- seq(from = 1.056222, to = 1.210000, length.out = 10)

df <- data.frame(h = hol,  ps = rep(ps,10), pc = rep(pc, 10), ps_1 = numeric(length(hol)),
                 pc_1 = numeric(length(hol)), ps_2 = numeric(length(hol)), pc_2 = numeric(length(hol)))



for(i in 1:nrow(df)){
  
  h <- hol[i]
  p <- c(df$ps[i], df$pc[i])
  
  out1 <- nleqslv(x = p, fn = sysEqs_6_1, control = list(maxit=2000, allowSingular = TRUE))
  out2 <- nleqslv(x = p, fn = sysEqs_6_2, control = list(maxit=2000, allowSingular = TRUE))
  
  df$ps_1[i] <- out1$x[1]
  df$pc_1[i] <- out1$x[2]
  df$ps_2[i] <- out2$x[1]
  df$pc_2[i] <- out2$x[2]
}

df_e_uq <- unique(df %>% select(ps_1, pc_1, ps_2, pc_2) %>% round(digits=6))

row.names(df_e_uq) <- 1:10                                                                     


df_e_uq <- round(df_e_uq %>% mutate(ps_m = (ps_1 + ps_2)/2, pc_m = (pc_1 + pc_2)/2),6)


a <- df_e_uq$ps_m[10]
b <- df_e_uq$pc_m[10]


# g * K - (1/delta^3)  * k_6 - A * (exp( (mu_tilde - ((a/phi)- (b/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((a/phi)- (b/phi)))/s_tilde))) 


k6 <- A * (1/(1+ exp((mu_tilde - ((a/phi) - (b/phi)))/s_tilde)))
k5 <- (1/delta) * k6
k4 <- (1/delta) * k5
k3 <- (1/delta) * k4

0.5*g*K > k3











##############################################################################
sysEqs <- function(p){
  ps <- p[1]
  pc <- p[2]
  # h <- p[3]
  h <- ((1-beta)/(1-beta^4)) * (1/(1+g*beta*(gamma0 + gamma1*beta))) * ((beta^4)*p[2] - ( 1- ((g*(beta^3)) * ((1-beta^4)/(1-beta)) ) ) *p[1])
  
  F <- numeric(length(p))
  
  F[1] <- g * K - (1/delta^3)  * k_6 - A * (exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde))) 
  
  F[2] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  # F[3] <- ps * ( 1- ((g*(beta^3)) * ((1-beta^4)/(1-beta)) ) ) - (beta^4)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^4)/(1-beta))*h
  
  # F[3] <- k_6  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  print(h)
  
  print(F)
}

h <- 0.1
p <- c(0, 0)
out1 <- nleqslv(x = p, fn = sysEqs, control = list(maxit=2000, allowSingular = TRUE), jacobian = TRUE)
out2 <- nleqslv(x = p, fn = sysEqs_6_2, control = list(maxit=2000, allowSingular = TRUE))






# c(1.27, 0.792, 0.5)
#p contains the initial guess of slaughtered price, culled price, and holding costs per pound of live animal respectively
p <- c(1.020, 0.8385, 0.2)
p <- c(0, 0, 0)

#Using the function nonlinear least squares
out1 <- nleqslv(x = p, fn = sysEqs_6_1, control = list(maxit=2000, allowSingular = TRUE))

out2 <- nleqslv(x = p, fn = sysEqs_6_2, control = list(maxit=2000))

#Using the function dfsane (solves non linear system of equations)
bb(par = p, fn=sysEqs_6, control = list(maxit=3000))

ps_e <- 1.579917
pc_e <- 1.409386










# ps <- seq(from = 1.020, to = 1.690, length.out = 10)
# pc <- seq(from = 0.5180, to = 1.2100, length.out = 10)
# pss <- NA
# pcc <- NA
# 
# for (i in 1:length(ps)){
#   
#   for(j in 1:length(pc)){
#       diff <- ps[i]-pc[j]
#       
#       if(diff>=0.1674606 && diff<=0.1876275){
#         pss[j] <- ps[i]
#         pcc[j] <- pc[j]
#         print(c(ps[i],pc[j]))
#       }
#   }
#   
# }


