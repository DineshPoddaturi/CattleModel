install.packages("BB")
require(BB)
require(nleqslv)



####################################################
### All 10-year old cows are culled ######
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
mu <- 0.7655656 
p_std <- 0.137359

#multiplying them by 0.63 to convert it into per pound of live cattle
mu_tilde <- mu * phi
s_tilde <- (p_std * (sqrt(3)/pi)) * phi



#Assuming a 1300 pound animal the supply of live animal in meat is 
K <- 23411950 * 1300

k_10 <- (delta^7) * ((1-delta)/(1-delta^8)) * K
k_3 <- (1/(delta^7)) * k_10

sysEqs_10 <- function(p){
  ps <- p[1]
  pc <- p[2]
  h <- p[3]
  
  F <- numeric(length(p))
  
  F[1] <- g * K - k_3  - A * (exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde))) 
  
  F[2] <- ps * ( 1- ((g*(beta^3)) * ((1-beta^8)/(1-beta)) ) ) - (beta^8)*pc + (1+g*beta*(gamma0 + gamma1*beta))*((1-beta^8)/(1-beta))*h
  
  F[3] <- (1/delta^3) * k_10  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))
  
  print(F)
}

#p contains the initial guess of slaughtered price, culled price, and holding costs per pound of live animal respectively
p <- c(1.27, 0.792, 0.5)

#Using the function nonlinear least squares
nleqslv(x = p, fn = sysEqs_10, control = list(maxit=2000, allowSingular = TRUE))

#Using the function dfsane (solves non linear system of equations)
dfsane(par = p, fn=sysEqs_10, control = list(maxit=3000))


ps <- 1.27
pc <- 0.792



g * K - k_3  - A * (exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde) / (1 + exp( (mu_tilde - ((ps/phi)- (pc/phi)))/s_tilde)))

(1/delta^3) * k_10  - A * (1/(1+ exp((mu_tilde - ((ps/phi) - (pc/phi)))/s_tilde)))



