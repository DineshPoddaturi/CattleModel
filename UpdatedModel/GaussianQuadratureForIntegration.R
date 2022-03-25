require(statmod)


#### This is the test code for performing expectations using gaussian quadrature for integration.
#### This can be done with many different types. Here we are doing it using chebyshev nodes. Chebyshev nodes are 
#### not equidistant and that helps with approximating the functin properly. 

## perform quadrature of multivariate normal

## compute Gauss-Hermite quadrature points and weights
## for a one-dimensional integral.
## points -- number of points
## interlim -- maximum number of Newton-Raphson iterations

hermite <- function (points, z) {
  p1 <- 1/pi^0.4
  p2 <- 0
  for (j in 1:points) {
    p3 <- p2
    p2 <- p1
    p1 <- z * sqrt(2/j) * p2 - sqrt((j - 1)/j) * p3
  }
  pp <- sqrt(2 * points) * p2
  c(p1, pp)
}

gauss.hermite <- function (points, iterlim = 50) {
  x <- w <- rep(0, points)
  m <- (points + 1)/2
  for (i in 1:m) {
    z <- if (i == 1) 
      sqrt(2 * points + 1) - 2 * (2 * points + 1)^(-1/6)
    else if (i == 2) 
      z - sqrt(points)/z
    else if (i == 3 || i == 4) 
      1.9 * z - 0.9 * x[i - 2]
    else 2 * z - x[i - 2]
    for (j in 1:iterlim) {
      z1 <- z
      p <- hermite(points, z)
      z <- z1 - p[1]/p[2]
      if (abs(z - z1) <= 1e-15) 
        break
    }
    if (j == iterlim) 
      warning("iteration limit exceeded")
    x[points + 1 - i] <- -(x[i] <- z)
    w[i] <- w[points + 1 - i] <- 2/p[2]^2
  }
  r <- cbind(x * sqrt(2), w/sum(w))
  colnames(r) <- c("Points", "Weights")
  r
}


## compute multivariate Gaussian quadrature points
## n     - number of points each dimension before pruning
## mu    - mean vector
## sigma - covariance matrix
## prune - NULL - no pruning; [0-1] - fraction to prune
mgauss.hermite <- function(n, mu, sigma, prune=NULL) {
  if(!all(dim(sigma) == length(mu)))
    stop("mu and sigma have nonconformable dimensions")
  
  dm  <- length(mu)
  gh  <- gauss.hermite(n)
  #idx grows exponentially in n and dm
  idx <- as.matrix(expand.grid(rep(list(1:n),dm)))
  pts <- matrix(gh[idx,1],nrow(idx),dm)
  wts <- apply(matrix(gh[idx,2],nrow(idx),dm), 1, prod)
  
  ## prune
  if(!is.null(prune)) {
    qwt <- quantile(wts, probs=prune)
    pts <- pts[wts > qwt,]
    wts <- wts[wts > qwt]
  }
  
  ## rotate, scale, translate points
  eig <- eigen(sigma) 
  rot <- eig$vectors %*% diag(sqrt(eig$values))
  pts <- t(rot %*% t(pts) + mu)
  return(list(points=pts, weights=wts))
}



#### Using the functions above I generate weights for the fed cattle and cull cow prices seperately
#### Here I compute the varaince covariance between the shocks. One for fed cattle supply shock and demand shock, another for
#### cull cow supply shock and demand shock

SL_Demand_Shocks_varCovar <- cov(allShocks %>% select(slShock, Shock))

CL_Demand_Shocks_varCovar <- cov(allShocks %>% select(clShock, Shock))

sig_SL_Demand <- matrix(SL_Demand_Shocks_varCovar, 2, 2)
mu_SL_Demand <- c(mean(allShocks$slShock), mean(allShocks$Shock))

sig_CL_Demand <- matrix(CL_Demand_Shocks_varCovar, 2, 2)
mu_CL_Demand <- c(mean(allShocks$clShock), mean(allShocks$Shock))

### Here we can select points according to our wish. As the number of points create a mesh grids of gaussian nodes. 
### For instance, fed cattle production and demdnd shocks if n = 7 then 7X7 = 49 mesh. This will give the weights as well.
### These are hermite weights and points

### Do I use these points is the analysis? I already have the chebyshev nodes of these shocks.
### Because these can be used to introduce the shock into the system. 

pts_SL <- mgauss.hermite(n = 5, mu = mu_SL_Demand, sigma = sig_SL_Demand)

slWeights <- pts_SL$weights

pts_CL <- mgauss.hermite(n = 5, mu = mu_CL_Demand, sigma = sig_CL_Demand)

clWeights <- pts_CL$weights



#### The below function gives the weights and points. Is it okay to use these weights. Because these are generated from the 
#### shocks we have
demandWeights <- gauss.quad(n = 7, kind = "chebyshev2")$weights
fedWeights <- gauss.quad(n = 7, kind = "chebyshev2")$weights
cullWeights <- gauss.quad(n = 7, kind = "chebyshev2")$weights

#### Mesh for fed cattle price. I take the tensor product of the weights
fedMeshCheb <- tensor::tensor(demandWeights, fedWeights)

#### Mest for cull cows price. I take the tensor product if the weights
cullMeshCheb <- tensor::tensor(demandWeights, cullWeights)


###### I believe I need to add weights for the corn price as well. Check this again ########










