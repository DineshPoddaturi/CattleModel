f <- function(x) log(x[[1]])*sqrt(x[[2]])/log(sum(x))
ch <- ipol(f, dims=c(5,8), intervals=list(c(1,2), c(15,20)), method='chebyshev')
