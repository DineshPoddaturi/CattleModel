install.packages("chebpol")
require(chebpol)


chebknots(4,interval=c(2,3))

expand.grid(chebknots(c(x=3,y=4,z=5), list(c(1,3), c(4,6), c(800,900))))

X <- matrix(data = c(1,0,1,1), nrow = 2, byrow = TRUE)
Y <- matrix(data = c(1,0,0,1,1,1,1,2,4),  nrow=3, byrow = TRUE)

kronecker(X,Y)




rho <- 0.75

ourFunction <- function(x,y,rho){
  f <- ( x^rho + y^rho )^rho
}

x_min <- 0.01
x_max <- 2
y_min <- 0.01
y_max <- 2

m_x <- 20
m_y <- 20

n_x <- 5
n_y <- 5

r_x <-  -cos((2*seq(1,m_x+1) - 1) * pi / (2*m_x))
r_y <-  -cos((2*seq(1,m_y+1) - 1) * pi / (2*m_y))

scale_up <- function(z,x_min,x_max){
 
    #Scales up z \in [-1,1] to x \in [x_min,x_max]
    #where z = (2 * (x - x_min) / (x_max - x_min)) - 1

    return (x_min + (z + 1) * (x_max - x_min) / 2)
}

x = scale_up(r_x,x_min,x_max)
y = scale_up(r_y,y_min,y_max)






















