###############################################################################################
#                                                                                             #
#           Value function iteration with linear interpolation and optimization               #
#                                                                                             #
# Not that it is terribly innovative, but it may be helpful to compare different approaches   #
# or programmes, including Python, R, or GAMS.                                                #
#                                                                                             #
# The model is a simple deterministic dynamic fisheries problem with no fishing costs and     #
# Gordon-Schaefer growth.                                                                     #
#                                                                                             #
###############################################################################################

rho        <- 0.05           # Discount rate
beta       <- 1/(1+rho)      # Discount factor
gridSize   <- 500            # Number of gridpoints
maxStock   <- 100.0          # Maximum stock size, or carrying capacity
minState   <- 0.0            # Minimum stock size, or lower bound state space
growthRate <- 0.2            # Intrinsic growth rate
numIter    <- 10             # Number of iterations

# Define state space
stateSpace <- seq(from=minState,to=maxStock,length.out=gridSize)

# Vectors needed for the algorithm
valueFunction <- array(0,gridSize)    # Value function
nextValue     <- array(0,gridSize)    # Temporary value function
optH          <- array(0,gridSize)    # Policy function
lastOptH      <- array(0,gridSize)    # Last policy function
convergence   <- array(0,numIter)     # Indicator of convergence

# Function to calculate next state from harvest
# Variable tempState will get value later on
nextState <- function(h) {
  max(0.0,tempState+growthRate*tempState*(1-tempState/maxStock)-h)
}

# Function to estimate value of the Bellman function for some harvest h, at some value of the
# state variable
bellMan <- function(h){
  if(h > tempState | h < 0.0){
    Inf
  }
  else {
    h+beta*approx(stateSpace,valueFunction,nextState(h))$y
  }
}

# Iterate value function
for(iter in 1:numIter){
  for(iState in 1:length(stateSpace)){
    tempState <- stateSpace[iState]
    if(tempState>0){                                                             # Function does not work for zero state
      out <- optimize(bellMan,lower=0.0,upper=tempState-minState,maximum=TRUE)   # Find optimum
      optH[iState] <- out$maximum                                                # Read optimal harvest
      nextValue[iState] <- out$objective                                         # Read maximum value
    }
    else {                                   # So if the stock is zero:
      optH[iState] <- 0                      # Harvest is zero
      nextValue[iState] <- 0                 # Value is zero (yes I KNOW this could be more efficient)
    }
  }
  convergence[iter] <- sum(abs(lastOptH-optH))
  lastOptH <- optH
  valueFunction <- nextValue                  # Replace old value function by new one
}

# Optimal steady state stock according to the model
optX1 <- array(0,gridSize)
for(iState in 1:length(stateSpace)){
  tempState <- stateSpace[iState]
  optX1[iState] <- nextState(optH[iState])
}

# Which, for example, would be for the largest 5 stock sizes:
optX1[496:500]

# Optimal steady state according to the Golden Rule
(growthRate-rho)/(2*(growthRate/maxStock))
