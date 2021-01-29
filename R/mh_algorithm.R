iterations <- 5000
J <- 2 
set.seed(159) 
X <- runif(n,-2,2) 
beta <- runif(J,-2,2) 
lambda <- exp(beta[1] + beta[2] * X)
y <- rpois(n, lambda = lambda) 
LikelihoodFunction <- function(param){
  beta0 <- param[1] 
  beta1 <- param[2] 
  lambda <- exp(beta1*X + beta0)
  
  loglikelihoods <- sum(dpois(y, lambda = lambda, log=T)) 
  return(loglikelihoods)
}
LogPriorFunction <- function(param){
  beta0 <- param[1] 
  beta1 <- param[2] 
  beta0prior <- dnorm(beta0, 0, sqrt(100), log=TRUE)
  beta1prior <- dnorm(beta1, 0, sqrt(100), log=TRUE)
  return(beta0prior + beta1prior) 
}
PosteriorFunction <- function(param){
  return (LikelihoodFunction(param) + LogPriorFunction(param)) 
}
ProposalFunction <- function(param){
  return(rnorm(2, mean = param, sd = 0.01))
}
RunMetropolisMCMC <- function(startvalue, iterations){
  
  chain <- array(dim=c(iterations + 1, 2)) 
  chain[1, ] <- startvalue 
  for (i in 1:iterations){
    
    Y <- ProposalFunction(chain[i, ]) 
    
    probability <- exp(PosteriorFunction(Y) - 
                         PosteriorFunction(chain[i, ]))
    
    if (runif(1) < probability) {
      chain[i+1, ] <- Y
    }else{ 
      chain[i+1, ] <- chain[i, ]
    }
  }
  return(chain)
}
startvalue <- c(0, 0) 
iterations <- 20 * 10000 
chain <- RunMetropolisMCMC(startvalue, iterations)

cfinal <- matrix(NA, ncol = 2, nrow = 10000)
for (i in 1:10000){
  if (i == 1){
    cfinal[i, ] <- chain[i*20,]
  } else {
    cfinal[i, ] <- chain[i*20,]
  }
}

burnIn <- 5000 
