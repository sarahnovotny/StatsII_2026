##################
#### Stats II ####
##################

###############################
#### Tutorial 3: MLE ####
###############################

# In today's tutorial, we'll begin to explore MLE in R
#     1. Import/wrangle data
#     2. Create log-likelihood functions
#     3. Compare our own MLE models with build-in function glm()

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set seed so we all get same answers 
set.seed(1234)
# create design matrix
# only 2 predictors, remember 1st column is 1s
X <- rnorm(100)
# define "real"/true relationship
real_beta <- 3

# create output variables
# as linear function of covariates
# (1) binom
y_binom <- rbinom(100, 1, exp(X*real_beta)/(1+exp(X*real_beta)))

# derive our log-likelihood function for binomial distribution
binom_likelihood <- function(outcome, input, parameter) {
  # calculate probability of success on each trial
  p <- exp(parameter[1] + parameter[2]*input)/(1+exp(parameter[1] + parameter[2]*input))
  # access probability density function (pdf) for binomial distribution
  # specifically, calculate log.likelihood function 
  # using sum and negative since its' log, not normal likelihood function
  -sum(dbinom(outcome, 1, p, log=TRUE))
}

# optimise our log-likelihood function
# need to put in par, which are initial values for parameters to be optimized over
# we'll start with zero and 1 for intercept and beta
# using BFGS because it's a quasi-Newton method
# so similar to what we did in class, and what you'll get from glm()
results_binom <- optim(fn=binom_likelihood, outcome=y_binom, input=X, par=0:1, hessian=T, method="BFGS")
# print our estimated coefficients (intercept and beta_1)
results_binom$par
# confirm that we get the same thing in with glm()
coef(glm(y_binom~X, family=binomial))

# now do the same process to derive our log-likelihood function for normal distribution
# we can use the same predictor and "real" effect, just need to create a new outcome variable
y_norm <- X*real_beta + rnorm(100, 0, 0.5)

norm_likelihood <- function(outcome, input, parameter) {
  n      <- nrow(input)
  k      <- ncol(input)
  beta   <- parameter[1:k]
  sigma2 <- parameter[k+1]^2
  e      <- outcome - input%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}
# show you two different ways to set up same likelihood function
norm_likelihood2 <- function(outcome, input, parameter) {
  n <- ncol(input)
  beta <- parameter[1:n]
  sigma <- parameter[1+n]
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}
# print our estimated coefficients (intercept and beta_1)
results_norm <- optim(fn=norm_likelihood, outcome=y_norm, input=cbind(1, X), par=c(1,1,1), hessian=T, method="BFGS")
results_norm2 <- optim(fn=norm_likelihood2, outcome=y_norm, input=cbind(1, X), par=c(1,1,1), hessian=T, method="BFGS")
# print our estimated coefficients (intercept and beta_1)
# get same results regardless of which log-likelihood function we use
results_norm$par; results_norm2$par
# Q: what that third parameter that we've exstimated w/ our log-likelihood function?
# confirm that we get the same thing in with glm()
coef(lm(y_norm~X))

# now your turn using the poisson distribution:
# first, let's create design matrix w/ only 1 predictor
X <- rnorm(100)

# define "real"/true relationship
real_beta0 <- 0.5      # true intercept
real_beta1 <- 0.8      # true slope

# create output variable
# as a log-linear function of covariates
# log(lambda_i) = beta0 + beta1 * X_i
lambda <- exp(real_beta0 + real_beta1 * X)
y_pois <- rpois(100, lambda)

# write your own likelihood function
# maximize your likelihood function using optim(), print the results
# compare your results to the built-in glm() function