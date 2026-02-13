#####################
# load libraries
# set wd
# clear global .envir
#####################

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# create data
set.seed(123)
n <- 1000
empirical <- rcauchy(n, location = 0, scale = 1)
# create K-S function to compare data against normal distribution 
ksTest <- function (data){
  # create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data))) 
  # calculate p-value
  # empty vector to be filled
  summed <- NULL
  for(i in 1:n){
    summed <- c(summed, exp((-(2 * i - 1)^2 * pi^2) / ((8 * D)^2)))
  }
  pValue <- sqrt(2*pi)/D * sum(summed) 
  cat("D =", D, "\n")
  cat("p-value =", pValue, "\n")
}
# run "by hand" K-S test
ksTest(empirical)

# double check K-S test with R function
ks.test(empirical, "pnorm")

#####################
# Problem 2
#####################
# create data
set.seed (123)
ex_data <- data.frame(x = runif(200, 1, 10))
ex_data$y <- 0 + 2.75*ex_data$x + rnorm(200, 0, 1.5)

# create log-likelihood function
# we need to specify what the outcome will be,
# what the input variables are, and the starting values
# of the parameters to be estimated
norm_log_likelihood <- function(outcome, input, parameter) {
  # how many betas are we estimating? (# of columns)
  n <- ncol(input)
  beta <- parameter[1:n]
  # estimate of our variance as well
  sigma <- parameter[1+n]
  # put all of those into our log-likelihood (since log=T)
  # remember, we're referencing normal distribution, so 
  # that's why you see dnorm()
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}

# print our estimated coefficients (intercept and beta_1)
results_norm <- optim(fn=norm_log_likelihood, outcome=ex_data$y, input=cbind(1, ex_data$x), par=c(1, 1, 1), hessian=T)
# print our estimated coefficients (intercept and beta_1)
# get same results regardless of which log-likelihood function we use
round(results_norm$par, 2)
# confirm that we get the same thing with lm()
ols_model <- lm(y~x, data=ex_data)
round(coef(ols_model), 2)
round(sigma(ols_model), 2)
