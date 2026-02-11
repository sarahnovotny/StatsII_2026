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

# Generate Cauchy data and test
set.seed(123)
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# create empirical distribution of observed data
ECDF <- ecdf(cauchy_data)
empiricalCDF <- ECDF(cauchy_data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(cauchy_data)))

ks_test_normal <- function(x) {
  n <- length(x)
  x_sorted <- sort(x)

  mu <- mean(x)
  sigma <- sd(x)

  # Theoretical CDF values at each ordered observation
  F_theoretical <- pnorm(x_sorted, mean = mu, sd = sigma)
  
  # Empirical CDF: i/n for upper, (i-1)/n for lower
  F_upper <- (1:n) / n
  F_lower <- (0:(n - 1)) / n
  
  # KS statistic: max absolute difference
  # We check both F_upper and F_lower to capture the max gap
  D <- max(max(abs(F_theoretical - F_upper)),
           max(abs(F_theoretical - F_lower)))
  
  # Return the test statistic
  # (p-value approximation via the KS CDF is complex; 
  #  we can compare to ks.test() for validation)
  return(list(D = D, n = n))
}  

ks_pvalue <- function(D, n) {
  # Scale the statistic by sqrt(n)
  d <- D  * sqrt(n)
  
  # Approximate the infinite sum with enough terms 
  # (per Marsaglia, Tsang and Wang, 2003)
  k <- 1:1000
  sum_terms <- sum(exp(-(2*k - 1)^2 * pi^2 / (8 * d^2)))
  
  # K-S CDF
  p_leq <- (sqrt(2 * pi) / d) * sum_terms
  
  # p-value is the upper tail
  p_value <- 1 - p_leq
  return(p_value)
}


result <- ks_test_normal(cauchy_data)
result_pvalue <- ks_pvalue(result$D, result$n)
# this ends up bottoming out at machine epsilon for double.neg 
# -- for more precision use pkg functions.

mu_hat <- mean(cauchy_data)
sigma_hat <- sd(cauchy_data)
builtin <- ks.test(cauchy_data, "pnorm", mean = mu_hat, sd = sigma_hat)

sink("KSv.builtin.tex", type = "output")

cat("% KS Test: Custom Implementation vs Built-in ks.test()\n")
cat("\\subsection*{Kolmogorov-Smirnov Test Comparison}\n")
cat("\\begin{table}[h]\n")
cat("\\centering\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\hline\n")
cat("Metric & Custom & Built-in \\\\\n")
cat("\\hline\n")
cat("$D$ Statistic &", round(result$D, 6), "&", round(as.numeric(builtin$statistic), 6), "\\\\\n")
cat("$p$-value &", format(ks_pvalue(result$D, result$n), scientific = TRUE), "&", 
    format(builtin$p.value, scientific = TRUE), "\\\\\n")
cat("\\hline\n")
cat("\\end{tabular}\n")
cat("\\caption{Comparison of custom KS test implementation vs built-in \\texttt{ks.test()}.}\n")
cat("\\label{tab:ks_comparison}\n")
cat("\\end{table}\n")

sink()

#####################
# Problem 2
#####################

set.seed (123)
df <- data.frame(x = runif(200, 1, 10))
df$y <- 0 + 2.75*df$x + rnorm(200, 0, 1.5)


# Method 1 basic LM
#
ols_fit <- lm(y ~ x, data = df)
# summary(ols_fit)

# Method 2 BFGS

negative_log_llh <- function(params, X, y) {
  n <- length(y)
  beta0 <- params[1]
  beta1 <- params[2]
  sigma2 <- params[3]

  if (sigma2 <= 0) return(1e10)  # penalty: push optimizer away
  
  epsilon <- y - (beta0 + beta1 * X)
  nll <- ((n/2) * log(2 * pi) + (n/2) * log(sigma2) + 
    sum(epsilon^2) / (2 * sigma2))
   return(nll)
}

X <- df$x
start_params <- c(0, 0, 1) # Starting values: 2 betas + sigma2

bfgs_fit <- optim(par = start_params,
                  fn = negative_log_llh,
                  X = X, y = df$y,
                  method = "BFGS"
)


sink("OLSv.BFGS.tex", type = "output")

cat("\\subsection*{OLS Comparison: lm() vs BFGS}\n")
cat("\\begin{table}[h]\n")
cat("\\centering\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\hline\n")
cat("Parameter & lm() & BFGS \\\\\n")
cat("\\hline\n")
cat("$\\hat{\\beta}_0$ &", round(coef(ols_fit)[1], 4), "&", round(bfgs_fit$par[1], 4), "\\\\\n")
cat("$\\hat{\\beta}_1$ &", round(coef(ols_fit)[2], 4), "&", round(bfgs_fit$par[2], 4), "\\\\\n")
cat("$\\hat{\\sigma}$ &", round(summary(ols_fit)$sigma, 4), "&", round(sqrt(bfgs_fit$par[3]), 4), "\\\\\n")
cat("\\hline\n")
cat("\\end{tabular}\n")
cat("\\caption{Comparison of OLS estimates via lm() and BFGS optimization.}\n")
cat("\\label{tab:ols_comparison}\n")
cat("\\end{table}\n")

sink()

# Method 3 Generalized for multiple variables

negative_log_llh_multi <- function(params, X, y) {
  n <- length(y)
  beta <- params[1:(length(params) - 1)]
  sigma2 <- params[length(params)]
  
  if (sigma2 <= 0) return(1e10)
  
  epsilon <- y - X %*% beta
  nll <- (n/2) * log(2 * pi) + (n/2) * log(sigma2) + 
         sum(epsilon^2) / (2 * sigma2)
  return(nll)
}
#Matrix X
X <- cbind(1, df$x)

start_params <- c(0, 0, 1)

bfgs_fit <- optim(par = start_params,
                  fn = negative_log_llh_multi,
                  X = X, y = df$y,
                  method = "BFGS")

# --- Compare ---
cat("lm coefficients:\n")
print(coef(ols_fit))

cat("\nBFGS coefficients:\n")
names(bfgs_fit$par) <- c("(Intercept)", "x", "sigma2")
print(bfgs_fit$par[1:2])

cat("\nlm sigma:", summary(ols_fit)$sigma, "\n")
cat("BFGS sigma:", sqrt(bfgs_fit$par[3]), "\n")
