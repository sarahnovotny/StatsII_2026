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
lapply(c("survival", "stargazer", "sampleSelection", "eha"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# helper for writing stargazer output to tex files
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=FALSE)
}

#####################
# Problem 1
#####################

# load data on child mortality
library(eha)
data("child")

# examine the data
str(child)
summary(child)

# fit Cox Proportional Hazard model
# Surv(enter, exit, event) defines the survival object for left-truncated data
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
summary(cox_model)

# save output
output_stargazer("./cox_model.tex", cox_model,
                 title = "Cox Proportional Hazard Model: Child Mortality")

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

# examine the data
str(disaster_data)
summary(disaster_data$binContribution)
summary(disaster_data$originalContributionMillionUSDLogged)

# drop rows with missing values in the variables we use
disaster_clean <- disaster_data[complete.cases(
  disaster_data[, c("binContribution", "originalContributionMillionUSDLogged",
                     "occurrences", "deathsEM", "normalizedDamageEMLogged")]), ]

# Heckman selection model (two-step)
# Selection equation: binContribution (whether any donation occurs)
# Outcome equation: originalContributionMillionUSDLogged (amount, if donated)
heckman_model <- selection(binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
                           originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
                           data = disaster_clean,
                           method = "2step")
summary(heckman_model)

# save output — stargazer does not support heckit objects directly,
# so we build a summary table manually
heckman_summ <- summary(heckman_model)

# probit selection equation coefficients
sel_coefs <- heckman_summ$estimate[1:4, ]
# outcome equation coefficients
out_coefs <- heckman_summ$estimate[5:8, ]
# rho, sigma, inverse Mills ratio
rho_sigma <- heckman_summ$estimate[9:nrow(heckman_summ$estimate), ]

sel_df <- as.data.frame(round(sel_coefs, 4))
out_df <- as.data.frame(round(out_coefs, 4))
rho_df <- as.data.frame(round(rho_sigma, 4))

output_stargazer("./heckman_selection.tex", sel_df, summary = FALSE,
                 title = "Heckman Selection Equation (Probit)")
output_stargazer("./heckman_outcome.tex", out_df, summary = FALSE,
                 title = "Heckman Outcome Equation (OLS with correction)")
output_stargazer("./heckman_diagnostics.tex", rho_df, summary = FALSE,
                 title = "Heckman Model Diagnostics")
