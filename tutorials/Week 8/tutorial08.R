##############################################
# Tutorial 5 
# Ordered and Multinomial Logistic Regression
##############################################

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

lapply(c("MASS", "nnet", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This data set is analyzed by Long (1997).  The response variable has four ordered categories:
# Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement

# "A working mother can establish just as warm and secure a relationship with her children as a mother who does not work."

# The explanatory variables are:
# the year of the survey (1977 or 1989),
# the gender of the respondent,
# the race of the respondent (white or non-white),
# the respondent's age, and
# the prestige of the respondent's occupation (a quantitative variable)

# load data
workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# Re-label your outcome, gender, race, and year so they are legible
# i.e. "SD", "D", "A", "SA" to "Strongly Disagree", "Disagree", "Agree", "Strongly Agree" 
# 0,1 to "Non-white", "White"
# and "Year1977", "Year1989" to "1977", "1989"

# Plot prestige (y-axis) by your outcome (x-axis) by gender ~ year

# a) Perform an ordered (proportional odds) logistic regression

# Calculate a p value
# Calculate confidence intervals
# Convert to odds ratio

# How do we interpret these coefficients?

# b) fit a multinomial logit model
# with Strongly Disagree as reference level for the outcome

# run model
# get p values
# how do we interpret these coefficients?

# calculate predicted probabilities to help our interpretation

# c) Consider gender as an interaction with your other predictors
# i.e. consider that possibility that gender interacts with the other explanatory variables in influencing the response variable

# What do you find?