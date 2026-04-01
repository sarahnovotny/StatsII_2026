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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)
# create factor variable for outcome
gdp_data[gdp_data$GDPWdiff==0, "GDPcat"] <- "no change"
gdp_data[gdp_data$GDPWdiff>0, "GDPcat"] <- "positive"
gdp_data[gdp_data$GDPWdiff<0, "GDPcat"] <- "negative"
gdp_data$GDPcat <- relevel(as.factor(gdp_data$GDPcat), ref="no change")

# (1)
# run unordered multinom logit
unordered_logit <- multinom(GDPcat ~ REG + OIL, data=gdp_data)
summary(unordered_logit)
# (2)
# re-level factor to impose ordering
gdp_data$GDPcat <- relevel(gdp_data$GDPcat, ref="negative")
# run ordered multinom logit
ordered_logit <- polr(GDPcat ~ REG + OIL, data=gdp_data)
summary(ordered_logit)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
# estimate poisson model
mex_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family=poisson)
summary(mex_poisson)

# (c)
# option #1: manually
exp(coef(mex_poisson)[1] + coef(mex_poisson)[2]*1 + coef(mex_poisson)[3]*0 + coef(mex_poisson)[4]*1)

# option #2: predict() function
predict(mex_poisson, newdata=data.frame(competitive.district=1, marginality.06 = 0, PAN.governor.06=1), type="response")

