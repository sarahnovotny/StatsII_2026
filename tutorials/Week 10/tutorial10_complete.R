###################################
# Tutorial 10: 
# Survival Analysis #
# Truncated Data #
###################################

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

#### Survival Analysis
data(child)

#### Completed
# a)
child_surv <- with(child, Surv(enter, exit, event))

km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

km_socBranch <- survfit(child_surv ~ socBranch, data = child)
summary (km_socBranch)
autoplot(km_socBranch)

# b)
cox <- coxph(child_surv ~ sex + socBranch, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
                 )
               )

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))
# Note: the confidence intervals on this plot are for the prediction, not
# the standard error of the terms in the model (the effect of sex in the 
# cox ph model was significant, here the CIs overlap. Always check your
# results and interpretation!)


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")

#### Truncated Data

education_data <- read.table("https://www.john-fox.ca/AppliedRegression/datasets/Long-PhDs.txt")

with(education_data, hist(satVerbal))
with(education_data, hist(satMath))

# OLS on original data
summary(m1 <- lm(satVerbal ~ ., education_data)) 
summary(m1 <- lm(satMath ~ ., education_data)) 

# Tobit 
summary(m.tobit <- vglm(satVerbal ~ ., tobit(Lower = 200, Upper = 800), education_data)) 
summary(m.tobit <- vglm(satMath ~ ., tobit(Lower = 200, Upper = 800), education_data)) 

