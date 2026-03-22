#######################
# Tutorial 9: Poisson #
#######################

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

lapply(c("ggplot2", "stargazer", "AER", "pscl"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990)
# Productivity of doctoral students in biochemistry during the last three years
# of their PhD programmes.
# The response variable is the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student's marital status (mar= 1 if married, 0 otherwise)
# - student's number of children five years old or younger (kid5);
# - a rating of the prestige of the student's PhD department (phd);
# - number of articles published by the student's mentor during the three-year period (ment)

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

# Make sure your data are in the correct format
str(long_data)
summary(long_data)

# fem and mar are binary — convert to factor
long_data$fem <- factor(long_data$fem, levels = c(0, 1), labels = c("Male", "Female"))
long_data$mar <- factor(long_data$mar, levels = c(0, 1), labels = c("Not Married", "Married"))

###############################################################################
# (a) Examine the distribution of the response variable.
# Does least-squares linear regression appear a promising strategy?
# Do we meet OLS assumptions?
###############################################################################

# Distribution of art (count of articles published)
summary(long_data$art)
table(long_data$art)

# Histogram
ggplot(long_data, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Articles Published",
       x = "Number of Articles", y = "Count") +
  theme_minimal()

# The distribution is right-skewed with many zeros — typical of count data.
# OLS is problematic because:
# 1. The response is a non-negative integer count, not continuous
# 2. The distribution is right-skewed (not normal)
# 3. OLS could predict negative values

# Fit OLS to check assumptions formally
ols_model <- lm(art ~ fem + mar + kid5 + phd + ment, data = long_data)
summary(ols_model)

# Diagnostic plots (one at a time to avoid Positron graphics state issues)
plot(ols_model, which = 1)  # Residuals vs Fitted
plot(ols_model, which = 2)  # Q-Q
plot(ols_model, which = 3)  # Scale-Location
plot(ols_model, which = 5)  # Residuals vs Leverage

# Residuals vs Fitted: non-constant variance (fan shape) — violates homoscedasticity
# Q-Q plot: heavy right tail — residuals not normally distributed
# Conclusion: OLS assumptions are NOT met. A count model (Poisson) is more appropriate.

###############################################################################
# (b) Perform a Poisson regression
###############################################################################

poisson_model <- glm(art ~ fem + mar + kid5 + phd + ment,
                     data = long_data,
                     family = poisson(link = "log"))
summary(poisson_model)

# Exponentiate coefficients to get incidence rate ratios (IRRs)
exp(coef(poisson_model))
# with 95% CIs
exp(confint(poisson_model))

# Interpretation of coefficients (as IRRs):
# fem (Female): Being female is associated with a decrease in the expected
#   article count relative to males (IRR < 1), holding other variables constant.
# mar (Married): Being married is associated with an increase in the expected
#   article count (IRR > 1).
# kid5: Each additional young child is associated with a decrease in expected
#   article count.
# phd: Higher department prestige rating is associated with a slight increase
#   in expected article count.
# ment: Each additional article by the mentor is associated with an increase
#   in expected article count.

# Check Poisson assumption: mean = variance (equidispersion)
# Residual deviance / df should be approximately 1
deviance(poisson_model) / poisson_model$df.residual
# If >> 1, we have over-dispersion

# Predicted number of articles for a married male PhD researcher
# with 1 child at 2-rated institute whose PhD supervisor published 5 articles
new_data <- data.frame(
  fem = factor("Male", levels = c("Male", "Female")),
  mar = factor("Married", levels = c("Not Married", "Married")),
  kid5 = 1,
  phd = 2,
  ment = 5
)
predict(poisson_model, newdata = new_data, type = "response")

# Plot predictions vs actual count
long_data$predicted <- predict(poisson_model, type = "response")

ggplot(long_data, aes(x = predicted, y = art)) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0.1, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Article Count (Poisson)",
       x = "Predicted Count", y = "Observed Count") +
  theme_minimal()

# Pseudo R-squared (McFadden)
null_model <- glm(art ~ 1, data = long_data, family = poisson(link = "log"))
pseudo_r2 <- 1 - (logLik(poisson_model) / logLik(null_model))
cat("McFadden Pseudo R-squared:", as.numeric(pseudo_r2), "\n")

# RMSE
rmse <- sqrt(mean((long_data$art - long_data$predicted)^2))
cat("RMSE:", rmse, "\n")

# Should we add an interaction for gender with our covariates?
poisson_interact <- glm(art ~ fem * (mar + kid5 + phd + ment),
                        data = long_data,
                        family = poisson(link = "log"))
summary(poisson_interact)

# Compare models with likelihood ratio test
anova(poisson_model, poisson_interact, test = "Chisq")
# If p < 0.05, the interaction model is a significant improvement

###############################################################################
# (c) Over-dispersion
###############################################################################

# Check dispersion parameter
# Residual deviance / df >> 1 suggests over-dispersion
cat("Dispersion estimate:", deviance(poisson_model) / poisson_model$df.residual, "\n")

# Formal test: compare residual deviance to chi-squared distribution
pchisq(deviance(poisson_model), poisson_model$df.residual, lower.tail = FALSE)
# Small p-value indicates over-dispersion

# Cameron & Trivedi (1990) regression-based test via AER package
# H0: equidispersion (Var = mean), H1: Var = mean + alpha * f(mean)
# Default uses f(mean) = mean (linear), so H1: Var = mean * (1 + alpha)
dispersiontest(poisson_model)
# trafo = 2 tests the alternative H1: Var = mean + alpha * mean^2 (NB2 form)
dispersiontest(poisson_model, trafo = 2)
# If p < 0.05, reject equidispersion — evidence of over-dispersion

# Fit quasi-Poisson to account for over-dispersion
quasi_poisson <- glm(art ~ fem + mar + kid5 + phd + ment,
                     data = long_data,
                     family = quasipoisson(link = "log"))
summary(quasi_poisson)

# Compare Poisson vs quasi-Poisson
# Point estimates are identical — only SEs change (inflated by sqrt(dispersion))
# The dispersion parameter is estimated from the data
cat("Estimated dispersion parameter:", summary(quasi_poisson)$dispersion, "\n")

# Side-by-side comparison
stargazer(poisson_model, quasi_poisson,
          type = "text",
          title = "Poisson vs Quasi-Poisson",
          column.labels = c("Poisson", "Quasi-Poisson"))

# Conclusion: If dispersion >> 1, the Poisson SEs are too small (anti-conservative).
# The quasi-Poisson corrects this by scaling SEs. Coefficients stay the same,
# but some previously-significant effects may lose significance with the
# larger standard errors.

###############################################################################
# Zero-Inflated Poisson (ZIP)
###############################################################################

# The data has excess zeros (students who published nothing). A ZIP model
# handles this by combining two processes:
#   1. A logistic model for whether someone is a "certain zero" (never publishes)
#   2. A Poisson model for the count among those who could publish
# This can be a better fit than standard Poisson when over-dispersion is
# driven by excess zeros rather than general variance inflation.

# How many zeros?
cat("Zero counts:", sum(long_data$art == 0),
    "out of", nrow(long_data),
    paste0("(", round(100 * mean(long_data$art == 0), 1), "%)\n"))

# Fit ZIP — same predictors in both the count and zero-inflation parts
zip_model <- zeroinfl(art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment,
                      data = long_data,
                      dist = "poisson")
summary(zip_model)

# The output has two sets of coefficients:
# "Count model" — Poisson part: log(expected count) among potential publishers
# "Zero-inflation model" — logit part: log-odds of being a certain zero
#
# Positive zero-inflation coefficients increase the probability of excess zeros.
# E.g. if fem is positive in the zero part, women are more likely to be
# "certain zeros" beyond what the Poisson model alone would predict.

# Compare Poisson vs ZIP using Vuong test
# H0: both models fit equally well
# Positive test statistic favours ZIP; negative favours standard Poisson
vuong(zip_model, poisson_model)

# Compare all three models' predicted zeros vs observed
cat("Observed zeros:", sum(long_data$art == 0), "\n")
cat("Poisson predicted zeros:", round(sum(dpois(0, fitted(poisson_model)))), "\n")
cat("ZIP predicted zeros:", round(sum(predict(zip_model, type = "prob")[, 1])), "\n")
