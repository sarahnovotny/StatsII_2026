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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# function for formatting the regression table
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=FALSE)
}

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Examine the data
str(gdp_data)
summary(gdp_data)
table(gdp_data$GDPWdiff)

# GDPWdiff is numeric -- create categorical version
gdp_data$GDPWdiff_cat <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                                 ifelse(gdp_data$GDPWdiff < 0, "negative", "no change"))
gdp_data$GDPWdiff_cat <- factor(gdp_data$GDPWdiff_cat,
                                 levels = c("no change", "negative", "positive"))

# Convert REG and OIL to factors for interpretability
gdp_data$REG <- factor(gdp_data$REG, levels = c(0, 1),
                       labels = c("Non-Democracy", "Democracy"))
gdp_data$OIL <- factor(gdp_data$OIL, levels = c(0, 1),
                       labels = c("Not Oil Exporter", "Oil Exporter"))

# (1) Unordered multinomial logit
# "no change" is the reference (base) category
multinom_model <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)
summary(multinom_model)

# Relative risk ratios
rrr <- exp(coef(multinom_model))
rrr

# Save RRR table to tex
rrr_df <- as.data.frame(round(rrr, 4))
rrr_df$Category <- rownames(rrr_df)
output_stargazer("./multinom_rrr.tex", rrr_df, summary = FALSE,
                 title = "Relative Risk Ratios (Unordered Multinomial Logit)")

# Stargazer output (log-odds coefficients)
output_stargazer("./multinom_model.tex", multinom_model)

# (2) Ordered multinomial logit
# Need ordered factor: negative < no change < positive
gdp_data$GDPWdiff_ord <- ordered(gdp_data$GDPWdiff_cat,
                                  levels = c("negative", "no change", "positive"))

ordered_model <- polr(GDPWdiff_ord ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(ordered_model)

# Compute p-values
coef_table <- coef(summary(ordered_model))
p_ordered <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
cbind(coef_table, "p value" = round(p_ordered, 4))

# Confidence intervals
confint(ordered_model)

# Odds ratios
exp(coef(ordered_model))

# Stargazer output
output_stargazer("./ordered_model.tex", ordered_model)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

# Examine the data
str(mexico_elections)
summary(mexico_elections$PAN.visits.06)
table(mexico_elections$PAN.visits.06)

# (a) Poisson regression
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                     data = mexico_elections,
                     family = poisson(link = "log"))
summary(poisson_model)

output_stargazer("./poisson_model.tex", poisson_model)

# Test: do PAN candidates visit swing districts more?
coef_comp <- summary(poisson_model)$coefficients["competitive.district", ]
comp_test <- data.frame(
  Statistic = c("Coefficient", "z-statistic", "p-value", "IRR"),
  Value = c(coef_comp["Estimate"], coef_comp["z value"],
            coef_comp["Pr(>|z|)"], exp(coef_comp["Estimate"]))
)
output_stargazer("./competitive_test.tex", comp_test, summary = FALSE,
                 title = "Test for Competitive District Effect")

# (b) Incidence rate ratios for all coefficients
irr_df <- data.frame(
  IRR = round(exp(coef(poisson_model)), 4)
)
output_stargazer("./poisson_irr.tex", irr_df, summary = FALSE,
                 title = "Incidence Rate Ratios (Poisson)")

# (c) Predicted number of visits for specified scenario
new_district <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1
)
predicted_visits <- predict(poisson_model, newdata = new_district, type = "response")
pred_df <- data.frame(
  Scenario = "competitive=1, marginality=0, PAN.governor=1",
  Predicted.Visits = round(predicted_visits, 4)
)
output_stargazer("./predicted_visits.tex", pred_df, summary = FALSE,
                 title = "Predicted Number of PAN Visits")
