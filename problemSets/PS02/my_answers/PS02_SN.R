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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# Recode choice as binary: 1 = Supported, 0 = Not supported
climateSupport$choice_bin <- ifelse(climateSupport$choice == "Supported", 1, 0)

# Set reference levels (first unorder the factors)
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)
climateSupport$countries <- relevel(climateSupport$countries, ref = "20 of 192")
climateSupport$sanctions <- relevel(climateSupport$sanctions, ref = "None")


model_additive <- glm(
  choice_bin ~ countries + sanctions, 
  binomial(link = logit),
  data = climateSupport 
)

summary(model_additive)

# function for formatting the regression table
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=FALSE)
}

output_stargazer("./regression_output_model_additive.tex", model_additive)


# Null Hypothesis test  (Liklihood Ratio)

# Null model (intercept only)
model_null <- glm(choice_bin ~ 1,
  data = climateSupport,
  binomial(link = logit)
)

anova_output <- anova(model_null, model_additive, test = "LRT")

output_stargazer("./regression_output_anova_output.tex", summary = FALSE, anova_output)

prob_support_80 <- predict(model_additive, 
        newdata = data.frame(countries = "80 of 192", sanctions = "None"), 
        type = "response")
prob_support_80


# Fit interaction model

model_interaction <- glm(choice_bin ~ countries * sanctions,
                         data = climateSupport,
                         family = binomial(link = "logit"))

summary(model_interaction)

output_stargazer("./regression_output_model_interaction.tex", model_interaction)

# LR test: additive vs interaction
anova2_output <- anova(model_additive, model_interaction, test = "LRT")

output_stargazer("./regression_output_anova2_output.tex", summary = FALSE, anova2_output)