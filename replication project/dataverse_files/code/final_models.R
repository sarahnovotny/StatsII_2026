## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
# if using Rmd file, run the lines below to install rmarkdown if needed,
# if using the R file, you can comment them out
if (!("rmarkdown" %in% installed.packages()[, "Package"])) {
  install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
}

# note: root.dir (for Rmd file) or setwd (for R file) should set the directory to the path where the
# README_replication.md file is located on your computer, please set it accordingly.
# If you use the Rmd file, make sure to open it in the accompanying Rproj environment so that paths 
# can be set correctly.

# for Rmd file
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, root.dir = "../")
# for R file, uncomment and run the line below instead of the above
# setwd("../")


## ----install_packages------------------------------------------------------------------------------------------------------
# install necessary packages if not already installed
packages_to_load <- c("labelled", "estimatr", "rlang", "texreg", "scales", 
                      "mice", "tableone",  "broom", "mediation", "gt", "tidyverse")
packages_to_install <- packages_to_load[!(packages_to_load %in% installed.packages()[, "Package"])]
if (length(packages_to_install)) install.packages(packages_to_install)


## ----load_libraries, include = FALSE---------------------------------------------------------------------------------------
# load libraries
library(labelled)    # for dealing with labels
library(estimatr)    # for cluster robust regression
library(rlang)       # for functions
library(texreg)      # for beautiful table output
library(scales)      # for plot labels and scales
library(tidyverse)   # for data wrangling


## ----source_function_definitions-------------------------------------------------------------------------------------------
# load functions for data cleaning and analysis
source(file = "code/functions.R")


## --------------------------------------------------------------------------------------------------------------------------
# data based on listwise deletion of missing values
df.experiment_long <- read_csv("data/intermediate/survey/df.experiment_long.csv", 
                               show_col_types = FALSE)

# imputed data
for (i in 1:5) {
  eval(parse(text = paste0('imputations', i, 
                           ' <- read_csv("data/intermediate/survey/imputations', i, '.csv", show_col_types = FALSE, name_repair = "minimal") %>% dplyr::select(-1)')))
}
imputations <- list(imp1 = imputations1, imp2 = imputations2, imp3 = imputations3,
                    imp4 = imputations4, imp5 = imputations5)


## --------------------------------------------------------------------------------------------------------------------------
# data based on listwise deletion of missing values
df.experiment_long_robustnesscheck <- read_csv("data/intermediate/survey/df.experiment_long_robustnesscheck.csv", 
                                               show_col_types = FALSE)

# imputed data
for (i in 1:5) {
  eval(parse(text = paste0('imputations_robustnesscheck', i, 
                           ' <- read_csv("data/intermediate/survey/imputations_robustnesscheck', i, '.csv", show_col_types = FALSE, name_repair = "minimal") %>% dplyr::select(-1)')))
}
imputations_robustnesscheck <- list(imp1 = imputations_robustnesscheck1, 
                                    imp2 = imputations_robustnesscheck2, 
                                    imp3 = imputations_robustnesscheck3,
                                    imp4 = imputations_robustnesscheck4, 
                                    imp5 = imputations_robustnesscheck5)


## --------------------------------------------------------------------------------------------------------------------------
# Function reapply_labels
# Reapplies labels to all imputed datasets.
# params: data: dataset to which labels should be reapplied
# returns: dataset with reapplied labels
reapply_labels <- function(data) {
  data <- data %>%
    # make participant ID character
    mutate(id = as.character(id)) %>% 
    # set value labels
    set_value_labels(hispanic = c(No = 0,
                                  Yes = 1),
                     education = c(`No high school diploma or GED` = 1,
                                   `High school graduate` = 2,
                                   `Some college or Associate degree` = 3,
                                   `Bachelor's degree` = 4,
                                   `Master's degree or above` = 5),
                     gender = c(Male = 0, Female = 1),
                     aligned = c(No = 0, Yes = 1), 
                     remove = c(`Allow it to remain on the social media site` = 0, `Remove it from the social media site` = 1),
                     harm = c(No = 0, Yes = 1),
                     censorship = c(No = 0, Yes = 1),
                     accuracy_order = c(`Accuracy question second` = 0, `Accuracy question first` = 1),
                     accuracy_binary = c(`Not at all accurate or Not very accurate` = 0, `Somewhat accurate or Very accurate` = 1),
                     accuracy = c(`Not at all accurate` = 1,
                                  `Not very accurate` = 2,
                                  `Somewhat accurate` = 3,
                                  `Very accurate` = 4), 
                     social_media_most_common_newsformat = c(No = 0, Yes = 1),
                     social_media_post_flagged = c(No = 0, Yes = 1),
                     social_media_post_removed = c(No = 0, Yes = 1),
                     household_income = c(`Under $10,000` = 1,
                                          `$10,000 to $24,999` = 2,
                                          `$25,000 to $49,999` = 3,
                                          `$50,000 to $74,999` = 4,
                                          `$75,000 to $99,999` = 5,
                                          `$100,000 to $149,999` = 6,
                                          `$150,000 or more` = 7), 
                     race = c(White = "White", `Non-White` = "Non-White")) %>% 
    mutate(race = factor(race, 
                         labels = c("White", "Non-White"), 
                         levels = c("White", "Non-White")),
           headline = factor(headline, 
                             labels = c("headline_pro_dem_1", "headline_pro_dem_2", "headline_pro_dem_3",
                                        "headline_pro_dem_4", "headline_pro_dem_5", "headline_pro_dem_6", 
                                        "headline_pro_dem_7", "headline_pro_dem_8", "headline_pro_dem_9",
                                        "headline_pro_rep_1", "headline_pro_rep_2", "headline_pro_rep_3",
                                        "headline_pro_rep_4", "headline_pro_rep_5", "headline_pro_rep_6",
                                        "headline_pro_rep_7", "headline_pro_rep_8", "headline_pro_rep_9"), 
                             levels = c("headline_pro_dem_1", "headline_pro_dem_2", "headline_pro_dem_3",
                                        "headline_pro_dem_4", "headline_pro_dem_5", "headline_pro_dem_6",
                                        "headline_pro_dem_7", "headline_pro_dem_8", "headline_pro_dem_9",
                                        "headline_pro_rep_1", "headline_pro_rep_2", "headline_pro_rep_3",
                                        "headline_pro_rep_4", "headline_pro_rep_5", "headline_pro_rep_6",
                                        "headline_pro_rep_7", "headline_pro_rep_8", "headline_pro_rep_9")))

  return(data)
}

# apply function
df.experiment_long <- reapply_labels(df.experiment_long)
imputations <- map(imputations, reapply_labels)

df.experiment_long_robustnesscheck <- reapply_labels(df.experiment_long_robustnesscheck)
imputations_robustnesscheck <- map(imputations_robustnesscheck, reapply_labels)


## --------------------------------------------------------------------------------------------------------------------------
vec.controls <- c("age", "gender", "education", "hispanic", "race", 
                  "household_income", "political_interest", 
                  "social_media_most_common_newsformat", 
                  "social_media_post_flagged", "social_media_post_removed")


## --------------------------------------------------------------------------------------------------------------------------
list.coef_map <- list("(Intercept)" = "Intercept",
                      "party_idDemocrat" = "Democrat",
                      "party_idRepublican" = "Republican",
                      "party_id_dem:headline_pro_dem" = "Democrat x Pro-Democrat Headline",
                      "party_id_rep:headline_pro_rep" = "Republican x Pro-Republican Headline",
                      "aligned" = "Aligned",
                      "accuracy" = "Accuracy",
                      "age" = "Age",
                      "gender" = "Gender: Female",
                      "education" = "Education",
                      "hispanic" = "Hispanic",
                      "raceNon-White" = "Race: Non-White",
                      "household_income" = "Household Income",
                      "political_interest" = "Political Interest",
                      "social_media_most_common_newsformat" = "Social Media Most Common News Format",
                      "social_media_post_flagged" = "Social Media Post Flagged",
                      "social_media_post_removed" = "Social Media Post Removed")


## --------------------------------------------------------------------------------------------------------------------------
list.coef_map_byheadline <- list("(Intercept)" = "Intercept",
                                 "party_idDemocrat" = "Democrat Respondent",
                                 "party_idRepublican" = "Republican Respondent",
                                 "party_id_dem:headline_pro_dem" = "Democrat x Pro-Democrat Headline",
                                 "party_id_rep:headline_pro_rep" = "Republican x Pro-Republican Headline",
                                 "aligned" = "Aligned",
                                 "accuracy" = "Accuracy",
                                 "age" = "Age",
                                 "gender" = "Gender: Female",
                                 "education" = "Education",
                                 "hispanic" = "Hispanic",
                                 "raceNon-White" = "Race: Non-White",
                                 "household_income" = "Household Income",
                                 "political_interest" = "Political Interest",
                                 "social_media_most_common_newsformat" = "Social Media Most Common News Format",
                                 "social_media_post_flagged" = "Social Media Post Flagged",
                                 "social_media_post_removed" = "Social Media Post Removed")


## --------------------------------------------------------------------------------------------------------------------------
list.coef_map_tripleinteraction <- list("party_idDemocrat" = "Democrat",
                                        "party_idRepublican" = "Republican",
                                        "party_id_dem:headline_pro_dem" = "Democrat x Pro-Democrat Headline",
                                        "party_id_rep:headline_pro_rep" = "Republican x Pro-Republican Headline",
                                        "accuracy_order" = "Accuracy Question First",
                                        "party_id_dem:headline_pro_dem:accuracy_order" = "Accuracy Question First x Democrat x Pro-Democrat Headline",
                                        "accuracy_order:party_id_rep:headline_pro_rep" = "Accuracy Question First x Republican x Pro-Republican Headline")


## --------------------------------------------------------------------------------------------------------------------------
# generate all potential model specifications for main dataframe
# model name will follow the schema
# mod.[outcome]_[model]_[control_vars]_[ses]_[weighting]_[accuracy_order]_[accuracy_subgroup]_[headlines]_[nas]
df.specifications <- expand.grid(outcome = c("remove", "harm", "censorship"), # outcomes to consider
                                 model = c("standard"), # type of model
                                 control_vars = c("nocontrols", "controls"), # whether model should include control variables
                                 ses = c("clusteredses", "standardses"), # whether standard errors should be clustered on participant
                                 weighting = c("weighted", "unweighted"), # whether observations should be weighted
                                 accuracy_order = c("anyaccorder", "firstaccorder", "secondaccorder"), # accuracy question order
                                 accuracy_subgroup = c("anysubgroup", "inaccsubgroup"), # accuracy rating subset to consider
                                 headlines = c("anyheadline", "firstheadline"), # set of headlines to consider
                                 nas = c("ld", "mi")) # missing values approach (listwise deletion or multiple imputation)

generate_regression_models(specification_data = df.specifications, 
                           experiment_data =  c("df.experiment_long", "imputations"))


## --------------------------------------------------------------------------------------------------------------------------
# generate all potential model specifications for dataframe with pre-registered coding for censorship
# model name will follow the schema
# mod.[outcome]_[model]_[control_vars]_[ses]_[weighting]_[accuracy_order]_[accuracy_subgroup]_[headlines]_[nas]_robustnesscheck
df.specifications_robustnesscheck <- expand.grid(outcome = c("censorship"), # outcomes to consider
                                                 model = c("standard"), # type of model
                                                 control_vars = c("nocontrols", "controls"), # whether model should include control variables
                                                 ses = c("clusteredses"), # whether standard errors should be clustered on participant
                                                 weighting = c("weighted"), # whether observations should be weighted
                                                 accuracy_order = c("anyaccorder"), # accuracy question order
                                                 accuracy_subgroup = c("anysubgroup", "inaccsubgroup"), # accuracy rating subset to consider
                                                 headlines = c("anyheadline"), # set of headlines to consider
                                                 nas = c("ld", "mi")) # missing values approach (listwise deletion or multiple imputation)

generate_regression_models(specification_data = df.specifications_robustnesscheck, 
                           experiment_data =  c("df.experiment_long_robustnesscheck", "imputations_robustnesscheck"),
                           robustness_check = TRUE)


## --------------------------------------------------------------------------------------------------------------------------
# add accuracy binary model
mod.accuracy_binary_by_party_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld <- 
  lm_robust(data = df.experiment_long,
            formula = accuracy_binary ~ 0 +  party_id_dem:headline_pro_dem + party_id_rep:headline_pro_rep + party_id,
            cluster = id,
            weights = weight)


## --------------------------------------------------------------------------------------------------------------------------
# models without interaction
for (outcome in c("remove", "harm", "censorship")) {
  for (subset in c("anysubgroup", "inaccsubgroup")) {
      model_name <- paste0("mod.", outcome, "_", subset, "_clusteredses_weighted_ld_without_interaction")
      subset_option <- if (subset == "anysubgroup") { NULL } else { "accuracy_binary == 0" }
      assign(model_name,
                 base::eval(parse(text = paste0("lm_robust(data = df.experiment_long", 
                                                ", formula = ", outcome, " ~ 0 + party_id",
                                                ", cluster = id",
                                                ", weights = weight",
                                                ", subset = ", subset_option, ")"))))
  }
} 


## --------------------------------------------------------------------------------------------------------------------------
# models that disaggregate results by headline
headlines <- levels(df.experiment_long$headline)

for (outcome in c("remove", "harm", "censorship")) {
  for (subset in c("anysubgroup", "inaccsubgroup")) {
    for (headline in headlines) {
      model_name <- paste0("mod.", outcome, "_", subset, "_clusteredses_weighted_ld_", headline)
      subset_option <- if (subset == "anysubgroup") { NULL } else { "accuracy_binary == 0" }
      assign(model_name,
                 base::eval(parse(text = paste0("lm_robust(data = df.experiment_long %>% filter(headline == \"", headline, "\")", 
                                                ", formula = ", outcome, " ~ 0 + party_id",
                                                ", cluster = id",
                                                ", weights = weight",
                                                ", subset = ", subset_option, ")"))))
    }
  }
} 


## --------------------------------------------------------------------------------------------------------------------------
# create dataframe to find consensus headlines
dem_means <- df.experiment_long %>%
  filter(party_id == "Democrat") %>% 
  group_by(headline) %>% 
  summarize(accuracy_mean_dem = mean(accuracy, na.rm = TRUE))

rep_means <- df.experiment_long %>%
  filter(party_id == "Republican") %>% 
  group_by(headline) %>% 
  summarize(accuracy_mean_rep = mean(accuracy, na.rm = TRUE))

df_diff <- left_join(dem_means, rep_means, by = "headline") %>% 
  mutate(mean_diff = accuracy_mean_rep - accuracy_mean_dem,
         mean_diff_abs = abs(accuracy_mean_rep - accuracy_mean_dem)) %>% 
  arrange(mean_diff_abs)

df_consensus_headlines <- df_diff %>% 
  # keep only consensus headlines rated as false by both groups
  filter(accuracy_mean_dem < 2 & accuracy_mean_rep < 2)

for (i in 2:nrow(df_consensus_headlines)) {
  selected_headlines <- df_consensus_headlines %>% 
    slice(1:i) %>% 
    pull(headline)

  df.specifications_selected_headlines <- expand.grid(outcome = c("remove", "harm", "censorship"), # outcomes to consider
                                                      model = c("standard"), # type of model
                                                      control_vars = c("nocontrols"), # whether model should include control variables
                                                      ses = c("clusteredses"), # whether standard errors should be clustered on participant
                                                      weighting = c("weighted"), # whether observations should be weighted
                                                      accuracy_order = c("anyaccorder"), # accuracy question order
                                                      accuracy_subgroup = c("anysubgroup"), # accuracy rating subset to consider
                                                      headlines = c("anyheadline"), # set of headlines to consider (any or first)
                                                      nas = c("ld")) # missing values approach (listwise deletion or multiple imputation)
  
  df_experiment_long_selcted_headlines <- df.experiment_long %>% 
                                            filter(headline %in% selected_headlines)
  
  generate_regression_models(specification_data = df.specifications_selected_headlines, 
                             experiment_data =  c("df_experiment_long_selcted_headlines"),
                             selected_headlines = paste0("selected_headlines_", i))
}


## --------------------------------------------------------------------------------------------------------------------------
# triple interaction models
mod.remove_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld <- 
  lm_robust(data = df.experiment_long,
           formula = remove ~ 0 + party_id_dem:headline_pro_dem*accuracy_order + party_id_rep:headline_pro_rep*accuracy_order + party_id, 
           cluster = id, 
           weights = weight, 
           subset = headline_order == 0)

mod.harm_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld <- 
  lm_robust(data = df.experiment_long,
            formula = harm ~ 0 + party_id_dem:headline_pro_dem * accuracy_order + party_id_rep:headline_pro_rep * accuracy_order + party_id,
            cluster = id,
            weights = weight,
            subset = headline_order == 0)

mod.censorship_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld <- 
  lm_robust(data = df.experiment_long,
            formula = censorship ~ 0 + party_id_dem:headline_pro_dem * accuracy_order + party_id_rep:headline_pro_rep * accuracy_order + party_id,
            cluster = id,
            weights = weight,
            subset = headline_order == 0)


## --------------------------------------------------------------------------------------------------------------------------
# models for descriptive barplot CIs
for (outcome in c("remove", "harm", "censorship")) {
  for (subset in c("anysubgroup", "inaccsubgroup")) {
    for (alignment in c("overall", "aligned", "misaligned")) {
      model_name <- paste0(c(paste0("mod.", outcome), "standard_nocontrols_clusteredses_weighted_anyaccorder",
                           subset, "ld", alignment), collapse = "_")
      subset_option <- if (alignment == "overall") { NULL } else if (alignment == "aligned") { "aligned == 1" } else { "aligned == 0" }
      formula <- paste0(outcome, " ~ 0 + party_id")
      
      assign(model_name,
             base::eval(parse(text = paste0(if_else(subset == "anysubgroup", 
                                                   "lm_robust(data = df.experiment_long",
                                                   "lm_robust(data = df.experiment_long %>% filter(accuracy_binary == 0)"),
                                            ", formula = ", formula,
                                            ", cluster = id",
                                            ", weights = weight",
                                            ", subset = ", subset_option, ")"))))
    }    
  }
}


## ----sessioninfo-----------------------------------------------------------------------------------------------------------
sessionInfo()


## ----include=FALSE, eval=FALSE---------------------------------------------------------------------------------------------
## knitr::purl(input = "code/final_models.Rmd",
##             output = "code/final_models.R",
##             documentation = 1)

