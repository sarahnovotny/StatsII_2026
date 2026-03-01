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
packages_to_load <- c("knitr", "haven", "expss", "sjlabelled",  "labelled",  
                      "estimatr", "Amelia", "rlang", "lavaan", "rjson", "janitor",
                      "mice", "tableone",  "broom", "mediation", "gt", "tidyverse")
packages_to_install <- packages_to_load[!(packages_to_load %in% installed.packages()[, "Package"])]
if (length(packages_to_install)) install.packages(packages_to_install)


## ----load_libraries, include = FALSE---------------------------------------------------------------------------------------
# load libraries
library(knitr)       # for formatting
library(haven)       # for handling SAV files
library(expss)       # for dealing with labelled data
library(sjlabelled)  # for dealing with labelled data
library(labelled)    # for dealing with labelled data
library(estimatr)    # for cluster robust regression
library(Amelia)      # for multiple imputation
library(lavaan)      # for factor analysis
library(rlang)       # for functions
library(rjson)       # for reading JSON files
library(tidyverse)   # for data wrangling


## --------------------------------------------------------------------------------------------------------------------------
# the following lines using the raw survey data will not run because we do not include the raw data 
# (the data source is Knight Foundation-Ipsos (2022) "Free Expression in America Post-2020" [Dataset], 
# and all intermediate data needed to reproduce tables and figures are provided and loaded in 
# the manuscript_code scripts)
df.experiment <- read_sav("data/input/survey/Knight Foundation_FX_SPSS_PLUS SCHOLAR DATA_121421.sav")


## --------------------------------------------------------------------------------------------------------------------------
# create subset of data with only our variables
df.experiment_clean <- df.experiment %>% 
  # select subset of variables
  dplyr::select(Censorship_Accuracy_Group_order, img_dem_rep_1, img_dem_rep_2, Q34_1:Q34_2, 
                Q35_1:Q35_4, Q45_1:Q49_4, Q50A:Q57A, QPID100, ppage:RACEETH, Q260, Main_Weights) %>% 
  # remove unnecessary variables
  dplyr::select(-c(
    Q48, # news media variable with only NAs
    ppeducat, # education variable that is less detailed than another
    ppethm, # race ethnicity (both race and ethnicity are included separately)
    pphouse4, # type of house lived in       
    pprent, # ownership status of living quarters 
    ppt18ov, # total number of HH members >= 18 
    xhhno, # household member ID number
    xknight, # survey target group
    ppmsacat, # metro area status (less detailed than urban rural city)
    RACEETH)) %>% # another race variable is already included
  # keep only participants in our experiment who saw our outcome variables
  filter(!is.na(Q50A)) %>% 
  # filter student observations that do not belong to overall sample
  filter(!is.na(Main_Weights)) %>%
  # remove participants that indicate proficiency in Spanish only
  filter(xacslang != 3) %>% 
  dplyr::select(-xacslang) %>% 
  # replace all missing values coded as -1 with NA
  mutate(across(everything(), ~na_if(., -1))) %>% 
  # remove participants for whom no survey language was indicated
  filter(!is.na(xspanish)) %>% 
  dplyr::select(-xspanish) %>% 
  # recode hispanic variable
  mutate(hispanic = case_when(xhispan == 1 ~ 0,
                              xhispan == 2 | xhispan == 3 | xhispan == 4 | xhispan == 8 ~ 1,
                              TRUE ~ NA_real_)) %>% 
  dplyr::select(-xhispan) %>% 
  # rename key variables
  rename(accuracy_order = Censorship_Accuracy_Group_order,
         headline_1_remove = Q50A, 
         headline_1_censorship = Q51A, 
         headline_1_harm = Q53A, 
         headline_1_accuracy = Q52A,
         headline_2_remove = Q54A, 
         headline_2_censorship = Q55A, 
         headline_2_harm = Q57A, 
         headline_2_accuracy = Q56A,
         social_media_news_post_freq = Q34_1,
         social_media_news_discussion_freq = Q34_2,
         social_media_post_removed = Q35_1,
         social_media_post_flagged = Q35_2,
         social_media_penalized_at_work_for_post = Q35_3,
         social_media_selfcensorship = Q35_4,
         trust_news_media_unbiased = Q45_1,
         trust_news_media = Q45_2,
         trust_social_media_companies = Q45_3,
         trust_government = Q45_4,
         trust_university_admins = Q45_5,
         trust_high_school_admins = Q45_6,
         news_media_use_freq_newspaper = Q46_1,
         news_media_use_freq_tv = Q46_2,
         news_media_use_freq_app = Q46_3,
         news_media_use_freq_radio = Q46_4,
         news_media_use_freq_magazine = Q46_5,
         news_media_use_freq_direct_communication = Q46_6,
         news_format_most_common = Q47,
         news_consumption_national_politics = Q49_1,
         news_consumption_state_politics = Q49_2,
         news_consumption_local_politics = Q49_3,
         news_consumption_international_politics = Q49_4,
         age = ppage,
         employment_status = ppemploy,
         gender = ppgender,
         education = ppeduc5,
         household_size = pphhsize,
         household_income = ppinc7,
         marital_status = ppmarit5,
         census_region = ppreg4,
         state = ppstaten,
         children_count = ppkid017,
         rural_urban_status = xurbanicity,
         race = xppracem,
         religion = Q260,
         weight = Main_Weights) %>% 
  # recode party ID
  mutate(party_id = sjlabelled::as_label(QPID100)) %>% 
  # remove old party ID variable
  dplyr::select(-QPID100) %>% 
  # exclude independents and those who didn't indicate partisanship
  filter(party_id %in% c("Democrat", "Republican")) %>% 
  # add binary variables for Democrat / Republican
  mutate(party_id_dem = if_else(party_id == "Democrat",
                                1,
                                0),
         party_id_rep = if_else(party_id == "Republican",
                                1,
                                0)) %>% 
  # add id column for participants
  rowid_to_column("id")


## --------------------------------------------------------------------------------------------------------------------------
## specify headline_1 and headline_2 variables to indicate headline orientation
df.experiment_clean <- df.experiment_clean %>% 
  # recode headlines
  mutate_at(vars(img_dem_rep_1, img_dem_rep_2),
            funs(case_when(
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_erGziE6XnUAT4pw" ~ "headline_pro_dem_1",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_5jQtyIBrkKERNzM" ~ "headline_pro_dem_2",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_ePVUruXfmMbkJhA" ~ "headline_pro_dem_3",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_0IpLlAeIyHZodUi" ~ "headline_pro_dem_4",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_6Qmzc7eol5vTvMi" ~ "headline_pro_dem_5",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_dcCLc8Ga55AaYzc" ~ "headline_pro_dem_6",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_8Dnr3Surzxk3pP0" ~ "headline_pro_dem_7",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_4MDYXXlUJJxc1E2" ~ "headline_pro_dem_8",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3OfNbLLsN2NzFCS" ~ "headline_pro_dem_9",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3gVnSn1U4mwhEjk" ~ "headline_pro_rep_1",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_2ox2dINPLXixYcS" ~ "headline_pro_rep_2",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_0Gth87cjXjTBqHc" ~ "headline_pro_rep_3",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_1LYlSNANFhccgjI" ~ "headline_pro_rep_4",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_1yJzElXjQmL4PzM" ~ "headline_pro_rep_5",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3n52QFm7tKt1jJc" ~ "headline_pro_rep_6",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_8v8TRmPUUWobIa2" ~ "headline_pro_rep_7",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_eFBTC9BblzOu95s" ~ "headline_pro_rep_8",
              . == "https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_a5lNSP8WRWOY2A6" ~ "headline_pro_rep_9",
              TRUE ~ NA_character_))) %>%
  rename(headline_1 = img_dem_rep_1, headline_2 = img_dem_rep_2)


## --------------------------------------------------------------------------------------------------------------------------
df.political_interest <- df.experiment_clean %>% 
  dplyr::select(contains("news_consumption_")) %>% 
  na.omit(.)


## --------------------------------------------------------------------------------------------------------------------------
mod.lavaan <- " f  =~ news_consumption_national_politics + news_consumption_state_politics + news_consumption_local_politics + news_consumption_international_politics"
output_lavaan <- lavaan::cfa(mod.lavaan, data = df.political_interest) 
summary(output_lavaan) 


## --------------------------------------------------------------------------------------------------------------------------
df.experiment_long <- df.experiment_clean %>%
  # pivot longer for headlines
  pivot_longer(cols = matches("headline_[12]_"),
               names_to = c("headline_order", "variable"),
               values_to = "rating",
               names_pattern = "headline_([12])_(.+)") %>% 
  pivot_wider(names_from = variable,
              values_from = rating) %>% 
  # turn headline order into numeric variable
  mutate(headline_order = as.integer(headline_order)) %>% 
  # combine headline_1 and headline_2 variables
  mutate(headline = if_else(headline_order == 1,
                            headline_1,
                            headline_2)) %>% 
  dplyr::select(-headline_1, -headline_2) %>% 
  # add headline orientation
  mutate(headline_orientation = str_match(headline, "headline_(.*)_.")[,2]) %>% 
  mutate(headline_pro_dem = if_else(headline_orientation == "pro_dem",
                                    1,
                                    0),
         headline_pro_rep = if_else(headline_orientation == "pro_rep",
                                    1,
                                    0)) %>% 
  # add binary accuracy variable for accurate and inaccurate subgroup analysis
  mutate(accuracy_binary = case_when(accuracy == 1 | accuracy == 2 ~ 0,
                                     accuracy == 3 | accuracy == 4 ~ 1,
                                     TRUE ~ NA_real_)) %>% 
  # reapply correct variable labels that were changed during reshaping
  var_labels(accuracy = !!str_match(get_label(df.experiment_clean$headline_1_accuracy), "(.*) \\[Field-Q52_order\\]")[2],
             remove = !!str_match(get_label(df.experiment_clean$headline_1_remove), "(.*) \\[Field-Q50_order\\]")[2],
             harm = !!str_match(get_label(df.experiment_clean$headline_1_harm), "(.*) \\[Field-Q53_order\\]")[2],
             censorship = !!str_match(get_label(df.experiment_clean$headline_1_censorship), "(.*) \\[Field-Q51_order\\]")[2]) %>%
  # reapply correct value labels that were changed during reshaping
  mutate(accuracy = set_labels(accuracy, labels = get_labels(df.experiment_clean$headline_1_accuracy)),
         remove = set_labels(remove, labels = get_labels(df.experiment_clean$headline_1_remove)),
         harm = set_labels(harm, labels = get_labels(df.experiment_clean$headline_1_harm)),
         censorship = set_labels(censorship, labels = get_labels(df.experiment_clean$headline_1_censorship)))


## --------------------------------------------------------------------------------------------------------------------------
# write function to recode and relabel variables to apply to dataframes
recode_and_relabel_data <- function(data) {
  data <- data %>% 
    mutate(
      # from 1, 2 (2 means accuracy questions came first) --> 0, 1
      accuracy_order = dplyr::recode(
        accuracy_order,
        `1` = 0, `2` = 1),
      # from 1 first headline, 2 second headline --> 0 first headline, 1 second headline
      headline_order = dplyr::recode(
        headline_order,
        `1` = 0, `2` = 1),    
      # from 1 allow, 2 remove --> 0 allow, 1 remove
      remove = dplyr::recode(
        remove,
        `1` = 0, `2` = 1,
        .combine_value_labels = TRUE), 
      # from 1 is Yes, 2 is No --> 0 is No, 1 is Yes
      harm = dplyr::recode(
        harm,
        `2` = 0,
        .combine_value_labels = TRUE),
      # from 1 Often to 4 Never --> 4 Often to 1 Never
      social_media_news_post_freq = dplyr::recode(
        social_media_news_post_freq,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      social_media_news_discussion_freq = dplyr::recode(
        social_media_news_discussion_freq,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      # from 1 Yes, 2 No, 3 Not Applicable --> 3 NA, 0 No, 1 Yes
      social_media_post_removed = dplyr::recode(
        social_media_post_removed,
        `2` = 0,
        .combine_value_labels = TRUE), 
      social_media_post_flagged = dplyr::recode(
        social_media_post_flagged,
        `2` = 0,
        .combine_value_labels = TRUE), 
      social_media_penalized_at_work_for_post = dplyr::recode(
        social_media_penalized_at_work_for_post,
        `2` = 0,
        .combine_value_labels = TRUE),
      social_media_selfcensorship = dplyr::recode(
        social_media_selfcensorship,
        `2` = 0,
        .combine_value_labels = TRUE), 
      # from 1 Great deal to 4 Not at all, 5 "No opinion" --> 1 Not at all to 4 Great deal, 5 NA
      trust_news_media_unbiased = dplyr::recode(
        trust_news_media_unbiased,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1),
      # manually set correct labels because .combine_value_labels does not work properly
      trust_news_media_unbiased = set_labels(trust_news_media_unbiased, labels = c("Not at all", "Not much", "Fair amount", "Great deal", "No opinion")),
      trust_news_media = dplyr::recode(
        trust_news_media,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1),
      # manually set correct labels because .combine_value_labels does not work properly
      trust_news_media = set_labels(trust_news_media, labels = c("Not at all", "Not much", "Fair amount", "Great deal", "No opinion")),
      trust_social_media_companies = dplyr::recode(
        trust_social_media_companies,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1),
      # manually set correct labels because .combine_value_labels does not work properly
      trust_social_media_companies = set_labels(trust_social_media_companies, labels = c("Not at all", "Not much", "Fair amount", "Great deal", "No opinion")),
      trust_government = dplyr::recode(
        trust_government,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      trust_university_admins = dplyr::recode(
        trust_university_admins,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1),
      # manually set correct labels because .combine_value_labels does not work properly
      trust_university_admins = set_labels(trust_university_admins, labels = c("Not at all", "Not much", "Fair amount", "Great deal", "No opinion")),
      trust_high_school_admins = dplyr::recode(
        trust_high_school_admins,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      # from 1 Daily to 5 Never --> 1 Never to 5 Daily
      news_media_use_freq_newspaper = dplyr::recode(
        news_media_use_freq_newspaper,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE), 
      news_media_use_freq_tv = dplyr::recode(
        news_media_use_freq_tv,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE), 
      news_media_use_freq_app = dplyr::recode(
        news_media_use_freq_app,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE), 
      news_media_use_freq_radio = dplyr::recode(
        news_media_use_freq_radio,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE), 
      news_media_use_freq_magazine = dplyr::recode(
        news_media_use_freq_magazine,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE), 
      news_media_use_freq_direct_communication = dplyr::recode(
        news_media_use_freq_direct_communication,
        `1` = 5, `2` = 4, `4` = 2, `5` = 1,
        .combine_value_labels = TRUE),
      # from 1 Very closely to 4 Not at all closely --> 4 Very closely to 1 Not at all closely
      news_consumption_national_politics = dplyr::recode(
        news_consumption_national_politics,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      news_consumption_state_politics = dplyr::recode(
        news_consumption_state_politics,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      news_consumption_local_politics = dplyr::recode(
        news_consumption_local_politics,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE), 
      news_consumption_international_politics = dplyr::recode(
        news_consumption_international_politics,
        `1` = 4, `2` = 3, `3` = 2, `4` = 1,
        .combine_value_labels = TRUE),
      # from 1 full time, 2 part-time, 3 not working --> 1 not working, 2 part-time, 3 full time
      employment_status = dplyr::recode(
        employment_status,
        `1` = 3, `3` = 1,
        .combine_value_labels = TRUE), 
      # from 1 Male, 2 Female --> 0 Male, 1 Female
      gender = dplyr::recode(
        gender,
        `1` = 0, `2` = 1,
        .combine_value_labels = TRUE)) %>% 
    # set missing values
    set_na(censorship, na = "Don’t know") %>% 
    set_na(social_media_news_post_freq, na = "No opinion") %>% 
    set_na(social_media_news_discussion_freq, na = "No opinion") %>%
    set_na(trust_news_media_unbiased, na = "No opinion") %>% 
    set_na(trust_news_media, na = "No opinion") %>%
    set_na(trust_social_media_companies, na = "No opinion") %>%
    set_na(trust_government, na = "No opinion") %>%
    set_na(trust_university_admins, na = "No opinion") %>%
    set_na(trust_high_school_admins, na = "No opinion") %>%
    set_na(social_media_post_removed, na = "Not applicable") %>%
    set_na(social_media_post_flagged, na = "Not applicable") %>%
    set_na(social_media_penalized_at_work_for_post, na = "Not applicable") %>%
    set_na(social_media_selfcensorship, na = "Not applicable") %>%
    # recode as factor with values equal to labels
    dplyr::mutate_at(c("news_format_most_common", "marital_status", "census_region", 
                       "state", "rural_urban_status", "race", "religion"),
                     sjlabelled::as_label) %>% 
    # change reference level for variables
    dplyr::mutate(marital_status = relevel(marital_status, ref = "Never married")) %>% 
    dplyr::mutate(religion = relevel(religion, ref = "No religion")) %>%
    # turn id variable into string
    dplyr::mutate(id = as.character(id)) %>% 
    # add recoded control variables
    mutate(social_media_most_common_newsformat = case_when(news_format_most_common == "From social media" ~ 1,
                                                           news_format_most_common %in% c("From a printed newspaper or magazine", "From television", "From radio", "From social media", "From friends and family") ~ 0,
                                                           TRUE ~ NA_real_),
           political_interest = (news_consumption_national_politics + news_consumption_state_politics + news_consumption_local_politics + news_consumption_international_politics) / 4,
           race = factor(case_when(race == "White" ~ "White",
                                   race == "Black or African American" | race == "American Indian or Alaska Native" | race == "Asian" | race == "Native Hawaiian/Pacific Islander" | race == "2+ races" ~ "Non-White",
                                   TRUE ~ NA_character_), 
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

  # make coding of education variable shorter and avoid encoding issues
  data$education <- set_labels(
    data$education,
    labels = c(`No high school diploma or GED` = 1,
               `High school graduate` = 2,
               `Some college or Associate degree` = 3,
               `Bachelor's degree` = 4,
               `Master's degree or above` = 5))
  
  # add aligned variable
  data <- data %>% 
    mutate(aligned = case_when(party_id_dem == 1 & headline_pro_dem == 1 ~ 1,
                               party_id_rep == 1 & headline_pro_rep == 1 ~ 1,
                               TRUE ~ 0))
  
  # update labels
  data <- data %>%
    # variable labels
    var_labels(id = "Participant ID",
               party_id_dem = "Democrat party ID indicator",
               party_id_rep = "Republican party ID indicator",
               hispanic = "Hispanic ethnicity indicator",
               age = "Age in years",
               accuracy_order = "Order in which accuracy question appeared",
               accuracy_binary = "Indicates whether a headline was deemed more or less accurate",
               headline = "Headline that participants saw",
               headline_pro_dem = "Indicates whether a headline is favorable for Democrats or unfavorable for Republicans",
               headline_pro_rep = "Indicates whether a headline is favorable for Republicans or unfavorable for Democrats",
               headline_orientation = "Whether a headline is pro-Democrat or pro-Republican",
               headline_order = "Whether participants saw a headline in the first or second round",
               aligned = "Indicates whether party ID and headline orientation are aligned (i.e., Democrat party ID and pro-Democrat headline, or Republican party ID and pro-Republican headline)",
               social_media_most_common_newsformat = "Indicates whether \"From social media\" was chosen in response to \"In which format do you get most of your news?\"",
               political_interest = "Political interest. Average of national politics, state politics, local politics, and international politics news consumption, each measured on a 4-point scale from 1 (Not at all closely) to 4 (Very closely)",
               weight = "Survey weight",
               race = "Race") %>%
    # value labels
    set_value_labels(party_id_dem = c(Yes = 1, No = 0),
                     party_id_rep = c(Yes = 1, No = 0),
                     headline_pro_dem = c(Yes = 1, No = 0),
                     headline_pro_rep = c(Yes = 1, No = 0),
                     hispanic = c(Yes = 1, No = 0),
                     accuracy_order = c(`Accuracy question first` = 1,
                                        `Accuracy question second` = 0),
                     social_media_most_common_newsformat = c(Yes = 1, No = 0),
                     aligned = c(Yes = 1, No = 0),
                     accuracy_binary = c(`Not at all accurate or Not very accurate` = 0,
                                         `Somewhat accurate or Very accurate` = 1),
                     headline_order = c(`First headline display` = 0,
                                        `Second headline display` = 1),
                     headline_orientation = c(`pro-Democrat` = "pro_dem",
                                              `pro-Republican` = "pro_rep")) %>%
    # drop unused labels from party ID and set labels
    mutate(party_id = factor(party_id, labels = c("Democrat", "Republican"), levels = c("Democrat", "Republican"))) %>%
    var_labels(party_id = "Party identification")
  
  data$race <- set_labels(data$race, labels = c(White = "White", `Non-White` = "Non-White"))
  
  data$headline <- set_labels(
    data$headline,
    labels = c(`https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_erGziE6XnUAT4pw` = "headline_pro_dem_1",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_5jQtyIBrkKERNzM` = "headline_pro_dem_2",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_ePVUruXfmMbkJhA` = "headline_pro_dem_3",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_0IpLlAeIyHZodUi` = "headline_pro_dem_4",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_6Qmzc7eol5vTvMi` = "headline_pro_dem_5",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_dcCLc8Ga55AaYzc` = "headline_pro_dem_6",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_8Dnr3Surzxk3pP0` = "headline_pro_dem_7",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_4MDYXXlUJJxc1E2` = "headline_pro_dem_8",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3OfNbLLsN2NzFCS` = "headline_pro_dem_9",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3gVnSn1U4mwhEjk` = "headline_pro_rep_1",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_2ox2dINPLXixYcS` = "headline_pro_rep_2",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_0Gth87cjXjTBqHc` = "headline_pro_rep_3",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_1LYlSNANFhccgjI` = "headline_pro_rep_4",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_1yJzElXjQmL4PzM` = "headline_pro_rep_5",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_3n52QFm7tKt1jJc` = "headline_pro_rep_6",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_8v8TRmPUUWobIa2` = "headline_pro_rep_7",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_eFBTC9BblzOu95s` = "headline_pro_rep_8",
               `https://surveys.ipsossay.com/ControlPanel/Graphic.php?IM=IM_a5lNSP8WRWOY2A6` = "headline_pro_rep_9"))

  return(data)
}


## --------------------------------------------------------------------------------------------------------------------------
# change coding of variables
df.experiment_long_robustnesscheck <- df.experiment_long %>% 
  mutate(
    # from 1 is Yes, 2 is No, 3 is Don't know --> 0 is Don't know, 0 is No, 1 is Yes
    censorship = dplyr::recode(
      censorship,
      `2` = 0, `3` = 0,
      .combine_value_labels = TRUE))

df.experiment_long_robustnesscheck <- recode_and_relabel_data(df.experiment_long_robustnesscheck)


## --------------------------------------------------------------------------------------------------------------------------
# change coding of variables
df.experiment_long <- df.experiment_long %>% 
  mutate(
    # from 1 is Yes, 2 is No, 3 is Don't know --> NA is Don't know, 0 is No, 1 is Yes
    # (applying the recode_and_relabel_data function below will recode 3 to NA)
    censorship = dplyr::recode(
      censorship,
      `2` = 0,
      .combine_value_labels = TRUE))

df.experiment_long <- recode_and_relabel_data(df.experiment_long)


## --------------------------------------------------------------------------------------------------------------------------
# Function update_imputed_data
# Update labels and variables in all imputed datasets in an Amelia object.
# params: data: list of datasets to update coming from the Amelia imputations output
# returns: Amelia object with updated imputed data
update_imputed_data <- function(data) {
  data <- data %>%
    mutate(
      accuracy_binary = case_when(accuracy == 1 | accuracy == 2 ~ 0,
                                  accuracy == 3 | accuracy == 4 ~ 1,
                                  TRUE ~ NA_real_),
      political_interest = (news_consumption_national_politics +
                            news_consumption_state_politics +
                            news_consumption_local_politics +
                            news_consumption_international_politics) / 4) %>%
    # reapply labels
    var_labels(accuracy_binary = "Indicates whether a headline was deemed more or less accurate",
               political_interest = "Political interest. Average of national politics, state politics, local politics, and international politics news consumption, each measured on a 4-point scale from 1 (Not at all closely) to 4 (Very closely)") %>%
    set_value_labels(accuracy_binary = c(`Not at all accurate or Not very accurate` = 0,
                                         `Somewhat accurate or Very accurate` = 1)) %>% 
    # remove variables that aren't needed anymore after imputation
    dplyr::select(-c("census_region", "rural_urban_status", "household_size", 
                     "social_media_news_discussion_freq" , 
                     "news_media_use_freq_newspaper", "news_media_use_freq_tv", 
                     "news_media_use_freq_app", "news_media_use_freq_radio", 
                     "news_media_use_freq_magazine", "news_media_use_freq_direct_communication", 
                     "religion", "marital_status", "employment_status",
                     "news_consumption_international_politics", "news_consumption_local_politics", 
                     "news_consumption_national_politics", "news_consumption_state_politics"))
}


## --------------------------------------------------------------------------------------------------------------------------
# Function impute_data
# Generating imputated datasets using Amelia package.
# params: data: raw dataset on which imputations should be performed.
#         seed: seed to use for reproducibility.
# returns: Amelia object with updated imputed data.
impute_data <- function(data, seed) {
  # set seed for reproducibility
  set.seed(seed)
  
  # prepare for imputation
  data <- data %>%
    # remove variables with relatively high missingness
    dplyr::select(-c(trust_news_media, trust_news_media_unbiased,
                     trust_social_media_companies, trust_government, 
                     trust_university_admins, trust_high_school_admins, 
                     social_media_penalized_at_work_for_post, 
                     social_media_selfcensorship)) %>%
    # remove variables with multicollinearity issues
    dplyr::select(-c(state, # multicollinearity issue with census_region, not all states represented
                     news_format_most_common, # multicollinearity issue with social_media_most_common_newsformat
                     children_count, # multicollinearity issue with household_size
                     social_media_news_post_freq)) %>% # multicollinearity issue with social_media_news_discussion_freq
    # convert to avoid tibble issues in Amelia
    as.data.frame() 
  
  vec.nominal_variables <- c("employment_status", "marital_status", "census_region", 
                             "rural_urban_status", "gender", "race", "hispanic", 
                             "religion", "party_id", "headline")
  vec.ordinal_variables <- c("accuracy_order",
                             "social_media_news_discussion_freq",
                             "social_media_post_removed",
                             "social_media_post_flagged",
                             "news_media_use_freq_newspaper",
                             "news_media_use_freq_tv","news_media_use_freq_app",
                             "news_media_use_freq_radio",
                             "news_media_use_freq_magazine",
                             "news_media_use_freq_direct_communication",
                             "news_consumption_national_politics",
                             "news_consumption_state_politics",
                             "news_consumption_local_politics",
                             "news_consumption_international_politics",
                             "household_income",
                             "accuracy", "harm", "education",
                             "social_media_most_common_newsformat", "remove",
                             "censorship")
  
  # create 5 imputed datasets
  a.out <- amelia(data, 
                  m = 5, 
                  # specify nominal variables
                  noms = vec.nominal_variables,
                  # specify ordinal variables to bound the imputations
                  ords = vec.ordinal_variables ,
                  # specify time series variable
                  ts = "headline_order",
                  # specify cross-section variable
                  cs = "id",
                  # specify idvars not to use in imputation, but keep in dataset
                  idvars = c("party_id_dem", "party_id_rep", 
                             "headline_orientation", "headline_pro_dem", 
                             "headline_pro_rep", "aligned",
                             # add variables that need to be recalculated
                             "political_interest", "accuracy_binary", "weight"))
  # continuous variables don't need to be specified: 
  # age, household_size, weight (these do not have missing values)
  
  # recalculate indices and transformations and remove unnecessary variables
  a.out$imputations <- map(a.out$imputations, update_imputed_data)
  
  # apply class types of original object
  class(a.out$imputations) <- c("mi", "list")
  
  return(a.out)
}


## --------------------------------------------------------------------------------------------------------------------------
# impute data
a.out <- impute_data(df.experiment_long, seed = 123456)


## --------------------------------------------------------------------------------------------------------------------------
# impute data
a.out_robustnesscheck <- impute_data(df.experiment_long_robustnesscheck, seed = 1234567)


## --------------------------------------------------------------------------------------------------------------------------
vec.vars_to_remove <- c("news_format_most_common", "census_region", "state", 
                        "children_count", "rural_urban_status", "household_size", 
                        "social_media_news_post_freq", 
                        "social_media_news_discussion_freq" , 
                        "news_media_use_freq_newspaper", "news_media_use_freq_tv", 
                        "news_media_use_freq_app", "news_media_use_freq_radio", 
                        "news_media_use_freq_magazine", 
                        "news_media_use_freq_direct_communication", "religion", 
                        "marital_status", "employment_status",
                        "trust_government", "trust_high_school_admins", "trust_news_media", 
                        "trust_news_media_unbiased", "trust_social_media_companies", 
                        "trust_university_admins", "social_media_penalized_at_work_for_post", 
                        "social_media_selfcensorship", "news_consumption_international_politics", 
                        "news_consumption_local_politics", "news_consumption_national_politics", 
                        "news_consumption_state_politics")


## --------------------------------------------------------------------------------------------------------------------------
# remove variables that are not needed anymore after imputation
df.experiment_long <- df.experiment_long %>% 
  dplyr::select(-all_of(vec.vars_to_remove))


## --------------------------------------------------------------------------------------------------------------------------
# remove variables that are not needed anymore after imputation
df.experiment_long_robustnesscheck <- df.experiment_long_robustnesscheck %>% 
  dplyr::select(-all_of(vec.vars_to_remove))


## --------------------------------------------------------------------------------------------------------------------------
# save each file
write.amelia(obj = a.out, file.stem = "data/intermediate/survey/imputations")


## --------------------------------------------------------------------------------------------------------------------------
# save each file
write.amelia(obj = a.out_robustnesscheck, file.stem = "data/intermediate/survey/imputations_robustnesscheck")


## --------------------------------------------------------------------------------------------------------------------------
# save dataframe
write_csv(df.experiment_long, 
          "data/intermediate/survey/df.experiment_long.csv")


## --------------------------------------------------------------------------------------------------------------------------
# save dataframe
write_csv(df.experiment_long_robustnesscheck, 
          "data/intermediate/survey/df.experiment_long_robustnesscheck.csv")


## --------------------------------------------------------------------------------------------------------------------------
# create data dictionary
df.dict_final <- create_dictionary(df.experiment_long, use_references = FALSE) %>% 
  mutate(`label type` = if_else(is.na(meta),
                               "value label",
                               "variable label")) %>% 
  dplyr::select(-meta) %>% 
  mutate(value = if_else(is.na(value), "", value)) %>% 
  arrange(variable, value) %>% 
  rename_with(., ~ janitor::make_clean_names(., case = "title")) %>% 
  dplyr::select(Variable, Value, `Label Type`, Label)

write_csv(df.dict_final, "data/intermediate/survey/codebook.csv")


## --------------------------------------------------------------------------------------------------------------------------
# the following lines using the Congressional speeches dataset will not run because we do not 
# include the raw data (they can be downloaded from 
# https://github.com/dallascard/us-immigration-speeches/)
df.by_party <- fromJSON(file = "data/input/speech/data/speeches/Congress/tagged_counts/token_counts_by_congress_by_party.json") %>% 
  unlist(.) %>% 
  tibble(party.congress.token_tag = names(.),
         count = .) %>%
  # keep token and tag together in case there is no tag
  tidyr::extract(party.congress.token_tag, 
                 into = c("party", "congress", "token_tag"), 
                 "^([\\w]).([\\d]+).(.*)") %>%
  # separate token and tag
  mutate(token = if_else(grepl(" \\(.\\)$", token_tag), 
                         str_sub(token_tag, 1, nchar(token_tag) - 4), 
                         token_tag),
         tag = if_else(grepl(" \\(.\\)$", token_tag), 
                       str_sub(token_tag, -3, -1),
                       NA_character_)) %>% 
  dplyr::select(-token_tag) %>% 
  mutate(congress = as.numeric(congress)) %>% 
  mutate(party = if_else(party == "D", "Democrat", "Republican")) %>% 
  # filter to censorship-related tokens
  filter(grepl("censor", token))


## --------------------------------------------------------------------------------------------------------------------------
# check for irrelevant tokens
df.by_party %>% 
  pull(token) %>% 
  unique()


## --------------------------------------------------------------------------------------------------------------------------
df.censor_tokens_congress <- df.by_party %>% 
  # remove irrelevant tokens
  filter(!grepl("licensor|censorious|censorioas|censorable", token)) %>%
  # sum tokens by party
  group_by(party, congress) %>%
  summarize(total_censor_token_count = sum(count, na.rm = TRUE),
            .groups = "drop")


## --------------------------------------------------------------------------------------------------------------------------
write_csv(df.censor_tokens_congress, 
          "data/intermediate/speech/df.censor_tokens_congress.csv")


## ----sessioninfo-----------------------------------------------------------------------------------------------------------
sessionInfo()


## ----include=FALSE, eval=FALSE---------------------------------------------------------------------------------------------
## knitr::purl(input = "code/data_cleaning.Rmd",
##             output = "code/data_cleaning.R",
##             documentation = 1)

