## ----setup, include=FALSE------------------------------------------------------------------------------------
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


## ----install_packages----------------------------------------------------------------------------------------
# install necessary packages if not already installed
packages_to_load <- c("vtable", "haven", "sjlabelled", "readxl",  "spatstat", 
                      "RColorBrewer", "patchwork", "ggbrace", "janitor",
                      "mice", "tableone",  "broom", "mediation", "gt", "tidyverse")
packages_to_install <- packages_to_load[!(packages_to_load %in% installed.packages()[, "Package"])]
if (length(packages_to_install)) install.packages(packages_to_install)


## ------------------------------------------------------------------------------------------------------------
if (!("ggbrace" %in% installed.packages()[, "Package"])) {
  # install devtools if not already present
  if (!("devtools" %in% installed.packages()[, "Package"])) {
    install.packages("devtools")
  }
  # install ggbrace from GitHub
  devtools::install_github("nicolash2/ggbrace") 
}


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------
# load functions for data cleaning and analysis
source(file = "code/final_models.R")


## ----load_libraries------------------------------------------------------------------------------------------
# load libraries
library(vtable)        # for descriptive statistics
library(tableone)      # for balance tables
library(haven)         # for dealing with labels
library(sjlabelled)    # for dealing with labels
library(readxl)        # for reading XLSX sheets
library(spatstat)      # for weighted median
library(RColorBrewer)  # for colors
library(patchwork)     # for combining plots
library(mediation)     # for mediation analysis
library(ggbrace)       # for plotting braces
library(gt)            # for LaTeX table formatting
library(tidyverse)     # for data wrangling


## ------------------------------------------------------------------------------------------------------------
# share of responses rating headlines as somewhat or very accurate
pct_accurate <- df.experiment_long %>% 
  summarize(mean_accurate = 100 * mean(accuracy == 3 | accuracy == 4, na.rm = TRUE)) %>% 
  round(2)

pct_accurate


## ------------------------------------------------------------------------------------------------------------
# get confidence intervals
model_list_cis_names <- c(
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_overall",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_aligned",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_misaligned",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_overall",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_aligned",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_misaligned",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_overall",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_aligned",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_ld_misaligned")

model_list_cis <- lapply(model_list_cis_names, function(x) eval(sym(x)))

names(model_list_cis) <- model_list_cis_names

df.cis <- get_cis(model_list_cis)


## ------------------------------------------------------------------------------------------------------------
# get dataframe with means for outcomes
df.means <- df.experiment_long %>% 
  pivot_longer(cols = c(remove, censorship, harm),
               names_to = "variable",
               values_to = "rating") %>% 
  mutate(party_id = factor(party_id)) %>% 
  dplyr::mutate(number = as.numeric(str_extract(headline, "[:digit:]+")),
                orientation = str_extract(headline, "(?<=headline_).*(?=_\\.*)"))  %>% 
  group_by(variable, party_id, orientation, aligned) %>% 
  summarize(mean = weighted.mean(rating, weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable, party_id) %>% 
  mutate(mean_by_party_id = mean(mean)) %>% 
  ungroup()

# add overall means
df.means_add <- df.experiment_long %>% 
  pivot_longer(cols = c(remove, censorship, harm),
               names_to = "variable",
               values_to = "rating") %>% 
  mutate(party_id = factor(party_id)) %>% 
  dplyr::mutate(number = as.numeric(str_extract(headline, "[:digit:]+")),
                orientation = str_extract(headline, "(?<=headline_).*(?=_\\.*)"))  %>% 
  group_by(variable, party_id) %>% 
  summarize(mean = weighted.mean(rating, weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable, party_id) %>% 
  mutate(mean_by_party_id = mean(mean)) %>% 
  ungroup() %>% 
  mutate(mean = mean_by_party_id,
         orientation = 'overall',
         aligned = 0.5)

df.means <- rbind(df.means, df.means_add) %>% 
  mutate(aligned = factor(aligned, levels = c(0.5, 0, 1)))

df.means <- left_join(df.means, df.cis, by = c("variable", "party_id", "aligned"))


## ------------------------------------------------------------------------------------------------------------
# create descriptive plot for remove
# lineheight parameter changes spacing between multiline labels for braces, can ignore warning
plot_gap_remove <- ggplot(df.means %>% 
         filter(variable == "remove"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Intent to Remove Headline",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "remove" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "remove" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "remove" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate Democrat party promotion
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(0.55, 0.65), y = df.means %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 
                 label = "party promotion (Democrat)"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "#377EB8", lineheight = 0.9) +  
  geom_segment(data = tibble(aligned = factor(c(0, 0, 1, 1), levels = c(0.5, 0, 1)),
                             x = c(1.45, 1.45, -Inf, -Inf), 
                             xend = c(Inf, Inf, 0.55, 0.55),
                             y = rep(df.means %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2),
                             yend = rep(df.means %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2)),
               aes(x = x, y = y, xend = xend, yend = yend), 
                   linetype = "dotted", color = "#377EB8", inherit.aes = FALSE) +  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ------------------------------------------------------------------------------------------------------------
# create descriptive plot for harm
plot_gap_harm <- ggplot(df.means %>% 
         filter(variable == "harm"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Intent to Report Headline as Harmful",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "harm" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "harm" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "harm" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate Democrat party promotion
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(0.55, 0.65), y = df.means %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 
                 label = "party promotion (Democrat)"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "#377EB8", lineheight = 0.9) +  
  geom_segment(data = tibble(aligned = factor(c(0, 0, 1, 1), levels = c(0.5, 0, 1)),
                             x = c(1.45, 1.45, -Inf, -Inf), 
                             xend = c(Inf, Inf, 0.55, 0.55),
                             y = rep(df.means %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2),
                             yend = rep(df.means %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2)),
               aes(x = x, y = y, xend = xend, yend = yend), 
                   linetype = "dotted", color = "#377EB8", inherit.aes = FALSE) +  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ------------------------------------------------------------------------------------------------------------
# create descriptive plot for censorship
plot_gap_censorship <- ggplot(df.means %>% 
         filter(variable == "censorship"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Perception of Headline Removal as Censorship",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "censorship" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "censorship" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means %>% filter(variable == "censorship" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, color = "grey50", lineheight = 0.9) +
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ------------------------------------------------------------------------------------------------------------
# get confidence intervals
model_list_cis_inacc_names <- c(
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_overall",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_aligned",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_misaligned",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_overall",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_aligned",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_misaligned",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_overall",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_aligned",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_ld_misaligned")

model_list_cis_inacc <- lapply(model_list_cis_inacc_names, function(x) eval(sym(x)))

names(model_list_cis_inacc) <- model_list_cis_inacc_names

df.cis_inacc <- get_cis(model_list_cis_inacc)


## ------------------------------------------------------------------------------------------------------------
# get dataframe with means for outcomes
df.means_inacc <- df.experiment_long %>% 
  # keep only participants who rated headlines as inaccurate
  filter(accuracy_binary == 0) %>%  
  pivot_longer(cols = c(remove, censorship, harm),
               names_to = "variable",
               values_to = "rating") %>% 
  mutate(party_id = factor(party_id)) %>% 
  dplyr::mutate(number = as.numeric(str_extract(headline, "[:digit:]+")),
                orientation = str_extract(headline, "(?<=headline_).*(?=_\\.*)"))  %>% 
  group_by(variable, party_id, orientation, aligned) %>% 
  summarize(mean = weighted.mean(rating, weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable, party_id) %>% 
  mutate(mean_by_party_id = mean(mean)) %>% 
  ungroup()

# add overall means
df.means_inacc_add <- df.experiment_long %>% 
  # keep only participants who rated headlines as inaccurate
  filter(accuracy_binary == 0) %>%  
  pivot_longer(cols = c(remove, censorship, harm),
               names_to = "variable",
               values_to = "rating") %>% 
  mutate(party_id = factor(party_id)) %>% 
  dplyr::mutate(number = as.numeric(str_extract(headline, "[:digit:]+")),
                orientation = str_extract(headline, "(?<=headline_).*(?=_\\.*)"))  %>% 
  group_by(variable, party_id) %>% 
  summarize(mean = weighted.mean(rating, weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable, party_id) %>% 
  mutate(mean_by_party_id = mean(mean)) %>% 
  ungroup() %>% 
  mutate(mean = mean_by_party_id,
         orientation = 'overall',
         aligned = 0.5)

df.means_inacc <- rbind(df.means_inacc, df.means_inacc_add) %>% 
  mutate(aligned = factor(aligned, levels = c(0.5, 0, 1)))

df.means_inacc <- left_join(df.means_inacc, df.cis_inacc, by = c("variable", "party_id", "aligned"))


## ------------------------------------------------------------------------------------------------------------
plot_gap_remove_inacc <- ggplot(df.means_inacc %>% 
         filter(variable == "remove"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Intent to Remove Headline",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "remove" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "remove" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "remove" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate Democrat party promotion
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(0.55, 0.65), y = df.means_inacc %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 
                 label = "party promotion (Democrat)"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "#377EB8", lineheight = 0.9) +  
  geom_segment(data = tibble(aligned = factor(c(0, 0, 1, 1), levels = c(0.5, 0, 1)),
                             x = c(1.45, 1.45, -Inf, -Inf), 
                             xend = c(Inf, Inf, 0.55, 0.55),
                             y = rep(df.means_inacc %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2),
                             yend = rep(df.means_inacc %>% filter(variable == "remove" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2)),
               aes(x = x, y = y, xend = xend, yend = yend), 
                   linetype = "dotted", color = "#377EB8", inherit.aes = FALSE) +  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ------------------------------------------------------------------------------------------------------------
plot_gap_harm_inacc <- ggplot(df.means_inacc %>% 
         filter(variable == "harm"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Intent to Report Headline as Harmful",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "harm" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "harm" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "harm" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "grey50", lineheight = 0.9) +
  # annotate Democrat party promotion
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(0.55, 0.65), y = df.means_inacc %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 
                 label = "party promotion (Democrat)"), 
             labelsize = 3, labeldistance = 0.05, rotate = 90, color = "#377EB8", lineheight = 0.9) +  
  geom_segment(data = tibble(aligned = factor(c(0, 0, 1, 1), levels = c(0.5, 0, 1)),
                             x = c(1.45, 1.45, -Inf, -Inf), 
                             xend = c(Inf, Inf, 0.55, 0.55),
                             y = rep(df.means_inacc %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2),
                             yend = rep(df.means_inacc %>% filter(variable == "harm" & party_id == "Democrat" & aligned != 0.5) %>% pull(mean), 2)),
               aes(x = x, y = y, xend = xend, yend = yend), 
                   linetype = "dotted", color = "#377EB8", inherit.aes = FALSE) +  
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ------------------------------------------------------------------------------------------------------------
plot_gap_censorship_inacc <- ggplot(df.means_inacc %>% 
         filter(variable == "censorship"),
       mapping = aes(x = party_id, y = mean, fill = party_id)) +
  geom_bar(stat = "identity") +
  # add estimate
  geom_text(aes(label = paste0(100*round(mean, 2), "%"), y = 0.05), 
            position = position_dodge(width = 0.9), color = "gray20", size = 3) +
  # add error bar
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, 
                position=position_dodge(.9), alpha = 0.5) +
  # add lines for overall means
  geom_hline(aes(yintercept = mean_by_party_id), linetype = "dotted", alpha = 0.3) +
  facet_wrap(~aligned, labeller = as_labeller(c(`0` = "Miasaligned", `1` = "Aligned", `0.5` = "Overall"))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(x = "Partisanship",
       y = "Average Perception of Headline Removal as Censorship",
       lty = "Overall mean") +
  # annotate overall gap
  geom_brace(data = tibble(aligned = factor(c(0.5, 0.5), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "censorship" & aligned == 0.5) %>% pull(mean), 
                 label = "overall gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, lineheight = 0.9) +
  # annotate misaligned gap
  geom_brace(data = tibble(aligned = factor(c(0, 0), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "censorship" & aligned == 0) %>% pull(mean), 
                 label = "misaligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, color = "grey50", lineheight = 0.9) +
  # annotate aligned gap
  geom_brace(data = tibble(aligned = factor(c(1, 1), levels = c(0.5, 0, 1)), 
                           party_id = c("Democrat", "Democrat"), # arbitrary values
                           mean = c(0.5, 0.5)), # arbitrary values 
             aes(x = c(1.45, 1.55), y = df.means_inacc %>% filter(variable == "censorship" & aligned == 1) %>% pull(mean), 
                 label = "aligned\npreference gap"), 
             labelsize = 3, labeldistance = 0.05, rotate = 270, color = "grey50", lineheight = 0.9) +
  theme(legend.position = 'none',
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey95'))


## ----coef_plot_prep------------------------------------------------------------------------------------------
# create named vector for labels
vec.variables <- c("Democrat", 
                   "Republican", 
                   "Democrat x\nPro-Democrat Headline", 
                   "Republican x\nPro-Republican Headline")
names(vec.variables) <- c("party_idDemocrat", 
                          "party_idRepublican", 
                          "party_id_dem:headline_pro_dem", 
                          "party_id_rep:headline_pro_rep")
# set colors, limits, and breaks for combined plots
vec.colors_accuracy <- c(head(brewer.pal(11, name = "PiYG"), 1),
                         tail(brewer.pal(11, name = "PiYG"), 1))
vec.colors <-  c(tail(brewer.pal(11, name = "Spectral"), 1),
                 head(brewer.pal(11, name = "Spectral"), 1))
vec.limits <- c(-0.5, 1)
vec.breaks <- seq(-0.5, 1, 0.25)


## ----component_coef_plots_all--------------------------------------------------------------------------------
# save component plots
## remove
plot_models_remove_main_all <- plot_models(
  models = list(# baseline models for all respondents
    mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld =
      mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld),
  model_labels = c(mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = "All respondents"),
  variables = vec.variables,
  colors = vec.colors[1],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Intent to Remove Headline",
  legend = TRUE)
  
## harm
plot_models_harm_main_all <- plot_models(
  models = list(# baseline models for all respondents
    mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = 
      mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld),
  model_labels = c(mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = "All respondents"),
  variables = vec.variables,
  colors = vec.colors[1],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Intent to Report Headline as Harmful",
  legend = TRUE)

## censor
plot_models_censor_main_all <- plot_models(
  models = list(# baseline models for all respondents
    mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld =
      mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld),
  model_labels = c(mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = "All respondents"),
  variables = vec.variables,
  colors = vec.colors[1],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Perception of Headline Removal as Censorship",
  legend = TRUE)


## ----component_coef_plots_inacc------------------------------------------------------------------------------
# save component plots
## remove
plot_models_remove_main_inacc <- plot_models(
  models = list(# baseline models for inaccurate subset
    mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = 
      mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld),
  model_labels = c(mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = "Subset inaccurate"),
  variables = vec.variables,
  colors = vec.colors[2],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Intent to Remove Headline",
  legend = TRUE)
  
## harm
plot_models_harm_main_inacc <- plot_models(
  models = list(# baseline models for inaccurate subset
    mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = 
      mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld),
  model_labels = c(mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = "Subset inaccurate"),
  variables = vec.variables,
  colors = vec.colors[2],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Intent to Report Headline as Harmful",
  legend = TRUE)

## censor
plot_models_censor_main_inacc <- plot_models(
  models = list(# baseline models for inaccurate subset
    mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = 
      mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld),
  model_labels = c(mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld = "Subset inaccurate"),
  variables = vec.variables,
  colors = vec.colors[2],
  limits = vec.limits,
  breaks = vec.breaks,
  title = "Perception of Headline Removal as Censorship",
  legend = TRUE)


## ----fig.width=6, fig.height=4-------------------------------------------------------------------------------
plot_models_accuracy_main_all <- plot_models(
  models = list(# baseline models for inaccurate subset
    mod.accuracy_binary_by_party_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = 
      mod.accuracy_binary_by_party_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld),
  model_labels = c(mod.accuracy_binary_by_party_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld = "All respondents"),
  variables = vec.variables,
  colors = 'navy',
  limits = c(-0.1, 0.5),
  breaks = vec.breaks,
  title = "Accuracy Rating of Headline",
  legend = FALSE)

plot_models_accuracy_main_all


## ----fig.width=13.5, fig.height=14.4-------------------------------------------------------------------------
# save figure 1
wrap_plots(plot_models_remove_main_inacc, plot_spacer(), plot_gap_remove_inacc,
  plot_models_harm_main_inacc, plot_spacer(), plot_gap_harm_inacc,
  plot_models_censor_main_inacc, plot_spacer(), plot_gap_censorship_inacc, 
  ncol = 3) +
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E", "F"))) +
  plot_layout(guides = "collect", widths = c(1.45, 0.05, 2)) & theme(legend.position = "none")

ggsave("figures/main_plots/coefficient_barplot_inaccsubgroup.png",
       width = 13.5, height = 14.4, dpi = 300, units = "in")


## ------------------------------------------------------------------------------------------------------------
# save figure 2
ggsave("figures/main_plots/coefficient_plot_models_combined_main_panel_2.png", 
       plot_models_accuracy_main_all,
       width = 6, height = 4)


## ----fig.width=13.5, fig.height=14.4-------------------------------------------------------------------------
# save figure 3
wrap_plots(plot_models_remove_main_all, plot_spacer(), plot_gap_remove,
  plot_models_harm_main_all, plot_spacer(), plot_gap_harm,
  plot_models_censor_main_all, plot_spacer(), plot_gap_censorship, 
  ncol = 3) +
  plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E", "F"))) +
  plot_layout(guides = "collect", widths = c(1.45, 0.05, 2)) & theme(legend.position = "none")

ggsave("figures/main_plots/coefficient_barplot_anysubgroup.png",
       width = 13.5, height = 14.4, dpi = 300, units = "in")


## ----main_mediation_table------------------------------------------------------------------------------------
generate_mediation_table(data = df.experiment_long,
                         include_controls = FALSE,
                         subset_condition = "party_id == 'Democrat'",
                         mediator = "accuracy",
                         label = "mediation-democrats-main",
                         caption = "\\textbf{Effect of Alignment Mediated by Accuracy for Democrats.}",
                         footnote = "Mediation models were run with standard errors clustered on participants, without weigthing observations and without control variables using a dataset in which missing values were addressed using listwise deletion.",
                         cluster = TRUE)


## ----balance_table_prep--------------------------------------------------------------------------------------
# create dataframe for balance tables with capitalized variable names
df.experiment_long_balance_headline_1 <- df.experiment_long %>%
  # filter to relevant headlines (we only need one headline headline per participant)
  filter(headline_order == 0) %>% 
  # select relevant variables
  dplyr::select(all_of(vec.controls), aligned, accuracy_order) %>% 
  # turn categorical variables treated as ordinal or binary into strings
  mutate_if(haven::is.labelled,
            sjlabelled::as_label)  %>% 
  rename_with(., ~ janitor::make_clean_names(., case = "title"))

vec.controls_balance <- janitor::make_clean_names(vec.controls, case = "title")


## ----balance_table_aligned-----------------------------------------------------------------------------------
generate_balance_table(data = df.experiment_long_balance_headline_1, 
                       covariates = vec.controls_balance, 
                       treatment = "Aligned", 
                       caption = "Balance Table for Partisan Alignment of Headline, First Headline",
                       footnote = c("p-values result from a joint F-test for continuous variables and from a Chi-squared test for categorical variables. Standardized mean difference (SMD) and p-values are exactly the same for the subset of data on the second headline, only the data in the Yes and No columns would be reversed, therefore we show only one table."),
                       label = "balance-table-aligned-h1")


## ----balance_table_accuracy_order----------------------------------------------------------------------------
generate_balance_table(data = df.experiment_long_balance_headline_1, 
                       covariates = vec.controls_balance, 
                       treatment = "Accuracy Order", 
                       caption = "Balance Table for Accuracy Question Order",
                       footnote = c("p-values result from a joint F-test for continuous variables and from a Chi-squared test for categorical variables. Accuracy question order was randomized at the participant level, therefore balance checks were run on the short data frame with one headline observations per participant."),
                       label = "balance-table-accuracyorder")


## ----balance_table_prep_2------------------------------------------------------------------------------------
# create dataframe for balance tables with capitalized variable names
df.experiment_long_balance_accuracy_partisanship <- df.experiment_long %>%
  # select relevant variables
  dplyr::select(all_of(vec.controls), aligned, accuracy_order, accuracy_binary, party_id) %>% 
  # turn categorical variables treated as ordinal or binary into strings
  mutate_if(haven::is.labelled,
            sjlabelled::as_label)  %>% 
  rename_with(., ~ janitor::make_clean_names(., case = "title")) %>% 
  rename(`Partisanship` = `Party Id`)

vec.controls_balance <- janitor::make_clean_names(vec.controls, case = "title")


## ------------------------------------------------------------------------------------------------------------
df.experiment_long_balance_accuracy_partisanship_dem_comparison <- 
  # create two groups: all Democrats vs. inaccurate subset
  rbind(df.experiment_long_balance_accuracy_partisanship %>% 
          filter(Partisanship == "Democrat") %>% 
          mutate(`Subset of Democrat Respondents` = "All Respondents"),
        df.experiment_long_balance_accuracy_partisanship %>% 
          filter(Partisanship == "Democrat" & `Accuracy Binary` == "Not at all accurate or Not very accurate") %>% 
          mutate(`Subset of Democrat Respondents` = "Inaccurate Subset"))


## ------------------------------------------------------------------------------------------------------------
df.experiment_long_balance_accuracy_partisanship_rep_comparison <- 
  # create two groups: all Republicans vs. inaccurate subset
  rbind(df.experiment_long_balance_accuracy_partisanship %>% 
          filter(Partisanship == "Republican") %>% 
          mutate(`Subset of Republican Respondents` = "All Respondents"),
        df.experiment_long_balance_accuracy_partisanship %>% 
          filter(Partisanship == "Republican" & `Accuracy Binary` == "Not at all accurate or Not very accurate") %>% 
          mutate(`Subset of Republican Respondents` = "Inaccurate Subset"))


## ------------------------------------------------------------------------------------------------------------
generate_balance_table(data = df.experiment_long_balance_accuracy_partisanship_dem_comparison, 
                       covariates = vec.controls_balance, 
                       treatment = "Subset of Democrat Respondents", 
                       caption = "Balance Table for Subset of Democrat Respondents",
                       footnote = c("p-values result from a joint F-test for continuous variables and from a Chi-squared test for categorical variables. Each participant rated the accuracy of two headlines, therefore each participant may account for up to two observations in the data if they rated the accuracy of two headlines."),
                       label = "balance-table-accuracy-selection-democrat")


## ------------------------------------------------------------------------------------------------------------
generate_balance_table(data = df.experiment_long_balance_accuracy_partisanship_rep_comparison, 
                       covariates = vec.controls_balance, 
                       treatment = "Subset of Republican Respondents", 
                       caption = "Balance Table for Subset of Republican Respondents",
                       footnote = c("p-values result from a joint F-test for continuous variables and from a Chi-squared test for categorical variables. Each participant rated the accuracy of two headlines, therefore each participant may account for up to two observations in the data if they rated the accuracy of two headlines."),
                       label = "balance-table-accuracy-selection-republican")


## ----descriptive_stats_data_prep-----------------------------------------------------------------------------
df.experiment_long_descriptive_stats <- df.experiment_long %>%
  filter(!duplicated(id)) %>%
  dplyr::select(id, all_of(vec.controls), party_id) %>% 
  mutate_if(haven::is.labelled,
            sjlabelled::as_label) %>% 
  rename_with(., ~ janitor::make_clean_names(., case = "title")) %>% 
  rename(Partisanship = `Party Id`,
         `Social Media Most Common News Format` = `Social Media Most Common Newsformat`) %>% 
  mutate(Race = relevel(Race, ref = "White"),
         Hispanic = relevel(Hispanic, ref = "Yes"),
         `Social Media Most Common News Format` = relevel(`Social Media Most Common News Format`, ref = "Yes"),
         `Social Media Post Flagged` = recode_factor(`Social Media Post Flagged`,
                                                     `Yes, I have experienced or done this` = "Yes",
                                                     `No, I have not experienced or done this` = "No",
                                                     .ordered = TRUE),
         `Social Media Post Removed` = recode_factor(`Social Media Post Removed`,
                                                     `Yes, I have experienced or done this` = "Yes",
                                                     `No, I have not experienced or done this` = "No",
                                                     .ordered = TRUE))


## ----descriptive_stats---------------------------------------------------------------------------------------
sumtable(df.experiment_long_descriptive_stats,
         add.median = TRUE,
         title = "Descriptive Statistics",
         anchor = "descriptive_stats",
         summ.names = c("N", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
         out = "latex",
         file = "tables/descriptives/descriptive_stats.tex")


## ----census_data---------------------------------------------------------------------------------------------
# get Census bureau statistics for year as close as possible to our study
# 2020 data (survey was conducted 2021)
df.hispanic <- read_excel("data/input/census/DECENNIALPL2020.P2-2022-09-07T194449.xlsx", 
                          sheet = "Data")

mean_hispanic_us <- 
  # Hispanic/Latino population
  (df.hispanic %>% 
     filter(Label == "Hispanic or Latino") %>% 
     pull(`United States`) %>%
     # remove thousands separator
     gsub(",", "", .) %>% 
     as.numeric()) /
  # divided by total population
  (df.hispanic %>% 
     filter(Label == "Total:") %>% 
     pull(`United States`) %>% 
     gsub(",", "", .) %>% 
     as.numeric())

median_age_us <- suppressMessages(read_excel("data/input/census/ACSST5Y2020.S0101-2022-09-07T230634.xlsx", 
                                  sheet = "Data")) %>% 
  filter(`...1` == "Median age (years)") %>% 
  pull(`United States`) %>% 
  as.numeric()


## ----representativeness_ipsos_full_sample, eval = FALSE------------------------------------------------------
## # the following lines (up to the regression plots section) using the raw survey data will not run
## # because we do not include the raw data  (the data source is Knight Foundation-Ipsos (2022)
## # "Free Expression in America Post-2020" [Dataset], and all intermediate data needed to reproduce
## # other tables and figures are provided and loaded in the manuscript_code scripts)
## 
## # load raw data with all participants
## df.experiment <- read_sav("data/input/survey/Knight Foundation_FX_SPSS_PLUS SCHOLAR DATA_121421.sav")
## 
## # check representativeness for all participants
## # (assuming student and main weights can be used together)
## df.representativeness_ipsos_fullsample <- df.experiment %>%
##   mutate(hispanic = case_when(xhispan == 1 ~ 0,
##                               xhispan == 2 | xhispan == 3 | xhispan == 4 | xhispan == 8 ~ 1,
##                               TRUE ~ NA_real_),
##          age = ppage,
##          weight = if_else(is.na(Main_Weights), Students_Weights, Main_Weights)) %>%
##   filter(!is.na(weight)) %>%
##   summarize(unweighted_median_age = median(age, na.rm = TRUE),
##             unweighted_mean_hispanic = mean(hispanic, na.rm = TRUE),
##             weighted_median_age = weighted.median(age, w = weight, na.rm = TRUE),
##             weighted_mean_hispanic = weighted.mean(hispanic, w = weight, na.rm = TRUE)) %>%
##   mutate(Sample = "Ipsos full sample (including students)")


## ----representativeness_ipsos_nonstudent_sample, eval = FALSE------------------------------------------------
## # check representativeness for all non-student participants
## df.representativeness_ipsos_nonstudentsample <- df.experiment %>%
##   mutate(hispanic = case_when(xhispan == 1 ~ 0,
##                               xhispan == 2 | xhispan == 3 | xhispan == 4 | xhispan == 8 ~ 1,
##                               TRUE ~ NA_real_),
##          age = ppage,
##          weight = Main_Weights) %>%
##   filter(!is.na(Main_Weights)) %>%
##   summarize(unweighted_median_age = median(age, na.rm = TRUE),
##             unweighted_mean_hispanic = mean(hispanic, na.rm = TRUE),
##             weighted_median_age = weighted.median(age, w = weight, na.rm = TRUE),
##             weighted_mean_hispanic = weighted.mean(hispanic, w = weight, na.rm = TRUE)) %>%
##   mutate(Sample = "Ipsos full sample (excluding students)")


## ----representativeness_ipsos_subsample, eval = FALSE--------------------------------------------------------
## # check representativeness for all non-student participants in our subsample (before removing independents etc.)
## df.representativeness_ipsos_subsample <- df.experiment %>%
##   # keep only participants in our experiment who saw outcome measures
##   filter(!is.na(Q50A)) %>%
##   mutate(hispanic = case_when(xhispan == 1 ~ 0,
##                               xhispan == 2 | xhispan == 3 | xhispan == 4 | xhispan == 8 ~ 1,
##                               TRUE ~ NA_real_),
##          age = ppage,
##          weight = Main_Weights) %>%
##   filter(!is.na(Main_Weights)) %>%
##   summarize(unweighted_median_age = median(age, na.rm = TRUE),
##             unweighted_mean_hispanic = mean(hispanic, na.rm = TRUE),
##             weighted_median_age = weighted.median(age, w = weight, na.rm = TRUE),
##             weighted_mean_hispanic = weighted.mean(hispanic, w = weight, na.rm = TRUE)) %>%
##   mutate(Sample = "Ipsos sample for this study (excluding students)")


## ----representativeness_final_sample, eval = FALSE-----------------------------------------------------------
## df.representativeness_finalsample <- df.experiment_long %>%
##   filter(!duplicated(id)) %>%
##   summarize(unweighted_median_age = median(age),
##             unweighted_mean_hispanic = mean(hispanic),
##             weighted_median_age = weighted.median(age, w = weight),
##             weighted_mean_hispanic = weighted.mean(hispanic, w = weight)) %>%
##   mutate(Sample = "Final sample")


## ----representativeness_table, eval = FALSE------------------------------------------------------------------
## df.representativeness <- rbind(df.representativeness_ipsos_fullsample,
##                                    df.representativeness_ipsos_nonstudentsample,
##                                    df.representativeness_ipsos_subsample,
##                                    df.representativeness_finalsample) %>%
##   pivot_longer(cols = c("unweighted_median_age", "unweighted_mean_hispanic", "weighted_median_age", "weighted_mean_hispanic"),
##                names_to = c("Weighting", "variable"),
##                names_pattern = "(unweighted|weighted)_(.*)") %>%
##   pivot_wider(names_from = "variable") %>%
##   rename(`Median Age` = median_age, `Share Hispanic or Latino` = mean_hispanic) %>%
##   add_row(Sample = "US Population", Weighting = "unweighted", `Median Age` = median_age_us, `Share Hispanic or Latino` = mean_hispanic_us, .before = 1)
## 
## kable(df.representativeness, booktabs = TRUE, digits = 3, caption = "Descriptive Statistics on Representativeness of Sample", label = "representativeness", linesep = "", format = "latex") %>%
##   row_spec(0, bold=TRUE) %>%
##   kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
##   footnote(general = c(paste0("\\\\small{\\\\textit{Sources:} ", "For 2020 age data: U.S. Census Bureau, 2016-2020 American Community Survey 5-Year Estimates, retrieved on September 7, 2022 from \\\\url{https://data.census.gov/cedsci/table?q=median%20age&g=0100000US&tid=ACSST5Y2020.S0101}. For 2020 ethnicity data: U.S. Census Bureau, 2020 Census Redistricting Data (Public Law 94-171), retrieved on September 7, 2022 from \\\\url{https://data.census.gov/cedsci/table?q=hispanic&g=0100000US&tid=DECENNIALPL2020.P2}." %>% gsub("%", "\\\\%", ., fixed = TRUE) %>% gsub("&", "\\\\&", ., fixed = TRUE), "}")),
##            general_title = paste0(""),
##            threeparttable = TRUE,
##            escape = FALSE) %>%
##   gsub("[H]", "[!htbp]", ., fixed = TRUE) %>%
##   cat(., file = paste0("tables/descriptives/", "representativeness", ".tex", sep = ""))


## ------------------------------------------------------------------------------------------------------------
model_list_remove_anyheadline_names <-
c(# baseline - any subgroup
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # baseline - inaccurate subgroup
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  # controls - any subgroup
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # controls - inaccurate subgroup
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi")

model_list_remove_anyheadline <- lapply(model_list_remove_anyheadline_names, function(x) eval(sym(x)))

names(model_list_remove_anyheadline) <- model_list_remove_anyheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_remove_anyheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Intent to Remove Headline",
            show_n_obs = TRUE,
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_remove_robustness_checks_anyheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_remove_firstheadline_names <-
c(# baseline - any subgroup
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # baseline - inaccurate subgroup
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.remove_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  # controls - any subgroup
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # controls - inaccurate subgroup
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.remove_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.remove_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi")

model_list_remove_firstheadline <- lapply(model_list_remove_firstheadline_names, function(x) eval(sym(x)))

names(model_list_remove_firstheadline) <- model_list_remove_firstheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_remove_firstheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Intent to Remove Headline",
            show_n_obs = TRUE,
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_remove_robustness_checks_firstheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_harm_anyheadline_names <-
c(# baseline - any subgroup
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # baseline - inaccurate subgroup
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  # controls - any subgroup
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # controls - inaccurate subgroup
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi")

model_list_harm_anyheadline <- lapply(model_list_harm_anyheadline_names, function(x) eval(sym(x)))

names(model_list_harm_anyheadline) <- model_list_harm_anyheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_harm_anyheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Intent to Report Headline as Harmful",
            show_n_obs = TRUE,
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_harm_robustness_checks_anyheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_harm_firstheadline_names <-
c(# baseline - any subgroup
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # baseline - inaccurate subgroup
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.harm_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  # controls - any subgroup
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # controls - inaccurate subgroup
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.harm_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.harm_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi")

model_list_harm_firstheadline <- lapply(model_list_harm_firstheadline_names, function(x) eval(sym(x)))

names(model_list_harm_firstheadline) <- model_list_harm_firstheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_harm_firstheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Intent to Report Headline as Harmful",
            show_n_obs = TRUE,
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_harm_robustness_checks_firstheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_anyheadline_names <-
c(# baseline - any subgroup
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # baseline - inaccurate subgroup
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  # controls - any subgroup
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_ld",     
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_anysubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_anyheadline_mi",
  # controls - inaccurate subgroup
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld",     
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_ld",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_anyheadline_mi")

model_list_censorship_anyheadline <- lapply(model_list_censorship_anyheadline_names, function(x) eval(sym(x)))

names(model_list_censorship_anyheadline) <- model_list_censorship_anyheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_censorship_anyheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Perception of Headline Removal as Censorship",
            show_n_obs = TRUE,
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_censorship_robustness_checks_anyheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_firstheadline_names <-
c(# baseline - any subgroup
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # baseline - inaccurate subgroup
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.censorship_standard_nocontrols_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_nocontrols_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  # controls - any subgroup
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_ld",     
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_anysubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_anysubgroup_firstheadline_mi",
  # controls - inaccurate subgroup
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_clusteredses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_ld",     
  "mod.censorship_standard_controls_standardses_weighted_anyaccorder_inaccsubgroup_firstheadline_mi",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_ld",
  "mod.censorship_standard_controls_standardses_unweighted_anyaccorder_inaccsubgroup_firstheadline_mi")

model_list_censorship_firstheadline <- lapply(model_list_censorship_firstheadline_names, function(x) eval(sym(x)))

names(model_list_censorship_firstheadline) <- model_list_censorship_firstheadline_names


## ----fig.height=17.1, fig.width=18---------------------------------------------------------------------------
plot_many_models(models = model_list_censorship_firstheadline, 
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.5, 1.55),
            breaks = seq(-0.5, 1.5, 0.25),
            title = "Perception of Headline Removal as Censorship",
            stars = TRUE,
            legend_direction = "vertical") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(1.4, 1.425)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 1.435), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 1.435), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

ggsave("figures/coefficient_plots/coefficient_plot_censorship_robustness_checks_firstheadline_si.png",
       width = 17.1, height = 18, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_coding_robustnesscheck_names <-
c(# baseline
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_robustnesscheck",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi_robustnesscheck",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld_robustnesscheck",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi_robustnesscheck",
  # controls
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_robustnesscheck",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_mi_robustnesscheck",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_ld_robustnesscheck",
  "mod.censorship_standard_controls_clusteredses_weighted_anyaccorder_inaccsubgroup_anyheadline_mi_robustnesscheck")

model_list_censorship_coding_robustnesscheck <- lapply(model_list_censorship_coding_robustnesscheck_names, function(x) eval(sym(x)))

names(model_list_censorship_coding_robustnesscheck) <- model_list_censorship_coding_robustnesscheck_names


## ----fig.width=7.2, fig.height=7.2---------------------------------------------------------------------------
plot_many_models(model_list_censorship_coding_robustnesscheck,
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            colors = c("#08519C", "#4292C6"),
            limits = c(-0.8, 1.55),
            breaks = seq(-0.75, 1.5, 0.25),
            title = "Perception of Headline Removal as Censorship",
            stars = TRUE,
            legend_box = "vertical",
            text_size = "normal") +
  # add information about subset
  geom_brace(aes(c(0.55, 0.77), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(0.775, 0.995), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(1.005, 1.225), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(1.23, 1.45), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(1.55, 1.77), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(1.775, 1.995), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(2.005, 2.225), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(2.23, 2.45), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(2.55, 2.77), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(2.775, 2.995), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(3.005, 3.225), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(3.23, 3.45), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(3.55, 3.77), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(3.775, 3.995), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(4.005, 4.225), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_brace(aes(c(4.23, 4.45), c(0.95, 0.975)), inherit.data = FALSE) +
  geom_text(aes(x = 0.66, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 0.885, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.115, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.34, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.66, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 1.885, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.115, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.34, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +  
  geom_text(aes(x = 2.66, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 2.885, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.115, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.34, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.66, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 3.885, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.115, y = 0.985), hjust = 0, label = "Subset Inaccurate", inherit.aes = FALSE, check_overlap = TRUE) +
  geom_text(aes(x = 4.34, y = 0.985), hjust = 0, label = "All respondents", inherit.aes = FALSE, check_overlap = TRUE)

# dev.size() # check actual device size to save in correct size - 7.2 x 7.2

ggsave("figures/coefficient_plots/coefficient_plot_censorship_robustness_checks_coding_si.png",
       width = 7.2, height = 7.2, dpi = 300, units = "in")


## ----fig.width=9, fig.height=9-------------------------------------------------------------------------------
model_list_remove_consensus_headlines_names <- c(
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_2",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_3",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_4",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_5",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_6",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_7",
  "mod.remove_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_8")

model_list_remove_consensus_headlines <- lapply(model_list_remove_consensus_headlines_names, function(x) eval(sym(x)))

names(model_list_remove_consensus_headlines) <- model_list_remove_consensus_headlines_names

plot_many_models(model_list_remove_consensus_headlines,
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            label_headlines = TRUE,
            colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF"),
            limits = c(-0.75, 1.5),
            breaks = seq(-0.75, 1.5, 0.25),
            title = "Intent to Remove Headline",
            stars = TRUE,
            legend_direction = "vertical",
            text_size = "normal")

ggsave("figures/coefficient_plots/coefficient_plot_remove_consensus_headline_si.png",
       width = 9, height = 9, dpi = 250, units = "in")


## ----fig.width=9, fig.height=9-------------------------------------------------------------------------------
model_list_harm_consensus_headlines_names <- c(
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_2",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_3",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_4",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_5",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_6",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_7",
  "mod.harm_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_8")

model_list_harm_consensus_headlines <- lapply(model_list_harm_consensus_headlines_names, function(x) eval(sym(x)))

names(model_list_harm_consensus_headlines) <- model_list_harm_consensus_headlines_names

plot_many_models(model_list_harm_consensus_headlines,
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            label_headlines = TRUE,
            colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF"),
            limits = c(-0.75, 1.5),
            breaks = seq(-0.75, 1.5, 0.25),
            title = "Intent to Report Headline as Harmful",
            stars = TRUE,
            legend_direction = "vertical",
            text_size = "normal")

ggsave("figures/coefficient_plots/coefficient_plot_harm_consensus_headline_si.png",
       width = 9, height = 9, dpi = 250, units = "in")


## ----fig.width=9, fig.height=9-------------------------------------------------------------------------------
model_list_censorship_consensus_headlines_names <- c(
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_2",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_3",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_4",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_5",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_6",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_7",
  "mod.censorship_standard_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_anyheadline_ld_selected_headlines_8")

model_list_censorship_consensus_headlines <- lapply(model_list_censorship_consensus_headlines_names, function(x) eval(sym(x)))

names(model_list_censorship_consensus_headlines) <- model_list_censorship_consensus_headlines_names

plot_many_models(model_list_censorship_consensus_headlines,
            variables = c("party_idDemocrat" = "Democrat", 
                          "party_idRepublican" = "Republican", 
                          "party_id_dem:headline_pro_dem" = "Democrat x\nPro-Democrat Headline", 
                          "party_id_rep:headline_pro_rep" = "Republican x\nPro-Republican Headline"),
            label_headlines = TRUE,
            colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF"),
            limits = c(-0.75, 1.5),
            breaks = seq(-0.75, 1.5, 0.25),
            title = "Perception of Headline Removal as Censorship",
            stars = TRUE,
            legend_direction = "vertical",
            text_size = "normal")

ggsave("figures/coefficient_plots/coefficient_plot_censorship_consensus_headline_si.png",
       width = 9, height = 9, dpi = 250, units = "in")


## ------------------------------------------------------------------------------------------------------------
# create consensus headline overview dataframe
df_consensus_headline_overview <- tibble(`Number of headlines` = numeric(),
                                         `Number of pro-Democrat headlines` = numeric(),
                                         `Number of pro-Republican headlines` = numeric(),
                                         `Maximum absolute accuracy difference` = numeric())

for (i in nrow(df_consensus_headlines):2) {
  df_consensus_headlines_subset <- df_consensus_headlines %>% 
    slice(1:i)

  n_headlines <- i
  n_pro_dem_headlines <- length(grep("pro_dem", df_consensus_headlines_subset$headline))
  n_pro_rep_headlines <- length(grep("pro_rep", df_consensus_headlines_subset$headline))
  max_abs_diff <- max(df_consensus_headlines_subset$mean_diff_abs)
  
  df_consensus_headline_overview <- df_consensus_headline_overview %>% 
    add_row(`Number of headlines` = n_headlines,
            `Number of pro-Democrat headlines` = n_pro_dem_headlines,
            `Number of pro-Republican headlines` = n_pro_rep_headlines,
            `Maximum absolute accuracy difference` = round(max_abs_diff, 4))
}

# save dataframe as LaTeX table
df_consensus_headline_overview %>% 
    # set rendering options such as alignment
    kable(booktabs = TRUE, format = "latex", align = c("c"),
          caption = "Consensus Headline Overview", label = "consensus-headline-overview") %>% 
    # make table full page width
    kable_styling(latex_options = c("HOLD_position")) %>% 
    # change column width
    column_spec(1:4, width = "1.5in") %>% 
    # make header row bold
    row_spec(0, bold = TRUE) %>% 
    gsub("[H]", "[!htbp]", ., fixed = TRUE) %>% 
   # export table to .tex file
   cat(file = "tables/descriptives/consensus-headline-overview.tex")


## ----regression_tables_main----------------------------------------------------------------------------------
# create grid all main models we want to show (1 row will correspond to 4 models)
df.main_regression_tables <- expand.grid(outcome = c("remove", "harm", "censorship"),
                                         weighting = c("weighted"),
                                         ses = c("clusteredses"),
                                         nas = c("ld"),
                                         specification = c("mainspecification"))

for (i in 1:nrow(df.main_regression_tables)) {
  # create list of models for current table
  model_list <- generate_main_model_list(outcome = df.main_regression_tables[["outcome"]][i], 
                                         weighting = df.main_regression_tables[["weighting"]][i], 
                                         ses = df.main_regression_tables[["ses"]][i], 
                                         nas = df.main_regression_tables[["nas"]][i], 
                                         specification = df.main_regression_tables[["specification"]][i])
  
  # generate table for current models and save to tex file
  custom_texreg(model_list,
                scalebox = 0.8)
}


## ------------------------------------------------------------------------------------------------------------
model_list_remove_without_interaction <- 
  generate_without_interaction_model_list(outcome = "remove")

custom_texreg(model_list_remove_without_interaction,
              custom_header = list("Intent to Remove Headline" = 1:2),
              model_type = "withoutinteraction")


## ------------------------------------------------------------------------------------------------------------
model_list_harm_without_interaction <- 
  generate_without_interaction_model_list(outcome = "harm") 

custom_texreg(model_list_harm_without_interaction,
              custom_header = list("Intent to Report Headline as Harmful" = 1:2),
              model_type = "withoutinteraction")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_without_interaction <- 
  generate_without_interaction_model_list(outcome = "censorship")

custom_texreg(model_list_censorship_without_interaction,
              custom_header = list("Perception of Headline Removal as Censorship" = 1:2),
              model_type = "withoutinteraction")


## ------------------------------------------------------------------------------------------------------------
model_list_remove_by_headline_pro_dem <- 
  generate_by_headline_model_list(outcome = "remove", 
                                  headline_orientation = "pro_dem")

custom_texreg(model_list = model_list_remove_by_headline_pro_dem,
              scalebox = 0.8, 
              caption = "Regression of Intent to Remove Headline on Partisanship by Headline (Pro-Democrat Headlines)",
              custom_header = list("Intent to Remove Headline" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
model_list_remove_by_headline_pro_rep <- 
  generate_by_headline_model_list(outcome = "remove", 
                                  headline_orientation = "pro_rep")
  
custom_texreg(model_list = model_list_remove_by_headline_pro_rep,
              scalebox = 0.8, 
              caption = "Regression of Intent to Remove Headline on Partisanship by Headline (Pro-Republican Headlines)",
              custom_header = list("Intent to Remove Headline" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
model_list_harm_by_headline_pro_dem <- 
  generate_by_headline_model_list(outcome = "harm", 
                                  headline_orientation = "pro_dem")

custom_texreg(model_list = model_list_harm_by_headline_pro_dem,
              scalebox = 0.8, 
              caption = "Regression of Intent to Report Headline as Harmful on Partisanship by Headline (Pro-Democrat Headlines)",
              custom_header = list("Intent to Report Headline as Harmful" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
model_list_harm_by_headline_pro_rep <- 
  generate_by_headline_model_list(outcome = "harm", 
                                  headline_orientation = "pro_rep")
  
custom_texreg(model_list = model_list_harm_by_headline_pro_rep,
              scalebox = 0.8, 
              caption = "Regression of Intent to Report Headline as Harmful on Partisanship by Headline (Pro-Republican Headlines)",
              custom_header = list("Intent to Report Headline as Harmful" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_by_headline_pro_dem <- 
  generate_by_headline_model_list(outcome = "censorship", 
                                  headline_orientation = "pro_dem")

custom_texreg(model_list = model_list_censorship_by_headline_pro_dem,
              scalebox = 0.8, 
              caption = "Regression of Perception of Headline Removal as Censorship on Partisanship by Headline (Pro-Democrat Headlines)",
              custom_header = list("Perception of Headline Removal as Censorship" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
model_list_censorship_by_headline_pro_rep <- 
  generate_by_headline_model_list(outcome = "censorship", 
                                  headline_orientation = "pro_rep")
  
custom_texreg(model_list = model_list_censorship_by_headline_pro_rep,
              scalebox = 0.8, 
              caption = "Regression of Perception of Headline Removal as Censorship on Partisanship by Headline (Pro-Republican Headlines)",
              custom_header = list("Perception of Headline Removal as Censorship" = 1:9),
              custom_coef_map = list.coef_map_byheadline,
              model_type = "byheadline")


## ------------------------------------------------------------------------------------------------------------
custom_texreg(model_list = list(
  `Accuracy Question First` = mod.remove_standard_nocontrols_clusteredses_weighted_firstaccorder_anysubgroup_firstheadline_ld,
  `Accuracy Question Second` = mod.remove_standard_nocontrols_clusteredses_weighted_secondaccorder_anysubgroup_firstheadline_ld, 
  `Accuracy Question Order Interaction` = mod.remove_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld),
              scalebox = 0.6,
              caption = "Regression of Intent to Remove Headline on Partisanship and Alignment",
              custom_header = list("Intent to Remove Headline" = 1:3),
              custom_coef_map = list.coef_map_tripleinteraction,
              model_type = "tripleinteraction")


## ------------------------------------------------------------------------------------------------------------
custom_texreg(model_list = list(
  `Accuracy Question First` = mod.harm_standard_nocontrols_clusteredses_weighted_firstaccorder_anysubgroup_firstheadline_ld,
  `Accuracy Question Second` = mod.harm_standard_nocontrols_clusteredses_weighted_secondaccorder_anysubgroup_firstheadline_ld, 
  `Accuracy Question Order Interaction` = mod.harm_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld),
              scalebox = 0.6,
              caption = "Regression of Intent to Report Headline as Harmful on Partisanship and Alignment",
              custom_header = list("Intent to Report Headline as Harmful" = 1:3),
              custom_coef_map = list.coef_map_tripleinteraction,
              model_type = "tripleinteraction")


## ------------------------------------------------------------------------------------------------------------
custom_texreg(model_list = list(
  `Accuracy Question First` = mod.censorship_standard_nocontrols_clusteredses_weighted_firstaccorder_anysubgroup_firstheadline_ld,
  `Accuracy Question Second` = mod.censorship_standard_nocontrols_clusteredses_weighted_secondaccorder_anysubgroup_firstheadline_ld, 
  `Accuracy Question Order Interaction` = mod.censorship_tripleinteraction_nocontrols_clusteredses_weighted_anyaccorder_anysubgroup_firstheadline_ld),
              scalebox = 0.6,
              caption = "Regression of Perception of Headline Removal as Censorship on Partisanship and Alignment",
              custom_header = list("Perception of Headline Removal as Censorship" = 1:3),
              custom_coef_map = list.coef_map_tripleinteraction,
              model_type = "tripleinteraction")


## ------------------------------------------------------------------------------------------------------------
generate_mediation_table(data = df.experiment_long,
                         controls = vec.controls,
                         subset_condition = "party_id == 'Democrat'",
                         label = "mediation-democrats",
                         caption = "Effect of Alignment Mediated by Accuracy for Democrats (4-Point Accuracy Variable)",
                         footnote = "Mediation models were run with standard errors clustered on participants and without weigthing observations using a dataset in which missing values were addressed using listwise deletion.",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_table(data = df.experiment_long,
                         controls = vec.controls,
                         mediator = "accuracy_binary",
                         subset_condition = "party_id == 'Democrat'",
                         label = "mediation-democrats-accuracybinary",
                         caption = "Effect of Alignment Mediated by Accuracy for Democrats (Binary Accuracy Variable)",
                         footnote = "Mediation models were run with standard errors clustered on participants and without weigthing observations using a dataset in which missing values were addressed using listwise deletion.",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_table(data = df.experiment_long,
                         controls = vec.controls,
                         outcomes = c("remove", "harm", "censorship"),
                         label = "mediation-all-partyid",
                         treatment = "party_id_dem",
                         caption = "Effect of Partisanship (Democrat as Treatment) Mediated by Accuracy (4-Point Accuracy Variable)",
                         footnote = "Mediation models were run with standard errors clustered on participants and without weigthing observations using a dataset in which missing values were addressed using listwise deletion.",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_table(data = df.experiment_long,
                         controls = vec.controls,
                         mediator = "accuracy_binary",
                         outcomes = c("remove", "harm", "censorship"),
                         label = "mediation-all-accuracybinary-partyid",
                         treatment = "party_id_dem",
                         caption = "Effect of Partisanship (Democrat as Treatment) Mediated by Accuracy (Binary Accuracy Variable)",
                         footnote = "Mediation models were run with standard errors clustered on participants and without weigthing observations using a dataset in which missing values were addressed using listwise deletion.",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_sensitivity_plots(data = df.experiment_long,
                         controls = vec.controls,
                         subset_condition = "party_id == 'Democrat'",
                         label = "mediation-democrats",
                         caption = "Effect of Alignment Mediated by Accuracy for Democrats (4-Point Accuracy Variable)",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_sensitivity_plots(data = df.experiment_long,
                         controls = vec.controls,
                         mediator = "accuracy_binary",
                         subset_condition = "party_id == 'Democrat'",
                         label = "mediation-democrats-accuracybinary",
                         caption = "Effect of Alignment Mediated by Accuracy for Democrats (Binary Accuracy Variable)",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_sensitivity_plots(data = df.experiment_long,
                         controls = vec.controls,
                         outcomes = c("remove", "harm", "censorship"),
                         label = "mediation-all-partyid",
                         treatment = "party_id_dem",
                         caption = "Effect of Partisanship (Democrat as Treatment) Mediated by Accuracy (4-Point Accuracy Variable)",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
generate_mediation_sensitivity_plots(data = df.experiment_long,
                         controls = vec.controls,
                         mediator = "accuracy_binary",
                         outcomes = c("remove", "harm", "censorship"),
                         label = "mediation-all-accuracybinary-partyid",
                         treatment = "party_id_dem",
                         caption = "Effect of Partisanship (Democrat as Treatment) Mediated by Accuracy (Binary Accuracy Variable)",
                         cluster = TRUE)


## ------------------------------------------------------------------------------------------------------------
df.censor_tokens_congress <- read_csv("data/intermediate/speech/df.censor_tokens_congress.csv",
                                      show_col_types = FALSE)


## ------------------------------------------------------------------------------------------------------------
df.party_division <- read_csv("data/input/party_division/congress_party_division.csv", show_col_types = FALSE) %>% 
  rename(house_majority = `House Majority`,
          senate_majority = `Senate Majority`,
          congress = Congress) %>% 
  mutate(congress = as.integer(str_extract(congress, "\\d+")),
         senate_majority = str_extract(senate_majority, "Democrat|Republican"),
         house_majority = str_extract(house_majority, "Democrat|Republican")) %>% 
  # special case 107th Congress
  mutate(senate_majority = if_else(congress == 107, "Democrat", senate_majority)) %>% 
  mutate(house_label = if_else(house_majority == "Democrat", "D", "R"),
         house_color = if_else(house_majority == "Democrat", "#377EB8", "#E41A1C"),
         senate_label = if_else(senate_majority == "Democrat", "D", "R"),
         senate_color = if_else(senate_majority == "Democrat", "#377EB8", "#E41A1C")) %>% 
  filter(congress %in% 46:116)


## ----fig.width=17, fig.height=9------------------------------------------------------------------------------
# letters indicate the majority party in a given Congress (darker font for House, lighter font for Senate)
ggplot(df.censor_tokens_congress,
       mapping = aes(x = congress, y = total_censor_token_count, color = party, group = party)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_text(data = df.party_division, mapping = aes(x = congress, y = 1.065 * max(df.censor_tokens_congress$total_censor_token_count), label = house_label), inherit.aes = FALSE, color = df.party_division$house_color) +
  geom_text(data = df.party_division, mapping = aes(x = congress, y = 1.045 * max(df.censor_tokens_congress$total_censor_token_count), label = senate_label), inherit.aes = FALSE, color = df.party_division$senate_color, alpha = 0.5) +
  scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  scale_x_continuous(breaks = seq(46, 116, 1)) +
  coord_cartesian(expand = FALSE, xlim = c(45, 117), ylim = c(0, 1.1* max(df.censor_tokens_congress$total_censor_token_count))) +
  labs(x = "Congress",
       y = "Count of tokens containing \"censor\"",
       color = "Party",
       caption = "") +
  theme(legend.position = "bottom")

ggsave("figures/paper/congress_speech_plot/congress_speech_plot.png", 
       width = 17, height = 9, dpi = 120)


## ----sessioninfo---------------------------------------------------------------------------------------------
sessionInfo()


## ----include=FALSE, eval=FALSE-------------------------------------------------------------------------------
## knitr::purl(input = "code/manuscript_code.Rmd",
##             output = "code/manuscript_code.R",
##             documentation = 1)

