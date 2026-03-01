# title: Functions
# date: 09/21/2023
# description: contains functions for data cleaning and analysis

# ------------------------- final_models.Rmd --------------------------------- #

# Function lm_mi
# Returns texreg model object from multiple imputation models
# params: imputations: list with multiple imputed dataframes
#         model_expression: expression for the model to run that does not 
#                           include a data argument (e.g., expr(lm(y ~ x)))
# returns: texreg model object with model information
lm_mi <- function(imputations, model_expression) {
  # run specified model on each imputed dataframe
  fits <- map(imputations, function(data) {
    base::eval(model_expression, env = data)
  })
  
  # pool the results across model
  pooled <- summary(mice::pool(fits), rule = "rubin1987")
  
  # add goodness of fit statistics
  r.squared <- mice::pool.r.squared(mice::pool(fits), adjusted = FALSE)
  adj.r.squared <- mice::pool.r.squared(mice::pool(fits), adjusted = TRUE)
  
  # number of observations and clusters are the same for all models
  nobs <- fits$imp1$nobs
  nclusters_value <- if (!fits$imp1$clustered) { NULL } else { fits$imp1$nclusters }
  nclusters_name <- if (!fits$imp1$clustered) { NULL } else { "N Clusters" }
  nclusters_dec <- if (!fits$imp1$clustered) { NULL } else { FALSE }
  
  # create texreg object
  model <- createTexreg(coef.names = as.character(pooled$term), 
                        coef = pooled$estimate, 
                        se = pooled$std.error, 
                        pvalues = pooled$p.value,
                        model.name = "Multiple Imputation",
                        gof.names = c("R^2", "Adj. R^2", "Num. obs.", nclusters_name),
                        gof = c(r.squared["est"], adj.r.squared["est"], nobs, nclusters_value),
                        gof.decimal = c(TRUE, TRUE, FALSE, nclusters_dec))
  
  return(model)
}

# Function generate_regression_models
# Generates regression models based on a dataframe with desired specifications
# and a vector of dataframe names with dataframes to run regressions on.
# params: specification_data: dataframe with desired specifications (one row
#                             corresponds to one regression to run)
#         experiment_data: vector of dataframe names indicating which dataframes 
#                          to run regressions on (first element is dataframe for
#                          regressions using listwise deletion,
#                          second element is list of imputed datasets)
#         robustness_check: indicates whether models are robustness check for
#                           censorship coding. If so, the model name will end in
#                           _robustnesscheck (default is FALSE)
#         selected_headlines: whether the data contains data for all headlines
#                             or subset (default is NULL for all headlines)
generate_regression_models <- function(specification_data, experiment_data, robustness_check = FALSE, selected_headlines = NULL) {
  # create a model for each specification
  for (i in 1:nrow(specification_data)) {
    # specify model name
    model_name <- paste0("mod.", paste(unlist(specification_data[i,]), collapse = "_"))
    if (robustness_check) { model_name <- paste0(model_name, "_robustnesscheck") }
    if (!is.null(selected_headlines)) { model_name <- paste0(model_name, "_", selected_headlines) }
    # specify main regressors
    rhs <- "0 + party_id_dem:headline_pro_dem + party_id_rep:headline_pro_rep + party_id"
    # add controls if applicable
    if (specification_data[["control_vars"]][i] == "controls") {
      rhs <- paste(rhs, paste(vec.controls, collapse = " + "), sep = "+")
    }
    # specify formula
    formula <- paste(specification_data[["outcome"]][i], rhs, sep = " ~ ")
    # set SE options
    se_options <- if (specification_data[["ses"]][i] == "clusteredses") { "id" } else { NULL}
    # set weigthing options
    weight_options <- if (specification_data[["weighting"]][i] == "weighted") { "weight" } else { NULL }
    # set subset options
    subset_accuracy_order <- 
      if (specification_data[["accuracy_order"]][i] == "anyaccorder") { 
        NULL 
      } else if (specification_data[["accuracy_order"]][i] == "firstaccorder") { 
        "accuracy_order == 1" 
      } else if (specification_data[["accuracy_order"]][i] == "secondaccorder") { 
        "accuracy_order == 0" 
      }
    subset_accuracy_binary <- 
      if (specification_data[["accuracy_subgroup"]][i] == "anysubgroup") { 
        NULL
      } else if (specification_data[["accuracy_subgroup"]][i] == "inaccsubgroup") {
        "accuracy_binary == 0"
      } else if (specification_data[["accuracy_subgroup"]][i] == "accsubgroup") { 
        "accuracy_binary == 1" }
    subset_headlines <- if (specification_data[["headlines"]][i] == "anyheadline") { NULL } else if (specification_data[["headlines"]][i] == "firstheadline") { "headline_order == 0" } else if (specification_data[["headlines"]][i] == "secondheadline") { "headline_order == 1" }
    subsets <- paste(c(subset_accuracy_order, subset_accuracy_binary, subset_headlines), collapse = " & ", sep = "")
    subset_options <- if (subsets == "") { NULL } else { subsets }
    
    # run regression
    if (specification_data[["nas"]][i] == "ld") { # listwise deletion
      assign(model_name,
             base::eval(parse(text = paste0("lm_robust(data = ", experiment_data[1], 
                                            ", formula = ", formula,
                                            ", cluster = ", se_options,
                                            ", weights = ", weight_options,
                                            ", subset = ", subset_options, ")"))),
             envir = globalenv())
    } else { # multiple imputation
      assign(model_name,
             lm_mi(imputations = eval(parse(text = experiment_data[2])), 
                   model_expression = parse(text = paste0("lm_robust(formula = ", formula, 
                                                          ", cluster = ", se_options,
                                                          ", weights = ", weight_options,
                                                          ", subset = ", subset_options, ")"))),
             envir = globalenv())    
    }
  }
}

# ------------------------- manuscript_code.Rmd ------------------------------ #

# Function plot_models 
# Plots model coefficients for multiple models.
# params: models: named list of model objects whose coefficients to plot
#         model_labels: vector of model labels in the order of the models
#         variables: named vector of variables to plot (the vector contains
#                    the labels, the vector names are the original variable names)
#                    (optional, all variables will be plotted by default)
#         colors: vector of colors for coefficients (default is blue)
#         limits: limits for the coefficients to plot
#         breaks: breaks for the coefficient axis
#         stars: indicates whether stars should be added for significance thresholds
#         significance_threshold: 3 significance thresholds if stars should be 
#                                 plotted (default is 0.001, 0.01, 0.05)
#         title: title of the plot (default is outcome variable name)
#         legend: whether legend should be plotted (default is TRUE)
#         tag: tag to show on upper left corner (optional)
# returns: plot of coefficients and 95%-CI bounds
plot_models <- function(models, model_labels = NULL, variables = NULL, colors = NULL, 
                        limits = NULL, breaks = NULL, title = NULL, stars = FALSE, 
                        significance_thresholds = c(0.001, 0.01, 0.05), legend = TRUE, tag = NULL) {
  # combine data for all models into one dataframe
  data <- tibble()
  for (i in 1:length(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    model_type <- str_match(model_name, "(nocontrols|controls)")[,2]
    model_na_approach <- str_match(model_name, "(ld|mi)")[,2]
    model_weighting <- str_match(model_name, "(weighted|unweighted)")[,2]
    model_seclustering <- str_match(model_name, "(clusteredses|standardses)")[,2]
    
    if (model_na_approach == "ld") { # lm_robust model
      # get coefficient information
      data_tmp <- broom::tidy(model) %>% 
        dplyr::select(-c(statistic, df))
      # add number of observations
      data_tmp$n_obs <- paste("N =", comma(model$nobs))
      # add model name to each observation
      data_tmp$model_id <- names(models)[i]
    } else if (model_na_approach == "mi") { # texreg model
      data_tmp <- tibble(term = model@coef.names, 
                         estimate = model@coef, 
                         std.error = model@se, 
                         p.value = model@pvalues, 
                         n_obs = paste("N =", comma(model@gof[3])),
                         model_id = names(models)[i],
                         outcome = str_match(model_name, "mod.(.*?)_")[,2]) %>% 
        mutate(conf.low = estimate - qnorm(0.975) * std.error,
               conf.high = estimate + qnorm(0.975) * std.error)
    } else {
      warning("plot_models can only handle lm_robust and texreg models.")
    }
    
    data_tmp <- data_tmp %>% 
      mutate(model_na_approach = model_na_approach,
             model_weighting = model_weighting,
             model_seclustering = model_seclustering,
             model_type = model_type)
    
    # add temporary data to all data
    data <- rbind(data, data_tmp)
  }
  
  # plot all variables if none specified or only labels provided
  if (is.null(variables) | is.null(names(variables))) {
    variables <- unique(data$term)
    names(variables) <- variables
  }
  
  # filter coefficients
  data <- data %>% 
    filter(term %in% names(variables))
  
  # order variables in the order they were provided or appear in summary
  data$term <- as.character(data$term)
  data$term <- factor(data$term, levels = rev(names(variables)))
  
  # order models in the order they were provided or appear in summary
  data$model_id <- as.character(data$model_id)
  data$model_id <- factor(data$model_id, levels = rev(names(models)))
  
  if (!is.null(model_labels)) {
    data <- data %>% 
      mutate(model_id = recode(model_id, !!!model_labels))
  }
  
  # keep number of observations only once
  data <- data %>%
    mutate(n_obs = if_else(term == rev(levels(data$term))[1], n_obs, ""))
  
  if (stars) {
    # prepare column with p-values and stars for printing
    data <- data %>%
      mutate(p.labels = case_when(
        p.value < significance_thresholds[1] ~ paste0(format(round(estimate, 2), nsmall = 2), " ***"),
        p.value < significance_thresholds[2] ~ paste0(format(round(estimate, 2), nsmall = 2), " **"),
        p.value < significance_thresholds[3] ~ paste0(format(round(estimate, 2), nsmall = 2), " *"),
        TRUE ~ paste0(format(round(estimate, 2), nsmall = 2))))
  }
  
  plot <- ggplot(data = data,
                 mapping = aes(x = term,
                               y = estimate,
                               color = fct_rev(model_id),
                               group = model_id)) +
    # add line at 0 intercept
    geom_hline(aes(yintercept = 0), color = "grey75", linewidth = 0.5) +
    # plot estimate and 95% confidence intervals
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), fatten = 0.75, 
                    position = position_dodge(width = 0.65)) +
    # add estimate label
    geom_text(aes(label = if (stars) { p.labels } else { format(round(estimate, 2), nsmall = 2) }, 
                  y = conf.high, hjust = -0.3), 
              position = position_dodge(width = 0.65), size = 3) +
    # set scale limits and breaks
    scale_y_continuous(limits = limits, breaks = breaks) +
    # set value colors
    { if (!is.null(colors)) { 
        scale_color_manual(values = rev(colors)) 
      } else { 
        scale_color_manual(values = rev(brewer.pal(n = 9, "Blues"))[1:length(unique(data$term))]) 
      } } +
    # add variable labels
    scale_x_discrete(labels = variables)
  
  if (is.null(limits)) {
    limits <- layer_scales(plot)$y$range$range
  }
  
  plot <- plot +
    # add number of observations
    geom_text(aes(label = n_obs, 
                  y = limits[1] + mean(range(limits)) * 0.0025), 
              vjust = -1, hjust = 0, 
              position = position_dodge(width = 0.65), size = 3) +
    coord_flip() +
    labs(title = if_else(is.null(title), unique(data$outcome), title),
         x = "",
         y = "Coefficient Estimate",
         color = "Model",
         tag = tag) +
    theme_bw() +
    theme(legend.position = if (legend) { "bottom" } else { "none" },
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.4, "cm"),
          legend.text = element_text(size = 11),
          legend.title = element_text(face = "bold"),
          axis.text.y = element_text(size = 11))
  
  return(plot)
}

# Function plot_many_models
# Plots model coefficients for many models.
# params: models: named list of model objects whose coefficients to plot
#         model_labels: vector of model labels in the order of the models
#         variables: named vector of variables to plot (the vector contains
#                    the labels, the vector names are the original variable names)
#                    (optional, all variables will be plotted by default)
#         colors: vector of colors for coefficients (default is blue)
#         limits: limits for the coefficients to plot
#         breaks: breaks for the coefficient axis
#         stars: indicates whether stars should be added for significance thresholds
#         significance_threshold: 3 significance thresholds if stars should be 
#                                 plotted (default is 0.001, 0.01, 0.05)
#         label_weighting: whether model weighting should be labeled (default 
#                          is TRUE) 
#         label_headlines: whether headline model should be labeled instead of 
#                          SEs (only applies to consensus model headlines,
#                          default is FALSE)
#         show_n_obs: whether to show number of observations (default is TRUE)
#         title: title of the plot (default is outcome variable name)
#         legend: whether legend should be plotted (default is TRUE)
#         legend_direction: direction of legend (default is "horizontal")
#         legend_box: direction of legend box (default is "horizontal")
#         text_size: font size on axis and legend (default is "large",
#                    for smaller font use "normal")
#         tag: tag to show on upper left corner (optional)
# returns: plot of coefficients and 95%-CI bounds
plot_many_models <- function(models, model_labels = NULL, variables = NULL, colors = NULL,
                             limits = NULL, breaks = NULL, title = NULL, stars = FALSE, 
                             significance_thresholds = c(0.001, 0.01, 0.05), 
                             label_weighting = TRUE, label_headlines = FALSE,
                             show_n_obs = TRUE, legend = TRUE, legend_direction = "horizontal",
                             legend_box = "horizontal", text_size = "large", tag = NULL) {
  # combine data for all models into one dataframe
  data <- tibble()
  for (i in 1:length(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    model_type <- str_match(model_name, "(nocontrols|controls)")[,2]
    model_na_approach <- str_match(model_name, "(ld|mi)")[,2]
    model_weighting <- str_match(model_name, "(weighted|unweighted)")[,2]
    model_seclustering <- str_match(model_name, "(clusteredses|standardses)")[,2]
    
    if (label_headlines) {
      model_headlines = str_match(model_name, "(selected_headlines_2|selected_headlines_3|selected_headlines_4|selected_headlines_5|selected_headlines_6|selected_headlines_7|selected_headlines_8)")[,2]
      model_headlines <- paste0(gsub("selected_headlines_", "", model_headlines), " consensus headlines")
    }
    
    if (model_na_approach == "ld") { # lm_robust model
      # get coefficient information
      data_tmp <- broom::tidy(model) %>% 
        dplyr::select(-c(statistic, df))
      # add number of observations
      data_tmp$n_obs <- paste("N =", comma(model$nobs))
      # add model name to each observation
      data_tmp$model_id <- names(models)[i]
    } else if (model_na_approach == "mi") { # texreg model
      data_tmp <- tibble(term = model@coef.names, 
                         estimate = model@coef, 
                         std.error = model@se, 
                         p.value = model@pvalues, 
                         n_obs = paste("N =", comma(model@gof[3])),
                         model_id = names(models)[i],
                         outcome = str_match(model_name, "mod.(.*?)_")[,2]) %>% 
        mutate(conf.low = estimate - qnorm(0.975) * std.error,
               conf.high = estimate + qnorm(0.975) * std.error)
    } else {
      warning("plot_models can only handle lm_robust and texreg models.")
    }
    
    data_tmp <- data_tmp %>% 
      mutate(model_na_approach = model_na_approach,
             model_weighting = model_weighting,
             model_seclustering = model_seclustering,
             model_type = model_type)
    
    if (label_headlines) {
      data_tmp$model_headlines <- model_headlines
    }
    
    # add temporary data to all data
    data <- rbind(data, data_tmp)
  }
  
  data <- data %>% 
    mutate(model_na_approach = if_else(model_na_approach == "ld", "listwise deletion", "multiple imputation"),
           model_seclustering = if_else(model_seclustering == "standardses", "standard", "clustered on participant"),
           model_type = if_else(model_type == "controls", "controls", "baseline"))
  
  # plot all variables if none specified or only labels provided
  if (is.null(variables) | is.null(names(variables))) {
    variables <- unique(data$term)
    names(variables) <- variables
  }
  
  # filter coefficients
  data <- data %>% 
    filter(term %in% names(variables))
  
  # order variables in the order they were provided or appear in summary
  data$term <- as.character(data$term)
  data$term <- factor(data$term, levels = rev(names(variables)))
  
  # order models in the order they were provided or appear in summary
  data$model_id <- as.character(data$model_id)
  data$model_id <- factor(data$model_id, levels = rev(names(models)))
  
  if (!is.null(model_labels)) {
    data <- data %>% 
      mutate(model_id = recode(model_id, !!!model_labels))
  }
  
  # keep number of observations only once
  data <- data %>%
    mutate(n_obs = if_else(term == rev(levels(data$term))[1], n_obs, ""))
  
  if (stars) {
    # prepare column with p-values and stars for printing
    data <- data %>%
      mutate(p.labels = case_when(
        p.value < significance_thresholds[1] ~ paste0(format(round(estimate, 2), nsmall = 2), " ***"),
        p.value < significance_thresholds[2] ~ paste0(format(round(estimate, 2), nsmall = 2), " **"),
        p.value < significance_thresholds[3] ~ paste0(format(round(estimate, 2), nsmall = 2), " *"),
        TRUE ~ paste0(format(round(estimate, 2), nsmall = 2))))
  }
  
  n_colors <- if (label_headlines) { length(unique(data$model_headlines)) } else { length(unique(data$term)) }
  
  plot <- ggplot(data = data,
                 mapping = aes(x = term,
                               y = estimate,
                               color = if (label_headlines) { model_headlines } else {  model_type },
                               group = model_id)) +
    # add line at 0 intercept
    geom_hline(aes(yintercept = 0), color = "grey75", linewidth = 0.5) +
    # plot estimate and 95% confidence intervals
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high,
                        alpha = model_na_approach,
                        linetype = if (label_weighting) { model_weighting } else { "solid" },
                        shape = model_seclustering),
                    fatten = 1, size = 1.75,
                    position = position_dodge(width = 0.9)) +
    # add estimate label
    geom_text(aes(label = if (stars) { p.labels } else { format(round(estimate, 2), nsmall = 2) },
                  alpha = model_na_approach,
                  y = conf.high, hjust = -0.3), 
              position = position_dodge(width = 0.9), size = 3) +
    # set scale limits and breaks
    scale_y_continuous(limits = limits, breaks = breaks) +
    # set alpha values
    scale_alpha_manual(values = c(1, 0.7)) +
    # set linetype
    scale_linetype_manual(values = if (label_weighting & length(unique(data$model_weighting)) > 1) { c("dashed", "solid") } else {"solid"}) +
    # set value colors
    { if (!is.null(colors)) { scale_color_manual(values = colors) } else { scale_color_manual(values = rev(brewer.pal(n = 9, "Blues"))[1:n_colors]) } } +
    guides(color = guide_legend(override.aes = list(size = 0.75)),
           shape = guide_legend(override.aes = list(size = 0.75)), 
           alpha = guide_legend(override.aes = list(size = 0.75)), 
           linetype = if (label_weighting) { guide_legend(reverse = TRUE, override.aes = list(shape = NA)) } else { "none" } ) +
    # add variable labels
    scale_x_discrete(labels = variables)
  
  if (is.null(limits)) {
    limits <- layer_scales(plot)$y$range$range
  }
  
  plot <- plot +
    # add number of observations
    geom_text(aes(label = if (show_n_obs) { n_obs } else { "" }, 
                  y = limits[1] + mean(range(limits)) * 0.0025,
                  x = 4, alpha = model_na_approach), 
              hjust = 0,
              position = position_dodge(width = 0.9), size = 3) + 
    coord_flip() +
    labs(title = if_else(is.null(title), unique(data$outcome), title),
         x = "",
         y = "Coefficient Estimate",
         color = "Model",
         shape = "SEs",
         linetype = "Weighting",
         alpha = "Missing Values",
         tag = tag) +
    theme_bw() +
    theme(legend.position = if (legend) { "bottom" } else { "none" },
          legend.direction = legend_direction,
          legend.box = legend_box,
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.4, "cm"))
  
  if (text_size == "normal") {
    plot <- plot +
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 11))
  } else if (text_size == "large") {
    plot <- plot +
      theme(plot.title = element_text(size = 24),
            legend.text = element_text(size = 18),
            legend.title = element_text(face = "bold", size = 18),
            axis.text.y = element_text(size = 18),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 16))
  }
  
  return(plot)
}

# Function generate_balance_table
# Generates and exports LaTeX code for a balance table.
# params: data: data for which to generate table
#         covariates: character vector of covariates to compare
#         treatment: treatment variable for which to check balance
#         caption: table caption (default is "Balance Table")
#         footnote: footnote for table (default is "Note: p-values for result 
#                   from a joint F-test for continuous variables and from a 
#                   Chi-squared test for categorical variables.")
#         label: label for table, also serves as file name when table is
#                exported to the subdirectory tables/balance
# returns: texreg model object with model information
generate_balance_table <- function(data, covariates, treatment,
                                   caption = "Balance Table", 
                                   footnote = c("p-values result from a joint F-test for continuous variables and from a Chi-squared test for categorical variables."), 
                                   label) {
  # create balance table using tableone package
  table <- tableone::CreateTableOne(vars = covariates, 
                                    strata = treatment, 
                                    data = data, 
                                    test = TRUE)
  
  printed_table <- print(table, smd = TRUE, printToggle = FALSE)
  
  # replace spaces by . . .
  rownames(printed_table)  <- gsub("   ", "... ", 
                                   rownames(printed_table), 
                                   fixed = TRUE)
  
  # convert to dataframe for customization
  df.table <- printed_table %>% 
    as.data.frame() %>% 
    dplyr::select(-test) %>% 
    rename(`p-value` = p) %>% 
    rownames_to_column("Variable") %>% 
    mutate(Variable = case_when(Variable == "n" ~ "Number of Observations",
                                Variable == "Social Media Most Common Newsformat = Yes (%)" ~ "Social Media Most Common News Format = Yes (%)",
                                Variable == "Social Media Post Flagged = Yes, I have experienced or done this (%)" ~ "Social Media Post Flagged = Yes (%)",
                                Variable == "Social Media Post Removed = Yes, I have experienced or done this (%)" ~ "Social Media Post Removed = Yes (%)",
                                TRUE ~ Variable)) %>% 
    mutate(Variable = gsub("%", "N (%)", Variable)) %>% 
    # reorder columns in logical order
    relocate(2, .after = 3)
  
  if (treatment == "Accuracy Order") {
    df.table <- df.table %>% 
      rename(First = `Accuracy question first`, Second = `Accuracy question second`)
  }
  
  # add header
  header <- c(1, 2, ncol(df.table) - 3) # set col spans
  names(header) <- c(" ", if (treatment == "Accuracy Order") { "Accuracy Question Order" } else { treatment }, " ") # set names
  
  table <- df.table %>% 
    # set rendering options such as alignment
    kable(booktabs = TRUE, format = "latex", align = c("l|r|r|r|r"), linesep = "", caption = caption, label = label) %>% 
    # make table full page width
    kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
    # make header row bold
    row_spec(0, bold = TRUE, align = c("c")) %>% 
    # add treatment variable in header
    add_header_above(header, bold = TRUE) %>% 
    # add footnote
    footnote(general = c(paste0("\\\\small{\\\\textit{Note:} ", footnote, "}")),
             general_title = paste0(""),
             threeparttable = TRUE,
             escape = FALSE) %>% 
    gsub("[H]", "[!htbp]", ., fixed = TRUE) %>% 
    gsub("<", "$<$", ., fixed = TRUE)
  
  # export table to .tex file
  cat(table, file = paste0("tables/balance/", label, ".tex", sep = ""))
}

# Function: generate_main_model_list
# Generates named list of main models that match the outcome, weighting, SEs,
# missing value approach and model specification provided. The list's names
# correspond to the elements in the list.
# params: outcome: outcome variable in the regression models 
#                  ("remove", "harm", or "censorship")
#         weighting: specifies data weighting ("weighted" or "unweighted")
#         ses: specifies standard error clustering ("standardses" for 
#              non-clustered SEs, or "clusteredses" for SEs clustered on 
#              participants)
#         nas: specifies whether NAs are dealt with by listwise deletion ("ld")
#              or multiple imputation ("mi")
#         specification: whether the model is a main model or a robustness check
#                        with first shown headlines ("mainspecification" or 
#                        "robustnesscheck")
#         robustness_check: indicates whether models are robustness check for
#                           censorship coding. If so, the model name will end in
#                           _robustnesscheck (default is FALSE)
#         selected_headlines: whether the data contains data for all headlines
#                             or subset (default is NULL for all headlines)
generate_main_model_list <- function(outcome, weighting, ses, nas, specification, robustness_check = FALSE, selected_headlines = NULL) {
  headlines <- if (specification == "mainspecification") { rep("anyheadline", 4) } else { rep("firstheadline", 4) }
  accuracy_order <- rep("anyaccorder", 4)
  accuracy_subgroup <- c(rep("anysubgroup", 2), rep("inaccsubgroup", 2))
  control_vars <- rep(c("nocontrols", "controls"), 2)
  model_list <- list()
  
  for (i in 1:4) {
    model_name <- paste0("mod.", outcome, "_standard", "_", control_vars[i], "_", 
                         ses, "_", weighting, "_", accuracy_order[i], "_", 
                         accuracy_subgroup[i], "_", headlines[i], "_", nas)
    if (robustness_check) { model_name <- paste0(model_name, "_robustnesscheck") }
    if (!is.null(selected_headlines)) { model_name <- paste0(model_name, "_", selected_headlines) }
    model_list[[i]] <- eval(parse(text = model_name))
    names(model_list)[i] <- model_name
  }
  return(model_list)
}

# Function generate_without_interaction_model_list
# Generates models without interaction given an outcome variable.
# params: outcome: outcome variable of the models
# returns: model list
generate_without_interaction_model_list <- function(outcome) {
  model_list <- list()
  subgroups <- c("anysubgroup", "inaccsubgroup")
  for (i in 1:2) {
    model_name <- paste0("mod.", outcome, "_", subgroups[i], "_clusteredses_weighted_ld_without_interaction")
    model_list[[i]] <- eval(parse(text = model_name))
    names(model_list)[i] <- model_name
  }
  return(model_list)
}

# Function generate_by_headline_model_list
# Generates models disaggregated by headline given an outcome variable
# and the headline orientation.
# params: outcome: outcome variable of the models
#         headline_orientation: whether models are for pro-Democrat ("pro_dem")
#                               or pro-Republican ("pro_rep") headlines
# returns: model list
generate_by_headline_model_list <- function(outcome, headline_orientation) {
  model_list <- list()
  for (i in 1:9) {
    model_name <- paste0("mod.", outcome, "_anysubgroup_clusteredses_weighted_ld_headline_", 
                         headline_orientation, "_", i)
    model_list[[i]] <- eval(parse(text = model_name))
    names(model_list)[i] <- model_name
  }
  return(model_list)
}

# Function custom_texreg
# Prints highly customized tables for the supplementary materials.
# params: model_list: named list of 8 models to show in table. Names must 
#                     contain all relevant model information. All models should be 
#                     for the same outcome and contain models in the following
#                     order: no controls and any accuracy question order, 
#                     controls and any accuracy question order, 
#                     no controls when accuracy question comes first,
#                     controls when accuracy question comes first.
#                     This order is repeated first for models with all observations,
#                     then for models with the inaccurate subgroup only.
#         robustness_check: indicates whether models are robustness check for
#                           censorship coding. If so, the model name will end in
#                           _robustnesscheck (default is FALSE)
#         selected_headlines: whether the data contains data for all headlines
#                             or subset (default is NULL for all headlines)
#         scalebox: factor by which table should be scaled (default is NULL)
#         caption: table caption (automatically inferred if not provided)
#         model_type: type of regression model (default is "mainmodel")
#         custom_coef_map: custom coefficient name map
#         custom_header: header to display above models (default is NULL)
# returns: customized LaTeX table
custom_texreg <- function(model_list, robustness_check = FALSE, 
                          selected_headlines = NULL, scalebox = NULL, 
                          caption = NULL, model_type = "mainmodel",
                          custom_coef_map = list.coef_map, custom_header = NULL) {
  # get model type and y-axis label
  if (model_type == "tripleinteraction" | model_type == "accmodel" | model_type == "accbinmodel") {
    if (model_type == "tripleinteraction") {
      y_label <- str_match(caption, "Regression of (.*) on Partisanship and Alignment")[2]
    } else if (model_type == "accmodel" | model_type == "accbinmodel") {
      y_label <- str_match(caption, "Regression of (.*) on Partisanship, Alignment and Accuracy")[2]
    }
    
    y_label_short <- case_when(y_label == "Intent to Remove Headline" ~ "remove",
                               y_label == "Intent to Report Headline as Harmful" ~ "harm",
                               y_label == "Perception of Headline Removal as Censorship" ~ "censorship")   
  } else {
    y_label <- case_when(grepl("remove", names(model_list)[[1]]) ~ "Intent to Remove Headline",
                         grepl("harm", names(model_list)[[1]]) ~ "Intent to Report Headline as Harmful",
                         grepl("censorship", names(model_list)[[1]]) ~ "Perception of Headline Removal as Censorship")
    
    y_label_short <- case_when(grepl("remove", names(model_list)[[1]]) ~ "remove",
                               grepl("harm", names(model_list)[[1]]) ~ "harm",
                               grepl("censorship", names(model_list)[[1]]) ~ "censorship")
  }
  
  # add headline orientation information for models by headline
  if (model_type == "byheadline") {
    y_label_short <- paste(y_label_short, if_else(grepl("pro_dem", names(model_list)[[1]]), "pro-dem", "pro-rep"), sep = "-")
  }
  
  weighting_status <- if_else(grepl("_unweighted", names(model_list)[[1]]), "unweighted", "weighted")
  imputation_status <- if_else(grepl("_mi", names(model_list)[[1]]), "mi", "ld")
  se_status <- if_else(grepl("_standardses", names(model_list)[[1]]), "standardses", "clusteredses")
  
  headline_header <- if_else(grepl("_firstheadline", names(model_list)) %>% unique(), "First", "Any")
  specification <- if_else(headline_header[1] == "Any", "mainspecification", "robustnesscheck")
  model_info <- paste0(c(model_type, specification), collapse = "-")
  
  table_label <- paste0("tab:", paste(model_info, y_label_short, weighting_status, se_status, imputation_status, sep = "-"))
  if (robustness_check) { table_label <- paste0(table_label, "-codingrobustnesscheck") }
  if (!is.null(selected_headlines)) { table_label <- paste0(table_label, "-", gsub("_", "", selected_headlines)) }
  
  custom_header <- if (model_type == "mainmodel") { 
    # specify accuracy subgroup for main models
    list("All" = 1:2, "Inaccurate Subgroup" = 3:4) 
  } else {
    custom_header
  }
  
  custom_model_names <- if (model_type == "mainmodel") {
    if_else(grepl("_controls", names(model_list)), "Controls", "Baseline")
  } else if (model_type == "withoutinteraction") {
    c("All", "Inaccurate Subgroup")
  } else if (model_type == "byheadline") {
    as.character(1:9)
  } else {
    NULL
  }
  
  table <- texreg(model_list,
                  include.ci = FALSE, 
                  stars = c(0.001, 0.01, 0.05, 0.1),
                  custom.header = custom_header,
                  custom.model.names = custom_model_names,
                  caption = if (!is.null(caption)) { caption } else { paste("Regression of", y_label, "on Partisanship and Alignment") },
                  caption.above = TRUE,
                  custom.coef.map = custom_coef_map,
                  scalebox = scalebox,
                  label = table_label,
                  return.string = TRUE)

  # remove package loading instructions from table if necessary
  if (!is.null(scalebox)) { table <- gsub("\\usepackage{graphicx}", '', table, fixed = TRUE) }
  
  # add table position
  table <- gsub("\\begin{table}" ,"\\begin{table}[!htpb]", table, fixed = TRUE)
  
  # adjust table width and font size
  if (model_type == "mainmodel") { 
    table <- gsub("\\begin{center}\n\\begin{tabular}{l c c c c}","\\begin{center}\n\\begin{tabular}{@{\\extracolsep{4pt}}l c c c c@{}}", table, fixed = TRUE) 
  }
  
  # add header row specifying dependent variable
  table <- if (model_type == "mainmodel") { 
    gsub("\n & \\multicolumn{2}{c}", 
         paste0("\n & \\multicolumn{4}{c}{DV: ", y_label, 
                "}\\\\\n\\cline{2-5}\n & \\multicolumn{2}{c}"), 
         table, fixed = TRUE) 
  } else if (model_type == "withoutinteraction") {
    gsub(paste0("\n & \\multicolumn{2}{c}{", y_label, "}"), 
         paste0("\n & \\multicolumn{2}{c}{DV: ", y_label, "}"),
         table, fixed = TRUE)
  } else if (model_type == "tripleinteraction") {
    gsub(paste0("\n & \\multicolumn{3}{c}{", y_label, "}"), 
         paste0("\n & \\multicolumn{3}{c}{DV: ", y_label, "}"),
         table, fixed = TRUE)
  } else { # model_type == "byheadline"
    gsub(paste0("\n & \\multicolumn{9}{c}{", y_label, "}"), 
         paste0("\n & \\multicolumn{9}{c}{DV: ", y_label, "}"), 
         table, fixed = TRUE)
  }
  
  # add header row for models by headline
  if (model_type == "byheadline") {
    table <- gsub("\\cline{2-10}\n", paste0("\\cline{2-10}\n\\textbf{Headline}"), table, fixed = TRUE)
  }
  
  # export table to .tex file
  cat(table, file = paste0("tables/regression/", substr(table_label, 5, nchar(table_label)), ".tex", sep = ""))
}

# Function: create_mediation_df
# Turns the mediation output into a dataframe.
# Based on the mediation package's print.summary.mediate function
# (see https://github.com/kosukeimai/mediation/blob/master/R/mediate.R).
# In case several summaries are to be combined, optionally takes a variable name 
# and model name as input to distinguish these when rendering a table.
# params: model_object: mediation output object
#         variable_name: optional parameter if the outcome variable name should be added to the df
#         model_name: optional parameter if the model name should be added to the df
#         shorten: boolean indicating whether CI columns should be removed (default is FALSE)
# returns: dataframe with mediation summary information, optionally with variable and model names
create_mediation_df <- function(model_object, variable_name = NULL, model_name = NULL, shorten = FALSE) {
  x <- summary(model_object)
  
  clp <- 100 * x$conf.level
  smat <- c(x$d1, x$d1.ci, x$d1.p)
  smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
  smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
  smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
  rownames(smat) <- c("ACME", "ADE", "Total Effect", "Proportion Mediated")
  
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep=""),
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  
  data <- as.data.frame(printCoefmat(smat, tst.ind = NULL)) %>% 
    rownames_to_column(var = "Measure")
  
  if (shorten) {
    data <- data %>% 
      dplyr::select(-c(`95% CI Lower`, `95% CI Upper`))
  }
  
  data <- data %>% 
    add_row(Measure = "N Observations", Estimate = x$nobs) %>% 
    add_row(Measure = "N Simulations", Estimate = x$sims) %>% 
    # add variable and model names if provided
    mutate(Variable = variable_name, 
           Model = model_name)
  
  return(data)
}

# Function: format_mediation_table
# Takes a dataframe with the output of several mediation tables and turns it
# into a formatted LaTeX table.
# params: data: mediation dataframe created by create_mediation_df function
#         label: label for table
#         caption: table caption (default is "Effect of Alignment
#                  Mediated by Accuracy")
#         footnote: footnote for the table (optional)
#         include_controls: whether to include models with control variables
#                           (default is TRUE)
# returns: string that containing LaTeX table
format_mediation_table <- function(data, caption, label, footnote = "",
                                   include_controls = TRUE) {
  table <- data %>% 
    # format p values
    mutate(`p-value` = format.pval(`p-value`, eps = 0.001, digits = 3, nsmall = 3),
           `p-value` = if_else(`p-value` == "NA", NA_character_, `p-value`),
           `p-value` = paste0('$', `p-value`, '$')) %>% 
    # group data to automatically create subheaders
    group_by(Variable, Model) %>% 
    # turn into gt object
    gt::gt(caption = "Mediation Analysis: Effect of Alignment Mediated by Accuracy") %>% 
    # format NAs that were introduced by coercion to dataframe as blanks
    gt::fmt_missing(columns = everything(), missing_text = "")  %>%
    # format numeric columns to feature 3 decimal places
    gt::fmt_number(columns = if (!include_controls) { c("Estimate") } else { c("Estimate", "95% CI Lower", "95% CI Upper") },
                   rows = Measure %in% c("ACME", "ADE", "Total Effect", "Proportion Mediated"),
                   decimals = 3) %>% 
    # add border above number of observations
    gt::tab_style(style = cell_borders(side = c("top"),  color = "black", weight = px(1)),
                  locations = cells_body(columns = everything(), 
                                         rows = Measure == "N Observations")) %>% 
    # change formatting of group header text
    gt::tab_style(style = list(cell_text(style = "oblique", align = "center")),
                  locations = cells_row_groups()) %>% 
    # change formatting of labels
    gt::tab_style(style = list(cell_text(weight = "bold", align = "center")),
                  locations = cells_column_labels()) %>% 
    # right align p-value
    gt::tab_style(style = list(cell_text(align = "right")),
                  locations = cells_body(columns = `p-value`)) %>%
    # adapt font and border formatting
    gt::tab_options(
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.top.width = 1,
      table_body.border.bottom.width = 1,
      table_body.hlines.color = "white",
      table.font.size = 8) %>% 
    gt::opt_table_font(font = "times") %>% 
    # turn into LaTeX code
    gt::as_latex() %>% 
    as.character() %>%
    gsub("\\$NA\\$", "", ., fixed = TRUE) %>% 
    gsub("\\$", "$", ., fixed = TRUE) %>% 
    gsub("\\captionsetup[table]", paste0("\\begin{table}[!htbp]\n\\begin{scriptsize}\n\\setlength{\\LTpost}{0pt}\n\\caption{", caption, "}\n\\begin{threeparttable}"), ., fixed = TRUE) %>% 
    gsub("{labelformat=empty,skip=1pt}", "", ., fixed = TRUE) %>% 
    gsub("rl}", "rr}", ., fixed = TRUE) %>% 
    gsub("minipage", "table", ., fixed = TRUE) %>% 
    gsub("\nN Observations", "\n\\hline\nN Observations", ., fixed = TRUE) %>% 
    gsub("\\multicolumn{1}{l}", "\\multicolumn{1}{c}", ., fixed = TRUE) %>% 
    gsub("\\begin{table}{\\linewidth}", "", ., fixed = TRUE) %>% 
    gsub("\\end{longtable}", paste0("\\end{longtable}\n\\begin{tablenotes}\n\\item \\small{\\textit{Note:} ", footnote, "}\n\\end{tablenotes}\n\\end{threeparttable}\n\\end{table}"), ., fixed = TRUE) %>%
    gsub("\\end{table}", paste0("\\label{tab:", label, "}\n\\end{scriptsize}\n\\end{table}"), ., fixed = TRUE) %>% 
    gsub("\\multicolumn{1}{c}", "\\multicolumn{5}{c}", ., fixed = TRUE) %>% 
    gsub("Measure & Estimate & 95\\% CI Lower & 95\\% CI Upper & p-value", "\\textbf{Measure} & \\textbf{Estimate} & \\textbf{95\\% CI Lower} & \\textbf{95\\% CI Upper} & \\textbf{p-value}", ., fixed = TRUE) %>% 
    gsub("<0.", "< 0.", ., fixed = TRUE)
  
  if (!include_controls) { 
    table <- gsub(" - Without Controls", "", table, fixed = TRUE) %>% 
      gsub("\\multicolumn{5}{c}", "\\multicolumn{3}{c}", ., fixed = TRUE) %>% 
      gsub("Measure & Estimate & p-value", "\\textbf{Measure} & \\textbf{Estimate} & \\textbf{p-value}", ., fixed = TRUE)
  } else {
    table <- gsub(" - With", " --- With", table, fixed = TRUE)
  }
  return(table)
}

# Function: generate_mediation_table
# Generates a mediation table with several mediation models in LaTeX format.
# All models include accuracy as the mediator for the effect of partisan 
# alignment. Tables include models for all outcome variables (remove, harm, 
# censorship), with and without specified control variables.
# The models are not weighted, have standard standard errors and use the data
# set in which missing values were handled using listwise deletion.
# The table output is saved to a tex file, so nothing is returned.
# params: data: data used to generate models
#         include_controls: whether to include models with control variables
#                           (default is TRUE)
#         controls: vector of strings containing control variables to be
#                   used for the models with control variables (optional, but
#                   must be specified if include_controls is TRUE)
#         mediator: string specifying mediating variable (default is "accuracy")
#         label: label for table. This also serves as the file name when the 
#                table is exported to the tables/mediation subdirectory
#         subset_condition: string that contains the condition for 
#                           subsetting the input data (optional)
#         caption: table caption (default is "Effect of Alignment
#                  Mediated by Accuracy")
#         outcomes: vector of strings indicating the outcomes to use
#                   (default is c("remove", "harm"))
#         treatment: string indicating the treatment to use (default is "aligned")
#         footnote: footnote for the table (optional)
#         cluster: whether standard errors should be clustered on participants
#                  (default is NULL, i.e., no clustering, TRUE leads to clustering)
generate_mediation_table <- function(data, include_controls = TRUE, controls = NULL, mediator = "accuracy", 
                                     label, subset_condition = NULL, outcomes = c("remove", "harm"),
                                     treatment = "aligned",
                                     caption = "Effect of Alignment Mediated by Accuracy", 
                                     footnote = "",
                                     cluster = NULL) {
  # suppress printing to console for function
  sink("/dev/null")
  # restore defaults for printing output to console when function exits
  on.exit(sink())
  
  if (!is.null(cluster)) {
    label <- paste0(label, "-clusteredses")
  }
  
  # subset data if desired
  if (!is.null(subset_condition)) {
    data <- data %>%
      filter(eval(parse(text = subset_condition)))    
  }
  
  # initialize dataframe to store mediation results
  df.mediations <- NULL
  
  # create models for all three outcomes
  for (outcome in outcomes) {
    
    data_mediation <- data %>% 
      filter(!is.na(!!sym(outcome)))
    
    if (!is.null(cluster)) {
      # remove missing values in mediator variable to avoid clustering error
      data_mediation <- data_mediation %>% 
        filter(!is.na(!!sym(mediator)))
    }
    
    control_options <- if (include_controls) { c("nocontrols", "controls") } else { "nocontrols" }
    # create models without and with controls
    for (control_vars in control_options) {
      vec.controls <- if (control_vars == "nocontrols") { NULL } else { controls }
      
      if(!is.null(cluster) & control_vars == "controls") {
        data_mediation <- data_mediation %>% 
          dplyr::select(all_of(c(outcome, mediator, vec.controls, treatment, "id"))) %>%
          drop_na() 
      }
      
      # fit models
      rhs_mediator <- paste0("1 + ", treatment)
      # add controls if applicable
      if (control_vars == "controls") {
        rhs_mediator <- paste(rhs_mediator, paste(vec.controls, collapse = " + "), sep = "+")
      }
      fml.mediator <- paste(mediator, rhs_mediator, sep = " ~ ")
      assign("mod.mediator",
             base::eval(parse(text = paste0("lm(data = data_mediation",
                                            ", formula = ", fml.mediator, ")"))),
             envir = current_env())
      
      rhs_outcome <- paste0("1 + ", mediator, " + " , treatment)
      # add controls if applicable
      if (control_vars == "controls") {
        rhs_outcome <- paste(rhs_outcome, paste(vec.controls, collapse = " + "), sep = "+")
      }
      fml.outcome <- paste(outcome, rhs_outcome, sep = " ~ ")
      assign("mod.outcome",
             base::eval(parse(text = paste0("lm(data = data_mediation", 
                                            ", formula = ", fml.outcome, ")"))),
             envir = current_env())
      
      # set seed for reproducibility
      set.seed(123456)
      
      # mediation analysis
      med.out <- mediation::mediate(model.m = mod.mediator, 
                                    model.y = mod.outcome,
                                    treat = treatment,
                                    mediator = mediator,
                                    sims = 1000, 
                                    cluster = if (!is.null(cluster)) { data_mediation$id } else { NULL })
      
      df.mediations <- rbind(df.mediations, 
                             create_mediation_df(med.out,
                                                 variable_name = case_when(outcome == "remove" ~ "Intent to Remove Headline",
                                                                           outcome == "harm" ~ "Intent to Report Headline as Harmful",
                                                                           outcome == "censorship" ~ "Perception of Headline Removal as Censorship"), 
                                                 model_name = if_else(control_vars == "nocontrols", "Without Controls", "With Controls"),
                                                 shorten = if_else(include_controls, FALSE, TRUE)))
    }
  }
  # generate LaTeX table from dataframe
  table <- format_mediation_table(data = df.mediations, caption = caption, label = label, footnote = footnote, include_controls = include_controls)
  
  # export table to .tex file
  cat(table, file = paste0("tables/mediation/", label, ".tex", sep = ""))
}

# Function: generate_mediation_sensitivity_plots
# Generates sensitivity plots for mediation analyses.
# All models include accuracy as the mediator for the effect of partisan 
# alignment. Tables include models for all outcome variables (remove, harm, 
# censorship), with and without specified control variables.
# The models are not weighted, have standard standard errors and use the data
# set in which missing values were handled using listwise deletion.
# The plots are saved to disk, and model information is printed, so nothing is returned.
# params: data: data used to generate models
#         include_controls: whether to include models with control variables
#                           (default is TRUE)
#         controls: vector of strings containing control variables to be
#                   used for the models with control variables (optional, but
#                   must be specified if include_controls is TRUE)
#         mediator: string specifying mediating variable (default is "accuracy")
#         outcomes: vector of strings indicating the outcomes to use
#                   (default is c("remove", "harm"))
#         treatment: string indicating the treatment to use (default is "aligned")
#         label: label for table. This also serves as the file name when the 
#                table is exported to the tables/mediation subdirectory
#         caption: caption printed with summary statistics (default is "Effect 
#                  of Alignment Mediated by Accuracy")
#         subset_condition: string that contains the condition for 
#                           subsetting the input data (optional)
#         cluster: whether standard errors should be clustered on participants
#                  (default is NULL, i.e., no clustering, TRUE leads to clustering)
generate_mediation_sensitivity_plots <- function(data, include_controls = TRUE, controls = NULL, mediator = "accuracy", 
                                                 label, outcomes = c("remove", "harm"),
                                                 treatment = "aligned",
                                                 caption = "Effect of Alignment Mediated by Accuracy", 
                                                 subset_condition = NULL,
                                                 cluster = NULL) {
  if (!is.null(cluster)) {
    label <- paste0(label, "-clusteredses")
  }
  
  # subset data if desired
  if (!is.null(subset_condition)) {
    data <- data %>%
      filter(eval(parse(text = subset_condition)))    
  }
  
  # initialize dataframe to store mediation results
  df.mediations <- NULL
  
  # create models for all three outcomes
  for (outcome in outcomes) {
    
    data_mediation <- data %>% 
      filter(!is.na(!!sym(outcome)))
    
    if (!is.null(cluster)) {
      # remove missing values in mediator variable to avoid clustering error
      data_mediation <- data_mediation %>% 
        filter(!is.na(!!sym(mediator)))
    }
    
    control_options <- if (include_controls) { c("nocontrols", "controls") } else { "nocontrols" }
    # create models without and with controls
    for (control_vars in control_options) {
      vec.controls <- if (control_vars == "nocontrols") { NULL } else { controls }
      
      if(!is.null(cluster) & control_vars == "controls") {
        data_mediation <- data_mediation %>% 
          dplyr::select(all_of(c(outcome, mediator, vec.controls, treatment, "id"))) %>%
          drop_na() 
      }
      
      # fit models
      rhs_mediator <- paste0("1 + ", treatment)
      # add controls if applicable
      if (control_vars == "controls") {
        rhs_mediator <- paste(rhs_mediator, paste(vec.controls, collapse = " + "), sep = "+")
      }
      fml.mediator <- paste(mediator, rhs_mediator, sep = " ~ ")
      assign("mod.mediator",
             base::eval(parse(text = paste0("lm(data = data_mediation",
                                            ", formula = ", fml.mediator, ")"))),
             envir = current_env())
      
      rhs_outcome <- paste0("1 + ", mediator, " + " , treatment)
      # add controls if applicable
      if (control_vars == "controls") {
        rhs_outcome <- paste(rhs_outcome, paste(vec.controls, collapse = " + "), sep = "+")
      }
      fml.outcome <- paste(outcome, rhs_outcome, sep = " ~ ")
      assign("mod.outcome",
             base::eval(parse(text = paste0("lm(data = data_mediation", 
                                            ", formula = ", fml.outcome, ")"))),
             envir = current_env())
      
      # set seed for reproducibility
      set.seed(123456)
      
      # mediation analysis
      med.out <- mediation::mediate(mod.mediator, 
                                    mod.outcome,
                                    treat = treatment,
                                    mediator = mediator,
                                    sims = 1000, 
                                    cluster = if (!is.null(cluster)) { data_mediation$id } else { NULL })
      
      print(caption)
      print(paste0("Outcome: ", outcome))
      print(paste0("Control variable option: ", control_vars))
      sens.out_acme <- medsens(med.out, effect.type = "indirect", sims = 1000)
      png(paste0("figures/sensitivity_plots/rho-acme-", outcome, "-", control_vars, "-", label, ".png", sep = ""), width = 9*2/3, height = 6.75*2/3, units = "in", res = 150)
      plot(sens.out_acme, sens.par = "rho", r.type = "total", ask = FALSE)
      dev.off()
      # print results table
      print("ACME results:")
      print(summary(sens.out_acme))
      
      sens.out_ade <- medsens(med.out, effect.type = "direct", sims = 1000)
      png(paste0("figures/sensitivity_plots/rho-ade-", outcome, "-", control_vars, "-", label, ".png", sep = ""), width = 9*2/3, height = 6.75*2/3, units = "in", res = 150)
      plot(sens.out_ade, sens.par = "rho", r.type = "total", ask = FALSE)
      dev.off()
      # print results table
      print("ADE results:")
      print(summary(sens.out_ade))
    }
  }
}

# Function get_cis
# Takes in a list of models and returns their CIs for the Democrat and
# Republican coefficients
# param: models: list of models to extracts CIs from
# return: dataframe with model information including CIs
get_cis <- function(models) {
  # combine data for all models into one dataframe
  data <- tibble()
  for (i in 1:length(models)) {
    model <- models[[i]]
    # get coefficient information
    data_tmp <- broom::tidy(model) %>% 
      dplyr::select(-c(statistic, df)) %>% 
      filter(term %in% c("party_idDemocrat", "party_idRepublican")) %>% 
      dplyr::select(term, estimate, std.error, p.value, conf.low, conf.high, outcome)
    
    aligned <- str_match(names(models)[i], "(overall|misaligned|aligned)")[,2]
    data_tmp$aligned <- aligned
    # add temporary data to all data
    data <- rbind(data, data_tmp)
  }
  # rename variables in final dataframe
  data <- data %>% 
    mutate(party_id = str_extract(term, "(Democrat|Republican)"),
           aligned = case_when(aligned == "overall" ~ 0.5,
                               aligned == "aligned" ~ 1,
                               aligned == "misaligned" ~ 0),
           aligned = factor(aligned, levels = c(0.5, 0, 1))) %>% 
    rename(variable = outcome,
           se = std.error,
           pvalue = p.value) %>% 
    dplyr::select(-term)
  
  return(data)
}