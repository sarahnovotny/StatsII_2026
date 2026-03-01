% Replication materials for "Partisan conflict over content moderation is more than disagreement about facts"

Science Advances, 2023.
By Ruth E. Appel, Jennifer Pan and Margaret E. Roberts.

## Overview

This repository contains all code and data necessary to replicate the results presented in the paper "Partisan conflict over content moderation is more than disagreement about facts" and its supplementary materials.

For files that share the same name with the exception of the extension (e.g., `Rmd` vs. `R`), the contents are the same, they are only rendered in different formats for convenience. Note that the session information for each of the main scripts can be found at the very end and is stored in the `html` version of the file.

## Structure and content

- `code`: This directory contains all code. 
    - `data_cleaning.Rmd`, `data_cleaning.R`, `data_cleaning.html`: Code that cleans the raw data for further analysis. This code is included for informational purposes and will not run without errors since we cannot share the full raw dataset from Ipsos and do not provide the full file of Congressional speeches that can be downloaded elsewhere. However, we share the cleaned datasets, and using those datasets, all results can be replicated by running the `manuscript_code.Rmd` or `manuscript_code.R` file alone.
    - `functions.R`: Functions for data modeling and analysis. This file does not need to be run by itself, all results can be replicated by running the `manuscript_code.Rmd` or `manuscript_code.R` file which will source this code.
    - `final_models.Rmd`, `final_models.R`, `final_models.html`: Code that generates all regression models. This file does not need to be run by itself, all results can be replicated by running the `manuscript_code.Rmd` or `manuscript_code.R` file which will source this code.
    - `manuscript_code.Rmd`, `manuscript_code.R`, `manuscript_code.html`: Code that generates all figures and tables shown in the main text and supplementary materials.
- `data`: This directory contains all data.
    - `input/census` subdirectory: Contains U.S. Census Bureau data in the files `ACSST5Y2020.S0101-2022-09-07T230634.xlsx` and `DECENNIALPL2020.P2-2022-09-07T194449.xlsx` that are used to check the representativeness of the sample.
    - `input/party_division` subdirectory: Contains `congress_party_division.csv`, a dataframe which shows which party had the majority in which chamber of Congress, which is used to create a figure showing censorship-related tokens by party and Congress for the supplementary materials.
    - `intermediate/survey` subdirectory: Contains intermediate data generated from the raw survey data.
        - `df.experiment_long.csv` (and `df.experiment_long_robustnesscheck.csv`): cleaned unimputed main data (and data for the robustness check with the pre-registered censorship outcome variable coding).
        - `imputations1.csv` to `imputations5.csv` (and `imputations1_robustnesscheck.csv` to `imputations5.csv_robustnesscheck`): 5 cleaned imputed datasets (and 5 imputed datasets for the robustness check with the pre-registered censorship outcome variable coding).
        - `codebook.csv`: Codebook that shows variable and value labels for each variable in the cleaned dataset.
    - `intermediate/speech` subdirectory: Contains `df.censor_tokens_congress.csv`, a dataframe with counts of censorship-related tokens by party and Congress generated from a large corpus of Congressional speeches, which is used to create a figure showing censorship-related tokens by party and Congress for the supplementary materials.
- `figures`: This directory serves as the location for the figures to be produced by the scripts.
    - `coefficient_plots`: Subdirectory for coefficient plots that appear in the supplementary materials.
    - `congress_speech_plot`: Subdirectory for the figure on censorship-related tokens in Congressional speeches by party over time that appears in the supplementary materials.
    - `main_plots`: Subdirectory for figures that appear in the main text.
    - `sensitivity_plots`: Subdirectory for sensitivity plots for the mediation analyses that appear in the supplementary materials.
- `tables`: This directory serves as the location for the tables to be produced by the scripts.
    - `balance`: Subdirectory for balance tables that appear in the supplementary materials.
    - `descriptives`: Subdirectory for tables with descriptive statistics that appear in the supplementary materials.
    - `mediation`: Subdirectory for mediation tables that appear in the main text or supplementary materials.
    - `regression`: Subdirectory for regression tables that appear in the supplementary materials.

## Datasets

- U.S. Census Bureau data in the files `ACSST5Y2020.S0101-2022-09-07T230634.xlsx` and `DECENNIALPL2020.P2-2022-09-07T194449.xlsx` in the `data/input/census` directory that are used to check the representativeness of the sample. See first sheet of each file for more details.
- `congress_party_division.csv` in the `data/input/party_division` directory is a dataframe which shows which party had the majority in which chamber of Congress. The source of this dataset downloaded on August 6, 2022 is History, Art & Archives, U.S. House of Representatives, Party Government Since 1857 [Dataset]. https://history.house.gov/Institution/Presidents-Coinciding/Party-Government/ (2022). We use the columns `Congress`, `House Majority` and `Senate Majority`, indicating to which Congress the data pertain, and which party had the House and Senate majority, respectively.
- `df.experiment_long.csv` (and `df.experiment_long_robustnesscheck.csv`) are the cleaned unimputed main data (and data for the robustness check with the pre-registered censorship outcome variable coding). `imputations1.csv` to `imputations5.csv` (and `imputations1_robustnesscheck.csv` to `imputations5.csv_robustnesscheck`) are 5 cleaned imputed datasets (and 5 imputed datasets for the robustness check with the pre-registered censorship outcome variable coding). See `codebook.csv` for detailed variable descriptions. All of these files are in the `data/intermediate/survey` directory. The source of the raw data that were preprocessed to yield the files in the `data/intermediate/survey` directory is Knight Foundation-Ipsos (2022) "Free Expression in America Post-2020" [Dataset].
- `df.censor_tokens_congress.csv` in the `data/intermediate/speech` directory is a dataframe with contains counts of censorship-related tokens by party and Congress. The variable `total_censor_token_count` indicates the total number of censorship-related token by a given party in a given Congress, `party` indicates whether the token count is for Republicans or Democrats, and `congress` indicates which congress the token counts refer to. The source of the data that were downloaded on August 3, 2022 and preprocessed to yield the file `df.censor_tokens_congress.csv` is D. Card, S. Chang, C. Becker, J. Mendelsohn, R. Voigt, L. Boustan, R. Abramitzky, D. Jurafsky, Replication code and data for “Computational analysis of 140 years of US political
speeches reveals more positive but increasingly polarized framing of immigration” [Dataset].
https://github.com/dallascard/us-immigration-speeches/ (2022).

## Notes

- Since software might change over time, replication might only be successful with the package and R versions indicated in the session information.
- Please set the working directory to the folder which contains `README_replication.md` in the beginning of the script. If you use the `Rmd` files, make sure to open them in the accompanying `Rproj` environment (e.g., by double clicking on `replication_package.Rproj` to open RStudio) so that paths can be set correctly.
- If you are working on a Windows machine, you might have to replace `/` with `\` in file paths when reading in data or storing output, including in the `functions.R`, `final_models.R` and `manuscript_code.R` or `manuscript_code.Rmd` file.
- Successful replication requires only running the `manuscript_code.Rmd` or `manuscript_code.R` file, which loads all other files that are needed.
- Resulting figures will be stored in the `figures` directory and its subdirectories.
- Resulting tables will be stored in the `tables` directory and its subdirectories.
- The only table that requires the full raw survey data and can therefore not be replicated using `manuscript_code.Rmd` or `manuscript_code.R` is a table about the representativeness of the sample shown in the supplementary materials. The code we ran to generate this table is shown in full, but commented out to prevent error messages.
- The `functions.R` file contains most of the functions written for this project. However, some functions are directly embedded in other scripts because they are helpful to view in full in the context of these script because they are e.g. highly customized data cleaning steps that load additional packages.

## Citation

Appel, R. E., Pan, J., & Roberts, M. E. Partisan conflict over content moderation is more than disagreement about facts. *Sciences Advances* (forthcoming) (2023).

```
@article{AppelPanRoberts2023,
author = {Appel, Ruth E. and Pan, Jennifer and Roberts, Margaret E.},
journal = {Science Advances},
title = {{Partisan conflict over content moderation is more than disagreement about facts}},
volume = {(forthcoming)},
year = {2023}
}
```
