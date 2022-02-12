## 12_supp_create_subset_for_joinpoint.R ----
##
## It is perfectly possible that our model does not adequate pick up the
## exponential increase in recent drug-related mortality and thus some of our
## excess deaths are representative of rapid pre-COVID growth. Here, we'll use
## joinpoint regression to detect natural breaks in the crude mortality rate
## by race, educ, region, and race*educ and outcome. To do that, we need to
## create proper data subsets. The actual joinpoints are run using the NCI
## Joinpoint Regression program: https://surveillance.cancer.gov/joinpoint/
##
## Side note: The data for the region sensitivity analysis is weekly and
## therefore the joinpoints can take a fairly long time.

## Imports ----
library(tidyverse)
library(here)
library(fs)
source(here::here("code", "utils.R"))

## Create dirs ----
fs::dir_create(here::here(
    "joinpoint",
    c("race_alone",
      "educ_alone",
      "selected_educ_and_race")
))

## Data ----
monthly_df <- readRDS(here::here("data", "analytic_monthly_df.RDS"))

## Raw death counts ----
monthly_df <- monthly_df %>%
    dplyr::mutate(n_deaths = ifelse(n_deaths == 0, .1, n_deaths)) %>%
    dplyr::filter(
        # race != "other" ,
        race != "all_nonhisp",
        educ != "over25",
        educ != "under25",
        death_type != "assault_drug_poisoning",
        death_type != "assault_nondrug_poisoning",
        death_type != "intent_nondrug_poisoning",
        death_type != "nondrug_poisoning",
        death_type != "other_opioid",
        death_type != "unintent_nondrug_poisoning",
        death_type != "unknown_drug_poisoning",
        death_type != "unknown_nondrug_poisoning",
        death_type != "no_specified_drug",
        death_type != "other_drug",
        !grepl("_alone", death_type, fixed = TRUE),
    ) %>%
    dplyr::arrange(race, educ, death_type, date)

## Save death counts ----
readr::write_csv(
    monthly_df %>%
        dplyr::filter(educ == "all",
                      !(educ == "all" & race == "all")),
    here::here(
        "joinpoint",
        "race_alone",
        "race_imputed_zero_monthly_deaths.csv"
    )
)
readr::write_csv(
    monthly_df %>%
        dplyr::filter(race == "all"),
    here::here(
        "joinpoint",
        "educ_alone",
        "educ_imputed_zero_monthly_deaths.csv"
    )
)
readr::write_csv(
    monthly_df %>%
        dplyr::filter(educ != "all",
                      race != "all"),
    here::here(
        "joinpoint",
        "selected_educ_and_race",
        "both_race_educ_imputed_zero_monthly_deaths.csv"
    )
)
