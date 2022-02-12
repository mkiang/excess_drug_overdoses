## Imports ----
library(tidyverse)
library(here)
source(here("code", "utils.R"))

## Data ----
weekly_df <- readRDS(here("data", "analytic_weekly_df_region.RDS"))
weekly_wide <- weekly_df %>%
    pivot_wider(
        id_cols = race:week_from_start,
        names_from = death_type,
        values_from = n_deaths
    ) %>%
    categorize_region()

x <- weekly_wide %>%
    filter(date < as.Date("2020-12-31")) %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(year, region_cat_num) %>%
    summarize(prop_at_least_1 = round(1 - (sum(no_specified_drug) / sum(any_poisoning)), 2)) %>%
    arrange(region_cat_num, year) %>%
    pivot_wider(id_cols = region_cat_num,
                names_from = year,
                values_from = prop_at_least_1)

write_csv(x, here("output", "specific_drug_reporting.csv"))
