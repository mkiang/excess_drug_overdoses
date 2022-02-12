## 03_munge_ca_pops_by_race_educ.R ----
##
## Creates the population counts we will need based on the IPUMS data. Note
## that we use IPUMS for the most part because it breaks down by race/ethnicity
## and education. The IPUMS raw data pulls are in ./data_raw.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

ca_pops <- purrr::map_df(.x = 2015:2019,
                         .f = ~ read_acs_microdata(here::here(
                             "data_raw",
                             "acs_microdata_pulls",
                             sprintf("ca_pop_race_age_education_%i.csv", .x)
                         ))) %>%
    dplyr::mutate(GEOID = "06",
                  st_name = "California")

## Add AI/AN into the "Other" category to match the death data ----
ca_pops <- ca_pops %>%
    dplyr::mutate(race = dplyr::case_when(race == "aian" ~ "other",
                                          TRUE ~ race)) %>%
    dplyr::group_by(year, educ, race, GEOID, st_name) %>%
    dplyr::summarize_all(sum)

## Make a version of Hispanic * education ----
hisp_pop_by_educ <- dplyr::bind_rows(
    ca_pops %>%
        dplyr::filter(race != "all") %>%
        dplyr::mutate(race = "hispanic") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(hispanic)),
    ca_pops %>%
        dplyr::filter(race != "all",
                      educ %in% c("under25", "over25")) %>%
        dplyr::mutate(race = "hispanic",
                      educ = "all") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(hispanic))
)

## Make a version of Non-Hispanic * education ----
nonhisp_pop_by_educ <- dplyr::bind_rows(
    ca_pops %>%
        dplyr::filter(race != "all") %>%
        dplyr::mutate(race = "all_nonhisp") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(nonhispanic)),
    ca_pops %>%
        dplyr::filter(race != "all",
                      educ %in% c("under25", "over25")) %>%
        dplyr::mutate(race = "all_nonhisp",
                      educ = "all") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(nonhispanic))
)

## Make a version of (non-Hispanic) race * education ----
race_pop_by_educ <- dplyr::bind_rows(
    ca_pops %>%
        dplyr::filter(race != "all") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(nonhispanic)),
    ca_pops %>%
        dplyr::filter(race != "all",
                      educ %in% c("under25", "over25")) %>%
        dplyr::mutate(educ = "all") %>%
        dplyr::group_by(GEOID, st_name, year, race, educ) %>%
        dplyr::summarize(pop = sum(nonhispanic))
)

## Calculate education for total pop ----
## This should have 40 rows for the 8 education categories (all, under25,
## over25, <hs, hs, <bs, bs, >bs) and 5 years (2015 to 2019).
total_pop_by_educ <- dplyr::bind_rows(
    ca_pops %>%
        dplyr::filter(
            race != "all",
            educ != "all_educ",
            educ != "under25",
            educ != "over25"
        ) %>%
        dplyr::mutate(race = "all") %>%
        dplyr::group_by(GEOID, st_name, race, year, educ) %>%
        dplyr::summarize(pop = sum(total)) %>%
        dplyr::arrange(educ, year),
    ca_pops %>%
        dplyr::filter(race == "all") %>%
        dplyr::select(GEOID, st_name, race, year, educ, pop = total)
)

## Save all combinations ----
all_combos <- dplyr::bind_rows(hisp_pop_by_educ,
                               nonhisp_pop_by_educ,
                               race_pop_by_educ,
                               total_pop_by_educ) %>%
    dplyr::ungroup()

saveRDS(all_combos,
        here::here("data", "california_pop_by_race_education.RDS"))
