## 02_summarize_death_data.R ----
##
## Takes the cleaned up data and creates aggregated version for analysis.
## Similar to the 01 code file, this file uses CDPH restricted access data and
## therefore can only be run in the secure environment.

## Imports ----
library(here)
library(fs)
library(tidyverse)
source(here::here("code" , "utils.R"))

## Constants ----
SAVE_DIR <- config::get("private_save_folder")
FIRST_WEEK <- config::get("first_week")

## Data ----
death_df <- readRDS(paste0(SAVE_DIR, "cleaned_line_data.RDS"))

## Subset to CA residents ----
death_df <- death_df %>%
    dplyr::filter(state_name == "CA") %>%
    dplyr::select(-state_name)

## Flag the death data ----
death_df <- death_df %>%
    flag_covid_death() %>%
    dplyr::mutate(non_covid = 1 - covid,
                  all_deaths = 1)

## Flag types only for non-COVID deaths
flagged_df <- dplyr::bind_rows(
    ## Excess deaths are only those NOT from COVID
    death_df %>%
        dplyr::filter(covid == 0) %>%
        flag_any_poisoning_death() %>%
        flag_drug_poisoning() %>%
        flag_nondrug_poisoning() %>%
        flag_alcohol() %>%
        flag_benzos() %>%
        flag_cocaine() %>%
        flag_opioids() %>%
        flag_meth() %>%
        flag_nonopioid_analgesics() %>%
        flag_psychotropics() %>%
        flag_anticoag() %>%
        flag_other_narco_psychodysleptics() %>%
        flag_other_drug() %>%
        flag_nonopioid_substance() %>%
        flag_solo_drugs(),
    death_df %>%
        dplyr::filter(covid == 1)
) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(dod >= FIRST_WEEK) %>%
    dplyr::select(
        -other_nonopioid_substance,
        -psychotropics,
        -anticoag,
        -nonopioid_analgesics,
        -other_narco_psychodysleptics
    )

## Rename race to match pops and collapse AI/NA ----
## We need ~2 observations per week for the model to work. AI/NA have 299 or
## about 1.2 per week. Other have 741 or about 2.9 per week. Collapse both.
flagged_df <- flagged_df %>%
    dplyr::mutate(
        race = dplyr::case_when(
            race == "White" ~ "white",
            race == "Black" ~ "black",
            race == "Asian / Pacific Islander" ~ "asian",
            race == "Hispanic" ~ "hispanic",
            race == "Other" ~ "other",
            race == "American Indian / Native American" ~ "other"
        )
    )

## Double the data frame for the "all" categories ----
flagged_df <- dplyr::bind_rows(flagged_df,
                               flagged_df %>%
                                   dplyr::mutate(race = "all"))
flagged_df <- dplyr::bind_rows(flagged_df,
                               flagged_df %>%
                                   dplyr::mutate(educ = "all"))

## Double up all non-Hispanic ----
flagged_df <- dplyr::bind_rows(
    flagged_df,
    flagged_df %>%
        dplyr::filter(race != "hispanic", race != "all") %>%
        dplyr::mutate(race = "all_nonhisp")
)

## Now do it again for the over 25 ----
## "under25" is already done
flagged_df <- dplyr::bind_rows(
    flagged_df,
    flagged_df %>%
        dplyr::filter(age >= 25,
                      educ != "all") %>%
        dplyr::mutate(educ = "over25")
)

## Add week and month markers ----
flagged_df$week_from_start <-
    as.integer(floor(difftime(
        flagged_df$dod, as.Date(FIRST_WEEK), units = "week"
    )) + 1)

flagged_df <- flagged_df %>%
    dplyr::filter(dod >= as.Date("2016-01-01")) %>%
    dplyr::mutate(year = lubridate::year(dod),
                  month = lubridate::month(dod)) %>%
    dplyr::mutate(month_from_start = (year - min(year)) * 12 + month)

## Summarize at weekly level ----
weekly_df <- flagged_df %>%
    dplyr::group_by(week_from_start) %>%
    dplyr::mutate(date = max(dod),
                  date_start = min(dod)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(race, educ, date_start) %>%
    dplyr::select(-id,
                  -age,
                  -dod,
                  -sex,
                  -county_name,
                  -year,
                  -month,
                  -month_from_start,
                  -county_fip) %>%
    dplyr::group_by(race, educ, date, date_start, week_from_start)

weekly_summarized_df <- weekly_df %>%
    dplyr::select(-ucod, -record_all) %>%
    dplyr::summarize_all(sum, na.rm = TRUE) %>%
    tidyr::pivot_longer(
        cols = covid:synthetic_alone,
        names_to = "death_type",
        values_to = "n_deaths"
    ) %>%
    dplyr::arrange(death_type, race, educ, date)

## Summarize at monthly level ----
monthly_df <- flagged_df %>%
    dplyr::group_by(month_from_start) %>%
    dplyr::mutate(date = max(dod),
                  date_start = min(dod)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(race, educ, date_start) %>%
    dplyr::select(-id,
                  -age,
                  -dod,
                  -sex,
                  -county_name,
                  -year,
                  -month,
                  -week_from_start,
                  -county_fip) %>%
    dplyr::group_by(race, educ, date, date_start, month_from_start)

monthly_summarized_df <- monthly_df %>%
    dplyr::select(-ucod, -record_all) %>%
    dplyr::summarize_all(sum, na.rm = TRUE) %>%
    tidyr::pivot_longer(
        cols = covid:synthetic_alone,
        names_to = "death_type",
        values_to = "n_deaths"
    ) %>%
    dplyr::arrange(death_type, race, educ, date)

## Save ----
saveRDS(
    weekly_summarized_df,
    here::here("data", "summarized_weekly_deaths.RDS"),
    compress = "xz"
)
saveRDS(
    monthly_summarized_df,
    here::here("data", "summarized_monthly_deaths.RDS"),
    compress = "xz"
)
