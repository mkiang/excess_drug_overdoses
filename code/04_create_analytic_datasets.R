## 04_create_analytic_datasets.R ----
##
## Using the population data and the summarized death data, we can create
## weekly and monthly analytic data files that will be used for all the
## modeling. Note that this is where the population extrapolation/interpolation
## occurs.

## Imports ----
library(here)
library(tidyverse)
source(here::here("code" , "utils.R"))

LAST_WEEK <- as.Date(config::get("last_week"))

## Data ----
pop_df <-
    readRDS(here::here("data", "california_pop_by_race_education.RDS"))
weekly_df <-
    readRDS(here::here("data", "summarized_weekly_deaths.RDS")) %>%
    dplyr::filter(date <= LAST_WEEK)
monthly_df <-
    readRDS(here::here("data", "summarized_monthly_deaths.RDS")) %>%
    dplyr::filter(date <= LAST_WEEK)

## Extrapolate out to 2020 ----
pop_df <- dplyr::bind_rows(
    pop_df,
    pop_df %>%
        dplyr::group_by(GEOID, st_name, race, educ) %>%
        dplyr::mutate(pop_change = pop[year == 2019] / pop [year == 2018]) %>%
        dplyr::arrange(st_name, race, educ, year) %>%
        dplyr::filter(year %in% 2019) %>%
        dplyr::mutate(pop_2020 = pop * pop_change ^ (as.numeric(
            difftime(LAST_WEEK, as.Date("2019-07-01"))
        ) / 365)) %>%
        dplyr::mutate(year = 2020,
                      pop = pop_2020) %>%
        dplyr::select(-pop_2020, -pop_change)
)

## Make a weekly, linearly interpolated population ----
pop_combos <- pop_df %>%
    dplyr::select(-pop, -year, -GEOID) %>%
    dplyr::distinct()

interpolated_data <- vector("list", NROW(pop_combos))
for (i in 1:NROW(pop_combos)) {
    st_name_x <- pop_combos$st_name[i]
    race_x <- pop_combos$race[i]
    educ_x <- pop_combos$educ[i]
    
    holder <- NULL
    for (y in 2015:2019) {
        start_date <- as.Date(sprintf("%s-07-01", y))
        end_date <- as.Date(ifelse((y + 1) == 2020,
                                   as.character(LAST_WEEK + 1),
                                   sprintf("%s-07-01", y + 1)
        ))
        seq_date <- seq(start_date, end_date - 1, by = 1)
        
        start_pop <- pop_df %>%
            dplyr::filter(st_name == st_name_x,
                          race == race_x,
                          educ == educ_x,
                          year == y) %>%
            dplyr::pull(pop)
        
        end_pop <-  pop_df %>%
            dplyr::filter(st_name == st_name_x,
                          race == race_x,
                          educ == educ_x,
                          year == y + 1) %>%
            dplyr::pull(pop)
        
        seq_pop <-
            round(seq(start_pop, end_pop, along.with = seq_date))
        
        holder <- dplyr::bind_rows(
            holder,
            dplyr::tibble(
                st_name = st_name_x,
                race = race_x,
                educ = educ_x,
                date = seq_date,
                pop = seq_pop
            )
        )
    }
    
    interpolated_data[[i]] <- holder
}
interpolated_data <- dplyr::bind_rows(interpolated_data)

## Reshape the death data ----
weekly_df <- weekly_df %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::left_join(pop_df) %>%
    dplyr::left_join(interpolated_data %>%
                         dplyr::rename(pop_weekly = pop)) %>%
    dplyr::select(-year, -GEOID, -st_name) %>%
    dplyr::filter(educ != "unknown edu") %>%
    dplyr::arrange(race, educ, death_type, date) %>%
    dplyr::ungroup()

monthly_df <- monthly_df %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::left_join(pop_df) %>%
    dplyr::left_join(interpolated_data %>%
                         dplyr::rename(pop_monthly = pop)) %>%
    dplyr::select(-GEOID, -st_name, -year) %>%
    dplyr::filter(educ != "unknown edu") %>%
    dplyr::arrange(race, educ, death_type, date) %>%
    dplyr::ungroup()

saveRDS(pop_df,
        here::here("data", "pop_by_race_educ_extrapolated.RDS"))
saveRDS(interpolated_data,
        here::here("data", "pop_by_race_educ_interpolated.RDS"))
saveRDS(weekly_df, here::here("data", "analytic_weekly_df.RDS"))
saveRDS(monthly_df, here::here("data", "analytic_monthly_df.RDS"))
