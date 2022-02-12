## 06_calculate_excess_stratified_by_region.R ----
##
## This file contains all the code necessary to do a region-specific analysis
## of our results stratifying the models by region. Note that due to sparse data
## we may not get all results for comparison.
##
## This file is broken up to reflect the separate files in the primary analysis.
## Specifically, Part 1 summarizes the raw data into weekly data. Part 2 preps
## the population data. Part 3 creates a new analytic data set. Part 4 runs
## the excess mortality models. Use the document outline in RStudio to help
## navigate the file (CMD + SHIFT + O).
## 
## NOTE: Part 1 must uses CDPH restricted access data and must be run on
## the secure environment. 

### Imports ----
library(here)
library(fs)
library(tidyverse)
library(excessmort)
library(future)
library(furrr)
library(janitor)
source(here::here("code" , "utils.R"))

### Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

### Set your max number of cores ----
future::plan(future::multisession(workers = 14))

### Constants ----
SAVE_DIR <- config::get("private_save_folder")
FIRST_WEEK <- config::get("first_week")
LAST_WEEK <- as.Date(config::get("last_week"))
FORECAST_START <- as.Date(config::get("forecast_start"))

## Step 1. Summarize raw data to weekly level stratified by sex ----
if (!fs::file_exists(here::here("data", "summarized_weekly_deaths_by_region.RDS"))) {
    ### Data ----
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
    
    ## Collapse AIAN into Other ----
    ## Let's collapse AINA and Other
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
        ) %>%
        dplyr::mutate(educ = "all")
    
    ## Double the data frame for the "all" categories ----
    flagged_df <- dplyr::bind_rows(flagged_df,
                            flagged_df %>%
                                dplyr::mutate(race = "all")) %>%
        dplyr::mutate(year = lubridate::year(dod),
               month = lubridate::month(dod))
    
    ## Add week markers ----
    flagged_df$week_from_start <-
        as.integer(floor(difftime(
            flagged_df$dod, as.Date(FIRST_WEEK), units = "week"
        )) + 1)
    
    ## Make different geographic subsets ----
    flagged_df <- dplyr::left_join(
        flagged_df %>% 
            dplyr::select(-county_name), 
        ca_regions_fips %>%
            dplyr::transmute(
                region, 
                region_name,
                county_name,
                county_fip = county_cdph
            )
    ) %>%
        dplyr::filter(!is.na(region_name))
    
    ## Summarize at weekly level ----
    weekly_df <- flagged_df %>%
        dplyr::group_by(week_from_start) %>%
        dplyr::mutate(date = max(dod),
               date_start = min(dod)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(race, educ, region_name, date_start) %>%
        dplyr::select(-id,
               -age,
               -dod,
               -sex,
               -county_name,
               -year,
               -month,
               -county_fip) %>%
        dplyr::group_by(race, educ, region, region_name, date, date_start, week_from_start)
    
    weekly_summarized_df <- weekly_df %>%
        dplyr::select(-ucod, -record_all) %>%
        dplyr::summarize_all(sum, na.rm = TRUE) %>%
        tidyr::pivot_longer(cols = covid:synthetic_alone,
                     names_to = "death_type",
                     values_to = "n_deaths") %>%
        dplyr::arrange(death_type, race, educ, region, region_name, date)
    
    ## Save ----
    saveRDS(
        weekly_summarized_df,
        here::here("data", "summarized_weekly_deaths_by_region.RDS"),
        compress = "xz"
    )
}

## Step 2. Get matching population data by race/ethnicity and sex ----
if (!fs::file_exists(here::here("data", "california_pop_by_race_region.RDS"))) {
    ### Import raw data ----
    ca_pops <- readr::read_csv(here::here("data_raw", "co-est2020.csv")) %>%
        dplyr::filter(STATE == "06", SUMLEV == "050") %>% 
        dplyr::select(-SUMLEV,
               -REGION,
               -DIVISION) %>% 
        dplyr::rename(st_name = STNAME) %>% 
        janitor::clean_names()
    
    ca_pops <- ca_pops %>% 
        dplyr::select(GEOID = state, county, st_name, ctyname, dplyr::starts_with("popestimate")) %>% 
        dplyr::select(-popestimate042020) %>% 
        tidyr::pivot_longer(cols = popestimate2010:popestimate2020, 
                     names_to = "year", 
                     values_to = "pop") %>% 
        dplyr::mutate(year = as.integer(gsub("popestimate", "", year))) %>% 
        dplyr::left_join(ca_regions_fips %>% 
                      dplyr::transmute(region, 
                                region_name, 
                                county = sprintf("%03d", county_fips)))
    
    ca_pops <- ca_pops %>% 
        dplyr::group_by(GEOID, st_name, year, region, region_name) %>%
        dplyr::summarize(pop = sum(pop)) %>% 
        dplyr::mutate(race = "all",
               educ = "all") %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(GEOID, st_name, educ, race, region, year) %>% 
        dplyr::filter(year %in% 2015:2019)
    
    saveRDS(ca_pops, here::here("data", "california_pop_by_race_region.RDS"))
} 

## Step 3. Create a new analytic file ----
if (!all(fs::file_exists(c(here::here("data", "pop_by_race_region_extrapolated.RDS"),
                       here::here("data", "pop_by_race_region_interpolated.RDS"),
                       here::here("data", "analytic_weekly_df_region.RDS"))))) {
    pop_df <- readRDS(here::here("data", "california_pop_by_race_region.RDS"))
    weekly_df <- readRDS(here::here("data", "summarized_weekly_deaths_by_region.RDS")) %>% 
        dplyr::filter(educ == "all",
               race == "all")
    
    ### Extrapolate out to 2020 ----
    pop_df <- dplyr::bind_rows(
        pop_df,
        pop_df %>% 
            dplyr::group_by(GEOID, st_name, race, educ, region) %>% 
            dplyr::mutate(pop_change = pop[year == 2019] / pop [year == 2018]) %>% 
            dplyr::arrange(st_name, race, educ, region, year) %>% 
            dplyr::filter(year %in% 2019) %>% 
            dplyr::mutate(pop_2020 = pop * pop_change ^ (as.numeric(difftime(LAST_WEEK, as.Date("2019-07-01"))) / 365)) %>% 
            dplyr::mutate(year = 2020, 
                   pop = pop_2020) %>% 
            dplyr::select(-pop_2020, -pop_change)
    ) %>% 
        dplyr::arrange(GEOID, st_name, region, year)
    
    ## Make a weekly, linearly interpolated population ----
    pop_combos <- pop_df %>%
        dplyr::select(-pop, -year, -GEOID) %>% 
        dplyr::distinct()
    
    interpolated_data <- vector("list", NROW(pop_combos))
    for (i in 1:NROW(pop_combos)) {
        st_name_x <- pop_combos$st_name[i]
        race_x <- pop_combos$race[i]
        educ_x <- pop_combos$educ[i]
        region_x <- pop_combos$region[i]
        region_name_x <- pop_combos$region_name[i]
        
        holder <- NULL
        for (y in 2015:2019) {
            start_date <- as.Date(sprintf("%s-07-01", y))
            end_date <- as.Date(ifelse((y + 1) == 2020,
                                       as.character(LAST_WEEK + 1),
                                       sprintf("%s-07-01", y + 1)))
            seq_date <- seq(start_date, end_date - 1, by = 1)
            
            start_pop <- pop_df %>% 
                dplyr::filter(st_name == st_name_x,
                       race == race_x,
                       educ == educ_x, 
                       region == region_x, 
                       region_name == region_name_x, 
                       year == y) %>% 
                dplyr::pull(pop)
            
            end_pop <-  pop_df %>% 
                dplyr::filter(st_name == st_name_x,
                       race == race_x,
                       educ == educ_x, 
                       region == region_x, 
                       region_name == region_name_x, 
                       year == y + 1) %>% 
                dplyr::pull(pop)
            
            seq_pop <- round(seq(start_pop, end_pop, along.with = seq_date))
            
            holder <- dplyr::bind_rows(
                holder,
                dplyr::tibble(
                    st_name = st_name_x,
                    race = race_x,
                    educ = educ_x,
                    region = region_x, 
                    region_name = region_name_x, 
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
        dplyr::filter(!is.na(race),
               !is.na(region)) %>% 
        dplyr::arrange(race, educ, region, death_type, date) %>% 
        dplyr::ungroup()
    
    saveRDS(pop_df, here::here("data", "pop_by_race_region_extrapolated.RDS"))
    saveRDS(interpolated_data, here::here("data", "pop_by_race_region_interpolated.RDS"))
    saveRDS(weekly_df, here::here("data", "analytic_weekly_df_region.RDS"))
}

## Part 4. Calculate excess deaths by sex ----

### Constants ----
FREQUENCY <- 52
TRAIN_SPLIT <- 70/100 ## Model selection is based on last 30% of TRAINING data
N_HARMONICS <- 1:4
N_TREND_KNOTS <- (0:2)/5

## What is the length of the prediction window? 
## Use the 75% of the prediction task as our window -- in this case
## it will result in 9-month prediction windows.
PREDICTION_WINDOW <- as.integer(difftime(LAST_WEEK, FORECAST_START, units = "weeks") * 9/12)

### Data ----
analytic_df <- readRDS(here::here("data", "analytic_weekly_df_region.RDS")) %>% 
    dplyr::filter(date <= LAST_WEEK) %>% 
    dplyr::select(-pop) %>% 
    dplyr::rename(pop = pop_weekly) %>% 
    dplyr::filter(!grepl("_alone", death_type, fixed = TRUE))

### Training data ----
### Make sure to remove *all* of the holdout (prediction) data
training_df <- dplyr::filter(analytic_df, date < FORECAST_START)

## Get a vector of test dates we will for out of sample forecasting errors
all_dates <- sort(unique(training_df$date))
end_training <- seq.Date(
    all_dates[round(NROW(all_dates) * TRAIN_SPLIT)],
    max(all_dates) - PREDICTION_WINDOW * 7,
    by = "1 week"
)

### Create a search grid of all models we'll need to run ----
model_grid <- expand.grid(
    death_type = unique(analytic_df$death_type),
    race = unique(analytic_df$race),
    educ = unique(analytic_df$educ),
    region = unique(analytic_df$region), 
    n_harmonic = N_HARMONICS,
    n_knot = N_TREND_KNOTS,
    training_start = min(all_dates),
    training_end = end_training,
    stringsAsFactors = FALSE
) %>%
    dplyr::arrange(
        n_harmonic,
        n_knot,
        death_type,
        race,
        educ, 
        region, 
        training_start,
        training_end
    ) %>%
    dplyr::mutate(forecast_end = training_end + PREDICTION_WINDOW * 7) %>%
    dplyr::as_tibble() %>% 
    dplyr::left_join(
        ca_regions_fips %>% 
            dplyr::select(region, region_name) %>% 
            dplyr::distinct()
    )

### Fit the training data on a rolling forecasting origin ----
## For all outcomes with at least 2 deaths per week
if (!file.exists(here::here("data", "model_forecasting_errors_region.RDS"))) {
    forecast_errors <- furrr::future_map_dfr(
        .x = 1:NROW(model_grid),
        .f = ~ {
            race_x <- model_grid$race[.x]
            educ_x <- model_grid$educ[.x]
            death_x <- model_grid$death_type[.x]
            region_x <- model_grid$region[.x]
            region_name_x <- model_grid$region_name[.x]
            n_harmonics <- model_grid$n_harmonic[.x]
            n_knots <- model_grid$n_knot[.x]
            date_end  <- model_grid$forecast_end[.x]
            train_end <- model_grid$training_end[.x]
            train_start <- model_grid$training_start[.x]
            
            current_training_df <- training_df %>% 
                dplyr::filter(race == race_x,
                       educ == educ_x, 
                       death_type == death_x,
                       region == region_x, 
                       date <= date_end) %>% 
                dplyr::transmute(date, outcome = n_deaths, population = pop)
            
            exclude_date <- current_training_df %>% 
                dplyr::filter(date > train_end) %>% 
                dplyr::pull(date)
            
            if (mean(current_training_df$outcome) >= 2) {
                current_model <- excessmort::compute_expected(
                    current_training_df,
                    harmonics = n_harmonics,
                    trend.knots.per.year = n_knots,
                    exclude = exclude_date,
                    frequency = FREQUENCY,
                    verbose = FALSE
                ) 
                
                current_model %>%
                    dplyr::as_tibble() %>%
                    dplyr::select(date, 
                           observed = outcome, 
                           predicted = expected, 
                           excluded) %>%
                    ## Filter out observations used for fitting
                    dplyr::filter(excluded) %>%
                    dplyr::mutate(
                        model_grid_ix = .x,
                        race = race_x,
                        educ = educ_x, 
                        region = region_x, 
                        region_name = region_name_x, 
                        death_type = death_x,
                        n_harmonic = n_harmonics,
                        n_knot = n_knots,
                        train_start = train_start,
                        train_end = train_end,
                        forecast_start = min(exclude_date),
                        forecast_end = max(exclude_date),
                        n_train_dates = sum(!current_model$excluded, na.rm = TRUE),
                        n_test_dates = sum(current_model$excluded, na.rm = TRUE)
                    ) %>%
                    dplyr::select(-excluded)
            }
        }
    )
    saveRDS(forecast_errors, here::here("data", "model_forecasting_errors_region.RDS"), compress = "xz")
} else {
    forecast_errors <- readRDS(here::here("data", "model_forecasting_errors_region.RDS"))
}

### Summarize the out of sample errors ----
if (!fs::file_exists( here::here("data", "model_error_summary_region.RDS"))) {
    error_summary <- forecast_errors %>%
        dplyr::mutate(
            error = predicted - observed,
            abs_error = abs(predicted - observed),
            sq_error = (predicted - observed) ^ 2,
            abs_perc_error = abs(predicted - observed) / observed * 100
        ) %>%
        dplyr::group_by(race, educ, region, region_name, death_type, n_harmonic, n_knot) %>%
        dplyr::summarize(n_predictions = sum(!is.na(predicted)),
                  mae = mean(abs_error, na.rm = TRUE),
                  mse = mean(sq_error, na.rm = TRUE),
                  rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                  mape = mean(abs_perc_error, na.rm = TRUE),
                  mean_obs = mean(observed)
        ) %>% 
        dplyr::group_by(race, educ, region, region_name, death_type) %>% 
        dplyr::mutate(model_rank = dplyr::row_number(mse)) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(n_predictions > 0) %>% 
        dplyr::arrange(race, educ, region, death_type, model_rank) %>% 
        categorize_race() %>% 
        categorize_educ() %>% 
        categorize_death() 
    
    saveRDS(error_summary, here::here("data", "model_error_summary_region.RDS"))
} else {
    error_summary <- readRDS(here::here("data", "model_error_summary_region.RDS"))
}

### Fit counterfactual models based on lowest out of sample errors ----
## Now take the best (in terms of out of sample prediction) for each
## race/outcome combination and fit it to the full training data and predict
## out to our holdout set.
best_models <- error_summary %>% 
    dplyr::filter(model_rank == 1)

## Need a vector of datas to *NOT* train the model on
exclude_dates <- analytic_df %>% 
    dplyr::filter(date >= FORECAST_START) %>% 
    dplyr::pull(date) %>% 
    unique()

if (!fs::file_exists(here::here("data", "expected_deaths_region.RDS"))) {
    expected_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            race_x <- best_models$race[.x]
            educ_x <- best_models$educ[.x]
            death_x <- best_models$death_type[.x]
            region_x <- best_models$region[.x]
            region_name_x <- best_models$region_name[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(race == race_x,
                       educ == educ_x, 
                       death_type == death_x,
                       region == region_x,
                       region_name == region_name_x) %>% 
                dplyr::transmute(date, outcome = n_deaths, population = pop)
            
            if (mean(train_data$outcome) >= 2) {
                excessmort::compute_expected(
                    train_data,
                    harmonics = n_harmonics, 
                    trend.knots.per.year = n_knots, 
                    exclude = exclude_dates, 
                    frequency = FREQUENCY
                )  %>%
                    dplyr::as_tibble() %>% 
                    dplyr::mutate(
                        ix = .x, 
                        race = race_x,
                        educ = educ_x, 
                        region = region_x,
                        region_name = region_name_x, 
                        death_type = death_x, 
                        lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                        upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                    ) 
            }
        }
    ) %>% 
        categorize_race() %>% 
        categorize_educ() %>% 
        categorize_death()
    saveRDS(expected_deaths, here::here("data", "expected_deaths_region.RDS"))
} else {
    expected_deaths <- readRDS(here::here("data", "expected_deaths_region.RDS"))
}

### Calculate excess mortality over time for each group ----
if (!fs::file_exists(here::here("data", "excess_deaths_region.RDS"))) {
    excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            race_x <- best_models$race[.x]
            educ_x <- best_models$educ[.x]
            death_x <- best_models$death_type[.x]
            region_x <- best_models$region[.x]
            region_name_x <- best_models$region_name[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(race == race_x,
                       educ == educ_x, 
                       region == region_x,
                       region_name == region_name_x,
                       death_type == death_x) %>% 
                dplyr::transmute(date, outcome = n_deaths, population = pop)
            
            excess_mort <- excessmort::excess_model(
                train_data,
                # start = as.Date("2020-01-01"), 
                start = min(exclude_dates),
                end = max(exclude_dates),
                exclude = exclude_dates,
                model = "quasipoisson",
                trend.knots.per.year = n_knots,
                harmonics = n_harmonics,
                discontinuity = FALSE, 
                frequency = FREQUENCY,
                knots.per.year = 6
            )
            
            dplyr::tibble(
                date = excess_mort$date,
                observed = excess_mort$observed,
                expected = excess_mort$expected,
                log_expected_se = excess_mort$log_expected_se,
                fitted = excess_mort$fitted,
                se = excess_mort$se,
                sd = excess_mort$sd
            ) %>%
                dplyr::mutate(
                    ## Assumes the fitted and expected are independent
                    abs_change = expected * fitted,
                    abs_change_se = sqrt(
                        (expected^2 * log_expected_se^2) * se^2 + 
                            (expected^2 * log_expected_se^2) * fitted^2 + 
                            se^2 * expected^2
                    ),
                    abs_change_lower = abs_change - 1.96 * abs_change_se,
                    abs_change_upper = abs_change + 1.96 * abs_change_se
                ) %>%
                dplyr::mutate(
                    rel_change = (observed - expected)/expected,
                    rel_change_lower = fitted - 1.96 * se,
                    rel_change_upper = fitted + 1.96 * se
                ) %>% 
                dplyr::mutate(
                    race = race_x,
                    educ = educ_x, 
                    region = region_x,
                    region_name = region_name_x, 
                    death_type = death_x, 
                    lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                    upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                )  
            
        }
    ) %>% 
        categorize_race() %>% 
        categorize_educ() %>% 
        categorize_death()
    saveRDS(excess_deaths, here::here("data", "excess_deaths_region.RDS"))
} else {
    excess_deaths <- readRDS(here::here("data", "excess_deaths_region.RDS"))
}

### Calculate cumulative excess mortality for each group ----
if (!fs::file_exists(here::here("data", "cume_excess_deaths_region.RDS"))) {
    cume_excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            race_x <- best_models$race[.x]
            educ_x <- best_models$educ[.x]
            death_x <- best_models$death_type[.x]
            region_x <- best_models$region[.x]
            region_name_x <- best_models$region_name[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(race == race_x,
                       educ == educ_x, 
                       region == region_x,
                       region_name == region_name_x,
                       death_type == death_x) %>% 
                dplyr::transmute(date, outcome = n_deaths, population = pop)
            
            excess_mort <- excessmort::excess_model(
                train_data,
                # start = as.Date("2020-01-01"), 
                start = min(exclude_dates),
                end = max(exclude_dates),
                exclude = exclude_dates,
                model = "quasipoisson",
                trend.knots.per.year = n_knots,
                harmonics = n_harmonics,
                discontinuity = FALSE, 
                frequency = FREQUENCY,
                knots.per.year = 6
            )
            
            excessmort::excess_cumulative(excess_mort, 
                              start = FORECAST_START,
                              # start = as.Date("2020-01-01"),
                              end = LAST_WEEK) %>% 
                dplyr::mutate(
                    race = race_x,
                    educ = educ_x,
                    region = region_x,
                    region_name = region_name_x, 
                    death_type = death_x,
                    upper = fitted + 1.96 * se,
                    lower = fitted - 1.96 * se
                )
        }
    ) %>% 
        categorize_race() %>% 
        categorize_educ() %>% 
        categorize_death()
    saveRDS(cume_excess_deaths, here::here("data", "cume_excess_deaths_region.RDS"))
}

## Close connections or RStudio crashes on restart ----
closeAllConnections()
