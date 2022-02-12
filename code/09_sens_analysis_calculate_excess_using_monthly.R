## 09_sens_analysis_calculate_excess_using_monthly.R ----
## 
## This file contains all the code necessary to do a sensitivity analysis
## using monthly data instead of weekly data. Monthly data means auto-
## correlation is less of a concern but at the cost of having ~1/4 the data.
##
## This file is broken up to reflect the separate files in the primary analysis.
## Specifically, Part 1 summarizes the raw data into weekly data. Part 2 preps
## the population data. Part 3 creates a new analytic data set. Part 4 runs
## the excess mortality models. Use the document outline in RStudio to help
## navigate the file (CMD + SHIFT + O).
## 
## NOTE: Part 1 must uses CDPH restricted access data and must be run on
## the secure environment. 

## Imports ----
library(tidyverse)
library(here)
library(excessmort)
library(future)
library(furrr)
source(here::here("code", "utils.R"))

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Set your max number of cores ----
future::plan(future::multisession(workers = 14))

## Constants ----
FORECAST_START <- as.Date(config::get("forecast_start"))
LAST_WEEK <- as.Date(config::get("last_week"))
FREQUENCY <- 12
TRAIN_SPLIT <- 70/100 ## Model selection is based on last 30% of data
N_HARMONICS <- 1:2
N_TREND_KNOTS <- (0:2)/5

## What is the length of the prediction window? Use the same length of the
## prediction window for our forecasting errors. 
PREDICTION_WINDOW <- as.integer(difftime(LAST_WEEK, FORECAST_START, units = "weeks") * 9/12 / 4) 

## Data ----
analytic_df <- readRDS(here::here("data", "analytic_monthly_df.RDS")) %>% 
    dplyr::filter(date <= LAST_WEEK) %>% 
    dplyr::select(-pop) %>% 
    dplyr::rename(pop = pop_monthly)

## Training data ----
### Make sure to remove *all* of the holdout (prediction) data ----
training_df <- dplyr::filter(analytic_df, date < FORECAST_START)

## Get a vector of test dates we will for out of sample forecasting errors
all_dates <- sort(unique(training_df$date))
end_training <- seq.Date(
    all_dates[round(NROW(all_dates) * TRAIN_SPLIT)],
    max(all_dates) - PREDICTION_WINDOW * 7,
    by = "1 month"
)

### Create a search grid of all models we'll need to run ----
model_grid <- expand.grid(
    death_type = unique(analytic_df$death_type),
    race = unique(analytic_df$race),
    educ = unique(analytic_df$educ),
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
        training_start,
        training_end
    ) %>%
    dplyr::mutate(forecast_end = training_end + PREDICTION_WINDOW * 7) %>%
    dplyr::as_tibble()

### Fit the training data on a rolling forecasting origin ----
## For all outcomes with at least 2 deaths per week
if (!file.exists(here::here("data", "supp_monthly_model_forecasting_errors.RDS"))) {
    forecast_errors <- furrr::future_map_dfr(
        .x = 1:NROW(model_grid),
        .f = ~ {
            race_x <- model_grid$race[.x]
            educ_x <- model_grid$educ[.x]
            death_x <- model_grid$death_type[.x]
            n_harmonics <- model_grid$n_harmonic[.x]
            n_knots <- model_grid$n_knot[.x]
            date_end  <- model_grid$forecast_end[.x]
            train_end <- model_grid$training_end[.x]
            train_start <- model_grid$training_start[.x]
            
            current_training_df <- training_df %>% 
                dplyr::filter(race == race_x,
                       educ == educ_x, 
                       death_type == death_x,
                       date <= date_end) %>% 
                dplyr::transmute(date, outcome = n_deaths, population = pop)
            
            exclude_date <- current_training_df %>% 
                dplyr::filter(date > train_end) %>% 
                dplyr::pull(date)
            
            if (mean(current_training_df$outcome) >= 8) {
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
    saveRDS(forecast_errors, here::here("data", "supp_monthly_model_forecasting_errors.RDS"), compress = "xz")
} else {
    forecast_errors <- readRDS(here::here("data", "supp_monthly_model_forecasting_errors.RDS"))
}

### Summarize the out of sample errors ----
if (!file.exists( here::here("data", "supp_monthly_model_error_summary.RDS"))) {
    error_summary <- forecast_errors %>%
        dplyr::mutate(
            error = predicted - observed,
            abs_error = abs(predicted - observed),
            sq_error = (predicted - observed) ^ 2,
            abs_perc_error = abs(predicted - observed) / observed * 100
        ) %>%
        dplyr::group_by(race, educ, death_type, n_harmonic, n_knot) %>%
        dplyr::summarize(n_predictions = sum(!is.na(predicted)),
                  mae = mean(abs_error, na.rm = TRUE),
                  mse = mean(sq_error, na.rm = TRUE),
                  rmse = sqrt(mean(sq_error, na.rm = TRUE)),
                  mape = mean(abs_perc_error, na.rm = TRUE)
        ) %>% 
        dplyr::group_by(race, educ, death_type) %>% 
        dplyr::mutate(model_rank = dplyr::row_number(mse)) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(n_predictions > 0) %>% 
        dplyr::arrange(race, educ, death_type, model_rank) %>% 
        categorize_race() %>% 
        categorize_educ() %>% 
        categorize_death()
    
    saveRDS(error_summary, here::here("data", "supp_monthly_model_error_summary.RDS"))
} else {
    error_summary <- readRDS(here::here("data", "supp_monthly_model_error_summary.RDS"))
}

## Fit counterfactual models based on lowest out of sample errors ----
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

expected_deaths <- furrr::future_map_dfr(
    .x = 1:NROW(best_models),
    .f = ~ {
        race_x <- best_models$race[.x]
        educ_x <- best_models$educ[.x]
        death_x <- best_models$death_type[.x]
        n_harmonics <- best_models$n_harmonic[.x]
        n_knots <- best_models$n_knot[.x]
        
        train_data <- analytic_df %>% 
            dplyr::filter(race == race_x,
                   educ == educ_x, 
                   death_type == death_x) %>% 
            dplyr::transmute(date, outcome = n_deaths, population = pop)
        
        excessmort::compute_expected(
            train_data,
            harmonics = n_harmonics, 
            trend.knots.per.year = n_knots, 
            exclude = exclude_dates, 
            frequency = FREQUENCY
        )  %>%
            dplyr::as_tibble() %>% 
            dplyr::mutate(
                race = race_x,
                educ = educ_x, 
                death_type = death_x, 
                lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                upper_expected = exp(log(expected) + 1.96 * log_expected_se)
            ) 
    }
) %>% 
    categorize_race() %>% 
    categorize_educ() %>% 
    categorize_death()
saveRDS(expected_deaths, here::here("data", "supp_monthly_expected_deaths.RDS"))

## Calculate excess mortality over time for each group ----
excess_deaths <- furrr::future_map_dfr(
    .x = 1:NROW(best_models),
    .f = ~ {
        race_x <- best_models$race[.x]
        educ_x <- best_models$educ[.x]
        death_x <- best_models$death_type[.x]
        n_harmonics <- best_models$n_harmonic[.x]
        n_knots <- best_models$n_knot[.x]
        
        train_data <- analytic_df %>% 
            dplyr::filter(race == race_x,
                   educ == educ_x, 
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
                death_type = death_x, 
                lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                upper_expected = exp(log(expected) + 1.96 * log_expected_se)
            )  
        
    }
) %>% 
    categorize_race() %>% 
    categorize_educ() %>% 
    categorize_death()
saveRDS(excess_deaths, here::here("data", "supp_monthly_excess_deaths.RDS"))

## Calculate cumulative excess mortality for each group ----
cume_excess_deaths <- furrr::future_map_dfr(
    .x = 1:NROW(best_models),
    .f = ~ {
        race_x <- best_models$race[.x]
        educ_x <- best_models$educ[.x]
        death_x <- best_models$death_type[.x]
        n_harmonics <- best_models$n_harmonic[.x]
        n_knots <- best_models$n_knot[.x]
        
        train_data <- analytic_df %>% 
            dplyr::filter(race == race_x,
                   educ == educ_x, 
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
                death_type = death_x,
                upper = fitted + 1.96 * se,
                lower = fitted - 1.96 * se
            )
    }
) %>% 
    categorize_race() %>% 
    categorize_educ() %>% 
    categorize_death()
saveRDS(cume_excess_deaths, here::here("data", "supp_monthly_cume_excess_deaths.RDS"))

## Close connections or RStudio crashes on restart ----
closeAllConnections()
