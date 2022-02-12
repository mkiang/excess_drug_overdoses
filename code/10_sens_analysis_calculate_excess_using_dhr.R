## 10_sens_analysis_calculate_excess_using_dhr.R ----
## 
## All excess mortality models assume that the baseline (i.e., expected)
## counterfactual model is correctly specified. This, in practice, is not
## testable. However, here, we will use a completely different family of 
## models (time series vs Poisson) to see if our results are sensitive to
## our underlying model assumptions. 

## Imports ----
library(forecast)
library(fable)
library(feasts)
library(future.apply)
library(tsibble)
library(tidyverse)
library(here)
library(config)
library(future)
library(fs)
library(furrr)
source(here::here("code", "utils.R"))

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Data ----
death_df <- readRDS(here::here("data", "summarized_weekly_deaths.RDS")) %>%
    dplyr::filter(
        death_type != "all_deaths",
        death_type != "non_covid",
        death_type != "other_opioid",
        death_type != "any_poisoning",
        death_type != "nondrug_poisoning",
        death_type != "unintent_nondrug_poisoning",
        death_type != "intent_nondrug_poisoning",
        death_type != "assault_drug_poisoning",
        death_type != "assault_nondrug_poisoning", 
        death_type != "no_specified_drug", 
        death_type != "other_drug", 
        !grepl("_alone", death_type, fixed = TRUE), 
        # race != "other",
        # race != "asian",
        race != "all_nonhisp",
        educ != "over25",
        educ != "under25"
    ) %>% 
    dplyr::group_by(race, educ, death_type) %>% 
    dplyr::filter(mean(n_deaths) > 2) %>% 
    dplyr::ungroup()

## Constants ----
FORECAST_START <- config::get("forecast_start")
LAST_WEEK <- config::get("last_week")
FORECAST_INTERVAL <- round(as.numeric(difftime(LAST_WEEK, FORECAST_START, units = "weeks") * 9/12))
N_BOOT <- 2000

## Create training data ----
training_df <- death_df %>%
    dplyr::filter(date < FORECAST_START) %>%
    dplyr::arrange(race, educ, death_type, date) %>%
    dplyr::mutate(yearweek = tsibble::yearweek(date)) %>%
    tsibble::tsibble(key = c("race", "death_type", "educ"),
            index = yearweek)

## Create all models on training data ----
if (!fs::file_exists(here::here("data", "supp_dhr_all_models.RDS"))) {
    ## fable package has weird memory leak issues (see Github Issue below) so
    ## you should use fewer cores than normal (14 cores results in ~200GB of 
    ## memory usage), or you should use a sequential plan and let it run 
    ## overnight. 
    ## 
    ## https://github.com/tidyverts/fabletools/issues/146
    ## https://github.com/tidyverts/fable/issues/230 
    
    future::plan(future::multisession(workers = 4))
    # future::plan(sequential)

    model_df1 <- training_df %>%
        fabletools::model(
            k00 = fable::ARIMA(n_deaths ~ PDQ(0, 0, 0),
                        stepwise = FALSE,
                        approximation = FALSE)
        )

    model_df2 <- training_df %>%
        fabletools::model(
            k01 = k_autoarima(k = 01),
            k02 = k_autoarima(k = 02),
            k03 = k_autoarima(k = 03),
            k04 = k_autoarima(k = 04),
            k05 = k_autoarima(k = 05)
        )


    model_df3 <- training_df %>%
        fabletools::model(
            k06 = k_autoarima(k = 06),
            k07 = k_autoarima(k = 07),
            k08 = k_autoarima(k = 08),
            k09 = k_autoarima(k = 09),
            k10 = k_autoarima(k = 10)
        )


    model_df4 <- training_df %>%
        fabletools::model(
            k11 = k_autoarima(k = 11),
            k12 = k_autoarima(k = 12),
            k13 = k_autoarima(k = 13),
            k14 = k_autoarima(k = 14),
            k15 = k_autoarima(k = 15)
        )

    model_df <- model_df1 %>% 
        dplyr::left_join(model_df2) %>% 
        dplyr::left_join(model_df3) %>% 
        dplyr::left_join(model_df4)
    
    rm(model_df1, model_df2, model_df3, model_df4); gc()
    
    saveRDS(model_df, here::here("data", "supp_dhr_all_models.RDS"), compress = "xz")
} else {
    model_df <- readRDS(here::here("data", "supp_dhr_all_models.RDS"))
}

## Transform our mable into a tibble with the *best* models only along with
## their fit statistics and autocorrelation tests
if (!fs::file_exists(here::here("data", "supp_dhr_best_models.RDS"))) {
    best_models <- fabletools::glance(model_df) %>%
        dplyr::left_join(model_df %>%
                      fabletools::augment(model_df) %>%
                      fabletools::features(.innov, ljung_box, lag = 10, dof = 3)) %>%
        dplyr::group_by(race, educ, death_type) %>%
        dplyr::arrange(race, educ, death_type, AICc) %>% 
        # mutate(model_rank = row_number(AICc)) %>% 
        dplyr::filter(AICc == min(AICc, na.rm = TRUE)) %>%
        dplyr::rename(n_harmonics = .model) %>%
        dplyr::left_join(tidyr::pivot_longer(
            model_df,
            names_to = "n_harmonics",
            values_to = "model",
            cols = c(dplyr::starts_with("k"))
        ))
    saveRDS(best_models, here::here("data", "supp_dhr_best_models.RDS"), 
            compress = "xz")
} else {
    best_models <- readRDS(here::here("data", "supp_dhr_best_models.RDS"))
}

## Set your max number of cores ----
closeAllConnections()
future::plan(future::multisession(workers = 14))

## Calculate the total excess mortality over the period of interest and
## bootstrapped CIs
param_grid <- best_models %>% 
    dplyr::ungroup() %>% 
    dplyr::select(race, death_type, educ) %>% 
    dplyr::distinct()

if (!fs::file_exists(here::here("data", "supp_dhr_excess_deaths_bootstrapped.RDS"))) {
    total_excess_df <- furrr::future_map_dfr(.x = 1:NROW(param_grid),
                                      .f = ~ {
                                          calculate_excess_deaths(
                                              death_df,
                                              educ_x = param_grid$educ[.x],
                                              race_x = param_grid$race[.x],
                                              death_x = param_grid$death_type[.x],
                                              best_model_df = best_models,
                                              forecast_start = FORECAST_START,
                                              last_week = LAST_WEEK,
                                              n_reps = N_BOOT
                                          )
                                      }) %>% 
        categorize_educ() %>% 
        categorize_death() %>% 
        categorize_race()
    
    saveRDS(total_excess_df,
            here::here("data", "supp_dhr_excess_deaths_bootstrapped.RDS"),
            compress = "xz")
} else {
    total_excess_df <- readRDS(here::here("data", "supp_dhr_excess_deaths_bootstrapped.RDS"))
}

## Tibble with the fitted, observed, and forecast deaths (+ 95% CI) 
if (!fs::file_exists(here::here("data", "supp_dhr_fitted_values.RDS"))) {
    fitted_values_df <- furrr::future_map_dfr(.x = 1:NROW(param_grid),
                                      .f = ~ {
                                          extract_fitted_values(
                                              death_df,
                                              educ_x = param_grid$educ[.x],
                                              race_x = param_grid$race[.x],
                                              death_x = param_grid$death_type[.x],
                                              best_model_df = best_models,
                                              forecast_start = FORECAST_START,
                                              last_week = LAST_WEEK,
                                              n_reps = N_BOOT
                                          )
                                      })
    saveRDS(fitted_values_df %>% 
                categorize_educ() %>% 
                categorize_death() %>% 
                categorize_race(),
            here::here("data", "supp_dhr_fitted_values.RDS"),
            compress = "xz")
} else {
    fitted_values_df <- readRDS(here::here("data", "supp_dhr_fitted_values.RDS"))
}

## Must manually close connections or RStudio will crash
closeAllConnections()
