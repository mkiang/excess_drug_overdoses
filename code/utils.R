## utils.R ----
## 
## M Kiang utility functions for California excess mortality project.

## Helpers for identifying cause of death ----
## Notes from CDC website:
## https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm
## 
## COVID is U07.1 for underlying or multiple cause
## 
## Main cause of death categories for CDC are respiratory diseases,
## circulatory diseases, malignant neoplasms, alzheimer disease and 
## dementia, and other select causes of death. We are further interested
## in natural causes, poisonings (by drug type), and injuries. 
## 
## 
## Respiratory diseases
##   Influenza and pneumonia (J09–J18)
##   Chronic lower respiratory diseases (J40–J47)
##   Other diseases of the respiratory system (J00–J06, J20–J39, 
##      J60–J70, J80–J86, J90–J96, J97–J99, R09.2, U04)
## 
## Circulatory diseases
##   Hypertensive diseases (I10–I15)
##   Ischemic heart disease (I20–I25)
##   Heart failure (I50)
##   Cerebrovascular diseases (I60–I69)
##   Other disease of the circulatory system (I00–I09, I26–I49, 
##      I51, I52, I70–I99)
## 
## Malignant neoplasms (C00–C97)
## 
## Alzheimer disease and dementia (G30, G31, F01, F03)
#
## Other select causes of death
##   Diabetes (E10–E14)
##   Renal failure (N17–N19)
##   Sepsis (A40–A41)
## 
## Natural causes A00–A39, A42–B99, D00–E07, E15–E68, 
##  E70–E90, F00, F02, F04–G26, G31–H95, K00–K93, 
##  L00–M99, N00–N16, N20–N98, O00–O99, P00–P96, Q00–Q99
library(tidyverse)
library(fable)
library(fabletools)

## Death helpers ----
flag_covid_death <- function(cleaned_df, ucod_only = FALSE) {
    ## cleaned_df is a dataframe with ucod and record_all columns
    if (ucod_only) {
        cleaned_df %>%
            mutate(covid = grepl("\\<U071", ucod) + 0)
    } else {
        cleaned_df %>%
            mutate(covid = grepl("\\<U071", record_all) + 0)
    }
}

flag_unintent_drug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(unintent_drug_poisoning = grepl("\\<X4[0-4]{1}", ucod) + 0)
}

flag_unintent_nondrug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(unintent_nondrug_poisoning = grepl("\\<X4[5-9]{1}", ucod) + 0)
}

flag_intent_drug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(intent_drug_poisoning = grepl("\\<X6[0-4]{1}", ucod) + 0)
}

flag_intent_nondrug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(intent_nondrug_poisoning = grepl("\\<X6[5-9]{1}", ucod) + 0)
}

flag_assault_drug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(assault_drug_poisoning = grepl("\\<X85", ucod) + 0)
}

flag_assault_nondrug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(assault_nondrug_poisoning = grepl("\\<X8[6-9]{1}|\\<X90", ucod) + 0)
}

flag_unknown_drug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(unknown_drug_poisoning = grepl("\\<Y1[0-4]{1}", ucod) + 0)
}

flag_unknown_nondrug_poisoning <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(unknown_nondrug_poisoning = grepl("\\<Y1[5-9]{1}", ucod) + 0)
}

flag_any_poisoning_death <- function(cleaned_df) {
    cleaned_df %>%
        flag_unintent_drug_poisoning() %>%
        flag_unintent_nondrug_poisoning() %>%
        flag_intent_drug_poisoning() %>%
        flag_intent_nondrug_poisoning() %>%
        flag_assault_drug_poisoning() %>%
        flag_assault_nondrug_poisoning() %>%
        flag_unknown_drug_poisoning() %>%
        flag_unknown_nondrug_poisoning() %>%
        mutate(any_poisoning = 0 + ((
            unintent_drug_poisoning +
                intent_drug_poisoning +
                assault_drug_poisoning +
                unknown_drug_poisoning +
                unintent_nondrug_poisoning +
                intent_nondrug_poisoning +
                assault_nondrug_poisoning +
                unknown_nondrug_poisoning
        ) > 0
        ))
}

flag_drug_poisoning <- function(cleaned_df) {
    cleaned_df %>%
        flag_unintent_drug_poisoning() %>%
        flag_intent_drug_poisoning() %>%
        flag_assault_drug_poisoning() %>%
        flag_unknown_drug_poisoning() %>%
        mutate(drug_poisoning = 0 + ((
            unintent_drug_poisoning +
                intent_drug_poisoning +
                assault_drug_poisoning +
                unknown_drug_poisoning
        ) > 0
        ))
}

flag_nondrug_poisoning <- function(cleaned_df) {
    cleaned_df %>%
        flag_unintent_nondrug_poisoning() %>%
        flag_intent_nondrug_poisoning() %>%
        flag_assault_nondrug_poisoning() %>%
        flag_unknown_nondrug_poisoning() %>%
        mutate(nondrug_poisoning = 0 + ((
            unintent_nondrug_poisoning +
                intent_nondrug_poisoning +
                assault_nondrug_poisoning +
                unknown_nondrug_poisoning
        ) > 0
        ))
}

flag_nonopioid_analgesics <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(nonopioid_analgesics = grepl("\\<T39", record_all) + 0)
}

flag_psychotropics <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(psychotropics = (drug_poisoning == 1 & 
                                    grepl("\\<T4[23]{1}", record_all)) + 0)
}

flag_benzos <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(benzos = (drug_poisoning == 1 & 
                             grepl("\\<T424", record_all)) + 0)
}

flag_meth <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(meth = (drug_poisoning == 1 & 
                           grepl("\\<T436", record_all)) + 0)
}

flag_anticoag <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(anticoag = (drug_poisoning == 1 & 
                               grepl("\\<T455", record_all)) + 0)
}

flag_other_narco_psychodysleptics <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(other_narco_psychodysleptics = (drug_poisoning == 1 & 
                                                   grepl("\\<T3[678]{1}|\\<T4[01456789]{1}|\\<T50[12345678]", record_all)) + 0)
}

flag_nonopioid_substance <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(other_nonopioid_substance = (drug_poisoning == 1 & 
                                                grepl("\\<T3[6789]{1}|\\<T4[1456789]{1}|\\<T50[12345678]", record_all)) + 0)
}

flag_opioids <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(opioids = (drug_poisoning == 1 & 
                              grepl("\\<T40[012346]{1}", record_all)) + 0,
               heroin = (drug_poisoning == 1 & 
                             grepl("\\<T401", record_all)) + 0,
               synthetic_opioid = (drug_poisoning == 1 & 
                                       grepl("\\<T404", record_all)) + 0,
               natural_opioid = (drug_poisoning == 1 & 
                                     grepl("\\<T402", record_all)) + 0,
               other_opioid = (drug_poisoning == 1 & 
                                   grepl("\\<T406", record_all)) + 0)
}

flag_cocaine <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(cocaine = (drug_poisoning == 1 & 
                              grepl("\\<T405", record_all)) + 0)
}

flag_other_drug <- function(cleaned_df) {
    cleaned_df %>% 
        flag_drug_poisoning() %>% 
        mutate(other_drug = (drug_poisoning == 1 & 
                                 grepl("\\<T509", record_all)) + 0)
}

flag_alcohol<- function(cleaned_df) {
    cleaned_df %>% 
        mutate(alcohol = grepl("\\<T51", record_all) + 0)
}

flag_solvents <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(solvents = grepl("\\<T5[23]{1}", record_all) + 0)
}

flag_gasses <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(gasses = grepl("\\<T5[89]{1}", record_all) + 0)
}

flag_other_nondrug <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(other_nondrug = grepl("\\<T5[4-7]{1}|\\<T60|\\<T65", record_all) + 0)
}

flag_solo_drugs <- function(cleaned_df) {
    cleaned_df %>% 
        mutate(
            opioids_alone = case_when(
                opioids == 1 &
                    (benzos + cocaine + meth + nonopioid_analgesics + 
                         psychotropics + anticoag + other_nonopioid_substance + 
                         other_drug) == 0 ~ 1,
                TRUE ~ 0),
            meth_alone = case_when(
                meth == 1 &
                    (benzos + cocaine + opioids +  nonopioid_analgesics + 
                         anticoag + other_nonopioid_substance + 
                         other_drug) == 0 ~ 1,
                TRUE ~ 0),
            cocaine_alone = case_when(
                cocaine == 1 &
                    (benzos + meth + opioids +  nonopioid_analgesics + 
                         psychotropics + anticoag + other_nonopioid_substance + 
                         other_drug) == 0 ~ 1,
                TRUE ~ 0),
            benzos_alone = case_when(
                benzos == 1 &
                    (cocaine + meth + opioids +  nonopioid_analgesics + 
                         anticoag + other_nonopioid_substance + 
                         other_drug) == 0 ~ 1,
                TRUE ~ 0),
            no_specified_drug = case_when(
                other_drug == 1 &
                    (cocaine + meth + opioids +  nonopioid_analgesics + 
                         other_narco_psychodysleptics + 
                         psychotropics + anticoag + alcohol + other_nonopioid_substance + 
                         benzos) == 0 ~ 1,
                TRUE ~ 0)
        ) %>% 
        mutate(
            heroin_alone = case_when(
                opioids_alone == 1 & 
                    heroin == 1 & 
                    (synthetic_opioid +
                         natural_opioid + 
                         other_opioid) == 0 ~ 1,
                TRUE ~ 0),
            natural_alone = case_when(
                opioids_alone == 1 & 
                    natural_opioid == 1 & 
                    (synthetic_opioid +
                         heroin + 
                         other_opioid) == 0 ~ 1,
                TRUE ~ 0),
            synthetic_alone = case_when(
                opioids_alone == 1 & 
                    synthetic_opioid == 1 & 
                    (heroin +
                         natural_opioid + 
                         other_opioid) == 0 ~ 1,
                TRUE ~ 0)
        )
}

## Get population denominators ----
## Note that because we decided to do race*education as one of the analyses,
## we cannot use a pre-made ACS table and need to use the microdata. This
## needs to be pulled manually. See ./data_raw/acs_microdata_pulls/README.md
## for details on how we pulled it. 
read_acs_microdata <- function(f_name) {
    x <- read_csv(
        f_name,
        skip = 7,
        col_names = c("target_pop", "total", "hispanic", "nonhispanic")
    )
    
    x$year <- as.numeric(substr(basename(f_name),
                                nchar(basename(f_name)) - 7,
                                nchar(basename(f_name)) - 4))
    x$educ <- c("all",
                rep("under25", 31),
                "over25",
                rep(c(
                    "over25", ">bs", "< hs", "hs", "< bs", "bs"
                ), 5))
    
    x$race <- c("all",
                "all",
                rep(c(
                    "white", "black", "aian", "asian", "other"
                ), each = 6),
                "all",
                rep(c(
                    "white", "black", "aian", "asian", "other"
                ), each = 6))
    
    x[c(1:3, 9, 15, 21, 27, 33:63), - 1]
}

read_acs_microdata_by_sex <- function(f_name) {
    x <- read_csv(
        f_name,
        skip = 5,
        col_names = c("target_pop", "total", "hispanic", "nonhispanic")
    )
    
    x$year <- as.numeric(substr(basename(f_name),
                                nchar(basename(f_name)) - 7,
                                nchar(basename(f_name)) - 4))
    x$educ <- "all"
    
    x$race <- c(
        rep("all", 3),
        c("white", "black", "aian", "asian", "other"),
        rep("all", 2),
        c("white", "black", "aian", "asian", "other")
    )
    
    x$sex <- c("both", rep(c("male", "female"), each = 7))
    
    x[c(3:8, 10:15), - 1]
}

read_acs_microdata_by_age <- function(f_name) {
    x <- read_csv(
        f_name,
        skip = 5,
        col_names = c("target_pop", "total", "hispanic", "nonhispanic")
    )
    
    x$year <- as.numeric(substr(basename(f_name),
                                nchar(basename(f_name)) - 7,
                                nchar(basename(f_name)) - 4))
    x$educ <- "all"
    
    x$race <- c(
        rep("all", 2),
        rep(c("white", "black", "aian", "asian", "other"), each = 6)
    )
    
    x$age_grp = c(
        rep("all", 2),
        rep(c("all", "65 and up", "0 to 17", "18 to 24", "25 to 39", "40 to 64"), 5)
    )
    
    x[, -1]
}

## Imports joinpoint results ----
## Give it the path of a joinpoint result file (f_path). Returns a tibble.
import_jp <- function(f_path, ctypes = NULL) {
    readr::read_delim(f_path, delim = ";", col_types = ctypes) %>%
        janitor::clean_names(.)
}

## Categorize string columns into factors ----
categorize_race <- function(df) {
    df %>%
        mutate(
            race_cat = factor(
                race,
                levels = c(
                    "all",
                    "white",
                    "black",
                    "hispanic",
                    "asian",
                    "other",
                    "all_nonhisp"
                ),
                labels = c(
                    "Total",
                    "Non-Hispanic White",
                    "Non-Hispanic Black",
                    "Hispanic",
                    "Non-Hispanic Asian",
                    "Other Non-Hispanic",
                    "All non-Hispanic"
                ),
                ordered = TRUE
            ),
            race_cat_rev = factor(
                race,
                levels = rev(
                    c(
                        "all",
                        "white",
                        "black",
                        "hispanic",
                        "asian",
                        "other",
                        "all_nonhisp"
                    )
                ),
                labels = rev(
                    c(
                        "Total",
                        "Non-Hispanic White",
                        "Non-Hispanic Black",
                        "Hispanic",
                        "Non-Hispanic Asian",
                        "Other Non-Hispanic",
                        "All non-Hispanic"
                    )
                ),
                ordered = TRUE
            )
        )
}

categorize_educ <- function(df) {
    df %>%
        mutate(
            educ_cat = factor(
                educ,
                levels = c("all",
                           "over25",
                           "under25",
                           "< hs",
                           "hs",
                           "< bs",
                           "bs",
                           ">bs"),
                labels = c(
                    "All levels",
                    "All 25+ y/o",
                    "All <25 y/o",
                    "Less than HS",
                    "HS / GED",
                    "Less than BA/BS",
                    "BA/BS",
                    "Graduate"
                ),
                ordered = TRUE
            ),
            educ_cat_rev = factor(
                educ,
                levels = rev(
                    c("all",
                      "over25",
                      "under25",
                      "< hs",
                      "hs",
                      "< bs",
                      "bs",
                      ">bs")
                ),
                labels = rev(
                    c(
                        "All levels",
                        "All 25+ y/o",
                        "All <25 y/o",
                        "Less than HS",
                        "HS / GED",
                        "Less than BA/BS",
                        "BA/BS",
                        "Graduate"
                    )
                ),
                ordered = TRUE
            )
        )
}

categorize_sex <- function(df) {
    df %>% 
        mutate(
            sex_cat = factor(
                sex, 
                levels = c("both", "male", "female"),
                labels = c("All", "Male", "Female"),
                ordered = TRUE
            )
        )
}

categorize_region <- function(df) {
    df %>%
        mutate(
            region_cat = factor(
                region_name,
                levels = c(
                    "California",
                    "Superior California",
                    "North Coast",
                    "San Francisco Bay Area",
                    "Northern San Joaquin Valley",
                    "Central Coast",
                    "Southern San Joaquin Valley",
                    "Inland Empire",
                    "Los Angeles County",
                    "Orange County",
                    "San Diego - Impreial"
                ),
                labels = c(
                    "California",
                    "Superior California",
                    "North Coast",
                    "San Francisco Bay Area",
                    "Northern San Joaquin Valley",
                    "Central Coast",
                    "Southern San Joaquin Valley",
                    "Inland Empire",
                    "Los Angeles County",
                    "Orange County",
                    "San Diego - Imperial"
                ),
                ordered = TRUE
            ),
            region_cat_rev = factor(
                region_name,
                levels = rev(
                    c(
                        "California",
                        "Superior California",
                        "North Coast",
                        "San Francisco Bay Area",
                        "Northern San Joaquin Valley",
                        "Central Coast",
                        "Southern San Joaquin Valley",
                        "Inland Empire",
                        "Los Angeles County",
                        "Orange County",
                        "San Diego - Impreial"
                    )
                ),
                labels = rev(
                    c(
                        "California",
                        "Superior California",
                        "North Coast",
                        "San Francisco Bay Area",
                        "Northern San Joaquin Valley",
                        "Central Coast",
                        "Southern San Joaquin Valley",
                        "Inland Empire",
                        "Los Angeles County",
                        "Orange County",
                        "San Diego - Imperial"
                    )
                ), 
                ordered = TRUE),
            region_cat_num = factor(
                region_name,
                levels = c(
                    "California",
                    "Superior California",
                    "North Coast",
                    "San Francisco Bay Area",
                    "Northern San Joaquin Valley",
                    "Central Coast",
                    "Southern San Joaquin Valley",
                    "Inland Empire",
                    "Los Angeles County",
                    "Orange County",
                    "San Diego - Impreial"
                ), 
                labels = c(
                    "California",
                    "(1) Superior California",
                    "(2) North Coast",
                    "(3) San Francisco Bay Area",
                    "(4) Northern San Joaquin Valley",
                    "(5) Central Coast",
                    "(6) Southern San Joaquin Valley",
                    "(7) Inland Empire",
                    "(8) Los Angeles County",
                    "(9) Orange County",
                    "(10) San Diego - Imperial"
                ),
                ordered = TRUE
            )
        )
}

categorize_death <- function(df) {
    df %>%
        mutate(
            death_cat_rev = factor(
                death_type,
                levels = rev(
                    c(
                        "all_deaths",
                        "non_covid",
                        "covid", 
                        "any_poisoning",
                        "drug_poisoning",
                        "nondrug_poisoning",
                        "alcohol",
                        "benzos",
                        "cocaine",
                        "meth",
                        "opioids",
                        "heroin",
                        "natural_opioid",
                        "synthetic_opioid",
                        "other_opioid",
                        "intent_drug_poisoning",
                        "assault_drug_poisoning",
                        "unintent_drug_poisoning",
                        "unknown_drug_poisoning",
                        "intent_nondrug_poisoning",
                        "assault_nondrug_poisoning",
                        "unintent_nondrug_poisoning",
                        "unknown_nondrug_poisoning",
                        "cocaine_alone",
                        "benzos_alone", 
                        "meth_alone", 
                        "opioids_alone",
                        "heroin_alone",
                        "natural_alone",
                        "synthetic_alone", 
                        "no_specified_drug",
                        "other_drug"
                    )
                ),
                labels = rev(
                    c(
                        "All causes",
                        "All non-COVID-19",
                        "COVID-19", 
                        "Any poisoning",
                        "Drug poisoning",
                        "Non-drug poisoning",
                        "Alcohol",
                        "Benzodiazepine",
                        "Cocaine",
                        "Methamphetamine",
                        "Opioids",
                        "Heroin",
                        "Natural opioid",
                        "Synthetic opioid",
                        "Other opioid",
                        "Intentional drug poisoning",
                        "Assault drug poisoning",
                        "Unintentional drug poisoning",
                        "Unknown intent drug poisoning",
                        "Intentional nondrug poisoning",
                        "Assault nondrug poisoning",
                        "Unintentional nondrug poisoning",
                        "Unknown intent nondrug poisoning", 
                        "Cocaine alone",
                        "Benzodiazepine alone", 
                        "Methamphetamine alone", 
                        "Opioids alone",
                        "Heroin alone",
                        "Natural opioid alone",
                        "Synthetic opioid alone", 
                        "No specified drug",
                        "Other drug alone"
                    )
                ),
                ordered = TRUE
            ),
            death_cat = factor(
                death_type,
                levels = c(
                    "all_deaths",
                    "non_covid",
                    "covid", 
                    "any_poisoning",
                    "drug_poisoning",
                    "nondrug_poisoning",
                    "alcohol",
                    "benzos",
                    "cocaine",
                    "meth",
                    "opioids",
                    "heroin",
                    "natural_opioid",
                    "synthetic_opioid",
                    "other_opioid",
                    "intent_drug_poisoning",
                    "assault_drug_poisoning",
                    "unintent_drug_poisoning",
                    "unknown_drug_poisoning",
                    "intent_nondrug_poisoning",
                    "assault_nondrug_poisoning",
                    "unintent_nondrug_poisoning",
                    "unknown_nondrug_poisoning",
                    "cocaine_alone",
                    "benzos_alone", 
                    "meth_alone", 
                    "opioids_alone",
                    "heroin_alone",
                    "natural_alone",
                    "synthetic_alone", 
                    "no_specified_drug",
                    "other_drug"
                ),
                labels = c(
                    "All causes",
                    "All non-COVID-19",
                    "COVID-19", 
                    "Any poisoning",
                    "Drug poisoning",
                    "Non-drug poisoning",
                    "Alcohol",
                    "Benzodiazepine",
                    "Cocaine",
                    "Methamphetamine",
                    "Opioids",
                    "Heroin",
                    "Natural opioid",
                    "Synthetic opioid",
                    "Other opioid",
                    "Intentional drug poisoning",
                    "Assault drug poisoning",
                    "Unintentional drug poisoning",
                    "Unknown intent drug poisoning",
                    "Intentional nondrug poisoning",
                    "Assault nondrug poisoning",
                    "Unintentional nondrug poisoning",
                    "Unknown intent nondrug poisoning", 
                    "Cocaine alone",
                    "Benzodiazepine alone", 
                    "Methamphetamine alone", 
                    "Opioids alone",
                    "Heroin alone",
                    "Natural opioid alone",
                    "Synthetic opioid alone", 
                    "No specified drug",
                    "Other drug alone"
                ),
                ordered = TRUE
            )
        )
}

## DHR / ARIMA helpers ----
k_autoarima <- function(k, p = 365.25 / 7) {
    ARIMA(n_deaths ~ fourier(K =  k, period = p) + PDQ(0, 0, 0),  
          stepwise = FALSE, 
          approximation = TRUE)
}

extract_fitted_values <- function(death_df,
                                  race_x,
                                  death_x,
                                  educ_x,
                                  exclude_hispanic_x, 
                                  best_model_df,
                                  forecast_start = FORECAST_START,
                                  last_week = LAST_WEEK,
                                  n_reps = N_BOOT) {
    
    ## Number of weeks to forecast
    h_x <- round(as.numeric(difftime(last_week, forecast_start, units = "weeks")))
    
    ## Get observed total number of deaths in our interval of interest
    sub_death <- death_df %>% 
        filter(race == race_x,
               educ == educ_x,
               death_type == death_x)
    sub_model <- best_model_df %>% 
        ungroup() %>% 
        filter(race == race_x,
               educ == educ_x,
               death_type == death_x)
    
    skeleton <- sub_death %>%
        filter(date <= as.Date(LAST_WEEK), 
               death_type == death_x) %>%
        mutate(yearweek = yearweek(date)) %>%
        select(yearweek,
               date,
               date_start,
               week_from_start,
               race,
               educ, 
               death_type,
               obs_deaths = n_deaths)
    
    ## Pull out the model used for forecasting
    target_model <- sub_model %>%
        pull(model)
    target_model <- target_model[[1]]
    
    left_join(
        skeleton,
        bind_rows(
            fitted(target_model) %>%
                select(yearweek, fitted = .fitted) %>%
                as_tibble(),
            forecast(
                target_model,
                simulate = TRUE,
                h = FORECAST_INTERVAL,
                bootstrap = TRUE,
                times = n_reps
            ) %>%
                mutate(fitted = hilo(n_deaths)) %>%
                unpack_hilo(fitted) %>%
                as_tibble() %>% 
                select(-n_deaths) %>% 
                select(yearweek,
                       fitted = .mean,
                       fitted_lower,
                       fitted_upper) 
        )
    )
}

calculate_excess_deaths <- function(death_df,
                                    race_x,
                                    educ_x,  
                                    death_x,
                                    best_model_df,
                                    forecast_start = FORECAST_START,
                                    last_week = LAST_WEEK,
                                    n_reps = N_BOOT) {
    ## Number of weeks to forecast
    h_x <- round(as.numeric(difftime(last_week, forecast_start, units = "weeks")))
    
    ## Get observed total number of deaths in our interval of interest
    sub_death <- death_df %>% 
        filter(race == race_x,
               educ == educ_x,
               death_type == death_x)
    sub_model <- best_model_df %>% 
        ungroup() %>% 
        filter(race == race_x,
               educ == educ_x,
               death_type == death_x)
    
    observed_total_deaths <- sub_death %>%
        filter(between(date, as.Date(forecast_start), as.Date(last_week)),
               death_type == death_x) %>%
        pull(n_deaths) %>%
        sum()
    
    ## Pull out the model used for forecasting
    target_model <- sub_model %>%
        pull(model)
    target_model <- target_model[[1]]
    
    ## Get bootstrap samples of the expected total deaths
    expected_total_deaths <- replicate(expr = {
        forecast(
            target_model,
            simulate = TRUE,
            times = 1,
            h = h_x
        ) %>%
            pull(.mean) %>%
            sum()
    },
    n = n_reps)
    
    excess_deaths <- observed_total_deaths - expected_total_deaths
    
    tibble(
        race = race_x,
        educ = educ_x,
        death_type = death_x,
        start_interval = as.Date(forecast_start),
        end_interval = as.Date(last_week),
        n = n_reps,
        mean = mean(excess_deaths),
        p025 = quantile(excess_deaths, .025),
        p250 = quantile(excess_deaths, .25),
        p500 = quantile(excess_deaths, .5),
        p750 = quantile(excess_deaths, .75),
        p975 = quantile(excess_deaths, .975),
        min = min(excess_deaths),
        max = max(excess_deaths),
        sd = sd(excess_deaths)
    )
}

## Extra data ----
ca_regions_fips <- tibble::tribble(
    ~region,                  ~region_name,      ~county_name, ~county_fips, ~county_cdph, 
    3L,      "San Francisco Bay Area",         "Alameda",           1L,        "001",
    4L, "Northern San Joaquin Valley",          "Alpine",           3L,        "002",
    4L, "Northern San Joaquin Valley",          "Amador",           5L,        "003",
    1L,         "Superior California",           "Butte",           7L,        "004",
    4L, "Northern San Joaquin Valley",       "Calaveras",           9L,        "005",
    1L,         "Superior California",          "Colusa",          11L,        "006",
    3L,      "San Francisco Bay Area",    "Contra Costa",          13L,        "007",
    2L,                 "North Coast",       "Del Norte",          15L,        "008",
    1L,         "Superior California",       "El Dorado",          17L,        "009",
    6L, "Southern San Joaquin Valley",          "Fresno",          19L,        "010",
    1L,         "Superior California",           "Glenn",          21L,        "011",
    2L,                 "North Coast",        "Humboldt",          23L,        "012",
    10L,        "San Diego - Impreial",        "Imperial",         25L,        "013",
    6L, "Southern San Joaquin Valley",            "Inyo",          27L,        "014",
    6L, "Southern San Joaquin Valley",            "Kern",          29L,        "015",
    6L, "Southern San Joaquin Valley",           "Kings",          31L,        "016",
    2L,                 "North Coast",            "Lake",          33L,        "017",
    1L,         "Superior California",          "Lassen",          35L,        "018",
    8L,          "Los Angeles County",     "Los Angeles",          37L,        "019",
    4L, "Northern San Joaquin Valley",          "Madera",          39L,        "020",
    3L,      "San Francisco Bay Area",           "Marin",          41L,        "021",
    4L, "Northern San Joaquin Valley",        "Mariposa",          43L,        "022",
    2L,                 "North Coast",       "Mendocino",          45L,        "023",
    4L, "Northern San Joaquin Valley",          "Merced",          47L,        "024",
    1L,         "Superior California",           "Modoc",          49L,        "025",
    4L, "Northern San Joaquin Valley",            "Mono",          51L,        "026",
    5L,               "Central Coast",        "Monterey",          53L,        "027",
    2L,                 "North Coast",            "Napa",          55L,        "028",
    1L,         "Superior California",          "Nevada",          57L,        "029",
    9L,               "Orange County",          "Orange",          59L,        "030",
    1L,         "Superior California",          "Placer",          61L,        "031",
    1L,         "Superior California",          "Plumas",          63L,        "032",
    7L,               "Inland Empire",       "Riverside",          65L,        "033",
    1L,         "Superior California",      "Sacramento",          67L,        "034",
    5L,               "Central Coast",      "San Benito",          69L,        "035",
    7L,               "Inland Empire",  "San Bernardino",          71L,        "036",
    10L,        "San Diego - Impreial",      "San Diego",          73L,        "037",
    3L,      "San Francisco Bay Area",   "San Francisco",          75L,        "038",
    4L, "Northern San Joaquin Valley",     "San Joaquin",          77L,        "039",
    5L,               "Central Coast", "San Luis Obispo",          79L,        "040",
    3L,      "San Francisco Bay Area",       "San Mateo",          81L,        "041",
    5L,               "Central Coast",   "Santa Barbara",          83L,        "042",
    3L,      "San Francisco Bay Area",     "Santa Clara",          85L,        "043",
    5L,               "Central Coast",      "Santa Cruz",          87L,        "044",
    1L,         "Superior California",          "Shasta",          89L,        "045",
    1L,         "Superior California",          "Sierra",          91L,        "046",
    1L,         "Superior California",        "Siskiyou",          93L,        "047",
    3L,      "San Francisco Bay Area",          "Solano",          95L,        "048",
    2L,                 "North Coast",          "Sonoma",          97L,        "049",
    4L, "Northern San Joaquin Valley",      "Stanislaus",          99L,        "050",
    1L,         "Superior California",          "Sutter",         101L,        "051",
    1L,         "Superior California",          "Tehama",         103L,        "052",
    2L,                 "North Coast",         "Trinity",         105L,        "053",
    6L, "Southern San Joaquin Valley",          "Tulare",         107L,        "054",
    4L, "Northern San Joaquin Valley",        "Tuolumne",         109L,        "055",
    5L,               "Central Coast",         "Ventura",         111L,        "056",
    1L,         "Superior California",            "Yolo",         113L,        "057",
    1L,         "Superior California",            "Yuba",         115L,        "058"
)
