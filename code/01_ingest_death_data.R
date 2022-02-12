## 01_ingest_death_data.R ----
##
## This file takes Yea-Hung's minimally processed raw data and converts it to
## just the subset of columns we need for our analysis. This is CDPH restricted
## data and therefore can only be run in the secure environment.

## Imports ----
library(here)
library(fs)
library(tidyverse)

## Constants ----
RAW_FILE <- config::get("raw_data_file")
SAVE_DIR <- config::get("private_save_folder")
FORCE_REFRESH <- TRUE

## Infrastructure ----
fs::dir_create(SAVE_DIR)

if (!fs::file_exists(paste0(SAVE_DIR, "cleaned_line_data.RDS")) |
    FORCE_REFRESH) {
    ## Ingest ----
    raw_data <- readRDS(RAW_FILE)
    
    ## Recode race  ----
    raw_data <- raw_data %>%
        dplyr::mutate(
            race = dplyr::case_when(
                ## Being Hispanic trumps all other racial categorization rules
                hispanic.origin == "Y" ~ "Hispanic",
                ## Mixed, other, unknown.
                race.1.code.final != race.2.code.final ~ "Other",
                race.1.code.final != race.3.code.final ~ "Other",
                race.2.code.final != race.3.code.final ~ "Other",
                ## Then do single race
                race.1.code.final == 10 ~ "White",
                race.1.code.final == 20 ~ "Black",
                race.1.code.final %in% c(30, 57, 58) ~ "American Indian / Native American",
                race.1.code.final %in% c(41:49, 52:56, 59) ~ "Asian / Pacific Islander",
                race.1.code.final %in% c(99, 51) ~ "Other",
                ## Some rows missing everything
                is.na(race.1.code.final) &
                    is.na(race.2.code.final) &
                    is.na(race.3.code.final) ~ "Other",
                ## Search on this to make sure it's correct
                TRUE ~ "You missed a code if this label exists"
            )
        )
    
    ## Recode education  ----
    raw_data <- raw_data %>%
        dplyr::mutate(age_int = as.integer(age.in.years)) %>%
        dplyr::mutate(
            educ = dplyr::case_when(
                age_int < 25 ~ "under25",
                education.degree.nchs %in% 1:2 ~ "< hs",
                education.degree.nchs == 3 ~ "hs",
                education.degree.nchs %in% 4:5 ~ "< bs",
                education.degree.nchs == 6 ~ "bs",
                education.degree.nchs %in% 7:8 ~ ">bs",
                TRUE ~ "unknown edu"
            )
        )
    
    ## Recode sex  ----
    raw_data <- raw_data %>%
        dplyr::mutate(
            sex_recode = dplyr::case_when(sex == "F" ~ "female",
                                          sex == "M" ~ "male",
                                          TRUE ~ NA_character_)
        )
    
    ## Remove unnecessary (potentially identifying) columns ----
    min_data <- raw_data %>%
        dplyr::select(
            id,
            race,
            educ,
            age = age_int,
            sex = sex_recode,
            dod = date.of.death,
            state_name = residence.state.province,
            county_name = county.of.residence.geocode.text,
            county_fip = final.county.of.residence.geocode.nchs,
            county_cdph = final.county.of.residence.geocode.cdph,
            ucod = final.cause.of.death.icd10,
            dplyr::starts_with("record.axis.code")
        )
    
    ## Clean up a little bit and unite contributory causes ----
    min_data <- min_data %>%
        tidyr::unite(record_all, 
                     dplyr::starts_with("record.axis.code"), 
                     sep = " ") %>%
        dplyr::mutate(ucod = trimws(ucod),
                      record_all = trimws(gsub(
                          pattern = " NA", replacement = "", record_all
                      ))) %>%
        dplyr::select(-dplyr::starts_with("record.axis.code"))
    
    ## Save processed line data ----
    saveRDS(min_data,
            paste0(SAVE_DIR, "cleaned_line_data.RDS"),
            compress = "xz")
} else {
    min_data <- readRDS(paste0(SAVE_DIR, "cleaned_line_data.RDS"))
}
