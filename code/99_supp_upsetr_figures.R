### Imports ----
library(here)
library(fs)
library(tidyverse)
library(janitor)
source(here("code" , "utils.R"))

### Constants ----
SAVE_DIR <- config::get("private_save_folder")
FIRST_WEEK <- config::get("first_week")
LAST_WEEK <- as.Date(config::get("last_week"))

### Make infrastructure ----
fs::dir_create(here("data_private"))

## Flag the relevant data ----
death_df <- readRDS(paste0(SAVE_DIR, "cleaned_line_data.RDS"))

## Subset to CA residents ----
death_df <- death_df %>%
    filter(state_name == "CA") %>%
    select(-state_name)

## Flag the death data ----
death_df <- death_df %>%
    flag_covid_death() %>%
    mutate(non_covid = 1 - covid,
           all_deaths = 1)

## Flag types only for non-COVID deaths
flagged_df <- death_df %>%
    filter(covid == 0) %>%
    flag_any_poisoning_death() %>%
    flag_drug_poisoning() %>%
    flag_nondrug_poisoning() %>%
    flag_benzos() %>%
    flag_cocaine() %>%
    flag_opioids() %>%
    flag_meth() %>%
    flag_nonopioid_analgesics() %>%
    flag_psychotropics() %>%
    flag_anticoag() %>%
    flag_other_drug() %>%
    flag_nonopioid_substance() %>%
    as_tibble() %>%
    filter(dod >= FIRST_WEEK,
           dod <= LAST_WEEK,
           drug_poisoning == 1) %>%
    transmute(
        race = case_when(
            race == "White" ~ "white",
            race == "Black" ~ "black",
            race == "Asian / Pacific Islander" ~ "asian",
            race == "Hispanic" ~ "hispanic",
            race == "Other" ~ "other",
            race == "American Indian / Native American" ~ "other"
        ),
        year = lubridate::year(dod),
        "Benzodiazepene" = benzos,
        "Cocaine" = cocaine,
        "Methamphetamine" = meth,
        "Heroin" = heroin,
        "Synthetic opioid" = synthetic_opioid,
        "Natural opioid" = natural_opioid,
        "Other opioid" = other_opioid,
        "Other drug" = other_drug
    )

## Save grobs in case we need to resize later ----
SET_ORDER <- rev(
    c(
        "Benzodiazepene",
        "Cocaine",
        "Methamphetamine",
        "Heroin",
        "Synthetic opioid",
        "Natural opioid",
        "Other opioid",
        "Other drug"
    )
)

loop_df <- flagged_df %>% 
    select(race, year) %>% 
    distinct()

for (i in 1:NROW(loop_df)) {
    temp_x <- UpSetR::upset(
        flagged_df %>%
            filter(year == loop_df$year[i],
                   race == loop_df$race[i]) %>% 
            as.data.frame(),
        nsets = 20,
        keep.order = TRUE,
        sets = SET_ORDER, 
        mb.ratio = c(.65, .35)
    )
    
    cairo_pdf(here("data_private", 
                   sprintf("plot_sets_%s_%i.pdf",
                           loop_df$race[i],
                           loop_df$year[i])), 
              width = 6, 
              height = 3.5, 
              pointsize = 7)
    print(temp_x)
    dev.off()
    
    jpeg(
        here(
            "data_private",
            sprintf("plot_sets_%s_%i.jpg",
                    loop_df$race[i],
                    loop_df$year[i])
        ),
        width = 6,
        height = 3.5,
        units = "in",
        quality = 100,
        res = 300, 
        pointsize = 7
    )
    print(temp_x)
    dev.off()
    
    saveRDS(temp_x, 
            here("data_private", 
                 sprintf("set_plot_%s_%i.RDS", 
                         loop_df$race[i], 
                         loop_df$year[i])))
}
