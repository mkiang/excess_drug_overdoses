## Imports ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))

## Data ----
reshaped_pop <- readRDS(here("data", "pop_by_race_educ_extrapolated.RDS"))
cume_excess_deaths <- readRDS(here("data", "cume_excess_deaths.RDS")) %>%
    filter(
        death_type != "all_deaths",
        death_type != "non_covid",
        death_type != "other_opioid",
        death_type != "any_poisoning",
        death_type != "nondrug_poisoning",
        death_type != "unintent_nondrug_poisoning",
        death_type != "intent_nondrug_poisoning",
        death_type != "no_specified_drug", 
        death_type != "other_drug", 
        !grepl("_alone", death_type, fixed = TRUE), 
        race == "all",
        educ != "all",
        educ != "over25",
        educ != "under25",
        date == max(date)
    ) %>%
    mutate(year = lubridate::year(date)) %>%
    left_join(reshaped_pop)

## Per capita ----
per_capita_cume <- ggplot(
    cume_excess_deaths,
    aes(
        x = fitted / pop * 100000,
        xmin = lower / pop * 100000,
        xmax = upper / pop * 100000,
        y = death_cat_rev,
        color = educ_cat,
        group = educ_cat
    )
) +
    geom_vline(xintercept = 0,
               color = "black",
               alpha = .7) +
    geom_errorbarh(height = .2,
                   position = position_dodge(width = .7),
                   alpha = .9) +
    geom_point(aes(size = abs(fitted)),
               position = position_dodge(width = .7),
               alpha = .9) +
    scale_x_continuous("Cumulative excess deaths per 100,000 population (95% CI)") +
    scale_y_discrete("Type of death") +
    mk_nytimes(legend.position = "right") +
    scale_color_manual("Educational attainment",
                       values = viridis::viridis_pal(begin = 0, end = .9)(5)) +
    scale_size_binned_area(
        "Absolute number\nof excess deaths",
        breaks = c(0,  25, 50, 100, 250),
        labels = function(x)
            round(x),
        max_size = 6
    )

## Save ----
ggsave(
    here("plots", "fig02_cume_excess_deaths_per_100k_by_educ.pdf"),
    per_capita_cume,
    width = 6.5,
    height = 4,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig02_cume_excess_deaths_per_100k_by_educ.jpg"),
    per_capita_cume,
    width = 6.5,
    height = 4,
    scale = 1,
    dpi = 600
)

## Data ----
write_csv(
    cume_excess_deaths %>%
        select(death_cat,
               educ_cat,
               race_cat,
               date, 
               pop,
               date,
               fitted,
               se,
               upper,
               lower),
    here("output", "fig02_data_excess_by_educ.csv")
)
