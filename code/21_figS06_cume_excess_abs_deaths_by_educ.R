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

## Absolute ----
abs_cume <- ggplot(
    cume_excess_deaths,
    aes(
        x = fitted,
        xmin = lower,
        xmax = upper,
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
    geom_point(position = position_dodge(width = .7),
               alpha = .9) + 
    scale_x_continuous("Cumulative excess deaths (95% CI)") + 
    scale_y_discrete("Type of death") + 
    mk_nytimes(legend.position = "right") + 
    scale_color_manual("Educational attainment",
                       values = viridis::viridis_pal(begin = 0, end = .9)(5)) +
    scale_size_binned_area("Absolute excess deaths\nper 100,000 population",
                           # breaks = c(0,  1, 100, 250, 500, 1000),
                           # labels = function(x) round(x),
                           max_size = 6)

ggsave(
    here("plots", "figS06_cume_excess_deaths_by_educ.pdf"),
    abs_cume,
    width = 6.5,
    height = 4,
    scale = 1,
    device = cairo_pdf
)

ggsave(
    here("plots", "figS06_cume_excess_deaths_by_educ.jpg"),
    abs_cume,
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
    here("output", "figS06_data_excess_by_educ.csv")
)
