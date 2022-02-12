## Imports ----
library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

## Data ----
reshaped_pop <- readRDS(here("data", "pop_by_race_sex_extrapolated.RDS"))
cume_excess_deaths <- readRDS(here("data", "supp_cume_excess_deaths_sex.RDS")) %>%
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
        # race != "other",
        # race != "asian",
        race != "all_nonhisp",
        educ == "all", 
        date == max(date)
    ) %>% 
    mutate(year = lubridate::year(date)) %>% 
    left_join(reshaped_pop) %>% 
    categorize_sex()

per_capita_cume <- ggplot(
    cume_excess_deaths,
    aes(
        x = fitted / pop * 100000,
        xmin = lower / pop * 100000,
        xmax = upper / pop * 100000,
        y = death_cat_rev, 
        color = race_cat,
        group = race_cat
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
    scale_color_brewer("Race and Ethnicity", 
                       palette = "Dark2") +
    scale_size_binned_area("Absolute number\nof excess deaths",
                           breaks = c(0,  50, 100, 250, 500, 1000),
                           labels = function(x) round(x),
                           max_size = 6) + 
    facet_wrap(~ sex_cat)

ggsave(
    here("plots", "figS04_cume_excess_deaths_per_100k_by_sex.pdf"),
    per_capita_cume,
    width = 8.5,
    height = 5,
    scale = .9,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS04_cume_excess_deaths_per_100k_by_sex.jpg"),
    per_capita_cume,
    width = 8.5,
    height = 5,
    scale = .9,
    dpi = 600
)
write_csv(cume_excess_deaths %>% 
              select(death_cat, 
                     educ_cat, 
                     race_cat, 
                     sex_cat, 
                     date, 
                     fitted, 
                     se, 
                     upper,
                     lower), 
          here("output", "figS04_data_excess_by_sex.csv"))
