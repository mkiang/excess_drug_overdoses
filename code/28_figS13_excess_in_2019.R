## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here("code", "mk_nytimes.R"))

## Data ----
reshaped_pop <- readRDS(here("data", "pop_by_race_educ_extrapolated.RDS"))
cume_excess_deaths <- bind_rows(
    readRDS(here("data", "supp_2019_cume_excess_deaths.RDS")) %>% 
        filter(date == max(date)),
    readRDS(here("data", "cume_excess_deaths.RDS")) %>% 
        filter(date == max(date))
    ) %>%
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
        educ == "all"
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
    geom_point(position = position_dodge(width = .7),
               alpha = .9) + 
    scale_x_continuous("Cumulative excess deaths (95% CI)") + 
    scale_y_discrete("Type of death") + 
    mk_nytimes(legend.position = "right") + 
    scale_color_brewer("Race and Ethnicity", 
                       palette = "Dark2") + 
    facet_wrap(~ year) + 
    labs(title = "Excess deaths")

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
    geom_point(position = position_dodge(width = .7),
               alpha = .9) + 
    scale_x_continuous("Cumulative excess deaths per 100,000 population (95% CI)") + 
    scale_y_discrete("Type of death") + 
    mk_nytimes(legend.position = "none") + 
    scale_color_brewer("Race and Ethnicity", 
                       palette = "Dark2") +
    scale_size_binned_area("Absolute number\nof excess deaths",
                           breaks = c(0,  50, 100, 250, 500, 1000),
                           labels = function(x) round(x),
                           max_size = 6) + 
    facet_wrap(~ year) + 
    labs(title = "Excess deaths per capita")

## Put plots together ----
p1 <- abs_cume + per_capita_cume + plot_layout(ncol = 1)

## Save ----
ggsave(
    here("plots", "figS13_comparing_2019_2020.pdf"),
    p1,
    width = 8.5,
    height = 6.5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS13_comparing_2019_2020.jpg"),
    p1,
    width = 8.5,
    height = 6.5,
    scale = 1,
    dpi = 600
)
