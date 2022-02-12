## Imports ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Data ----
reshaped_pop <- bind_rows(
    readRDS(here("data", "pop_by_race_educ_extrapolated.RDS")) %>% 
        mutate(region_name = "California",
               region = 99),
    readRDS(here("data", "pop_by_race_region_extrapolated.RDS"))
)

cume_excess_deaths <- bind_rows(
    readRDS(here("data", "cume_excess_deaths.RDS")) %>%
        filter(educ == "all",
               race == "all",
               date == max(date)) %>%
        mutate(region_name = "California",
               region = 99),
    readRDS(here("data", "cume_excess_deaths_region.RDS")) %>%
        filter(educ == "all",
               race == "all",
               date == max(date)) 
) %>%
    complete(nesting(region, region_name, race, educ, date), death_type) %>% 
    filter(
        death_type != "all_deaths",
        death_type != "non_covid",
        death_type != "other_opioid",
        death_type != "any_poisoning",
        death_type != "nondrug_poisoning",
        death_type != "unintent_nondrug_poisoning",
        death_type != "intent_nondrug_poisoning",
        death_type != "intent_drug_poisoning", 
        death_type != "no_specified_drug", 
        death_type != "other_drug", 
        !grepl("_alone", death_type, fixed = TRUE), 
    ) %>% 
    mutate(year = lubridate::year(date)) %>% 
    left_join(reshaped_pop) %>% 
    categorize_region() %>% 
    categorize_race() %>% 
    categorize_educ() %>% 
    categorize_death() %>% 
    filter(region != 99) %>%
    filter(
        !(death_type %in% c(
            "drug_poisoning",
            "meth",
            "opioids",
            "unintent_drug_poisoning"
        ))
    )

per_capita_cume <- ggplot(
    cume_excess_deaths,
    aes(
        x = fitted / pop * 100000,
        xmin = lower / pop * 100000,
        xmax = upper / pop * 100000,
        y = region_cat_rev
    )
) +
    geom_vline(xintercept = 0, 
               color = "black",
               alpha = .7) + 
    geom_errorbarh(height = .2, 
                   position = position_dodge(width = .7),
                   alpha = .8) +
    geom_point(aes(size = abs(fitted)), 
               position = position_dodge(width = .7),
               alpha = .8) + 
    scale_x_continuous("Cumulative excess deaths per 100,000 population (95% CI)") + 
    scale_y_discrete("California Region (ordered by latitude)") + 
    mk_nytimes(legend.position = "bottom") + 
    scale_size_binned_area("Absolute number of excess deaths",
                           breaks = c(0,  10, 25, 50, 100, 250),
                           labels = function(x) round(x),
                           max_size = 6, 
                           guide = guide_legend(title.position = "top",
                                                direction = "horizontal")) + 
    facet_wrap(~ death_cat, ncol = 3) 

ggsave(
    here("plots", "figS08_cume_excess_deaths_per_100k_by_region_ebars.pdf"),
    per_capita_cume,
    width = 12,
    height = 6,
    scale = .8,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS08_cume_excess_deaths_per_100k_by_region_ebars.jpg"),
    per_capita_cume,
    width = 12,
    height = 6,
    scale = .8,
    dpi = 600,
    device = grDevices::jpeg
)

write_csv(
    cume_excess_deaths %>%
        select(
            region,
            region_name,
            race_cat,
            educ_cat,
            death_cat,
            fitted,
            se,
            upper,
            lower
        ) %>%
        arrange(death_cat, region),
    here("output", "figS08_data_region.csv")
)
