## Imports ----
library(tidyverse)
library(here)
library(usmap)
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
        death_type != "non_covid", 
        death_type != "other_opioid",
        death_type != "any_poisoning",
        death_type != "nondrug_poisoning",
        death_type != "unintent_nondrug_poisoning",
        death_type != "intent_nondrug_poisoning", 
        death_type != "no_specified_drug", 
        death_type != "other_drug", 
        !grepl("_alone", death_type, fixed = TRUE), 
    ) %>% 
    mutate(year = lubridate::year(date)) %>% 
    left_join(reshaped_pop) %>% 
    categorize_region() %>% 
    categorize_race() %>% 
    categorize_educ() %>% 
    categorize_death()

ca_county <- cume_excess_deaths  %>% 
    filter(region != 99,
           death_type == "all_deaths") %>% 
    left_join(ca_regions_fips) %>% 
    mutate(fips = sprintf("06%03d", county_fips), 
           fitted_100k = fitted / pop * 100000)

brks <- c(min(ca_county$fitted_100k, na.rm = TRUE), 
          seq(80, 140, 20), 
          max(ca_county$fitted_100k, na.rm = TRUE))

p1 <- usmap::plot_usmap(
    regions = "counties",
    include = "CA",
    data = ca_county, 
    values = "fitted_100k",
    color = NA
) + 
    scale_fill_viridis_c(
        "Cumulative excess deaths\nper 100,000 population",
        option = "B",
        guide = guide_colorbar(
            title.position = "top",
            barwidth = unit(.25, "cm"),
            barheight = unit(7, "cm")
        ),
        breaks = brks,
        labels = round(brks), 
        na.value = "grey40",
        end = .85
    ) +
    theme(legend.position = c(1, .975),
          legend.justification = c(1, 1))

ggsave(
    here(
        "plots",
        "figS09_total_excess_deaths_per_100k_by_regional_map.pdf"
    ),
    p1,
    width = 3,
    height = 4.6,
    scale = 1.5,
    device = cairo_pdf
)
ggsave(
    here(
        "plots",
        "figS06_total_excess_deaths_per_100k_by_regional_map.jpg"
    ),
    p1,
    width = 3,
    height = 4.6,
    scale = 1.5,
    dpi = 600,
    device = grDevices::jpeg
)
write_csv(
    ca_county %>% 
        filter(death_type == "all_deaths") %>% 
        select(
            region,
            region_name,
            race_cat,
            educ_cat,
            death_cat,
            pop, 
            fitted,
            se,
            upper,
            lower
        ) %>% 
        distinct() %>% 
        arrange(death_cat, region),
    here("output", "figS09_data_region_map.csv")
)
