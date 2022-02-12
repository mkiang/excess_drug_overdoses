## Imports ----
library(tidyverse)
library(here)
library(usmap)
library(patchwork)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

## Data ----
reshaped_pop <- bind_rows(readRDS(here(
    "data", "pop_by_race_educ_extrapolated.RDS"
)) %>%
    mutate(region_name = "California",
           region = 99),
readRDS(here(
    "data", "pop_by_race_region_extrapolated.RDS"
)))

cume_excess_deaths <- bind_rows(
    readRDS(here("data", "cume_excess_deaths.RDS")) %>%
        filter(educ == "all",
               race == "all",
               date == max(date)) %>%
        mutate(region_name = "California",
               region = 99),
    readRDS(here(
        "data", "cume_excess_deaths_region.RDS"
    )) %>%
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
           death_type != "all_deaths") %>%
    left_join(ca_regions_fips) %>%
    mutate(fips = sprintf("06%03d", county_fips),
           fitted_100k = fitted / pop * 100000)

brks <- c(
    min(ca_county$fitted_100k, na.rm = TRUE),
    0,
    2.5,
    5,
    max(ca_county$fitted_100k, na.rm = TRUE)
)

p1 <- usmap::plot_usmap(
    regions = "counties",
    include = "CA",
    data = ca_county %>%
        filter(
            death_type %in% c(
                "drug_poisoning",
                "meth",
                "opioids",
                "unintent_drug_poisoning"
            )
        ),
    values = "fitted_100k",
    color = NA
) +
    facet_wrap( ~ death_cat, nrow = 1) +
    scale_fill_viridis_c(
        "Cumulative excess deaths\nper 100,000 population",
        option = "B",
        guide = guide_colorbar(
            title.position = "left",
            barwidth = unit(.2, "cm"),
            barheight = unit(5, "cm")
        ),
        na.value = "grey40",
        breaks = brks,
        labels = c(sprintf("%0.1f", round(brks, 1))),
        end = .85
    ) +
    mk_nytimes(legend.position = "none",
               legend.justification = 0,
               axis.text = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               panel.grid.major = element_blank())

p2 <- per_capita_cume <- ggplot(
    ca_county %>%
        filter(
            death_type %in% c(
                "drug_poisoning",
                "meth",
                "opioids",
                "unintent_drug_poisoning"
            )
        ),
    aes(
        x = fitted / pop * 100000,
        xmin = lower / pop * 100000,
        xmax = upper / pop * 100000,
        y = region_cat_rev,
        color = fitted_100k
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
    scale_y_discrete("California Region\n(ordered by latitude)") + 
    scale_color_viridis_c(NULL,
        option = "B",
        guide = "none", 
        end = .85
    ) + 
    mk_nytimes(legend.position = "bottom") + 
    scale_size_binned_area("Absolute number of excess deaths",
                           breaks = c(0,  20, 50, 100, 250, 500),
                           labels = function(x) round(x),
                           max_size = 6, 
                           guide = guide_legend(title.position = "top",
                                                direction = "horizontal")) + 
    facet_wrap(~ death_cat, ncol = 4)

p_all <- p1 + p2 + plot_layout(nrow = 2, heights = c(6.5, 3))

## Save ----
ggsave(
    here(
        "plots",
        "fig03_cume_excess_deaths_per_100k_by_region.pdf"
    ),
    p_all,
    width = 9,
    height =7,
    scale = 1.1,
    device = cairo_pdf
)
ggsave(
    here(
        "plots",
        "fig03_cume_excess_deaths_per_100k_by_region.jpg"
    ),
    p_all,
    width = 9,
    height =7,
    scale = 1.1,
    dpi = 600,
    device = grDevices::jpeg
)

## Data ----
write_csv(
    ca_county %>%
        filter(
            death_type %in% c(
                "drug_poisoning",
                "meth",
                "opioids",
                "unintent_drug_poisoning"
            )
        ) %>% select(
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
        distinct() %>% 
        arrange(death_cat, region),
    here("output", "fig03_data_excess_by_region.csv")
)
