## Imports ----
library(tidyverse)
library(here)
library(patchwork)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Data ----
reshaped_pop <- readRDS(here("data", "pop_by_race_educ_extrapolated.RDS"))
cume_excess_deaths <- bind_rows(
    readRDS(here("data", "cume_excess_deaths.RDS")) %>%
        filter(date == max(date)) %>%
        mutate(year = lubridate::year(date)) %>%
        left_join(reshaped_pop) %>%
        mutate(model_type = "main"),
    readRDS(here(
        "data", "supp_monthly_cume_excess_deaths.RDS"
    )) %>%
        filter(date == max(date)) %>%
        mutate(year = lubridate::year(date)) %>%
        left_join(reshaped_pop) %>%
        mutate(model_type = "monthly"),
    readRDS(here(
        "data", "supp_dhr_excess_deaths_bootstrapped.RDS"
    )) %>%
        transmute(
            date = end_interval,
            fitted = mean,
            lower = p025,
            upper = p975,
            death_type,
            race, 
            educ,
            race_cat,
            educ_cat) %>%
        mutate(model_type = "arima")
) %>%
    mutate(model_cat = factor(
        model_type,
        labels = c("Main (Poisson weekly)", "Poisson monthly", "ARIMA"),
        levels = c("main", "monthly", "arima"),
        ordered = TRUE
    )) %>%
    as_tibble() %>%
    categorize_death() %>%
    categorize_race() %>%
    categorize_educ() %>% 
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
        educ != "under25",
        educ != "unknown edu"
    ) %>%
    group_by(race, educ, death_type) %>%
    filter(n() == 3) %>%
    ungroup()

## Absolute ----
abs_cume_race <- ggplot(
    cume_excess_deaths %>%
        filter(educ == "all"),
    aes(
        x = fitted,
        xmin = lower,
        xmax = upper,
        y = death_cat_rev,
        color = model_cat,
        group = model_cat
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
    mk_nytimes(legend.position = "none",
               axis.text.x = element_text(
                   angle = 45,
                   vjust = 1,
                   hjust = 1
               ))+
    scale_color_brewer("Estimation method",
                       palette = "Set1") +
    facet_grid( ~ race_cat, scales = "free") + 
    labs(title = "By race and ethnicity")

abs_cume_educ <- ggplot(
    cume_excess_deaths %>%
        filter(race == "all"),
    aes(
        x = fitted,
        xmin = lower,
        xmax = upper,
        y = death_cat_rev,
        color = model_cat,
        group = model_cat
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
    mk_nytimes(legend.position = "bottom",
               axis.text.x = element_text(
                   angle = 45,
                   vjust = 1,
                   hjust = 1
               ))+
    scale_color_brewer("Estimation method",
                       palette = "Set1") +
    facet_grid( ~ educ_cat, scales = "free") + 
    labs(title = "By educational")

p1 <- abs_cume_race + abs_cume_educ + plot_layout(ncol = 1, guides = "auto")

ggsave(
    here("plots", "figS10_supp_sensitivity_methods.pdf"),
    p1,
    width = 10,
    height = 9,
    scale = .9,
    device = cairo_pdf
)

ggsave(
    here("plots", "figS10_supp_sensitivity_methods.jpg"),
    p1,
    width = 10,
    height = 9,
    scale = .9,
    dpi = 600
)

write_csv(
    cume_excess_deaths %>%
        select(race_cat,
               educ_cat,
               death_cat,
               date,
               pop,
               model_cat,
               fitted,
               se,
               lower,
               upper), 
    here("output", "figS10_data_by_method.csv")
)
