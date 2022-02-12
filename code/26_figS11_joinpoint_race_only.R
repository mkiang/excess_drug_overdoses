## Imports ----
library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

## Data ----
monthly_df <- readRDS(here("data", "analytic_monthly_df.RDS"))

## Retreive joinpoint results ----
jp_results <-
    left_join(import_jp(
        here::here(
            "joinpoint",
            "race_alone",
            "race_imputed_zero_monthly_deaths.data.txt"
        ),
        ctypes = "cccinnncii"
    ),
    monthly_df) %>%
    dplyr::left_join(
        import_jp(
            here::here(
                "joinpoint",
                "race_alone",
                "race_imputed_zero_monthly_deaths.mpc.txt"
            )
        ) %>%
            dplyr::select(
                race,
                educ,
                death_type,
                f_model = model,
                segment,
                month_from_start = segment_start,
                mpc,
                mpc_lower = mpc_95_percent_lcl,
                mpc_upper = mpc_95_percent_ucl,
                mpc_test_stat = test_statistic,
                mpc_pval = p_value
            )
    ) %>%
    mutate(modeled_n_deaths = model / 100000 * pop_monthly) %>%
    categorize_death() %>%
    categorize_race() %>%
    categorize_educ() %>%
    filter(
        !is.na(model),
        death_type %in% c(
            "alcohol",
            "benzos",
            "cocaine",
            "drug_poisoning",
            "heroin",
            "intent_drug_poisoning",
            "meth",
            "natural_opioid",
            "opioids",
            "synthetic_opioid",
            "unintent_drug_poisoning"
        )
    )

## Plots ----
p1 <- ggplot() +
    geom_line(data = jp_results,
              aes(
                  x = date,
                  y = model,
                  color = race_cat,
                  group = race_cat
              )) +
    geom_point(
        data = jp_results %>%
            filter(!is.na(flag)),
        aes(
            x = date,
            y = model,
            color = race_cat,
            group = race_cat,
            alpha = mpc_pval < .05
        ),
        size = 2
    ) +
    mk_nytimes(legend.position = c(1, 0),
               legend.justification = c(1, 0)) +
    scale_color_manual("Race and ethnicity",
                       values = RColorBrewer::brewer.pal(6, "Dark2")[-1]) +
    scale_alpha_manual(
        "Change in slope",
        values = c(.35, 1),
        labels = c(
            "Not statistically significant",
            "Statistical significant at .05"
        )
    ) +
    scale_y_continuous("Smoothed monthly crude mortality rate (per 100,000 person-months)",
                       expand = c(0, 0)) +
    scale_x_date(NULL, expand = c(.01, 0)) +
    geom_vline(xintercept = as.Date(config::get("forecast_start")),
               linetype = "dotted") +
    facet_wrap( ~ death_cat, scales = "free", ncol = 3)

## Save ----
ggsave(
    here("plots", "figS11_joinpoint_test_plot_by_race_only.pdf"),
    p1,
    width = 8,
    height = 10,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS11_joinpoint_test_plot_by_race_only.jpg"),
    p1,
    width = 8,
    height = 10,
    scale = 1,
    dpi = 600
)

write_csv(
    jp_results %>% select(
        race_cat,
        educ_cat,
        death_cat,
        date,
        date_start,
        model,
        standard_error,
        segment,
        mpc,
        mpc_lower,
        mpc_upper,
        mpc_pval,
        modeled_n_deaths
    ),
    here("output", "figS11_data_jp_by_race.csv")
)
