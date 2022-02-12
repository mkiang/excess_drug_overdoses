## Imports ----
library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))

## Constants ----
FORECAST_START <- as.Date(config::get("forecast_start"))
LAST_WEEK <- as.Date(config::get("last_week"))
FREQUENCY <- 52
TRAIN_SPLIT <-
    70 / 100 ## Model selection is based on last 30% of data

## What is the length of the prediction window? Use the same length of the
## prediction window for our forecasting errors.
PREDICTION_WINDOW <-
    as.integer(difftime(LAST_WEEK, FORECAST_START, units = "weeks") * 9/12)

## Data ----
analytic_df <- readRDS(here("data", "analytic_weekly_df.RDS")) %>%
    filter(date <= LAST_WEEK)

## Training data ----
## Make sure *all* of the holdout (prediction) data are removed just to be safe.
training_df <- filter(analytic_df, date < FORECAST_START)

## Get a vector of test dates we will for out of sample forecasting errors
all_dates <- sort(unique(training_df$date))
end_training <- seq.Date(all_dates[round(NROW(all_dates) * TRAIN_SPLIT)],
                         max(all_dates) - PREDICTION_WINDOW * 7,
                         by = "1 week")

training_df <- tibble(
    start = as.Date("2016-01-01"),
    end_training = end_training,
    test_start = end_training + 7,
    test_end = end_training + PREDICTION_WINDOW * 7,
    holdout_start = FORECAST_START,
    holdout_end = LAST_WEEK
) %>%
    mutate(training_set = 1:n())

p1 <- ggplot() +
    ## Training window
    geom_segment(
        data = training_df,
        aes(
            x = start,
            xend = end_training,
            y = training_set,
            yend = training_set
        ),
        size = 1,
        color = RColorBrewer::brewer.pal(9, "Set1")[1]
    ) +
    ## Testing set
    geom_segment(
        data = training_df,
        aes(
            x = test_start,
            xend = test_end,
            y = training_set,
            yend = training_set
        ),
        color = RColorBrewer::brewer.pal(9, "Set1")[2],
        size = 1
    ) +
    ## Unused
    geom_segment(
        data = training_df,
        aes(
            x = test_end,
            xend = holdout_start,
            y = training_set,
            yend = training_set
        ),
        color = "grey70",
        alpha = .7,
        size = 1
    ) +
    ## Forecasting set
    geom_segment(
        data = training_df,
        aes(
            x = holdout_start,
            xend = holdout_end,
            y = training_set,
            yend = training_set
        ),
        color = "black",
        size = 1
    ) +
    scale_y_reverse("Evaluation set",
                    expand = c(0, 1)) +
    scale_x_date("Date of data", expand = c(0, 0)) +
    mk_nytimes(panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_blank())

## Save ----
ggsave(
    here("plots", "figS02_crossvalidation_schematic.pdf"),
    p1,
    width = 8,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS02_crossvalidation_schematic.jpg"),
    p1,
    width = 8,
    height = 5,
    scale = 1,
    dpi = 600
)

## Data ----
write_csv(
    training_df,
    here("output", "figS02_data_cv.csv")
)
