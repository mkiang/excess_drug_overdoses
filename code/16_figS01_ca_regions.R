## Imports ----
library(tidyverse)
library(here)
library(RColorBrewer)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

region_df <- ca_regions_fips %>%
    mutate(fips = sprintf("06%03d", county_fips)) %>%
    categorize_region()

## Define the color palette
c_pal <- brewer.pal(name = "Spectral", n = 10)[c(1, 5, 9, 2, 6, 10, 3, 7, 4, 8)]

## Plot
p1 <- usmap::plot_usmap(
    regions = "counties",
    include = "CA",
    data = region_df,
    values = "region_cat_num",
    color = "white"
) +
    scale_fill_manual("California Census Region",
                      values = c_pal) +
    theme(legend.position = "right",
          legend.title.align = .5,
          legend.justification = .5)

## Save ----
ggsave(
    here("plots", "figS01_ca_census_regions.pdf"),
    p1,
    width = 7,
    height = 7,
    scale = .75,
    device = cairo_pdf
)
ggsave(
    here("plots", "figS01_ca_census_regions.jpg"),
    p1,
    width = 7,
    height = 7,
    scale = .75,
    dpi = 600,
    device = grDevices::jpeg
)

## Data ----
write_csv(
    region_df %>% 
        select(region, region_name, county_name, county_fips = fips),
    here("output", "figS01_data_ca_regions.csv")
)
