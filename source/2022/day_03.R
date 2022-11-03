#--- Script details ------------------------------------------------------------
# Creation date: 03 November 2022
# Project:       30daymapchallenge
# Description:   Day 03: polygons
# Author:        Nick Twort

library(tidyverse)
library(sf)
library(leaflet)
library(janitor)

#--- Import data ---------------------------------------------------------------

# Available at: 
# https://data.melbourne.vic.gov.au/api/geospatial/aia8-ryiq?method=export&format=Shapefile
blocks <- read_sf("data/raw/Blocks for Census of Land Use and Employment (CLUE)/geo_export_90ebf13f-8564-4047-a209-9c2e8ced645f.shp")

employment_by_block <- read_csv("https://data.melbourne.vic.gov.au/api/views/wxa3-hxd3/rows.csv?accessType=DOWNLOAD") |> 
    clean_names()

employment_by_block <- employment_by_block |> 
    filter(census_year %in% c(2002, 2020)) |> 
    select(census_year, block_id, total_jobs_in_block) |> 
    mutate(total_jobs_in_block = ifelse(is.na(total_jobs_in_block), 0.0001, total_jobs_in_block)) |> 
    pivot_wider(names_from = census_year, names_prefix = "jobs_", values_from = total_jobs_in_block) |> 
    mutate(jobs_ch = jobs_2020 / jobs_2002 - 1) |> 
    mutate(jobs_ch = ifelse(jobs_2020 == 0 & jobs_2002 == 0, 0, jobs_ch)) |> 
    mutate(block_id = as.character(block_id))

buckets <- quantile(employment_by_block$jobs_ch, na.rm = TRUE, probs = seq.int(0, 1, 0.1))

buckets <- tibble(bucket = buckets) |> 
    mutate(label = factor(
        x = c("-100% - -67%", "-67% - -39%", "-39% - -20%", "-20% - 0%", "0%", "0% - 7%", "7% - 37%", "37% - 86%", "86% - 312%", ">312%", ">312%"),
        levels =c("-100% - -67%", "-67% - -39%", "-39% - -20%", "-20% - 0%", "0%", "0% - 7%", "7% - 37%", "37% - 86%", "86% - 312%", ">312%"),
        ordered = TRUE
    ), dummy = TRUE) |> 
    mutate(ebucket = lead(bucket))

employment_by_block <- employment_by_block |> 
    mutate(dummy = TRUE) |> 
    left_join(buckets, by = "dummy") |>
    filter((jobs_ch >= bucket & jobs_ch < ebucket) | (is.na(jobs_ch) & is.na(jobs_2020) & label == "-100% - -67%") | ((is.infinite(jobs_ch) | (is.na(jobs_ch) & is.na(jobs_2002))) & label == ">312%")) |> 
    filter(!is.infinite(bucket))


g <- blocks |> 
    left_join(employment_by_block, by = "block_id") |>
    mutate(label2 = as.integer(label)) |>
    ggplot(aes(fill = label2)) +
    geom_sf(size = 0.2, colour = "#E6E7E8") +
    scale_fill_steps(low = "white", high = hktools::hk_colours[4], labels = c("Decrease", "", "", "Increase"), n.breaks = 6, guide = guide_coloursteps()) +
    theme_void() +
    labs(title = "Change in # of jobs by city block in Melbourne (2002 - 2020)", caption = "Data: City of Melbourne.\nMap: @nwbort.",  fill = "Change in jobs") +
    theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        plot.background = element_blank(),
        text = element_text(colour = "black"),
        legend.position = "bottom", 
        legend.direction = "horizontal"
    )

# Save it
ggsave(filename = "figures/svg/day_03.svg",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
ggsave(filename = "figures/png/day_03.png",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
