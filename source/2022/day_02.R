#--- Script details ------------------------------------------------------------
# Creation date: 02 November 2022
# Project:       30daymapchallenge
# Description:   Day 02: lines
# Author:        Nick Twort

library(tidyverse)
library(sf)
library(leaflet)
library(strayr)

n_lines <- 30

sf_use_s2(FALSE)

#--- Import data ---------------------------------------------------------------

aus <- strayr::read_absmap("aus2021")

#--- Process data --------------------------------------------------------------

aus <- aus |> 
    filter(!is.na(cent_lat))

boxy <- aus |> 
    st_bbox()

lines <- tibble(
    xmin = boxy$xmin,
    xmax = boxy$xmax,
    y = seq.int(boxy$ymin + 0.0001, boxy$ymax - 0.0001, length.out = n_lines)
) |> 
    mutate(rn = row_number()) |> 
    pivot_longer(cols = starts_with("x")) |> 
    select(id = rn, x = value, y = y)

lines <- lines |> 
    st_as_sf(coords = c("x", "y"), crs = 4326) |> 
    group_by(id) |> 
    summarise() |> 
    st_cast("LINESTRING")

aus_lines <- aus |> 
    st_intersection(lines)

#--- Map it --------------------------------------------------------------------

g <- aus_lines |> 
    mutate(colour = factor(id %% 2)) |> 
    ggplot() +
    geom_sf(aes(colour = colour)) +
    scale_colour_manual(values = c("#00843d", "#ffcd00"), guide = guide_none()) +
    theme_void() +
    labs(title = "Australia as lines", caption = "Data: ABS.\nMap: @nwbort") +
    theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        plot.background = element_blank(),
        text = element_text(colour = "white")
    )

# Save it
ggsave(filename = "figures/svg/day_02.svg",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
ggsave(filename = "figures/png/day_02.png",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
