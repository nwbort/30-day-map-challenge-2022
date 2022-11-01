#--- Script details ------------------------------------------------------------
# Creation date: 01 November 2022
# Project:       30daymapchallenge
# Description:   Day 01: points
# Author:        Nick Twort

library(tidyverse)
library(magrittr)
library(sf)
library(httr)
library(basemaps)

source(".secrets/here.R")

#--- Helper functions ----------------------------------------------------------

define_waypoints <- function(points) {
    
    startpoint <- paste0("&waypoint0=geo!", points[1], ",", points[2])
    
    endpoint <- paste0("&waypoint",(length(points) / 2) -1,"=geo!", points[length(points)-1], ",", points[length(points)])
    
    if (length(points) <= 4) {
        return(paste0(startpoint, endpoint))
    }
    
    midpoints <- c()
    
    for (i in seq(3,(length(points)-2),2)) {
        midpoints[(i-1)/2] <- paste0("&waypoint",(i-1)/2,"=passThrough!",points[i], ",", points[i+1])
    }
    
    return(paste0(startpoint, paste0(midpoints, collapse = ""), endpoint))
    
}

append_route_details <- function(df, details_v) {
    
    for (i in 1:length(names)) {
        df <- df |> mutate(new_col = details_v[[i]])
        names(df)[names(df) == "new_col"] <- names(details_v)[i]
    }
    
    return(df)
}


HERE_Route_API <- function ( route_points, route_name, city_name, from_NSEW, to_NSEW, 
                             output_sf = TRUE, mode_shortest = FALSE) {
    
    details_v <- list(
        route = route_name, 
        city = city_name, 
        from_location = from_NSEW,
        to_location = to_NSEW
    )
    
    API.base_url <- "https://route.cit.api.here.com"
    API.path <- "/routing/7.2/"
    API.resource <- "calculateroute"
    API.format <- ".json"
    API.applicationCode = HERE_API_config$API.applicationCode
    API.appID  = HERE_API_config$API.appID 
    API.parameters <- "&routeAttributes=shape,boundingBox&legattributes=li&links&mode=fastest;car;traffic:disabled"
    
    if (mode_shortest) {
        API.parameters <- "&routeAttributes=shape,boundingBox&legattributes=li&links&mode=shortest;car;traffic:disabled"
    }
    
    url <- paste0(API.base_url, API.path, API.resource, API.format, 
                  API.appID, API.applicationCode, 
                  define_waypoints(route_points), 
                  API.parameters)
    
    #query api
    raw.result <- GET(url = paste0(url))
    #refine data
    result <- rawToChar(raw.result$content)
    result_json <- jsonlite::fromJSON(result, simplifyMatrix = TRUE)
    
    if (!is.null(result_json$subtype) && result_json$subtype == "NoRouteFound") {
        return(NULL)
    }
    
    
    ## get full details of links on route
    links <- result_json$response$route$leg[[1]]$link[[1]] |> as_tibble()
    colnames(links) <- c("linkid_raw", "shape", "speed_limit", "type")
    links$linkid_raw <- strtoi(links$linkid_raw)
    links <- links |> 
        mutate("linkid" = ifelse(linkid_raw > 0, paste0(abs(linkid_raw), "F"), paste0(abs(linkid_raw), "T"))) #add route info
    
    links$order <- 1:nrow(links)
    links <- append_route_details(df = links, details_v)
    links<- unique(links) |> as_tibble()
    
    ## get attributes of start and end points
    start_point <- result_json$response$route$leg[[1]]$start |>
        mutate(mapped_latitude = mappedPosition$latitude,
               mapped_longitude = mappedPosition$longitude,
               original_latitude = originalPosition$latitude,
               original_longitude = originalPosition$longitude) |>
        select(linkId, mapped_latitude, mapped_longitude, original_latitude, original_longitude,
               type, spot, sideOfStreet, mappedRoadName, label, shapeIndex, source)
    
    end_point <- result_json$response$route$leg[[1]]$end |>
        mutate(mapped_latitude = mappedPosition$latitude,
               mapped_longitude = mappedPosition$longitude,
               original_latitude = originalPosition$latitude,
               original_longitude = originalPosition$longitude) |>
        select(linkId, mapped_latitude, mapped_longitude, original_latitude, original_longitude,
               type, spot, sideOfStreet, mappedRoadName, label, shapeIndex, source)
    
    route_nodes <- bind_rows(start_point, end_point)
    
    route_nodes <- route_nodes |> append_route_details(details_v)
    
    ## get maneuver dataframe
    maneuver <- result_json$response$route$leg[[1]]$maneuver[[1]] |> as_tibble()
    colnames(maneuver) <- c("position", "instruction", "travel_time", "length", "id", "type")
    maneuver <- maneuver |> 
        mutate(total_length = result_json$response$route$leg[[1]]$length,
               total_travel_time = result_json$response$route$leg[[1]]$travelTime,
               total_traffic_time = result_json$response$route$summary$trafficTime,
               total_base_time = result_json$response$route$summary$baseTime)
    
    maneuver <- maneuver |> append_route_details(details_v)
    
    if (output_sf == TRUE) {
        links_sfc <- lapply(
            links$shape,
            function(shape) strsplit(shape, ",") |> unlist |> 
                as.numeric() |> rev() |> matrix(ncol = 2, byrow = TRUE) |> apply(2, rev) |> st_linestring()
        ) |> st_sfc()
        
        links$wkb_geometry <- links_sfc
        
    }
    
    return(list(links = links,
                route_nodes = route_nodes,
                maneuver = maneuver))
    
    
}


route <- HERE_Route_API(
    route_points = c(-33.86805450311244, 151.25179268771814, -33.867868725164705, 151.2257851319822),
    route_name = "Point-to-point",
    city_name = "Sydney",
    from_NSEW = "A",
    to_NSEW = "B"
)

links <- route$links |> st_as_sf(crs = 4326)

ext <- links |>
    st_combine() |>
    st_convex_hull() |>
    st_transform(3577) |>
    st_buffer(500) |>
    st_transform(4326)

basemap_ext <- basemap_gglayer(
    ext = ext,
    map_service = "carto",
    map_type = "dark",
    map_res = 1,
    verbose = FALSE
)

g <- ggplot(st_transform(links, 3857)) + 
    basemap_ext +
    geom_sf(colour = "gold", size = 0.5, linetype = "15") +
    scale_fill_identity() + 
    coord_sf() +
    labs(title = "Points from point to point", caption = "Data: HERE, CARTO.\nMap: @nwbort.") +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, colour = "white"),
        plot.caption = element_text(colour = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")
    )

ggsave(filename = "figures/svg/day_01.svg",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
ggsave(filename = "figures/png/day_01.png",
       plot = g,
       width = 1600,
       height = 900,
       units = "px",
       bg = "transparent"
)
