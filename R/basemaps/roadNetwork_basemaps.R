library(tidyverse)
library(sf)
library(tigris)
library(glue)
library(colorspace)
library(jkmisc)
library(ggforce)
library(ragg)
library(here)
library(showtext)

# Add custom fonts
font_add_google("Hanalei", "Hanalei")
font_add_google("Hanalei Fill", "Hanalei Fill")
# automatically use showtext to render text
showtext_auto()
source(here::here("R/themes", "road_theme.R"))

# Get all Hawaii County road maps
counties <- c("Honolulu", "Hawaii", "Kalawao",  "Kauai", "Maui")

roads_data <- map(counties, ~roads("HI", .x, class = "sf")) %>% 
        do.call(sf:::rbind.sf, .)

# load latitude/longitude coordinates for cities
cities_geolocation <- read_csv(here::here("data/cities_geolocation.csv"))

# Build plot colors as a named vector and as a tibble
plotcolors <- c('Other' = '#cccccc',
                'Ave' = '#59c8e5',
                'St' = '#fed032',
                'Tunl' = '#fed032',
                'Brg' = '#fed032',
                'N' = '#fed032',
                'S' = '#fed032',
                'E' = '#fed032',
                'W' = '#fed032',
                'Rd' = '#4cb580',
                'Dr' = '#0a7abf', 
                'Hwy' = '#ff9223', 
                'Plz' = '#ff9223',
                'Viaduct' = '#ff9223', 
                'Expy' = '#ff9223', 
                'Pkwy' = '#ff9223',
                'Thruway' = '#ff9223',
                'State Hwy' = '#ff9223',
                'State' = '#ff9223',
                'US Hwy' = '#ff9223',
                'Blvd'= '#2e968c')

pc_tibble <- tibble(street_type = names(plotcolors),
                    color = plotcolors)

# Assign street types to roads
roads_sf <- roads_data %>% 
        filter(!is.na(RTTYP)) %>% 
        mutate(street_type = map_chr(FULLNAME, ~first(names(plotcolors)[str_which(.x, glue("{names(plotcolors)}\\b"))]))) %>% 
        mutate(street_type = if_else(str_detect(FULLNAME, "I-"), 'I-', street_type)) %>% 
        mutate(street_type = case_when(is.na(street_type) & MTFCC == "S1100" ~ 'Expy',
                                       is.na(street_type) & MTFCC == "S1200" ~ 'St',
                                       is.na(street_type) & !MTFCC %in% c("S1100", "S1200") ~ "Other",
                                       TRUE ~ street_type)) %>% 
        left_join(pc_tibble, by = "street_type")

# Construct the color legend
legend <- pc_tibble %>% 
        filter(street_type %in% c("Other","Ave","St", "Rd", "Dr", "Hwy", "Blvd")) %>% 
        mutate(street_type = factor(street_type, levels = c("Other", "Ave", "Dr", "Rd", "Blvd", "St", "Hwy"), labels = c("Other", "Avenue", "Drive", "Road", "Boulevard ", "Street", "Highway"))) %>%
        arrange(street_type) %>% 
        mutate(x0 = seq(3, by = 4.5, length.out = 7),
               r = 1.75,
               y0 = 0) %>% 
        ggplot(aes(x0 = x0, y0 = y0, r = r)) +
        geom_circle(aes(fill = color, color = darken(color))) +
        geom_text(aes(label = street_type, x = x0, y = 0), family = "Hanalei Fill", colour="#A77B56", size = 10) +
        annotate("text", family = "Hanalei Fill", colour="#A77B56", x = -2, y = 0, label = "Legend", size = 15) +
        scale_fill_identity() +
        scale_color_identity() +
        expand_limits(y = c(-0.5, 4),
                      x = c(-4, 24)) +
        labs(x = NULL,
             y = NULL) +
        coord_equal(clip = "off") +
        theme_jk(grid = FALSE, plot_title_size = 30) +
        theme(panel.grid.major = element_line(colour = "transparent"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) 

legend_grob <- ggplotGrob(legend)


# nice looking pngs
for(j in 1:nrow(cities_geolocation)){
        city <- cities_geolocation[j,]$city

pt <- data.frame(lat = cities_geolocation$lat[j], long = cities_geolocation$lon[j])
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
circle <- st_buffer(pt, dist = 24140.2)
circle <- circle %>% st_transform(st_crs(roads_sf))
roads <- st_intersection(circle, roads_sf)

# get coordinates of outer frame
roads_pt <- roads %>% st_cast("POINT")
roads_coords <- as_tibble(st_coordinates(roads_pt)) %>% setNames(c("lon", "lat"))

xmin <- min(roads_coords$lon); xmax <- max(roads_coords$lon)
ymin <- min(roads_coords$lat); ymax <- max(roads_coords$lat)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax

road_map <- ggplot() +
        geom_sf(data = filter(roads, street_type != "Other"), aes(color = color), size = 0.25) + 
        geom_sf(data = filter(roads, street_type == "Other"), aes(color = color), size = 0.35) + 
        annotation_custom(legend_grob, xmin = xmin, xmax = xmax, ymin = ymin-yoffset+0.02, ymax = ymin+yoffset) +
        scale_color_identity() +
        scale_size_identity() +
        coord_sf(clip = "off") +
        labs(title = city, x = NULL, y = NULL) +
        theme_roads() +
        theme(plot.title = element_text(size = 150))

ggsave(paste0(here::here("figures/png"), "/", city, "_road_network.png"), width = 210, height = 297, units = "mm", dpi = 350, dev = agg_png())
### if you want the highest quality for print use dpi = 1500 but you need to modify the fonts
}


# Construct the color legend
legend <- pc_tibble %>% 
        filter(street_type %in% c("Other","Ave","St", "Rd", "Dr", "Hwy", "Blvd")) %>% 
        mutate(street_type = factor(street_type, levels = c("Other", "Ave", "Dr", "Rd", "Blvd", "St", "Hwy"), labels = c("Other", "Avenue", "Drive", "Road", "Boulevard ", "Street", "Highway"))) %>%
        arrange(street_type) %>% 
        mutate(x0 = seq(3, by = 4.5, length.out = 7),
               r = 1.75,
               y0 = 0) %>% 
        ggplot(aes(x0 = x0, y0 = y0, r = r)) +
        geom_circle(aes(fill = color, color = darken(color))) +
        geom_text(aes(label = street_type, x = x0, y = 0), family = "Hanalei Fill", colour="#A77B56", size = 3) +
        annotate("text", family = "Hanalei Fill", colour="#A77B56", x = -2, y = 0, label = "Legend", size = 6) +
        scale_fill_identity() +
        scale_color_identity() +
        expand_limits(y = c(-0.5, 4),
                      x = c(-4, 24)) +
        labs(x = NULL,
             y = NULL) +
        coord_equal(clip = "off") +
        theme_jk(grid = FALSE, plot_title_size = 30) +
        theme(panel.grid.major = element_line(colour = "transparent"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) 

legend_grob <- ggplotGrob(legend)

# this is the overlapping legend not sure how to fix it...yet
for(j in 1:nrow(cities_geolocation)){
        city <- cities_geolocation[j,]$city
        
        pt <- data.frame(lat = cities_geolocation$lat[j], long = cities_geolocation$lon[j])
        pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
        circle <- st_buffer(pt, dist = 24140.2)
        circle <- circle %>% st_transform(st_crs(roads_sf))
        roads <- st_intersection(circle, roads_sf)
        
        # get coordinates of outer frame
        roads_pt <- roads %>% st_cast("POINT")
        roads_coords <- as_tibble(st_coordinates(roads_pt)) %>% setNames(c("lon", "lat"))
        
        xmin <- min(roads_coords$lon); xmax <- max(roads_coords$lon)
        ymin <- min(roads_coords$lat); ymax <- max(roads_coords$lat)
        yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
        
        road_map <- ggplot() +
                geom_sf(data = filter(roads, street_type != "Other"), aes(color = color), size = 0.25) + 
                geom_sf(data = filter(roads, street_type == "Other"), aes(color = color), size = 0.35) + 
                annotation_custom(legend_grob, xmin = xmin, xmax = xmax, ymin = ymin-yoffset+0.02, ymax = ymin+yoffset) +
                scale_color_identity() +
                scale_size_identity() +
                coord_sf(clip = "off") +
                labs(title = city, x = NULL, y = NULL) +
                theme_roads()
        
        saveRDS(road_map, paste0(here::here("figures/rds"), "/", city, "_road_network_grob_basemap.rds"))
}


# this is with the proper legend... and it doesn't work either...giving up for now
for(j in 1:nrow(cities_geolocation)){
        city <- cities_geolocation[j,]$city
        
        pt <- data.frame(lat = cities_geolocation$lat[j], long = cities_geolocation$lon[j])
        pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
        circle <- st_buffer(pt, dist = 24140.2)
        circle <- circle %>% st_transform(st_crs(roads_sf))
        roads <- st_intersection(circle, roads_sf)
        
        # Rename
        roads$street_type <- stringr::str_replace(roads$street_type, pattern = "Rd", replacement = "Road")
        roads$street_type <- stringr::str_replace(roads$street_type, pattern = "St", replacement = "Street")
        roads$street_type <- stringr::str_replace(roads$street_type, pattern = "Dr", replacement = "Drive")
        roads$street_type <- stringr::str_replace(roads$street_type, pattern = "Blvd", replacement = "Boulevard")
        roads$street_type <- stringr::str_replace(roads$street_type, pattern = "Hwy", replacement = "Highway")
        
        # get coordinates of outer frame
        roads_pt <- roads %>% st_cast("POINT")
        roads_coords <- as_tibble(st_coordinates(roads_pt)) %>% setNames(c("lon", "lat"))
        
        xmin <- min(roads_coords$lon); xmax <- max(roads_coords$lon)
        ymin <- min(roads_coords$lat); ymax <- max(roads_coords$lat)
        yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
        
        road_map <- ggplot() +
                geom_sf(data = filter(roads, street_type != "Other"), aes(color = color), size = 0.25, key_glyph = draw_key_dotplot) + 
                geom_sf(data = filter(roads, street_type == "Other"), aes(color = color), size = 0.35, key_glyph = draw_key_dotplot) + 
                scale_color_identity(guide = "legend") +
                scale_size_identity() +
                coord_sf(clip = "off") +
                labs(title = city, x = NULL, y = NULL, color = "") +
                guides(color = guide_legend(override.aes = list(size = 12))) +
                theme_roads()
        
        saveRDS(road_map, paste0(here::here("figures/rds"), "/", city, "_road_network_basemap.rds"))
}