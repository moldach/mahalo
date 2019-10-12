make_agriculture_map <- function(wetlands_geo = wetlands_geo, agriculture_geo = agriculture_geo, relief = relief, subtitle = subtitle) {
        basemap <- ggplot() +
                geom_raster(
                        data = relief,
                        inherit.aes = FALSE,
                        aes(
                                x = x,
                                y = y,
                                alpha = value
                        )
                ) +
                scale_alpha(
                        name = "",
                        range = c(0.4, 0.01),
                        guide = FALSE
                ) +
                geom_sf(
                        data = wetlands_geo,
                        fill = "#012F47",
                        color = "#012F47"
                ) +
                geom_sf(data = agriculture_geo, aes(fill = AGTYPE), color = "white", alpha = 0.3) +
                scale_fill_manual(values = AGTYPE) +
                labs(
                        title = paste0("Agricultural Lands of Importance \n", subtitle),
                        fill = ""
                ) +
                theme_roads()
}

agriculture_colors <- c(
        "Forest" = "#ffffcc",
        "Shrubland" = "#ffeda0",
        "Grassland" = "#fed976",
        "Agriculture" = "#feb24c",
        "Developed" = "#fd8d3c",
        "Not Vegetated" = "#fc4e2a"
        )

agriculture_vec <- c(
        "Forest",
        "Shrubland",
        "Grassland",
        "Agriculture",
        "Developed",
        "Not Vegetated"
                 )
