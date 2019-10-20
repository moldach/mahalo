make_agriculture_map <- function(wetlands_geo = wetlands_geo, agriculture_geo = agriculture_geo, subtitle = subtitle) {
        basemap <- ggplot() +
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
                geom_sf(data = agriculture_geo, aes(fill = Maj_LC), color = "transparent", alpha = 0.7) +
                scale_fill_manual(values = agriculture_colors) +
                labs(
                        title = paste0("Land Cover \n", subtitle),
                        fill = ""
                ) +
                theme_roads()
}

agriculture_colors <- c(
        "Forest" = "#182c25",
        "Shrubland" = "#467030",
        "Grassland" = "#D6C55A",
        "Agriculture" = "#837590",
        "Developed" = "#d1495b",
        "Not Vegetated" = "#2e4057",
        "Other" = "grey69"
        )

agriculture_vec <- c(
        "Forest",
        "Shrubland",
        "Grassland",
        "Agriculture",
        "Developed",
        "Not Vegetated",
        "Other")
