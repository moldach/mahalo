make_solrad_map <- function(wetlands_geo = wetlands_geo, solrad_geo = solrad_geo, relief = relief, subtitle = subtitle) {
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
                geom_sf(data = solrad_geo, aes(fill = solar_cal), color = "white", alpha = 0.3) +
                scale_fill_manual(values = solrad_colors) +
                labs(
                        title = paste0("Solar Radiation \n", subtitle),
                        fill = ""
                ) +
                theme_roads()
}

solrad_colors <- c(
        "200-250" = "#ffffcc",
        "250-300" = "#ffeda0",
        "300-350" = "#fed976",
        "350-400" = "#feb24c",
        "400-450" = "#fd8d3c",
        "450-500" = "#fc4e2a",
        "500-550" = "#e31a1c",
        "550-600" = "#bd0026",
        "600-650" = "#800026")

solrad_vec <- c( "200-250",
                   "250-300",
                   "300-350",
                   "350-400",
                   "400-450",
                   "450-500",
                   "500-550",
                   "550-600",
                   "600-650")
