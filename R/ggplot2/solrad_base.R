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
                geom_sf(data = solrad_geo, aes(fill = solar_cal), color = "white", alpha = 0.7) +
                scale_fill_manual(values = solrad_colors) +
                labs(
                        title = paste0("Solar Radiation \n", subtitle),
                        fill = ""
                ) +
                theme_roads()
}

solrad_colors <- c(
        "200-250" = "#FFFCD1",
        "250-300" = "#FEFF7F",
        "300-350" = "#FFFF05",
        "350-400" = "#FFE505",
        "400-450" = "#FFB805",
        "450-500" = "#FE612C",
        "500-550" = "#E04F00",
        "550-600" = "#D20701",
        "600-650" = "#3E0001")

solrad_vec <- c( "200-250",
                   "250-300",
                   "300-350",
                   "350-400",
                   "400-450",
                   "450-500",
                   "500-550",
                   "550-600",
                   "600-650")
