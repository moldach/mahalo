make_island_map <- function(coast_geo = coast_geo, wetlands_geo = wetlands_geo, relief = relief, subtitle = subtitle) {
  basemap <- ggplot() +
    scale_alpha(
      name = "",
      range = c(0.4, 0.01),
      guide = FALSE
    ) + # suppress legend
    # draw lakes in light blue
    geom_sf(
      data = coast_geo,
      fill = "#228b22",
      color = "transparent",
      alpha = 0.2
    ) +
    geom_sf(
      data = wetlands_geo,
      fill = "#012F47",
      color = "#012F47"
    ) +
    labs(
      title = paste0("Island Map \n", subtitle),
      fill = "Depth (meters)"
    ) +
    theme_roads()
}
