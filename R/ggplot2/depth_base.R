make_depth_map <- function(ocean_geo = ocean_geo, coast_geo = coast_geo, relief = relief, subtitle = subtitle) {
  state_base <- ggplot(data = ocean_geo) +
    # first: draw the relief
    geom_raster(
      data = relief,
      inherit.aes = FALSE,
      aes(
        x = x,
        y = y,
        alpha = value
      )
    ) +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(
      name = "",
      range = c(0.4, 0.01),
      guide = FALSE
    ) + # suppress legend
    geom_sf(
      mapping = aes(fill = factor(depth)),
      color = "white",
      size = 0.1,
      alpha = 0.5,
      key_glyph = draw_key_dotplot
    ) +
    # draw lakes in light blue
    geom_sf(
      data = coast_geo,
      fill = "#228b22",
      color = "transparent",
      alpha = 0.2
    ) +
    scale_fill_manual(
      values = c(
        "#96F3FA",
        "#4AE2EF",
        "#00B7D6",
        "#007DAB",
        "#005A87",
        "#003B61"
      ),
      guide = "legend"
    ) +
    labs(title = "Ocean Depth Chart",
         subtitle = subtitle,
         fill = "Depth (meters)") +
    labs(color = "") +
    guides(fill = guide_legend(override.aes = list(size = 12))) +
    theme_depth()
}
