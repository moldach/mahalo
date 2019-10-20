make_depth_map <- function(ocean_geo = ocean_geo, coast_geo = coast_geo, subtitle = subtitle) {
  state_base <- ggplot() +
    scale_alpha(
      name = "",
      range = c(0.4, 0.01),
      guide = FALSE
    ) +
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
    labs(title = subtitle,
         fill = "Depth (meters)") +
    labs(color = "") +
    guides(fill = guide_legend(override.aes = list(size = 12))) +
    theme_depth()
}
