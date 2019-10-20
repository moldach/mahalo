make_moisture_map <- function(wetlands_geo = wetlands_geo, moisture_geo = moisture_geo, subtitle = subtitle) {
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
    geom_sf(data = moisture_geo, aes(fill = MoistureZo), color = "white", alpha = 0.3) +
    scale_fill_manual(values = moisture_colors) +
    labs(
      title = paste0("Moisture Zones \n", subtitle),
      fill = ""
    ) +
    theme_roads()
}

moisture_colors <- c(
  "Arid" = "#ffffcc",
  "Very Dry" = "#d9f0a3",
  "Moderately Dry" = "#addd8e",
  "Seasonal Mesic" = "#78c679",
  "Moist Mesic" = "#41ab5d",
  "Moderately Wet" = "#238443",
  "Very Wet" = "#005a32"
)

moisture_vec <- c("Arid", 
                  "Very Dry",
                  "Moderately Dry",
                  "Seasonal Mesic",
                  "Moist Mesic",
                  "Moderately Wet",
                  "Very Wet")
