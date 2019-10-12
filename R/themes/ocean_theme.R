theme_ocean <- function(...) {
        theme_minimal() +
                theme(
                        text = element_text(family = "Hanalei", color = "#5e644f"),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.background = element_rect(fill = "#E0F2FE", color = NA), 
                        panel.background = element_rect(fill = "#E0F2FE", color = NA), 
                        legend.background = element_rect(fill = "#E0F2FE", color = NA),
                        panel.border = element_blank(),
                        legend.text = element_text(colour="#5e644f", size=12),
                        legend.position="bottom",
                        plot.title = element_text(family = "Hanalei", color = "#5e644f", size = 30, hjust = 0.97),
                        ...
                )
}