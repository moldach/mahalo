theme_depth_lrg <- function(...) {
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
                        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                        legend.background = element_rect(fill = "#f5f5f2", color = NA),
                        panel.border = element_blank(),
                        legend.text = element_text(family = "Hanalei Fill", colour="#A77B56", size=75),
                        legend.title = element_text(family = "Hanalei Fill", colour="#A77B56", size=115),
                        legend.position="bottom",
                        plot.title = element_text(family = "Hanalei", color = "#5e644f", size = 300, hjust = 0.48, vjust = -2),
                        ...
                )
}