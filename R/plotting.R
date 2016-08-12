theme_takiwaR <- function(base_size=12, base_family="") {
    grey <- "#737373"
    black <- "#000000"
    theme_bw(base_size=base_size, base_family=base_family) +
        theme(
            line = element_line(colour = grey),
            rect = element_rect(fill = "white", colour = NA),
            text = element_text(colour = black),
            axis.ticks = element_line(colour = black),
            legend.key = element_rect(colour = NA),
            ## Examples do not use grid lines
            panel.border = element_rect(colour = black),
            panel.grid = element_blank(),
            plot.background=element_blank(),
            strip.background = element_rect(fill="white", colour=NA)
        )
}