# generate figure of program outline
suppressPackageStartupMessages(library(tidyverse))

program_outline <- function(component = "all") {

    pr <- tibble(
        component = c(
            "Protobuf feed",
            "Vehicle state",
            "Network state",
            "Arrival time estimation"
        )
    ) %>%
        mutate(
            order = seq_along(component),
            x = c(1, 2, 2, 3),
            y = c(0.5, 1, 0, 0.5)
        )

    edges <- tibble(
        x = c(1.26, 2.26, 3, 2),
        xend = c(2, 3, 3, )
    )

    steps <- tibble(
        step = 1:5,
        x = c(1.5, 2, 2.5, 3, 3.5)
    )

    ggplot(pr, aes(x, y = y)) +
        geom_tile(
            height = 0.25, width = 0.5,
            fill = "white", colour = "black", lwd = 1
        ) +
        geom_text(
            aes(label = component)
        ) +
        geom_segment(
            aes(
                x = x + 0.26,
                xend = xend - 0.26,
                yend = yend
            ),
            data = edges,
            arrow = arrow(
                length = unit(0.1, "inches"),
                type = "closed"
            ),
            lwd = 1
        ) +
        geom_label(
            aes(x = x, y = 0.6, label = x),
            data = steps
        ) +
        ylim(0, 1)

}
