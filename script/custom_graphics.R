
## produce only integer breaks
int_breaks <- function(x, n = 5) {
  pretty(x, n)[pretty(x, n) %% 1 == 0]
}


## make text bigger on ggplot2 plots
large_txt <- ggplot2::theme(text = ggplot2::element_text(size = 16),
                            axis.text = ggplot2::element_text(size = 14))
smaller_axis_txt <- ggplot2::theme(axis.text = ggplot2::element_text(size = 12))

scale_months <- ggplot2::scale_x_date(breaks = "1 month",
                                      date_label = format("%d %b %Y"))


scale_weeks <- ggplot2::scale_x_date(breaks = "1 week",
                                     date_label = format("%d %b %Y"))



## strips for vertical facetting: horizontal labels, nice colors
custom_vert_facet <- theme(
    strip.text.y = element_text(size = 12, angle = 0, color = "#6b6b47"),
    strip.background = element_rect(fill = "#ebebe0", color = "#6b6b47"))

custom_horiz_facet <- theme(
    strip.text.x = element_text(size = 12, angle = 90, color = "#6b6b47"),
    strip.background = element_rect(fill = "#ebebe0", color = "#6b6b47"))

## rotate x annotations by 45 degrees
rotate_x <- theme(axis.text.x = element_text(angle = 45, hjust = 1L))
