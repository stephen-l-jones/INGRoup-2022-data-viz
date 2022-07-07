theme_network <- function (base_size = 8, base_family = "", ...) 
{
  theme_basic_no_grid(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      axis.line        = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(), 
      axis.ticks       = ggplot2::element_blank(), 
      axis.title       = ggplot2::element_blank()
    ) +
    ggplot2::theme(...)
}

theme_basic <- function (base_size = 10, base_family = "", ...) 
{
  theme_basic_no_grid(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      axis.line          = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(size = base_size/32, color = "grey80"),
      panel.grid.minor.y = ggplot2::element_line(size = base_size/32, color = "grey80")
    ) +
    ggplot2::theme(...)
}

theme_basic_no_grid <- function (base_size = 10, base_family = "", ...) {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      axis.line        = ggplot2::element_line(size = base_size/32, color = "grey40"),
      axis.ticks       = ggplot2::element_line(size = base_size/32, color = "grey40"),
      axis.text        = ggplot2::element_text(size = rel(1), colour = "black"),
      legend.background = ggplot2::element_blank(),
      legend.key       = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(), 
      panel.border     = ggplot2::element_blank(), 
      panel.grid       = ggplot2::element_blank(),
      plot.caption     = ggplot2::element_text(size = rel(.6), color = "grey40"),
      plot.title       = ggplot2::element_text(face = "bold", size = rel(1.5)),
      strip.text       = ggplot2::element_text(face = "bold", size = rel(1)),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::theme(...)
}
