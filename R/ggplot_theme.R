sysfonts::font_add_google("Roboto")

theme_network <- function (base_size = 8, base_family = "", ...) 
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      axis.text        = ggplot2::element_blank(), 
      axis.ticks       = ggplot2::element_blank(), 
      axis.title       = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      panel.border     = ggplot2::element_blank(), 
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      ...
    )
}

theme_histogram <- function (base_size = 10, base_family = "", ...) 
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      axis.ticks       = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(), 
      panel.border     = ggplot2::element_blank(), 
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      
      ...
    )
}