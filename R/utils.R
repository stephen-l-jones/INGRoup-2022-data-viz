
ggsave_showtext <- function (
  filename, plot = last_plot(), device = NULL, path = NULL, 
  scale = 1, width = NA, height = NA, units = c("in", "cm", "mm", "px"), 
  dpi = 300, limitsize = TRUE, bg = NULL, ...
) {
  old_dpi <- showtext:::.pkg.env$.dpi
  showtext::showtext_opts(dpi = dpi)
  ggplot2::ggsave(filename, plot, device, path, scale, width, height, units, dpi, limitsize, 
                  bg, ...)
  showtext::showtext_opts(dpi = old_dpi)
}

str_capitalize <- function (x) {
  paste0(str_to_upper(str_extract(x, "^.")), str_remove(x, "^."))
}

label_skip <- function(x, gap = 1, offset = 0) {
  x <- as.character(x)
  x[-(seq(1, length(x), gap) + offset)] <- ""
  x
}

google_font <- function (family) {
  value <- tryCatch(
    expr = {
      if (!(family %in% sysfonts::font_families())) {
        sysfonts::font_add_google(family)
      }
    }, 
    error = function(e) warning(sprintf("Google font family '%s' not found.", family))
  )
  family
}

colorsafe_swatchplot <- function (x) {
  colorspace::swatchplot(x      = x, 
                         border = "transparent",
                         cvd    = c("deutan","protan","desaturate"))
}
