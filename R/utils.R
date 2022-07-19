# Change the showtext DPI setting when saving a plot and then convert DPI back to the original
# setting.
ggsave_showtext <- function (
  filename, plot = last_plot(), device = NULL, path = NULL, 
  scale = 1, width = NA, height = NA, units = c("in", "cm", "mm", "px"), 
  dpi = 300, limitsize = TRUE, bg = NULL, ...
) {
  old_dpi <- showtext:::.pkg.env$.dpi
  showtext::showtext_opts(dpi = dpi)
  tryCatch(
    expr = ggplot2::ggsave(filename, plot, device, path, scale, width, height, units, dpi, 
                           limitsize, bg, ...), 
    finally = showtext::showtext_opts(dpi = old_dpi)
  )
}

# Capitalize the first letter of a string
str_capitalize <- function (x) {
  paste0(str_to_upper(str_extract(x, "^.")), str_remove(x, "^."))
}

# Set label text to blank based on gap.
label_skip <- function(x, gap = 1, offset = 0) {
  x <- as.character(x)
  x[-(seq(1, length(x), gap) + offset)] <- ""
  x
}

# Set label text to blank based on gap.
label_skip_dollar <- function (gap = 1, offset = 0, ...) {
  function(x) {
    lbl <- dollar(x, ...)
    lbl[-(seq(1, length(lbl), gap) + offset)] <- ""
    lbl
  }  
}

# Set label text to blank based on gap.
label_skip_percent <- function (gap = 1, offset = 0, ...) {
  function(x) {
    lbl <- percent(x, ...)
    lbl[-(seq(1, length(lbl), gap) + offset)] <- ""
    lbl
  }  
}

# Set label text to blank based on gap.
label_skip_number <- function (gap = 1, offset = 0, ...) {
  function(x) {
    lbl <- number(x, ...)
    lbl[-(seq(1, length(lbl), gap) + offset)] <- ""
    lbl
  }  
}

# Add the google font if not already installed in sysfonts::font_families().
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

# Swatchplot for color vision deficiency
colorsafe_swatchplot <- function (x) {
  colorspace::swatchplot(x      = x, 
                         border = "transparent",
                         cvd    = c("deutan","protan","desaturate"))
}
