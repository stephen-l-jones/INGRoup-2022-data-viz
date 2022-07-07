# ggplot default qualitative colors
png("plot/colors (a).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(scales::hue_pal()(5))
dev.off()

# Color Brewer default qualitative colors
png("plot/colors (b).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(RColorBrewer::brewer.pal(5, "Set1"))
dev.off()

# viridis default qualitative colors
png("plot/colors (c).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(viridis::inferno(5))
dev.off()

# colorspace default qualitative colors
png("plot/colors (d).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(colorspace::qualitative_hcl(5))
dev.off()

# colorsafe default qualitative colors
png("plot/colors (e).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(colorsafe::qualitative_palette(5))
dev.off()

# colorblindr default qualitative colors
png("plot/colors (f).png", width = 3, height = 2, units = "in", res = 300)
colorsafe_swatchplot(colorblindr::palette_OkabeIto[1:5])
dev.off()
