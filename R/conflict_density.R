# Load team conflict data
load("data/team_conflict.rda")
team_conflict <- team_conflict %>% as_tibble()

# Convert to long format
conflict_density <- team_conflict %>% 
  pivot_longer(cols          = matches("_(task|relationship)"),
               names_to      = c(".value","conflict_type"),
               names_pattern = "(.*)_(.*)") %>%
  dplyr::filter(origin != "none") %>%
  mutate(origin = factor(str_capitalize(origin), c("Individual","Dyad","Subgroup","Team")))

# Conflict density (a)
p <- ggplot(conflict_density) +
  geom_density(aes(x = conflict, y = after_stat(count)), 
               color = "grey75", fill = "grey95", size = .3,
               data = conflict_density %>% select(-origin)) +
  geom_density(aes(x = conflict, y = after_stat(count), fill = origin), color = NA) +
  scale_x_continuous("Conflict level", breaks = 1:7, limits = c(1, 7), expand = c(0,0)) +
  scale_y_continuous(NULL, expand = c(0,0)) +
  scale_fill_colorsafe_discrete(palette = "Set 2.5", chroma = "high") +
  facet_wrap(~ origin) +
  coord_cartesian(ylim = c(0, 35)) +
  labs(title = "Conflict density by origin",
       subtitle = "Total density in gray") +
  theme_basic_no_grid(base_family = google_font("Roboto"),
                      legend.position = "none",
                      axis.line.y  = element_blank(),
                      axis.text.y  = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.spacing = unit(5, "mm"))
showtext::showtext_auto()
print(p)
ggsave_showtext("plot/Conflict density (a).png", device = "png", width = 5, height = 5)
showtext::showtext_auto(enable = FALSE)

# Conflict density (b)
p <- ggplot(conflict_density) +
  geom_sina(aes(x = origin, y = conflict, shape = origin, size = involved, color = team_size)) +
  scale_x_discrete(NULL) +
  scale_y_continuous("Conflict level", breaks = 1:7) +
  scale_shape("Origin") +
  scale_color_colorsafe_continuous("Team size", palette = "Red", reverse = TRUE) +
  scale_size_area("Members\ninvolved") +
  coord_cartesian(ylim = c(1,7)) +
  labs(title = "Conflict density by origin",
       subtitle = " ") +
  theme_basic(legend.margin = margin(0,0,0,0))
print(p)
ggsave_showtext("plot/Conflict density (b).png", device = "png", width = 5, height = 5)
  

  