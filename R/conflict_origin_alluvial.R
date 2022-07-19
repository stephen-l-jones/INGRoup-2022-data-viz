# Use geom_sankey to create an alluvial diagram. The function's package is available 
# on gihub at "davidsjoberg/ggsankey". Use devtools::install_github("davidsjoberg/ggsankey")
# to install.
load("data/team_conflict.rda")

# Organize the data for the alluvial diagram
origin_order <- c("individual","dyad","subgroup","team","none")
origin_alluvial <- team_conflict %>%
  mutate(Task = origin_task, Relationship = origin_relationship) %>%
  ggsankey::make_long(Task, Relationship) %>%
  mutate(node      = factor(node, origin_order, str_capitalize(origin_order)),
         next_node = factor(next_node, origin_order, str_capitalize(origin_order)))

# The text functionality for geom_sankey_text is currently limited. Set up text positions
# and use geom_text instead.
alluvial_text <- team_conflict %>%
  pivot_longer(matches("origin_"), names_prefix = "origin_") %>%
  count(name, value) %>%
  mutate(x    = factor(str_capitalize(name), c("Task","Relationship")),
         node = factor(value, origin_order, str_capitalize(origin_order))) %>%
  group_by(x) %>%
  arrange(node, .by_group = TRUE) %>%
  mutate(y  = c(0, head(cumsum(n + 2), -1)) + n / 2)

# Alluvial diagram of conflict origins
p <- ggplot(origin_alluvial, aes(x = x, next_x = next_x, node = node, next_node = next_node)) +
  geom_sankey(aes(fill = node), 
              flow.fill = "grey60", flow.alpha = .3, size = .1, smooth = 6,
              type = "alluvial", space = 2) +
  geom_text(aes(x = x, y = y, label = node), hjust = 1, nudge_x = -.06, size = 10/.pt,
            inherit.aes = FALSE, data = alluvial_text[alluvial_text$x == "Task",]) +
  geom_text(aes(x = x, y = y, label = node), hjust = 0, nudge_x = .06, size = 10/.pt,
            inherit.aes = FALSE, data = alluvial_text[alluvial_text$x == "Relationship",]) +
  scale_y_continuous(NULL, expand = c(0.01,0)) +
  scale_fill_manual(values = c(colorsafe::qualitative_palette(4, name = "Set 1.4"), "grey60")) +
  coord_cartesian(xlim = c(1.3,1.7)) +
  labs(title = "Task and relationship conflict origins",
       subtitle = "Seventy manufacturing teams") +
  theme_network(base_size = 10,
                base_family = google_font("Roboto"),
                axis.text.x = element_text(),
                legend.position = "none")

showtext_auto()
print(p)
ggsave_showtext("plot/conflict_origin_alluvial.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

