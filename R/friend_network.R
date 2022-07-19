load("data/friend_network.rda")

# Calculate individual centrality and team density, order by density, and create team label factor
midpoint_network <- friend_network %>%
  dplyr::filter(time == 1) %>%
  group_by(team_id, ego_id) %>%
  mutate(ego_centrality = mean(friendship)) %>%
  group_by(team_id) %>%
  mutate(density    = mean(friendship), 
         team_label = paste("Team", team_id)) %>%
  ungroup() %>%
  arrange(desc(density)) %>%
  mutate(team_label = factor(team_label, unique(team_label)))

# Get nodes and their attributes and create x-y coordinates ordered by centrality
nodes <- midpoint_network %>%
  group_by(team_label, ego_id) %>%
  summarize(gender     = first(ego_gender),
            centrality = first(ego_centrality)) %>%
  group_by(team_label) %>%
  mutate(local({
    theta <- seq(90, -270, length.out = n() + 1)[-1] * pi / 180
    data.frame(x = cos(theta), y = sin(theta))[rank(centrality, ties.method = "first"),]    
  })) 

# Get edge x-y coordinates
edges <- midpoint_network %>% 
  dplyr::filter(friendship > 0 & ego_id < alter_id) %>%
  select(team_label, ego_id, alter_id, friendship, same_gender) %>%
  inner_join(select(nodes, -gender, -centrality), by = c("team_label", "ego_id")) %>%
  inner_join(select(nodes, -gender, -centrality), by = c("team_label", alter_id = "ego_id"), 
             suffix = c("","end"))

# Convert edges to bezier curves
beziers <- edges %>%
  group_by(team_label, ego_id, alter_id) %>%
  summarize(friendship, same_gender, x = c(x, 0, xend), y = c(y, 0, yend))

# Create network plot 1
p1 <- ggplot(nodes) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), color = NA, fill = "grey95") +
  geom_bezier(aes(x = x, y = y, group = paste(team_label, ego_id, alter_id), size = friendship),
              data = beziers) +
  geom_point(aes(x = x, y = y), size = 1.8) +
  scale_size("Friendship strength", range = c(.4, 1.5)) +
  facet_wrap(~ team_label, nrow = 4) +
  coord_fixed() +
  labs(title    = "Team friendship network",
       subtitle = "Project midpoint") +
  theme_network(base_family = google_font("Roboto"),
                legend.position = c(1,0),
                legend.justification = c(1,0),
                legend.key.height = unit(4, "mm"))

showtext_auto()
print(p1)
ggsave_showtext("plot/friend_network_1.png", width = 10, height = 5)
showtext_auto(enable = FALSE)

# Recreate without ordering by density and centrality
nodes <- midpoint_network %>%
  arrange(team_id) %>%
  mutate(team_label = factor(team_label, unique(team_label))) %>%
  group_by(team_label, ego_id) %>%
  summarize(gender     = first(ego_gender),
            centrality = first(ego_centrality)) %>%
  group_by(team_label) %>%
  mutate(local({
    theta <- seq(90, -270, length.out = n() + 1)[-1] * pi / 180
    data.frame(x = cos(theta), y = sin(theta))    
  })) 
edges <- midpoint_network %>% 
  dplyr::filter(friendship > 0 & ego_id < alter_id) %>%
  select(team_label, ego_id, alter_id, friendship, same_gender) %>%
  inner_join(select(nodes, -gender, -centrality), by = c("team_label", "ego_id")) %>%
  inner_join(select(nodes, -gender, -centrality), by = c("team_label", alter_id = "ego_id"), 
             suffix = c("","end"))
beziers <- edges %>%
  group_by(team_label, ego_id, alter_id) %>%
  summarize(friendship, same_gender, x = c(x, 0, xend), y = c(y, 0, yend))

# Create network plot 2
p2 <- ggplot(nodes) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), color = NA, fill = "grey95") +
  geom_bezier(aes(x = x, y = y, group = paste(team_label, ego_id, alter_id), size = friendship),
              data = beziers) +
  geom_point(aes(x = x, y = y), size = 1.8) +
  scale_size("Friendship strength", range = c(.4, 1.5)) +
  facet_wrap(~ team_label, nrow = 4) +
  coord_fixed() +
  labs(title    = "Team friendship network",
       subtitle = "Project midpoint") +
  theme_network(base_family = google_font("Roboto"),
                legend.position = c(1,0),
                legend.justification = c(1,0),
                legend.key.height = unit(4, "mm"))

showtext_auto()
print(p2)
ggsave_showtext("plot/friend_network_2.png", width = 10, height = 5)
showtext_auto(enable = FALSE)
