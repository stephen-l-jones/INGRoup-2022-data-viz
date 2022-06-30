load("data/friend_network.rda")
friend_network <- friend_network %>%
  mutate(project      = factor(time, 0:2, c("Start","Midpoint","End")),
         same_gender  = factor(ifelse(same_gender == 1, "Yes","No"), c("Yes","No")),
         ego_gender   = factor(ifelse(ego_gender == 1, "Male","Female")),
         alter_gender = factor(ifelse(alter_gender == 1, "Male","Female")))

# Create team label <factor> ordered by friendship density
midpoint_network <- friend_network %>%
  filter(time == 1) %>%
  group_by(team_id) %>%
  mutate(density = mean(friendship), 
         team_label = paste("Team", team_id)) %>%
  ungroup() %>%
  mutate(team_label = factor(team_label, unique(team_label[order(density)])))

# Get nodes and their attributes and create x-y coordinates
nodes <- midpoint_network %>%
  group_by(team_label, ego_id) %>%
  summarize(ego_gender = first(ego_gender)) %>%
  group_by(team_label) %>%
  mutate(local({
    theta <- seq(-90, 270, length.out = n() + 1)[-1] * pi / 180
    data.frame(x = cos(theta), y = sin(theta))    
  })) 

# Get edge x-y coordinates
edges <- midpoint_network %>% 
  filter(friendship > 0 & ego_id < alter_id) %>%
  select(team_label, ego_id, alter_id, friendship, same_gender) %>%
  inner_join(select(nodes, -ego_gender), by = c("team_label", "ego_id")) %>%
  inner_join(select(nodes, -ego_gender), by = c("team_label", alter_id = "ego_id"), 
             suffix = c("","end"))

# Convert edges to bezier curves
beziers <- edges %>%
  group_by(team_label, ego_id, alter_id) %>%
  summarize(friendship, same_gender, x = c(x, 0, xend), y = c(y, 0, yend))

# Create network plot
showtext_opts(dpi = 96)
ggplot(nodes) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.2), color = NA, fill = "#f2f2f2") +
  geom_bezier(aes(x = x, y = y, group = paste(team_label, ego_id, alter_id), size = friendship),
              data = beziers) +
  geom_point(aes(x = x, y = y), size = 2) +
  facet_wrap(~ team_label) +
  scale_size_continuous("Friendship", breaks = c(1,4), labels = c("Weak","Strong"), 
                        range = c(.2, 1.2)) +
  coord_fixed() +
  theme_network(base_family = "Roboto", legend.position = "bottom", 
                plot.title = element_text(face = "bold", size = 14)) +
  labs(title    = "Team friendship network",
       subtitle = "Project midpoint survey")

showtext_opts(dpi = 300)
ggsave("plot/Team friendship network.png",
       device = "png", width = 6.5, height = 8, dpi = 300)

histogram <- friend_network %>%
  group_by(friendship, project) %>%
  summarise(count = n())

showtext_opts(dpi = 96)
ggplot(histogram) +
  geom_col(aes(x = friendship, y = count)) +
  facet_wrap(~ project) +
  scale_x_continuous("Friendship strength") +
  scale_y_continuous(NULL, expand = c(0,0)) +
  theme_histogram(base_family = "Roboto",
                  plot.title = element_text(face = "bold", size = 14)) +
  labs(title    = "Friendship histogram",
       subtitle = "By project period")
showtext_opts(dpi = 300)
ggsave("plot/Friendship histogram.png",
       device = "png", width = 6.5, height = 4, dpi = 300)


homophily <- friend_network %>%
  group_by(same_gender, project) %>%
  summarise(friendship = sum(friendship > 2))


