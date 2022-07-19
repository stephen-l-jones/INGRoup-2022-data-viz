# Load network data and create factors
load("data/friend_network.rda")
friend_network <- friend_network %>%
  mutate(period       = factor(time, 0:2, c("Start","Midpoint","End")),
         same_gender  = factor(ifelse(same_gender == 1, "Yes","No"), c("Yes","No")),
         ego_gender   = factor(ifelse(ego_gender == 1, "Male","Female")),
         alter_gender = factor(ifelse(alter_gender == 1, "Male","Female"))) %>%
  as_tibble()

# Summarize by friendship strength and project period
tie_summary <- friend_network %>%
  group_by(friendship, period) %>%
  summarise(count = n()) %>%
  group_by(period) %>%
  mutate(pct = count / sum(count))

# Histogram 1
p1 <- ggplot(tie_summary) +
  geom_col(aes(x = friendship, y = count), fill = colorsafe_palette("Red", 3, "high")[1]) +
  scale_x_continuous("Friendship strength") +
  scale_y_continuous(NULL, expand = c(0,0), breaks = seq(0, 2000, 400)) +
  labs(title    = "Friendship ties",
       subtitle = "Tie count") +
  theme_basic(base_family = google_font("Roboto"))
showtext_auto()
print(p1)
ggsave_showtext("plot/friendship_histogram_1.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

# Histogram 2
p2 <- p1
p2$layers[[1]] <- NULL

p2 <- p2 +
  geom_col(aes(x = friendship, y = count, fill = period)) +
  scale_fill_manual("Project period", 
                    values = colorsafe_palette("Red", 3, "high", reverse = TRUE)) +
  theme_basic(base_family = google_font("Roboto"),
              legend.position = c(.8,.8), 
              legend.background = element_rect(color = "grey80"))

showtext_auto()
print(p2)
ggsave_showtext("plot/friendship_histogram_2.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

# Histogram 3
p3 <- p2 +
  facet_wrap(~ period) +
  scale_y_continuous(NULL, expand = c(0,0), breaks = seq(0, 2000, 200))  +
  labs(subtitle = "Tie count by project period") +
  theme_basic(base_family = google_font("Roboto"),
              legend.position = "none")

showtext_auto()
print(p3)
ggsave_showtext("plot/friendship_histogram_3.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

# Histogram 4
p4 <- ggplot(tie_summary) +
  geom_col(aes(x = period, y = pct, fill = friendship), color = "grey90", size = .3) +
  scale_x_discrete(NULL) +
  scale_y_reverse(NULL, breaks = seq(0,1,.2), labels = scales::percent(seq(1, 0, -.2)), 
                  limits = c(1,0), expand = c(0.01,0)) +
  scale_fill_colorsafe_continuous("Friendship\nstrength", palette = "Red", reverse = TRUE,
                         luminance_range = c(20, 100),
                         guide = guide_colorbar(reverse = TRUE)) +
  labs(title    = "Friendship ties",
       subtitle = "Percentage of ties") +
  theme_basic(base_family = google_font("Roboto"),
              legend.spacing.y = unit(3, "mm"))

showtext_auto()
print(p4)
ggsave_showtext("plot/friendship_histogram_4.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

# Histogram 5
p5_insert <- ggplot(tie_summary %>% dplyr::filter(friendship >= 1)) +
  geom_col(aes(x = period, y = count), fill = colorsafe_palette("Red", 3, "high")[3]) +
  scale_x_discrete("Project period") +
  scale_y_continuous(NULL, expand = c(0,0)) +
  labs(subtitle = "Ties with strength 1+") +
  theme_basic(base_size = 8,
              plot.background = element_rect(color = "gray80"))
p5 <- p1 + 
  annotation_custom(ggplotGrob(p5_insert), xmin = 2.3, xmax = 4.2, ymin = 770, ymax = 1270)

showtext_auto()
print(p5)
ggsave_showtext("plot/friendship_histogram_5.png", width = 5, height = 5)
showtext_auto(enable = FALSE)

