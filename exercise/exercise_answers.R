
# Exercise 1. Effect of salary on regular-season wins
p <- ggplot(wins_prediction, aes(x = salary_team, group = season_3)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray60", alpha = .3) +
  geom_line(aes(y = predicted, linetype = season_3, color = season_3)) +
  scale_x_continuous(breaks = seq(0, 300, 50), label = label_dollar(), expand = c(.02,0)) +
  scale_y_continuous(label = label_percent()) +
  scale_linetype_manual(values = c("dashed","solid")) +
  scale_color_manual(values = c("black","gray40")) +
  labs(title = "Effect of salary on regular-season wins",
       x = "Team salary (inflation-adjusted, in millions)", 
       y = "Percent regular-season wins",
       linetype = "Season",
       color = "Season") +
  theme_basic_no_grid(legend.position = c(.05,.95),
                      legend.justification = c(0,1),
                      legend.background = element_rect(fill = "gray90", color = "gray40", size = .25))
print(p)
ggsave("exercise/exercise_1.png", width = 5, height = 5)

# Exercise 2. Salary spend and winning over time for select teams
p <- ggplot(wins_path, aes(x = salary_team, y = pct_win, group = team_code)) +
  geom_path(color = "gray80", size = .3, 
            arrow = grid::arrow(angle = 15, length = unit(2, "mm"), type = "closed"),
            data = dplyr::filter(wins_path, team_label == "Other")) +
  geom_path(aes(color = team_label), size = 1,
            arrow = grid::arrow(angle = 15, length = unit(3, "mm"), type = "closed"),
            data = dplyr::filter(wins_path, team_label != "Other")) +
  geom_text(aes(color = team_label, label = team_name), 
            fontface = "bold", size = 8/.pt, hjust = 1, nudge_x = -2,
            data = dplyr::filter(wins_path, team_label != "Other" & year == 2014)) +
  scale_x_continuous(breaks = seq(0, 300, 10), label = label_skip_dollar(5), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, .1), label = label_skip_percent(2)) +
  scale_color_manual(values = c("#5b0616","#f5b325","#660099")) +
  coord_cartesian(xlim = c(20, 290)) +
  labs(title = "Salary spend and winning over time",
       x = "Team salary (inflation-adjusted, in millions)", 
       y = "Percent regular-season wins") +
  theme_basic_no_grid(legend.position = "none")
print(p)
ggsave("exercise/exercise_2.png", width = 5, height = 5)

# Exercise 3. Histogram of team salary spending
p <- ggplot(salary) +
  geom_histogram(aes(x = salary_team), binwidth = 10, color = "white", size = .3125) +
  facet_wrap(~ season_3) +
  scale_x_continuous(breaks = seq(0, 300, 10), label = label_skip_dollar(5), expand = c(0,5)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Team salary spend histogram",
       x = "Team salary (inflation-adjusted, in millions)", 
       y = NULL) +
  theme_basic(axis.line.x = element_line(size = .3125, color = "grey40"),
              axis.ticks.x = element_line(size = .3125, color = "grey40"))
print(p)
ggsave("exercise/exercise_3.png", width = 5, height = 5)

# Exercise 4. Ridgeline plot of salary on court
p <- ggplot(nba_team_game) +
  ggridges::geom_density_ridges(aes(x = salary_on_court, y = season), size = .2) +
  scale_x_continuous(breaks = seq(0, 2000, 100), labels = label_skip_dollar(5), expand = c(0,0)) +
  scale_y_discrete(expand = c(.01, 0)) +
  coord_cartesian(xlim = c(0, 1750)) +
  labs(title = "Salary-on-court density by season",
       x = "Salary on court (inflation-adjusted, in thousands)", 
       y = NULL) +
  theme_basic_no_grid(axis.line.y = element_blank(),
                      axis.ticks.y = element_blank())
print(p)
ggsave("plot/exercise_4.png", width = 5, height = 5)

# Exercise 5. Three-pointers and game wins
p <- ggplot(field_goals_3pt) +
  geom_raster(aes(x = bin_3pt_attempted, y = bin_3pt_pct, fill = pct_win), hjust = 1, vjust = 1) +
  scale_x_continuous(breaks = seq(0, 100, 5), labels = label_skip_number(4), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = label_skip_percent(4), expand = c(0,0)) +
  scale_fill_viridis_c(option = "magma", begin = .2, end = .9, labels = label_percent()) +
  labs(title = "3-pointers and game wins",
       x = "3-point attempts",
       y = "3-point success",
       fill = "Percent wins") +
  theme_basic_no_grid(legend.position = "top", 
                      legend.title = element_text(vjust = .75))
print(p)
ggsave("exercise/exercise_5.png", width = 5, height = 5)

