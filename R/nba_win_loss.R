load("data/nba_team_game.rda")

# Logistic regression of regular season games.
win_loss_glm <- glm(
  formula = is_win ~ 
    year * (salary_on_court + I(salary_on_court^2)) + 
    salary_gini * (salary_on_court + year) +
    defensive_rebounds * blocks +
    personal_fouls * (assists + defensive_rebounds + offensive_rebounds) + I(personal_fouls^2) +
    turnovers, 
  data = nba_team_game, 
  family = "binomial"
)
summary(win_loss_glm)

# 1. Plot the predicted effect of salary_on_court and salary_gini on the probability of winning
# (is_win) for the 2013-14 and 2021-22 seasons. Use salary_gini as the grouping variable, and  
# set its values at the 10th and 90th percentile. Only predict salary_on_court within the relevant
# range for the given year and show 95% confidence intervals.
plot_data <- nba_team_game %>%
  dplyr::filter(year %in% c(2014,2022)) %>%
  group_by(year) %>%
  summarise(season = season[1],
            datagrid(
              model           = win_loss_glm,
              year            = year[1],
              salary_gini     = quantile(.$salary_gini, c(.10, .90)),
              salary_on_court = seq(min(salary_on_court), max(salary_on_court), length.out = 40)
            )) %>%
  predictions(win_loss_glm, newdata = ., type = "response", vcov = TRUE) %>%
  mutate(salary_gini_label = sprintf(ifelse(salary_gini == min(salary_gini),
                                            "Low inequality (%s)",
                                            "High inequality (%s)"), number(salary_gini, .01)))

p1 <- ggplot(plot_data, aes(x = salary_on_court, y = predicted, group = salary_gini)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "gray60", alpha = .3) +
  geom_line(aes(color = salary_gini_label)) +
  geom_text(aes(label = salary_gini_label, 
                color = salary_gini_label,
                family = google_font("Crimson Pro")),
            hjust = 0, nudge_x = 20,
            data = plot_data %>% 
              dplyr::filter(year == 2014 & salary_on_court == max(salary_on_court[year == 2014]))) +
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(200, 2000, 100), 
                     labels = label_skip_dollar(4)) +
  scale_y_continuous(breaks = seq(0, 1, .1), 
                     labels = label_skip_percent(2)) +
  scale_colour_colorsafe_discrete(palette = "Set 2.3") +
  facet_wrap(~ season) +
  labs(title = "Probability of winning a regular-season game",
       subtitle = "**Gini index** measures players' salary inequality. 
       **Salary on court** is the sum of the players' per-game salary. 
       Both are weighted by minutes played.",
       x = "Salary on court (in thousands)",
       y = NULL,
       parse = TRUE) +
  theme_basic_no_grid(base_family = google_font("Crimson Pro"),
                      base_size = 10,
                      plot.subtitle = element_textbox(),
                      legend.position = "none")

showtext_auto()
print(p1)
ggsave_showtext("plot/nba_win_loss_1.png", width = 10, height = 5)
showtext_auto(enable = FALSE)

# 2. Plot the predicted effect as a panel grid, showing more years and Gini values.
plot_data <- nba_team_game %>%
  dplyr::filter(year %in% c(2014,2016,2018,2020,2022)) %>%
  group_by(year) %>%
  summarise(
    season = season[1],
    datagrid(
      model = win_loss_glm,
      year  = year,
      salary_gini = round(quantile(.$salary_gini, c(.05,.5,.95)), 2),
      salary_on_court = seq(min(salary_on_court), max(salary_on_court), length.out = 40)
    )
  ) %>%
  predictions(win_loss_glm, newdata = ., type = "response", vcov = FALSE) %>%
  group_by(year) %>%
  mutate(salary_gini_label = paste0("Gini ", 
                                    rep(c("5th","50th","95th"), each = 40), " (",
                                    number(salary_gini, accruacy = .01), ")")) %>%
  mutate(salary_gini_label = factor(salary_gini_label, unique(salary_gini_label)))


p2 <- ggplot(plot_data, aes(x = salary_on_court, y = predicted)) +
  geom_line() +
  facet_grid(salary_gini_label ~ season, scale = "free_x") +
  scale_x_continuous(expand = c(0,0), labels = label_dollar()) +
  labs(title = "Probability of winning a regular-season game",
       subtitle = "**Gini index** measures players' salary inequality. 
       **Salary on court** is the sum of the players' per-game salary. 
       Both are weighted by minutes played.",
       x = "Salary on court (in thousands)",
       y = "Probability of winning") +
  theme_basic_no_grid(base_family = google_font("Crimson Pro"),
                      base_size = 10,
                      panel.spacing = unit(4, "mm"),
                      panel.background = element_rect(fill = "gray95", color = NA),
                      panel.grid.major.y = element_line(color = "white"),
                      plot.subtitle = element_textbox())
showtext_auto()
print(p2)
ggsave_showtext("plot/nba_win_loss_2.png", width = 10, height = 5)
showtext_auto(enable = FALSE)

# 3. Plot the effect of personal fouls on the probability of winning, moderated by assists,
# defensive rebounds and offensive rebounds. Show the 95% confidence intervals.
moderators <- c("assists","defensive_rebounds","offensive_rebounds")
group_labels <- c("Assists","Defensive rebounds","Offensive rebounds")
names(group_labels) <- moderators

# Loop through the three moderators.
plist <- lapply(moderators, function(moderator) {
  
  # Create the plot data.
  args <- list(model = win_loss_glm,
               personal_fouls = seq(min(nba_team_game$personal_fouls), 
                                    max(nba_team_game$personal_fouls), 
                                    length.out = 40))
  args[[moderator]] <- quantile(pull(nba_team_game, moderator), c(.10, .90))
  plot_data <- predictions(win_loss_glm, newdata = do.call(datagrid, args))
  moderator_key <- paste0(c("Low (","High ("), round(plot_data[, moderator], 2), ")")
  plot_data[, moderator] <- factor(moderator_key, moderator_key[1:2])
  
  # Plot the moderator line plot.
  p <- ggplot(plot_data, aes_string(x = "personal_fouls", y = "predicted", group = moderator)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray60", alpha = .3) +
    geom_line(aes_string(color = moderator)) +
    scale_x_continuous(expand = c(0.01,0), 
                       breaks = seq(0, 50, 5), 
                       labels = label_skip_number(2)) +
    scale_y_continuous(breaks = seq(0, 1, .1), 
                       labels = label_skip_percent(2)) +
    scale_colour_colorsafe_discrete(palette = "Set 2.3") +
    coord_cartesian(ylim = c(.05,.9)) +
    labs(subtitle = group_labels[moderator],
         x = "Personal fouls",
         color = group_labels[moderator]) +
    theme_basic_no_grid(base_family = google_font("Crimson Pro"),
                        legend.title = element_blank(),
                        legend.position = c(.5, 1),
                        legend.justification = c(.5, 1),
                        legend.direction = "horizontal",
                        plot.subtitle = element_text(face = "bold", hjust = .5),
                        plot.margin = margin(t = 30))
  if (moderator == "assists") {
    p <- p + labs(y = "Probability of winning")
  } else {
    p <- p + labs(y = NULL)
  }
  return(p)
})

# Combine the three plots together and add a title.
p3 <- cowplot::plot_grid(plotlist = plist, nrow = 1, align = "v")
p3 <- cowplot::ggdraw(p3) +
  cowplot::draw_label("When personal fouls help or hurt", size = 15,
                      x = 0.01, y = .99, hjust = 0, vjust = 1,
                      fontfamily = google_font("Crimson Pro"), 
                      fontface = "bold")

showtext_auto()
print(p3)
ggsave_showtext("plot/nba_win_loss_3.png", width = 10, height = 5)
showtext_auto(enable = FALSE)

# 4. Plot the effect of personal fouls using heat maps.
# Loop through the three moderators and combine in long format.
plot_data <- do.call(rbind, lapply(moderators, function(moderator) {
  
  # Create the plot data.
  args <- list(model = win_loss_glm,
               personal_fouls = seq(min(nba_team_game$personal_fouls), 
                                    max(nba_team_game$personal_fouls), 
                                    length.out = 40))
  args[[moderator]] <- seq(min(pull(nba_team_game, moderator)), 
                           max(pull(nba_team_game, moderator)), 
                           length.out = 40)
  predictions(win_loss_glm, newdata = do.call(datagrid, args)) %>%
    mutate(moderator_value = .data[[moderator]],
           moderator_name  = group_labels[moderator],
           width = personal_fouls[41] - personal_fouls[1],
           height = .data[[moderator]][2] - .data[[moderator]][1])
}))

p4 <- ggplot(plot_data, aes(x = personal_fouls, y = moderator_value, fill = predicted)) +
  geom_tile(aes(width = width, height = height)) +
  facet_wrap(~ moderator_name, scale = "free_y", strip.position = "left") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_c(option = "inferno", breaks = seq(0, 1, .2), labels = percent) +
  labs(title = "When personal fouls help or hurt",
       x = "Personal fouls",
       y = NULL,
       fill = "Probility of winning") +
  theme_basic_no_grid(base_family = google_font("Crimson Pro"),
                      legend.position = "bottom",
                      legend.title = element_text(vjust = .75),
                      strip.placement = "outside",
                      strip.text = element_text(face = "plain"))

showtext_auto()
print(p4)
ggsave_showtext("plot/nba_win_loss_4.png", width = 10, height = 5)
showtext_auto(enable = FALSE)




