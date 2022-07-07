load("data/nba_team_game.rda")
nba_team_game <- nba_team_game %>% as_tibble()

# Calculate cumulative win-loss record, and identify Lebron's teams.
cumulative_win_loss <- nba_team_game %>%
  dplyr::filter(year >= 2015) %>%
  group_by(year, team_code) %>%
  summarise(season      = season[1], 
            team_name   = team_name[1], 
            game_number = c(0, game_number), 
            win_loss    = c(0, cumsum(ifelse(is_win, 1, -1)))) %>%
  mutate(lebron_team = ifelse(year >= 2015 & year <= 2018 & team_code == "CLE"
                              | year > 2018 & team_code == "LAL", "Yes","No"),
         focus_team = ifelse(team_code %in% c("CLE","LAL"), team_name, "Other")) %>%
  mutate(focus_team = factor(focus_team, c("Other","Cleveland Cavaliers","Los Angeles Lakers")))

p1a <- ggplot(cumulative_win_loss) +
  geom_line(aes(x = game_number, y = win_loss, group = team_code, color = focus_team), 
            size = .3, alpha = .6,
            data = cumulative_win_loss %>% dplyr::filter(focus_team == "Other")) +
  geom_hline(aes(yintercept = 0), color = "grey0") +
  geom_line(aes(x = game_number, y = win_loss, group = team_code, 
                color = focus_team, size = lebron_team), lineend = "round",
            data = cumulative_win_loss %>% dplyr::filter(focus_team != "Other")) +
  scale_x_continuous("Games played", expand = c(0.02,0), 
                     breaks = seq(0,80,10), labels = label_skip(seq(0,80,10), 2, 1)) +
  scale_y_continuous(NULL, expand = c(0,0),
                     breaks = seq(-90,90,10), labels = label_skip(seq(-90,90,10), 3)) +
  scale_size_discrete(range = c(.6, 1.2), guide = "none") +
  scale_color_discrete(NULL, type = c("#5b0616","#f5b325","gray80")) +
  facet_wrap(~ season, nrow = 2) +
  labs(title = "Win-loss differential of Lebron James's teams",
       subtitle = "Played for Cavaliers 2014-18, Lakers 2018-22") +
  theme_basic_no_grid(base_size = 9,
                      base_family = google_font("Crimson Pro"), 
                      legend.position = "top", 
                      legend.justification = "left",
                      legend.margin = margin(0),
                      legend.box.margin = margin(l = -2, unit = "mm"))
showtext::showtext_auto()
print(p1a)
ggsave_showtext("plot/Lebrons teams (a).png", width = 5, height = 5)
showtext::showtext_auto(enable = FALSE)


p1b <- ggplot(cumulative_win_loss) +
  geom_line(aes(x = game_number, y = win_loss, group = team_code, 
                color = team_name, size = lebron_team), lineend = "round") +
  scale_x_continuous("Games played", expand = c(0.01,0), breaks = seq(0,80,10)) +
  scale_y_continuous(NULL, expand = c(0,0)) +
  scale_size_discrete(range = c(.6, 1.2), guide = "none") +
  scale_color_discrete(NULL, type = c("#5b0616","#f5b325")) +
  facet_wrap(~ season, nrow = 2) +
  labs(title = "Win-loss differential of Lebron James's teams",
       subtitle = "Played for Cavaliers 2014-18, Lakers 2018-22") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#bb00cc"),
        panel.background = element_rect(fill = "#c3fad0"),
        plot.title = element_text(color = "#937903"),
        plot.subtitle = element_text(color = "#a8ff09"))
print(p1b)
ggsave("plot/Lebrons teams (b).png", device = "png", width = 5, height = 5)